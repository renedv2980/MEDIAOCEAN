*          DATA SET SPREPC102  AT LEVEL 123 AS OF 08/14/08                      
*PHASE SPC102A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE XSORT                                                                  
         SPACE 1                                                                
**********************************************************************          
*   BPOO 5/8/98   CHANGE NETWORK DEPENDENCE TO SCBL64 AND SCBLSEQ               
*                 INSTEAD OF SYSNET.                                            
*   SCHO 7/20/99  DISPLAY ADDRESS WITH REPORT                                   
**********************************************************************          
*                                                                               
* QOPT1 DETERMINES REQUEST TYPE                                                 
* QOPT2 Y - ONLY DEACTIVATED STATIONS                                           
* QOPT3 Y - STATION WITH EIX=Y  N - STATION WITH EIX=N                          
* QOPT5 TRACE                                                                   
*                                                                               
         SPACE 1                                                                
         TITLE 'SPC102 - CABLE FILE REPORT'                                     
SPC102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPC102                                                         
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING SPC102+4096,R9                                                   
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         EJECT                                                                  
         SPACE 1                                                                
         CLI   MODE,REQFRST                                                     
         BE    MAIN10                                                           
         CLI   MODE,CLTFRST                                                     
         BE    PROCSTAT                                                         
         CLI   MODE,REQLAST                                                     
         BE    MAIN100                                                          
         B     EXIT                                                             
*                                                                               
YES      SR    RC,RC               SET CC                                       
NO       LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        REQUEST FIRST                                                          
*                                                                               
MAIN10   DS    0H                                                               
         LA    R1,MYHEAD                                                        
         ST    R1,HEADHOOK                                                      
*                                                                               
         ZAP   TOTREC,=P'0'                                                     
*        XC    SRREC,SRREC                                                      
         LA    RE,SRREC                                                         
         LA    RF,L'SRREC                                                       
         XCEFL                                                                  
*                                                                               
         L     RF,=V(SORTER)                                                    
         ST    RF,VSORTER                                                       
         L     RF,=V(XSORT)                                                     
         ST    RF,VXSORT                                                        
*                                                                               
         MVI   SORTSTAT,0                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         BAS   RE,GETMGRP          GET MKTS FROM MGRP                           
         MVC   SVNTWRK,QBOOK1                                                   
*                                                                               
         MVI   RCSUBPRG,1                                                       
         CLI   QOPT1,C'1'          REQUEST TYPE                                 
         BE    MAINX                                                            
         MVI   RCSUBPRG,2                                                       
         CLI   QOPT1,C'2'                                                       
         BE    MAINX                                                            
         MVI   RCSUBPRG,3                                                       
         XC    LSTCALL,LSTCALL                                                  
*                                                                               
MAINX    B     EXIT                                                             
*                                                                               
*        RUN LAST                                                               
*                                                                               
MAIN100  DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'END'                                             
         MVI   SORTSTAT,0                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
PROCSTAT LA    R8,KEY                                                           
         USING STATRECD,R8                                                      
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVC   STAKMED,QMED        MEDIA                                        
         MVC   STAKCALL,=C'0000'   START WITH NUMERIC                           
         MVC   MYKEY,KEY                                                        
         L     R8,ADSTAT                                                        
         GOTO1 HIGHSTA                                                          
         B     ST20                                                             
*                                                                               
ST10     GOTO1 SEQSTA                                                           
*                                                                               
ST20     CLI   QOPT5,C'Y'          TRACE ALL RECORDS READ                       
         BNE   ST25                                                             
         MVC   P(12),=C'STATION READ'                                           
         MVC   P+13(15),STAKEY                                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
ST25     CLC   STAKEY(2),MYKEY     SAME RECORD TYPE/MEDIA                       
         BNE   ST100                                                            
         CLC   STAKAGY,AGENCY      SAME AGENCY                                  
         BNE   ST10                                                             
         BAS   RE,TSTMGRP          IS THIS IN THE REQUESTED MGRP                
         BNE   ST10                                                             
*                                                                               
         CLC   QBOOK1,SPACES       IF REQUESTING NETWORK FILTER                 
         BNH   *+12                                                             
         BAS   RE,TSTNTFLT         TEST FOR NETWORK FILTER                      
         BNE   ST10                                                             
*                                                                               
         CLI   QOPT2,C'Y'          ONLY DEACTIVATED STATIONS                    
         BNE   ST25A                                                            
         CLI   SSYSDACT,X'FF'      YES - IS THIS A DEACTIVATED STATION          
         BNE   ST10                                                             
         B     ST25B               YES CONTINUE                                 
*                                                                               
ST25A    CLI   SSYSDACT,X'FF'      ELSE IF DEACTIVATED STATION                  
         BE    ST10                GET NEXT STATION                             
*                                                                               
ST25B    CLI   QOPT3,C'Y'          ARE WE FILTERING ON EIX?                     
         BNE   ST25C                                                            
         CLI   SEIXSTA,C'Y'        WE ONLY WANT EIX=Y                           
         BNE   ST10                NEXT RECORD                                  
*                                                                               
ST25C    CLI   QOPT1,C'3'          FOR VERSION 3 (BY STATION)                   
         BNE   ST27                                                             
         CLC   =C'ALL',QCLT        ALL CLIENTS ONLY GET GLOBALS                 
         BNE   ST26                                                             
         CLC   STAKCLT,ZEROES      IS THIS THE REQUESTED CLIENT                 
         BNE   ST10                                                             
         B     ST27                                                             
*                                                                               
ST26     CLC   STAKCALL,LSTCALL    DID WE ALREADY DO THIS STATION               
         BE    ST10                                                             
         CLC   STAKCLT,QCLT        SPECIFIC CLIENTS GET THEIRS                  
         BE    ST26A                                                            
         CLC   STAKCLT,ZEROES      OR GLOBAL                                    
         BNE   ST10                                                             
*                                                                               
ST26A    MVC   LSTCALL,STAKCALL                                                 
*                                                                               
ST27     CLC   =C'ALL',QMKT        MARKET FILTER                                
         BE    ST30                                                             
         CLC   SMKT,QMKT                                                        
         BNE   ST10                                                             
*                                                                               
ST30     LA    R3,SRREC                                                         
*        XC    SRREC,SRREC                                                      
         LA    RE,SRREC                                                         
         LA    RF,L'SRREC                                                       
         XCEFL                                                                  
*                                                                               
* ----------  CHECK IF RECORD IS A CONVERTED RECORD -----------                 
*----------- IF NOT THEN IT DOESN'T HAVE ANY NETWORKS ---------                 
*---------- OR CLT SPEFIC RECORDS -----------------------------                 
*                                                                               
         LA    RE,SCBLSQNQ                                                      
         ZICM  R0,STAKLEN,2                                                     
         CR    RE,R0                                                            
         BNE   ST33                                                             
         CLC   STAKCLT,=C'000'    MAKE SURE NOT CLT SPECIFIC                    
         BE    *+16                                                             
ST33     XC    SCBL24,SCBL24                                                    
         XC    SCBLSEQ,SCBLSEQ                                                  
*                                                                               
ST35     CLI   QOPT1,C'1'          REQUEST TYPE                                 
         BE    ST40                                                             
         CLI   QOPT1,C'2'                                                       
         BE    ST50                                                             
         CLI   QOPT1,C'3'                                                       
         BE    ST60                                                             
         B     ST100                                                            
*                                                                               
         USING SMKTD,R3            SORT BY MARKET                               
ST40     DS    0H                                                               
         MVC   SMCALL,STAKCALL     CALL LETTERS                                 
         MVC   SMCLT,STAKCLT       CLIENT                                       
         MVC   SMREP,SPAYREP       REP                                          
         MVC   SMMKT,SMKT          MARKET                                       
         MVC   SMSYSCD,SSYSNAME    SYSTEM NAME                                  
         MVC   SMSYSNET,SSYSNETS   SYSTEM NETWORKS                              
         MVC   SMSCBL24,SCBL24                                                  
         MVC   SMSCBLSQ,SCBLSEQ                                                 
         BAS   RE,GETADDR          GET ADDRESS RECORD                           
         MVC   SMCITY,CITY         CITY                                         
         MVC   SMSTATE,STATE       STATE                                        
         MVC   SMSCADD,STREET      STREET ADDRESS                               
         MVC   SMSCZIP,ZIP         ZIP                                          
*                                                                               
         B     ST70                                                             
         DROP  R3                                                               
*                                                                               
         USING SCITYD,R3           SORT BY CITY                                 
ST50     DS    0H                                                               
         MVC   SCCALL,STAKCALL     CALL LETTERS                                 
         MVC   SCCLT,STAKCLT       CLIENT                                       
         MVC   SCREP,SPAYREP       REP                                          
         MVC   SCSYSCD,SSYSNAME    SYSTEM NAME                                  
         MVC   SCSYSNET,SSYSNETS   SYSTEM NETWORKS                              
         MVC   SCSCBL24,SCBL24                                                  
         MVC   SCSCBLSQ,SCBLSEQ                                                 
         BAS   RE,GETADDR          GET ADDRESS RECORD                           
         MVC   SCCITY,CITY         CITY                                         
         MVC   SCSTATE,STATE       STATE                                        
         MVC   SCSCADD,STREET      STREET ADDRESS                               
         MVC   SCSCZIP,ZIP         ZIP                                          
         B     ST70                                                             
         DROP  R3                                                               
*                                                                               
         USING SSYSD,R3            SORT BY CALL LETTERS                         
ST60     DS    0H                                                               
         MVC   SSCALL,STAKCALL     CALL LETTERS                                 
         MVC   SSCLT,STAKCLT       CLIENT                                       
         MVC   SSREP,SPAYREP       REP                                          
         MVC   SSMKT,SMKT          MARKET                                       
         MVC   SSSYSCD,SSYSNAME    SYSTEM NAME                                  
         MVC   SSSYSNET,SSYSNETS   SYSTEM NETWORKS                              
         MVC   SSSCBL24,SCBL24                                                  
         MVC   SSSCBLSQ,SCBLSEQ                                                 
         BAS   RE,GETADDR          GET ADDRESS RECORD                           
         MVC   SSCITY,CITY         CITY                                         
         MVC   SSSTATE,STATE       STATE                                        
         MVC   SSSCADD,STREET      STREET ADDRESS                               
         MVC   SSSCZIP,ZIP         ZIP                                          
         B     ST70                                                             
         DROP  R3                                                               
*                                                                               
ST70     BAS   RE,PUTSORT          PUT RECORD TO SORTER                         
         B     ST10                                                             
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
*        GET RECORDS FROM SORTER & PRINT REPORT                                 
*                                                                               
ST100    DS    0H                                                               
         CLI   SORTSTAT,0                                                       
         BE    STX                                                              
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     R3,4(R1)                                                         
         LTR   R3,R3                                                            
         BZ    ST150               NO MORE RECORDS                              
         LA    R2,P                                                             
         XC    MYSCBLSQ,MYSCBLSQ                                                
         XC    MYSCBL24,MYSCBL24                                                
*                                                                               
         CLI   QOPT1,C'1'          REQUEST TYPE                                 
         BE    ST110                                                            
         CLI   QOPT1,C'2'                                                       
         BE    ST120                                                            
         CLI   QOPT1,C'3'                                                       
         BE    ST130                                                            
         B     ST150                                                            
*                                                                               
         USING SMKTD,R3            BY MARKET                                    
         USING LMKTD,R2                                                         
ST110    DS    0H                                                               
         MVC   LMCALL,SMCALL       CALL LETTERS                                 
         MVI   LMCALL+4,C'T'                                                    
         CLC   SMCLT,ZEROES                                                     
         BE    *+10                                                             
         MVC   LMCLT,SMCLT         CLIENT                                       
         CLC   SMREP,ZEROES                                                     
         BE    *+10                                                             
         MVC   LMREP,SMREP         REP                                          
         MVC   LMMKT,SMMKT         MARKET                                       
         MVC   WORK(4),SMMKT                                                    
         BAS   RE,GETMKT                                                        
         MVC   LMMKTNM,MYMKTNM                                                  
         MVC   LMSYSCD,SMSYSCD     SYSTEM NAME                                  
         MVC   LMCITY,SMCITY       CITY                                         
         MVC   LMSTATE,SMSTATE     STATE                                        
*        MVC   MYSYSNET,SMSYSNET   SYSTEM NETWORKS                              
         MVC   MYSCBL24,SMSCBL24                                                
         MVC   MYSCBLSQ,SMSCBLSQ                                                
*                                                                               
*  GET THE PACKED MKT/STA SO I MAY USE IT FOR MSUNPK LATER                      
         GOTO1 MSPACK,DMCB,(X'40',SMMKT),LMCALL,MYMKTSTA                        
*                                                                               
         LA    R1,LMSYSNET                                                      
         LA    R6,L'LMSYSNET(R1)                                                
         BAS   RE,SETNET2                                                       
         LA    R2,P2                                                            
         MVC   LMCITY,SMSCADD                                                   
         LA    R2,P3                                                            
         MVC   LMCITY(10),SMSCZIP                                               
         B     ST140                                                            
         DROP  R2,R3                                                            
*                                                                               
         USING SCITYD,R3           BY CITY                                      
         USING LCITYD,R2                                                        
ST120    DS    0H                                                               
         MVC   LCCALL,SCCALL       CALL LETTERS                                 
         MVI   LCCALL+4,C'T'                                                    
         CLC   SCCLT,ZEROES                                                     
         BE    *+10                                                             
         MVC   LCCLT,SCCLT         CLIENT                                       
         CLC   SCREP,ZEROES                                                     
         BE    *+10                                                             
         MVC   LCREP,SCREP         REP                                          
         MVC   LCSYSCD,SCSYSCD     SYSTEM NAME                                  
         MVC   LCCITY,SCCITY       CITY                                         
         MVC   LCSTATE,SCSTATE     STATE                                        
*        MVC   MYSYSNET,SCSYSNET   SYSTEM NETWORKS                              
         MVC   MYSCBL24,SCSCBL24                                                
         MVC   MYSCBLSQ,SCSCBLSQ                                                
*                                                                               
*  GET THE PACKED MKT/STA SO I MAY USE IT FOR MSUNPK LATER                      
         GOTO1 MSPACK,DMCB,(X'40',=C'0000'),LCCALL,MYMKTSTA                     
*                                                                               
         LA    R1,LCSYSNET         SYSTEM NETWORKS                              
         LA    R6,L'LCSYSNET(R1)                                                
         BAS   RE,SETNET2                                                       
         LA    R2,P2                                                            
         MVC   LCSYSCD,SCSCADD     PUT ADDRESS UNDER NETWORK FOR OPT2           
         LA    R2,P3                                                            
         MVC   LCSYSCD(10),SCSCZIP                                              
         B     ST140                                                            
         DROP  R2,R3                                                            
*                                                                               
         USING SSYSD,R3            BY CALL LETTERS                              
         USING LSYSD,R2                                                         
ST130    DS    0H                                                               
         MVC   LSCALL,SSCALL       CALL LETTERS                                 
         MVI   LSCALL+4,C'T'                                                    
         CLC   SSCLT,ZEROES                                                     
         BE    *+10                                                             
         MVC   LSCLT,SSCLT         CLIENT                                       
         CLC   SSREP,ZEROES                                                     
         BE    *+10                                                             
         MVC   LSREP,SSREP         REP                                          
         MVC   LSMKT,SSMKT         MARKET                                       
         MVC   WORK(4),SSMKT                                                    
         BAS   RE,GETMKT                                                        
         MVC   LSMKTNM,MYMKTNM                                                  
         MVC   LSSYSCD,SSSYSCD     SYSTEM NAME                                  
         MVC   LSCITY,SSCITY       CITY                                         
         MVC   LSSTATE,SSSTATE     STATE                                        
*        MVC   MYSYSNET,SSSYSNET                                                
         MVC   MYSCBL24,SSSCBL24                                                
         MVC   MYSCBLSQ,SSSCBLSQ                                                
*                                                                               
*  GET THE PACKED MKT/STA SO I MAY USE IT FOR MSUNPK LATER                      
         GOTO1 MSPACK,DMCB,(X'40',SSMKT),LSCALL,MYMKTSTA                        
*                                                                               
         LA    R1,LSSYSNET         SYSTEM NETWORKS                              
         LA    R6,L'LSSYSNET(R1)                                                
         BAS   RE,SETNET2                                                       
         LA    R2,P2                                                            
         MVC   LSCITY,SSSCADD                                                   
         LA    R2,P3                                                            
         MVC   LSCITY(10),SSSCZIP                                               
         B     ST140                                                            
         DROP  R2,R3                                                            
*                                                                               
ST140    GOTO1 REPORT                                                           
         B     ST100                                                            
*                                                                               
ST150    CP    TOTREC,=P'0'                                                     
         BE    STX                                                              
         GOTO1 REPORT                                                           
         EDIT  TOTREC,(12,P),ZERO=NOBLANK,COMMAS=YES                            
         MVC   P+13(13),=C'CABLE RECORDS'                                       
         GOTO1 REPORT                                                           
         MVI   SORTSTAT,0                                                       
*                                                                               
STX      GOTO1 AENDREQ                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        GET STATION ADDRESS RECORD                                             
*                                                                               
GETADDR  NTR1                                                                   
         USING STATRECD,R2                                                      
         L     R2,ADSTAT                                                        
         MVC   MYKEY,STAKEY        SAVE KEY                                     
         LA    R2,MYKEY                                                         
         MVC   CITY,SPACES                                                      
         MVC   STATE,SPACES        INITIALIZE TO SPACES                         
         MVC   STREET,SPACES                                                    
         MVC   ZIP,SPACES                                                       
         LA    R7,KEY                                                           
         USING ADDRECD,R7                                                       
         XC    KEY,KEY                                                          
         MVI   ADDKTYPE,ADDKTYPQ  RECORD TYPE                                   
         MVC   ADDKMED,QMED        MEDIA                                        
         MVC   ADDKCALL,STAKCALL   CALL LETTERS                                 
         MVC   ADDKAGY,AGENCY      AGENCY                                       
         GOTO1 HIGHSTAD                                                         
         L     R7,ADSTATAD                                                      
         CLC   ADDRKEY(ADDKFILL-ADDRKEY),KEYSAVE                                
         BNE   GAX                                                              
         MVC   CITY,A2LINE         CITY                                         
         MVC   STATE,A3LINE        STATE                                        
         MVC   STREET,A1LINE       STREET ADDRESS                               
         MVC   ZIP,ABIGZIP         ZIP CODE                                     
*                                                                               
         CLI   QOPT5,C'Y'          TRACE ALL RECORDS READ                       
         BNE   GAX                                                              
         MVC   P(12),=C'ADDRESS READ'                                           
         MVC   P+13(15),ADDRKEY                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
GAX      MVC   KEY,MYKEY                                                        
         GOTO1 HIGHSTA             RESTORE READ SEQUENCE                        
         CLI   QOPT5,C'Y'          TRACE ALL RECORDS READ                       
         BNE   GAX10                                                            
         MVC   P(15),=C'STATION RE-READ'                                        
         MVC   P+16(15),KEY                                                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
GAX10    B     EXIT                GET THE NEXT RECORD                          
         DROP  R2,R7                                                            
         EJECT                                                                  
*                                                                               
*        GET MARKET RECORD                                                      
*                                                                               
GETMKT   NTR1                                                                   
         USING MKTRECD,R2                                                       
         LA    R2,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   MKTKTYPE,MKTKTYPQ   RECORD TYPE                                  
         MVC   MKTKMED,QMED        MEDIA                                        
         MVC   MKTKMKT,WORK        MARKET                                       
         MVC   MKTKAGY,AGENCY      AGENCY                                       
         MVC   MKTKFILL,ZEROES                                                  
         GOTO1 HIGHMKT                                                          
         L     R2,ADMARKET                                                      
         CLC   MKTKEY,KEYSAVE                                                   
         BNE   GMX                                                              
         MVC   MYMKTNM,MKTNAME                                                  
         CLI   QOPT5,C'Y'          TRACE ALL RECORDS READ                       
         BNE   GMX                                                              
         MVC   P(11),=C'MARKET READ'                                            
         MVC   P+13(15),MKTKEY                                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
GMX      B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
SETNET2  NTR1  LABEL=*                                                          
*                                                                               
         LA    R3,MYCABTAB                                                      
         XCEF  (R3),508                                                         
         XC    COUNT,COUNT                                                      
         LA    R7,1                                                             
         LA    R3,MYSCBL24                                                      
         ST    R1,STARTNET                                                      
         LA    R5,MYCABTAB                                                      
*                                                                               
SETLOOP  TM    0(R3),X'80'           IF THIS TOP 24 BIT IS SET                  
         BZ    SET270                                                           
* CHECK THE  LINES                                                              
SET230   DS    0H                                                               
*                                                                               
         XC    WORK,WORK                                                        
         STC   R7,WORK+4                                                        
         MVI   WORK+2,X'F0'                                                     
         LR    R0,R1                                                            
         GOTO1 MSUNPK,DMCB,(X'C0',WORK),WORK+10,WORK+15                         
         LR    R1,R0                                                            
         LA    R3,WORK                                                          
         MVC   0(3,R5),WORK+20                                                  
         MVI   3(R5),C' '         PADD THE FOURTH BLANK SPACE IN                
         LA    R5,4(R5)                                                         
*                                                                               
SET270   DS    0X                                                               
         LA    R7,1(R7)                                                         
         ZICM  RE,MYSCBL24,3                                                    
         SLL   RE,1                                                             
         STCM  RE,7,MYSCBL24                                                    
         LA    R3,MYSCBL24                                                      
         OC    MYSCBL24,MYSCBL24                                                
         BNZ   SETLOOP                                                          
*                                                                               
* ------------  NOW DO NON TOP 24 -------------------------------               
         LA    R7,MYSCBLSQ                                                      
         LA    R3,206(R7)          R3 = A(END OF TABLE)                         
         MVI   COUNT,25                                                         
SETN24   CLC   0(2,R7),=XL2'0'                                                  
         BE    SETDID24                                                         
         CR    R7,R3                                                            
         BNL   SETDID24                                                         
         CLC   0(2,R7),=X'FFFF'    CABLE NETWORK PLACEHOLDER?                   
         BNE   SET275                                                           
         LA    R7,2(R7)            YES, WE CAN SKIP THEM                        
         ZIC   RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,COUNT                                                         
         B     SETN24                                                           
*                                                                               
SET275   XC    WORK,WORK                                                        
         LR    R0,R1                                                            
         MVC   MSUMKSTA,MYMKTSTA                                                
         OC    MSUMKSTA+4(1),COUNT                                              
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'C0',MSUMKSTA),WORK+10,WORK+15                     
         LR    R1,R0                                                            
         MVC   0(3,R5),WORK+20                                                  
         MVI   3(R5),C' '         PADD THE FOURTH BLANK SPACE IN                
*                                                                               
         LA    R7,2(R7)           GET NEXT SEQ NUMBER                           
         LA    R5,4(R5)           ADVANCE NETWORK CABTAB                        
         LA    RE,MYCABEND                                                      
         CR    R5,RE               TOO MANY IN MYCABTAB                         
         BL    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,COUNT                                                         
         B     SETN24                                                           
*                                                                               
SETDID24 LA    R5,MYCABTAB                                                      
         XC    COUNT,COUNT                                                      
*   COUNT TOTAL NUMBER OF NETWORKS IN TABLE TO SORT                             
SETCOUNT OC    0(4,R5),0(R5)                                                    
         BZ    SETSORT                                                          
         ZIC   RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,COUNT                                                         
         LA    R5,4(R5)                                                         
         B     SETCOUNT                                                         
*                                                                               
* NOW I SHOULD XSORT THE TABLE OF NETWORKS *                                    
SETSORT  DS    0X                                                               
         LA    R5,MYCABTAB                                                      
         ZIC   R7,COUNT                                                         
         LR    R0,R1                            SAVE PRINT POSITION             
         GOTO1 VXSORT,DMCB,(0,MYCABTAB),(R7),4,4,0                              
*****************************************************************               
SET310   LA    R5,MYCABTAB                                                      
*                                                                               
         LR    R1,R0                             RESTORE PRINT POSITION         
         LA    R6,L'LMSYSNET(R1)                                                
SETLOOP2 OC    0(4,R5),0(R5)                                                    
         BZ    SETEXIT                                                          
*                                                                               
* CHECK THE  LINES                                                              
*                                                                               
         LR    R0,R1                                                            
         LA    RE,4                                                             
         AR    R0,RE                 DOES 4 MORE CHARS                          
         CR    R0,R6                 FIT ON THIS LINE?                          
         BNH   SET530                                                           
*                                                                               
* NOW MOVE TO NEXT PRINT LINE POSITION                                          
         L     R1,STARTNET                                                      
         LA    R1,132(R1)                                                       
         ST    R1,STARTNET                                                      
         LA    R6,132(R6)                                                       
*                                                                               
SET530   MVC   0(4,R1),0(R5)         MOVE IN TO PRINT LINE                      
         LA    R1,4(R1)              ADVANCE PRINT POSITION                     
*                                                                               
SET570   DS    0X                                                               
         LA    R5,4(R5)              GET NEXT TABLE ENTRY                       
         B     SETLOOP2                                                         
*                                                                               
SETEXIT  B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        SORT UTILITIES                                                         
*                                                                               
PUTSORT  NTR1                                                                   
         CLI   SORTSTAT,0                                                       
         BNE   PS10                                                             
         GOTO1 VSORTER,DMCB,SORT,RECCARD                                        
         MVI   SORTSTAT,1          SORTER HAS BEEN INITIIALIZED                 
*                                                                               
PS10     GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
         AP    TOTREC,=P'1'                                                     
         CLI   QOPT4,C'Y'          TRACE ALL RECORDS PUT TO SORT                
         BNE   PS20                                                             
         MVC   P(7),=C'TO SORT'                                                 
         MVC   P+8(75),SRREC                                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
PS20     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        GET ALL MARKETS IN MARKET GROUP                                        
*                                                                               
GETMGRP  NTR1                                                                   
         CLI   QMGR,C' '           IF REQUESTING MARKET GROUP                   
         BNH   GMGX                                                             
         MVC   MYKEY,KEY                                                        
*                                                                               
         USING MKGRECD,R4                                                       
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'    RECORD TYPE                                  
         MVC   MKGKAGMD,SVAGYMD                                                 
         CLI   QMGR,C'F'           TEST ID A-F                                  
         BH    GMG10                                                            
         CLC   =C'ALL',QCLT                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 CLPACK,DMCB,QCLT,MYBCLT                                          
         MVC   MKGKCLT,MYBCLT      A-F REQUIRE CLT                              
*                                                                               
GMG10    MVC   MKGKMID,QMGR        MKTGRP ID                                    
         DROP  R4                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                ***** TEST ONLY *****                        
         MVC   SVMGRKEY,KEY                                                     
         GOTO1 GETMKTGR                                                         
*                                                                               
         L     R6,ADMKTGRP                                                      
         USING MKGRECD,R6                                                       
         LA    R6,MKGEL                                                         
         USING MKGEL01,R6                                                       
         MVC   SVMGRP1,MKGBK1                                                   
         MVC   SVMGRP2,MKGBK2                                                   
         MVC   SVMGRP3,MKGBK3                                                   
         DROP  R6                                                               
*                                                                               
         CLC   =C'ALL',QSTA                                                     
         BE    GMG12                                                            
         MVC   WORK(4),QSTA                                                     
         LA    R1,WORK+3                                                        
         LA    R2,4                                                             
         CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         BCT   R2,*-10                                                          
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),C'*'                                                       
         BNE   *+14                                                             
         MVI   0(R1),C' '                                                       
         BCTR  R1,0                                                             
         BCT   R2,*-14             R2 HAS COMPARE LEN ON EXIT                   
*                                                                               
         PACK  DUB,WORK(5)                                                      
         MVC   KEY+9(2),DUB+5      SET LOW MKTGRP                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(9),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         UNPK  DUB,KEY+9(3)                                                     
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   DUB+3(0),WORK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVMGRKEY,KEY                                                     
         GOTO1 GETMKTGR                                                         
*                                                                               
         L     R6,ADMKTGRP                                                      
         USING MKGRECD,R6                                                       
         MVI   ELCODE,X'10'                                                     
         USING MKGEL10,R6                                                       
         BAS   RE,GETEL                                                         
         BNE   GMG12                                                            
         MVC   SVMGRPN1,MKGNAM1                                                 
         MVC   SVMGRPN2,MKGNAM2                                                 
         MVC   SVMGRPN3,MKGNAM3                                                 
         DROP  R6                                                               
*                                                                               
GMG12    LA    R3,MGRPTAB                                                       
         MVI   0(R3),X'FF'                                                      
         USING MKGRECD,R4                                                       
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   MKGPTYP,=X'0D82'    RECORD TYPE                                  
         MVC   MKGPAGMD,SVAGYMD                                                 
         CLI   QMGR,C'F'           TEST ID A-F                                  
         BH    *+10                                                             
         MVC   MKGPCLT,MYBCLT      A-F REQUIRE CLT                              
         MVC   MKGPMID,QMGR        MKTGRP ID                                    
         CLC   =C'ALL',QSTA                                                     
         BE    GMG15                                                            
         MVC   WORK(4),QSTA                                                     
         LA    R1,WORK+3                                                        
         LA    R2,4                                                             
         CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         BCT   R2,*-10                                                          
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),C'*'                                                       
         BNE   *+14                                                             
         MVI   0(R1),C' '                                                       
         BCTR  R1,0                                                             
         BCT   R2,*-14             R2 HAS COMPARE LEN ON EXIT                   
*                                                                               
         BCTR  R2,0                DEC 1 FOR EX BELOW                           
         PACK  DUB,WORK(5)                                                      
         MVC   KEY+9(2),DUB+5      SET LOW MKTGRP                               
*                                                                               
GMG15    GOTO1 HIGH                                                             
         B     GMG30                                                            
*                                                                               
GMG20    GOTO1 SEQ                                                              
*                                                                               
GMG30    CLC   KEY(5),KEYSAVE      SAME TYPE/AM/CLT                             
         BNE   GMG50                                                            
         OC    MKGPCLT,MKGPCLT     IS THIS CLIENT EXCEPTION                     
         BZ    GMG40                                                            
         CLC   MKGPCLT,MYBCLT      THIS CLIENT                                  
         BNE   GMG20                                                            
*                                                                               
GMG40    CLC   KEY(9),KEYSAVE      CMP UP TO MGR ID                             
         BNE   GMG20                                                            
         CLC   =C'ALL',QSTA                                                     
         BE    GMG45                                                            
*                                                                               
         CLC   KEY(9),KEYSAVE                                                   
         BNE   GMG20                                                            
         UNPK  DUB,KEY+9(3)                                                     
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   DUB+3(0),WORK                                                    
         BNE   GMG20                                                            
*                                                                               
GMG45    MVC   0(2,R3),MKGPMKT                                                  
         MVI   2(R3),X'FF'         SET END OF TABLE                             
         LA    R3,2(R3)            BUMP TABLE                                   
         LA    R1,MGRPTABX                                                      
         CR    R3,R1                                                            
         BL    GMG20                                                            
         DC    H'0'                                                             
*                                                                               
GMG50    MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                RESET READ SEQUENCE                          
         GOTO1 GETCLT                                                           
*                                                                               
GMGX     B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        TEST IS THIS STATION/MARKET IS IN THIS GROUP                           
*                                                                               
TSTMGRP  NTR1                                                                   
         CLI   QMGR,C' '           IF REQUESTING MARKET GROUP                   
         BNH   TMGYES                                                           
         L     R8,ADSTAT                                                        
         USING STATRECD,R8                                                      
         PACK  DUB,SMKT                                                         
         CVB   R1,DUB                                                           
         STCM  R1,3,DUB                                                         
         LA    R3,MGRPTAB                                                       
*                                                                               
TMG10    CLI   0(R3),X'FF'                                                      
         BE    TMGNO                                                            
         CLC   0(2,R3),DUB                                                      
         BE    TMGYES                                                           
         LA    R3,2(R3)                                                         
         B     TMG10                                                            
*                                                                               
TMGYES   B     YES                                                              
TMGNO    B     NO                                                               
         EJECT                                                                  
*                                                                               
* EST FOR NETWORK FILTER                                                        
* CALL STAPACK WITH HEADEND/NETWORK AND TEST VALID                              
*                                                                               
TSTNTFLT NTR1                                                                   
         L     R8,ADSTAT                                                        
         USING STATRECD,R8                                                      
*                                                                               
         CLC   QBOOK1,SPACES                                                    
         BNH   YES                                                              
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGY                                                      
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0000'   MARKET                                       
         MVC   STAPQSTA(5),STAKCALL  HEAD-F-END                                 
         MVC   STAPQNET,QBOOK1                                                  
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    YES                                                              
         B     NO                                                               
         DROP R1                                                                
         EJECT                                                                  
*                                                                               
*        HEADHOOK ROUTINE                                                       
*                                                                               
MYHEAD   NTR1                                                                   
         CLI   QOPT2,C'Y'          DEACTIVATED ONLY                             
         BNE   HD10                                                             
         MVC   H3+57(16),=C'DEACTIVATED ONLY'                                   
*                                                                               
HD10     CLI   QMGR,C' '           IF REQUESTING MARKET GROUP                   
         BNH   HEADX                                                            
         CLC   =C'ALL',QSTA                                                     
         BE    HD20                                                             
         LA    R1,H4                                                            
         CLC   SVMGRP1,SPACES                                                   
         BNH   HD13                                                             
         MVC   55(12,R1),SVMGRP1                                                
         LA    R2,67(R1)                                                        
*                                                                               
HD11     CLI   0(R2),C' '                                                       
         BH    HD12                                                             
         BCT   R2,HD11                                                          
*                                                                               
HD12     MVI   1(R2),C':'                                                       
         MVC   69(24,R1),SVMGRPN1                                               
         LA    R1,L'P(R1)                                                       
*                                                                               
HD13     CLC   SVMGRP2,SPACES                                                   
         BNH   HD16                                                             
         MVC   55(12,R1),SVMGRP2                                                
         LA    R2,67(R1)                                                        
*                                                                               
HD14     CLI   0(R2),C' '                                                       
         BH    HD15                                                             
         BCT   R2,HD14                                                          
*                                                                               
HD15     MVI   1(R2),C':'                                                       
         MVC   69(24,R1),SVMGRPN2                                               
         LA    R1,L'P(R1)                                                       
*                                                                               
HD16     CLC   SVMGRP3,SPACES                                                   
         BNH   HD19                                                             
         MVC   55(12,R1),SVMGRP3                                                
         LA    R2,67(R1)                                                        
*                                                                               
HD17     CLI   0(R2),C' '                                                       
         BH    HD19                                                             
         BCT   R2,HD17                                                          
*                                                                               
HD18     MVI   1(R2),C':'                                                       
         MVC   69(24,R1),SVMGRPN2                                               
*                                                                               
HD19     B     HEADX                                                            
*                                                                               
HD20     MVC   H4+62(12),SVMGRP1                                                
         MVC   H5+62(12),SVMGRP2                                                
         MVC   H6+62(12),SVMGRP3                                                
*                                                                               
HEADX    B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
ZEROES   DC    C'0000000'                                                       
*                                                                               
*        SRREC - SORTER RECORD                                                  
*                                                                               
SORT     DC    C'SORT FIELDS=(1,177,BI,A),WORK=1 '                              
RECCARD  DC    C'RECORD TYPE=F,LENGTH=324 '                                     
*                                                                               
         DS    0F                                                               
SRREC    DS    CL324                                                            
SORTSTAT DS    CL1                                                              
*                                                                               
ELCODE   DS    CL1                                                              
*                                                                               
TOTREC   DS    PL6                                                              
CITY     DS    CL24                                                             
STREET   DS    CL24                                                             
ZIP      DS    CL10                                                             
MYMKTNM  DS    CL24                                                             
STATE    DS    CL3                                                              
MYBCLT   DS    XL2                                                              
MYKEY    DS    CL(L'KEY)                                                        
MYSYSNET DS    XL16                                                             
MYSCBL24 DS    XL3                                                              
MYSCBLSQ DS    XL206                                                            
MYMKTSTA DS    XL5                                                              
MSUMKSTA DS    CL5                 MSUNPK'S  MARKET/STA                         
MYCABTAB DS    CL508                                                            
MYCABEND DC    X'00000000'                                                      
COUNT    DS    X                                                                
MYBYTES  DS    XL8                                                              
SVNTWRK  DS    CL3                 REQUESTED NETWORK FILTER                     
LSTCALL  DS    CL5                 LAST CALL LETTERS USED                       
BYTECNTR DS    XL1                                                              
BITCNTR  DS    XL1                                                              
SVMGRP1  DS    CL12                                                             
SVMGRP2  DS    CL12                                                             
SVMGRP3  DS    CL12                                                             
SVMGRP1L DS    CL1                                                              
SVMGRP2L DS    CL1                                                              
SVMGRP3L DS    CL1                                                              
SVMGRPN1 DS    CL24                                                             
SVMGRPN2 DS    CL24                                                             
SVMGRPN3 DS    CL24                                                             
*                                                                               
         DS    0F                                                               
STARTNET DS    A                                                                
*                                                                               
VSORTER  DS    V                                                                
VXSORT   DS    V                                                                
*                                                                               
MGRPTAB  DS    1000XL2                                                          
MGRPTABX DS    X                                                                
         EJECT                                                                  
         LTORG                                                                  
********************************************************************            
         EJECT                                                                  
LMKTD    DSECT                     PRINT DSECT                                  
LMMKT    DS    CL4                 MARKET                                       
         DS    CL1                                                              
LMMKTNM  DS    CL24                MARKET NAME                                  
         DS    CL1                                                              
LMCITY   DS    CL24                CITY                                         
         DS    CL1                                                              
LMSTATE  DS    CL2                 STATE                                        
         DS    CL1                                                              
LMCALL   DS    CL4                 CALL LETTERS                                 
         DS    CL1                                                              
LMCLT    DS    CL3                 CLIENT                                       
         DS    CL1                                                              
LMREP    DS    CL3                 REP                                          
         DS    CL1                                                              
LMSYSCD  DS    CL24                SYS NAME                                     
         DS    CL1                                                              
LMSYSNET DS    CL35                NETWORKS                                     
*                                                                               
LCITYD   DSECT                                                                  
LCCITY   DS    CL24                CITY                                         
         DS    CL2                                                              
LCSTATE  DS    CL3                 STATE                                        
         DS    CL3                                                              
LCCALL   DS    CL4                 CALL LETTERS                                 
         DS    CL3                                                              
LCCLT    DS    CL3                 CLIENT                                       
         DS    CL1                                                              
LCREP    DS    CL3                 REP                                          
         DS    CL1                                                              
LCSYSCD  DS    CL24                SYS NAME                                     
         DS    CL1                                                              
LCSYSNET DS    CL56                NETWORKS                                     
*                                                                               
LSYSD    DSECT                                                                  
LSCALL   DS    CL4                 CALL LETTERS                                 
         DS    CL1                                                              
LSCLT    DS    CL3                 CLIENT                                       
         DS    CL1                                                              
LSREP    DS    CL3                 REP                                          
         DS    CL1                                                              
LSSYSCD  DS    CL24                SYS NAME                                     
         DS    CL1                                                              
LSMKT    DS    XL4                 MARKET                                       
         DS    CL1                                                              
LSMKTNM  DS    CL24                MARKET NAME                                  
         DS    CL1                                                              
LSCITY   DS    CL24                CITY                                         
         DS    CL1                                                              
LSSTATE  DS    CL2                 STATE                                        
         DS    CL2                                                              
LSSYSNET DS    CL34                NETWORKS                                     
         EJECT                                                                  
*                                                                               
SMKTD    DSECT                                                                  
SMMKT    DS    CL4                 MARKET                                       
SMCITY   DS    CL24                CITY                                         
SMSTATE  DS    CL3                 STATE                                        
SMCALL   DS    CL4                 CALL LETTERS                                 
SMCLT    DS    CL3                 CLIENT                                       
SMSYSCD  DS    CL24                CYC NAME                                     
SMSYSNET DS    XL16                NETWORKS                                     
SMREP    DS    CL3                 REP                                          
SMSCBL24 DS    XL3                                                              
SMSCBLSQ DS    CL206                                                            
SMSCADD  DS    CL24                STREET ADDRESS                               
SMSCZIP  DS    CL10                ZIP CODE                                     
SMKTLNQ  EQU   *-SMKTD                                                          
*                                                                               
SCITYD   DSECT                                                                  
SCCITY   DS    CL24                CITY                                         
SCSTATE  DS    CL3                 STATE                                        
SCCALL   DS    CL4                 CALL LETTERS                                 
SCCLT    DS    CL3                 CLIENT                                       
SCSYSCD  DS    CL24                SYS NAME                                     
SCSYSNET DS    XL16                NETWORKS                                     
SCREP    DS    CL3                 REP                                          
SCSCBL24 DS    XL3                                                              
SCSCBLSQ DS    CL206                                                            
SCSCADD  DS    CL24                STREET ADDRESS                               
SCSCZIP  DS    CL10                ZIP CODE                                     
         DS    XL4                                                              
SCITYLNQ EQU   *-SCITYD                                                         
*                                                                               
SSYSD    DSECT                                                                  
SSCALL   DS    CL4                 CALL LETTERS                                 
SSCLT    DS    CL3                 CLIENT                                       
SSSYSCD  DS    CL24                SYS NAME                                     
SSMKT    DS    XL4                 MARKET                                       
SSCITY   DS    CL24                CITY                                         
SSSTATE  DS    CL3                 STATE                                        
SSSYSNET DS    XL16                NETWORKS                                     
SSREP    DS    CL3                 REP                                          
SSSCBL24 DS    XL3                                                              
SSSCBLSQ DS    CL206                                                            
SSSCADD  DS    CL24                STREET ADDRESS                               
SSSCZIP  DS    CL10                ZIP CODE                                     
SSYSLNQ  EQU   *-SSYSD                                                          
         SPACE 2                                                                
*SPREPWORKD                                                                     
*SPREPMODES                                                                     
*SPGENADD                                                                       
*SPGENMKT                                                                       
*SPGENSTA                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
ADDRECD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STATRECD DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENMKG                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'123SPREPC102 08/14/08'                                      
         END                                                                    
