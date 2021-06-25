*          DATA SET PPREPJW04  AT LEVEL 190 AS OF 05/01/02                      
*PHASE PPJW02A,*  ****** NOTE PHASE NAME = PPJW02A *****                        
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE QSORT                                                                  
         SPACE 2                                                                
****************************************************************                
*                                                              *                
*                                                              *                
* TABLES PROVIDED BY JWT PRIOR TO CONVERSION ---               *                
*                                                              *                
*   CPETAB - USED TO CONVERT CLT/PRD/EST CODES                 *                
*                                                              *                
*   INTERNAL TABLES CONSTRUCTED AT RUN TIME ---                *                
*                                                              *                
*   CPELST - LIST OF ALL VALID CLIENTS/PRODUCTS/ESTIMATES      *                
*                                                              *                
****************************************************************                
PPJW02A  TITLE 'JWT ESTIMATE HEADERS CONVERSION PROGRAM'                        
         SPACE 1                                                                
PPJW02A  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPJW02A                                                        
*                                                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LA    R3,2048(RA)                                                      
         LA    R3,2048(R3)                                                      
         USING PPJW02A+4096,RA,R3                                               
*                                                                               
         L     R9,0(R1)                                                         
         USING PPWORKD,R9                                                       
*                                                                               
         L     RC,PPFILEC                                                       
         LR    R8,RC                                                            
         AH    R8,=H'4096'                                                      
         USING PPFILED,RC,R8                                                    
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BE    JW10                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
JW10     DS    0H                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         OPEN  (FILEOUT,(OUTPUT))                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         LOAD  EPLOC=JWCPETAB                                                   
         LR    RE,R0                                                            
         MVC   MYCPEPRM(24),0(RE)                                               
         B     JW11                                                             
JWCPETAB DC    CL8'JWCPETAB'                                                    
*                                                                               
JW11     DS    0H                                                               
         GOTO1 =A(BLDCPE),DMCB,(RC) BUILD CLT/PRD LIST                          
*                                                                               
         CLI   QOPT1,C'Y'          TEST BUILD ALL RECORDS                       
         BNE   JW11A                                                            
         BAS   RE,READRECS         READ AGY/CLT RECS                            
         MVI   MEDFLAG,C'N'        INIT MEDIA FLAG                              
*                                                                               
JW11A    CLC   =C'MAX=',QUESTOR                                                 
         BNE   JW13                                                             
         SR    RE,RE                                                            
         LA    R1,QUESTOR+4                                                     
JW12A    CLI   0(R1),C'0'                                                       
         BL    JW12X                                                            
         CLI   0(R1),C'9'                                                       
         BH    JW12X                                                            
         LA    R1,1(R1)                                                         
         BCT   RE,JW12A                                                         
*                                                                               
JW12X    LPR   RE,RE                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  MAXIN,QUESTOR+4(0)                                               
         B     JW14                                                             
*                                                                               
JW13     CLC   =C'SKIP=',QUESTOR                                                
         BNE   JW14                                                             
         SR    RE,RE                                                            
         LA    R1,QUESTOR+5                                                     
JW13A    CLI   0(R1),C'0'                                                       
         BL    JW13X                                                            
         CLI   0(R1),C'9'                                                       
         BH    JW13X                                                            
         LA    R1,1(R1)                                                         
         BCT   RE,JW13A                                                         
*                                                                               
JW13X    LPR   RE,RE                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  SKIPCNT,QUESTOR+5(0)                                             
         B     JW14                                                             
*                                                                               
JW14     XC    PESTKEY,PESTKEY     CLEAR TOP OF ESTREC                          
*                                                                               
JW15     LA    R7,JWNEXT                                                        
         B     JW20                                                             
*                                                                               
         EJECT                                                                  
SORTCARD DC    CL80'SORT FIELDS=(1,25,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=2000'                                  
*                                                                               
JW20     DS    0H                                                               
         CLI   PESTKEY,0           TEST RECORD IN OUTPUT BUFFER                 
         BE    JW22                NO                                           
*                                                                               
         CLC   PESTKEY,SAVEKEY     SAME ESTIMATE                                
         BE    JW22                                                             
*                                                                               
         CLI   MEDFLAG,C'Y'        WAS MEDIA I                                  
         BNE   JW21C                                                            
         XC    JWBUFF,JWBUFF       CLEAR BUFFER                                 
         MVC   JWBUFF,PESTKEY      SAVE RECORD                                  
         B     JW21C                                                            
*                                                                               
JW21     DS    0H                                                               
         MVI   MEDFLAG,C'X'        RESET FLAG                                   
         MVC   PESTKEY(249),JWBUFF RESTORE REC                                  
         MVI   PESTKMED,C'T'                                                    
         MVI   PESTNAME+1,C'T'                                                  
         B     JW21C                                                            
*                                                                               
JW21B    DS    0H                                                               
         MVI   MEDFLAG,C'Q'        RESET FLAG                                   
         MVC   PESTKEY(249),JWBUFF RESTORE REC                                  
         MVI   PESTKMED,C'S'                                                    
         MVI   PESTNAME+1,C'S'                                                  
         B     JW21C                                                            
*                                                                               
JW21BB   DS    0H                                                               
         MVI   MEDFLAG,C'N'        RESET FLAG                                   
         MVC   PESTKEY(249),JWBUFF RESTORE REC                                  
         MVI   PESTKMED,C'N'                                                    
         MVI   PESTNAME+1,C'N'                                                  
*                                                                               
JW21C    DS    0H                                                               
         LA    R1,PESTREC          POINT TO RECORD                              
         BAS   RE,PUTSORT          AND PUT TO SORT                              
         MVC   SAVEKEY(25),PESTKEY     SAVE CURRENT KEY JUST ENTERED            
         CLI   MEDFLAG,C'Y'        WAS MEDIA I                                  
         BE    JW21                RESTORE OLD RC                               
         CLI   MEDFLAG,C'X'        WAS MEDIA I                                  
         BE    JW21B               RESTORE OLD RC                               
         CLI   MEDFLAG,C'Q'        WAS MEDIA I                                  
         BE    JW21BB              RESTORE OLD RC                               
*                                                                               
JW22     DS    0H                                                               
         LA    R1,PESTREC                                                       
         XC    0(249,R1),0(R1)     CLEAR RECORD                                 
*                                                                               
         BAS   RE,GETFILE          GET NEXT FILE RECORD                         
         CLI   EOFSW,C'Y'                                                       
         BE    JW400                                                            
*                                                                               
         LA    R7,JWNEXT                                                        
         USING JWREC,R7                                                         
         CLI   JWRCD,C'R'          R RECORD TYPE?                               
         BNE   JW22                SKIP IF NOT                                  
         EJECT                                                                  
         SPACE 1                                                                
* CREATE A NEW EST RECORD *                                                     
         SPACE 1                                                                
JW24     DS    0H                                                               
         MVC   PESTKAGY(2),=C'JW'  MOVE AGENCY                                  
         MVI   PESTKRCD,7                                                       
         MVI   PESTLEN+1,249                                                    
         MVC   PESTKMED,JWMEDIA                                                 
         CLI   PESTKMED,C'I'                                                    
         BNE   JW24AA                                                           
         MVI   PESTKMED,C'M'                                                    
         MVI   PESTNAME+1,C'M'                                                  
         MVI   MEDFLAG,C'Y'                                                     
*                                                                               
JW24AA   DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),JWCLT                                                    
         MVC   MYCPEPR1(24),MYCPEPRM                                            
         MVI   CPEKLEN+3,4         SET KEY LENGTH                               
         GOTO1 BINSRCH,MYCPEPR1,WORK                                            
         CLI   0(R1),X'01'                                                      
         BNE   JW24A                                                            
         BAS   RE,CLTERR                                                        
         XC    PESTKEY,PESTKEY                                                  
         B     JW20                                                             
*                                                                               
JW24A    DS    0H                                                               
         MVC   WORK+4(3),JWPRD                                                  
         GOTO1 BINSRCH,MYCPEPRM,WORK                                            
         CLI   0(R1),X'01'                                                      
         BNE   JW26                                                             
         BAS   RE,PRDERR                                                        
         XC    PESTKEY,PESTKEY                                                  
         B     JW20                                                             
*                                                                               
JW26     DS    0H                                                               
         L     RE,0(R1)            POINT TO CLT/PRD ENTRY                       
         USING CPETABD,RE                                                       
         MVC   PESTKCLT,CPEDDCLT   MOVE NEW CLT                                 
         MVC   PESTKPRD,CPEDDPRD            PRD                                 
         XR    R0,R0                                                            
*                                                                               
JW27     DS    0H                                                               
         CLC   =C'1575',JWCLT                                                   
         BNE   JW28                                                             
         CLC   =C'000',JWPRD                                                    
         BNE   JW28                                                             
         CLI   JWEST,C'8'                                                       
         BNE   JW28                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),JWEST+3                                                  
         XR    R2,R2                                                            
         ZIC   R2,WORK             HIGH DIGIT                                   
         LA    R2,1(R2)            INC R2                                       
         STC   R2,WORK             REPLACE HIGH DIGIT                           
         PACK  DUB,WORK(3)                                                      
         B     JW40                                                             
*                                                                               
JW28     DS    0H                                                               
         CLC   JWEST+4(2),=C'  '   INPUT 07060087105                            
         BNE   JW29                                                             
         BAS   RE,DATAERR          COUNT BAD RECORDS                            
         XC    PESTKEY,PESTKEY                                                  
         B     JW20                                                             
         EJECT                                                                  
*                                                                               
JW29     ZAP   COUNT,=P'0'                                                      
         LA    RE,ETAB6                                                         
         ZAP   INDEX,=P'600'                                                    
         CLI   JWEST,C'6'                                                       
         BE    JW30                                                             
*                                                                               
         LA    RE,ETAB7                                                         
         ZAP   INDEX,=P'700'                                                    
         CLI   JWEST,C'7'                                                       
         BE    JW30                                                             
*                                                                               
         LA    RE,ETAB8                                                         
         ZAP   INDEX,=P'800'                                                    
         CLI   JWEST,C'8'                                                       
         BE    JW30                                                             
*                                                                               
         LA    RE,ETAB9                                                         
         ZAP   INDEX,=P'900'                                                    
         CLI   JWEST,C'9'                                                       
         BE    JW30                                                             
*                                                                               
         BAS   RE,DATAERR                                                       
         XC    PESTKEY,PESTKEY                                                  
         B     JW20                                                             
*                                                                               
JW30     DS    0H                                                               
         CLI   0(RE),0             END OF TABLE?                                
         BE    JW31                                                             
         CLC   0(4,RE),JWEST+2     SAME JWT EST NUM?                            
         BE    JW33                                                             
         LA    RE,6(RE)            BUMP TABLE                                   
         AP    COUNT,=P'1'                                                      
         B     JW30                                                             
*                                                                               
JW31     DS    0H                                                               
         MVC   0(6,RE),JWEST                                                    
*                                                                               
JW33     DS    0H                                                               
         ZAP   DUB,=P'99'                                                       
         SP    DUB,COUNT                                                        
         AP    DUB,INDEX                                                        
JW40     DS    0H                                                               
         CVB   R0,DUB                                                           
         STCM  R0,3,PESTKEST                                                    
         DROP  RE                                                               
*                                                                               
         OC    CPELSCNT,CPELSCNT   TEST LIST BUILT                              
         BZ    JW50                NO - SKIP                                    
         XC    WORK,WORK                                                        
         MVC   WORK(1),PESTKMED                                                 
         MVC   WORK+1(3),PESTKCLT                                               
         MVC   WORK+4(3),PESTKPRD                                               
         GOTO1 BINSRCH,CPELSPRM,WORK                                            
         CLI   0(R1),X'01'                                                      
         BNE   JW50                                                             
         BAS   RE,NOCLTERR                                                      
         XC    PESTKEY,PESTKEY     SUPPRESS RECORD                              
         B     JW20                                                             
         EJECT                                                                  
*                                                                               
JW50     DS    0H                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         MVI   PESTELEM,7          ELEM CODE                                    
         MVI   PESTELEM+1,X'D8'    LEN                                          
         MVC   PESTNAME(6),JWEST   1ST 6 CHARS                                  
         CLC   JWEST+2(4),=C'9999'                                              
         BNE   *+8                                                              
         MVI   PESTTEST,X'80'      PER SUSIE MOUNTBATTEN                        
         MVI   PESTPROF,C'0'                                                    
         MVC   PESTPROF+1(31),PESTPROF                                          
         CLI   MEDFLAG,C'Y'                                                     
         BNE   JW60                                                             
         MVI   PESTNAME+1,C'M'                                                  
*                                                                               
JW60     DS    0H                                                               
         MVI   PESTNAME+6,C' '     BLANK                                        
         GOTO1 =V(CHOPPER),DMCB,(57,JWDES),(13,WORK),1                          
         MVC   PESTNAME+7(13),WORK                                              
         GOTO1 =V(CHOPPER),DMCB,(44,JWDES+13),(20,WORK),1                       
         MVC   PESTNAM2,WORK                                                    
*                                                                               
         MVC   PESTST,JWOPEN       START DATE                                   
         MVC   PESTEND,JWCLOSE     END DATE                                     
         B     JW20                                                             
         EJECT                                                                  
* RETRIEVE SORT OUTPUT AND WRITE OUTPUT FILE *                                  
         SPACE 1                                                                
JW400    DS    0H                                                               
         XC    KEY,KEY                                                          
*                                                                               
JW402    DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R4,15,4(R1)         R4 POINTS TO 4 BYTE RECLEN                   
         BZ    JW410                                                            
         CLC   KEY(25),4(R4)                                                    
         BNE   JW404                                                            
         AP    DUPREC,=P'1'                                                     
         B     JW402                                                            
*                                                                               
JW404    DS    0H                                                               
         MVC   KEY(25),4(R4)       SAVE CURRENT KEY                             
         LR    R0,R4                                                            
         PUT   FILEOUT,(R0)                                                     
         B     JW402                                                            
*                                                                               
JW410    CLOSE FILEOUT                                                          
*                                                                               
         LA    R4,COUNTS                                                        
         LA    R5,NCOUNTS                                                       
*                                                                               
JW420    OI    3(R4),X'0F'                                                      
         UNPK  P(7),0(4,R4)                                                     
         MVC   P+9(20),4(R4)                                                    
         GOTO1 REPORT                                                           
         LA    R4,L'COUNTS(R4)                                                  
         BCT   R5,JW420                                                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
DUB2     DS    D                                                                
JWBUFF   DS    CL249                                                            
         DS    0F                  SET ALIGNMENT                                
WORK1    DS    CL12                                                             
MEDFLAG  DS    CL1                                                              
         DS    0D                                                               
         DC    CL8'*TABLES*'                                                    
ETAB6    DS    100CL6              TABLES TO BUILD EST NUMS                     
ETAB7    DS    100CL6                                                           
ETAB8    DS    100CL6                                                           
ETAB9    DS    100CL6                                                           
         DS    0D                                                               
INDEX    DC    PL4'0'                                                           
COUNT    DC    PL4'0'                                                           
TABCLT   DS    CL7                                                              
         SPACE 1                                                                
* ON ENTRY R1 POINTS TO RECORD *                                                
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         SR    R0,R0                                                            
         ICM   R0,3,(25)(R1)       ALL REC KEYS ALIKE                           
         AH    R0,=H'4'                                                         
         SLL   R0,16                                                            
         SH    R1,=H'4'            BACK UP TO RECLEN                            
         ST    R0,0(R1)            STORE IT IN FRONT OF RECORD                  
         LR    R0,R1               POINT R0 TO RECORD                           
         GOTO1 SORTER,DMCB,=C'PUT',(R0)                                         
         AP    OUTCOUNT,=P'1'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
GETFILE  NTR1                                                                   
         ZAP   DUB,SKIPCNT         SET SKIP COUNT                               
         CP    INCNT,MAXIN                                                      
         BH    ENDIN                                                            
*                                                                               
GETFILE1 AP    INCNT,=P'1'                                                      
*                                                                               
GETFILE2 GET   FILEIN,JWNEXT                                                    
         CLC   TABCLT(7),JWNEXT+1  SAME CLT PRD CODE                            
         BE    GETFILE3                                                         
         MVC   TABCLT(7),JWNEXT+1                                               
         ZAP   COUNT,=P'0'         CLEAR OUT COUNT                              
         LR    R1,RE               SAVE RE                                      
         XCEF  ETAB6,2400          CLEAR 3 TABLES                               
         LR    RE,R1               RESTORE RE                                   
*                                                                               
GETFILE3 DS    0H                                                               
         CLI   QOPT2,C' '          TEST PROCESS ALL MEDIA                       
         BE    GETFILEX                                                         
         CLI   QOPT2,C'X'          TEST SUPPRESS PUBLIST                        
         BE    GETFILEX                                                         
         CLC   QOPT2,JWMEDIA       ELSE MATCH MEDIA CODE                        
         BNE   GETFILE2                                                         
*                                                                               
GETFILEX CP    DUB,=P'0'           TEST SKIPPED ENOUGH YET                      
         BE    EXIT                                                             
         SP    DUB,=P'1'           NO- DECR COUNT AND READ NEXT                 
         B     GETFILE1                                                         
ENDIN    CLOSE FILEIN                                                           
         MVI   JWNEXT,X'FF'                                                     
         MVC   JWNEXT+1(199),JWNEXT                                             
         MVI   EOFSW,C'Y'                                                       
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 1                                                                
READRECS NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'JW'       AGENCY CODE                                  
         MVI   KEY+2,C'M'          MEDIA CODE                                   
         MVI   KEY+3,1             RECORD CODE/AGENCY REC                       
*                                                                               
RR5      DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      MUST EXIST                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETAGY              GET SINGLE AGENCY RECORD                     
         LA    R1,PAGYKEY                                                       
         BAS   RE,PUTSORT                                                       
*                                                                               
RR10     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),KEYSAVE                                                   
         CLI   KEY+2,C'M'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'N'                                                       
         B     RR5                                                              
         CLI   KEY+2,C'N'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'O'                                                       
         B     RR5                                                              
         CLI   KEY+2,C'O'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'S'                                                       
         B     RR5                                                              
         CLI   KEY+2,C'S'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'T'                                                       
         B     RR5                                                              
*                                                                               
RR15     DS    0H                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVC   KEY(2),=C'JW'       AGENCY CODE                                  
         MVI   KEY+2,C'M'          MEDIA CODE                                   
         MVI   KEY+3,2             RECORD CODE/CLIENT REC                       
RR17     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     RR30                                                             
*                                                                               
RR20     DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
RR30     DS    0H                                                               
         GOTO1 GETCLI                                                           
         CLC   KEY(4),KEYSAVE      SAME RECORD TYPE                             
         BNE   RR35                GOT ALL RECS ALREADY                         
         LA    R1,PCLTKEY                                                       
         MVC   PCLTPROF+12(1),=C'N'    NO CONTRACT                              
         BAS   RE,PUTSORT                                                       
         B     RR20                                                             
*                                                                               
RR35     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),KEYSAVE                                                   
         CLI   KEY+2,C'M'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'N'                                                       
         B     RR17                                                             
         CLI   KEY+2,C'N'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'O'                                                       
         B     RR17                                                             
         CLI   KEY+2,C'O'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'S'                                                       
         B     RR17                                                             
         CLI   KEY+2,C'S'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'T'                                                       
         B     RR17                                                             
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVC   KEY(2),=C'JW'       AGENCY CODE                                  
         MVI   KEY+2,C'M'          MEDIA CODE                                   
         MVI   KEY+3,6             RECORD CODE/PROD REC                         
*                                                                               
RR37     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     RR50                                                             
*                                                                               
RR40     DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
RR50     DS    0H                                                               
         GOTO1 GETPROD                                                          
         CLC   KEY(4),KEYSAVE      SAME RECORD TYPE                             
         BNE   RR55                GOT ALL RECS ALREADY                         
         LA    R1,PPRDKEY                                                       
         BAS   RE,PUTSORT                                                       
         B     RR40                                                             
*                                                                               
RR55     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),KEYSAVE                                                   
         CLI   KEY+2,C'M'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'N'                                                       
         B     RR37                                                             
         CLI   KEY+2,C'N'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'O'                                                       
         B     RR37                                                             
         CLI   KEY+2,C'O'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'S'                                                       
         B     RR37                                                             
         CLI   KEY+2,C'S'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'T'                                                       
         B     RR37                                                             
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVC   KEY(2),=C'JW'       AGENCY CODE                                  
         MVI   KEY+2,C'M'          MEDIA CODE                                   
         MVI   KEY+3,7             RECORD CODE/EST REC                          
*                                                                               
RR57     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     RR70                                                             
*                                                                               
RR60     DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
RR70     DS    0H                                                               
         GOTO1 GETEST                                                           
         CLC   KEY(4),KEYSAVE      SAME RECORD TYPE                             
         BNE   RR75                GOT ALL RECS ALREADY                         
         LA    R1,PESTKEY                                                       
         BAS   RE,PUTSORT                                                       
         B     RR60                                                             
*                                                                               
RR75     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),KEYSAVE                                                   
         CLI   KEY+2,C'M'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'N'                                                       
         B     RR57                                                             
         CLI   KEY+2,C'N'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'O'                                                       
         B     RR57                                                             
         CLI   KEY+2,C'O'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'S'                                                       
         B     RR57                                                             
         CLI   KEY+2,C'S'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'T'                                                       
         B     RR57                                                             
*                                                                               
RR100    B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
CLTERR   L     R1,=A(MSG1)                                                      
         AP    CLTERRS,=P'1'                                                    
         CLC   SRCHCLT(4),WORK     SAME CLT                                     
         BER   RE                  YES - IGNORE                                 
         MVC   SRCHCLT(4),WORK     ELSE SAVE IT                                 
         B     CONERR              AND PRINT ERROR ONCE                         
*                                                                               
NOCLTERR L     R1,=A(MSG5)                                                      
         AP    CLTERRS,=P'1'                                                    
         CLC   SRCHCLT(4),WORK     SAME CLT                                     
         BER   RE                  YES - IGNORE                                 
         MVC   SRCHCLT(4),WORK     ELSE SAVE IT                                 
         MVC   P+83(6),=C'CLT = '  DDS CLIENT CODE                              
         MVC   P+90(3),PESTKCLT    DDS CLIENT CODE                              
         MVC   P+99(6),=C'PRD = '                                               
         MVC   P+106(3),PESTKPRD   DDS PRODUCT CODE                             
         B     CONERR              AND PRINT ERROR ONCE                         
*                                                                               
PRDERR   L     R1,=A(MSG2)                                                      
         AP    PRDERRS,=P'1'                                                    
         CLC   SRCHCLT(7),WORK     SAME CLT/PRD                                 
         BER   RE                  YES - IGNORE                                 
         MVC   SRCHCLT(7),WORK     ELSE SAVE IT                                 
         B     CONERR              AND PRINT ERROR ONCE                         
*                                                                               
DATAERR  L     R1,=A(MSG6)                                                      
         AP    DATAERRS,=P'1'                                                   
         B     CONERR              AND PRINT ERROR ONCE                         
*                                                                               
CONERR   NTR1                                                                   
         MVC   P(11),=C'** ERROR **'                                            
         MVC   P+12(40),0(R1)                                                   
         MVC   P+60(6),=C'KEY = '                                               
         MVC   P+66(13),JWKEY                                                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 1                                                                
*====================================*                                          
* BUILD LIST OF ALL CLT/PRD HDRS *                                              
*====================================*                                          
         SPACE 1                                                                
BLDCPE   NMOD1 0,*BLDCPE*                                                       
         L     RC,0(R1)            RESTORE REG POINTER                          
*                                                                               
         L     R4,=A(CPELST)                                                    
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'JW'       SET AGENCY CODE                              
         MVI   KEY+2,C'M'          SET FOR MAGAZINES                            
         MVI   KEY+3,6             SET RECORD CODE                              
*                                                                               
BLDCPE1  GOTO1 HIGH                                                             
         B     BLDCPE4                                                          
*                                                                               
BLDCPE2  DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
BLDCPE4  DS    0H                                                               
         CLC   KEY(4),KEYSAVE      SAME AG/M/CODE                               
         BNE   BLDCPE10                                                         
*                                                                               
BLDCPE8  DS    0H                                                               
         C     R4,=A(CPELSTX)                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   0(1,R4),KEY+2       MOVE MEDIA                                   
         MVC   1(6,R4),KEY+4       MOVE CLT/PRD                                 
         LA    R4,7(R4)                                                         
         BCT   R5,BLDCPE2                                                       
*                                                                               
BLDCPE10 XC    KEY,KEY                                                          
         MVC   KEY(4),KEYSAVE      RESTORE PREV AG/M/CODE                       
         CLI   KEY+2,C'M'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'N'                                                       
         B     BLDCPE1                                                          
         CLI   KEY+2,C'N'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'O'                                                       
         B     BLDCPE1                                                          
         CLI   KEY+2,C'O'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'S'                                                       
         B     BLDCPE1                                                          
         CLI   KEY+2,C'S'                                                       
         BNE   *+12                                                             
         MVI   KEY+2,C'T'                                                       
         B     BLDCPE1                                                          
         EJECT                                                                  
*                                                                               
BLDCPEX  LPR   R5,R5                                                            
         L     RE,=A(CPELSCNT)                                                  
         ST    R5,0(RE)            SET NUMBER OF ENTRIES IN PARMS               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=FB,MACRF=GM,EODAD=ENDIN,   X        
               BLKSIZE=3000,LRECL=300                                           
         SPACE 1                                                                
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               BLKSIZE=32760,LRECL=4004                                         
*                                                                               
         DS    0D                                                               
COUNTS   DS    0CL24                                                            
INCNT    DC    PL4'0',CL20'INPUT RECORDS'                                       
OUTCOUNT DC    PL4'0',CL20'TOTAL RECORDS OUT'                                   
CLTERRS  DC    PL4'0',CL20'MISSING CLIENTS'                                     
PRDERRS  DC    PL4'0',CL20'MISSING PRODUCTS'                                    
DATAERRS DC    PL4'0',CL20'INCORRECT DATA'                                      
DUPREC   DC    PL4'0',CL20'DUPLICATE KEYS'                                      
NCOUNTS  EQU   ((*-COUNTS)/24)                                                  
*                                                                               
MAXIN    DC    PL4'9999999'                                                     
SKIPCNT  DC    PL4'0'                                                           
EOFSW    DC    X'00'                                                            
SRCHCLT  DS    XL4                                                              
SRCHPRD  DS    XL7                                                              
SAVEKEY  DS    CL25                                                             
PRECLEN  DS    CL2                                                              
         DS    0F                                                               
COUNTER  DC    PL4'0100'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*JWNEXT*'                                                    
JWNEXT   DS    CL300                                                            
*                                                                               
MYCPEPRM DC    6A(0)              PARAMS FOR CLIENT CONV CODE SEARCH            
MYCPEPR1 DC    4A(0)                                                            
CPEKLEN  DC    2A(0)                                                            
*                                                                               
CPELSPRM DC    A(0)                PARAMS FOR CLT/PRD LIST SEARCH               
         DC    A(CPELST)                                                        
CPELSCNT DC    F'0'                                                             
         DC    A(7)                                                             
         DC    AL1(0),AL3(7)       MED(1)/CLT(3)/PRD(3)                         
         DC    A((CPELSTX-CPELST)/7)                                            
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
         SPACE 1                                                                
MSG1     DC    CL40'CLIENT NOT IN CONV TABLE'                                   
MSG2     DC    CL40'PRODUCT NOT IN CONV TABLE'                                  
MSG3     DC    CL40'ESTIMATE NOT IN CONV TABLE'                                 
MSG4     DC    CL40'PUB NOT ON FILE'                                            
MSG5     DC    CL40'CLT/PRD NOT ON FILE'                                        
MSG6     DC    CL40'INCORRECT INPUT DATA'                                       
         SPACE 2                                                                
JWREC    DSECT                                                                  
         DS    CL1                                                              
JWKEY    DS    0CL23                                                            
JWCLT    DS    CL4                                                              
JWPRD    DS    CL3                                                              
JWEST    DS    CL6                                                              
JWRCD    DS    CL1                                                              
JWFIL    DS    CL6                                                              
JWDES    DS    CL57                                                             
JWMEDIA  DS    CL1                                                              
         ORG   JWREC+177                                                        
JWOPEN   DS    CL6                                                              
JWCLOSE  DS    CL6                                                              
         ORG   JWREC+300                                                        
         SPACE 2                                                                
PPJW02A  CSECT                                                                  
         SPACE 1                                                                
*====================================*                                          
* LIST OF CLT/PRD  CODES ON DDS FILE *                                          
* CONSTRUCTED AT RUN TIME            *                                          
*====================================*                                          
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'*CPELST*'                                                    
CPELST   DS    9000XL7           MED(1)/CLT(3)/PRD(3)                           
CPELSTX  EQU   *                                                                
         SPACE 2                                                                
*=======================================*                                       
* TABLE OF CLIENT CONVERSION CODES      *                                       
*=======================================*                                       
         SPACE 1                                                                
CPETABD  DSECT        * DSECT FOR CLT/PRD/EST CONVERSION TABLE *                
CPEJWCLT DS    CL4                                                              
CPEJWPRD DS    CL3                                                              
CPEDDCLT DS    CL3                                                              
CPEDDPRD DS    CL3                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'190PPREPJW04 05/01/02'                                      
         END                                                                    
