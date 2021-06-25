*          DATA SET CT02A      AT LEVEL 002 AS OF 05/01/02                      
*PHASE SPCT02A                                                                  
*INCLUDE SORTER                                                                 
**********************************************************************          
*                                                                    *          
* LEV 1     DEC10/96 COPY COMMERCIAL RECORD COKEAT CC TO LIPA/APNY CC*          
*                                                                    *          
*                                                                    *          
**********************************************************************          
         TITLE 'SPCT02 - COKE TRAFFIC COMMERCIAL RECORD TRANSFER'               
SPCT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPCT02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         LA    R8,2048(RC)                                                      
         LA    R8,2048(R8)                                                      
         USING SPCT02,RB,RC,R8                                                  
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    CT10                                                             
*****    CLI   MODE,CLTFRST                                                     
******   BE    CT20                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*===============================================================*               
* REQFRST PROCESSING                                            *               
*===============================================================*               
                                                                                
CT10     DS    0H                                                               
         MVC   SVAGYA,AGENCY                                                    
         L     RE,UTL                                                           
         MVC   SVSTRSEA,4(RE)         SAVE AGENCY STRAFFIC SE NUMBER            
********************************************                                    
*  TEMPORARY CODE                                                               
         SPACE                                                                  
         MVC   SVSTRSEC,=X'46'     SAVE SE STR NUMBER FOR CKOKEAT               
         SPACE                                                                  
         CLC   =C'LI',SVAGYA       IF AGENCY LIPA                               
         BNE   *+14                                                             
         MVC   SVSTRSEA,=X'43'     THEN SAVE STR3 SE NUMBER                     
         B     CT10C                                                            
         MVC   SVSTRSEA,=X'46'     ELSE SAVE STR4 SE NUMBER FOR APNY            
         CLC   =C'AP',SVAGYA       IF AGENCY IS NOT APNY                        
         BE    *+6                                                              
         DC    H'0'                THEN DIE                                     
********************************************                                    
         SPACE                                                                  
CT10C    XC    CNDATA,CNDATA                                                    
         LA    R5,CNDATA                                                        
         USING CND,R5                                                           
         MVC   CNAGY(2),=C'CK'     MOVE COKE'S ALPHA AGENCY CODE                
                                                                                
         GOTO1 CONFID,DMCB,(R5),(1,FULL)                                        
         OC    FULL,FULL           TEST ID FOUND                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,UTL                                                           
         MVC   4(1,RE),CNSSE       MOVE SPOT SYSTEM NUMBER                      
         DROP  R5                                                               
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',FLISTS,ADBUY                    
         SPACE                                                                  
         GOTO1 MEDGET,DMCB,(QMED,=C'CK'),DATAMGR,WORK                           
                                                                                
         MVC   SVCCAGMD,WORK                                                    
         SPACE                                                                  
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVCCAGMD                                                
         MVC   KEY+2(2),BCLT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,CCCC                                                          
         ST    R1,AREC                                                          
         GOTO1 GET                                                              
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'SPOT'                                 
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         SPACE                                                                  
* READ COKE'S COMMERCIAL RECORDS AND PUT TO SORTER                              
         SPACE                                                                  
         L     RE,UTL                                                           
         MVC   4(1,RE),SVSTRSEC    RESTORE COKE STRAFFIC SE NUMBER              
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'STR',FLISTT,ADBUY                     
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(1),SVCCAGMD                                                
         MVC   KEY+3(2),=X'885F'   CLIENT CC                                    
         MVC   FILE,=C'TRFDIR'                                                  
         GOTO1 HIGH                                                             
         SPACE                                                                  
CT12     CLC   KEY(5),KEYSAVE                                                   
         BNE   CT15                                                             
         SPACE                                                                  
         OC    KEY+5(8),KEY+5      ANY CML ?                                    
         BZ    CT14                BYPASS RECS WITHOUT CMLS                     
         SPACE                                                                  
         LA    R1,REC                                                           
         ST    R1,AREC                                                          
         MVC   FILE,=C'TRFFIL'                                                  
         GOTO1 GET                                                              
         SPACE                                                                  
         LA    RE,REC                                                           
         ZICM  RF,13(RE),2         GET RECORD LENGTH                            
         LA    RF,4(RF)            2 BYTES OF LEN + 2 BYTES OF NULLS            
         STCM  RF,3,REC1           PUT LENGTH OF RECORD FOR SORT                
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,=C'PUT',REC1                                     
         SPACE                                                                  
CT14     GOTO1 SEQ                                                              
         B     CT12                                                             
         SPACE                                                                  
CT15     DS    0H                                                               
         XC    FILE,FILE                                                        
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'STR'                                  
         SPACE                                                                  
         XC    CNDATA,CNDATA       RE-OPEN ORIGINAL SPOT FILE                   
         LA    R5,CNDATA                                                        
         SPACE                                                                  
         USING CND,R5                                                           
         MVC   CNAGY(2),SVAGYA     MOVE SAVED ALPHA AGENCY CODE                 
         SPACE                                                                  
         GOTO1 CONFID,DMCB,(R5),(1,FULL)                                        
         OC    FULL,FULL           TEST ID FOUND                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,UTL                                                           
         MVC   4(1,RE),CNSSE       MOVE SPOT SYSTEM NUMBER                      
         DROP  R5                                                               
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',FLISTS,ADBUY                    
         SPACE                                                                  
         XC    KEY,KEY             GET CLT CC RECORD FOR AGENCY                 
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,CCAGY                                                         
         ST    R1,AREC                                                          
         GOTO1 GET                                                              
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'SPOT'                                 
         SPACE                                                                  
         BAS   RE,CTBLDPRD         BUILD PRODUCT TRANSLATE TABLE                
         SPACE                                                                  
         B     CT20                                                             
******   B     EXIT                                                             
         EJECT                                                                  
                                                                                
*        READ RECOVERY FILE - IF A TRAFFIC COMMERCIAL RECORD                    
*        WAS ADDED FOR CLT CC - ADD IT TO THIS AGENCY'S FILE                    
*        AFTER TRANSLATING THE PRODUCT CODE                                     
                                                                                
CT20     DS    0H                                                               
         SPACE                                                                  
         L     RE,UTL                                                           
         MVC   4(1,RE),SVSTRSEA    RESTORE AGENCY STRAFFIC SE NUMBER            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'STR',FLISTT,ADBUY                     
         SPACE                                                                  
CT21     GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         SPACE                                                                  
         ICM   R4,15,4(R1)         ADDRESS OF CML RECORD (COKEAT)               
         BZ    CT25                DONE                                         
         SPACE                                                                  
* READ AGENCY CML RECORD                                                        
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(13),4(R4)                                                    
         MVC   KEY+2(1),BAGYMD                                                  
         SPACE                                                                  
         MVC   FILE,=C'TRFDIR'                                                  
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    CT22                                                             
         CLC   KEY(5),KEYSAVE      SAME AGY/MED/CLT                             
         BNE   CT25                                                             
         SPACE                                                                  
CT22     LA    R2,4(R4)            POINT TO THE ACTUAL DATA                     
         ST    R2,AREC             SAVE ADDRESS OF THE RECORD                   
         SPACE                                                                  
CT23     AP    INCNT,=P'1'                                                      
         AP    RDCNT,=P'1'         ADD TO RDDCNT                                
         SPACE                                                                  
         CLI   QOPT2,C'Y'          TRACE RECORDS                                
         BNE   CT30                                                             
         GOTO1 MYTRACE,DMCB,0(R2),0,=C'*BEFORE*',8                              
         SPACE                                                                  
CT30     BAS   RE,CTCHGPRD         TRANSLATE PRD CODES                          
         BE    CT21                PRODS MATCH, GET NEXT RECORD                 
         SPACE                                                                  
         CLC   KEY(13),KEYSAVE     WAS RECORD FOUND ?                           
         BNE   *+14                                                             
         AP    CHGCNT,=P'1'         YES, ADD TO CHGCNT                          
         B     *+10                                                             
         AP    ADDCNT,=P'1'        ADD TO ADDCNT                                
         SPACE                                                                  
         CLI   QOPT1,C'Y'          TEST RUN                                     
         BNE   CT40                                                             
         CP    ADDCNT,=P'10'                                                    
         BH    CT21                                                             
         SPACE                                                                  
CT40     CLI   QOPT2,C'Y'          TRACE RECORDS                                
         BNE   CT50                                                             
         SPACE                                                                  
         GOTO1 MYTRACE,DMCB,0(R2),0,=C'*TO*',4                                  
         SPACE                                                                  
         CLC   KEY(13),KEYSAVE     WAS RECORD FOUND                             
         BNE   CT60                 GO ADD NEW RECORD                           
         SPACE                                                                  
CT50     DS    0H                                                               
         LA    R2,REC                                                           
         ST    R2,AREC                                                          
         GOTO1 PUT                                                              
         BAS   RE,PRINTIT                                                       
         B     CT21                GET NEXT                                     
         SPACE                                                                  
CT60     DS    0H                                                               
         GOTO1 ADD                                                              
         BAS   RE,PRINTIT                                                       
         B     CT21                GET NEXT                                     
         SPACE                                                                  
CT25     DS    0H             *                                                 
         XC    FILE,FILE                                                        
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         EJECT                                                                  
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,CTRS                                                          
                                                                                
CT110    DS    0H                                                               
         MVC   P(16),4(R4)                                                      
         EDIT  (P4,0(R4)),(8,P+22)                                              
         GOTO1 REPORT                                                           
         LA    R4,L'CTRS(R4)                                                    
         LA    R0,CTRX                                                          
         CR    R4,R0                                                            
         BL    CT110                                                            
         GOTO1 AENDREQ                                                          
         B     EXIT                                                             
         EJECT                                                                  
*========================================================*                      
* SET UP EQUIVALENCE TABLE BETWEEN CC/CC AND AGY/CK      *                      
*========================================================*                      
         SPACE 1                                                                
CTBLDPRD NTR1                                                                   
         LA    R4,CCCC             POINT TO COKE CC CLTHDR                      
         LA    R4,CLIST-CLTHDRD(R4)                                             
         LA    RF,PRDTAB1                                                       
         XC    0(256,RF),0(RF)                                                  
*                                                                               
BLDPRD2  LA    R5,CCAGY            POINT TO AGY CC CLTHDR                       
         LA    R5,CLIST-CLTHDRD(R5)                                             
*                                                                               
BLDPRD4  CLC   0(3,R4),0(R5)       MATCH ALPHA PRD CODE                         
         BE    BLDPRD6                                                          
         LA    R5,4(R5)                                                         
         CLI   0(R5),C'A'                                                       
         BNL   BLDPRD4                                                          
         B     BLDPRD10            IGNORE PRD NOT FOUND                         
*                                                                               
BLDPRD6  ZIC   RE,3(R4)            GET PRD NUM                                  
         AR    RE,RF               POINT TO PROPER SLOT                         
         MVC   0(1,RE),3(R5)       MOVE CC PRD CODE TO SLOT                     
*                                                                               
BLDPRD10 LA    R4,4(R4)                                                         
         CLI   0(R4),C'A'                                                       
         BNL   BLDPRD2                                                          
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
*========================================================*                      
* TRANSLATE PRODUCT CODES                                *                      
* R2 - START OF TRAFFIC COMMERCIAL RECORD                *                      
*========================================================*                      
         SPACE 1                                                                
CTCHGPRD NTR1                                                                   
         MVI   PRDCHGSW,0          RESET PROD CHANGED SWITCH                    
         LA    R1,PRDTAB1                                                       
         SPACE                                                                  
         LR    R6,R2                                                            
         MVI   ELCODE,X'20'        PRODUCT LIST ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
CP20     DS    0H                                                               
         ZIC   R2,1(R6)                                                         
         SH    R2,=H'2'            DECREMENT ELEMENT CODE & LENGTH              
         LA    R3,2(R6)            POINT TO 1ST PRODUCT                         
         SPACE                                                                  
CP30     CLI   0(R3),X'FF'         ALL PRODUCTS                                 
         BE    CPEQX                LEAVE IT ALONE                              
         ZIC   R4,0(R3)                                                         
         AR    R4,R1               ADD TO DISPL A(START OF TABLE)               
         CLI   0(R4),0             NO PRD CODE                                  
         BE    CPNEQX              NO MATCH                                     
         CLC   0(1,R3),0(R4)       SAME PRODS?                                  
         BE    *+14                 YES, CHK NEXT PROD                          
         MVC   0(1,R3),0(R4)       INSERT AGY'S PRD CODE                        
         MVI   PRDCHGSW,1          PROD CHANGED SWITCH                          
         LA    R3,1(R3)            BUMP TO NEXT PRD                             
         BCT   R2,CP30                                                          
         SPACE                                                                  
         CLI   PRDCHGSW,1          ANY PRODS CHANGED?                           
         BNE   CPEQX                NO                                          
         SPACE                                                                  
CPNEQX   DS    0H                                                               
         L     R2,AREC             GET ADDRESS OF THE RECORD                    
         MVC   2(1,R2),BAGYMD      MOVE NEW AGY/MED                             
         SPACE                                                                  
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     CPX                                                              
         SPACE                                                                  
CPEQX    CLC   KEY(13),KEYSAVE     WAS AGY CML REC FOUND                        
         BNE   CPNEQX               NO                                          
         SPACE                                                                  
         CR    RB,RB               SET CC EQUAL                                 
CPX      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        PRINT OUT INFO LINE                                                    
*                                                                               
PRINTIT  NTR1                                                                   
         L     R2,AREC                                                          
         USING CMLRECD,R2                                                       
         MVC   CMLCML,CMLKCML      COMMERCIAL ID                                
         LA    R4,CMLPRD                                                        
                                                                                
         LR    R6,R2                                                            
         MVI   ELCODE,X'20'        PRODUCT LIST ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
PR20     ZIC   R2,1(R6)                                                         
         SH    R2,=H'2'            DECREMENT ELEMENT CODE & LENGTH              
         LA    R3,2(R6)            POINT TO 1ST PRODUCT                         
                                                                                
PR30     CLI   0(R3),X'FF'         ALL PRODUCTS                                 
         BE    PR60                                                             
         LA    RE,CCAGY                                                         
         LA    RE,CLIST-CLTHDRD(RE)                                             
*                                                                               
PR40     CLC   0(1,R3),3(RE)       MATCH PRD NUMBERS                            
         BE    PR50                                                             
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   PR40                                                             
         ZIC   R0,0(R1)            ANYTHING IS BETTER THAN BLOWING UP           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL(3),DUB                                                      
         LA    RE,FULL                                                          
                                                                                
PR50     MVC   0(3,R4),0(RE)                                                    
         LA    R4,2(R4)                                                         
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         BCT   R2,PR40                                                          
         B     PR70                                                             
                                                                                
PR60     MVC   0(3,R4),=C'ALL'                                                  
         LA    R4,4(R4)                                                         
                                                                                
PR70     BCTR  R4,0                                                             
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         GOTO1 REPORT                                                           
                                                                                
PRX      B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* TRACE DATA BLOCK                                                              
*                                                                               
*        PARAMETER 1 - A(DATA)                                                  
*        PARAMETER 2 - L(DATA) OR ZERO FOR RECORD                               
*        PARAMETER 3 - A(LABEL) OR ZERO FOR NO LABEL                            
*        PARAMETER 4 - L(LABEL) IF PARM 3 IS NOT ZERO                           
*                                                                               
*=================================================================*             
MYTRACE  NTR1                                                                   
*                                                                               
         LM    R2,R5,0(R1)         R2 = A(DATA)                                 
*                                  R3 = L(DATA)                                 
*                                  R4 = A(LABEL)                                
*                                  R5 = L(LABEL)                                
*                                                                               
         LTR   R4,R4               IF CALLER SUPPLIED A LABEL                   
         BZ    TR10                                                             
*                                                                               
         MVI   P,C'-'              THEN FILL PRINT LINE WITH '-'S               
         MVC   P+1(131),P                                                       
*                                                                               
         LR    RE,R5               RF = A(PLACE TO CENTER LABEL)                
         SRL   RE,1                                                             
         LA    RF,66                                                            
         SR    RF,RE                                                            
         LA    RF,P(RF)                                                         
*                                                                               
         BCTR  R5,0                MOVE LABEL TO CENTER OF PRINT LINE           
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
*                                                                               
         GOTO1 REPORT              PRINT LABEL LINE                             
*                                                                               
TR10     LTR   R3,R3               IF DATA IS A RECORD                          
         BNZ   TR50                                                             
         OC    DATADISP,DATADISP   IF THERE IS A KEY                            
         BZ    TR15                                                             
*                                  PRINT OUT ITS KEY                            
         LH    R3,DATADISP                                                      
         GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR15     LR    R6,R2               A(RECORD)                                    
         AH    R6,DATADISP         + DISPLACEMENT TO FIRST ELEMENT              
         MVI   ELCODE,0                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   TR100                                                            
*                                                                               
TR20     ZIC   R4,1(R6)            PRINT ELEMENT                                
         GOTO1 PRNTBL,DMCB,0,(R6),C'DUMP',(R4),=X'01C4'                         
*                                                                               
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE ELEMENTS                
         BE    TR20                                                             
         B     TR100                                                            
*                                  ELSE PRINT ENTIRE DATA BLOCK                 
TR50     GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR100    DS    0H                                                               
*                                                                               
TRX      B     EXIT                                                             
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
APRDTAB  DS    A                                                                
AIO      DS    A                                                                
SVAGYA   DS    CL2                                                              
SVSTRSEC DS    XL1                 STR SE NUMBER FOR COKEAT                     
SVSTRSEA DS    XL1                 STR SE NUMBER FOR OTHER AGENCY               
SVCCAGMD DS    XL1                                                              
SVAGYB   DS    XL1                                                              
CNDATA   DS    XL14                                                             
ELEMENT  DS    CL255                                                            
ELCODE   DS    XL1                                                              
PRDCHGSW DC    X'00'                                                            
*                                                                               
CTRS     DS    0CL20                                                            
INCNT    DC    PL4'0',CL16'RECS IN'                                             
RDCNT    DC    PL4'0',CL16'CC TRAFFIC RECS'                                     
ADDCNT   DC    PL4'0',CL16'RECS ADDED'                                          
CHGCNT   DC    PL4'0',CL16'RECS CHANGED'                                        
ERRCNT   DC    PL4'0',CL16'RECS IN ERROR'                                       
CTRX     EQU   *-1                                                              
         EJECT                                                                  
*                                                                               
** SORTER'S CARDS **                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,13,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=4004'                                  
*                                                                               
FLISTS   DC    CL8' SPTFILE'                                                    
         DC    CL8' SPTDIR '                                                    
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
*                                                                               
FLISTT   DC    CL8' TRFFILE'                                                    
         DC    CL8' TRFDIR '                                                    
         DC    CL8'X       '                                                    
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
                                                                                
         DS    0D                                                               
         DC    C'*PRDTAB1'                                                      
PRDTAB1  DS    XL256               USED TO CONVERT CC/CC TO AGY/CC              
*                                                                               
         DS    0D                                                               
         DC    CL8'**CCAGY*'                                                    
CCAGY    DS    1500C               THIS AREA FOR AGENCY'S CC CLTHDR             
*                                                                               
         DS    0D                                                               
         DC    CL8'**CCCC**'                                                    
CCCC     DS    1500C               THIS AREA FOR COKE'S CC CLTHDR               
*                                                                               
         DS    0D                                                               
         DC    C'**REC***'                                                      
REC1     DS    F                                                                
REC      DS    3000C                                                            
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPTRCMML                                                       
RECVHDRD DSECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
RCVRECRD DS    0X                                                               
         PRINT ON                                                               
SPWORKD  DSECT                                                                  
         ORG   P                                                                
ERRKEY   DS    CL30                                                             
ERRMSG   DS    CL102                                                            
         ORG   P                                                                
CMLCML   DS    CL8                                                              
         DS    CL4                                                              
CMLPRD   DS    CL50                                                             
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CT02A     05/01/02'                                      
         END                                                                    
