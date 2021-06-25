*          DATA SET PPINS02    AT LEVEL 131 AS OF 07/09/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044157.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE T41F02A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41F02- INSERTION ORDERS- BUY READ'                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 05/07/12 NEED TO COMPARE THE LAST EIO# IN CANCELLATION LOGIC             
*                                                                               
* KWAN 03/21/11 EIO BY ESTIMATE PERIOD                                          
*                                                                               
* KWAN 04/07/06 STEWARDSHIP INSERTION ORDER                                     
*                                                                               
* KWAN 06/22/05 ADD LOCK                                                        
*                                                                               
* KWAN 05/17/05 IO TO EIO TRANSITION                                            
*                                                                               
* KWAN 06/10/04 CONVERT WRKIO TO LINKIO INTERFACE                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT NOGEN                                                            
T41F02   CSECT                                                                  
         NMOD1 0,T41F02,RR=R2                                                   
*                                                                               
         ST    R2,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING POLWRKD,RC                                                       
         USING IOWORKD,R8                                                       
         LA    R7,1(R9)                                                         
         LA    R7,4095(R7)                                                      
         USING POLFILE,R9,R7                                                    
         USING T41FFFD,RA                                                       
*                                                                               
         XC    ERR(2),ERR          FOR ERROR NUMBER                             
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
*                                                                               
         OC    KEY,KEY                                                          
         BNZ   RD6                                                              
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),PRQMED                                                  
         MVI   KEY+3,X'20'                                                      
         CLC   PRQCLT,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   KEY+4(3),PRQCLT                                                  
*                                                                               
         CLC   PRQPRD,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   KEY+7(3),PRQPRD                                                  
*                                                                               
         CLC   PRQEST,=C'ALL'                                                   
         BE    RD4                                                              
         PACK  DUB,PRQEST                                                       
         CVB   R0,DUB                                                           
         STH   R0,BEST                                                          
         MVC   KEY+19(2),BEST                                                   
*                                                                               
RD4      DS    0H                                                               
         CLC   PRQPUB(3),=C'ALL'                                                
         BE    *+10                                                             
         MVC   KEY+10(6),BPUB                                                   
*                                                                               
         MVC   KEY+16(3),BSTART                                                 
*                                                                               
         GOTOR HIGH                                                             
         B     RD6H                                                             
*                                                                               
RD6      GOTOR SEQ                                                              
*                                                                               
RD6H     CLC   KEY(4),KEYSAVE      SAME AGY/MED/RECORD CODE?                    
         BNE   RD40                                                             
         CLI   KEY+25,X'FF'                                                     
         BE    RD6                                                              
         CLC   PRQCLT,=C'ALL'                                                   
         BE    RD8                                                              
         CLC   KEY+4(3),PRQCLT                                                  
         BNE   RD40                                                             
*                                                                               
RD8      DS    0H                                                               
         CLC   PRQPRD,=C'ALL'                                                   
         BE    RD10                                                             
         CLC   KEY+7(3),PRQPRD                                                  
         BE    RD12                                                             
RD8B     CLC   PRQCLT,=C'ALL'                                                   
         BE    RD6                                                              
         B     RD40                                                             
*                                                                               
RD10     DS    0H                                                               
         CLC   KEY+7(3),=C'ZZZ'    BYPASS POL IF PRD=ALL                        
         BE    RD8B                                                             
*                                                                               
RD12     DS    0H                                                               
         OC    KEY+21(3),KEY+21    BYPASS PASSIVE                               
         BNZ   RD6                                                              
*                                                                               
         OC    BEST,BEST                                                        
         BZ    RD14                                                             
         CLC   KEY+19(2),BEST                                                   
         BNE   RD6                                                              
*                                                                               
RD14     DS    0H                                                               
         CLC   PRQPUB(3),=C'ALL'                                                
         BE    RD16                                                             
         CLC   KEY+10(6),BPUB                                                   
         BE    RD16                                                             
         CLC   QPUB+8(3),=C'ZZZ'   TEST MULTI ZONES/EDITS                       
         BNE   RD14B                                                            
         CLC   KEY+10(4),BPUB                                                   
         BE    RD16                                                             
*                                                                               
RD14B    DS    0H                                                               
         CLC   PRQPRD,=C'ALL'                                                   
         BE    RD6                                                              
         B     RD8B                                                             
*                                                                               
RD16     DS    0H                                                               
         CLC   KEY+16(3),BSTART                                                 
         BL    RD6                                                              
         CLC   KEY+16(3),BEND                                                   
         BNH   RD18                                                             
*                                                                               
         CLC   PRQPUB(3),=C'ALL'   IF ALL PUBS                                  
         BE    RD6                 CONTINUE                                     
         CLC   QPUB+8(3),=C'ZZZ'   OR IF MULTI Z/E                              
         BE    RD6                                                              
         B     RD14B                                                            
*                                                                               
RD18     DS    0H                                                               
         CLC   QBUYLIN,SPACES                                                   
         BE    RD20                                                             
         PACK  DUB,QBUYLIN                                                      
         CVB   R0,DUB                                                           
         STC   R0,BYTE                                                          
         CLC   BYTE,KEY+24         TEST LINE NUMBER                             
         BNE   RD6                                                              
*                                                                               
RD20     DS    0H                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTOR GETPRT                                                           
*                                                                               
         CLI   INSORTYP,IOTSTEWQ   STEWARDSHIP ORDER REQUEST?                   
         JE    RD20H                                                            
         CLI   INSORTYP,IOTSTWEQ   STEWARDSHIP EIO BY ESTIMATE?                 
         JE    RD20H                                                            
         CLI   INSORTYP,IOT1SWPQ   STEWARDSHIP EIO BY ESTIMATE PERIOD?          
         JE    RD20H                                                            
         J     RD20K                                                            
RD20H    TM    PBDSTAT2,X'40'      STEWARDSHIP INSERTION?                       
         BZ    RD6                 NO, SKIP                                     
         B     *+12                                                             
RD20K    CLI   PBDBFD,C'T'         TEST BUYS?                                   
         BE    RD6                 YES, SKIP                                    
*                                                                               
         TM    PBDSTAT,X'08'       HELD - NOT ON I/O OR CONTRACT?               
         BO    RD6                 YES, SKIP                                    
*                                                                               
         TM    PBDSTAT,X'20'       NO TRAFFIC?                                  
         BO    RD6                 YES, SKIP                                    
*                                                                               
         XC    FULL,FULL                                                        
         CLI   SHWACHGR,C'Y'       SHOW ADDITIONAL CHARGES?                     
         BE    *+8                                                              
         MVI   FULL,C'X'           TO EXCLUDE ADDITIONAL CHARGES                
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD,FULL                          
*                                                                               
         OC    PBDJOB,PBDJOB                                                    
         BNZ   *+16                                                             
         BRAS  RE,CKADCODE         CK AD CODE FOR WEB IO                        
         JNE   EXIT                                                             
         B     RD6                                                              
*                                                                               
RD20M    CLC   PRQJOB,=CL6'ALL'                                                 
         BE    RD21                                                             
         CLC   PBDJOB,PRQJOB                                                    
         BNE   RD6                                                              
*                                                                               
RD21     DS    0H                                                               
         TM    ADBSW,AS_ADBRQ      ADBUYER?                                     
         BZ    RD22                NO - CONTINUE                                
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'99'        SERIAL NUMBER ELEMENT                        
         BRAS  RE,NXTEL                                                         
         BNE   RD6                 SKIP BUY - NO SERIAL NO ELEM IN REC          
         L     R3,ASER#TAB         POINT TO MAP ELEM OF SERIAL#S                
         USING PSERELMD,R2                                                      
         USING SER#TABD,R3                                                      
         SR    RF,RF                                                            
         ICM   RF,3,NUMSER#S       NUMBER OF SERIAL#S IN TABLE                  
*                                                                               
RD21LUP  BRAS  RE,CKEIOEBC         EIO BY EST, BACKWARD COMPATIBLE?             
         JNE   EXIT                                                             
         CP    S#SERIAL,PSERNUM    SERIAL# MATCH?                               
         BNE   RD21BMP             NO - TRY NEXT TABLE ENTRY                    
*                                                                               
         BRAS  RE,CKIOCANC         CK FOR CANCELLATIONS                         
         BE    RD21_OK                                                          
         OC    ERR(2),ERR                                                       
         JNZ   EXIT                ERROR OCCURED, EXIT WITH ERROR CODE          
         B     RD21BMP             ALREADY CANCELLED, SKIP INSERTION            
*                                                                               
RD21_OK  MVI   S#STATUS,S#USED_Q   INDICATE BUY REC "USED" HERE                 
         B     RD22                CONTINUE                                     
*                                                                               
RD21BMP  LA    R3,SER#TBLQ(R3)     BUMP TO NEXT TABLE ENTRY                     
         BCT   RF,RD21LUP                                                       
*                                                                               
         BRAS  RE,CKWIONUM         CK FOR WEB IO#                               
         BE    RD21_M              NEED TO INCLUDE THIS INSERTION               
         BRAS  RE,CKSTD_IO         CK FOR STANDARD IO#                          
         BE    RD21_M              NEED TO INCLUDE THIS INSERTION               
RD21_ERR OC    ERR(2),ERR                                                       
         JNZ   EXIT                ERROR OCCURED, EXIT WITH ERROR CODE          
         B     RD6                 NO ERROR, BUT NEED TO SKIP THIS BUY          
*                                                                               
RD21_M   BRAS  RE,CKIO_EST         CK FOR INSERTION ORDER BY ESTIMATE           
         BNE   RD21_ERR                                                         
*                                                                               
RD22     CLI   RCWRITE,C'Y'        UPDATING RECORDS?                            
         BNE   RD22H               NO, THEN NO NEED TO CHECK LOCKS              
*                                                                               
         BRAS  RE,CKIORERR         CK FOR ERRORS IN WRITE MODE                  
         JNE   EXIT                                                             
         DROP  R2,R3                                                            
*                                                                               
         CLI   DATALKSW,X'FE'      LOCK ALREADY ISSUED?                         
         BE    RD22H                                                            
         BRAS  RE,ADDLOCK          CK DATA LOCK AND ISSUE ONE                   
         BE    RD22C                                                            
         MVI   ERRAREA,X'FE'       DATA IS ALREADY LOCKED!                      
         MVC   ERR(2),=AL2(DATALOCK)                                            
         J     EXIT                                                             
*                                                                               
RD22C    MVI   DATALKSW,X'FE'      LOCK HAS BEEN ADDED                          
         GOTOR HIGH                NEED TO RESTORE SEQUENCES                    
         CLC   KEY(25),KEYSAVE     SAME RECORD RETURNED?                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RD22H    XC    DUB,DUB             GET PUB                                      
         MVC   DUB(6),PBUYKPUB                                                  
         CLC   QPUB+8(3),=C'ZZZ'   IF MULTI ZONES/EDITS                         
         BE    RD23                                                             
         BRAS  RE,FNDPUB                                                        
         B     RD23F                                                            
*                                                                               
RD23     DS    0H                  MULTI Z/E                                    
         CLC   SVZON(2),PBUYKPUB+4 TEST NEW Z/E                                 
         BE    RD23B                                                            
         BRAS  RE,FNDPUB                                                        
         MVC   SVZON(2),PBUYKPUB+4 SAVE Z/E                                     
         MVC   SVZONNM,PUBZNAME    AND ZONE NAME                                
*                                                                               
RD23B    DS    0H                                                               
         MVC   DUB(6),BPUB         NOW READ BASE PUB                            
         BRAS  RE,FNDPUB                                                        
*                                                                               
RD23F    DS    0H                                                               
         MVC   PPGKEY,KEY                                                       
*                                  MAKE SURE HAVE PROPER HEADERS                
         XC    WORK(32),WORK                                                    
*                                  CLIENT                                       
         MVC   WORK(7),PBUYKEY                                                  
         MVI   WORK+3,2                                                         
         CLC   WORK(25),PCLTKEY                                                 
         BE    RD24                                                             
         MVC   KEY,WORK                                                         
         GOTOR READ                                                             
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTOR GETPRT                                                           
*                                  PRODCUT                                      
RD24     DS    0H                                                               
         MVI   WORK+3,6                                                         
         MVC   WORK+7(3),PBUYKPRD                                               
         CLC   WORK(25),PPRDKEY                                                 
         BE    RD25                                                             
         MVC   KEY,WORK                                                         
         GOTOR READ                                                             
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         GOTOR GETPRT                                                           
*                                                                               
RD25     DS    0H             MUST READ EST TO SEE IF TEST                      
         B     RD26           NO LONGER NEEDED - OUT 3/8/88                     
*                                                                               
RD26     DS    0H                                                               
*                                  TRY FOR ADDR ELEM                            
         MVC   SVLANG,LANG         SAVE LANGUAGE FOR LAST PRINTING              
         MVC   LANG,PUBLANG                                                     
*                                                                               
         MVC   SCPSCOM1,CPSCOM1    SAVE LAST FOR PRINTING                       
         MVC   SCPSCOM2,CPSCOM2    SAVE LAST FOR PRINTING                       
         XC    CPSCOM1,CPSCOM1     CLEAR                                        
         XC    CPSCOM2,CPSCOM2     CLEAR                                        
         MVI   ADRTYP,C'T'         TRA                                          
         CLI   ADDROPT,C'1'        SHIPPING ONLY                                
         BE    RD26A                                                            
         CLI   ADDROPT,C'2'        OR SHIPPING + PUB                            
         BNE   RD26A5                                                           
*                                                                               
RD26A    MVI   ADRTYP,C'S'                                                      
*                                                                               
RD26A5   DS    0H                                                               
         XC    PREPREC(PUBREC-PREPREC),PREPREC                                  
         XC    DUB,DUB                                                          
*                                    FILL IN 7 BYTES OF CLTDATA                 
RD26B    DS    0H                                                               
         MVC   CLTAGY,PUBKAGY        AGY                                        
         MVC   CLTMED,PRQMED         MED                                        
         MVC   CLTCODE,PCLTKCLT      CLIENT CODE                                
         MVC   CLTOFF,PCLTOFF        CLIENT OFFICE                              
*                                                                               
         GOTO1 APPGETAD,DMCB,(ADRTYP,CLTDATA),PUBREC,VDATAMGR                   
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),0             ADDRESS REC FOUND ?                          
         BNE   RD26C               YES                                          
*                                  NO ADDRESS REC FOUND                         
         JIF   ADRTYP,NE,C'S',AND,ADRTYP,NE,C'T',RD26X,JUMP=N                   
*                                  DIDN'T FIND SHIPPING OR TRAFFIC              
         CLI   QMEDIA,C'O'                                                      
         BNE   RD26X               TRY FOR REP                                  
         MVI   ADRTYP,C'C'         USE CON FOR OUTDOOR                          
         B     RD26A5              TRY FOR CONTRACT ADDRESS                     
*                                                                               
RD26C    MVC   DUB(3),1(R1)        ADDREC LEVEL(3)                              
         MVC   ELCODE,0(R1)        CODE FOUND                                   
         L     R2,4(R1)            A(ADDRESS INFO FROM CALL)                    
         STCM  R2,15,FULL          SAVE A(ADDRESS)                              
*                                                                               
RD26D    DS    0H                                                               
         CLI   ELCODE,X'0B'        SEE IF I WAS LOOKING FOR SHIPPING            
         BE    RD27M               AND FOUND IT - THEN SKIP REP                 
*                                                                               
RD26X    DS    0H                  TRY FOR REP                                  
         MVI   ELCODE,X'14'                                                     
         LA    R2,PUBREC+33                                                     
*                                                                               
RD27B    DS    0H                                                               
         BRAS  RE,NXTEL                                                         
         BNE   RD27M                                                            
         USING PUBREPEL,R2                                                      
         CLC   PUBRPOFF,=3X'FF'                                                 
         BE    RD27D                                                            
         CLC   PUBRPOFF,PCLTKCLT                                                
         BE    RD27D                                                            
         CLI   PUBRPOFF,X'FF'                                                   
         BNE   RD27B                                                            
         CLC   PUBRPOFF+1(1),PCLTOFF                                            
         BNE   RD27B                                                            
*                                                                               
RD27D    DS    0H                                                               
         CLI   PUBREPEL+1,X'2D'    SEE IF BIG REP ELEM                          
         BL    RD27G                                                            
         TM    PUBCSCC1,X'80'      SEE IF ON INSERTION ORDERS                   
         BZ    RD27E                                                            
         MVC   CPSCOM1,PUBCSC1                                                  
RD27E    TM    PUBCSCC2,X'80'                                                   
         BZ    RD27G                                                            
         MVC   CPSCOM2,PUBCSC2                                                  
*                                                                               
RD27G    OC    PUBPAREP(12),PUBPAREP                                            
         BZ    RD27B                                                            
         LA    RF,PUBTRREP                                                      
         CLI   QMEDIA,C'O'                                                      
         BNE   *+8                                                              
         LA    RF,PUBCNREP                                                      
         OC    0(4,RF),0(RF)                                                    
         BZ    RD27M                                                            
         MVC   DUB+3(3),PUBRPOFF                                                
*                                                                               
         OC    DUB(3),DUB                                                       
         BZ    *+14                                                             
         CLC   DUB(3),DUB+3        SELECT MOST SPECIFIC                         
         BNH   RD27M                                                            
         XC    WORK(32),WORK                                                    
         MVC   WORK(2),PUBKAGY                                                  
         MVC   WORK+2(1),PRQMED                                                 
         MVI   WORK+3,X'11'                                                     
         MVC   WORK+4(4),0(RF)                                                  
         CLC   WORK(25),PREPKEY                                                 
         BE    RD30                                                             
         MVC   KEY,WORK                                                         
         GOTOR READ                                                             
         LA    R0,PREPREC                                                       
         ST    R0,AREC                                                          
         GOTOR GETPRT                                                           
         B     RD30                                                             
*                                                                               
RD27M    DS    0H                                                               
         OC    DUB(3),DUB                                                       
         BZ    RD30                                                             
         ICM   R2,15,FULL                                                       
         USING PGETADRD,R2                                                      
         MVC   PREPELEM(2),PGADELEM                                             
*                                                                               
* NOW MOVE FIELDS ONE AT A TIME                                                 
*                                                                               
         MVC   PREPNAME,PGADNAME                                                
         MVC   PREPLIN1,PGADLIN1                                                
         MVC   PREPLIN2,PGADLIN2                                                
         MVC   PREPATTN,PGADATTN                                                
         MVC   PREPTEL,PGADTEL                                                  
*                                                                               
         XC    PREPLIN3,PREPLIN3   MUST CLEAR                                   
         XC    PREPFAX,PREPFAX                                                  
*                                                                               
         L     RE,AWIODSST         POINT TO WIO STORAGE BLOCK                   
         USING WIODSD,RE           WEB IO STORAGE BLOCK                         
         XC    RPUB_NAM,RPUB_NAM                                                
         MVC   RPUB_NAM(L'PGADATTN),PGADATTN                                    
         OC    RPUB_NAM,SPACES                                                  
         CLC   RPUB_NAM,SPACES     ATTENTION?                                   
         BH    *+10                                                             
         MVC   RPUB_NAM,PGADNAME   DEFAULT TO NAME                              
         MVC   RPUB_EML,PGADEADD   E-MAIL ADDRESS                               
         DROP  RE                                                               
*                                                                               
* ELEMENTS SHOULD ALL BE THE SAME LENGTH NOW                                    
*                                                                               
         OC    PGADFAX,SPACES                                                   
         CLC   PGADFAX,SPACES      ANY FAX ?                                    
         BNH   RD27S               NO - MUST HAVE EMAIL ONLY                    
         MVC   PREPLIN3(26),PGADLIN3                                            
         MVC   PREPFAX(12),PGADFAX                                              
*                                                                               
RD27S    MVC   PREPKEY(3),PCLTKEY                                               
         DROP  R2                                                               
*                                                                               
RD30     DS    0H                                                               
         XC    MYFAX,MYFAX                                                      
         CLI   PREPKEY,0                                                        
         BE    RD30A                                                            
         MVC   MYFAX,PREPFAX                                                    
         L     RE,AWIODSST         POINT TO WIO STORAGE BLOCK                   
         USING WIODSD,RE           WEB IO STORAGE BLOCK                         
         XC    RPUB_NAM,RPUB_NAM                                                
         MVC   RPUB_NAM(L'PREPATTN),PREPATTN                                    
         OC    RPUB_NAM,SPACES                                                  
         CLC   RPUB_NAM,SPACES     ATTENTION?                                   
         BH    *+10                                                             
         MVC   RPUB_NAM,PREPNAME   DEFAULT TO NAME                              
         B     RD30D                                                            
         DROP  RE                                                               
*                                                                               
RD30A    LA    R2,PUBREC+33                                                     
         CLI   0(R2),X'11'                                                      
         BE    RD30B                                                            
         MVI   ELCODE,X'11'                                                     
         BRAS  RE,NXTEL                                                         
         BNE   RD30D                                                            
         USING PUBSADEL,R2                                                      
RD30B    MVC   MYFAX,PUBSFAXN                                                   
*                                                                               
         L     RE,AWIODSST         POINT TO WIO STORAGE BLOCK                   
         USING WIODSD,RE           WEB IO STORAGE BLOCK                         
         MVC   RPUB_NAM,SPACES                                                  
         MVC   RPUB_NAM(L'PUBATTN),PUBATTN                                      
         OC    RPUB_NAM,SPACES                                                  
         CLC   RPUB_NAM,SPACES     ATTENTION?                                   
         BH    RD30D                                                            
         LA    R2,PUBREC+33                                                     
         CLI   0(R2),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PUBNAMEL,R2                                                      
         MVC   RPUB_NAM,SPACES                                                  
         MVC   RPUB_NAM(L'PUBNAME),PUBNAME                                      
         DROP  R2,RE                                                            
*                                                                               
RD30D    OC    MYFAX,SPACES                                                     
*                                                                               
RD30S    DS    0H                                                               
         CLC   PPGKEY(25),KEY                                                   
         BE    RD32                                                             
         MVC   KEY(25),PPGKEY                                                   
         GOTOR HIGH                                                             
*                                                                               
RD32     DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
RD40     DS    0H                  END                                          
         MVI   PBUYKEY,X'FF'                                                    
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
ADRTYP   DS    CL1                 TYPE OF ADDR REC-PAY,TRAFFIC,ETC.            
CLTDATA  DS    0CL7                KEY INFO TO PPGETADR MODULE                  
CLTAGY   DS    CL2                                                              
CLTMED   DS    CL1                                                              
CLTCODE  DS    CL3                                                              
CLTOFF   DS    CL1                                                              
*                                                                               
FNDPUB   NTR1                      GET PUB RECORD- PUB CODE IN DUB              
*                                                                               
         XC    FULL,FULL                                                        
         MVI   PUBREC,0                                                         
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(1),PRQMED                                                    
         MVC   KEY+1(6),DUB                                                     
         MVC   KEY+7(2),AGYALPHA                                                
         GOTOR HIGHPB                                                           
         B     FP2K                                                             
FP2      GOTOR SEQPB                                                            
FP2K     CLC   KEY(7),KEYSAVE                                                   
         BE    FP3                                                              
         CLC   QPUB+8(3),=C'ZZZ'   TEST MULTI Z/E                               
         BNE   FP8                                                              
         CLC   KEY(5),KEYSAVE      TEST THRU Z/E                                
         BNE   FP8                                                              
*                                                                               
FP3      DS    0H                                                               
         CLC   KEY+7(2),KEYSAVE+7                                               
         BNE   FP2                                                              
         CLI   KEY+9,X'81'                                                      
         BNE   FP2                                                              
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         GOTOR GETPUB                                                           
*                                                                               
         CLI   FROPT,C'Y'          SEE IF USING PUB'S LANGUAGE                  
         BE    *+8                 IF NOT - ALWAYS IN ENGLISH                   
         MVI   PUBLANG,0           THIS IS SAVE SINCE I NEVER                   
*                                  WILL BE WRITING THE PUB BACK                 
FP8      DS    0H                                                               
         MVC   KEY(64),WORK                                                     
         CLI   PUBREC,0                                                         
         BE    FPNO                                                             
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXIT     XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FPNO     DS    0H                                                               
         DC    H'0'                PUB MISSING                                  
*                                                                               
NXTEL    ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R2),0                                                          
         JNE   NXTEL                                                            
         LTR   R2,R2               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CK FOR AD CODE FOR WEB INSERTION ORDER                                        
*                                                                               
* CC EQUAL     = INSERTION HAS AD CODE OR NOT WEB IO, OKAY TO PROCESS           
* CC NOT EQUAL = NO AD CODE, REPLY ERROR SET IN "ERR"                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKADCODE NTR1  BASE=*,LABEL=*      CK AD CODE FOR WEB IO                        
*                                                                               
         TM    ADBSW,AS_WEBIO      WEB IO?                                      
         JZ    SETCCEQ                                                          
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'99'        SERIAL# ELEM CODE                            
         BRAS  RE,NXTEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE SERIAL#                            
*                                                                               
         USING PSERELMD,R2                                                      
         USING SER#TABD,R3                                                      
*                                                                               
         L     R3,ASER#TAB         POINT TO MAP ELEM OF SERIAL#S                
         SR    RF,RF                                                            
         ICM   RF,3,NUMSER#S       NUMBER OF SERIAL#S IN TABLE                  
*                                                                               
CKADC20  CP    S#SERIAL,PSERNUM    SERIAL# MATCH?                               
         BNE   *+8                 NO - TRY NEXT TABLE ENTRY                    
         B     CKADCERR            AD CODE IS MISSING FOR WEB IO                
*                                                                               
         LA    R3,SER#TBLQ(R3)     BUMP TO NEXT TABLE ENTRY                     
         BCT   RF,CKADC20                                                       
         J     SETCCEQ                                                          
*                                                                               
CKADCERR MVC   ERR(2),=AL2(ADCODERR)                                            
         XC    NUMPRCIN,NUMPRCIN   CLEAR INSERTION PROCESSED COUNTER            
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB,R2,R3                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CHECK ADBUYER VERSION TO DETERMINE OKAY OR NOT TO CONTINUE ORDER              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKEIOEBC NTR1  BASE=*,LABEL=*      CK FOR BACKWARD COMPATIBILITY                
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         CLC   LIOBPCV1,=AL1(03,05,00,06)                                       
         BNL   CKEBC_X                                                          
         MVI   ELCODE,PWIOELCQ     WEB IO ELEM CODE                             
         LA    R2,PBUYREC+33                                                    
         USING PWIOELEM,R2                                                      
         CLI   SVEIOESW,0                                                       
         BE    CKEBC_X                                                          
         CLI   SVEIOESW,C'Y'       USE EIO BY EST STARTING MOS?                 
         BNE   CKEBC_30                                                         
         CLC   SVEACMOS,PBUYREC+(PBUYKDAT-PBUYKEY)                              
         BNH   CKEBC_ER                                                         
CKEBC_24 BRAS  RE,NXTEL                                                         
         BNE   CKEBC_X                                                          
         CLI   PWIOMODC,C'D'       ALREADY CANCELLED?                           
         BE    CKEBC_24                                                         
         TM    PWIOSTAT,PWIODBMQ   DEL'D BUY MOVE CHANGE ORDER ISSUED?          
         BNZ   CKEBC_24                                                         
         TM    PWIOSTAT,PWIOESTQ   EIO BY ESTIMATE?                             
         BZ    CKEBC_24                                                         
         B     CKEBC_ER                                                         
*                                                                               
CKEBC_30 CLI   SVEIOESW,C'N'       DON'T USE EIO BY EST STARTING MOS?           
         BNE   CKEBC_ER                                                         
         CLC   SVEACMOS,PBUYREC+(PBUYKDAT-PBUYKEY)                              
         BL    CKEBC_ER                                                         
         B     CKEBC_24                                                         
*                                                                               
CKEBC_X  J     SETCCEQ                                                          
*                                                                               
CKEBC_ER MVC   ERR(2),=AL2(NEWVERRQ)                                            
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R2                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* IF INSERTION IS NOT IN ADBUYER REQUEST LIST, NEED TO CK IF WEB IO#            
* IS SAME (REVISION # COULD BE DIFFERENT)                                       
*                                                                               
* CC EQUAL     = NEED TO INCLUDE THIS INSERTION ON ORDER                        
* CC NOT EQUAL = OKAY TO SKIP THIS INSERTION                                    
*                ALSO CK "ERR" FOR POSSIBLE ERRORS                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKWIONUM NTR1  BASE=*,LABEL=*      CK FOR WEB IO#                               
*                                                                               
         TM    ADBSW,AS_WEBIO      WEB IO?                                      
         JZ    SETCCNEQ                                                         
*                                                                               
         L     R4,AWIODSST         POINT TO WIO STORAGE BLOCK                   
         USING WIODSD,R4           WEB IO STORAGE BLOCK                         
         LA    R0,EIO#RTAB                                                      
         LHI   R1,E#R_MAXQ*E#R_LENQ                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R2,PBUYREC+33                                                    
         USING PWIOELEM,R2                                                      
         MVI   ELCODE,PWIOELCQ     WEB IO ELEM CODE                             
         BRAS  RE,NXTEL                                                         
         JNE   SETCCNEQ                                                         
*                                                                               
CKW#10_T MVC   FULL,PWIO#YER       SAVE YEAR AND SEQ#                           
CKW#20   LR    RF,R2               SAVE CURRENT EIO ELEM LOCATION               
         BRAS  RE,NXTEL                                                         
         BNE   CKW#30                                                           
         J     CKW#10_T            ALLOW DIFFERENT IO# TO GO THROUGH            
         CLC   FULL,PWIO#YER                                                    
         BE    CKW#20                                                           
CKW#20_T MVC   ERR(2),=AL2(OAENRVER)                                            
         J     SETCCNEQ                                                         
*                                                                               
CKW#30   LR    R2,RF               POINT TO LAST ESR ELEM                       
         CLI   PWIOMODC,C'D'       ALREADY CANCELLED?                           
         JE    CKW#40                                                           
         CLC   SVWIO#,PWIO#YER                                                  
         JE    CKW#40                                                           
         CLI   INSORTYP,IOT1ESTQ   EIO BY ESTIMATE?                             
         JE    CKW#34                                                           
         CLI   INSORTYP,IOTSTWEQ   STEWARDSHIP EIO BY ESTIMATE?                 
         JE    CKW#34                                                           
         CLI   INSORTYP,IOT1ESPQ   EIO BY ESTIMATE PERIOD?                      
         JE    CKW#36                                                           
         CLI   INSORTYP,IOT1SWPQ   STEWARDSHIP EIO BY ESTIMATE PERIOD?          
         JE    CKW#36                                                           
*                                                                               
         TM    PWIOSTAT,PWIOESTQ+PWIOEPRQ                                       
         JNZ   SETCCNEQ            SKIP, ALREADY ON DIFFERENT EIO               
         J     CKW#40                                                           
*                                                                               
         J     CKW#20_T                                                         
CKW#34   TM    PWIOSTAT,PWIOESTQ   EIO BY ESTIMATE?                             
         JZ    SETCCNEQ            SKIP, ALREADY ON DIFFERENT EIO               
         J     CKW#20_T                                                         
CKW#36   TM    PWIOSTAT,PWIOEPRQ   EIO BY ESTIMATE PERIOD?                      
         JZ    SETCCNEQ            SKIP, ALREADY ON DIFFERENT EIO               
         J     CKW#20_T                                                         
*                                                                               
CKW#40   LR    R2,RF               POINT TO LAST EIO ELEM                       
         CLI   PWIOMODC,C'D'       ALREADY CANCELLED?                           
         JE    SETCCNEQ            NEED TO SKIP ALREADY CANCELLED               
         TM    PWIOSTAT,PWIODBMQ   DEL'D BUY MOVE CHANGE ORDER ISSUED?          
         JNZ   SETCCNEQ            SKIP DELETED CHANGED BUY MOVE ORDER          
*                                                                               
CKW#50   CLI   INSORTYP,IOT1ESTQ   EIO BY ESTIMATE?                             
         BE    *+12                                                             
         CLI   INSORTYP,IOTSTWEQ   EIO BY ESTIMATE?                             
         JNE   CKW#52                                                           
         TM    PWIOSTAT,PWIOESTQ   EIO BY ESTIMATE?                             
         BNZ   CKW#80                                                           
         J     CKW#56                                                           
*                                                                               
CKW#52   CLI   INSORTYP,IOT1ESPQ   EIO BY ESTIMATE PERIOD?                      
         JE    *+12                                                             
         CLI   INSORTYP,IOT1SWPQ   STEWARDSHIP EIO BY ESTIMATE PERIOD?          
         JNE   CKW#60                                                           
         TM    PWIOSTAT,PWIOEPRQ   EIO BY ESTIMATE PERIOD?                      
         JNZ   CKW#80                                                           
*                                                                               
CKW#56   XC    WORK(E#R_LENQ),WORK                                              
         MVC   WORK(L'E#R_ESTC),PBUYREC+(PBUYKEST-PBUYKEY)                      
         MVC   WORK+L'E#R_ESTC(L'E#R_EIO#),PWIONUMB                             
         MVC   WORK+L'E#R_ESTC+L'E#R_EIO#(L'E#R_DATE),PWIODATE                  
         LA    RE,EIO#RTAB                                                      
         LHI   RF,E#R_MAXQ                                                      
CKW#58   OC    0(E#R_LENQ,RE),0(RE)                                             
         BNZ   *+14                                                             
         MVC   0(E#R_LENQ,RE),WORK                                              
         B     CKW#80                                                           
         CLC   WORK(E#R_DATE-E#R_ESTC),0(RE)                                    
         BE    CKW#80                                                           
         LA    RE,E#R_LENQ(RE)                                                  
         BCT   RF,CKW#58                                                        
*                                                                               
CKW#60   TM    PWIOSTAT,PWIOESTQ   EIO BY ESTIMATE?                             
         JNZ   CKW#56                                                           
         TM    PWIOSTAT,PWIOEPRQ   EIO BY ESTIMATE PERIOD?                      
         JNZ   CKW#56                                                           
*                                                                               
CKW#80   CLC   SVWIO#,FULL         SAME WEB IO #?                               
         JE    SETCCEQ             NEED TO INCLUDE IT FOR SAME #                
*                                                                               
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB,R2,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* IF WEB IO IS ALREADY CANCELLED, NEED TO SKIP THIS INSERTION                   
*                                                                               
* CC EQUAL     = NEED TO INCLUDE THIS INSERTION ON ORDER                        
* CC NOT EQUAL = OKAY TO SKIP THIS INSERTION                                    
*                ALSO CK "ERR" FOR POSSIBLE ERRORS                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKIOCANC NTR1  BASE=*,LABEL=*      CK FOR WEB IO CANCELLATIONS                  
*                                                                               
         TM    ADBSW,AS_WEBIO      WEB IO?                                      
         JZ    SETCCEQ                                                          
*                                                                               
         XC    ERR(2),ERR                                                       
*                                                                               
         LA    R2,PBUYREC+33                                                    
         USING PWIOELEM,R2                                                      
         MVI   ELCODE,PWIOELCQ     WEB IO ELEM CODE                             
         BRAS  RE,NXTEL                                                         
         BNE   CKIOC60                                                          
CKIOC10T MVC   FULL,PWIO#YER       SAVE YEAR AND SEQ#                           
CKIOC20  LR    RF,R2               SAVE CURRENT EIO ELEM LOCATION               
         BRAS  RE,NXTEL                                                         
         BNE   CKIOC40                                                          
         J     CKIOC10T            ALLOW DIFFERENT IO# TO GO THROUGH            
         CLC   FULL,PWIO#YER                                                    
         BE    CKIOC20                                                          
CKIOC20T MVC   ERR(2),=AL2(OAENRVER)         NOT USED FOR NOW                   
         J     SETCCNEQ                                                         
*                                                                               
CKIOC40  LR    R2,RF               POINT TO LAST EIO ELEM                       
         CLI   PWIOMODC,C'D'       ALREADY CANCELLED?                           
         JE    SETCCNEQ            SKIP ALREADY CANCELLED                       
         TM    PWIOSTAT,PWIODBMQ   DEL'D BUY MOVE CHANGE ORDER ISSUED?          
         JNZ   SETCCNEQ            SKIP DELETED CHANGED BUY MOVE ORDER          
         J     SETCCEQ                                                          
*                                                                               
CKIOC60  LA    R2,PBUYREC+33                                                    
         USING PIOELEM,R2                                                       
         MVI   ELCODE,X'70'        STANDARD IO ELEM CODE                        
CKIOC66  LR    RF,R2               SAVE CURRENT STD IO ELEM LOCATION            
         BRAS  RE,NXTEL                                                         
         BE    CKIOC66                                                          
*                                                                               
         LR    R2,RF               POINT TO LAST STD IO ELEM                    
         CLI   PIOELEM,X'70'                                                    
         JNE   SETCCEQ                                                          
         OC    PIODATE,PIODATE                                                  
         JZ    SETCCEQ                                                          
*                                                                               
         CLI   PIOTYP,C'D'         CANCELLATION ORDER?                          
         JE    SETCCNEQ                                                         
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R2                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CK FOR VARIOUS ERROR CONDITIONS FOR INS ORDER AND WEB IO                      
* ALSO CAN BE USED FOR OTHER ERRORS                                             
*                                                                               
* CC EQUAL     = NO ERROR FOUND, OKAY TO WRITE                                  
* CC NOT EQUAL = CANNOT WRITE, RECORD IS BAD, "ERR" HAS ERROR NUMBER            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKIORERR NTR1  BASE=*,LABEL=*      CK FOR ERRORS IN WRITE MODE                  
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,PBUYLEN                                                     
         CHI   RE,2800             RECORD IS GETTING TOO BIG?                   
         BH    CKIOR91                                                          
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         CLC   LIOBPCV1,=X'03020011'                                            
         JNL   SETCCEQ                                                          
*                                                                               
         MVI   BYTE2,0             FOR SWITCHES                                 
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'70'        INS ORDER ELEM CODE                          
CKIOR05  BRAS  RE,NXTEL                                                         
         BNE   CKIOR10                                                          
         OC    2(3,R2),2(R2)       DATE PRESENT?                                
         BZ    *+8                                                              
         OI    BYTE2,B_X70FDQ      X'70' ELEM WITH DATE FOUND                   
         B     CKIOR05                                                          
*                                                                               
CKIOR10  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,PWIOELCQ     WEB IO ELEM CODE                             
CKIOR15  BRAS  RE,NXTEL                                                         
         BNE   CKIOR20                                                          
         OC    2(3,R2),2(R2)       CONTAIN NONE WEB IO ELEM?                    
         BZ    *+8                                                              
         OI    BYTE2,B_X71FDQ      X'71' ELEM WITH DATE FOUND                   
         B     CKIOR15                                                          
*                                                                               
CKIOR20  DS    0H                                                               
         CLI   BYTE2,0             NOTHING FOUND?                               
         JE    SETCCEQ                                                          
*                                                                               
         TM    BYTE2,B_X70FDQ      INS ORDER ELEM FOUND?                        
         BZ    CKIOR60                                                          
         TM    BYTE2,B_X71FDQ      WEB IO ELEM FOUND?                           
         BZ    CKIOR60                                                          
         MVC   ERR(2),=AL2(X70X71ER)                                            
         J     SETCCNEQ                                                         
*                                                                               
CKIOR60  TM    BYTE2,B_X70FDQ      INS ORDER ELEM FOUND?                        
         BZ    CKIOR62                                                          
         TM    ADBSW,AS_WEBIO      WEB IO?                                      
         BO    CKIOR90                                                          
*                                                                               
CKIOR62  TM    BYTE2,B_X71FDQ      WEB IO ELEM FOUND?                           
         BZ    CKIOR64                                                          
         TM    ADBSW,AS_WEBIO      WEB IO?                                      
         BZ    CKIOR90                                                          
*                                                                               
CKIOR64  DS    0H                                                               
         J     SETCCEQ                                                          
*                                                                               
CKIOR90  MVC   ERR(2),=AL2(WIOINSER)                                            
         J     SETCCNEQ                                                         
*                                                                               
CKIOR91  MVC   ERR(2),=AL2(RECMAXER)                                            
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSTD_IO NTR1  BASE=*,LABEL=*      CK FOR STANDARD IO#                          
*                                                                               
         TM    ADBSW,AS_WEBIO      WEB IO?                                      
         JZ    SETCCNEQ                                                         
*                                                                               
         XC    ERR(2),ERR                                                       
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         CLC   LIOBPCV1,=AL1(03,02,00,17)                                       
         JL    SETCCEQ                                                          
*                                                                               
         LA    R2,PBUYREC+33                                                    
         USING PWIOELEM,R2                                                      
         MVI   ELCODE,PWIOELCQ     WEB IO ELEM CODE                             
CKS_IO10 LR    RF,R2               SAVE CURRENT EIO ELEM LOCATION               
         BRAS  RE,NXTEL                                                         
         BE    CKS_IO10                                                         
         LR    R2,RF               POINT TO LAST EIO ELEM                       
         CLI   PWIOELCO,PWIOELCQ                                                
         BNE   CKS_IO24                                                         
         CLI   PWIOMODC,C'D'       CANCELLATION ORDER?                          
         JE    SETCCNEQ            NO NEED TO INCLUDE THIS INSERTION            
         TM    PWIOSTAT,PWIODBMQ   DEL'D BUY MOVE CHANGE ORDER ISSUED?          
         JNZ   SETCCNEQ            SKIP DELETED CHANGED BUY MOVE ORDER          
         J     SETCCEQ             IF NOT CANCEL, NEED TO INCLUDE IT            
*                                                                               
CKS_IO24 LA    R2,PBUYREC+33                                                    
         USING PIOELEM,R2                                                       
         MVI   ELCODE,X'70'        STANDARD IO ELEM CODE                        
CKS_IO26 LR    RF,R2               SAVE CURRENT STD IO ELEM LOCATION            
         BRAS  RE,NXTEL                                                         
         BE    CKS_IO26                                                         
*                                                                               
         LR    R2,RF                                                            
         CLI   PIOELEM,X'70'                                                    
         JNE   SETCCNEQ            NOT FOUND, NO NEED TO INCLUDE                
         OC    PIODATE,PIODATE                                                  
         JZ    SETCCNEQ            NO NEED TO INCLUDE THIS INSERTION            
*                                                                               
         CLI   PIOTYP,C'D'         CANCELLATION ORDER?                          
         JE    SETCCNEQ                                                         
*                                                                               
         J     SETCCEQ             NEED TO INCLUDE THIS INSERTION               
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R2                                                         
         EJECT                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* IF INSERTION IS NOT IN ADBUYER REQUEST LIST, NEED TO CK IF THIS               
* INSERTION IS ORDERED BY ESTIMATE                                              
*                                                                               
* CC EQUAL     = NEED TO INCLUDE THIS INSERTION ON ORDER                        
* CC NOT EQUAL = OKAY TO SKIP THIS INSERTION                                    
*                ALSO CK "ERR" FOR POSSIBLE ERRORS                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKIO_EST NTR1  BASE=*,LABEL=*      CK FOR INSERTION ORDER BY ESTIMATE           
*                                                                               
         TM    ADBSW,AS_WEBIO      WEB IO?                                      
         JZ    SETCCNEQ                                                         
*                                                                               
         LA    R2,PBUYREC+33                                                    
         USING PWIOELEM,R2                                                      
         MVI   ELCODE,PWIOELCQ     WEB IO ELEM CODE                             
         BRAS  RE,NXTEL                                                         
         JNE   SETCCEQ                                                          
*                                                                               
CKES20   LR    RF,R2               SAVE CURRENT EIO ELEM LOCATION               
         BRAS  RE,NXTEL                                                         
         BE    CKES20                                                           
         LR    R2,RF               POINT TO LAST EIO ELEM                       
*                                                                               
         CLI   INSORTYP,IOT1ESTQ   EIO BY ESTIMATE?                             
         JE    *+12                                                             
         CLI   INSORTYP,IOTSTWEQ   EIO BY ESTIMATE?                             
         JNE   CKES30                                                           
         TM    PWIOSTAT,PWIOESTQ   EIO BY ESTIMATE?                             
         JZ    SETCCNEQ                                                         
         J     SETCCEQ                                                          
*                                                                               
CKES30   CLI   INSORTYP,IOT1ESPQ   EIO BY ESTIMATE PERIOD?                      
         JE    *+12                                                             
         CLI   INSORTYP,IOT1SWPQ   STEWARDSHIP EIO BY ESTIMATE PERIOD?          
         JNE   CKES40                                                           
         TM    PWIOSTAT,PWIOEPRQ   EIO BY ESTIMATE PERIOD?                      
         JZ    SETCCNEQ                                                         
         J     SETCCEQ                                                          
*                                                                               
CKES40   TM    PWIOSTAT,PWIOESTQ   EIO BY ESTIMATE?                             
         JNZ   SETCCNEQ                                                         
         TM    PWIOSTAT,PWIOEPRQ   EIO BY ESTIMATE PERIOD?                      
         JNZ   SETCCNEQ                                                         
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R2                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TEST DATA LOCKED BY OFFLINE APPLICATION                                       
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS               
* ARE AGREED (LOCKUP/LOCKET DSECTS ARE IDENTICAL)                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ADDLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,KEY+2                                                  
         MVC   L.LOCKCLT,KEY+4                                                  
*                                                                               
ADDLK2   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK2              BUSY, TRY AGAIN                              
         CLI   4(R1),1                                                          
         JE    SETCCNEQ            ALREADY LOCKED?                              
         CLI   4(R1),0                                                          
         JNE   SETCCNEQ            ERROR OCCURED                                
*                                                                               
         CLC   PRQPUB(3),=C'ALL'   PUB IS ALL?                                  
         BE    ADDLK4              YES, GO AHEAD AND ISSUE CLIENT LOCK          
         OC    KEY+10(4),KEY+10    NOTHING IN BASE PUB NUMBER?                  
         BZ    ADDLK4              YES, GO AHEAD AND ISSUE CLIENT LOCK          
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BP'    CLIENT LOCK                                  
         MVC   L.LOCKMED,KEY+02                                                 
         MVC   L.LOCKCLT,KEY+04                                                 
         MVC   L.LOCKPUB,KEY+10    BASE PUB NUMBER                              
*                                                                               
         XC    L.LOCKPUB,=4X'FF'   COMPLEMENT PUB (NO BINARY ZERO)              
*                                                                               
ADDLK3   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK3              BUSY, TRY AGAIN                              
         CLI   4(R1),1                                                          
         JE    SETCCNEQ            ALREADY LOCKED?                              
         CLI   4(R1),0                                                          
         JNE   SETCCNEQ            ERROR OCCURED                                
*                                                                               
ADDLK4   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKLOCKQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK4              BUSY, TRY AGAIN                              
         CLI   4(R1),1                                                          
         JE    SETCCNEQ            ALREADY LOCKED                               
         CLI   4(R1),0                                                          
         JNE   SETCCNEQ            ERROR OCCURED                                
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,L                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPINSWRK1                                                      
*                                                                               
       ++INCLUDE POLFILE                                                        
*                                                                               
       ++INCLUDE PPINSWRK2                                                      
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'131PPINS02   07/09/14'                                      
         END                                                                    
