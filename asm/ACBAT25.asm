*          DATA SET ACBAT25    AT LEVEL 034 AS OF 05/01/02                      
*PHASE T61B25A                                                                  
*INCLUDE PUBVAL                                                                 
         TITLE 'VOID CHEQUE AND REVERSE BANK POSTING'                           
T61B25   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**VOI**,R7,CLEAR=YES                                
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         EJECT                                                                  
         LA    R2,VOIBNKH          VALIDATE BANK ACCOUNT                        
         SR    R6,R6                                                            
         BAS   RE,ANY                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         LA    RF,COMPEL                                                        
         USING ACCOMPD,RF                                                       
         MVC   KEY+1(2),ACMPBANK                                                
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),VOIBNK                                                  
         BAS   RE,GETACC                                                        
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'      BALANCE ELEMENT                              
         BZ    ERROR                                                            
*&&UK                                                                           
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    ERROR                                                            
*&&                                                                             
         MVC   BANK,ACCTNUM                                                     
         TM    VOIBNKH+4,X'20'                                                  
         BO    V10                                                              
         OI    VOIBNKH+4,X'20'                                                  
         MVC   VOIBNKN,ACCTNAME                                                 
         OI    VOIBNKNH+6,X'80'                                                 
         SPACE 2                                                                
V10      LA    R2,VOIPAYH          VALIDATE PAYEE                               
         BAS   RE,ANY                                                           
         MVC   KEY+1(14),SPACES                                                 
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),VOIPAY     INPUT HAS UNIT/LEDGER ON FRONT               
         CLI   KEY+2,C'P'                                                       
         BE    V12                 PRINTPAK PAYEES                              
         CLI   KEY+2,C'Q'                                                       
         BNE   V14                                                              
V12      CLI   5(R2),8                                                          
         BL    V14                                                              
         LR    R3,RF                                                            
         SH    R3,=H'2'                                                         
         MVI   ERRNUM,OK                                                        
         GOTO1 AGETACC,DMCB,KEY,(R6)                                            
         CLI   ERRNUM,OK                                                        
         BE    V14                 FOUND SP OR SQ ACCOUNT, ELSE PUBVAL?         
         MVC   KEY+4(20),SPACES                                                 
         GOTO1 =V(PUBVAL),DMCB,((R3),VOIPAY+3),(1,KEY+4),RR=RB                  
         MVI   ERRNUM,17                                                        
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
V14      BAS   RE,GETACC                                                        
         TM    ACCTSTAT,X'80'                                                   
         BZ    ERROR                                                            
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'10'                                                   
         BO    ERROR                                                            
         MVC   PAYEE,ACCTNUM                                                    
         TM    VOIPAYH+4,X'20'                                                  
         BO    V15                                                              
         OI    VOIPAYH+4,X'20'                                                  
         MVC   VOIPAYN,ACCTNAME                                                 
         OI    VOIPAYNH+6,X'80'                                                 
         SPACE 1                                                                
V15      MVC   BCONTRA,PAYEE                                                    
         MVC   BCONTRAN,VOIPAYN                                                 
*&&US*&& B     V21                                                              
*&&UK                                                                           
         CLC   PAYEE+1(2),=C'ST'   UK ARTISTS HAVE FUNNY CONTRA ON              
         BNE   V21                 BANK POSTING(AGENT)                          
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),=C'ST'                                                  
         BAS   RE,READ                                                          
         LA    R4,IOAREA                                                        
         SR    RF,RF                                                            
V16      CLI   0(R4),0             GET HEIRARCHY ELEMENT                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'16'                                                      
         BE    V17                                                              
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     V16                                                              
         USING ACHEIRD,R4                                                       
V17      IC    RF,ACHRLEVB         L'LEVA AND LEVB                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),PAYEE+3                                                 
         LA    R2,VOIPAYH                                                       
         BAS   RE,GETACC                                                        
         MVC   BCONTRA,KEY                                                      
         MVC   BCONTRAN,ACCTNAME                                                
         B     V21                                                              
*&&                                                                             
         EJECT                                                                  
V21      LA    R2,VOIUNH                                                        
         TM    COMPSTAT,X'20'      OFFICE AGENCY                                
         BZ    V21A                                                             
         BAS   RE,ANY              TEST FOR OFFICE INPUT                        
V21A     CLI   VOIUN,C'.'          A PERIOD IS ALWAYS VALID                     
         BE    V21C                BUT I'LL CHANGE IT TO BLANK                  
         OC    VOIUN,SPACES                                                     
         GOTO1 AVALOFFC,DMCB,(X'80',VOIUN)                                      
         CLI   ERRNUM,OK                                                        
         BNE   ERROR                                                            
V21C     LA    R2,VOICNOH          CHECK NUMBER INPUT                           
         BAS   RE,ANY                                                           
         MVI   ERRNUM,2                                                         
         CLI   5(R2),6             MUST BE 6-LONG                               
         BNE   ERROR                                                            
         MVC   WORK(5),=5X'F0'     AND LAST 5-NUMERIC                           
         MVZ   WORK(5),VOICNO+1                                                 
         CLC   WORK(5),=5X'F0'                                                  
         BNE   ERROR                                                            
         MVC   CHNO,VOICNO                                                      
         SPACE 2                                                                
         LA    R2,VOICDATH         CHEQUE DATE                                  
         BAS   RE,ANY                                                           
         MVI   ERRNUM,13                                                        
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHDATE3)                                 
         GOTO1 (RF),(R1),,(8,CHDATE8)                                           
         OC    CHDATE8,SPACES                                                   
         SPACE 2                                                                
         LA    R2,VOICAMTH         CHEQUE AMOUNT                                
         BAS   RE,ANY                                                           
         MVI   ERRNUM,25                                                        
         ZIC   R3,5(R2)                                                         
         GOTO1 AMTVAL,DMCB,8(R2),(R3)                                           
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         L     RF,DMCB+4                                                        
         LA    RF,0(RF)                                                         
         ZAP   CHAMT,0(8,RF)                                                    
         ZAP   TRANSAMT,0(8,RF)                                                 
         SPACE 1                                                                
         LA    R2,VOIACTH                                                       
         BAS   RE,ANY                                                           
         MVI   PACTION,C' '                                                     
         CLI   VOIACT,C'U'                                                      
         BE    V22                                                              
         MVI   ERRNUM,2                                                         
         CLI   VOIACT,C'V'                                                      
         BNE   ERROR                                                            
         MVI   PACTION,C'V'                                                     
         MVC   VOIACT(4),=C'VOID'                                               
         B     V24                                                              
V22      MVC   VOIACT(6),=C'UNVOID'                                             
V24      OI    VOIACTH+6,X'80'                                                  
         B     V30                                                              
         EJECT                                                                  
*              READ BANK POSTINGS FOR CHEQUE                                    
         SPACE 2                                                                
*&&US                                                                           
V30      LA    R2,VOICNOH                                                       
         MVC   KEY,SPACES                                                       
         LA    RF,KEY                                                           
         USING ACKEYD,RF                                                        
         MVC   ACKEYACC,BANK       BUILD KEY TO READ CHEQUE POSTING             
         MVC   ACKEYCON,BCONTRA                                                 
         MVC   ACKEYDTE,CHDATE3                                                 
         MVC   ACKEYREF,CHNO                                                    
         MVI   ACKEYSBR,0                                                       
         BAS   RE,HIGH                                                          
         CLC   KEY(41),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    R2,VOICAMTH                                                      
         MVI   ERRNUM,25                                                        
         LA    RF,IOAREA                                                        
         CLI   0(RF),X'44'                                                      
         BNE   ERROR                                                            
         USING TRANSD,RF                                                        
         CP    TRNSAMNT,CHAMT      AMOUNTS MUST MATCH                           
         BNE   ERROR                                                            
         B     V50                                                              
*&&                                                                             
*&&UK                                                                           
V30      LA    R2,VOICNOH                                                       
         MVC   KEY,SPACES                                                       
         LA    RF,KEY                                                           
         USING ACKEYD,RF                                                        
         MVC   ACKEYACC,BANK       BUILD KEY TO READ CHEQUE POSTING             
         MVC   ACKEYCON,BCONTRA                                                 
         MVC   ACKEYDTE,CHDATE3                                                 
         MVC   ACKEYREF,CHNO                                                    
         MVI   ACKEYSBR,0                                                       
         BAS   RE,HIGH                                                          
V32      CLC   KEY(41),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    R2,VOICAMTH                                                      
         MVI   ERRNUM,25                                                        
         LA    RF,IOAREA                                                        
         CLI   0(RF),X'44'                                                      
         BNE   ERROR                                                            
         USING TRANSD,RF                                                        
         CLC   PAYEE+1(2),=C'ST'   FOR UK ARTISTS SKIP AMOUNT TEST              
         BE    V50                                                              
         CP    TRNSAMNT,CHAMT      AMOUNTS MUST MATCH                           
         BNE   V34                                                              
         CLI   TWAACCS,C'*'        AND IF OFFICE LOGON                          
         BNE   V50                                                              
         CLC   TRNSANAL(1),TWAACCS+1  OFFICES MUST MATCH ALSO                   
         BE    V50                                                              
V34      BAS   RE,SEQ              IF NOT TRY NEXT CHEQUE                       
         B     V32                 AS WE MAY HAVE OFFICE CHEQUES                
*&&                                                                             
         EJECT                                                                  
*              READ AND MARK PAYEE POSTINGS                                     
         SPACE 2                                                                
V50      MVC   KEY,SPACES                                                       
         MVC   KEY(15),PAYEE                                                    
         ZAP   CASHTOT,=P'0'                                                    
         ZAP   ITEMS,=P'0'                                                      
*&&US                                                                           
         BAS   RE,READL                                                         
V52      BAS   RE,SEQL                                                          
*&&                                                                             
*&&UK                                                                           
         BAS   RE,READ                                                          
V52      BAS   RE,SEQ                                                           
*&&                                                                             
         CLC   PAYEE,KEY           SAME PAYEE                                   
         BNE   V60                 NO- CHECK AMOUNTS AGAIN                      
         LA    RF,IOAREA                                                        
         USING TRANSD,RF                                                        
         CLI   TRNSEL,X'44'        FIND TRANSACTION                             
         BNE   V52                                                              
         TM    TRNSSTAT,X'80'      ONLY WANT DEBITS                             
         BZ    V52                                                              
         CLI   TRNSTYPE,X'81'                                                   
         BNE   V52                                                              
         CLC   CHNO,TRNSNARR       MATCH ON CHECK NUMBER                        
         BNE   V52                                                              
*&&US*&& CLC   CHDATE8,TRNSNARR+6  AND DATE                                     
*&&UK*&& CLC   CHDATE8(7),TRNSNARR+6                                            
         BNE   V52                                                              
*&&UK                                                                           
         CLI   VOIUNH+5,0          AND OFFICE IF INPUT                          
         BE    V54                                                              
         CLI   TRNSANAL,C' '       UNLESS TRANSACTION HAS NO OFFICE             
         BE    V54                                                              
         CLC   VOIUN,TRNSANAL                                                   
         BNE   V52                                                              
V54      DS    0H                                                               
*&&                                                                             
         CLC   TRNSNARR+40(1),PACTION                                           
         BE    V52                 MARK(OR UNMARK) TRANSACTION                  
         AP    CASHTOT,TRNSAMNT                                                 
         AP    ITEMS,=P'1'                                                      
         MVC   TRNSNARR+40(1),PACTION                                           
*&&US*&& BAS   RE,WRITE                                                         
         B     V52                                                              
         SPACE 2                                                                
V60      CP    CASHTOT,CHAMT       TEST CHEQUE AMOUNT VS ITEM AMOUNT            
         BE    V100                                                             
         CP    CASHTOT,=P'0'       IF NOT ZERO - POSSIBLE DISCOUNT              
         BNE   V100                INVOLVED                                     
         LA    R2,VOICAMTH         IF NOT EQUAL - POSSIBLY DOUBLE INPUT         
         MVI   ERRNUM,25                                                        
         B     ERROR                                                            
         EJECT                                                                  
*              GENERATE A PAIR OF POSTINGS                                      
         SPACE 2                                                                
V100     LA    R6,IOAREA+2         DESCRIPTION ELEMENT                          
         USING DLDESCD,R6                                                       
         MVI   DLDSEL,X'64'                                                     
         MVC   DLDSREF,CHNO                                                     
         MVC   DLDSDATE,CHDATE3                                                 
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
*&&UK*&& OI    DLDSSTAT,X'08'      AUTHORISE                                    
         BAS   RE,GETODAY                                                       
         MVC   DLDSNARR(30),SPACES                                              
*&&US                                                                           
         MVC   DLDSNARR(5),=C'CHECK'                                            
         LA    R3,DLDSNARR+6                                                    
*&&                                                                             
*&&UK                                                                           
         MVC   DLDSNARR(6),=C'CHEQUE'                                           
         LA    R3,DLDSNARR+7                                                    
*&&                                                                             
         CLI   VOIACT,C'U'                                                      
         BNE   V102                                                             
         MVC   0(2,R3),=C'UN'                                                   
         LA    R3,2(R3)                                                         
V102     MVC   0(9,R3),=C'VOIDED ON'                                            
         GOTO1 DATCON,DMCB,(0,WORK),(8,10(R3))                                  
         LA    R3,DLDSNARR+24                                                   
         CLI   VOIACT,C'U'                                                      
         BNE   *+8                                                              
         LA    R3,2(R3)                                                         
         SR    R3,R6                                                            
         STC   R3,DLDSLEN                                                       
         SPACE 2                                                                
         CLI   VOIACT,C'U'                                                      
         BNE   V103                                                             
*&&US                                                                           
         ZAP   DUB,CHAMT        REVERSE POSTINGS IF UNVOID                      
         MP    DUB,=P'-1'                                                       
         ZAP   CHAMT,DUB                                                        
*&&                                                                             
*&&UK                                                                           
         ZAP   DUB,CASHTOT                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   CASHTOT,DUB                                                      
*&&                                                                             
V103     AR    R6,R3                                                            
         USING DLPOSTD,R6                                                       
         MVC   DLPSEL(2),=X'6A71'  CREDITS ONLY                                 
         MVC   DLPSCRAC,BANK                                                    
         MVC   DLPSCRNM,VOIBNKN                                                 
         MVC   DLPSDBAC,BCONTRA                                                 
         MVC   DLPSDBNM,BCONTRAN                                                
         MVI   DLPSTYPE,0                                                       
*&&US*&& ZAP   DLPSAMNT,CHAMT                                                   
*&&UK*&& ZAP   DLPSAMNT,CASHTOT                                                 
         ZAP   DUB,DLPSAMNT                                                     
         MP    DUB,=P'-1'     POST MINUS CR TO BANK                             
         ZAP   DLPSAMNT,DUB                                                     
         MVC   DLPSANAL,SPACES                                                  
         CLI   VOIUN,C'.'          PERIOD IS LIKE A BLANK                       
         BE    V104                                                             
         CLI   VOIUNH+5,0                                                       
         BE    V104                                                             
         MVC   DLPSANAL,VOIUN                                                   
         SPACE 2                                                                
V104     ZIC   RF,DLPSLEN                                                       
         AR    R6,RF                                                            
         MVC   DLPSEL(2),=X'6A71'                                               
         MVC   DLPSCRAC,PAYEE                                                   
         MVC   DLPSCRNM,VOIPAYN                                                 
         MVC   DLPSDBAC,BANK                                                    
         MVC   DLPSDBNM,VOIBNKN                                                 
         MVI   DLPSTYPE,0                                                       
*&&US*&& ZAP   DLPSAMNT,CHAMT                                                   
*&&UK*&& ZAP   DLPSAMNT,CASHTOT                                                 
*&&UK*&& ZAP   TRANSAMT,CASHTOT                                                 
         MVC   DLPSANAL,SPACES                                                  
         CLI   VOIUN,C'.'          PERIOD IS LIKE A BLANK                       
         BE    V105                                                             
         CLI   VOIUNH+5,0                                                       
         BE    V105                                                             
         MVC   DLPSANAL,VOIUN                                                   
         SPACE 1                                                                
V105     ZIC   RF,DLPSLEN                                                       
         AR    R6,RF                                                            
         MVI   0(R6),0             E-O-R                                        
         LA    RF,IOAREA-1                                                      
         SR    R6,RF                                                            
         STH   R6,HALF                                                          
         MVC   IOAREA(2),HALF      RECORD LENGTH                                
         BAS   RE,PUTDAY                                                        
         SPACE 2                                                                
         XC    WORK(20),WORK       BUILD TWA ENTRY                              
         MVC   WORK(6),CHNO                                                     
         L     RF,DMCB+8                                                        
         MVC   WORK+10(4),0(RF)                                                 
         BAS   RE,ADTWA1                                                        
         SPACE 1                                                                
         MVC   VOIMSG,SPACES                                                    
         EDIT  ITEMS,(4,VOIMSG),ALIGN=LEFT                                      
         MVC   VOIMSG+5(21),=C'ITEMS UNVOIDED,VALUE='                           
         CLI   PACTION,C'V'                                                     
         BNE   *+10                                                             
         MVC   VOIMSG+11(2),SPACES                                              
         CP    ITEMS,=P'1'                                                      
         BNE   *+8                                                              
         MVI   VOIMSG+9,C' '                                                    
         LA    RF,VOIMSG+26                                                     
         EDIT  CASHTOT,(10,0(RF)),2,ALIGN=LEFT                                  
         GOTO1 SQUASHER,DMCB,VOIMSG,52                                          
         OI    VOIMSGH+6,X'80'                                                  
         SPACE 1                                                                
         LA    R2,VOIBNKH                                                       
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                   ** NEW             
       ++INCLUDE ACBATDSECT                                  ** NEW             
       ++INCLUDE ACBATD6D                                                       
         EJECT                                                                  
PROGD    DSECT                                               ** NEW             
BANK     DS    CL15                                                             
PAYEE    DS    CL15                                                             
BCONTRA  DS    CL15                                                             
BCONTRAN DS    CL36                                                             
CHNO     DS    CL6                                                              
CHDATE3  DS    CL3                                                              
CHDATE8  DS    CL8                                                              
CHAMT    DS    PL6                                                              
ITEMS    DS    PL4                                                              
CASHTOT  DS    PL6                                                              
PACTION  DS    CL1                                                              
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
*        ACGENBOTH                                                              
*        ACGENDAY                                                               
*        DDFLDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034ACBAT25   05/01/02'                                      
         END                                                                    
