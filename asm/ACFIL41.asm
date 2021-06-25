*          DATA SET ACFIL41    AT LEVEL 007 AS OF 08/16/00                      
*PHASE T62341A,*                                                                
*MWHE    L 02  CHANGE WORKCODE VALIDATION                                       
*MWHE    L 03  FIX BUG DISPLAYING ZERO BILLED COMMISSION                        
*MWHE    L 04  ADD ORDER NUMBER COLUMN                                          
*RWIL    L 05  SHOW ORDERS UNDER REAL WORKCODE                                  
*MWHE    L 06  HANDLE 5 CHAR JOB CODE FOR 5 CHAR CLI CODE                       
*                                                                               
         TITLE 'PRODUCTION BILLING REPORT DOWNLOAD'                             
FIL41    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,ACFIL41*,R5,R6,R7,RR=RE                                        
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
*                                                                               
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                 UNKNOWN ROUTINE                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
         SPACE 1                                                                
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
         SPACE 2                                                                
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT NOT WANTED FOR FILTER                   
         B     EXITOK                                                           
*                                                                               
DIE      DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   LR    RE,RF               @@ DEBUG  @@                                 
         ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         LA    R1,SVPARMS                                                       
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(EXITH)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(ODLOAD),AL1(0,0,0),AL4(DLOAD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLIST                             
         L     R1,AREP                                                          
         USING REPD,R1                                                          
         MVC   REPAPQB,ATIA        ??????                                       
         MVC   REPACOM,ACOM        INITIALIZE REPBLK VALUES                     
         LA    R0,REPHS                                                         
         ST    R0,REPABUF                                                       
         MVC   REPSUBID,=C'TOP'                                                 
         MVI   REPHEADN,REPHN                                                   
         MVI   REPMIDSN,REPMN                                                   
         MVI   REPPRNTN,REPPN                                                   
         MVI   REPFOOTN,REPFN                                                   
         OI    REPIND2,REPILOW                                                  
         OI    REPHEADI,REPHSPAC+REPHCLRA                                       
         OI    REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVC   REPDATE,ASBDAT                                                   
         MVC   REPSYSID,=C'AC'                                                  
         MVC   REPPRGID,=C'NF'                                                  
         MVC   REPRLH,=Y(48)                                                    
         MVC   REPRDH,=Y(12)                                                    
*                                                                               
         MVC   INSYSID,=CL2'AC'                                                 
         MVC   INPRGID,=CL2'NF'                                                 
         MVC   INJCLID,=CL2'FD'                                                 
         MVI   INPRTY1,C' '                                                     
         MVI   INPRTY2,C' '                                                     
         MVI   INWHEN,INWNNOW                                                   
*                                                                               
         LA    R2,IOKEY                                                         
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN      SET COMPANY CODE                             
         MVC   LDGKUNT(2),ULSJ     SET SJ LEDGER                                
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'G',=C'ACCMST'),('ACLELQ',AIO2),0                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,12(R1)                                                        
         USING ACLEL,R2                                                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVC   CLILEN,ACLVLEN      SAVE CLIENT CODE LENGTH                      
         B     EXITOK                                                           
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R0,R3,SVPARMS                                                    
         USING TRNRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** FIRST TIME FOR KEY OBJECT ***                 
*                                 -------------------------                     
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   XC    TRNKEY,TRNKEY                                                    
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(2),ULSJ                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                  *         
* P2 HOLDS EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA ACTION         *         
* P3 BYTE  0   HOLDS EQUATED DATA VERB IF P2 IS ZERO                  *         
* P3 BYTES 1-3 HOLDS EQUATED ACTION VERB                              *         
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                 *         
*                                                                     *         
* FIELD DATA IS EXTRACTED/OUTPUT INTO FVIFLD                          *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
DATA     ICM   R1,15,SVPARMS2      R1=DATA IDENTIFIER                           
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS4                                                      
         USING FDRRECD,R2                                                       
         XR    R1,R1                                                            
         IC    R1,SVPARMS3         GET GLOBAL VERB                              
         LA    RF,DTATABL          TABLE OF GLOBAL VERBS                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DATA02   LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA04   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA06                                                           
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA04                                                           
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
*                                                                               
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING TRNRECD,R2          R2 HOLDS A(RECORD)                           
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3           R3=A(TRNEL)                                  
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(00750),AL4(CLIDTA)    CLIENT CODE                            
         DC    AL2(00751),AL4(PRODTA)    PRODUCT CODE                           
         DC    AL2(00752),AL4(JOBDTA)    JOB CODE                               
         DC    AL2(00753),AL4(WCDTA)     WORKCODE                               
         DC    AL2(00754),AL4(BILDTA)    BILL NUMBER                            
         DC    AL2(00755),AL4(SUPDTA)    SUPPLIER CODE                          
         DC    AL2(00756),AL4(SUNDTA)    SUPPLIER NAME                          
         DC    AL2(00757),AL4(DATDTA)    DATE                                   
         DC    AL2(00758),AL4(REFDTA)    REFERENCE NUMBER                       
         DC    AL2(00759),AL4(NARDTA)    NARRATIVE / DESCRIPTION                
         DC    AL2(00760),AL4(NETDTA)    NET AMOUNT                             
         DC    AL2(00761),AL4(COMDTA)    COMMISSION AMOUNT                      
         DC    AL2(00762),AL4(NBLDTA)    NET BILLED                             
         DC    AL2(00763),AL4(CBLDTA)    COMMISSION BILLED                      
         DC    AL2(00764),AL4(BNMDTA)    BILL NUMBER                            
         DC    AL2(00765),AL4(BDTDTA)    BILL DATE                              
         DC    AL2(00766),AL4(RIDDTA)    REPORT ID                              
         DC    AL2(00767),AL4(ORDDTA)    ORDER NUMBER                           
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL41    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CLIENT CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CLIDTA   LA    RF,CLITBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CLITBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCLI) - ON SCREEN                     
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCLI) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CLIENT CODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISCLI   DS    0H                                                               
         XR    R1,R1                                                            
         IC    R1,CLILEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   FVIFLD(0),TRNKACT                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CLIENT CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALCLI   DS    0H           CHECK CLIENT CODE RIGHT LENGTH & EXISTS             
         CLC   FVILEN,CLILEN       INPUT LENGTH TOO LONG                        
         BH    EXITNV                                                           
         LA    R2,IOKEY                                                         
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN      SET COMPANY CODE                             
         MVC   TRNKUNT(2),ULSJ     SET SJ LEDGER                                
         XR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         EX    R1,*+4                                                           
         MVC   TRNKACT(0),FVIFLD   PUT IN CLIENT CLODE                          
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BNE   EXITNV                                                           
         MVC   CLICODE,TRNKACT                                                  
         B     EXITOK              CLIENT CODE NOT VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR PRODUCT CODE                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PRODTA   LA    RF,PROTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PROTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRO) - ON SCREEN                     
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRO) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PRODUCT CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISPRO   DS    0H                                                               
         XR    R1,R1                                                            
         IC    R1,CLILEN           R1=L'CLIENT CODE                             
         LA    R1,TRNKACT(R1)      R1=A(PRODUCT CODE)                           
         MVC   FVIFLD(2),0(R1)                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PRODUCT CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALPRO   DS    0H                                                               
         LA    R2,IOKEY                                                         
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN      SET COMPANY CODE                             
         MVC   TRNKUNT(2),ULSJ     SET SJ LEDGER                                
         XR    R1,R1                                                            
         IC    R1,CLILEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   TRNKACT(0),CLICODE  PUT IN CLIENT CLODE                          
         LA    R1,TRNKACT+1(R1)                                                 
         MVC   0(2,R1),FVIFLD      PUT IN PRODUCT CODE                          
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BNE   EXITNV                                                           
         MVC   PROCODE,FVIFLD                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR JOB CODE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
JOBDTA   LA    RF,JOBTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
JOBTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISJOB) - ON SCREEN                     
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALJOB) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY JOB CODE FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISJOB   DS    0H                                                               
         XR    RF,RF                                                            
         IC    RF,CLILEN           R1=L'CLIENT CODE                             
         LA    RF,2(RF)            ADD ON PRODUCT CODE LENGTH                   
         LA    RE,L'TRNKACT-1                                                   
         SR    RE,RF                                                            
         LA    RF,TRNKACT(RF)      R1=A(JOB CODE)                               
         EX    RE,*+4                                                           
         MVC   FVIFLD(0),0(RF)                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE JOB CODE FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALJOB   DS    0H                                                               
         LA    R2,IOKEY                                                         
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN      SET COMPANY CODE                             
         MVC   TRNKUNT(2),ULSJ     SET SJ LEDGER                                
         XR    R1,R1                                                            
         IC    R1,CLILEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   TRNKACT(0),CLICODE  PUT IN CLIENT CLODE                          
         LA    R1,TRNKACT+1(R1)                                                 
         MVC   0(2,R1),PROCODE     PUT IN PRODUCT CODE                          
         LA    R5,2(R1)                                                         
         XR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         EX    R1,*+4                                                           
         MVC   0(0,R5),FVIFLD      PUT IN JOB CODE                              
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BNE   EXITNV                                                           
         XR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         EX    R1,*+4                                                           
         MVC   JOBCODE(0),0(R5)                                                 
         MVC   CLPRJO,TRNKACT                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR WORKCODE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
WCDTA    LA    RF,WCTBL            TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
WCTBL    DC    AL1(DDIS),AL1(0,0,0),AL4(DISWC) - ON SCREEN                      
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWC) - ON SCREEN                      
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY WORKCODE FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISWC    MVC   FVIFLD(L'TRNKWORK),TRNKWORK                                      
         CLC   =C'**',TRNKWORK                                                  
         BNE   DISWCX                                                           
         GOTO1 VHELLO,BOPARM,(C'G',=C'ACCMST'),('OAMELQ',TRNRECD),0             
         CLI   12(R1),0                                                         
         BNE   DISWCX                                                           
         L     RF,BOPARM+12                                                     
         MVC   FVIFLD(L'TRNKWORK),OAMWORK-OAMELD(RF)                            
DISWCX   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE WORKCODE FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALWC    DS    0H                                                               
         CLI   FVILEN,0                                                         
         BNE   VALWC02                                                          
         XC    WORKCODE,WORKCODE                                                
         B     EXITOK                                                           
*                                                                               
VALWC02  CLI   FVILEN,2                                                         
         BNH   VALWC04                                                          
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),AC@ALL                                                 
         BNE   EXITNV                                                           
         MVC   WORKCODE,FFFF                                                    
         B     EXITOK                                                           
*                                                                               
VALWC04  LA    R4,IOKEY                                                         
         USING WCORECD,R4                                                       
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN      SET COMPANY CODE                             
         MVC   WCOKUNT(2),ULSJ     SET SJ LEDGER                                
         MVC   WCOKWRK,FVIFLD                                                   
         GOTO1 AIO,IORD+IOACCDIR+IO2                                            
         BNE   EXITNV                                                           
         MVC   WORKCODE,WCOKWRK                                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BILL NUMBER                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
BILDTA   LA    RF,BILTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BILTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBIL) - ON SCREEN                     
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBIL) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BILL NUMBER FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISBIL   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BILL NUMBER FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALBIL   DS    0H                                                               
         CLI   FVILEN,0                                                         
         BNE   VALBIL02                                                         
         OC    WORKCODE,WORKCODE                                                
         BZ    EXITNO                                                           
         MVC   BILLNUM,BCSPACES                                                 
         B     EXITOK                                                           
*                                                                               
VALBIL02 MVC   BILLNUM,FVIFLD                                                   
         OC    WORKCODE,WORKCODE                                                
         BZ    EXITOK                                                           
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR SUPPLIER CODE                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SUPDTA   LA    RF,SUPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SUPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSUP) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SUPPLIER CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISSUP   MVC   FVIFLD(L'TRNKULC),TRNKULC                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR SUPPLIER NAME                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SUNDTA   LA    RF,SUNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SUNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSUN) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SUPPLIER NAME FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISSUN   DS    0H                                                               
         MVC   FVIFLD(L'SUPNAME),SUPNAME                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TRANSACTION DATE                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DATDTA   LA    RF,DATTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DATTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDAT) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TRANSACTION DATE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISDAT   DS    0H                                                               
         GOTO1 VDATCON,BOPARM,(1,TRNKDATE),(17,FVIFLD)                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TRANSACTION REFERENCE                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
REFDTA   LA    RF,REFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
REFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISREF) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TRANSACTION REFERENCE FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DISREF   MVC   FVIFLD(L'TRNKREF),TRNKREF                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR ORDER NUMBER                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
ORDDTA   LA    RF,ORDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
ORDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISORD) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ORDER NUMBER FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISORD   LA    RE,TRNRFST                                                       
         USING FFNELD,RE                                                        
         SR    R0,R0                                                            
DISORD02 CLI   FFNEL,0                                                          
         BE    EXITOK                                                           
         CLI   FFNEL,FFNELQ                                                     
         BE    *+14                                                             
         IC    R0,FFNLN                                                         
         AR    RE,R0                                                            
         B     DISORD02                                                         
*                                                                               
         MVC   FVIFLD(L'FFNONUM),FFNONUM                                        
         B     EXITOK                                                           
         DROP  RE                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR NARRATIVE / DESCRITPION                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NARDTA   LA    RF,NARTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
NARTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAR) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY NARRATIVE FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
DISNAR   DS    0H                                                               
         SR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         SH    RE,=Y(TRNLN1Q)                                                   
         BZ    EXITOK                                                           
         LA    RF,75               MAXIMUM NARRATIVE LENGTH                     
         CLC   TRNANAL,WC99        TEST PREVIOUS BILL                           
         BNE   *+8                                                              
         LA    RF,15               SET MAXIMUM BILLING NARR                     
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     EXITOK                                                           
         MVC   FVIFLD(0),TRNNARR                                                
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR NET AMOUNT                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NETDTA   LA    RF,NETTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
NETTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNET) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY NET AMOUNT FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISNET   DS    0H                                                               
         GOTO1 VPRORATA,BOPARM,(R2),AGOPBLK,ACOM,0,PRORBLK,0                    
         CURED PA$NET,(12,FVIFLD),2,ALIGN=LEFT,MINUS=YES                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR COMMISION AMOUNT                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
COMDTA   LA    RF,COMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
COMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOM) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY COMMISSION AMOUNT FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DISCOM   DS    0H                                                               
         CURED PA$NLCOM,(12,FVIFLD),2,ALIGN=LEFT,MINUS=YES                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR NET BILLED AMOUNT                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NBLDTA   LA    RF,NBLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
NBLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNBL) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY NET BILLED FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISNBL   DS    0H                                                               
         CLC   TRNKWORK,WC99       TEST W/C 99                                  
         BNE   DISNBL01                                                         
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         CURED TRNAMNT,(12,FVIFLD),2,ALIGN=LEFT,MINUS=YES                       
         B     EXITOK                                                           
*                                                                               
DISNBL01 CLC   BILLNUM,BCSPACES    TEST BILL NUMBER SPECIFIED                   
         BNE   DISNBL02                                                         
         CURED PA$NETBL,(12,FVIFLD),2,ALIGN=LEFT,MINUS=YES                      
         B     EXITOK                                                           
*                                                                               
         USING PTAELD,R3                                                        
DISNBL02 BAS   RE,GETPTA                                                        
         BE    *+6                                                              
         DC    H'0'                ER.. SOMETHING WRONG HERE                    
         CLI   PTAEL,PTAELQ                                                     
         BNE   DISNBL04                                                         
         ZAP   BCDUB,PTANET                                                     
         B     DISNBL06                                                         
*                                                                               
         USING BNDELD,R3                                                        
DISNBL04 ICM   R0,15,BNDAMNT                                                    
         CVD   R0,BCDUB                                                         
DISNBL06 CURED BCDUB,(12,FVIFLD),2,ALIGN=LEFT,MINUS=YES                         
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR COMMISSION BILLED AMOUNT                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CBLDTA   LA    RF,CBLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CBLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCBL) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY COMMISSION BILLED FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DISCBL   DS    0H                                                               
         ZAP   PL8,=P'0'           SET ZERO COMMISSION - NOT STORED             
         CLC   TRNKWORK,WC99       TEST W/C 99                                  
         BNE   DISCBL01                                                         
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         CLI   TRNLN,TRNLN2Q                                                    
         BNE   DISCBLA                                                          
         ZAP   PL8,TRNBLCOM                                                     
         B     DISCBL10                                                         
*                                                                               
         USING SCIELD,R3                                                        
DISCBLA  CLI   SCIEL,0                                                          
         BE    DISCBL10                                                         
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLI   SCITYPE,SCITCOMM                                                 
         BE    DISCBLB                                                          
         IC    R0,SCILN                                                         
         AR    R3,R0                                                            
         B     DISCBLA                                                          
DISCBLB  ZAP   PL8,SCIAMNT                                                      
         B     DISCBL10                                                         
*                                                                               
DISCBL01 CLC   BILLNUM,BCSPACES    TEST BILL NUMBER SPECIFIED                   
         BNE   DISCBL02                                                         
         ZAP   PL8,PA$COMBL                                                     
         B     DISCBL10                                                         
*                                                                               
         USING PTAELD,R3                                                        
DISCBL02 BAS   RE,GETPTA                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PTAEL,PTAELQ                                                     
         BNE   DISCBL10                                                         
         ZAP   PL8,PTARCOM                                                      
*                                                                               
DISCBL10 CURED PL8,(12,FVIFLD),2,ALIGN=LEFT,MINUS=YES                           
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BILL NUMBER                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
BNMDTA   LA    RF,BNMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BNMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBNM) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BILL NUMBERS FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING PTAELD,R3                                                        
DISBNM   DS    0H                                                               
         CLC   TRNKWORK,WC99       TEST W/C 99                                  
         BNE   *+14                                                             
         MVC   FVIFLD(L'TRNKREF),TRNKREF                                        
         B     EXITOK                                                           
*                                                                               
         CLC   BILLNUM,BCSPACES                                                 
         BE    DISBNM01                                                         
         BAS   RE,GETPTA                                                        
         MVC   FVIFLD(L'PTARBLNO),PTARBLNO                                      
         CLI   PTAEL,PTAELQ        IS IT A PTAEL                                
         BE    EXITOK                                                           
         MVC   FVIFLD(L'BNDBNO),BNDBNO-BNDELD(R3)                               
         BE    EXITOK                                                           
*                                                                               
DISBNM01 LA    R3,TRNRFST                                                       
         SR    R0,R0                                                            
         LA    R4,FVIFLD                                                        
DISBNM02 CLI   PTAEL,0             TEST E-O-R                                   
         BE    EXITOK                                                           
         CLI   PTAEL,PTAELQ        TEST NEW BILLING ELEMENT                     
         BNE   DISBNM04                                                         
         CLI   PTATYPE,PTATRAL     TEST REGULAR ALLOCATION                      
         BNE   DISBNM06                                                         
         TM    PTASTAT1,PTASPEND   TEST ALLOCATION STILL PENDING                
         BO    DISBNM06                                                         
         MVC   0(L'PTARBLNO,R4),PTARBLNO                                        
         LA    R4,L'PTARBLNO(R4)                                                
         B     DISBNM06                                                         
*                                                                               
         USING BNDELD,R3                                                        
DISBNM04 CLI   BNDEL,BNDELQ                                                     
         BNE   DISBNM06                                                         
         CLC   BNDBNO,BCSPACES                                                  
         BNH   DISBNM06                                                         
         MVC   0(L'BNDBNO,R4),BNDBNO                                            
         LA    R4,L'BNDBNO(R4)                                                  
*                                                                               
DISBNM06 LA    RF,FVIFLD+L'FVIFLD-6                                             
         CR    R4,RF               TEST GOING TO OVERFLOW                       
         BH    EXITOK              YES - THAT'S ENOUGH                          
         IC    R0,BNDLN                                                         
         AR    R3,R0                                                            
         B     DISBNM02                                                         
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BILL DATE                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
BDTDTA   LA    RF,BDTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BDTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBDT) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BILL DATES FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING PTAELD,R3                                                        
DISBDT   DS    0H                                                               
         CLC   TRNKWORK,WC99       TEST W/C 99                                  
         BNE   DISBDTA                                                          
         GOTO1 VDATCON,BOPARM,(1,TRNKDATE),(0,FVIFLD)                           
         B     EXITOK                                                           
*                                                                               
DISBDTA  CLC   BILLNUM,BCSPACES                                                 
         BE    DISBDT01                                                         
         BAS   RE,GETPTA                                                        
         MVC   BCHALF,PTARDATE     GET FROM PTAEL                               
         CLI   PTAEL,PTAELQ                                                     
         BE    *+10                                                             
         MVC   BCHALF,BNDDTE-BNDELD(R3)  OR BNDEL                               
         GOTO1 VDATCON,BOPARM,(2,BCHALF),(0,FVIFLD)                             
         B     EXITOK                                                           
*                                                                               
DISBDT01 LA    R3,TRNRFST                                                       
         SR    R0,R0                                                            
         LA    R4,FVIFLD                                                        
DISBDT02 CLI   PTAEL,0             TEST E-O-R                                   
         BE    EXITOK                                                           
         CLI   PTAEL,PTAELQ        TEST NEW BILLING ELEMENT                     
         BNE   DISBDT04                                                         
         CLI   PTATYPE,PTATRAL     TEST REGULAR ALLOCATION                      
         BNE   DISBDT06                                                         
         TM    PTASTAT1,PTASPEND   TEST ALLOCATION STILL PENDING                
         BO    DISBDT06                                                         
         CLC   BILLNUM,BCSPACES    TEST BILL NUMBER SPECIFIED                   
         BE    *+14                                                             
         CLC   BILLNUM,PTARBLNO    FIND MATCHING BILLING ELEMENT                
         BNE   DISBDT06                                                         
         GOTO1 VDATCON,BOPARM,(2,PTARDATE),(0,0(R4))                            
         LA    R4,6(R4)                                                         
         B     DISBDT06                                                         
*                                                                               
         USING BNDELD,R3                                                        
DISBDT04 CLI   BNDEL,BNDELQ                                                     
         BNE   DISBDT06                                                         
         CLC   BNDBNO,BCSPACES                                                  
         BNH   DISBDT06                                                         
         GOTO1 VDATCON,BOPARM,(2,BNDDTE),(0,0(R4))                              
         LA    R4,6(R4)                                                         
*                                                                               
DISBDT06 LA    RF,FVIFLD+L'FVIFLD-6                                             
         CR    R4,RF               TEST GOING TO OVERFLOW                       
         BH    EXITOK              YES - THAT'S ENOUGH                          
         IC    R0,BNDLN                                                         
         AR    R3,R0                                                            
         B     DISBDT02                                                         
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR REPORT ID                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
RIDDTA   LA    RF,RIDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RIDTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALRID) - ON SCREEN                     
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* VALIDATE REPORT ID FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALRID   DS    0H                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,3                                                         
         BNE   EXITNV                                                           
         L     R1,AREP                                                          
         USING REPD,R1                                                          
         MVC   REPSUBID(3),FVIFLD                                               
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DOWNLOAD OBJECT                                                     *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
DLOAD    LM    R0,R3,SVPARMS                                                    
         LA    RF,DWNTABL                                                       
         B     ITER                ITERATE TABLE                                
*                                                                               
DWNTABL  DC    AL1(DAPPCOL),AL1(0,0,0),AL4(DLNOW)                               
         DC    AL1(DSCREEN),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(EOT)                                                         
*                                                                               
GSFRP    USING FRPELD,GSFRPEL                                                   
DLNOW    XR    RE,RE               RE=NUMBER OF COLS                            
         LA    R1,LSVARCLM                                                      
         USING DCTABD,R1                                                        
         LA    RF,CLMS                                                          
*                                                                               
DLNOW02  XC    0(DCTABL,R1),0(R1)  CLEAR COLUMN ENTRY                           
         CLC   =AL2(EOT),0(RF)     END OF COLUMN LIST                           
         BE    DLNOW10                                                          
*                                                                               
         MVC   DCTFLD#,0(RF)       SET FIELD NUMBER                             
         MVC   DCTCOL#,2(RF)       SET COLUMN NUMBER                            
         LA    RE,1(RE)            INCREMENT NUMBER OF COLUMNS                  
*                                                                               
         LA    R1,DCTABL(R1)                                                    
         LA    RF,3(RF)                                                         
         B     DLNOW02                                                          
*                                                                               
DLNOW10  XC    0(DCTABL,R1),0(R1)                                               
         STH   RE,LSVARNUM                                                      
         MVI   GSFRP.FRPTYPE,FRPTDWN                                            
         B     EXITOK                                                           
*                                                                               
CLMS     DC    AL2(750),AL1(1)     CLIENT                                       
         DC    AL2(751),AL1(2)     PRODUCT                                      
         DC    AL2(752),AL1(3)     JOB                                          
         DC    AL2(753),AL1(4)     WORKCODE                                     
*        DC    AL2(754),AL1(5)     BILL NUMBER                                  
         DC    AL2(755),AL1(5)     SUPPLIER CODE                                
         DC    AL2(756),AL1(6)     SUPPLIER NAME                                
         DC    AL2(757),AL1(7)     DATE                                         
         DC    AL2(758),AL1(8)     REFERENCE                                    
         DC    AL2(767),AL1(9)     ORDER#                                       
         DC    AL2(759),AL1(10)    NARRATIVE                                    
         DC    AL2(760),AL1(11)    NET AMOUNT                                   
         DC    AL2(761),AL1(12)    COMMISSION                                   
         DC    AL2(762),AL1(13)    NET BILLED                                   
         DC    AL2(763),AL1(14)    COMMISSION BILLED                            
         DC    AL2(764),AL1(15)    BILL NUMBER                                  
         DC    AL2(765),AL1(16)    BILL DATE                                    
         DC    AL2(EOT)                                                         
         B     EXITOK                                                           
         DROP  GSFRP                                                            
         SPACE 2                                                                
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         DROP  R2                                                               
         SPACE 1                                                                
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING TRNRECD,R2                                                       
LAST     USING TRNRECD,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     DS    0H                                                               
*                                                                               
         CLC   THIS.TRNKACT,BCSPACES                                            
         BH    FLST10                                                           
         MVC   THIS.TRNKACT,CLPRJO                                              
         XC    THIS.TRNKWORK,THIS.TRNKWORK                                      
*                                                                               
         L     R4,AGOPBLK                                                       
         USING GOBLOCKD,R4         R4=A(GETOPT BLOCK)                           
         MVC   GOCTRY,CUCTRY                                                    
         MVC   GOADM,VDMGR                                                      
         MVC   GOAEXT,AGOXBLK      SET A(BLOCK EXTENSION)                       
         MVC   GOACOMP,AIO6                                                     
         MVC   GOALEDG,AIO7                                                     
         MVC   GOACLI,AIO8                                                      
         MVC   GOAPRO,AIO9                                                      
         MVC   GOAJOB,AIOA                                                      
         MVC   GOSELCUL,THIS.TRNKEY                                             
         MVC   GOSELCLI,BCSPACES                                                
         MVC   GOSELCLI(L'CLICODE),CLICODE                                      
         MVC   GOSELPRO,BCSPACES                                                
         MVC   GOSELPRO(L'PROCODE),PROCODE                                      
         MVC   GOSELJOB,BCSPACES                                                
         MVC   GOSELJOB(L'JOBCODE),JOBCODE                                      
         MVC   GOSELWC,BCSPACES                                                 
         CLC   WORKCODE,FFFF       ALL WORKCODES ?                              
         BE    *+10                                                             
         MVC   GOSELWC,WORKCODE                                                 
         GOTO1 VGETOPT,BCPARM,AGOPBLK                                           
         DROP  R4                                                               
*                                                                               
FLST10   MVC   IOKEY(L'TRNKEY),THIS.TRNKEY                                      
         L     R1,=AL4(IOHI+IOACCMST+IOREC)                                     
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     L     R1,=AL4(IOSEQ+IOACCMST+IOREC)                                    
         GOTO1 AIO                                                              
         BNE   NLSTX               END OF FILE                                  
*                                                                               
NLST02   CLC   IOKEY(TRNKWORK-TRNKEY),IOKEYSAV                                  
         BNE   NLSTX               END OF JOB                                   
         CLC   WORKCODE,FFFF       ALL WORKCODES ?                              
         BE    NLST06                                                           
         OC    WORKCODE,WORKCODE                                                
         BZ    NLST06                                                           
         CLC   IOKEY+(TRNKWORK-TRNKEY)(L'TRNKWORK),WORKCODE                     
         BE    NLST06              MATCHING WORKCODE                            
         BH    NLSTX               WORKCODE HIGH - END OF READING               
         CLC   =C'**',IOKEY+(TRNKWORK-TRNKEY)                                   
         BE    NLST04                                                           
         CLC   IOKEY+(TRNKWORK-TRNKEY)(L'TRNKWORK),BCSPACES                     
         BE    NLST                                                             
         MVC   THIS.TRNKWORK,WORKCODE                                           
         MVC   THIS.TRNKCULC(TRNKSBR+1-TRNKCULC),BCSPACES                       
         B     FLST10              START AGAIN WITH SELECTED WORKCODE           
*                                                                               
NLST04   CLC   IOKEY+(CHDKSPCS-CHDRECD)(L'CHDKSPCS),BCSPACES                    
         BNH   NLST08              MUST BE TRANSACTION                          
         GOTO1 VHELLO,BOPARM,(C'G',=C'ACCMST'),('OAMELQ',AIOREC),0              
         CLI   12(R1),0                                                         
         BNE   NLST                                                             
         L     RF,BOPARM+12                                                     
         CLC   WORKCODE,OAMWORK-OAMELD(RF)                                      
         BNE   NLST                                                             
         B     NLST10              REAL ORDERED WORKCODE MATCHES                
*                                                                               
NLST06   CLC   IOKEY+(CHDKSPCS-CHDRECD)(L'CHDKSPCS),BCSPACES                    
         BH    NLST10              MUST BE TRANSACTION                          
NLST08   GOTO1 VHELLO,BOPARM,(C'G',=C'ACCMST'),('CACELQ',AIOREC),0              
         CLI   12(R1),0                                                         
         BNE   NLST                                                             
         MVC   SUPNAME,BCSPACES    CLEAR PREVIOUS SUPPLIER NAME                 
         L     RF,12(R1)                                                        
         USING CACELD,RF                                                        
         SR    RE,RE                                                            
         IC    RE,CACLN                                                         
         SH    RE,=Y(CACLN1Q+1)                                                 
         EX    RE,*+4                                                           
         MVC   SUPNAME(0),CACNAME  SAVE THIS SUPPLIER NAME                      
         B     NLST                                                             
*                                                                               
NLST10   CLC   BILLNUM,BCSPACES    TEST BILL NUMBER SPECIFIED                   
         BE    NLST12                                                           
         LR    RF,R2                                                            
         L     R2,AIOREC                                                        
         BAS   RE,GETPTA           FIND MATCHING ELEMENT                        
         LR    R2,RF                                                            
         BNE   NLST                NO MATCH - GET NEXT                          
NLST12   MVC   THIS.TRNKEY,IOKEY                                                
         L     R4,AGOPBLK                                                       
         USING GOBLOCKD,R4                                                      
         CLC   IOKEY+(TRNKWORK-TRNKEY)(2),GOSELWC                               
         BE    NLST16                                                           
         CLC   IOKEY+(TRNKWORK-TRNKEY)(2),BCSPACES                              
         BNH   NLST16                                                           
         MVC   GOSELWC,IOKEY+(TRNKWORK-TRNKEY)                                  
         GOTO1 VGETOPT,BCPARM,AGOPBLK                                           
         DROP  R4                                                               
*                                                                               
NLST16   B     EXITOK                                                           
*                                                                               
NLSTX    B     EXITL                                                            
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* FIND PTAEL WITH MATCHING BILL NUMBER                                *         
* ON ENTRY R2=A(ACCMST RECORD)                                        *         
* ON EXIT CC=EQ  R3=A(MATCHING PTAEL)                                 *         
*         CC=NEQ R3=A(E-O-R)                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING PTAELD,R3                                                        
GETPTA   CLC   TRNKWORK,WC99       TEST W/C 99                                  
         BNE   *+18                                                             
         CLC   BILLNUM,TRNKREF     MATCH BILLNUM WITH REF                       
         BE    GPTAY                                                            
         B     GPTAN                                                            
*                                                                               
         LA    R3,TRNRFST                                                       
         CLI   0(R3),TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
GPTA02   CLI   PTAEL,0             TEST E-O-R                                   
         BE    GPTAN               NO MATCHING BILLING ELEMENT                  
         CLI   PTAEL,PTAELQ                                                     
         BNE   GPTA04                                                           
         CLI   PTATYPE,PTATRAL     TEST REGULAR BILLING                         
         BNE   GPTA08                                                           
         TM    PTASTAT1,PTASPEND   TEST PENDING                                 
         BO    GPTA08                                                           
         CLC   BILLNUM,PTARBLNO                                                 
         BE    GPTAY               FOUND A MATCH                                
         USING BNDELD,R3                                                        
GPTA04   CLI   BNDEL,BNDELQ        MAY BE OLD STYLE BILLING ELEMENT             
         BNE   GPTA08                                                           
         CLC   BNDBNO,BILLNUM      MAY BE OLD STYLE BILLING ELEMENT             
         BE    GPTAY                                                            
GPTA08   IC    R0,BNDLN                                                         
         AR    R3,R0                                                            
         B     GPTA02                                                           
GPTAN    LTR   RB,RB                                                            
         BR    RE                                                               
GPTAY    CR    RB,RB                                                            
         BR    RE                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
ULSJ     DC    C'SJ'                                                            
WC99     DC    C'99'                                                            
FFFF     DC    X'FFFF'                                                          
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#ALL,4                                                         
         DC    X'00'                                                            
         SPACE 2                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
PL8      DS    PL8                                                              
CLILEN   DS    XL1                 CLIENT CODE LENGTH (3 OR 5)                  
CLICODE  DS    CL5                                                              
PROCODE  DS    CL2                                                              
JOBCODE  DS    CL6                                                              
CLPRJO   DS    CL(L'TRNKACT)       CLIENT / PRODUCT / JOB                       
WORKCODE DS    CL2                 ZERO=NOT DEFINED, X'FFFF'=ALL                
BILLNUM  DS    CL6                                                              
SUPNAME  DS    CL36                SUPPLIER NAME                                
*                                                                               
DSLIST   DS    0D                                                               
AC@ALL   DS    CL4                                                              
*                                                                               
                                                                                
PRORBLK  DS    0C                                                               
       ++INCLUDE ACPRORATAD                                                     
*                                                                               
         SPACE 2                                                                
*        ACFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
*        FASYSLSTD                                                              
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
*        DDDDEQUS                                                               
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
*        CTMSGEQUS                                                              
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
*        FASELIST                                                               
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
*        FAPGMLST                                                               
         PRINT OFF                                                              
       ++INCLUDE FAPGMLST                                                       
         PRINT ON                                                               
*        FASYSFAC                                                               
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
*        DDSCANBLKD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*        DDCOMFACS                                                              
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*        FACTRY                                                                 
         PRINT OFF                                                              
       ++INCLUDE FACTRY                                                         
         PRINT ON                                                               
*        FACTRYEQUS                                                             
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACFIL41   08/16/00'                                      
         END                                                                    
