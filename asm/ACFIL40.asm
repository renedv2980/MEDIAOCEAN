*          DATA SET ACFIL40    AT LEVEL 056 AS OF 08/16/00                      
*&&      SET   NOP=N                                                            
*PHASE T62340A,*                                                                
         TITLE 'NEW DEMEDA INTERFACE FOR SCRIPT UPLOAD'                         
*                                                                               
* THE PROGRAM USES THE FOLLOWING I/O AREAS:                                     
* IO4 POSTING RECORD BUILD AREA                                                 
* IO5/6 4K WORKER FILE BUFFER                                                   
*                                                                               
FIL40    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,ACFIL40*,R5,R6,RR=RE                                           
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         LH    R7,=Y(TWSAVE-TWAD)                                               
         A     R7,ATWA                                                          
         USING SAVED,R7                                                         
         USING UKRECD,SWKEY                                                     
         USING WKRECD,SWREC+28                                                  
         USING SKBUFFD,SWSAVE                                                   
         SPACE 1                                                                
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
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
SFN      USING NAMELD,SFNAMEL                                                   
SZN      USING NAMELD,SZNAMEL                                                   
SFA      USING ADRELD,SFADREL                                                   
SZA      USING ADRELD,SZADREL                                                   
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
ITER02   ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
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
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OACTH),AL1(0,0,0),AL4(ACTH)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     GOTOX VDICTAT,BOPARM,C'LU  ',DCLIST,DSLIST                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ACTION CODE HANDLING OBJECT                                         *         
*                                                                     *         
* NTRY: P1        = ACTION HANDLING OBJECT                            *         
*       P2 BYTE 0 = SUB-VERB (ACTNTR OR ACTXIT OR 0)                  *         
*          BYTE 3 = VERB                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
ACTH     LA    R4,ACHTAB                                                        
         USING OBJTABD,R4                                                       
         ICM   RF,15,SVPARMS2                                                   
*                                                                               
ACH02    CLI   OBJVERB,EOT                                                      
         BE    ACH10                                                            
         CLM   RF,1,OBJVERB                                                     
         BE    *+12                MATCHED                                      
         LA    R4,OBJTABL(R4)                                                   
         B     ACH02               BUMP & LOOP                                  
*                                                                               
ACH04    CLM   RF,8,OBJSVB         MATCH ON SUB-VERB                            
         BE    ACH06                                                            
         CLI   OBJSVB,0                                                         
         BNE   *+6                                                              
         LR    R0,R4                                                            
         LA    R4,OBJTABL(R4)                                                   
         CLC   OBJVERB,SVPARMS2+3                                               
         BE    ACH04                                                            
         LTR   R4,R0               NO SUB-VERB MATCH USE 0 IF THERE             
         BZ    ACH10                                                            
*                                                                               
ACH06    TM    OBJIND1,OBJPRIV     PRIVATE?                                     
         BZ    ACH08                                                            
         L     RE,4(RD)            MAKE SURE INVOKED AT THIS LEVEL              
         L     RF,4(RE)                                                         
         CLC   16(4,RE),16(RF)                                                  
         BE    ACH08                                                            
         DC    H'0'                                                             
*                                                                               
ACH08    ICM   RF,15,OBJADR                                                     
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  R4                                                               
*                                                                               
ACH10    B     EXITH               NOT KNOWN AT THIS LEVEL                      
*                                                                               
ACHTAB   DC    AL1(ACTOPN,0,0,0),AL4(AOPEN)                                     
         DC    AL1(ACTPRC,0,0,0),AL4(APROCESS)                                  
         DC    AL1(ACTCLS,0,0,0),AL4(ACLOSE)                                    
*                                                                               
ACHTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* OPEN WORKER FILE TO POST TO & LOAD SCREENS FOR SCRIPT               *         
***********************************************************************         
         SPACE 1                                                                
AOPEN    GOTOX AGEN,BOPARM,OSCRN,SKEY LOAD KEY SCREEN                           
         MVI   GSSMPAGE,1                                                       
         GOTOX AGEN,BOPARM,OSCRN,SPAGE                                          
         OI    GSINDSL1,GSIXMNT    TURN ON MAINT SCREEN LOADED FLAG             
*                                                                               
         ZAP   STOTCASH,BCPZERO                                                 
         ZAP   STOTRECS,BCPZERO                                                 
*                                                                               
         MVC   FVMSGNO,=AL2(AI$EBADT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* OPEN WORKER FILE TO POST TO & LOAD SCREENS FOR SCRIPT               *         
***********************************************************************         
         SPACE 1                                                                
APROCESS GOTOX AGEN,BOPARM,ORECH,RVAL,AIOREC                                    
         BL    EXITL               MAKE SURE INPUT OK BEFORE OPEN FILE          
*                                                                               
         TM    SFLAG,FF            ALREADY INITIALISED?                         
         BO    PROC02              YES - RESTORE FILE                           
         MVI   SFLAG,FF                                                         
*                                                                               
         XC    SWKEY,SWKEY     *** BUILD WORKER FILE KEY & OPEN IT              
         XC    SWREC,SWREC                                                      
         MVC   UKUSRID,CUUSER      SET ORIGIN ID                                
         MVC   UKSYSPRG,PRGCODE    SYSTEM = ACCOUNT, PROGRAM = DEMEDA           
         MVC   UKDAY,BCTODAYP+2    DAY = TODAY PWOS                             
         MVC   UKCLASS,POSTING     CLASS = POSTING                              
         OI    UKFLAG,X'01'        SET ALLOW DUPLICATE KEYS                     
         OI    UKFLAG,X'10'        SET RETENTION DAYS                           
         OI    UKFLAG,X'08'        SET COMMENT                                  
*                                                                               
*        MVC   WKRETN,=H'14'       NUMBER OF DAYS TO RETAIN                     
         MVC   WKCOMNT,PRGNAME     SET COMMENT TO DEMEDA UPLOAD                 
*                                                                               
         GOTOX VDMGR,BOPARM,BUFFER,WKFILE,SWKEY,SWSAVE,AIO5                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                PROBLEM INITIALISING BUFFER                  
*                                                                               
         L     R4,AIO5                                                          
         ICM   R4,15,8(R4)                                                      
         ST    R4,CDISP            SAVE DISP TO SKBUFFD                         
*                                                                               
         GOTOX VDMGR,BOPARM,OPEN,WKFILE,SWKEY,SWREC,AIO5                        
         CLI   8(R1),0                                                          
         BE    PROC04                                                           
         DC    H'0'                PROBLEM OPENING WORKER FILE                  
*                                                                               
PROC02   GOTOX VDMGR,BOPARM,DMREAD,WKFILE,SKADDR,AIO5                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                PROBLEM READING BUFFER BACK                  
*                                                                               
         ICM   R4,15,CDISP         RESTORE ALL SKBUFFD INFORMATION              
         A     R4,AIO5                                                          
         MVC   SKLABEL+3(L'ASSIN),ASSIN  HAVE TO SET FASIN HERE                 
         MVC   0(L'SWSAVE,R4),SWSAVE                                            
*                                                                               
PROC04   CLI   XTYPE,X1TYPQ        BILLING INTERFACE                            
         BNE   *+12                                                             
         BAS   RE,TYPE1                                                         
         B     PROC06                                                           
*                                                                               
         CLI   XTYPE,X2TYPQ        PAYING INTERFACE                             
         BNE   *+12                                                             
         BAS   RE,TYPE2                                                         
         B     PROC06                                                           
*                                                                               
         CLI   XTYPE,X3TYPQ        SPECIAL MEDIA DISCOUNT INTERFACE             
         BNE   *+12                                                             
         BAS   RE,TYPE2                                                         
         B     PROC06                                                           
*                                                                               
         DC    H'0'                INTERFACE TYPE MESSED UP                     
*                                                                               
PROC06   L     R4,CDISP            SAVE OFF SKBUFFD INFORMATION                 
         A     R4,AIO5                                                          
         MVC   SWSAVE,0(R4)                                                     
*                                                                               
         GOTOX VDMGR,BOPARM,DMWRITE,WKFILE,SKADDR,AIO5                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                PROBLEM WRITING BUFFER TO DISK               
*                                                                               
         MVC   FVMSGNO,=AL2(GI$RCADD)                                           
         MVI   FVOMTYP,GTMINF      SET 'RECORD ADDED' MESSAGE                   
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* CLOSE BATCH TO POST                                                *          
**********************************************************************          
         SPACE 1                                                                
ACLOSE   GOTOX VDMGR,BOPARM,DMREAD,WKFILE,SKADDR,AIO5                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                PROBLEM READING BUFFER BACK                  
*                                                                               
         ICM   R4,15,CDISP         RESTORE ALL SKBUFFD INFORMATION              
         A     R4,AIO5             INTO BUFFER                                  
         MVC   SKLABEL+3(L'ASSIN),ASSIN  HAVE TO SET FASIN HERE                 
         MVC   0(L'SWSAVE,R4),SWSAVE                                            
*                                                                               
         L     R1,AIO4             BUILD POSTING TRAILER ELEMENT                
         LA    RE,4(R1)                                                         
         USING PSSUBFD,RE                                                       
         MVI   PSSBEL,PSSBELQ                                                   
         MVI   PSSBLEN,PSSBLENQ                                                 
         MVC   PSSBDESC,PRGNAME                                                 
         ZAP   PSSBRECS,STOTRECS                                                
         ZAP   PSSBCASH,STOTCASH                                                
         DROP  RE                                                               
*                                                                               
         XR    RF,RF                                                            
         IC    RF,1(RE)                                                         
         LA    RE,0(RF,RE)                                                      
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         SR    RE,R1                                                            
         STH   RE,0(R1)                                                         
*                                                                               
         GOTOX VDMGR,BOPARM,ADD,WKFILE,SWKEY,AIO4,AIO5                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                PROBLEM ADDING RECORD TO WORKER FILE         
*                                                                               
         GOTOX VDMGR,BOPARM,CLOSE,WKFILE,SWKEY,SWREC,AIO5                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                PROBLEM CLOSING WORKER FILE                  
*                                                                               
         MVC   FVMSGNO,=AL2(AI$BACLO)                                           
         MVI   FVOMTYP,GTMINF      SET 'BATCH CLOSED' MESSAGE                   
         B     EXITOK                                                           
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
         USING FDRRECD,R2                                                       
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
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   B     EXITOK                                                           
         EJECT                                                                  
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
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(00001),AL4(TYPDTA)    UPLOAD TYPE CODE                       
         DC    AL2(00002),AL4(AGYDTA)    AGENCY ID                              
         DC    AL2(00003),AL4(CLIDTA)    CLIENT CODE                            
         DC    AL2(00004),AL4(CNMDTA)    CLIENT NAME                            
         DC    AL2(00005),AL4(PRODTA)    PRODUCT CODE                           
         DC    AL2(00006),AL4(ORDDTA)    ORDER NUMBER                           
         DC    AL2(00007),AL4(OFFDTA)    OFFICE CODE                            
         DC    AL2(00008),AL4(BILDTA)    BILL NUMBER                            
         DC    AL2(00009),AL4(MCDDTA)    MEDIA CODE                             
         DC    AL2(00010),AL4(MNMDTA)    MEDIA NAME                             
         DC    AL2(00011),AL4(SUPDTA)    SUPPLIER CODE                          
         DC    AL2(00012),AL4(RF2DTA)    SECOND REFERENCE                       
         DC    AL2(00013),AL4(PSNDTA)    POSTING NARRATIVE                      
         DC    AL2(00014),AL4(TAXDTA)    TAX INDICATOR                          
         DC    AL2(00015),AL4(DSCDTA)    DISCOUNT PERCENTAGE                    
         DC    AL2(00016),AL4(SNMDTA)    SUPPLIER NAME                          
         DC    AL2(00017),AL4(AL1DTA)    ADDRESS 1                              
         DC    AL2(00018),AL4(AL2DTA)    ADDRESS 2                              
         DC    AL2(00019),AL4(AL3DTA)    ADDRESS 3                              
         DC    AL2(00020),AL4(AL4DTA)    ADDRESS 4                              
         DC    AL2(00021),AL4(BDTDTA)    BILL DATE                              
         DC    AL2(00022),AL4(DUEDTA)    DUE DATE                               
         DC    AL2(00023),AL4(NETDTA)    NET AMOUNT                             
         DC    AL2(00024),AL4(VATDTA)    VAT AMOUNT                             
         DC    AL2(00025),AL4(CSHDTA)    CASH DISCOUNT AMOUNT                   
         DC    AL2(00026),AL4(COMDTA)    COMMISSION AMOUNT                      
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL40    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3         VERB                                         
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
DFTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   MVC   SJ,BCCPYPRD         PRODUCTION U/L                               
         MVC   SR,BCCPYREC         DEBTORS U/L                                  
         MVC   CPYS1,BCCPYST1      STATUS BYTE 1 FOR VATICAN                    
*                                                                               
*                              *** READ SG LEDGER INFORMATION                   
         MVC   LGRRD,SG                                                         
         BAS   RE,READLGR          READLGR RETURNS LEDGER RECORD IN IO1         
*                                                                               
         GOTOX VHELLO,BOPARM,(C'G',GCFILNAM),('LDGELQ',AIO1),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                LEDGER RECORD MISSING LDGEL                  
*                                                                               
         ICM   R1,15,12(R1)                                                     
         USING LDGELD,R1                                                        
         MVC   SGOFFPOS,LDGOPOS    OFFICE POSITION IN KEY FOR SG                
         B     EXITOK                                                           
         DROP  R1                                                               
*                                                                               
*                              *** READ SJ LEDGER INFORMATION                   
         MVC   LGRRD,SJ                                                         
         BAS   RE,READLGR          READLGR RETURNS LEDGER RECORD IN IO1         
*                                                                               
         GOTOX VHELLO,BOPARM,(C'G',GCFILNAM),('ACLELQ',AIO1),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                LEDGER RECORD MISSING ACLEL                  
*                                                                               
         ICM   R1,15,12(R1)                                                     
         USING ACLELD,R1                                                        
         XC    LDGDISP,LDGDISP     CUMULATIVE DISPLACEMENTS                     
         LA    RE,ACLVALS                                                       
         IC    RF,ACLLN                                                         
         SRL   RF,4                WILL GIVE NUMBER OF LEVELS                   
         LA    R1,LDGDISP          FIRST ONE                                    
*                                                                               
INIT02   MVC   0(1,R1),0(RE)                                                    
         LA    RE,16(RE)                                                        
         LA    R1,1(R1)                                                         
         BCT   RF,INIT02           GET ALL OF THEM                              
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* READ LEDGER RECORD ROUTINE                                          *         
*                                                                     *         
* NTRY: UNIT/LEDGER PASSED IN LGRRD                                   *         
* EXIT: LEDGER RECORD RETURNED IN IO1                                 *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING LDGRECD,IOKEY                                                    
READLGR  NTR1                                                                   
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(L'LDGKUNT+L'LDGKLDG),LGRRD                               
         L     R1,=AL4(XORD+XOACCDIR+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                WHERE IS THE LEDGER RECORD                   
*                                                                               
         L     R1,=AL4(XOGET+XOACCMST+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    EXITOK                                                           
         DC    H'0'                DISK ERROR - LOOK AT IOERR                   
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS3         VERB IN R1                                   
         LA    RF,DLTABL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DLTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TYPE CODE                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TYPDTA   LA    RF,TYPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TYPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTYP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTYP)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TYPE CODE                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISTYP   MVC   FVIFLD(L'XTYPE),XTYPE                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A TYPE CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALTYP   MVC   XTYPE,FVIFLD                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR AGENCY ID                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
AGYDTA   LA    RF,AGYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
AGYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAGY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAGY)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN AGENCY ID                                                *         
***********************************************************************         
         SPACE 1                                                                
DISAGY   MVC   FVIFLD(L'XAGYCDE),XAGYCDE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN AGENGY ID                                               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING CT5REC,IOKEY                                                     
VALAGY   MVC   XAGYCDE,FVIFLD                                                   
*                                                                               
         XC    CT5KEY,CT5KEY       BUILD KEY OF ACCESS RECORD                   
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,XAGYCDE    AGENCY ALPHA FROM INPUT RECORD               
         ICM   R1,15,=AL4(XOCONFIL+XOREAD+XIO1)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                MISSING ACCESS REC FOR THIS ALPHA ID         
*                                                                               
         L     RF,AIO1                                                          
         LA    RF,CT5DATA-CT5REC(RF) POINT TO FIRST ELEMENT                     
         USING CTSYSD,RF                                                        
         XR    RE,RE               BUMP TO NEXT ELEMENT                         
*                                                                               
VAGY02   CLI   CTSYSEL,0           EOR?                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CTSYSEL,CTSYSELQ    TEST SYSTEM ELEMENT                          
         BNE   VAGY04                                                           
         CLI   CTSYSNUM,4          4 = MEDIA SYSTEM                             
         BNE   VAGY04                                                           
         MVC   MEDCPY,CTSYSAGB     SET COMPANY CODE - MEDIA                     
         B     EXITOK                                                           
*                                                                               
VAGY04   IC    RE,1(RF)            BUMP TO NEXT ELEMENT                         
         LA    RF,0(RE,RF)         ..                                           
         B     VAGY02              AND TRY AGAIN                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CLIENT CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
CLIDTA   LA    RF,CLITBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CLITBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCLI)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCLI)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CLIENT CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISCLI   MVC   FVIFLD(L'XCLICDE),XCLICDE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CLIENT CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALCLI   MVC   XCLICDE,FVIFLD                                                   
*                                                                               
         USING ACTRECD,IOKEY       1. ACCOUNT IS SET AT CLIENT LEVEL            
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN      ACCOUNT SYSTEM ID                            
         MVC   ACTKUNT(L'SJ),SJ    PRODUCTION U/L                               
         MVC   ACTKACT(L'XCLICDE),XCLICDE  CLIENT CODE SPACE FILLED             
         L     R1,=AL4(XOREAD+XOACCDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITNV              CAN'T FIND THE CLIENT                        
         L     R1,=AL4(XOGET+XOACCMST+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIO1                                                          
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         XR    RF,RF                                                            
         USING PPRELD,R1                                                        
*                                                                               
VCLI02   CLI   PPREL,0             EOR                                          
         BE    EXITOK              YES                                          
*                                                                               
         CLI   PPREL,PPRELQ        PPREL YET?                                   
         BE    *+16                                                             
         IC    RF,PPRLN                                                         
         LA    R1,0(RF,R1)         NO - KEEP LOOKING                            
         B     VCLI02                                                           
*                                                                               
         OC    PPRCOST,PPRCOST     COSTING ACCOUNT IN PPREL?                    
         BZ    EXITOK                                                           
         MVC   COSTACCT,PPRCOST    PPRCOST HOLDS COSTING ACCOUNT                
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CLIENT NAME                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CNMDTA   LA    RF,CNMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CNMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCNM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCNM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CLIENT NAME                                               *         
***********************************************************************         
         SPACE 1                                                                
DISCNM   MVC   FVIFLD(L'XCLINAM),XCLINAM                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CLIENT NAME                                              *         
***********************************************************************         
         SPACE 1                                                                
VALCNM   MVC   XCLINAM,FVIFLD                                                   
*                                                                               
         XC    SZNAMEL,SZNAMEL     BUILD SZ ACCOUNT NAME ELEMENT                
         MVI   SZN.NAMEL,NAMELQ                                                 
         MVC   SZN.NAMEREC(L'XCLINAM),XCLINAM                                   
         LA    R0,SZNAMEL                                                       
         LA    RF,SZNAMEL+L'SZNAMEL-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,1(RF)                                                         
         SR    RF,R0                                                            
         STC   RF,SZN.NAMLN                                                     
         B     EXITOK                                                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PRODUCT CODE                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
PRODTA   LA    RF,PROTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PROTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRO)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PRODUCT CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISPRO   MVC   FVIFLD(L'XPROCDE),XPROCDE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PRODUCT CODE                                             *         
***********************************************************************         
         SPACE 1                                                                
VALPRO   MVC   XPROCDE,FVIFLD                                                   
         B     EXITOK                                                           
*                                                                               
         XC    COSTACCT,COSTACCT   CLEAR COSTING ACCOUNT                        
*                                                                               
         USING ACTRECD,IOKEY       1. ACCOUNT IS SET AT CLIENT LEVEL            
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN      ACCOUNT SYSTEM ID                            
         MVC   ACTKUNT(L'SJ),SJ    PRODUCTION U/L                               
         MVC   ACTKACT(L'XCLICDE),XCLICDE  CLIENT CODE SPACE FILLED             
         L     R1,=AL4(XOREAD+XOACCDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITNV              CAN'T FIND THE CLIENT                        
         L     R1,=AL4(XOGET+XOACCMST+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIO1                                                          
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         XR    RF,RF                                                            
         USING PPRELD,R1                                                        
*                                                                               
VPRO02   CLI   PPREL,0             EOR                                          
         BE    EXITOK              YES                                          
*                                                                               
         CLI   PPREL,PPRELQ        PPREL YET?                                   
         BE    *+16                                                             
         IC    RF,PPRLN                                                         
         LA    R1,0(RF,R1)         NO - KEEP LOOKING                            
         B     VPRO02                                                           
*                                                                               
         OC    PPRCOST,PPRCOST     COSTING ACCOUNT IN PPREL?                    
         BZ    *+10                                                             
         MVC   COSTACCT,PPRCOST    PPRCOST HOLDS COSTING ACCOUNT                
*                                                                               
         CLI   XTYPE,X1TYPQ        PAYMENTS NEED COST ACCOUNT                   
         BNE   EXITOK                                                           
         OC    COSTACCT,COSTACCT                                                
         BNZ   EXITOK                                                           
         MVI   FVOMTYP,GTMSCR      MISSING COSTING ACCOUNT                      
         MVC   FVMSGNO,=AL2(AS$CSTAM)                                           
         B     EXITL                                                            
         POP   USING                                                            
         EJECT                                                                  
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
ORDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISORD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALORD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN ORDER NUMBER                                             *         
***********************************************************************         
         SPACE 1                                                                
DISORD   MVC   FVIFLD(L'XORDNUM),XORDNUM                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN ORDER NUMBER                                            *         
***********************************************************************         
         SPACE 1                                                                
VALORD   MVC   XORDNUM,FVIFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OFFICE CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
OFFDTA   LA    RF,OFFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
OFFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFF)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN OFFICE CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISOFF   MVC   FVIFLD(L'XOFFCDE),XOFFCDE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN OFFICE NUMBER                                           *         
***********************************************************************         
         SPACE 1                                                                
VALOFF   MVC   XOFFCDE,FVIFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
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
BILTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBIL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBIL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BILL NUMBER                                               *         
***********************************************************************         
         SPACE 1                                                                
DISBIL   MVC   FVIFLD(L'XBILNUM),XBILNUM                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A BILL NUMBER                                              *         
***********************************************************************         
         SPACE 1                                                                
VALBIL   MVC   XBILNUM,FVIFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MEDIA CODE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
MCDDTA   LA    RF,MCDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
MCDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMCD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMCD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A MEDIA CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
DISMCD   MVC   FVIFLD(L'XMEDCDE),XMEDCDE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A MEDIA CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
VALMCD   MVC   XMEDCDE,FVIFLD                                                   
*                                                                               
         XC    COMMACT,COMMACT     CLEAR ACCOUNTS                               
         XC    DISCACT,DISCACT                                                  
*                                                                               
         LA    RF,IOKEY            BUILD MEDIA RECORD PASSIVE KEY               
         USING MEDKEY,RF                                                        
         XC    MEDKEY,MEDKEY                                                    
         MVC   MEDKAM,MEDCPY                                                    
         MVI   MEDKTYP,MEDKPASQ                                                 
         MVC   MEDKCODE,XMEDCDE                                                 
         DROP  RF                                                               
*                                                                               
         L     R1,=AL4(XOREAD+XOMEDDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITNV                                                           
*                                                                               
         L     R1,=AL4(XOGET+XOMEDFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                GETREC PROBLEM - LOOK AT IOERR               
*                                                                               
         L     R2,AIO1                                                          
         USING MEDKEY,R2                                                        
         MVC   UFA,MEDREVAC        COSTING ACCOUNT CODE FOR BILL                
*                                                                               
         XR    RE,RE                                                            
         LA    RF,MEDEL                                                         
         USING DULA,RF                                                          
VMCD02   CLI   ULAEL,0             NO OVERRIDE - USE DEFAULT                    
         BE    VMCD16                                                           
*                                                                               
         CLI   ULAEL,ULAELQ        U/L/A OVERRIDE ELEMENT                       
         BNE   VMCD04                                                           
         CLI   ULATYPE,ULATINCQ    OVERRIDE INCOME ACCOUNT                      
         BE    VMCD08                                                           
         CLI   ULATYPE,ULATSMVQ    DISCOUNT ACCOUNT - SMVG                      
         BE    VMCD10                                                           
         CLI   ULATYPE,ULATDSCQ    DISCOUNT ACCOUNT                             
         BE    VMCD12                                                           
*                                                                               
VMCD04   CLI   ULAEL,CDSELQ                                                     
         BE    VMCD14                                                           
*                                                                               
VMCD06   IC    RE,ULALEN                                                        
         LA    RF,0(RE,RF)                                                      
         B     VMCD02                                                           
*                                                                               
VMCD08   MVC   COMMACT,ULAULA                                                   
         B     VMCD06                                                           
*                                                                               
         USING DULA,RF                                                          
VMCD10   CLI   XTYPE,X3TYPQ        SMVG IS DIFFERENT                            
         BNE   VMCD06                                                           
         MVC   DISCACT,ULAULA      DISCOUNT ACCOUNT OVERRIDE                    
         B     VMCD06                                                           
*                                                                               
VMCD12   CLI   XTYPE,X3TYPQ                                                     
         BE    VMCD06                                                           
         MVC   DISCACT,ULAULA      DISCOUNT ACCOUNT OVERRIDE                    
         B     VMCD06                                                           
*                                                                               
         USING CDSEL,RF                                                         
VMCD14   OC    DISCACT,DISCACT     DISCOUNT ACCOUNT OVERRIDDEN?                 
         BNZ   VMCD06              YES                                          
         MVC   DISCACT,CDSBILL     DEFAULT DISCOUNT ACCOUNT                     
         B     VMCD06                                                           
*                                                                               
VMCD16   OC    COMMACT,COMMACT     COMMISSION ACCOUNT?                          
         BNZ   EXITOK              YES                                          
*                                                                               
         MVC   COMMACT,BCSPACES    COMMISSION ACCOUNT FROM MEDCOMM SET          
         MVC   COMMACT(2),SI       AS THE DEFAULT                               
         MVC   COMMACT+2(L'MEDCOMM),MEDCOMM                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MEDIA NAME                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
MNMDTA   LA    RF,MNMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
MNMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMNM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMNM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A MEDIA NAME                                                *         
***********************************************************************         
         SPACE 1                                                                
DISMNM   MVC   FVIFLD(L'XMEDNAM),XMEDNAM                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A MEDIA NAME                                               *         
***********************************************************************         
         SPACE 1                                                                
VALMNM   MVC   XMEDNAM,FVIFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SUPPLIER NUMBER                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SUPDTA   LA    RF,SUPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SUPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSUP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSUP)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SUPPLIER NUMBER                                           *         
***********************************************************************         
         SPACE 1                                                                
DISSUP   MVC   FVIFLD(L'XSUPNUM),XSUPNUM                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SUPPLIER NUMBER                                          *         
***********************************************************************         
         SPACE 1                                                                
VALSUP   MVC   XSUPNUM,FVIFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SECOND REFERENCE                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
RF2DTA   LA    RF,RF2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RF2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRF2)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRF2)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SECOND REFERENCE                                          *         
***********************************************************************         
         SPACE 1                                                                
DISRF2   MVC   FVIFLD(L'XREF2),FVIFLD                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SECOND REFERENCE                                         *         
***********************************************************************         
         SPACE 1                                                                
VALRF2   MVC   XREF2,FVIFLD                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR POSTING NARRATIVE                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PSNDTA   LA    RF,PSNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PSNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPSN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPSN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A POSTING NARRATIVE                                         *         
***********************************************************************         
         SPACE 1                                                                
DISPSN   MVC   FVIFLD(L'XPSTNARR),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A POSTING NARRATIVE                                        *         
***********************************************************************         
         SPACE 1                                                                
VALPSN   MVC   XPSTNARR,FVIFLD                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TAX INDICATOR                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TAXDTA   LA    RF,TAXTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TAXTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTAX)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTAX)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TAX INDICATOR                                             *         
***********************************************************************         
         SPACE 1                                                                
DISTAX   MVC   FVIFLD(L'XTAXIND),FVIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A TAX INDICATOR                                            *         
***********************************************************************         
         SPACE 1                                                                
VALTAX   MVC   XTAXIND,FVIFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISCOUNT PERCENTAGE                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DSCDTA   LA    RF,DSCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DSCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDSC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DISCOUNT PERCENTAGE                                       *         
***********************************************************************         
         SPACE 1                                                                
DISDSC   MVC   FVIFLD(L'XDSCPCT),FVIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A DISCOUNT PERCENTAGE                                      *         
***********************************************************************         
         SPACE 1                                                                
VALDSC   MVC   XDSCPCT,FVIFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SUPPLIER NAME                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SNMDTA   LA    RF,SNMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SNMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSNM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSNM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SUPPLIER NAME                                             *         
***********************************************************************         
         SPACE 1                                                                
DISSNM   MVC   FVIFLD(L'XSUPNAM),FVIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SUPPLIER NAME                                            *         
***********************************************************************         
         SPACE 1                                                                
VALSNM   MVC   XSUPNAM,FVIFLD                                                   
*                                                                               
         XC    SFNAMEL,SFNAMEL     BUILD SF ACCOUNT NAME ELEMENT                
         MVI   SFN.NAMEL,NAMELQ                                                 
         MVC   SFN.NAMEREC(L'XSUPNAM),XSUPNAM                                   
         LA    R0,SFNAMEL                                                       
         LA    RF,SFNAMEL+L'SFNAMEL-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,1(RF)                                                         
         SR    RF,R0                                                            
         STC   RF,SFN.NAMLN                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADDRESS LINE 1                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
AL1DTA   LA    RF,AL1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
AL1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAL1)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAL1)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ADDRESS LINE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
DISAL1   MVC   FVIFLD(L'XSUPADR1),XSUPADR1                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ADDRESS LINE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
VALAL1   MVC   XSUPADR1,FVIFLD                                                  
*                                                                               
         XC    SZADREL,SZADREL     BUILD DUMMY SZADREL                          
         MVI   SZA.ADREL,ADRELQ                                                 
         MVI   SZA.ADRLN,3                                                      
         MVI   SZA.ADRNUM,0                                                     
*                                                                               
         XC    SFADREL,SFADREL     BUILD EMPTY SFADREL                          
         MVI   SFA.ADREL,ADRELQ                                                 
         MVI   SFA.ADRLN,3                                                      
         MVI   SFA.ADRNUM,0                                                     
*                                                                               
         CLI   FVILEN,0            ANY LINE 1 ADDRESS?                          
         BE    EXITOK              NO                                           
*                                                                               
         MVI   SFA.ADRNUM,1        SET 1 ADDRESS LINE                           
         MVI   SFA.ADRLN,ADRLN1Q                                                
         MVC   SFA.ADRADD1,XSUPADR1                                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADDRESS LINE 2                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
AL2DTA   LA    RF,AL2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
AL2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAL2)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAL2)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ADDRESS LINE 2                                              *         
***********************************************************************         
         SPACE 1                                                                
DISAL2   MVC   FVIFLD(L'XSUPADR2),XSUPADR2                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ADDRESS LINE 2                                             *         
***********************************************************************         
         SPACE 1                                                                
VALAL2   MVC   XSUPADR2,FVIFLD                                                  
*                                                                               
         CLI   FVILEN,0            ANY LINE 2 ADDRESS?                          
         BE    EXITOK              NO                                           
*                                                                               
         MVI   SFA.ADRNUM,2        SET 2 ADDRESS LINES                          
         MVI   SFA.ADRLN,ADRLN2Q                                                
         MVC   SFA.ADRADD2,XSUPADR2                                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADDRESS LINE 3                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
AL3DTA   LA    RF,AL3TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
AL3TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAL3)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAL3)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ADDRESS LINE 3                                              *         
***********************************************************************         
         SPACE 1                                                                
DISAL3   MVC   FVIFLD(L'XSUPADR3),XSUPADR3                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ADDRESS LINE 3                                             *         
***********************************************************************         
         SPACE 1                                                                
VALAL3   MVC   XSUPADR3,FVIFLD                                                  
*                                                                               
         CLI   FVILEN,0            ANY LINE 3 ADDRESS?                          
         BE    EXITOK              NO                                           
*                                                                               
         MVI   SFA.ADRNUM,3        SET 3 ADDRESS LINES                          
         MVI   SFA.ADRLN,ADRLN3Q                                                
         MVC   SFA.ADRADD3,XSUPADR3                                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADDRESS LINE 4                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
AL4DTA   LA    RF,AL4TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
AL4TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAL4)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAL4)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ADDRESS LINE 4                                              *         
***********************************************************************         
         SPACE 1                                                                
DISAL4   MVC   FVIFLD(L'XSUPADR4),XSUPADR4                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ADDRESS LINE 4                                             *         
***********************************************************************         
         SPACE 1                                                                
VALAL4   MVC   XSUPADR4,FVIFLD                                                  
*                                                                               
         CLI   FVILEN,0            ANY LINE 4 ADDRESS?                          
         BE    EXITOK              NO                                           
*                                                                               
         MVI   SFA.ADRNUM,4        SET 4 ADDRESS LINES                          
         MVI   SFA.ADRLN,ADRLN4Q                                                
         MVC   SFA.ADRADD4,XSUPADR4                                             
         B     EXITOK                                                           
         EJECT                                                                  
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
BDTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBDT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBDT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BILL DATE                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISBDT   MVC   FVIFLD(L'XBILDTE),XBILDTE                                        
         XC    FVIFLD(2),FVIFLD+4                                               
         XC    FVIFLD+4(2),FVIFLD                                               
         XC    FVIFLD(2),FVIFLD+4                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A BILL DATE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALBDT   MVC   XBILDTE,FVIFLD                                                   
         XC    XBILDTE(2),XBILDTE+4                                             
         XC    XBILDTE+4(2),XBILDTE                                             
         XC    XBILDTE(2),XBILDTE+4                                             
*                                                                               
         GOTOX VDATCON,BOPARM,(0,XBILDTE),(1,XBILDTEP)                          
*                                                                               
         GOTOX (RF),(R1),(5,0),(0,DATE)                                         
         MVC   MOS(1),DATE+1       YEAR                                         
         IC    RF,DATE+3                                                        
         N     RF,=X'0000000F'     1-9 IN RF NOW                                
         CLI   DATE+2,C'1'         WAS IT MONTH 10,11,12 ?                      
         BNE   *+8                                                              
         LA    RF,10(RF)           MAKE IT SO                                   
         BCTR  RF,0                                                             
         LA    RF,MONTH(RF)                                                     
         MVC   MOS+1(1),0(RF)      BUILDS MONTH OF SERVICE                      
*                                                                               
         CLI   XTYPE,X1TYPQ        PAYMENTS ARE DIFFERENT -                     
         BNE   EXITOK              AND IT`S A REAL PAIN IN THE BUTT             
*                                                                               
         MVC   DATE,XBILDTE        DATE OF BILL YYMMDD                          
         XR    RF,RF               YYMMDD IN DATE                               
         MVC   MOS(1),XBILDTE+1    YEAR                                         
         IC    RF,XBILDTE+3                                                     
         N     RF,=X'0000000F'     1-9 IN RF NOW                                
         CLI   XBILDTE+2,C'1'      WAS IT MONTH 10,11,12 ?                      
         BNE   *+8                                                              
         LA    RF,10(RF)           MAKE IT SO                                   
         BCTR  RF,0                                                             
         LA    RF,MONTH(RF)                                                     
         MVC   MOS+1(1),0(RF)      BUILDS MOS FOR BILL DATE                     
*                                                                               
VBDT02   XC    DATE(2),DATE+4                                                   
         XC    DATE+4(2),DATE                                                   
         XC    DATE(2),DATE+4      DATE HERE DDMMYY                             
         XC    BOWORK1,BOWORK1                                                  
         GOTOX VBMONVAL,BOPARM,(L'DATE,DATE),(9,ACOM),                 *        
               (CULANG,BOWORK1),(CUABIN,0)                                      
         CLI   BOWORK1+(BMOERR-BMONVALD),BMOEOKQ TEST FOR ERROR                 
         BE    EXITOK              NO ERROR                                     
*                                                                               
         CLI   MOS+1,C'C'          TAKE NEXT MOS                                
         BNE   *+12                                                             
         MVI   MOS+1,C'1'                                                       
         B     VBDT04                                                           
*                                                                               
         CLI   MOS+1,C'9'                                                       
         BNE   *+12                                                             
         MVI   MOS+1,C'A'                                                       
         B     VBDT04                                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,MOS+1                                                         
         LA    RF,1(RF)                                                         
         STC   RF,MOS+1                                                         
*                                                                               
VBDT04   CLI   MOS,C'9'            GET NEXT YEAR                                
         BNE   *+12                                                             
         MVI   MOS,C'0'                                                         
         B     VBDT06                                                           
*                                                                               
         IC    RF,MOS                                                           
         LA    RF,1(RF)                                                         
         STC   RF,MOS                                                           
*                                                                               
VBDT06   MVC   DATE(2),=CL2'01'   DDMMYY SET TO FIRST OF MONTH                  
         XC    DATE(2),DATE+4                                                   
         XC    DATE+4(2),DATE                                                   
         XC    DATE(2),DATE+4    DATE HERE YYMMDD                               
         XC    DATEX,DATEX                                                      
         GOTOX VADDAY,BOPARM,DATE,DATEX,31                                      
         MVC   DATE,DATEX        NEXT MONTH                                     
         B     VBDT02                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DUE DATE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DUEDTA   LA    RF,DUETBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DUETBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDUE)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDUE)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DUE DATE                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISDUE   MVC   FVIFLD(L'XDUEDTE),XDUEDTE                                        
         XC    FVIFLD(2),FVIFLD+4                                               
         XC    FVIFLD+4(2),FVIFLD                                               
         XC    FVIFLD(2),FVIFLD+4                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A DUE DATE                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALDUE   MVC   XDUEDTE,FVIFLD                                                   
         XC    XDUEDTE(2),XDUEDTE+4                                             
         XC    XDUEDTE+4(2),XDUEDTE                                             
         XC    XDUEDTE(2),XDUEDTE+4                                             
         GOTOX VDATCON,BOPARM,(0,XDUEDTE),(1,XDUEDTEP)                          
         B     EXITOK                                                           
         EJECT                                                                  
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
NETTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNET)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNET)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A NET AMOUNT                                                *         
***********************************************************************         
         SPACE 1                                                                
DISNET   CURED (P8,XNETAMT),(10,FVIFLD),0,DMCB=BODMCB                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A NET AMOUNT                                               *         
***********************************************************************         
         SPACE 1                                                                
VALNET   ZAP   XNETAMT,BCPZERO                                                  
         CLI   FVILEN,0            A BLANK FIELD CAN MEAN ZERO                  
         BE    EXITOK                                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTOX VCASHVAL,BOPARM,(C'0',FVIFLD),(RF)                               
         CLI   0(R1),FF                                                         
         BE    EXITNV                                                           
*                                                                               
         ZAP   XNETAMT,4(8,R1)                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR VAT AMOUNT                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
VATDTA   LA    RF,VATTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
VATTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISVAT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVAT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A VAT AMOUNT                                                *         
***********************************************************************         
         SPACE 1                                                                
DISVAT   CURED (P8,XVATAMT),(10,FVIFLD),0,DMCB=BODMCB                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A VAT AMOUNT                                               *         
***********************************************************************         
         SPACE 1                                                                
VALVAT   ZAP   XVATAMT,BCPZERO                                                  
         CLI   FVILEN,0            A BLANK FIELD CAN MEAN ZERO                  
         BE    EXITOK                                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTOX VCASHVAL,BOPARM,(C'0',FVIFLD),(RF)                               
         CLI   0(R1),FF                                                         
         BE    EXITNV                                                           
*                                                                               
         ZAP   XVATAMT,4(8,R1)                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CASH DISCOUNT AMOUNT                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CSHDTA   LA    RF,CSHTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CSHTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCSH)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCSH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CASH DISCOUNT AMOUNT                                      *         
***********************************************************************         
         SPACE 1                                                                
DISCSH   CURED (P8,XCSHDSC),(10,FVIFLD),0,DMCB=BODMCB                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CASH DISCOUNT AMOUNT                                     *         
***********************************************************************         
         SPACE 1                                                                
VALCSH   ZAP   XCSHDSC,BCPZERO                                                  
         CLI   FVILEN,0            A BLANK FIELD CAN MEAN ZERO                  
         BE    EXITOK                                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTOX VCASHVAL,BOPARM,(C'0',FVIFLD),(RF)                               
         CLI   0(R1),FF                                                         
         BE    EXITNV                                                           
*                                                                               
         ZAP   XCSHDSC,4(8,R1)                                                  
         CP    XCSHDSC,BCPZERO                                                  
         BE    EXITOK                                                           
*                                                                               
         OC    DISCACT,DISCACT     SET DISCOUNT ACCOUNT?                        
         BNZ   EXITOK              YES                                          
         MVC   FVMSGNO,=AL2(AE$DACCM)                                           
         B     EXITL               DISCOUNT ACCOUNT MISSING                     
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMMISSION AMOUNT                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
COMDTA   LA    RF,COMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
COMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A COMMISSION AMOUNT                                         *         
***********************************************************************         
         SPACE 1                                                                
DISCOM   CURED (P8,XCOMAMT),(10,FVIFLD),0,DMCB=BODMCB                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A COMMISSION AMOUNT                                        *         
***********************************************************************         
         SPACE 1                                                                
VALCOM   ZAP   XCOMAMT,BCPZERO                                                  
         CLI   FVILEN,0            A BLANK FIELD CAN MEAN ZERO                  
         BE    EXITOK                                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTOX VCASHVAL,BOPARM,(C'0',FVIFLD),(RF)                               
         CLI   0(R1),FF                                                         
         BE    EXITNV                                                           
*                                                                               
         ZAP   XCOMAMT,4(8,R1)                                                  
*                                                                               
         SP    XNETAMT,XCOMAMT     DEMEDA INTERFACE MATHS CORRECTION            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD TRANSACTION RECORDS FOR TYPE CODE 1 (BILLING INTERFACE)       *         
***********************************************************************         
         SPACE 1                                                                
TYPE1    NTR1  ,                   ALL POSTING RECORDS BUILT HERE               
         L     R2,AIO4                                                          
         LA    R2,4(R2)            FIRST 4 BYTES ARE LENGTH OF RECORD           
         USING PSHEADD,R2                                                       
         XC    PSHDEL(PSHEADL),PSHDEL                                           
         MVI   PSHDEL,PSHDELQ      HEADER CODE                                  
         LA    RF,PSHEADL                                                       
         STC   RF,PSHDLEN          HEADER LENGTH                                
*                                                                               
*                              *** POSTING TO SR(CLIENT)                        
*                                                                               
         MVC   PSHDACPY,CUABIN     FACPAK COMPANY ID                            
         MVC   PSHDAUNT(2),SR      U/L IS SR                                    
         MVC   PSHDAACT,BCSPACES                                                
         MVC   PSHDANAL,BCSPACES                                                
         MVC   PSHDAACT(L'XCLICDE),XCLICDE CLIENT CODE                          
         MVC   PSHDSCPY(3),BCSPACES                                             
         MVC   PSHDSACT,XMEDNAM    CONTRA IS BILLING SOURCE                     
         MVC   PSHDSBNM,BCSPACES                                                
*                                                                               
         LA    R3,PSHEADD+PSHEADL                                               
         USING TRNELD,R3       *** TRNEL                                        
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q       LENGTH                                       
         MVC   TRNREF,XBILNUM      BILL NUMBER                                  
         MVC   TRNDATE,XBILDTEP    BILL DATE - PWOS                             
         MVC   TRNANAL,BCSPACES                                                 
         MVC   TRNANAL(L'XOFFCDE),XOFFCDE  OFFICE CODE                          
         MVI   TRNTYPE,9           FROM THE DT02                                
         MVI   TRNSUB,0                                                         
         MVC   TRNBTCH(2),MOS                                                   
         MVC   TRNBTCH+2(L'AC@AUTO),AC@AUTO                                     
*                                                                               
         ZAP   TRNAMNT,XNETAMT     NET AMOUNT                                   
         AP    TRNAMNT,XVATAMT     + VAT                                        
         AP    TRNAMNT,XCOMAMT     + COMMISSION                                 
         SP    TRNAMNT,XCSHDSC     - CASH DISCOUNT                              
         OI    TRNSTAT,TRNSDR+TRNSAUTH  FORCE DEBIT, SET AUTHORISED             
*                                                                               
         LA    R4,TRNELD+TRNLN1Q                                                
         USING DUEELD,R4       *** DUEEL                                        
         MVI   DUEEL,DUEELQ                                                     
         MVI   DUELN,DUELNQ        LENGTH                                       
         GOTOX VDATCON,BOPARM,(1,XDUEDTEP),(2,DUEDATE)                          
*                                                                               
         LA    R4,DUELNQ(R4)                                                    
         USING SORELD,R4       *** SOREL                                        
         MVI   SOREL,SORELQ                                                     
         MVI   SORLN,SORMLNQ       LENGTH                                       
         MVI   SORSYS,SORSMED      SET SYSTEM TO 'MEDIA'                        
         MVC   SORMCPC,BCSPACES                                                 
         MVC   SORMMED,XMEDCDE                                                  
         MVC   SORMCLI,XCLICDE                                                  
         MVC   SORMPRO,XPROCDE                                                  
*                                                                               
         LA    R4,SORALNQ(R4)                                                   
         MVI   0(R4),0             PUT 0 AT EOR                                 
         BAS   RE,MAKEPOST         MAKE SR POSTING                              
*                                                                               
*                              *** POSTING TO SG(VAT A/C)                       
*                                                                               
         MVI   VTBYTE,VTCAOLUP     LOOK UP OUTPUT VAT ACCOUNT                   
         BAS   RE,VATCALL                                                       
*                                                                               
         MVC   PSHDACPY(L'VATACCT),VATACCT    VAT ACCOUNT                       
         MVC   PSHDSCPY,CUABIN     CONTRA ACCOUNT                               
         MVC   PSHDSUNT(2),SJ      SJ                                           
         MVC   PSHDSACT,BCSPACES                                                
         MVC   PSHDSACT(L'XCLICDE),XCLICDE CLIENT CODE                          
         XR    RE,RE                                                            
         IC    RE,SJCLI            L'CLIENT CODE                                
         LA    RE,PSHDSACT(RE)     PRODUCT CODE STRAIGHT AFTER CLIENT           
         MVC   0(L'XPROCDE,RE),XPROCDE                                          
         MVC   PSHDSBNM,BCSPACES                                                
*                                                                               
         USING TRNELD,R3       *** TRNEL                                        
         NI    TRNSTAT,255-TRNSDR  FORCE CREDIT                                 
         ZAP   TRNAMNT,XVATAMT                                                  
*                                                                               
         LA    R4,TRNELD+TRNLN1Q                                                
         USING SCIELD,R4       *** SCIEL                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q       LENGTH                                       
         MVI   SCITYPE,SCITGLEV    GROSS-VAT (=NET AMOUNT)                      
         ZAP   SCIAMNT,XNETAMT     NET AMOUNT                                   
         AP    SCIAMNT,XCOMAMT     + COMMISSION                                 
         SP    SCIAMNT,XCSHDSC     - CASH DISCOUNT                              
*                                                                               
         LA    R4,SCILN1Q(R4)                                                   
         MVI   0(R4),0             PUT 0 AT EOR                                 
         BAS   RE,MAKEPOST         MAKE THE SG POSTING                          
*                                                                               
*                              *** POSTING TO SI(MEDIA ACCOUNT)                 
*                                                                               
         MVC   PSHDAUNT(L'COMMACT),COMMACT    RESOLVED EARLIER                  
*                                                                               
         USING TRNELD,R3       *** TRNEL                                        
         ZAP   TRNAMNT,XCOMAMT     COMMISSION AMOUNT                            
*                                                                               
         LA    R4,TRNELD+TRNLN1Q                                                
         USING SCIELD,R4       *** SCIELS                                       
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITIVAT    ** VAT                                       
         ZAP   SCIAMNT,XVATAMT     VAT AMOUNT                                   
         LA    R4,SCILN1Q(R4)                                                   
*                                                                               
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS    ** GROSS BILLING                             
         ZAP   SCIAMNT,XNETAMT       NET AMOUNT                                 
         AP    SCIAMNT,XVATAMT     + VAT AMOUNT                                 
         AP    SCIAMNT,XCOMAMT     + COMMISSION AMOUNT                          
         SP    SCIAMNT,XCSHDSC     - DISCOUNT AMOUNT                            
*                                                                               
         LA    R4,SCILN1Q(R4)                                                   
         MVI   0(R4),0             PUT 0 AT EOR                                 
         BAS   RE,MAKEPOST         MAKE THE SI POSTING                          
*                                                                               
*                              *** POSTING TO SI(DISCOUNT)                      
*                                                                               
         MVC   PSHDAUNT(L'DISCACT),DISCACT    RESOLVED EARLIER                  
*                                                                               
         USING TRNELD,R3       *** TRNEL                                        
         ZAP   TRNAMNT,XCSHDSC     CASH DISCOUNT AMOUNT                         
         MP    TRNAMNT,=P'-1'      MAKE -VE                                     
         NI    TRNSTAT,X'FF'-(TRNSDR) SET CREDIT                                
*                                                                               
         LA    R4,TRNLN1Q(R3)                                                   
         MVI   0(R4),0             PUT 0 AT EOR                                 
         BAS   RE,MAKEPOST         MAKE THE SI POSTING                          
*                                                                               
*                              *** POSTING TO SZ(CLIENT)                        
*                                                                               
         MVC   PSHDAUNT(2),SZ      U/L IS SZ                                    
         MVC   PSHDAACT,BCSPACES                                                
         MVC   PSHDAACT(L'XMEDCDE),XMEDCDE MEDIA CODE                           
         MVC   PSHDAACT+L'XMEDCDE(L'XCLICDE),XCLICDE CLIENT CODE                
         MVC   PSHDSCPY(3),BCSPACES                                             
         MVC   PSHDSACT(L'XPROCDE),XPROCDE                                      
         XR    RF,RF                                                            
         IC    RF,SJPRO            L'CLIENT + L'PRODUCT                         
         XR    RE,RE                                                            
         IC    RE,SJCLI            L'CLIENT                                     
         SR    RF,RE               RF=L'PRODUCT ONLY                            
         LA    RF,PSHDSACT(RF)     CONTRA IS PRODUCT + ORDER NUMBER             
         MVC   0(L'XORDNUM,RF),XORDNUM                                          
         MVC   PSHDSBNM,BCSPACES   CLEAR SUB-NAME                               
*                                                                               
*        USING TRNELD,R3       *** TRNEL                                        
         NI    TRNSTAT,X'FF'-(TRNSDR) SET CREDIT                                
         MVI   TRNTYPE,9                                                        
         ZAP   TRNAMNT,XNETAMT     POST NET AMOUNT                              
*                                                                               
         LA    R4,TRNLN1Q(R3)                                                   
         XR    RF,RF           *** NAMEL                                        
         IC    RF,SZN.NAMLN        LENGTH OF THE NAMEL                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R4),SZNAMEL     MOVE IN THE NAMEL                            
*                                                                               
         LA    R4,1(RF,R4)                                                      
         CLI   SZA.ADRLN,3     *** ADREL                                        
         BE    T102                IGNORE EMPTY ADRELS                          
*                                                                               
         IC    RF,SZA.ADRLN                                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R4),SZADREL     MOVE IN ADREL                                
*                                                                               
         LA    R4,1(RF,R4)                                                      
         USING OTHELD,R4       *** OTHEL                                        
T102     CLC   XREF2,BCSPACES      SECOND NUMBER                                
         BE    T104                NO - NO OTHEL THEN                           
*                                                                               
         XC    0(OTHLN1Q,R4),0(R4)                                              
         MVI   OTHEL,OTHELQ                                                     
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM(L'XREF2),XREF2  INTERNAL REFERENCE NUMBER                 
*                                                                               
         LA    R4,OTHLN1Q(R4)                                                   
T104     MVI   0(R4),0             PUT A 0 AT THE END OF THE RECORD &           
         BAS   RE,MAKEPOST         MAKE THE SZ POSTING                          
*                                                                               
*                              *** POSTING TO 11 & 12 (COSTING)                 
*                                                                               
         LA    R4,TRNEL+TRNLN1Q    POST SHORT TRNEL ONLY TO THE                 
         MVI   0(R4),0             COSTING ACCOUNTS                             
*                                                                               
         MVC   PSHDACC,COSTACCT    COSTING ACCOUNT                              
         MVC   PSHDSBAC,CUABIN                                                  
         MVC   PSHDSUNT(L'UL12),UL12                                            
         MVC   PSHDSACT,BCSPACES                                                
         MVC   PSHDSACT(L'UFA),UFA                                              
         MVC   PSHDSBNM,BCSPACES                                                
*                                                                               
         OI    TRNSTAT,TRNSDR      POST DEBIT                                   
         ZAP   TRNAMNT,XCOMAMT     COMMISSION AMOUNT                            
         BAS   RE,MAKEPOST         POST COSTING : 12                            
*                                                                               
         XC    PSHDAUNT(14),PSHDSUNT                                            
         XC    PSHDSUNT(14),PSHDAUNT                                            
         XC    PSHDAUNT(14),PSHDSUNT                                            
         NI    TRNSTAT,255-TRNSDR  POST CREDIT                                  
*                                                                               
         BAS   RE,MAKEPOST         POST 12 : COSTING                            
*                                                                               
         MVC   PSHDACC,COSTACCT                                                 
         MVC   PSHDSUNT(L'UL11),UL11                                            
         MVC   PSHDSACT,BCSPACES                                                
         MVC   PSHDSACT(L'UFA),UFA                                              
         MVC   PSHDSBNM,BCSPACES                                                
         OI    TRNSTAT,TRNSDR      POST DEBIT                                   
         ZAP   TRNAMNT,XNETAMT     BILLING - LESS VAT                           
         AP    TRNAMNT,XCOMAMT     + COMMISSION AMOUNT                          
*                                                                               
         BAS   RE,MAKEPOST         POST COSTING : 11                            
*                                                                               
         XC    PSHDAUNT(14),PSHDSUNT                                            
         XC    PSHDSUNT(14),PSHDAUNT                                            
         XC    PSHDAUNT(14),PSHDSUNT                                            
         NI    TRNSTAT,255-TRNSDR  POST CREDIT                                  
*                                                                               
         BAS   RE,MAKEPOST         POST 11 : COSTING                            
         B     EXITOK              FINISHED                                     
         EJECT                                                                  
***********************************************************************         
* BUILD TRANSACTION RECORDS FOR TYPE CODE 2 (PAYING INTERFACE)        *         
***********************************************************************         
         SPACE 1                                                                
TYPE2    NTR1  ,                   POSTING RECORDS BUILT IN IO4                 
         L     R2,AIO4                                                          
         LA    R2,4(R2)            FIRST 4 CHARACTERS ARE LENGTH                
         USING PSHEADD,R2                                                       
         MVC   PSHDEL(PSHEADL),BCSPACES                                         
         MVI   PSHDEL,PSHDELQ      HEADER CODE                                  
         LA    RF,PSHEADL                                                       
         STC   RF,PSHDLEN          HEADER LENGTH                                
*                                                                               
*                              *** POSTING TO SF(MEDIA+A+SUPPLIER)              
*                                                                               
         MVC   PSHDACPY,CUABIN     FACPAK COMPANY ID                            
         MVC   PSHDAUNT(2),SF      U/L IS SF                                    
         MVC   PSHDAACT,BCSPACES                                                
         MVC   PSHDANAL,BCSPACES                                                
         MVC   PSHDAACT(L'XMEDCDE),XMEDCDE   MEDIA ACCOUNT                      
         MVI   PSHDAACT+L'XMEDCDE,C'A'                                          
         MVC   PSHDAACT+L'XMEDCDE+1(L'XSUPNUM),XSUPNUM                          
         MVC   PSHDSCPY(3),BCSPACES C/U/L IS EMPTY                              
         MVC   PSHDSACT,BCSPACES   CONTRA IS SUPPLIER NUMBER                    
         MVC   PSHDSACT(L'XSUPNUM),XSUPNUM                                      
         MVC   PSHDSBNM(L'XSUPNAM),XSUPNAM                                      
*                                                                               
         LA    R3,PSHEADD+PSHEADL                                               
         USING TRNELD,R3       *** TRNEL                                        
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,(TRNLN1Q+L'XPSTNARR)  LENGTH INCLUDING NARRATIVE           
         MVC   TRNREF,XBILNUM      BILL NUMBER                                  
         MVC   TRNDATE,XBILDTEP    BILL DATE - PWOS                             
         MVI   TRNSTAT,TRNSAUTH    SET AUTHORISED                               
         MVC   TRNANAL,BCSPACES                                                 
         MVC   TRNANAL(L'XOFFCDE),XOFFCDE  OFFICE CODE                          
         MVI   TRNSUB,0                                                         
         MVI   TRNTYPE,9                                                        
         MVC   TRNBTCH,BCSPACES                                                 
         MVC   TRNBTCH(2),MOS                                                   
         MVC   TRNBTCH+2(L'AC@AUTO),AC@AUTO                                     
         ZAP   TRNAMNT,XNETAMT       NET                                        
         AP    TRNAMNT,XVATAMT     + VAT                                        
         MVC   TRNNARR(L'XPSTNARR),XPSTNARR  POSTING NARRATIVE                  
*                                                                               
         LA    R4,TRNLN1Q+L'XPSTNARR(R3)                                        
         XR    RF,RF           *** NAMEL                                        
         IC    RF,SFN.NAMLN                                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R4),SFNAMEL     MOVE IN NAMEL                                
         LA    R4,1(RF,R4)                                                      
*                                                                               
         IC    RF,SFA.ADRLN    *** ADREL                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R4),SFADREL     MOVE IN THE ADREL                            
*                                                                               
         LA    R4,1(RF,R4)         NEXT FREE SPACE                              
         USING OTHELD,R4       *** OTHEL                                        
         CLC   XREF2,BCSPACES      SECOND REFERENCE NUMBER?                     
         BE    T202                NO - THEN NO OTHEL                           
*                                                                               
         XC    0(OTHLN1Q,R4),0(R4)                                              
         MVI   OTHEL,OTHELQ                                                     
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM(L'XREF2),XREF2                                            
         LA    R4,OTHLN1Q(R4)                                                   
         SPACE 1                                                                
*                                                                               
         LA    R4,OTHLN1Q(R4)                                                   
         USING DUEELD,R4       *** DUEEL                                        
T202     MVI   DUEEL,DUEELQ                                                     
         MVI   DUELN,DUELNQ                                                     
         GOTOX VDATCON,BOPARM,(1,XDUEDTEP),(2,DUEDATE)                          
*                                                                               
         LA    R4,DUELNQ(R4)                                                    
         USING RATELD,R4       *** RATEL                                        
         MVI   RATEL,RATEVATQ      ELEMENT CODE                                 
         MVI   RATLN,RATLNQ                                                     
*                                                                               
         MVI   VTBYTE,VTCAILUP                                                  
         BAS   RE,VATCALL          GET THE VAT ACCOUNT & VAT RATE               
         MVC   RATRATE,VATRATE                                                  
*                                                                               
         LA    R4,RATLNQ(R4)                                                    
         USING SCIELD,R4       *** SCIEL                                        
         MVI   SCIEL,SCIELQ        ELEMENT CODE                                 
         MVI   SCILN,SCILN1Q       LENGTH                                       
         MVI   SCITYPE,SCITCDSC    CASH DISCOUNT                                
         ZAP   SCIAMNT,XCSHDSC     AMOUNT                                       
*                                                                               
         LA    R4,SCILN1Q(R4)                                                   
         MVI   0(R4),0             PUT A 0 AT THE END OF THE RECORD &           
         BAS   RE,MAKEPOST         MAKE THE SF POSTING                          
*                                                                               
*                              *** POSTING TO SG(VAT)                           
*                                                                               
         MVC   PSHDSBAC,PSHDACC    CONTRA IS THE SF ACCOUNT                     
         MVC   PSHDACPY(L'VATACCT),VATACCT    VAT ACCOUNT                       
*                                                                               
         USING TRNELD,R3       *** TRNEL                                        
         OI    TRNSTAT,TRNSDR      SET DEBIT                                    
         ZAP   TRNAMNT,XVATAMT     VAT AMOUNT                                   
         LA    R4,TRNLN1Q+L'XPSTNARR(R3)                                        
*                                                                               
         LA    R4,TRNLN1Q+L'XPSTNARR(R3)                                        
         USING SCIELD,R4       *** SCIEL                                        
         MVI   SCIEL,SCIELQ        ELEMENT CODE                                 
         MVI   SCILN,SCILN1Q       LENGTH                                       
         MVI   SCITYPE,SCITGLEV    GROSS-VAT (=NET AMOUNT)                      
         ZAP   SCIAMNT,XNETAMT     NET AMOUNT                                   
*                                                                               
         LA    R4,SCILN1Q(R4)                                                   
         MVI   0(R4),0                                                          
         BAS   RE,MAKEPOST         MAKE THE SG POSTING                          
*                                                                               
*                              *** POSTING TO SZ(CLI)                           
*                                                                               
         MVC   PSHDAUNT(2),SZ      U/L IS SZ                                    
         MVC   PSHDAACT,BCSPACES                                                
         MVC   PSHDAACT(L'XMEDCDE),XMEDCDE MEDIA CODE                           
         MVC   PSHDAACT+L'XMEDCDE(L'XCLICDE),XCLICDE CLIENT CODE                
         MVC   PSHDSCPY(3),BCSPACES C/U/L IS EMPTY                              
         MVC   PSHDSACT(L'XPROCDE),XPROCDE                                      
         XR    RF,RF                                                            
         IC    RF,SJPRO            L'CLIENT + L'PRODUCT                         
         XR    RE,RE                                                            
         IC    RE,SJCLI            L'CLIENT                                     
         SR    RF,RE               RF=L'PRODUCT ONLY                            
         LA    RF,PSHDSACT(RF)     CONTRA IS PRODUCT + ORDER NUMBER             
         MVC   0(L'XORDNUM,RF),XORDNUM                                          
         MVC   PSHDSBNM,BCSPACES   CLEAR SUB-NAME                               
*                                                                               
         USING TRNELD,R3       *** TRNEL                                        
         OI    TRNSTAT,TRNSDR      SET DEBIT                                    
         MVI   TRNTYPE,9                                                        
         ZAP   TRNAMNT,XNETAMT     POST NET AMOUNT                              
*                                                                               
         LA    R4,TRNLN1Q+L'XPSTNARR(R3)                                        
         XR    RF,RF           *** NAMEL                                        
         IC    RF,SZN.NAMLN                                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R4),SZNAMEL     MOVE IN THE NAMEL                            
*                                                                               
         LA    R4,1(RF,R4)                                                      
         CLI   SZA.ADRLN,3     *** ADREL                                        
         BE    T204                IGNORE DUMMY ADRELS                          
         IC    RF,SZA.ADRLN                                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R4),SZADREL     MOVE IN THE ADREL                            
*                                                                               
         LA    R4,1(RF,R4)         NEXT FREE SPACE                              
         USING OTHELD,R4       *** OTHEL                                        
T204     CLC   XREF2,BCSPACES      SECOND REF?                                  
         BE    T206                NO - IGNORE OHTEL                            
*                                                                               
         XC    0(OTHLN1Q,R4),0(R4)                                              
         MVI   OTHEL,OTHELQ                                                     
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM(L'XREF2),XREF2  INTERNAL REFERENCE NUMBER                 
*                                                                               
         LA    R4,OTHLN1Q(R4)                                                   
T206     MVI   0(R4),0                                                          
         BAS   RE,MAKEPOST         MAKE THE SZ POSTING                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD TRANSACTION RECORDS FOR TYPE CODE 3 (SPECIAL MEDIA DISCOUNT)  *         
***********************************************************************         
         SPACE 1                                                                
TYPE3    NTR1  ,                                                                
         L     R2,AIO4                                                          
         LA    R2,4(R2)            FIRST 4 BYTES ARE LENGTH OF RECORD           
         USING PSHEADD,R2                                                       
         MVC   PSHDEL(PSHEADL),BCSPACES                                         
         MVI   PSHDEL,PSHDELQ      HEADER CODE                                  
         LA    RF,PSHEADL                                                       
         STC   RF,PSHDLEN          HEADER LENGTH                                
*                                                                               
*                              *** POSTING TO SF(MEDIA + A + SUPPLIER)          
*                                                                               
         MVC   PSHDACPY,CUABIN     FACPAK COMPANY ID                            
         MVC   PSHDAUNT(2),SF      U/L IS SF                                    
         MVC   PSHDAACT,BCSPACES                                                
         MVC   PSHDANAL,BCSPACES                                                
         MVC   PSHDAACT(L'XMEDCDE),XMEDCDE   MEDIA ACCOUNT                      
         MVI   PSHDAACT+L'XMEDCDE,C'A'                                          
         MVC   PSHDAACT+L'XMEDCDE+1(L'XSUPNUM),XSUPNUM                          
         MVC   PSHDSCPY(3),BCSPACES C/U/L IS EMPTY                              
         MVC   PSHDSACT,BCSPACES   CONTRA IS SUPPLIER NUMBER                    
         MVC   PSHDSACT(L'XSUPNUM),XSUPNUM                                      
         MVC   PSHDSCPY(3),BCSPACES C/U/L IS EMPTY                              
         MVC   PSHDSACT(L'XSUPNUM),XSUPNUM CONTRA IS SUPPLIER CODE              
         MVC   PSHDSBNM(L'XSUPNAM),XSUPNAM                                      
*                                                                               
         LA    R3,PSHEADD+PSHEADL                                               
         USING TRNELD,R3       *** TRNEL                                        
         MVI   TRNEL,TRNELQ        X'44' TRANSACTION ELEMENT                    
         MVI   TRNLN,(TRNLN1Q+L'XPSTNARR)  LENGTH INCLUDING NARRATIVE           
         MVC   TRNREF,XBILNUM      BILL NUMBER                                  
         MVC   TRNDATE,XBILDTEP    BILL DATE - PWOS                             
         MVI   TRNSTAT,TRNSAUTH    SET CREDIT + AUTHORISED                      
         MVC   TRNANAL,BCSPACES                                                 
         MVC   TRNANAL(L'XOFFCDE),XOFFCDE  OFFICE CODE                          
         MVI   TRNTYPE,9                                                        
         MVI   TRNSUB,0                                                         
         ZAP   TRNAMNT,XNETAMT       SMVG AMOUNT                                
         AP    TRNAMNT,XVATAMT     + VAT                                        
         MVC   TRNBTCH(2),MOS                                                   
         MVC   TRNBTCH+2(L'AC@AUTO),AC@AUTO                                     
         MVC   TRNNARR(L'XPSTNARR),XPSTNARR  POSTING NARRATIVE                  
*                                                                               
         LA    R4,TRNLN1Q+L'XPSTNARR(R3)                                        
         XR    RF,RF           *** NAMEL                                        
         IC    RF,SFN.NAMLN        LENGTH OF THE NAMEL                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R4),SFNAMEL     MOVE IN THE NAMEL                            
*                                                                               
         LA    R4,1(RF,R4)                                                      
         IC    RF,SFA.ADRLN    *** ADREL                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R4),SFADREL     MOVE IN THE ADREL                            
*                                                                               
         LA    R4,1(RF,R4)                                                      
         USING OTHELD,R4       *** OTHEL                                        
         CLC   XREF2,BCSPACES      SECOND REF?                                  
         BE    T302                NO - NO OTHEL THEN                           
*                                                                               
         XC    0(OTHLN1Q,R4),0(R4)                                              
         MVI   OTHEL,OTHELQ                                                     
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM(L'XREF2),XREF2                                            
*                                                                               
         LA    R4,OTHLN1Q(R4)                                                   
         USING DUEELD,R4       *** DUEEL                                        
T302     MVI   DUEEL,DUEELQ        ELEMENT CODE                                 
         MVI   DUELN,DUELNQ        LENGTH                                       
         GOTOX VDATCON,BOPARM,(1,XDUEDTEP),(2,DUEDATE)                          
*                                                                               
         LA    R4,DUELNQ(R4)                                                    
         USING SCIELD,R4       *** SCIEL                                        
         MVI   SCIEL,SCIELQ        ELEMENT CODE                                 
         MVI   SCILN,SCILN1Q       LENGTH                                       
         MVI   SCITYPE,SCITCDSC    CASH DISCOUNT                                
         ZAP   SCIAMNT,XCSHDSC     AMOUNT                                       
*                                                                               
         LA    R4,SCILN1Q(R4)                                                   
         USING RATELD,R4       *** RATEL                                        
         MVI   RATEL,RATEVATQ      ELEMENT CODE                                 
         MVI   RATLN,RATLNQ                                                     
         MVI   VTBYTE,VTCAILUP                                                  
         BAS   RE,VATCALL          GO GET A VAT ACCOUNT                         
         MVC   RATRATE,VATRATE                                                  
*                                                                               
         LA    R4,RATLNQ(R4)                                                    
         MVI   0(R4),0                                                          
         BAS   RE,MAKEPOST         MAKE THE SF POSTING                          
*                                                                               
*                              *** POSTING TO SG(VAT)                           
*                                                                               
         MVC   PSHDSBAC,PSHDACC    CONTRA IS THE SF ACCOUNT                     
         MVC   PSHDACPY(L'VATACCT),VATACCT    VAT ACCOUNT                       
*                                                                               
         USING TRNELD,R3       *** TRNEL                                        
         OI    TRNSTAT,TRNSDR      FORCE DEBIT                                  
         ZAP   TRNAMNT,XVATAMT     VAT AMOUNT                                   
*                                                                               
         LA    R4,TRNLN1Q+L'XPSTNARR(R3)                                        
         USING SCIELD,R4       *** SCIEL                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGLEV    GROSS-VAT (=NET AMOUNT)                      
         ZAP   SCIAMNT,XNETAMT     NET AMOUNT                                   
*                                                                               
         LA    R4,SCILN1Q(R4)                                                   
         MVI   0(R4),0             PUT A 0 AT THE END OF THE RECORD &           
         BAS   RE,MAKEPOST         MAKE THE SG POSTING                          
*                                                                               
*                              *** POSTING TO SI(MEDIA ACCOUNT)                 
*                                                                               
         MVC   PSHDAUNT(L'DISCACT),DISCACT                                      
*                                                                               
         USING TRNELD,R3       *** TRNEL                                        
         ZAP   TRNAMNT,XNETAMT     SMVG AMOUNT                                  
         MP    TRNAMNT,=P'-1'                                                   
         NI    TRNSTAT,255-TRNSDR  FORCE CREDIT                                 
*                                                                               
         LA    R4,TRNLN1Q+L'XPSTNARR(R3)                                        
         USING SCIELD,R4       *** SCIELS                                       
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITIVAT    ** VAT                                       
         ZAP   SCIAMNT,XVATAMT     VAT AMOUNT                                   
         LA    R4,SCILN1Q(R4)                                                   
         SPACE 1                                                                
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS    ** GROSS BILLING =                           
         ZAP   SCIAMNT,XNETAMT       SMVG AMOUNT                                
         AP    SCIAMNT,XVATAMT     + VAT AMOUNT                                 
         AP    SCIAMNT,XCOMAMT     + COMMISSION AMOUNT                          
         SP    SCIAMNT,XCSHDSC     - DISCOUNT AMOUNT                            
*                                                                               
         LA    R4,SCILN1Q(R4)                                                   
         MVI   0(R4),0                                                          
         BAS   RE,MAKEPOST         MAKE THE SI POSTING                          
         B     EXITOK                                                           
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* WRITE WKFILE RECORDS                                                *         
***********************************************************************         
         SPACE 1                                                                
MAKEPOST NTR1  ,                                                                
         L     R2,AIO4             RECORD BUILT IN IO4                          
         LA    R2,4(R2)            FIRST 4 BYTES HOLD LENGTH                    
         XR    RF,RF                                                            
*                                                                               
POST02   CLI   0(R2),0             EOR                                          
         BE    POST06              YES                                          
         CLI   0(R2),TRNELQ        TRANSACTION ELEMENT?                         
         BNE   POST04              NO                                           
*                                                                               
         USING TRNELD,R2                                                        
         TM    TRNSTAT,TRNSDR      ONLY ADD CREDITS                             
         BO    POST04                                                           
         AP    STOTCASH,TRNAMNT    ADD TO TOTAL POSTING AMOUNT                  
         DROP  R2                                                               
*                                                                               
POST04   ICM   RF,1,1(R2)          ZERO LENGTH ELEMENT CHECK                    
         BNZ   *+12                                                             
         MVI   0(R2),0                                                          
         B     POST06                                                           
*                                                                               
         LA    R2,0(R2,RF)                                                      
         B     POST02                                                           
*                                                                               
POST06   LA    R2,1(R2)            GET LENGTH OF RECORD TO POST                 
         L     RF,AIO4                                                          
         SR    R2,RF                                                            
         SLL   R2,16                                                            
         ST    R2,0(RF)            SAVE IT AT FRONT OF RECORD                   
*                                                                               
         AP    STOTRECS,=P'1'      INCREMENT POSTED RECORD COUNT                
         GOTOX VDMGR,BOPARM,ADD,WKFILE,SWKEY,AIO4,AIO5                          
         CLI   8(R1),0                                                          
         BE    EXITOK                                                           
         DC    H'0'                PROBLEM ADDING RECORD TO WORKER FILE         
         SPACE 2                                                                
***********************************************************************         
* DO A VATICAN CALL                                                   *         
***********************************************************************         
         SPACE 1                                                                
VATCALL  NTR1  ,                   BUILD VATICAN CALL                           
         XC    BOWORK1,BOWORK1                                                  
         LA    R1,BOWORK1                                                       
         USING VTCD,R1                                                          
         MVC   VTCACTN,VTBYTE                                                   
         MVC   VTCCPY,CUABIN       COMPANY CODE                                 
         MVC   VTCOFFC(L'XOFFCDE),XOFFCDE OFFICE CODE                           
         MVC   VTCSGOPO,SGOFFPOS   SG OFFICE POSITION                           
         MVC   VTCCOMF,ACOM        COMFACS                                      
         MVC   VTCINVD,XBILDTEP    INVOICE DATE                                 
         MVC   VTCTYPE,XTAXIND     TAX TYPE                                     
         MVC   VTCCPYS1,CPYS1      COMPANY STATUS BYTE 1                        
         GOTOX VVATICAN                                                         
         BE    *+6                 DRAT-IT FAILED                               
         DC    H'0'                                                             
*                                                                               
         MVC   VATACCT,VTCACT      VAT ACCOUNT HERE                             
         MVC   VATRATE,VTCRATE     VAT RATE                                     
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
ACTOPN   EQU   40                  ACTION EQUATES FOR ACTION RECORDS            
ACTPRC   EQU   41                                                               
ACTCLS   EQU   42                                                               
PSSBLENQ EQU   X'1D'               LENGTH OF SUB-FILE TRAILER ELEMENT           
*                                                                               
FF       EQU   X'FF'                                                            
MONTH    DC    C'123456789ABC'     THIS IS FOR THE MOS CALCULATIONS             
*                                                                               
SF       DC    CL2'SF'             UNIT/LEDGER CONSTANTS                        
SG       DC    CL2'SG'                                                          
SI       DC    CL2'SI'                                                          
SZ       DC    CL2'SZ'                                                          
UL11     DC    CL2'11'                                                          
UL12     DC    CL2'12'                                                          
*                                                                               
PRGCODE  DC    CL3'ADM'                                                         
PRGNAME  DC    CL16'DEMEDA UPLOAD'                                              
POSTING  DC    CL1'P'                                                           
*                                                                               
PONE     DC    P'1'                                                             
*                                                                               
WKFILE   DC    CL8'WKFILE'         WORKER FILE COMMANDS                         
OPEN     DC    CL8'OPEN'                                                        
CLOSE    DC    CL8'CLOSE'                                                       
ADD      DC    CL8'ADD'                                                         
READ     DC    CL8'READ'                                                        
PURGE    DC    CL8'PURGE'                                                       
BUFFER   DC    CL8'BUFFER'                                                      
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
DMWRITE  DC    CL8'DMWRT'                                                       
TEMPSTR  DC    CL8'TEMPSTR'                                                     
SAVLEN   DC    H'6000'                                                          
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#AUTO,4                                                        
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
*                                                                               
PSTELS   DS    0A                  POSTING ELEMENT ADDRESSES                    
APSTHEAD DS    A                                                                
APSTSUBF DS    A                                                                
APSTNAME DS    A                                                                
APSTTRNS DS    A                                                                
PSTELSLQ EQU   *-PSTELS                                                         
*                                                                               
LGRRD    DS    XL2                 LEDGER TO READ FOR READLGR ROUTINE           
*                                                                               
VTBYTE   DS    XL1                                                              
         SPACE 1                                                                
LDGDISP  DS    0XL4                LEDGER STRUCTURE (SJ LEDGER)                 
SJCLI    DS    XL1                 L'CLIENT                                     
SJPRO    DS    XL1                 L'CLIENT + L'PRODUCT                         
SJJOB    DS    XL1                 L'CLIENT + L'PRODUCT + L'JOB                 
SJNOLVL  DS    XL1                 N/D FOR SJ                                   
         SPACE 1                                                                
MOS      DS    XL2                 DATE FROM DUE DATE FOR TRNBTCH               
SJ       DS    CL2                 U/L FROM COMPANY RECORD                      
SR       DS    CL2                 "   "    "       "                           
CPYS1    DS    CL1                 COMPANY STATUS BYTE 1 (VATICAN)              
UFA      DS    XL1                 UFA FROM MEDREVAC                            
SGOFFPOS DS    XL1                 DISPLACEMENT TO OFFICE IN SG KEY             
         SPACE 1                                                                
VATACCT  DS    XL15                VAT ACCOUNT CODE FROM VATICAN                
VATRATE  DS    XL(L'VTCRATE)       VAT RATE                                     
COSTACCT DS    XL15                COSTING ACCOUNT FROM PPREL ON SJ             
COMMACT  DS    XL14                COMMISSION ACCOUNT FROM MEDIA FILE           
DISCACT  DS    XL14                COMMISSION ACCOUNT FROM MEDIA FILE           
SFNAMEL  DS    CL(L'NAMEREC+3)     NAMEL IF REQUIRED - SF                       
SZNAMEL  DS    CL(L'NAMEREC+3)     NAMEL IF REQUIRED - SZ                       
SFADREL  DS    CL(ADRLN4Q)         ADREL IF REQUIRED - SF                       
SZADREL  DS    CL(ADRLN4Q)         ADREL IF REQUIRED - SZ                       
*                                                                               
INPUT    DS    XL10                USED FOR PROCESSING NUMERICAL INPUT          
*                                                                               
DATE     DS    CL6                                                              
DATEX    DS    CL6                 USED FOR CALCULATING THE MOS                 
*                                                                               
DSLIST   DS    0D                                                               
AC@AUTO  DS    XL4                                                              
*                                                                               
SAVED    DSECT                 *** OVERLAY SAVED STORAGE ***                    
SFLAG    DS    XL1                 INITIALISATION FLAG                          
STOTRECS DS    PL8                 COUNT OF POSTINGS                            
STOTCASH DS    PL8                 TOTAL CREDIT POSTINGS                        
*                                                                               
CDISP    DS    F                                                                
SWKEY    DS    XL16                WORKER FILE KEY                              
SWREC    DS    XL96                OPEN RECORD                                  
SWSAVE   DS    XL(SKEND-SKBUFFD)   SAVED BETWEEN TRANSACTIONS                   
*                                                                               
MEDCPY   DS    XL1                 FACPAK COMPANY HEXCOMP - MEDIA               
*                                                                               
XPLOAD   DS    0X              *** DESCRIBES FORMAT OF UPLOADED RECORD          
XTYPE    DS    XL1                 TYPE CODE:                                   
X1TYPQ   EQU   C'1'                BILLING                                      
X2TYPQ   EQU   C'2'                PAYING                                       
X3TYPQ   EQU   C'3'                SPECIAL MEDIA DISCOUNT                       
XAGYCDE  DS    XL2                 2 CHARACTER AGENCY ID                        
XCLICDE  DS    XL5                 CLIENT CODE                                  
XCLINAM  DS    XL30                CLIENT NAME                                  
XPROCDE  DS    XL2                 PRODUCT CODE                                 
XORDNUM  DS    XL6                 ORDER NUMBER                                 
XOFFCDE  DS    XL1                 OFFICE CODE                                  
XMEDCDE  DS    XL1                 MEDIA CODE                                   
XBILNUM  DS    XL6                 BILL NUMBER                                  
XBILDTE  DS    XL6                 BILL DATE DDMMYY                             
XBILDTEP DS    XL3                 BILL DATE PWOS                               
XDUEDTE  DS    XL6                 DUE DATE  DDMMYY                             
XDUEDTEP DS    XL3                 DUE DATE  PWOS                               
XNETAMT  DS    XL10                NET AMOUNT                                   
XVATAMT  DS    XL10                VAT AMOUNT                                   
XTAXIND  DS    XL1                 TAX CODE INDICATOR                           
XTAX0Q   EQU   C'0'                ZERO VAT INDICATOR                           
XTAX14Q  EQU   C'1'                14% CODE INDICATOR                           
XTAX15Q  EQU   C'2'                15% CODE INDICATOR                           
XCSHDSC  DS    XL10                CASH DISCOUNT AMOUNT                         
XDSCPCT  DS    XL2                 DISCOUNT PERCENTAGE                          
XCOMAMT  DS    XL10                COMMISSION AMOUNT                            
XMEDNAM  DS    CL12                MEDIA NAME - AUENWERBUNG                    
XSUPNUM  DS    CL4                 0 FOLLOWED BY 3 CHAR SUPPLIER NUMBER         
XREF2    DS    CL6                 SECOND REFERENCE NUMBER                      
XPSTNARR DS    CL30                POSTING NARRATIVE                            
XSUPNAM  DS    CL30                SUPPLIER NAME                                
XSUPADR1 DS    CL26                ADDRESS 1                                    
XSUPADR2 DS    CL26                "       2                                    
XSUPADR3 DS    CL26                "       3                                    
XSUPADR4 DS    CL26                "       4                                    
XPLOADLQ EQU   *-XPLOAD                                                         
*                                                                               
*        ACFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
*        FAFACTS                                                                
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
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
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*        ACGENPOST                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
*        MEFILMED                                                               
         PRINT OFF                                                              
       ++INCLUDE MEFILMED                                                       
         PRINT ON                                                               
*        MEFILULAEL                                                             
         PRINT OFF                                                              
       ++INCLUDE MEFILULAEL                                                     
         PRINT ON                                                               
*        MEFILCDSEL                                                             
         PRINT OFF                                                              
       ++INCLUDE MEFILCDSEL                                                     
         PRINT ON                                                               
*        CTGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*        DMWRKRD                                                                
         PRINT OFF                                                              
       ++INCLUDE DMWRKRD                                                        
         PRINT ON                                                               
*        DMWRKRD                                                                
         PRINT OFF                                                              
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
*        DMWRKRS                                                                
         PRINT OFF                                                              
       ++INCLUDE DMWRKRS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056ACFIL40   08/16/00'                                      
         END                                                                    
