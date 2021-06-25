*          DATA SET ACFIL30    AT LEVEL 006 AS OF 08/10/11                      
*&&      SET   NOP=N                                                            
*PHASE T62330C,*                                                                
         SPACE 1                                                                
FIL30    TITLE 'ADJUSTED RATES RECORD'                                          
         SPACE 2                                                                
FIL30    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL30**,R7,R5,RR=RE                                           
         USING WORKD,R9                                                         
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
         LH    R6,=Y(TWUSER-TWAD)                                               
         AR    R6,RA                                                            
         USING TWUSER,R6                                                        
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
         L     R1,CALLR1                                                        
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
EXIT     XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
EXITNVRC MVC   FVMSGNO,=AL2(AE$INREC)     INVALID RECORD                        
         LH    R0,GSDSPREC                                                      
         A     R0,ATWA                                                          
         STCM  R0,15,BOCURSOR     SET CURSOR TO RECORD FIELDD                   
         B     EXITL                                                            
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT DEFINATELY NOT VALID                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
*NIT     TM    TWAAUTH,X'10'       PASSWORD MUST BE USED                        
*        BZ    EXITNVRC                                                         
*                                                                               
INIT     MVC   STFUL,=C'1R'                                                     
         MVC   OFFUL,=C'2D'                                                     
         MVC   PRODUL,=C'SJ'                                                    
         GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
*                                                                               
T        USING ACTRECD,IOKEY                                                    
INIT02   MVC   T.ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                     
         MVC   T.ACTKCPY,CUABIN    CONNECTED ID                                 
         MVC   T.ACTKUNT(L'STFUL),STFUL                                         
         GOTO1 AGETLDG             GET LEDGER INFO.                             
         BNE   EXITL               LEDGER RECORD MISSING                        
         ICM   R4,15,ACALDG                                                     
         USING LDGTABD,R4                                                       
         MVC   OFFLEN(4),LDGTLVA   SET 1R LEDGER LENGTHS                        
*                                                                               
         USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                     
         MVC   T.ACTKCPY,CUABIN    CONNECTED ID                                 
         MVC   T.ACTKUNT(L'OFFUL),OFFUL                                         
         GOTO1 AGETLDG             GET LEDGER INFO.                             
         BNE   EXITL               LEDGER RECORD MISSING                        
         ICM   R4,15,ACALDG                                                     
         USING LDGTABD,R4                                                       
         MVC   OFF2DLEN(4),LDGTLVA  SET 2D LEDGER LENGTHS                       
         B     EXITOK                                                           
         DROP  R4,T                                                             
         SPACE 2                                                                
***********************************************************************         
* TABLE  ITERATION ROUTINE - EXPECTS R1 TO HOLD EQUATED VERB          *         
*                          - EXPECTS RF TO HOLD A(TABLE)              *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
*TER     TM    TWAAUTH,X'10'       PASSWORD MUST BE USED                        
*        BZ    EXITNVRC                                                         
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE TABLE                                
*                                                                               
ITER02   ICM   RF,15,OBJADR        INVOKE OBJECT                                
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
         LA    RF,TABLEOO                                                       
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCREEN)                                
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         SPACE 2                                                                
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
         USING PAJRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4         TABLE OF KNOWN INVOKERS                      
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   MVI   RATEIND,0           SET INDICATOR TO NULL                        
         MVI   RATEIND2,0                                                       
         XC    PAJKEY,PAJKEY       INITIALIZE KEY OF RECORD                     
         MVI   PAJKTYP,PAJKTYPQ                                                 
         MVC   PAJKCPY,CUABIN      CONNECTED ID                                 
         CLI   CSACT,A#DIS         FOR ACTION DISPLAY                           
         BNE   EXITOK              THEN SUPPRESS ASKING FOR KEY                 
         OI    GSINDSL1,GSIXKEY                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVI   RATEIND,0           SET INDICATOR TO NULL                        
         MVI   RATEIND2,0                                                       
         XC    PAJKEY,PAJKEY       INITIALIZE KEY OF RECORD                     
         MVI   PAJKTYP,PAJKTYPQ                                                 
         MVC   PAJKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SCREEN OBJECT                                                       *         
***********************************************************************         
         SPACE 1                                                                
SCREEN   LM    R0,R3,SVPARMS                                                    
         LA    RF,SCRTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
SCRTABL  DC    AL1(SKSET),AL1(0,0,0),AL4(SCKSET)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET KEY SCREEN CODE                                                 *         
***********************************************************************         
         SPACE 1                                                                
SCKSET   XC    GSSKCODE,GSSKCODE                                                
         CLI   CSACT,A#DIS         FOR ACTION DISPLAY                           
         BNE   EXITOK              THEN SUPPRESS ASKING FOR KEY                 
         OI    GSINDSL1,GSIXKEY                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RECORD OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(RECORD)                                                        *         
* P4 HOLDS SUB-ACTION VERB                                            *         
***********************************************************************         
         SPACE 1                                                                
RECORD   LM    R0,R3,SVPARMS                                                    
         USING PAJRECD,R2                                                       
         LA    RF,TABLREC                                                       
         B     ITER                                                             
*                                                                               
TABLREC  DC    AL1(RFIRST),AL1(0,0,0),AL4(RECFRST)                              
         DC    AL1(RLAST),AL1(0,0,0),AL4(RECLAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT                                        *         
***********************************************************************         
         SPACE 1                                                                
RECFRST  L     R1,SVPARMS4                                                      
         LA    RF,RFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
RFTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(RCPY),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD                                  *         
***********************************************************************         
         SPACE 1                                                                
RFADD    GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',PAJRECD),0               
         CLI   12(R1),0            RSTEL ON RECORD?                             
         BE    RFADD02                                                          
         GOTO1 AADDRST,PAJRECD     ADD A RSTEL IF IT DOESN`T EXIST              
         BNE   EXITL               SOMETHING WRONG                              
         B     EXITOK                                                           
*                                                                               
RFADD02  L     RF,12(R1)                                                        
         USING RSTELD,RF                                                        
         MVC   RSTBDATE,BCTODAYP   SET TO TODAY                                 
         MVC   RSTTDATE,BCTODAYP                                                
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
RECLAST  L     R1,SVPARMS4                                                      
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
RLTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)  RCPY NOT NEEDED B/C             
         DC    AL1(RRES),AL1(0,0,0),AL4(RLADD)  RADD IS CALLED FOR              
         DC    AL1(RDEL),AL1(0,0,0),AL4(RLADD)  COPY ACTION(WAS CAUSING         
         DC    AL1(RREN),AL1(0,0,0),AL4(RLADD)  DUPL. SEARCH POINTERS)          
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - ADD, RECSTORE, COPY                   *         
***********************************************************************         
         SPACE 1                                                                
RLADD    DS    0H                                                               
         B     EXITOK                                                           
* PRESTO DECIDED THEY DID NOT NEED THE AJRATE POINTERS BUT KEEP HERE            
* ANYWAY JUST IN CASE THE RULES CHANGE.                                         
*        GOTO1 ADDACTPT,AIOREC     MAINTAIN THE 14 POINTER RECORD               
         B     EXITOK                                                           
         SPACE 2                                                                
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
DATA     ICM   R1,15,SVPARMS2      R1 HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING PAJRECD,R2                                                       
         XR    R1,R1                                                            
         IC    R1,SVPARMS+8        GET GLOBAL VERB                              
         LA    RF,DTATABL                                                       
         B     ITER                                                             
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
         SPACE 1                                                                
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING PAJRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(AR#OFF),AL4(OFCDTA)     OFFICE CODE                          
         DC    AL2(AR#OFFNM),AL4(OFCNDTA)  OFFICE CODE NAME                     
         DC    AL2(AR#CLI),AL4(CLIDTA)     CLIENT CODE                          
         DC    AL2(AR#CLINM),AL4(CLINDTA)  CLIENT CODE NAME                     
         DC    AL2(AR#PRO),AL4(PRODTA)     PRODUCT CODE                         
         DC    AL2(AR#PRONM),AL4(PRONDTA)  PRODUCT CODE NAME                    
         DC    AL2(AR#JOB),AL4(JOBDTA)     JOB CODE                             
         DC    AL2(AR#JOBNM),AL4(JOBNDTA)  JOB CODE NAME                        
         DC    AL2(AR#DOFF),AL4(OFFDTA)    OFFICE CODE                          
         DC    AL2(AR#DOFFNM),AL4(OFFNDTA) OFFICE NAME                          
         DC    AL2(AR#DPT),AL4(DEPDTA)     DEPARTMENT CODE                      
         DC    AL2(AR#DPTNM),AL4(DEPNDTA)  DEPARTMENT NAME                      
         DC    AL2(AR#SDPT),AL4(SUBDTA)    SUB-DEPARTMENT CODE                  
         DC    AL2(AR#SDPTNM),AL4(SUBNDTA) SUB-DEPARMMENT NAME                  
         DC    AL2(AR#STF),AL4(STFDTA)     STAFF CODE                           
         DC    AL2(AR#STFNM),AL4(STFNDTA)  STAFF NAME                           
         DC    AL2(AR#TSK),AL4(TSKDTA)     TASK CODE                            
         DC    AL2(AR#TSKNM),AL4(TSKNDTA)  TASK CODE NAME                       
         DC    AL2(AR#EFFDT),AL4(EDTDTA)   EFFECTIVE DATE (PWOS)                
         DC    AL2(AR#RATE),AL4(RAT1DTA)   CHARGE RATE (2DP)                    
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL30    CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OFFICE CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFCDTA   LA    RF,OFCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFC)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISOFC)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETOFC)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOFC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALOFC)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOFC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHOFC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETOFC  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN OFFICE CODE FROM THE KEY                                 *         
***********************************************************************         
         SPACE 1                                                                
DISOFC   DS    0H                                                               
         MVC   AOFCFLD,FVADDR      SAVE A(OFFICE CODE FIELD)                    
         OC    PAJKOFF(PAJKLNQ-L'PAJKTYP-L'PAJKCPY),PAJKOFF                     
         BNZ   *+14                         FOR HIGH LVL RECORD                 
         MVC   FVIFLD(L'UC@ALL),UC@ALL      DISPLAY ALL                         
         B     EXITOK                                                           
         OC    PAJKOFF,PAJKOFF                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'PAJKOFF),PAJKOFF                                        
         B     EXITOK                                                           
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN OFFICE FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALOFC   MVC   AOFCFLD,FVADDR      SAVE A(OFFICE CODE FIELD)                    
         XC    PAJKOFF,PAJKOFF                                                  
         CLI   FVILEN,0            ANY INPUT ?                                  
         BNE   VOFC02                                                           
         OI    RATEIND,RATEAOFC    ALL OFFICE CODE                              
         B     EXITOK                                                           
*                                                                               
VOFC02   CLC   FVIFLD(L'UC@ALL),UC@ALL  IF HIGH LEVEL RECORD IGNORE             
         BNE   *+12                                                             
         OI    RATEIND,RATEAOFC    ALL OFFICE CODE                              
         B     EXITOK                                                           
         MVC   PAJKOFF,FVIFLD                                                   
         MVC   FLTIFLD(L'PAJKOFF),PAJKOFF                                       
*                                                                               
         CLI   CSACT,A#LST         IF FILTERING ON LIST DON'T VALIDATE          
         BE    EXITOK                                                           
*                                                                               
         TM    BCCPYST4,CPYSOFF2   ON NEW OFFICES?                              
         BO    VOFC06              READ SJ OFFICE RECORDS                       
*                                                                               
         CLC   LDG2DLVA,FVILEN     INPUT LENGTH SHORT ENOUGH?                   
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
         CLC   FVIFLD(L'UC@ALL),UC@ALL WANT AGENCY LEVEL RATE?                  
         BE    EXITOK                                                           
*                                                                               
         USING ACTRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   IOKEY,BCSPACES      READ THE UNIT RECORD FOR THIS ID             
         MVC   ACTKCPY,CUABIN    COMPANY                                        
         MVC   ACTKUNT(L'OFFUL),OFFUL    OFFICE UNIT/LEDGER                     
         MVC   ACTKACT(L'PAJKOFF),PAJKOFF                                       
*                                                                               
VOFC04   GOTO1 AGETACT,0                GET ACCOUNT/TEST SECURITY               
         BNE   EXITL                                                            
         B     EXITOK                                                           
*                                                                               
         USING OFFALD,R1                                                        
VOFC06   L     R1,AOFFBLK          TEST OFFICE SECURITY                         
*        MVI   OFFAACT,OFFAREQ     VALIDATE REQUESTED OFFICE                    
*        LA    RE,OFFAWORK                                                      
*        ST    RE,OFFAREQL         A(REQUESTED OFFICE LIST OUTPUT AREA)         
*        MVC   OFFAOFFC,FVIFLD     OFFICE TO VALIDATE                           
*        MVC   OFFAOPOS,LDGTOFFP                                                
         MVI   OFFAACT,OFFAVAL                                                  
         MVC   OFFAOFFC,FVIFLD                                                  
         GOTO1 VOFFAL                                                           
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$SECLK)  SECURITY LOCKOUT                         
         B     EXITL                                                            
         DROP  R1                                                               
*                                                                               
         USING OGRRECD,R3                                                       
         LA    R3,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   OGRKTYP,OGRKTYPQ    X'2C04'                                      
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,CUABIN                                                   
         MVC   OGRKUNT(2),=C'SJ'                                                
         MVC   OGRKOFC,PAJKOFF                                                  
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$IVOFF) INVALID OFFICE                            
         B     EXITL                                                            
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN OFFICE FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DFLTOFC  MVC   FVIFLD(L'UC@ALL),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON OFFICE CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
SRCHOFC  CLI   CSACT,A#LST         DON'T SEARCH FOR ACTION LIST                 
         BE    EXITOK              MESSES UP FILTERING                          
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,OFFUL,ACOM,    C        
               (X'11',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR OFFICE                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTOFC  CLI   FLTIFLD,0                 ANY FILTER?                            
         BE    EXITOK                                                           
         CLC   PAJKOFF,FLTIFLD     COMPARE OFFICE WITH FILTER DATA              
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A OFFICE CODE NAME FIELD                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFCNDTA  LA    RF,OFCNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFCNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFCN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE CODE NAME FIELD FROM THE KEY                       *         
***********************************************************************         
         SPACE 1                                                                
DISOFCN  OC    PAJKOFF,PAJKOFF     TEST ALL OFFICE                              
         BZ    EXITOK              YES - DON'T DISPLAY OFFICE CODE NAME         
*                                                                               
         TM    BCCPYST4,CPYSOFF2   ON NEW OFFICES?                              
         BO    DOFCN10             READ SJ OFFICE RECORDS                       
*                                                                               
         USING ACTRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
         MVC   ACTKCPY,CUABIN    COMPANY                                        
         MVC   ACTKUNT(L'OFFUL),OFFUL      UNIT/LEDGER                          
         MVC   ACTKACT(L'PAJKOFF),PAJKOFF  OFFICE CODE CODE                     
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    DOFCN20                                                          
         DC    H'0'                                                             
*                                                                               
         USING OGRRECD,R3                                                       
DOFCN10  LA    R3,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   OGRKTYP,OGRKTYPQ    X'2C04'                                      
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,CUABIN                                                   
         MVC   OGRKUNT(2),=C'SJ'                                                
         MVC   OGRKOFC,PAJKOFF                                                  
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DOFCN20  L     R1,AIO1                                                          
         GOTO1 AGETNAM             GET OFFICE CODE NAME                         
         B     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CLIENT CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CLIDTA   LA    RF,CLITBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CLITBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCLI)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCLI)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISCLI)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETCLI)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCLI)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALCLI)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCLI)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHCLI)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETCLI  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CLIENT CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISCLI   OC    PAJKCLI,PAJKCLI        TEST ALL CLIENT CODE                      
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'PAJKCLI),PAJKCLI   MOVE IN CLIENT                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CLIENT CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALCLI   XC    ALLEVEL,ALLEVEL                                                  
         MVC   SJACCT,BCSPACES                                                  
         MVC   SVACCT,BCSPACES                                                  
         XC    PROFOFF,PROFOFF                                                  
         CLI   FVILEN,0                                                         
         BNE   VCLI04                                                           
         OI    RATEIND,RATEACLI+RATEAPRO+RATEAJOB                               
         B     EXITOK                                                           
*                                                                               
VCLI04   MVC   ALLEVEL,FVADDR                                                   
         CLC   BCCLILEN,FVILEN     INPUT LENGTH SHORT ENOUGH?                   
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
         SR    RF,RF                                                            
         IC    RF,BCCLILEN         LENGTH OF THE CLIENT CODE                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PAJKCLI(0),FVIFLD   MOVE IN CLIENT CODE                          
         EX    RF,*+4                                                           
         MVC   FLTIFLD(0),FVIFLD   MOVE IN CLIENT CODE TO FILTER FIELD          
         EXMVC RF,SJACCT,FVIFLD    BUILD THE SJ ACCOUNT                         
*                                                                               
         CLI   CSACT,A#LST         IF FILTERING ON LIST DON'T VALIDATE          
         BE    EXITOK                                                           
*                                                                               
*&&DO                                                                           
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKCPY,CUABIN    COMPANY                                      
         MVC   T.ACTKUNT(L'PRODUL),PRODUL    UNIT/LEDGER                        
         MVC   T.ACTKACT,SJACCT    CLIENT                                       
         DROP  T                                                                
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
*&&                                                                             
*                                                                               
         MVC   SVACCT,BCSPACES                                                  
         MVC   SVUL,PRODUL         SJ LEDGER                                    
         MVC   SVACCT,SJACCT       ACCT TO VALIDATE                             
         BRAS  RE,RDACC                                                         
         BNE   EXITL                                                            
         GOTO1 AGETEL,BOPARM,('PPRELQ',AIO1),0                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NOPPF) NO PRODUCTION PROFILE ELEMENT             
         B     EXITL                                                            
         PUSH  USING                                                            
         USING PPRELD,BOELEM                                                    
         MVC   PROFOFF,PPRGAOFF    SAVE OFFICE FROM PROD PROFILE                
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CLIENT CODE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTCLI  SR    RF,RF                                                            
         IC    RF,BCCLILEN         LENGTH OF THE CLIENT CODE                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),FLTIFLD   MOVE IN CLIENT CODE FROM KEY                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A CLIENT CODE FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
SRCHCLI  CLI   CSACT,A#LST         DON'T SEARCH FOR ACTION LIST                 
         BE    EXITOK              MESSES UP FILTERING                          
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,PRODUL,ACOM,   C        
               (X'11',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR CLIENT CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFTCLI  CLI   FLTIFLD,0                                                        
         BE    EXITOK                                                           
*                                                                               
         ZIC   RF,BCCLILEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   PAJKCLI(0),FLTIFLD                                               
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A CLIENT NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CLINDTA  LA    RF,CLINTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CLINTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCLIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE NAME FIELD FROM THE KEY                            *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
DISCLIN  XR    RF,RF                                                            
         IC    RF,BCCLILEN         LENGTH OF THE CLIENT CODE                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BZ    EXITOK              ALL CLIENT - DON'T DISPLAY NAME              
         OC    PAJKCLI(0),PAJKCLI  TEST ALL CLIENT CODE                         
*                                                                               
         MVC   SVACCT,BCSPACES                                                  
         MVC   SVUL,PRODUL         SJ LEDGER                                    
         EXMVC RF,SVACCT,SJACCT                                                 
         BRAS  RE,RDACC                                                         
         BE    *+6                                                              
         DC    H'0'                SHOULD HAVE FOUND THE ACCT                   
*                                                                               
         L     R1,AIO1                                                          
         GOTO1 AGETNAM             GET CLIENT NAME                              
         B     EXITOK                                                           
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PRODUCT CODE                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PRODTA   LA    RF,PROTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
PROTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRO)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISPRO)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETPRO)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTPRO)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALPRO)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTPRO)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHPRO)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETPRO  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PRODUCT CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
DISPRO   XR    RF,RF               (RF)=L'CLIENT+L'PRODUCT                      
         IC    RF,BCPROLEN                                                      
         XR    RE,RE               (RE)=L'CLIENT                                
         IC    RE,BCCLILEN                                                      
         SR    RF,RE               (RF)=L'PRODUCT                               
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BZ    EXITOK                                                           
         OC    PAJKPRO(0),PAJKPRO           TEST ALL PRODUCT CODE               
*                                                                               
DPRO02   EX    RF,*+4                                                           
         MVC   FVIFLD(0),PAJKPRO   MOVE THE PRODUCT CODE                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PRODUCT CODE FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DFLTPRO  XR    RF,RF               (RF)=L'CLIENT+L'PRODUCT                      
         IC    RF,BCPROLEN                                                      
         XR    RE,RE               (RE)=L'CLIENT                                
         IC    RE,BCCLILEN                                                      
         SR    RF,RE               (RF)=L'PRODUCT                               
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),FLTIFLD   MOVE THE PRODUCT FROM THE FILTER             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PRODUCT CODE FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
VALPRO   TM    RATEIND,RATEAPRO    TEST ALL PRODUCT                             
         BO    EXITOK                                                           
         CLI   FVILEN,0                                                         
         BE    VPRO06                                                           
*                                                                               
VPRO04   MVC   ALLEVEL,FVADDR                                                   
         XR    RF,RF               (RF)=L'CLIENT+PRODUCT                        
         IC    RF,BCPROLEN                                                      
         XR    RE,RE               (RE)=L'CLIENT                                
         IC    RE,BCCLILEN                                                      
         SR    RF,RE               (RF)=L'PRODUCT                               
         CLM   RF,1,FVILEN                                                      
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PAJKPRO,FVIFLD      MOVE THE PRODUCT INTO THE RATE KEY           
         EX    RF,*+4                                                           
         MVC   FLTIFLD(0),FVIFLD   MOVE THE PRODUCT INTO THE FILTER             
         ZIC   R1,BCCLILEN                                                      
         LA    R3,SJACCT                                                        
         AR    R3,R1               BUMP TO SPOT FOR PRODUCT IN SJ ACCT          
         EXMVC RF,0(R3),FVIFLD                                                  
*                                                                               
         CLI   CSACT,A#LST         IF FILTERING ON LIST DON'T VALIDATE          
         BE    EXITOK                                                           
*                                                                               
*&&DO                                                                           
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKCPY,CUABIN    COMPANY                                      
         MVC   T.ACTKUNT(L'PRODUL),PRODUL    UNIT/LEDGER                        
         MVC   T.ACTKACT,SJACCT    PRODUCT                                      
         DROP  T                                                                
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
*&&                                                                             
         MVC   SVUL,PRODUL         SJ LEDGER                                    
         MVC   SVACCT,SJACCT       ACCT TO VALIDATE                             
         BRAS  RE,RDACC                                                         
         BNE   EXITL                                                            
         GOTO1 AGETEL,BOPARM,('PPRELQ',AIO1),0                                  
         BNE   VPRO06                                                           
         PUSH  USING                                                            
         USING PPRELD,BOELEM                                                    
         CLC   PPRGAOFF,BCSPACES                                                
         BNH   VPRO06                                                           
         MVC   PROFOFF,PPRGAOFF                                                 
*                                                                               
VPRO06   CLI   CSACT,A#LST         FOR ACTION LIST                              
         BE    EXITOK              JUST XIT SINCE DON'T NEED THE OFFICE         
         OC    PROFOFF,PROFOFF     SKIP IF OFFICE NOT FROM PROFILE              
         BZ    EXITOK                                                           
         OC    PAJKOFF,PAJKOFF     IF NO OFFICE ENTERED                         
         BZ    VPRO08              FILL IN FROM PROFILE                         
         CLC   PAJKOFF,PROFOFF     OK IF THEY MATCH                             
         BE    EXITOK                                                           
         MVC   FVADDR,AOFCFLD      SET CURSOR TO OFFICE FIELD                   
         MVC   FVMSGNO,=AL2(AE$IVOFF) INVALID OFFICE                            
         B     EXITL                                                            
*                                                                               
VPRO08   L     R3,AOFCFLD          POINT TO SAVED OFFICE FIELD                  
         USING FHD,R3                                                           
         MVC   FHDA(L'UC@ALL),BCSPACES CLEAR FIRST                              
         MVC   FHDA(L'PAJKOFF),PROFOFF  MOVE IN THE OFFICE                      
         MVI   FHIL,L'PAJKOFF           SET LENGTH                              
         OI    FHOI,FHOITR              AND TRANSMIT                            
         MVC   PAJKOFF,PROFOFF     AND MOVE IN KEY TOO                          
         B     EXITOK                                                           
         POP   USING                                                            
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A PRODUCT CODE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
SRCHPRO  CLI   CSACT,A#LST         DON'T SEARCH FOR ACTION LIST                 
         BE    EXITOK              MESSES UP FILTERING                          
         MVC   BOWORK1,BCSPACES                                                 
         MVC   BOWORK1(L'PAJKCLI+L'PAJKPRO),PAJKCLI                             
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,PRODUL,ACOM,   C        
               (X'22',BOWORK1)     GET THE PRODUCT CODE                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PRODUCT CODE                                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTPRO  CLI   FLTIFLD,0           ANY FILTER                                   
         BE    EXITOK                                                           
         XR    RF,RF               (RF)=L'CLIENT+L'PRODUCT                      
         IC    RF,BCPROLEN                                                      
         XR    RE,RE               (RE)=L'CLIENT                                
         IC    RE,BCCLILEN                                                      
         SR    RF,RE               (RF)=L'PRODUCT                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   PAJKPRO,FLTIFLD     COMPARE PRODUCT & FILTER                     
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A PRODUCT NAME FIELD                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PRONDTA  LA    RF,PRONTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
PRONTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRON)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PRODUCT NAME FIELD FROM THE KEY                           *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
DISPRON  XR    RF,RF               (RF)=L'CLIENT+L'PRODUCT                      
         IC    RF,BCPROLEN                                                      
         XR    RE,RE               (RE)=L'CLIENT                                
         IC    RE,BCCLILEN                                                      
         SR    RF,RE               (RF)=L'PRODUCT                               
                                                                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BZ    EXITOK              ALL PRODUCT - DON'T DISPLAY NAME             
         OC    PAJKPRO(0),PAJKPRO     TEST ALL PRODUCT CODE                     
*                                                                               
         MVC   SVACCT,BCSPACES                                                  
         MVC   SVUL,PRODUL         SJ LEDGER                                    
         ZIC   RF,BCPROLEN                                                      
         BCTR  RF,0                                                             
         EXMVC RF,SVACCT,SJACCT                                                 
         BRAS  RE,RDACC                                                         
         BE    *+6                                                              
         DC    H'0'                SHOULD HAVE FOUND THE ACCT                   
*                                                                               
         L     R1,AIO1                                                          
         GOTO1 AGETNAM             GET CLIENT NAME                              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR JOB CODE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
JOBDTA   LA    RF,JOBTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
JOBTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISJOB)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALJOB)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISJOB)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETJOB)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTJOB)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALJOB)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTJOB)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHJOB)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETJOB  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A JOB CODE FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISJOB   XR    RF,RF               (RF)=L'CLIENT+L'PRODUCT+L'JOB                
         IC    RF,BCJOBLEN                                                      
         XR    RE,RE               (RE)=L'PRODUCT                               
         IC    RE,BCPROLEN                                                      
         SR    RF,RE               (RF)=L'JOB                                   
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BZ    EXITOK                                                           
         OC    PAJKJOB(0),PAJKJOB           TEST ALL JOB CODES                  
*                                                                               
DJOB02   EX    RF,*+4                                                           
         MVC   FVIFLD(0),PAJKJOB   MOVE THE JOB CODE                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A JOB CODE FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DFLTJOB  XR    RF,RF               (RF)=L'CLIENT+L'PRODUCT+L'JOB                
         IC    RF,BCJOBLEN                                                      
         XR    RE,RE               (RE)=L'PRODUCT                               
         IC    RE,BCPROLEN                                                      
         SR    RF,RE               (RF)=L'JOB                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),FLTIFLD   MOVE THE JOB FROM THE FILTER                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A JOB CODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
VALJOB   TM    RATEIND,RATEAJOB    TEST ALL JOBS                                
         BZ    VJOB05                                                           
         CLC   SJACCT,BCSPACES     WAS ANY LEVEL ENTERED?                       
         BNH   EXITOK              NO                                           
         MVC   SVUL,PRODUL         SJ LEDGER                                    
         MVC   SVACCT,SJACCT       ACCT TO VALIDATE                             
         BRAS  RE,VALACC           GET ACCOUNT/TEST SECURITY                    
         BE    EXITOK                                                           
         MVC   FVADDR,ALLEVEL      SET ADDR TO LOWEST LVL ENTERED               
         B     EXITL                                                            
*                                                                               
VJOB05   CLI   FVILEN,0                                                         
         BE    VJOB10                                                           
*                                                                               
         MVC   ALLEVEL,FVADDR                                                   
         XR    RF,RF               (RF)=L'CLIENT+L'PRODUCT+L'JOB                
         IC    RF,BCJOBLEN                                                      
         XR    RE,RE               (RE)=L'PRODUCT                               
         IC    RE,BCPROLEN                                                      
         SR    RF,RE               (RF)=L'JOB                                   
         CLM   RF,1,FVILEN                                                      
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PAJKJOB,FVIFLD      MOVE THE JOB INTO THE RATE KEY               
         EX    RF,*+4                                                           
         MVC   FLTIFLD(0),FVIFLD   MOVE THE JOB INTO THE FILTER                 
         ZIC   R1,BCPROLEN                                                      
         LA    R3,SJACCT                                                        
         AR    R3,R1               BUMP TO SPOT FOR JOB IN SJ ACCT              
         EXMVC RF,0(R3),FVIFLD                                                  
*                                                                               
         CLI   CSACT,A#LST         IF FILTERING ON LIST DON'T VALIDATE          
         BE    EXITOK                                                           
*                                                                               
*&&DO                                                                           
         MVC   IOKEY,BCSPACES      READ THE SJ RECORD                           
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKCPY,CUABIN    COMPANY                                      
         MVC   T.ACTKUNT(L'PRODUL),PRODUL    UNIT/LEDGER                        
         MVC   T.ACTKACT,SJACCT    JOB                                          
         DROP  T                                                                
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         B     EXITOK                                                           
*&&                                                                             
*                                                                               
VJOB10   MVC   SVUL,PRODUL         SJ LEDGER                                    
         MVC   SVACCT,SJACCT       ACCT TO VALIDATE                             
         BRAS  RE,VALACC           GET ACCOUNT/TEST SECURITY                    
         BE    EXITOK                                                           
         MVC   FVADDR,ALLEVEL      SET ADDR TO LOWEST LVL ENTERED               
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A JOB CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
SRCHJOB  CLI   CSACT,A#LST         DON'T SEARCH FOR ACTION LIST                 
         BE    EXITOK              MESSES UP FILTERING                          
         MVC   BOWORK1,BCSPACES                                                 
         MVC   BOWORK1(L'PAJKCLI+L'PAJKPRO+L'PAJKJOB),PAJKCLI                   
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,PRODUL,ACOM,   C        
               (X'33',BOWORK1)     GET THE JOB CODE                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR JOB CODE                                           *         
***********************************************************************         
         SPACE 1                                                                
DOFTJOB  CLI   FLTIFLD,0                                                        
         BE    EXITOK                                                           
         XR    RF,RF               (RF)=L'CLIENT+L'PRODUCT+L'JOB                
         IC    RF,BCJOBLEN                                                      
         XR    RE,RE               (RE)=L'PRODUCT                               
         IC    RE,BCPROLEN                                                      
         SR    RF,RE               (RF)=L'JOB                                   
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   PAJKJOB,FLTIFLD     COMPARE JOB & FILTER                         
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A JOB NAME FIELD                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
JOBNDTA  LA    RF,JOBNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
JOBNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISJOBN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A JOB NAME FIELD FROM THE KEY                               *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
DISJOBN  XR    RF,RF               (RF)=L'CLIENT+L'PRODUCT+L'JOB                
         IC    RF,BCJOBLEN                                                      
         XR    RE,RE               (RE)=L'PRODUCT                               
         IC    RE,BCPROLEN                                                      
         SR    RF,RE               (RF)=L'JOB                                   
                                                                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BZ    EXITOK              ALL JOBS - DON'T DISPLAY NAME                
         OC    PAJKJOB(0),PAJKJOB     TEST ALL PRODUCT CODE                     
*                                                                               
         MVC   SVACCT,BCSPACES                                                  
         MVC   SVUL,PRODUL         SJ LEDGER                                    
         MVC   SVACCT,SJACCT       ACCT TO VALIDATE                             
         BRAS  RE,RDACC                                                         
         BE    *+6                                                              
         DC    H'0'                SHOULD HAVE FOUND THE ACCT                   
*                                                                               
         L     R1,AIO1                                                          
         GOTO1 AGETNAM             GET CLIENT NAME                              
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR OFFICE CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFFDTA   LA    RF,OFFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFF)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETOFF)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOFF)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALOFF)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOFF)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHOFF)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
DSETOFF  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISOFF   XR    RF,RF                                                            
         IC    RF,OFFLEN           LENGTH OF THE OFFICE                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BZ    EXITOK                                                           
         OC    PAJKDOF(0),PAJKDOF        ANY OFFICE?                            
*                                                                               
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),PAJKDOF   MOVE IN OFFICE FROM KEY                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A OFFICE CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALOFF   XC    ALLEVEL,ALLEVEL                                                  
         MVC   ACCT1R,BCSPACES     TO BUILD ACCOUNT                             
         MVC   SVACCT,BCSPACES                                                  
         CLI   FVILEN,0            ANY INPUT ?                                  
         BNE   VOFF02                                                           
         OI    RATEIND,RATEAOFF+RATEADEP+RATEASUB+RATEASTF                      
         B     EXITOK              ALL FOR ALL LOWER LEVELS                     
*                                                                               
VOFF02   MVC   ALLEVEL,FVADDR                                                   
         CLC   OFFLEN,FVILEN     INPUT LENGTH SHORT ENOUGH?                     
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
         SR    RF,RF                                                            
         IC    RF,OFFLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PAJKDOF(0),FVIFLD   MOVE IN OFFICE                               
         OC    PAJKDOF,BCSPACES                                                 
         EX    RF,*+4                                                           
         MVC   ACCT1R(0),PAJKDOF   BUILD ACCOUNT                                
         EX    RF,*+4                                                           
         MVC   FLTIFLD(0),FVIFLD   MOVE IN OFFICE TO FILTER FIELD               
         B     EXITOK                                                           
*                                                                               
*&&DO                                                                           
         CLI   CSACT,A#LST         IF FILTERING ON LIST DON'T VALIDATE          
         BE    EXITOK                                                           
*                                                                               
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKCPY,CUABIN    COMPANY                                      
         MVC   T.ACTKUNT(L'STFUL),STFUL UNIT/LEDGER                             
         MVC   T.ACTKACT,ACCT1R    OFFICE                                       
         DROP  T                                                                
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         B     EXITOK                                                           
*&&                                                                             
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE CODE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTOFF  SR    RF,RF                                                            
         IC    RF,OFFLEN           LENGTH OF THE OFFICE                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),FLTIFLD   MOVE IN OFFICE FROM KEY                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A OFFICE CODE                                             *         
***********************************************************************         
         SPACE 1                                                                
SRCHOFF  CLI   CSACT,A#LST         DON'T SEARCH FOR ACTION LIST                 
         BE    EXITOK              MESSES UP FILTERING                          
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,STFUL,ACOM,    C        
               (X'11',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR OFFICE CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFTOFF  CLI   FLTIFLD,0                                                        
         BE    FLTXE                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,OFFLEN           LENGTH OF THE OFFICE                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   PAJKDOF(0),FLTIFLD                                               
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A OFFICE NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFFNDTA  LA    RF,OFFNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFFNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFFN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE NAME FIELD FROM THE KEY                            *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
DISOFFN  XR    RF,RF                                                            
         IC    RF,OFFLEN           LENGTH OF THE OFFICE                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BZ    EXITOK              ALL OFFICE - DON'T SHOW NAME                 
         OC    PAJKDOF(0),PAJKDOF        TEST ALL OFFICE                        
*                                                                               
         MVC   SVACCT,BCSPACES                                                  
         MVC   SVUL,STFUL          1R LEDGER                                    
         EXMVC RF,SVACCT,ACCT1R    ACCT TO VALIDATE                             
         BRAS  RE,RDACC                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO1                                                          
         GOTO1 AGETNAM             GET OFFICE NAME                              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEPARTMENT CODE                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DEPDTA   LA    RF,DEPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
DEPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDEP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDEP)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISDEP)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETDEP)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTDEP)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALDEP)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTDEP)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHDEP)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETDEP  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DEPARTMENT CODE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DISDEP   CLI   DEPLEN,0            DO WE HAVE DEPARTMENT LEVEL?                 
         BE    EXITOK                                                           
         XR    RF,RF               (RF)=L'OFFICE+L'DEPARTMENT                   
         IC    RF,DEPLEN                                                        
         XR    RE,RE               (RE)=L'OFFICE                                
         IC    RE,OFFLEN                                                        
         SR    RF,RE               (RF)=L'DEPARTMENT                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BZ    EXITOK                                                           
         OC    PAJKDEP(0),PAJKDEP           TEST ANY DEPT CODE                  
*                                                                               
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),PAJKDEP   MOVE THE DEPARTMENT CODE                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A DEPARTMENT CODE                                          *         
***********************************************************************         
         SPACE 1                                                                
VALDEP   TM    RATEIND,RATEADEP    TEST ALL DEPARTMENT                          
         BO    EXITOK                                                           
         CLI   DEPLEN,0            DO WE HAVE DEPARTMENT LEVEL?                 
         BNE   *+12                                                             
         OI    FVATRB,FVAPROT      PROTECT FIELD IF NO DEPT LEVEL               
         B     EXITOK                                                           
*                                                                               
         CLI   FVILEN,0            ANY INPUT ?                                  
         BNE   VDEP02                                                           
         OC    PAJKDOF,PAJKDOF     ENTERED OFFICE?                              
         BNZ   EXITNO              THAN MUST ENTER DEPT                         
         OI    RATEIND,RATEADEP+RATEASUB+RATEASTF                               
         B     EXITOK                                                           
*                                                                               
VDEP02   MVC   ALLEVEL,FVADDR                                                   
         XR    RF,RF               (RF)=L'OFFICE+DEPARTMENT                     
         IC    RF,DEPLEN                                                        
         XR    RE,RE               (RE)=L'OFFICE                                
         IC    RE,OFFLEN                                                        
         SR    RF,RE               (RF)=L'DEPARTMENT                            
         CLM   RF,1,FVILEN                                                      
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PAJKDEP(0),FVIFLD      MOVE THE DEPT INTO THE RATE KEY           
         OC    PAJKDEP,BCSPACES                                                 
         EX    RF,*+4                                                           
         MVC   FLTIFLD(0),FVIFLD   MOVE THE DEPARTMENT INTO THE FILTER          
         ZIC   R1,OFFLEN                                                        
         LA    R3,ACCT1R           R3 POINTS TO BUILD ACCT1R FIELD              
         AR    R3,R1               BUMP TO SPOT FOR DEPARMTENT                  
         EXMVC RF,0(R3),FVIFLD    AND MOVE IT IN                                
         B     EXITOK                                                           
*                                                                               
*&&DO                                                                           
         CLI   CSACT,A#LST         IF FILTERING ON LIST DON'T VALIDATE          
         BE    EXITOK                                                           
*                                                                               
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKCPY,CUABIN    COMPANY                                      
         MVC   T.ACTKUNT(L'STFUL),STFUL   UNIT/LEDGER                           
         MVC   T.ACTKACT,ACCT1R    DEPARTMENT                                   
         DROP  T                                                                
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         B     EXITOK                                                           
*&&                                                                             
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DEPARTMENT CODE FILTER FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DFLTDEP  CLI   DEPLEN,0            DO WE HAVE DEPARTMENT LEVEL?                 
         BE    EXITOK                                                           
*                                                                               
         XR    RF,RF               (RF)=L'OFFICE+L'DEPARTMENT                   
         IC    RF,DEPLEN                                                        
         XR    RE,RE               (RE)=L'OFFICE                                
         IC    RE,OFFLEN                                                        
         SR    RF,RE               (RF)=L'DEPARTMENT                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),FLTIFLD   MOVE THE DEPARTMENT FROM THE FILTER          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A DEPARTMENT CODE FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
SRCHDEP  CLI   CSACT,A#LST         DON'T SEARCH FOR ACTION LIST                 
         BE    EXITOK              MESSES UP FILTERING                          
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,STFUL,ACOM,    C        
               (X'22',ACCT1R)      GET THE DEPARTMENT CODE                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR DEPARTMENT CODE                                    *         
***********************************************************************         
         SPACE 1                                                                
DOFTDEP  CLI   FLTIFLD,0                                                        
         BE    FLTXE                                                            
*                                                                               
         XR    RF,RF               (RF)=L'OFFICE+L'DEPARTMENT                   
         IC    RF,DEPLEN                                                        
         XR    RE,RE               (RE)=L'OFFICE                                
         IC    RE,OFFLEN                                                        
         SR    RF,RE               (RF)=L'DEPARTMENT                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   PAJKDEP(0),FLTIFLD  COMPARE DEPARTMENT & FILTER                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A DEPARTMENT NAME FIELD                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DEPNDTA  LA    RF,DEPNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
DEPNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDEPN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DEPARTMENT NAME FIELD FROM THE KEY                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
DISDEPN  CLI   DEPLEN,0            DO WE HAVE DEPARTMENT LEVEL                  
         BE    EXITOK              NO - EXIT                                    
         XR    RF,RF               (RF)=L'OFFICE+L'DEPARTMENT                   
         IC    RF,DEPLEN                                                        
         XR    RE,RE               (RE)=L'OFFICE                                
         IC    RE,OFFLEN                                                        
         SR    RF,RE               (RF)=L'DEPARTMENT                            
                                                                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BZ    EXITOK              ALL DEPT - DON'T SHOW NAME                   
         OC    PAJKDEP(0),PAJKDEP     TEST ALL DEPT                             
*                                                                               
         MVC   SVACCT,BCSPACES                                                  
         MVC   SVUL,STFUL          1R LEDGER                                    
         ZIC   RF,DEPLEN                                                        
         BCTR  RF,0                                                             
         EXMVC RF,SVACCT,ACCT1R                                                 
         BRAS  RE,RDACC                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO1                                                          
         GOTO1 AGETNAM             GET DEPARTMENT NAME                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SUB-DEPT CODE                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBDTA   LA    RF,SUBTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SUBTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSUB)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSUB)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISSUB)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETSUB)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSUB)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALSUB)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSUB)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHSUB)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETSUB  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SUB-DEPT CODE FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DISSUB   CLI   SUBLEN,0            DO WE HAVE SUB-DEPT LEVEL?                   
         BE    EXITOK                                                           
         XR    RF,RF               (RF)=L'DEPARTMENT+L'SUB-DEPT                 
         IC    RF,SUBLEN                                                        
         XR    RE,RE               (RE)=L'DEPARTMENT                            
         IC    RE,DEPLEN                                                        
         SR    RF,RE               (RF)=L'SUB-DEPT                              
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BZ    EXITOK                                                           
         OC    PAJKSUB(0),PAJKSUB           TEST ANY SUB-DEPT CODE              
*                                                                               
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),PAJKSUB   MOVE THE SUB-DEPT CODE                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SUB-DEPT CODE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VALSUB   TM    RATEIND,RATEASUB    TEST ALL SUB-DEPT                            
         BO    EXITOK                                                           
         CLI   SUBLEN,0            DO WE HAVE SUB-DEPT LEVEL?                   
         BNE   *+12                                                             
         OI    FVATRB,FVAPROT      PROTECT FIELD IF NO DEPT LEVEL               
         B     EXITOK                                                           
         CLI   FVILEN,0            ANY INPUT ?                                  
         BNE   VSUB02                                                           
         OI    RATEIND,RATEASUB+RATEASTF                                        
         B     EXITOK                                                           
*                                                                               
VSUB02   MVC   ALLEVEL,FVADDR                                                   
         XR    RF,RF               (RF)=L'DEPARTMENT+SUB-DEPT                   
         IC    RF,SUBLEN                                                        
         XR    RE,RE               (RE)=L'DEPARTMENT                            
         IC    RE,DEPLEN                                                        
         SR    RF,RE               (RF)=L'SUB-DEPT                              
         CLM   RF,1,FVILEN                                                      
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PAJKSUB(0),FVIFLD    MOVE THE SUB-DEPT INTO THE RATE KEY         
         OC    PAJKSUB,BCSPACES                                                 
         EX    RF,*+4                                                           
         MVC   FLTIFLD(0),FVIFLD   MOVE THE SUB-DEPT INTO THE FILTER            
         ZIC   R1,DEPLEN                                                        
         LA    R3,ACCT1R           R3 POINTS TO BUILD ACCOUNT AREA              
         AR    R3,R1               BUMP TO POSITION FOR SUB-DEPT                
         EXMVC RF,0(R3),FVIFLD                                                  
         B     EXITOK                                                           
*                                                                               
*&&DO                                                                           
         CLI   CSACT,A#LST         IF FILTERING ON LIST DON'T VALIDATE          
         BE    EXITOK                                                           
*                                                                               
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKCPY,CUABIN    COMPANY                                      
         MVC   T.ACTKUNT(L'STFUL),STFUL  UNIT/LEDGER                            
         MVC   T.ACTKACT,ACCT1R    SUB-DEPT                                     
         DROP  T                                                                
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         B     EXITOK                                                           
*&&                                                                             
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SUB-DEPT CODE FILTER FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
DFLTSUB  CLI   SUBLEN,0            DO WE HAVE SUB-DEPT LEVEL?                   
         BE    EXITOK                                                           
*                                                                               
         XR    RF,RF               (RF)=L'DEPARTMENT+L'SUB-DEPT                 
         IC    RF,SUBLEN                                                        
         XR    RE,RE               (RE)=L'DEPARTMENT                            
         IC    RE,DEPLEN                                                        
         SR    RF,RE               (RF)=L'SUB-DEPT                              
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),FLTIFLD   MOVE THE SUB-DEPT FROM THE FILTER            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A SUB-DEPT CODE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
SRCHSUB  CLI   CSACT,A#LST         DON'T SEARCH FOR ACTION LIST                 
         BE    EXITOK              MESSES UP FILTERING                          
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,STFUL,ACOM,    C        
               (X'33',ACCT1R)      GET THE SUB-DEPT CODE                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR SUB-DEPT CODE                                      *         
***********************************************************************         
         SPACE 1                                                                
DOFTSUB  CLI   FLTIFLD,0                                                        
         BE    FLTXE                                                            
         XR    RF,RF               (RF)=L'DEPARTMENT+L'SUB-DEPT                 
         IC    RF,SUBLEN                                                        
         XR    RE,RE               (RE)=L'DEPARTMENT                            
         IC    RE,DEPLEN                                                        
         SR    RF,RE               (RF)=L'SUB-DEPT                              
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   PAJKSUB(0),FLTIFLD     COMPARE SUB-DEPT & FILTER                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A SUB-DEPT NAME FIELD                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBNDTA  LA    RF,SUBNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SUBNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSUBN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SUB-DEPT NAME FIELD FROM THE KEY                          *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
DISSUBN  CLI   SUBLEN,0            DO WE HAVE SUB-DEPT LEVEL?                   
         BE    EXITOK              NO - EXIT                                    
         XR    RF,RF               (RF)=L'DEPARTMENT+L'SUB-DEPT                 
         IC    RF,SUBLEN                                                        
         XR    RE,RE               (RE)=L'DEPARTMENT                            
         IC    RE,DEPLEN                                                        
         SR    RF,RE               (RF)=L'SUB-DEPT                              
                                                                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BZ    EXITOK              NO SUB-DEPT - DON'T SHOW NAME                
         OC    PAJKSUB(0),PAJKSUB     TEST ALL SUB-DEPT                         
*                                                                               
         MVC   SVACCT,BCSPACES                                                  
         MVC   SVUL,STFUL          1R LEDGER                                    
         ZIC   RF,SUBLEN                                                        
         BCTR  RF,0                                                             
         EXMVC RF,SVACCT,ACCT1R      SUB-DEPT CODE                              
         BRAS  RE,RDACC                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO1                                                          
         GOTO1 AGETNAM             GET SUB-DEPT NAME                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR STAFF CODE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
STFDTA   LA    RF,STFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
STFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTF)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISSTF)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETSTF)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSTF)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALSTF)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSTF)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHSTF)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETSTF  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A STAFF CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISSTF   CLI   STFLEN,0            DO WE HAVE STAFF LEVEL?                      
         BE    EXITOK                                                           
         XR    RF,RF               (RF)=L'SUB-DEPT+L'STAFF                      
         IC    RF,STFLEN                                                        
         XR    RE,RE               (RE)=L'SUB-DEPT                              
         IC    RE,SUBLEN                                                        
         SR    RF,RE               (RF)=L'STAFF                                 
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BZ    EXITOK                                                           
         OC    PAJKSTF(0),PAJKSTF        TEST ANY STAFF CODE                    
                                                                                
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),PAJKSTF   MOVE THE STAFF CODE                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A STAFF CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALSTF   TM    RATEIND,RATEASTF    TEST ALL STAFF                               
*        BZ    VSTF01                                                           
         BNZ   VSTFA                                                            
         CLI   STFLEN,0            DO WE HAVE STAFF LEVEL?                      
         BNE   *+12                                                             
         OI    FVATRB,FVAPROT      PROTECT FIELD IF NO DEPT LEVEL               
         B     EXITOK                                                           
         CLI   FVILEN,0            ANY INPUT ?                                  
         BNE   VSTF02                                                           
         OI    RATEIND,RATEASTF                                                 
*                                                                               
VSTFA    CLC   ACCT1R,BCSPACES     WAS ANY LEVEL ENTERED?                       
         BNH   EXITOK              NO                                           
         MVC   SVUL,STFUL          1R LEDGER                                    
         MVC   SVACCT,ACCT1R       ACCT TO VALIDATE                             
         BRAS  RE,VALACC           GET ACCOUNT/TEST SECURITY                    
         BE    EXITOK                                                           
         MVC   FVADDR,ALLEVEL      SET ADDR TO LOWEST LVL ENTERED               
         B     EXITL                                                            
*                                                                               
VSTF01   CLI   STFLEN,0            DO WE HAVE STAFF LEVEL?                      
         BNE   *+12                                                             
         OI    FVATRB,FVAPROT      PROTECT FIELD IF NO DEPT LEVEL               
         B     EXITOK                                                           
         CLI   FVILEN,0            ANY INPUT ?                                  
         BNE   VSTF02                                                           
         OI    RATEIND,RATEASTF                                                 
         B     EXITOK                                                           
*                                                                               
VSTF02   MVC   ALLEVEL,FVADDR                                                   
         XR    RF,RF               (RF)=L'SUB-DEPT+STAFF                        
         IC    RF,STFLEN                                                        
         XR    RE,RE               (RE)=L'SUB-DEPT                              
         IC    RE,SUBLEN                                                        
         SR    RF,RE               (RF)=L'STAFF                                 
         CLM   RF,1,FVILEN                                                      
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PAJKSTF(0),FVIFLD      MOVE THE STAFF INTO THE RATE KEY          
         OC    PAJKSTF,BCSPACES                                                 
         EX    RF,*+4                                                           
         MVC   FLTIFLD(0),FVIFLD   MOVE THE STAFF INTO THE FILTER               
         ZIC   R1,SUBLEN                                                        
         LA    R3,ACCT1R           R3 POINTS TO ACCOUNT BUILD AREA              
         AR    R3,R1               BUMP TO POSITION FOR STAFF                   
         EXMVC RF,0(R3),FVIFLD    AND MOVE IN                                   
*                                                                               
         CLI   CSACT,A#LST         IF FIILTERING ON LIST DON'T VALIDATE         
         BE    EXITOK                                                           
*                                                                               
*&&DO                                                                           
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKCPY,CUABIN    COMPANY                                      
         MVC   T.ACTKUNT(L'STFUL),STFUL UNIT/LEDGER                             
         MVC   T.ACTKACT,ACCT1R    STAFF                                        
         DROP  T                                                                
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         B     EXITOK                                                           
*&&                                                                             
         MVC   SVUL,STFUL          1R LEDGER                                    
         MVC   SVACCT,ACCT1R       ACCT TO VALIDATE                             
         BRAS  RE,VALACC           GET ACCOUNT/TEST SECURITY                    
         BE    EXITOK                                                           
         MVC   FVADDR,ALLEVEL      SET ADDR TO LOWEST LVL ENTERED               
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A STAFF CODE FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTSTF  CLI   STFLEN,0            DO WE HAVE STAFF LEVEL?                      
         BE    EXITOK                                                           
*                                                                               
         XR    RF,RF               (RF)=L'SUB-DEPT+L'STAFF                      
         IC    RF,STFLEN                                                        
         XR    RE,RE               (RE)=L'SUB-DEPT                              
         IC    RE,SUBLEN                                                        
         SR    RF,RE               (RF)=L'STAFF                                 
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),FLTIFLD   MOVE THE STAFF FROM THE FILTER               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A STAFF CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
SRCHSTF  CLI   CSACT,A#LST         DON'T SEARCH FOR ACTION LIST                 
         BE    EXITOK              MESSES UP FILTERING                          
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,STFUL,ACOM,    C        
               (X'44',ACCT1R)      GET THE STAFF CODE                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR STAFF CODE                                         *         
***********************************************************************         
         SPACE 1                                                                
DOFTSTF  CLI   FLTIFLD,0                                                        
         BE    FLTXE                                                            
         XR    RF,RF               (RF)=L'SUB-DEPT+L'STAFF                      
         IC    RF,STFLEN                                                        
         XR    RE,RE               (RE)=L'SUB-DEPT                              
         IC    RE,SUBLEN                                                        
         SR    RF,RE               (RF)=L'STAFF                                 
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   PAJKSTF(0),FLTIFLD     COMPARE STAFF & FILTER                    
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A STAFF NAME FIELD                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
STFNDTA  LA    RF,STFNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
STFNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTFN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A STAFF NAME FIELD FROM THE KEY                             *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
DISSTFN  CLI   STFLEN,0            DO WE HAVE STAFF LEVEL?                      
         BE    EXITOK                                                           
         XR    RF,RF               (RF)=L'SUB-DEPT+L'STAFF                      
         IC    RF,STFLEN                                                        
         XR    RE,RE               (RE)=L'SUB-DEPT                              
         IC    RE,SUBLEN                                                        
         SR    RF,RE               (RF)=L'STAFF                                 
                                                                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BZ    EXITOK              ALL STAFF - DON'T SHOW NAME                  
         OC    PAJKSTF(0),PAJKSTF     TEST ALL STAFF CODE                       
*                                                                               
         MVC   SVUL,STFUL          1R LEDGER                                    
         MVC   SVACCT,ACCT1R       ACCT TO VALIDATE                             
         BRAS  RE,RDACC                                                         
         BE    *+6                                                              
         DC    H'0'                SHOULD HAVE FOUND THE ACCT                   
*                                                                               
         L     R1,AIO1                                                          
         GOTO1 AGETNAM             GET STAFF NAME                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TAST/WORK CODE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TSKDTA   LA    RF,TSKTBL                                                        
         B     ITER                                                             
*                                                                               
TSKTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTSK)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTSK)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTTSK)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALTSK)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTTSK)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISTSK)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETTSK)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETTSK  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TASK/WORK CODE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISTSK   OC    PAJKTSK,PAJKTSK     TEST ALL TASK/WORK CODE                      
         BZ    DTSK10                                                           
         MVC   FVIFLD(L'PAJKTSK),PAJKTSK                                        
*                                                                               
DTSK10   CLI   CSACT,A#LST         FOR LIST SCREEN ALREADY FILLED               
         BE    EXITOK              IN OFFICE SO DON'T DO IT HERE                
         CLI   RATEIND,X'FF'                                                    
         BNE   DTSK20              ** THIS CODE BELOW IS ONLY FOR               
         TM    RATEIND2,RATEATSK      THE DISPLAY SCREEN **                     
         BZ    DTSK20                                                           
         L     R3,AOFCFLD          POINT TO SAVED OFFICE FIELD                  
         USING FHD,R3              NO FIELD ENTERED SO...                       
         MVC   FHDA(L'UC@ALL),UC@ALL    MOVE IN 'ALL'                           
         MVI   FHIL,L'UC@ALL            SET LENGTH                              
         OI    FHOI,FHOITR              AND TRANSMIT                            
         B     EXITOK                                                           
*                                                                               
* GOT RID OF THIS CODE BECAUSE IT WAS CLEARING OUT THE OFFICE                   
* FIELD EVEN WHEN THEY ENTERED AN OFFICE BUT NOW IT ALWAYS                      
* DISPLAYS 'ALL' NOT SURE IF THAT IS WHAT THEY WANT                             
* I WAS CHECKING RATEIND FOR FF'S INSTEAD OF JUST CHECKING THE                  
* OFFICE BIT.                                                                   
*                                                                               
DTSK20   DS    0H                                                               
         B     EXITOK                                                           
*        L     R3,AOFCFLD          POINT TO SAVED OFFICE FIELD                  
*        USING FHD,R3              FIELD(S) ENTERED SO...                       
*        MVC   FHDA(L'UC@ALL),BCSPACES  CLEAR OUT PROD OFFICE FIELD             
*        MVI   FHIL,L'UC@ALL            SET LENGTH                              
*        OI    FHOI,FHOITR              AND TRANSMIT                            
*        B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A TASK/WORK CODE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VALTSK   CLI   FVILEN,0            ANY INPUT ?                                  
         BNE   VTSK04                                                           
         OI    RATEIND2,RATEATSK                                                
         B     EXITOK                                                           
*        CLI   RATEIND,FF          ENTERED SOMETHING                            
*        BNE   EXITOK                                                           
*        CLI   CSACT,A#DIS         FOR DISPLAY/SEL FROM LIST                    
*        BE    EXITOK              IT'S OKAY TO HAVE NO KEY FIELDS              
*        MVC   FVADDR,AOFCFLD      SET CURSOR TO OFFICE CODE FIELD              
*        B     EXITNO              AND EXIT WITH ERROR                          
*                                                                               
T        USING WCORECD,IOKEY                                                    
VTSK04   MVC   IOKEY,BCSPACES                                                   
         MVI   T.WCOKTYP,WCOKTYPQ             TASK/WORK CODE RECORD             
         MVC   T.WCOKCPY,CUABIN               COMPANY                           
         MVC   T.WCOKUNT(L'PRODUL),PRODUL     PRODUCTION LEDGER                 
         MVC   PAJKTSK,FVIFLD                                                   
         MVC   FLTIFLD(L'PAJKTSK),PAJKTSK                                       
         MVC   T.WCOKWRK,PAJKTSK              TASK/WORK CODE                    
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    EXITOK              WORK CODE FOUND                              
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXITL               INVALID WORK CODE                            
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TASK/WORK CODE FILTER FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
DFLTTSK  MVC   FVIFLD(L'PAJKTSK),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR TASK/WORK CODE                                     *         
***********************************************************************         
         SPACE 1                                                                
DOFTTSK  CLI   FLTIFLD,0                                                        
         BE    EXITOK                                                           
         CLC   PAJKTSK,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TASK/WORK CODE NAME                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TSKNDTA  LA    RF,TSKNTBL                                                       
         B     ITER                                                             
*                                                                               
TSKNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISTSKN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TASK/WORK CODE NAME FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DISTSKN  OC    PAJKTSK,PAJKTSK     ALL TASK/WORK CODE?                          
         BZ    EXITOK                                                           
*                                                                               
T        USING WCORECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES                                                   
         MVI   T.WCOKTYP,WCOKTYPQ             WORK CODE RECORD                  
         MVC   T.WCOKCPY,CUABIN               COMPANY                           
         MVC   T.WCOKUNT(L'PRODUL),PRODUL     PRODUCTION LEDGER                 
         MVC   T.WCOKWRK,PAJKTSK              TASK/WORK CODE                    
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXITL               INVALID TASK/WORK CODE                       
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AGETEL,BOPARM,('WCOELQ',AIO1),0                                  
         BNE   EXITOK                                                           
         USING WCOELD,BOELEM                                                    
         MVC   FVIFLD(L'WCODESC),WCODESC                                        
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EFFECTIVE DATE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
EDTDTA   LA    RF,EDTTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
EDTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISEDT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEDT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A EFFECTIVE DATE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISEDT   OC    TLKTDTE,TLKTDTE                                                  
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BODMCB,(1,TLKTDTE),(8,FVIFLD)                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A EFFECTIVE DATE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VALEDT   MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         XC    TLKTDTE,TLKTDTE                                                  
         B     EXITOK                                                           
*                                                                               
         ZIC   RF,FVXLEN                                                        
         EXCLC RF,FVIFLD,UC@DEL    ENTERED 'DELETE'?                            
         BNE   VALED05                                                          
         XC    TLKTDTE,TLKTDTE                                                  
         XC    TLKTAMT,TLKTAMT                                                  
         OI    LSLNIND1,LSLNIDEL   YES SO DELETE THIS DATE AND RATE             
         B     EXITOK                                                           
*                                                                               
VALED05  OC    TLKTDTE,TLKTDTE     DATE ALREADY THERE?                          
         BNZ   VALE10              YES SO DON'T CHK FOR MAX #                   
         LH    RE,LSLST#X          LAST LIST REC #                              
         LH    RF,LSLST#1             FIRST LIST REC #                          
         SR    RE,RF                                                            
         LA    RE,1(RE)            BUMP UP RE FOR REAL NUMBER                   
         CHI   RE,27               27 RATES MAX                                 
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RATMX)                                           
         B     EXITL                                                            
*                                                                               
VALE10   XC    TLKTDTE,TLKTDTE                                                  
         GOTO1 VDATVAL,BODMCB,FVIFLD,BODUB1                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     EXITL               INVAILD DATE                                 
         GOTO1 VDATCON,BODMCB,BODUB1,(1,TLKTDTE)                                
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RATE FIELD                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
RAT1DTA  LA    RF,RAT1TBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
RAT1TBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISRAT1)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRAT1)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RATE                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISRAT1  OC    TLKTAMT,TLKTAMT                                                  
         BZ    EXITOK                                                           
         CURED (P4,TLKTAMT),(9,FVIFLD),4,FLOAT=-,DMCB=BODMCB                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RATE                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALRAT1  CLI   FVILEN,0                                                         
         BNE   VRAT10                                                           
         CLI   CSACT,A#ADD                                                      
         BE    EXITNO                                                           
         OC    TLKTDTE,TLKTDTE     IF ENTERED AN EFFECTIVE DATE                 
         BNZ   EXITNO              THAN MUST ENTERE A RATE ELSE                 
         OI    LSLNIND1,LSLNIDEL   DELETE THE LINE                              
         B     EXITOK                                                           
*                                                                               
VRAT10   OC    TLKTAMT,TLKTAMT     IF AMT IS ALREADY THERE MUST BE              
         BNZ   VRAT15              CHANGING AN EXISTING ENTRY SO SKIP           
         LH    RE,LSLST#X          LAST LIST REC #                              
         LH    RF,LSLST#1             FIRST LIST REC #                          
         SR    RE,RF                                                            
         LA    RE,1(RE)            BUMP UP RE FOR REAL NUMBER                   
         CHI   RE,27               27 RATES MAX                                 
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RATMX)                                           
         B     EXITL                                                            
VRAT15   SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(4,FVIFLD),(RF)                                  
         CLI   BODMCB,FF                                                        
         BE    VRAT20              INVALID AMOUNT                               
         L     RF,BODMCB+4                                                      
         CVD   RF,BODUB1                                                        
         CP    BODUB1,=P'9999999'  MAXIMUM IS 999.9999                          
         BNH   *+14                TOO HIGH - ERROR                             
VRAT20   MVC   FVMSGNO,=AL2(AE$INAMT)  INVALID AMOUNT                           
         B     EXITL                                                            
*                                                                               
         MVC   TLKTAMT,BODUB1+4                                                 
         MVI   TLKSEQK,0           FOR ADDS/CHA DON'T CARRY SEQ IN KEY          
         MVC   TLKSEQR,SEQNUMR      MOVE IN SEQUENCE NUMBER                     
         ZIC   R1,SEQNUMR          AND BUMP UP 1                                
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUMR                                                       
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* NTRSES OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(BLOCK) COVERED BY SSAVD                                  *         
***********************************************************************         
         SPACE 1                                                                
NTRSES   LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTROUT),AL1(0,0,0),AL4(NTROUT)                              
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   CLI   SACT,A#LST          ARE WE GOING TO LIST A RECORD?               
         BNE   EXITOK                                                           
         CLI   SREC,R#RATE         RATES RECORD                                 
         BE    *+12                                                             
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   EXITOK                                                           
         OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER FROM HIGHER LEVEL        *         
***********************************************************************         
         SPACE 1                                                                
XITIN    CLI   SACT,A#LST          PAGE DISPLAY SCREEN FROM LIST SCREEN         
         BNE   EXITH                                                            
         NI    SNINDS1,FF-SNIUSECR   TURN OFF USE CURRENT RECORD BIT            
         MVI   LSLTIND1,0          TURN OFF LIST INDICATORS                     
         OI    LSSCIND1,LSSCIBLD   AND REBUILD LIST                             
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* ROUTINE TO GENERATE ACTIVITY POINTER ELEMENTS                                 
* R1 POINTS TO RECORD                                                           
***********************************************************************         
         SPACE 1                                                                
         USING RAPPERD,R3                                                       
ADDACTEL NTR1                                                                   
         TM    BCCPYST6,CPYSRAPP   TEST FOR RECORD ACTIVITY POINTERS            
         BZ    EXITOK              NO                                           
         L     R2,0(R1)                                                         
         L     R3,AIO3                                                          
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM    BUILD RAP PTR ELEM                           
         MVC   RAPCPY,CUABIN                                                    
         MVI   RAPRTYP,RAPKRADJ                                                 
         MVI   RAPEMU,C'N'                                                      
         MVC   RAPACOM,ACOM                                                     
         ST    R2,RAPAREC         ADDRESS OF RECORD                             
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GENERATE ACTIVITY POINTER                                *         
***********************************************************************         
         SPACE 1                                                                
         USING RAPPERD,R3                                                       
ADDACTPT NTR1                                                                   
         TM    BCCPYST6,CPYSRAPP   TEST FOR RECORD ACTIVITY POINTERS            
         BZ    EXITOK              NO                                           
         L     R3,AIO3                                                          
         MVI   RAPACTN,RAPAPTR     BUILD RAP PTR RECORD                         
         MVC   RAPCPY,CUABIN                                                    
         MVI   RAPRTYP,RAPKRADJ                                                 
         MVI   RAPEMU,C'N'                                                      
         MVC   RAPACOM,ACOM                                                     
         ST    R2,RAPAREC         ADDRESS OF RECORD                             
         CLI   CSACT,A#CPY         FOR ACTN COPY CLEAR OUT THE OLD PTR          
         BNE   *+10                OR ELSE WON'T BUILD THE ACTIVITY             
         XC    RAPOLDP,RAPOLDP     REC                                          
         CLI   CSACT,A#REN         IF ACTN RENAME MUST PASS THE D/A             
         BNE   *+10                                                             
         MVC   RAPRDA,IODA                                                      
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING PAJRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING PAJRECD,R2                                                       
LAST     USING PAJRECD,R3                                                       
         LA    RF,LISTABL                                                       
         USING OBJTABD,RF                                                       
LITER    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK                                                           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    LITER02             MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     LITER               ITERATE TABLE                                
*                                                                               
LITER02  CLC   OBJIND3,GSSMPAGE    CHECK PAGE OK (0 FOR LIST)                   
         BE    LITER04                                                          
         LA    RF,OBJTABL(RF)                                                   
         B     LITER                                                            
*                                                                               
LITER04  ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         SPACE 2                                                                
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'PAJKEY),THIS.PAJKEY                                      
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    NLST02                                                           
         BL    EXITL               HARD I/O ERROR                               
         TM    IOERR,IOEDEL        IF ERROR IS THAT REC IS DELETED              
         BO    NLST                READ NEXT INSTEAD OF EXIT                    
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     LHI   R1,XOSEQ+XOACCDIR+XIO1                                           
         GOTO1 AIO                                                              
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   IOKEY(PAJKOFF-PAJRECD),THIS.PAJRECD                              
         BNE   EXITL               CHANGE COMPANY                               
*                                                                               
NLST04   MVC   THIS.PAJKEY(ACCKLEN),IOKEY   WE WANT THIS KEY HERE...            
         B     EXITOK                                                           
         DROP  THIS,LAST                                                        
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT1,LSSBALL+LSSMULIN+LSSTSAR                                 
         OI    LSSTAT2,LSSADD+LSSNOSEQ+LSS1HEAD                                 
         MVC   LSCOLLIN,=AL2(26)   NUMBER OF COLUMNS PER LIST LINE              
         MVC   LSLINROW,=AL2(3)    NUMBER OF LIST LINES PER ROW                 
         MVI   SEQNUMK,1            SET THE SEQUENCE NUMBER                     
         MVI   SEQNUMR,1            SET THE SEQUENCE NUMBER                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,PAJRFST-PAJRECD                                               
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1                                                    *         
***********************************************************************         
         SPACE 1                                                                
FLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         C     RF,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    RF,PAJRFST-PAJRECD(RF) IT IS NOW.                                
         XR    RE,RE                                                            
*                                                                               
         USING TCIELD,RF                                                        
FML02    CLI   TCIEL,0             RECORD END?                                  
         BE    EXITL               YES                                          
         CLI   TCIEL,TCIELQ        TCIEL?                                       
         BNE   NML04               NO                                           
                                                                                
FML04    S     RF,AIOREC                                                        
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1                                                     *         
***********************************************************************         
NLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         XR    RE,RE                                                            
         C     RF,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    NML04                                                            
         LA    RF,PAJRFST-PAJRECD(RF) IT IS NOW.                                
*                                                                               
         USING TCIELD,RF                                                        
NML02    CLI   TCIEL,0             RECORD END?                                  
*        BE    EXITL               YES                                          
         BNE   NML03                                                            
         ZIC   R1,SEQNUMR          NO MORE ELEMENTS SO BUMP                     
         LA    R1,1(R1)            UP THE SEQUENCE # FOR ANY NEW                
         STC   R1,SEQNUMR          ELEMENTS                                     
         B     EXITL                                                            
NML03    CLI   TCIEL,TCIELQ        TCIEL?                                       
         BE    NML06               YES                                          
                                                                                
NML04    IC    RE,TCILN                                                         
         LA    RF,0(RE,RF)                                                      
         B     NML02                                                            
*                                                                               
NML06    S     RF,AIOREC                                                        
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         LH    RF,MNTDISP                                                       
         A     RF,AIOREC                                                        
         USING TCIELD,RF           MOVE IN DETAILS FROM ELEMENT                 
         MVC   TLKTDTE,TCIDTE      EFFECTIVE DATE (PWOS)                        
         MVC   TLKTAMT,TCIAMT      CHARGE RATE (2DP)                            
         MVC   TLKSEQR,TCICM       SEQ # STORED IN FIRST BYTE                   
         MVC   SEQNUMR,TCICM        SAVE THE SEQ # FOR NEXT ONE                 
         MVI   TLKSEQK,0                                                        
         CLI   CSACT,A#DIS                                                      
         BNE   EXITOK                                                           
         MVC   TLKSEQK,SEQNUMK     MOVE IN SEQ # IN KEY FOR ACTION              
         ZIC   R1,SEQNUMK          DISPLAY SO WE CAN SEE ANY RECORDS            
         LA    R1,1(R1)            WITH DUPLICATE DATES.                        
         STC   R1,SEQNUMK                                                       
         B     EXITOK                                                           
         DROP  RF,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 MVI   ANYELEMS,NO         RESET BIT                                    
         CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE              
         BNE   EXITOK              A MAIN ACTION OF CHANGE                      
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('TCIELQ',AIOREC),0                
         B     EXITOK                                                           
*                                                                               
*PDMF02  L     R2,AIOREC                                                        
*        LA    R2,PCRRFST-PCHRECD(R2)                                           
*        USING TCIELD,R2                                                        
*        SR    R4,R4                                                            
**DMF04  CLI   TCIEL,0                                                          
*        BE    UPDMF08             END OF RECORD                                
*        CLI   TCIEL,TCIELQ        TEST FURTHER TCIEL                           
*        BE    UPDMF06                                                          
*        IC    R4,TCILN                                                         
*        AR    R2,R4                                                            
*        B     UPDMF04                                                          
*PDMF06  GOTO1 VTOBACCO,BODMCB,('TOBAADEL',0),AIOREC,ACOM,0,TCIELD,0            
*        B     UPDMF04                                                          
*PDMF08  GOTO1 VTOBACCO,BODMCB,('TOBAACNV',COMPCURD),AIOREC,ACOM,0              
*        B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
*        GOTO1 ADDACTEL,AIOREC     ADD/CHANGE THE ACTIVITY ELEM                 
* PRESTO DECIDED THEY DID NOT NEED THE AJRATE RECORD POINTER BUT KEEP           
* HERE ANYWAY JUST IN CASE THE RULES CHANGE.                                    
*                                                                               
UPDLAST1 CLI   CSACT,A#CHA                                                      
         BE    *+12                                                             
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
         CLI   ANYELEMS,YES        ANY ELEMENTS IN THE RECORD                   
         BE    EXITOK              YES - GOOD                                   
         LH    R0,LS1STLIN                                                      
         A     R0,ATWA                                                          
         STCM  R0,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         B     EXITNO              NO INPUT                                     
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING PCHRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  LM    R2,R3,SVPARMS3                                                   
         CLI   CSACT,A#CHA         ONLY UPDATE ELEMENT IF WE HAVE               
         BE    *+12                A MAIN ACTION OF CHANGE OR ADD               
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
                                                                                
         MVI   ANYELEMS,YES                                                     
         USING TCIELD,R4                                                        
         LA    R4,BOELEM                                                        
         XC    BOELEM,BOELEM       MOVE IN DETAILS TO ELEMENT                   
         MVI   TCIEL,TCIELQ                                                     
         MVI   TCILN,TCILNQ                                                     
         XC    TCICM,TCICM                                                      
         MVC   TCICM(1),TLKSEQR    SAVE SEQ NUMBER IN FIRST BYTE                
         MVC   TCIDTE,TLKTDTE      EFFECTIVE DATE (PWOS)                        
         MVC   TCIAMT,TLKTAMT      CHARGE RATE (2DP)                            
*                                                                               
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* USE THIS ROUTINE TO VALIDATE THE 1R/SJ ACCOUNT AT ANY LEVEL WITHOUT *         
* TESTING SECURITY.                                                   *         
* ALSO USE THIS ROUTINE TO READ THE ACCOUNT IN ORDER TO GET THE NAME  *         
*                                                                               
* ENTRY - SVUL CONTAINS EITHER THE 1R OR SJ LEDGER                    *         
*         SVACCT CONTAINS THE ACCOUNT TO VALIDATE                     *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
RDACC    NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEY,BCSPACES                                                   
         MVC   T.ACTKCPY,CUABIN         COMPANY                                 
         MVC   T.ACTKUNT(L'SVUL),SVUL   UNIT/LEDGER                             
         MVC   T.ACTKACT,SVACCT                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RDACCL                                                           
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     RDACCE                                                           
*                                                                               
RDACCL   CLI   *,FF                                                             
         B     RDACCX                                                           
RDACCE   CR    RB,RB                                                            
RDACCX   XIT1                                                                   
         DROP  T                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* USE THIS ROUTINE TO VALIDATE THE ACCOUNT AT ANY LEVEL AND TEST      *         
* SECURITY                                                            *         
*                                                                               
* ENTRY - SVUL CONTAINS EITHER THE 1R OR SJ LEDGER                    *         
*         SVACCT CONTAINS THE ACCOUNT TO VALIDATE                     *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
VALACC   NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEY,BCSPACES                                                   
         MVC   T.ACTKCPY,CUABIN              COMPANY                            
         MVC   T.ACTKUNT(L'SVUL),SVUL        UNIT/LEDGER                        
         MVC   T.ACTKACT,SVACCT                                                 
         GOTO1 AGETACT,0             GET ACCOUNT/TEST SECURITY                  
         BE    VACCE                                                            
*                                                                               
VACCL    CLI   *,FF                                                             
         B     VACCX                                                            
VACCE    CR    RB,RB                                                            
VACCX    XIT1                                                                   
         DROP  T                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
EFFALL   DC    32X'FF'                                                          
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
WRKCODE  DC    C'WC'                                                            
STMPSTRQ EQU   X'03'               TEMPSTORE PAGE NO. FOR USE IN SEARCH         
RATELN   EQU   5                                                                
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#ALL,L'UC@ALL,L                                                
         DCDDL AC#DEL,L'UC@DEL,L                                                
DCLISTX  DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORKA                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
RAPPERD DSECT                                                                   
       ++INCLUDE ACRAPPERD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
*                                                                               
AOFCFLD  DS    A                   A(OFFICE CODE FIELD)                         
ALLEVEL  DS    A                   A(LOWEST LVL ENTERED FOR 1R/SJ)              
OFFLEN   DS    XL(L'LDGTLVA)       OFFICE LENGTH                                
DEPLEN   DS    XL(L'LDGTLVB)       DEPARTMENT LENGTH                            
SUBLEN   DS    XL(L'LDGTLVC)       SUB-DEPARTMENT LENGTH                        
STFLEN   DS    XL(L'LDGTLVD)       STAFF LENGTH                                 
*                                                                               
OFF2DLEN DS    0XL4                SAVED 2D LEDGER LENGTHS                      
LDG2DLVA DS    XL(L'LDGTLVA)                                                    
LDG2DLVB DS    XL(L'LDGTLVB)                                                    
LDG2DLVC DS    XL(L'LDGTLVC)                                                    
LDG2DLVD DS    XL(L'LDGTLVD)                                                    
*                                                                               
RATEIND  DS    XL1                 RATES RECORD INDICATOR                       
RATEAOFC EQU   X'80'               ALL OFFICE CODE                              
RATEACLI EQU   X'40'               ALL CLIENT                                   
RATEAPRO EQU   X'20'               ALL PRODUCT                                  
RATEAJOB EQU   X'10'               ALL JOB                                      
RATEAOFF EQU   X'08'               ALL OFFICE                                   
RATEADEP EQU   X'04'               ALL DEPARTMENT                               
RATEASUB EQU   X'02'               ALL SUB-DEPARTMENT                           
RATEASTF EQU   X'01'               ALL STAFF                                    
*                                                                               
RATEIND2 DS    XL1                                                              
RATEATSK EQU   X'80'               ALL TASK CODE                                
*                                                                               
PROFOFF  DS    CL2                 OFFICE FROM PRODUCTION PROFILE               
COMPCURD DS    0CL6                CASHVAL CURRENCY CODES                       
OCANWORK DS    XL7                                                              
ANYELEMS DS    XL1                 DO WE HAVE ANY ELEMENTS                      
SEQNUMK  DS    XL1                 SEQUENCE NUMBER TO SEE DUPS(KEY)             
SEQNUMR  DS    XL1                 SEQUENCE NUMBER TO PREVENT DUPS(REC)         
*                                                                               
MNTDISP  DS    H                                                                
STFUL    DS    CL2                 SAVED UNIT/LEDGER                            
PRODUL   DS    CL2                 SAVED PRODUCTION LEDGER                      
OFFUL    DS    CL2                 SAVED OFFICE LEDGER                          
SVUL     DS    CL2                                                              
SVACCT   DS    CL12                TEMP ACCOUNT AREA                            
ACCT1R   DS    CL12                BUILD ACCOUNT AREA                           
SJACCT DS      CL12                BUILD SJ ACCOUNT AREA                        
         ORG   TWUSER+L'TWUSER-(*-SAVEVALS)                                     
         EJECT                                                                  
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
         SPACE 2                                                                
DSLISTU  DS    0D                  UPPERCASE FOR MATCHING                       
UC@ALL   DS    CL3                                                              
UC@DEL   DS    CL6                                                              
OVERWRKN EQU   *-OVERWRKD                                                       
         EJECT                                                                  
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKTDTE  DS    CL(L'TCIDTE)        EFFECTIVE DATE (PWOS)                        
TLKSEQK  DS    XL1                 SEQUENCE # TO SEE PRE-EXISTING DUPS          
         ORG   TLUSER                                                           
TLKSEQR  DS    XL1                 SEQUENCE # TO PREVENT DUPLICATES             
TLKTAMT  DS    XL(L'TCIAMT)        CHARGE RATE (2DP)                            
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACFIL30   08/10/11'                                      
         END                                                                    
