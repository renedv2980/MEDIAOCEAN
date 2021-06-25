*          DATA SET ACFIL48    AT LEVEL 008 AS OF 08/10/11                      
*PHASE T62348C,*                                                                
         SPACE 1                                                                
* TKLU 002 01JUN05 - USE DIFFERENT DATA DICT FOR GERMAN WORDING                 
* YNGX 003 03JAN06 <LO01-5056> NEW TYPES                                        
* NSHE 004 13OCT06 <1051865> DON'T ALLOW WORKCODES ON ESTIMATE XD RECS          
* TKLU 005 10NOV06 <BR10007L> BUG FIX TO EXP.TYPE VALIDATION                    
* MPEN 006 16JUL08 <LO01-7936> ADD SCHEME INFO TO FLIST SCREEN                  
* MPEN 007 01JUL08 <LO01-7395> ADD ROUTINES TO WRITE/DEL XDF PASSIVES           
* SMAN 007 21AUG08 <BR12879D> REDUCE NUMBER OF MAXLINES                         
* MPEN 008 23JUL08 <DU01-7726> CHANGED NTROUT FOR XDLIST REC                    
*                              AND ADDED NEW EDIT RULE X                        
* MPEN 009 20MAR09 <NY02-0028> ADDITIONAL SETTING FOR REQUIRED FIELD            
* MPEN 010 03JUL09 <LO01-9013> CHANGE TO EXPENDITURE TYPE RECORD                
         SPACE 1                                                                
FIL48    TITLE 'EXTRA DATA FIELD RECORD'                                        
         SPACE 2                                                                
FIL48    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL48**,R6,R7,RR=RE                                           
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
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
EXITOFF  MVC   FVMSGNO,=AL2(AE$EXPNV) EXPENDITURE TYPE NOT VALID FOR            
         B     EXITL                  THIS OFFICE                               
*                                                                               
EXITNLDG MVC   FVMSGNO,=AL2(AE$MSJL)    MISSING SJ LEDGER RECORD                
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
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
*                                                                               
         OC    BCCPYPRD,BCCPYPRD                                                
         BNZ   *+6                                                              
         DC    H'0'                PRODUCTION LEDGER MISSING                    
                                                                                
         LA    R2,IOKEY            READ SJ LEDGER RECORD                        
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN      READ PRODUCT LEDGER RECORD                   
         MVC   LDGKUNT(L'BCCPYPRD),BCCPYPRD                                     
         GOTO1 AGETLDG                                                          
         BE    *+12                                                             
         OI    DATERR,DATNPLDG     PRODUCT LEDGER MISSING                       
         B     EXITOK                                                           
*                                                                               
         ICM   RF,15,ACALDG        SET L'CLIENT, PRODUCT AND JOB CODES          
         MVC   CLILEN,LDGTLVA-LDGTABD(RF)                                       
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* TABLE  ITERATION ROUTINE - EXPECTS R1 TO HOLD EQUATED VERB          *         
*                          - EXPECTS RF TO HOLD A(TABLE)              *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     TM    DATERR,DATNPLDG   ANY ERROR                                      
         BNZ   EXITNLDG                                                         
*                                                                               
ITER02   CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER04              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITER                ITERATE TABLE                                
*                                                                               
ITERH    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               * NOT OVERRIDE                               
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER04              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITERH               ITERATE TABLE                                
*                                                                               
ITER04   ICM   RF,15,OBJADR        INVOKE OBJECT                                
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
         B     ITERH                                                            
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OOPT),AL1(0,0,0),AL4(OPT)                                    
         DC    AL1(EOT)                                                         
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
         USING XDFRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4         SUB ACTION                                   
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
KFKVAL   XC    XDFKEY,XDFKEY       INITIALIZE KEY OF RECORD                     
         MVI   XDFKTYP,XDFKTYPQ    EXTRA DATA FIELD RECORD TYPE                 
         MVI   XDFKSUB,XDFKSUBQ    EXTRA DATA FIELD SUB-RECORD TYPE             
         MVC   XDFKCPY,CUABIN      CONNECTED ID                                 
         MVI   XDFKORTY,XDFKDFT                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    XDFKEY,XDFKEY       INITIALIZE KEY OF RECORD                     
         MVI   XDFKTYP,XDFKTYPQ    EXTRA DATA FIELD RECORD TYPE                 
         MVI   XDFKSUB,XDFKSUBQ    EXTRA DATA FIELD SUB-RECORD TYPE             
         MVC   XDFKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR KEY OBJECT                                            *         
***********************************************************************         
         SPACE 1                                                                
KEYLAST  L     R1,SVPARMS4         SUB ACTION                                   
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** FIRST TIME FOR KEY OBJECT ***                 
*                                 -------------------------                     
KLTABL   DC    AL1(KFVAL),AL1(0,0,0),AL4(KLKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KLKFVAL  XC    CDOPTION,CDOPTION                                                
         GOTO1 AVALDOPT,0                                                       
         BL    EXITL                                                            
         CLC   CDOPTION,SDOPTION   HAS OPTIONS BEEN CHANGED?                    
         BE    EXITOK                                                           
         MVI   LSSCIND1,LSSCIFLT   REFRESH LIST                                 
         MVC   SDOPTION(SDOPTSL),CDOPTION                                       
         B     EXITOK                                                           
         DROP  R2                                                               
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
         USING XDFRECD,R2                                                       
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
RFTABL   DC    AL1(RDEL),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    MVI   XDAINDS,XDALAXD     SET LIMITED AMENDMENTS ALLOWED               
         OC    GCRACADD,GCRACADD                                                
         BZ    RFDEL10             NO - ACTIVITY ADD DATE!!                     
         LA    RF,GCRACADD                                                      
         CLC   BCTODAYP,RACDATE-RACELD(RF)                                      
         BNE   RFDEL10             NO RESTRICTION IF ADDED TODAY                
         NI    XDAINDS,FF-XDALAXD                                               
*                                                                               
RFDEL10  TM    XDAINDS,XDALAXD                                                  
         BZ    EXITOK                                                           
         LH    RF,GSDSPACT         DISPLACEMENT TO ACTION FIELD                 
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO ACTION FIELD                   
         MVC   FVMSGNO,=AL2(AE$CDREC)                                           
         B     EXITL               CAN'T DELETE RECORD                          
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE                                                                  
RECLAST  L     R1,SVPARMS4         R1=INVOKER`S VERB                            
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RLTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RLDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RLRES)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLWRT)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - ADD ALL XDATA SUB-RECORDS             *         
***********************************************************************         
         SPACE 2                                                                
RLADD    MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(1),CUABIN     READ COMPANY RECORD                          
         L     R1,=AL4(XORDUP+XOACCMST+XIO2)                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                NO FOUND!!                                   
         GOTO1 AGETEL,BOPARM,('CPYELQ',AIO2),0                                  
         BE    *+6                                                              
         DC    H'0'                CPYEL NO FOUND!!                             
*                                                                               
T        USING CPYELD,BOELEM                                                    
         MVI   T.CPYLN,CPYLN4Q     MAKE SURE IT'S LONG ENOUGH                   
         SR    RF,RF                                                            
         ICM   RF,3,T.CPYXDPTR     RF=LAST XDATA POINTER                        
         AHI   RF,1                                                             
         STCM  RF,3,T.CPYXDPTR     NEXT XDATA POINTER                           
         STCM  RF,3,MYHALF         SAVE NEW XDATA POINTER                       
         DROP  T                                                                
         GOTO1 AREPEL,BOPARM,('CPYELQ',AIO2),0,BOELEM                           
         LHI   R1,XOPUTREC+XOACCMST+XIO2   CHANGE COMPANY RECORD                
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
T        USING XDFRECD,R4                                                       
         LA    R4,IOKEY            READ NEXT XDATA SUB-RECORD                   
         MVC   T.XDFKEY,XDFKEY                                                  
*                                                                               
         L     R1,=AL4(XORDUP+XOACCDIR+XIO2)                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'               DELETE OR NO FOUND                            
*                                                                               
         MVC   T.XDFSPTR,MYHALF   SET POINTER ON DIR                            
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD DIR RECORD                               
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
         L     R4,AIO2                                                          
         MVC   T.XDFRPTR,MYHALF    SET POINTER ON MASTER                        
         LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                PROBLEM WRITING IT BACK                      
         GOTO1 ADDPAS,BOPARM,AIO2                                               
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DELETE ALL XDATA SUB-RECORDS          *         
***********************************************************************         
         SPACE 2                                                                
T        USING XDFRECD,R4                                                       
RLDEL    MVC   IOKEY(L'XDFKEY),XDFKEY                                           
*                                                                               
RLDEL10  LA    R4,IOKEY            READ NEXT XDATA SUB-RECORD                   
         SR    RF,RF               THEY ARE ALWAYS IN SEQUENCE                  
         IC    RF,T.XDFKSEQ                                                     
         AHI   RF,1                                                             
         STC   RF,T.XDFKSEQ                                                     
*                                                                               
         L     R1,=AL4(XORDUP+XOACCDIR+XIO2)                                    
         MVC   XDFSAVDA,IOKEY+XDFKDA-XDFRECD                                    
         GOTO1 AIO                                                              
         BNE   EXITOK              DELETE OR NO FOUND - END                     
*                                                                               
         OI    T.XDFKSTAT,XDFSDELT DELETE XDATA DIR                             
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD DIR RECORD                               
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
         L     R4,AIO2                                                          
         OI    T.XDFRSTAT,XDFSDELT DELETE MASTER RECORD                         
         LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                                                              
         GOTO1 DELPAS,BOPARM,AIO2                                               
         B     RLDEL10                                                          
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - RESTORE ALL XDATA SUB-RECORDS         *         
***********************************************************************         
         SPACE 2                                                                
T        USING XDFRECD,R4                                                       
RLRES    MVC   IOKEY(L'XDFKEY),XDFKEY                                           
*                                                                               
RLRES10  LA    R4,IOKEY            READ NEXT XDATA SUB-RECORD                   
         SR    RF,RF               THEY ARE ALWAYS IN SEQUENCE                  
         IC    RF,T.XDFKSEQ                                                     
         AHI   RF,1                                                             
         STC   RF,T.XDFKSEQ                                                     
*                                                                               
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO2)                                   
         GOTO1 AIO                                                              
         BE    RLRES10             RECORD FOUND!!! - GET NEXT                   
         TM    IOERR,IOERRS-(IOEDEL+IOERNF)                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOERNF        RECORD NOT FOUND - END                       
         BO    EXITOK                                                           
*                                                                               
         NI    T.XDFKSTAT,FF-XDFSDELT     RESTORE IT                            
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD DIR RECORD                               
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
         L     R4,AIO2                                                          
         NI    T.XDFRSTAT,FF-XDFSDELT                                           
         LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                                                              
         B     RLRES10                                                          
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE ALL XDATA SUB-RECORDS           *         
***********************************************************************         
         SPACE 2                                                                
RLWRT    DS    0H                                                               
         GOTO1 ADDPAS,BOPARM,XDFRECD                                            
         B     EXITOK                                                           
         EJECT ,                                                                
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
         USING XDFRECD,R2                                                       
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
         LA    RF,KNOWLQ(,RF)                                                   
         B     DATA04                                                           
         SPACE 1                                                                
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING XDFRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(F#XDAT#CLI),AL4(CLIDTA)     CLIENT CODE                      
         DC    AL2(F#XDAT#CLINM),AL4(CLINDTA)  CLIENT NAME                      
         DC    AL2(F#XDAT#OFF),AL4(OFFDTA)     OFFICE CODE                      
         DC    AL2(F#XDAT#OFFNM),AL4(OFFNDTA)  OFFICE NAME                      
         DC    AL2(F#XDAT#OTYP),AL4(OTYDTA)    ORDER TYPE                       
         DC    AL2(F#XDAT#ETY),AL4(ETYDTA)     EXP. TYPE CODE                   
         DC    AL2(F#XDAT#ETYNM),AL4(ETYNDTA)  EXP. TYEP NAME                   
         DC    AL2(F#XDAT#WC),AL4(WCDTA)       WORK CODE                        
         DC    AL2(F#XDAT#WCNM),AL4(WCNDTA)    WORK CODE NAME                   
         DC    AL2(F#XDAT#MED),AL4(MEDDTA)     MEDIA CODE                       
         DC    AL2(F#XDAT#MEDNM),AL4(MEDNDTA)  MEDIA NAME                       
         DC    AL2(F#XDAT#SCMCD),AL4(SCHEMDTA) SCHEME CODE                      
         DC    AL2(F#XDAT#SCMNM),AL4(SCHENDTA) SCHEME NAME                      
         DC    AL2(F#XDAT#LCODE),AL4(LCODDTA)  XDATA CODE (LIST)                
         DC    AL2(F#XDAT#LNAME),AL4(LNAMDTA)  XDATA NAME (LIST)                
         DC    AL2(F#XDAT#LEDIT),AL4(LEDTDTA)  EDIT RULE  (LIST)                
         DC    AL2(F#XDAT#LMXLN),AL4(LMAXDTA)  MAXIMUM LENGTH (LIST)            
         DC    AL2(F#XDAT#LREQD),AL4(LREQDTA)  REQUIRED FIELD (LIST)            
         DC    AL2(F#XDAT#CODE),AL4(CODDTA)    XDATA CODE                       
         DC    AL2(F#XDAT#NAME),AL4(NAMDTA)    XDATA NAME                       
         DC    AL2(F#XDAT#EDIT),AL4(EDTDTA)    EDIT RULE                        
         DC    AL2(F#XDAT#MXLN),AL4(MXLNDTA)   MAXIMUM LENGTH                   
         DC    AL2(F#XDAT#REQD),AL4(REQDDTA)   REQUIRED FIELD                   
         DC    AL2(F#XDAT#CUT),AL4(CUTDDTA)    CUTOFF DATE                      
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL48    CSECT                                                                  
         EJECT ,                                                                
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** FIRST TIME FOR DATA OBJECT ***                
*                                 -------------------------                     
DFTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   CLM   R2,B'0111',ATLST+1  ARE WE PROCESSING MAINTENANCE LIST?          
         BE    EXITOK              YES - EXIT                                   
         MVI   XDAINDS,0           CLEAR INDICATOR                              
         CLI   CSACT,A#CHA                                                      
         BNE   EXITOK                                                           
*                                                                               
         OI    XDAINDS,XDALAXD     SET LIMITED AMENDMENTS ALLOWED               
         OC    GCRACADD,GCRACADD                                                
         BZ    EXITOK              NO - ACTIVITY ADD DATE!!                     
         LA    RF,GCRACADD                                                      
         CLC   BCTODAYP,RACDATE-RACELD(RF)                                      
         BNE   EXITOK                                                           
         NI    XDAINDS,FF-XDALAXD  NO RESTRICTION IF ADDED TODAY                
         B     EXITOK                                                           
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
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCLI)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCLI)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHCLI)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETCLI  B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CLIENT CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISCLI   CLI   CSACT,A#LST                                                      
         BNE   *+14                                                             
         CLC   LSROWREP,=AL2(1)                                                 
         BH    EXITOK              EXIT - IF NOT THE FIRST ROW                  
*                                                                               
         OC    XDFKCLI,XDFKCLI     ALL CLIENT                                   
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'XDFKCLI),XDFKCLI                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CLIENT CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALCLI   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
VCLI04   CLC   CLILEN,FVILEN       INPUT LENGTH SHORT ENOUGH?                   
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)   INPUT TOO LONG                           
         B     EXITL                                                            
*                                                                               
         MVC   XDFKCLI,FVIFLD      MOVE IN CLIENT CODE                          
*                                                                               
T        USING ACTRECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
         MVC   T.ACTKCPY,XDFKCPY   COMPANY                                      
         MVC   T.ACTKUNT(L'PRODUL),PRODUL    UNIT/LEDGER                        
         MVC   T.ACTKACT(L'XDFKCLI),XDFKCLI  CLIENT CODE                        
         DROP  T                                                                
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CLIENT CODE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTCLI  MVC   FVIFLD(L'XDFKCLI),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CLIENT CODE FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTCLI  CLC   CLILEN,FVILEN       INPUT LENGTH SHORT ENOUGH?                   
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         MVC   XDFKCLI,FVIFLD      MOVE IN CLIENT CODE                          
         MVC   FLTIFLD(L'XDFKCLI),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A CLIENT CODE FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
SRCHCLI  CLI   CSACT,A#LST                                                      
         BNE   SRCLI10                                                          
         L     RF,FVADDR                                                        
         CLI   FHDA-FHD(RF),C'='                                                
         BNE   EXITOK                                                           
SRCLI10  GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,PRODUL,ACOM,   C        
               (X'11',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR CLIENT CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFTCLI  CLC   XDFKCLI,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
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
* DISPLAY A CLIENT NAME FIELD FROM THE KEY                            *         
***********************************************************************         
         SPACE 1                                                                
DISCLIN  OC    XDFKCLI,XDFKCLI     TEST ALL CLIENT CODE                         
         BZ    EXITOK                                                           
*                                                                               
T        USING ACTRECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
         MVC   T.ACTKCPY,XDFKCPY   COMPANY                                      
         MVC   T.ACTKUNT(L'PRODUL),PRODUL     UNIT/LEDGER                       
         MVC   T.ACTKACT(L'XDFKCLI),XDFKCLI  CLIENT CODE                        
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET CLIENT NAME                              
         B     EXITOK                                                           
         EJECT ,                                                                
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
         SPACE 1                                                                
DSETOFF  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OFFICE CODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISOFF   CLI   CSACT,A#LST                                                      
         BNE   DOFF02                                                           
         CLC   LSROWREP,=AL2(1)                                                 
         BH    EXITOK              EXIT - IF NOT THE FIRST ROW                  
                                                                                
         OC    XDFKOFF,XDFKOFF                                                  
         BNZ   DOFF02                                                           
         CLI   XDFKORTY,XDFKDFT                                                 
         BNE   EXITOK                                                           
         OC    XDFKCLI(XDFKSEQ-XDFKCLI),XDFKCLI                                 
         BNZ   EXITOK                                                           
         MVI   FVIFLD,C'*'         ALL                                          
         B     EXITOK                                                           
*                                                                               
DOFF02   MVC   FVIFLD(L'XDFKOFF),XDFKOFF                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALOFF   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   XDFKOFF,FVIFLD                                                   
         MVC   FLTIFLD(L'XDFKOFF),XDFKOFF                                       
*                                                                               
         CLI   CSACT,A#DIS         DON'T TEST OFFICE CODE                       
         BE    EXITOK              IF DISPLAY                                   
         GOTO1 ATSTOFF,FVIFLD      TEST OFFICE CODE                             
         BNE   EXITL               INVALID OFFICE                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OFFICE CODE FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DFLTOFF  MVC   FVIFLD(L'XDFKOFF),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR OFFICE CODE                                        *         
* OVERLAY WILL DO ITS OWN FILTERING                                   *         
***********************************************************************         
         SPACE 1                                                                
DOFTOFF  CLC   XDFKOFF,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON OFFICE CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
SRCHOFF  CLI   CSACT,A#LST                                                      
         BNE   SROFF10                                                          
         L     RF,FVADDR                                                        
         CLI   FHDA-FHD(RF),C'='                                                
         BNE   EXITOK                                                           
SROFF10  GOTO1 VACSRCHC,BOPARM,(3,FVADDR),ATWA,OFFUL,ACOM,(X'11',0)             
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A OFFICE CODE NAME FIELD                 *         
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
* DISPLAY A OFFICE CODE NAME FIELD FROM THE KEY                       *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
DISOFFN  OC    XDFKOFF,XDFKOFF     TEST ALL OFFICE                              
         BZ    EXITOK              YES - DON'T DISPLAY OFFICE CODE NAME         
*                                                                               
         MVC   IOKEY,BCSPACES                READ THE ACCOUNT RECORD            
         MVC   T.ACTKCPY,XDFKCPY             COMPANY                            
         MVC   T.ACTKUNT(L'OFFUL),OFFUL      UNIT/LEDGER                        
         MVC   T.ACTKACT(L'XDFKOFF),XDFKOFF  OFFICE CODE CODE                   
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET OFFICE CODE NAME                         
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR ORDER TYPE XDFKORTY                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OTYDTA   LA    RF,OTYTBL                                                        
         B     ITER                                                             
*                                                                               
OTYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOTY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOTY)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISOTY)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETOTY)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOTY)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALOTY)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOTY)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETOTY  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A ORDER TYPE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISOTY   CLI   CSACT,A#LST                                                      
         BNE   *+14                                                             
         CLC   LSROWREP,=AL2(1)                                                 
         BH    EXITOK              EXIT - IF NOT THE FIRST ROW                  
*                                                                               
         CLI   XDFKORTY,XDFKTYEO   EXPENSE ORDERS?                              
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLEO),UC@FLEO                                        
         B     EXITOK                                                           
         CLI   XDFKORTY,XDFKTYPO   PRODUCTION ORDERS?                           
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLPO),UC@FLPO                                        
         B     EXITOK                                                           
         CLI   XDFKORTY,XDFKTYAO   ARTIST ORDERS?                               
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLAO),UC@FLAO                                        
         B     EXITOK                                                           
         CLI   XDFKORTY,XDFKTYES   ESTIMATES?                                   
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLES),UC@FLES                                        
         B     EXITOK                                                           
         CLI   XDFKORTY,XDFKTYEX   EXPENSE CLAIMS?                              
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLEX),UC@FLEX                                        
         B     EXITOK                                                           
         CLI   XDFKORTY,XDFKINT    INTERNAL?                                    
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@INT),UC@INT                                          
         B     EXITOK                                                           
*                                                                               
         B     EXITOK              UNKNOWN TYPE!!!!!!                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A ORDER TYPE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALOTY   MVI   XDFKORTY,XDFKDFT                                                 
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,2                                                         
         BNE   VOTY04                                                           
         CLC   UC@FLEO,FVIFLD      EXPENSE ORDERS?                              
         BNE   *+12                                                             
         MVI   XDFKORTY,XDFKTYEO                                                
         B     VOTY10                                                           
         CLC   UC@FLPO,FVIFLD      PRODUCTION ORDERS?                           
         BNE   *+12                                                             
         MVI   XDFKORTY,XDFKTYPO                                                
         B     VOTY10                                                           
         CLC   UC@FLAO,FVIFLD      ARTIST ORDERS?                               
         BNE   *+12                                                             
         MVI   XDFKORTY,XDFKTYAO                                                
         B     VOTY10                                                           
         CLC   UC@FLES,FVIFLD      ESTIMATES?                                   
         BNE   *+12                                                             
         MVI   XDFKORTY,XDFKTYES                                                
         B     VOTY10                                                           
         CLC   UC@FLEX,FVIFLD      EXPENSE CLAIMS?                              
         BNE   *+12                                                             
         MVI   XDFKORTY,XDFKTYEX                                                
         B     VOTY10                                                           
         CLC   UC@INT,FVIFLD       INTERNAL?                                    
         BNE   *+12                                                             
         MVI   XDFKORTY,XDFKINT                                                 
         B     VOTY10                                                           
*                                                                               
VOTY04   MVC   FVMSGNO,=AL2(AE$IVTYP)                                           
         B     EXITL               INVALID TYPE                                 
*                                                                               
VOTY10   MVC   FLTIFLD(L'XDFKORTY),XDFKORTY                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A ORDER TYPE FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTOTY  CLI   FLTIFLD,XDFKDFT     DEFAULT?                                     
         BE    EXITOK                                                           
         CLI   FLTIFLD,XDFKTYEO    EXPENSE ORDERS?                              
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLEO),UC@FLEO                                        
         B     EXITOK                                                           
         CLI   FLTIFLD,XDFKTYPO    PRODUCTION ORDERS?                           
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLPO),UC@FLPO                                        
         B     EXITOK                                                           
         CLI   FLTIFLD,XDFKTYAO    ARTIST ORDERS?                               
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLAO),UC@FLAO                                        
         B     EXITOK                                                           
         CLI   FLTIFLD,XDFKTYES    ESTIMATES?                                   
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLES),UC@FLES                                        
         B     EXITOK                                                           
         CLI   FLTIFLD,XDFKTYEX    EXPENSE CLAIMS?                              
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLEX),UC@FLEX                                        
         B     EXITOK                                                           
         CLI   FLTIFLD,XDFKINT     INTERNAL?                                    
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@INT),UC@INT                                          
         B     EXITOK                                                           
*                                                                               
         B     EXITOK              UNKNOWN TYPE!!!!!!                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR ORDER TYPE                                         *         
***********************************************************************         
         SPACE 1                                                                
DOFTOTY  CLC   XDFKORTY,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR EXP. TYPE CODE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ETYDTA   LA    RF,ETYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
ETYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISETY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALETY)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISETY)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETETY)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTETY)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTETY)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTETY)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHETY)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETETY  B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A EXP. TYPE CODE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISETY   CLI   CSACT,A#LST                                                      
         BNE   *+14                                                             
         CLC   LSROWREP,=AL2(1)                                                 
         BH    EXITOK              EXIT - IF NOT THE FIRST ROW                  
*                                                                               
         OC    XDFKETY,XDFKETY     ALL EXP. TYPE                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'XDFKETY),XDFKETY                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A EXP. TYPE CODE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VALETY   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   XDFKORTY,XDFKTYES   ESTIMATES?                                   
         BNE   VALETY2                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITL               INVALID EXP. TYPE                            
*                                                                               
VALETY2  MVC   XDFKETY,FVIFLD      MOVE IN EXP. TYPE CODE                       
*                                                                               
T        USING ETYRECD,IOKEY                                                    
         XC    IOKEY,IOKEY         READ THE EXP. TYPE RECORD                    
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,XDFKCPY   COMPANY                                      
         MVC   T.ETYKCODE,XDFKETY  EXP. TYPE CODE                               
         MVC   SVIOKEY,IOKEY                                                    
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         CLC   SVIOKEY(ETYKOFFC-ETYRECD),IOKEY                                  
         BE    VALETY4                                                          
         MVC   FVMSGNO,=AL2(AE$INETY)                                           
         B     EXITL               INVALID EXP. TYPE                            
*                                                                               
VALETY4  CLC   XDFKOFF,BCSPACES                                                 
         BNH   VALETY6                                                          
         CLC   T.ETYKOFFC,BCSPACES OK TO HAVE GLOBAL EXPENDITURE TYPE           
         BE    EXITOK                                                           
         CLC   T.ETYKOFFC,XDFKOFF  MATCH OFFICE TO EXPENDITURE TYPE             
         BE    EXITOK                                                           
         B     EXITOFF                                                          
*                                  NO OFFICE THEN CHECK EXPENDITURE             
VALETY6  CLC   T.ETYKOFFC,BCSPACES TYPE IS VALID FOR THIS OFFICE                
         BE    EXITOK              OK TO ADD GLOBAL                             
         CLI   CUACCS,0            GLOBAL LOGON?                                
         BE    EXITOK              THEN CAN ADD ANYTHING                        
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BNZ   VALETY10                                                         
         CLI   CUACCS,C'$'         IS IT OFFICE LIST LOGON?                     
         BNE   VALETY8                                                          
         CLI   T.ETYKOFFC,C'$'     OFFICE LIST EXPENDITURE TYPE                 
         BNE   VALETY8                                                          
         CLC   CUACCS(2),T.ETYKOFFC CHECK WHETHER OFFICE LIST MATCHES           
         BE    EXITOK                                                           
         B     EXITOFF             EXPENDITURE TYPE NOT VALID ON THIS           
*                                                                               
VALETY8  GOTO1 ATSTOFF,T.ETYKOFFC                                               
         BE    EXITOK                                                           
         B     EXITOFF             EXPENDITURE TYPE NOT VALID                   
*                                                                               
X        USING OFFRECD,IOKEY       CHECK WHETHER WE HAVE OFFICE LIST            
VALETY10 MVC   SVIOKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   X.OFFKTYP,OFFKTYPQ                                               
         MVC   X.OFFKCPY,CUABIN                                                 
         MVC   X.OFFKOFF,CUACCS+2                                               
         L     R1,=AL4(XOHI+XOACCDIR+XIO2)                                      
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    X.OFFKSTAT,OFFSLIST  OFFICE LIST?                                
         BZ    VALETY12             NO THEN VALIDATE OFFICE                     
         MVC   IOKEY,SVIOKEY                                                    
         CLC   CUACCS+2(2),T.ETYKOFFC CHECK WHETHER MATCH ON                    
         BE    EXITOK               OFFICE LIST                                 
*                                                                               
VALETY12 MVC   IOKEY,SVIOKEY                                                    
         GOTO1 ATSTOFF,T.ETYKOFFC                                               
         BE    EXITOK                                                           
         B     EXITOFF             EXPENDITURE TYPE NOT VALID                   
         DROP  X,T                                                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A EXP. TYPE CODE FILTER FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
DFLTETY  MVC   FVIFLD(L'XDFKETY),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A EXP. TYPE CODE FILTER FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
VFLTETY  MVC   XDFKETY,FVIFLD      MOVE IN EXP. TYPE CODE                       
         MVC   FLTIFLD(L'XDFKETY),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR EXP. TYPE CODE                                     *         
***********************************************************************         
         SPACE 1                                                                
DOFTETY  CLC   XDFKETY,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A WORK CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
SRCHETY  CLI   CSACT,A#LST                                                      
         BNE   SRETY10                                                          
         L     RF,FVADDR                                                        
         CLI   FHDA-FHD(RF),C'='                                                
         BNE   EXITOK                                                           
SRETY10  GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,EXTYP,ACOM,0            
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A EXP. TYPE NAME FIELD                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ETYNDTA  LA    RF,ETYNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
ETYNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISETYN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A EXP. TYPE NAME FIELD FROM THE KEY                         *         
***********************************************************************         
         SPACE 1                                                                
DISETYN  OC    XDFKETY,XDFKETY     TEST ALL EXP. TYPE CODE                      
         BZ    EXITOK                                                           
*                                                                               
T        USING ETYRECD,IOKEY                                                    
         XC    IOKEY,IOKEY         READ THE EXP. TYPE RECORD                    
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,XDFKCPY   COMPANY                                      
         MVC   T.ETYKCODE,XDFKETY  EXP. TYPE CODE                               
         MVC   IOKEYSAV,IOKEY                                                   
         DROP  T                                                                
         LHI   R1,XOHIGH+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         CLC   IOKEYSAV(ETYKOFFC-ETYRECD),IOKEY                                 
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET EXP. TYPE NAME                           
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR WORK CODE                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
WCDTA    LA    RF,WCTBL            TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
WCTBL    DC    AL1(DDIS),AL1(0,0,0),AL4(DISWC)                                  
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWC)                                  
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISWC)                                  
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETWC)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTWC)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTWC)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTWC)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHWC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETWC   B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A WORK CODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISWC    CLI   CSACT,A#LST                                                      
         BNE   *+14                                                             
         CLC   LSROWREP,=AL2(1)                                                 
         BH    EXITOK              EXIT - IF NOT THE FIRST ROW                  
*                                                                               
         OC    XDFKWC,XDFKWC       ALL WORK CODE                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'XDFKWC),XDFKWC                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A WORK CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALWC    CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   XDFKORTY,XDFKTYES   ESTIMATES?                                   
         BNE   VALWC02                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITL               INVALID WORK CODE                            
*                                                                               
VALWC02  MVC   XDFKWC,FVIFLD       MOVE IN WORK CODE                            
T        USING WCORECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES      READ THE WORK RECORD                         
         MVI   T.WCOKTYP,WCOKTYPQ                                               
         MVC   T.WCOKCPY,XDFKCPY            COMPANY                             
         MVC   T.WCOKUNT(L'PRODUL),PRODUL   SJ LEDGER                           
         MVC   T.WCOKWRK,XDFKWC             WORK CODE                           
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXITL               INVALID WORK CODE                            
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A WORK CODE FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DFLTWC   MVC   FVIFLD(L'XDFKWC),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A WORK CODE FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
VFLTWC   MVC   XDFKWC,FVIFLD       MOVE IN WORK CODE                            
         MVC   FLTIFLD(L'XDFKWC),FVIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR WORK CODE                                          *         
***********************************************************************         
         SPACE 1                                                                
DOFTWC   CLC   XDFKWC,FLTIFLD                                                   
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
***********************************************************************         
* SEARCH ON A WORK CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
SRCHWC   CLI   CSACT,A#LST                                                      
         BNE   SRWC10                                                           
         L     RF,FVADDR                                                        
         CLI   FHDA-FHD(RF),C'='                                                
         BNE   EXITOK                                                           
SRWC10   GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,WCQ,ACOM,      C        
               PRODUL                                                           
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A WORK NAME FIELD                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
WCNDTA   LA    RF,WCNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
WCNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISWCN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A WORK NAME FIELD FROM THE KEY                              *         
***********************************************************************         
         SPACE 1                                                                
DISWCN   OC    XDFKWC,XDFKWC       TEST ALL WORK CODE                           
         BZ    EXITOK                                                           
*                                                                               
T        USING WCORECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES      READ THE WORK RECORD                         
         MVI   T.WCOKTYP,WCOKTYPQ                                               
         MVC   T.WCOKCPY,XDFKCPY            COMPANY                             
         MVC   T.WCOKUNT(L'PRODUL),PRODUL   SJ LEDGER                           
         MVC   T.WCOKWRK,XDFKWC             WORK CODE                           
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('WCOELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(,R1)                                                       
         MVC   FVIFLD(L'WCODESC),WCODESC-WCOELD(RF)                             
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR MEDIA CODE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
MEDDTA   LA    RF,MEDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MEDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMED)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMED)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISMED)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETMED)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTMED)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTMED)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTMED)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHMED)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETMED  B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A MEDIA CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISMED   CLI   CSACT,A#LST                                                      
         BNE   *+14                                                             
         CLC   LSROWREP,=AL2(1)                                                 
         BH    EXITOK              EXIT - IF NOT THE FIRST ROW                  
*                                                                               
         OC    XDFKMED,XDFKMED     ALL MEDIA CODE                               
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'XDFKMED),XDFKMED                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A MEDIA CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALMED   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   FVILEN,1                                                         
         BNE   EXITNV                                                           
         MVC   XDFKMED,FVIFLD      MOVE IN MEDIA CODE                           
*                                                                               
T        USING PMDRECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES      READ THE MEDIA RECORD                        
         MVI   T.PMDKTYP,PMDKTYPQ                                               
         MVC   T.PMDKCPY,XDFKCPY            COMPANY                             
         MVC   T.PMDKMED,XDFKMED            MEDIA CODE                          
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$IVMED)                                           
         B     EXITL               INVALID MEDIA CODE                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A MEDIA CODE FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTMED  MVC   FVIFLD(L'XDFKMED),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A MEDIA CODE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
VFLTMED  CLI   FVILEN,1                                                         
         BNE   EXITNV                                                           
         MVC   XDFKMED,FVIFLD      MOVE IN MEDIA CODE                           
         MVC   FLTIFLD(L'XDFKMED),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR MEDIA CODE                                         *         
***********************************************************************         
         SPACE 1                                                                
DOFTMED  CLC   XDFKMED,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A MEDIA CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
SRCHMED  CLI   CSACT,A#LST                                                      
         BNE   SRMED10                                                          
         L     RF,FVADDR                                                        
         CLI   FHDA-FHD(RF),C'='                                                
         BNE   EXITOK                                                           
SRMED10  GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,MEDIAQ,ACOM             
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A MEDIA NAME FIELD                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
MEDNDTA  LA    RF,MEDNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MEDNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISMEDN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A MEDIA NAME FIELD FROM THE KEY                             *         
***********************************************************************         
         SPACE 1                                                                
DISMEDN  OC    XDFKMED,XDFKMED     TEST ALL MEDIA CODE                          
         BZ    EXITOK                                                           
*                                                                               
T        USING PMDRECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES               READ THE MEDIA RECORD               
         MVI   T.PMDKTYP,PMDKTYPQ                                               
         MVC   T.PMDKCPY,XDFKCPY            COMPANY                             
         MVC   T.PMDKMED,XDFKMED            MEDIA CODE                          
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('PMDELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(,R1)                                                       
         MVC   FVIFLD(L'PMDDESC),PMDDESC-PMDELD(RF)                             
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR SCHEME CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SCHEMDTA LA    RF,SCHEMTBL         TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SCHEMTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISSCHEM)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSCH)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISSCHEM)                               
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETSCH)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSCH)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTSCH)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSCH)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETSCH  B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SCHEME CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISSCHEM CLI   CSACT,A#LST                                                      
         BNE   *+14                                                             
         CLC   LSROWREP,=AL2(1)                                                 
         BH    EXITOK              EXIT - IF NOT THE FIRST ROW                  
         OC    XDFKSCH,XDFKSCH     ALL SCHEME CODE                              
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'XDFKSCH),XDFKSCH                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SCHEME CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALSCH   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         MVC   XDFKSCH,FVIFLD      MOVE IN SCHEME CODE                          
*                                                                               
T        USING SCHRECD,IOKEY                                                    
         XC    IOKEY,IOKEY         READ THE SCHEME RECORD                       
         MVI   T.SCHKTYP,SCHKTYPQ                                               
         MVI   T.SCHKSUB,SCHKSUBQ                                               
         MVC   T.SCHKCPY,XDFKCPY   COMPANY                                      
         MVC   T.SCHKUNT,PRODUL    UNIT                                         
         MVC   T.SCHKLDG,PRODUL+1  LEDGER                                       
         MVC   T.SCHKCODE,XDFKSCH                                               
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
*                                                                               
VALSCH1  GOTO1 AIO                                                              
         BE    EXITOK                                                           
VALSCHE  MVC   FVMSGNO,=AL2(AE$INSCH)                                           
         B     EXITL               INVALID SCHEME CODE                          
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SCHEME CODE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTSCH  MVC   FVIFLD(L'XDFKSCH),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SCHEME CODE FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTSCH  MVC   XDFKSCH,FVIFLD      MOVE IN SCHEME CODE                          
         MVC   FLTIFLD(L'XDFKSCH),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR SCHEME CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFTSCH  CLC   XDFKSCH,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A SCHEME NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SCHENDTA LA    RF,SCHENTBL         TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SCHENTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISSCHN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SCHEME NAME FIELD FROM THE KEY                            *         
***********************************************************************         
         SPACE 1                                                                
DISSCHN  OC    XDFKSCH,XDFKSCH     TEST ALL SCHEME CODE                         
         BZ    EXITOK                                                           
*                                                                               
T        USING SCHRECD,IOKEY                                                    
         XC    IOKEY,IOKEY         READ THE SCHEME RECORD                       
         MVI   T.SCHKTYP,SCHKTYPQ                                               
         MVI   T.SCHKSUB,SCHKSUBQ                                               
         MVC   T.SCHKCPY,XDFKCPY   COMPANY                                      
         MVC   T.SCHKUNT,PRODUL    UNIT                                         
         MVC   T.SCHKLDG,PRODUL+1  LEDGER                                       
         MVC   T.SCHKCODE,XDFKSCH                                               
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
*                                                                               
DISSCH1  GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
DISSCH2  LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('SCHELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(,R1)                                                       
         MVC   FVIFLD(L'SCHNAME),SCHNAME-SCHELD(RF)   DISPLAY                   
         B     EXITOK                                 SCHEME NAME               
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR EXTRA DATA CODE - LIST ACTION ONLY                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LCODDTA  LA    RF,LCODTBL                                                       
         B     ITER                                                             
*                                                                               
LCODTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLCOD)                                
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY A EXTRA DATA CODE ON LIST SCREEN                            *         
***********************************************************************         
         SPACE 1                                                                
         USING XDFELD,R4                                                        
DISLCOD  CLI   CSACT,A#LST                                                      
         BNE   EXITOK                                                           
         LH    RF,LSROWREP         ROW NUMBER                                   
         LA    R4,XDFRFST                                                       
         SR    R0,R0                                                            
DLCOD02  CLI   XDFEL,0             EOR                                          
         BE    EXITOK                                                           
         CLI   XDFEL,XDFELQ                                                     
         BE    DLCOD04                                                          
         IC    R0,XDFLN                                                         
         AR    R4,R0               GET NEXT XDFEL                               
         B     DLCOD02                                                          
DLCOD04  BCT   RF,*-10             GET THE CORRECT XDFEL                        
*                                                                               
         MVC   FVIFLD(L'XDFCODE),XDFCODE                                        
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR EXTRA DATA NAME - LIST ACTION ONLY                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LNAMDTA  LA    RF,LNAMTBL                                                       
         B     ITER                                                             
*                                                                               
LNAMTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLNAM)                                
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY A EXTRA DATA NAME ON LIST SCREEN                            *         
***********************************************************************         
         SPACE 1                                                                
         USING XDFELD,R4                                                        
DISLNAM  CLI   CSACT,A#LST                                                      
         BNE   EXITOK                                                           
         LH    RF,LSROWREP         ROW NUMBER                                   
         LA    R4,XDFRFST                                                       
         SR    R0,R0                                                            
DLNAM02  CLI   XDFEL,0             EOR                                          
         BE    EXITOK                                                           
         CLI   XDFEL,XDFELQ                                                     
         BE    DLNAM04                                                          
         IC    R0,XDFLN                                                         
         AR    R4,R0               GET NEXT XDFEL                               
         B     DLNAM02                                                          
DLNAM04  BCT   RF,*-10             GET THE CORRECT XDFEL                        
*                                                                               
         SR    RE,RE                                                            
         IC    RE,XDFLN                                                         
         SHI   RE,XDFLN1Q+1                                                     
         BM    EXITOK              NO NAME !!                                   
         EXMVC RE,FVIFLD,XDFNAME                                                
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR EDIT RULE - LIST ACTION ONLY                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LEDTDTA  LA    RF,LEDTTBL                                                       
         B     ITER                                                             
*                                                                               
LEDTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLEDT)                                
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY A EDIT RULE ON LIST SCREEN                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING XDFELD,R4                                                        
DISLEDT  CLI   CSACT,A#LST                                                      
         BNE   EXITOK                                                           
         LH    RF,LSROWREP         ROW NUMBER                                   
         LA    R4,XDFRFST                                                       
         SR    R0,R0                                                            
DLEDT02  CLI   XDFEL,0             EOR                                          
         BE    EXITOK                                                           
         CLI   XDFEL,XDFELQ                                                     
         BE    DLEDT04                                                          
         IC    R0,XDFLN                                                         
         AR    R4,R0               GET NEXT XDFEL                               
         B     DLEDT02                                                          
DLEDT04  BCT   RF,*-10             GET THE CORRECT XDFEL                        
*                                                                               
         MVI   FVIFLD,C'?'         UNKNOWN                                      
         OC    XDFEDIT,XDFEDIT                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(1),UC@DATE                                                
         CLI   XDFEDIT,C'D'                                                     
         BE    EXITOK                                                           
         MVC   FVIFLD(1),UC@NUM                                                 
         CLI   XDFEDIT,C'N'                                                     
         BE    EXITOK                                                           
         MVC   FVIFLD(1),UC@LTRS2                                               
         CLI   XDFEDIT,C'C'                                                     
         BE    EXITOK                                                           
         MVC   FVIFLD(1),UC@AMT                                                 
         CLI   XDFEDIT,C'A'                                                     
         BE    EXITOK                                                           
         MVC   FVIFLD(1),BC@YES                                                 
         CLI   XDFEDIT,C'Y'                                                     
         BE    EXITOK                                                           
         MVI   FVIFLD,C'X'                                                      
         CLI   XDFEDIT,C'X'                                                     
         BE    EXITOK                                                           
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR MAX LENGTH - LIST ACTION ONLY                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LMAXDTA  LA    RF,LMAXTBL                                                       
         B     ITER                                                             
*                                                                               
LMAXTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLMAX)                                
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY A MAX LENGTH ON LIST SCREEN                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING XDFELD,R4                                                        
DISLMAX  CLI   CSACT,A#LST                                                      
         BNE   EXITOK                                                           
         LH    RF,LSROWREP         ROW NUMBER                                   
         LA    R4,XDFRFST                                                       
         SR    R0,R0                                                            
DLMAX02  CLI   XDFEL,0             EOR                                          
         BE    EXITOK                                                           
         CLI   XDFEL,XDFELQ                                                     
         BE    DLMAX04                                                          
         IC    R0,XDFLN                                                         
         AR    R4,R0               GET NEXT XDFEL                               
         B     DLMAX02                                                          
DLMAX04  BCT   RF,*-10             GET THE CORRECT XDFEL                        
*                                                                               
         OC    XDFMXLN,XDFMXLN                                                  
         BZ    EXITOK                                                           
         LH    RE,XDFMXLN                                                       
         CURED (RE),(3,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR REQUIRED FIELD - LIST ACTION ONLY                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LREQDTA  LA    RF,LREQTBL                                                       
         B     ITER                                                             
*                                                                               
LREQTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLREQ)                                
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY A REQUIRED FIELD ON LIST SCREEN                             *         
***********************************************************************         
         SPACE 1                                                                
         USING XDFELD,R4                                                        
DISLREQ  MVI   PRTMORE,YES         PRINT INDICATOR ON LAST LINE                 
         LH    RF,LSROWREP         ROW NUMBER                                   
         CHI   RF,MAXLINES                                                      
         BE    *+8                                                              
         MVI   PRTMORE,NO                                                       
         LA    R4,XDFRFST                                                       
         USING XDFELD,R4                                                        
         SR    R3,R3                                                            
         SR    R0,R0                                                            
DLREQ02  CLI   XDFEL,0             EOR                                          
         BNE   DLREQ04                                                          
         LTR   R3,R3               DO WE HAVE XDFEL?                            
         BZ    EXITOK              NO - EXITOK                                  
         MVI   PRTMORE,NO                                                       
         B     DLREQ10                                                          
*                                                                               
DLREQ04  CLI   XDFEL,XDFELQ                                                     
         BE    DLREQ08                                                          
DLREQ06  IC    R0,XDFLN                                                         
         AR    R4,R0               GET NEXT XDFEL                               
         B     DLREQ02                                                          
*                                                                               
DLREQ08  LTR   R3,R3               HAVE XDFEL ALREADY?                          
         BNZ   DLREQ10                                                          
         BCT   RF,DLREQ06          GET THE CORRECT XDFEL                        
         LR    R3,R4               SAVE R4                                      
         CLI   PRTMORE,YES         TEST IF WE WANT TO PRINT INDICATOR           
         BE    DLREQ06                                                          
*                                                                               
DLREQ10  CLI   PRTMORE,YES         TO INDICATE THAT THERE IS MORE XDFEL         
         BNE   *+10                                                             
         MVC   FVIFLD+1(2),=C'>>'                                               
         MVC   FVIFLD(1),BC@NO                                                  
         TM    XDFSTAT1,XDFSRECP     NO RECEIPT COMPULSORY                      
         BZ    *+14                                                             
         MVC   FVIFLD(1),UC@RCPTS                                               
         B     EXITOK                                                           
         TM    XDFSTAT1,XDFSREQD                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(1),BC@YES                                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR XDATA CODE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
CODDTA   LA    RF,CODTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
CODTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY XDATA CODE FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISCOD   MVC   FVIFLD(L'TLKCODE),TLKCODE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE XDATA CODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
VALCOD   OI    TLXDSTT,TLNEWEL                                                  
         CLC   TLKCODE,BCSPACES                                                 
         BNH   *+8                                                              
         NI    TLXDSTT,FF-TLNEWEL  EXISTING XDATA ELEMENT                       
*                                                                               
         XC    TLKCODE,TLKCODE                                                  
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   VCOD10                                                           
         TM    TLXDSTT,TLNEWEL     NEW ELEMENT ?                                
         BO    *+12                YES - OK                                     
         TM    XDAINDS,XDALAXD     ELEMENT ADDED TODAY ?                        
         BO    *+12                NO - SHOW ERROR                              
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CDLIN)                                           
         B     EXITL               CAN'T DELETE THIS LINE                       
*                                                                               
VCOD10   LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITEMS         MAX OF 100 LINES                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         MVC   TLKCODE,FVIFLD                                                   
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR XDATA NAME                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
NAMDTA   LA    RF,NAMTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
NAMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNAM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY XDATA NAME                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISNAM   MVC   FVIFLD(L'TLNAME),TLNAME                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE XDATA NAME                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALNAM   OC    TLKCODE,TLKCODE                                                  
         BZ    EXITOK                                                           
         CLI   FVILEN,0                                                         
         BE    EXITNO              ERROR - NO INPUT                             
         MVC   TLNMLEN,FVILEN      LENGTH OF NAME                               
         MVC   TLNAME,FVIFLD                                                    
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR EDIT RULE                                           *         
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
         DC    AL1(DHED),AL1(0,0,0),AL4(SETHED)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY EDIT RULE                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISEDT   MVI   FVIFLD,C'?'         UNKNOWN                                      
         OC    TLEDIT,TLEDIT                                                    
         BZ    EXITOK                                                           
         MVC   FVIFLD(1),UC@DATE                                                
         CLI   TLEDIT,C'D'                                                      
         BE    EXITOK                                                           
         MVC   FVIFLD(1),UC@NUM                                                 
         CLI   TLEDIT,C'N'                                                      
         BE    EXITOK                                                           
         MVC   FVIFLD(1),UC@LTRS2                                               
         CLI   TLEDIT,C'C'                                                      
         BE    EXITOK                                                           
         MVC   FVIFLD(1),UC@AMT                                                 
         CLI   TLEDIT,C'A'                                                      
         BE    EXITOK                                                           
         MVC   FVIFLD(1),BC@YES                                                 
         CLI   TLEDIT,C'Y'                                                      
         BE    EXITOK                                                           
         MVI   FVIFLD,C'X'                                                      
         CLI   TLEDIT,C'X'                                                      
         BE    EXITOK                                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE EDIT RULE                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALEDT   OC    TLKCODE,TLKCODE                                                  
         BZ    EXITOK                                                           
*                                                                               
         MVC   BOBYTE1,TLEDIT                                                   
         XC    TLEDIT,TLEDIT                                                    
         CLI   FVILEN,0                                                         
         BE    VEDT05                                                           
         MVI   TLEDIT,C'D'                                                      
         CLC   FVIFLD(1),UC@DATE                                                
         BE    VEDT10                                                           
         MVI   TLEDIT,C'N'                                                      
         CLC   FVIFLD(1),UC@NUM                                                 
         BE    VEDT10                                                           
         MVI   TLEDIT,C'C'                                                      
         CLC   FVIFLD(1),UC@LTRS2                                               
         BE    VEDT10                                                           
         MVI   TLEDIT,C'A'                                                      
         CLC   FVIFLD(1),UC@AMT                                                 
         BE    VEDT10                                                           
         MVI   TLEDIT,C'Y'                                                      
         CLC   FVIFLD(1),BC@YES                                                 
         BE    VEDT10                                                           
         MVI   TLEDIT,C'X'                                                      
         CLC   FVIFLD(1),=C'X'                                                  
         BE    VEDT10                                                           
*                                                                               
VEDT05   MVC   FVMSGNO,=AL2(AE$INVEC)                                           
         B     EXITL               INVALID EDIT CODE                            
*                                                                               
VEDT10   TM    TLXDSTT,TLNEWEL     NEW ELEMENT ?                                
         BO    EXITOK              YES - OK                                     
         TM    XDAINDS,XDALAXD     ELEMENT ADDED TODAY ?                        
         BZ    EXITOK              YES - OK                                     
         CLC   BOBYTE1,TLEDIT                                                   
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CCFLD)                                           
         B     EXITL               CAN'T CHANGE THIS FIELD                      
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR MAX LEN                                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
MXLNDTA  LA    RF,MXLNTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
MXLNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISMXLN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMXLN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY MAX LEN                                                     *         
***********************************************************************         
         SPACE 1                                                                
DISMXLN  OC    TLMXLN,TLMXLN                                                    
         BZ    EXITOK                                                           
         LH    RE,TLMXLN                                                        
         CURED (RE),(3,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE MAX LEN                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALMXLN  OC    TLKCODE,TLKCODE                                                  
         BZ    EXITOK                                                           
         CLI   FVILEN,0                                                         
         BE    EXITNO              ERROR - NO INPUT                             
         TM    FVIIND,FVINUM       FIELD MUST BE NUMERIC IF INPUT               
         BZ    EXITNOTN                                                         
*                                                                               
         ICM   RE,15,BCFULL        NUMBER CONSTRAINED TO 1-80                   
         BZ    *+12                                                             
         CHI   RE,80                                                            
         BNH   VMXLN10                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL                                                            
*                                                                               
VMXLN10  TM    TLXDSTT,TLNEWEL     NEW ELEMENT ?                                
         BO    VMXLN20             YES - OK                                     
         TM    XDAINDS,XDALAXD     ELEMENT ADDED TODAY ?                        
         BZ    VMXLN20             YES - OK                                     
         CH    RE,TLMXLN                                                        
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CCFLD)                                           
         B     EXITL               CAN'T CHANGE THIS FIELD                      
*                                                                               
VMXLN20  STH   RE,TLMXLN                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR REQUIRED FIELD                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
REQDDTA  LA    RF,REQDTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
REQDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISREQD)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALREQD)                                
         DC    AL1(DHED),AL1(0,0,0),AL4(SETHED)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY REQUIRED FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISREQD  TM   TLSTAT1,XDFSRECP     NO RECEIPT COMPULSORY                        
         BZ   DISREQ2                                                           
         MVC  FVIFLD(L'UC@RCPTS),UC@RCPTS                                       
         B    EXITOK                                                            
*                                                                               
DISREQ2  MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    TLSTAT1,XDFSREQD                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE REQUIRED FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALREQD  OC    TLKCODE,TLKCODE                                                  
         BZ    EXITOK                                                           
*                                                                               
         MVC   BOBYTE1,TLSTAT1                                                  
         NI    TLSTAT1,FF-(XDFSREQD+XDFSRECP)                                   
         CLI   FVILEN,0                                                         
         BE    VREQD10                                                          
         CLC   FVIFLD(1),UC@RCPTS  NO RECEIPT COMPULSORY                        
         BNE   VREQD02                                                          
         OI    TLSTAT1,XDFSRECP                                                 
         B     VREQD10                                                          
*                                                                               
VREQD02  CLC   FVIFLD(1),BC@NO                                                  
         BE    VREQD10                                                          
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    TLSTAT1,XDFSREQD                                                 
*                                                                               
VREQD10  TM    TLXDSTT,TLNEWEL     NEW ELEMENT ?                                
         BO    EXITOK              YES - OK                                     
         TM    XDAINDS,XDALAXD     ELEMENT ADDED TODAY ?                        
         BZ    EXITOK              YES - OK                                     
         CLC   BOBYTE1,TLSTAT1                                                  
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CCFLD)                                           
         B     EXITL               CAN'T CHANGE THIS FIELD                      
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR CUTOFF DATE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
CUTDDTA  LA    RF,CUTDTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
CUTDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCUTD)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCUTD)                                
         DC    AL1(DHED),AL1(0,0,0),AL4(SETHED)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CUTOFF DATE                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISCUTD  OC    TLCUTOFF,TLCUTOFF                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(1,TLCUTOFF),(17,FVIFLD),0                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CUTOFF DATE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALCUTD  OC    TLKCODE,TLKCODE                                                  
         BZ    EXITOK                                                           
*                                                                               
         MVC   BODUB2(L'TLCUTOFF),TLCUTOFF                                      
         XC    TLCUTOFF,TLCUTOFF                                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         GOTO1 VDATVAL,BODMCB,FVIFLD,BODUB1                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         BZ    EXITL               INVAILD DATE                                 
*                                                                               
         GOTO1 VDATCON,BODMCB,BODUB1,(1,TLCUTOFF)                               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* OPTIONS OBJECT                                                      *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS DEFAULT HELP NUMBER IN 1ST BYTE. CHANGE IF REQUIRED.       *         
***********************************************************************         
         SPACE 1                                                                
OPT      LM    R0,R3,SVPARMS                                                    
         LA    RF,OPTTABL1                                                      
         B     ITER                                                             
*                                                                               
OPTTABL1 DC    AL1(OHLP),AL1(0,0,0),AL4(OPTHLP)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* OPTION HELP HOOK                                                    *         
***********************************************************************         
         SPACE 1                                                                
OPTHLP   CLI   CSACT,A#LST         LIST USES OPTIONS                            
         BE    EXITOK              HELP=DEFAULT                                 
         B     EXITL               OTHERWISE OPTIONS NOT USED.                  
         EJECT ,                                                                
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
         BNE   NTROU1                                                           
         CLI   SREC,R#XDAT         EXTRA DATA FIELD RECORD                      
         BE    *+12                                                             
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   EXITOK                                                           
         B     NTROU4                                                           
*                                                                               
NTROU1   CLI   BCPFKEY,PFK04       SEE IF IT IS ADD NEW XDLIST REC              
         BE    NTROU1A                                                          
         CLI   BCPFKEY,PFK05       SEE IF IT IS DISP NEW XDLIST                 
         BE    NTROU1A                                                          
         CLI   BCPFKEY,PFK06       SEE IF IT IS A CHANGE                        
         BNE   EXITOK                                                           
*                                                                               
NTROU1A  LH    R4,LS1STLIN                                                      
         A     R4,ATWA             ATWA=START OF SCREEN AREA                    
         L     RF,BCACUR                                                        
         CR    R4,RF               CHECK CURSOR POSITION IS IN XDFELD           
         BH    NTRERR              LIST AREA                                    
         LH    RE,LSBOTDSP         CHECK IF CURSOR IS ON PFKEY LINE             
         A     RE,ATWA                                                          
         CR    RF,RE                                                            
         BNL   NTRERR                                                           
*                                                                               
         USING FHD,RF                                                           
         CLC   FHDA(L'XDFCODE),BCSPACES                                         
         BE    NTRERR                                                           
         MVC   SDATA(L'XDFCODE),FHDA SAVE XDF DATA CODE                         
         OC    SDATA,BCSPACES        MAKE SURE BLANKS ARE SPACES                
         XR    RE,RE                                                            
         IC    RE,FHLN             JUMP TO 2ND FIELD                            
         AR    RF,RE                                                            
         IC    RE,FHLN             JUMP TO 3RD FIELD                            
         AR    RF,RE                                                            
         CLI   FHDA,C'X'           CHECK OUR EDIT CODE IS X                     
         BNE   NTRERR              XDLIST DROPDOWN LISTS                        
         DROP  RF                                                               
*                                                                               
NTROU4   OI    SNINDS1,SNIPARMS                                                 
         CLI   BCPFKEY,PFK04       CHECK IF WE'RE DOING AN ADD                  
         BE    NTROU5                                                           
         CLI   BCPFKEY,PFK05       A CHANGE......                               
         BE    NTROU5                                                           
         CLI   BCPFKEY,PFK06       A DISPLAY                                    
         BNE   EXITOK                                                           
*                                                                               
NTROU5   XC    IOKEY,IOKEY         READ XDL RECORD                              
         LA    R4,IOKEY            TO SEE IF IT EXISTS                          
         LA    RE,GSRECKEY                                                      
C        USING XDFRECD,RE                                                       
         USING XDLRECD,R4                                                       
         MVI   XDLKTYP,XDLKTYPQ                                                 
         MVI   XDLKSUB,XDLKSUBQ                                                 
         MVC   XDLKCPY,C.XDFKCPY                                                
         MVC   XDLKOFF,C.XDFKOFF                                                
         MVC   XDLKORTY,C.XDFKORTY                                              
         MVC   XDLKCLI,C.XDFKCLI                                                
         MVC   XDLKETY,C.XDFKETY                                                
         MVC   XDLKWC,C.XDFKWC                                                  
         MVC   XDLKMED,C.XDFKMED                                                
         MVC   XDLKSCH,C.XDFKSCH                                                
         MVC   XDLKCODE,SDATA                                                   
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         CLI   BCPFKEY,PFK04       CHECK IF WE'RE DOING AN ADD                  
         BE    NTROU6                                                           
         CLI   BCPFKEY,PFK05       A CHANGE......                               
         BE    NTROU7                                                           
         CLI   BCPFKEY,PFK06       A DISPLAY                                    
         BE    NTROU7                                                           
         DC    H'0'                                                             
*                                                                               
NTROU6   GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$XDLEX) ERROR XDLIST ALREADY EXISTS               
         L     RD,BCSVRD           ROOT'S RD                                    
         L     RD,8(,RD)           BACK UP ONE LVL SO EXITL TO ROOT             
         MVC   BOCURSOR,BCACUR                                                  
         B     EXITL                                                            
*                                                                               
NTROU7   GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$XDLNO) ERROR XDLIST DOESN'T EXIST                
         L     RD,BCSVRD           ROOT'S RD                                    
         L     RD,8(,RD)           BACK UP ONE LVL SO EXITL TO ROOT             
         MVC   BOCURSOR,BCACUR                                                  
         B     EXITL                                                            
*                                                                               
NTRERR   MVC   FVMSGNO,=AL2(AE$EDNOX) EDIT CODE NOT X                           
         L     RD,BCSVRD           ROOT'S RD                                    
         L     RD,8(,RD)           BACK UP ONE LVL SO EXITL TO ROOT             
         MVC   BOCURSOR,BCACUR                                                  
         B     EXITL                                                            
         DROP  R4,C                                                             
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER FROM HIGHER LEVEL        *         
***********************************************************************         
         SPACE 1                                                                
XITIN    CLI   SACT,A#LST          PAGE DISPLAY SCREEN FROM LIST SCREEN         
         BNE   EXITH                                                            
         NI    SNINDS1,FF-SNIUSECR TURN OFF USE CURRENT RECORD BIT              
         MVI   LSLTIND1,0          TURN OFF LIST INDICATORS                     
         OI    LSSCIND1,LSSCIBLD   AND REBUILD LIST                             
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING XDFRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING XDFRECD,R2                                                       
LAST     USING XDFRECD,R3                                                       
         LA    RF,LISTABL                                                       
         USING OBJTABD,RF                                                       
LITER    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK                                                           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    LITER02             MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     LITER               ITERATE TABLE                                
*                                                                               
LITER02  CLC   OBJIND3,GSSMPAGE    CHECK PAGE OK (0 FOR LIST)                   
         BE    LITER04                                                          
         LA    RF,OBJTABL(,RF)                                                  
         B     LITER                                                            
*                                                                               
LITER04  ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         SPACE 2                                                                
*                                                                               
LISTABL  DC    AL1(LINIT),AL1(0,0,0),AL4(ILST)                                  
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LTSARFIL),AL1(0,0,0),AL4(TSARFIL)                            
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LIST                                                     *         
***********************************************************************         
         SPACE 1                                                                
ILST     DS    0H                                                               
*&&UK*&& OI    LSSTAT3,LS3RVAR                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
X        USING XDFRECD,IOKEY                                                    
FLST     DS    0H                                                               
*&&US*&& OI    LSSTAT3,LS3RVAR                                                  
         MVC   X.XDFKEY,THIS.XDFKEY                                             
         L     R1,=AL4(XOHID+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         BE    NLST02                                                           
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     L     R1,=AL4(XOSQD+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         BE    NLST02                                                           
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
*                                                                               
NLST02   CLC   X.XDFKEY(XDFKREM-XDFRECD),THIS.XDFKEY                            
         BNE   EXITL                                                            
*                                                                               
         CLI   X.XDFKSEQ,0         FIRST XDATA RECORD                           
         BNE   NLST                NO - GET THE NEXT ONE                        
         CLI   CRECDEL,0           NO FILTER - DEFAULT IS DELETE=NO             
         BE    NLST04                                                           
         CLI   CRECDEL,YES         DELETE=YES                                   
         BE    NLST06                                                           
         CLI   CRECDEL,ONLY        DELETE=ONLY                                  
         BNE   NLST04                                                           
         TM    IOERR,IOEDEL        TEST IF RECORD IS DELETED                    
         BZ    NLST                NO - GET NEXT                                
         B     NLST06                                                           
*                                                                               
NLST04   TM    IOERR,IOEDEL        IT MUST BE DELETE=NO                         
         BO    NLST                                                             
*                                                                               
NLST06   MVC   THIS.XDFKEY(ACCKLEN),IOKEY                                       
         B     EXITOK                                                           
         DROP  LAST,X                                                           
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM FILE                                               *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL  L     R3,SVPARMS4                                                      
         USING TLSTD,R3                                                         
         L     R2,AIOREC                                                        
         USING XDFRECD,R2                                                       
         LA    RF,XDFRFST                                                       
         USING XDFELD,RF                                                        
         SR    RE,RE               RE=NUMBER OF XDFEL IN THIS RECORD            
         SR    R0,R0                                                            
TSFIL04  CLI   XDFEL,0             END OF REOCRD                                
         BE    TSFIL08                                                          
         CLI   XDFEL,XDFELQ        TEST TIME CHARGE INFO ELEMENT                
         BNE   TSFIL06                                                          
         AHI   RE,1                INCREMENT XDFEL COUNTER                      
         CHI   RE,MAXLINES                                                      
         BNL   TSFILX              NO MORE THAN 16 LIST LINES                   
*                                                                               
TSFIL06  IC    R0,XDFLN            ELEMENT LENGTH                               
         AR    RF,R0                                                            
         B     TSFIL04                                                          
*                                                                               
TSFIL08  LTR   RE,RE                                                            
         BNZ   TSFILX                                                           
         LA    RE,1                DEFAULT TO ONE ROW                           
*                                                                               
TSFILX   STC   RE,TLROWS           SET NUMBER OF ROWS PER LINE                  
*&&UK*&& MHI   RE,80               80*ROWS COLUMNS PER LIST LINE                
*&&UK*&& STH   RE,LSCOLLIN                                                      
         B     EXITOK                                                           
         DROP  R2,R3,RF                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT1,LSSBALL+LSSTSAR                                          
         OI    LSSTAT2,LSSNOSEQ+LSSADD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,XDFRFST-XDFRECD                                               
         STH   RF,MNTDISP                                                       
         MVI   READSEQ#,0                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1                                                    *         
* AIO5 -> NEXT XDATE RECORD IF THE FIRST ONE IS NOT BIG ENOUGH        *         
***********************************************************************         
         SPACE 1                                                                
FLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST XDATA RECORD?                          
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         AR    RF,R1               A(RECORD)                                    
         CR    RF,R1               MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    RF,XDFRFST-XDFRECD(,RF) IT IS NOW.                               
         XR    RE,RE                                                            
*                                                                               
         USING XDFELD,RF                                                        
FML02    CLI   XDFEL,0             RECORD END?                                  
         BNE   FML04               NO                                           
         BAS   RE,READNXT          READ NEXT XDATA RECORD                       
         BNE   EXITL               NO MORE RECORD                               
         LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIO5             NEW RECORD IN AIO5                           
         XR    RE,RE                                                            
*                                                                               
FML04    CLI   XDFEL,XDFELQ        XDFEL?                                       
         BNE   NML06               NO                                           
                                                                                
FML08    L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST XDATA RECORD?                          
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         SR    RF,R1                                                            
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1                                                     *         
***********************************************************************         
NLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST XDATA RECORD?                          
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         AR    RF,R1               A(RECORD)                                    
         XR    RE,RE                                                            
         CR    RF,R1               MAKE SURE MNTDISP INITIALISED                
         BH    NML06                                                            
         LA    RF,XDFRFST-XDFRECD(,RF) IT IS NOW.                               
*                                                                               
         USING XDFELD,RF                                                        
NML02    CLI   XDFEL,0             RECORD END?                                  
         BNE   NML04               NO                                           
         BAS   RE,READNXT          READ NEXT XDATA RECORD                       
         BNE   EXITL               NO MORE RECORD                               
         LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIO5             NEW RECORD IN AIO5                           
         XR    RE,RE                                                            
*                                                                               
NML04    CLI   XDFEL,XDFELQ        XDFEL?                                       
         BE    NML08               YES                                          
                                                                                
NML06    IC    RE,XDFLN                                                         
         LA    RF,0(RE,RF)                                                      
         B     NML02                                                            
*                                                                               
NML08    L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST XDATA RECORD?                          
         BE    *+8                                                              
         L     R1,AIO5                                                          
                                                                                
         SR    RF,R1                                                            
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
         L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST XDATA RECORD?                          
         BE    *+8                                                              
         L     R1,AIO5                                                          
                                                                                
         AR    RF,R1               A(RECORD)                                    
         USING XDFELD,RF           MOVE IN DETAILS FROM ELEMENT                 
         MVI   TLXDSTT,0                                                        
         MVC   TLSEQNO,XDFSEQ                                                   
         MVC   TLKCODE,XDFCODE                                                  
         MVC   TLEDIT,XDFEDIT                                                   
         MVC   TLMXLN,XDFMXLN                                                   
         MVC   TLSTAT1,XDFSTAT1                                                 
         MVC   TLCUTOFF,XDFCUT                                                  
         MVC   TLNAME,BCSPACES                                                  
         MVI   TLNMLEN,0                                                        
         CLI   XDFLN,XDFLN1Q                                                    
         BNH   EXITOK                                                           
         SR    RE,RE                                                            
         IC    RE,XDFLN                                                         
         SHI   RE,XDFLN1Q                                                       
         CHI   RE,L'XDFNAME                                                     
         BNH   *+8                                                              
         LHI   RE,L'XDFNAME        USE MAX. LENGTH                              
         STC   RE,TLNMLEN                                                       
         BCTR  RE,0                                                             
         EXMVC RE,TLNAME,XDFNAME                                                
         B     EXITOK                                                           
         DROP  RF,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 MVI   ANYLINES,NO                                                      
         MVI   ADDSEQ#,0                                                        
         MVI   LSTSEQNO,0                                                       
         CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE              
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
*                                                                               
         LH    RF,LSLST#X                                                       
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITEMS         MAX OF 100 LINES                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         L     R1,AIOREC                                                        
         BAS   RE,GETLSN           GET LAST SEQUENCE NUMBER                     
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('XDFELQ',AIOREC),0                
*                                                                               
         L     RF,AIOREC                                                        
         MVC   IOKEY(L'XDFKEY),0(RF)                                            
*                                                                               
T        USING XDFRECD,R4                                                       
UPDF104  LA    R4,IOKEY            READ NEXT XDATA SUB-RECORD                   
         SR    RF,RF               THEY ARE ALWAYS IN SEQUENCE                  
         IC    RF,T.XDFKSEQ                                                     
         AHI   RF,1                                                             
         STC   RF,T.XDFKSEQ                                                     
*                                                                               
         L     R1,=AL4(XOHIUPD+XOACCDIR+XIO2)                                   
         GOTO1 AIO                                                              
         CLC   T.XDFKEY(XDFKSEQ-XDFRECD),IOKEYSAV                               
         BNE   EXITOK              EXIT - NOT SUB-RECORD                        
         TM    IOERR,FF-IOEDEL                                                  
         BZ    *+6                                                              
         DC    H'0'                ERROR READING THE RECORD                     
*                                                                               
         OI    T.XDFKSTAT,XDFSDELT DELETE XDATA DIR                             
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD DIR RECORD                               
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
         L     R4,AIO2                                                          
         TM    T.XDFRSTAT,XDFSDELT RECORD DELETED ?                             
         BO    UPDF108                                                          
         OI    T.XDFRSTAT,XDFSDELT DELETE MASTER RECORD                         
         LR    R1,R4                                                            
         BAS   RE,GETLSN           GET LAST SEQUENCE NUMBER                     
*                                                                               
UPDF108  GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('XDFELQ',AIO2),0                  
*                                                                               
         LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                                                              
         GOTO1 DELPAS,BOPARM,AIO2                                               
         B     UPDF104                                                          
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
* AIO6 = A(XDATA RECORD) IF ADDSEQ# IS NOT 0                          *         
***********************************************************************         
         SPACE 1                                                                
         USING XDFRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE OR ADD               
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
*                                                                               
         MVI   ANYLINES,YES        WE HAVE AT LEAST ONE INPUT LINE              
         LM    R2,R3,SVPARMS3                                                   
         CLI   ADDSEQ#,0           FIRST XDATA RECORD?                          
         BE    *+8                                                              
         L     R2,AIO6             R4 = NEW XDATA RECORD                        
*                                                                               
T        USING XDFELD,BOELEM       BUILD NEW ELEMENT                            
         XC    BOELEM,BOELEM                                                    
         MVI   T.XDFEL,XDFELQ                                                   
         MVI   T.XDFLN,XDFLN1Q                                                  
         MVC   T.XDFSEQ,TLSEQNO                                                 
*                                                                               
         TM    TLXDSTT,TLNEWEL     NEW XDATA ELEMENT                            
         BZ    UREC106                                                          
         CLI   LSTSEQNO,255        ANY SPARE SEQ. NO                            
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               NO - TOO MANY ITEMS                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,LSTSEQNO                                                      
         AHI   RE,1                                                             
         STC   RE,LSTSEQNO                                                      
         STC   RE,T.XDFSEQ         NEW SEQUENCE NO                              
*                                                                               
UREC106  MVC   T.XDFCODE,TLKCODE                                                
         MVC   T.XDFEDIT,TLEDIT                                                 
         MVC   T.XDFMXLN,TLMXLN                                                 
         MVC   T.XDFSTAT1,TLSTAT1                                               
         MVC   T.XDFCUT,TLCUTOFF                                                
         SR    RE,RE                                                            
         IC    RE,TLNMLEN          LENGTH OF XDATA NAME                         
         BCTR  RE,0                                                             
         EXMVC RE,T.XDFNAME,TLNAME                                              
         LA    RE,XDFLN1Q+1(,RE)                                                
         STC   RE,T.XDFLN                                                       
         DROP  T                                                                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,XDFRLEN                                                     
         CHI   RF,IOMAXLNQ         GREATER MAX RECORD ALLOWED ?                 
         BNH   UREC108             NO - ADD IT INTO CURRENT RECORD              
*                                                                               
         CLI   ADDSEQ#,0           FIRST XDATA RECORD ?                         
         BE    *+8                                                              
         BAS   RE,ADDXDF           NO - ADD SAVED XDATA RECORD IN AIO6          
*                                                                               
         L     R2,AIO6             R2=A(NEW XDATA SUB-RECORD)                   
         L     RF,AIOREC                                                        
         MVC   XDFKEY(L'XDFKEY+L'XDFRLEN+L'XDFRSTA),0(RF)                       
         SR    RF,RF                                                            
         IC    RF,ADDSEQ#                                                       
         AHI   RF,1                                                             
         STC   RF,ADDSEQ#          NEXT SEQUENCE NUMBER                         
         STC   RF,XDFKSEQ                                                       
*                                                                               
         LA    RE,XDFRFST                                                       
         MVI   0(RE),0                                                          
         SR    RE,R2                                                            
         AHI   RE,1                                                             
         STCM  RE,3,XDFRLEN        LENGTH OF XDATA RECORD                       
*                                                                               
UREC108  GOTO1 AADDEL,BOPARM,(R2)  ADD NEW XDATA ELEMENT                        
         BE    EXITOK                                                           
         DC    H'0'                ERROR ADDING ELEMENT                         
         DROP  R2,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 CLI   CSACT,A#CHA                                                      
         BE    *+12                                                             
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
         CLI   ANYLINES,YES        EMPTY LIST?                                  
         BNE   ULAST104            NO - OK                                      
         CLI   ADDSEQ#,0           FIRST XDATA RECORD ?                         
         BE    EXITOK                                                           
         BAS   RE,ADDXDF           NO - ADD SAVED XDATA RECORD IN AIO6          
*                                                                               
         L     RF,AIOREC           RESTORE IOADDR FOR CONTRALLER                
         ST    RF,IOADDR                                                        
         B     EXITOK                                                           
*                                                                               
ULAST104 MVC   FVMSGNO,=AL2(AE$NLINE)                                           
         LH    RF,LS1STLIN                                                      
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         CLI   CSACT,A#ADD                                                      
         BE    EXITL                                                            
         NI    LSLTIND1,FF-LSLTIBLD REBUILD THE LIST                            
         XC    GCLASKEY,GCLASKEY    SET KEY HAS BEEN CHANGED                    
         NI    GSINDSL1,FF-GSIXMNT  TURN OF MAINT SCREEN LOADED FLAG            
         B     EXITL                                                            
         EJECT ,                                                                
***********************************************************************         
* SET COLUMN HEADINGS TO NORMAL INTENSITY                             *         
***********************************************************************         
         SPACE 1                                                                
SETHED   L     R1,CALLR1                                                        
*&&US*&& L     RE,SVPARMS5         HEADLINE 1                                   
*&&US*&& L     RF,SVPARMS6         HEADLINE 2                                   
*&&UK                                                                           
         ICM   RE,15,24(R1)        RE=A(HEADLINE 1 FIELD HEADER)                
         ICM   RF,15,28(R1)        RF=A(HEADLINE 2 FIELD HEADER)                
         NI    1(RE),FF-X'08'      SET TO NORMAL INTENSITY                      
         NI    1(RF),FF-X'08'      SAME FOR HEADLINE 2                          
         OI    6(RE),X'80'         AND TRANSMIT                                 
         OI    6(RF),X'80'                                                      
*&&                                                                             
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO READ THE NEXT XDATA RECORD                               *         
* EXIT - READSEQ# : NEXT RECORD SEQUENCE NUMBER                       *         
*      - MNTDISP: DISPLACEMENT TO THE FIRST ELEMENT                   *         
***********************************************************************         
         SPACE 1                                                                
T        USING XDFRECD,IOKEY                                                    
READNXT  NTR1  ,                                                                
         L     RF,AIOREC           A(CURRENCT XDATA RECORD)                     
         MVC   T.XDFKEY,0(RF)                                                   
         SR    RF,RF                                                            
         IC    RF,READSEQ#                                                      
         AHI   RF,1                                                             
         STC   RF,READSEQ#                                                      
         STC   RF,T.XDFKSEQ                                                     
*                                                                               
         LHI   R1,XOREAD+XOACCMST+XIO5                                          
         GOTO1 AIO                                                              
         BNE   EXITL               NO MORE XDATA RECORD                         
         LA    RF,XDFRFST-XDFRECD                                               
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD XDATA RECORD                                         *         
* EXIT - AIO6: A(NEW RECORD)                                          *         
***********************************************************************         
         SPACE 1                                                                
T        USING XDFRECD,R4                                                       
ADDXDF   NTR1  ,                                                                
         LA    R4,IOKEY                                                         
         L     RF,AIO6                                                          
         MVC   T.XDFKEY,0(RF)                                                   
*                                                                               
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO2)   READ FOR UPDATE                 
         GOTO1 AIO                                                              
         BE    ADDXDF10                                                         
         TM    IOERR,IOERRS-(IOEDEL+IOERNF)                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVIOERR,IOERR       SAVE IOERR                                   
         TM    IOERR,IOERNF                                                     
         BNO   ADDXDF10                                                         
         MVC   IOKEY,IOKEYSAV      RESTORE KEY IF NOT FOUND                     
         B     ADDXDF20                                                         
*                                                                               
ADDXDF10 NI    T.XDFKSTAT,FF-XDFSDELT  SET DELETE OFF                           
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
ADDXDF20 L     R0,AIO6             COPY FROM SUB-RECORD                         
         L     RE,AIO2             COPY TO SUB-RECORD                           
         LA    RF,IOAREALN         L'IOAREA                                     
         LR    R1,RF               LENGTH IS LENGTH OF RECORD                   
         MVCL  RE,R0                                                            
*                                                                               
         LHI   R1,XOADDREC+XOACCMST+XIO2   ADD DIR + FILE RECORDS               
         TM    SVIOERR,IOERNF                                                   
         BO    *+8                                                              
         LHI   R1,XOPUTREC+XOACCMST+XIO2   CHANGE XDATA RECORD                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                ERROR - CAN'T PUT RECORD BACK                
         GOTO1 ADDPAS,BOPARM,AIO2                                               
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET LAST SEQUENCE NUMBER FOR XDATA ELEMENTS              *         
* ETRY - R1      : XDATA RECORD                                       *         
* EXIT - LSTSEQNO: LAST SEQUENCE NUMBER                               *         
***********************************************************************         
         SPACE 1                                                                
GETLSN   NTR1  ,                                                                
         LA    R1,XDFRFST-XDFRECD(R1)                                           
         USING XDFELD,R1                                                        
         SR    R0,R0                                                            
GETLST10 CLI   XDFEL,0             END OF RECORD                                
         BE    EXITOK                                                           
         CLI   XDFEL,XDFELQ                                                     
         BNE   GETLST20                                                         
         CLC   LSTSEQNO,XDFSEQ                                                  
         BH    GETLST20                                                         
         MVC   LSTSEQNO,XDFSEQ     SET LAST SEQUENCE NUMBER                     
GETLST20 IC    R0,XDFLN                                                         
         AR    R1,R0                                                            
         B     GETLST10                                                         
         DROP R1                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD XDF PASSIVES                                         *         
* NTRY - P1  = APPROVER RECORD                                                  
***********************************************************************         
         SPACE 1                                                                
F        USING XDFRECD,R3                                                       
ADDPAS   NTR1  ,                                                                
         L     R3,0(R1)                                                         
         LA    R2,IOKEY                                                         
         MVC   IOKEY,F.XDFKEY                                                   
         L     R1,=AL4(XORDD+XOACCDIR+XIO4) READ DIR FOR DA                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   XDFSAVDA,IOKEY+XDFKDA-XDFRECD                                    
*                                                                               
T        USING CPTRBLK,CPTRWRK                                                  
         XC    T.CPTRBLK(CPTRBLKL),T.CPTRBLK                                    
         GOTO1 VPADDLE,BODMCB,(C'A',(R3)),T.CPTRBLK,XDFSAVDA,0,ACOM             
         B     EXITOK                                                           
         DROP  F,T                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DEL XDF PASSIVES                                         *         
* NTRY - P1  = APPROVER RECORD                                                  
***********************************************************************         
         SPACE 1                                                                
DELPAS   NTR1  ,                                                                
*                                                                               
         L     R3,0(R1)                                                         
                                                                                
T        USING CPTRBLK,CPTRWRK                                                  
DELPAS2  XC    T.CPTRBLK(CPTRBLKL),T.CPTRBLK                                    
         GOTO1 VPADDLE,BODMCB,(C'D',(R3)),(C'K',T.CPTRBLK),0,0,ACOM             
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
STMPSTRQ EQU   X'03'               TEMPSTORE PAGE NO. FOR USE IN SEARCH         
MAXLINES EQU   15                  MAXIMUM LIST LINES                           
MAXDISL  EQU   11                  MAXIMUM DISPLAY LINES                        
MAXITEMS EQU   100                                                              
IOMAXLNQ EQU   1800                                                             
*                                                                               
PRODUL   DC    C'SJ'               PRODUCTION UNIT/LEDGER CODE                  
OFFUL    DC    C'2D'                                                            
EXTYP    DC    C'ET'                                                            
WCQ      DC    C'WC'                                                            
MEDIAQ   DC    C'MEDIA'                                                         
         SPACE 2                                                                
DCLIST   DS    0D                                                               
         DCDDL AC#DATE,L'UC@DATE,L                                              
         DCDDL AC#NUM,L'UC@NUM,L                                                
         DCDDL AC#LTRS2,L'UC@LTRS2,L                                            
         DCDDL AC#AMT,L'UC@AMT,L                                                
         DCDDL AC#FLPO,L'UC@FLPO,L                                              
         DCDDL AC#FLEO,L'UC@FLEO,L                                              
         DCDDL AC#FLAO,L'UC@FLAO,L                                              
         DCDDL AC#FLEX,L'UC@FLEX,L                                              
         DCDDL AC#FLES,L'UC@FLES,L                                              
         DCDDL AC#INT2,L'UC@INT,L                                               
         DCDDL AC#RCPTS,L'UC@RCPTS,L                                            
DCLISTX  DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
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
XDFSAVDA DS    CL(L'XDFKDA)                                                     
SVIOKEY  DS    XL42                                                             
*                                                                               
MYHALF   DS    H                                                                
READSEQ# DS    XL(L'XDFKSEQ)                                                    
ADDSEQ#  DS    XL(L'XDFKSEQ)                                                    
LSTSEQNO DS    XL(L'XDFSEQ)                                                     
PRTMORE  DS    CL1                 LAST ROW ON LIST SCREEN                      
XDAINDS  DS    XL1                                                              
XDALAXD  EQU   X'80'               LIMIT AMENTMENTS ALLOWED                     
DATERR   DS    XL1                 ERROR BYTE                                   
DATNPLDG EQU   X'80'               NO PRODUCTION LEDGER                         
SVIOERR  DS    CL(L'IOERR)                                                      
*                                                                               
CLILEN   DS    XL1                 LENGTH OF CLIENT                             
*                                                                               
ANYLINES DS    CL1                                                              
MNTDISP  DS    H - MOVE TO SAVED STORAGE                                        
*                                                                               
DSLISTU  DS    0F                                                               
UC@DATE  DS    CL4                 DATE                                         
UC@NUM   DS    CL3                 NUM                                          
UC@LTRS2 DS    CL7                 LETTERS                                      
UC@AMT   DS    CL6                 AMOUNT                                       
UC@FLPO  DS    CL2                 PRODUCTION ORDERS                            
UC@FLEO  DS    CL2                 EXPENSE ORDERS                               
UC@FLAO  DS    CL2                 ARTIST ORDERS                                
UC@FLEX  DS    CL2                 EXPENSE CLAIMS                               
UC@FLES  DS    CL2                 ESITMATE                                     
UC@INT   DS    CL2                 INTERNAL                                     
UC@RCPTS DS    CL2                 RECEIPTS                                     
*                                                                               
CPTRWRK  DS    XL128               CPTR WORKING STORAGE                         
OVERWRKN EQU   *-OVERWRKD                                                       
         EJECT ,                                                                
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKCODE  DS    CL(L'XDFCODE)       XDATA CODE                                   
         ORG   TLUSER                                                           
TLXDSTT  DS    XL1                 STATUS                                       
TLNEWEL  EQU   X'80'               NEW ELEMENT                                  
TLSEQNO  DS    XL(L'XDFSEQ)        FIELD SEQUENCE NO                            
TLNMLEN  DS    XL1                 LENGTH OF DATA NAME                          
TLNAME   DS    CL(L'XDFNAME)       XDATA NAME                                   
TLEDIT   DS    CL(L'XDFEDIT)       EDIT RULE                                    
TLMXLN   DS    H                   MAXIMUN LENGTH                               
TLSTAT1  DS    XL1                 STATUS 1                                     
TLCUTOFF DS    PL3                 CUTOFF DATE                                  
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACFIL48   08/10/11'                                      
         END                                                                    
