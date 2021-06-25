*          DATA SET ACFIL1F    AT LEVEL 010 AS OF 08/10/11                      
*PHASE T6231FC                                                                  
         SPACE 1                                                                
FIL1F    TITLE 'BRANDOCEAN FORMAT RECORD'                                       
         SPACE 1                                                                
*TKLU 001 16MAY06 <DU01-4565> NEW OVERLAY FOR MCS                               
*TKLU 002 09FEB07 <LO01-6079> ORDER FORMAT CODE SUPPORT                         
*TKLU 003 16FEB07 <LO01-6079> ORDER FORMAT CODE SUB LISTS                       
*TKLU 004 09MAY07 <LO01-6412> FORMAT RECORD OFFICE SET UP                       
*MPEN 005 12JUN09 <LO01-9013> CHANGES TO EXPENDITURE TYPE RECORD                
         SPACE 1                                                                
FIL1F    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL1F**,R7,RR=RE                                              
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
*                                                                               
         L     R6,ATWA                                                          
         MVC   SESNL,TWASESNL-TWAD(R6)                                          
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
EXIT     L     R1,CALLR1           RETURN PARAMETERS TO CALLER                  
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
EXITLONG MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL               EXIT WITH FIELD TOO LONG SET                 
EXITOFF  MVC   FVMSGNO,=AL2(AE$EXPNV) EXPENDITURE TYPE NOT VALID FOR            
         B     EXITL                  THIS OFFICE                               
*                                                                               
EXITIACT MVC   FVMSGNO,=AL2(AE$IACTS)   INVALID ACTION FOR THIS SCREEN          
         LH    RF,GSDSPACT         SET CURSOR TO ACTION FIELD                   
         A     RF,ATWA                                                          
         ST    RF,FVADDR                                                        
         B     EXITL                                                            
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT NOT WANTED FOR FILTER                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLISTU,DSLISTU                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDING           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITERH    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               * NOT OVERRIDE                               
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITERH               ITERATE TABLE                                
*                                                                               
ITER02   ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         LA    R1,SVPARMS                                                       
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITERH                                                            
*                                                                               
TABLEOO  DC    AL1(OSCRN),AL1(0,0,0),AL4(SCREEN)                                
         DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OOPT),AL1(0,0,0),AL4(OPT)                                    
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SCREEN OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(SCREEN)                                                        *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
SCREEN   LM    R0,R3,SVPARMS                                                    
         LA    RF,TABLSCR                                                       
         B     ITER                                                             
*                                                                               
TABLSCR  DC    AL1(SSET),AL1(0,0,0),AL4(SCRMSET)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* SET MAINTENANCE DATA SCREEN                                         *         
***********************************************************************         
         SPACE 1                                                                
SCRMSET  MVI   GSSMCODE,0                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT ,                                                                
***********************************************************************         
* KEY OBJECT                                                          *         
* ----------                                                          *         
* SVPARMS1 HOLDS EQUATED OBJECT                                       *         
* SVPARMS2 HOLDS EQUATED VERB                                         *         
* SVPARMS3 A(KEY)                                                     *         
* SVPARMS4 HOLDS SUB-ACTION                                           *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R1,R2,SVPARMS2                                                   
         USING ERFRECD,R2                                                       
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
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   XC    ERFKEY,ERFKEY                                                    
         MVI   ERFKTYP,ERFKTYPQ                                                 
         MVI   ERFKSUB,ERFKSUBQ                                                 
         MVC   ERFKCPY,CUABIN                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    ERFKEY,ERFKEY                                                    
         MVI   ERFKTYP,ERFKTYPQ                                                 
         MVI   ERFKSUB,ERFKSUBQ                                                 
         MVC   ERFKCPY,CUABIN                                                   
         B     EXITOK                                                           
         EJECT ,                                                                
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
         USING ERFRECD,R2                                                       
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
         DC    AL1(RWRT),AL1(0,0,0),AL4(RFWRT)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD OR COPY                          *         
***********************************************************************         
         SPACE 1                                                                
RFADD    CLI   CSACT,A#ADD                                                      
         BNE   RFADD02                                                          
         CLI   GSSMPAGE,1                                                       
         BNE   EXITIACT                                                         
*                                                                               
RFADD02  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',ERFRECD),0               
         CLI   12(R1),0            RSTEL ON RECORD?                             
         BE    RFADD04                                                          
         GOTO1 AADDRST,ERFRECD     ADD A RSTEL IF IT DOESN`T EXIST              
         BNE   EXITL               SOMETHING WRONG                              
         B     RFADD06                                                          
*                                                                               
RFADD04  L     RF,12(,R1)                                                       
         USING RSTELD,RF                                                        
         MVC   RSTBDATE,BCTODAYP   SET TO TODAY                                 
         MVC   RSTTDATE,BCTODAYP                                                
*                                                                               
RFADD06  DS    0H                                                               
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD OR COPY                          *         
***********************************************************************         
         SPACE 1                                                                
RFWRT    GOTO1 DELRECS,BOPARM,ERFRECD                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
RECLAST  L     R1,SVPARMS4                                                      
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
RLTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLWRT)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RLDEL)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - ADD                                   *         
***********************************************************************         
         SPACE 1                                                                
RLADD    GOTO1 ADDRECS,BOPARM,ERFRECD                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DELETE                                *         
***********************************************************************         
         SPACE 1                                                                
RLDEL    GOTO1 DELRECS,BOPARM,ERFRECD                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE                                 *         
***********************************************************************         
         SPACE 1                                                                
RLWRT    GOTO1 ADDRECS,BOPARM,ERFRECD                                           
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                               
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
         USING ERFRECD,R2                                                       
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
         LA    RF,KNOWLQ(,RF)                                                   
         B     DATA04                                                           
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
*                                                                               
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING ERFRECD,R2          R2 HOLDS A(RECORD)                           
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                         *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(F#ERFO#CODE),AL4(FRMDTA)    FORMAT CODE                      
         DC    AL2(F#ERFO#APPL),AL4(APPDTA)    APPLICATION                      
         DC    AL2(F#ERFO#SUPC),AL4(SALDTA)    SUPPLIER ACCOUNT LIST            
         DC    AL2(F#ERFO#SUPN),AL4(SANDTA)    SUPPLIER ACC NAME LIST           
         DC    AL2(F#ERFO#ETYC),AL4(ETCDTA)    EXPENDITURE CODE LIST            
         DC    AL2(F#ERFO#ETYN),AL4(ETNDTA)    EXPENDITURE NAME LIST            
         DC    AL2(F#ERFO#OTYP),AL4(OTYDTA)    ORDER TYPE LIST                  
         DC    AL2(F#ERFO#OFFC),AL4(OFFDTA)    OFFICE FILTER                    
         DC    AL2(F#ERFO#OFFN),AL4(OFNDTA)    OFFICE NAME                      
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL1F    CSECT                                                                  
         EJECT                                                                  
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
DFDVAL   CLM   R2,7,ATLST+1        TEST PROCESSING LIST 1 (NOT LIST 0)          
         BNE   EXITOK                                                           
         OI    ETLBLK,X'40'        RESET ANY 'DEFAULT' INDS SET BY LIST         
         OI    OTLBLK,X'40'        BLOCKS NOW USED FOR DUP CHECKING             
         OI    SALBLK,X'40'                                                     
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS3                                                      
         LA    RF,DLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                             *** LAST TIME FOR DATA OBJECT ***                 
*                                 ------------------------                      
DLTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FORMAT CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FRMDTA   LA    RF,FRMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FRMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFRM)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISFRM)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSTFRM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFRM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FORMAT CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISFRM   MVC   FVIFLD(L'ERFKFRMT),ERFKFRMT                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FORMAT CODE FIELD ON NTRSES                               *         
***********************************************************************         
         SPACE 1                                                                
DSTFRM   DS    0H                                                               
         B     FLTXX               RETURN 'NOT VALID' TO FORCE UNPROT           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A REPORT FORMAT CODE                                       *         
***********************************************************************         
         SPACE 1                                                                
VALFRM   GOTO1 ACHKFLD,BOPARM,('CKKEYQ',CKTAB1Q)                                
         BNE   EXITNV              INVALID                                      
         MVC   ERFKFRMT,FVIFLD                                                  
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
OFFDTA   LA    RF,OFFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFF)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETOFF)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOFF)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTOFF)                               
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
T        USING ERFRSTA,GSRECSTA                                                 
DISOFF   MVC   FVIFLD(L'ERFRSOFF),T.ERFRSOFF                                    
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
T        USING ERFRSTA,GSRECSTA                                                 
VALOFF   MVC   T.ERFRSOFF,BCSPACES                                              
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,2                                                         
         BH    EXITLONG                                                         
         MVC   T.ERFRSOFF,FVIFLD                                                
*                                                                               
         CLI   CSACT,A#DIS         DON'T TEST OFFICE CODE                       
         BE    EXITOK              IF DISPLAY OR LIST                           
         GOTO1 ATSTOFF,FVIFLD      TEST OFFICE CODE                             
         BNE   EXITL                                                            
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OFFICE CODE FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DFLTOFF  MVC   FVIFLD(L'ERFRSOFF),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE CODE FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
VFLTOFF  MVC   FLTIFLD,BCSPACES                                                 
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'ERFRSOFF                                                
         BH    EXITLONG                                                         
         MVC   FLTIFLD(2),FVIFLD                                                
         OC    FLTIFLD(2),BCSPACES                                              
         GOTO1 ATSTOFF,FLTIFLD     TEST OFFICE CODE                             
         BNE   EXITL                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR OFFICE CODE                                        *         
* OVERLAY WILL DO ITS OWN FILTERING                                   *         
***********************************************************************         
         SPACE 1                                                                
DOFTOFF  CLC   FLTIFLD(2),BCSPACES                                              
         BNH   FLTXE                                                            
         CLC   ERFRSOFF,FLTIFLD                                                 
         BE    FLTXE                                                            
         B     FLTXL                                                            
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
OFNDTA   LA    RF,OFNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE CODE NAME FIELD FROM THE KEY                       *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
DISOFN   CLC   ERFRSOFF,BCSPACES   TEST ALL OFFICE                              
         BE    EXITOK              YES - DON'T DISPLAY OFFICE CODE NAME         
*                                                                               
         MVC   IOKEY,BCSPACES                  READ THE ACCOUNT RECORD          
         MVC   T.ACTKCPY,ERFKCPY               COMPANY                          
         MVC   T.ACTKUNT(L'OFFUL),OFFUL        UNIT/LEDGER                      
         MVC   T.ACTKACT(L'ERFRSOFF),ERFRSOFF  OFFICE CODE CODE                 
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
* DATA OBJECT FOR APPLICATION TYPE                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APPDTA   LA    RF,APPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
APPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPP)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISAPP)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSTAPP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPP)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTAPP)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALAPP)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTAPP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY APPLICATION TYPE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISAPP   LA    RE,AC@EST                                                        
         CLI   ERFKAPPL,ERFKESTQ                                                
         BE    DISAPP2                                                          
         LA    RE,AC@ORDER                                                      
         CLI   ERFKAPPL,ERFKORDQ                                                
         BE    DISAPP2                                                          
         DC    H'0'                                                             
         SPACE 1                                                                
DISAPP2  MVC   FVIFLD(L'ERFKAPPL),0(RE)                                         
         MVC   XAPPTYP,ERFKAPPL                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A APPLICATION FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTAPP  LA    RE,AC@EST                                                        
         CLI   FLTIFLD,ERFKESTQ                                                 
         BE    DFLTAPP2                                                         
         LA    RE,AC@ORDER                                                      
         CLI   FLTIFLD,ERFKORDQ                                                 
         BE    DFLTAPP2                                                         
         LA    RE,BCSPACES                                                      
         SPACE 1                                                                
DFLTAPP2 MVC   FVIFLD(L'ERFKAPPL),0(RE)                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR APPLICATION                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFTAPP  CLC   ERFKAPPL,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT APPLICATION FIELD ON NTRSES                               *         
***********************************************************************         
         SPACE 1                                                                
DSTAPP   DS    0H                                                               
         B     FLTXX               RETURN 'NOT VALID' TO FORCE UNPROT           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE APPLICATION TYPE                                           *         
***********************************************************************         
         SPACE 1                                                                
VALAPP   LA    RE,ERFKESTQ         ESTIMATE                                     
         CLC   FVIFLD(1),AC@EST                                                 
         BE    VALAPP2                                                          
         LA    RE,ERFKORDQ         ORDER                                        
         CLC   FVIFLD(1),AC@ORDER                                               
         BE    VALAPP2                                                          
         B     EXITNV              INVALID                                      
         SPACE 1                                                                
VALAPP2  STC   RE,ERFKAPPL                                                      
         MVC   FLTIFLD(L'ERFKAPPL),ERFKAPPL                                     
         MVC   XAPPTYP,ERFKAPPL                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SUPPLIER ACCOUNT LIST                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
SALDTA   LA    RF,SALTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SALTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSAL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSAL)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHSA)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SUPPLIER ACCOUNT LIST FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DISSAL   CLI   XAPPTYP,ERFKESTQ                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'TLKSA),TLKSA                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SUPPLIER ACCOUNT LIST FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
VALSAL   XC    TLKSA,TLKSA                                                      
         MVC   TLRLEN,=AL2(TLLNQ)  SET TSAR RECLEN                              
         CLI   FVILEN,0                                                         
         BNE   VALSAL2                                                          
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
VALSAL2  CLI   XAPPTYP,ERFKORDQ                                                 
         BNE   EXITNV                                                           
         CLI   FVILEN,2                                                         
         BNH   VALSAINV            INVALID ACCOUNT                              
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES   READ FOR SUPPLIER ACCOUNT                    
         MVC   T.ACTKCPY,CUABIN                                                 
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         MVC   T.ACTKUNT(0),FVIFLD                                              
         EX    RE,*-6                                                           
         CLC   =C'ST',T.ACTKUNT                                                 
         BE    VALSAL4                                                          
         CLC   =C'SV',T.ACTKUNT                                                 
         BE    VALSAL4                                                          
         CLC   =C'SX',T.ACTKUNT                                                 
         BE    VALSAL4                                                          
         CLC   =C'SK',T.ACTKUNT                                                 
         BE    VALSAL4                                                          
         CLI   CUCTRY,CTRYGER                                                   
         BE    VALSAINV                                                         
         CLC   =C'SI',T.ACTKUNT                                                 
         BNE   VALSAINV                                                         
VALSAL4  LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   VALSAINV            INVALID ACCOUNT                              
         GOTO1 AGETEL,BOPARM,('ABLELQ',AIO2),0                                  
         BNE   VALSAINV            NOT A LOW-LEVEL ACCOUNT                      
         MVC   TLKSA,T.ACTKUNT                                                  
         B     EXITOK                                                           
         SPACE 1                      * ERROR EXITS *                           
VALSAINV MVC   FVMSGNO,=AL2(AE$INACC) INVALID ACCOUNT                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'ACTKULA),FVIFLD                                         
         B     EXITL                                                            
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* SEARCH ON AN SUPPLIER A/C FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
SRCHSA   GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,0,ACOM,        >        
               (X'44',0)                                                        
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A SUPPLIER ACCOUNT NAME FIELD            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
SANDTA   LA    RF,SANTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SANTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSAN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SUPPLIER ACCOUNT NAME FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTKEY,IOKEY                                                     
DISSAN   CLC   TLKSA,BCSPACES                                                   
         BNH   EXITOK                                                           
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,TLKSA       SUPPLIER U/L/A FROM TSAR REC                 
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         SPACE 1                                                                
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EXPENDITURE TYPE FIELD                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ETCDTA   LA    RF,ETCTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
ETCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISETC)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHETC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALETC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY EXPENDITURE TYPE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISETC   CLI   XAPPTYP,ERFKESTQ                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'TLKET),TLKET                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A EXPENDITURE TYPE FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
SRCHETC  GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,EXTYP,ACOM,0            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE EXPENDITURE TYPE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VALETC   XC    TLKET,TLKET                                                      
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   VALETC2                                                          
         OI    LSLNIND1,LSLNIDEL   DELETE THE LINE                              
         B     EXITOK                                                           
VALETC2  CLI   XAPPTYP,ERFKORDQ                                                 
         BNE   EXITNV                                                           
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,50               MAXIMUM NUMBER OF ITEMS                      
         BNH   VALETC4                                                          
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
VALETC4  CLI   FVILEN,L'ETYKCODE                                                
         BH    EXITLONG            FIELD TOO LONG                               
T        USING ETYRECD,IOKEY                                                    
         XC    T.ETYKEY,T.ETYKEY   READ EXPENDITURE TYPE RECORD                 
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,CUABIN    CONNECTED ID                                 
         MVC   T.ETYKCODE,FVIFLD                                                
         MVC   IOKEYSAV,IOKEY                                                   
         LHI   R1,XOHIGH+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         CLC   IOKEYSAV(ETYKOFFC-ETYRECD),IOKEY                                 
         BE    VALETC6                                                          
         MVC   FVMSGNO,=AL2(AE$INETY)                                           
         B     EXITL               INVALID EXPENDITURE CODE                     
*                                                                               
VALETC6  CLC   T.ETYKOFFC,BCSPACES NO OFFICE CODE THEN FINE                     
         BNH   VALETC14                                                         
         CLI   CUACCS,0            GLOBAL LOGON                                 
         BE    VALETC14            THEN FINE TO USE IT                          
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BNZ   VALETC10                                                         
         CLI   CUACCS,C'$'         LIMIT LIST LOGON?                            
         BNE   VALETC8                                                          
         CLI   T.ETYKOFFC,C'$'     OFFICE LIST EXPENDITURE TYPE                 
         BNE   VALETC8                                                          
         CLC   CUACCS(2),T.ETYKOFFC CHECK WHETHER OFFICE LIST MATCHES           
         BE    VALETC14                                                         
         B     EXITOFF             EXPENDITURE TYPE NOT VALID ON THIS           
*                                                                               
VALETC8  GOTO1 ATSTOFF,T.ETYKOFFC                                               
         BE    VALETC14                                                         
         B     EXITOFF             EXPENDITURE TYPE NOT VALID                   
*                                                                               
X        USING OFFRECD,IOKEY       CHECK WHETHER WE HAVE OFFICE LIST            
VALETC10 MVC   SVIOKEY,IOKEY                                                    
         MVC   IOKEY,BCSPACES                                                   
         MVI   X.OFFKTYP,OFFKTYPQ                                               
         MVC   X.OFFKCPY,CUABIN                                                 
         MVC   X.OFFKOFF,CUACCS+2                                               
         L     R1,=AL4(XOHI+XOACCDIR+XIO3)                                      
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    X.OFFKSTAT,OFFSLIST  OFFICE LIST?                                
         BZ    VALETC12             NO THEN VALIDATE OFFICE                     
         MVC   IOKEY,SVIOKEY                                                    
         CLC   CUACCS+2(2),T.ETYKOFFC CHECK WHETHER MATCH ON                    
         BE    VALETC14             OFFICE LIST                                 
*                                                                               
VALETC12 MVC   IOKEY,SVIOKEY                                                    
         GOTO1 ATSTOFF,T.ETYKOFFC                                               
         BE    VALETC14                                                         
         B     EXITOFF             EXPENDITURE TYPE NOT VALID                   
*                                                                               
VALETC14 MVC   TLKET,T.ETYKCODE    SORT THE EXPENDITURE CODE                    
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR EXPENDITURE TYPE NAME FIELD                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ETNDTA   LA    RF,ETNTAB           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
ETNTAB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISETN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY EXPENDITURE TYPE NAME                                       *         
***********************************************************************         
         SPACE 1                                                                
DISETN   CLC   TLKET,BCSPACES                                                   
         BNH   EXITOK                                                           
T        USING ETYRECD,IOKEY                                                    
         XC    T.ETYKEY,T.ETYKEY   READ EXPENDITURE TYPE RECORD                 
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,CUABIN    CONNECTED ID                                 
         MVC   T.ETYKCODE,TLKET                                                 
         LHI   R1,XOHIGH+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR ORDER TYPE LIST                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
OTYDTA   LA    RF,OTYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OTYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOTY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOTY)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ORDER TYPE LIST FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DISOTY   CLI   XAPPTYP,ERFKESTQ                                                 
         BE    EXITOK                                                           
         CLC   TLKOT,BCSPACES                                                   
         BNH   EXITOK                                                           
         LA    RF,AC@ARTST                                                      
         CLI   TLKOT,C'A'                                                       
         BE    DISOTY2                                                          
         LA    RF,AC@INT                                                        
         CLI   TLKOT,C'I'                                                       
         BE    DISOTY2                                                          
         LA    RF,AC@PROO                                                       
         CLI   TLKOT,C'P'                                                       
         BE    DISOTY2                                                          
         LA    RF,AC@EXP                                                        
         CLI   TLKOT,C'E'                                                       
         BE    DISOTY2                                                          
         DC    H'0'                                                             
DISOTY2  MVC   FVIFLD(20),0(RF)                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ORDER TYPE LIST FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VALOTY   XC    TLKOT,TLKOT                                                      
         MVC   TLRLEN,=AL2(TLLNQ)  SET TSAR RECLEN                              
         CLI   FVILEN,0                                                         
         BNE   VALOTY2                                                          
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
VALOTY2  CLI   XAPPTYP,ERFKORDQ                                                 
         BNE   EXITNV                                                           
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         LA    RF,C'A'                                                          
         EX    RE,VALOTYA                                                       
         BE    VALOTY4                                                          
         LA    RF,C'I'                                                          
         EX    RE,VALOTYI                                                       
         BE    VALOTY4                                                          
         LA    RF,C'P'                                                          
         EX    RE,VALOTYP                                                       
         BE    VALOTY4                                                          
         LA    RF,C'E'                                                          
         EX    RE,VALOTYE                                                       
         BE    VALOTY4                                                          
         B     VALOTYN                                                          
VALOTY4  STC   RF,TLKOT                                                         
         B     EXITOK                                                           
*                                     * ERROR EXITS *                           
VALOTYA  CLC   FVIFLD(0),AC@ARTST                                               
VALOTYI  CLC   FVIFLD(0),AC@INT                                                 
VALOTYP  CLC   FVIFLD(0),AC@PROO                                                
VALOTYE  CLC   FVIFLD(0),AC@EXP                                                 
*                                     * ERROR EXITS *                           
VALOTYN  MVC   FVMSGNO,=AL2(AE$INVOT) INVALID ORDER TYPE                        
         MVC   FVXTRA,BCSPACES                                                  
         B     EXITL                                                            
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
         B     EXITL                                                            
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
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   CLI   SACT,A#LST          ARE WE GOING TO LIST A RECORD?               
         BNE   EXITOK                                                           
         CLI   SREC,R#ERFO         REPORT FORMAT RECORD                         
         BE    *+12                                                             
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   EXITOK                                                           
         OI    SNINDS1,SNIPARMS                                                 
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
         USING ETYRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING ERFRECD,R2                                                       
LAST     USING ERFRECD,R3                                                       
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
LISTABL  DC    AL1(LDEFCLM),AL1(0,0,0),AL4(DEFCLM)                              
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
*                                                                               
         DC    AL1(LSCRFRST),AL1(0,0,1),AL4(SCRF1)                              
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LLSTLAST),AL1(0,0,1),AL4(LTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
*                                                                               
         DC    AL1(LSCRFRST),AL1(0,0,2),AL4(SCRF1)                              
         DC    AL1(LGETFRST),AL1(0,0,2),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,2),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,2),AL4(FTFLST1)                            
         DC    AL1(LLSTLAST),AL1(0,0,2),AL4(LTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,2),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,2),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,2),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,2),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,2),AL4(UPDLAST1)                           
*                                                                               
         DC    AL1(LSCRFRST),AL1(0,0,3),AL4(SCRF1)                              
         DC    AL1(LGETFRST),AL1(0,0,3),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,3),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,3),AL4(FTFLST1)                            
         DC    AL1(LLSTLAST),AL1(0,0,3),AL4(LTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,3),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,3),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,3),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,3),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,3),AL4(UPDLAST1)                           
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DEFINE COLUMNS FOR LIST                                             *         
***********************************************************************         
         SPACE 1                                                                
DEFCLM   MVI   GSSMCODE,0                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
X        USING ERFRECD,IOKEY                                                    
FLST     MVC   X.ERFKEY,THIS.ERFKEY                                             
         L     R1,=AL4(XOHI+XOACCDIR+XIO11)                                     
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
NLST     XR    RF,RF               CONTROLLER REESTABLISHES SEQUENCE            
         IC    RF,X.ERFKFRMT+L'ERFKFRMT-1                                       
         AHI   RF,1                                                             
         STC   RF,X.ERFKFRMT+L'ERFKFRMT-1                                       
*                                                                               
         L     R1,=AL4(XOHID+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         BE    NLST02                                                           
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
*                                                                               
NLST02   CLC   X.ERFKEY(ERFKAPPL-ERFRECD),THIS.ERFKEY                           
         BNE   EXITL               DIFFERENT COMPANY - IGNORE IT                
*                                                                               
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
NLST06   CLC   X.ERFKSOFF,BCSPACES TEST OFFICE CODE                             
         BNH   NLST08                                                           
         GOTO1 ATSTOFF,X.ERFKSOFF                                               
         BNE   NLST                                                             
*                                                                               
NLST08   MVC   THIS.ERFKEY(ACCKLEN),IOKEY   WE WANT THIS KEY                    
         B     EXITOK                                                           
         DROP  THIS,LAST,X                                                      
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1/2/3                                   (LINIT) *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT1,LSSBALL+LSSTSAR                                          
         OI    LSSTAT2,LSSADD+LSSNOSEQ                                          
         MVC   LSCOLLIN,=AL2(78)  NUMBER OF COLUMNS PER LIST LINE               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST SCREEN 1/2/3                              (LSCRFRST) *         
***********************************************************************         
         SPACE 1                                                                
SCRF1    CLI   BCPFKEY,PFK09                                                    
         BE    *+8                                                              
         CLI   BCPFKEY,PFK10                                                    
         BNE   EXITOK                                                           
         NI    LSLTIND1,FF-LSLTIBLD REBUILD THE LIST                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR LIST 1/2/3                                (LLSTLAST)  *         
***********************************************************************         
         SPACE 1                                                                
LTFLST1  LH    RF,LS1STLIN                                                      
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1/2/3                                (LLSTFRST) *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  CLI   GSSMPAGE,OTLISTQ                                                 
         BNE   *+10                                                             
         XC    OTLBLK,OTLBLK                                                    
         CLI   GSSMPAGE,ETLISTQ                                                 
         BNE   *+10                                                             
         XC    ETLBLK,ETLBLK                                                    
         CLI   GSSMPAGE,SALISTQ                                                 
         BNE   *+10                                                             
         XC    SALBLK,SALBLK                                                    
         LA    RF,ERFRFST-ERFRECD                                               
         STH   RF,DISPGEN                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1/2/3                                     (LGETFRST) *         
* BUILD OTLBLK/ETLBLK/SALBLK FROM FORMAT RECORD LIDELS                *         
***********************************************************************         
         SPACE 1                                                                
FLST1    XR    R0,R0                                                            
         MVI   ANYLIST,NO                                                       
         CLI   GSSMPAGE,OTLISTQ                                                 
         BNE   *+12                                                             
         LA    RF,OTLBLK                                                        
         ST    RF,OTLDISP                                                       
         CLI   GSSMPAGE,ETLISTQ                                                 
         BNE   *+12                                                             
         LA    RF,ETLBLK                                                        
         ST    RF,ETLDISP                                                       
         CLI   GSSMPAGE,SALISTQ                                                 
         BNE   *+12                                                             
         LA    RF,SALBLK                                                        
         ST    RF,SALDISP                                                       
         LH    RF,DISPGEN          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         C     RF,AIOREC           MAKE SURE DISPGEN INITIALISED                
         BH    *+8                                                              
         AHI   RF,ERFRFST-ERFRECD                                               
*                                                                               
         USING LIDELD,RF                                                        
FLS02    CLI   LIDEL,0             RECORD END?                                  
         BE    FLSEX               YES                                          
         CLI   LIDEL,LIDELQ        LIST DATA ELEMENT                            
         BE    FLS06               YES                                          
                                                                                
FLS04    IC    R0,LIDLN                                                         
         AR    RF,R0                                                            
         B     FLS02                                                            
*                                                                               
FLS06    XR    RE,RE                                                            
*                                                                               
         CLI   LIDTYPE,LIDTOTYP    TEST ORDER TYPE LIST                         
         BNE   FLS08                                                            
         CLI   GSSMPAGE,OTLISTQ    TEST ORDER TYPE LIST PAGE                    
         BNE   FLS04                                                            
         IC    RE,LIDLN                                                         
         AHI   RE,-(LIDDATA-LIDELD+1)                                           
         MVC   OTLBLK(0),LIDDATA   COPY TO ORDER TYPE LIST BLOCK                
         EX    RE,*-6                                                           
         MVI   ANYLIST,YES                                                      
         B     FLSEX               EXIT                                         
*                                                                               
FLS08    CLI   LIDTYPE,LIDTEXPD    TEST EXPENDITURE TYPE LIST                   
         BNE   FLS10                                                            
         CLI   GSSMPAGE,ETLISTQ    TEST EXPENDITURE TYPE LIST PAGE              
         BNE   FLS04                                                            
         IC    RE,LIDLN                                                         
         AHI   RE,-(LIDDATA-LIDELD+1)                                           
         MVC   ETLBLK(0),LIDDATA   COPY TO EXPENDITURE TYPE LIST BLOCK          
         EX    RE,*-6                                                           
         MVI   ANYLIST,YES                                                      
         B     FLSEX               EXIT                                         
*                                                                               
FLS10    CLI   LIDTYPE,LIDTSUPP    TEST SUPPLIER ACCOUNT LIST                   
         BNE   FLS04                                                            
         CLI   GSSMPAGE,SALISTQ    TEST SUPPLIER ACCOUNT LIST PAGE              
         BNE   FLS04                                                            
         IC    RE,LIDLN                                                         
         AHI   RE,-(LIDDATA-LIDELD+1)                                           
         MVC   SALBLK(0),LIDDATA   COPY TO SUPPLIER ACC LIST BLOCK              
         EX    RE,*-6                                                           
         MVI   ANYLIST,YES                                                      
         B     FLSEX                                                            
*                                                                               
FLSEX    CLI   ANYLIST,YES                                                      
         BNE   EXITL               NO LIST FOR THIS PAGE                        
         B     EXITOK              ELSE OK                                      
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
* NEXT FOR LIST 1/2/3                                      (LGETNEXT) *         
* UPDATE OTL/ETL/SAL DISPLACEMENTS                                    *         
***********************************************************************         
         SPACE 1                                                                
NLST1    MVI   ANYLIST,NO                                                       
*                                                                               
         CLI   GSSMPAGE,OTLISTQ    TEST ORDER TYPE LIST PAGE                    
         BNE   NLST102                                                          
         L     RF,OTLDISP                                                       
         LA    RF,L'TLKOT(RF)      BUMP TO NEXT ENTRY                           
         OC    0(L'TLKOT,RF),0(RF) TEST END OF W/C LIST                         
         BZ    NLSEX                                                            
         ST    RF,OTLDISP                                                       
         MVI   ANYLIST,YES                                                      
         B     NLSEX                                                            
*                                                                               
NLST102  CLI   GSSMPAGE,ETLISTQ    TEST EXPENDITURE TYPE LIST PAGE              
         BNE   NLST104                                                          
         L     RF,ETLDISP                                                       
         LA    RF,L'TLKET(RF)      BUMP TO NEXT ENTRY                           
         OC    0(L'TLKET,RF),0(RF) TEST END OF EXPENDITURE TYPE LIST            
         BZ    NLSEX                                                            
         ST    RF,ETLDISP                                                       
         MVI   ANYLIST,YES                                                      
         B     NLSEX                                                            
*                                                                               
NLST104  CLI   GSSMPAGE,SALISTQ    TEST SUPPLIER ACCOUNT LIST                   
         BNE   EXITOK                                                           
         L     RF,SALDISP                                                       
         LA    RF,L'TLKSA(RF)      BUMP TO NEXT ENTRY                           
         OC    0(L'TLKSA,RF),0(RF) TEST END OF SUPPLIER ACCOUNT LIST            
         BZ    NLSEX                                                            
         ST    RF,SALDISP                                                       
         MVI   ANYLIST,YES                                                      
         B     NLSEX                                                            
*                                                                               
NLSEX    CLI   ANYLIST,YES         TEST ANYTHING MORE TO SHOW                   
         BE    EXITOK                                                           
         B     EXITL               NO - FINISHED                                
         EJECT ,                                                                
***********************************************************************         
* SET UP TSAR RECORD:                                      (LTSARFIL) *         
* TAKE AN ENTRY FROM OT/ET/SALBLK ACCORDING TO LIST PAGE              *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)                                               
*                                                                               
         CLI   GSSMPAGE,OTLISTQ    TEST ORDER TYPE LIST PAGE                    
         BNE   TSARFI02                                                         
         L     RF,OTLDISP          DISPLACE INTO OT LIST BLOCK                  
         MVC   TLKOT,0(RF)                                                      
         B     EXITOK                                                           
*                                                                               
TSARFI02 CLI   GSSMPAGE,ETLISTQ    TEST EXPENDITURE TYPE LIST PAGE              
         BNE   TSARFI04                                                         
         L     RF,ETLDISP          DISPLACE INTO EXP. TYPE LIST BLK             
         MVC   TLKET,0(RF)                                                      
         B     EXITOK                                                           
*                                                                               
TSARFI04 CLI   GSSMPAGE,SALISTQ    TEST SUPPLIER ACCOUNT LIST PAGE              
         BNE   EXITOK                                                           
         L     RF,SALDISP          DISPLACE INTO SUPPLIER AC LIST BLK           
         MVC   TLKSA,0(RF)                                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1/2/3                              (LUPDFRST) *         
* CLEAR OELBLK/ETLBLK/SALBLK TO REBUILD FROM TSAR RECORDS             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 CLI   CSACT,A#CHA         TEST ACTION IS CHANGE                        
         BE    *+8                                                              
         CLI   CSACT,A#ADD                     OR ADD                           
         BNE   EXITOK                                                           
         CLI   BCPFKEY,PFK09       TEST PAGE BACKWARD                           
         BE    EXITOK                                                           
         CLI   BCPFKEY,PFK10       TEST PAGE FORWARD                            
         BE    EXITOK                                                           
*                                                                               
         MVI   ANYINPUT,NO                                                      
         CLI   GSSMPAGE,OTLISTQ                                                 
         BNE   UPDF02                                                           
         XC    OTLBLK,OTLBLK       CLEAR BLOCKS FOR RE-BUILDING LIDELS          
         LA    RF,OTLBLK                                                        
         ST    RF,OTLDISP                                                       
UPDF02   CLI   GSSMPAGE,ETLISTQ                                                 
         BNE   UPDF04                                                           
         XC    ETLBLK,ETLBLK                                                    
         LA    RF,ETLBLK                                                        
         ST    RF,ETLDISP                                                       
UPDF04   CLI   GSSMPAGE,SALISTQ                                                 
         BNE   EXITOK                                                           
         XC    SALBLK,SALBLK                                                    
         LA    RF,SALBLK                                                        
         ST    RF,SALDISP                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* UPDATE OT/ET/SA BLOCKS FROM TSAR RECORD 1/2/3             (LUPDREC) *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R3                                                         
UPDREC1  CLI   CSACT,A#CHA         TEST ACTION IS CHANGE                        
         BE    *+8                                                              
         CLI   CSACT,A#ADD                     OR ADD                           
         BNE   EXITOK                                                           
         CLI   BCPFKEY,PFK09       TEST PAGE BACKWARD                           
         BE    EXITOK                                                           
         CLI   BCPFKEY,PFK10       TEST PAGE FORWARD                            
         BE    EXITOK                                                           
         L     R3,SVPARMS4                                                      
         CLI   GSSMPAGE,OTLISTQ    TEST ORDER TYPE LIST PAGE                    
         BNE   UPDR02                                                           
         CLC   TLKOT,BCSPACES      IF NON-BLANK                                 
         BNH   UPDREX                                                           
         L     RF,OTLDISP                                                       
         LA    RE,OTLBLK+L'OTLBLK-L'TLKOT                                       
         CR    RF,RE               TEST ROOM FOR ANOTHER LIST ENTRY             
         BH    UPDRERR                                                          
         MVC   0(L'TLKOT,RF),TLKOT ADD TO LIST BLOCK                            
         LA    RF,L'TLKOT(RF)      BUMP FOR NEXT TIME                           
         ST    RF,OTLDISP                                                       
         MVI   ANYINPUT,YES                                                     
         B     UPDREX                                                           
UPDR02   CLI   GSSMPAGE,ETLISTQ    TEST EXPENDITURE TYPE LIST PAGE              
         BNE   UPDR04                                                           
         CLC   TLKET,BCSPACES                                                   
         BNH   UPDREX                                                           
         L     RF,ETLDISP                                                       
         LA    RE,ETLBLK+L'ETLBLK-L'TLKET                                       
         CR    RF,RE               TEST ROOM FOR ANOTHER LIST ENTRY             
         BH    UPDRERR                                                          
         MVC   0(L'TLKET,RF),TLKET                                              
         LA    RF,L'TLKET(RF)                                                   
         ST    RF,ETLDISP                                                       
         MVI   ANYINPUT,YES                                                     
UPDR04   CLI   GSSMPAGE,SALISTQ    TEST SUPPLIER ACCOUNT LIST PAGE              
         BNE   UPDREX                                                           
         CLC   TLKSA,BCSPACES                                                   
         BNH   UPDREX                                                           
         L     RF,SALDISP                                                       
         LA    RE,SALBLK+L'SALBLK-L'TLKSA                                       
         CR    RF,RE               TEST ROOM FOR ANOTHER LIST ENTRY             
         BH    UPDRERR                                                          
         MVC   0(L'TLKSA,RF),TLKSA                                              
         LA    RF,L'TLKSA(RF)                                                   
         ST    RF,SALDISP                                                       
         MVI   ANYINPUT,YES                                                     
UPDREX   B     EXITOK                                                           
UPDRERR  MVC   FVMSGNO,=AL2(AE$TMILS) TOO MANY LINES IN LIST                    
         LH    RF,LSCURLIN                                                      
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO CURRENT LINE                   
         NI    LSLTIND1,FF-(LSLTIBLD) REBUILD THE LIST                          
         B     EXITL                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                   (LUPDLAST) *         
* DELETE/RE-ADD LIDELS USING OTLBLK/ETBLK/SABLK BUILT IN UPDREC1      *         
* P3 = A(FILE RECORD)                                                 *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 LA    RE,L'TLKOT-1        DO FINAL VALIDATION                          
         LA    RF,OTLBLK                                                        
         CLI   GSSMPAGE,OTLISTQ    ACCORDING TO LIST PAGE                       
         BE    *+16                                                             
         LA    RE,L'TLKET-1                                                     
         LA    RF,ETLBLK                                                        
         CLI   GSSMPAGE,ETLISTQ                                                 
         BE    *+16                                                             
         LA    RE,L'TLKSA-1                                                     
         LA    RF,SALBLK                                                        
         CLI   GSSMPAGE,SALISTQ                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDLA04  CLI   CSACT,A#CHA                                                      
         BE    *+8                                                              
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
         CLI   BCPFKEY,PFK09       TEST PAGE BACKWARD                           
         BE    EXITOK                                                           
         CLI   BCPFKEY,PFK10       TEST PAGE FORWARD                            
         BE    EXITOK                                                           
*                                                                               
UPDLA06  GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',AIOREC),0                
         ORG   *-2                                                              
*                                  INCLUDE SEARCH ARG TO HELLO PARMLIST         
*                                  TO ENSURE CORRECT LIDEL IS DELETED           
         CLI   GSSMPAGE,OTLISTQ    ORDER TYPE LIST PAGE                         
         BNE   *+12                                                             
         LA    RE,=AL1(L'TLKOT,LIDTOTYP)                                        
         B     UPDLA08                                                          
         CLI   GSSMPAGE,ETLISTQ    EXPENDITURE TYPE LIST PAGE                   
         BNE   *+12                                                             
         LA    RE,=AL1(L'TLKET,LIDTEXPD)                                        
         B     UPDLA08                                                          
         CLI   GSSMPAGE,SALISTQ    SUPPLIER ACCOUNT LIST PAGE                   
         BNE   *+12                                                             
         LA    RE,=AL1(L'TLKSA,LIDTSUPP)                                        
         B     UPDLA08                                                          
UPDLA08  ST    RE,8(R1)            SET A(ARG)                                   
         MVI   8(R1),2             AND L'ARG                                    
         BASR  RE,RF                                                            
*                                                                               
         USING LIDELD,BOELEM                                                    
         XC    BOELEM,BOELEM                                                    
         MVI   LIDEL,LIDELQ                                                     
         CLI   GSSMPAGE,OTLISTQ                                                 
         BNE   UPDLA12                                                          
         OC    OTLBLK(L'TLKOT),OTLBLK                                           
         BZ    UPDLA12             TEST ANY ORDER TYPES TO ADD                  
         MVI   LIDITLN,L'TLKOT     LIST ITEM LENGTH                             
         MVI   LIDTYPE,LIDTOTYP    SET LIST TYPE                                
UPDLA10  LA    RE,OTLBLK                                                        
         L     RF,OTLDISP                                                       
         SR    RF,RE                                                            
         AHI   RF,-1                                                            
         MVC   LIDDATA(0),OTLBLK                                                
         EX    RF,*-6                                                           
         LA    RF,(LIDDATA-LIDELD)+1(RF)                                        
         STC   RF,LIDLN                                                         
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         BNE   UPDLAERR                                                         
         B     UPDLAEX                                                          
*                                                                               
UPDLA12  CLI   GSSMPAGE,ETLISTQ                                                 
         BNE   UPDLA16                                                          
         OC    ETLBLK(L'TLKET),ETLBLK                                           
         BZ    UPDLA16             TEST ANY EXPENDITURE TYPES TO ADD            
         MVI   LIDITLN,L'TLKET     LIST ITEM LENGTH                             
         MVI   LIDTYPE,LIDTEXPD    SET LIST TYPE                                
UPDLA14  LA    RE,ETLBLK                                                        
         L     RF,ETLDISP                                                       
         SR    RF,RE                                                            
         AHI   RF,-1                                                            
         MVC   LIDDATA(0),ETLBLK                                                
         EX    RF,*-6                                                           
         LA    RF,(LIDDATA-LIDELD)+1(RF)                                        
         STC   RF,LIDLN                                                         
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         BNE   UPDLAERR                                                         
         B     UPDLAEX                                                          
*                                                                               
UPDLA16  CLI   GSSMPAGE,SALISTQ                                                 
         BNE   UPDLAEX                                                          
         OC    SALBLK(L'TLKSA),SALBLK                                           
         BZ    UPDLAEX             TEST ANY SUPPLIER ACCS TO ADD                
         MVI   LIDITLN,L'TLKSA     LIST ITEM LENGTH                             
         MVI   LIDTYPE,LIDTSUPP    SET LIST TYPE                                
UPDLA18  LA    RE,SALBLK                                                        
         L     RF,SALDISP                                                       
         SR    RF,RE                                                            
         AHI   RF,-1                                                            
         MVC   LIDDATA(0),SALBLK                                                
         EX    RF,*-6                                                           
         LA    RF,(LIDDATA-LIDELD)+1(RF)                                        
         STC   RF,LIDLN                                                         
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         BNE   UPDLAERR                                                         
UPDLAEX  B     EXITOK                                                           
UPDLAERR MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
         EJECT ,                                                                
         DROP  R2                                                               
***********************************************************************         
* UPDATE PASSIVE EXPENDITURE CATEGORY RECORDS                         *         
*                                                                     *         
* NTRY - P1  = FORMAT RECORD                                          *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
ADDRECS  NTR1  ,                                                                
         L     R3,0(R1)                                                         
         USING ERFRECD,R3                                                       
         LA    R2,IOKEY                                                         
         MVC   IOKEY,0(R3)                                                      
         L     R1,=AL4(XORDD+XOACCDIR+XIO4) READ DIR FOR DA                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FORMDA,IOKEY+ERFKDA-ERFRECD                                      
*                                                                               
         USING CPTRBLK,CPTRWRK                                                  
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,BODMCB,(C'A',(R3)),CPTRBLK,FORMDA,0,ACOM                 
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* DELETE FORMAT RECORDS AND PASSIVES                                  *         
*                                                                     *         
* NTRY - P1  = FORMAT RECORD                                          *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
DELRECS  NTR1  ,                                                                
         L     R3,0(R1)            GET RECORD ADDRESS                           
         USING ERFRECD,R3                                                       
         USING CPTRBLK,CPTRWRK                                                  
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,BODMCB,(C'D',(R3)),(C'K',CPTRBLK),0,0,ACOM               
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
EXTYP    DC    C'ET'                                                            
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
STMPSTRQ EQU   X'03'                                                            
OFFUL    DC    C'2D'                                                            
         SPACE 1                                                                
OTLISTQ  EQU   1                                                                
ETLISTQ  EQU   2                                                                
SALISTQ  EQU   3                                                                
         SPACE 1                                                                
DCLISTU  DS    0D                                                               
         DCDDL AC#EST,3,L                                                       
         DCDDL AC#ORDER,3,L                                                     
         DCDDL AC#EXP,20,L                                                      
         DCDDL AC#ARTST,20,L                                                    
         DCDDL AC#INT,20,L                                                      
         DCDDL AC#PROO,20,L                                                     
DCLISUX  DC    AL1(EOT)                                                         
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
DUB      DS    D                                                                
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
         SPACE 1                                                                
OTLDISP  DS    F                   DISP INTO OTLBLK                             
ETLDISP  DS    F                   DISP INTO ETLBLK                             
SALDISP  DS    F                   DISP INTO SALBLK                             
DISPGEN  DS    F                   CURRENT DISPLACEMENT INTO RECORD             
WORK     DS    XL64                                                             
SVIOKEY  DS    XL42                                                             
FORMDA   DS    XL(L'ERFKDA)                                                     
CPTRWRK  DS    XL128                                                            
SESNL    DS    XL1                 CURRENT SESSION                              
*                                                                               
DSLISTU  DS    0D                                                               
AC@EST   DS    CL3                                                              
AC@ORDER DS    CL3                                                              
AC@EXP   DS    CL20                                                             
AC@ARTST DS    CL20                                                             
AC@INT   DS    CL20                                                             
AC@PROO  DS    CL20                                                             
*                                                                               
LBLKLNQ  EQU   255-(LIDDATA-LIDELD)                                             
*                                                                               
OTLBLK   DS    CL(L'TLKOT*OTLMAX)                                               
OTLMAX   EQU   LBLKLNQ/L'TLKOT                                                  
*                                                                               
ETLBLK   DS    CL(L'TLKET*ETLMAX)                                               
ETLMAX   EQU   LBLKLNQ/L'TLKET                                                  
*                                                                               
SALBLK   DS    CL(L'TLKSA*SALMAX)                                               
SALMAX   EQU   LBLKLNQ/L'TLKSA                                                  
*                                                                               
         SPACE 1                                                                
ANYLIST  DS    CL1                                                              
ANYINPUT DS    CL1                                                              
XAPPTYP  DS    CL1                                                              
OVERWRKX EQU   *-OVERWRKD                                                       
         SPACE 1                                                                
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKELEM  DS    0XL14                                                            
TLKOT    DS    CL1                 ORDER TYPE                                   
         DS    XL13                                                             
         ORG   TLKSRT                                                           
TLKET    DS    CL3                 EXPENDITURE TYPE                             
         DS    XL11                                                             
         ORG   TLKSRT                                                           
TLKSA    DS    CL14                SUPPLIER U/L/ACCOUNT                         
         ORG   TLUSER                                                           
         DS    XL4                 DUMMY TO KEEP TSAR HAPPY                     
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACFIL1F   08/10/11'                                      
         END                                                                    
