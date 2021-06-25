*          DATA SET RECOV11    AT LEVEL 253 AS OF 08/10/11                      
*          DATA SET RECOV11    AT LEVEL 247 AS OF 05/13/98                      
*PHASE T82311C                                                                  
         PRINT NOGEN                                                            
         TITLE 'COVERSHEET RECORD MAINT PHASE'                                  
**********************************************************************          
*  RECOV11 (T82311) REP COVERSHEET MAINTENANCE OVERLAY               *          
*--------------------------------------------------------------------*          
* 04APR01 RHV - FIX HELLO CONDITON CHECK BUG                         *          
* 07MAY04 SKU - FIX HELLO OVERFLOW                                   *          
*  JUL11  SMY - MODIFY FOR CHANGED RECOVWORK AND FIX HELLO           *          
*                 OVERFLOW DEATH IN UPDCON                           *          
*                                                                    *          
**********************************************************************          
COV11    CSECT                                                                  
         NMOD1 0,RECOV11*,R6,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         L     R7,ASVEBLK                                                       
         USING MYSAVED,R7                                                       
         ST    RE,BORELO                                                        
         ST    RB,BOBASE1                                                       
         ST    R6,BOBASE2                                                       
***>     ST    R5,BOBASE3                                                       
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
         B     LAST                LAST TIME CALL                               
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
         SPACE 1                                                                
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT NOT WANTED FOR FILTER                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T                                        
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
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(SUB)                                 
         DC    AL1(OREP),AL1(0,0,0),AL4(EXITH)                                  
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFK)                                    
         DC    AL1(OIO),AL1(0,0,0),AL4(EXITH)                                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     DS     0H                                                              
         GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         GOTO1 (RF),(R1),C'LL  ',,DSLISTL                                       
*                                                                               
         OI    TWASRVH+1,X'01'     SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    TWASRVH+6,X'80'                                                  
         OI    GCINDS1,GCIPROT     UNPROT ON NTRSES                             
         OI    LSSTAT1,LSSBALL     BUILD ALL OF LIST IN ONE GO                  
*                                                                               
*                                  CHECK FOR QUIT PFKEY                         
         CLI   BCPFKEY,12          PFKEY 12 HIT?                                
         BNE   EXITOK              NO                                           
         TM    KFLAGS,KFFROMK      FROM CONTRACT?                               
         BZ    EXIT OK             NO - IGNORE, HANDLED IN 00 MODULE            
         GOTO1 =A(DORETURN),RR=Y                                                
         DC    H'0'                SHOULDN'T MAKE IT HERE                       
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* LAST TIME CALL                                                      *         
***********************************************************************         
         SPACE 1                                                                
LAST     DS     0H                                                              
         GOTOX TSAR2,TSASAV                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                                    
*                                                                               
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 A(KEY OR WHERE TO BUILD THE KEY)                                           
* P4 HOLDS SUB-ACTION                                                           
***********************************************************************         
KEY      LM    R0,R2,SVPARMS                                                    
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE WE DO ANYTHING TO THE KEY FIELDS ON THE SCREEN                         
***********************************************************************         
KEYFRST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)                                 
         DC    AL1(KDIS),AL1(0,0,0),AL4(EXITH)                                  
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)                               
         DC    AL1(KFDIS),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* KEY FIRST - KEY FILTER VAL                                                    
***********************************************************************         
KFKFVAL  DS    0H                                                               
         USING RCOVREC,R2                                                       
         XC    RCOVKEY,RCOVKEY                                                  
         MVI   RCOVKTYP,X'49'                                                   
         MVC   RCOVKREP,CUAALF                                                  
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* KEY FIRST - KEY VAL                                                           
***********************************************************************         
KFKVAL   DS    0H                                                               
         USING RCOVREC,R2                                                       
         XC    RCOVKEY,RCOVKEY                                                  
         MVI   RCOVKTYP,X'49'                                                   
         MVC   RCOVKREP,CUAALF                                                  
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* AFTER WE'VE DONE EVERYTHING TO THE KEY FIELDS ON THE SCREEN                   
***********************************************************************         
KEYLAST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KLTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER VALIDATING THE KEY FIELDS                                               
***********************************************************************         
KLKVAL   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RECORD OBJECT                                                                 
*                                                                               
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 A(KEY OR WHERE TO BUILD THE KEY)                                           
* P4 HOLDS SUB-ACTION                                                           
***********************************************************************         
RECORD   LM    R0,R2,SVPARMS                                                    
         LA    RF,RECRDTBL                                                      
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
RECRDTBL DC    AL1(RFIRST),AL1(0,0,0),AL4(RECFRST)                              
         DC    AL1(RLAST),AL1(0,0,0),AL4(RECLAST)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE THE CONTROLLER CALLS THE I/O ACTION                                    
***********************************************************************         
RECFRST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,RFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RFTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RFRADD)      ADD                        
         DC    AL1(RWRT),AL1(0,0,0),AL4(RFRWRT)      WRITE                      
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE THE I/O CALL TO ADD THE RECORD                                         
***********************************************************************         
RFRADD   DS    0H                                                               
         B     EXITOK                                                           
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* BEFORE THE I/O CALL TO WRITE THE RECORD                                       
***********************************************************************         
RFRWRT   DS    0H                                                               
*                                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* AFTER THE CONTROLLER CALLS THE I/O ACTION                                     
***********************************************************************         
RECLAST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RLTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RLRADD)      ADD                        
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLRWRT)      WRITE                      
         DC    AL1(RDEL),AL1(0,0,0),AL4(RLRDEL)      DEL                        
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER THE I/O CALL TO ADD THE RECORD                                          
***********************************************************************         
         USING RCOVREC,R2                                                       
RLRADD   DS    0H                                                               
         TM    KFLAGS,KFFROMK      SWAPPED IN?                                  
         BZ    EXITOK                                                           
         GOTO1 =A(UPDCON),RR=Y     UPDATE CONTRACT(S)                           
         B     EXITOK                                                           
***********************************************************************         
* AFTER THE I/O CALL TO WRITE THE RECORD                                        
***********************************************************************         
RLRWRT   DS    0H                                                               
         TM    KFLAGS,KFFROMK      SWAPPED IN?                                  
         BZ    EXITOK              NO - GET OUT OF HERE                         
         GOTO1 =A(UPDCON),RR=Y     UPDATE CONTRACT(S)                           
         B     EXITOK                                                           
***********************************************************************         
* AFTER THE I/O CALL TO DELETE THE RECORD                                       
***********************************************************************         
RLRDEL   DS    0H           DELETE ALL OF THE RECORDS IN THE CHAIN              
         MVC   IOKEY(L'RCOVKEY),RCOVKEY                                         
         ZIC   R4,RCOVKSEQ                                                      
         LA    R4,1(R4)                                                         
         STC   R4,IOKEY+26                                                      
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XORDUPD)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   RLRDEL20            NOT FOUND                                    
         TM    IOERR,IOEDEL        ALREADY DELETED?                             
         BO    RLRDEL20            ALL DONE                                     
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGETRUP)                               
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO4                                                          
         OI    RCOVCNTL,X'80'      DELETE RECORD                                
***>     GOTOX ADELEL,BOPARM,(3,AIO4),0  DELETE ELEMENTS                        
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOPUT)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    IOKEY+27,X'80'      DELETE KEY                                   
         ICM   R1,15,=AL4(XOREPDIR+XOWRITE)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    RLRDEL              NEXT RECORD                                  
         DC    H'0'                                                             
*                                                                               
RLRDEL20 DS    0H                                                               
         TM    KFLAGS,KFFROMK                                                   
         BZ    RLRDELX                                                          
         GOTO1 =A(UPDCON),RR=Y                                                  
         GOTO1 =A(DORETURN),RR=Y   SWAP BACK TO CONTRACT                        
         DC    H'0'                                                             
*                                                                               
RLRDELX  B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SCREEN OBJECT                                                       *         
***********************************************************************         
         SPACE 1                                                                
SCRN     LM    R0,R3,SVPARMS                                                    
         USING FDRRECD,R2                                                       
         LA    RF,TABLSCRN                                                      
         B     ITER                ITERATE TABLE                                
*                                                                               
TABLSCRN DC    AL1(SSET),AL1(0,0,0),AL4(SCRSET)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET SCREEN CODE FOR MAINT SCREEN                                    *         
***********************************************************************         
         SPACE 1                                                                
SCRSET   MVI   GSSMCODE,0                                                       
         CLI   CSACT,A#DIS         CHANGE HAS DIFFERENT SCREEN                  
         BNE   *+8                                                              
         MVI   GSSMCODE,C'A'                                                    
         B     EXITOK                                                           
         DROP  R2                                                               
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
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DFRST)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DFRST                                                                         
***********************************************************************         
DFRST    DS    0H                                                               
         L     R1,SVPARMS3         R1=INVOKING ACTION                           
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
DFTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE VALIDATING THE KEY FIELDS                                              
***********************************************************************         
         SPACE 1                                                                
DFDVAL   DS    0H                                                               
         USING RCOVREC,R2          DO WE HAVE AN 01 ELEM?                       
         GOTOX AGETEL,BOPARM,(1,RCOVREC),0                                      
         BE    EXITOK              YES -DON'T HAVE TO ADD A NEW ONE             
*                                                                               
         LA    R3,BOELEM                                                        
         USING RCOVELEM,R3                                                      
         XC    RCOVELEM(RCOVELLQ),RCOVELEM                                      
         MVI   RCOVELCD,X'01'                                                   
         MVI   RCOVELLN,RCOVELLQ                                                
         GOTOX AADDEL,BODMCB,RCOVREC                                            
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DS    0AL2                                                             
         DC    AL2(00100),AL4(COVDTA)    COVERSHEET NAME                        
         DC    AL2(00200),AL4(DATDTA)    LAST CHANGED DATE                      
         DC    AL2(00150),AL4(DSCDTA)    DESCRITPION                            
         DC    AL2(00151),AL4(DSCDTA)    DESCRITPION  (DISPLAY)                 
         DC    AL2(00250),AL4(NUMDTA)    SKIP NUMBERED LINES                    
         DC    AL2(00260),AL4(FMTDTA)    AUTOFORMAT                             
         DC    AL2(00261),AL4(FMTDTA)    AUTOFORMAT (DISPLAY)                   
         DC    AL2(01004),AL4(LINDTA)    DATA LINE                              
         DC    AL2(01104),AL4(LINDTA)    DATA LINE (DISPLAY)                    
         DC    AL2(01005),AL4(LNMDTA)    LINE NUMBER                            
         DC    AL2(01006),AL4(STRDTA)    START LINE NUMBER                      
         DC    AL2(01009),AL4(NLNDTA)    NUMBER OF LINES IN LIST                
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
COV11    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COVERSHEET NAME                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
COVDTA   LA    RF,COVTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
COVTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOV)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DFLTCOV)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOV)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCOV)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCOV)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* SET DEFAULT VALUE FOR COVERSHEET (WHEN SWAPED FROM CONTRACT)        *         
***********************************************************************         
DFLTCOV  DS    0H                                                               
         TM    KFLAGS,KFFROMK      FROM CONTRACT?                               
         BZ    EXITOK              NO DEFAULT                                   
         USING RCOVREC,R2                                                       
*                                                                               
         L     R4,FVADDR                                                        
         OI    1(R4),X'20'         PROTECT                                      
         OI    6(R4),X'80'         XMIT                                         
*                                                                               
         SR    R4,RA                                                            
         ST    R4,NAMFLD           TWA DISPL TO THIS FIELD                      
*                                                                               
         LA    R3,SVCOVEL                                                       
         USING GLCOVNAM,R3                                                      
         OC    GLCOVNAM,GLCOVNAM                                                
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE A NAME COMING IN                   
         MVC   RCOVKNAM,GLCOVNAM                                                
         CLI   RCOVKNAM,X'FF'      NUMBERED COVER?                              
         BNE   DFLTC030            NO                                           
         MVC   FVIFLD(8),=C'CONTRACT'                                           
*                                                                               
         CLI   CSACT,A#MAINT                                                    
         BNE   DFLTC050                                                         
         MVC   IOKEY(L'RCOVKEY),RCOVKEY                                         
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XORDUPD)                                
         GOTOX ('XIO',AGROUTS)                                                  
         TM    IOERR,IOEDEL        RECORD DELETED?                              
         BZ    DFLTC050            OK - NO NEED TO RESTORE                      
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGETRUP+XORDEL)                        
         GOTOX ('XIO',AGROUTS)                                                  
         TM    IOERR,X'FF'-IOEDEL                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO4             RESTORE REC                                  
         NI    RCOVCNTL-RCOVKEY(R4),X'FF'-X'80'                                 
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOPUT)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    IOKEY+27,X'FF'-X'80' RESTORE KEY                                 
         ICM   R1,15,=AL4(XOREPDIR+XOWRITE)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DFLTC050                                                         
DFLTC030 DS    0H                                                               
         MVC   FVIFLD(8),GLCOVNAM                                               
DFLTC050 B     EXITOK                                                           
         DROP  R3,R2                                                            
***********************************************************************         
* DISPLAY A COVERSHEET NAME FIELD                                     *         
***********************************************************************         
DISCOV   DS    0H                                                               
         USING RCOVREC,R2                                                       
         CLI   RCOVKNAM,X'FF'      NUMBERED SHEET?                              
         BNE   DISCOV10                                                         
         TM    KFLAGS,KFFROMK                                                   
         BZ    DISCOV05                                                         
***>     CLI   CSACT,A#LST         EXPAND NAME FOR LIST                         
***>     BE    DISCOV05                                                         
         MVC   FVIFLD(8),=C'CONTRACT'                                           
         B     EXITOK                                                           
DISCOV05 DS    0H                                                               
         GOTOX (RFCONNUM,VREPFACS),BODMCB,(1,RCOVKNAM+4),(6,FVIFLD)             
         B     EXITOK                                                           
DISCOV10 DS    0H                                                               
         MVC   FVIFLD(8),RCOVKNAM                                               
         B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* VALIDATE A COVERSHEET NAME FIELD                                    *         
***********************************************************************         
VALCOV   DS    0H                                                               
         L     R4,FVADDR                                                        
         SR    R4,RA                                                            
         ST    R4,NAMFLD           TWA DISPL TO THIS FIELD                      
*                                                                               
         AR    R4,RA                                                            
         TM    KFLAGS,KFFROMK      FROM CONTRACT?                               
         BZ    VALCOV05            NO                                           
         OI    1(R4),X'20'         PROTECT                                      
         OI    6(R4),X'80'         XMIT                                         
*                                                                               
VALCOV05 DS    0H                                                               
         CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
*                                                                               
         OC    FVIFLD(8),=8C' '                                                 
         USING RCOVKEY,R2                                                       
*                                                                               
         ZIC   R4,FVXLEN                                                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=C'CONTRACT'                                           
         BNE   VALCOV20                                                         
         TM    KFLAGS,KFFROMK                                                   
         BZ    EXITNV                                                           
         LA    R3,SVCOVEL                                                       
         USING GLCOVNAM,R3                                                      
         CLI   GLCOVNAM,X'FF'                                                   
         BNE   VALCOV10                                                         
         MVC   RCOVKNAM,GLCOVNAM                                                
         B     EXITOK                                                           
VALCOV10 DS    0H                                                               
         GOTO1 =A(GETCON),RR=Y                                                  
         GOTOX (RFCONLOW,VREPFACS),BODMCB,AIO2                                  
         MVI   RCOVKNAM,X'FF'                                                   
         MVC   RCOVKNAM+4(4),BODMCB                                             
*                                                                               
VALCOV15 DS    0H                                                               
         CLI   CSACT,A#MAINT                                                    
         BNE   EXITOK                                                           
         MVC   IOKEY(L'RCOVKEY),RCOVKEY                                         
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XORDUPD)                                
         GOTOX ('XIO',AGROUTS)                                                  
         TM    IOERR,IOEDEL        RECORD DELETED?                              
         BZ    EXITOK              OK - NO NEED TO RESTORE                      
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGETRUP+XORDEL)                        
         GOTOX ('XIO',AGROUTS)                                                  
         TM    IOERR,X'FF'-IOEDEL                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO4             RESTORE REC                                  
         NI    RCOVCNTL-RCOVKEY(R4),X'FF'-X'80'                                 
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOPUT)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    IOKEY+27,X'FF'-X'80' RESTORE KEY                                 
         ICM   R1,15,=AL4(XOREPDIR+XOWRITE)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXITOK                                                           
*                                                                               
VALCOV20 DS    0H                                                               
         MVC   RCOVKNAM,FVIFLD                                                  
         B     EXITOK                                                           
         DROP  R2,R3                                                            
***********************************************************************         
* VALIDATE A COVERSHEET NAME FIELD AS DATA FITLER                     *         
***********************************************************************         
VFLTCOV  CLI   FVILEN,0            ANY FILTER?                                  
         BE    EXITNO              NO                                           
*                                                                               
         OC    FVIFLD(8),=8C' '                                                 
         USING RCOVKEY,R2                                                       
         ZIC   R3,FVXLEN                                                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   RCOVKNAM(0),FVIFLD                                               
         MVC   FLTIFLD(1),FVILEN                                                
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   FLTIFLD+1(0),FVIFLD                                              
         B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* FILTER RECORDS BY COVERSHEET NAME                                   *         
***********************************************************************         
DOFTCOV  DS    0H                                                               
         USING RCOVKEY,R2                                                       
         ZIC   R3,FLTIFLD                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   RCOVKNAM(0),FLTIFLD+1                                            
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
***********************************************************************         
* DATA OBJECT FOR AUTO FORMAT SWITCH                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         USING RCOVREC,R2                                                       
FMTDTA   LA    RF,FMTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FMTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFMT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFMT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY AUTO FORMAT FLAG                                            *         
***********************************************************************         
DISFMT   DS    0H                                                               
         TM    RCOVFLAG,RCOVFLAF                                                
         BO    DISFMT5                                                          
         MVC   FVIFLD(3),=C'Off'                                                
         B     EXITOK                                                           
DISFMT5  DS    0H                                                               
         MVC   FVIFLD(2),=C'On'                                                 
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE AUTO FORMAT FLAG                                           *         
***********************************************************************         
VALFMT   DS    0H                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,3                                                         
         BH    EXITNO                                                           
         MVC   BOWORK1(3),FVIFLD                                                
         OC    BOWORK1(3),=3C' '                                                
         ZIC   R4,FVXLEN                                                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   BOWORK1(0),=C'ON'                                                
         BNE   VALFMT5                                                          
         OI    RCOVFLAG,RCOVFLAF                                                
         B     EXITOK                                                           
VALFMT5  DS    0H                                                               
         CLI   FVILEN,3                                                         
         BH    EXITNV                                                           
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   BOWORK1(0),=C'OFF'                                               
         BNE   EXITNV                                                           
         NI    RCOVFLAG,X'FF'-RCOVFLAF                                          
         B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* DATA OBJECT FOR NUMBERED COVERSHEET FILTER                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
NUMDTA   LA    RF,NUMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
NUMTBL   DC    AL1(DDFLTF),AL1(0,0,0),AL4(DFFLTNUM)                             
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DISNUM)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALNUM)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTNUM)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* SET DEFAULT VAUE FOR NUMBERED COVERSHEET FILTER                     *         
***********************************************************************         
DFFLTNUM DS    0H                                                               
         MVI   FLTIFLD,X'FF'       FLAG TO SKIP NUMBERED SHEETS                 
         MVC   FVIFLD(2),=C'No'                                                 
         B     EXITOK                                                           
***********************************************************************         
* DISPLAY A NUMBERED COVERSHEET FIELD AS FILTER                       *         
***********************************************************************         
DISNUM   DS    0H                                                               
         CLI   FLTIFLD,X'FF'                                                    
         BNE   DISNUM5                                                          
         MVC   FVIFLD(2),=C'No'                                                 
         B     EXITOK                                                           
DISNUM5  DS    0H                                                               
         MVC   FVIFLD(3),=C'Yes'                                                
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE A NUMBERED COVERSHEET FIELD AS FILTER                      *         
***********************************************************************         
VALNUM   DS    0H                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,3                                                         
         BH    EXITNO                                                           
         OC    FVIFLD(3),=3C' '                                                 
         ZIC   R3,FVXLEN                                                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=C'YES'                                                
         BNE   VALNUM5                                                          
         MVI   FLTIFLD,0                                                        
         B     EXITOK                                                           
VALNUM5  DS    0H                                                               
         CLI   FVILEN,2                                                         
         BH    EXITNV                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=C'NO'                                                 
         BNE   EXITNV                                                           
         MVI   FLTIFLD,X'FF'                                                    
         B     EXITOK                                                           
***********************************************************************         
* FILTER NUMBERED COVERSHEET RECORDS                                  *         
***********************************************************************         
DOFTNUM  DS    0H                                                               
         USING RCOVKEY,R2                                                       
         CLI   FLTIFLD,X'FF'       ARE WE FILTERING?                            
         BNE   FLTXE               NO                                           
         CLI   RCOVKNAM,X'FF'                                                   
         BE    FLTXH                                                            
         B     FLTXE                                                            
         DROP  R2                                                               
***********************************************************************         
* DATA OBJECT FOR LAST CHANGE DATE                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
DATDTA   LA    RF,DATTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DATTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDAT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY LAST CHANGE DATE FIELD                                      *         
***********************************************************************         
DISDAT   DS    0H                                                               
         USING RCOVREC,R2                                                       
         XC    FVIFLD(8),FVIFLD                                                 
         GOTOX AGETEL,BODMCB,('RACELQ',RCOVREC),(1,=X'2')                       
         BNE   EXITOK                                                           
         LA    R3,BOELEM                                                        
         USING RACELD,R3                                                        
         GOTO1 VDATCON,BODMCB,(1,RACDATE),(11,FVIFLD)                           
         B     EXITOK                                                           
         DROP  R2,R3                                                            
***********************************************************************         
***********************************************************************         
* DATA OBJECT FOR COVERSHEET DESCRIPTION                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
DSCDTA   LA    RF,DSCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DSCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDSC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A COVERSHEET DESCRIPTION FIELD                              *         
***********************************************************************         
DISDSC   DS    0H                                                               
         USING RCOVREC,R2                                                       
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(L'RCOVDESC),RCOVDESC                                      
         B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* VALIDATE A RECORD DESCRIPTION FIELD                                 *         
***********************************************************************         
VALDSC   DS    0H                                                               
         USING RCOVREC,R2                                                       
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         XC    RCOVDESC,RCOVDESC                                                
         ZIC   R4,FVXLEN                                                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   RCOVDESC(0),FVIFLD                                               
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR ONE LINE NUMBER DISPLAY                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
LNMDTA   LA    RF,LNMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LNMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLNM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(EXITOK)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LINE NUMBER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
DISLNM   DS    0H                                                               
         EDIT  TLKSNUM,(4,FVIFLD),ZERO=NOBLANK,DUB=BODUB1,WRK=BOWORK1, +        
               FILL=0                                                           
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR ONE LINE OF RECORD DATA                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
LINDTA   LA    RF,LINTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LINTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLIN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLIN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LINE OF RECORD DATA                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
DISLIN   XC    T2LST,T2LST                                                      
         MVC   T2KEY,TL2MAP                                                     
         GOTOX TSAR2,TSARDH                                                     
         CLC   T2KEY,TL2MAP                                                     
         BNE   DISLINC             MEANS BUFFER MESSED UP                       
*                                                                               
         XC    FVIFLD,FVIFLD                                                    
         CLI   T2LLEN,0            BLANK LINE?                                  
         BE    EXITOK                                                           
         ZIC   R4,T2LLEN                                                        
         BCTR  R4,0                                                             
         EX    R4,*+4                                                           
         MVC   FVIFLD(0),T2LINE    MOVE DATA OUT TO LINE                        
         B     EXITOK                                                           
*                                                                               
DISLINC  DC    H'0'                THE C MEANS CRAP OUT                         
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A LINE OF RECORD DATA                                      *         
***********************************************************************         
         SPACE 1                                                                
VALLIN   XC    T2LST,T2LST                                                      
         MVC   T2KEY,TL2MAP                                                     
         GOTOX TSAR2,TSARDH                                                     
         CLC   T2KEY,TL2MAP                                                     
         BNE   VALLINC             MEANS BUFFER MESSED UP                       
*                                                                               
         MVC   T2LINE,FVIFLD       MOVE DATA BACK TO TSAR RECORD                
         MVC   T2LLEN,FVILEN                                                    
         GOTOX TSAR2,TSAWRT                                                     
         BE    EXITOK                                                           
*                                                                               
VALLINC  DC    H'0'                                                             
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR START LINE NUMBER                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
STRDTA   LA    RF,STRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
STRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTR)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DFTSTR)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY START SEQUENCE NUMBER                                       *         
***********************************************************************         
         SPACE 1                                                                
DISSTR   XR    RF,RF                                                            
         ICM   RF,3,SEQNUM         TEST SEQUENCE NUMBER SET                     
         BZ    EXITOK                                                           
*                                                                               
         CURED (RF),(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE START SEQUENCE NUMBER IN LINE                              *         
***********************************************************************         
         SPACE 1                                                                
VALSTR   MVC   ASEQFLD,FVADDR      SAVE A(SEQUENCE NUMBER FIELD)                
         CLI   CSACT,A#DIS                                                      
         BNE   *+10                                                             
         MVC   BOCURSOR,FVADDR                                                  
*                                                                               
         CLC   GSRECKEY,GCLASKEY   RESET SEQUENCE IF KEY CHANGES                
         BE    *+10                                                             
         XC    SEQLAST,SEQLAST                                                  
*                                                                               
         CLI   FVILEN,0            IF INPUT, MUST BE A NUMBER                   
         BNE   *+14                                                             
         XC    SEQNUM,SEQNUM                                                    
         B     EXITOK                                                           
*                                                                               
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL                                                     
         BZ    EXITNV                                                           
         STH   RF,SEQNUM                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY START SEQUENCE NUMBER DEFAULT                               *         
***********************************************************************         
         SPACE 1                                                                
DFTSTR   MVI   FVIFLD,C'1'                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR NUMBER OF LINES IN A SCRIPT                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NLNDTA   LA    RF,NLNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
NLNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNLN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY NUMBER OF LINES                                             *         
***********************************************************************         
         SPACE 1                                                                
DISNLN   DS    0H                                                               
         XC    FVIFLD,FVIFLD       CLEAR FIELD                                  
         MVC   ACOUNT,FVADDR       SAVE FIELD ADDRESS                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* NTRSES OBJECT                                                       *         
* -------------                                                       *         
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
* BUILD PARAMETER LIST FOR NTRSES TRANSFER (OUT)                      *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                          *         
***********************************************************************         
         SPACE 1                                                                
XITIN    CLI   NSACT,A#LST                                                      
         BNE   EXITOK                                                           
         NI    SNINDS1,FF-SNIUSECR                                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
LIST     LM    R0,R3,SVPARMS                                                    
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
         EJECT                                                                  
*                                                                               
LISTABL  DS    0A                                                               
         DC    AL1(LINIT),AL1(0,0,0),AL4(INITL)     INIT                        
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)   RDHI                        
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)   RDSEQ                       
         DC    AL1(LSCRLAST),AL1(0,0,0),AL4(SCRLAST)                            
*                                                                               
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LSCRFRST),AL1(0,0,1),AL4(FSCR1)                              
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LTSARDIR),AL1(0,0,1),AL4(TSARDIR1)                           
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(LNEWITEM),AL1(0,0,1),AL4(NEWITEM1)                           
         DC    AL1(LSCRLAST),AL1(0,0,1),AL4(SCRLAST1)                           
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
INITL    DS    0H                                                               
***>     MVC   LSNUMHED,=AL2(1)                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     DS    0H                                                               
         USING RCOVKEY,R2                                                       
         MVC   IOKEY(L'RCOVKEY),RCOVKEY                                         
         ICM   R1,15,=AL4(XIO11+XOREPDIR+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               READ HIGH UNHAPPY                            
         B     NLST05                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     DS    0H                                                               
         USING RCOVREC,R2                                                       
         MVC   IOKEY(L'RCOVKEY),RCOVKEY                                         
         ICM   R1,15,=AL4(XIO11+XOREPDIR+XOSEQ)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               READ HIGH UNHAPPY                            
*                                                                               
NLST05   DS    0H                                                               
         CLC   IOKEY(RCOVKNAM-RCOVKEY),RCOVKEY                                  
         BNE   EXITL                                                            
*                                                                               
         CLI   IOKEY+26,0          SKIP NON 0 SEQUENCE RECS                     
         BNE   NLST                                                             
*                                                                               
         MVC   RCOVKEY(L'IOKEY),IOKEY        WE WANT THIS RECORD                
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* LAST FOR SCREEN                                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT2,(LSSNOSEQ+LSSIUPD)                                       
         OI    LSSTAT1,(LSSTSAR+LSSBALL)                                        
         MVC   LSNUMHED,=AL2(1)                                                 
         MVI   LSSUBLEN,SBLEN                                                   
         CLI   CSACT,A#DIS                                                      
         BNE   *+8                                                              
         MVI   LSSUBLEN,0                                                       
*                                                                               
         NI    LSTSINDS,FF-LSTSIRES  TEST TEMPEST BUFFER RESTORED               
         XC    CCOUNT,CCOUNT       RESET COUNT OF ITEMS IN LIST                 
         XC    PCOUNT,PCOUNT       RESET COUNT OF ITEMS IN BUFFER 2             
         LA    RF,TSAR2            DELETE EVERYTHING IN BUFFER2                 
*                                                                               
INI102   XC    T2LST,T2LST                                                      
         GOTOX (RF),TSARDH                                                      
         BL    EXITOK              NOTHING LEFT IN SECOND BUFFER                
         GOTOX (RF),TSADEL                                                      
         B     INI102                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR SCREEN 1                                                  *         
***********************************************************************         
         SPACE 1                                                                
FSCR1    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST PAGE 1                                          *         
***********************************************************************         
         SPACE 1                                                                
FLST1    XC    MYELDISP,MYELDISP                                                
         L     R3,AIOREC                                                        
         USING RCOVREC,R3                                                       
         MVC   IOKEY(L'RCOVKEY),RCOVKEY                                         
         MVI   IOKEY+26,1          FIRST TEXT REC IN SEQUENCE                   
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XORD)                                   
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               NO SEQUENCE OF RECORDS                       
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGET)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         L     R3,AIO4                                                          
         BE    NLST102                                                          
         DC    H'0'                READ ERROR                                   
         SPACE 2                                                                
***********************************************************************         
* BUILD LIST PAGE 1                                                   *         
***********************************************************************         
         SPACE 1                                                                
NLST1    LH    R3,MYELDISP         A(LAST 03 ELEM)                              
         A     R3,AIO4             FIND IT                                      
         XR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF               NEXT ELEM                                    
         CLI   0(R3),3             IS 03?                                       
         BE    NLST105             YES                                          
*                                                                               
*        END OF 03 ELEMS IN REC, SEE IF CONTINUATION REC EXISTS                 
*                                                                               
         L     R3,AIO4                                                          
         USING RCOVREC,R3                                                       
         MVC   IOKEY(L'RCOVKEY),RCOVKEY                                         
         ZIC   R4,RCOVKSEQ                                                      
         LA    R4,1(R4)                                                         
         STC   R4,IOKEY+26                                                      
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XORD)                                   
         GOTOX ('XIO',AGROUTS)                                                  
         TM    IOERR,IOERNF                                                     
         BO    EXITL                                                            
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGET)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BE    NLST102                                                          
         DC    H'0'                                                             
*                                                                               
NLST102  LA    R3,RCOVEL1          1ST ELEM IN REC                              
         CLI   0(R3),3             MUST BE AN 03 IN CONTINUATION RECS           
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
NLST105  DS    0H                                                               
         S     R3,AIO4             DISP TO 03 ELEM                              
         STH   R3,MYELDISP         SAVE IT                                      
*                                                                               
         L     R1,AIO4                                                          
         MVC   0(L'RCOVKEY,R2),0(R1)                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM DIRECTORY 1                                        *         
***********************************************************************         
         SPACE 1                                                                
TSARDIR1 B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,SVPARMS4         A(TSAR BUFFER)                               
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)  SET NEW RECORD LENGTH                        
         XC    T2LST,T2LST                                                      
*                                                                               
         LH    RF,CCOUNT           CURRENT LINE INDEX                           
         LA    RF,1(RF)                                                         
         STH   RF,CCOUNT                                                        
         STCM  RF,3,TLKSNUM        LINE NUMBER                                  
         LH    RF,PCOUNT           CURRENT POINTER COUNTER                      
         LA    RF,1(RF)                                                         
         STH   RF,PCOUNT                                                        
         STCM  RF,3,TL2MAP         BUFFER 1 -> BUFFER 2 POINTER                 
         STCM  RF,3,T2KEY          BUFFER 2 KEY                                 
*                                                                               
         L     R2,AIO4                                                          
         AH    R2,MYELDISP         GET TO CURRENT 03 ELEM IN REC                
         CLI   0(R2),3                                                          
         BE    *+6                                                              
         DC    H'0'                SHOULD BE LOOKING AT AN 03 ELEM!             
         XR    R4,R4                                                            
         CLI   1(R2),3             BLANK LINE?                                  
         BL    TFIL102             YES                                          
*                                                                               
         IC    R4,1(R2)                                                         
         SH    R4,=H'3'            COPY DATA LINE TO BUFFER2                    
         EX    R4,*+4                                                           
         MVC   T2LINE(0),2(R2)     ACTUAL DATA LINE                             
         LA    R4,1(R4)                                                         
*                                                                               
TFIL102  STC   R4,T2LLEN           LINE DATA LENGTH                             
         GOTOX TSAR2,TSAADD                                                     
         BE    EXITOK                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 MVI   MYIOFLAG,0          FIRST TIME                                   
         GOTO1 =A(NEXTREC),RR=Y    GET 1ST REC FOR TEXT ELEMS                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 GOTO1 =A(WRITEREC),RR=Y   WRITE THE CURRENT REC                        
*        L     R2,AIO4             NOW FIND AND DELETE THE REST                 
         USING RCOVREC,R2                                                       
*                                                                               
UPDL105  MVC   IOKEY(L'RCOVKEY),RCOVKEY                                         
         ZIC   R4,RCOVKSEQ                                                      
         LA    R4,1(R4)                                                         
         STC   R4,IOKEY+26                                                      
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XORDUPD)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   UPDL1X              NOT FOUND                                    
         TM    IOERR,IOEDEL        ALREADY DELETED?                             
         BO    UPDL1X              ALL DONE                                     
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGETRUP)                               
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    RCOVCNTL,X'80'      DELETE RECORD                                
***>     GOTOX ADELEL,BOPARM,(3,AIO4),0  DELETE ELEMENTS                        
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOPUT)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    IOKEY+27,X'80'      DELETE KEY                                   
         ICM   R1,15,=AL4(XOREPDIR+XOWRITE)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    UPDL105             NEXT RECORD                                  
         DC    H'0'                                                             
*                                                                               
UPDL1X   B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* SET UP A NEW LINE IN THE LIST 1                                     *         
***********************************************************************         
         SPACE 1                                                                
NEWITEM1 L     R3,SVPARMS3         A(TSAR BUFFER)                               
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)  SET NEW RECORD LENGTH                        
         XC    T2LST,T2LST                                                      
*                                                                               
         LH    RF,CCOUNT           CURRENT LINE INDEX                           
         LA    RF,1(RF)                                                         
         STH   RF,CCOUNT                                                        
         STCM  RF,3,TLKSNUM        SET LINE INDEX NUMBER                        
         LH    RF,PCOUNT           CURRENT POINTER COUNT                        
         LA    RF,1(RF)                                                         
         STH   RF,PCOUNT                                                        
         STCM  RF,3,TL2MAP         BUFFER 1 -> BUFFER 2 POINTER                 
         STCM  RF,3,T2KEY          BUFFER 2 KEY                                 
*                                                                               
         GOTOX TSAR2,TSAADD                                                     
         BE    EXITOK                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* UPDATE DIRECTORY FROM TSAR RECORD 1                                 *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
UPDREC1  L     R2,AIO4                                                          
         USING RCOVREC,R2                                                       
         USING TLSTD,R3                                                         
         XC    T2LST,T2LST                                                      
         MVC   T2KEY,TL2MAP                                                     
         GOTOX TSAR2,TSARDH                                                     
         CLC   T2KEY,TL2MAP                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    BOELEM,BOELEM                                                    
         MVI   BOELEM,3                                                         
         XR    RF,RF                                                            
         ICM   RF,1,T2LLEN                                                      
         BZ    UREC102                                                          
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   BOELEM+2(0),T2LINE                                               
         LA    RF,3(RF)                                                         
         B     *+8                                                              
*                                                                               
UREC102  LA    RF,2                                                             
         STC   RF,BOELEM+1                                                      
         DROP  R2                                                               
*                                                                               
UPDR105  DS    0H                                                               
         GOTOX VHELLO,BOPARM,(C'P',GCFILNAM),(R2),BOELEM,=C'ADD=CODE'           
         CLI   12(R1),0                                                         
         BE    EXITOK              NO OVERFLOW - OK, ALL DONE                   
         CLI   12(R1),5            REC OVERFLOW?                                
         BE    *+6                                                              
         DC    H'0'                NO OTHER ERRORS TOLERATED                    
*                                                                               
* FOLLOWING CODES REMOVED SINCE HELLO WILL NOT UPDATE RECORD DUE                
* TO OVERFLOW                                                                   
*                                                                               
*&&DO                                                                           
         L     R4,AIO4             NOW DELETE THE ELEM WE JUST ADDED            
         USING RCOVREC,R4                                                       
         LA    R4,RCOVEL1          1ST ELEM IN RECORD                           
         XR    RE,RE                                                            
*                                                                               
UPDR110  LR    RF,R4               REMEMBER THIS SPOT                           
         IC    RE,1(R4)                                                         
         AR    R4,RE               NEXT ELEM                                    
         CLI   0(R4),0             END OF RECORD?                               
         BNE   UPDR110             NO - KEEP GOING                              
*                                                                               
         IC    RE,1(RF)            YES - GET LEN OF LAST ELEM IN REC            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    UPDR112                                                          
         CLC   0(0,RF),BOELEM      IS THIS THE ELEM WE JUST ADDED?              
         DC    H'0'                IT HAD BETTER BE                             
*                                                                               
UPDR112  MVI   0(RF),0             KILL ELEM                                    
         LA    RE,1(RE)            FIX LEN                                      
         L     R4,AIO4                                                          
         XR    RF,RF                                                            
         ICM   RF,3,RCOVLEN        RECORD LEN                                   
         SR    RF,RE               NEW REC LEN                                  
         STCM  RF,3,RCOVLEN                                                     
         DROP  R4                                                               
*&&                                                                             
*                                                                               
UPDR113  DS    0H                                                               
         MVC   BOWORK1,BOELEM      SAVE ELEM                                    
         GOTO1 =A(WRITEREC),RR=Y                                                
         GOTO1 =A(NEXTREC),RR=Y    GET NEXT RECORD                              
         MVC   BOELEM(L'BOWORK1),BOWORK1                                        
         B     UPDR105             TRY AGAIN                                    
         SPACE 2                                                                
***********************************************************************         
* LAST FOR SCREEN PAGE 1 (SORTS & REDISPLAYS TSAR RECORDS)            *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST1 MVC   GS1STREC,LS1STLIN                                                
         TM    DOFLAG,DOFROM+DOTO                                               
         BO    SLAST02             FROM & TO ACTIONS BOTH SET                   
         BNZ   SLAST04             EITHER FROM OR TO ACTION SET                 
*                                                                               
         TM    DOFLAG,DODEL+DOREP+DOINS                                         
         BZ    SLAST06             NO ACTIONS SET                               
*                                                                               
SLAST02  BAS   RE,SORT             PROVIDE CODE TO RESOLVE RESULTS              
         BE    SLAST03                                                          
*                                                                               
         BAS   RE,REACT            ACTIONS NEED REDRAWING                       
         MVC   BOCURSOR,AFRSTFR    MOVE/COPY BEFORE/AFTER CONFLICT              
         MVC   FVMSGNO,=AL2(ER#CMDCF)                                           
         B     SLASTL                                                           
*                                                                               
SLAST03  GOTO1 =A(REDRAW),RR=BORELO                                             
         OI    GCINDS2,GCIANYCH    SET CHANGES THIS TIME                        
         B     SLAST06                                                          
*                                                                               
SLAST04  BAS   RE,REACT            ACTIONS NEED REDRAWING                       
*                                                                               
         CLC   DOFRCNT,=H'1'       TEST DUPLICATE FROM FIELDS                   
         BNH   *+20                NO - GOOD                                    
         MVC   BOCURSOR,AFRSTFR                                                 
         MVC   FVMSGNO,=AL2(ER#CMDCF)                                           
         B     SLASTL              SET COMMAND CONFLICT                         
*                                                                               
         CLC   DOTOCNT,=H'1'       TEST DUPLICATE TO FIELDS                     
         BNH   *+20                NO - GOOD                                    
         MVC   BOCURSOR,AFRSTTO                                                 
         MVC   FVMSGNO,=AL2(ER#CMDCF)                                           
         B     SLASTL                                                           
*                                                                               
         MVC   FVMSGNO,=AL2(ER#MCIP)                                            
*                                                                               
SLAST06  CLC   SEQNUM,SEQLAST      SEQUENCE NUMBER CHANGED?                     
         BE    SLAST08             NO                                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,SEQNUM         REQUESTED SEQUENCE NUMBER                    
         BNP   *+6                                                              
         BCTR  RF,0                ZERO BASE IT                                 
*                                                                               
         AH    RF,LSLST#1          FIRST NUMBER IN LIST                         
         CH    RF,LSLST#X          STILL IN LIST?                               
         BNH   *+8                                                              
         LH    RF,LSLST#X                                                       
         STH   RF,LSPAG#1          FIRST IN PAGE NUMBER                         
*                                                                               
         L     R3,ASEQFLD          EDIT OUT CORRECT NUMBER                      
         USING FHD,R3                                                           
         OI    FHOI,FHOITR                                                      
         SH    RF,LSLST#1          FIRST NUMBER IN LIST                         
         LA    RF,1(RF)                                                         
         CURED (RF),(3,FHDA),0,DMCB=BOPARM,ALIGN=LEFT                           
         DROP  R3                                                               
*                                                                               
         MVC   SEQLAST,SEQNUM                                                   
         GOTO1 =A(REDRAW),RR=BORELO REDISPLAY A SCREEN FULL OF DATA             
*                                                                               
SLAST08  DS    0H                                                               
         OC    AINSFLD,AINSFLD     WANT TO BUNG CURSOR ON INSERT LINE?          
         BZ    *+10                NO                                           
         MVC   BOCURSOR,AINSFLD    DO IT BEAVIS...                              
*                                                                               
         LH    R1,LSLST#X                                                       
         SH    R1,LSLST#1                                                       
         LA    R1,1(R1)                                                         
         CH    R1,LSLINPAG         LIST LINES=LINES ON PAGE                     
         BNE   SLAST10             NO                                           
*                                                                               
         L     R3,ATLST            YES - ADD AN EXTRA DUMMY RECORD              
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)  SET NEW RECORD LENGTH                        
         MVC   TLKSES,TWASESNL                                                  
*                                                                               
         LH    RF,CCOUNT           CURRENT LINE INDEX                           
         LA    RF,1(RF)                                                         
         STH   RF,CCOUNT                                                        
         STCM  RF,3,TLKSNUM        LINE NUMBER                                  
         LH    RF,PCOUNT           CURRENT POINTER COUNTER                      
         LA    RF,1(RF)                                                         
         STH   RF,PCOUNT                                                        
         STCM  RF,3,TL2MAP         BUFFER 1 -> BUFFER 2 POINTER                 
         STCM  RF,3,T2KEY          BUFFER 2 KEY                                 
         MVI   T2LLEN,1         LINE DATA LENGTH                                
         MVC   T2LINE,BCSPACES                                                  
         GOTOX AGENLST,BOPARM,OLIST,LTSARADD                                    
         GOTOX TSAR2,TSAADD                                                     
         BE    SLAST10                                                          
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
SLAST10  CLC   FVMSGNO,=AL2(ER#MCIP)                                            
         BE    SLASTL              SETTING OWN MESSAGE                          
         L     RE,ACOUNT                                                        
         EDIT  CCOUNT,(5,8(RE)),ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1               
         OI    6(RE),X'80'                                                      
         B     SLASTOK                                                          
*                                                                               
SLASTL   DS    0H                                                               
         B     EXITL                                                            
*                                                                               
SLASTOK  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SUB ACTION OBJECT                                                   *         
* -----------------                                                   *         
* P3 = A(SUB-ACTION FIELD)                                            *         
***********************************************************************         
         SPACE 1                                                                
SUB      OC    GSSMPAGE,GSSMPAGE   VALIDATION ONLY FOR MAINT LIST               
         BZ    EXITH                                                            
*                                                                               
         TM    LSSCIND2,LSSCIDIS   IGNORE IT IF WE ARE DISPLAYING               
         BO    EXITOK                                                           
*                                                                               
         LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,SUBTABL                                                       
         B     ITER                                                             
*                                                                               
SUBTABL  DC    AL1(SAVAL),AL1(0,0,0),AL4(SUBVAL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SUB ACTION FOR LIST                                        *         
***********************************************************************         
         SPACE 1                                                                
SNGL     USING SUBACTSD,LSNTRY                                                  
MLTI     USING SUBACTSD,LMNTRY                                                  
SUBVAL   XC    LSNTRY,LSNTRY       CLEAR CURRENT SUB-ACTION ELEMENT             
         OI    LSSCIND1,LSSCIINP   KEEP CURRENT SCREEN                          
         L     R2,SVPARMS3         R2=A(SUB-ACTION FIELD)                       
         USING FHD,R2                                                           
         L     R4,ATLST            R4=A(TSAR RECORD)                            
         USING TLSTD,R4                                                         
*                                                                               
         OC    TLUSTAT,TLUSTAT     ANY ACTIONS FOR THIS RECORD                  
         BZ    SVAL04              NO                                           
         TM    FHII,FHIITH         FIELD INPUT BY USER?                         
         BO    SVAL04              YES - OVERRIDE CURRENT SETTING               
*                                                                               
         LA    RF,TLUACTS          ACTION NAME DISPLAY TABLE                    
         USING TLUACTSD,RF                                                      
SVAL02   CLI   0(RF),EOT           UNKNOWN FLAG IN TSAR RECORD                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BOBYTE1,TLUSTAT     COPY STATUS FLAG                             
         NC    BOBYTE1,TLUFLAG     IS THIS ACTION REQUEST SET?                  
         BNZ   *+12                YES                                          
         LA    RF,TLULQ(RF)                                                     
         B     SVAL02              NEXT KNOWN ACTION                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,TLUNAME                                                     
         A     RE,AOVERWRK         RE=A(NAME OF ACTION)                         
         MVC   FHDA(SBLEN),0(RE)   MOVE OUT NAME INTO FIELD                     
         OI    FHOI,FHOITR         TRANSMIT IT                                  
         OI    FHII,FHIIVA         SET FIELD VALID                              
         DROP  RF                                                               
*                                                                               
SVAL04   TM    FHII,FHIIVA         FIELD VALIDATED?                             
         BO    *+8                 NO                                           
         OI    LSSCIND1,LSSCIINP   YES - KEEP CURRENT SCREEN                    
*                                                                               
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         CLI   FVILEN,0            SUB-ACTION ENTERED?                          
         BE    SVAL18              NO                                           
         CLI   FVIFLD,C'*'         IGNORE THIS FIELD?                           
         BE    SVAL18              YES                                          
         CLI   CSACT,A#MAINT                                                    
         BE    SVAL05              MAINT ACTION OK                              
         CLI   CSACT,A#CHA                                                      
         BNE   EXITNV              INPUT INVALID UNLESS ACTION CHANGE           
*                                                                               
SVAL05   DS    0H                                                               
         XC    LMCOUNT,LMCOUNT     RESET REPEAT COUNT                           
         XR    RE,RE                                                            
         ICM   RE,1,FVXLEN                                                      
         BZ    SVAL12              ONLY 1 CHARACTER INPUT                       
         LA    RF,FVIFLD(RE)       TEST SUFFIX CHARACTER                        
*                                                                               
         CLI   0(RF),C'0'          NUMERICAL ENDING?                            
         BL    SVAL12              NO                                           
         CLI   0(RF),C'9'                                                       
         BH    SVAL12              NO                                           
*                                                                               
         XR    R1,R1               R1 HOLDS LENGTH                              
SVAL06   CLI   0(RF),C'0'          STILL NUMERIC?                               
         BL    SVAL08              NO                                           
         CLI   0(RF),C'9'                                                       
         BH    SVAL08              NO                                           
         LA    R1,1(R1)                                                         
         BCTR  RF,0                                                             
         BCT   RE,SVAL06                                                        
*                                                                               
SVAL08   BCTR  R1,0                                                             
         EX    R1,SVALPAK          OBTAIN PACKED NUMBER                         
         CVB   R0,GCDUB1                                                        
         EX    R1,SVALMVE          CLEAR NUMBER FROM INPUT FIELD                
         B     SVAL10                                                           
*                                                                               
SVALPAK  PACK  GCDUB1,1(0,RF)      PACK NUMBER INTO GCDUB1                      
SVALMVE  MVC   1(0,RF),BCSPACES    CLEAR NUMERIC PORTION OF FIELD               
*                                                                               
SVAL10   STH   R0,LMCOUNT          SAVE MULTILINE ACTION REPEAT NUMBER          
         XR    R0,R0                                                            
         IC    R0,FVILEN           REVALIDATE FVIFLD                            
         GOTOX ('FLDVAL',AGROUTS),0                                             
*                                                                               
SVAL12   LA    R3,SUBACTS          TRY TO MATCH SUB-ACTION                      
         USING SUBACTSD,R3                                                      
         XR    R1,R1                                                            
         IC    R1,FVXLEN           LENGTH OF INPUT                              
*                                                                               
SVAL14   CLC   SUBUPR,=AL2(EOT)    REACHED END OF TABLE?                        
         BE    SVALL               YES - INVALID SUB-ACTION                     
         XR    RF,RF                                                            
         ICM   RF,3,SUBUPR         TRY TO MATCH UPPERCASE NAME                  
         A     RF,AOVERWRK                                                      
         EX    R1,SUBMCH                                                        
         BE    SVAL16                                                           
         XR    RF,RF                                                            
         ICM   RF,3,SUBLWR         TRY TO MATCH LOWERCASE NAME                  
         A     RF,AOVERWRK                                                      
         EX    R1,SUBMCH                                                        
         BE    SVAL16                                                           
         LA    R3,SUBACTLQ(R3)                                                  
         B     SVAL14                                                           
*                                                                               
SUBMCH   CLC   FVIFLD(0),0(RF)                                                  
*                                                                               
SVAL16   MVC   LSNTRY,0(R3)        SAVE THIS SINGLE ENTRY                       
         OI    FHII,FHIIVA         SET FIELD VALID                              
*                                                                               
         ICM   RF,15,SNGL.SUBRTN   PROCESSING ROUTINE                           
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  R3                                                               
*                                                                               
SVAL18   OC    TLUSTAT,TLUSTAT     ACTION FLAG ON THIS FIELD BEFORE?            
         BZ    EXITOK              NO - SAFE TO IGNORE IT                       
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         LA    R1,FHD(RE)          R1=A(FIRST DATA FIELD)                       
FLD      USING FHD,R1                                                           
*                                                                               
         LA    RF,FHD              START OF THIS LINE                           
         AH    RF,LSLINLEN         LENGTH OF A LINE                             
         BCTR  RF,0                RF=A(END OF THIS LINE-1)                     
*                                                                               
SVAL20   IC    RE,FLD.FHLN         SET INDEX IN RE                              
         OI    FLD.FHOI,FHOITR     TRANSMIT FIELD                               
         NI    FLD.FHAT,FF-FHATHI  TURN OFF HIGHLIGHT                           
         BXLE  R1,RE,SVAL20        REPEAT FOR ALL FIELDS ON LINE                
         DROP  FLD                                                              
*                                                                               
         TM    TLUSTAT,TLUSCPY+TLUSMVE   'FROM' FLAG ON THIS ONE                
         BZ    SVAL22                    NO                                     
         LH    RF,DOFRCNT                DECREMENT 'FROM' COUNT                 
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DOFROM          TURN OFF FLAG IF NO 'FROM'             
         STH   RF,DOFRCNT                SAVE NEW 'FROM' COUNT                  
*                                                                               
SVAL22   TM    TLUSTAT,TLUSBEF+TLUSAFT   'TO' FLAG ON THIS ONE                  
         BZ    SVAL24                    NO                                     
         LH    RF,DOTOCNT                DECREMENT 'TO' COUNT                   
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DOTO            TURN OFF FLAG IF NO 'TO'               
         STH   RF,DOTOCNT                SAVE NEW 'TO' COUNT                    
*                                                                               
SVAL24   TM    TLUSTAT,TLUSDEL           'DELETE' FLAG ON THIS ONE              
         BZ    SVAL26                    NO                                     
         LH    RF,DODLCNT                DECREMENT 'DELETE' COUNT               
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DODEL           TURN OFF FLAG IF NO 'DELETE'           
         STH   RF,DODLCNT                                                       
*                                                                               
SVAL26   TM    TLUSTAT,TLUSREP     'REPLICATE' FLAG ON THIS ONE                 
         BZ    SVAL28              NO                                           
         LH    RF,DORPCNT          DECREMENT 'REPLICATE' COUNT                  
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DOREP                                                  
         STH   RF,DORPCNT                                                       
*                                                                               
SVAL28   TM    TLUSTAT,TLUSINS     'INSERT' FLAG ON THIS ONE                    
         BZ    SVAL30              NO                                           
         LH    RF,DOINCNT          DECREMENT 'INSERT' COUNT                     
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DOINS                                                  
         STH   RF,DORPCNT                                                       
*                                                                               
SVAL30   XC    TLUSTAT,TLUSTAT     RESET ACTION FLAG ON THIS ONE                
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
         OI    FHII,FHIIVA                                                      
         B     EXITOK                                                           
*                                                                               
SVALL    B     EXITL                                                            
         DROP  R2,R4                                                            
*                                                                               
SUBACTS  DS    0H              *** TABLE OF VALID SUB-ACTIONS ***               
         DC    AL2(FU@COPY-OVERWRKD,FL@COPY-OVERWRKD),AL4(SUB_CPY)              
         DC    AL2(FU@MOVE-OVERWRKD,FL@MOVE-OVERWRKD),AL4(SUB_MVE)              
         DC    AL2(FU@DEL-OVERWRKD,FL@DEL-OVERWRKD),AL4(SUB_DEL)                
         DC    AL2(FU@BFR-OVERWRKD,FL@BFR-OVERWRKD),AL4(SUB_BFR)                
         DC    AL2(FU@AFTER-OVERWRKD,FL@AFTER-OVERWRKD),AL4(SUB_AFT)            
         DC    AL2(FU@REPL-OVERWRKD,FL@REPL-OVERWRKD),AL4(SUB_REP)              
         DC    AL2(FU@INSRT-OVERWRKD,FL@INSRT-OVERWRKD),AL4(SUB_INS)            
         DC    AL2(EOT)                                                         
*                                                                               
SUBACTSD DSECT                                                                  
SUBUPR   DS    AL2                 DISPLACEMENT TO UPPERCASE NAME               
SUBLWR   DS    AL2                 DISPLACEMENT TO LOWER CASE NAME              
SUBRTN   DS    AL4                 DISPLACEMENT TO VALIDATION ROUTINE           
SUBACTLQ EQU   *-SUBACTSD                                                       
*                                                                               
COV11    CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DELETE A LINE FROM THE LIST                              *         
***********************************************************************         
         SPACE 1                                                                
SUB_DEL  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSDEL     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSDEL     SET TO DELETE THIS LINE                      
         XR    R0,R0                                                            
         ICM   R0,3,LMCOUNT        SET NUMBER OF REPEATS REQUIRED               
         BNZ   *+8                                                              
         LA    R0,1                                                             
         STC   R0,TLURPT                                                        
*                                                                               
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SDEL02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SDEL02   LH    RF,DODLCNT          INCREMENT COUNT OF DELETE FIELDS             
         LA    RF,1(RF)                                                         
         STH   RF,DODLCNT                                                       
         OI    DOFLAG,DODEL        SET A DELETE ACTION REQUIRED                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO REPLICATE A LINE IN THE LIST                             *         
***********************************************************************         
         SPACE 1                                                                
SUB_REP  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSREP     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSREP     SET TO REPLICATE THIS LINE                   
         XR    R0,R0                                                            
         ICM   R0,3,LMCOUNT        SET NUMBER OF REPEATS REQUIRED               
         BNZ   *+8                                                              
         LA    R0,1                                                             
         STC   R0,TLURPT                                                        
*                                                                               
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SREP02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SREP02   LH    RF,DORPCNT          INCREMENT COUNT OF REPLICATE FIELDS          
         LA    RF,1(RF)                                                         
         STH   RF,DORPCNT                                                       
         OI    DOFLAG,DOREP        SET A REPLICATE ACTION REQUIRED              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO INSERT A LINE IN THE LIST                                *         
***********************************************************************         
         SPACE 1                                                                
SUB_INS  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSINS     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSINS     SET TO REPLICATE THIS LINE                   
         XR    R0,R0                                                            
         ICM   R0,3,LMCOUNT        SET NUMBER OF REPEATS REQUIRED               
         BNZ   *+8                                                              
         LA    R0,1                                                             
         STC   R0,TLURPT                                                        
*                                                                               
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SINS02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SINS02   LH    RF,DOINCNT          INCREMENT COUNT OF REPLICATE FIELDS          
         LA    RF,1(RF)                                                         
         STH   RF,DOINCNT                                                       
         OI    DOFLAG,DOINS        SET A INSERT ACTION REQUIRED                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO COPY A LINE IN A LIST                                    *         
***********************************************************************         
         SPACE 1                                                                
SUB_CPY  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSCPY     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSCPY     SET TO COPY THIS FIELD                       
         XR    R0,R0                                                            
         ICM   R0,3,LMCOUNT        SET NUMBER OF REPEATS REQUIRED               
         BNZ   *+8                                                              
         LA    R0,1                                                             
         STC   R0,TLURPT                                                        
*                                                                               
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SCPY02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SCPY02   LH    RF,DOFRCNT          INCREMENT COUNT OF 'FROM' FIELDS             
         LA    RF,1(RF)                                                         
         STH   RF,DOFRCNT                                                       
         OI    DOFLAG,DOFROM       SET A 'FROM' ACTION REQUIRED                 
*                                                                               
         CLC   DOFRCNT,=H'1'       TEST ANOTHER FROM FIELD SET UP               
         BNH   EXITOK              NO - GOOD                                    
         MVC   FVMSGNO,=AL2(ER#CMDCF)                                           
         B     EXITL                                                            
         SPACE  2                                                               
***********************************************************************         
* ROUTINE TO MOVE A LINE IN A LIST                                    *         
***********************************************************************         
         SPACE 1                                                                
SUB_MVE  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSMVE     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSMVE                                                  
         XR    R0,R0                                                            
         ICM   R0,3,LMCOUNT        SET NUMBER OF REPEATS REQUIRED               
         BNZ   *+8                                                              
         LA    R0,1                                                             
         STC   R0,TLURPT                                                        
*                                                                               
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SMVE02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SMVE02   LH    RF,DOFRCNT          INCREMENT COUNT OF 'FROM' FIELDS             
         LA    RF,1(RF)                                                         
         STH   RF,DOFRCNT                                                       
         OI    DOFLAG,DOFROM                                                    
         CLC   DOFRCNT,=H'1'       TEST ANOTHER FROM FIELD SET UP               
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(ER#CMDCF)                                           
         B     EXITL                                                            
         SPACE  2                                                               
***********************************************************************         
* ROUTINE TO COPY BEFORE THIS LINE IN A LIST                          *         
***********************************************************************         
         SPACE 1                                                                
SUB_BFR  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSBEF     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSBEF                                                  
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SBEF02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SBEF02   LH    RF,DOTOCNT          INCREMENT COUNT OF 'TO' FIELDS               
         LA    RF,1(RF)                                                         
         STH   RF,DOTOCNT                                                       
         OI    DOFLAG,DOTO                                                      
         CLC   DOTOCNT,=H'1'       TEST ANOTHER COPY TO FIELD SET UP            
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(ER#CMDCF)                                           
         B     EXITL                                                            
         SPACE  2                                                               
***********************************************************************         
* ROUTINE TO COPY AFTER THIS LINE IN A LIST                           *         
***********************************************************************         
         SPACE 1                                                                
SUB_AFT  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSAFT     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSAFT                                                  
*                                                                               
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SAFT02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SAFT02   LH    RF,DOTOCNT          INCREMENT COUNT OF 'TO' FIELDS               
         LA    RF,1(RF)                                                         
         STH   RF,DOTOCNT                                                       
         OI    DOFLAG,DOTO                                                      
         CLC   DOTOCNT,=H'1'       TEST ANOTHER COPY TO FIELD SET UP            
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(ER#CMDCF)                                           
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* PFKEY OBJECT                                                        *         
* ------------                                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
PFK      LM    R0,R3,SVPARMS                                                    
         LA    RF,PFKTABL                                                       
         B     ITER                                                             
*                                                                               
PFKTABL  EQU   *                                                                
*        DC    AL1(PFREC),AL1(0,0,0),AL4(PPFREC)                                
*        DC    AL1(PFACT),AL1(0,0,0),AL4(PPFACT)                                
*        DC    AL1(PFUSER),AL1(0,0,0),AL4(PPFUSR)                               
         DC    AL1(PFLST),AL1(0,0,0),AL4(PPFLST)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DETERMINE WHETHER PFKEY VALID FOR INCLUSION                         *         
***********************************************************************         
         SPACE 1                                                                
PPFLST   L     RF,SVPARMS3                                                      
         USING FRPELD,RF                                                        
         TM    KFLAGS,KFFROMK      FROM CONTRACT?                               
         BZ    EXITOK              NO - ALL PFKEYS OK                           
*                                                                               
         CLI   FRPPFK#,PFK02       DISP/CHA TOGGLE                              
         BE    EXITL               NOT VALID                                    
         B     EXITOK              OTHER KEYS OK                                
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* SORT OUT TSAR RECORDS BASED ON FLAGS SET IN THEM                    *         
***********************************************************************         
         SPACE 1                                                                
SORT     NTR1  ,                                                                
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         XC    TLNUM,TLNUM                                                      
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,TWASESNL     SET CURRENT NEST LEVEL                       
         LA    R1,TSARDH                                                        
         B     *+8                                                              
*                                                                               
SRT02    LA    R1,TSANXT           FIND ANY MOVE/COPY RECORDS FLAGGED           
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    SRT06               END OF FILE                                  
         CLC   TLKSES,TWASESNL     CORRECT NEST LEVEL?                          
         BNE   SRT06               FINISHED                                     
*                                                                               
         TM    TLUSTAT,TLUSCPY+TLUSMVE                                          
         BZ    SRT02               NOT A COPY OR A MOVE RECORD                  
*                                                                               
         MVC   TLMSTAT,TLUSTAT                                                  
         MVI   BOBYTE1,FF          FLAG RECORD(S) TO COPY/MOVE                  
         XR    R0,R0                                                            
         ICM   R0,1,TLURPT         MULTI-LINE COPY/MOVE COUNTER                 
         BNZ   *+8                                                              
         LA    R0,1                                                             
*                                                                               
SRT04    TM    TLUSTAT,(TLUSAFT+TLUSBEF+TLUSDEL+TLUSREP+TLUSINS)                
         BNZ   SRTERR              COMMAND CONFLICT                             
*                                                                               
         MVC   TLMSVE,TLNUM        SAVE THIS RECORD NUMBER                      
         MVI   TLKSES,C'M'         TEMPORARY MOVE/COPY RECORD STAMP             
*                                                                               
         TM    TLMSTAT,TLUSCPY     IS THIS A COPY?                              
         BZ    SRT05               NO                                           
         XC    T2LST,T2LST         GET POINTER RECORD                           
         MVC   T2KEY,TL2MAP                                                     
         GOTOX TSAR2,TSARDH                                                     
         CLC   T2KEY,TL2MAP                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LH    RE,PCOUNT                                                        
         LA    RE,1(RE)                                                         
         STH   RE,PCOUNT                                                        
         STCM  RE,3,T2KEY                                                       
         STCM  RE,3,TL2MAP                                                      
         GOTOX TSAR2,TSAADD        ADD POINTER COPY WITH NEW KEY                
*                                                                               
SRT05    GOTOX ('TSARIO',AGROUTS),TSAADD                                        
         MVC   TLNUM,TLMSVE        RESTORE THIS RECORD NUMBER                   
         GOTO1 (RF),TSAGET         AND GET RECORD BACK                          
         GOTO1 (RF),TSANXT         GET NEXT RECORD IN LIST                      
         BL    SRT06               END OF FILE                                  
         CLC   TLKSES,TWASESNL     CORRECT NEST LEVEL?                          
         BNE   SRT06               FINISHED                                     
         BCT   R0,SRT04                                                         
*                                                                               
SRT06    XC    CCOUNT,CCOUNT       RESET CURRENT LIST COUNT                     
         XC    TLNUM,TLNUM                                                      
*                                                                               
SRT08    XC    TLKEY,TLKEY                                                      
         MVC   TLKSES,TWASESNL                                                  
         GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    SRT20               END OF FILE                                  
         CLC   TLKSES,TWASESNL     CHECK NEST LEVEL                             
         BNE   SRT20               DONE ALL FOR THIS LEVEL                      
*                                                                               
         TM    TLUSTAT,(TLUSDEL+TLUSMVE)                                        
         BZ    SRT12               NOT MOVING OR DELETING THIS LINE             
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,TLURPT         MULTI-LINE DELETE/MOVE COUNTER               
         BNZ   *+8                                                              
         LA    R0,1                                                             
*                                                                               
SRT10    GOTO1 (RF),TSADEL         DELETE THIS RECORD                           
         GOTO1 (RF),TSARDH         READ NEXT RECORD                             
         BL    SRT20                                                            
         CLC   TLKSES,TWASESNL     TEST NEST LEVEL                              
         BNE   SRT20                                                            
         BCT   R0,SRT10            DELETE NEXT IN SERIES                        
         B     SRT08               CONTINUE                                     
*                                                                               
SRT12    CLI   BOBYTE1,FF          COPY/MOVE RECORD(S) TO ADD IN?               
         BNE   SRT14               NO                                           
         TM    TLUSTAT,(TLUSAFT+TLUSBEF)                                        
         BZ    SRT14               NO ADD BEFORE/AFTER THIS RECORD              
*                                                                               
         MVI   BOBYTE2,FF          SET ADD AFTER FLAG                           
         TM    TLUSTAT,TLUSAFT     ADDING AFTER THIS RECORD?                    
         BO    SRT14               YES                                          
         MVI   BOBYTE2,0           RESET ADD AFTER FLAG                         
*                                                                               
         MVC   TLMSVE,TLNUM        SAVE CURRENT RECORD                          
         BAS   RE,ADDIN            ADD RECORD(S) BEFORE THIS RECORD             
         MVC   TLNUM,TLMSVE        RESTORE CURRENT RECORD                       
         GOTOX (RF),TSAGET                                                      
         B     SRT18                                                            
*                                                                               
SRT14    TM    TLUSTAT,(TLUSREP+TLUSINS)                                        
         BZ    SRT18               NOT REPLICATING OR INSERTING                 
*                                                                               
         MVC   TLMSVE,TLNUM        SAVE CURRENT RECORD                          
         MVC   TLMSTAT,TLUSTAT     SAVE LINE STATUS BYTE                        
         MVC   TLMAP,TL2MAP        AND POINTER NUMBER                           
*                                                                               
         XR    R0,R0               GET REPEAT COUNT                             
         ICM   R0,1,TLURPT                                                      
         BNZ   *+8                                                              
         LA    R0,1                                                             
*                                                                               
         XC    T2LST,T2LST         GET ASSOCIATED POINTER RECORD                
         MVC   T2KEY,TL2MAP                                                     
         GOTOX TSAR2,TSARDH                                                     
         CLC   T2KEY,TL2MAP                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TLUSTAT,TLUSINS     INSERTING A LINE?                            
         BZ    *+14                NO                                           
         XC    T2LINE,T2LINE       CLEAR LINE                                   
         MVI   T2LLEN,0                                                         
*                                                                               
SRT16    LH    RE,PCOUNT           ADD POINTER RECORD                           
         LA    RE,1(RE)                                                         
         STH   RE,PCOUNT                                                        
         STCM  RE,3,T2KEY          WITH NEW KEY                                 
         STCM  RE,3,TL2MAP                                                      
         GOTOX TSAR2,TSAADD        ADD POINTER RECORD                           
*                                                                               
         MVI   TLKSES,TLKSKEYC     SET TEMPORARY SHUFFLE KEY                    
         MVI   TLUSTAT,TLUTINS     FLAG LINE INSERTED                           
         XC    TLURPT,TLURPT       RESET REPEAT COUNT                           
         LH    RE,CCOUNT                                                        
         LA    RE,1(RE)                                                         
         STH   RE,CCOUNT                                                        
         STCM  RE,3,TLKSNUM        RESET KEY NUMBER                             
         GOTOX ('TSARIO',AGROUTS),TSAADD                                        
*                                                                               
         MVC   TLUSTAT,TLMSTAT     RESTORE STATUS AND POINTER                   
         BCT   R0,SRT16            LOOP FOR REQUIRED REPEAT COUNT               
         MVC   TLNUM,TLMSVE        RESTORE CURRENT RECORD                       
         GOTOX (RF),TSAGET                                                      
*                                                                               
SRT18    GOTOX ('TSARIO',AGROUTS),TSADEL                                        
         MVI   TLKSES,TLKSKEYC     SET TEMPORARY SHUFFLE KEY                    
         LH    RE,CCOUNT           RENUMBER KEY                                 
         LA    RE,1(RE)                                                         
         STH   RE,CCOUNT                                                        
         STCM  RE,3,TLKSNUM                                                     
         XC    TLUSTAT,TLUSTAT     RESET STATUS BYTE                            
         GOTOX (RF),TSAADD         ADD TEMPORARY COPY OF RECORD                 
*                                                                               
         CLI   BOBYTE2,FF          ADD AFTER FLAG SET?                          
         BNE   SRT08               NO                                           
         BAS   RE,ADDIN            ADD RECORD AFTER THIS RECORD                 
         B     SRT08               NEXT IN LIST                                 
*                                                                               
SRT20    XC    TLKEY,TLKEY         DELETE ANY CURRENT RECORDS LEFT              
         MVC   TLKSES,TWASESNL                                                  
         L     RF,AGROUTS                                                       
         ICM   RF,8,=AL1(TSARIO)                                                
*                                                                               
SRT21    GOTOX (RF),TSARDH                                                      
         BL    SRT22               END OF FILE                                  
         CLC   TLKSES,TWASESNL     SAME NEST LEVEL?                             
         BNE   SRT22               NO - FINISHED                                
         GOTOX (RF),TSADEL                                                      
         B     SRT21                                                            
*                                                                               
SRT22    XC    TLKEY,TLKEY         MOVE TEMPORARY TO CURRENT SESSION            
         MVI   TLKSES,TLKSKEYC                                                  
         GOTOX (RF),TSARDH                                                      
         BL    SRTX                END OF FILE                                  
         CLI   TLKSES,TLKSKEYC     STILL TEMPORARY LEVEL?                       
         BNE   SRTX                NO                                           
         GOTOX (RF),TSADEL         DELETE TEMPORARY RECORD                      
         MVC   TLKSES,TWASESNL     ADD CURRENT SESSION RECORD                   
         XC    TLUSTAT,TLUSTAT                                                  
         GOTOX (RF),TSAADD                                                      
         B     SRT22                                                            
*                                                                               
SRTX     MVC   LSLST#X,LSLST#1     SET START                                    
         NI    LSLTIND1,FF-LSLTISOL                                             
         XR    RF,RF                                                            
         ICM   RF,3,CCOUNT         COUNT OF ITEMS ADDED TO LIST                 
         BZ    EXITOK              NOTHING                                      
         AH    RF,LSLST#X                                                       
         BCTR  RF,0                                                             
         STH   RF,LSLST#X          SET CORRECT VALUE FOR END                    
         OI    LSLTIND1,LSLTISOL                                                
         B     EXITOK                                                           
*                                                                               
SRTERR   XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,C'M'         TEMP KEY FOR MOVE/COPY RECORDS               
         GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    SRTERR02                                                         
         CLI   TLKSES,C'M'                                                      
         BNE   SRTERR02                                                         
         GOTOX (RF),TSADEL         DELETE THIS RECORD                           
*                                                                               
SRTERR02 XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKSKEYC     TEMPORARY SHUFFLED NEST LEVEL                
         GOTOX (RF),TSARDH                                                      
         BL    SRTERRX                                                          
         CLI   TLKSES,TLKSKEYC                                                  
         BNE   SRTERRX                                                          
         GOTOX (RF),TSADEL         DELETE THIS RECORD                           
         B     SRTERR02                                                         
*                                                                               
SRTERRX  B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A RECORD SAVED IN SVLST TO CURRENT POINTER IN LIST   *         
***********************************************************************         
         SPACE 1                                                                
ADDIN    NTR1  ,                                                                
*                                                                               
ADDIN02  XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,C'M'         TEMP KEY FOR MOVE/COPY RECORDS               
         GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    ADDINX                                                           
         CLI   TLKSES,C'M'                                                      
         BNE   ADDINX                                                           
         GOTOX (RF),TSADEL         DELETE THIS RECORD                           
*                                                                               
         MVI   TLKSES,TLKSKEYC     SET TEMPORARY SHUFFLE KEY                    
         XC    TLUSTAT,TLUSTAT                                                  
         LH    R1,CCOUNT           SET NEW ORDERING FOR LINE COUNT              
         LA    R1,1(R1)                                                         
         STH   R1,CCOUNT                                                        
         STCM  R1,3,TLKSNUM        RESET KEY NUMBER                             
         GOTOX ('TSARIO',AGROUTS),TSAADD                                        
         B     ADDIN02                                                          
*                                                                               
ADDINX   B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO REDISPLAY A SCREENS WORTH OF SUB-ACTION FIELDS           *         
***********************************************************************         
         SPACE 1                                                                
REACT    NTR1  ,                                                                
         TM    LSLTIND1,LSLTISOL   LIST HAS DATA IN IT?                         
         BZ    REACX               NO                                           
*                                                                               
         XC    AFRSTFR,AFRSTFR     RESET FIELD ADDRESSES                        
         XC    AFRSTTO,AFRSTTO                                                  
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   LSLINE#,LSPAG#1     SET CURRENT LINE TO FIRST ON SCREEN          
*                                                                               
REAC02   LH    R2,LSLINE#          CURRENT LINE OF SCREEN                       
         CH    R2,LSPAG#X          FINISHED DRAWING SCREEN YET?                 
         BH    REACX               YES                                          
         SH    R2,LSPAG#1                                                       
         MH    R2,LSLINLEN                                                      
         AH    R2,LS1STLIN                                                      
         STH   R2,LSCURLIN         SET DISPLACEMENT TO CURRENT LINE             
         A     R2,ATWA                                                          
S        USING FHD,R2              R2=A(SUB-ACTION FIELD)                       
*                                                                               
         CLC   TLNUM,LSLINE#       ALREADY HAVE RECORD?                         
         BE    REAC04              YES                                          
         MVC   TLNUM,LSLINE#                                                    
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
*                                                                               
REAC04   OC    TLUSTAT,TLUSTAT     ANY ACTIONS FOR THIS RECORD                  
         BZ    REAC12              NO                                           
*                                                                               
         LA    RF,TLUACTS          ACTION NAME DISPLAY TABLE                    
         USING TLUACTSD,RF                                                      
REAC06   CLI   0(RF),EOT           UNKNOWN FLAG IN TSAR RECORD                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BOBYTE1,TLUSTAT     COPY STATUS FLAG                             
         NC    BOBYTE1,TLUFLAG     IS THIS ACTION REQUEST SET?                  
         BNZ   *+12                YES                                          
         LA    RF,TLULQ(RF)                                                     
         B     REAC06              NEXT KNOWN ACTION                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,TLUNAME                                                     
         A     RE,AOVERWRK         RE=A(NAME OF ACTION)                         
         MVC   S.FHDA(SBLEN),0(RE) MOVE OUT NAME INTO FIELD                     
         OI    S.FHOI,FHOITR       TRANSMIT IT                                  
         OI    S.FHII,FHIIVA       SET FIELD VALID                              
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,TLURPT         GET REPEAT COUNT                             
         BZ    REAC08                                                           
         CLM   R0,1,=AL1(1)        SHOW ONLY IF >1                              
         BNH   REAC08                                                           
*                                                                               
         CVD   R0,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
*                                                                               
         XR    RF,RF               SCOPE NUMBER OF DIGITS TO DISPLAY            
         LA    RE,S.FHDA+SBLEN-1                                                
         CLI   TLURPT,9                                                         
         BNH   *+10                                                             
         LA    RF,1                                                             
         BCTR  RE,0                                                             
         CLI   TLURPT,99                                                        
         BNH   *+10                                                             
         LA    RF,2                                                             
         BCTR  RE,0                                                             
         EX    RF,REACUNPK                                                      
         B     REAC08                                                           
*                                                                               
REACUNPK UNPK  0(0,RE),BODUB1        SET LINE NUMBER IN TSAR RECORD             
*                                                                               
REAC08   TM    TLUSTAT,TLUSCPY+TLUSMVE                                          
         BZ    *+18                                                             
         OC    AFRSTFR,AFRSTFR     SET ADDRESS OF FIRST 'FROM' FIELD            
         BNZ   *+8                                                              
         ST    R2,AFRSTFR                                                       
*                                                                               
         TM    TLUSTAT,TLUSBEF+TLUSAFT                                          
         BZ    *+18                                                             
         OC    AFRSTTO,AFRSTTO     SET ADDRESS OF FIRST 'TO' FIELD              
         BNZ   *+8                                                              
         ST    R2,AFRSTTO                                                       
*                                                                               
         XR    RE,RE                                                            
         IC    RE,S.FHLN                                                        
         LA    R1,S.FHD(RE)        R1=A(FIRST DATA FIELD)                       
FLD      USING FHD,R1                                                           
*                                                                               
         LA    RF,S.FHD            START OF THIS LINE                           
         AH    RF,LSLINLEN         LENGTH OF A LINE                             
         BCTR  RF,0                RF=A(END OF THIS LINE-1)                     
*                                                                               
REAC10   IC    RE,FLD.FHLN         SET INDEX IN RE                              
         OI    FLD.FHII,FHIIVA     SET FIELD VALIDATED                          
         OI    FLD.FHOI,FHOITR     TRANSMIT FIELD                               
         OI    FLD.FHAT,FHATHI     TURN ON HIGHLIGHT                            
         BXLE  R1,RE,REAC10        REPEAT FOR ALL FIELDS ON LINE                
         DROP  RF,S,FLD                                                         
*                                                                               
REAC12   LH    RF,LSLINE#          NEXT LINE ON SCREEN                          
         LA    RF,1(RF)                                                         
         STH   RF,LSLINE#                                                       
         B     REAC02              DO NEXT LINE                                 
*                                                                               
REACX    B     EXITOK              FINISHED                                     
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO INTERFACE WITH TSAR BUFFER 2                             *         
* NTRY: R1 = REQUESTED ACTION                                         *         
*                                                                     *         
* EXIT: CC LOW FOR END OF FILE ERROR                                  *         
*       CC HIGH FOR RECORD NOT FOUND                                  *         
***********************************************************************         
         SPACE 1                                                                
TSAR2    NTR1  ,                                                                
B2       USING TSARD,R3                                                         
*                                                                               
         LA    R3,T2BUFF           SECOND TSAR BUFFER                           
         OI    B2.TSIND2,TSI2BUF2  USING TSAR BUFFER 2                          
*                                                                               
         STCM  R1,1,TS2ACTN        REQUESTED ACTION                             
         LA    R0,T2REC                                                         
         ST    R0,B2.TSAREC        SET A(RECORD)                                
*                                                                               
         TM    LSTSINDS,LSTSIRES   TEST TEMPEST BUFFER RESTORED                 
         BO    TSAR04                                                           
         MVC   B2.TSACOM,ACOM      SET A(COMFACS)                               
         MVI   B2.TSKEYL,T2KEYLQ   SET KEY LENGTH                               
         MVC   B2.TSACOM,ACOM      SET A(COMFACS)                               
         NI    B2.TSRECI,FF-TSRVAR SET FIXED                                    
         MVC   B2.TSRECL,=Y(T2LNQ) SET MAXIMUM RECORD LENGTH                    
         MVI   B2.TSPAGN,TSPEXPN   SET NUMBER OF TEMPEST PAGES                  
         MVI   B2.TSACTN,TSAINI    SET INITIALISE                               
         MVI   B2.TSINDS,TSIALLOC  SET TO ALLOCATE FROM TEMPEST                 
*                                                                               
         CLI   TS2ACTN,TSASAV      WANT TO SAVE BEFORE RESTORING?               
         BE    TSARX                                                            
*                                                                               
         TM    LSTSINDS,LSTSIINI   TEST TEMPEST BUFFER INITIALISED              
         BZ    TSAR02              NO                                           
         MVI   B2.TSACTN,TSARES    SET RESTORE                                  
         MVC   B2.TSPAGL,LSTSLOWP  SET LOW PAGE NUMBER                          
         MVC   B2.TSPAGN,LSTSNUMP  SET NUMBER OF PAGES ALLOCATED                
*                                                                               
TSAR02   GOTOX AGTSAR,B2.TSARD     CALL TO INITIALISE/RESTORE                   
         BNE   TSARDIE             ABEND                                        
*                                                                               
         MVC   LSTSLOWP,B2.TSPAGL  SAVE LOW TSAR PAGE NUMBER                    
         MVC   LSTSNUMP,B2.TSPAGN  SAVE NUMBER OF PAGES ALLOCATED               
         OI    LSTSINDS,LSTSIINI+LSTSIRES                                       
*                                                                               
TSAR04   MVC   B2.TSACTN,TS2ACTN   SET ACTION NUMBER                            
         CLI   B2.TSACTN,TSAINI    EXPLICIT INITIALISE?                         
         BE    TSARX                                                            
         CLI   B2.TSACTN,TSARES    EXPLICIT RESTORE?                            
         BE    TSARX                                                            
         CLI   B2.TSACTN,TSASAV    SAVE?                                        
         BNE   TSAR06              NO                                           
         NI    LSTSINDS,FF-LSTSIRES                                             
         GOTOX AGTSAR,B2.TSARD                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TSAR06   MVC   B2.TSRNUM,T2NUM     SET TSAR NUMBER                              
         GOTOX AGTSAR,B2.TSARD                                                  
         MVC   T2NUM,B2.TSRNUM     SET RECORD LIST NUMBER                       
         BE    TSAR10                                                           
*                                                                               
         CLI   B2.TSACTN,TSAADD    TEST ADDING                                  
         BE    TSAR08                                                           
         CLI   B2.TSACTN,TSARDH    TEST READ-HIGH/NEXT                          
         BE    TSAR08                                                           
         CLI   B2.TSACTN,TSANXT                                                 
         BE    TSAR08                                                           
         DC    H'0'                                                             
*                                                                               
TSAR08   TM    B2.TSERRS,TSEEOF    RETURN CC=LOW FOR END-OF-FILE ERROR          
         BO    EXITL                                                            
         TM    B2.TSERRS,TSERNF    RETURN CC=HIGH IF RECORD NOT FOUND           
         BO    EXITH                                                            
         DC    H'0'                                                             
*                                                                               
TSAR10   DS    0H                  COMPLETELY SUPERFLUOUS LABEL                 
*                                                                               
TSARX    B     EXITOK                                                           
*                                                                               
TSARDIE  LH    R1,GSDSPACT         ABEND IF INITIALISE/RESTORE FAILS            
         A     R1,ATWA                                                          
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GE$ISUTS)                                           
         OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         L     RD,BCSVRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  EXIT TO ROOT CALL POINT                      
         DROP  B2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
***>     DROP  R7                  GET RID OF TWSAVE                            
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
SBLEN    EQU   4                   LENGTH OF A SUB-ACT FIELD                    
HSCRAMT  EQU   10                  HORIZONTAL SCROLL AMOUNT EQUATE              
MAXEQFR  EQU   8                   MAX LENGTH OF AN EQUATE NAME                 
MAXEQTO  EQU   32                  MAX LENGTH OF AN EQUATE VALUE                
*                                                                               
ER#CMDCF EQU   784                 COMMAND CONFLICT                             
ER#MCIP  EQU   785                 MOVE/COPY IN PROGRESS                        
*                                                                               
*                                                                               
TLUACTS  DS    0H              *** TABLE OF TSAR SUB-ACTION NAMES ***           
         DC    AL1(TLUSDEL),AL2(FL@DEL-OVERWRKD)                                
         DC    AL1(TLUSREP),AL2(FL@REPL-OVERWRKD)                               
         DC    AL1(TLUSINS),AL2(FL@INSRT-OVERWRKD)                              
         DC    AL1(TLUSCPY),AL2(FL@COPY-OVERWRKD)                               
         DC    AL1(TLUSMVE),AL2(FL@MOVE-OVERWRKD)                               
         DC    AL1(TLUSAFT),AL2(FL@AFTER-OVERWRKD)                              
         DC    AL1(TLUSBEF),AL2(FL@BFR-OVERWRKD)                                
         DC    AL1(EOT)                                                         
*                                                                               
TLUACTSD DSECT                                                                  
TLUFLAG  DS    XL1                                                              
TLUNAME  DS    AL2                                                              
TLULQ    EQU   *-TLUACTSD                                                       
*                                                                               
COV11    CSECT                                                                  
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL GE#COPY,SBLEN,L     COPY                                         
         DCDDL GE#MOVE,SBLEN,L     MOVE                                         
         DCDDL GE#DEL,SBLEN,L      DELETE                                       
         DCDDL GE#BFR,SBLEN,L      BEFORE                                       
         DCDDL GE#AFTER,SBLEN,L    AFTER                                        
         DCDDL GE#REPL,SBLEN,L     REPLACE (CHANGE THIS TO REPLICATE)           
         DCDDL GE#INSRT,SBLEN,L    INSERT                                       
         DCDDL GE#YES,4,L          YES                                          
         DCDDL GE#NO,4,L           NO                                           
         DCDDL GE#DESC,15,L        DESCRIPTION                                  
         DCDDL GE#LINE,5,L         LINE                                         
         DCDDL GE#HALF,8,L         HALF                                         
         DCDDL GE#PAGE,8,L         PAGE                                         
         DCDDL GE#TITLE,10,L       TITLE                                        
         DCDDL GE#DSP,8,L          DISPLAY (USED FOR DISPLACEMENT)              
*                                                                               
* ROB - IF YOU ADD AN ENTRY TO THIS LIST, IT IS VITAL THAT YOU ADD              
*       CORRESPONDING FU@.... AND FL@.... ENTRIES IN W/S BELOW!!!!              
*                                                                               
DCLISTX  DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
***>     DROP  RB,R6,R5                                                         
         DROP  RB,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DORETURN - INITATE RETURN GLOBBER CALL TO CONTRACT                            
***********************************************************************         
DORETURN NTR1  BASE=*,LABEL=*                                                   
         TM    KFLAGS,KFFROMK      FROM CONTRACT?                               
         BO    *+6                                                              
         DC    H'0'                CAN'T RETURN TO CONTRACT                     
*                                                                               
         LA    R2,SVCOVEL          ORIGINAL COVERSHEET GLOB ELEM                
         USING GLCOVNAM,R2                                                      
*                                                                               
         XC    BOELEM,BOELEM                                                    
         LA    R3,BOELEM                                                        
         USING GLCONNUM,R3                                                      
         GOTOX (RFCONNUM,VREPFACS),BODMCB,(1,GLCOVCON),(6,GLCONNUM)             
         MVC   GLCONCA(3),=C'DIS'                                               
         MVC   GLCONBA(4),=C'HIST'                                              
         DROP  R3                                                               
*                                                                               
         L     RF,ACOM             WRITE RETURN ELEMENT                         
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),BODMCB,=C'PUTD',BOELEM,GLCONLNQ,GLRKACT                     
         CLI   BODMCB+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    BOELEM,BOELEM                                                    
         LA    R3,BOELEM                                                        
         USING GLVXFRSY,R3                                                      
         MVC   GLVXFRSY,=C'REP'                                                 
         MVC   GLVXFRPR,=C'COV'                                                 
         MVC   GLVXTOSY,=C'REP'                                                 
         MVC   GLVXTOPR,=C'CON'                                                 
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS                                       
         GOTO1 (RF),BODMCB,=C'PUTD',BOELEM,24,GLVXCTL                           
         CLI   BODMCB+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RD,BCSVRD           GET ALL THE WAY OUT                          
         XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GETCON - READ SOURCE K INTO IO2                                               
***********************************************************************         
GETCON   NTR1  BASE=*,LABEL=*                                                   
         TM    KFLAGS,KFFROMK                                                   
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,SVCOVEL          ORIGINAL COVERSHEET GLOB ELEM                
         USING GLCOVNAM,R3                                                      
*                                                                               
         LA    R4,IOKEY                                                         
         USING RCONREC,R4                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,CUAALF                                                  
         GOTOX (RFCONNUM,VREPFACS),BODMCB,(1,GLCOVCON),(2,RCONPCON)             
         ICM   R1,15,=AL4(XIO2+XOREPDIR+XORD)                                   
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                FAILED TO READ SOURCE CONTRACT               
         ICM   R1,15,=AL4(XIO2+XOREPFIL+XOGET)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         DROP  R3,R4                                                            
***********************************************************************         
* UPDCON - UPDATE CONTRACT RECORD                                               
***********************************************************************         
UPDCON   NTR1  BASE=*,LABEL=*                                                   
         TM    KFLAGS,KFFROMK                                                   
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R5,R5               INITIALIZE                                   
*                                                                               
         LA    R3,SVCOVEL          ORIGINAL COVERSHEET GLOB ELEM                
         USING GLCOVNAM,R3                                                      
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING RCONREC,R4                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,CUAALF                                                  
         GOTOX (RFCONNUM,VREPFACS),BODMCB,(1,GLCOVCON),(2,RCONPCON)             
         B     UPDCON40                                                         
*                                                                               
UPDCON30 DS    0H                                                               
         LA    R4,IOKEY            NOW USE 0C KEY                               
         MVC   RCONKSTA,0(R2)                                                   
         MVC   RCONKCON,5(R2)                                                   
         CLC   BOFULL1,RCONKCON    ALREADY PROCESSED?                           
         BE    UPDCON45            YES - SKIP                                   
UPDCON40 DS    0H                                                               
         ICM   R1,15,=AL4(XIO2+XOREPDIR+XORDUP)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                FAILED TO READ SOURCE CONTRACT               
*                                                                               
         ICM   R1,15,=AL4(XIO2+XOREPFIL+XOGETRUP)                               
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO2                                                          
         BAS   RE,COVEL            WRITE COVERSHEET LINK ELEM TO K              
         BAS   RE,CONVER           BUMP K VERSION                               
         MVC   IOKEY(L'RCONKEY),RCONKEY RESTORE KEY                             
*                                                                               
         ICM   R1,15,=AL4(XIO2+XOREPFIL+XOPUTREC)                               
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDCON45 DS    0H                                                               
         LTR   R5,R5                                                            
         BZ    UPDCON50                                                         
         LA    R2,9(R2)            NEXT CONTRACT                                
         BCT   R5,UPDCON30                                                      
         B     UPDCONX             ALL DONE                                     
*                                                                               
UPDCON50 DS    0H                                                               
         GOTOX (RFCMBNUM,VREPFACS),BODMCB,AIO2                                  
         CLI   0(R1),1                                                          
         BE    UPDCONX             NOT COMBO                                    
*                                                                               
         ZIC   R5,0(R1)            # OF CONTRACTS IN COMBO                      
         L     R2,0(R1)            A(COMBO ELEM)                                
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOWORK2(0),0(R2)    SAVE 17 ELEM IN BOWORK2                      
         LA    R2,BOWORK2+2        FIRST CONTRACT IN ELEMENT                    
         MVC   BOFULL1,RCONKCON    SAVE 1ST CONTRACT PROCESSED                  
         B     UPDCON30                                                         
*                                                                               
UPDCONX  XIT1                                                                   
         DROP  R3,R4                                                            
***********************************************************************         
* COVEL - ROUTINE TO READ UPDATE COVERSHEET ELEMENT ON CONTRACT                 
*         IOREC = LINKED COVERSHEET RECORD                                      
*         IO2   = CONTRACT RECORD TO UPDATE                                     
***********************************************************************         
COVEL    NTR1                                                                   
         L     R3,AIOREC                                                        
         USING RCOVREC,R3                                                       
         L     R4,AIO2                                                          
*SMY*    USING RCONREC,R4                                                       
         CLI   0(R3),X'49'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'0C'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*SMY*    GOTOX ADELEL,BOPARM,(X'A6',RCONREC),0                                  
         GOTOX ADELEL,BOPARM,(X'A6',(R4)),0                                     
         TM    RCOVCNTL,X'80'                                                   
         BO    COVELX                                                           
*                                                                               
         XC    BOELEM,BOELEM                                                    
         LA    R2,BOELEM                                                        
         USING RCONCVEL,R2                                                      
         MVI   RCONCVEL,X'A6'                                                   
         MVI   RCONCVLN,RCONCVLQ                                                
         MVC   RCONCVNM,RCOVKNAM                                                
         DROP  R2                                                               
*SMY*    GOTOX AADDEL,BODMCB,RCONREC                                            
*                                                                               
         GOTO1 VHELLO,BODMCB,(C'P',GCFILNAM),(R4),BOELEM                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COVELX   XIT1                                                                   
         DROP  R3                                                               
*                                                                               
***********************************************************************         
* CONVER - ROUTINE TO ADVANCE CONTRACT VERSION (WHEN NECESSARY)                 
*          IO2 = CONTRACT RECORD                                                
***********************************************************************         
CONVER   NTR1                                                                   
         TM    KFLAGS,KFFROMK                                                   
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO2                                                          
         USING RCONREC,R2                                                       
         CLI   0(R2),X'0C'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VHELLO,BODMCB,(C'G',GCFILNAM),(X'1F',RCONREC),0                  
         CLI   12(R1),0                                                         
         BNE   CONV020                                                          
*                                                                               
         L     R4,12(R1)           A(1F ELEM)                                   
         USING RCONXEL,R4                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    CONV020                                                          
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREV.                      
         OI    RCONCONF,X'80'      NOT CONFIRMED                                
         DROP  R4                                                               
*                                                                               
CONV020  DS    0H                                                               
         GOTO1 VHELLO,BODMCB,(C'G',GCFILNAM),(X'20',RCONREC),0                  
         CLI   12(R1),0                                                         
         BNE   CONV050                                                          
*                                                                               
         L     R4,12(R1)                                                        
         USING RCONSEND,R4                                                      
*                                                                               
* REP CAN'T MAKE CHANGE IF STATION IS IN MIDDLE OF CHANGES                      
*                                                                               
         TM    RCONSENF,X'10'      X'10'=STA VERS NOT ADVANCED                  
         BO    *+6                 SHOULDN'T HAVE MADE IT THIS FAR              
         DC    H'0'                LATEST STA VERSION NOT YET SENT              
*                                                                               
         TM    RCONSENF,X'20'      REP VERSION NOT ADVANCED                     
         BZ    CONV050                                                          
         DROP  R4                                                               
*                                                                               
* ADVANCE REP VERSION AND SAVE VERSION DATES                                    
*                                                                               
         MVC   BOWORK1(4),VHELLO   SAVE OFF VERSION DATE                        
         MVC   BOWORK1+4(4),VDATCON                                             
         GOTOX (RFGENVER,VREPFACS),BODMCB,(C'R',RCONREC),BOWORK1,      +        
               =XL4'80'                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* DELETE CFC (PCF) COMMENT                                                      
*                                                                               
         GOTOX (RFCONLOW,VREPFACS),BODMCB,RCONREC                               
*                                                                               
         DROP  R2                                                               
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY            BUILD CFC REC KEY                            
         USING RCFCREC,R4                                                       
         MVI   RCFCKTYP,RCFCKTYQ                                                
*                                                                               
*SMY*    MVC   RCFCKREP,RCONKREP                                                
         L     R2,AIO2                                                          
         CLI   0(R2),X'0C'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RCFCKREP,RCONKREP-RCONKEY(R2)                                    
*                                                                               
         MVC   RCFCKCON,0(R1)                                                   
         ICM   R1,15,=AL4(XIO6+XOREPDIR+XORDUP)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BNE   CONV050             NO CFC REC FOUND                             
*                                                                               
         ICM   R1,15,=AL4(XIO6+XOREPFIL+XOGETRUP)                               
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO6                                                          
         OI    RCFCCNTL,X'80'      DELETE RECORD                                
*                                                                               
         ICM   R1,15,=AL4(XIO6+XOREPFIL+XOPUTREC)                               
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    IOKEY+27,X'80'      DELETE KEY                                   
         ICM   R1,15,=AL4(XOREPDIR+XOWRITE)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CONV050  DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO REDISPLAY A SCREENS WORTH OF DATA BASED ON NEW RECORDS   *         
***********************************************************************         
         SPACE 1                                                                
REDRAW   NTR1  BASE=*,LABEL=*                                                   
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         LH    R1,LSPAG#1          SET LIST NO. OF END OF PAGE                  
         AH    R1,LSLINPAG         MAXIMUM NUMBER OF LINES ON A PAGE            
         BCTR  R1,0                                                             
         STH   R1,LSPAG#X          SET LAST NUMBER                              
*                                                                               
         CLC   LSPAG#X,LSLST#X     WILL LIST ALL FIT ON 1 SCREEN?               
         BL    *+10                NO                                           
         MVC   LSPAG#X,LSLST#X     SET END OF PAGE                              
         LH    R1,LSPAG#X                                                       
         SH    R1,LSPAG#1                                                       
         STCM  R1,3,LSNUMPAG       SET ACTUAL NUMBER OF LINES ON PAGE           
*                                                                               
         LH    R1,LS1STLIN         DISPLACEMENT TO FIRST LIST LINE              
         A     R1,ATWA                                                          
         USING FHD,R1                                                           
*                                                                               
         XR    RE,RE                                                            
         LH    RF,LSLINLEN         LENGTH OF A LINE                             
         MH    RF,LSLINPAG         NUMBER OF LINES ON A PAGE                    
         LA    RF,FHD(RF)                                                       
         BCTR  RF,0                RF=A(END OF LIST PORTION-1)                  
*                                                                               
RDRW02   IC    RE,FHLN             SET INDEX IN RE                              
         OI    FHII,FHIIVA         SET FIELD VALIDATED                          
         OI    FHOI,FHOITR         TRANSMIT FIELD                               
         NI    FHAT,FF-FHATHI      TURN OFF HIGHLIGHT                           
         LR    R2,RE                                                            
         SH    R2,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER                        
         BO    *+8                                                              
         LA    R2,FHDAD(R2)                                                     
         EX    R2,*+4                                                           
         MVC   FHDA(0),BCSPACES    SPACE FILL TEXT PORTION                      
*                                                                               
         BXLE  R1,RE,RDRW02        REPEAT FOR ALL FIELDS                        
         DROP  R1                                                               
*                                                                               
         TM    LSLTIND1,LSLTISOL   LIST HAS DATA IN IT?                         
         BZ    RDRWX               NO                                           
*                                                                               
         MVC   LSLINE#,LSPAG#1     SET CURRENT LINE TO FIRST ON SCREEN          
         XC    AINSFLD,AINSFLD                                                  
*                                                                               
RDRW04   LH    R2,LSLINE#          CURRENT LINE OF SCREEN                       
         CH    R2,LSPAG#X          FINISHED DRAWING SCREEN YET?                 
         BH    RDRWX               YES                                          
         SH    R2,LSPAG#1                                                       
         MH    R2,LSLINLEN                                                      
         AH    R2,LS1STLIN                                                      
         STH   R2,LSCURLIN         SET DISPLACEMENT TO CURRENT LINE             
         A     R2,ATWA                                                          
S        USING FHD,R2              R2=A(SUB-ACTION FIELD)                       
*                                                                               
         CLC   TLNUM,LSLINE#       ALREADY HAVE RECORD?                         
         BE    RDRW05              YES                                          
         MVC   TLNUM,LSLINE#                                                    
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
*                                                                               
RDRW05   TM    TLUSTAT,TLUTINS     THIS LINE INSERTED?                          
         BZ    RDRW06              NO                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,S.FHLN                                                        
         LA    RF,S.FHD(RF)                                                     
         OC    AINSFLD,AINSFLD                                                  
         BNZ   *+8                                                              
         ST    RF,AINSFLD                                                       
*                                                                               
         NI    TLUSTAT,FF-TLUTINS                                               
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
RDRW06   LA    R5,LSLIN            CURRENT DISPLAY COLUMNS                      
         USING LINTABD,R5                                                       
*                                                                               
RDRW08   CLI   LINTABD,LINTEOT     DISPLAYED ALL FIELDS?                        
         BE    RDRW16              YES                                          
*                                                                               
         LH    RE,LINHDR           DISPLACEMENT TO HEADER FIELD IN LINE         
         LA    R4,S.FHD(RE)                                                     
FLD      USING FHD,R4              R4=A(FIELD HEADER FOR OUTPUT)                
         TM    LININDS,LINIOPEN    ENSURE OPEN INPUT FIELD IS OPEN              
         BZ    *+8                                                              
         NI    FLD.FHAT,FF-FHATPR                                               
         MVC   FVIHDR,FLD.FHD      SAVE OFF FIELD HEADER                        
*                                                                               
         TM    LININDS,LINIOPEN    OPEN FIELD?                                  
         BZ    RDRW12              NO                                           
*                                                                               
         GOTOX AGEN,BOPARM,ODATA,DDIS,FLD.FHD,AIOREC                            
         B     RDRW14                                                           
*                                                                               
RDRW12   GOTOX AGEN,BOPARM,ODATA,DNDIS,LINFLD#,AIOREC                           
*                                                                               
         LH    R1,LINFLD#          FIELD NUMBER                                 
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,AFDRADDR         INDEX INTO FORMATTED FIELD ELEMENTS          
         L     R1,0(R1)                                                         
         USING FDRELD,R1                                                        
         LH    RE,LINDSP           DISPLACEMENT INTO FIELD                      
         LA    RF,FLD.FHDA(RE)                                                  
         IC    RE,FDRLCLEN         COLUMN WIDTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD      COPY FIELD INTO CLOSED FIELD                 
         DROP  R1                                                               
*                                                                               
RDRW14   LA    R5,LINTABL(R5)      NEXT FIELD                                   
         B     RDRW08                                                           
         DROP  R5,FLD                                                           
*                                                                               
RDRW16   LH    RF,LSLINE#          NEXT LINE ON SCREEN                          
         LA    RF,1(RF)                                                         
         STH   RF,LSLINE#                                                       
         B     RDRW04              DO NEXT LINE                                 
*                                                                               
RDRWX    XC    DOFLAG,DOFLAG                                                    
         XC    DOFRCNT,DOFRCNT                                                  
         XC    DOTOCNT,DOTOCNT                                                  
         XC    DODLCNT,DODLCNT                                                  
         XC    DORPCNT,DORPCNT                                                  
         XC    DOINCNT,DOINCNT                                                  
         XIT1  ,                                                                
*                                                                               
***********************************************************************         
* NEXTREC - GETS NEXT RECORD IN SEQUENCE FOR WRITING                  *         
***********************************************************************         
NEXTREC  NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO4                                                          
         USING RCOVREC,R2                                                       
*                                                                               
         TM    MYIOFLAG,MYIOINIT   HAVE WE BEEN THRU HERE BEFORE?               
         BO    NREC010             YES-SHOULD HAVE A REC                        
         L     R4,AIOREC           0 SEQUENCE REC                               
         MVC   RCOVKEY,0(R4)       SOMETHING TO START WITH                      
         OI    MYIOFLAG,MYIOINIT                                                
*                                                                               
NREC010  DS    0H                                                               
         MVC   IOKEY(L'RCOVKEY),RCOVKEY                                         
         ZIC   R4,RCOVKSEQ                                                      
         LA    R4,1(R4)            NEXT REC IN SEQUENCE                         
         STC   R4,IOKEY+26                                                      
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XORDUPD)                                
         GOTOX ('XIO',AGROUTS)                                                  
         TM    IOERR,IOERNF        NOT FOUND?                                   
         BZ    NREC020                                                          
*                                                                               
***>     MVC   RCOVKEY,IOKEY       CREATE NEW EMPTY RECORD                      
         STC   R4,RCOVKSEQ                                                      
         XC    RCOVLEN(9),RCOVLEN                                               
         MVC   RCOVLEN,=H'35'                                                   
         OI    MYIOFLAG,MYIOADD    ADD REC LATER                                
         B     NREC040                                                          
*                                                                               
NREC020  DS    0H                                                               
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGETRUP+XORDEL)                        
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    NREC030                                                          
         TM    IOERR,IOEDEL        KEY DELETED?                                 
         BO    *+6                                                              
         DC    H'0'                NO OTHER ERRORS ACCEPTABLE                   
         OI    MYIOFLAG,MYIOREST   RESTORE REC LATER                            
*                                                                               
NREC030  DS    0H                                                               
         GOTOX ADELEL,BOPARM,(3,AIO4),0                                         
NREC040  DS    0H                                                               
NRECX    XIT1                                                                   
         DROP  R2                                                               
***********************************************************************         
* WRITEREC - WRITES CURRENT RECORD BACK TO FILE                                 
***********************************************************************         
WRITEREC NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO4             RECORD TO WRITE                              
         USING RCOVREC,R2                                                       
         GOTOX AGETEL,BOPARM,(3,RCOVREC),0                                      
         BE    WREC010             WE HAVE AT LEAST 1 TEXT ELEM                 
*                                                                               
         TM    MYIOFLAG,MYIOADD    NEW RECORD                                   
         BO    WRECX               DON'T NEED TO DELETE IT, JUST LEAVE          
         ZICM  R4,RCOVLEN,2        PUT IN A DUMMY 02 ELEM                       
         LA    R4,2(R4)                                                         
         STCM  R4,3,RCOVLEN                                                     
         MVC   RCOVEL1(2),=X'0302'                                              
         OI    RCOVCNTL,X'80'      FLAG REC FOR DELETE                          
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOPUT)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    IOKEY+27,X'80'      FLAG KEY FOR DELETE                          
         ICM   R1,15,=AL4(XOREPDIR+XOWRITE)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    WRECX                                                            
         DC    H'0'                                                             
*                                                                               
WREC010  DS    0H                                                               
         TM    MYIOFLAG,MYIOADD    ADDING REC?                                  
         BZ    WREC020             NO                                           
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOADDREC)                               
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                DON'T PUT UP WITH ANY DM SHIT                
         NI    MYIOFLAG,X'FF'-MYIOADD                                           
         B     WRECX                                                            
*                                                                               
WREC020  DS    0H                                                               
         TM    MYIOFLAG,MYIOREST   NEED TO RESTORE REC?                         
         BZ    *+8                                                              
         NI    RCOVCNTL,X'FF'-X'80' RESTORE                                     
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOPUT)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                DON'T PUT UP WITH ANY DM SHIT                
*                                                                               
         TM    MYIOFLAG,MYIOREST   NEED TO RESTORE KEY NOW?                     
         BZ    WRECX               NO - ALL DONE                                
         NI    IOKEY+27,X'FF'-X'80' RESTORE KEY                                 
         ICM   R1,15,=AL4(XOREPDIR+XOWRITE)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                DON'T PUT UP WITH ANY DM SHIT                
         NI    MYIOFLAG,X'FF'-MYIOREST                                          
WRECX    XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
MYELDISP DS    H                   DISP TO 03 ELEM IN COVER RECORD              
*                                                                               
MYIOFLAG DS    X                                                                
MYIOREST EQU   X'80'               REC IS DELETED NEED TO RESTORE               
MYIOADD  EQU   X'40'               NEW REC NEED TO ADD                          
MYIOINIT EQU   X'20'               IO SEQUENCE HAS BEEN INITIALIZED             
*                                                                               
AFRSTFR  DS    A                   A(FIRST 'FROM' FIELD)                        
AFRSTTO  DS    A                   A(FIRST 'TO' FIELD)                          
*                                                                               
ACOUNT   DS    A                   A(TOTAL LINE COUNT FIELD)                    
*                                                                               
ASEQFLD  DS    A                   A(SEQUENCE NUMBER FIELD)                     
AINSFLD  DS    A                   A(CURSOR) FOR INSERTED FIELD                 
*                                                                               
TLMSVE   DS    XL2                 SAVED TSAR NUMBER                            
TLMSTAT  DS    XL1                 SAVED TSAR STATUS BYTE                       
TLMAP    DS    XL2                 SAVED TSAR POINTER                           
*                                                                               
DSLISTU  DS    0D                                                               
FU@COPY  DS    CL(SBLEN)                                                        
FU@MOVE  DS    CL(SBLEN)                                                        
FU@DEL   DS    CL(SBLEN)                                                        
FU@BFR   DS    CL(SBLEN)                                                        
FU@AFTER DS    CL(SBLEN)                                                        
FU@REPL  DS    CL(SBLEN)                                                        
FU@INSRT DS    CL(SBLEN)                                                        
FU@YES   DS    CL4                                                              
FU@NO    DS    CL4                                                              
FU@DESC  DS    CL15                                                             
FU@LINE  DS    CL5                                                              
FU@HALF  DS    CL8                                                              
FU@PAGE  DS    CL8                                                              
FU@TITLE DS    CL10                                                             
FU@DSP   DS    CL8                                                              
*                                                                               
DSLISTL  DS    0D                                                               
FL@COPY  DS    CL(SBLEN)                                                        
FL@MOVE  DS    CL(SBLEN)                                                        
FL@DEL   DS    CL(SBLEN)                                                        
FL@BFR   DS    CL(SBLEN)                                                        
FL@AFTER DS    CL(SBLEN)                                                        
FL@REPL  DS    CL(SBLEN)                                                        
FL@INSRT DS    CL(SBLEN)                                                        
FL@YES   DS    CL4                                                              
FL@NO    DS    CL4                                                              
FL@DESC  DS    CL15                                                             
FL@LINE  DS    CL5                                                              
FL@HALF  DS    CL8                                                              
FL@PAGE  DS    CL8                                                              
FL@TITLE DS    CL10                                                             
FL@DSP   DS    CL8                                                              
*                                                                               
T2BUFF   DS    XL(TSARDL)          BUFFER FOR SECOND TSAR BUFFER                
*                                                                               
         DS    0A                                                               
T2LST    DS    0XL73                                                            
T2NUM    DS    XL2                 RECORD NUMBER                                
T2REC    DS    0XL71                                                            
T2KEY    DS    XL2                 LINE NUMBER IS KEY                           
T2KEYLQ  EQU   *-T2KEY                                                          
T2LLEN   DS    XL1                 LINE LENGTH                                  
T2LINE   DS    XL68                DATA LENGTH                                  
T2LNQ    EQU   *-T2LST             LENGTH OF TSAR RECORD                        
*                                                                               
TS2ACTN  DS    XL1                 REQUESTED ACTION                             
T2HINUM  DS    H                                                                
*                                                                               
       ++INCLUDE RECOVWORK                                                      
*                                                                               
*                                                                               
       ++INCLUDE CTDDEQUS                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'253RECOV11   08/10/11'                                      
         END                                                                    
