*          DATA SET REPRO14S   AT LEVEL 005 AS OF 09/04/96                      
*&&      SET   NOP=N                                                            
*PHASE T80A14B                                                                  
*                                                                               
T80A14   TITLE 'REPRO14 - PROPOSAL RECORD(LIST/OVERLAY)'                        
PRO14    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REPRO14*,R7,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
*                                                                               
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
*                                                                               
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)                        
*                          - EXPECTS R1 TO HOLD VERB                            
***********************************************************************         
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
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                            
*                                                                               
* P1 HOLDS EQUATED VERB                                                         
***********************************************************************         
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECRD)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFKEY)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(SUBACT)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                                
***********************************************************************         
INIT     DS    0H                                                               
         OI    TWASRVH+1,X'01'     SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    TWASRVH+6,X'80'                                                  
         NI    GSINDSL1,X'FF'-GSINOIO    TURN OFF OUR IO'S                      
         OI    GSINDSL1,GSIXKEY    NO ENTER KEY MESG                            
*        OI    LSSTAT1,LSSTSAR     TSAR ONLY LIST                               
*                                                                               
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
KFTABL   DC    AL1(KDIS),AL1(0,0,0),AL4(KFKDIS)      DISPLAY                    
         DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE DISPLAYING THE KEY FIELDS                                              
***********************************************************************         
KFKDIS   DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* BEFORE VALIDATING THE KEY FIELDS                                              
***********************************************************************         
KFKVAL   DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1KYCHG   DON'T KNOW IF KEY CHANGED YET          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* AFTER WE'VE DONE EVERYTHING TO THE KEY FIELDS ON THE SCREEN                   
***********************************************************************         
KEYLAST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KLTABL   DC    AL1(KDIS),AL1(0,0,0),AL4(KLKDIS)      DISPLAY                    
         DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KLFVAL)     VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER DISPLAYING THE KEY FIELDS                                               
***********************************************************************         
KLKDIS   DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* AFTER VALIDATING THE KEY FIELDS                                               
***********************************************************************         
KLKVAL   DS    0H                                                               
         USING RPROKEY,R2                                                       
         MVI   RPROPTYP,RPROPTYQ                                                
         MVI   RPROKSTY,RPROPSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
         DROP  R2                                                               
*                                                                               
KLKVX    B     EXITOK                                                           
***********************************************************************         
* AFTER VALIDATING THE KEY FILTER FIELDS                                        
***********************************************************************         
KLFVAL   DS    0H                                                               
         USING RPROKEY,R2                                                       
         XC    RPROKEY,RPROKEY                                                  
         MVI   RPROPTYP,RPROPTYQ                                                
         MVI   RPROPSTY,RPROPSBQ                                                
         MVC   RPROPRCD,CUAALF                                                  
         MVC   RPROPSAL,LKSAL                                                   
         MVC   RPROPSTA,LKSTA                                                   
         DROP  R2                                                               
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
RECRD    LM    R0,R2,SVPARMS                                                    
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
RFTABL   DS    0H                                                               
         DC    AL1(EOT)                                                         
*        DC    AL1(RADD),AL1(0,0,0),AL4(RFRADD)      ADD                        
*        DC    AL1(RWRT),AL1(0,0,0),AL4(RFRWRT)      WRITE                      
*        DC    AL1(RDEL),AL1(0,0,0),AL4(RFRDEL)      DELETE                     
*        DC    AL1(RRES),AL1(0,0,0),AL4(RFRRES)      RESTORE                    
*                                                                               
         EJECT                                                                  
***********************************************************************         
* AFTER THE CONTROLLER CALLS THE I/O ACTION                                     
***********************************************************************         
RECLAST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RLTABL   DS    0H                                                               
         DC    AL1(EOT)                                                         
*        DC    AL1(RADD),AL1(0,0,0),AL4(RLRADD)      ADD                        
*        DC    AL1(RWRT),AL1(0,0,0),AL4(RLRWRT)      WRITE                      
*        DC    AL1(RDEL),AL1(0,0,0),AL4(RLRDEL)      DELETE                     
*        DC    AL1(RRES),AL1(0,0,0),AL4(RLRRES)      RESTORE                    
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECTS FOR KEY DATA OR RECORD DATA                                      
*                                                                               
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                            
* P2 HOLDS EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA ACTION                   
* P3 BYTE  0   HOLDS EQUATED DATA VERB IF P2 IS ZERO                            
* P3 BYTES 1-3 HOLDS EQUATED ACTION VERB                                        
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                           
*                                                                               
* FIELD DATA IS EXTRACTED/OUTPUT INTO FVIFLD                                    
***********************************************************************         
DATA     ICM   R1,15,SVPARMS2      DOING ACTION ON SPECIFIC DATA OBJ?           
         BNZ   DATA10              YES                                          
***********************************************************************         
************** DOING A GLOBAL ACTION ON ENTIRE RECORD *****************         
***********************************************************************         
         L     R2,SVPARMS4         R2 = A(RECORD)                               
         SR    R1,R1                                                            
         IC    R1,SVPARMS3         R1 = GLOBAL ACTION                           
         LA    RF,DTATABL          TABLE OF GLOBAL VERBS                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE WE DO ANYTHING TO THE DATA FIELDS ON THE SCREEN                        
***********************************************************************         
DTAFRST  DS    0H                                                               
         L     R1,SVPARMS3         VERB                                         
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE DISPLAYING THE DATA FIELDS                                             
***********************************************************************         
DFDDIS   DS    0H                                                               
         TM    MISCFLG1,MF1PFRET   RETURNING FROM CALLED SESSION?               
         BNZ   EXITL               SO WE DON'T HAVE TO RE-DISPLAY               
*                                                                               
         B     EXITOK                                                           
***********************************************************************         
* BEFORE VALIDATING THE DATA FIELDS                                             
***********************************************************************         
DFDVAL   DS    0H                                                               
*                                                                               
DFDVALX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* AFTER WE'VE DONE EVERYTHING TO THE DATA FIELDS ON THE SCREEN                  
***********************************************************************         
DTALAST  DS    0H                                                               
         L     R1,SVPARMS3         VERB IN R1                                   
         LA    RF,DLTABL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DLTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DLDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* AFTER DISPLAYING THE DATA FIELDS                                              
***********************************************************************         
DLDDIS   DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* AFTER VALIDATING THE DATA FIELDS                                              
***********************************************************************         
DLDVAL   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
************ DOING AN ACTION ON A SPECIFIC DATA OBJECT ****************         
***********************************************************************         
DATA10   DS    0H                                                               
         LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA20   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA30              YES                                          
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA20                                                           
***********************************                                             
* WE KNOW OF THIS DATA OBJECT                                                   
***********************************                                             
DATA30   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         DROP  RF                                                               
*                                                                               
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING FRRRECD,R2          R2 HOLDS A(RECORD)                           
         L     R3,AFRREL                                                        
         USING FRRELD,R3           R3=A(FRREL ON RECORD)                        
         BR    RF                                                               
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
KNOWTAB  DS    0XL(KNOWLQ)                                                      
* FILTERS FOR LIST SCREEN                                                       
         DC    AL2(00023),AL4(SALDTA)    SALESPERSON                            
         DC    AL2(00024),AL4(STADTA)    STATION                                
         DC    AL2(00025),AL4(AGYDTA)    AGENCY                                 
         DC    AL2(00026),AL4(ADVDTA)    ADVERTISER                             
         DC    AL2(00027),AL4(FLTDTA)    FLIGHT DATES                           
         DC    AL2(00043),AL4(DVSDTA)    DEVELOPMENT SALESPERSON                
         DC    AL2(00044),AL4(DVTDTA)    DEVELOPMENT CONRACT TYPE               
         DC    AL2(00028),AL4(CONDTA)    CONTRACT                               
* LIST FIELDS                                                                   
         DC    AL2(00030),AL4(LCONDTA)   LIST CONTRACT #                        
         DC    AL2(00031),AL4(LPRODTA)   LIST PROPOSAL #                        
         DC    AL2(00032),AL4(LAGYDTA)   LIST AGENCY                            
         DC    AL2(00033),AL4(LADVDTA)   LIST ADVERTISER                        
         DC    AL2(00034),AL4(LPRDDTA)   LIST PRODUCT                           
         DC    AL2(00035),AL4(LSALDTA)   LIST SALESPERSON                       
         DC    AL2(00036),AL4(LBUYDTA)   LIST BUYER                             
         DC    AL2(00037),AL4(LSTADTA)   LIST STATION                           
         DC    AL2(00038),AL4(LFLTDTA)   LIST FLIGHT                            
         DC    AL2(00041),AL4(LDVSDTA)   LIST DEVELOPMENT SALESPERSON           
         DC    AL2(00042),AL4(LDVTDTA)   LIST DEVELOPMENT CONRACT TYPE          
*                                                                               
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
PRO14    CSECT                                                                  
         EJECT                                                                  
*          DATA SET REPRO11    AT LEVEL 037 AS OF 12/13/95                      
***********************************************************************         
* DATA OBJECT FOR CONTRACT NUMBER                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
CONDTA   DS    0H                                                               
         LA    RF,CONTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CONTBL   DC    AL1(DFVAL),AL1(0,0,0),AL4(FLTCON)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFCON)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE CONTRACT FILTER                                                      
***********************************************************************         
FLTCON   DS    0H                                                               
         XC    FLTIFLD,FLTIFLD                                                  
         CLI   FVILEN,0                                                         
         BE    FLTCONX                                                          
*                                                                               
         TM    FVIIND,FVINUM       VALID NUMERIC?                               
         BZ    EXITNOTN            NO, ERROR                                    
*                                                                               
         ZIC   R1,FVXLEN           PACK THE CONTRACT NUMBER                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,FVIFLD(0)                                                 
         CVB   R0,BODUB1                                                        
*                                                                               
         ZAP   BOWORK1+10(5),=P'99999999'                                       
         SP    BOWORK1+10(5),BODUB1+3(5)                                        
         MVO   BOWORK1(5),BOWORK1+10(5) CONTRACT NUM IN PWOS                    
         MVC   FLTIFLD(4),BOWORK1                                               
*                                                                               
FLTCONX  B     EXITOK                                                           
***********************************************************************         
* DO CONTRACT FILTERING                                                         
***********************************************************************         
DOFCON   DS    0H                                                               
         USING RPROPTYP,R2                                                      
*                                                                               
         CLC   RPROPCON,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR AGENCY                                                        
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
AGYDTA   DS    0H                                                               
         LA    RF,AGYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
AGYTBL   DC    AL1(DFVAL),AL1(0,0,0),AL4(FLTAGY)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFAGY)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE AGENCY FILTER                                                        
***********************************************************************         
FLTAGY   DS    0H                                                               
         XC    FLTIFLD,FLTIFLD                                                  
         CLI   FVILEN,0            ANY INPUT?                                   
         BE    EXITOK                                                           
*                                                                               
         CLI   FVILEN,L'RAGYKAGY+1+L'RAGYKAOF   TOO LONG?                       
         BH    INVAGYX             YES                                          
*                                                                               
         NI    MISCFLG1,X'FF'-MF1TMPBT   SET AGENCY OFFICE NOT GIVEN            
         LA    RE,FVIFLD                                                        
FLTAGY5  CLI   0(RE),C' '                                                       
         BE    FLTAGY10            AGENCY ONLY                                  
         CLI   0(RE),C'-'                                                       
         BE    FLTAGY8                                                          
         LA    RE,1(RE)                                                         
         B     FLTAGY5                                                          
*                                                                               
FLTAGY8  OI    MISCFLG1,MF1TMPBT   SET AGENCY OFFICE GIVEN                      
*                                                                               
FLTAGY10 LA    R1,1(RE)            AGENCY POSITION FOR LATER                    
         LA    RF,FVIFLD                                                        
         SR    RE,RF               DATA LENGTH                                  
         STC   RE,BOBYTE1                                                       
         CLI   BOBYTE1,L'RAGYKAGY  TOO LONG?                                    
         BH    INVAGYX             YES                                          
         TM    MISCFLG1,MF1TMPBT   AGENCY OFFICE GIVEN?                         
         BZ    FLTAGY16            NO                                           
*                                                                               
         LR    RE,R1               CHECK AGENCY OFFICE                          
         CLC   0(L'RAGYKAOF,RE),BCSPACES                                        
         BE    INVAGYX                                                          
*                                                                               
FLTAGY12 CLI   0(RE),C' '                                                       
         BE    FLTAGY14                                                         
         LA    RE,1(RE)                                                         
         B     FLTAGY12                                                         
*                                                                               
FLTAGY14 DS    0H                                                               
         SR    RE,R1               DATA LENGTH                                  
         STC   RE,BOBYTE2                                                       
         CLI   BOBYTE2,L'RAGYKAOF  TOO LONG?                                    
         BH    INVAGYX             YES                                          
         B     FLTAGY20            NO                                           
*                                                                               
FLTAGY16 CLC   BOBYTE1,FVILEN      EXTRA GARBAGE?                               
         BNE   INVAGYX             YES                                          
         B     FLTAGY20                                                         
*                                                                               
FLTAGY20 OC    FVIFLD(L'RAGYKAGY+1+L'RAGYKAOF),BCSPACES                         
         LA    RE,IOKEY                                                         
         USING RAGYKEY,RE                                                       
         XC    RAGYKEY,RAGYKEY                                                  
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY,BCSPACES                                                
         ZIC   RF,BOBYTE1                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RAGYKAGY(0),FVIFLD                                               
         MVC   RAGYKAOF,0(R1)                                                   
         MVC   RAGYKREP,CUAALF                                                  
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RAGYKEY),IOKEYSAV                                        
         BNE   INVAGYX             AGENCY NOT ON RECORD                         
*                                                                               
         LA    RE,IOKEY                                                         
         USING RAGYKEY,RE                                                       
         MVC   FLTIFLD(L'RAGYKAGY),RAGYKAGY                                     
         MVC   FLTIFLD+L'RAGYKAGY(L'RAGYKAOF),RAGYKAOF                          
         DROP  RE                                                               
*                                                                               
FLTAGYX  B     EXITOK                                                           
*                                                                               
INVAGYX  MVC   FVMSGNO,=AL2(INVAGY)    EXIT WITH INVALID AGENCY                 
         B     EXITL                                                            
***********************************************************************         
* DO AGENCY FILTERING                                                           
***********************************************************************         
DOFAGY   DS    0H                                                               
         USING RPRSWELD,R2                                                      
         CLC   RPRSWAGY,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    DOFAGY5             AGENCY OK                                    
         BH    FLTXH                                                            
*                                                                               
DOFAGY5  CLC   FLTIFLD+L'RPRSWAGY(L'RPRSWAOF),BCSPACES                          
         BE    FLTXE                                                            
*                                                                               
         CLC   RPRSWAOF,FLTIFLD+L'RPRSWAGY                                      
         BL    FLTXL                                                            
         BE    FLTXE               AGENCY OFFICE OK                             
         BH    FLTXH                                                            
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADVERTISER                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
ADVDTA   DS    0H                                                               
         LA    RF,ADVTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
ADVTBL   DC    AL1(DFVAL),AL1(0,0,0),AL4(FLTADV)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFADV)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE ADVERTISER FILTER                                                    
***********************************************************************         
FLTADV   DS    0H                                                               
         CLI   FVILEN,L'RADVKADV   MAX CODE LENGTH                              
         BH    INVADVX                                                          
*                                                                               
         XC    FLTIFLD,FLTIFLD                                                  
         CLI   FVILEN,0                                                         
         BE    FLTAGYX                                                          
*                                                                               
         OC    FVIFLD(L'RADVKADV),SPACES     UPPERCASE                          
         LA    RE,IOKEY                                                         
         USING RADVKEY,RE                                                       
         XC    RADVKEY,RADVKEY                                                  
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,FVIFLD                                                  
         MVC   RADVKREP,CUAALF                                                  
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RAGYKEY),IOKEYSAV                                        
         BNE   INVADVX             SALESPERSON NOT ON RECORD                    
*                                                                               
         MVC   FLTIFLD(L'RAGYKAGY),FVIFLD                                       
FLTADVX  B     EXITOK                                                           
*                                                                               
INVADVX  MVC   FVMSGNO,=AL2(INVADV) EXIT WITH INVALID ADVERTISER                
         B     EXITL                                                            
***********************************************************************         
* DO ADVERTISER FILTERING                                                       
***********************************************************************         
DOFADV   DS    0H                                                               
         USING RPRSWELD,R2                                                      
         CLC   RPRSWADV,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SALESPERSON                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SALDTA   DS    0H                                                               
         LA    RF,SALTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SALTBL   DC    AL1(DFVAL),AL1(0,0,0),AL4(FLTSAL)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFSAL)                                 
         DC    AL1(DDFLTF),AL1(0,0,0),AL4(DEFSAL)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* SET DEFAULT STATION FILTER                                                    
***********************************************************************         
DEFSAL   DS    0H                                                               
         CLI   TWAACCS,C'$'                                                     
         BNE   EXITNO                                                           
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE SALESPERSON FILTER                                                   
***********************************************************************         
FLTSAL   DS    0H                                                               
         XC    FLTIFLD,FLTIFLD                                                  
         CLI   FVILEN,L'RSALKSAL   MAX CODE LENGTH                              
         BH    INVSALX                                                          
*                                                                               
         OC    FVIFLD(L'RSALKSAL),SPACES     UPPERCASE                          
         LA    RE,IOKEY                                                         
         USING RSALKEY,RE                                                       
         XC    IOKEY(L'RSALKEY),IOKEY                                           
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,CUAALF                                                  
         MVC   RSALKSAL,FVIFLD                                                  
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RSALKEY),IOKEYSAV                                        
         BNE   INVSALX             SALESPERSON NOT ON RECORD                    
*                                                                               
*                                                                               
         L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO4                                                          
         LA    R6,RSALELEM-RSALREC(R6)                                          
         MVI   ELCODE,X'01'        SALESPERSON ELEMENT                          
         BAS   RE,FIRSTEL                                                       
         BNE   FLTSALX                                                          
         USING RSALELEM,R6                                                      
         MVC   LKSAL,FVIFLD                                                     
         MVC   LKTEAM,RSALTEAM     GET THE TEAM                                 
         MVC   LKOFF,RSALOFF       GET THE TEAM                                 
*                                                                               
         CLI   ASONOFF,ASOFF       RUNNING OFFLINE?                             
         BE    FLTSALX                                                          
         TM    CUSTAT,CUSDDS       DDS TERMINAL?                                
         BNZ   FLTSALX                                                          
         CLC   =C'O=',CUACCS       TEST FOR OFFICE RESTRICTION                  
         BNE   FLTSALX                                                          
         TM    CUAUTH,X'80'        TEST IF TERMINAL ALLOWED ACCESS              
         BO    FLTSALX             TO ALL OFFICES                               
         CLC   LKOFF,CUACCS+2      ELSE,COMPARE OFFICES                         
         BNE   INVSALX                                                          
         DROP  R6                                                               
*                                                                               
FLTSALX  DS    0H                                                               
         MVC   FLTIFLD(L'RCONSAL),FVIFLD                                        
         B     EXITOK                                                           
*                                                                               
INVSALX  MVC   FVMSGNO,=AL2(INVSALP) EXIT WITH INVALID SALESPERSON              
         B     EXITL                                                            
***********************************************************************         
* DO SALSPERSON FILTERING                                                       
***********************************************************************         
DOFSAL   DS    0H                                                               
         USING RPROKEY,R2                                                       
         CLC   RPROPSAL,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR STATION                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
STADTA   DS    0H                                                               
         LA    RF,STATBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
STATBL   DC    AL1(DFVAL),AL1(0,0,0),AL4(FLTSTA)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFSTA)                                 
         DC    AL1(DDFLTF),AL1(0,0,0),AL4(DEFSTA)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* SET DEFAULT STATION FILTER                                                    
***********************************************************************         
DEFSTA   DS    0H                                                               
         CLI   TWAACCS,C'$'                                                     
         BE    EXITNO                                                           
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE STATION FILTER                                                       
***********************************************************************         
FLTSTA   DS    0H                                                               
         XC    LKSTA,LKSTA                                                      
         CLI   FVILEN,L'RSTAKSTA   MAX CODE LENGTH                              
         BH    INVSTAX                                                          
*                                                                               
         XC    FLTIFLD,FLTIFLD                                                  
*                                                                               
         OC    FVIFLD(L'RSALKSAL),SPACES     UPPERCASE                          
         LA    RE,IOKEY                                                         
         USING RSTAKEY,RE                                                       
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,CUAALF                                                  
         MVC   RSTAKSTA,FVIFLD                                                  
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RSTAKEY),IOKEYSAV                                        
         BNE   INVSTAX             SALESPERSON NOT ON RECORD                    
*                                                                               
         MVC   FLTIFLD(L'RSTAKSTA),FVIFLD                                       
         MVC   LKSTA,FVIFLD                                                     
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   FLTSTAX                                                          
*                                                                               
         L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO4             CK STATION AUTH TO SEE CONTRACT              
         LA    R6,RSTAELEM-RSTARECD(R6)                                         
         USING RSTASOEL,R6         RSTASID IS VALID SIGN-ON FOR THIS            
         XR    RF,RF                                                            
FLTSTA10 CLI   0(R6),0                                                          
         BE    EXITSLCK            ERROR, SECURITY LOCKOUT                      
*                                                                               
         CLI   0(R6),X'06'                                                      
         BE    *+16                                                             
FLTSTA15 IC    RF,1(R6)                                                         
         LA    R6,0(RF,R6)                                                      
         B     FLTSTA10                                                         
*                                                                               
         CLC   CUUSER,RSTASID    STATION'S CONTRACTS                            
         BNE   FLTSTA15                                                         
         DROP  R6                                                               
*                                                                               
FLTSTAX  B     EXITOK                                                           
*                                                                               
INVSTAX  MVC   FVMSGNO,=AL2(INVSTA) EXIT WITH INVALID STATION                   
         B     EXITL                                                            
***********************************************************************         
* DO STATION FILTERING                                                          
***********************************************************************         
DOFSTA   DS    0H                                                               
         USING RPROKEY,R2                                                       
         CLC   RPROPSTA,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FLIGHT DATES                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
FLTDTA   DS    0H                                                               
         LA    RF,FLTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FLTTBL   DC    AL1(DFVAL),AL1(0,0,0),AL4(FLTFLT)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFFLT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE FLIGHT FILTER                                                        
***********************************************************************         
FLTFLT   DS    0H                                                               
         XC    FLTIFLD,FLTIFLD                                                  
         CLI   FVILEN,0                                                         
         BE    FLTFLTX                                                          
*                                                                               
         GOTO1 VPERVAL,BODMCB,(FVILEN,FVIFLD),(0,BOWORK1)                       
         CLI   4(R1),0             EVERYTHING OK?                               
         BNE   EXITNV              NO                                           
*                                                                               
         MVC   FLTIFLD(L'RCONDATE),BOWORK1+28                                   
FLTFLTX  B     EXITOK                                                           
***********************************************************************         
* DO FLIGHT FILTERING                                                           
***********************************************************************         
DOFFLT   DS    0H                                                               
         USING RPRSWELD,R2                                                      
*                                                                               
         CLC   RPRSWFLT(3),FLTIFLD          START/START.                        
         BL    DOFFLT10            STARTS BEFORE THE FILTER                     
         BE    FLTXE               START IS THE SAME                            
*                                  STARTS AFTER THE FILTER                      
*                                                                               
DOFFLT5  CLC   RPRSWFLT(3),FLTIFLD+3        START/END                           
         BH    FLTXX               STARTS AFTER END OF FILTER                   
         BE    FLTXE               START IS THE END OF FILTER                   
         BL    FLTXE               STARTS DURING THE FILTER                     
*                                                                               
DOFFLT10 CLC   RPRSWFLT+3(3),FLTIFLD        END/START                           
         BL    FLTXX               ENDS BEFORE THE FILTER                       
         BE    FLTXE               END IS THE START OF FILTER                   
         BH    FLTXE               ENDS AFTER THE START OF FILTER               
*                                                                               
         DC    H'0'                DIE                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEVELOPMENT SALESPERSON FILTER                                
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DVSDTA   DS    0H                                                               
         LA    RF,DVSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DVSTBL   DC    AL1(DFVAL),AL1(0,0,0),AL4(FLTDVS)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFDVS)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE LIST DEVELOPMENT SALESPERSON FILTER                                  
***********************************************************************         
FLTDVS   DS    0H                                                               
         XC    FLTIFLD,FLTIFLD                                                  
         CLI   FVILEN,0                                                         
         BE    FLTDVSX                                                          
*                                                                               
         CLI   FVILEN,L'RDSPKSAL   TOO LONG?                                    
         BH    INVDVSX             YES                                          
*                                                                               
         OC    FVIFLD(L'RDSPKSAL),BCSPACES   UPPERCASE                          
         LA    RE,IOKEY                                                         
         USING RDSPKEY,RE                                                       
         XC    RDSPKEY,RDSPKEY                                                  
         MVI   RDSPKTYP,X'3A'                                                   
         MVC   RDSPKSAL,FVIFLD                                                  
         MVC   RDSPKREP,CUAALF                                                  
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RDSPKEY),IOKEYSAV                                        
         BNE   INVDVSX             DEV. SALESPERSON NOT ON RECORD               
*                                                                               
         MVC   FLTIFLD(L'RCONDVSP),FVIFLD                                       
*                                                                               
FLTDVSX B      EXITOK                                                           
*                                                                               
INVDVSX  MVC   FVMSGNO,=AL2(INVDVSAL) EXIT WITH INVALID DEV SALESPERSON         
         B     EXITL                                                            
***********************************************************************         
* DO DEVELOPMENT SALESPERSON FILTERING                                          
***********************************************************************         
DOFDVS   DS    0H                                                               
*                                                                               
         USING RPRSWELD,R2                                                      
         CLC   RPRSWDSP,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEVELOPMENT TYPE FILTER                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DVTDTA   DS    0H                                                               
         LA    RF,DVTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DVTTBL   DC    AL1(DFVAL),AL1(0,0,0),AL4(FLTDVT)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFDVT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE DEVELOPMENT CONTRACT TYPE FILTER                                     
***********************************************************************         
FLTDVT   DS    0H                                                               
         XC    FLTIFLD,FLTIFLD                                                  
         CLI   FVILEN,0                                                         
         BE    FLTDVTX                                                          
*                                                                               
         CLI   FVILEN,L'RDCTKCTY   TOO LONG?                                    
         BH    INVDVTX             YES                                          
*                                                                               
         OC    FVIFLD(L'RDCTKCTY),BCSPACES   UPPERCASE                          
         LA    RE,IOKEY                                                         
         USING RDCTKEY,RE                                                       
         XC    RDCTKEY,RDCTKEY                                                  
         MVI   RDCTKTYP,X'3B'                                                   
         MVC   RDCTKCTY,FVIFLD                                                  
         MVC   RDCTKREP,CUAALF                                                  
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RDCTKEY),IOKEYSAV                                        
         BNE   INVDVTX             DEV. CONTRACT TYPE NOT ON RECORD             
*                                                                               
         MVC   FLTIFLD(L'RCONDVCT),FVIFLD                                       
*                                                                               
FLTDVTX B      EXITOK                                                           
*                                                                               
INVDVTX  MVC   FVMSGNO,=AL2(INVDVTYP) EXIT WITH INVALID DEV CON TYPE            
         B     EXITL                                                            
***********************************************************************         
* DO DEVELOPMENT CONTRACT TYPE FILTERING                                        
***********************************************************************         
DOFDVT   DS    0H                                                               
*                                                                               
         USING RPRSWELD,R2                                                      
         CLC   RPRSWDCT,FLTIFLD                                                 
         BL    FLTXX                                                            
         BE    FLTXE                                                            
         BH    FLTXX                                                            
         DROP  R2                                                               
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST CONTRACT NUMBER                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LCONDTA  DS    0H                                                               
         LA    RF,LCONTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LCONTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLCON)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A LIST CONTRACT FIELD                                                 
***********************************************************************         
DISLCON  DS    0H                                                               
         USING TLSTD,R2                                                         
         ZAP   BOWORK1+20(5),=P'0'        EDIT USES 17 BYTES OF WORK            
         MVO   BOWORK1+20(5),LDCONNUM                                           
         EDIT  (P5,BOWORK1+20),(8,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,      X        
               DUB=BODUB1                                                       
         DROP  R2                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST PROPOSAL NUMBER                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LPRODTA  DS    0H                                                               
         LA    RF,LPROTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LPROTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLPRO)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A LIST CONTRACT FIELD                                                 
***********************************************************************         
DISLPRO  DS    0H                                                               
*                                                                               
         USING RPROKEY,R2                                                       
         ZIC   RE,RPROKPRO                                                      
         LA    R0,X'FF'                                                         
         SR    R0,RE                                                            
         EDIT  (R0),(3,FVIFLD),WRK=BOWORK1,DUB=BODUB1                           
         DROP  R2                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST AGENCY                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LAGYDTA  DS    0H                                                               
         LA    RF,LAGYTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LAGYTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLAGY)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A LIST AGENCY FIELD                                                   
***********************************************************************         
DISLAGY  DS    0H                                                               
*                                                                               
         USING TLSTD,R2                                                         
         MVC   FVIFLD(L'LDAGY),LDAGY                                            
         CLC   LDAOF,SPACES                                                     
         BE    DISLAGYX                                                         
*                                                                               
         LA    RE,FVIFLD+L'RCONKAGY                                             
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNH   *-6                                                              
         MVI   1(RE),C'-'                                                       
         LA    RE,2(RE)                                                         
         MVC   0(L'RCONKAOF,RE),LDAOF                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
DISLAGYX B    EXITOK                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST ADVERTISER                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LADVDTA  DS    0H                                                               
         LA    RF,LADVTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LADVTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLADV)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A LIST ADVERTISER FIELD                                               
***********************************************************************         
DISLADV  DS    0H                                                               
*                                                                               
         USING TLSTD,R2                                                         
         MVC   FVIFLD(L'LDADV),LDADV                                            
         DROP  R2                                                               
*                                                                               
DISLADVX B    EXITOK                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST PRODUCT                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LPRDDTA  DS    0H                                                               
         LA    RF,LPRDTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LPRDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLPRD)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A PRODUCT LIST FIELD                                                  
***********************************************************************         
DISLPRD  DS    0H                                                               
*                                                                               
         USING TLSTD,R2                                                         
         MVC   FVIFLD(L'LDPRD),LDPRD                                            
         DROP  R2                                                               
*                                                                               
DISLPRDX B    EXITOK                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST SALESPERSON                                              
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LSALDTA  DS    0H                                                               
         LA    RF,LSALTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LSALTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLSAL)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A LIST SALESPERSON FIELD                                              
***********************************************************************         
DISLSAL  DS    0H                                                               
*                                                                               
         USING TLSTD,R2                                                         
         MVC   FVIFLD(L'LDSALP),LDSALP                                          
         DROP  R2                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST BUYER                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LBUYDTA  DS    0H                                                               
         LA    RF,LBUYTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LBUYTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLBUY)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A LIST BUYER FIELD                                                    
***********************************************************************         
DISLBUY  DS    0H                                                               
*                                                                               
         USING TLSTD,R2                                                         
         MVC   FVIFLD(L'LDBUYER),LDBUYER                                        
         DROP  R2                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST STATION                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LSTADTA  DS    0H                                                               
         LA    RF,LSTATBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LSTATBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLSTA)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A LIST STATION FIELD                                                  
***********************************************************************         
DISLSTA  DS    0H                                                               
*                                                                               
         USING TLSTD,R2                                                         
         MVC   FVIFLD(L'LDSTA),LDSTA                                            
         DROP  R2                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST FLIGHT                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LFLTDTA  DS    0H                                                               
         LA    RF,LFLTTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LFLTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLFLT)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A LIST FLIGHT FIELD                                                   
***********************************************************************         
DISLFLT  DS    0H                                                               
*                                                                               
         USING TLSTD,R2                                                         
         GOTO1 VDATCON,BODMCB,(3,LDFLIGHT),(5,FVIFLD)                           
         MVI   FVIFLD+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,LDFLIGHT+3),(5,FVIFLD+9)                            
         DROP  R2                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST DEVELOPMENT SALESPERSON                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LDVSDTA  DS    0H                                                               
         LA    RF,LDVSTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LDVSTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLDVS)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY LIST DEVELOPMENT SALESPERSON FIELD                                    
***********************************************************************         
DISLDVS  DS    0H                                                               
         L     R6,ATLST                                                         
         USING TLSTD,R6                                                         
         MVC   FVIFLD(L'LDDVSAL),LDDVSAL                                        
         DROP  R6                                                               
*                                                                               
DISLDVSX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEVELOPMENT TYPE                                              
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LDVTDTA  DS    0H                                                               
         LA    RF,LDVTTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LDVTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLDVT)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEVELOPMENT TYPE                                                      
***********************************************************************         
DISLDVT  DS    0H                                                               
         L     R6,ATLST                                                         
         USING TLSTD,R6                                                         
         MVC   FVIFLD(L'LDDVTYP),LDDVTYP                                        
         DROP  R6                                                               
*                                                                               
DISLDVTX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
LIST     DS    0H                                                               
         LM    R0,R3,SVPARMS                                                    
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LTSARDIR),AL1(0,0,0),AL4(TSARDR)                             
         DC    AL1(LTSARFIL),AL1(0,0,0),AL4(TSARF)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
FLST     DS    0H                                                               
         MVC   IOKEY(L'RPROKEY),0(R2)                                           
         ICM   R1,15,=AL4(XIO11+XOREPDIR+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         LA    RE,RPROPSTA-(RPROKEY+1)                                          
         CLI   TWAACCS,C'$'        STATION IS USER?                             
         BNE   *+8                 NO                                           
         LA    RE,RPROPSAL-(RPROKEY+1)                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),0(R2)                                                   
         BE    NLST50              FOUND A PROPOSAL                             
         B     EXITL               NO PROPOSALS TO FIND                         
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     DS    0H                                                               
         MVC   IOKEY(L'RPROKEY),0(R2)                                           
         ICM   R1,15,=AL4(XIO11+XOREPDIR+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RPROKEY),0(R2)                                           
         BNE   EXITL               CAN'T RESTORE SEQUENCE                       
*                                                                               
         ICM   R1,15,=AL4(XIO11+XOREPDIR+XOSEQ)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               EOF                                          
*                                                                               
         LA    RE,RPROPSTA-(RPROKEY+1)                                          
         CLI   TWAACCS,C'$'        STATION IS USER?                             
         BNE   *+8                 NO                                           
         LA    RE,RPROPSAL-(RPROKEY+1)                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),0(R2)                                                   
         BNE   EXITL               NO PROPOSALS TO FIND                         
*                                                                               
NLST50   DS    0H                  READ THE ASSOCIATED CONTRACT                 
         MVC   SAVPROKY,IOKEY      KEEP THIS AROUND                             
         MVC   0(L'RCONKEY+5,R2),SAVPROKY                                       
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER?                             
         BNE   NLST51              NO                                           
*                                                                               
         GOTOX AGEN,BOPARM,OFILT,FDUSR,23,SAVPROKY       SALESPERSON            
         BL    NLST                                      SKIP                   
*                                                                               
NLST51   GOTOX AGEN,BOPARM,OFILT,FDUSR,24,SAVPROKY       STATION                
         BL    NLST                                      SKIP                   
         GOTOX AGEN,BOPARM,OFILT,FDUSR,28,SAVPROKY       CONTRACT               
         BL    NLST                                      SKIP                   
*                                                                               
         BAS   RE,GETFRST           GET FIRST RECORD                            
         BNE   NLST                 ERROR - SKIP THIS RECORD                    
*                                                                               
         L     R5,AIOREC            FILTER ON SWITCH ELEMENT                    
         LA    R5,RPROR1ST-RPROHDRD(R5)                                         
NLST52   ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BE    NLST                                                             
         CLI   0(R5),RPRSWELQ                                                   
         BNE   NLST52                                                           
*                                                                               
         GOTOX AGEN,BOPARM,OFILT,FDUSR,25,(R5)           AGENCY                 
         BL    NLST                SKIP THIS RECORD                             
         GOTOX AGEN,BOPARM,OFILT,FDUSR,26,(R5)           ADVERTISER             
         BL    NLST                SKIP THIS RECORD                             
         GOTOX AGEN,BOPARM,OFILT,FDUSR,27,(R5)           FLIGHT DATES           
         BL    NLST                SKIP THIS RECORD                             
         GOTOX AGEN,BOPARM,OFILT,FDUSR,43,(R5)           DEV SALPERSON          
         BL    NLST                SKIP THIS RECORD                             
         GOTOX AGEN,BOPARM,OFILT,FDUSR,44,(R5)           DEV CONT. TYP          
         BL    NLST                SKIP THIS RECORD                             
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER?                             
         BNE   NLSTX               NO                                           
*                                                                               
         L     R5,AIOREC           CHECK ACCESS FLAG                            
         LA    R5,RPROR1ST-RPROHDRD(R5)                                         
NLST62   CLI   0(R5),0                                                          
         BE    NLST                                                             
         CLI   0(R5),RPRDSELQ                                                   
         BE    NLST63                                                           
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     NLST62                                                           
*                                                                               
NLST63   TM    RPRDSFLG-RPRDSELD(R5),RPRDSSTA                                   
         BZ    NLST                                                             
*                                                                               
NLSTX    MVC   0(L'RCONKEY+5,R2),SAVPROKY   HERE WITH D/A & STATUS              
         B     EXITOK                                                           
***********************************************************************         
* GET THE FIRST RECORD IN THE SET                                               
***********************************************************************         
GETFRST  NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
K        USING RPROKEY,IOKEY                                                    
P        USING RPROKEY,SAVPROKY                                                 
         MVI   K.RPROKTYP,RPROKTYQ                                              
         MVI   K.RPROKSTY,RPROKSBQ                                              
         MVC   K.RPROKRCD,P.RPROPRCD                                            
         MVC   K.RPROKCON,P.RPROPCON                                            
         MVC   K.RPROKPRO,P.RPROPPRO                                            
*                                                                               
         ICM   R1,15,=AL4(XIO11+XOREPDIR+XOHID)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   GTFRNO              SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RPROKMST),IOKEYSAV                                       
         BE    *+6                 SHOULD HAVE FOUND 1                          
         DC    H'0'                                                             
         MVC   P.RPROKDA,K.RPROKDA                                              
         DROP  K,P                                                              
*                                                                               
         ICM   R1,15,=AL4(XIO11+XOREPFIL+XOGET)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNL   *+6                 SCREW UP ON THE GETREC                       
         DC    H'0'                                                             
         BNE   GTFRNO              SKIP THIS RECORD                             
*                                                                               
GTFRYES  SR    R5,R5                                                            
         B     GTFR10                                                           
*                                                                               
GTFRNO   LA    R5,1                                                             
         B     GTFR10                                                           
*                                                                               
GTFR10   DS    0H                  RESTORE SEQUENCE                             
         MVC   IOKEY(L'RPROKEY),SAVPROKY                                        
         ICM   R1,15,=AL4(XIO11+XOREPDIR+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RPROKEY),SAVPROKY                                        
         BNE   EXITL               CAN'T RESTORE SEQUENCE                       
*                                                                               
         LTR   R5,R5                                                            
         BZ    EXITOK                                                           
         B     EXITL                                                            
*                                                                               
***********************************************************************         
* TSAR DIRECTORY RECORD UPDATE                                                  
***********************************************************************         
         SPACE 1                                                                
TSARDR   DS    0H                                                               
         L     R5,ATLST            POINT TO THE TSAR RECORD                     
         USING TLSTD,R5                                                         
         USING RPROKEY,R2                                                       
*                                                                               
         MVC   LDSTA,RPROPSTA                                                   
         B     EXITOK                                                           
         DROP  R5,R2                                                            
***********************************************************************         
* TSAR FILE RECORD UPDATE                                             *         
***********************************************************************         
         SPACE 1                                                                
TSARF    DS    0H                                                               
*                                                                               
         L     R5,ATLST            POINT TO THE TSAR RECORD                     
         USING TLSTD,R5                                                         
******************                                                              
* SWITCH ELEMENT *                                                              
******************                                                              
         L     R6,AIOREC                                                        
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
TSARF2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                SHOULD HAVE SKIPPED                          
         CLI   0(R6),RPRSWELQ                                                   
         BNE   TSARF2                                                           
         USING RPRSWELD,R6                                                      
*                                                                               
         MVC   TLRLEN,=AL2(TSARLNQ)     MY TSAR RECORD LENGTH                   
         MVC   LDAGY,RPRSWAGY      AGENCY                                       
         MVC   LDAOF,RPRSWAOF      AGENCY OFFICE                                
         MVC   LDADV,RPRSWADV      ADVERTISER                                   
         MVC   LDSALP,RPRSWSAL     SALESPERSON                                  
         MVC   LDDVSAL,RPRSWDSP    DEVELOPMENTAL SALESPERSON                    
         MVC   LDDVTYP,RPRSWDCT    DEVELOPMENTAL CONTRACT TYPE                  
         MVC   LDFLIGHT,RPRSWFLT   FLIGHT DATES                                 
         DROP  R6                                                               
*******************                                                             
* CONTRACT RECORD *                                                             
*******************                                                             
         MVC   LDBUYER,=CL30'NO CONTRACT'                                       
         MVC   LDPRD,=CL30'NO CONTRACT'                                         
*                                                                               
         L     R6,AIOREC                                                        
         USING RPROKEY,R6                                                       
         LA    RE,IOKEY                                                         
         USING RCONPTYP,RE                                                      
         XC    RCONKEY,RCONKEY                                                  
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,CUAALF                                                  
         MVC   RCONPCON,RPROKCON                                                
         DROP  RE,R6                                                            
*                                                                               
         ICM   R1,15,=AL4(XIO8+XOREPDIR+XOREAD)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNL   *+6                 SCREW UP ON THE READ                         
         DC    H'0'                                                             
         BNE   TSARFX              SKIP THIS RECORD                             
*                                                                               
         MVC   IODAOVER,IOKEY+(L'RCONKEY+1)    GET DISK ADDR                    
*                                                                               
         ICM   R1,15,=AL4(XIO8+XOREPFIL+XOGET)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BNL   *+6                 SCREW UP ON THE GETREC                       
         DC    H'0'                                                             
         BNE   TSARFX              SKIP THIS RECORD                             
*                                                                               
         L     R6,AIO8             CONTRACT RECORD                              
         MVC   LDCONNUM,RCONKCON-RCONREC(R6)                                    
         LA    R6,RCONELEM-RCONREC(R6)                                          
         USING RCONELEM,R6                                                      
         MVI   ELCODE,X'01'        DESCRIPTION ELEMENT                          
         BAS   RE,FIRSTEL                                                       
         BE    *+6                 REQUIRED                                     
         DC    H'0'                                                             
*                                                                               
         MVC   LDBUYER,RCONBUYR                                                 
*                                                                               
         CLC   RCONPRD,SPACES                                                   
         BE    TSARF10                                                          
         MVC   LDPRD(L'RCONPRD),RCONPRD                                         
         B     TSARF20                                                          
*                                                                               
TSARF10  DS    0H                                                               
         L     R6,AIO8             CONTRACT RECORD                              
         LA    R6,RCONELEM-RCONREC(R6)                                          
         USING RCONEXEL,R6                                                      
         MVI   ELCODE,X'05'        PRODUCT NAME ELEMENT                         
         BAS   RE,FIRSTEL                                                       
         BNE   TSARF20                                                          
         MVC   LDPRD,RCONEXPR                                                   
*                                                                               
TSARF20  DS    0H                                                               
*                                                                               
TSARFX   B     EXITOK                                                           
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* PFKEY OBJECT                                                                  
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
PFKEY    LM    R0,R3,SVPARMS                                                    
         LA    RF,PFKYTBL                                                       
         B     ITER                                                             
*                                                                               
PFKYTBL  DC    AL1(PFREC),AL1(0,0,0),AL4(RECPFK)                                
         DC    AL1(PFACT),AL1(0,0,0),AL4(ACTPFK)                                
         DC    AL1(PFUSER),AL1(0,0,0),AL4(USRPFK)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* CAN SET THE RECORD FOR THE PFKEY                                              
***********************************************************************         
RECPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),PFKYUP                                                     
         BNL   NOTPFK              DON'T OUTPUT RECORD NAME                     
*                                                                               
RECPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE ACTION FOR THE PFKEY                                              
***********************************************************************         
ACTPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
*                                                                               
ACTPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE USER NAME FOR THE PFKEY                                           
***********************************************************************         
USRPFK   DS    0H                                                               
*                                                                               
USRPFKX  B     EXITOK                                                           
***********************************************************************         
* PFKEY DEFINITION (RECORD, ACTION, OR USER) NOT WANTED                         
***********************************************************************         
NOTPFK   OI    SVPARMS3,X'80'                                                   
         B     EXITOK                                                           
***********************************************************************         
* SUB-ACTION OVERRIDE OBJECT                                                    
***********************************************************************         
SUBACT   DS    0H                                                               
         B     EXITH               NONE TO DO                                   
         EJECT                                                                  
***********************************************************************         
* NTRSES OBJECT                                                                 
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 HOLDS A(BLOCK) COVERED BY SSAVD                                            
***********************************************************************         
NTRSES   LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTROUT),AL1(0,0,0),AL4(NTROUT)                              
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITIN),AL1(0,0,0),AL4(NTRXIN)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                                      
***********************************************************************         
         PUSH  USING                                                            
         USING FSRRECD,GSRECKEY                                                 
NTROUT   DS    0H                                                               
*                                                                               
NTROUTX  B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                                    
***********************************************************************         
         PUSH  USING                                                            
NTRIN    DS    0H                                                               
         NI    GSINDSL1,X'FF'-GSINOIO    TURN OFF OUR IO'S                      
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS COMIMG BACK FROM CALLED SESSION                                       
***********************************************************************         
         PUSH  USING                                                            
NTRXIN   DS    0H                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* SCREEN OBJECT                                                                 
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
SCRN     LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,SCRNTBL                                                       
         B     ITER                                                             
*                                                                               
SCRNTBL  DC    AL1(SSET),AL1(0,0,0),AL4(SETSCR)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* SET THE SCREEN CODE                                                           
***********************************************************************         
SETSCR   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
***************                                                                 
* INFO EXITS                                                                    
***************                                                                 
EXITENTR MVC   FVMSGNO,=AL2(GI$ENTER)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXITL               EXIT WITH ENTER DATA                         
***************                                                                 
* ERROR EXITS                                                                   
***************                                                                 
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
EXITRCNF MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITL               EXIT WITH RECORD NOT ON FILE                 
EXITRCDL MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         B     EXITL               EXIT WITH RECORD IS DELETED                  
EXITRCAE MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     EXITL               EXIT WITH RECORD ALREADY EXISTS              
EXITCRES MVC   FVMSGNO,=AL2(FVFXRES)                                            
         B     EXITL               EXIT WITH RECORD CAN'T BE RESTORED           
INVLUPGD MVC   FVMSGNO,=AL2(INVUPGRD)                                           
         B     EXITL               INVALID UPGRADE EXPRESSION                   
EXITNMOR MVC   FVMSGNO,=AL2(NMOREPRO)                                           
         B     EXITL               NO MORE PROPOSALS FOR THIS CONTRACT          
EXITSLCK MVC   FVMSGNO,=AL2(GE$SLOCK)                                           
         B     EXITL               EXIT WITH SECURITY LOCKOUT                   
*********                                                                       
* FILTER EXITS                                                                  
*********                                                                       
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT NOT WANTED FOR FILTER                   
         B     EXITOK                                                           
DIE      DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* GETEL                                                                         
***********************************************************************         
         GETEL R6,=AL2(RCONELEM-RCONKEY),ELCODE                                 
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
*                                                                               
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
SPACES   DC    CL80' '                                                          
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                                       
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
AFRREL   DS    A                                                                
AFVADDR  DS    A                                                                
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1KYCHG EQU   X'80'                - KEY FIELD CHANGED                         
MF1PFRET EQU   X'40'                - RETURNING FROM CALLED SESSION             
MF1MNADD EQU   X'20'                - NEED MINO ADD ON FETCH                    
MF1TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
MNIOFLAG DS    XL1                 MINIO FLAG                                   
MNIOCLSQ EQU   X'80'               - A CHANGE WAS MADE, CLOSE MINIO             
*                                                                               
ELCODE   DS    X                                                                
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE AREA                          
*                                                                               
SAVPROKY DS    CL(L'RPROKEY+5)     SAVED PROPOSAL KEY+STATUS+D/A                
*                                                                               
LKSAL    DS    CL(L'RSALKSAL)      SAVE THE LIST KEY SALSPERSON CODE            
LKTEAM   DS    CL(L'RSALTEAM)      SAVE THE LIST KEY SALSPERSON TEAM            
LKOFF    DS    CL(L'RSALOFF)       SAVE THE LIST KEY SALESPERSON OFFICE         
LKSTA    DS    CL(L'RSTAKSTA)      SAVE THE LIST KEY STATION                    
*                                                                               
         EJECT                                                                  
PFKYUP   EQU   PFK07               PFKEY FOR SCROLL UP                          
PFKYDOWN EQU   PFK08               PFKEY FOR SCROLL DOWN                        
PFKYLEFT EQU   PFK09               PFKEY FOR SCROLL LEFT                        
PFKYRGHT EQU   PFK10               PFKEY FOR SCROLL RIGHT                       
PFKYNEXT EQU   PFK11               PFKEY FOR NEXT(FROM LIST)                    
PFKYQUIT EQU   PFK12               PFKEY FOR QUIT                               
         EJECT                                                                  
* REPROWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REPROWORK                                                      
         PRINT ON                                                               
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
***********************************                                             
* SAVE AREA EXCEPT BETWEEN NTRSES                                               
***********************************                                             
MINBKS   DS    0XL(7*(3+10))       3-BYTE BOOK & 10-BYTE UPGRD (MINIO)          
MINBK    DS    7XL(3+10)            - NULL: EMPTY OR LABEL                      
*                                                                               
MINLBLS  DS    0CL(7*5)            LABELS FOR USER DEFINED BKS (MINIO)          
MINLBL   DS    7CL5                 - NULL: EMPTY OR LABEL                      
*                                                                               
MINDMOS  DS    0CL(7*3)            3-BYTE DEMO                                  
MINDMO   DS    7CL3                                                             
         DS    XL(TWUSER+L'TWUSER-*)   # OF SPARE BEFORE TWSECBLK               
         SPACE 2                                                                
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLUSER                                                           
*************************                                                       
* CONTRACT INFO IN TSAR "FILE REOCRD"                                           
*************************                                                       
LDCONNUM DS    XL(L'RCONKCON)      CONTRACT # IN PWOS                           
LDSTA    DS    CL(L'RCONKSTA)      STATION CALL LETTERS                         
LDAGY    DS    CL(L'RCONKAGY)      AGENCY CODE                                  
LDAOF    DS    CL(L'RCONKAOF)      AGENCY OFFICE                                
LDADV    DS    CL(L'RCONKADV)      ADVERTISER CODE                              
LDPRD    DS    CL(L'RCONEXPR)      PRODUCT (NAME OR CODE)                       
LDFLIGHT DS    CL(L'RCONDATE)      FLIGHT DATES                                 
LDBUYER  DS    CL(L'RCONBUYR)      BUYER                                        
LDSALP   DS    CL(L'RCONSAL)       SALESPERSON CODE                             
LDDVSAL  DS    CL(L'RCONDVSP)      DEVELOPMENTAL SALESPERSON CODE               
LDDVTYP  DS    CL(L'RCONDVCT)      DEVELOPMENTAL CONTRACT TYPE                  
TSARLNQ  EQU   *-TLSTD             SAR LENGTH EQUATE                            
         SPACE 2                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
* DDDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
* CTMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REPRO14S  09/04/96'                                      
         END                                                                    
*                                                                               
