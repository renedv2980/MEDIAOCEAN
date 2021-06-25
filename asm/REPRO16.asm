*          DATA SET REPRO16    AT LEVEL 002 AS OF 08/16/00                      
*&&      SET   NOP=N                                                            
*PHASE T80A16A                                                                  
T80A16   TITLE 'REPRO16 - REP PROPOSALS WORKSHEET RECAP'                        
PRO16    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REPRO16*,R7,RR=RE                                              
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
GSFRR    USING FRRELD,GSFRREL      CURRENT RECORD ELEMENT                       
GSFRA    USING FRAELD,GSFRAEL      CURRENT ACTION ELEMENT                       
GSFRP    USING FRPELD,GSFRPEL      CURRENT PFKEY ELEMENT                        
PSFRR    USING FRRELD,PSFRREL      PREVIOUS SESSION'S RECORD ELEMENT            
PSFRA    USING FRAELD,PSFRAEL      PREVIOUS SESSION'S ACTION ELEMENT            
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
NUMSTAS  EQU   4                   NUMBER OF STATIONS                           
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
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
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
         OI    GSINDSL1,GSINOIO    WE'LL DO THE IO'S                            
         OI    GSINDSL1,GSIXKEY    NO ENTER KEY MESG                            
         OI    LSSTAT1,LSSBALL     BUILD ALL OF LIST IN ONE GO                  
         OI    LSSTAT1,LSSTSAR     TSAR ONLY LIST                               
*                                                                               
INITX    B     EXITOK                                                           
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
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE VALIDATING THE KEY FIELDS                                              
***********************************************************************         
KFKVAL   DS    0H                                                               
* DON'T KNOW IF KEY OR VIEW WAS CHANGED YET                                     
         NI    MISCFLG1,X'FF'-MF1KYCHG-MF1VWCHG                                 
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
KLTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER VALIDATING THE KEY FIELDS                                               
***********************************************************************         
KLKVAL   DS    0H                                                               
         GOTO1 =A(KLSTKVAL),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
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
RECRDTBL DC    AL1(RLAST),AL1(0,0,0),AL4(RECLAST)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER THE CONTROLLER CALLS THE I/O ACTION                                     
***********************************************************************         
RECLAST  DS    0H                                                               
*        BAS   RE,MINIOCLS                                                      
         B     EXITOK                                                           
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE DISPLAYING THE DATA FIELDS                                             
***********************************************************************         
DFDDIS   DS    0H                                                               
         MVC   SVRECDA,GSRECDA                                                  
         GOTO1 =A(D1STDDIS),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
************ DOING AN ACTION ON A SPECIFIC DATA OBJECT ****************         
***********************************************************************         
DATA10   DS    0H                                                               
         LR    RF,RB               TABLE OF KNOWN OBJECTS                       
         AH    RF,=Y(KNOWTAB-PRO16)                                             
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
*                                                                               
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING FRRRECD,R2          R2 HOLDS A(RECORD)                           
         L     R3,AFRREL                                                        
         USING FRRELD,R3           R3=A(FRREL ON RECORD)                        
         BR    RF                                                               
         EJECT                                                                  
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
CONTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALCON)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(NTRCON)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE A CONTRACT FIELD                                                     
***********************************************************************         
VALCON   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         CLI   FVILEN,0            THIS FIELD IS REQUIRED                       
         BE    EXITNO                                                           
*                                                                               
         GOTOX (VALCONQ,AREPRO01)                                               
         BL    EXITL                                                            
*                                                                               
         OI    FVIIND,FVIVAL       VALIDATED                                    
         B     EXITOK                                                           
***********************************************************************         
* DISPLAY A CONTRACT FIELD                                                      
***********************************************************************         
NTRCON   DS    0H                                                               
         LR    RE,RA               DO WE HAVE ANY CONTRACT NUMBER?              
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         OC    0(L'SVCONNUM,RE),0(RE)                                           
         BZ    NTRCONX             NO                                           
*                                                                               
         ZAP   BOWORK1+10(5),=P'0'                                              
         MVO   BOWORK1+10(5),0(4,RE)                                            
         ZAP   BOWORK1(5),=P'99999999'                                          
         SP    BOWORK1(5),BOWORK1+10(5)                                         
         OI    BOWORK1+4,X'0F'                                                  
         UNPK  FVIFLD(8),BOWORK1(5)                                             
NTRCONX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PROPOSAL NUMBER                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRODTA   DS    0H                                                               
         LA    RF,PROTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PROTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRO)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(NTRPRO)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE A PROPOSAL FIELD                                                     
***********************************************************************         
VALPRO   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         GOTOX (VALPROQ,AREPRO01)                                               
         BL    EXITL                                                            
*                                                                               
         OI    FVIIND,FVIVAL       VALIDATED                                    
*                                                                               
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
*                                                                               
         GOTOX (MNIOINQ,AREPRO01)  INITIALIZE MINIO                             
*                                                                               
         GOTO1 =A(RDBKSDMS),BODMCB,(R9),RR=BORELO                               
*                                                                               
         XC    RPROKEY,RPROKEY                                                  
*                                                                               
VALPROX  B     EXITOK                                                           
***********************************************************************         
* PASS PROPOSAL NUMBER TO NEXT SESSION                                          
***********************************************************************         
NTRPRO   DS    0H                                                               
         LR    RE,RA               DO WE HAVE ANY PROPOSAL NUMBER?              
         AH    RE,=Y(SVPRONUM-TWAD)                                             
         CLI   0(RE),0                                                          
         BE    NTRPROX             NO                                           
*                                                                               
         MVC   BOBYTE1,0(RE)                                                    
         XI    BOBYTE1,X'FF'                                                    
         EDIT  (B1,BOBYTE1),(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,         X        
               DUB=BODUB1                                                       
NTRPROX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR AGENCY                                                        
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
AGYDTA   DS    0H                                                               
         MVC   FVIFLD(L'EAGYNAM1),EAGYNAM1                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR ADVERTISER                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
ADVDTA   DS    0H                                                               
         MVC   FVIFLD(L'EADVNAME),EADVNAME                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR PRODUCT                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRDDTA   DS    0H                                                               
         MVC   FVIFLD(L'EPRDNAME),EPRDNAME                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR SALESPERSON                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SALDTA   DS    0H                                                               
         MVC   FVIFLD(L'ESALNAME),ESALNAME                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR BUYER                                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
BYRDTA   DS    0H                                                               
         MVC   FVIFLD(L'ECONBUYR),ECONBUYR                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR FLIGHT DATES                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
FLTDTA   DS    0H                                                               
         GOTO1 VDATCON,BODMCB,(3,CCONDAT),(5,FVIFLD)                            
         MVI   FVIFLD+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,CCONDAT+3),(5,FVIFLD+9)                             
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR DISK ADDRESS                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DSKDTA   DS    0H                                                               
         GOTO1 VHEXOUT,BODMCB,GSRECDA,FVIFLD,L'GSRECDA                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEVELOPMENT SALESPERSON                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DVSDTA   DS    0H                                                               
         LA    RF,DVSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DVSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDVS)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEVELOPMENT SALESPERSON FIELD                                         
***********************************************************************         
DISDVS   DS    0H                                                               
         MVC   FVIFLD(L'EDVSNAME),EDVSNAME                                      
         B     DISDVSX                                                          
*                                                                               
DISDVS5  MVC   FVIFLD(L'CCONDVS),CCONDVS                                        
*                                                                               
DISDVSX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEVELOPMENT TYPE                                              
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DVTDTA   DS    0H                                                               
         LA    RF,DVTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DVTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDVT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEVELOPMENT TYPE                                                      
***********************************************************************         
DISDVT   DS    0H                                                               
         MVC   FVIFLD(L'CCONDVT),CCONDVT                                        
*                                                                               
DISDVTX  B     EXITOK                                                           
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
STATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTA)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTA)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY STATION FIELD                                                         
***********************************************************************         
DISSTA   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
*                                                                               
         TM    MISCFLG1,MF1KYCHG      DID THE KEY CHANGE?                       
         BZ    *+8                    NO                                        
         MVI   0(RE),1                STATION FROM CONTRACT IS #1               
*                                                                               
         CLI   0(RE),0                ANY STATION?                              
         BE    DISSTAX                NONE                                      
         ZIC   R1,0(RE)                                                         
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R1,SAVSTAS(R1)                                                   
         MVC   FVIFLD(L'SAVSTA),0(R1)                                           
*                                                                               
DISSTAX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE STATION FIELD                                                        
***********************************************************************         
VALSTA   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1VWCHG   NO, THE VIEW HAS CHANGED                     
*                                                                               
         CLI   FVILEN,0            ANY STATION?                                 
         BNE   VALSTA00                                                         
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         MVI   0(RE),0             NONE                                         
         B     VALSTAX                                                          
*                                                                               
VALSTA00 CLI   FVILEN,6            MORE THAN 5 LETTERS FOR STATION?             
         BH    EXITNV              YES, INVALID STATION (FOR NOW)               
*                                                                               
         OC    FVIFLD,BCSPACES                                                  
         LA    R1,1                                                             
         LA    RE,SAVSTAS                                                       
VALSTA10 CLC   0(L'SAVSTA,RE),FVIFLD                                            
         BE    VALSTA20                                                         
         LA    RE,L'SAVSTA(RE)                                                  
         LA    R1,1(R1)                                                         
         LA    RF,SAVSTAS+L'SAVSTAS                                             
         CR    RE,RF                                                            
         BL    VALSTA10                                                         
         B     EXITNV                                                           
*                                                                               
VALSTA20 LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         STC   R1,0(RE)              SAVE INTERNAL ORDER # OF STATION           
*                                                                               
VALSTAX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
*          DATA SET REPRO23    AT LEVEL 095 AS OF 04/17/96                      
***********************************************************************         
* DATA OBJECT FOR OPTIONS                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
OPTDTA   DS    0H                                                               
         LA    RF,OPTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
OPTTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALOPT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE OPTIONS FIELD                                                        
***********************************************************************         
VALOPT   DS    0H                                                               
         GOTO1 =A(VALOPTNS),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BOOK                                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
BOKDTA   DS    0H                                                               
         LA    RF,BOKTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BOKTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALBOK)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE KEY BOOK FIELD                                                       
***********************************************************************         
VALBOK   DS    0H                                                               
         GOTO1 =A(VALKYBOK),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEMO                                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DMODTA   DS    0H                                                               
         LA    RF,DMOTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DMOTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALDMO)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE DEMO FIELD                                                           
***********************************************************************         
VALDMO   DS    0H                                                               
         GOTO1 =A(VALDEMOS),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DAYPART FIELD                                                 
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LDPTDTA  DS    0H                                                               
         LA    RF,LDPTTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LDPTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLDPT)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DAYPART FIELD                                                         
***********************************************************************         
DISLDPT  DS    0H                                                               
         USING TLSTD,R2                                                         
***      CLI   TRCPDPT,C' '                                                     
***      BH    *+6                                                              
***      DC    H'0'                                                             
         MVC   FVIFLD(L'TRCPDPT),TRCPDPT                                        
DSLDPTX  B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* DATA OBJECT FOR COST FIELD                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LCSTDTA  DS    0H                                                               
         LA    RF,LCSTTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LCSTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLCST)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY COST FIELD                                                            
***********************************************************************         
DISLCST  DS    0H                                                               
         USING TLSTD,R2                                                         
         EDIT  (B4,TRCPCOST),(8,FVIFLD),2,DUB=BODUB1,                  X        
               WRK=BOWORK1,ZERO=NOBLANK                                         
*                                                                               
DSLCSTX  B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RATING FIELD                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LRTGDTA  DS    0H                                                               
         LA    RF,LRTGTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LRTGTBL  DS    0H                                                               
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISLRTG)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY RATING FIELD                                                          
***********************************************************************         
DISLRTG  DS    0H                                                               
         USING TLSTD,R2                                                         
         EDIT  (B4,TRCPRTG),(8,FVIFLD),1,DUB=BODUB1,                   X        
               WRK=BOWORK1,ZERO=NOBLANK                                         
*                                                                               
         B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* DATA OBJECT FOR CPP FIELD                                                     
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LCPPDTA  DS    0H                                                               
         LA    RF,LCPPTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LCPPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLCPP)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY CPP FIELD                                                             
***********************************************************************         
DISLCPP  DS    0H                                                               
         USING TLSTD,R2                                                         
         EDIT  (B4,TRCPCPP),(8,FVIFLD),2,DUB=BODUB1,                   X        
               WRK=BOWORK1,ZERO=NOBLANK                                         
*                                                                               
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SPOTS FIELD                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LSPTDTA  DS    0H                                                               
         LA    RF,LSPTTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LSPTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLSPT)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY SPOTS FIELD                                                           
***********************************************************************         
DISLSPT  DS    0H                                                               
         USING TLSTD,R2                                                         
         EDIT  (B4,TRCPSPTS),(8,FVIFLD),WRK=BOWORK1,                   X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
         B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* DATA OBJECT FOR # OF DETAIL LINES                                             
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LNDPDTA  DS    0H                                                               
         LA    RF,LNDPTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LNDPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLNDP)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY # OF DETAIL LINES                                                     
***********************************************************************         
DISLNDP  DS    0H                                                               
         USING TLSTD,R2                                                         
         EDIT  (B2,TRCPNDP),(8,FVIFLD),WRK=BOWORK1,                    X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
         B     EXITOK                                                           
         DROP  R2                                                               
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
* DISPLAY LIST STATION FIELD                                                    
***********************************************************************         
DISLSTA  DS    0H                                                               
         USING TLSTD,R2                                                         
         MVC   FVIFLD(L'TLKDISTA),TLKDISTA                                      
*                                                                               
         B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* LIST OBJECT                                                                   
*                                                                               
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 HOLDS CURRENT KEY BUILD AREA   (THIS)                                      
* P4 HOLDS PREVIOUS KEY BUILD AREA  (LAST)                                      
***********************************************************************         
         SPACE 1                                                                
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING RPROKEY,R2                                                       
LAST     USING RPROKEY,R3                                                       
*                                                                               
         LR    RF,RB                                                            
         AH    RF,=Y(LISTABL-PRO16)                                             
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
***********************************************************************         
* SET DEFAULT COLUMNS                                                           
***********************************************************************         
DEFCLM1  DS    0H                                                               
         GOTO1 =A(DEFCLMNS),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISE FOR LIST                                                           
***********************************************************************         
INITL1   DS    0H                                                               
*        OI    LSSTAT1,LSSSEL+LSSTSAR   SELECTABLE LIST OF TSAR RECS            
         OI    LSSTAT2,LSSIUPD          WE WANT TO DO OUR OWN UPDATES           
         MVI   LSSUBLEN,2               LENGTH OF SUB-ACTION FIELD              
*                                                                               
         NI    LSSTAT2,X'FF'-LSSADD   NOT VALID TO ADD NEW LIST LINES           
*                                                                               
         B     EXITOK                                                           
***********************************************************************         
* FIRST TIME FOR LIST                                                           
***********************************************************************         
FTFLST1  DS    0H                                                               
         XC    SVMINEKY,SVMINEKY                                                
         MVI   SVMINEKY,RPRDTELQ   NEED A DETAIL DESCRIPTION ELEMENT            
         B     EXITOK                                                           
***********************************************************************         
* FIRST FOR LIST                                                                
***********************************************************************         
FLST1    DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'SVMINEKY),SVMINEKY                                     
         BAS   RE,MINIOHI                                                       
         BNE   EXITL               MINIO HIGH IS UNHAPPY                        
*                                                                               
         L     R0,AIO1             BUILD LIST HERE FIRST                        
         LH    R1,=AL2(IOAREALN)                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     FML20                                                            
*                                                                               
FML10    BAS   RE,MINIOSEQ                                                      
         BNE   FML90                                                            
*                                                                               
FML20    L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         CLI   0(R6),RPRDTELQ      GOT A DETAIL DESCRIPTION ELEM?               
         BNE   FML10               NO                                           
*                                                                               
         LR    RE,RA               DO WE HAVE A STATION FILTER?                 
         AH    RE,=Y(SVSTATN-TWAD)                                              
         CLI   0(RE),0                                                          
         BE    *+14                                                             
         CLC   RPRDTSTA,0(RE)      YES, MATCH?                                  
         BNE   FML10                    NO, CHECK NEXT DETAIL                   
*                                                                               
         TM    FILTFLG1,FF1SPOTS   FILTER FOR THOSE WITH SPOTS?                 
         BZ    *+14                NO                                           
         OC    RPRDTTSP,RPRDTTSP   YES, ANY SPOTS FOR THIS DETAIL?              
         BZ    FML10                    NONE                                    
*                                                                               
         TM    FILTFLG1,FF1HIDN    DISPLAY HIDDEN LINES?                        
         BZ    FML21               NO                                           
         TM    RPRDTFL1,RPRDTF1H   HIDDEN?                                      
         BZ    FML10               NO - NEXT DETAIL                             
         B     FML22                                                            
*                                                                               
FML21    TM    RPRDTFL1,RPRDTF1H   HIDDEN?                                      
         BNZ   FML10               YES - NEXT DETAIL                            
*                                                                               
FML22    DS    0H                                                               
*                                                                               
FML30    MVI   SVMINEKY,RPRDTELQ                                                
         MVC   SVMINEKY+1(L'SVMINEKY-1),RPRDTSTA                                
*                                                                               
         L     R4,AIO1                                                          
         USING RECAPLN,R4                                                       
FML32    CLI   RCPDPT,0            END OF DPTS?                                 
         BE    FML34               YES                                          
         CLC   RCPDPT,RPRDTDPT     DPT MATCH?                                   
         BNE   *+14                NO                                           
         CLC   RCPSTA,RPRDTSTA     STATION MATCH?                               
         BE    FML40               YES                                          
         LA    R4,RCPLNQ(R4)                                                    
         B     FML32                                                            
*                                                                               
FML34    DS    0H                                                               
         MVC   RCPDPT,RPRDTDPT     DAYPART CODE                                 
         MVC   RCPSTA,RPRDTSTA     STATION INTERNAL CODE                        
FML40    DS    0H                                                               
         ZICM  R0,RPRDTNC1,4       COST                                         
         A     R0,RCPCOST                                                       
         ST    R0,RCPCOST                                                       
*                                                                               
         ZICM  R0,RPRDTTAB,4       CPP                                          
         A     R0,RCPCPP                                                        
         ST    R0,RCPCPP                                                        
*                                                                               
         ZICM  R0,RPRDTTSP,2       SPOTS                                        
         A     R0,RCPSPTS                                                       
         ST    R0,RCPSPTS                                                       
*                                                                               
         LH    R0,RCPNDP           # OF DETAILS                                 
         AH    R0,=H'1'                                                         
         STH   R0,RCPNDP                                                        
*                                                                               
*** RATING ****                                                                 
*                                                                               
         B     FML10                    NEXT DETAIL                             
         DROP  R6                                                               
*****************************                                                   
** ADD RECAP LINES TO TSAR **                                                   
*****************************                                                   
FML90    DS    0H                                                               
         L     R6,ATLST                                                         
         USING TLSTD,R6                                                         
         L     R4,AIO1                                                          
         USING RECAPLN,R4                                                       
*                                                                               
FML92    CLI   RCPDPT,0            END OF DPTS?                                 
         BE    FML94               YES                                          
*                                                                               
         GOTOX AGENLST,BOPARM,OLIST,LTSARDIR,IOKEY                              
*                                                                               
         MVC   TLRLEN,=AL2(TSARLNQ)     MY TSAR RECORD LENGTH                   
         GOTO1 DPTSEQ,BODMCB,(RCPDPT,0)                                         
         MVC   TLKDIDSQ,0(R1)                                                   
         MVC   TLKDIDPT,RCPDPT                       DAYPART                    
*                                                                               
         ZIC   RE,RCPSTA           STATION CALL LETTERS                         
         BCTR  RE,0                                                             
         MH    RE,=Y(L'SAVSTA)                                                  
         LA    RE,SAVSTAS(RE)                                                   
         MVC   TLKDISTA,0(RE)                                                   
*                                                                               
         MVC   TRCPDPT(RCPLNQ),RCPDPT                                           
*                                                                               
         GOTOX AGENLST,BOPARM,OLIST,LTSARADD                                    
         LA    R4,RCPLNQ(R4)                                                    
         B     FML92                                                            
         DROP  R4,R6                                                            
*                                                                               
FML94    DS    0H                                                               
*                                                                               
FLSTX    B     EXITL                                                            
         DROP  R5                                                               
***********************************************************************         
* NEXT FOR LIST                                                                 
***********************************************************************         
NLST1    DS    0H                                                               
         B     EXITL               PRETEND NOTHING TO FIND                      
         EJECT                                                                  
***********************************************************************         
* SET UP TSAR FROM DIRECTORY                                                    
***********************************************************************         
TSARDIR1 DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* SET UP TSAR FROM FILE                                                         
***********************************************************************         
TSARFIL1 DS    0H                                                               
         B     EXITOK                                                           
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
         OI    SNINDS1,SNIPARMS    SO WE CAN GET DNTR                           
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                                    
***********************************************************************         
         PUSH  USING                                                            
NTRIN    DS    0H                                                               
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS COMIMG BACK FROM CALLED SESSION                                       
***********************************************************************         
         PUSH  USING                                                            
NTRXIN   DS    0H                                                               
         NI    SNINDS1,FF-SNIUSECR ?????                                        
         OI    MISCFLG1,MF1PFRET                                                
         B     EXITOK                                                           
         POP   USING                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SCREEN OBJECT                                                                 
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
SCRN     LM    R0,R3,SVPARMS                                                    
         LA    RF,SCRNTBL                                                       
         B     ITER                                                             
*                                                                               
SCRNTBL  DC    AL1(SKSET),AL1(0,0,0),AL4(SETKSCR)                               
         DC    AL1(SSET),AL1(0,0,0),AL4(SETMSCR)                                
         DC    AL1(SMOD),AL1(0,0,0),AL4(MODSCR)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* SET THE KEY SCREEN CODE                                                       
***********************************************************************         
SETKSCR  DS    0H                                                               
*        MVI   GSSKCODE,0                                                       
*        CLI   CSACT,A#RECAP                                                    
*        BNE   SETKSCRX                                                         
         MVI   GSSKCODE,C'R'       UPDATE KEY SCREEN                            
*                                                                               
         B     EXITOK                                                           
***********************************************************************         
* SET THE MAINT SCREEN CODE                                                     
***********************************************************************         
SETMSCR  DS    0H                                                               
         TM    MISCFLG1,MF1KYCHG   DID THE KEY CHANGE?                          
         BNZ   SETMSCR0            YES, WE NEED TO DO THE SCREEN THEN           
         TM    MISCFLG1,MF1VWCHG   DO WE NEED TO CHANGE VIEWS?                  
         BNZ   SETMSCR0            YES                                          
         B     SETMSCRX                                                         
*                                                                               
SETMSCR0 CLI   GSSMCODE,C'R'                                                    
         BE    *+12                                                             
         MVI   GSSMCODE,C'R'                                                    
         B     *+8                                                              
         MVI   GSSMCODE,C'S'                                                    
*                                                                               
         XC    GCLASKEY,GCLASKEY   SO IT DOESN'T GO TO RVAL IN GEFIL02          
*                                                                               
SETMSCRX B   EXITOK                                                             
***********************************************************************         
* MODIFY THE SCREEN FIELDS (PULL OUT THE KEYS FROM AKYFLD)                      
***********************************************************************         
MODSCR   DS    0H                                                               
         L     R5,ATWA             SCREEN                                       
         LA    R5,64(R5)           SKIP HEADER                                  
*                                                                               
MODSCR10 CLI   0(R5),0             END OF SCREEN?                               
         BE    MODSCRX             YES, WE'RE DONE                              
*                                                                               
         TM    1(R5),X'02'         EXTENDED HEADER?                             
         BZ    MODSCRNX            NO, SKIP TO NEXT FIELD                       
*                                                                               
MODSCR20 LR    RF,R5               RF = A(EXTENDED FIELD HDR)                   
         ZIC   R0,0(R5)                                                         
         AR    RF,R0                                                            
         SH    RF,=H'8'                                                         
         USING FVIXHDR,RF                                                       
         ZICM  RE,FVIXUS,2         RE = FIELD #                                 
         DROP  RF                                                               
         BCTR  RE,0                MAKE IT ZERO-BASED                           
         LTR   RE,RE                                                            
         BM    MODSCRNX            SKIP FLUFF                                   
*                                                                               
         SLL   RE,2                MULTIPLY BY 4                                
         L     R6,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    R6,0(RE,R6)         A(THIS FIELD ENTRY)                          
         L     RF,0(R6)            THIS FIELD ENTRY                             
         USING FDRELD,RF                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
         ZICM  RE,FDRNUM,2                                                      
         DROP  RF                                                               
*                                                                               
         LA    RF,KNOWTAB2                                                      
         USING KNOWTABD,RF                                                      
MODSCR30 CLC   KNOWID,=AL2(EOT)    REACH END OF KEY WANTED LIST?                
         BE    MODSCRNX            YES, CHECK NEXT SCREEN FIELD                 
         CLM   RE,3,KNOWID         IS THIS A WANTED KEY?                        
         BE    MODSCR40            YES                                          
         LA    RF,KNOWLQ(RF)                                                    
         B     MODSCR30                                                         
         DROP  RF                                                               
*                                                                               
MODSCR40 SR    R0,R0               SEE IF WE HAVE DATA FOR THIS FIELD           
         L     RF,AKYFLD                                                        
         ZICM  R1,0(RF),2          R1 = A(AFTER LAST KEY FIELD ENTRY)           
         AR    R1,RF                                                            
         LA    RF,2(RF)            RF = A(1ST ENTRY IN KEY FIELD TBL)           
         USING KEYELD,RF                                                        
MODSCR45 CLI   KEYEL,KEYELQ                                                     
         BNE   MODSCRNX            NO DATA FOR THIS KEY FIELD                   
*                                                                               
         CLM   RE,3,KEYNUM         MATCH ON THIS FIELD?                         
         BE    MODSCR50                                                         
         IC    R0,KEYLN            NO                                           
         AR    RF,R0                                                            
         CR    RF,R1                                                            
         BNL   MODSCRNX                                                         
         B     MODSCR45                                                         
*                                                                               
MODSCR50 ZIC   R1,KEYLN            COPY THE DATA OVER TO THE FIELD              
         SH    R1,=Y(KEYLN1Q+1)                                                 
         BM    MODSCRNX                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    8(0,R5),8(R5)       IS THERE ANYTHING HERE?                      
         BNZ   MODSCRNX            YES                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R5),KEYDATA                                                  
         LA    R1,1(R1)                                                         
         STC   R1,5(R5)                                                         
         OI    6(R5),X'80'         AND TRANSMIT IT                              
         DROP  RF                                                               
*                                                                               
MODSCRNX ZIC   R0,0(R5)            BUMP TO NEXT SCREEN FIELD                    
         AR    R5,R0                                                            
         B     MODSCR10                                                         
*                                                                               
MODSCRX  B     EXITOK                                                           
***********************************************************************         
* TABLE OF WANTED KEY OBJECTS                                                   
***********************************************************************         
KNOWTAB2 DS    0XL(KNOWLQ)                                                      
         DC    AL2(00001),AL4(FLDPROT)   CONTRACT                               
         DC    AL2(00002),AL4(FLDPROT)   PROPOSAL                               
         DC    AL2(EOT)                                                         
*                                                                               
FLDPROT  DC    H'0'                DUMMY BRANCH                                 
         EJECT                                                                  
***********************************************************************         
* SUB-ACTION OBJECT                                                             
* -----------------                                                             
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
SUBACT   DS    0H                                                               
         B     EXITH                                                            
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
         CLI   0(RE),PFAVAIL       AVAIL?                                       
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Ava'                                              
         B     RECPFKX                                                          
*                                                                               
         CLI   0(RE),PFPACKGE      PACKAGE?                                     
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Pack'                                             
         B     RECPFKX                                                          
*                                                                               
         CLI   0(RE),PFMBOOK       MBOOK?                                       
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Mbk'                                              
         B     RECPFKX                                                          
*                                                                               
         CLI   0(RE),PFMDEMO       MDEMO?                                       
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Mdem'                                             
         B     RECPFKX                                                          
*                                                                               
         B     NOTPFK              EVERYTHING ELSE - NO RECORD                  
*                                                                               
RECPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE ACTION FOR THE PFKEY                                              
***********************************************************************         
ACTPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         B     NOTPFK              EVERYTHING ELSE - NO ACTION                  
*                                                                               
ACTPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE USER DEFINED NAME FOR THE PFKEY                                   
***********************************************************************         
USRPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
*                                                                               
         CLI   0(RE),PFKYUP        UP?                                          
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Up'                                               
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFKYDOWN      DOWN?                                        
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Dwn'                                              
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFKYLEFT      LEFT?                                        
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Lft'                                              
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFKYRGHT      RIGHT?                                       
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Rt'                                               
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFRETURN      RETURN?                                      
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Re'                                               
         B     USRPFKX                                                          
*                                                                               
USRPFKX  B     EXITOK                                                           
***********************************************************************         
* PFKEY DEFINITION (RECORD, ACTION, OR USER) NOT WANTED                         
***********************************************************************         
NOTPFK   OI    SVPARMS3,X'80'                                                   
         B     EXITOK                                                           
         EJECT                                                                  
HERE     EQU   *-PRO16                                                          
THERE    EQU   X'1000'                                                          
         AIF   ('HERE' GT 'THERE').NOJUMP                                       
         ORG   PRO16+X'1000'                                                    
.NOJUMP  ANOP                                                                   
***********************************************************************         
* THIS ROUTINE WILL TRY TO REDUCE THE DECIMAL EXCEPTIONS THAT MIGHT             
* HAPPEN ON A 'DP' INSTRUCTION                                                  
*                                                                               
* ON ENTRY:    PCKOF16B            DIVIDEND                                     
*              PCKOF08B            DIVISOR (SHOULD NOT BE P'0')                 
*                                                                               
* ON EXIT:     (CC)                NEQ - DIVIDING BY ZERO                       
*                                  EQ  - QUOTIENT IN PCKOF16B                   
*                                                                               
*              PCKOF16B            QUOTIENT (ROUNDED)                           
*                                                                               
* NOTE: BOWORK1 WILL GET CLOBBERED                                              
***********************************************************************         
DIVPACKD NTR1                                                                   
         CP    PCKOF08B,=P'0'      IS THE DIVISOR 0?                            
         BE    DIVPCKNO            YES, CAN'T DIVIDE BY 0                       
*                                                                               
         SRP   PCKOF16B,1,0        MULTIPLY DIVIDEND BY 10 TO ROUND             
***************                                                                 
* CHECKS IF BOTH THE DIVIDEND AND DIVISOR ARE DIVISIBLE BY 10.                  
* IF THEY ARE WE CAN REDUCE THE NUMBER OF HALF BYTES THEY TAKE UP.              
***************                                                                 
DIVPCK10 CP    PCKOF08B+L'PCKOF08B-1(1),=P'0'                                   
         BNE   DIVPCK20                                                         
         CP    PCKOF16B+L'PCKOF16B-1(1),=P'0'                                   
         BNE   DIVPCK20                                                         
         SRP   PCKOF08B,64-1,0                                                  
         SRP   PCKOF16B,64-1,0                                                  
         B     DIVPCK10                                                         
***************                                                                 
* SEE IF WE CAN REDUCE THE NUMBER OF BYTES THE DIVISOR TAKES UP.                
* IE: A DIVISOR OF '7900' STORED IN PCKOF08B NEED ONLY TAKE UP 3 BYTES.         
***************                                                                 
DIVPCK20 LA    R1,L'PCKOF08B                                                    
         LA    R2,PCKOF08B                                                      
DIVPCK25 CLI   0(R2),0                                                          
         BNE   DIVPCK30                                                         
         LA    R2,1(R2)                                                         
         BCT   R1,DIVPCK25                                                      
***************                                                                 
* R2 = A(PACKED DIVISOR)  AND  R1 = L(PACKED DIVISOR)                           
***************                                                                 
DIVPCK30 LR    RE,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         DP    PCKOF16B,0(0,R2)                                                 
***************                                                                 
* HAVE TO ROUND UP   IF (DIVISOR - REMAINDER)  <  REMAINDER                     
***************                                                                 
         LA    RE,L'PCKOF16B-1                                                  
         SR    RE,R1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         ZAP   BOWORK1(L'PCKOF16B),PCKOF16B(0)                                  
         LA    RF,PCKOF16B+1(RE)   RF = A(REMAINDER)                            
*                                                                               
         LR    RE,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         SP    PCKOF08B,0(0,RF)    PCKOF08B = DIVISOR - REMAINDER               
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CP    PCKOF08B,0(0,RF)    (DIVISOR - REMAINDER) <= REMAINDER ?         
         BH    *+10                        NO                                   
         AP    BOWORK1(L'PCKOF16B),=P'1'   YES, ROUND BY ADDING 1               
*                                                                               
         ZAP   PCKOF16B,BOWORK1(L'PCKOF16B)                                     
*                                                                               
DIVPCKYS B     EXITOK                                                           
*                                                                               
DIVPCKNO B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETURNS THE SEQ NUMBER FOR A GIVEN DPT BY USING                  
*    LDPTSEQS                                                                   
*                                                                               
* INPUT:                                                                        
*        PARAM1       BYTE1 = DPT                                               
*                                                                               
* OUTPUT:                                                                       
*        PARAM1       BYTE1 = SEQ #                                             
*                                                                               
***********************************************************************         
DPTSEQ   NTR1                                                                   
         LA    RE,LDPTSEQS                                                      
         SR    RF,RF                                                            
*                                                                               
DPTSEQ5  CLC   0(1,R1),0(RE)                                                    
         BE    DPTSEQ10                                                         
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         LA    R0,LDPTSEQS+L'LDPTSEQS                                           
         CR    RE,R0                                                            
         BL    DPTSEQ5                                                          
         MVI   0(R1),X'FF'         NOT FOUND                                    
         B     DPTSEQX                                                          
*                                                                               
DPTSEQ10 STC   RF,0(R1)                                                         
*                                                                               
DPTSEQX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS FOR A MINIO ELEMENT                                        
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIORD  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINRD',(R5))                                     
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS HIGH FOR A MINIO ELEMENT.                                  
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIOHI  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINHI',(R5))                                     
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         B     EXITL               OTHERWISE RETURN 'NO'                        
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS SEQUENTIAL FOR A MINIO ELEMENT.                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOSEQ NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINSEQ',(R5))                                    
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    EXITL                                                            
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOWRT NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         GOTO1 VMINIO,BODMCB,('MINWRT',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.                                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOADD NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         GOTO1 VMINIO,BODMCB,('MINADD',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    EXITL               YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE DELETES A MINIO ELEMENT.  CALLER IS RESPONSIBLE FOR              
* POINTING TO ELEMENT FIRST.                                                    
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIODEL NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINDEL',(R5))                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         ZIC   RF,MINNBUF          NUMBER OF BUFFERS BEING USED                 
         L     RE,MINBUFF          A(1ST BUFFER BEING USED BY MINIO)            
         USING RPROHDRD,RE                                                      
MNIODEL2 CLC   RPRORLEN,=Y(RPROR1ST-RPROHDRD)   ANY ELEMENTS IN RECORD?         
         BH    MNIODEL4                         YES, CHECK NEXT BUFFER          
*                                                                               
         MVC   RPRORLEN,=Y(RPROR1ST-RPROHDRD+2) DMDALINK: MIN IS 36             
         OI    RPRORSTA,X'80'                   MARK FOR DELETE                 
         LA    R1,RPROR1ST                                                      
         XC    0(3,R1),0(R1)                    FAKE ELEMENT                    
*                                                                               
MNIODEL4 AH    RE,MINFRCLM         BUMP TO NEXT MINIO BUFFER                    
         BCT   RF,MNIODEL2         LOOP UNTIL ALL BUFFERS CHECKED               
*                                                                               
MNIODELX B     EXITOK                                                           
         DROP  R5,RE                                                            
***********************************************************************         
* THIS ROUTINE CLOSES MINIO AND FLUSHES OUT THE BUFFERS TO THE MINIO            
* RECORDS.                                                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOCLS NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINCLS',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* REORGANIZATION OF  SAVBKS, SAVLBLS, & SAVDMOS  BASED ON PRIME                 
***********************************************************************         
REORGPRM NTR1                                                                   
         CLI   PRIMEBK,1           DO WE HAVE TO?                               
         BE    REORG50             NO                                           
*********                                                                       
* SAVBKS PORTION                                                                
*********                                                                       
         ZIC   R1,PRIMEBK          R1 = A(ENTRY THAT WILL BE PRIME)             
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVBK)                                                   
         LA    R1,SAVBKS(R1)                                                    
*                                                                               
         MVC   BOWORK1(L'SAVBK),0(R1)  MAKE A COPY OF PRIME                     
*                                                                               
         LA    R0,SAVBKS           R1 = # OF BYTES TO COPY FROM MINBKS          
         SR    R1,R0                                                            
*                                                                               
         MVC   SAVBKS(L'SAVBK),BOWORK1  OUR PRIME IS IN 1ST SPOT                
*                                                                               
         LR    RE,RA               SHIFT SO WHAT WAS 1ST IS NOW 2ND ...         
         AH    RE,=Y(MINBKS-TWAD)                                               
         BCTR  R1,0                SET UP FOR EX                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAVBKS+L'SAVBK(0),0(RE)                                          
*********                                                                       
* SAVLBLS PORTION                                                               
*********                                                                       
         ZIC   R1,PRIMEBK          R1 = A(ENTRY THAT WILL BE PRIME)             
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVLBL)                                                  
         LA    R1,SAVLBLS(R1)                                                   
*                                                                               
         MVC   BOWORK1(L'SAVLBL),0(R1)   MAKE A COPY OF PRIME                   
*                                                                               
         LA    R0,SAVLBLS          R1 = # OF BYTES TO COPY FROM MINLBLS         
         SR    R1,R0                                                            
*                                                                               
         MVC   SAVLBLS(L'SAVLBL),BOWORK1    OUR PRIME IS IN 1ST SPOT            
*                                                                               
         LR    RE,RA               SHIFT SO WHAT WAS 1ST IS NOW 2ND ...         
         AH    RE,=Y(MINLBLS-TWAD)                                              
         BCTR  R1,0                SET UP FOR EX                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAVLBLS+L'SAVLBL(0),0(RE)                                        
***************                                                                 
* REORGANIZATION OF  SAVDMOS  BASED ON PRIMEDM                                  
***************                                                                 
REORG50  CLI   PRIMEDM,1                                                        
         BE    REORGX                                                           
*                                                                               
         ZIC   R1,PRIMEDM          R1 = A(ENTRY THAT WILL BE PRIME)             
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVDMO)                                                  
         LA    R1,SAVDMOS(R1)                                                   
*                                                                               
         MVC   BOWORK1(L'SAVDMO),0(R1) MAKE A COPY OF PRIME                     
*                                                                               
         LA    R0,SAVDMOS          R1 = # OF BYTES TO COPY FROM MINDMOS         
         SR    R1,R0                                                            
*                                                                               
         MVC   SAVDMOS(L'SAVDMO),BOWORK1  OUR PRIME IS IN 1ST SPOT              
*                                                                               
         LR    RE,RA               SHIFT SO WHAT WAS 1ST IS NOW 2ND ...         
         AH    RE,=Y(MINDMOS-TWAD)                                              
         BCTR  R1,0                SET UP FOR EX                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAVDMOS+L'SAVDMO(0),0(RE)                                        
*                                                                               
REORGX   B     EXITOK                                                           
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
EXITCCHG MVC   FVMSGNO,=AL2(CSTCHNGD)                                           
         B     EXITL               EXIT WITH COST WAS ALREADY CHANGED           
TABCHNGD MVC   FVMSGNO,=AL2(BCPPCHGD)                                           
         B     EXITL               EXIT WITH BUYER'S CPP WAS CHANGED            
*                                                                               
DIE      DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
PFAVAIL  EQU   PFK03               PFKEY FOR AVAIL                              
PFPACKGE EQU   PFK04               PFKEY FOR PACKAGE                            
PFMBOOK  EQU   PFK05               PFKEY FOR MULTI-BOOK                         
PFMDEMO  EQU   PFK06               PFKEY FOR MULTI-DEMO                         
PFKYUP   EQU   PFK07               PFKEY FOR SCROLL UP                          
PFKYDOWN EQU   PFK08               PFKEY FOR SCROLL DOWN                        
PFKYLEFT EQU   PFK09               PFKEY FOR SCROLL LEFT                        
PFKYRGHT EQU   PFK10               PFKEY FOR SCROLL RIGHT                       
PFRETURN EQU   PFK12               PFKEY FOR RETURN                             
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE OPTIONS FIELD                                                        
***********************************************************************         
VALOPTNS DS    0H                                                               
         NMOD1 0,**VOPT**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         TM    FVIIND,FVIVAL                                                    
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1VWCHG   THE VIEW WAS CHANGED                         
*                                                                               
         XC    LDPTSEQS,LDPTSEQS                                                
         LA    RF,LDPTSEQS                                                      
         LA    RE,SAVDPTS                                                       
VALOPT02 MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,L'SAVDPT(RE)                                                  
         LA    R0,SAVDPTS+L'SAVDPTS                                             
         CR    RE,R0                                                            
         BL    VALOPT02                                                         
*                                                                               
         CLI   FVILEN,0                                                         
         BE    VALOPTX                                                          
*                                                                               
         L     RE,AIO4             CLEAR FOR PARSNIP                            
         LA    RF,480                                                           
         XCEFL                                                                  
*                                                                               
         GOTO1 VPARSNIP,BODMCB,(FVILEN,FVIFLD),(15,AIO4),0                      
*                                                                               
         CLI   8(R1),0                                                          
         BE    VALOPT05                                                         
         L     R1,8(R1)                                                         
         LA    R1,0(R1)                                                         
         LA    R0,FVIFLD                                                        
         SR    R1,R0                                                            
         STC   R1,FVERRNDX         INDEX OF WHERE ERROR IS                      
         B     EXITNV                                                           
*                                                                               
VALOPT05 CLI   4(R1),1                                                          
         BL    EXITNV                                                           
         CLI   4(R1),15                                                         
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(TOOMNYOP)   TOO MANY OPTION DATA                    
         B     EXITL                                                            
*                                                                               
         L     R3,AIO4             R3 = A(1ST PARSNIP FIELD)                    
         USING PSND,R3                                                          
*                                                                               
VALOPT10 CLI   PSNTAG,0            ANY MORE FIELDS?                             
         BE    VALOPT40            NO                                           
*                                                                               
         OC    PSNCOMP,PSNCOMP                                                  
         BZ    EXITNV                                                           
*                                                                               
         L     RE,PSNCOMP          CALCULATE WHERE TO POINT CURSOR              
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         STC   RE,FVERRNDX                                                      
*                                                                               
         CLI   PSNERR,0            COMPONENT IS IN ERROR?                       
         BNE   EXITNV                                                           
*                                                                               
         CLI   PSNTAG,PSNFLDQ      REGUALR FIELD COMPONENT                      
         BNE   EXITNV                                                           
*                                                                               
         CLI   PSNLEN,0            MISSING FIELD                                
         BE    EXITNV                                                           
*                                                                               
         L     RE,PSNCOMP                                                       
         ZIC   R1,PSNLEN                                                        
         STC   R1,BOWORK2+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'SPOTS'                                              
         BNE   *+12                                                             
         OI    FILTFLG1,FF1SPOTS                                                
         B     VALOPT30                                                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'HIDDEN'                                             
         BNE   *+12                                                             
         OI    FILTFLG1,FF1HIDN                                                 
         B     VALOPT30                                                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'DPT'                                                
         BNE   VALOPT12                                                         
*                                                                               
         LA    R3,PSNL(R3)         NEXT FIELD                                   
         CLI   PSNTAG,0            ANY MORE FIELDS?                             
         BE    EXITNV              NO                                           
*                                                                               
         OC    PSNCOMP,PSNCOMP                                                  
         BZ    EXITNV                                                           
*                                                                               
         L     RE,PSNCOMP          CALCULATE WHERE TO POINT CURSOR              
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         STC   RE,FVERRNDX                                                      
*                                                                               
         CLI   PSNERR,0            COMPONENT IS IN ERROR?                       
         BNE   EXITNV                                                           
*                                                                               
         CLI   PSNTAG,PSNVALQ      VALUE FIELD COMPONENT                        
         BNE   EXITNV                                                           
*                                                                               
         CLI   PSNLEN,0            MISSING FIELD                                
         BE    EXITNV                                                           
*                                                                               
         CLI   PSNLEN,8            TOO MANY?                                    
         BH    EXITNV                                                           
*                                                                               
         XC    LDPTSEQS,LDPTSEQS                                                
         L     RE,PSNCOMP          CALCULATE WHERE TO POINT CURSOR              
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LDPTSEQS(0),0(RE)                                                
*                                                                               
         B     VALOPT30                                                         
*                                                                               
VALOPT12 DS    0H                                                               
         MVC   FVMSGNO,=AL2(INVLOPTN)   OPTION NOT AVAILABLE                    
         B     EXITNV                                                           
*                                                                               
VALOPT30 DS    0H                                                               
         ZIC   RE,FVERRNDX         CURSOR KLUGE                                 
         ZIC   RF,PSNLEN                                                        
         LA    RE,1(RF,RE)                                                      
         STC   RE,FVERRNDX                                                      
*                                                                               
         LA    R3,PSNL(R3)         BUMP TO THE NEXT FIELD                       
         B     VALOPT10                                                         
         DROP  R3                                                               
*                                                                               
VALOPT40 MVI   FVERRNDX,0          NO MORE INDEX INTO FIELD NEEDED              
*                                                                               
VALOPTX  OI    FVIIND,FVIVAL       SET AS VALIDATED                             
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE DEMO FIELD                                                           
***********************************************************************         
VALDEMOS DS    0H                                                               
         NMOD1 0,**VDMO**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         TM    FVIIND,FVIVAL                                                    
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1VWCHG   THE VIEW WAS CHANGED                         
*                                                                               
         MVI   PRIMEDM,1                                                        
         CLI   FVILEN,0            ANY DEMO ENTERED HERE?                       
         BNE   VALDMO10            YES, PRIMEDM=1                               
*                                                                               
         L     R6,AIO5             NO, SHOW WHAT THE PRIMARY DEMO IS            
         USING DBLOCK,R6                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOM                                                    
         MVI   DBSELMED,C'T'                                                    
         LA    R5,BOWORK1                                                       
         XC    BOWORK1(50),BOWORK1                                              
         MVC   BOWORK1(L'SAVDMO-1),SAVDMO+1                                     
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         DROP  R6                                                               
*                                                                               
         GOTO1 VDEMOCON,BODMCB,(1,BOWORK1),(9,FVIFLD),(0,AIO5)                  
         OI    FVIIND,FVIVAL                                                    
         OI    FVOIND,FVOXMT                                                    
         B     VALDMOX                                                          
*                                                                               
VALDMO10 ZIC   R0,FVILEN                                                        
         LA    RF,FVIFLD                                                        
         CLI   0(RF),C'='                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'$'                                                       
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         GOTOX (VALDMOQ,AREPRO01),BODMCB,(C'Y',FVIHDR),(1,BOWORK1)              
         BL    EXITNV              NO, BAD DEMO                                 
*                                                                               
         LR    R2,RA               FIND THE PRIME DEMO NUMBER                   
         AH    R2,=Y(MINDMOS-TWAD)                                              
         LA    R1,1                START WITH THE FIRST ONE                     
*                                                                               
VALDMO20 CLC   BOWORK1(3),1(R2)    MATCH ON THIS 3-BYTE DEMO?                   
         BE    VALDMO30            YES                                          
         LA    R1,1(R1)                                                         
         LA    R2,L'MINDMO(R2)     BUMP TO NEXT MINDMOS ENTRY                   
         LR    R0,RA                                                            
         AH    R0,=Y(MINDMOS+L'MINDMOS-TWAD)                                    
         CR    R2,R0                                                            
         BNL   EXITNV              DEMO IS NOT PART OF OUR DEMOS LIST           
         B     VALDMO20                                                         
*                                                                               
VALDMO30 STC   R1,PRIMEDM          SAVE # OF WHERE IN THE DISPLAY LIST          
*                                                                               
VALDMOX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET DEFAULT COLUMNS 1                                                         
***********************************************************************         
DEFCLMNS DS    0H                                                               
         NMOD1 0,**DEFC**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         LA    RF,LSFIXCLM         RF = A(1ST FIXED COLUMN)                     
         USING DCTABD,RF                                                        
*                                                                               
         MVC   BOHALF1,=AL2(6)     FIXED COLS W/O STATION                       
         LR    RE,RA               DO WE HAVE A STATION IN THE KEY?  HE         
         AH    RE,=Y(SVSTATN-TWAD)                                              
         CLI   0(RE),0                                                          
         BNE   DMLC00              YES                                          
         MVC   DCTFLD#,=AL2(98)    STATION COLUMN                               
         LA    RF,DCTABL(RF)                                                    
         MVC   BOHALF1,=AL2(7)     FIXED COLS W/STATION                         
*                                                                               
DMLC00   MVC   DCTFLD#,=AL2(92)    DPT FIELD                                    
         LA    RF,DCTABL(RF)                                                    
         MVC   DCTFLD#,=AL2(93)    TOTAL SPOTS FIELD                            
         LA    RF,DCTABL(RF)                                                    
         MVC   DCTFLD#,=AL2(94)    TOTAL RATING FIELD                           
         LA    RF,DCTABL(RF)                                                    
         MVC   DCTFLD#,=AL2(95)    TOTAL CPP FIELD                              
         LA    RF,DCTABL(RF)                                                    
         MVC   DCTFLD#,=AL2(96)    TOTAL COST FIELD                             
         LA    RF,DCTABL(RF)                                                    
         MVC   DCTFLD#,=AL2(97)    TOTAL BUGET FIELD                            
*                                                                               
         MVC   LSFIXNUM,BOHALF1    # OF FIXED COLUMNS                           
         MVC   LSVARNUM,=AL2(0)    # OF VAR. COLUMNS                            
*                                                                               
DMLCX    B     EXITOK                                                           
         DROP  RF                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY BOOK FIELD                                                       
***********************************************************************         
VALKYBOK DS    0H                                                               
         NMOD1 0,**VKBK**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         TM    FVIIND,FVIVAL                                                    
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1VWCHG   THE VIEW WAS CHANGED                         
*                                                                               
         MVI   PRIMEBK,1                                                        
         CLI   FVILEN,0            ANY BOOK ENTERED HERE?                       
         BNE   VALBOK10            YES, VALIDATE IT                             
*                                                                               
         OC    SAVLBL,SAVLBL       NO, SHOW WHAT THE PRIMARY BOOK IS            
         BZ    *+14                                                             
         MVC   FVIFLD(L'SAVLBL),SAVLBL    USER DEFINED                          
         B     VALBOK05                                                         
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(3),SAVBKS+1  JUST SHOW THE 1ST (PRIMARY BOOK)            
         GOTOX (UNBOOKQ,AREPRO01),BODMCB,(1,BOWORK1),FVIHDR,0,         X        
               (C'+',=CL6' ')                                                   
*                                                                               
VALBOK05 OI    FVIIND,FVIVAL                                                    
         OI    FVOIND,FVOXMT                                                    
         B     VALBOKX                                                          
*                                                                               
VALBOK10 DS    0H                                                               
         GOTO1 VBOOKVAL,BODMCB,(C'N',FVIHDR),(1,BOWORK1),              X        
               (C'B',VSCANNER),,                                                
         CLI   4(R1),0             GOOD BOOK?                                   
         BE    VALBOK50            NO, COULD BE USER-DEFINED BOOK               
*                                                                               
         LR    R2,RA               FIND THE PRIME BOOK NUMBER                   
         AH    R2,=Y(MINBKS-TWAD)                                               
         LA    R1,1                START WITH THE FIRST ONE                     
*                                                                               
VALBOK20 CLC   BOWORK1(3),1(R2)    MATCH ON THIS 3-BYTE BOOK?                   
         BE    VALBOK60            YES                                          
         LA    R1,1(R1)                                                         
         LA    R2,L'MINBK(R2)      BUMP TO NEXT MINBKS ENTRY                    
         LR    R0,RA                                                            
         AH    R0,=Y(MINBKS+L'MINBKS-TWAD)                                      
         CR    R2,R0                                                            
         BNL   EXITNV              BOOK IS NOT PART OF OUR BOOKS LIST           
         B     VALBOK20                                                         
*                                                                               
VALBOK50 LR    R2,RA                                                            
         AH    R2,=Y(MINLBLS-TWAD)                                              
         LA    R1,1                                                             
         OC    FVIFLD(8),BCSPACES                                               
*                                                                               
VALBOK55 CLC   FVIFLD(L'MINLBL),0(R2)   MATCH ON THIS USER-DEFINED LBL?         
         BE    VALBOK60                 YES                                     
         LA    R1,1(R1)                                                         
         LA    R2,L'MINLBL(R2)                                                  
         LR    R0,RA                                                            
         AH    R0,=Y(MINLBLS+L'MINLBLS-TWAD)                                    
         CR    R2,R0                                                            
         BNL   EXITNV              BOOK IS NOT PART OF OUR BOOKS LIST           
         B     VALBOK55                                                         
*                                                                               
VALBOK60 STC   R1,PRIMEBK          SAVE # OF WHERE IN THE DISPLAY LIST          
*                                                                               
VALBOKX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READS THE BOOK AND DEMO ELEMENTS INTO SAVBKS, SAVLBLS, SAVDMOS                
*                                       MINBKS, MINLBLS, MINDMOS                
***********************************************************************         
RDBKSDMS DS    0H                                                               
         NMOD1 0,**BKDM**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
***************                                                                 
* BOOK ELEMENT(S)                                                               
***************                                                                 
         XC    SAVBKS,SAVBKS                                                    
         XC    SAVLBLS,SAVLBLS                                                  
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ    GET THE BOOK ELEMENT                         
         BAS   RE,MINIOHI                                                       
         BNE   RDBDBKX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRBKELD,R6                                                      
*                                                                               
RDBDBK10 CLI   0(R6),RPRBKELQ                                                   
         BNE   RDBDBKX                                                          
*                                                                               
         ZIC   R1,RPRBKDOR         DISPLAY ORDER NUMBER                         
         BCTR  R1,0                                                             
         LR    RE,R1                                                            
         MH    R1,=Y(L'SAVBK)                                                   
         LA    R1,SAVBKS(R1)                                                    
*                                                                               
         MVC   0(L'RPRBKIOR,R1),RPRBKIOR                                        
         MVC   L'SAVBK-1(1,R1),RPRBKFLG                                         
*                                                                               
         CLI   RPRBKLEN,RPRBKOVQ   USER DEFINED BOOK?                           
         BH    RDBDBK20            YES                                          
*********                                                                       
* REGULAR BOOK                                                                  
*********                                                                       
         MVC   1(L'RPRBKSTT,R1),RPRBKSTT                                        
         MVC   1+L'RPRBKSTT(L'RPRBKBYM,R1),RPRBKBYM                             
         B     RDBDBK50                                                         
*********                                                                       
* USER-DEFINED BOOK                                                             
*********                                                                       
RDBDBK20 MVC   1+L'RPRBKSTT+L'RPRBKBYM(RPRBKUPG-RPRBKBKS,R1),RPRBKBKS           
         MH    RE,=Y(L'SAVLBL)                                                  
         LA    RE,SAVLBLS(RE)                                                   
         MVC   0(L'SAVLBL,RE),RPRBKUDF  SAVE THE LABEL                          
*                                                                               
RDBDBK50 BAS   RE,MINIOSEQ                                                      
         BE    RDBDBK10                                                         
*                                                                               
RDBDBKX  LR    RE,RA               SAVE THESE SO WE KNOW WHAT CHANGED           
         AH    RE,=Y(MINBKS-TWAD)                                               
         MVC   0(L'MINBKS,RE),SAVBKS                                            
         LR    RE,RA                                                            
         AH    RE,=Y(MINLBLS-TWAD)                                              
         MVC   0(L'MINLBLS,RE),SAVLBLS                                          
***************                                                                 
* DEMO ELEMENT(S)                                                               
***************                                                                 
         XC    SAVDMOS,SAVDMOS                                                  
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDMELQ    GET THE DEMO ELEMENT                         
         BAS   RE,MINIOHI                                                       
         BNE   RDBDDMX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDMELD,R6                                                      
*                                                                               
RDBDDM10 CLI   0(R6),RPRDMELQ                                                   
         BNE   RDBDDMX                                                          
*                                                                               
         ZIC   R1,RPRDMDOR         DISPLAY ORDER NUMBER                         
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVDMO)                                                  
         LA    R1,SAVDMOS(R1)                                                   
*                                                                               
         MVC   0(L'RPRDMIOR,R1),RPRDMIOR                                        
         MVC   1(L'SAVDMO-1,R1),RPRDMBY1                                        
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         BE    RDBDDM10                                                         
*                                                                               
RDBDDMX  LR    RE,RA                                                            
         AH    RE,=Y(MINDMOS-TWAD)                                              
         MVC   0(L'MINDMOS,RE),SAVDMOS                                          
         DROP  R6                                                               
***************                                                                 
* DAYPART ELEMENT(S)                                                            
***************                                                                 
RDBDDP00 XC    SAVDPTS,SAVDPTS                                                  
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDPELQ    GET THE DAYPART ELEMENT                      
         BAS   RE,MINIOHI                                                       
         BNE   RDBDDPX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDPELD,R6                                                      
         LA    R1,SAVDPTS                                                       
*                                                                               
RDBDDP10 CLI   0(R6),RPRDPELQ      DISPLAY ORDER NUMBER                         
         BNE   RDBDDPX                                                          
*                                                                               
         MVC   0(L'RPRDMIOR,R1),RPRDPDPT                                        
         LA    RE,CSARDPT                                                       
RDBDDP15 CLC   0(L'RPRDPDPT,RE),RPRDPDPT                                        
         BE    RDBDDP20                                                         
         LA    RE,3(RE)                                                         
         LA    R0,CSARDPT+L'CSARDPT                                             
         CR    RE,R0                                                            
         BL    RDBDDP15                                                         
         B     RDBDDP30                                                         
*                                                                               
RDBDDP20 MVC   1(4,R1),1(RE)                                                    
*                                                                               
RDBDDP30 DS    0H                                                               
         LA    R1,L'SAVDPT(R1)                                                  
         BAS   RE,MINIOSEQ                                                      
         BE    RDBDDP10                                                         
*                                                                               
RDBDDPX  LR    RE,RA                                                            
         AH    RE,=Y(MINDPTS-TWAD)                                              
         MVC   0(L'MINDPTS,RE),SAVDPTS                                          
         DROP  R6                                                               
***************                                                                 
* STATION ELEMENT(S)                                                            
***************                                                                 
RDBDST00 XC    SAVSTAS,SAVSTAS                                                  
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRSTELQ    GET THE STATION ELEMENT                      
         BAS   RE,MINIOHI                                                       
         BNE   RDBDSTX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRSTELD,R6                                                      
*                                                                               
RDBDST10 CLI   0(R6),RPRSTELQ                                                   
         BNE   RDBDSTX                                                          
*                                                                               
         ZIC   R1,RPRSTICD                                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R1,SAVSTAS(R1)                                                   
         MVC   0(L'SAVSTA,R1),RPRSTSTA                                          
*                                                                               
         TM    RPRSTFLG,RPRSTSTL   SATELLITE STATION?                           
         BZ    RDBDST20                                                         
         MVI   4(R1),C'1'          YES, C'1' AFTER STATION CALL LTRS            
*                                                                               
RDBDST20 BAS   RE,MINIOSEQ                                                      
         BE    RDBDST10                                                         
*                                                                               
RDBDSTX  LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS-TWAD)                                              
         MVC   0(L'MINSTAS,RE),SAVSTAS                                          
         DROP  R6                                                               
*                                                                               
RDBKDMX  B     EXITOK                                                           
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* AFTER VALIDATING THE KEY FIELDS                                               
***********************************************************************         
KLSTKVAL DS    0H                                                               
         NMOD1 0,**KLKV**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
*                                                                               
KLKVX    B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BEFORE DISPLAYING THE DATA FIELDS                                             
***********************************************************************         
D1STDDIS DS    0H                                                               
         NMOD1 0,**DFDD**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         OC    0(L'SVCONNUM,RE),0(RE)    ANY CONTRACT NUMBER?                   
         BZ    DFDDISX                                                          
         CLI   SVPRONUM-SVCONNUM(RE),0    ANY PROPOSAL NUMBER?                  
         BZ    DFDDISX                                                          
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ                                                 
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DFDDIS00 L     R6,MINELEM                                                       
         CLI   0(R6),RPRDPELQ                                                   
         BH    DFDDIS20            NO MORE, DON'T NEED TO FETCH AGAIN           
*                                                                               
         CLI   0(R6),RPRBKELQ                                                   
         BNE   DFDDIS02                                                         
         USING RPRBKELD,R6                                                      
         TM    RPRBKFLG,RPRBKFFT   FETCHED FOR THIS BOOK ALREADY?               
         BZ    DFDDIS10            NO, GO DO IT THEN                            
         B     DFDDIS09            YES                                          
*                                                                               
DFDDIS02 CLI   0(R6),RPRDMELQ                                                   
         BNE   DFDDIS04                                                         
         USING RPRDMELD,R6                                                      
         TM    RPRDMFLG,RPRDMFFT   FETCHED FOR THIS DEMO ALREADY?               
         BZ    DFDDIS10            NO, GO DO IT THEN                            
         B     DFDDIS09            YES                                          
*                                                                               
DFDDIS04 CLI   0(R6),RPRSTELQ                                                   
         BNE   DFDDIS06                                                         
         USING RPRSTELD,R6                                                      
         TM    RPRSTFLG,RPRSTFFT   FETCHED FOR THIS STATION ALREADY?            
         BZ    DFDDIS10            NO, GO DO IT THEN                            
         B     DFDDIS09            YES                                          
*                                                                               
DFDDIS06 CLI   0(R6),RPRDPELQ                                                   
         BNE   DFDDIS09                                                         
         USING RPRDPELD,R6                                                      
         TM    RPRDPFLG,RPRDPFFT   FETCHED FOR THIS DPT ALREADY?                
         BZ    DFDDIS10            NO, GO DO IT THEN                            
*                                                                               
DFDDIS09 BAS   RE,MINIOSEQ                                                      
         BNE   DFDDIS20                                                         
         B     DFDDIS00                                                         
         DROP  R6                                                               
***************                                                                 
* CALL 'T80A24' TO DO THE FETCH                                                 
***************                                                                 
DFDDIS10 GOTO1 VCOLY,BODMCB,(X'24',0),(0,0)                                     
         CLI   BODMCB+4,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,BODMCB                                                        
         GOTO1 (RF),BODMCB,(R9)                                                 
         BL    EXITL                                                            
         OI    LSSCIND1,LSSCIBLD   REBUILD THE LIST ALSO                        
***********************************                                             
* GOOD TIME TO PUT VALUES INTO OUR SAVED VARIABLES FROM THE RECORD              
***********************************                                             
***************                                                                 
* DESCRIPTION ELEMENT                                                           
***************                                                                 
DFDDIS20 MVI   SAVOPTNS,0                                                       
         XC    SAVSLNS,SAVSLNS                                                  
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDSELQ    GET THE DESCRIPTION ELEMENT                  
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                BETTER HAVE ONE                              
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDSELD,R6                                                      
         TM    RPRDSOPT,RPRDSOTX   DISPLAY TEXT FROM INVENTORY RECS?            
         BZ    *+8                                                              
         OI    SAVOPTNS,OPTNTXTQ   YES                                          
         TM    RPRDSOPT,RPRDSODC   USE DEMO PRECISION?                          
         BZ    *+8                                                              
         OI    SAVOPTNS,OPTNDECQ   YES                                          
         MVC   SAVSLNS,RPRDSSEC    COPY THE SPOT LENGTHS                        
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(RDBKSDMS),BODMCB,(R9),RR=BORELO                               
*                                                                               
         BAS   RE,REORGPRM         REORG  SAVBKS, SAVLBLS, & SAVDMOS            
*                                                                               
DFDDISX  B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
LISTABL  DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LDEFCLM),AL1(0,0,1),AL4(DEFCLM1)                             
         DC    AL1(LTSARDIR),AL1(0,0,1),AL4(TSARDIR1)                           
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(EOT)                                                         
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
KNOWTAB  DS    0XL(KNOWLQ)                                                      
* KEY (UNPROTECTED PORTION)                                                     
         DC    AL2(00001),AL4(CONDTA)    CONTRACT                               
         DC    AL2(00002),AL4(PRODTA)    PROPOSAL                               
         DC    AL2(00008),AL4(STADTA)    STATION                                
         DC    AL2(00011),AL4(OPTDTA)    OPTIONS                                
         DC    AL2(00013),AL4(BOKDTA)    BOOK                                   
         DC    AL2(00014),AL4(DMODTA)    DEMO                                   
* KEY (PROTECTED PORTION)                                                       
         DC    AL2(00003),AL4(AGYDTA)    AGENCY                                 
         DC    AL2(00004),AL4(ADVDTA)    ADVERTISER                             
         DC    AL2(00005),AL4(PRDDTA)    PRODUCT                                
         DC    AL2(00006),AL4(SALDTA)    SALESPERSON                            
         DC    AL2(00007),AL4(BYRDTA)    BUYER                                  
         DC    AL2(00009),AL4(FLTDTA)    FLIGHT DATES                           
         DC    AL2(00010),AL4(DSKDTA)    DISK ADDRESS                           
         DC    AL2(00087),AL4(DVSDTA)    DEVELOPMENT SALESPERSON                
         DC    AL2(00088),AL4(DVTDTA)    DEVELOPMENT CONTRACT TYPE              
* RECORD LIST                                                                   
         DC    AL2(00092),AL4(LDPTDTA)   DPT                                    
         DC    AL2(00093),AL4(LSPTDTA)   TOTAL SPOTS                            
         DC    AL2(00094),AL4(LRTGDTA)   TOTAL POINTS                           
         DC    AL2(00095),AL4(LCSTDTA)   TOTAL COST                             
         DC    AL2(00096),AL4(LCPPDTA)   TOTAL CPP/M                            
         DC    AL2(00097),AL4(LNDPDTA)   # OF DETAIL LINES                      
         DC    AL2(00098),AL4(LSTADTA)   STATION CALL LETTERS                   
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 FIELD NUMBER                                 
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT FOR RECAP LINES BUILT IN AIO1                                           
***********************************************************************         
RECAPLN  DSECT                                                                  
RCPDPT   DS    CL1                 DAYPART                    92                
RCPSTA   DS    XL1                 STATION INTERNAL CODE      98                
RCPCPP   DS    F                   TOTAL CPP/M                96                
RCPRTG   DS    F                   TOTAL RATING POINTS        94                
RCPSPTS  DS    F                   TOTAL SPOTS                93                
RCPCOST  DS    F                   TOTAL COST                 95                
RCPNDP   DS    H                   # DETAILS                  97                
RCPLNQ   EQU   *-RECAPLN                                                        
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
AVDIC    DS    A                                                                
ADDIC    DS    A                                                                
AFRREL   DS    A                                                                
AFVADDR  DS    A                                                                
ESCLEN   DS    XL1                                                              
ESCCHAR  DS    XL2                                                              
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS, SET #1                  
MF1KYCHG EQU   X'80'                - KEY FIELD CHANGED                         
MF1PFRET EQU   X'40'                - RETURNING FROM CALLED SESSION             
MF1VWCHG EQU   X'20'                - VIEW WAS CHANGED                          
MF1GLOBR EQU   X'02'                - CAME BACK FROM GLOBBER                    
MF1TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
FILTFLG1 DS    XL1                 FILTER FLAGS                                 
FF1SPOTS EQU   X'80'                - FILTER FOR LINES WITH SPOTS               
FF1HIDN  EQU   X'40'                - FILTER FOR HIDDEN LINES                   
*                                                                               
LDPTSEQS DS    CL8                 DAYPART LIST SEQUENCE TABLE                  
*                                                                               
PRIMEBK  DS    X                   INTERNAL ORD # OF DISPLAYED BOOK             
PRIMEDM  DS    X                   INTERNAL ORD # OF DISPLAYED DEMO             
*                                                                               
PRMBOOKQ EQU   41                  FIELD NUMBER FOR PRIME BOOK                  
PRMDEMOQ EQU   51                  FIELD NUMBER FOR PRIME DEMO                  
PRMDCPPQ EQU   61                  FIELD NUMBER FOR PRIME DEMO'S CPP            
*                                                                               
*                                                                               
PREVRTG  DS    XL4                 PREVIOUS RATING                              
*                                                                               
PCKOF06B DS    PL6                 PACKED OF 6  BYTES                           
PCKOF08B DS    PL8                 PACKED OF 8  BYTES                           
PCKOF16B DS    PL16                PACKED OF 16 BYTES                           
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE AREA                          
*                                                                               
SAVSTAS  DS    0CL(NUMSTAS*5)            SAVED 5-BYTE CALL LETTERS              
SAVSTA   DS    (NUMSTAS)CL5                                                     
*                                                                               
SAVBKS   DS    0XL(7*(1+3+12+1))     SAVED 1-BYTE INTERNAL ORDER NUMBER         
SAVBK    DS    7XL(1+3+12+1)               3-BYTE BOOK                          
*                                         12-BYTE UPGRADE                       
*                                          1-BYTE FLAG                          
*                                                                               
SAVLBLS  DS    0CL(7*5)            SAVED LABELS FOR USER DEFINED BOOKS          
SAVLBL   DS    7CL5                 - NULL: EMPTY OR LABEL                      
*                                                                               
SAVDMOS  DS    0CL(7*(1+3))        SAVED 1-BYTE INTERNAL ORDER NUMBER           
SAVDMO   DS    7CL(1+3)                  3-BYTE DEMO                            
*                                                                               
SAVDPTS  DS    0CL(8*(1+4))         - 1 BYTE DAYPART CODE                       
SAVDPT   DS    8CL(1+4)             - 4 BYTE BYR CPP                            
*                                                                               
SAVSLNS  DS    0CL(6*1)            SAVED 1-BYTE SPOT LENGTHS                    
SAVSLN   DS    6XL1                                                             
*                                                                               
SAVOPTNS DS    XL1                 OPTIONS                                      
OPTNTXTQ EQU   X'80'                - TEXT BIT                                  
OPTNDECQ EQU   X'40'                - DEMO DECIMAL PRECISION BIT                
*                                                                               
SVRECDA  DS    XL(L'GSRECDA)                                                    
SVMINEKY DS    XL(L'RPROKMEL)      SAVED MINIO ELEMENT KEY                      
         EJECT                                                                  
* REPROWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REPROWORK                                                      
         PRINT ON                                                               
TWAD     DSECT                                                                  
***********************************                                             
* SAVE AREA - SAVED/RESTORED BETWEEN NTRSES                                     
***********************************                                             
         ORG   SVMORE                                                           
*RNDTCST DS    PL16                GRAND TOTAL COST                             
*RNDTRTG DS    PL8                 GRAND TOTAL RATING                           
*RNDTSPT DS    PL6                 GRAND TOTAL SPOTS                            
*RNDTCPP DS    PL16                GRAND TOTAL COST PER POINT                   
*RNDTDTL DS    PL3                 GRAND TOTAL NUMBER OF DETAILS                
         ORG   TWUSER                                                           
***********************************                                             
* SAVE AREA EXCEPT BETWEEN NTRSES                                               
***********************************                                             
MINSTAS  DS    0XL(4*5)             - 5-BYTE CALL LETTERS                       
MINSTA   DS    4XL5                                                             
*                                                                               
MINBKS   DS    0XL(7*(1+3+12+1))      - 1 BYTE  INTERNAL ORDER NUMBER           
MINBK    DS    7XL(1+3+12+1)          - 3 BYTE  BOOK                            
*                                     - 12 BYTE UPGRADE                         
*                                     - 1 BYTE FLAG                             
*                                                                               
MINLBLS  DS    0CL(7*5)            LABELS FOR USER DEFINED BKS (MINIO)          
MINLBL   DS    7CL5                 - NULL: EMPTY OR LABEL                      
*                                                                               
MINDMOS  DS    0CL(7*(1+3))         - 1 BYTE INTERNAL ORDER NUMBER              
MINDMO   DS    7CL(1+3)             - 3 BYTE DEMO                               
*                                                                               
MINDPTS  DS    0CL(8*(1+4))         - 1 BYTE DAYPART CODE                       
MINDPT   DS    8CL(1+4)             - 4 BYTE BYR CPP                            
******                                                                          
         DS    XL(TWUSER+L'TWUSER-*)   # OF SPARE BEFORE TWSECBLK               
         DS    0X                                                               
* FAFACTS                                                                       
* FASYSLSTD                                                                     
* DDDDEQUS                                                                      
* CTMSGEQUS                                                                     
* FASELIST                                                                      
* FASYSFAC                                                                      
* DDSCANBLKD                                                                    
* DDFLDHDR                                                                      
* DDCOMFACS                                                                     
* DDGLVXCTLD                                                                    
* DDGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE DDDDEQUS                                                       
       ++INCLUDE CTMSGEQUS                                                      
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
         EJECT                                                                  
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKEYD   DS    0X                                                               
TLKDIDSQ DS    XL1                 DAYPART SEQUENCE #                           
TLKDIDPT DS    CL1                 DPT CODE                                     
TLKDISTA DS    CL5                 STATION CALL LETTERS                         
         ORG   TLUSER                                                           
TLRECD   DS    0X                                                               
*                                                                               
TRCPDPT  DS    CL1                 DAYPART                    92                
TRCPSTA  DS    XL1                 STATION INTERNAL CODE      98                
TRCPCPP  DS    F                   TOTAL CPP/M                96                
TRCPRTG  DS    F                   TOTAL RATING POINTS        94                
TRCPSPTS DS    F                   TOTAL SPOTS                93                
TRCPCOST DS    F                   TOTAL COST                 95                
TRCPNDP  DS    H                   # DETAILS                  97                
*                                                                               
TSARLNQ  EQU   *-TLSTD             LENGTH OF TSAR RECORD                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002REPRO16   08/16/00'                                      
         END                                                                    
