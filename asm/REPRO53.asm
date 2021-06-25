*          DATA SET REPRO53    AT LEVEL 007 AS OF 03/04/98                      
*&&      SET   NOP=N                                                            
*PHASE T80A53C                                                                  
T80A53   TITLE 'REPRO53 - REP PROPOSALS WORK/COMP'                              
PRO53    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REPRO53*,R7,RR=RE                                              
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
OBJECT   DS    0H                                                               
         L     R1,SVPARMS                                                       
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
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                                
***********************************************************************         
INIT     DS    0H                                                               
         OI    TWASRVH+1,X'01'     SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    TWASRVH+6,X'80'                                                  
         OI    GCINDS1,GCIPROT             UNPROT ON NTRSES                     
         OI    GSINDSL1,GSINOIO+GSIXKEY    WE'LL DO THE IO'S                    
         OI    LSSTAT1,LSSBALL     BUILD ALL OF LIST IN ONE GO                  
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
         NI    MISCFLG1,X'FF'-MF1KYCHG                                          
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
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
         B     EXITOK                                                           
         DROP  R2                                                               
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
*        DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
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
         AH    RF,=Y(KNOWTAB-PRO53)                                             
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
         GOTOX (VALCONQ,AREPRO01),BOPARM                                        
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
         GOTOX (VALPROQ,AREPRO01),BOPARM                                        
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
         GOTOX (MNIOINQ,AREPRO01),BOPARM INITIALIZE MINIO                       
*                                                                               
         GOTO1 =A(RDBKSDMS),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
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
         MVC   FVIFLD(L'CCONDVT),CCONDVT                                        
         B     EXITOK                                                           
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
DISSTA10 ZIC   R1,0(RE)            LAST STATION USED BY USER                    
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R1,SAVSTAS(R1)                                                   
         USING STALIN,R1                                                        
         MVC   FVIFLD(L'STLNSTA),STLNSTA                                        
         DROP  R1                                                               
*                                                                               
DISSTAX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE STATION FIELD                                                        
***********************************************************************         
VALSTA   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THE KEY HAS CHANGED                      
*                                                                               
         CLI   FVILEN,0            ANY STATION?                                 
         BNE   VALSTA00                                                         
*                                                                               
         TM    MISCFLG1,MF1PFCOM   NONE, CAME FROM ANOTHER SESSION?             
         BNZ   VALSTAX             YES, SVSTATN SHOULD BE SET THEN              
         TM    MISCFLG1,MF1KYCHG   NO, WAS THE KEY CHANGED?                     
         BZ    EXITNO                                                           
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         MVI   0(RE),1                                                          
         B     DISSTA10            <=== SHOW THE STATION                        
*                                                                               
VALSTA00 CLI   FVILEN,6            MORE THAN 5 LETTERS FOR STATION?             
         BH    EXITNV              YES, INVALID STATION (FOR NOW)               
*                                                                               
         OC    FVIFLD,BCSPACES                                                  
         LA    R1,1                                                             
         LA    RE,SAVSTAS                                                       
         USING STALIN,RE                                                        
VALSTA10 CLC   STLNSTA,FVIFLD                                                   
         BE    VALSTA20                                                         
         LA    RE,L'SAVSTA(RE)                                                  
         LA    R1,1(R1)                                                         
         LA    RF,SAVSTAS+L'SAVSTAS                                             
         CR    RE,RF                                                            
         BL    VALSTA10                                                         
         B     EXITNV                                                           
         DROP  RE                                                               
*                                                                               
VALSTA20 LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
*&&DO                                                                           
         CLI   TWASESNL,1               ANY PREVIOUS SESSION?                   
         BE    VALSTA30                 NO                                      
         TM    MISCFLG1,MF1PFCOM        JUST COMING IN NOW?                     
         BNZ   VALSTA30                 YES, NEED TO SAVE IT ONCE               
         CLM   R1,1,0(RE)               NO, THIS FLD SHOULDN'T CHANGE           
         BNE   EXITPF12                                                         
*&&                                                                             
VALSTA30 STC   R1,0(RE)              SAVE INTERNAL ORDER # OF STATION           
*                                                                               
VALSTAX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DAYPART FIELD                                                 
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DPTDTA   DS    0H                                                               
         LA    RF,DPTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DPTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDPT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDPT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DAYPART FIELD                                                         
***********************************************************************         
DISDPT   DS    0H                                                               
         MVC   FVIFLD(L'SVKYDYPT),SVKYDYPT                                      
DISDPTX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE DAYPART FIELD                                                        
***********************************************************************         
VALDPT   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         CLI   FVILEN,0            THIS FIELD IS REQUIRED                       
         BNE   VALDPT00                                                         
         TM    MISCFLG1,MF1PFCOM                                                
         BZ    EXITNO                                                           
         MVC   FVIFLD(L'SVKYDYPT),SVKYDYPT                                      
         MVI   FVILEN,L'SVKYDYPT                                                
*                                                                               
VALDPT00 TM    CCONFLG1,CCONDPMQ   USES HARDCODED TABLE?                        
         BO    VALDPT60            YES                                          
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RRDPKEY,RE                                                       
         MVI   RRDPKTYP,RRDPKIDQ                                                
         MVC   RRDPKREP,CUAALF                                                  
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(RRDPKDPT-RRDPKEY),IOKEYSAV                                 
         BNE   VALDPT60            NO DAYPART RECORDS                           
*                                                                               
****************************************                                        
** READ DAYPART RECORD FOR VALIDATION **                                        
****************************************                                        
         CLI   FVILEN,L'RRDPKDPT                                                
         BNE   EXITNV                                                           
*                                                                               
         MVC   IOKEY+RRDPKDPT-RRDPKEY(L'RRDPKDPT),FVIFLD                        
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RRDPKEY),IOKEYSAV                                        
         BNE   EXITNV              INVALID DAYPART                              
         B     VALDPT90                                                         
*                                                                               
*************************************************                               
** READ HARDCODED DAYPART TABLE FOR VALIDATION **                               
*************************************************                               
VALDPT60 DS    0H                                                               
         LA    RE,DPTTABLE            VALIDATE THE DAYPART CODE                 
         ZIC   R3,FVXLEN              R3 = LENGTH OF INPUT -1                   
*                                                                               
VALDPT65 CLI   0(RE),X'FF'         DID WE HIT THE END OF DPTTABLE?              
         BE    EXITNV              YES, INVALID DAYPART CODE                    
*                                                                               
         EX    R3,*+8              VALID 1-BYTE DAYPART CODE?                   
         B     *+10                                                             
         CLC   FVIFLD(0),3(RE)                                                  
         BE    VALDPT90                                                         
         LA    RE,L'DPTTABLE(RE)                                                
         B     VALDPT65                                                         
*                                                                               
VALDPT90 DS    0H                                                               
         LR    RE,RA               SAVE THE DAYPART ENTERED                     
         AH    RE,=Y(DPTINKEY-TWAD)                                             
*&&DO                                                                           
         CLI   TWASESNL,1               ANY PREVIOUS SESSION?                   
         BE    VALDPT30                 NO                                      
         TM    MISCFLG1,MF1PFCOM        JUST COMING IN NOW?                     
         BNZ   VALDPT30                 YES, NEED TO SAVE IT ONCE               
         CLC   0(L'DPTINKEY,RE),FVIFLD  NO, THIS FLD SHOULDN'T CHANGE           
         BNE   EXITPF12                                                         
*&&                                                                             
VALDPT30 MVC   0(L'DPTINKEY,RE),FVIFLD                                          
         MVC   SVKYDYPT,FVIFLD                                                  
*                                                                               
VALDPTX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
DPTTABLE DS    0CL4                                                             
         DC    CL4'MNGM'           MORNING                                      
         DC    CL4'DAYD'           DAYTIME                                      
         DC    CL4'ELYE'           EARLY FRINGE                                 
         DC    CL4'ENWR'           EARLY NEWS                                   
         DC    CL4'ACCA'           PRIME ACCESS                                 
         DC    CL4'LNWT'           LATE NEWS                                    
         DC    CL4'LTEL'           LATE FRINGE                                  
         DC    CL4'WKDW'           WEEKEND                                      
         DC    CL4'KIDK'           KIDS                                         
         DC    CL4'FRGF'           FRINGE                                       
         DC    CL4'NWSN'           NEWS                                         
         DC    CL4'PRIP'           PRIME                                        
         DC    CL4'MOVV'           MOVIES                                       
         DC    CL4'SPES'           SPECIALS                                     
         DC    CL4'SPOJ'           SPORTS                                       
         DC    CL4'SPSO'           SOAPS                                        
         DC    CL4'COMU'           COMPETITIVE                                  
         DC    CL4'LOCX'           LOCAL                                        
         DC    CL4'OTHY'           OTHER                                        
         DC    X'FF'               END OF TABLE                                 
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DAY/TIME FIELD                                                
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DTMDTA   DS    0H                                                               
         LA    RF,DTMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DTMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDTM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDTM)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DAY/TIME FIELD                                                        
***********************************************************************         
DISDTM   DS    0H                                                               
         MVC   FVIFLD(L'SVKYDYTM),SVKYDYTM                                      
DISDTMX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE DAY/TIME FIELD                                                       
***********************************************************************         
VALDTM   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         CLI   FVILEN,0            THIS IS A REQUIRED FIELD                     
         BNE   VALDTM06                                                         
*                                                                               
         TM    MISCFLG1,MF1PFCOM   NOTHING, FROM ANOTHER SESSION?               
         BZ    EXITNO                                                           
         MVC   FVIFLD(L'SVKYDYTM),SVKYDYTM                                      
*                                                                               
         LA    RF,FVIFLD+L'SVKYDYTM-1                                           
VALDTM02 CLI   0(RF),C' '                                                       
         BH    VALDTM04                                                         
         BCTR  RF,0                                                             
         B     VALDTM02                                                         
VALDTM04 LA    RF,1(RF)                                                         
         LA    RE,FVIFLD                                                        
         SR    RF,RE                                                            
         STC   RF,FVILEN                                                        
*                                                                               
VALDTM06 L     RE,AIO4                                                          
         LA    RF,480                                                           
         XCEFL                                                                  
*                                                                               
         GOTO1 VSCANNER,BODMCB,FVIHDR,(X'83',AIO4),C',=/='                      
         CLI   4(R1),2             SHOULD HAVE 2 COMPONENTS                     
         BNE   EXITNV                NO MORE, NO LESS                           
*                                                                               
         L     R3,AIO4                                                          
         CLI   1(R3),0                                                          
         BNE   EXITNV                                                           
         CLI   0(R3),0                                                          
         BE    EXITNO                                                           
*                                                                               
         GOTO1 VDAYVAL,BODMCB,(0(R3),12(R3)),BOBYTE2,BOBYTE1                    
         CLI   0(R4),0             DAYVAL COMPLAINS IF INPUT > 11               
         BE    EXITNV                                                           
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(DAYINKEY-TWAD)                                             
*&&DO                                                                           
         CLI   TWASESNL,1               ANY PREVIOUS SESSION?                   
         BE    VALDTM08                 NO                                      
         TM    MISCFLG1,MF1PFCOM        JUST COMING IN NOW?                     
         BNZ   VALDTM08                 YES, NEED TO SAVE IT ONCE               
         CLC   0(L'DAYINKEY,RE),BOBYTE2 NO, THIS FLD SHOULDN'T CHANGE           
         BNE   EXITPF12                                                         
*&&                                                                             
VALDTM08 MVC   0(L'DAYINKEY,RE),BOBYTE2                                         
*                                                                               
VALDTM10 LA    R3,32(R3)                                                        
*                                                                               
         CLI   1(R3),0                                                          
         BNE   EXITNV                                                           
         CLI   0(R3),0                                                          
         BNE   VALDTM20                                                         
         MVC   FVERRNDX,4(R3)     WE NEED THE TIME PORTION ALSO                 
         B     EXITNO                                                           
*                                                                               
VALDTM20 GOTO1 VTIMVAL,BODMCB,(0(R3),12(R3)),BOFULL1                            
         CLI   0(R1),X'FF'                                                      
         BNE   *+14                                                             
         MVC   FVERRNDX,4(R3)     POINT TO THE TIME PORTION                     
         B     EXITNV                 THAT'S IN ERROR                           
*                                                                               
         GOTOX (PCKTIMQ,AREPRO01),BODMCB,BOFULL1,BOFULL1+2,            X        
               BOFULL2                                                          
*                                                                               
         LR    R6,RA                                                            
         AH    R6,=Y(TIMINKEY-TWAD)                                             
*&&DO                                                                           
         CLI   TWASESNL,1               ANY PREVIOUS SESSION?                   
         BE    VALDTM30                 NO                                      
         TM    MISCFLG1,MF1PFCOM        JUST COMING IN NOW?                     
         BNZ   VALDTM30                 YES, NEED TO SAVE IT ONCE               
         CLC   0(L'TIMINKEY,R6),BOFULL1 NO, THIS FLD SHOULDN'T CHANGE           
         BNE   EXITPF12                                                         
*&&                                                                             
VALDTM30 MVC   0(L'TIMINKEY,R6),BOFULL1                                         
         MVC   PTMINMKY-TIMINKEY(L'PTMINMKY,R6),BOFULL2                         
*                                                                               
         MVC   SVKYDYTM,FVIFLD                                                  
*                                                                               
VALDTMX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SEQUENCE FIELD                                                
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SEQDTA   DS    0H                                                               
         LA    RF,SEQTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SEQTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSEQ)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSEQ)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY SEQUENCE FIELD                                                        
***********************************************************************         
DISSEQ   DS    0H                                                               
         MVC   FVIFLD(L'SVKYSEQN),SVKYSEQN                                      
DISSEQX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE SEQUENCE FIELD                                                       
***********************************************************************         
VALSEQ   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         CLI   FVILEN,0                                                         
         BNE   VALSEQ00                                                         
*                                                                               
         MVI   FVIFLD,C'0'                                                      
         MVC   FVIFLD+1(2),FVIFLD                                               
         TM    MISCFLG1,MF1PFCOM                                                
         BZ    *+10                WE HAVE A DEFAULT                            
         MVC   FVIFLD(L'SVKYSEQN),SVKYSEQN                                      
         PACK  BODUB1,FVIFLD(L'SVKYSEQN)                                        
         CVB   R1,BODUB1                                                        
         B     VALSEQ05                                                         
*                                                                               
VALSEQ00 TM    FVIIND,FVINUM       VALID NUMERIC FIELD?                         
         BZ    EXITNOTN            NO                                           
         L     R1,BCFULL           THIS IS THE BINARY FOR THE NUMBER            
VALSEQ05 CH    R1,=H'255'                                                       
         BH    EXITNV                                                           
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SEQINKEY-TWAD)                                             
*&&DO                                                                           
         CLI   TWASESNL,1               ANY PREVIOUS SESSION?                   
         BE    VALSEQ10                 NO                                      
         TM    MISCFLG1,MF1PFCOM        JUST COMING IN NOW?                     
         BNZ   VALSEQ10                 YES, NEED TO SAVE IT ONCE               
         CLM   R1,1,0(RE)               NO, THIS FLD SHOULDN'T CHANGE           
         BNE   EXITPF12                                                         
*&&                                                                             
VALSEQ10 STC   R1,0(RE)            SEQUENCE NUMBER ENTERED BY USER              
*                                                                               
VALSEQ20 MVC   SVKYSEQN,FVIFLD                                                  
         CLI   0(RE),0                                                          
         BNE   VALSEQ25                                                         
         MVI   SVKYSEQN,C'0'                                                    
         MVC   SVKYSEQN+1(L'SVKYSEQN-1),SVKYSEQN                                
*                                                                               
VALSEQ25 BAS   RE,GETCLSTR         DOES THE ELEMENT EXIST?                      
         BE    VALSEQ30            YES                                          
         CLI   CSACT,A#ADD         NO, USER WANTS TO ADD THE ELEMENT?           
         BE    VALSEQX                 YES                                      
         B     EXITRCNF                NO, 'RECORD NOT ON FILE'                 
*                                                                               
VALSEQ30 CLI   CSACT,A#ADD         ELEMENT ALREADY EXISTS, ADDING?              
         BE    EXITRCAE            YES, 'RECORD ALREADY EXISTS'                 
*                                                                               
VALSEQX  OI    FVIIND,FVIVAL                                                    
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
         TM    FVIIND,FVIVAL                                                    
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   THE KEY WAS CHANGED                          
*                                                                               
VALDMO00 DS    0H                                                               
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
         MVC   BOWORK1(L'DMLNDEMO),SAVDMO+(DMLNDEMO-DEMOLIN)                    
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         DROP  R6                                                               
*                                                                               
         GOTO1 VDEMOCON,BODMCB,(1,BOWORK1),(9,FVIFLD),(0,AIO5)                  
         OI    FVIIND,FVIVAL                                                    
         OI    FVOIND,FVOXMT                                                    
         LA    R2,SAVDMO                                                        
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
         USING DEMOLIN,R2                                                       
         LA    R1,1                START WITH THE FIRST ONE                     
*                                                                               
VALDMO20 CLC   BOWORK1(3),DMLNDEMO MATCH ON THIS 3-BYTE DEMO?                   
         BE    VALDMO30            YES                                          
         LA    R1,1(R1)                                                         
         LA    R2,L'MINDMO(R2)     BUMP TO NEXT MINDMOS ENTRY                   
         LR    R0,RA                                                            
         AH    R0,=Y(MINDMOS+L'MINDMOS-TWAD)                                    
         CR    R2,R0                                                            
         BL    VALDMO20                                                         
         TM    MISCFLG1,MF1PFRET                                                
         BZ    EXITNV              DEMO IS NOT PART OF OUR DEMOS LIST           
         MVI   FVILEN,0                                                         
         B     VALDMO00                                                         
         DROP  R2                                                               
*                                                                               
VALDMO30 DS    0H                                                               
*                                                                               
VALDMOX  OI    FVIIND,FVIVAL                                                    
         MVC   SAVDMX,0(R2)                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LENGTH FIELD                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LENDTA   DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         CLI   0(R6),RPRDTELQ                                                   
         BE    DISLEN0                                                          
         BAS   RE,GETCLSTR                                                      
*                                                                               
         USING RPRDTELD,R6                                                      
DISLEN0  EDIT  (B1,RPRDTSLN),(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,        X        
               DUB=BODUB1                                                       
*                                                                               
         B     EXITOK                                                           
         DROP  R5,R6                                                            
***********************************************************************         
* DATA OBJECT FOR PROGRAM FIELD                                                 
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRGDTA   DS    0H                                                               
         LA    RF,PRGTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PRGTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRG)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY THE PROGRAM                                                           
***********************************************************************         
DISPRG   DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
*                                                                               
DISPRG0  XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRTXELQ                                                 
         MVI   MINEKEY+1,RPRTXPRQ                                               
         MVC   MINEKEY+6(L'RPRDTPRG),RPRDTPRG                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SVMINEKY,MINEKEY                                                 
         BNE   DISPRGX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRTXELD,R6                                                      
         ZIC   R1,RPRTXLEN                                                      
         SH    R1,=Y(RPRTXOVQ+1)                                                
         BM    DISPRGX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),RPRTXTXT                                               
*                                                                               
DISPRGX  OI    FVATRB,FVAPROT                                                   
         B     EXITOK                                                           
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SHARE FIELD                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SHRDTA   DS    0H                                                               
         LA    RF,SHRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SHRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSHR)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY THE SHARES                                                            
***********************************************************************         
DISSHR   DS    0H                                                               
         LR    R5,RA                                                            
         AH    R5,=Y(CLCMPAGE-TWAD)                                             
*                                                                               
         LA    R4,SAVBK                                                         
         USING BOOKLIN,R4                                                       
         ZIC   RE,BKLNIORD                                                      
         BCTR  RE,0                                                             
         MH    RE,=Y(L'CLCMSHR)                                                 
         LA    RE,1(RE,R5)                                                      
         LA    R2,FVIFLD                                                        
         EDIT  (B4,1(RE)),(5,0(R2)),1,ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1         
*                                                                               
         CLI   0(R5),1             1ST PAGE                                     
         BNE   *+12                                                             
         LA    R4,L'SAVBK(R4)                                                   
         B     *+8                                                              
         LA    R4,L'SAVBK*4(R4)                                                 
*                                                                               
         LA    R3,3                3 MORE PER PAGE                              
DSSHR10  DS    0H                                                               
         ZIC   RE,BKLNIORD                                                      
         BCTR  RE,0                                                             
         MH    RE,=Y(L'CLCMSHR)                                                 
         LA    RE,1(RE,R5)                                                      
         DROP  R4                                                               
*                                                                               
         OC    0(L'SAVBK,R4),0(R4) ANY MORE TO SHOW?                            
         BZ    DISSHRX             NO                                           
*                                                                               
         LA    R2,FVIFLD                                                        
         ZIC   R0,0(RE)                                                         
         SH    R0,=H'8'            ALIGN IT CORRECTLY                           
         AR    R2,R0                                                            
         EDIT  (B4,1(RE)),(5,0(R2)),1,ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1         
         LA    R4,L'SAVBK(R4)                                                   
         BCT   R3,DSSHR10                                                       
*                                                                               
DISSHRX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR STATION                                                       
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
* DISPLAY THE STATION                                                           
***********************************************************************         
DISLSTA  DS    0H                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
*                                                                               
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         CLI   TLKLINE#,1          NOT THE RATINGS LINE?                        
         BH    DISLSTAX                                                         
         MVC   FVIFLD(L'TLKSTATN),TLKSTATN                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
*                                                                               
         MVC   BOCURSOR,FVADDR                                                  
*                                                                               
DISLSTAX B     EXITOK                                                           
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
LRTGTBL  DC    AL1(DHED),AL1(0,0,0),AL4(HEDLRTG)                                
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISLRTG)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEMO FIELD HEADLINE FOR THE COLUMN                                    
***********************************************************************         
HEDLRTG  DS    0H                                                               
         TM    MISCFLG1,MF1KYCHG                                                
         BZ    HEDLV08                                                          
*                                                                               
         CLI   SVPARMS2+3,PRMBOOKQ                                              
         BNE   HEDLV08                                                          
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         CLI   0(R6),RPRDTELQ                                                   
         BE    HEDLV00                                                          
         BAS   RE,GETCLSTR                                                      
*                                                                               
HEDLV00  XC    SAVFTDT,SAVFTDT                                                  
         MVC   SAVFTDT(1),RPRDTDAY                                              
         MVC   SAVFTDT+1(4),RPRDTSTM                                            
         DROP  R5,R6                                                            
*                                                                               
         SR    R0,R0                                                            
HEDLV02  IC    R0,1(R6)            SECONDARY DAY/TIMES                          
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    HEDLV06                                                          
         CLI   0(R6),RPRDYELQ                                                   
         BNE   HEDLV02                                                          
*                                                                               
         ZIC   RE,1(R6)                                                         
         AR    RE,R6               END OF ELEMENT                               
         LA    R6,RPRDYDTM-RPRDYELD(R6)                                         
         LA    RF,SAVFTDT+L'RPRDYDTM                                            
HEDLV04  CR    R6,RE                                                            
         BNL   HEDLV06                                                          
         MVC   0(L'RPRDYDTM,RF),0(R6)                                           
         LA    R6,L'RPRDYDTM(R6)                                                
         LA    RF,L'RPRDYDTM(RF)                                                
         B     HEDLV04                                                          
*                                                                               
HEDLV06  DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         ZIC   R1,0(RE)            LAST STATION USED BY USER                    
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R1,SAVSTAS(R1)                                                   
         USING STALIN,R1                                                        
         MVC   BOWORK2(L'STLNSTA),STLNSTA                                       
         DROP  R1                                                               
         CLI   BOWORK2+4,C' '                                                   
         BNH   *+8                                                              
         MVI   BOWORK2+4,C'T'                                                   
*                                                                               
         GOTOX (DTLNDFQ,AREPRO01),BODMCB,BOWORK2,SAVBKS,SAVDMX,SAVFTDT          
*                                                                               
         L     RE,AIO5                                                          
         LA    RF,IOAREALN                                                      
         LR    R0,RA                                                            
         AH    R0,=Y(FOO-TWAD)                                                  
         LA    R1,L'FOO                                                         
         MVCL  R0,RE                                                            
*                                                                               
HEDLV08  L     RF,SVPARMS6                                                      
         MVC   1(20,RF),BCSPACES                                                
         ZIC   RE,SVPARMS2+3       FIELD NUMBER                                 
         SH    RE,=Y(PRMBOOKQ)     OFFSETED BY THE PRIME BOOK                   
*                                                                               
         LR    R1,RA               DEFAULT IS 1ST PAGE OF BOOKS                 
         AH    R1,=Y(CLCMPAGE-TWAD)                                             
         MVI   0(R1),1                                                          
         CH    RE,=H'3'                                                         
         BNH   *+8                                                              
         MVI   0(R1),2             UNLESS WE'RE SHOWING 4TH BOOK ON             
*                                                                               
         MH    RE,=Y(L'SAVBK)                                                   
         LA    RE,SAVBKS(RE)                                                    
         ZIC   R0,BKLNIORD-BOOKLIN(RE)                                          
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         MH    RE,=Y(L'TLRDMBK)                                                 
         LR    R0,RA                                                            
         AH    R0,=Y(FOO-TWAD)                                                  
         AR    RE,R0                                                            
*                                                                               
         SR    R0,R0               LEVEL                                        
         ICM   R0,15,8(RE)                                                      
         EDIT  (R0),(8,0(RF)),1,ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1               
*                                                                               
         L     R2,SVPARMS5                                                      
         ZIC   R1,SVPARMS2+3       FIELD NUMBER                                 
***************                                                                 
* BOOK NAME FOR THE RATING COLUMN                                               
***************                                                                 
         SH    R1,=Y(PRMBOOKQ)     OFFSETED BY THE PRIME BOOK                   
         LR    R3,R1                                                            
         LR    R4,R1                                                            
         MH    R3,=Y(L'SAVBK)                                                   
         LA    R3,SAVBKS(R3)                                                    
         USING BOOKLIN,R3                                                       
         MH    R4,=Y(L'SAVLBL)                                                  
         LA    R4,SAVLBLS(R4)                                                   
*                                                                               
         OC    0(L'SAVLBL,R4),0(R4)   USER DEFINED BOOK?                        
         BZ    *+14                                                             
         MVC   0(5,R2),0(R4)                                                    
         B     HEDLRTGX                                                         
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         XC    FVIFLD,FVIFLD                                                    
         MVC   BOWORK1(3),BKLNBK                                                
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(2),=X'0B07'      PUT OUT SOURCE                           
         MVC   BOELEM+2(1),BKLNSPBK                                             
         LA    RE,20+8                                                          
         TM    FVIHDR+1,X'02'       EXT FIELD HDR?                              
         BNE   *+8                                                              
         AH    RE,=H'8'                                                         
         STC   RE,FVIHDR                                                        
         DROP  R3                                                               
         GOTOX (UNBOOKQ,AREPRO01),BODMCB,(1,BOWORK1),FVIHDR,           X        
               (C'L',BOELEM),(C'+',=CL6' ')                                     
*                                                                               
         ZIC   RE,FVIHDR                                                        
         LA    RE,FVIHDR(RE)                                                    
         TM    FVIHDR+1,X'02'       EXT FIELD HDR?                              
         BNE   *+8                                                              
         SH    RE,=H'8'                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         CLI   0(RE),C')'                                                       
         BNE   HDLRTG02                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     HDLRTG02                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
HDLRTG02 DS    0H                                                               
         LA    RF,FVIHDR+8                                                      
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),FVIHDR+8                                                 
*                                                                               
HEDLRTGX B     EXITOK                                                           
***********************************************************************         
* DISPLAY RATING FIELD                                                          
***********************************************************************         
DISLRTG  DS    0H                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
*                                                                               
         ZIC   R3,SVPARMS2+3       OFFSET FROM PRIME BOOK                       
         SH    R3,=Y(PRMBOOKQ)                                                  
*                                                                               
         LR    R1,R3                                                            
         LR    RE,R3                                                            
         MH    R1,=Y(L'SAVBK)                                                   
         LA    R1,SAVBKS(R1)                                                    
         USING BOOKLIN,R1                                                       
         ZIC   R3,BKLNIORD           INTERNAL ORDER NUMBER OF THE BOOK          
         DROP  R1                                                               
*                                                                               
         LTR   RE,RE               PRIME BOOK?                                  
         BNZ   DSLRTG00            NO                                           
         TM    MISCFLG1,MF1TMPBT   DOING COLUMNS?                               
         BZ    DSLRTG10            NO, NOW WE ARE                               
         OI    MISCFLG1,MF1DS1ST   DISPLAYED 1ST LINE?                          
         B     DSLRTG20                                                         
*                                                                               
DSLRTG00 TM    MISCFLG1,MF1DS1ST   DISPLAYED 1ST LINE ALREADY?                  
         BNZ   DSLRTG20            YES                                          
DSLRTG10 OI    MISCFLG1,MF1TMPBT   IN MIDST OF DOING COLUMNS                    
*********                                                                       
* STORE COLUMN # OF THIS RATING BOOK                                            
*********                                                                       
         CLI   TLKLINE#,1          1ST LINE FOR THE STATION?                    
         BNE   DSLRTG20                                                         
         ZICM  R1,FVABSA,2         ABSOLUTE ADDRESS OF THE FIELD                
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         AH    R0,=H'1'            GOT COLUMN NUMBER FOR FIELD                  
         LR    R1,R3                                                            
         BCTR  R1,0                                                             
         MH    R1,=Y(L'CLCMSHR)                                                 
         LR    RE,RA                                                            
         AH    RE,=Y(CLCMSHRS-TWAD)                                             
         AR    R1,RE                                                            
         STC   R0,0(R1)                                                         
*                                                                               
DSLRTG20 DS    0H                                                               
         TM    TLRFLAG1,TLRF1LST   LAST LINE?                                   
         BNZ   DSLRTG50            YES                                          
*                                                                               
         LA    R6,FVIFLD                                                        
         BCTR  R3,0                                                             
         MH    R3,=Y(L'TLRDMBK)                                                 
         LA    R3,TLRDMBK(R3)                                                   
         SR    R0,R0               DEMO                                         
         ICM   R0,15,0(R3)                                                      
         EDIT  (R0),(8,0(R6)),1,ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1               
         AR    R6,R0                                                            
         MVI   0(R6),C'/'                                                       
         LA    R6,1(R6)                                                         
         SR    R0,R0               SHARE                                        
         ICM   R0,15,4(R3)                                                      
         EDIT  (R0),(8,0(R6)),1,ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1               
         AR    R6,R0                                                            
*                                                                               
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     DISLRTGX                                                         
*                                                                               
DSLRTG50 DS    0H                  DISPLAY FOOTNOTE                             
         BCTR  R3,0                                                             
         MH    R3,=Y(L'TLRFTBK)                                                 
         LA    R3,TLRFTBK(R3)                                                   
         MVC   FVIFLD(L'TLRFTBK),0(R3)                                          
         OI    FVATRB,FVAPROT              PROTECT                              
*                                                                               
DISLRTGX B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
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
         AH    RF,=Y(LISTABL-PRO53)                                             
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
         LA    RF,LSFIXCLM         RF = A(1ST FIXED COLUMN)                     
         USING DCTABD,RF                                                        
*                                                                               
         MVC   DCTFLD#,=AL2(82)    STATION FIELD                                
         MVI   DCTINDS1,DCTIOPEN                                                
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         MVC   DCTFLD#,=Y(PRMBOOKQ)   PRIME BOOK FIELD                          
         MVI   DCTINDS1,DCTIOPEN                                                
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         MVC   LSFIXNUM,=AL2(2)    # OF FIXED COLUMNS                           
*                                                                               
         LA    RF,LSVARCLM           RF = A(1ST VARIABLE COLUMN)                
         LA    RE,SAVBKS+L'SAVBK     RE = A(2ND BOOK)                           
         LA    R1,PRMBOOKQ+1         FIELD EQUATE FOR 2ND BOOK FIELD            
         SR    R2,R2                 R3 = NUMBER OF VARIABLE COLUMNS            
DMLC33   OC    0(L'SAVBK,RE),0(RE)   ANY BOOK DEFINED HERE?                     
         BZ    DMLC36                NO                                         
*                                                                               
         STCM  R1,3,DCTFLD#        BOOK FIELD                                   
         MVI   DCTINDS1,DCTIOPEN                                                
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         LA    R2,1(R2)            WE PUT TWO MORE VARIABLE COLUMNS             
         LA    R1,1(R1)                                                         
         LA    RE,L'SAVBK(RE)                                                   
         LA    R0,SAVBKS+L'SAVBKS                                               
         CR    RE,R0                                                            
         BL    DMLC33              LOOP BACK UNTIL ALL DEMOS ARE DONE           
*                                                                               
DMLC36   STCM  R2,3,LSVARNUM       NUMBER OF VARIABLE COLUMNS                   
*                                                                               
DMLCX    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISE FOR LIST                                                           
***********************************************************************         
INITL1   DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         XC    SAVFTDT,SAVFTDT                                                  
         MVC   SAVFTDT(1),RPRDTDAY                                              
         MVC   SAVFTDT+1(4),RPRDTSTM                                            
         DROP  R5,R6                                                            
*                                                                               
         SR    R0,R0                                                            
INITL02  IC    R0,1(R6)            SECONDARY DAY/TIMES                          
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    INITL06                                                          
         CLI   0(R6),RPRDYELQ                                                   
         BNE   INITL02                                                          
*                                                                               
         ZIC   RE,1(R6)                                                         
         AR    RE,R6               END OF ELEMENT                               
         LA    R6,RPRDYDTM-RPRDYELD(R6)                                         
         LA    RF,SAVFTDT+L'RPRDYDTM                                            
INITL04  CR    R6,RE                                                            
         BNL   INITL06                                                          
         MVC   0(L'RPRDYDTM,RF),0(R6)                                           
         LA    R6,L'RPRDYDTM(R6)                                                
         LA    RF,L'RPRDYDTM(RF)                                                
         B     INITL04                                                          
*                                                                               
INITL06  DS    0H                                                               
         MVI   SVSTANO,1                                                        
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         ZIC   R1,0(RE)            LAST STATION USED BY USER                    
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R1,SAVSTAS(R1)                                                   
         USING STALIN,R1                                                        
         MVC   BOWORK2(L'STLNSTA),STLNSTA                                       
         CLI   BOWORK2+4,C' '                                                   
         BH    *+8                                                              
         MVI   BOWORK2+4,C'T'                                                   
         MVC   SVSTA(L'SVSTA1),BOWORK2                                          
*                                                                               
         XC    IOKEY,IOKEY         CHECK FOR MARKET/STATIONS MENU               
         LA    R6,IOKEY                                                         
         USING RSETRECD,R6                                                      
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,CUAALF                                                  
         MVC   RSETKSET,=C'MS'                                                  
         MVC   RSETKID,BOWORK2                                                  
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   INITL100            SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RSETKEY),IOKEYSAV                                        
         BNE   INITL100            WRONG RECORD                                 
*                                                                               
         L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO4                                                          
         LA    R6,RSETELEM                                                      
         USING RSETMEMD,R6                                                      
INITL10  DS    0H                                                               
         CLI   0(R6),0             EOR?                                         
         BE    INITL40             YES                                          
         CLI   0(R6),RSETMCDQ      MEMBER ELEMENT?                              
         BNE   INITL30             NO                                           
*                                                                               
         ZIC   RE,1(R6)                                                         
         AR    RE,R6               END OF ELEMENT                               
         MVC   BOBYTE1,RSETMLEN    MEMBER LENGTH                                
         LA    R6,RSETMTOV(R6)     MEMBER LIST                                  
         DROP  R6                                                               
INITL20  CR    R6,RE               END OF ELMEMENT?                             
         BNL   INITL10             YES                                          
*                                                                               
         CLI   SVSTANO,NMAXSTA     FILLED?                                      
         BNL   INITL40             YES                                          
*                                                                               
         CLC   BOWORK2(5),0(R6)    PRIMARY STATION?                             
         BE    INITL22             YES                                          
*                                                                               
         ZIC   R1,SVSTANO                                                       
         LR    RF,R1                                                            
         MH    RF,=Y(L'SVSTA1)                                                  
         LA    RF,SVSTA(RF)                                                     
         MVC   0(L'SVSTA1,RF),0(R6)                                             
         CLI   4(RF),C' '                                                       
         BH    *+8                                                              
         MVI   4(RF),C'T'                                                       
         LA    R1,1(R1)                                                         
         STC   R1,SVSTANO                                                       
*                                                                               
INITL22  ZIC   R0,BOBYTE1          NEXT MEMBER                                  
         AR    R6,R0                                                            
         B     INITL20                                                          
*                                                                               
INITL30  DS    0H                  NEXT ELEMENT                                 
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     INITL10                                                          
*                                                                               
INITL40  DS    0H                                                               
         B     INITL200                                                         
*                                                                               
INITL100 XC    SVSTA,SVSTA         READ DEMO MARKET                             
         LA    R6,DEMBLOCK                                                      
         USING DBLOCK,R6                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIOREC                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOM                                                    
         MVI   DBSELMED,C'T'                                                    
         MVI   DBSELSRC,C'N'       NEILSEN                                      
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         ZIC   R1,0(RE)            LAST STATION USED BY USER                    
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R1,SAVSTAS(R1)                                                   
         USING STALIN,R1                                                        
         MVC   DBSELSTA(L'STLNSTA),STLNSTA                                      
         CLI   DBSELSTA+4,C' '                                                  
         BNH   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
         MVC   SVSTA(L'SVSTA1),DBSELSTA                                         
         DROP  R1                                                               
*                                                                               
         MVC   DBSELBK,=X'6005'  <== JUST FOR NOW                               
*                                                                               
         MVI   DBFUNCT,DBGETMK     GET THE RATING SERVICE MARKET                
         L     R1,ACOM                                                          
         L     RF,CDEMAND-COMFACSD(R1)                                          
         GOTO1 (RF),BODMCB,DBLOCK,0,0                                           
*                                                                               
         OC    DBACTRMK,DBACTRMK                                                
         BZ    INITL200            NO DEMO INFO AVAILABLE                       
*                                                                               
         MVI   DBFUNCT,DBGETMS     GET THE STATIONS                             
         MVC   DBSELRMK,DBACTRMK                                                
         GOTO1 (RF),BODMCB,DBLOCK,STAHOOK,0                                     
*                                                                               
INITL200 OI    LSSTAT1,LSSTSAR        LIST OF TSAR RECS                         
         OI    LSSTAT2,LSSIUPD        WE WANT TO DO OUR OWN UPDATES             
         NI    LSSTAT2,X'FF'-LSSADD   NOT VALID TO ADD NEW LIST LINES           
         MVI   STACNTR,0              OFFSET FROM FIRST STATION                 
         MVI   LINECNTR,1             1ST ROW OF 3                              
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(CLCMPAGE-TWAD)                                             
         MVI   0(RE),1                DEFAULT IS THE FIRST PAGE                 
         XC    1(L'CLCMSHRS,RE),1(RE) CLEAR CUMMULATIVE SHARES                  
*                                                                               
         B     EXITOK                                                           
***********************************************************************         
* DEMAND HOOK FOR EXTRACTING MARKET STATIONS                                    
***********************************************************************         
STAHOOK  L     R4,DBAREC                                                        
         USING MLKEY,R4                                                         
         OC    MLKMKT,MLKMKT       TEST SPILL MARKET                            
         BNZ   STAHX               YES - IGNORE                                 
*                                                                               
         TM    MLSTAT,X'F0'        TEST STATION NUMERIC                         
         BO    STAHX               YES - IGNORE                                 
*                                                                               
         CLC   DBSELSTA(5),MLSTAT  TEST STATION = REQUEST STATION               
         BE    STAHX               YES - IGNORE (ALREADY FIRST IN LIST)         
*                                                                               
         CLI   SVSTANO,NMAXSTA                                                  
         BNL   STAHX                                                            
*                                                                               
         ZIC   R1,SVSTANO                                                       
         LR    RF,R1                                                            
         MH    RF,=Y(L'SVSTA1)                                                  
         LA    RF,SVSTA(RF)                                                     
         MVC   0(L'SVSTA1,RF),MLSTAT                                            
         LA    R1,1(R1)                                                         
         STC   R1,SVSTANO                                                       
STAHX    BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR LIST                                                           
***********************************************************************         
FTFLST1  DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* FIRST FOR LIST                                                                
***********************************************************************         
FLST1    DS    0H                                                               
         MVI   LINECNTR,1          YES, GET NEXT DEMO LINE                      
         MVI   STACNTR,0                                                        
         LA    R5,SVSTA                                                         
*                                                                               
         L     RE,AIO5                                                          
         LA    RF,IOAREALN                                                      
         LR    R0,RA                                                            
         AH    R0,=Y(FOO-TWAD)                                                  
         LA    R1,L'FOO                                                         
         MVCL  RE,R0                                                            
*                                                                               
         B     EXITOK                                                           
***********************************************************************         
* NEXT FOR LIST                                                                 
***********************************************************************         
NLST1    DS    0H                                                               
         CLI   LINECNTR,2          HAVE 2 ROWS FOR THIS LINE ALREADY?           
         BNH   NMLX                NO, MAKE CONTROLLER ADD IT                   
*                                                                               
NML05    MVI   LINECNTR,1          YES, GET NEXT DEMO LINE                      
         ZIC   R5,STACNTR                                                       
         LA    R5,1(R5)                                                         
         STC   R5,STACNTR                                                       
*                                                                               
         CLI   STACNTR,NMAXSTA     MORE THAN MAXIMUM ALLOWABLE?                 
         BNL   EXITL               YES, EXIT                                    
*                                                                               
         MH    R5,=Y(L'SVSTA1)                                                  
         LA    R5,SVSTA(R5)                                                     
         MVC   BOWORK2(L'SVSTA1),0(R5)                                          
         CLI   BOWORK2+4,C' '                                                   
         BNH   *+8                                                              
         MVI   BOWORK2+4,C'T'                                                   
*                                                                               
         GOTOX (DTLNDFQ,AREPRO01),BODMCB,BOWORK2,SAVBKS,SAVDMX,SAVFTDT          
*                                                                               
NML20    LA    R0,SVSTA+L'SVSTA    END OF TABLE?                                
         CR    R5,R0                                                            
         BNL   EXITL                  YES                                       
         OC    0(L'SVSTA,R5),0(R5)    ANY MORE STATIONS?                        
         BZ    EXITL                  THAT'S ALL                                
         CLC   SVSTA(4),0(R5)      PRIME STATION                                
         BE    NML05                                                            
*                                                                               
NMLX     B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP TSAR FROM DIRECTORY                                                    
***********************************************************************         
TSARDIR1 DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,SVPARMS3                                                      
         USING RPROKEY,R6                                                       
         L     RF,MINBUFF                                                       
         MVC   RPROKEY(RPROKMEL-RPROKEY),0(RF)                                  
         MVC   RPROKCTL,GSRECSTA                                                
         MVC   RPROKDA,SVRECDA                                                  
         B     EXITOK                                                           
         DROP  R5,R6                                                            
***********************************************************************         
* SET UP TSAR FROM FILE                                                         
***********************************************************************         
TSARFIL1 DS    0H                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
*                                                                               
         MVC   TLRLEN,=Y(TLRLNQ)                                                
*                                                                               
         MVC   TLKSCNTR,STACNTR    STACNTR                                      
         ZIC   RF,STACNTR                                                       
         MH    RF,=Y(L'SVSTA1)                                                  
         LA    RF,SVSTA(RF)                                                     
         MVC   TLKSTATN,0(RF)      STATION CALL LETTERS                         
         MVC   TLKLINE#,LINECNTR   ROW NUMBER                                   
*                                                                               
         ZIC   R1,LINECNTR                                                      
         LA    R1,1(R1)                                                         
         STC   R1,LINECNTR                                                      
*                                                                               
         CLI   LINECNTR,2          LAST LINE                                    
         BNH   *+8                 NO                                           
         OI    TLRFLAG1,TLRF1LST   YES, SET LASTLINE FLAG                       
*                                                                               
         L     R5,AIO5                                                          
         MVC   TLRDMBKS,0(R5)                                                   
         LA    R5,L'TLRDMBKS(R5)                                                
*                                                                               
         CLI   LINECNTR,2          DO CUMMULATIVE ONLY FOR FIRST LINE           
         BH    TSRF18                                                           
*                                                                               
         LA    RF,NUMBKS                                                        
         LR    RE,RA                                                            
         AH    RE,=Y(CLCMSHRS-TWAD)                                             
         LA    R1,TLRDMBKS                                                      
TSRF10   MVC   BOFULL1,4(R1)       THE SHARE FOR THAT BOOK                      
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R0,1(RE)                                                         
         A     R0,BOFULL1          TOTAL SHARES FOR THAT BOOK                   
         ST    R0,1(RE)                                                         
         LA    R1,L'TLRDMBK(R1)                                                 
         LA    RE,L'CLCMSHR(RE)                                                 
         BCT   RF,TSRF10                                                        
*                                                                               
TSRF18   DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    TSRF20                                                           
         ZIC   R1,1(R5)                                                         
         BCTR  R1,0                                                             
         MH    R1,=Y(L'TLRFTBK)                                                 
         LA    R1,TLRFTBKS(R1)                                                  
         ZIC   RF,0(R5)                                                         
         LR    RE,RF                                                            
         SH    RF,=H'6'            5+1 FOR EX                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),5(R5)                                                    
*                                                                               
         LA    R5,0(RE,R5)                                                      
         B     TSRF18                                                           
*                                                                               
TSRF20   DS    0H                                                               
*                                                                               
TSARFILX B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* FIRST TIME FOR UPDATE                                                         
***********************************************************************         
UPDFRST1 DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* LAST TIME FOR UPDATE                                                          
***********************************************************************         
UPDLAST1 DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* UPDATE DIRECTORY FROM TSAR RECORD                                             
* P3 = A (DIRECTORY RECORD)                                                     
* P4 = A (TSAR RECORD)                                                          
***********************************************************************         
UPDDIR1  DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD                                                  
* P3 = A (FILE RECORD)   <==  NOT REALLY THE RECORD FOR US                      
* P4 = A (TSAR RECORD)                                                          
***********************************************************************         
UPDREC1  DS    0H                                                               
         B     EXITOK                                                           
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
         CLI   SREC,O#MAX          CHECK FOR CONTROLLER                         
         BNH   EXITOK                                                           
*                                                                               
         OI    SNINDS1,SNIPARMS    SO WE CAN GET DNTR                           
         LA    RF,SDATA+6                                                       
         MVI   0(RF),RPRDTELQ                                                   
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         MVC   RPRDTSTA-RPRDTSTA+1(L'SVSTATN,RF),0(RE)                          
         LR    RE,RA                                                            
         AH    RE,=Y(DPTINKEY-TWAD)                                             
         MVC   RPRDTDPT-RPRDTSTA+1(L'DPTINKEY,RF),0(RE)                         
         MVC   RPRDTDAY-RPRDTSTA+1(L'DAYINKEY,RF),DAYINKEY-DPTINKEY(RE)         
         MVC   RPRDTTIM-RPRDTSTA+1(L'PTMINMKY,RF),PTMINMKY-DPTINKEY(RE)         
         MVC   RPRDTSEQ-RPRDTSTA+1(L'SEQINKEY,RF),SEQINKEY-DPTINKEY(RE)         
*                                                                               
NTROUTX  B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                                    
***********************************************************************         
         PUSH  USING                                                            
NTRIN    DS    0H                                                               
         OI    MISCFLG1,MF1PFCOM                                                
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         MVC   0(L'SVSTATN,RE),SDATA+6+RPRDTSTA-RPRDTELD-1                      
*                                                                               
         MVC   SVKYDYPT,SDATA+6+RPRDTDPT-RPRDTELD-1                             
*                                                                               
         ZIC   R4,SDATA+6+RPRDTSEQ-RPRDTELD-1                                   
         EDIT  (R4),(3,SVKYSEQN),FILL=0,WRK=BOWORK1,DUB=BODUB1                  
*                                                                               
         LA    R6,SDATA+6+RPRDTDAY-RPRDTELD-1                                   
         GOTO1 VDAYUNPK,BODMCB,0(R6),SVKYDYTM                                   
*                                                                               
         LA    RE,SVKYDYTM                                                      
NTRIN10  CLI   0(RE),C' '          CONVERT '/'S TO ','S                         
         BNH   NTRIN20                                                          
         CLI   0(RE),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         B     NTRIN10                                                          
*                                                                               
NTRIN20  LA    R4,SVKYDYTM+9       FIND WHERE TO PUT THE TIMES                  
NTRIN30  CLI   0(R4),C' '                                                       
         BH    NTRIN40                                                          
         BCTR  R4,0                                                             
         B     NTRIN30                                                          
NTRIN40  LA    R4,1(R4)                                                         
*                                                                               
NTRIN50  MVI   0(R4),C'/'          SEPARATE DAYS AND TIMES WITH A C'/'          
         LA    R4,1(R4)                                                         
*                                                                               
         LA    R6,SDATA+6+L'RPROKMEL                                            
         GOTO1 VUNTIME,BODMCB,0(R6),0(R4)                                       
*                                                                               
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS COMIMG BACK FROM CALLED SESSION                                       
***********************************************************************         
         PUSH  USING                                                            
NTRXIN   DS    0H                                                               
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
         MVI   GSSKCODE,C'K'                                                    
SETKSCRX B     EXITOK                                                           
***********************************************************************         
* SET THE MAINT SCREEN CODE                                                     
***********************************************************************         
SETMSCR  DS    0H                                                               
         TM    BCINDS1,BCINACT     IS THIS THE FIRST TIME?                      
         BNZ   *+12                                                             
         TM    MISCFLG1,MF1KYCHG   DID THE KEY CHANGE?                          
         BZ    SETMSCRX            NO                                           
*                                                                               
         CLI   GSSMCODE,C'K'                                                    
         BE    *+12                                                             
         MVI   GSSMCODE,C'K'       WORK/DEMO SCREEN                             
         B     SETMSCRX                                                         
         MVI   GSSMCODE,C'H'                                                    
*                                                                               
SETMSCRX B     EXITOK                                                           
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
         ZIC   RE,FVIXUS2          RE = FIELD #                                 
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* CAN SET THE RECORD FOR THE PFKEY                                              
***********************************************************************         
RECPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),PFKYUP        > UP?                                        
         BNL   NOTPFK                                                           
*                                                                               
RECPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE ACTION FOR THE PFKEY                                              
***********************************************************************         
ACTPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),PFTEXT        TEXT?                                        
         BE    NOTPFK                                                           
*                                                                               
ACTPFKX  B     EXITOK                                                           
***********************************************************************         
* PFKEY DEFINITION (RECORD, ACTION, OR USER) NOT WANTED                         
***********************************************************************         
NOTPFK   OI    SVPARMS3,X'80'                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GETS THE CLUSTER THAT'S BEING AFFECTED                                        
***********************************************************************         
GETCLSTR NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ                                                 
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         MVC   MINEKEY+1(L'SVSTATN),0(RE)                                       
         LR    RE,RA                                                            
         AH    RE,=Y(DPTINKEY-TWAD)                                             
         MVC   MINEKEY+2(L'DPTINKEY),0(RE)                                      
         MVC   MINEKEY+3(L'DAYINKEY),DAYINKEY-DPTINKEY(RE)                      
         MVC   MINEKEY+4(L'PTMINMKY),PTMINMKY-DPTINKEY(RE)                      
         MVC   MINEKEY+6(L'SEQINKEY),SEQINKEY-DPTINKEY(RE)                      
*                                                                               
         BAS   RE,MINIOHI                                                       
         BE    GCLSTR10                                                         
         CLI   MINERR,MINEEOF      END OF FILE?                                 
         BE    GCLSTRNO                                                         
         CLI   MINERR,MINERNF      RECORD NOT FOUND?                            
         BE    GCLSTRNO            YES, RETURN A NO                             
         DC    H'0'                                                             
*                                                                               
         USING RPRDTELD,R6                                                      
GCLSTR10 L     R6,MINELEM                                                       
         CLI   0(R6),RPRDTELQ                                                   
         BNE   GCLSTRNO                                                         
*                                                                               
         CLC   RPRDTSTA(L'RPROKMEL-1),SVMINEKY+1                                
         BNE   GCLSTRNO                                                         
*                                                                               
GCLSTRYS B     EXITOK                                                           
*                                                                               
GCLSTRNO B     EXITL                                                            
         DROP  R5,R6                                                            
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
         MVC   SVMINEKY,MINEKEY                                                 
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
         B     EXITERR             EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITERR             EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITERR             EXIT WITH FIELD NOT NUMERIC SET              
EXITRCNF MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITERR             EXIT WITH RECORD NOT ON FILE                 
EXITRCDL MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         B     EXITERR             EXIT WITH RECORD IS DELETED                  
EXITRCAE MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     EXITERR             EXIT WITH RECORD ALREADY EXISTS              
EXITCRES MVC   FVMSGNO,=AL2(FVFXRES)                                            
         B     EXITERR             EXIT WITH RECORD CAN'T BE RESTORED           
EXITCCHG MVC   FVMSGNO,=AL2(CSTCHNGD)                                           
         B     EXITERR             EXIT WITH COST WAS ALREADY CHANGED           
TABCHNGD MVC   FVMSGNO,=AL2(BCPPCHGD)                                           
         B     EXITERR             EXIT WITH BUYER'S CPP WAS CHANGED            
EXITPF12 MVC   FVMSGNO,=AL2(SELKYCHG)                                           
         B     EXITL               EXIT WITH SEL KEY WAS CHANGED - ...          
*                                                                               
EXITERR  DS    0H                                                               
         B     EXITL                                                            
*                                                                               
DIE      DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
*                                                                               
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
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
         BNE   RDBDBK60                                                         
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRBKELD,R6                                                      
*                                                                               
RDBDBK10 CLI   0(R6),RPRBKELQ                                                   
         BNE   RDBDBK60                                                         
*                                                                               
         ZIC   R1,RPRBKDOR         DISPLAY ORDER NUMBER                         
         BCTR  R1,0                                                             
         LR    RE,R1                                                            
         MH    R1,=Y(L'SAVBK)                                                   
         LA    R1,SAVBKS(R1)                                                    
*                                                                               
         USING BOOKLIN,R1                                                       
*                                                                               
         CLI   RPRBKLEN,RPRBKOVQ   USER DEFINED BOOK?                           
         BH    RDBDBK20            YES                                          
*********                                                                       
* REGULAR BOOK                                                                  
*********                                                                       
         TM    RPRBKSTT,RPRBKSES+RPRBKSPJ+RPRBKST2+RPRBKSTP                     
         BNZ   RDBDBK50            SKIP E/P/S/T                                 
*                                                                               
         MVC   BKLNDORD,RPRBKDOR   DISPLAY ORDER #                              
         MVC   BKLNIORD,RPRBKIOR   INTERNAL ORDER #                             
         MVC   BKLNFLG,RPRBKFLG    FLAGS                                        
         MVC   BKLNBK,RPRBKSTT     BOOK                                         
         MVC   BKLNSPBK,RPRBKBKT   SPECIAL BOOK TYPE                            
         MVC   BKLNFIL,RPRBKFIL    BOOK SOURCE(I/T/P/4)                         
         B     RDBDBK50                                                         
*********                                                                       
* USER-DEFINED BOOK                                                             
*********                                                                       
RDBDBK20 DS    0H                                                               
         MVC   BKLNDORD,RPRBKDOR   DISPLAY ORDER #                              
         MVC   BKLNIORD,RPRBKIOR   INTERNAL ORDER #                             
         MVC   BKLNFLG,RPRBKFLG    FLAGS                                        
         MVC   BKLNBK,RPRBKSTT     BOOK                                         
         MVC   BKLNSPBK,RPRBKBKT   SPECIAL BOOK TYPE                            
         MVC   BKLNFIL,RPRBKFIL    BOOK SOURCE(I/T/P/4)                         
         MVC   BKLNUPGD,RPRBKBKS   UPGRADE FORMULA                              
         MVC   BKLNXBKS,RPRBKXBK   EXTRA BASE BOOKS                             
         DROP  R1                                                               
*                                                                               
         MH    RE,=Y(L'SAVLBL)                                                  
         LA    RE,SAVLBLS(RE)                                                   
         MVC   0(L'SAVLBL,RE),RPRBKUDF  SAVE THE LABEL                          
*                                                                               
RDBDBK50 BAS   RE,MINIOSEQ                                                      
         BE    RDBDBK10                                                         
*                                                                               
RDBDBK60 OC    SAVBKS,SAVBKS       ANY BOOKS?                                   
         BNZ   *+14                YES                                          
         MVC   FVMSGNO,=AL2(FVFMIXI)                                            
         B     EXITL                                                            
*                                                                               
         L     RE,AIO1             CLEAR TEMP AREA                              
         LA    RF,IOAREALN                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,AIO1             COPY BOOK LIST                               
         LA    RF,L'SAVBKS                                                      
         LA    R0,SAVBKS                                                        
         LA    R1,L'SAVBKS                                                      
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,AIO1             COPY LABEL LIST                              
         LA    RE,L'SAVBKS(RE)                                                  
         LA    RF,L'SAVLBLS                                                     
         LA    R0,SAVLBLS                                                       
         LA    R1,L'SAVLBLS                                                     
         MVCL  RE,R0                                                            
*                                                                               
         LA    RE,SAVBKS           CLEAR BOOK LIST                              
         LA    RF,L'SAVBKS                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    RE,SAVLBLS          CLEAR LABEL LIST                             
         LA    RF,L'SAVLBLS                                                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,AIO1             COMPRESS BOOK LIST                           
         LA    RF,L'SAVBKS(RE)                                                  
         LA    R1,SAVBKS                                                        
RDBDBK62 DS    0H                                                               
         CLI   BKLNIORD-BOOKLIN(RE),0                                           
         BE    *+14                                                             
         MVC   0(L'SAVBK,R1),0(RE)                                              
         LA    R1,L'SAVBK(R1)                                                   
         LA    RE,L'SAVBK(RE)                                                   
         CR    RE,RF                                                            
         BL    RDBDBK62                                                         
*                                                                               
         L     R1,AIO1             COMPRESS LABEL LIST                          
         LA    R1,L'SAVBKS(R1)                                                  
         LA    RE,SAVBKS                                                        
         LA    RF,SAVLBLS                                                       
         LA    R0,L'SAVBKS(RE)                                                  
RDBDBK64 DS    0H                                                               
         ZIC   R6,BKLNDORD-BOOKLIN(RE)                                          
         LTR   R6,R6                                                            
         BZ    RDBDBK66                                                         
         BCTR  R6,0                                                             
         MH    R6,=Y(L'SAVLBL)                                                  
         LA    R6,0(R1,R6)                                                      
         MVC   0(L'SAVLBL,RF),0(R6)                                             
RDBDBK66 DS    0H                                                               
         LA    RE,L'SAVBK(RE)                                                   
         LA    RF,L'SAVLBL(RF)                                                  
         CR    RE,R0                                                            
         BL    RDBDBK64                                                         
*                                                                               
         LR    RE,RA               SAVE THESE SO WE KNOW WHAT CHANGED           
         AH    RE,=Y(MINBKS-TWAD)                                               
*                                                                               
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
         USING DEMOLIN,R1                                                       
*                                                                               
         MVC   DMLNIORD,RPRDMIOR                                                
         MVC   DMLNDORD,RPRDMDOR                                                
         MVC   DMLNDEMO,RPRDMBY1                                                
         MVC   DMLNFLG,RPRDMFLG                                                 
         DROP  R1                                                               
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
         MVC   1(4,R1),RPRDPTAB                                                 
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
         USING STALIN,R1                                                        
         MVC   STLNIORD,RPRSTICD                                                
         MVC   STLNSTA,RPRSTSTA                                                 
         MVC   STLNFLG,RPRSTFLG                                                 
*                                                                               
         TM    RPRSTFLG,RPRSTSTL   SATELLITE STATION?                           
         BZ    RDBDST20                                                         
         MVI   STLNSTA+4,C'1'      YES, C'1' AFTER STATION CALL LTRS            
         DROP  R1                                                               
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
         CLC   SVPARMS4,ATLST                                                   
         BE    DFDDISX                                                          
*                                                                               
         SPACE 2                                                                
****************************************************                            
** CHECK IF FETCH REQUIRED FOR STATIONS & DAYPART **                            
****************************************************                            
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
         SPACE 2                                                                
*****************************************************                           
** CALL 'T80A24' TO DO THE FETCH HEADER DATA FETCH **                           
*****************************************************                           
DFDDIS10 GOTO1 VCOLY,BODMCB,(X'24',0),(0,0)                                     
         CLI   BODMCB+4,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,BODMCB                                                        
         GOTO1 (RF),BODMCB,(R9)                                                 
         BL    EXITL                                                            
         OI    LSSCIND1,LSSCIBLD   REBUILD THE LIST ALSO                        
         SPACE 2                                                                
         EJECT                                                                  
***************                                                                 
* DESCRIPTION ELEMENT                                                           
***************                                                                 
DFDDIS20 DS    0H                                                               
         MVI   SAVOPTNS,0                                                       
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
         BL    EXITL                                                            
         SPACE 2                                                                
**********************************************************                      
** DO FETCH FOR ALL BOOKS AND DEMOS ON THIS DETAIL LINE **                      
**********************************************************                      
**       TM    MISCFLG1,MF1PFRET   RETURNING FROM CALLED SESSION?               
**       BNZ   EXITL                                                            
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         MVC   BOWORK1(L'SVSTATN),0(RE)                                         
         LR    RE,RA                                                            
         AH    RE,=Y(DPTINKEY-TWAD)                                             
         MVC   BOWORK1+1(L'DPTINKEY),0(RE)                                      
         MVC   BOWORK1+2(L'DAYINKEY),DAYINKEY-DPTINKEY(RE)                      
         MVC   BOWORK1+3(L'PTMINMKY),PTMINMKY-DPTINKEY(RE)                      
         MVC   BOWORK1+5(L'SEQINKEY),SEQINKEY-DPTINKEY(RE)                      
*                                                                               
         GOTOX (DTLNFTQ,AREPRO01),BODMCB,SAVSTAS,SAVBKS,SAVDMOS,       X        
               BOWORK1                                                          
*                                                                               
**       BAS   RE,GETCLSTR                                                      
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
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
         DC    AL1(LUPDDIR),AL1(0,0,1),AL4(UPDDIR1)                             
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(LGETFRST),AL1(0,0,2),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,2),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,2),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,2),AL4(INITL1)                                
         DC    AL1(LDEFCLM),AL1(0,0,2),AL4(DEFCLM1)                             
         DC    AL1(LTSARDIR),AL1(0,0,2),AL4(TSARDIR1)                           
         DC    AL1(LTSARFIL),AL1(0,0,2),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,2),AL4(UPDFRST1)                           
         DC    AL1(LUPDLAST),AL1(0,0,2),AL4(UPDLAST1)                           
         DC    AL1(LUPDDIR),AL1(0,0,2),AL4(UPDDIR1)                             
         DC    AL1(LUPDREC),AL1(0,0,2),AL4(UPDREC1)                             
         DC    AL1(EOT)                                                         
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
KNOWTAB  DS    0XL(KNOWLQ)                                                      
* KEY (UNPROTECTED PORTION)                                                     
         DC    AL2(00001),AL4(CONDTA)    CONTRACT                               
         DC    AL2(00002),AL4(PRODTA)    PROPOSAL                               
         DC    AL2(00008),AL4(STADTA)    STATION                                
         DC    AL2(00073),AL4(DPTDTA)    DAYPART                                
         DC    AL2(00071),AL4(DTMDTA)    DAY/TIME                               
         DC    AL2(00021),AL4(SEQDTA)    SEQUENCE NUMBER FOR DUPLICATES         
         DC    AL2(00014),AL4(DMODTA)    DEMO                                   
* KEY (PROTECTED PORTION)                                                       
         DC    AL2(00010),AL4(DSKDTA)    DISK ADDRESS                           
* RECORD (PROTECTED PORTION)                                                    
         DC    AL2(00110),AL4(LENDTA)    LENGTH                                 
         DC    AL2(00118),AL4(PRGDTA)    PROGRAM                                
         DC    AL2(00119),AL4(SHRDTA)    PROGRAM                                
* BOOK FIELDS                                                                   
         DC    AL2(00082),AL4(LSTADTA)   STATION                                
         DC    AL2(00111),AL4(LRTGDTA)   BOOK #1'S RATING                       
         DC    AL2(00112),AL4(LRTGDTA)   BOOK #2'S RATING                       
         DC    AL2(00113),AL4(LRTGDTA)   BOOK #3'S RATING                       
         DC    AL2(00114),AL4(LRTGDTA)   BOOK #4'S RATING                       
         DC    AL2(00115),AL4(LRTGDTA)   BOOK #5'S RATING                       
         DC    AL2(00116),AL4(LRTGDTA)   BOOK #6'S RATING                       
         DC    AL2(00117),AL4(LRTGDTA)   BOOK #7'S RATING                       
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 FIELD NUMBER                                 
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
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
MF1PFCOM EQU   X'20'                - COMING FROM A CALLER SESSION              
MF1DS1ST EQU   X'02'                - DISPLAYED FIRST LINE ALREADY              
MF1TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
PFTEXT   EQU   PFK01               PFKEY FOR TEXT                               
PFKYUP   EQU   PFK07               PFKEY FOR SCROLL UP                          
PFKYDOWN EQU   PFK08               PFKEY FOR SCROLL DOWN                        
PFKYLEFT EQU   PFK09               PFKEY FOR SCROLL LEFT                        
PFKYRGHT EQU   PFK10               PFKEY FOR SCROLL RIGHT                       
PFNEXT   EQU   PFK11               PFKEY FOR NEXT                               
PFRETURN EQU   PFK12               PFKEY FOR RETURN                             
*                                                                               
PRMBOOKQ EQU   111                 FIELD NUMBER FOR PRIME BOOK                  
*                                                                               
FILTDPT  DS    CL1                 DAYPART FILTER                               
*                                                                               
PCKOF06B DS    PL6                 PACKED OF 6  BYTES                           
PCKOF08B DS    PL8                 PACKED OF 8  BYTES                           
PCKOF16B DS    PL16                PACKED OF 16 BYTES                           
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE AREA                          
*                                                                               
STACNTR  DS    XL1                 OFFSET FROM 1ST STATION                      
LINECNTR DS    XL1                 ROW NUMBER OF TSAR LIST LINE                 
*                                                                               
SVKYDYPT DS    CL1                 DAYPART BUILT BY NTRIN                       
SVKYDYTM DS    CL19                DAY/TIME BUILT BY NTRIN                      
SVKYSEQN DS    CL3                 SEQUENCE NUMBER BY NTRIN                     
*                                                                               
SAVSTAS  DS    0XL(NUMSTAS*STLNLENQ)     SAVED STATIONS                         
SAVSTA   DS    (NUMSTAS)XL(STLNLENQ)                                            
*                                                                               
SAVBKS   DS    0XL(NUMBKS*BKLNLENQ)      SAVED BOOKS                            
SAVBK    DS    (NUMBKS)XL(BKLNLENQ)                                             
*                                                                               
SAVLBLS  DS    0CL(NUMBKS*5)             SAVED LABELS                           
SAVLBL   DS    (NUMBKS)CL5                                                      
*                                                                               
SAVDMOS  DS    0CL(NUMDEMS*DMLNLENQ)     SAVED DEMOS                            
SAVDMO   DS    (NUMDEMS)XL(DMLNLENQ)                                            
*                                                                               
SAVDPTS  DS    0CL(NUMDPTS*(1+4))       - 1 BYTE DAYPART CODE                   
SAVDPT   DS    (NUMDPTS)CL(1+4)         - 4 BYTE BYR CPP                        
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
*                                                                               
SVSTANO  DS    XL1                                                              
NMAXSTA  EQU   21                                                               
SVSTA    DS    XL(NMAXSTA*5)                                                    
SVSTA1   DS    (NMAXSTA)XL5                                                     
*                                                                               
SAVFTDT  DS    XL(8*L'RPRDYDTM)                                                 
*                                                                               
SAVDMX   DS    XL(DMLNLENQ)        SAVED PRIME DEMO                             
*                                                                               
DEMBLOCK DS    XL256                                                            
         EJECT                                                                  
       ++INCLUDE REPROLN                                                        
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
MINSTAS  DS    0XL(NUMSTAS*STLNLENQ)     SAVED STATIONS                         
MINSTA   DS    (NUMSTAS)XL(STLNLENQ)                                            
*                                                                               
MINBKS   DS    0XL(NUMBKS*BKLNLENQ)      SAVED BOOKS                            
MINBK    DS    (NUMBKS)XL(BKLNLENQ)                                             
*                                                                               
MINLBLS  DS    0CL(NUMBKS*5)             SAVED LABELS                           
MINLBL   DS    (NUMBKS)CL5                                                      
*                                                                               
MINDMOS  DS    0CL(NUMDEMS*DMLNLENQ)     SAVED DEMOS                            
MINDMO   DS    (NUMDEMS)XL(DMLNLENQ)                                            
*                                                                               
MINDPTS  DS    0CL(NUMDPTS*(1+4))       - 1 BYTE DAYPART CODE                   
MINDPT   DS    (NUMDPTS)CL(1+4)         - 4 BYTE BYR CPP                        
*                                                                               
DPTINKEY DS    CL1                 DAYPART IN THE KEY SCREEN                    
DAYINKEY DS    XL1                 DAY BITS IN KEY SCREEN                       
TIMINKEY DS    XL4                 TIMES IN KEY SCREEN                          
SEQINKEY DS    XL1                 SEQUENCE NUMBER IN KEY SCREEN                
PTMINMKY DS    XL2                 PACKED TIME FOR MINIO ELEM KEY               
******                                                                          
CLCMPAGE DS    XL1                 PAGE WE'RE UPTO                              
CLCMSHRS DS    0XL(NUMBKS*(1+4))   COL# & CUMMULATIVE SHARES FOR BOOKS          
CLCMSHR  DS    (NUMBKS)XL(1+4)                                                  
******                                                                          
FOO      DS    XL1000                                                           
******                                                                          
         DS    XL(TWUSER+L'TWUSER-*)   # OF SPARE BEFORE TWSECBLK               
         DS    0X                                                               
         EJECT                                                                  
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
* DEDEMFILE                                                                     
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
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKEYD   DS    0X                                                               
TLKSCNTR DS    XL1                 DISPLAYED ORDER NUMBER                       
TLKSTATN DS    CL5                 STATION EBCDIC                               
TLKLINE# DS    XL1                 LINKED TSAR LINE # (ROWS 1-3)                
         ORG   TLUSER                                                           
TLRECD   DS    0X                                                               
TLRFTBKS DS    0CL(NUMBKS*17)      FOOTNOTES FOR THE BOOKS                      
TLRFTBK  DS    (NUMBKS)CL17                                                     
TLRDMBKS DS    0XL(NUMBKS*12)      DEMO FOR THE BOOKS                           
TLRDMBK  DS    (NUMBKS)XL12                                                     
TLRFLAG1 DS    XL1                 FLAG                                         
TLRF1LST EQU   X'80'                - LAST LINE IN ENTRY                        
TLRLNQ   EQU   *-TLSTD             LENGTH OF TSAR RECORD                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007REPRO53   03/04/98'                                      
         END                                                                    
