*          DATA SET REPRO43    AT LEVEL 010 AS OF 08/22/97                      
*&&      SET   NOP=N                                                            
*PHASE T80A43A                                                                  
T80A43   TITLE 'REPRO43 - REP PROPOSALS WORK/DEMO'                              
PRO43    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REPRO43*,R7,RR=RE                                              
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
         MVI   MINIOFLG,0                                                       
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
*                                                                               
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
         BAS   RE,MINIOCLS                                                      
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
         MVC   SVRECDA,GSRECDA                                                  
         GOTO1 =A(D1STDDIS),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
***********************************************************************         
* BEFORE VALIDATING THE DATA FIELDS                                             
***********************************************************************         
DFDVAL   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS-TWAD)                                              
         MVC   SAVSTAS,0(RE)                                                    
         LR    RE,RA                                                            
         AH    RE,=Y(MINBKS-TWAD)                                               
         MVC   SAVBKS,0(RE)                                                     
         LR    RE,RA                                                            
         AH    RE,=Y(MINLBLS-TWAD)                                              
         MVC   SAVLBLS,0(RE)                                                    
         LR    RE,RA                                                            
         AH    RE,=Y(MINDMOS-TWAD)                                              
         MVC   SAVDMOS,0(RE)                                                    
         LR    RE,RA                                                            
         AH    RE,=Y(MINDPTS-TWAD)                                              
         MVC   SAVDPTS,0(RE)                                                    
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
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
         DROP  R5,R6                                                            
*                                                                               
         BAS   RE,GETCLSTR                                                      
         BAS   RE,INTOAIO5                                                      
*                                                                               
DFDVALX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* AFTER WE'VE DONE EVERYTHING TO THE DATA FIELDS ON THE SCREEN                  
***********************************************************************         
DTALAST  DS    0H                                                               
         L     R1,SVPARMS3         VERB                                         
         LA    RF,DLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
DLTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER VALIDATING THE DATA FIELDS                                              
***********************************************************************         
DLDVAL   DS    0H                                                               
         OI    LSSCIND2,LSSCIPAG   REDISPLAY PAGE IF CHANGE MADE SO WE          
*                                    CAN DISPLAY AFFECTED RTG/SHR/LVL           
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         BAS   RE,FROMAIO5                                                      
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         MVC   RPRDTNC1,SAVCOST                                                 
         DROP  R5,R6                                                            
*                                                                               
                                                                                
         BAS   RE,MINIOWRT                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
************ DOING AN ACTION ON A SPECIFIC DATA OBJECT ****************         
***********************************************************************         
DATA10   DS    0H                                                               
         LR    RF,RB               TABLE OF KNOWN OBJECTS                       
         AH    RF,=Y(KNOWTAB-PRO43)                                             
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
* DISPLAY INVENTORY ID FIELD                                                    
***********************************************************************         
PRGDTA   DS    0H                                                               
         BAS   RE,GETCLSTR                                                      
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
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
***********************************************************************         
* DISPLAY INVENTORY ID FIELD                                                    
***********************************************************************         
INVDTA   DS    0H                                                               
         BAS   RE,GETCLSTR                                                      
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         MVC   FVIFLD(L'RPRDTINM),RPRDTINM                                      
         OI    FVATRB,FVAPROT                                                   
         B     EXITOK                                                           
         DROP  R5,R6                                                            
***********************************************************************         
* DISPLAY EFFECTIVE DATES FIELD                                                 
***********************************************************************         
EDTDTA   DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         OC    RPRDTEFF,RPRDTEFF               EFFECTIVE DATE START?            
         BZ    DISEDTX                         NONE                             
*                                                                               
         GOTO1 VDATCON,BODMCB,(8,RPRDTEFF),(17,FVIFLD)                          
*                                                                               
         OC    RPRDTEEF,RPRDTEEF                                                
         BZ    DISEDTX                         NONE                             
         MVI   FVIFLD+8,C'-'                                                    
         GOTO1 VDATCON,BODMCB,(8,RPRDTEEF),(17,FVIFLD+9)                        
*                                                                               
DISEDTX  OI    FVATRB,FVAPROT                                                   
         B     EXITOK                                                           
         DROP  R5,R6                                                            
***********************************************************************         
* DISPLAY LENGTH FIELD                                                          
***********************************************************************         
LENDTA   DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         EDIT  (B1,RPRDTSLN),(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,        X        
               DUB=BODUB1                                                       
         OI    FVATRB,FVAPROT                                                   
*                                                                               
DISLENX  B     EXITOK                                                           
         DROP  R5,R6                                                            
***********************************************************************         
* DATA OBJECT FOR COST FIELD                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
CSTDTA   DS    0H                                                               
         LA    RF,CSTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CSTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCST)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCST)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY COST FIELD                                                            
***********************************************************************         
DISCST   DS    0H                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   *+8                                                              
         OI    FVATRB,FVAPROT                                                   
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         MVC   SAVCOST,RPRDTNC1                                                 
         TM    RPRDTNC1,X'80'      N/A?                                         
         BZ    *+14                                                             
         MVC   FVIFLD(2),=C'NA'                                                 
         B     DISCSTX                                                          
*                                                                               
         EDIT  (B4,RPRDTNC1),(17,FVIFLD),2,ALIGN=LEFT,WRK=BOWORK1,     X        
               DUB=BODUB1                                                       
*                                                                               
DISCSTX  B     EXITOK                                                           
         DROP  R5,R6                                                            
***********************************************************************         
* VALIDATE COST FIELD                                                           
***********************************************************************         
VALCST   DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
*                                                                               
         CLC   =C'NA',FVIFLD                                                    
         BNE   *+14                                                             
         MVC   SAVCOST,=X'80000000'                                             
         B     VALCSTX                                                          
*                                                                               
         ZIC   R0,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(2,FVIFLD),(R0)                                  
         CLI   0(R1),0                                                          
         BNE   EXITNV                                                           
*                                                                               
         MVC   SAVCOST,BODMCB+4                                                 
*                                                                               
VALCSTX  B     EXITOK                                                           
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEMO NAME FIELD                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LDNMDTA  DS    0H                                                               
         LA    RF,LDNMTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LDNMTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLDNM)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEMO NAME FIELD                                                       
***********************************************************************         
DISLDNM  DS    0H                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         OI    FVATRB,FVAPROT      ALWAYS PROTECTED                             
*                                                                               
         CLC   LSROWREP,=H'1'      NOT THE RATINGS LINE?                        
         BH    DISLDNMX            NO, DON'T SHOW DEMO NAME AGAIN               
*                                                                               
         L     R6,AIO5                                                          
         USING DBLOCK,R6                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOM                                                    
         MVI   DBSELMED,C'T'                                                    
         LA    R5,BOWORK1                                                       
         XC    BOWORK1(50),BOWORK1                                              
         MVC   BOWORK1(L'DMLNDEMO),TLKDMDMO                                     
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         DROP  R6                                                               
*                                                                               
         GOTO1 VDEMOCON,BODMCB,(1,BOWORK1),(9,FVIFLD),(0,AIO5)                  
*                                                                               
DISLDNMX B     EXITOK                                                           
         DROP  R2                                                               
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLRTG)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEMO FIELD HEADLINE FOR THE COLUMN                                    
***********************************************************************         
HEDLRTG  DS    0H                                                               
**********************************************************                      
** DO FETCH FOR ALL BOOKS AND DEMOS ON THIS DETAIL LINE **                      
**********************************************************                      
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
         GOTOX (DTLNFTQ,AREPRO01),BODMCB,SAVSTAS,(X'40',SAVBKS),       X        
               SAVDMOS,BOWORK1                                                  
*                                                                               
         L     R1,SVPARMS6                                                      
         MVC   0(20,R1),BCSPACES                                                
*                                                                               
         SR    R1,R1                 PRIME IS DEFAULT                           
         CLI   SVPARMS2+3,PRMBK2Q    BOOK FIELD HERE?                           
         BL    HEDFNT02              YES                                        
         IC    R1,SVPARMS2+3         BIG BOOK FIELD                             
         SH    R1,=Y(PRMBK2Q)                                                   
         B     HEDFNT04                                                         
HEDFNT02 IC    R1,SVPARMS2+3         YES, FIND OUT WHICH BOOK FIELD             
         SH    R1,=Y(PRMBKQ)         BY SUBTRACTING PRIME EQUATE                
HEDFNT04 DS    0H                                                               
         MH    R1,=Y(L'SAVBK)                                                   
         LA    R1,SAVBKS(R1)                                                    
         USING BOOKLIN,R1                                                       
         ZIC   R3,BKLNIORD           INTERNAL ORDER NUMBER OF THE BOOK          
         DROP  R1                                                               
*                                                                               
         L     RE,AIOREC                                                        
HEDFNT06 CLI   0(RE),0                                                          
         BE    HDLRTG00                                                         
         CLM   R3,1,1(RE)           BOOK MATCH?                                 
         BE    HEDFNT08                                                         
         ZIC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         B     HEDFNT06                                                         
*                                                                               
HEDFNT08 DS    0H                                                               
*                                                                               
         L     R1,SVPARMS6                                                      
         ZIC   RF,0(RE)                                                         
         SH    RF,=H'6'            5+1 FOR EX                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),5(RE)                                                    
*                                                                               
         CLI   7(R1),C'/'                                                       
         BNE   *+8                                                              
         MVI   6(R1),C'/'                                                       
*                                                                               
HDLRTG00 L     R2,SVPARMS5                                                      
         ZIC   R1,SVPARMS2+3       FIELD NUMBER                                 
***************                                                                 
* BOOK NAME FOR THE RATING COLUMN                                               
***************                                                                 
         SH    R1,=Y(PRMBKQ)       OFFSETED BY THE PRIME BOOK                   
         LR    R3,R1                                                            
         MH    R3,=Y(L'SAVBK)                                                   
         LA    R3,SAVBKS(R3)                                                    
         USING BOOKLIN,R3                                                       
         LR    R4,R1                                                            
         MH    R4,=Y(L'SAVLBL)                                                  
         LA    R4,SAVLBLS(R4)                                                   
*                                                                               
         OC    0(L'SAVLBL,R4),0(R4)   USER DEFINED BOOK?                        
         BZ    *+14                                                             
         MVC   0(5,R2),0(R4)                                                    
         B     HEDLRTGX                                                         
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
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
         USING TLSTD,R2                                                         
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         BAS   RE,INTOAIO5                                                      
*                                                                               
         ZIC   R1,SVPARMS2+3       FIND OUT WHICH BOOK FIELD                    
         SH    R1,=Y(PRMBKQ)       BY SUBTRACTING PRIME EQUATE                  
*                                                                               
         CLC   LSROWREP,=H'4'                                                   
         BE    DISLCPP                                                          
*                                                                               
DSLRTG00 MH    R1,=Y(L'SAVBK)      RE = INTERNAL ORDER NUMBER OF BOOK           
         LA    R1,SAVBKS(R1)                                                    
         USING BOOKLIN,R1                                                       
         ZIC   RE,BKLNIORD                                                      
         OC    BKLNUPGD,BKLNUPGD   UPGRADE?                                     
         BNZ   *+16                                                             
         TM    SELPROF+SELDMOVB,SELDMOVA                                        
         BZ    *+8                                                              
         OI    FVATRB,FVAPROT                                                   
         DROP  R1                                                               
*                                                                               
         L     R6,AIO5                                                          
         USING RPROKEY,R6                                                       
         LA    R6,RPROR1ST                                                      
DSLRTG01 CLI   0(R6),0             NO DEMO VALUE ELEMENT FOR THIS BOOK          
         BNE   *+12                                                             
DSLRTG02 MVI   FVIFLD,C'0'                                                      
         B     DISLRTGX                                                         
*                                                                               
         USING RPRDVELD,R6                                                      
         CLI   0(R6),RPRDVELQ      FIND THE DEMO VALUE ELEMENT FOR THE          
         BE    DSLRTG06              INTERNAL BOOK NUMBER                       
DSLRTG04 ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DSLRTG01                                                         
*                                                                               
DSLRTG06 CLM   RE,1,RPRDVBNM       MATCHING ON BOOK NUMBER                      
         BNE   DSLRTG04                                                         
*                                                                               
         ZIC   R1,TLKDCNTR                                                      
         MH    R1,=Y(L'SAVDMO)                                                  
         LA    R1,SAVDMOS(R1)                                                   
         USING DEMOLIN,R1                                                       
         ZIC   R3,DMLNIORD                                                      
         DROP  R1                                                               
*                                                                               
         BCTR  R3,0                MAKE SURE DEMO IS IN ELEMENT                 
         MH    R3,=Y(L'RPRDVDMO)                                                
         LA    R3,RPRDVDMO(R3)                                                  
         LR    RF,R3                                                            
         LA    R0,RPRDVEL                                                       
         SR    RF,R0                                                            
         CLM   RF,1,RPRDVLEN                                                    
         BNL   DSLRTG02                                                         
*                                                                               
         LH    R1,LSROWREP                                                      
         BCTR  R1,0                                                             
         MH    R1,=H'4'                                                         
         LA    R3,0(R3,R1)                                                      
*                                                                               
         MVC   BOFULL1,0(R3)                                                    
         LA    R4,FVIFLD                                                        
         TM    BOFULL1,X'80'       IS THIS A DEMO OVERRIDE?                     
         BZ    DSLRTG15                                                         
         MVI   0(R4),C'*'          YES, THEN SHOW AN '*' BEFORE RATING          
         LA    R4,1(R4)                                                         
         NI    BOFULL1,X'FF'-X'80'   GET RID OF DEMO OVERRIDE BIT               
*                                                                               
DSLRTG15 TM    SAVOPTNS,OPTNDECQ   DISPLAY DEMO PRECISION?                      
         BZ    DSLRTG20            NO                                           
         EDIT  (B4,BOFULL1),(5,0(R4)),1,ALIGN=LEFT,WRK=BOWORK1,        X        
               DUB=BODUB1,ZERO=NOBLANK                                          
         B     DSLRTG30                                                         
*                                                                               
DSLRTG20 L     R3,BOFULL1          DON'T DISPLAY DEMO PRECISION                 
         CVD   R3,BODUB1                                                        
         ZAP   PCKOF08B,BODUB1                                                  
         SRP   PCKOF08B,64-1,5     ROUND THE # OFF INSTEAD                      
         EDIT  (P8,PCKOF08B),(5,0(R4)),ALIGN=LEFT,WRK=BOWORK1,         X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*&&DO                                                                           
         ZIC   R1,SVPARMS2+3        FIND OUT WHICH BOOK FIELD                   
         SH    R1,=Y(PRMBKQ)        BY SUBTRACTING PRIME EQUATE                 
         LR    R3,R1                                                            
         MH    R3,=Y(L'TLRDEMOS)                                                
         LA    R3,TLRDEMOS(R3)                                                  
*                                                                               
         LH    R1,LSROWREP                                                      
         BCTR  R1,0                                                             
         MH    R1,=H'4'                                                         
         LA    R3,0(R1,R3)                                                      
*                                                                               
         MVC   BOFULL1,0(R3)                                                    
         LA    R4,FVIFLD                                                        
         TM    BOFULL1,X'80'       IS THIS A DEMO OVERRIDE?                     
         BZ    DSLRTG15                                                         
         MVI   0(R4),C'*'          YES, THEN SHOW AN '*' BEFORE RATING          
         LA    R4,1(R4)                                                         
         NI    BOFULL1,X'FF'-X'80'   GET RID OF DEMO OVERRIDE BIT               
*                                                                               
DSLRTG15 TM    SAVOPTNS,OPTNDECQ   DISPLAY DEMO PRECISION?                      
         BZ    DSLRTG20            NO                                           
         EDIT  (B4,BOFULL1),(5,0(R4)),1,ALIGN=LEFT,WRK=BOWORK1,        X        
               DUB=BODUB1,ZERO=NOBLANK                                          
         B     DSLRTG30                                                         
*                                                                               
DSLRTG20 L     R3,BOFULL1          DON'T DISPLAY DEMO PRECISION                 
         CVD   R3,BODUB1                                                        
         ZAP   PCKOF08B,BODUB1                                                  
         SRP   PCKOF08B,64-1,5     ROUND THE # OFF INSTEAD                      
         EDIT  (P8,PCKOF08B),(5,0(R4)),ALIGN=LEFT,WRK=BOWORK1,         X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*&&                                                                             
DSLRTG30 DS    0H                                                               
*                                                                               
DISLRTGX B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* DISPLAY CPP FIELD                                                             
***********************************************************************         
DISLCPP  DS    0H                                                               
         USING TLSTD,R2                                                         
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         OI    FVATRB,FVAPROT                                                   
*                                                                               
         TM    SAVCOST,X'80'       N/A?                                         
         BZ    *+14                NO                                           
         MVC   FVIFLD(2),=C'NA'                                                 
         B     DISLCPPX                                                         
*                                                                               
         ZIC   R1,SVPARMS2+3       FIND OUT WHICH BOOK FIELD                    
         SH    R1,=Y(PRMBKQ)       BY SUBTRACTING PRIME EQUATE                  
*                                                                               
         MH    R1,=Y(L'SAVBK)      RE = INTERNAL ORDER NUMBER OF BOOK           
         LA    R1,SAVBKS(R1)                                                    
         USING BOOKLIN,R1                                                       
         ZIC   RE,BKLNIORD                                                      
         DROP  R1                                                               
*                                                                               
         L     R6,AIO5                                                          
         USING RPROKEY,R6                                                       
         LA    R6,RPROR1ST                                                      
DSLCPP01 CLI   0(R6),0             NO DEMO VALUE ELEMENT FOR THIS BOOK          
         BNE   *+12                                                             
DSLCPP02 MVI   FVIFLD,C'0'                                                      
         B     DISLCPPX                                                         
*                                                                               
         USING RPRDVELD,R6                                                      
         CLI   0(R6),RPRDVELQ      FIND THE DEMO VALUE ELEMENT FOR THE          
         BE    DSLCPP06              INTERNAL BOOK NUMBER                       
DSLCPP04 ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DSLCPP01                                                         
*                                                                               
DSLCPP06 CLM   RE,1,RPRDVBNM       MATCHING ON BOOK NUMBER                      
         BNE   DSLCPP04                                                         
*                                                                               
         ZIC   R1,TLKDCNTR                                                      
         MH    R1,=Y(L'SAVDMO)                                                  
         LA    R1,SAVDMOS(R1)                                                   
         USING DEMOLIN,R1                                                       
         ZIC   R3,DMLNIORD                                                      
         DROP  R1                                                               
*                                                                               
         BCTR  R3,0                MAKE SURE DEMO IS IN ELEMENT                 
         MH    R3,=Y(L'RPRDVDMO)                                                
         LA    R3,RPRDVDMO(R3)                                                  
         LR    RF,R3                                                            
         LA    R0,RPRDVEL                                                       
         SR    RF,R0                                                            
         CLM   RF,1,RPRDVLEN                                                    
         BNL   DSLCPP02                                                         
*                                                                               
         MVC   BOFULL1,0(R3)                                                    
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ                                                
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5                                                    
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF08B,BODUB1                                                  
*                                                                               
         ICM   R0,15,SAVCOST         NEGOTIATED COST                            
         CVD   R0,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         BAS   RE,DIVPACKD         CPP = COST / RATING                          
         BE    DSLCPP30                                                         
         MVI   FVIFLD,C'*'         YES, '****' OUT THE CPP                      
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
         B     DISLCPPX                                                         
*                                                                               
DSLCPP30 EDIT  (P16,PCKOF16B),(7,FVIFLD),2,WRK=BOWORK1,ALIGN=LEFT,     X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
DISLCPPX DS    0H                                                               
         B     EXITOK                                                           
         DROP  R5,R2,R6                                                         
***********************************************************************         
* VALIDATE RATING FIELD                                                         
***********************************************************************         
VALLRTG  DS    0H                                                               
         GOTO1 =A(VALRATNG),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
         B     EXITOK                                                           
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
         AH    RF,=Y(LISTABL-PRO43)                                             
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
         CLI   SELPROFS,RREPQSEL                                                
         BE    DEFCL0                                                           
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
DEFCL0   DS    0H                                                               
         MVC   LSROWLIN,=AL2(4)                                                 
         MVC   LSCOLLIN,=AL2(80)                                                
         OI    LSSTAT1,LSSMUROW                                                 
         OI    LSSTAT3,LS3RFIX    USES MULTIROW LIST                            
*                                                                               
         LA    RF,LSFIXCLM         RF = A(1ST FIXED COLUMN)                     
         USING DCTABD,RF                                                        
*                                                                               
         MVC   DCTFLD#,=AL2(14)    DEMO FIELD                                   
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(41)    REGULAR PROGRAM FIELD                        
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         MVC   LSFIXNUM,=AL2(2)    # OF FIXED COLUMNS                           
*                                                                               
         SR    R3,R3                                                            
DEFCL20  DS    0H                                                               
         LA    RF,LSVARCLM           RF = A(1ST VARIABLE COLUMN)                
         LA    RE,SAVBKS+L'SAVBK     RE = A(2ND BOOK)                           
         LA    R1,PRMBKQ+1           FIELD EQUATE FOR 2ND BOOK FIELD            
         SR    R2,R2                 R3 = NUMBER OF VARIABLE COLUMNS            
DMLC33   OC    0(L'SAVBK,RE),0(RE)   ANY BOOK DEFINED HERE?                     
         BZ    DMLC36                NO                                         
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         STCM  R1,3,DCTFLD#        BOOK FIELD                                   
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
         OI    LSSTAT1,LSSTSAR        LIST OF TSAR RECS                         
         OI    LSSTAT2,LSSIUPD        WE WANT TO DO OUR OWN UPDATES             
         NI    LSSTAT2,X'FF'-LSSADD   NOT VALID TO ADD NEW LIST LINES           
         MVI   DEMOCNTR,0             OFFSET FROM FIRST DISPLAYED DEMO          
         B     EXITOK                                                           
***********************************************************************         
* FIRST TIME FOR LIST                                                           
***********************************************************************         
FTFLST1  DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* FIRST FOR LIST                                                                
***********************************************************************         
FLST1    DS    0H                                                               
         ZIC   R5,DEMOCNTR                                                      
         MH    R5,=Y(L'SAVDMO)                                                  
         LA    R5,SAVDMOS(R5)                                                   
         B     NML20                                                            
***********************************************************************         
* NEXT FOR LIST                                                                 
***********************************************************************         
NLST1    DS    0H                                                               
         ZIC   R5,DEMOCNTR                                                      
         LA    R5,1(R5)                                                         
         STC   R5,DEMOCNTR                                                      
*                                                                               
         MH    R5,=Y(L'SAVDMO)                                                  
         LA    R5,SAVDMOS(R5)                                                   
*                                                                               
NML20    LA    R0,SAVDMOS+L'SAVDMOS    END OF TABLE?                            
         CR    R5,R0                                                            
         BNL   EXITL                   YES                                      
         OC    0(L'SAVDMO,R5),0(R5)    ANY MORE DEMOS?                          
         BZ    EXITL                   THAT'S ALL                               
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
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         USING RPRDVELD,R6                                                      
*                                                                               
         MVC   TLRLEN,=Y(TLRLNQ)                                                
*                                                                               
         ZIC   RF,DEMOCNTR                                                      
         MH    RF,=Y(L'SAVDMO)                                                  
         LA    RF,SAVDMOS(RF)                                                   
         USING DEMOLIN,RF                                                       
         MVC   TLKDMDMO,DMLNDEMO   3 BYTE DEMO                                  
         MVC   BOBYTE1,DMLNIORD    1 BYTE INTERNAL ORDER NUMBER                 
         DROP  RF                                                               
         MVC   TLKDCNTR,DEMOCNTR   DEMOCNTR                                     
*                                                                               
TSARFILX B     EXITOK                                                           
         DROP  R6,R3                                                            
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
         MVI   GSSKCODE,0                                                       
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
         CLI   GSSMCODE,C'E'                                                    
         BE    *+12                                                             
         MVI   GSSMCODE,C'E'       WORK/DEMO SCREEN                             
         B     SETMSCRX                                                         
         MVI   GSSMCODE,C'I'                                                    
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
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         OI    MINIOFLG,MNIOFCLS   MINIO NEEDS TO BE CLOSED                     
         B     EXITOK                                                           
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
         BE    MNIOADD1                                                         
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    EXITL               YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
MNIOADD1 OI    MINIOFLG,MNIOFCLS   MINIO NEEDS TO BE CLOSED                     
         B     EXITOK                                                           
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
MNIODELX OI    MINIOFLG,MNIOFCLS   MINIO NEEDS TO BE CLOSED                     
         B     EXITOK                                                           
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
* SETS UP AIO5 SO WE CAN USE RECUP TO ADD ELEMENTS, DELETE ELEMENTS, OR         
* CHANGE THE SIZE OF EXISTING ELEMENTS OF THE CLUSTER IN MINELEM                
***********************************************************************         
INTOAIO5 NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         L     RE,AIO5             COPY KEY, LEN, STAT, & LINK                  
         L     RF,MINBUFF                                                       
         MVC   0(RPROR1ST-RPROKEY,RE),0(RF)                                     
         LA    RE,RPROR1ST-RPROKEY(RE)                CLUSTER GOES HERE         
         LA    RF,IOAREALN-(RPROR1ST-RPROKEY)                                   
*                                                                               
         L     R0,MINELEM          COPY THE ENTIRE CLUSTER                      
         LH    R1,MINELEML                                                      
         MVCL  RE,R0                                                            
*                                                                               
         LH    R1,MINELEML                                                      
         AH    R1,=Y(RPROR1ST-RPROKEY)     L(FAKE RECORD)                       
         L     RE,AIO5             COPY KEY, LEN, STAT, & LINK                  
         STCM  R1,3,RPRORLEN-RPROKEY(RE)                                        
         B     EXITOK                                                           
         DROP  R5                                                               
***********************************************************************         
* SETS UP MINELEM FROM AIO5 BECAUSE WE NEEDED RECUP                             
***********************************************************************         
FROMAIO5 NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         L     RE,AIO5                                                          
         ZICM  RF,RPRORLEN-RPROKEY(RE),2                                        
         SH    RF,=Y(RPROR1ST-RPROKEY)     L'CLUSTER                            
         LA    RE,RPROR1ST-RPROKEY(RE)                                          
*                                                                               
         L     R0,MINELEM                                                       
         LH    R1,MINMAXEL                                                      
         MVCL  R0,RE               COPY CLUSTER                                 
*                                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
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
EXITERR  TM    MINIOFLG,MNIOFCLS      DO WE NEED TO CLOSE MINIO?                
         BZ    EXITL                                                            
         BAS   RE,MINIOCLS                                                      
         NI    MINIOFLG,X'FF'-MNIOFCLS   DON'T NEED THIS ANYMORE                
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
* VALIDATE RATING FIELD                                                         
***********************************************************************         
VALRATNG DS    0H                                                               
         NMOD1 0,**VRTG**                                                       
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
         USING TLSTD,R2                                                         
         CLC   LSROWREP,=H'4'                                                   
         BE    VALLRTGX                                                         
*                                                                               
         ZIC   R1,SVPARMS2+3       FIND OUT WHICH BOOK FIELD                    
         SH    R1,=Y(PRMBKQ)       BY SUBTRACTING PRIME EQUATE                  
         LR    R3,R1                                                            
         MH    R3,=Y(L'SAVBK)                                                   
         LA    R3,SAVBKS(R3)                                                    
         USING BOOKLIN,R3                                                       
         MVC   BOBYTE1,BKLNIORD    INTERNAL ORDER NUMBER OF BOOK                
         DROP  R3                                                               
*                                                                               
         XC    BODMCB(6*L'BODMCB),BODMCB      PARAMS FOR A FETCH                
         ST    R3,BODMCB+12                   BOOKLINE                          
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         USING RPRDVELD,R6                                                      
         NI    MISCFLG1,X'FF'-MF1TMPBT  USED TO FLG IF WE NEED DEMO VAL         
*                                                                               
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
VLLRTG10 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BNE   VLLRTG15                                                         
         OI    MISCFLG1,MF1TMPBT   YES, WE NEED A DEMO VALUE ELEMENT            
         LR    R0,R6                                                            
         LA    R6,BOELEM              BUILD ELEM TO BE ADDED TO CLUSTER         
         XC    BOELEM,BOELEM                                                    
         MVI   RPRDVEL,RPRDVELQ                                                 
         MVC   RPRDVBNM,BOBYTE1    INTERNAL ORDER NUMBER OF BOOK                
         MVI   RPRDVLEN,RPRDVOVQ   SINCE NO DEMO VALUES, MINIMUM LENGTH         
         LR    R6,R0                                                            
         B     VLLRTG20                                                         
*                                                                               
VLLRTG15 CLI   RPRDVEL,RPRDVELQ    DEMO VALUE ELEMENT?                          
         BNE   VLLRTG10                                                         
         CLC   RPRDVBNM,BOBYTE1    SAME AS INTERNAL ORD # OF BOOK?              
         BNE   VLLRTG10            NO, WE NEED THE PRIME BOOK'S                 
         XC    BOELEM,BOELEM                                                    
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)     COPY ELEM IN CASE IT GETS BIGGER             
*                                                                               
VLLRTGD  USING RPRDVELD,BOELEM                                                  
VLLRTG20 DS    0H                                                               
         ZIC   R3,TLKDCNTR           FIND OUT WHICH DEMO                        
         MH    R3,=Y(L'SAVDMO)                                                  
         LA    R3,SAVDMOS(R3)                                                   
         USING DEMOLIN,R3                                                       
         LA    RF,DMLNDEMO                                                      
         DROP  R3                                                               
         ST    RF,BODMCB+16        P5 IS THE A(DEMO)                            
*                                                                               
         OI    FVIFLD,C' '         UPPERCASE                                    
         CLI   FVIFLD,C'X'         CHECK FOR FETCH                              
         BE    VLLRTG30                                                         
         SPACE 2                                                                
***************************                                                     
** HANDLE USER OVERRIDES **                                                     
***************************                                                     
         ZIC   R0,FVILEN                                                        
         LA    R4,FVIFLD                                                        
         CLI   FVIFLD,C'*'         CHECK FOR OVERRIDE '*'                       
         BNE   VLLRTG22                                                         
         LA    R4,1(R4)                                                         
         SH    R0,=H'1'                                                         
*                                                                               
VLLRTG22 GOTO1 VCASHVAL,BODMCB,(1,(R4)),(R0)    1 DECIMAL PLACE                 
         CLI   0(R1),X'FF'                                                      
         BE    EXITNOTN            NOT NUMERIC                                  
*                                                                               
         ZIC   RF,TLKDCNTR         STORE DEMO VALUE IN CORRECT POSTION          
         MH    RF,=Y(L'SAVDMO)                                                  
         LA    RF,SAVDMOS(RF)                                                   
         ZIC   R1,DMLNIORD-DEMOLIN(RF)                                          
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,VLLRTGD.RPRDVDMO(R1)                                          
         LR    RF,R1                                                            
*                                                                               
         CLC   LSROWREP,=H'2'      SHARE LINE?                                  
         BNE   *+12                NO                                           
         LA    R1,4(R1)                                                         
         B     VLLRTG24                                                         
*                                                                               
         CLC   LSROWREP,=H'3'      LEVEL LINE?                                  
         BNE   *+8                 NO                                           
         LA    R1,8(R1)                                                         
*                                                                               
VLLRTG24 DS    0H                                                               
         MVC   0(4,R1),BODMCB+4    JUST CHANGE THE RATING PORTION               
         OI    0(R1),X'80'         DEFINITELY AN OVERRIDE                       
***************                                                                 
* HAVE TO CALCULATE THE RATING/SHARE BASED ON WHAT CHANGED                      
***************                                                                 
         CLC   LSROWREP,=H'1'              RATING LINE?                         
         BNE   VLLRTG26                    NO                                   
*********                                                                       
* SHARE = RATING / LEVEL                                                        
*********                                                                       
         L     RE,BODMCB+4                 YES, SHR = RTG / LVL                 
         CVD   RE,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         SRP   PCKOF16B,3,0                                                     
*                                                                               
         MVC   BOFULL1,8(RF)       LEVEL                                        
         NI    BOFULL1,X'FF'-X'80'                                              
         L     RE,BOFULL1                                                       
         CVD   RE,BODUB1                                                        
         ZAP   PCKOF08B,BODUB1                                                  
*                                                                               
         LR    R0,RF                                                            
         BAS   RE,DIVPACKD                                                      
         LR    RF,R0                                                            
         BNE   VLLRTG29            DIVIDING BY 0, LEAVE SHR ALONE               
         SRP   PCKOF16B,64-1,5                                                  
*                                                                               
         ZAP   BODUB1,PCKOF16B                                                  
         CVB   RE,BODUB1                                                        
         ST    RE,4(RF)            STORE SHARE VALUE IN CORRECT PLACE           
         OI    4(RF),X'80'         DEFINITELY AN OVERRIDE                       
         B     VLLRTG29                                                         
*********                                                                       
* RATING = SHARE * LEVEL                                                        
*********                                                                       
VLLRTG26 L     RE,BODMCB+4                                                      
         CVD   RE,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         MVC   BOFULL1,4(RF)       SHARE VALUE                                  
         CLC   LSROWREP,=H'3'       LEVEL LINE?                                 
         BE    *+10                YES                                          
         MVC   BOFULL1,8(RF)       NO, MUST OF CHANGE SHARE, NEED LEVEL         
*                                                                               
         NI    BOFULL1,X'FF'-X'80'                                              
         L     RE,BOFULL1                                                       
         CVD   RE,BODUB1                                                        
         ZAP   PCKOF08B,BODUB1                                                  
*                                                                               
         CP    PCKOF08B,PCKOF16B   IS MULTIPLIER > MULTIPLICAND?                
         BNH   *+16                                                             
         ZAP   PCKOF08B,PCKOF16B   YES, SWAP THE 2                              
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF08B                                                
         SRP   PCKOF16B,64-3,5                                                  
         ZAP   BODUB1,PCKOF16B                                                  
         CVB   RE,BODUB1                                                        
*                                                                               
VLLRTG27 DS    0H                                                               
         ST    RE,0(RF)            STORE RATING VALUE IN CORRECT PLACE          
         OI    0(RF),X'80'         DEFINITELY AN OVERRIDE                       
*                                                                               
VLLRTG29 LR    R1,RF               RE-ESTABLISH R1 TO BEG OF DEMO               
         B     VLLRTG50                                                         
         EJECT                                                                  
****************************                                                    
** USER REQUEST FOR FETCH **                                                    
****************************                                                    
VLLRTG30 L     RE,AIO5                                                          
         LA    RE,RPROR1ST-RPROHDRD(RE)                                         
         USING RPRDTELD,RE                                                      
         ZIC   RF,RPRDTSTA                                                      
         BCTR  RF,0                                                             
         MH    RF,=Y(L'SAVSTA)                                                  
         LA    R0,SAVSTAS+(STLNSTA-STALIN)(RF)      A(STATION TEXT)             
         ST    R0,BODMCB                                                        
*                                                                               
         CLC   RPRDTINM,BCSPACES                                                
         BNH   VLLRTG32                                                         
*                                                                               
         LA    R0,RPRDTINM         A(INVENTORY #)                               
         ST    R0,BODMCB+4                                                      
*                                                                               
         LA    R0,RPRDTEFF         A(EFFECTIVE DATES)                           
         ST    R0,BODMCB+8                                                      
*                                                                               
         B     VLLRTG33                                                         
         DROP  RE                                                               
VLLRTG32 ST    RE,BODMCB+4                                                      
         MVI   BODMCB+4,X'80'                                                   
*                                                                               
VLLRTG33 GOTOX (FETCHQ,AREPRO01),BODMCB                                         
*                                                                               
         ZIC   RF,TLKDCNTR         STORE DEMO VALUE IN CORRECT POSTION          
         MH    RF,=Y(L'SAVDMO)                                                  
         LA    RF,SAVDMOS(RF)                                                   
         ZIC   R1,DMLNIORD-DEMOLIN(RF)                                          
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,VLLRTGD.RPRDVDMO(R1)                                          
         LR    RF,R1                                                            
*                                                                               
         CLC   LSROWREP,=H'2'      SHARE LINE?                                  
         BNE   *+16                NO                                           
         LA    RE,4(R1)                                                         
         LA    RF,BODMCB+4                                                      
         B     VLLRTG34                                                         
*                                                                               
         CLC   LSROWREP,=H'3'      LEVEL LINE?                                  
         BNE   *+16                NO                                           
         LA    RE,8(R1)                                                         
         LA    RF,BODMCB+8                                                      
         B     VLLRTG34                                                         
*                                                                               
         LA    RE,0(R1)            DISPLAY RATINGS OTHERWISE                    
         LA    RF,BODMCB                                                        
VLLRTG34 MVC   0(L'RPRDVDMO,R1),BODMCB    COPY RTG/SHR/HPT FROM FETCH           
         EJECT                                                                  
***************************                                                     
** COMPLETE RATING UPDATE *                                                     
***************************                                                     
VLLRTG50 LA    R1,L'RPRDVDMO(R1)   R1 = A(ELEMENT BEYOND THAT DEMO)             
         LA    R0,VLLRTGD.RPRDVEL                                               
         SR    R1,R0                                                            
         CLM   R1,1,VLLRTGD.RPRDVLEN                                            
         BL    *+8                                                              
         STC   R1,VLLRTGD.RPRDVLEN                                              
*                                                                               
         TM    MISCFLG1,MF1TMPBT   DO WE NEED A NEW DEMO VALUE ELEMENT?         
         BNZ   VLLRTG56            YES, ADD IT                                  
*                                                                               
         CLM   R1,1,RPRDVLEN       SAME LENGTH AS BEFORE?                       
         BNE   VLLRTG54            NO, NEED TO REMOVE AND ADD                   
         ZIC   R1,RPRDVLEN         YES                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RPRDVEL,VLLRTGD.RPRDVEL                                          
         B     VLLRTG58                                                         
         DROP  VLLRTGD                                                          
*                                                                               
VLLRTG54 DS    0H                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
VLLRTG56 DS    0H                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
*                                                                               
VLLRTG58 DS    0H                                                               
*                                                                               
VALLRTGX B     EXITOK                                                           
         DROP  R2,R5                                                            
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
         USING BOOKLIN,R1                                                       
*                                                                               
         CLI   RPRBKLEN,RPRBKOVQ   USER DEFINED BOOK?                           
         BH    RDBDBK20            YES                                          
*********                                                                       
* REGULAR BOOK                                                                  
*********                                                                       
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
         CLI   SELPROFS,RREPQSEL                                                
         BE    DFDDIS0                                                          
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
*                                                                               
DFDDIS0  DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         OC    0(L'SVCONNUM,RE),0(RE)    ANY CONTRACT NUMBER?                   
         BZ    DFDDISX                                                          
         CLI   SVPRONUM-SVCONNUM(RE),0    ANY PROPOSAL NUMBER?                  
         BZ    DFDDISX                                                          
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
         SPACE 2                                                                
         BAS   RE,GETCLSTR                                                      
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
* RECORD (PROTECTED PORTION)                                                    
         DC    AL2(00072),AL4(PRGDTA)    PROGRAM                                
         DC    AL2(00069),AL4(INVDTA)    INVENTORY NUMBER                       
         DC    AL2(00070),AL4(EDTDTA)    EFFECTIVE DATE                         
         DC    AL2(00086),AL4(LENDTA)    LENGTH                                 
         DC    AL2(00074),AL4(CSTDTA)    COST                                   
* RECORD (LIST PORTION)                                                         
         DC    AL2(00014),AL4(LDNMDTA)   DEMO NAME                              
* BOOK FIELDS                                                                   
         DC    AL2(00041),AL4(LRTGDTA)   BOOK #1'S RATING                       
         DC    AL2(00042),AL4(LRTGDTA)   BOOK #2'S RATING                       
         DC    AL2(00043),AL4(LRTGDTA)   BOOK #3'S RATING                       
         DC    AL2(00044),AL4(LRTGDTA)   BOOK #4'S RATING                       
         DC    AL2(00045),AL4(LRTGDTA)   BOOK #5'S RATING                       
         DC    AL2(00046),AL4(LRTGDTA)   BOOK #6'S RATING                       
         DC    AL2(00047),AL4(LRTGDTA)   BOOK #7'S RATING                       
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
SELPROFS DS    0CL10                CONTRACT PROFILES                           
         DS    CL1                 PROGRAM #                                    
         DS    CL1                 SPARE                                        
SELPROF  DS    CL8                 PROFILE BITS                                 
*                                                                               
*                                                                               
SAVCOST  DS    F                    NEED TO WRITE THIS AFTER A READ             
*                                                                               
ESCLEN   DS    XL1                                                              
ESCCHAR  DS    XL2                                                              
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS, SET #1                  
MF1KYCHG EQU   X'80'                - KEY FIELD CHANGED                         
MF1PFRET EQU   X'40'                - RETURNING FROM CALLED SESSION             
MF1PFCOM EQU   X'20'                - COMING FROM A CALLER SESSION              
MF1TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
MINIOFLG DS    XL1                 MINIO MISC FLAGS                             
MNIOFCLS EQU   X'80'                - NEED TO CLOSE MINIO                       
*                                                                               
PRMBKQ   EQU   41                  FIELD NUMBER FOR PRIME BOOK                  
PRMBK2Q  EQU   111                 FIELD NUMBER FOR BIG PRIME BOOK              
*                                                                               
PCKOF06B DS    PL6                 PACKED OF 6  BYTES                           
PCKOF08B DS    PL8                 PACKED OF 8  BYTES                           
PCKOF16B DS    PL16                PACKED OF 16 BYTES                           
*                                                                               
DEMOCNTR DS    XL1                 OFFSET FROM 1ST DISPLAYED DEMO               
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
PFTEXT   EQU   PFK01               PFKEY FOR TEXT                               
PFKYUP   EQU   PFK07               PFKEY FOR SCROLL UP                          
PFKYDOWN EQU   PFK08               PFKEY FOR SCROLL DOWN                        
PFKYLEFT EQU   PFK09               PFKEY FOR SCROLL LEFT                        
PFKYRGHT EQU   PFK10               PFKEY FOR SCROLL RIGHT                       
PFNEXT   EQU   PFK11               PFKEY FOR NEXT                               
PFRETURN EQU   PFK12               PFKEY FOR RETURN                             
*                                                                               
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
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKEYD   DS    0X                                                               
TLKDCNTR DS    XL1                 DISPLAYED ORDER NUMBER                       
TLKDMDMO DS    XL3                 3 BYTE DEMO                                  
         ORG   TLUSER                                                           
TLRECD   DS    0X                                                               
TLRFLAG1 DS    XL1                 FLAG                                         
TLRLNQ   EQU   *-TLSTD             LENGTH OF TSAR RECORD                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010REPRO43   08/22/97'                                      
         END                                                                    
