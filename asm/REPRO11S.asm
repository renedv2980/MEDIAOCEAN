*          DATA SET REPRO11S   AT LEVEL 016 AS OF 05/15/97                      
*&&      SET   NOP=N                                                            
*PHASE T80A11B                                                                  
T80A11   TITLE 'REPRO11 - PROPROSAL RECORDS - MAINTENANCE OVERLAY'              
PRO11    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REPRO11*,R7,RR=RE                                              
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
*****************************                                                   
** KEY SCREEN CODE EQUATES **                                                   
*****************************                                                   
KYDEF    EQU   0                                                                
         SPACE 1                                                                
******************************                                                  
** DATA SCREEN CODE EQUATES **                                                  
******************************                                                  
*TADD    EQU   C'A'                ADD DATA SCREEN                              
DTADD    EQU   C'2'                ADD DATA SCREEN                              
*TDEF    EQU   0                                                                
DTDEF    EQU   C'2'                                                             
         EJECT                                                                  
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                            
*                                                                               
* P1 HOLDS EQUATED VERB                                                         
***********************************************************************         
OBJECT   L     R1,SVPARMS                                                       
         L     RF,=A(TABLEOO)                                                   
         A     RF,BORELO           KNOWN OBJECTS                                
         B     ITER                                                             
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                                
***********************************************************************         
INIT     DS    0H                                                               
         OI    TWASRVH+1,X'01'     SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    TWASRVH+6,X'80'                                                  
         OI    GCINDS1,GCIPROT             UNPROT ON NTRSES                     
         OI    GSINDSL1,GSINOIO+GSIXKEY    WE'LL DO THE IO'S                    
*                                                                               
         GOTO1 VDATCON,BODMCB,(5,0),(3,TODAYBIN)                                
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
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
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
KLTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER VALIDATING THE KEY FIELDS                                               
***********************************************************************         
KLKVAL   DS    0H                                                               
         GOTO1 =A(KLSTKVAL),RR=BORELO                                           
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
         GOTO1 =A(R1STRADD),BODMCB,(R9),(RA),RR=BORELO                          
         BL    EXITL                                                            
*                                                                               
         B     EXITOK                                                           
***********************************************************************         
* BEFORE THE I/O CALL TO WRITE THE RECORD                                       
***********************************************************************         
RFRWRT   DS    0H                                                               
         GOTO1 =A(R1STRWRT),BODMCB,(R9),(RA),RR=BORELO                          
         BL    EXITL                                                            
*                                                                               
         B     EXITOK                                                           
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER THE I/O CALL TO ADD THE RECORD                                          
***********************************************************************         
RLRADD   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVPRONUM-TWAD)                                             
         MVC   0(L'SVPRONUM,RE),BPRONUM                                         
         BAS   RE,MINIOCLS                                                      
*                                                                               
         GOTOX APRG,BODMCB,OIO,IDIRGET    TO SET 'GSRECDA'                      
*                                                                               
         XC    IOKEY,IOKEY         BUILD PASSIVE 'C301' KEY                     
         LA    R6,IOKEY                                                         
         USING RPROKEY,R6                                                       
         MVI   RPROPTYP,RPROPTYQ                                                
         MVI   RPROPSTY,RPROPSBQ                                                
         MVC   RPROPRCD,CUAALF                                                  
         MVC   RPROPSAL,CCONSAL                                                 
         MVC   RPROPSTA,CCONKSTA                                                
         MVC   RPROKDA,GSRECDA                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         MVC   RPROPCON,0(RE)                                                   
         MVC   RPROPPRO,SVPRONUM-SVCONNUM(RE)                                   
         DROP  R6                                                               
*                                                                               
         L     R1,=AL4(XOREPDIR+XOADD+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    IOKEY,IOKEY         BUILD PASSIVE 'C302' KEY                     
         LA    R6,IOKEY                                                         
         USING RPROKEY,R6                                                       
         MVI   RPROOTYP,RPROOTYQ                                                
         MVI   RPROOSTY,RPROOSBQ                                                
         MVC   RPROORCD,CUAALF                                                  
         MVC   RPROOOFF,CCONKOFF                                                
         MVC   RPROOSAL,CCONSAL                                                 
         MVC   RPROKDA,GSRECDA                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         MVC   RPROOCON,0(RE)                                                   
         MVC   RPROOPRO,SVPRONUM-SVCONNUM(RE)                                   
         DROP  R6                                                               
*                                                                               
         L     R1,=AL4(XOREPDIR+XOADD+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXITOK                                                           
***********************************************************************         
* AFTER THE I/O CALL TO WRITE THE RECORD                                        
***********************************************************************         
RLRWRT   DS    0H                                                               
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
         LR    RE,RA               DO WE HAVE ANY CONTRACT NUMBER?              
         AH    RE,=Y(SVCONNUM-TWAD)                                             
*                                                                               
         TM    MISCFLG1,MF1PFRET   RETURNING FROM CALLED SESSION?               
         BNZ   *+12                                                             
         CLI   BCPFKEY,12                                                       
         BNE   DFD10               RETURN PFKEY HIT                             
         OI    MISCFLG1,MF1PFRET                                                
*                                                                               
         OC    0(L'SVCONNUM,RE),0(RE)                                           
         BZ    DFDDX               NO                                           
         CLI   L'SVCONNUM(RE),0                                                 
         BZ    DFDDX               NO                                           
*                                                                               
         ZAP   BOWORK1+10(5),=P'0'                                              
         MVO   BOWORK1+10(5),0(4,RE)                                            
         ZAP   BOWORK1(5),=P'99999999'                                          
         SP    BOWORK1(5),BOWORK1+10(5)                                         
         OI    BOWORK1+4,X'0F'                                                  
         UNPK  FVIFLD(8),BOWORK1(5)                                             
         MVI   FVILEN,8                                                         
         MVI   FVXLEN,7                                                         
         OI    FVIIND,FVINUM                                                    
*                                                                               
         GOTOX (VALCONQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
         OI    MISCFLG1,MF1KYCHG                                                
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$PSRES)                                           
         MVC   FVXTRA,BCSPACES           JUST IN CASE                           
*                                                                               
DFD10    GOTO1 =A(D1STDDIS),BODMCB,(R9),(RA),RR=BORELO                          
         BL    EXITL                                                            
*                                                                               
         NI    MISCFLG1,X'FF'-MF1PFRET                                          
*                                                                               
DFDDX    B     EXITOK                                                           
***********************************************************************         
* BEFORE VALIDATING THE DATA FIELDS                                             
***********************************************************************         
DFDVAL   DS    0H                                                               
         CLI   CSACT,A#ADD         ARE WE ADDING?                               
         BNE   DFDVAL10            NO                                           
         TM    GCINDS2,GCINTRS     JUST ENTER SES'D?                            
         BO    *+12                YES                                          
         TM    MISCFLG1,MF1KYCHG   DID THE KEY CHANGE?                          
         BZ    DFDVAL10            NO                                           
         MVC   FVADDR,AFVADDR      1ST DATA INPUT FIELD                         
         B     EXITENTR            YES, PUT OUT ENTER DATA MESSAGE              
*                                                                               
DFDVAL10 XC    DISBOOKS,DISBOOKS                                                
         XC    DISLBLS,DISLBLS                                                  
         XC    DISUPGDS,DISUPGDS                                                
         XC    DISDEMOS,DISDEMOS                                                
         XC    SAVSLNS,SAVSLNS                                                  
         MVI   WHICHUPE,1          USE THE 1ST USER-DEFINED EXPRESSION          
         MVI   SVDPTLIN,0          NOT ON ANY DAYPART LINE YET                  
         MVI   SVPRJFLD,0          NOT IN ANY PROJECTION FIELD YET              
         MVI   SAVOPTNS,0                                                       
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINDPTS-TWAD)                                              
         MVC   SAVDPTS,0(RE)                                                    
         LR    RE,RA                                                            
         AH    RE,=Y(MINDYTMS-TWAD)                                             
         MVC   SAVDYTMS,0(RE)                                                   
         LR    RE,RA                                                            
         AH    RE,=Y(MINSEDTS-TWAD)                                             
         MVC   SAVSEDTS,0(RE)                                                   
*                                                                               
DFDVALX  B     EXITOK                                                           
***********************************************************************         
* AFTER  WE DO ANYTHING TO THE DATA FIELDS ON THE SCREEN                        
***********************************************************************         
DTALAST  DS    0H                                                               
         L     R1,SVPARMS3         VERB                                         
         LA    RF,DLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
DLTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DLDDIS)      DISPLAY                    
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER DISPLAYING THE DATA FIELDS                                              
***********************************************************************         
DLDDIS   DS    0H                                                               
         CLI   CSACT,A#ADD         ARE WE ADDING?                               
         BNE   DLDDIS10            NO                                           
         TM    GCINDS2,GCINTRS     JUST ENTER SES'D?                            
         BO    *+12                YES                                          
         TM    MISCFLG1,MF1KYCHG   DID THE KEY CHANGE?                          
         BZ    DLDDIS10            NO                                           
         MVC   FVADDR,AFVADDR      1ST DATA INPUT FIELD                         
         B     EXITENTR            YES, PUT OUT ENTER DATA MESSAGE              
*                                                                               
DLDDIS10 DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
************ DOING AN ACTION ON A SPECIFIC DATA OBJECT ****************         
***********************************************************************         
DATA10   DS    0H                                                               
         L     RF,=A(KNOWTAB)      TABLE OF KNOWN OBJECTS                       
         A     RF,BORELO                                                        
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
CONTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCON)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCON)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(NTRCON)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A CONTRACT FIELD                                                      
***********************************************************************         
DISCON   DS    0H                                                               
         USING RPROKMST,R2                                                      
         ZAP   BOWORK1+20(5),=P'99999999' EDIT USES 17 BYTES OF WORK            
         ZAP   BOWORK1+10(5),=P'0'                                              
         MVO   BOWORK1+10(5),RPROKCON                                           
         SP    BOWORK1+20(5),BOWORK1+10(5)                                      
         DROP  R2                                                               
         EDIT  (P5,BOWORK1+20),(8,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,      X        
               DUB=BODUB1                                                       
         B     EXITOK                                                           
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
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQCNT',CNTPROFS)                 
         TM    CONPROFS+CNTMEDTB,CNTMEDTA                                       
         BZ    VALCONX                                                          
*                                                                               
         TM    CCONFLG1,CCONSARQ                                                
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(601)                                                
         B     EXITL                                                            
*                                                                               
VALCONX  OI    FVIIND,FVIVAL       VALIDATED                                    
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
PROTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRO)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(NTRPRO)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A PROPOSAL FIELD                                                      
***********************************************************************         
DISPRO   DS    0H                                                               
         USING RPROKEY,R2                                                       
         ZIC   RE,RPROKPRO                                                      
         LA    R0,X'FF'                                                         
         SR    R0,RE                                                            
         EDIT  (R0),(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,DUB=BODUB1                
         DROP  R2                                                               
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE A PROPOSAL FIELD                                                     
***********************************************************************         
VALPRO   DS    0H                                                               
         GOTO1 =A(VALPRONM),RR=BORELO                                           
         BL    EXITL                                                            
         B     EXITOK                                                           
*                                                                               
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY STATION FIELD                                                         
***********************************************************************         
DISSTA   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS-TWAD)                                              
         USING STALIN,RE                                                        
         MVC   FVIFLD(L'STLNSTA),STLNSTA                                        
         DROP  RE                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BOOKS                                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
BKSDTA   DS    0H                                                               
         LA    RF,BKSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BKSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBKS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBKS)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY BOOKS FIELD                                                           
***********************************************************************         
DISBKS   DS    0H                                                               
         GOTO1 =A(DISBKSB),BODMCB,RR=BORELO                                     
         BL    EXITL                                                            
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE BOOKS FIELD                                                          
***********************************************************************         
VALBKS   DS    0H                                                               
         GOTO1 =A(VALBOOKS),BODMCB,RR=BORELO                                    
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PROJECTIONS                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRJDTA   DS    0H                                                               
         LA    RF,PRJTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PRJTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRJ)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRJ)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY PROJECTIONS FIELD                                                     
***********************************************************************         
DISPRJ   DS    0H                                                               
         GOTO1 =A(DISPROJS),BODMCB,RR=BORELO                                    
         BL    EXITL                                                            
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE PROJECTIONS FIELD                                                    
***********************************************************************         
VALPRJ   DS    0H                                                               
         GOTO1 =A(VALPROJS),BODMCB,RR=BORELO                                    
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PROJECTION TYPE                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PTPDTA   DS    0H                                                               
         LA    RF,PTPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PTPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPTP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPTP)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY PROJECTION TYPE FIELD                                                 
***********************************************************************         
DISPTP   DS    0H                                                               
         ZIC   R1,SVPRJFLD                                                      
         BCTR  R1,0                OOPS                                         
         XC    FVIFLD,FVIFLD                                                    
*                                                                               
         SR    RF,RF               LABEL MATCHED COUNT                          
         LA    R3,DISLBLS                                                       
         LA    R2,DISBOOKS                                                      
DISPTP5  OC    0(L'DISLBL,R3),0(R3)                                             
         BZ    DISPTP10                                                         
         CR    R1,RF               IS THIS THE LABEL WE WANT?                   
         BE    DISPTP15            YES                                          
         LA    RF,1(RF)                                                         
*                                                                               
DISPTP10 LA    R3,L'DISLBL(R3)                                                  
         LA    R2,L'DISBOOK(R2)                                                 
         LA    RE,DISLBLS+L'DISLBLS                                             
         CR    R3,RE                                                            
         BNL   DISPTPX             NOTHING LEFT                                 
         B     DISPTP5                                                          
*                                                                               
DISPTP15 DS    0H                                                               
         USING BOOKLIN,R2                                                       
         MVC   FVIFLD(L'BKLNFIL),BKLNFIL                                        
         DROP  R2                                                               
DISPTPX  DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE PROJECTION TYPE FIELD                                                
***********************************************************************         
VALPTP   DS    0H                                                               
         ZIC   R1,SVPRJFLD                                                      
*                                                                               
         ZIC   RE,WHICHUPE                                                      
         BCTR  RE,0                                                             
         MH    RE,=Y(L'DISUPGD)                                                 
         LA    R2,DISUPGDS(RE)                                                  
*                                                                               
         OC    0(L'DISUPGD,R2),0(R2)     ANY UPGRADE FORLMULA?                  
         BNZ   VALPTP10                  YES                                    
*                                                                               
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     VALPTPX                                                          
*                                                                               
VALPTP10 DS    0H                                                               
         CLI   FVILEN,0                                                         
         BE    VALPTP15                                                         
*                                                                               
         CLI   SELPROFS,RREPQSEL                                                
         BE    VALPTP11                                                         
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
VALPTP11 DS    0H                                                               
         CLI   FVIFLD,RPRBKINQ                                                  
         B     *+8                 *!LIVE*                                      
         BE    VALPTP20                                                         
*                                                                               
         CLI   FVIFLD,RPRBKTPQ                                                  
         BNE   *+16                                                             
         TM    SELPROF+SELTPB,SELTPA                                            
         BO    EXITNV                                                           
         B     VALPTP20                                                         
*                                                                               
         CLI   FVIFLD,RPRBKT4Q                                                  
         BNE   *+16                                                             
         TM    SELPROF+SELT4B,SELT4A                                            
         BO    EXITNV                                                           
         B     VALPTP20                                                         
*                                                                               
         CLI   FVIFLD,RPRBKPAQ                                                  
         BNE   *+16                                                             
         TM    SELPROF+SELPAVB,SELPAVA                                          
         BO    EXITNV                                                           
         B     VALPTP20                                                         
*                                                                               
         B     EXITNV                                                           
*                                                                               
VALPTP15 MVI   FVIFLD,RPRBKTPQ     DEFAULT                                      
         CLI   SAVBKTYP,RPRBKINQ   INV BOOK SOURCE?                             
         BE    *+10                YES <- LIVE BUT LINKED TO !LIVE              
         MVC   FVIFLD,SAVBKTYP                                                  
*                                                                               
VALPTP20 DS    0H                                                               
         LA    RF,DISLBLS+L'DISLBLS                                             
         LA    RE,DISLBLS                                                       
         LA    R3,DISBOOKS                                                      
VALPTP21 CLC   0(L'DISLBL,R2),0(RE)                                             
         BE    VALPTP2X                                                         
         LA    RE,L'DISLBL(RE)                                                  
         LA    R3,L'DISBOOK(R3)                                                 
         CR    RE,RF                                                            
         BL    VALPTP21                                                         
         DC    H'0'                                                             
*                                                                               
         USING BOOKLIN,R3                                                       
VALPTP2X MVC   BKLNFIL,FVIFLD                                                   
         DROP  R3                                                               
         CLI   WHICHUPE,1          FIRST FORMULA?                               
         BE    VALPTP30            YES                                          
*                                                                               
         LA    RE,DISLBLS                                                       
         LA    RF,DISBOOKS                                                      
*                                                                               
VALPTP22 OC    0(L'DISLBL,RE),0(RE)   ANY LABEL?                                
         BZ    VALPTP24               NO                                        
         CR    RF,R3                  SAME LABEL?                               
         BE    VALPTP24               YES                                       
*                                                                               
         CLC   0(L'DISBOOK,R3),0(RF)  SAME EXPRESSION?                          
         BE    EXITDUPL               YES                                       
*                                                                               
VALPTP24 LA    RF,L'DISBOOK(RF)                                                 
         LA    RE,L'DISLBL(RE)                                                  
         LA    R0,DISLBLS+L'DISLBLS                                             
         CR    RE,R0                                                            
         BL    VALPTP22                                                         
*                                                                               
VALPTP30 DS    0H                                                               
         ZIC   RE,WHICHUPE                                                      
         LA    RE,1(RE)                                                         
         STC   RE,WHICHUPE                                                      
*                                                                               
VALPTPX  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEMOS                                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DMODTA   DS    0H                                                               
         LA    RF,DMOTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DMOTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDMO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDMO)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEMOS FIELD                                                           
***********************************************************************         
DISDMO   DS    0H                  (SEE RECNT70)                                
         OC    DISDEMOS,DISDEMOS                                                
         BZ    DISDMOX                                                          
*                                                                               
         L     R6,AIO5                                                          
         USING DBLOCK,R6                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOM                                                    
         MVI   DBSELMED,C'T'                                                    
         LA    R5,BOWORK1                                                       
         XC    BOWORK1(50),BOWORK1                                              
*                                                                               
         LA    RE,DISDEMOS                                                      
         LA    RF,DISDEMOS+L'DISDEMOS                                           
         USING DEMOLIN,RE                                                       
         MVC   0(L'DMLNDEMO,R5),DMLNDEMO                                        
         LA    R5,L'DMLNDEMO(R5)                                                
         LA    RE,DMLNLENQ(RE)                                                  
         CR    RE,RF                                                            
         BL    *-16                                                             
         DROP  RE                                                               
*                                                                               
         LA    R5,BOWORK1                                                       
         LA    R3,NUMDEMS                                                       
*                                                                               
DISDMO10 CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         LA    R5,3(R5)                                                         
         BCT   R3,DISDMO10                                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 VDEMOCON,BODMCB,('NUMDEMS',BOWORK1),(9,FVIFLD),(0,AIO5)          
*                                                                               
DISDMOX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE DEMOS FIELD                                                          
***********************************************************************         
VALDMO   ZIC   R0,FVILEN                                                        
         LA    RF,FVIFLD                                                        
*                                                                               
         CLI   0(RF),C'='                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'$'                                                       
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         GOTOX (VALDMOQ,AREPRO01),BODMCB,(C'Y',FVIHDR),                X        
               ('NUMDEMS',BOWORK1)                                              
         BL    EXITL                                                            
*                                                                               
         LA    RE,BOWORK1           CHECK FOR DUPLICATES                        
VALDMO10 CLI   0(RE),X'FF'          END OF DEMOS?                               
         BE    VALDMO30             YES                                         
*                                                                               
         LA    RF,3(RE)                                                         
VALDMO15 CLC   1(3-1,RF),1(RE)                                                  
         BE    VALDMO20                                                         
         LA    RF,3(RF)                                                         
         LA    R0,BOWORK1+(3*NUMDEMS)                                           
         CR    RF,R0                                                            
         BL    VALDMO15                                                         
*                                                                               
         LA    RE,3(RE)                                                         
         LA    R0,BOWORK1+(3*(NUMDEMS-1))                                       
         CR    RE,R0                                                            
         BL    VALDMO10                                                         
         B     VALDMO30                                                         
*                                                                               
VALDMO20 DS    0H                  PLACE THE CURSOR                             
         LA    RE,BOWORK1                                                       
         SR    R4,R4                                                            
VALDMO22 CR    RE,RF                                                            
         BE    VALDMO24                                                         
         LA    RE,3(RE)                                                         
         LA    R4,1(R4)                                                         
         B     VALDMO22                                                         
*                                                                               
VALDMO24 LR    R0,R4                                                            
         LTR   R0,R0                                                            
         BZ    EXITDUPL                                                         
         LA    RE,FVIFLD                                                        
*                                                                               
VALDMO26 CLI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         BE    *+8                                                              
         B     VALDMO26                                                         
         BCT   R0,VALDMO26                                                      
*                                                                               
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         STC   RE,FVERRNDX                                                      
         B     EXITDUPL                                                         
*                                                                               
VALDMO30 DS    0H                                                               
         LA    RE,BOWORK1                                                       
         LA    RF,DISDEMOS                                                      
         LA    R0,BOWORK1+(3*NUMDEMS)                                           
         USING DEMOLIN,RF                                                       
*                                                                               
VALDMO32 CLI   0(RE),FF                                                         
         BE    VALDMOX                                                          
         MVC   DMLNDEMO,0(RE)                                                   
         LA    RE,3(RE)                                                         
         LA    RF,DMLNLENQ(RF)                                                  
         CR    RE,R0                                                            
         BL    VALDMO32                                                         
         DROP  RF                                                               
*                                                                               
VALDMOX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LENGTHS                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LNSDTA   DS    0H                                                               
         LA    RF,LNSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LNSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLNS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLNS)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY LENGTHS FIELD                                                         
***********************************************************************         
DISLNS   DS    0H                                                               
         OC    SAVSLNS,SAVSLNS                                                  
         BZ    DISLNSX                                                          
*                                                                               
         LA    R3,SAVSLNS          R3 = A(1ST LENGTH)                           
         LA    R0,6                MAX OF 6 LENGTHS IN CONTRACT                 
         LA    R2,FVIFLD                                                        
*                                                                               
DISLNS10 OC    0(L'SAVSLN,R3),0(R3)   ANY MORE LENGTHS?                         
         BZ    DISLNS30                                                         
*                                                                               
         ZIC   R1,0(R3)            SHOW THE LENGTH                              
         CVD   R1,BODUB1                                                        
         UNPK  0(3,R2),BODUB1                                                   
         OI    2(R2),X'F0'                                                      
*                                                                               
         CLI   0(R2),C'0'          2 CHAR LENGTH?                               
         BNE   DISLNS20                                                         
         MVC   0(1,R2),1(R2)       YES                                          
         MVC   1(1,R2),2(R2)                                                    
         LA    R2,2(R2)                                                         
         B     *+8                                                              
*                                                                               
DISLNS20 LA    R2,3(R2)            NO, 3 CHAR LENGTH                            
         MVI   0(R2),C','                                                       
*                                                                               
         LA    R3,L'SAVSLN(R3)                                                  
         LA    R2,1(R2)                                                         
         BCT   R0,DISLNS10         SHOW ALL THE LENGTHS                         
*                                                                               
DISLNS30 BCTR  R2,0                REMOVE THE LAST COMMA                        
         MVI   0(R2),C' '                                                       
*                                                                               
DISLNSX  DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE LENGTHS FIELD                                                        
***********************************************************************         
VALLNS   DS    0H                                                               
         L     RE,AIO4                                                          
         XC    0(256,RE),0(RE)                                                  
         GOTO1 VSCANNER,BODMCB,FVIHDR,(X'87',AIO4)                              
         CLI   4(R1),0                                                          
         BE    EXITNV                                                           
         CLI   4(R1),7                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(TOOMNYLN)                                           
         B     EXITL               MAXIMUM OF SIX LENGTHS ALLOWED               
*                                                                               
         L     R3,AIO4                                                          
VALLNS10 OC    0(2,R3),0(R3)       NO MORE LENGTHS?                             
         BE    VALLNSX             NO MORE                                      
*                                                                               
         MVC   FVERRNDX,4(R3)                                                   
*                                                                               
         CLI   1(R3),0             CAN'T HAVE 30=45                             
         BNE   EXITNV                                                           
*                                                                               
         TM    2(R3),X'80'         NUMERIC FIELD?                               
         BZ    EXITNOTN            INVALID, NON-NUMERIC DATA                    
*                                                                               
***************                                                                 
* CHECK IF WE HAVE A DUPLICATE ENTRY                                            
***************                                                                 
         LA    RF,SAVSLNS                                                       
         LA    RE,6                                                             
VALLNS25 CLI   0(RF),0                                                          
         BE    VALLNS30            FOUND A(END OF SECONDS LIST)                 
*                                                                               
         CLC   0(1,RF),7(R3)                                                    
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(DPNTRYNA)                                           
         B     EXITL               DUPLICATE ENTRY NOT ALLOWED                  
*                                                                               
         LA    RF,1(RF)                                                         
         BCT   RE,VALLNS25                                                      
         DC    H'0'                CAN'T POSSIBLY GET HERE                      
***************                                                                 
* VALID AND UNIQUE ENTRY                                                        
***************                                                                 
VALLNS30 MVC   0(1,RF),7(R3)       COPY THE LENGTH TO THE LIST                  
         LA    R3,32(R3)                                                        
         B     VALLNS10            CHECK NEXT SCANNER ENTRY                     
*                                                                               
VALLNSX  MVI   FVERRNDX,0                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DAYPART                                                       
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
         DC    AL1(DNTR),AL1(0,0,0),AL4(NTRF17)    FIELD #17                    
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DAYPART FIELD                                                         
***********************************************************************         
DISDPT   DS    0H                                                               
         NI    FVATRB,FF-FVAPROT   ALWAYS UNPROTECT TO START                    
*                                                                               
         ZIC   R1,SVDPTLIN         WE'RE ON A NEW (OR 1ST) DAYPART LINE         
         LA    R1,1(R1)                                                         
         STC   R1,SVDPTLIN                                                      
*                                                                               
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVDPT)                                                  
         LA    R1,SAVDPTS(R1)                                                   
         USING DPTLIN,R1                                                        
*                                                                               
         CLI   DPLNDPT,C' '        ANY DAYPART?                                 
         BNH   DISDPTX                                                          
         MVC   FVIFLD(1),DPLNDPT   YES, SHOW THE DAYPART                        
*                                                                               
         TM    DPLNFLG,RPRDPFFT    WAS THIS DAYPART FETCHED ALREADY?            
         BZ    DISDPTX             NO                                           
         OI    FVATRB,FVAPROT      CHANGE TO PROTECTED FIELD                    
         DROP  R1                                                               
*                                                                               
DISDPTX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE DAYPART FIELD                                                        
*                                                                               
*   USE HARDCODED TABLE IF THE CCONDPMQ BIT IS ON IN CCONFLG1                   
*     OTHERWISE USE THE DAYPART RECORDS                                         
***********************************************************************         
VALDPT   DS    0H                                                               
         OC    SVDPTLIN,SVDPTLIN                                                
         BNZ   *+10                                                             
         MVC   AFVADDR,FVADDR                                                   
*                                                                               
         ZIC   R1,SVDPTLIN         WE'RE ON A NEW (OR 1ST) DAYPART LINE         
         LA    R1,1(R1)                                                         
         STC   R1,SVDPTLIN                                                      
*                                                                               
         BCTR  R1,0                RE = A(ENTRY FOR THIS DAYPART)               
         MH    R1,=Y(L'SAVDPT)                                                  
         LA    RE,SAVDPTS(R1)                                                   
*                                                                               
         CLI   FVILEN,0                ANY INPUT ON THIS LINE?                  
         BNE   VALDPT10                                                         
         XC    0(L'SAVDPT,RE),0(RE)                                             
         CLI   SVDPTLIN,NUMDPTS        ARE WE ON THE LAST DPT LINE?             
         BNE   VALDPTX                 NO - OKAY                                
*                                                                               
         OC    SAVDPTS,SAVDPTS         YES, NEED AT LEAST ONE DPT               
         BNZ   VALDPTX                 THERE WAS ONE                            
         MVC   FVADDR,AFVADDR                                                   
         B     EXITNO                  MISSING INPUT                            
*                                                                               
VALDPT10 DS    0H                                                               
         TM    CCONFLG1,CCONDPMQ   USES HARDCODED TABLE?                        
         BO    VALDPT60            YES                                          
*                                                                               
****************************************                                        
** READ DAYPART RECORD FOR VALIDATION **                                        
****************************************                                        
         CLI   FVILEN,L'RDPTKDPT                                                
         BNE   EXITNV                                                           
*                                                                               
         ST    RE,BOFULL2          SAVE ENTRY IN SAVDPTS                        
*                                                                               
         MVC   BODMCB(2),CUAALF    GET PARENT REP                               
         GOTOX (GETPRNT,AREPRO01),BODMCB                                        
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RRDPKEY,RE                                                       
         MVI   RRDPKTYP,RRDPKIDQ                                                
         MVC   RRDPKREP,BODMCB                                                  
         MVC   RRDPKDPT,FVIFLD                                                  
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RRDPKEY),IOKEYSAV                                        
         BNE   EXITNV                                                           
*                                                                               
         L     RE,BOFULL2          RESTORE ENTRY IN SAVDPTS                     
         USING DPTLIN,RE                                                        
         OI    DPLNFLG,RPRDPFMN    USE DAYPART MENU FLAG                        
         B     VALDPT90                                                         
*                                                                               
*************************************************                               
** READ HARDCODED DAYPART TABLE FOR VALIDATION **                               
*************************************************                               
VALDPT60 LR    R2,RB               VALIDATE THE DAYPART CODE                    
         AH    R2,=Y(DPTTABLE-PRO11)                                            
         ZIC   R3,FVXLEN           R3 = LENGTH OF INPUT -1                      
*                                                                               
VALDPT65 CLI   0(R2),X'FF'         DID WE HIT THE END OF DPTTABLE?              
         BE    EXITNV              YES, INVALID DAYPART CODE                    
*                                                                               
         EX    R3,*+8              VALID 1-BYTE DAYPART CODE?                   
         B     *+10                                                             
         CLC   FVIFLD(0),3(R2)                                                  
         BE    VALDPT90                                                         
         LA    R2,L'DPTTABLE(R2)                                                
         B     VALDPT65                                                         
*                                                                               
VALDPT90 DS    0H                  COPY THE 1-BYTE DAYPART CODE                 
         USING DPTLIN,RE                                                        
         MVC   DPLNDPT,FVIFLD      COPY THE 1-BYTE DAYPART CODE                 
*                                                                               
         LA    R2,CSARDPT          SEE IF WE CAN GET TAB CPP FROM  SAR          
VALDPT92 CLI   0(R2),0                                                          
         BE    VALDPTX                                                          
*                                                                               
         CLC   0(1,R2),FVIFLD      MATCH ON THIS DAYPART?                       
         BE    VALDPT94                                                         
         LA    R2,5(R2)            NO, CHECK THE NEXT ONE IN  SAR               
         LA    R0,CSARDPT+L'CSARDPT                                             
         CR    R2,R0                                                            
         BL    VALDPT92                                                         
         XC    DPLNCPP,DPLNCPP     CLEAR THE CPP                                
         B     VALDPTX                                                          
*                                                                               
VALDPT94 MVC   DPLNCPP,1(R2)         WE GOT A TAB CPP FOR THIS DAYPART          
         DROP  RE                                                               
*                                                                               
VALDPTX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PASS FIELD 17 TO THE NEXT SESSION                                             
***********************************************************************         
NTRF17   DS    0H                                                               
         CLI   BCPFKEY,PFAVAIL     GOING TO AVAIL?                              
         BNE   *+14                                                             
         MVC   FVIFLD(5),=C'AVAIL'                                              
         B     NTRF17X                                                          
*                                                                               
         CLI   BCPFKEY,PFPACKGE    GOING TO PACKAGE?                            
         BNE   *+14                                                             
         MVC   FVIFLD(7),=C'PACKAGE'                                            
         B     NTRF17X                                                          
*                                                                               
         CLI   BCPFKEY,PFMBOOK     GOING TO MBOOK?                              
         BNE   *+14                                                             
         MVC   FVIFLD(5),=C'MBOOK'                                              
         B     NTRF17X                                                          
*                                                                               
         CLI   BCPFKEY,PFMDEMO     GOING TO MDEMO?                              
         BNE   *+14                                                             
         MVC   FVIFLD(5),=C'MDEMO'                                              
         B     NTRF17X                                                          
*                                                                               
         CLI   BCPFKEY,PFMCOST     GOING TO MCOST?                              
         BNE   *+14                                                             
         MVC   FVIFLD(5),=C'MCOST'                                              
         B     NTRF17X                                                          
*                                                                               
NTRF17X  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DAYS                                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DAYDTA   DS    0H                                                               
         LA    RF,DAYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DAYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDAY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDAY)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DAYS FIELD                                                            
***********************************************************************         
DISDAY   DS    0H                                                               
         NI    FVATRB,FF-FVAPROT   ALWAYS UNPROTECT TO START                    
*                                                                               
         ZIC   R3,SVDPTLIN         R3 = A(ENTRY IN SAVDYTMS)                    
         BCTR  R3,0                                                             
*                                                                               
         LR    R1,R3               SO WE CAN TELL IF WE HAVE TO PROTECT         
         MH    R1,=Y(L'SAVDPT)                                                  
         LA    R1,SAVDPTS(R1)                                                   
         USING DPTLIN,R1                                                        
*                                                                               
         MH    R3,=Y(L'SAVDYTM)                                                 
         LA    R3,SAVDYTMS(R3)                                                  
*                                                                               
         TM    DPLNFLG,RPRDPFFT      WAS THIS DAYPART FETCHED ALREADY?          
         BZ    *+8                                                              
         OI    FVATRB,FVAPROT      CHANGE TO PROTECTED FIELD                    
         DROP  R1                                                               
*                                                                               
         OC    0(L'SAVDYTM,R3),0(R3)     ANY DAY/TIME?                          
         BZ    DISDAYX                   NO, NONE                               
*                                                                               
         LA    R4,FVIFLD                 YES, WHERE TO START DISPLAYING         
*                                                                               
         CLI   0(R3),0                        ANY DAYS FOR THIS LINE?           
         BE    DISDAYX                        NO                                
         GOTO1 VDAYUNPK,BODMCB,0(R3),FVIFLD   YES                               
*                                                                               
         LA    RE,FVIFLD                                                        
DISDAY10 CLI   0(RE),C' '          CONVERT '/'S TO ','S                         
         BNH   DISDAYX                                                          
         CLI   0(RE),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         B     DISDAY10                                                         
*                                                                               
DISDAYX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE DAYS FIELD                                                           
***********************************************************************         
VALDAY   DS    0H                                                               
         ZIC   R3,SVDPTLIN                                                      
         BCTR  R3,0                                                             
         LR    R4,R3                                                            
         MH    R3,=Y(L'SAVDPT)                                                  
         MH    R4,=Y(L'SAVDYTM)                                                 
         LA    R3,SAVDPTS(R3)                                                   
         LA    R4,SAVDYTMS(R4)                                                  
*                                                                               
         CLI   FVILEN,0            ANY INPUT?                                   
         BNE   *+14                                                             
         XC    0(L'SAVDYTM,R4),0(R4)                                            
         B     VALDAYX             NONE                                         
*                                                                               
         OC    0(L'SAVDPT,R3),0(R3)     ANY DAYPART FOR THIS LINE?              
         BZ    EXITNV                   NONE, NEED THE DAYPART!                 
*                                                                               
         GOTO1 VDAYVAL,BODMCB,(FVILEN,FVIFLD),0(R4),BOBYTE1                     
         CLI   0(R4),0             DAYVAL COMPLAINS IF INPUT > 11               
         BE    EXITNV                                                           
*                                                                               
VALDAYX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TIMES                                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
TIMDTA   DS    0H                                                               
         LA    RF,TIMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TIMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTIM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTIM)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DAYS/TIMES FIELD                                                      
***********************************************************************         
DISTIM   DS    0H                                                               
         NI    FVATRB,FF-FVAPROT   ALWAYS UNPROTECT TO START                    
*                                                                               
         ZIC   R3,SVDPTLIN         R3 = A(ENTRY IN SAVDYTMS)                    
         BCTR  R3,0                                                             
*                                                                               
         LR    R1,R3               SO WE CAN TELL IF WE HAVE TO PROTECT         
         MH    R1,=Y(L'SAVDPT)                                                  
         LA    R1,SAVDPTS(R1)                                                   
         USING DPTLIN,R1                                                        
*                                                                               
         MH    R3,=Y(L'SAVDYTM)                                                 
         LA    R3,SAVDYTMS(R3)                                                  
*                                                                               
         TM    DPLNFLG,RPRDPFFT      WAS THIS DAYPART FETCHED ALREADY?          
         BZ    *+8                                                              
         OI    FVATRB,FVAPROT      CHANGE TO PROTECTED FIELD                    
         DROP  R1                                                               
*                                                                               
         OC    0(L'SAVDYTM,R3),0(R3)     ANY DAY/TIME?                          
         BZ    DISTIMX                   NO, NONE                               
*                                                                               
         LA    R4,FVIFLD                 YES, WHERE TO START DISPLAYING         
*                                                                               
         OC    1(4,R3),1(R3)       ANY TIMES FOR THIS LINE?                     
         BZ    DISTIMX                                                          
*                                                                               
         GOTO1 VUNTIME,BODMCB,1(R3),0(R4)                                       
*                                                                               
DISTIMX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE TIMES FIELD                                                          
***********************************************************************         
VALTIM   DS    0H                                                               
         ZIC   R3,SVDPTLIN                                                      
         BCTR  R3,0                                                             
         LR    R4,R3                                                            
         MH    R3,=Y(L'SAVDPT)                                                  
         MH    R4,=Y(L'SAVDYTM)                                                 
         LA    R3,SAVDPTS(R3)                                                   
         LA    R4,SAVDYTMS(R4)                                                  
*                                                                               
         CLI   FVILEN,0            ANY INPUT?                                   
         BNE   VALTIM0                                                          
         XC    1(L'SAVDYTM-1,R4),1(R4)                                          
         B     VALTIMX             NONE                                         
*                                                                               
VALTIM0  OC    0(L'SAVDPT,R3),0(R3)     ANY DAYPART FOR THIS LINE?              
         BZ    EXITNV                   NONE, NEED THE DAYPART!                 
*                                                                               
         GOTO1 VTIMVAL,BODMCB,(FVILEN,FVIFLD),1(R4)                             
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BNE   VALTIMX                                                          
         B     EXITNV                 THAT'S IN ERROR                           
*                                                                               
VALTIMX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DATES                                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DTSDTA   DS    0H                                                               
         LA    RF,DTSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DTSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDTS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDTS)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DATES FIELD                                                           
***********************************************************************         
DISDTS   DS    0H                                                               
         NI    FVATRB,FF-FVAPROT   ALWAYS UNPROTECT TO START                    
*                                                                               
         ZIC   R3,SVDPTLIN         R3 = A(ENTRY IN SAVSEDTS)                    
         BCTR  R3,0                                                             
*                                                                               
         LR    R1,R3               SO WE CAN TELL IF WE HAVE TO PROTECT         
         MH    R1,=Y(L'SAVDPT)                                                  
         LA    R1,SAVDPTS(R1)                                                   
         USING DPTLIN,R1                                                        
*                                                                               
         MH    R3,=Y(L'SAVSEDT)                                                 
         LA    R3,SAVSEDTS(R3)                                                  
*                                                                               
         TM    DPLNFLG,RPRDPFFT      WAS THIS DAYPART FETCHED ALREADY?          
         BZ    *+8                                                              
         OI    FVATRB,FVAPROT      CHANGE TO PROTECTED FIELD                    
         DROP  R1                                                               
*                                                                               
         OC    0(L'SAVSEDT,R3),0(R3)    ANY DATES HERE?                         
         BZ    DISDTSX                  NONE                                    
*                                                                               
         GOTO1 VDATCON,BODMCB,(X'28',(R3)),(17,FVIFLD),3(R3)                    
*                                                                               
DISDTSX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE DATES FIELD                                                          
***********************************************************************         
VALDTS   DS    0H                                                               
         ZIC   R3,SVDPTLIN                                                      
         BCTR  R3,0                                                             
         LR    R4,R3                                                            
         MH    R3,=Y(L'SAVDPT)                                                  
         MH    R4,=Y(L'SAVSEDT)                                                 
         LA    R3,SAVDPTS(R3)                                                   
         LA    R4,SAVSEDTS(R4)                                                  
*                                                                               
         CLI   FVILEN,0            ANY INPUT?                                   
         BNE   *+14                                                             
         XC    0(L'SAVSEDT,R4),0(R4)                                            
         B     VALDTSX             NONE                                         
*                                                                               
         OC    0(L'SAVDPT,R3),0(R3)     ANY DAYPART FOR THIS LINE?              
         BZ    EXITNV                   NONE, NEED THE DAYPART!                 
*                                                                               
         GOTO1 VPERVAL,BODMCB,(FVILEN,FVIFLD),PERVALST                          
         TM    4(R1),X'03'                                                      
         BNZ   EXITNV                                                           
*                                                                               
VALDTSD  USING PERVALD,PERVALST                                                 
         GOTO1 VDATCON,BODMCB,(0,VALDTSD.PVALESTA),(19,0(R4))                   
         GOTO1 VDATCON,BODMCB,(0,VALDTSD.PVALEEND),(19,3(R4))                   
         DROP  VALDTSD                                                          
*                                                                               
VALDTSX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CPP                                                           
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
CPPDTA   DS    0H                                                               
         LA    RF,CPPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CPPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCPP)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY CPP FIELD                                                             
***********************************************************************         
DISCPP   DS    0H                                                               
         ZIC   R1,SVDPTLIN         GET DAYPART LINE                             
         BCTR  R1,0                ENTRIES ARE 3-BYTES LONG IN CSARDPT          
         MH    R1,=Y(L'SAVDPT)                                                  
         LA    R4,SAVDPTS(R1)                                                   
         USING DPTLIN,R4                                                        
*                                                                               
         OC    0(L'SAVDPT,R4),0(R4)      ANY DAYPART?                           
         BZ    DISCPPX                                                          
         EDIT  DPLNCPP,(8,FVIFLD),2,WRK=BOWORK1,DUB=BODUB1,ALIGN=LEFT           
         DROP  R4                                                               
*                                                                               
         OI    FVIIND,FVIVAL       PREVIOUSLY VALIDATED                         
*                                                                               
DISCPPX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE CPP FIELD                                                            
***********************************************************************         
VALCPP   DS    0H                                                               
         ZIC   R1,SVDPTLIN         WE'RE ON A NEW (OR 1ST) DAYPART LINE         
         BCTR  R1,0                RE = A(ENTRY FOR THIS DAYPART)               
         MH    R1,=Y(L'SAVDPT)                                                  
         LA    R2,SAVDPTS(R1)                                                   
         USING DPTLIN,R2                                                        
*                                                                               
         CLI   FVILEN,0                ANY INPUT ON THIS LINE?                  
         BE    VALCPP8                 NO                                       
*                                                                               
         CLI   DPLNDPT,C' '          ANY DAYPART                                
         BNH   EXITNV                                                           
*                                                                               
         ZIC   R0,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(2,FVIFLD),(R0)                                  
         CLI   0(R1),0                                                          
         BNE   EXITNV                                                           
         CLC   =F'9999999',4(R1)                                                
         BL    EXITNV              WON'T DISPLAY                                
         MVC   2(4,R2),4(R1)                                                    
*                                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   VALCPPX             YES - ALL DONE                               
*                                                                               
         LA    RE,SAVDPTS                                                       
N        USING DPTLIN,RE                                                        
VALCPP5  CLC   N.DPLNDPT,DPLNDPT   DPT MATCH?                                   
         BNE   *+10                                                             
         MVC   N.DPLNCPP,DPLNCPP   COPY NEW CPP ON MATCH                        
         LA    RE,L'SAVDPT(RE)                                                  
         LA    R0,SAVDPTS+L'SAVDPTS                                             
         CR    RE,R0                                                            
         BL    VALCPP5                                                          
         DROP  N,R2                                                             
*                                                                               
VALCPP8  CLI   SVDPTLIN,NUMDPTS    LAST LINE ?                                  
         BNE   VALCPPX                                                          
*                                                                               
         LA    RF,SAVDPTS                                                       
N        USING DPTLIN,RE                                                        
         LA    RE,L'SAVDPT(RF)                                                  
O        USING DPTLIN,RF                                                        
VALCPP10 DS    0H                                                               
         CLC   N.DPLNDPT,O.DPLNDPT DPT MATCH?                                   
         BNE   *+10                                                             
         MVC   N.DPLNCPP,O.DPLNCPP COPY NEW CPP ON MATCH                        
         LA    RE,L'SAVDPT(RE)                                                  
         LA    R0,SAVDPTS+L'SAVDPTS                                             
         CR    RE,R0                                                            
         BL    VALCPP10                                                         
*                                                                               
         LA    RF,L'SAVDPT(RF)                                                  
         LA    RE,L'SAVDPT(RF)                                                  
         LA    R0,SAVDPTS+L'SAVDPTS                                             
         CR    RE,R0                                                            
         BL    VALCPP10                                                         
         DROP  N,O                                                              
*                                                                               
VALCPPX  DS    0H                                                               
         OI    FVIIND,FVIVAL       PREVIOUSLY VALIDATED                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DAYPART TEXT FLAG                                             
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DTXDTA   DS    0H                                                               
         LA    RF,DTXTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DTXTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDTX)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDTX)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DAYPART TEXT FLAG FIELD                                               
***********************************************************************         
DISDTX   DS    0H                                                               
         ZIC   R1,SVDPTLIN         GET DAYPART LINE                             
         BCTR  R1,0                ENTRIES ARE 3-BYTES LONG IN CSARDPT          
         MH    R1,=Y(L'SAVDPT)                                                  
         LA    R4,SAVDPTS(R1)                                                   
         USING DPTLIN,R4                                                        
*                                                                               
         OC    0(L'SAVDPT,R4),0(R4)      ANY DAYPART?                           
         BZ    DISDTXX                                                          
*                                                                               
         MVI   FVIFLD,C'N'                                                      
         TM    DPLNFLG,RPRDPFAI                                                 
         BNO   *+8                                                              
         MVI   FVIFLD,C'Y'                                                      
         DROP  R4                                                               
*                                                                               
         OI    FVIIND,FVIVAL       PREVIOUSLY VALIDATED                         
*                                                                               
DISDTXX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE DAYPART TEXT FLAG FIELD                                              
***********************************************************************         
VALDTX   DS    0H                                                               
         ZIC   R1,SVDPTLIN         WE'RE ON A NEW (OR 1ST) DAYPART LINE         
         BCTR  R1,0                RE = A(ENTRY FOR THIS DAYPART)               
         MH    R1,=Y(L'SAVDPT)                                                  
         LA    R2,SAVDPTS(R1)                                                   
         USING DPTLIN,R2                                                        
*                                                                               
         NI    DPLNFLG,FF-RPRDPFAI                                              
*                                                                               
         CLI   FVILEN,0            ANY INPUT ON THIS LINE?                      
         BE    VALDTX8             NO                                           
*                                                                               
         CLI   DPLNDPT,C' '        ANY DAYPART                                  
         BNH   EXITNV                                                           
*                                                                               
         CLI   FVIFLD,C'N'                                                      
         BE    VALDTX4                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    DPLNFLG,RPRDPFAI                                                 
*                                                                               
VALDTX4  TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   VALDTXX             YES - ALL DONE                               
*                                                                               
         LA    RE,SAVDPTS                                                       
N        USING DPTLIN,RE                                                        
VALDTX5  CLC   N.DPLNDPT,DPLNDPT   DPT MATCH?                                   
         BNE   VALDTX6             NO                                           
         CR    RE,R2                                                            
         BE    VALDTX6                                                          
*                                                                               
         NI    N.DPLNFLG,FF-RPRDPFAI                                            
         TM    DPLNFLG,RPRDPFAI                                                 
         BNO   *+8                                                              
         OI    N.DPLNFLG,RPRDPFAI                                               
*                                                                               
VALDTX6  LA    RE,L'SAVDPT(RE)                                                  
         LA    R0,SAVDPTS+L'SAVDPTS                                             
         CR    RE,R0                                                            
         BL    VALDTX5                                                          
         DROP  N,R2                                                             
*                                                                               
VALDTX8  CLI   SVDPTLIN,NUMDPTS    LAST LINE ?                                  
         BNE   VALDTXX                                                          
*                                                                               
         LA    RF,SAVDPTS                                                       
N        USING DPTLIN,RE                                                        
         LA    RE,L'SAVDPT(RF)                                                  
O        USING DPTLIN,RF                                                        
VALDTX10 DS    0H                                                               
         CLC   N.DPLNDPT,O.DPLNDPT DPT MATCH?                                   
         BNE   VALDTX11                                                         
*                                                                               
         NI    N.DPLNFLG,FF-RPRDPFAI                                            
         TM    O.DPLNFLG,RPRDPFAI                                               
         BNO   *+8                                                              
         OI    N.DPLNFLG,RPRDPFAI                                               
*                                                                               
VALDTX11 LA    RE,L'SAVDPT(RE)                                                  
         LA    R0,SAVDPTS+L'SAVDPTS                                             
         CR    RE,R0                                                            
         BL    VALDTX10                                                         
*                                                                               
         LA    RF,L'SAVDPT(RF)                                                  
         LA    RE,L'SAVDPT(RF)                                                  
         LA    R0,SAVDPTS+L'SAVDPTS                                             
         CR    RE,R0                                                            
         BL    VALDTX10                                                         
         DROP  N,O                                                              
*                                                                               
VALDTXX  DS    0H                                                               
         OI    FVIIND,FVIVAL       PREVIOUSLY VALIDATED                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMPETITIVE STATIONS                                          
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
* DISPLAY COMPETITIVE STATIONS FIELD                                            
***********************************************************************         
DISCST   DS    0H                                                               
         LA    R5,DISCSTS                                                       
         USING STALIN,R5                                                        
         LA    R6,FVIFLD                                                        
         LA    R0,NUMCSTA          NUMBER OF COMPETITIVE STATIONS               
         B     DISCST10                                                         
*                                                                               
DISCST5  OC    STLNSTA,STLNSTA        ANYTHING TO DISPLAY?                      
         BZ    DISCSTX                NO                                        
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
*                                                                               
DISCST10 MVC   0(L'STLNSTA,R6),STLNSTA                                          
         LA    R6,L'STLNSTA-1(R6)                                               
         CLI   0(R6),C' '                                                       
         BH    *+10                                                             
         BCTR  R6,0                REMOVE SPACES                                
         B     *-10                                                             
         LA    R6,1(R6)                                                         
         LA    R5,L'DISCSTA(R5)                                                 
         BCT   R0,DISCST5                                                       
         DROP  R5                                                               
*                                                                               
DISCSTX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE COMPETITIVE STATIONS FIELD                                           
***********************************************************************         
VALCST   DS    0H                                                               
         CLI   FVILEN,0                                                         
         BE    VALCSTX                                                          
*                                                                               
         L     R0,AIO4             CLEAR AIO4                                   
         LA    R1,500                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 VPARSNIP,BODMCB,(FVILEN,FVIFLD),(NUMSTAS,AIO4),0                 
         CLI   4(R1),1                                                          
         BL    VALCST99                                                         
         CLI   4(R1),NUMCSTA                                                    
         BH    EXITNV              TOO MANY STATIONS                            
*                                                                               
         LA    RE,DISBOOKS                                                      
         LA    R0,DISBOOKS+L'DISBOOKS                                           
         USING BOOKLIN,RE                                                       
VALCST02 DS    0H                                                               
         OC    0(BKLNLENQ,RE),0(RE)                                             
         BZ    *+12                                                             
         CLI   BKLNFIL,RPRBKINQ                                                 
         BNE   VALCST04                                                         
         LA    RE,BKLNLENQ(RE)                                                  
         CR    RE,R0                                                            
         BL    VALCST02                                                         
         B     *+8                 *!LIVE*                                      
         B     VALCST09            PATRICE SAID OK                              
*                                                                               
VALCST04 ZIC   RE,4(R1)                                                         
         BCTR  RE,0                                                             
         LA    RF,CSTBKTB(RE)                                                   
         CLC   BKCOUNT,0(RF)                                                    
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(TOOMNYBK)                                           
         B     EXITL                                                            
*                                                                               
VALCST09 LA    R6,DISCSTS          R6 = A(1ST STATION)                          
         L     R3,AIO4             R3 = A(1ST PARSNIP FIELD)                    
         USING PSND,R3                                                          
*                                                                               
VALCST10 DS    0H                                                               
         CLI   PSNTAG,0            ANY MORE FIELDS?                             
         BE    VALCST40            NO                                           
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
         CLI   PSNTAG,C'F'         REGUALR FIELD COMPONENT                      
         BNE   EXITNV                                                           
*                                                                               
         TM    PSNSTAT,PSNNUMQ     FIELD SHOULD NOT BE NUMERIC                  
         BNZ   EXITNV                                                           
*                                                                               
         CLI   PSNLEN,0            MISSING FIELD                                
         BE    EXITNV                                                           
*                                                                               
         CLI   PSNLEN,L'RSTAKSTA   TOO LONG                                     
         BH    EXITNV                                                           
*                                                                               
         MVC   BOWORK1,BCSPACES                                                 
         L     RE,PSNCOMP                                                       
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOWORK1(0),0(RE)                                                 
*                                                                               
         OC    BOWORK1,BCSPACES   UPPERCASE                                     
         CLC   CCONKSTA,BOWORK1   DON'T ALLOW PRIMARY STATION                   
         BE    EXITNV                                                           
*                                                                               
         LA    R1,DISCSTS         DON'T ALLOW DUPLICATES                        
         USING STALIN,R1                                                        
VALCST15 CR    R1,R6                                                            
         BE    VALCST20                                                         
         CLC   STLNSTA,BOWORK1                                                  
         BE    EXITNV                                                           
         LA    R1,L'DISCSTA(R1)                                                 
         B     VALCST15                                                         
         DROP  R1                                                               
*                                                                               
VALCST20 LA    RE,IOKEY                                                         
         USING RSTAKEY,RE                                                       
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,CUAALF                                                  
         MVC   RSTAKSTA,BOWORK1                                                 
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO6+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RSTAKEY),IOKEYSAV                                        
         BNE   EXITNV                                                           
*                                                                               
         XC    0(L'DISCSTA,R6),0(R6)                                            
         USING STALIN,R6                                                        
         MVC   STLNSTA,BOWORK1                                                  
         DROP  R6                                                               
*                                                                               
VALCST30 ZIC   RE,FVERRNDX         CURSOR KLUGE                                 
         ZIC   RF,PSNLEN                                                        
         LA    RE,1(RF,RE)                                                      
         STC   RE,FVERRNDX                                                      
*                                                                               
         LA    R3,PSNL(R3)         BUMP TO THE NEXT FIELD                       
         LA    R6,L'DISCSTA(R6)                                                 
         B     VALCST10                                                         
         DROP  R3                                                               
*                                                                               
VALCST40 MVI   FVERRNDX,0          NO MORE INDEX INTO FIELD NEEDED              
         B     VALCSTX                                                          
*                                                                               
VALCST99 XC    DISCSTS,DISCSTS     ALL STATIONS DELETED                         
*                                                                               
VALCSTX  B     EXITOK                                                           
         SPACE 2                                                                
CSTBKTB  DS    0AL1              # BOOKS FOR # OF STATIONS                      
         DC    AL1(5)              FOR 1                                        
         DC    AL1(3)              FOR 2                                        
         DC    AL1(2)              FOR 3                                        
         DC    AL1(0)              FOR 4                                        
         DC    AL1(0)              FOR 5                                        
         DC    AL1(0)              FOR 6                                        
         DC    AL1(0)              FOR 7                                        
         DC    AL1(0)              FOR 8                                        
         DC    AL1(0)              FOR 9                                        
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR HIATUS                                                        
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
HIADTA   DS    0H                                                               
         LA    RF,HIATBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
HIATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISHIA)                                 
*        DC    AL1(DVAL),AL1(0,0,0),AL4(VALHIA)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY HIATUS FIELD                                                          
***********************************************************************         
DISHIA   DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE HIATUS FIELD                                                         
***********************************************************************         
VALHIA   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TEXT                                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
TXTDTA   DS    0H                                                               
         LA    RF,TXTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TXTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTXT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTXT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY TEXT FIELD                                                            
***********************************************************************         
DISTXT   DS    0H                                                               
         MVI   FVIFLD,C'N'                                                      
         TM    SAVOPTNS,OPTNTXTQ   IS THE TEXT BIT ON?                          
         BZ    DISTXTX                                                          
         MVI   FVIFLD,C'Y'                                                      
DISTXTX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE TEXT FIELD                                                           
***********************************************************************         
VALTXT   DS    0H                                                               
         CLI   FVIFLD,C'N'                                                      
         BE    VALTXTX                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV              INVALID IF NEITHER Y/N                       
         OI    SAVOPTNS,OPTNTXTQ         TEXT SET TO Y                          
*                                                                               
VALTXTX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DECIMAL PRECISION                                             
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DECDTA   DS    0H                                                               
         LA    RF,DECTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DECTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDEC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDEC)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DECIMAL PRECISION FIELD                                               
***********************************************************************         
DISDEC   DS    0H                                                               
         MVI   FVIFLD,C'N'                                                      
         TM    SAVOPTNS,OPTNDECQ   IS THE DECIMAL BIT ON?                       
         BNZ   DISTXTX                                                          
         MVI   FVIFLD,C'Y'         NO, WANT ROUNDING                            
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE DECIMAL PRECISION FIELD                                              
***********************************************************************         
VALDEC   DS    0H                                                               
         CLI   FVIFLD,C'Y'                                                      
         BE    VALDECX                                                          
         CLI   FVIFLD,C'N'                                                      
         BNE   EXITNV              INVALID IF NEITHER Y/N                       
         OI    SAVOPTNS,OPTNDECQ         ROUND DEMO SET TO Y                    
*                                                                               
VALDECX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BOOK TYPE                                                     
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
BTPDTA   DS    0H                                                               
         LA    RF,BTPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BTPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBTP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBTP)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY BOOK TYPE FIELD                                                       
***********************************************************************         
DISBTP   DS    0H                                                               
         MVC   FVIFLD(1),SAVBKTYP                                               
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE BOOK TYPE FIELD                                                      
***********************************************************************         
VALBTP   DS    0H                                                               
         CLI   FVILEN,0                                                         
         BE    VALBTP15                                                         
*                                                                               
         CLI   SELPROFS,RREPQSEL                                                
         BE    VALBTP0                                                          
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
VALBTP0  DS    0H                                                               
         CLI   FVIFLD,RPRBKINQ                                                  
         BE    VALBTP20                                                         
*                                                                               
         CLI   FVIFLD,RPRBKTPQ                                                  
         BNE   *+16                                                             
         TM    SELPROF+SELTPB,SELTPA                                            
         BO    EXITNV                                                           
         B     VALBTP20                                                         
*                                                                               
         CLI   FVIFLD,RPRBKT4Q                                                  
         BNE   *+16                                                             
         TM    SELPROF+SELT4B,SELT4A                                            
         BO    EXITNV                                                           
         B     VALBTP20                                                         
*                                                                               
         CLI   FVIFLD,RPRBKPAQ                                                  
         BNE   *+16                                                             
         TM    SELPROF+SELPAVB,SELPAVA                                          
         BO    EXITNV                                                           
         B     VALBTP20                                                         
*                                                                               
         B     EXITNV                                                           
*                                                                               
VALBTP15 MVI   FVIFLD,RPRBKINQ     DEFAULT                                      
*                                                                               
VALBTP20 MVC   0(1,RE),FVIFLD                                                   
         MVC   SAVBKTYP,FVIFLD                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ONLINE STATION ACCESS                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SACDTA   DS    0H                                                               
         LA    RF,SACTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SACTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSAC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSAC)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY ONLINE STATION ACCESS FIELD                                           
***********************************************************************         
DISSAC   DS    0H                                                               
         MVI   FVIFLD,C'N'                                                      
         TM    DESCFLGS,RPRDSSTA                                                
         BZ    *+8                                                              
         MVI   FVIFLD,C'Y'                                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE ONLINE STATION ACCESS FIELD                                          
***********************************************************************         
VALSAC   DS    0H                                                               
         NI    DESCFLGS,FF-RPRDSSTA                                             
         CLI   FVILEN,0                                                         
         BE    VALSACX                                                          
         CLI   FVIFLD,C'N'                                                      
         BE    VALSACX                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    DESCFLGS,RPRDSSTA                                                
*                                                                               
VALSACX  B     EXITOK                                                           
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
SCRNTBL  DC    AL1(SSET),AL1(0,0,0),AL4(SETSCR)                                 
         DC    AL1(SKSET),AL1(0,0,0),AL4(SETKSCR)                               
         DC    AL1(SMOD),AL1(0,0,0),AL4(MODSCR)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* SET THE KEY SCREEN CODE                                                       
***********************************************************************         
SETKSCR  DS    0H                                                               
         MVI   GSSKCODE,0          CLEAR THE CODE                               
*                                                                               
         MVI   GSSKCODE,KYDEF      SET TO DEFAULT SCREEN CODE                   
*                                                                               
SETKSCRX B     EXITOK                                                           
***********************************************************************         
* SET THE DATA SCREEN CODE                                                      
***********************************************************************         
SETSCR   DS    0H                                                               
         MVI   GSSMCODE,0          CLEAR THE CODE                               
*                                                                               
         CLI   CSACT,A#ADD                                                      
         BNE   *+12                                                             
         MVI   GSSMCODE,DTADD                                                   
         B     SETSCRX                                                          
*                                                                               
         MVI   GSSMCODE,DTDEF      SET TO DEFAULT SCREEN CODE                   
*                                                                               
SETSCRX  B     EXITOK                                                           
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
         CLI   CSACT,A#ADD                                                      
         BNE   *+8                                                              
         LA    RF,KNOWTAB3                                                      
*                                                                               
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
* TABLE OF WANTED KEY OBJECTS FOR ADD                                           
***********************************************************************         
KNOWTAB3 DS    0XL(KNOWLQ)                                                      
         DC    AL2(00001),AL4(FLDPROT)   CONTRACT                               
         DC    AL2(EOT)                                                         
***********************************************************************         
* TABLE OF WANTED KEY OBJECTS                                                   
***********************************************************************         
KNOWTAB2 DS    0XL(KNOWLQ)                                                      
         DC    AL2(00001),AL4(FLDPROT)   CONTRACT                               
         DC    AL2(00002),AL4(FLDPROT)   PROPOSAL                               
         DC    AL2(EOT)                                                         
FLDPROT  DC    H'0'                DUMMY BRANCH                                 
*                                                                               
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
         DC    AL1(PFLST),AL1(0,0,0),AL4(VALPFK)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE PFKEY                                                                
***********************************************************************         
VALPFK   DS    0H                                                               
         CLI   CSACT,A#ADD                                                      
         BNE   PFKYES                                                           
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVPRONUM-TWAD)                                             
         OC    0(L'SVPRONUM,RE),0(RE)                                           
         BNZ   PFKYES                                                           
*                                                                               
         L     RE,8(R1)                                                         
         USING FRPELD,RE                                                        
***      CLI   FRPPFK#,PFPENDNG                                                 
***      BE    PFKNO                                                            
         CLI   FRPPFK#,PFAVAIL                                                  
         BE    PFKNO                                                            
         CLI   FRPPFK#,PFPACKGE                                                 
         BE    PFKNO                                                            
         CLI   FRPPFK#,PFMBOOK                                                  
         BE    PFKNO                                                            
         CLI   FRPPFK#,PFMDEMO                                                  
         BE    PFKNO                                                            
         CLI   FRPPFK#,PFMCOST                                                  
         BE    PFKNO                                                            
         CLI   FRPPFK#,PFKYRIS                                                  
         BE    PFKNO                                                            
         CLI   FRPPFK#,PFKYMORE                                                 
         BE    PFKNO                                                            
         CLI   FRPPFK#,PFCSTLST                                                 
         BE    PFKNO                                                            
         DROP  RE                                                               
*                                                                               
PFKYES   B     EXITOK                                                           
PFKNO    B     EXITL                                                            
***********************************************************************         
* CAN SET THE RECORD FOR THE PFKEY                                              
***********************************************************************         
RECPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),PFPENDNG                                                   
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'Pend'                                             
         B     RECPFKX                                                          
         CLI   0(RE),PFAVAIL                                                    
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'Avail'                                            
         B     RECPFKX                                                          
         CLI   0(RE),PFPACKGE                                                   
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'Package'                                          
         B     RECPFKX                                                          
         CLI   0(RE),PFMBOOK                                                    
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'Mbook'                                            
         B     RECPFKX                                                          
         CLI   0(RE),PFMDEMO                                                    
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'Mdemo'                                            
         B     RECPFKX                                                          
         CLI   0(RE),PFMCOST                                                    
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'Mcost'                                            
         B     RECPFKX                                                          
         CLI   0(RE),PFCSTLST                                                   
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'Cost'                                             
         B     RECPFKX                                                          
         CLI   0(RE),7             ANYTHING ABOVE LEFT - NO RECORD              
         BNL   NOTPFK                                                           
*                                                                               
RECPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE ACTION FOR THE PFKEY                                              
***********************************************************************         
ACTPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),PFPENDNG                                                   
         BL    ACTPFKX                                                          
         BH    *+14                                                             
         MVC   FVIFLD(8),=CL8'Cha'                                              
         B     ACTPFKX                                                          
*                                                                               
         CLI   0(RE),PFMDEMO                                                    
         BNH   NOTPFK              BTWN PF3 AND PF6, NO ACTION WANTED           
*                                                                               
         CLI   0(RE),PFMCOST                                                    
         BE    NOTPFK              OF PF15, NO ACTION WANTED                    
*                                                                               
         CLI   0(RE),PFCSTLST                                                   
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'List'                                             
         B     ACTPFKX                                                          
*                                                                               
ACTPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE USER NAME FOR THE PFKEY                                           
***********************************************************************         
USRPFK   DS    0H                                                               
         L     RE,8(R1)            R2 = A(PFKEY #)                              
*                                                                               
         CLI   0(RE),PFKYRIS       PFKEY TO RIS?                                
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'RIS'                                              
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),11                                                         
         BNE   USRPFK5                                                          
         CLI   PSACT,A#LST        PREV ACTION LIST                              
         BNE   NOTPFK                                                           
*                                                                               
USRPFK5  DS    0H                                                               
*                                                                               
USRPFKX  B     EXITOK                                                           
***********************************************************************         
* PFKEY DEFINITION (RECORD, ACTION, OR USER) NOT WANTED                         
***********************************************************************         
NOTPFK   OI    SVPARMS3,X'80'                                                   
         B     EXITOK                                                           
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
         L     RE,AIO5                                                          
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
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 VMINIO,BODMCB,('MINWRT',(R5))                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         LA    R5,PSSAV               COMING FROM WORK/UPDATE?                  
         USING SSAVD,R5                                                         
         CLC   =C'WORUPD',SDATA                                                 
         BNE   MINWRTX                NO                                        
         MVC   SDATA(7),=C'PROCHAY'   YES, DRASTIC CHANGE TO PROPOSAL           
*                                                                               
MINWRTX  B     EXITOK                                                           
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
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
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
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
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
         TM    MNIOFLAG,MNIOCLSQ   DO WE NEED TO?                               
         BZ    EXITOK              NO                                           
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINCLS',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
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
EXITENTR MVC   FVMSGNO,=AL2(84)                                                 
         MVI   FVOMTYP,C'I'                                                     
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
EXITIOBK MVC   FVMSGNO,=AL2(INVOVBOK)                                           
         B     EXITL               INVALID OVERRIDE BOOK                        
EXITIBKE MVC   FVMSGNO,=Y(INVBKEXP)                                             
         B     EXITL               INVALID BOOK EXPRESSION                      
EXITNMOR MVC   FVMSGNO,=AL2(NMOREPRO)                                           
         B     EXITL               NO MORE PROPOSALS FOR THIS CONTRACT          
EXITDUPL MVC   FVMSGNO,=AL2(401)                                                
         B     EXITL               DUPLICATE ENTRY NOT ALLOWED                  
EXITSLCK MVC   FVMSGNO,=AL2(GE$SLOCK)                                           
         B     EXITL               EXIT WITH SECURITY LOCKOUT                   
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
* DATA OBJECT FOR SATELLITE STATION                                             
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
         DS    0D                                                               
         USING *,RB                                                             
STLDTA   DS    0H                                                               
         LR    RB,RF                                                            
         B     *+12                                                             
         DC    CL8'*STLDTA*'                                                    
*                                                                               
         LA    RF,STLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
STLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTL)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY SATELLITE STATION                                                     
***********************************************************************         
DISSTL   DS    0H                                                               
         NI    FVATRB,FF-FVAPROT                                                
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ                                                 
         BAS   RE,MINIOHI                                                       
         BNE   DISSTL00                                                         
         L     RE,MINELEM                                                       
         CLI   0(RE),RPRDTELQ                                                   
         BNE   DISSTL00                                                         
         DROP  R5                                                               
*                                                                               
         OI    FVATRB,FVAPROT                                                   
*                                                                               
DISSTL00 DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS-TWAD) PRIMARY STARTION ENTRY                       
         USING STALIN,RE                                                        
         MVI   FVIFLD,C'N'                                                      
*                                                                               
         CLI   STLNSTA+4,C'T'                                                   
         BE    DISSTL02                                                         
         CLI   STLNSTA+4,C' '                                                   
         BE    DISSTL02                                                         
         OI    FVATRB,FVAPROT                                                   
*                                                                               
DISSTL02 TM    STLNFLG,RPRSTSTL                                                 
         BZ    *+8                                                              
         MVI   FVIFLD,C'Y'                                                      
         DROP  RE                                                               
*                                                                               
DISSTLX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE SATELLITE STATION                                                    
***********************************************************************         
VALSTL   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS-TWAD) PRIMARY STARTION ENTRY                       
         USING STALIN,RE                                                        
         NI    STLNFLG,FF-RPRSTSTL                                              
*                                                                               
         CLI   STLNSTA+4,C'T'                                                   
         BE    VALSTL02                                                         
         CLI   STLNSTA+4,C' '                                                   
         BE    VALSTL02                                                         
         OI    FVATRB,FVAPROT                                                   
         MVI   FVIFLD,C'N'                                                      
         B     VALSTLX                                                          
*                                                                               
VALSTL02 CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         MVI   FVIFLD,C'N'                                                      
         B     VALSTLX                                                          
*                                                                               
         CLI   FVIFLD,C'N'                                                      
         BE    VALSTLX                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    STLNFLG,RPRSTSTL                                                 
         DROP  RE                                                               
*                                                                               
VALSTLX  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACTIVITY DATES                                                
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
         DS    0D                                                               
         USING *,RB                                                             
ADTDTA   DS    0H                                                               
         LR    RB,RF                                                            
         B     *+12                                                             
         DC    CL8'*ADTDTA*'                                                    
*                                                                               
         LA    RF,ADTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
ADTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISADT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY ACTIVITY DATES FIELD                                                  
***********************************************************************         
DISADT   DS    0H                                                               
         MVC   FVIFLD(9),=C'CHG(ADD) '                                          
         LA    R2,FVIFLD+9                                                      
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         USING RPRACELD,R6                                                      
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRACELQ                                                 
         MVI   MINEKEY+1,RPRACPHC                                               
         BAS   RE,MINIOHI                                                       
         CLI   0(R6),RPRACELQ                                                   
         BNE   DISADT02                                                         
         CLI   RPRACTYP,RPRACPHC                                                
         BNE   DISADT02                                                         
*                                                                               
         GOTO1 VDATCON,BODMCB,(8,RPRACDAT),(11,0(R2))                           
         LA    R2,8(R2)                                                         
*                                                                               
         TM    RPRACFLG,RPRACFAS                                                
         BZ    *+12                                                             
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
DISADT02 DS    0H                                                               
         MVI   0(R2),C'('                                                       
         LA    R2,1(R2)                                                         
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRACELQ                                                 
         MVI   MINEKEY+1,RPRACADD                                               
         BAS   RE,MINIOHI                                                       
         CLI   0(R6),RPRACELQ                                                   
         BNE   DISADT04                                                         
         CLI   RPRACTYP,RPRACADD                                                
         BNE   DISADT04                                                         
*                                                                               
         GOTO1 VDATCON,BODMCB,(8,RPRACDAT),(11,0(R2))                           
         LA    R2,8(R2)                                                         
*                                                                               
         TM    RPRACFLG,RPRACFAS                                                
         BZ    *+12                                                             
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
DISADT04 DS    0H                                                               
         MVI   0(R2),C')'                                                       
*                                                                               
         B     EXITOK                                                           
         DROP  R5,R6                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BEFORE THE DISPLAYING THE DATA                                                
***********************************************************************         
D1STDDIS DS    0H                                                               
         NMOD1 0,**DFDIS*                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
         L     RA,4(R1)                                                         
         USING TWAD,RA                                                          
*                                                                               
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         CLI   CSACT,A#ADD         ARE WE ADDING?                               
         BNE   DFDDIS50            NO                                           
*                                                                               
         TM    MISCFLG1,MF1PFRET   RETURNING FROM CALLED SESSION?               
         BNZ   *+12                                                             
         CLI   BCPFKEY,12                                                       
         BNE   DFDDIS5             RETURN PFKEY HIT                             
         OI    MISCFLG1,MF1PFRET                                                
*                                                                               
         LR    RE,RA               DO WE HAVE ANY PROPOSAL NUMBER?              
         AH    RE,=Y(SVPRONUM-TWAD)                                             
         OC    0(L'SVPRONUM,RE),0(RE)                                           
         BZ    DFDDIS5             NO                                           
         B     DFDDIS50            RECORD ADDED REDISPLAY                       
*                                                                               
         ZAP   BOWORK1+10(5),=P'0'                                              
         MVO   BOWORK1+10(5),0(4,RE)                                            
         ZAP   BOWORK1(5),=P'99999999'                                          
         SP    BOWORK1(5),BOWORK1+10(5)                                         
         OI    BOWORK1+4,X'0F'                                                  
         UNPK  FVIFLD(8),BOWORK1(5)                                             
         MVI   FVILEN,8                                                         
         MVI   FVXLEN,7                                                         
         OI    FVIIND,FVINUM                                                    
*                                                                               
         GOTOX (VALCONQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*                                                                               
DFDDIS5  DS    0H                                                               
         TM    MISCFLG1,MF1KYCHG           DID THE KEY CHANGE?                  
         BZ    DFDDIS99                                                         
*                                                                               
         XC    DISDEMOS,DISDEMOS   COPY THE CONTRACT'S DEMOS                    
         LA    RE,DISDEMOS                                                      
         USING DEMOLIN,RE                                                       
         LA    RF,CSARDEM                                                       
         LA    R0,CSARDEM+L'CSARDEM                                             
DFDDIS7  MVC   DMLNDEMO,0(RF)                                                   
         LA    RE,L'DISDEMO(RE)                                                 
         LA    RF,3(RF)                                                         
         CR    RF,R0                                                            
         BL    DFDDIS7                                                          
         DROP  RE                                                               
*                                                                               
         XC    SAVSLNS,SAVSLNS     COPY THE CONTRACT'S LENGTHS                  
         LA    RE,6                   HAS A MAX OF 6 LENGTHS                    
         LA    RF,CSARLEN                                                       
         LA    R1,SAVSLNS                                                       
DFDDIS10 OC    0(2,RF),0(RF)          ANY MORE LENGTHS?                         
         BZ    DFDDIS20               NO                                        
*                                                                               
         MVC   0(L'SAVSLN,R1),1(RF)   COPY THE LENGTH (1 BYTE)                  
         LA    RF,2(RF)                                                         
         LA    R1,L'SAVSLN(R1)                                                  
         BCT   RE,DFDDIS10                                                      
*                                                                               
DFDDIS20 XC    SAVDPTS,SAVDPTS     COPY THE CONTRACT'S DAYPARTS                 
         LA    RF,SAVDPTS                                                       
         USING DPTLIN,RF                                                        
         LA    RE,CSARDPT                                                       
         LA    R0,8                                                             
*                                                                               
DFDDIS22 DS    0H                                                               
         OC    0(5,RE),0(RE)       END OF DAYPARTS?                             
         BE    DFDDIS24            YES                                          
         MVC   DPLNDPT,0(RE)                                                    
         MVC   DPLNCPP,1(RE)                                                    
         TM    CCONFLG1,CCONDPMQ   USE HARD CODED TABLE?                        
         BO    *+8                 YES                                          
         OI    DPLNFLG,RPRDPFMN                                                 
*                                                                               
         LA    RE,5(RE)                                                         
         LA    RF,L'SAVDPT(RF)                                                  
         BCT   R0,DFDDIS22                                                      
         DROP  RF                                                               
*                                                                               
DFDDIS24 DS    0H                                                               
         OI    SAVOPTNS,OPTNDECQ   DEFAULT TO NO ROUNDING                       
*                                                                               
         LR    RE,RA               SET STATION TABLE                            
         AH    RE,=Y(MINSTAS-TWAD)                                              
         XC    0(L'MINSTAS,RE),0(RE)                                            
         USING STALIN,RE                                                        
         MVC   STLNSTA,CCONKSTA                                                 
         DROP  RE                                                               
         B     DFDDIS99                                                         
******                                                                          
* NON ADD ACTION                                                                
******                                                                          
DFDDIS50 DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
***************                                                                 
* DESCRIPTION ELEMENT                                                           
***************                                                                 
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
         MVC   SAVFTM,RPRDSFTM     COPY THE FETCH METHOD                        
         MVC   SAVBKTYP,RPRDSBTP   COPY BOOK TYPE                               
         CLI   SAVBKTYP,C' '                                                    
         BH    *+8                                                              
         MVI   SAVBKTYP,RPRDSBIQ                                                
         MVC   DESCFLGS,RPRDSFLG                                                
***************                                                                 
* BOOK ELEMENT(S)                                                               
***************                                                                 
         LR    RE,RA                                                            
         AH    RE,=Y(MINBKS-TWAD)                                               
         XC    0(L'MINBKS,RE),0(RE)                                             
         LR    RE,RA                                                            
         AH    RE,=Y(MINLBLS-TWAD)                                              
         XC    0(L'MINLBLS,RE),0(RE)                                            
*                                                                               
         XC    DISBOOKS,DISBOOKS                                                
         XC    DISLBLS,DISLBLS                                                  
         XC    DISUPGDS,DISUPGDS                                                
         MVI   WHICHUPE,1                                                       
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ    GET THE BOOK ELEMENT                         
         BAS   RE,MINIOHI                                                       
         BNE   DFDDBKX                                                          
*                                                                               
         USING BOOKLIN,R4                                                       
*                                                                               
DFDDBK10 L     R6,MINELEM                                                       
         USING RPRBKELD,R6                                                      
         ZIC   R4,RPRBKDOR         DISPLAY ORDER NUMBER                         
         BCTR  R4,0                                                             
         LR    RE,R4                                                            
         MH    R4,=Y(L'DISBOOK)                                                 
         LA    R4,DISBOOKS(R4)                                                  
*                                                                               
         CLI   RPRBKLEN,RPRBKOVQ   USER DEFINED BOOK?                           
         BH    DFDDBK20            YES                                          
*********                                                                       
* REGULAR BOOK                                                                  
*********                                                                       
         MVC   BKLNBK,RPRBKSTT      BOOK                                        
         MVC   BKLNSPBK,RPRBKBKT    SPECIAL BOOK TYPE                           
         MVC   BKLNFIL,RPRBKFIL     BOOK SOURCE(I/T/P/4)                        
*                                                                               
         ZIC   R4,RPRBKIOR                                                      
         BCTR  R4,0                                                             
         MH    R4,=Y(L'MINBK)                                                   
         LR    R0,RA                                                            
         AH    R0,=Y(MINBKS-TWAD)                                               
         AR    R4,R0                                                            
         MVC   BKLNBK,RPRBKSTT      BOOK                                        
         MVC   BKLNSPBK,RPRBKBKT    SPECIAL BOOK TYPE                           
         MVC   BKLNFIL,RPRBKFIL     BOOK SOURCE(I/T/P/4)                        
         B     DFDDBK50                                                         
*********                                                                       
* USER-DEFINED BOOK                                                             
*********                                                                       
DFDDBK20 DS    0H                                                               
         MVC   BKLNBK,RPRBKSTT      BOOK                                        
         MVC   BKLNSPBK,RPRBKBKT    SPECIAL BOOK TYPE                           
         MVC   BKLNFIL,RPRBKFIL     BOOK SOURCE(I/T/P/4)                        
         MVC   BKLNUPGD,RPRBKBKS    UPGRADE FORMULA                             
         MVC   BKLNXBKS,RPRBKXBK    EXTRA BASE BOOKS                            
*                                                                               
         MH    RE,=Y(L'DISLBL)                                                  
         LA    RE,DISLBLS(RE)                                                   
         MVC   0(L'DISLBL,RE),RPRBKUDF  SAVE THE LABEL                          
*                                                                               
         ZIC   R4,RPRBKIOR                                                      
         BCTR  R4,0                                                             
         LR    RE,R4                                                            
         MH    R4,=Y(L'MINBK)                                                   
         LR    R0,RA                                                            
         AH    R0,=Y(MINBKS-TWAD)                                               
         AR    R4,R0                                                            
         MVC   BKLNBK,RPRBKSTT      BOOK                                        
         MVC   BKLNSPBK,RPRBKBKT    SPECIAL BOOK TYPE                           
         MVC   BKLNFIL,RPRBKFIL     BOOK SOURCE(I/T/P/4)                        
         MVC   BKLNUPGD,RPRBKBKS    UPGRADE FORMULA                             
         MVC   BKLNXBKS,RPRBKXBK    EXTRA BASE BOOKS                            
*                                                                               
         DROP  R4                                                               
*                                                                               
         MH    RE,=Y(L'MINLBL)                                                  
         LR    R0,RA                                                            
         AH    R0,=Y(MINLBLS-TWAD)                                              
         AR    RE,R0                                                            
         MVC   0(L'MINLBL,RE),RPRBKUDF  SAVE THE LABEL                          
*                                                                               
         ZIC   R1,WHICHUPE         DETERMINE WHICH UPGRADE FORMULA              
         BCTR  R1,0                   WE'RE USING                               
         MH    R1,=Y(L'DISUPGD)                                                 
         LA    R1,DISUPGDS(R1)                                                  
*                                                                               
         ZIC   RE,WHICHUPE                                                      
         LA    RE,1(RE)                                                         
         STC   RE,WHICHUPE                                                      
*                                                                               
         MVC   0(L'DISLBL,R1),RPRBKUDF    COPY THE LABEL HERE ALSO              
         IC    RE,RPRBKLEN                COPY THE UPGRADE FORMULA              
         SH    RE,=Y(RPRBKUOQ+1)                                                
         BM    DFDDBK50                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   L'DISLBL(0,R1),RPRBKUPG                                          
*                                                                               
DFDDBK50 BAS   RE,MINIOSEQ                                                      
         BNE   DFDDBKX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         CLI   0(R6),RPRBKELQ      BOOK ELEMENT STILL?                          
         BE    DFDDBK10                                                         
*                                                                               
DFDDBKX  DS    0H                                                               
***************                                                                 
* DEMO ELEMENT(S)                                                               
***************                                                                 
         XC    DISDEMOS,DISDEMOS                                                
         LR    RE,RA                                                            
         AH    RE,=Y(MINDMOS-TWAD)                                              
         XC    0(L'MINDMOS,RE),0(RE)                                            
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDMELQ    GET THE DEMO ELEMENT                         
         BAS   RE,MINIOHI                                                       
         BNE   DFDDDMX                                                          
*                                                                               
DFDDDM10 L     R6,MINELEM                                                       
         USING RPRDMELD,R6                                                      
         ZIC   R1,RPRDMDOR         DISPLAY ORDER NUMBER                         
         BCTR  R1,0                                                             
         MH    R1,=Y(L'DISDEMO)                                                 
         LA    R1,DISDEMOS(R1)                                                  
         USING DEMOLIN,R1                                                       
         MVC   DMLNIORD,RPRDMIOR                                                
         MVC   DMLNDORD,RPRDMDOR                                                
         MVC   DMLNDEMO,RPRDMBY1                                                
         MVC   DMLNFLG,RPRDMFLG                                                 
*                                                                               
         ZIC   R1,RPRDMIOR         INTERNAL ORDER NUMBER                        
         BCTR  R1,0                                                             
         MH    R1,=Y(L'MINDMO)                                                  
         LR    R0,RA                                                            
         AH    R0,=Y(MINDMOS-TWAD)                                              
         AR    R1,R0                                                            
         MVC   DMLNIORD,RPRDMIOR                                                
         MVC   DMLNDORD,RPRDMDOR                                                
         MVC   DMLNDEMO,RPRDMBY1                                                
         MVC   DMLNFLG,RPRDMFLG                                                 
         DROP  R1                                                               
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         BNE   DFDDDMX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         CLI   0(R6),RPRDMELQ      DEMO ELEMENT STILL?                          
         BE    DFDDDM10                                                         
*                                                                               
DFDDDMX  DS    0H                                                               
***************                                                                 
* DAYPART ELEMENT(S)                                                            
***************                                                                 
         XC    SAVDPTS,SAVDPTS                                                  
         XC    SAVDYTMS,SAVDYTMS                                                
         XC    SAVSEDTS,SAVSEDTS                                                
         LA    R2,SAVDPTS                                                       
         USING DPTLIN,R2                                                        
         LA    R3,SAVDYTMS                                                      
         LA    R4,SAVSEDTS                                                      
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDPELQ    GET THE DAYPART ELEMENT                      
         BAS   RE,MINIOHI                                                       
         BNE   DFDDDPX                                                          
*                                                                               
DFDDDP10 L     R6,MINELEM                                                       
         USING RPRDPELD,R6                                                      
*                                                                               
         MVC   DPLNDPT,RPRDPDPT                                                 
         MVC   DPLNFLG,RPRDPFLG                                                 
         MVC   DPLNCPP,RPRDPTAB                                                 
         MVC   0(L'RPRDPDAY,R3),RPRDPDAY                                        
         MVC   L'RPRDPDAY(L'RPRDPSTM+L'RPRDPETM,R3),RPRDPSTM                    
         MVC   0(L'RPRDPSDT+L'RPRDPEDT,R4),RPRDPSDT                             
         DROP  R2                                                               
*                                                                               
         LA    R2,L'SAVDPT(R2)                                                  
         LA    R3,L'SAVDYTM(R3)                                                 
         LA    R4,L'SAVSEDT(R4)                                                 
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         BNE   DFDDDPX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         CLI   0(R6),RPRDPELQ      DAYPART ELEMENT STILL?                       
         BE    DFDDDP10                                                         
*                                                                               
DFDDDPX  DS    0H                                                               
************                                                                    
* STATIONS *                                                                    
************                                                                    
         XC    DISCSTS,DISCSTS                                                  
         LR    R4,RA                                                            
         AH    R4,=Y(MINSTAS-TWAD)                                              
         XC    0(L'MINSTAS,R4),0(R4)                                            
         USING STALIN,R4                                                        
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRSTELQ    GET THE STATION ELEMENT                      
         L     R6,MINELEM                                                       
         USING RPRSTELD,R6                                                      
         BAS   RE,MINIOHI                                                       
         BE    *+6                 NEED AT LEAST ONE                            
         DC    H'0'                                                             
         CLI   RPRSTEL,RPRSTELQ                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   RPRSTICD,1          PRIMARY ?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DFDDCS5  DS    0H                                                               
         CLI   RPRSTEL,RPRSTELQ                                                 
         BNE   DFDDCS10            NO MORE COMPETITIVE STATIONS                 
         MVC   STLNFLG,RPRSTFLG                                                 
         MVC   STLNSTA,RPRSTSTA                                                 
         MVC   STLNIORD,RPRSTICD                                                
         LA    R4,L'MINSTA(R4)                                                  
         BAS   RE,MINIOSEQ                                                      
         BE    DFDDCS5                                                          
         DROP  R4                                                               
*                                                                               
DFDDCS10 LR    R4,RA                                                            
         AH    R4,=Y(MINSTAS+L'MINSTA-TWAD)                                     
         MVC   DISCSTS,0(R4)       DISPLAY STATIONS                             
*                                                                               
DFDDCSX  DS    0H                                                               
         DROP  R5,R6                                                            
***********************************                                             
* DON'T CARE WHERE THE INFO IS COMING FROM                                      
***********************************                                             
DFDDIS99 MVI   SVDPTLIN,0          NOT ON ANY DAYPART LINE YET                  
         MVI   SVPRJFLD,0          NOT IN ANY PROJECTION FIELD YET              
         LR    RE,RA                                                            
         AH    RE,=Y(MINDPTS-TWAD)                                              
         MVC   0(L'MINDPTS,RE),SAVDPTS                                          
         LR    RE,RA                                                            
         AH    RE,=Y(MINDYTMS-TWAD)                                             
         MVC   0(L'MINDYTMS,RE),SAVDYTMS                                        
         LR    RE,RA                                                            
         AH    RE,=Y(MINSEDTS-TWAD)                                             
         MVC   0(L'MINSEDTS,RE),SAVSEDTS                                        
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BEFORE THE I/O CALL TO ADD THE RECORD                                         
***********************************************************************         
R1STRADD DS    0H                                                               
         NMOD1 0,**RFRA**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
         L     RA,4(R1)                                                         
         USING TWAD,RA                                                          
*                                                                               
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINBKS-TWAD)                                               
         XC    0(L'MINBKS,RE),0(RE)                                             
         LR    RE,RA                                                            
         AH    RE,=Y(MINLBLS-TWAD)                                              
         XC    0(L'MINLBLS,RE),0(RE)                                            
         LR    RE,RA                                                            
         AH    RE,=Y(MINDMOS-TWAD)                                              
         XC    0(L'MINDMOS,RE),0(RE)                                            
         LR    RE,RA                                                            
         AH    RE,=Y(MINDPTS-TWAD)                                              
         XC    0(L'MINDPTS,RE),0(RE)                                            
         LR    RE,RA                                                            
         AH    RE,=Y(MINDYTMS-TWAD)                                             
         XC    0(L'MINDYTMS,RE),0(RE)                                           
         LR    RE,RA                                                            
         AH    RE,=Y(MINSEDTS-TWAD)                                             
         XC    0(L'MINSEDTS,RE),0(RE)                                           
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
***************                                                                 
* DESCRIPTION ELEMENT                                                           
***************                                                                 
         L     R6,MINELEM                                                       
         USING RPRDSELD,R6                                                      
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRDSEL,RPRDSELQ                                                 
         MVI   RPRDSLEN,RPRDSLNQ                                                
         MVC   RPRDSFTM,SAVFTM     FETCH METHOD                                 
         MVC   RPRDSSAL,CCONSAL    SALESPERSON                                  
         MVC   RPRDSBTP,SAVBKTYP   BOOK TYPE                                    
*                                                                               
         TM    SAVOPTNS,OPTNTXTQ   DISPLAY TEXT FROM INVENTORY RECS?            
         BZ    *+8                                                              
         OI    RPRDSOPT,RPRDSOTX   YES                                          
*                                                                               
         TM    SAVOPTNS,OPTNDECQ   USE DEMO DECIMAL PRECISION?                  
         BZ    *+8                                                              
         OI    RPRDSOPT,RPRDSODC   YES                                          
*                                                                               
         MVC   RPRDSSEC,SAVSLNS    SECONDS LIST                                 
         MVC   RPRDSFLG,DESCFLGS   FLAGS                                        
*                                                                               
         BAS   RE,MINIOADD         ADD THE DESCRIPTION ELEMENT                  
         BE    *+6                                                              
         DC    H'0'                                                             
***************                                                                 
* SWITCH ELEMENT                                                                
***************                                                                 
         L     R6,MINELEM                                                       
         USING RPRSWELD,R6                                                      
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRSWEL,RPRSWELQ                                                 
         MVI   RPRSWLEN,RPRSWLNQ                                                
         MVC   RPRSWSAL,CCONSAL                                                 
         MVC   RPRSWOFF,CCONKOFF                                                
         MVC   RPRSWTEM,CCONTEM                                                 
         MVC   RPRSWSTA,CCONKSTA                                                
         MVC   RPRSWADV,CCONKADV                                                
         MVC   RPRSWAGY,CCONKAGY                                                
         MVC   RPRSWAOF,CCONKAOF                                                
         MVC   RPRSWGRP,CCONKGRP                                                
         MVC   RPRSWDSP,CCONDVS                                                 
         MVC   RPRSWDCT,CCONDVT                                                 
         MVC   RPRSWFLT,CCONDAT                                                 
*                                                                               
         BAS   RE,MINIOADD         ADD THE DESCRIPTION ELEMENT                  
         BE    *+6                                                              
         DC    H'0'                                                             
***************                                                                 
* ACTIVITY DATE ELEMENT                                                         
***************                                                                 
         L     R6,MINELEM                                                       
         USING RPRACELD,R6                                                      
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRACEL,RPRACELQ                                                 
         MVI   RPRACLEN,RPRACLNQ                                                
         MVI   RPRACTYP,RPRACADD                                                
         GOTO1 VDATCON,BODMCB,(5,0),(19,RPRACDAT)                               
*                                                                               
         BAS   RE,MINIOADD         ADD THE ACTIVITY ELEMENT                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRACELD,R6                                                      
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRACEL,RPRACELQ                                                 
         MVI   RPRACLEN,RPRACLNQ                                                
         MVI   RPRACTYP,RPRACPHC                                                
         GOTO1 VDATCON,BODMCB,(5,0),(19,RPRACDAT)                               
*                                                                               
         BAS   RE,MINIOADD         ADD THE ACTIVITY ELEMENT                     
         BE    *+6                                                              
         DC    H'0'                                                             
***************                                                                 
* BOOK ELEMENT(S)                                                               
***************                                                                 
RFRABK00 LA    R2,DISBOOKS                                                      
         USING BOOKLIN,R2                                                       
         LA    R3,DISLBLS                                                       
         MVI   WHICHUPE,1                                                       
         LA    R0,1                INTERNAL ORDER NUMBER                        
         MVI   WHICHUPE,1                                                       
         L     R6,MINELEM                                                       
         USING RPRBKELD,R6                                                      
*                                                                               
RFRABK10 OC    0(L'DISLBL,R3),0(R3)    ANY LABEL DEFINED HERE?                  
         BNZ   *+14                                                             
         OC    0(L'DISBOOK,R2),0(R2)   ANY BOOK DEFINED HERE?                   
         BZ    RFRABKX                                                          
*                                                                               
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRBKEL,RPRBKELQ                                                 
         MVI   RPRBKLEN,RPRBKOVQ   L(ELEM IF NOT A USER DEFINED BOOK)           
         STC   R0,RPRBKIOR         INTERNAL ORDER NUMBER                        
*                                                                               
         STC   R0,RPRBKDOR         DISPLAY = INTERNAL IF ADDING                 
         MVC   RPRBKSTT,BKLNBK                                                  
         MVC   RPRBKBYM,BKLNBK+L'RPRBKSTT                                       
         MVC   RPRBKBKT,BKLNSPBK                                                
*                                                                               
         MVI   RPRBKFIL,RPRBKINQ     DEFAULT OF INV                             
         TM    RPRBKSTT,RPRBKSES+RPRBKSPJ+RPRBKST2+RPRBKSTP                     
         BNZ   *+10                                                             
         MVC   RPRBKFIL,BKLNFIL                                                 
*                                                                               
         OC    0(L'DISLBL,R3),0(R3)  USER DEFINED BOOK?                         
         BZ    RFRABK50              NO                                         
*                                                                               
*********                                                                       
* USER-DEFINED BOOK                                                             
*********                                                                       
         MVC   RPRBKUDF,0(R3)      SAVE THE USER DEFINED LABEL                  
         MVC   RPRBKBKS(RPRBKUPG-RPRBKBKS),BKLNUPGD                             
         MVC   RPRBKXBK,BKLNXBKS                                                
*                                                                               
         ZIC   RF,WHICHUPE                                                      
         BCTR  RF,0                                                             
         MH    RF,=Y(L'DISUPGD)                                                 
         LA    RF,DISUPGDS(RF)                                                  
*                                                                               
         MVC   RPRBKUPG(L'DISUPGD),L'DISLBL(RF)                                 
         LA    RE,RPRBKUPG+L'DISUPGD-2-L'DISLBL                                 
         CLI   0(RE),C' '          SHORTEN EXPRESSION AS MUCH AS                
         BH    *+10                    POSSIBLE                                 
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RE,1(RE)                                                         
         LR    R1,RE                                                            
         SR    R1,R6                                                            
         STC   R1,RPRBKLEN         NEW LENGTH OF ELEMENT                        
*                                                                               
         ZIC   R1,WHICHUPE         USE NEXT UPGRADE EXPRESSION                  
         LA    R1,1(R1)                                                         
         STC   R1,WHICHUPE                                                      
*                                                                               
RFRABK50 DS    0H                  UPDATE MINBKS & MINLBLS                      
         LR    RE,RA                                                            
         AH    RE,=Y(MINBKS-TWAD)                                               
         LR    RF,R0                                                            
         BCTR  RF,0                ZERO BASED                                   
         MH    RF,=Y(L'MINBK)                                                   
         AR    RE,RF                                                            
         MVC   0(L'MINBK,RE),0(R2)                                              
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINLBLS-TWAD)                                              
         LR    RF,R0                                                            
         BCTR  RF,0                ZERO BASED                                   
         MH    RF,=Y(L'MINLBL)                                                  
         AR    RE,RF                                                            
         MVC   0(L'MINLBL,RE),0(R3)                                             
*                                                                               
         BAS   RE,MINIOADD         ADD THE BOOK ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,L'DISBOOK(R2)    LOOP UNTIL ALL BOOKS COPIED                  
         LA    R3,L'DISLBL(R3)                                                  
         AH    R0,=H'1'            INCREMENT INTERNAL ORDER NUMBER              
*                                                                               
         LA    RE,DISBOOKS+L'DISBOOKS DID WE PASS THE END OF DISBOOKS?          
         CR    R2,RE                                                            
         BL    RFRABK10            NO, MORE BOOKS                               
*                                                                               
RFRABKX  DS    0H                                                               
         DROP  R2                                                               
***************                                                                 
* DEMO ELEMENT(S)                                                               
***************                                                                 
RFRADM00 LA    R2,DISDEMOS                                                      
         USING DEMOLIN,R2                                                       
         LA    R3,1                INTERNAL ORDER NUMBER                        
         L     R6,MINELEM                                                       
         USING RPRDMELD,R6                                                      
*                                                                               
RFRADM10 OC    DMLNDEMO,DMLNDEMO   END OF DEMO LIST?                            
         BZ    RFRADMX             YES                                          
         CLI   DMLNDEMO,FF         END OF DEMO LIST?                            
         BE    RFRADMX             YES                                          
*                                                                               
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRDMEL,RPRDMELQ                                                 
         MVI   RPRDMLEN,RPRDMLNQ                                                
         STC   R3,RPRDMIOR         INTERNAL ORDER NUMBER                        
*                                                                               
         STC   R3,RPRDMDOR         DISPLAY = INTERNAL IF ADDING                 
         MVI   RPRDMSRC,C'N'       NEILSEN SO FAR                               
         MVC   RPRDMBY1(L'DMLNDEMO),DMLNDEMO                                    
*                                                                               
         LR    RE,RA               UPDATE MINDMOS                               
         AH    RE,=Y(MINDMOS-TWAD)                                              
         LR    RF,R3                                                            
         BCTR  RF,0                ZERO BASED                                   
         MH    RF,=Y(L'MINDMO)                                                  
         AR    RE,RF                                                            
M        USING DEMOLIN,RE                                                       
         MVC   M.DMLNDEMO,DMLNDEMO                                              
         DROP  M                                                                
*                                                                               
         BAS   RE,MINIOADD         ADD THE DEMO ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,L'DISDEMO(R2)    LOOP UNTIL ALL BOOKS COPIED                  
         LA    R3,1(R3)            INCREMENT INTERNAL ORDER NUMBER              
*                                                                               
         LA    RE,DISDEMOS+L'DISDEMOS                                           
         CR    R2,RE                                                            
         BL    RFRADM10            NO                                           
         DROP  R2                                                               
*                                                                               
RFRADMX  DS    0H                                                               
***************                                                                 
* STATION ELEMENT                                                               
***************                                                                 
RFRAST00 L     R6,MINELEM                                                       
         USING RPRSTELD,R6                                                      
*                                                                               
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRSTEL,RPRSTELQ                                                 
         MVI   RPRSTLEN,RPRSTLNQ                                                
         MVI   RPRSTICD,X'01'      CONTRACT STATION IS 1ST STATION              
         LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS-TWAD) PRIMARY STATION ENTRY                        
         USING STALIN,RE                                                        
*                                                                               
         CLI   STLNSTA,C' '         DISAPPEARING STATION CHECK                  
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(611)                                                
         B     EXITL                                                            
*                                                                               
         MVC   RPRSTSTA,STLNSTA                                                 
         MVC   RPRSTFLG,STLNFLG                                                 
         DROP  RE                                                               
*                                                                               
         BAS   RE,MINIOADD         ADD THE STATION ELEMENT                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RFRASTX  DS    0H                                                               
***************                                                                 
* DAYPART ELEMENT(S)                                                            
***************                                                                 
RFRADP00 LA    R1,1                                                             
         LA    R2,SAVDPTS                                                       
         USING DPTLIN,R2                                                        
         LA    R3,SAVDYTMS                                                      
         LA    R4,SAVSEDTS                                                      
         L     R6,MINELEM                                                       
         USING RPRDPELD,R6                                                      
*                                                                               
RFRADP10 OC    0(L'SAVDPT,R2),0(R2)   ANY DAYPART?                              
         BZ    RFRADP20               NO SKIP                                   
*                                                                               
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRDPEL,RPRDPELQ                                                 
         MVI   RPRDPLEN,RPRDPLNQ                                                
         STC   R1,RPRDPSEQ         INTERNAL ORDER NUMBER                        
*                                                                               
         MVC   RPRDPFLG,DPLNFLG       COPY FLAGS                                
         MVC   RPRDPDPT,DPLNDPT       COPY 1-BYTE DAYPART CODE                  
         MVC   RPRDPTAB,DPLNCPP       COPY 4 BYTE BUYER CPP                     
         MVC   RPRDPDAY,0(R3)         COPY 1-BYTE DAY CODE                      
         MVC   RPRDPSTM(L'RPRDPSTM+L'RPRDPETM),1(R3)                            
         MVC   RPRDPSDT(L'RPRDPSDT+L'RPRDPEDT),0(R4)                            
         DROP  R2                                                               
*                                                                               
         LR    RE,RA               UPDATE MINDPTS                               
         AH    RE,=Y(MINDPTS-TWAD)                                              
         LR    RF,R1                                                            
         BCTR  RF,0                ZERO BASED                                   
         MH    RF,=Y(L'MINDPT)                                                  
         AR    RE,RF                                                            
         MVC   0(L'MINDPT,RE),0(R2)                                             
*                                                                               
         LR    RE,RA               UPDATE MINDYTMS                              
         AH    RE,=Y(MINDYTMS-TWAD)                                             
         LR    RF,R1                                                            
         BCTR  RF,0                ZERO BASED                                   
         MH    RF,=Y(L'MINDYTM)                                                 
         AR    RE,RF                                                            
         MVC   0(L'MINDYTM,RE),0(R3)                                            
*                                                                               
         LR    RE,RA               UPDATE MINSEDTS                              
         AH    RE,=Y(MINSEDTS-TWAD)                                             
         LR    RF,R1                                                            
         BCTR  RF,0                ZERO BASED                                   
         MH    RF,=Y(L'MINSEDT)                                                 
         AR    RE,RF                                                            
         MVC   0(L'MINSEDT,RE),0(R4)                                            
*                                                                               
         BAS   RE,MINIOADD         ADD DAYPART ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,1(R1)            LOOP UNTIL ALL DAYPARTS COPIED               
RFRADP20 LA    R2,L'SAVDPT(R2)                                                  
         LA    R3,L'SAVDYTM(R3)                                                 
         LA    R4,L'SAVSEDT(R4)                                                 
*                                                                               
         LA    RE,SAVDPTS+L'SAVDPTS  DID WE PASS THE END OF SAVDPTS?            
         CR    R2,RE                                                            
         BL    RFRADP10              NO, WE CAN LOOP                            
*                                                                               
RFRADPX  DS    0H                                                               
*                                                                               
***************                                                                 
* COMPETITIVE STATION ELEMENTS                                                  
***************                                                                 
RFRACS00 DS    0H                                                               
         L     R6,MINELEM                                                       
         USING RPRSTELD,R6                                                      
         LA    R2,DISCSTS          COMPETITIVE STATIONS                         
         USING STALIN,R2                                                        
         LA    R0,2                INTERNAL ORDER #                             
*                                                                               
RFRACS10 OC    0(L'DISCSTA,R2),0(R2)   ANY STATION DEFINED HERE?                
         BZ    RFRACSX                                                          
*                                                                               
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRSTEL,RPRSTELQ                                                 
         MVI   RPRSTLEN,RPRSTLNQ                                                
         STC   R0,RPRSTICD                                                      
         STC   R0,STLNIORD                                                      
         MVC   RPRSTSTA,STLNSTA                                                 
         DROP  R2                                                               
*                                                                               
         BAS   RE,MINIOADD         ADD THE STATION ELEMENT                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,L'DISCSTA(R2)    LOOP UNTIL ALL STATIONS                      
         AH    R0,=H'1'            INCREMENT INTERNAL ORDER NUMBER              
*                                                                               
         LA    RE,DISCSTS+L'DISCSTS  DID WE PASS THE END OF DISCSTS?            
         CR    R2,RE                                                            
         BL    RFRACS10              NO                                         
*                                                                               
RFRACSX  DS    0H                                                               
***************                                                                 
** FINISH UP **                                                                 
***************                                                                 
RFRADDX  LR    RE,RA                                                            
         AH    RE,=Y(MINDPTS-TWAD)                                              
         MVC   SAVDPTS,0(RE)                                                    
         LR    RE,RA                                                            
         AH    RE,=Y(MINDYTMS-TWAD)                                             
         MVC   SAVDYTMS,0(RE)                                                   
         LR    RE,RA                                                            
         AH    RE,=Y(MINSEDTS-TWAD)                                             
         MVC   SAVSEDTS,0(RE)                                                   
         LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS+L'MINSTA-TWAD)                                     
         MVC   0(L'DISCSTS,RE),DISCSTS                                          
*                                                                               
         B     EXITOK                                                           
         DROP  R5,R6                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BEFORE THE I/O CALL TO WRITE THE RECORD                                       
***********************************************************************         
R1STRWRT DS    0H                                                               
         NMOD1 0,**RFRW**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
         L     RA,4(R1)                                                         
         USING TWAD,RA                                                          
*                                                                               
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
***********************************                                             
* DESCRIPTION ELEMENT                                                           
***********************************                                             
RFRWDS00 L     R6,MINELEM                                                       
         USING RPRDSELD,R6                                                      
         XC    0(RPRDSLNQ,R6),0(R6)                                             
         MVI   RPRDSEL,RPRDSELQ                                                 
         MVI   RPRDSLEN,RPRDSLNQ                                                
*                                                                               
         TM    SAVOPTNS,OPTNTXTQ   DISPLAY TEXT FROM INVENTORY RECS?            
         BZ    *+8                                                              
         OI    RPRDSOPT,RPRDSOTX   YES                                          
*                                                                               
         TM    SAVOPTNS,OPTNDECQ   USE DEMO DECIMAL PRECISION?                  
         BZ    *+8                                                              
         OI    RPRDSOPT,RPRDSODC   YES                                          
*                                                                               
         MVC   RPRDSSEC,SAVSLNS    SECONDS LIST                                 
         MVC   RPRDSFTM,SAVFTM     FETCH METHOD                                 
         MVC   RPRDSBTP,SAVBKTYP   BOOK TYPE                                    
         MVC   RPRDSFLG,DESCFLGS   FLAGS                                        
*                                                                               
         MVC   BOELEM,0(R6)        SAVE THIS COPY WHILE WE USE MINIO            
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDSELQ                                                 
         BAS   RE,MINIORD                                                       
*                                                                               
         CLC   0(RPRDSLNQ,R6),BOELEM    DO WE NEED TO CHANGE THIS ELEM?         
         BE    RFRWDSX                  NO, WE DON'T                            
         MVC   0(RPRDSLNQ,R6),BOELEM                                            
         BAS   RE,MINIOWRT         WRITE THE DESCRIPTION ELEMENT                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    MINEKEY,MINEKEY     CLEAN UP SPOT LENGTHS                        
         MVI   MINEKEY,RPRDTELQ    DETAIL CLUSTER                               
         BAS   RE,MINIOHI                                                       
         BNE   RFRWDSX                                                          
RFRWDS02 L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         CLI   0(R6),RPRDTELQ                                                   
         BNE   RFRWDSX                                                          
*                                                                               
         LA    RE,SAVSLNS          SECONDS LIST                                 
         LA    RF,SAVSLNS+L'SAVSLNS                                             
RFRWDS04 CLC   RPRDTSLN,0(RE)      LENGTH FOUND?                                
         BE    RFRWDS06            YES                                          
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BL    RFRWDS04                                                         
*                                                                               
         MVC   RPRDTSLN,SAVSLNS    LENGHT DELETED - SET TO DEFAULT              
*                                                                               
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    MINEKEY,MINEKEY     SOMETHING SCREWY HERE                        
         MVI   MINEKEY,RPRDTELQ                                                 
         L     R6,MINELEM                                                       
         MVC   MINEKEY+1(7),2(R6)                                               
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RFRWDS06 DS    0H                                                               
         BAS   RE,MINIOSEQ                                                      
         BE    RFRWDS02                                                         
*                                                                               
RFRWDSX  DS    0H                                                               
***************                                                                 
* ACTIVITY DATE ELEMENT                                                         
***************                                                                 
         L     R6,MINELEM                                                       
         USING RPRACELD,R6                                                      
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRACELQ                                                 
         MVI   MINEKEY+1,RPRACPHC                                               
         BAS   RE,MINIOHI                                                       
         CLI   0(R6),RPRACELQ                                                   
         BNE   RFRWAC02                                                         
         CLI   RPRACTYP,RPRACPHC                                                
         BNE   RFRWAC02                                                         
*                                                                               
         BAS   RE,MINIODEL         DELETE EXISTING ACTIVITY ELEMENT             
*                                                                               
RFRWAC02 DS    0H                                                               
         L     R6,MINELEM                                                       
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRACEL,RPRACELQ                                                 
         MVI   RPRACLEN,RPRACLNQ                                                
         MVI   RPRACTYP,RPRACPHC                                                
         GOTO1 VDATCON,BODMCB,(5,0),(19,RPRACDAT)                               
*                                                                               
         BAS   RE,MINIOADD         ADD THE ACTIVITY ELEMENT                     
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
***************                                                                 
* STATION ELEMENT                                                               
***************                                                                 
RFRWST00 L     R6,MINELEM                                                       
         USING RPRSTELD,R6                                                      
*                                                                               
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRSTEL,RPRSTELQ                                                 
         MVI   RPRSTLEN,RPRSTLNQ                                                
         MVI   RPRSTICD,X'01'      CONTRACT STATION IS 1ST STATION              
         LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS-TWAD) PRIMARY STATION ENTRY                        
         USING STALIN,RE                                                        
*                                                                               
         CLI   STLNSTA,C' '         DISAPPEARING STATION CHECK                  
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(611)                                                
         B     EXITL                                                            
*                                                                               
         MVC   RPRSTSTA,STLNSTA                                                 
         MVC   RPRSTFLG,STLNFLG                                                 
         DROP  RE                                                               
*                                                                               
         MVC   BOELEM,0(R6)        SAVE THIS COPY WHILE WE USE MINIO            
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRSTELQ                                                 
         MVI   MINEKEY+(RPRSTICD-RPRSTELD)-1,X'01'                              
         BAS   RE,MINIORD                                                       
*                                                                               
         CLC   0(RPRSTLNQ,R6),BOELEM    DO WE NEED TO CHANGE THIS ELEM?         
         BE    RFRWSTX                  NO, WE DON'T                            
         MVC   0(RPRSTLNQ,R6),BOELEM                                            
         BAS   RE,MINIOWRT         WRITE THE DESCRIPTION ELEMENT                
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,UNFETCH                                                       
*                                                                               
RFRWSTX  DS    0H                                                               
         EJECT                                                                  
***********************************                                             
* BOOK ELEMENT(S)                                                               
***********************************                                             
         NI    MISCFLG1,X'FF'-MF1CLRPF    DON'T CLEAR PRIME FETCH BIT           
*******************                                                             
***** PASS 1 ******                ***** CHANGING AND DELETING *****            
*******************                                                             
RFRWBK00 LR    R2,RA               WHAT IS IN MINIO NOW                         
         AH    R2,=Y(MINBKS-TWAD)                                               
         LR    R3,RA                                                            
         AH    R3,=Y(MINLBLS-TWAD)                                              
         LA    R4,1                INTERNAL ORDER NUMBER                        
*                                                                               
RFRWBK10 LA    R6,1                DISPLAY ORDER NUMBER                         
*                                                                               
         OC    0(L'MINLBL,R3),0(R3)     ANY LABEL HERE?                         
         BNZ   RFRWBK20                 YES                                     
         OC    0(L'MINBK,R2),0(R2)      NO, ANY BOOK THEN?                      
         BZ    RFRWBK40                     NO, SKIP IT                         
*********                                                                       
* CHECK FOR BOOK                                                                
*********                                                                       
M        USING BOOKLIN,R2                                                       
         LA    RE,DISBOOKS         WHAT WE'RE GOING TO CHANGE INTO              
         USING BOOKLIN,RE                                                       
RFRWBK15 CLC   M.BKLNCBK,BKLNCBK                                                
         BE    RFRWBK35            CHANGE THE BOOK ELEMENT                      
         LA    RE,L'DISBOOK(RE)                                                 
         LA    R6,1(R6)            BUMP TO NEXT BOOK IN DISPLAY                 
         LA    R0,DISBOOKS+L'DISBOOKS                                           
         CR    RE,R0               DID WE HIT THE END OF DISPLAY LIST?          
         BL    RFRWBK15            NO                                           
         B     RFRWBK30            YES, DELETE THIS BOOK                        
         DROP  M,RE                                                             
*********                                                                       
* CHECK FOR LABEL                                                               
*********                                                                       
RFRWBK20 LA    RE,DISLBLS          WHAT WE'RE GOING TO CHANGE INTO              
         MVI   WHICHUPE,0                                                       
RFRWBK25 OC    0(L'DISLBL,RE),0(RE)   DO WE HAVE AN UPGRADE EXPRESSION?         
         BZ    RFRWBK26                                                         
         ZIC   R0,WHICHUPE            YES, WE HAVE TO KNOW WHICH ONE            
         AH    R0,=H'1'                 TO USE                                  
         STC   R0,WHICHUPE                                                      
*                                                                               
RFRWBK26 CLC   0(L'MINLBL,R3),0(RE)                                             
         BE    RFRWBK27                                                         
         LA    RE,L'DISLBL(RE)                                                  
         LA    R6,1(R6)            BUMP TO NEXT BOOK IN DISPLAY                 
         LA    R0,DISLBLS+L'DISLBLS                                             
         CR    RE,R0                                                            
         BL    RFRWBK25            NO                                           
         B     RFRWBK30            YES, DELETE THIS LABEL                       
*                                                                               
RFRWBK27 DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ                                                 
         STC   R4,MINEKEY+1                                                     
         BAS   RE,MINIORD                                                       
         L     RE,MINELEM                                                       
         USING RPRBKELD,RE                                                      
         LR    RF,R6                                                            
         BCTR  RF,0                                                             
         LR    R0,RF                                                            
         MH    RF,=Y(L'DISBOOK)    RE = A(BOOKLIN)                              
         LA    RF,DISBOOKS(RF)                                                  
         USING BOOKLIN,RF                                                       
*                                                                               
         CLC   BKLNBK,RPRBKSTT                                                  
         BNE   RFRWBK30            BASE BOOK CHANGE                             
*                                                                               
         CLC   BKLNSPBK,RPRBKBKT                                                
         BNE   RFRWBK30            SPECIAL BOOK SOURCE CHANGE                   
*                                                                               
         CLC   BKLNXBKS,RPRBKXBK                                                
         BNE   RFRWBK30            EXTRA BASE BOOK CHANGE                       
*                                                                               
         CLC   BKLNUPGD,RPRBKBKS                                                
         BNE   RFRWBK30            FORMULA CHANGE                               
*                                                                               
         CLC   BKLNFIL,RPRBKFIL    BOOK SOURCE(I/T/P/4) CHANGE?                 
         BNE   RFRWBK30            YES                                          
         B     RFRWBK35            NO - DISPLAY ORDER CHANGE ONLY               
         DROP  RE,RF                                                            
*                                                                               
******************                                                              
***** DELETE *****                                                              
******************                                                              
RFRWBK30 XC    MINEKEY,MINEKEY     DELETE THIS BOOK OR LABEL                    
         MVI   MINEKEY,RPRBKELQ                                                 
         STC   R4,MINEKEY+1                                                     
         BAS   RE,MINIORD                                                       
         BAS   RE,MINIODEL                                                      
         XC    0(L'MINBK,R2),0(R2)   TO MAKE ROOM FOR ADDING                    
         XC    0(L'MINLBL,R3),0(R3)                                             
         B     RFRWBKX0                                                         
******************                                                              
***** CHANGE *****                                                              
******************                                                              
RFRWBK35 XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ                                                 
         STC   R4,MINEKEY+1                                                     
         BAS   RE,MINIORD                                                       
         LR    RE,R6                                                            
         L     R6,MINELEM                                                       
         USING RPRBKELD,R6                                                      
         STC   RE,RPRBKDOR         DISPLAY ORDER                                
         BAS   RE,MINIOWRT             NO - WRITE ELEMENT OUT                   
         B     RFRWBK40                                                         
         DROP  R6                                                               
*                                                                               
*===========                                                                    
* DELETE ASSOCIATED DETAIL DATA                                                 
*===========                                                                    
RFRWBKX0 DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBXELQ    EXTENSION ELEMENT                            
         STC   R4,MINEKEY+1                                                     
         BAS   RE,MINIOHI                                                       
         BNE   RFRWBKXD                                                         
*                                                                               
         L     RE,MINELEM                                                       
         CLI   0(RE),RPRBXELQ                                                   
         BNE   RFRWBKXD                                                         
         USING RPRBXELD,RE                                                      
         CLM   R4,1,RPRBXIOR                                                    
         BNE   RFRWBKXD                                                         
         DROP  RE                                                               
*                                                                               
         BAS   RE,MINIODEL                                                      
*                                                                               
RFRWBKXD DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ    DETAIL CLUSTER                               
         BAS   RE,MINIOHI                                                       
         BNE   RFRWBKX4                                                         
RFRWBKX1 L     RE,MINELEM                                                       
         CLI   0(RE),RPRDTELQ                                                   
         BNE   RFRWBKX4                                                         
*                                                                               
         LA    R1,SBBITS(R4)       TURN OFF ANY SURPRESSED DETAILS              
         BCTR  R1,0                                                             
         NC    RPRDTBKS-RPRDTELD(1,RE),0(R1)                                    
*                                                                               
         BAS   RE,INTOAIO5         SO WE CAN USE RECUP                          
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)    FIRST ELEMENT                         
RFRWBKX2 CLI   0(R6),0             END OF CLUSTER?                              
         BE    RFRWBKX3                                                         
         ZIC   RE,1(R6)            ELEMENT LENGTH                               
         AR    R6,RE                                                            
         CLI   0(R6),RPRDVELQ      DEMO VALUE ELEMENT?                          
         BNE   RFRWBKX2            NO                                           
*                                                                               
         USING RPRDVELD,R6                                                      
         CLM   R4,1,RPRDVBNM       BOOK MATCH?                                  
         BNE   RFRWBKX2            NO                                           
*                                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
         BAS   RE,FROMAIO5                                                      
RFRWBKX3 DS    0H                                                               
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    MINEKEY,MINEKEY     SOMETHING SCREWY HERE                        
         MVI   MINEKEY,RPRDTELQ                                                 
         L     R6,MINELEM                                                       
         MVC   MINEKEY+1(7),2(R6)                                               
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         BNE   RFRWBKX4            NO MORE DETAILS                              
         B     RFRWBKX1                                                         
*                                                                               
SBBITS   DC    X'BF',X'DF',X'EF',X'F7',X'FB',X'FD',X'FE'                        
*                                                                               
RFRWBKX4 DS    0H                  CHECK COST HEADERS FOR BOOK                  
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRCHELQ    DETAIL CLUSTER                               
         BAS   RE,MINIOHI                                                       
         BNE   RFRWBKX7                                                         
*                                                                               
RFRWBKX5 L     RE,MINELEM                                                       
         CLI   0(RE),RPRCHELQ                                                   
         BNE   RFRWBKX7                                                         
         USING RPRCHELD,R6                                                      
         CLM   R4,1,RPRCHBK                                                     
         BNE   RFRWBKX6                                                         
*                                                                               
         MVI   RPRCHBK,0                                                        
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    MINEKEY,MINEKEY     SOMETHING SCREWY HERE                        
         MVI   MINEKEY,RPRCHELQ                                                 
         L     R6,MINELEM                                                       
         MVC   MINEKEY+1(7),2(R6)                                               
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RFRWBKX6 BAS   RE,MINIOSEQ                                                      
         BNE   RFRWBKX7            NO MORE DETAILS                              
         B     RFRWBKX5                                                         
*                                                                               
RFRWBKX7 DS    0H                                                               
****************                                                                
***** MORE *****                                                                
****************                                                                
RFRWBK40 LA    R2,L'MINBK(R2)                                                   
         LA    R3,L'MINLBL(R3)                                                  
         LA    R4,1(R4)                                                         
         LR    R0,RA                                                            
         AH    R0,=Y(MINBKS+L'MINBKS-TWAD)                                      
         CR    R2,R0                                                            
         BL    RFRWBK10                                                         
*******************                                                             
***** PASS 2 ******                ***** ADDING *****                           
*******************                                                             
RFRWBK50 LA    R2,DISBOOKS         FIND OUT WHAT NEEDS TO BE ADDED              
         LA    R3,DISLBLS                                                       
         LA    R4,1                                                             
         MVI   WHICHUPE,0                                                       
*                                                                               
RFRWBK60 LR    RE,RA               WHAT WE'RE GOING TO ADD TO                   
         AH    RE,=Y(MINBKS-TWAD)                                               
         LR    RF,RA                                                            
         AH    RF,=Y(MINLBLS-TWAD)                                              
*                                                                               
         OC    0(L'DISLBL,R3),0(R3)     ANY LABEL HERE?                         
         BNZ   RFRWBK70                 YES                                     
         OC    0(L'DISBOOK,R2),0(R2)    NO, ANY BOOK THEN?                      
         BZ    RFRWBKX                      NO, WE'RE DONE                      
*********                                                                       
* CHECK FOR BOOK                                                                
*********                                                                       
RFRWBK65 OC    0(L'DISLBL,RF),0(RF)   MAKE SURE NO LABEL FIRST                  
         BNZ   *+14                   THERE IS, SKIP TO NEXT IN MINIO           
         CLC   0(L'DISBOOK,R2),0(RE)  MATCH ON BOOK?                            
         BE    RFRWBK90               YES, DON'T HAVE TO ADD THIS THEN          
*                                                                               
         LA    RE,L'MINBK(RE)                                                   
         LA    RF,L'MINLBL(RF)                                                  
         LR    R0,RA                                                            
         AH    R0,=Y(MINBKS+L'MINBKS-TWAD)                                      
         CR    RE,R0               DID WE HIT THE END OF MINIO LIST?            
         BL    RFRWBK65            NO                                           
*                                                                               
         LA    R6,1                                                             
         LR    RE,RA               WHAT WE'RE GOING TO ADD TO                   
         AH    RE,=Y(MINBKS-TWAD)                                               
         LR    RF,RA                                                            
         AH    RF,=Y(MINLBLS-TWAD)                                              
*                                                                               
RFRWBK67 OC    0(L'MINBK,RE),0(RE)    FOUND AN EMPTY SLOT                       
         BNZ   *+14                                                             
         OC    0(L'MINLBL,RF),0(RF)                                             
         BZ    RFRWBK80               YES, ADD IT HERE                          
         LA    R6,1(R6)               NO, BUMP INTERNAL ORDER NUMBER            
         LA    RE,L'MINBK(RE)                                                   
         LA    RF,L'MINLBL(RF)                                                  
         B     RFRWBK67                                                         
*********                                                                       
* CHECK FOR LABEL                                                               
*********                                                                       
RFRWBK70 ZIC   R0,WHICHUPE         WHAT UPGRADE EXPRESSION WE'RE USING          
         AH    R0,=H'1'                                                         
         STC   R0,WHICHUPE                                                      
*                                                                               
RFRWBK72 CLC   0(L'DISLBL,R3),0(RF)                                             
         BE    RFRWBK90            IT'S THERE ALREADY, NEXT ONE                 
         LA    RF,L'MINLBL(RF)                                                  
         LR    R0,RA                                                            
         AH    R0,=Y(MINLBLS+L'MINLBLS-TWAD)                                    
         CR    RF,R0               DID WE HIT THE END OF MINIO LIST?            
         BL    RFRWBK72            NO                                           
*                                                                               
         LA    R6,1                                                             
         LR    RE,RA               WHAT WE'RE GOING TO ADD TO                   
         AH    RE,=Y(MINBKS-TWAD)                                               
         LR    RF,RA                                                            
         AH    RF,=Y(MINLBLS-TWAD)                                              
*                                                                               
RFRWBK78 OC    0(L'MINBK,RE),0(RE)    FOUND AN EMPTY SLOT                       
         BNZ   *+14                                                             
         OC    0(L'MINLBL,RF),0(RF)                                             
         BZ    RFRWBK80               YES, ADD IT HERE                          
         LA    R6,1(R6)               NO, BUMP INTERNAL ORDER NUMBER            
         LA    RE,L'MINBK(RE)                                                   
         LA    RF,L'MINLBL(RF)                                                  
         B     RFRWBK78                                                         
****************                                                                
***** ADD ******                                                                
****************                                                                
RFRWBK80 MVC   0(L'MINBK,RE),0(R2)   WE'RE TAKING THIS ENTRY                    
         MVC   0(L'MINLBL,RF),0(R3)                                             
         USING BOOKLIN,R2                                                       
*                                                                               
         LR    RE,R6               RE = INTERNAL ORDER NUMBER                   
         L     R6,MINELEM                                                       
         USING RPRBKELD,R6                                                      
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRBKEL,RPRBKELQ                                                 
         MVI   RPRBKLEN,RPRBKOVQ   L(ELEM IF NOT A USER DEFINED BOOK)           
         STC   RE,RPRBKIOR         INTERNAL ORDER NUMBER                        
*                                                                               
         STC   R4,RPRBKDOR         DISPLAY = INTERNAL IF ADDING                 
         MVC   RPRBKSTT,BKLNBK                                                  
         MVC   RPRBKBYM,BKLNBK+L'RPRBKSTT                                       
         MVC   RPRBKBKT,BKLNSPBK                                                
         MVC   RPRBKFIL,BKLNFIL                                                 
*                                                                               
         OC    0(L'DISLBL,R3),0(R3)  USER DEFINED BOOK?                         
         BZ    RFRWBK85              NO, WE CAN ADD NOW                         
*********                                                                       
* USER-DEFINED BOOK                                                             
*********                                                                       
         MVC   RPRBKUDF,0(R3)      SAVE THE USER DEFINED LABEL                  
         MVC   RPRBKBKS(RPRBKUPG-RPRBKBKS),BKLNUPGD                             
         MVC   RPRBKXBK,BKLNXBKS                                                
*                                                                               
         LA    RE,DISUPGDS                                                      
         LA    RF,DISUPGDS+L'DISUPGDS                                           
         CLC   0(L'DISLBL,RE),0(R3)    LABEL MATCH?                             
         BE    *+16                    YES                                      
         LA    RE,L'DISUPGD(RE)                                                 
         CR    RE,RF                                                            
         BL    *-16                                                             
         DC    H'0'                    SHOULD HAVE                              
*                                                                               
         MVC   RPRBKUPG(L'DISUPGD),L'DISLBL(RE)                                 
         LA    RE,RPRBKUPG+L'DISUPGD-1-L'DISLBL                                 
         CLI   0(RE),C' '          SHORTEN EXPRESSION AS MUCH AS                
         BH    *+10                    POSSIBLE                                 
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RE,1(RE)                                                         
         LR    R1,RE                                                            
         SR    R1,R6                                                            
         STC   R1,RPRBKLEN         NEW LENGTH OF ELEMENT                        
*                                                                               
RFRWBK85 BAS   RE,MINIOADD         ADD THE BOOK ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
****************                                                                
***** MORE *****                                                                
****************                                                                
RFRWBK90 LA    R2,L'DISBOOK(R2)                                                 
         LA    R3,L'DISLBL(R3)                                                  
         LA    R4,1(R4)                                                         
         LA    R0,DISBOOKS+L'DISBOOKS                                           
         CR    R2,R0                                                            
         BL    RFRWBK60                                                         
*                                                                               
RFRWBKX  DS    0H                                                               
         DROP  R6,R2                                                            
         EJECT                                                                  
***********************************                                             
* DEMO ELEMENT(S)                                                               
***********************************                                             
******************                                                              
***** PASS 1 *****                 ***** CHANGING AND DELETING *****            
******************                                                              
RFRWDM00 LR    R2,RA               WHAT IS IN MINIO NOW                         
         AH    R2,=Y(MINDMOS-TWAD)                                              
         LA    R3,1                INTERNAL ORDER NUMBER                        
*                                                                               
RFRWDM10 LA    R6,1                DISPLAY ORDER NUMBER                         
*                                                                               
         OC    0(L'MINDMO,R2),0(R2)    ANY DEMO HERE?                           
         BZ    RFRWDM40                NO                                       
*                                                                               
M        USING DEMOLIN,R2                                                       
         LA    RE,DISDEMOS         WHAT IT'S GOING TO BE                        
         USING DEMOLIN,RE                                                       
RFRWDM20 CLC   M.DMLNDEMO,DMLNDEMO                                              
         BE    RFRWDM30            GOT A MATCH, CHECK THE NEXT ONE              
         LA    RE,L'DISDEMO(RE)                                                 
         LA    R6,1(R6)                                                         
         LA    R0,DISDEMOS+L'DISDEMOS                                           
         CR    RE,R0                                                            
         BL    RFRWDM20                                                         
         DROP  M,RE                                                             
******************                                                              
***** DELETE *****                                                              
******************                                                              
         XC    MINEKEY,MINEKEY     DELETE THIS DEMO ELEMENT                     
         MVI   MINEKEY,RPRDMELQ                                                 
         STC   R3,MINEKEY+1                                                     
         BAS   RE,MINIORD                                                       
         L     RE,MINELEM                                                       
         USING RPRDMELD,RE                                                      
         MVC   BOBYTE1,RPRDMFLG    SAVE THE FLAGS                               
         DROP  RE                                                               
         BAS   RE,MINIODEL                                                      
         XC    0(L'MINDMO,R2),0(R2)                                             
*                                                                               
*===========                                                                    
* DELETE ASSOCIATED DETAIL DATA                                                 
*===========                                                                    
RFRWDMX0 DS    0H                                                               
         TM    BOBYTE1,RPRDMFFT    FETCHED?                                     
         BZ    RFRWDM40            NO - NOTHING TO DO                           
*                                                                               
         NI    MISCFLG1,FF-MF1MXWRT                                             
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ    DETAIL CLUSTER                               
         BAS   RE,MINIOHI                                                       
         BNE   RFRWDM40                                                         
*                                                                               
RFRWDMX1 L     RE,MINELEM                                                       
         CLI   0(RE),RPRDTELQ                                                   
         BNE   RFRWDM40                                                         
         BAS   RE,INTOAIO5         SO WE CAN USE RECUP                          
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)    FIRST ELEMENT                         
RFRWDMX2 CLI   0(R6),0             END OF CLUSTER?                              
         BE    RFRWDMX5                                                         
         ZIC   RE,1(R6)            ELEMENT LENGTH                               
         AR    R6,RE                                                            
         CLI   0(R6),RPRDVELQ      DEMO VALUE ELEMENT?                          
         BNE   RFRWDMX2            NO                                           
*                                                                               
         LR    RF,R3               THE INTERNAL ORDER #                         
         BCTR  RF,0                ZERO BASED                                   
         MH    RF,=Y(L'RPRDVDMO)   INDEX INTO VALUE LIST                        
         LR    R0,RF               KEEP IT AROUND                               
         AH    RF,=Y(RPRDVOVQ+L'RPRDVDMO)                                       
         CLM   RF,1,1(R6)          LENGTH WITH THIS DEMO                        
         BH    RFRWDMX2            DEMO ISN'T HERE                              
*                                                                               
         ZIC   RE,1(R6)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)     SAVE THE ELEMENT                             
*                                  THEN DELETE IT                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
         LR    R1,R0                        INDEX INTO DEMO VALUES              
         LA    RE,BOELEM+RPRDVOVQ(R1)                                           
         XC    0(L'RPRDVDMO,RE),0(RE)       CLEAR THE VALUE                     
*                                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
*                                                                               
         OI    MISCFLG1,MF1MXWRT         SET WRT NEEDED                         
         B     RFRWDMX2                                                         
*                                                                               
RFRWDMX5 DS    0H                                                               
         TM    MISCFLG1,MF1MXWRT                                                
         BZ    RFRWDMX6                                                         
*                                                                               
         BAS   RE,FROMAIO5                                                      
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    MINEKEY,MINEKEY     SOMETHING SCREWY HERE                        
         MVI   MINEKEY,RPRDTELQ                                                 
         L     R6,MINELEM                                                       
         MVC   MINEKEY+1(7),2(R6)                                               
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RFRWDMX6 DS    0H                                                               
         BAS   RE,MINIOSEQ                                                      
         BNE   RFRWDM40            NO MORE DETAILS                              
         B     RFRWDMX1                                                         
******************                                                              
***** CHANGE *****                                                              
******************                                                              
RFRWDM30 XC    MINEKEY,MINEKEY     CHANGE THIS DEMO ELEMENT                     
         MVI   MINEKEY,RPRDMELQ                                                 
         STC   R3,MINEKEY+1                                                     
         BAS   RE,MINIORD                                                       
         LR    RE,R6                                                            
         L     R6,MINELEM                                                       
         USING RPRDMELD,R6                                                      
         STC   RE,RPRDMDOR         DISPLAY ORDER                                
         BAS   RE,MINIOWRT                                                      
******************                                                              
****** MORE ******                                                              
******************                                                              
RFRWDM40 LA    R2,L'MINDMO(R2)                                                  
         LA    R3,1(R3)                                                         
         LR    R0,RA                                                            
         AH    R0,=Y(MINDMOS+L'MINDMOS-TWAD)                                    
         CR    R2,R0                                                            
         BL    RFRWDM10                                                         
******************                                                              
***** PASS 2 *****                 ***** ADDING *****                           
******************                                                              
RFRWDM50 LA    R2,DISDEMOS                                                      
         USING DEMOLIN,R2                                                       
         LA    R3,1                                                             
*                                                                               
RFRWDM60 LR    RE,RA               WHAT WE'RE GOING TO ADD TO                   
         AH    RE,=Y(MINDMOS-TWAD)                                              
M        USING DEMOLIN,RE                                                       
*                                                                               
RFRWDM65 OC    DMLNDEMO,DMLNDEMO   END OF DEMO LIST?                            
         BZ    RFRWDMX             YES                                          
         CLI   DMLNDEMO,FF         END OF DEMO LIST?                            
         BE    RFRWDMX             YES                                          
*                                                                               
         CLC   DMLNDEMO,M.DMLNDEMO                                              
         BE    RFRWDM90            IT'S THERE ALREADY, NEXT ONE                 
         LA    RE,L'MINDMO(RE)                                                  
         LR    R0,RA                                                            
         AH    R0,=Y(MINDMOS+L'MINDMOS-TWAD)                                    
         CR    RE,R0               DID WE HIT THE END OF MINIO LIST             
         BL    RFRWDM65                                                         
*                                                                               
         LA    R6,1                                                             
         LR    RE,RA                                                            
         AH    RE,=Y(MINDMOS-TWAD)                                              
*                                                                               
RFRWDM70 OC    0(L'MINDMO,RE),0(RE)   FOUND AN EMPTY SLOT?                      
         BZ    RFRWDM80               YES, ADD IT HERE                          
         LA    R6,1(R6)               NO, BUMP INTERNAL ORDER NUMBER            
         LA    RE,L'MINDMO(RE)                                                  
         B     RFRWDM70                                                         
***************                                                                 
***** ADD *****                                                                 
***************                                                                 
RFRWDM80 MVC   M.DMLNDEMO,DMLNDEMO  WE'RE TAKING THIS ENTRY                     
         DROP  M                                                                
*                                                                               
         LR    RE,R6                                                            
         L     R6,MINELEM                                                       
         USING RPRDMELD,R6                                                      
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRDMEL,RPRDMELQ                                                 
         MVI   RPRDMLEN,RPRDMLNQ                                                
         STC   RE,RPRDMIOR         INTERNAL ORDER NUMBER                        
*                                                                               
         STC   R3,RPRDMDOR         DISPLAY = INTERNAL IF ADDING                 
         MVI   RPRDMSRC,C'N'       NEILSEN SO FAR                               
         MVC   RPRDMBY1(L'DMLNDEMO),DMLNDEMO                                    
         DROP  R2                                                               
*                                                                               
         BAS   RE,MINIOADD         ADD THE DEMO ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    MISCFLG1,MF1CLRPF   CLEAR PRIME FETCH FLAG                       
****************                                                                
***** MORE *****                                                                
****************                                                                
RFRWDM90 LA    R2,L'DISDEMO(R2)                                                 
         LA    R3,1(R3)                                                         
         LA    R0,DISDEMOS+L'DISDEMOS                                           
         CR    R2,R0                                                            
         BL    RFRWDM60                                                         
*                                                                               
RFRWDMX  DS    0H                                                               
         EJECT                                                                  
***********************************                                             
* DAYPART ELEMENT(S)                                                            
***********************************                                             
RFRWDP00 LA    R1,1                                                             
         LA    R2,SAVDPTS                                                       
         USING DPTLIN,R2                                                        
         LA    R3,SAVDYTMS                                                      
         LA    R4,SAVSEDTS                                                      
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDPELQ                                                 
         L     R6,MINELEM                                                       
         USING RPRDPELD,R6                                                      
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   RFRWDP50            ADD ALL THE DAYPARTS                         
*******************                                                             
* SKIP ALL FETCHED DAYPARTS                                                     
*******************                                                             
RFRWDP10 DS    0H                                                               
         OC    0(L'SAVDPT,R2),0(R2)  ANY DAYPART?                               
         BZ    RFRWDP16              NO - SKIP THIS LINE                        
*                                                                               
         TM    DPLNFLG,RPRDPFFT    WAS THIS DAYPART FETCHED ALREADY?            
         BZ    RFRWDP20            NO - DELETE EVERYTHING                       
*                                                                               
         CLI   RPRDPEL,RPRDPELQ    DAYPART ELEMENT?                             
         BNE   RFRWDP50            NO - ADD REMAINING DAYPARTS                  
*                                                                               
         CLM   R1,1,RPRDPSEQ       INTERNAL ORDER NUMBER                        
         BE    *+6                 HAD BETTER MATCH                             
         DC    H'0'                                                             
*                                                                               
         TM    RPRDPFLG,RPRDPFFT   WAS THIS DAYPART FETCHED ALREADY?            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   RPRDPTAB,DPLNCPP    CPP CHANGED?                                 
         BE    RFRWDP14            NO                                           
*                                                                               
*         SHOULD CHECK IF ALREADY DONE(PREV OCCURANCES)                         
*                                                                               
         BAS   RE,UPDDTL           UPDATE ALL DETAILS FOR THIS DPT              
*                                                                               
         MVC   RPRDPTAB,DPLNCPP                                                 
*                                                                               
RFRWDP14 DS    0H                                                               
         NI    RPRDPFLG,FF-RPRDPFAI                                             
         TM    DPLNFLG,RPRDPFAI                                                 
         BNO   *+8                                                              
         OI    RPRDPFLG,RPRDPFAI                                                
*                                                                               
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDPELQ                                                 
         MVC   MINEKEY+1(7),2(R6)                                               
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RFRWDP15 LA    R1,1(R1)            LOOP UNTIL ALL DAYPARTS CHANGED OR           
RFRWDP16 DS    0H                     ADDED                                     
         LR    RF,R2               NEED THIS L8R                                
         LA    R2,L'SAVDPT(R2)                                                  
         LA    R3,L'SAVDYTM(R3)                                                 
         LA    R4,L'SAVSEDT(R4)                                                 
         LA    RE,SAVDPTS+L'SAVDPTS                                             
         CR    R2,RE                                                            
         BL    RFRWDP18                                                         
         CLI   0(R6),RPRDPELQ                                                   
         BNE   RFRWDP50                                                         
         SH    RE,=Y(L'SAVDPT)                                                  
         CLC   RPRDPDPT,DPLNDPT-DPTLIN(RE)                                      
         BNE   RFRWDP20                                                         
         BAS   RE,MINIOSEQ                                                      
         BE    RFRWDP20                                                         
         B     RFRWDP50            ADD REMAINING DAYPARTS                       
*                                                                               
RFRWDP18 CLI   DPLNDPT-DPTLIN(RF),C' '                                          
         BNH   RFRWDP10            NO DAYPART = NO SEQ                          
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         BE    RFRWDP10                                                         
         B     RFRWDP50            ADD REMAINING DAYPARTS                       
*                                                                               
*********************                                                           
* DELETE - ALL UNFETCHED DAYPARTS                                               
*********************                                                           
RFRWDP20 DS    0H                  DELETE DAY PARTS BEGINGING HERE              
         CLI   RPRDPEL,RPRDPELQ    DAYPART ELEMENT??                            
         BNE   RFRWDP50            DO THE ADDS MNOW                             
         TM    RPRDPFLG,RPRDPFFT   WAS THIS DAYPART FETCHED ALREADY?            
         BZ    *+6                 NO - CONTINUE                                
         DC    H'0'                                                             
         BAS   RE,MINIODEL                                                      
         BAS   RE,MINIOSEQ                                                      
         BNE   RFRWDP50            DO THE ADDS NOW                              
         B     RFRWDP20                                                         
**********************                                                          
* ADDS                                                                          
**********************                                                          
RFRWDP50 DS    0H                                                               
         LA    RE,SAVDPTS+L'SAVDPTS                                             
         CR    R2,RE                                                            
         BNL   RFRWDPX             NOTHING LEFT TO DO                           
*                                                                               
RFRWDP55 DS    0H                  ADD REMAINING DAYPARTS                       
         OC    0(L'SAVDPT,R2),0(R2)  ANY DAYPART HERE?                          
         BZ    RFRWDP60              NO - SKIP IT                               
*                                                                               
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRDPEL,RPRDPELQ                                                 
         MVI   RPRDPLEN,RPRDPLNQ                                                
         STC   R1,RPRDPSEQ         INTERNAL ORDER NUMBER                        
*                                                                               
         MVC   RPRDPFLG,DPLNFLG       COPY FLAGS                                
         MVC   RPRDPDPT,DPLNDPT       COPY 1-BYTE DAYPART CODE                  
         MVC   RPRDPDAY,0(R3)         COPY 1-BYTE DAY CODE                      
         MVC   RPRDPSTM(L'RPRDPSTM+L'RPRDPETM),1(R3)                            
         MVC   RPRDPSDT(L'RPRDPSDT+L'RPRDPEDT),0(R4)                            
         MVC   RPRDPTAB,DPLNCPP                                                 
         TM    DPLNFLG,RPRDPFFT       WAS THIS DAYPART FETCHED ALREADY?         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,MINIOADD         ADD THE ELEMENT                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    MISCFLG1,MF1CLRPF   CLEAR PRIME FETCH FLAG                       
*                                                                               
         LA    R1,1(R1)            LOOP UNTIL ALL DAYPARTS ADDED                
RFRWDP60 LA    R2,L'SAVDPT(R2)                                                  
         LA    R3,L'SAVDYTM(R3)                                                 
         LA    R4,L'SAVSEDT(R4)                                                 
*                                                                               
         LA    R0,SAVDPTS+L'SAVDPTS   DID WE PASS THE END OF SAVDPTS?           
         CR    R2,R0                                                            
         BNL   RFRWDPX                  YES - ALL DONE                          
         B     RFRWDP55                 CONTINUE ADDING                         
         DROP  R2                                                               
*                                                                               
RFRWDPX  DS    0H                                                               
*                                                                               
***************                                                                 
* COMPETITIVE STATION ELEMENTS                                                  
*--------------                                                                 
*--------------                                                                 
* PASS 1                                                                        
***************                                                                 
RFRWCS00 DS    0H                                                               
         L     R6,MINELEM                                                       
         USING RPRSTELD,R6                                                      
         LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS+L'MINSTA-TWAD)                                     
         XC    0(L'DISCSTS,RE),0(RE)                                            
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRSTELQ    GET THE STATION ELEMENT                      
         L     R6,MINELEM                                                       
         USING RPRSTELD,R6                                                      
         BAS   RE,MINIOHI                                                       
         BE    *+6                 NEED AT LEAST ONE                            
         DC    H'0'                                                             
         CLI   RPRSTEL,RPRSTELQ                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   RPRSTICD,1                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,MINIOSEQ                                                      
         BNE   RFRWCS40            NO COMPETITIVE STATIONS                      
*                                                                               
RFRWCS10 DS    0H                                                               
         LA    R4,DISCSTS          DISPLAY STATIONS                             
         USING STALIN,R4                                                        
         CLI   RPRSTEL,RPRSTELQ    STATION ELEMENT?                             
         BNE   RFRWCS40            NO                                           
*                                                                               
RFRWCS15 DS    0H                                                               
         CLC   RPRSTSTA,STLNSTA         STATION MATCH?                          
         BE    RFRWCS30                 YES                                     
         LA    R4,L'DISCSTA(R4)                                                 
         LA    RE,DISCSTS+L'DISCSTS     END OF TABLE CHECK                      
         CR    R4,RE                                                            
         BL    RFRWCS15                                                         
*==========                                                                     
* DELETE THE STATION AND ALL ITS INVENTORY                                      
*==========                                                                     
RFRWCS20 DS    0H                                                               
         MVC   BOBYTE1,RPRSTICD    INTERNAL STATION CODE                        
         MVC   BOBYTE2,RPRSTFLG    FLAG                                         
         BAS   RE,MINIODEL         STATION ELEMENT                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    BOBYTE2,RPRSTFFT    FETCHED?                                     
         BZ    RFRWCS25            NO - DELETE COMPLETED                        
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ    GET THE DETAIL CLUSTERS                      
         MVC   MINEKEY+1(1),BOBYTE1   STATION INTERNAL CODE                     
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         BAS   RE,MINIOHI                                                       
         BNE   RFRWCS25            DELETE COMPLETED                             
*                                                                               
RFRWCS22 DS    0H                                                               
         CLI   RPRDTEL,RPRDTELQ    DETAIL CLUSTER?                              
         BNE   RFRWCS25            NO -DELETE COMPLETED                         
         CLC   RPRDTSTA,BOBYTE1    CORRECT STATION?                             
         BNE   RFRWCS25            NO DELETE COMPLETE                           
*                                                                               
         BAS   RE,MINIODEL         DETAIL CLUSTER                               
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,MINIOSEQ                                                      
         BE    RFRWCS22            NEXT DETAIL CLUSTER                          
*                                                                               
*===========                                                                    
* PROCESS NEXT STATION ELEMENT                                                  
*===========                                                                    
RFRWCS25 DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRSTELQ    GET THE STATION ELEMENT                      
         MVC   MINEKEY+1(1),BOBYTE1                                             
         BAS   RE,MINIOHI                                                       
         BE    RFRWCS10                                                         
         B     RFRWCS40                                                         
*                                                                               
*===========                                                                    
* KEEP THIS STATION ELEMENT                                                     
*===========                                                                    
RFRWCS30 DS    0H                                                               
         USING RPRSTELD,R6                                                      
         ZIC   RE,RPRSTICD                                                      
         SH    RE,=H'1'                                                         
         CH    RE,=AL2(NUMSTAS-1)                                               
         BNH   *+6                                                              
         DC    H'0'                                                             
         LR    R1,RA                                                            
         AH    R1,=Y(MINSTAS-TWAD)                                              
         MH    RE,=Y(L'MINSTA)                                                  
         AR    R1,RE                                                            
M        USING STALIN,R1                                                        
         MVC   M.STLNSTA,STLNSTA                                                
         DROP  M                                                                
         MVI   STLNSTA,X'FF'         MARK MATCHED                               
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         BE    RFRWCS10            NEXT STATION ELEMENT                         
         B     RFRWCS40                                                         
         DROP  R6,R4                                                            
************                                                                    
* PASS 2                                                                        
************                                                                    
RFRWCS40 DS    0H                                                               
         LA    R2,DISCSTS                                                       
         USING STALIN,R2                                                        
RFRWCS42 OC    0(L'DISCSTA,R2),0(R2)    DONE PROCESSING??                       
         BZ    RFRWCSX                   YES                                    
         LA    RE,DISCSTS+L'DISCSTS                                             
         CR    R2,RE                                                            
         BNL   RFRWCSX                   YES                                    
         CLI   STLNSTA,FF          STATION ALREADY DONE??                       
         BE    RFRWCS50            YES                                          
*                                                                               
         LR    R4,RA                                                            
         AH    R4,=Y(MINSTAS-TWAD)                                              
         LA    RE,L'MINSTAS(R4)                                                 
         LA    R0,1                     INTERNAL ID #                           
RFRWCS44 OC    0(L'MINSTA,R4),0(R4)    FIND INTERNAL ID                         
         BZ    RFRWCS46                                                         
         AH    R0,=H'1'                                                         
         LA    R4,L'MINSTA(R4)                                                  
         CR    R4,RE               END OF TABLE CHECK                           
         BL    RFRWCS44                                                         
         DC    H'0'                SHOULD BE A SLOT                             
*                                                                               
RFRWCS46 DS    0H                                                               
M        USING STALIN,R4                                                        
         MVC   M.STLNSTA,STLNSTA                                                
         L     R6,MINELEM                                                       
         USING RPRSTELD,R6                                                      
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRSTEL,RPRSTELQ                                                 
         MVI   RPRSTLEN,RPRSTLNQ                                                
         STC   R0,RPRSTICD                                                      
         MVC   RPRSTSTA(L'STLNSTA),STLNSTA                                      
         DROP  M,R2                                                             
*                                                                               
         BAS   RE,MINIOADD         ADD THE STATION ELEMENT                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    MISCFLG1,MF1CLRPF   CLEAR PRIME FETCH FLAG                       
*                                                                               
RFRWCS50 DS    0H                                                               
         LA    R2,L'DISCSTA(R2)                                                 
         B     RFRWCS42                                                         
*                                                                               
RFRWCSX  DS    0H                                                               
***************                                                                 
* DO WE NEED TO CLEAR PRIME BOOK FETCH FLAGS?                                   
***************                                                                 
RFRWCP00 TM    MISCFLG1,MF1CLRPF   DO WE NEED TO?                               
         BZ    RFRWCPX             NO                                           
*                                                                               
         XC    MINEKEY,MINEKEY     YES, GO THROUGH ALL THE BOOKS                
         MVI   MINEKEY,RPRBKELQ       AND CLEAR OUT THAT BIT                    
         BAS   RE,MINIOHI                                                       
         BNE   RFRWCPX                                                          
*                                                                               
RFRWCP10 L     R6,MINELEM                                                       
         CLI   0(R6),RPRBKELQ                                                   
         BNE   RFRWCPX                                                          
*                                                                               
         USING RPRBKELD,R6                                                      
         NI    RPRBKFLG,X'FF'-RPRBKPFT                                          
*                                                                               
         BAS   RE,MINIOWRT                                                      
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         BE    RFRWCP10                                                         
*                                                                               
RFRWCPX  DS    0H                                                               
***************                                                                 
** FINISH UP **                                                                 
***************                                                                 
RFRWRTX  LR    RE,RA                                                            
         AH    RE,=Y(MINDPTS-TWAD)                                              
         MVC   SAVDPTS,0(RE)                                                    
         LR    RE,RA                                                            
         AH    RE,=Y(MINDYTMS-TWAD)                                             
         MVC   SAVDYTMS,0(RE)                                                   
         LR    RE,RA                                                            
         AH    RE,=Y(MINSEDTS-TWAD)                                             
         MVC   SAVSEDTS,0(RE)                                                   
         LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS+L'MINSTA-TWAD)                                     
         MVC   DISCSTS,0(RE)                                                    
*                                                                               
         B     EXITOK                                                           
         DROP  R5,R6                                                            
         SPACE 2                                                                
***********************************************************************         
* UPDATE DETAIL ELEMENTS FOR THIS DPT                                           
*                                                                               
*        R6 - DETAIL ELEMENT                                                    
*        R2 - THE MATCHING LINE IN SAVDPTS                                      
*                                                                               
***********************************************************************         
UPDDTL   NTR1                                                                   
         MVC   BOWORK1(RPRDPLNQ),0(R6)          ELEMENT FOR RESTORE             
         USING DPTLIN,R2                                                        
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         LA    R4,1                START WITH STATION 1                         
         SR    R3,R3                                                            
*                                                                               
UPDDTL3  XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ                                                 
         STC   R4,MINEKEY+1                                                     
         MVC   MINEKEY+2(1),DPLNDPT                                             
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   UPDDTLX                                                          
         L     R6,MINELEM                                                       
*                                                                               
UPDDTL5  DS    0H                                                               
         LA    R3,1(R3)                                                         
         CLI   0(R6),RPRDTELQ      DETAIL ELEMENT?                              
         BNE   UPDDTLX             NO                                           
         USING RPRDTELD,R6                                                      
         CLM   R4,1,RPRDTSTA                                                    
         BNE   UPDDTL10            GONE PAST IT                                 
         CLC   RPRDTDPT,DPLNDPT                                                 
         BL    UPDDTL5             HAVN'T FOUND IT YET                          
         BH    UPDDTL10            GONE PAST IT                                 
*                                                                               
         MVC   RPRDTTAB,DPLNCPP    NEW TAB CPP                                  
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ                                                 
         MVC   MINEKEY+1(7),2(R6)                                               
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,MINIOSEQ                                                      
         BNE   UPDDTLX                                                          
         B     UPDDTL5                                                          
*                                                                               
UPDDTL10 DS    0H                  NEXT STATION CODE                            
         LA    R4,1(R4)                                                         
         B     UPDDTL3                                                          
         DROP  R6                                                               
*                                                                               
UPDDTLX  DS    0H                  RESTORE SEQ                                  
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDPELQ                                                 
         MVC   MINEKEY+1(7),BOWORK1+2                                           
         BAS   RE,MINIORD                                                       
         BE    *+6                 HAD BETTER BE THERE                          
         DC    H'0'                                                             
*                                                                               
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* MARK ALL ELEMENTS AS UNFETCHED                                                
***********************************************************************         
UNFETCH  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ                                                 
         L     R6,MINELEM                                                       
         BAS   RE,MINIOHI                                                       
         BNE   UFTCH22                                                          
UFTCH02  DS    0H                                                               
         CLI   0(R6),RPRDTELQ      UPTO DETAILS?                                
         BNL   UFTCH22             YES BREAK OUT                                
*                                                                               
         CLI   0(R6),RPRBKELQ      BOOK ELEMENT?                                
         BNE   *+12                NO                                           
         NI    RPRBKFLG-RPRBKELD(R6),FF-RPRBKFFT                                
         B     UFTCH20                                                          
*                                                                               
         CLI   0(R6),RPRDMELQ      DEMO ELEMENT?                                
         BNE   *+12                NO                                           
         NI    RPRDMFLG-RPRDMELD(R6),FF-RPRDMFFT                                
         B     UFTCH20                                                          
*                                                                               
         CLI   0(R6),RPRSTELQ      STATION ELEMENT?                             
         BNE   *+12                NO                                           
         NI    RPRSTFLG-RPRSTELD(R6),FF-RPRSTFFT                                
         B     UFTCH20                                                          
*                                                                               
         CLI   0(R6),RPRDPELQ      DAYPART ELEMENT?                             
         BNE   *+12                NO                                           
         NI    RPRDPFLG-RPRDPELD(R6),FF-RPRDPFFT                                
         B     UFTCH20                                                          
*                                                                               
         CLI   0(R6),RPRCHELQ      COST HEADER ELEMENT?                         
         BNE   *+12                NO                                           
         NI    RPRCHFLG-RPRCHELD(R6),FF-RPRCHFTC                                
         B     UFTCH20                                                          
*                                                                               
UFTCH20  DS    0H                                                               
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(R6)                                                 
         MVC   MINEKEY+1(7),2(R6)                                               
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,MINIOSEQ                                                      
         BE    UFTCH02                                                          
*                                                                               
UFTCH22  DS    0H                                                               
         LA    RE,SAVDPTS                                                       
         LA    RF,SAVDPTS+L'SAVDPTS                                             
         USING DPTLIN,RE                                                        
UFTCH24  DS    0H                                                               
         OC    0(L'SAVDPT,RE),0(RE)                                             
         BZ    *+8                                                              
         NI    DPLNFLG,FF-RPRDPFFT                                              
         LA    RE,DPLNLENQ(RE)                                                  
         CR    RE,RF                                                            
         BL    UFTCH24                                                          
         DROP  RE                                                               
*                                                                               
UNFETCHX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROPOSAL NUMBER FIELD                                                
***********************************************************************         
         DS    0H                                                               
VALPRONM NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'VALPRONM'                                                    
*                                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         CLI   CSACT,A#ADD         ARE WE ADDING?                               
         BNE   VALPRO50            NO                                           
********************                                                            
** ADD VALIDATION **                                                            
********************                                                            
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHID+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
*                                                                               
         CLC   IOKEY(RPROKPRO-RPROKEY),IOKEYSAV   SAME UPTO PROPOSAL #?         
         BE    VALPRO10                           YES                           
         LA    R1,X'FF'                                                         
         B     VALPRO15                                                         
*                                                                               
VALPRO10 ZIC   R1,RPROKPRO                                                      
         CLI   RPROKPRO,0                                                       
         BE    EXITNMOR            NO MORE PROPOSAL FOR THIS CONTRACT           
*                                                                               
VALPRO15 BCTR  R1,0                                                             
         STC   R1,BPRONUM                                                       
*                                                                               
         STC   R1,BOBYTE1                                                       
         XI    BOBYTE1,X'FF'                                                    
*                                                                               
         CLI   FVILEN,0            USER INPUT?                                  
         BE    VALPRO20            NO - OUTPUT THE NEXT AVAIL. #                
*                                                                               
         CLC   BOBYTE1,BCFULL+3    IS THE INPUT < THE NEXT PRO #                
         BH    EXITRCAE            YES                                          
*                                                                               
         CLC   BOBYTE1,BCFULL+3    IS THE INPUT THE NEXT PRO #                  
         BNE   EXITNV              NO                                           
         B     VALPRO49                                                         
*                                                                               
VALPRO20 EDIT  (B1,BOBYTE1),(3,FVIFLD),WRK=BOWORK1,DUB=BODUB1,         X        
               ALIGN=LEFT                                                       
         STC   R0,FVILEN                                                        
         SH    R0,=H'1'                                                         
         STC   R0,FVXLEN                                                        
         OI    FVIIND,FVINUM                                                    
         DROP  R2                                                               
*                                                                               
VALPRO49 LA    R1,SVPARMS          RESET BECAUSE WE USED R1                     
         OI    FVIIND,FVIVAL       VALIDATED                                    
         MVC   BPRONUM,BOBYTE1                                                  
         XI    BPRONUM,X'FF'                                                    
*                                                                               
         LR    RE,RA               CLEAR SAVED PROPOSAL #                       
         AH    RE,=Y(SVPRONUM-TWAD)                                             
         XC    0(L'SVPRONUM,RE),0(RE)                                           
         B     VALPROX                                                          
*                                                                               
************************                                                        
** NON ADD VALIDATION **                                                        
************************                                                        
VALPRO50 CLI   FVILEN,0            ANY DATA IN THIS FIELD?                      
         BE    EXITNO              NO, NEED A NUMBER                            
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   VALPRO52            NO                                           
*                                                                               
         CLI   CSACT,A#DIS                                                      
         BE    VALPRO52                                                         
         CLI   CSACT,A#SEL                                                      
         BE    VALPRO52                                                         
         B     EXITSLCK            SECURITY LOCKOUT                             
*                                                                               
VALPRO52 GOTOX (VALPROQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*                                                                               
         OI    FVIIND,FVIVAL       VALIDATED                                    
*                                                                               
VALPROX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE BOOKS FIELD                                                       
***********************************************************************         
         DS    0H                                                               
DISBKSB  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'DISBOOKS'                                                    
*                                                                               
         CLI   CSACT,A#ADD         ARE WE ADDING?                               
         BNE   DISBKS10                                                         
         TM    MISCFLG1,MF1KYCHG   KEY CHANGED?                                 
         BZ    DISBKS10            NO                                           
***************                                                                 
* FOR ACTION ADD WHEN THE KEY CHANGED                                           
***************                                                                 
         MVC   AFVADDR,FVADDR      SAVE THIS ADDRESS FOR DFDVAL                 
*                                                                               
         OC    CSARBKS,CSARBKS     ANY BOOKS FROM THE CONTRACT SAR?             
         BZ    DISBKSX             NONE                                         
*                                                                               
DISBKA10 DS    0H                                                               
         LA    R2,FVIFLD                                                        
         LA    R3,CSARBKS                                                       
*                                                                               
DISBKA20 CLI   0(R3),0                USER DEFINED BOOK?                        
         BNE   DISBKA30               YES                                       
*                                                                               
         OC    0(5,R3),0(R3)          ANY BOOK?                                 
         BZ    DISBKA60               NO MORE BOOKS                             
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(L'FVIHDR),FVIHDR                                         
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(2),=X'0B07'      PUT OUT SOURCE                           
         MVC   BOELEM+2(1),1(R3)                                                
         GOTOX (UNBOOKQ,AREPRO01),BODMCB,(1,2(R3)),BOWORK1,            X        
               (C'L',BOELEM),(C'+',=CL6' ')                                     
*                                                                               
         ZIC   RE,BOWORK1                                                       
         LA    RE,BOWORK1(RE)                                                   
         TM    BOWORK1+1,X'02'       EXT FIELD HDR?                             
         BNE   *+8                                                              
         SH    RE,=H'8'                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         CLI   0(RE),C')'                                                       
         BNE   DISBKA28                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     DISBKA28                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
DISBKA28 LA    RF,BOWORK1+8                                                     
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),BOWORK1+8                                                
         LA    R2,1(RE,R2)                                                      
         B     DISBKA50                                                         
*                                                                               
DISBKA30 MVC   0(5,R2),0(R3)         SHOW THE USER-DEFINED LABEL                
*                                                                               
         LA    R2,5-1(R2)            REMOVE AS MUCH SPACES AS WE CAN            
DISBKA33 CLI   0(R2),C' '                                                       
         BH    DISBKA36                                                         
         BCTR  R2,0                                                             
         B     DISBKA33                                                         
DISBKA36 LA    R2,1(R2)                                                         
         B     DISBKA50                                                         
*                                                                               
DISBKA50 MVI   0(R2),C','                                                       
*                                                                               
         LA    R2,1(R2)            BUMP TO THE NEXT BOOK                        
         LA    R3,5(R3)                                                         
         LA    RE,CSARBKS+L'CSARBKS                                             
         CR    R3,RE                                                            
         BL    DISBKA20                                                         
*                                                                               
DISBKA60 BCTR  R2,0                REMOVE THE LAST COMMA                        
         MVI   0(R2),C' '                                                       
         B     DISBKSX                                                          
*                                                                               
***************                                                                 
* FOR OTHER ACTIONS AND ADD WHEN THE KEY DIDN'T CHANGE                          
***************                                                                 
DISBKS10 DS    0H                                                               
         LA    R2,FVIFLD                                                        
         OI    FVOIND,FVOCUR       POINT CURSOR HERE                            
         LA    R3,DISBOOKS                                                      
         USING BOOKLIN,R3                                                       
         LA    R4,DISLBLS                                                       
*                                                                               
DISBKS20 OC    0(L'DISLBL,R4),0(R4)   USER DEFINED BOOK?                        
         BNZ   DISBKS30               YES                                       
*                                                                               
         OC    0(L'DISBOOK,R3),0(R3)  ANY BOOK?                                 
         BZ    DISBKS60               NO MORE BOOKS                             
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(L'FVIHDR),FVIHDR                                         
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(2),=X'0B07'      PUT OUT SOURCE                           
         MVC   BOELEM+2(1),BKLNSPBK                                             
         GOTOX (UNBOOKQ,AREPRO01),BODMCB,(1,BKLNBK),BOWORK1,           X        
               (C'L',BOELEM),(C'+',=CL6' ')                                     
*                                                                               
         ZIC   RE,BOWORK1                                                       
         LA    RE,BOWORK1(RE)                                                   
         TM    BOWORK1+1,X'02'       EXT FIELD HDR?                             
         BNE   *+8                                                              
         SH    RE,=H'8'                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         CLI   0(RE),C')'                                                       
         BNE   DISBKS28                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     DISBKS28                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
DISBKS28 LA    RF,BOWORK1+8                                                     
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),BOWORK1+8                                                
         LA    R2,1(RE,R2)                                                      
         B     DISBKS50                                                         
*                                                                               
DISBKS30 MVC   0(L'DISLBL,R2),0(R4)   SHOW THE USER-DEFINED LABEL               
*                                                                               
         LA    R2,L'DISLBL-1(R2)      REMOVE AS MUCH SPACES AS WE CAN           
DISBKS33 CLI   0(R2),C' '                                                       
         BH    DISBKS36                                                         
         BCTR  R2,0                                                             
         B     DISBKS33                                                         
DISBKS36 LA    R2,1(R2)                                                         
         B     DISBKS50                                                         
*                                                                               
DISBKS50 MVI   0(R2),C','                                                       
*                                                                               
         LA    R2,1(R2)            BUMP TO THE NEXT BOOK                        
         LA    R3,L'DISBOOK(R3)                                                 
         LA    R4,L'DISLBL(R4)                                                  
         LA    RE,DISBOOKS+L'DISBOOKS                                           
         CR    R3,RE                                                            
         BL    DISBKS20                                                         
*                                                                               
DISBKS60 BCTR  R2,0                REMOVE THE LAST COMMA                        
         MVI   0(R2),C' '                                                       
*                                                                               
DISBKSX  B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE BOOKS FIELD                                                      
***********************************************************************         
         DS    0H                                                               
VALBOOKS NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'VALBOOKS'                                                    
*                                                                               
         MVI   BOBYTE2,0           # OF USER-DEFINED BOOKS                      
         MVI   BKCOUNT,0                                                        
*                                                                               
         L     R0,AIO4             CLEAR FOR PARSNIP                            
         LA    R1,IOAREALN                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 VPARSNIP,BODMCB,(FVILEN,FVIFLD),(16,AIO4),('PSNAOKQ',0)          
         CLI   8(R1),0                                                          
         BE    VALBKS05                                                         
         L     R1,8(R1)                                                         
         LA    R1,0(R1)                                                         
         LA    R0,FVIFLD                                                        
         SR    R1,R0                                                            
         STC   R1,FVERRNDX         INDEX OF WHERE ERROR IS                      
         B     EXITIBKE                                                         
*                                                                               
VALBKS05 CLI   4(R1),1                                                          
         BL    EXITIBKE                                                         
*                                                                               
         LA    R2,DISLBLS          R2 = A(1ST LABEL)                            
         LA    R6,DISBOOKS         R6 = A(1ST 3-BYTE BK, 8-BYTE UPGRD)          
         USING BOOKLIN,R6                                                       
         L     R3,AIO4             R3 = A(1ST PARSNIP FIELD)                    
         USING PSND,R3                                                          
*                                                                               
VALBKS10 CLI   PSNTAG,0            ANY MORE FIELDS?                             
         BE    VALBKS40            NO                                           
*                                                                               
         OC    PSNCOMP,PSNCOMP                                                  
         BZ    EXITIBKE                                                         
*                                                                               
         L     RE,PSNCOMP          CALCULATE WHERE TO POINT CURSOR              
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         STC   RE,FVERRNDX                                                      
*                                                                               
         CLI   PSNERR,0            COMPONENT IS IN ERROR?                       
         BNE   EXITIBKE                                                         
*                                                                               
         CLI   PSNTAG,C'F'         REGUALR FIELD COMPONENT                      
         BNE   EXITIBKE                                                         
*                                                                               
         TM    PSNSTAT,PSNNUMQ     FIELD SHOULD NOT BE NUMERIC                  
         BNZ   EXITIBKE                                                         
*                                                                               
         CLI   PSNLEN,0            MISSING FIELD                                
         BE    EXITIBKE                                                         
*                                                                               
         XC    BOWORK2,BOWORK2                                                  
         XC    BODUB1,BODUB1                                                    
         L     RE,PSNCOMP                                                       
         ZIC   R1,PSNLEN                                                        
         STC   R1,BOWORK2+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOWORK2+8(0),0(RE)                                               
         LA    RF,BOWORK2+9(R1)                                                 
*                                                                               
         OC    PSNATTR,PSNATTR     ETHINIC BOOK?                                
         BZ    VALBKS12            NO                                           
*                                                                               
         LA    R3,PSNL(R3)                                                      
         OC    PSNATTR,PSNATTR     ANOTHER ETHINIC BOOK?                        
         BNZ   EXITIBKE            YES                                          
*                                                                               
         CLI   PSNLEN,1                                                         
         BNE   EXITIBKE                                                         
*                                                                               
         L     RE,PSNCOMP                                                       
         BCTR  RE,0                WANT THE '(' & ')'                           
         MVC   0(3,RF),0(RE)                                                    
         ZIC   RE,BOWORK2+5                                                     
         LA    RE,3(RE)                                                         
         STC   RE,BOWORK2+5                                                     
*                                                                               
VALBKS12 GOTO1 VBOOKVAL,BODMCB,(C'N',BOWORK2),(1,BOWORK1),             X        
               (C'B',VSCANNER),BODUB1                                           
*                                                                               
         ZIC   RE,BKCOUNT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,BKCOUNT                                                       
*                                                                               
         CLI   4(R1),0             GOOD BOOK?                                   
         BNE   VALBKS20            YES                                          
*                                                                               
         CLI   PSNTAG,C'F'         REGUALR FIELD COMPONENT                      
         BNE   EXITIBKE                                                         
*                                                                               
         CLI   PSNLEN,3            MAKE SURE USER DEFINED BTWN 3-5 CHR          
         BL    EXITIBKE                                                         
         CLI   PSNLEN,L'DISLBL                                                  
         BH    EXITIBKE                                                         
*                                                                               
         ZIC   R1,BOBYTE2                                                       
         AH    R1,=H'1'                                                         
         STC   R1,BOBYTE2                                                       
*                                                                               
         CLI   BOBYTE2,2                                                        
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(MXOF2UDB)                                           
         B     EXITL               MAX OF 2 USER-DEFINED BOOKS                  
*                                                                               
         L     RE,PSNCOMP          SAVE THE LABEL                               
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)                                                    
         OC    0(L'DISLBL,R2),=CL6' '                                           
*                                                                               
         LA    RE,DISLBLS                                                       
         CR    R2,RE                                                            
         BE    VALBKS30                                                         
VALBKS15 CLC   0(L'DISLBL,RE),0(R2)                                             
         BE    EXITDUPL                            DUPLICATE FOUND              
         LA    RE,L'DISLBL(RE)                                                  
         CR    RE,R2                                                            
         BL    VALBKS15                                                         
         B     VALBKS30                                                         
*                                                                               
VALBKS20 DS    0H                                                               
         LA    RE,DISBOOKS                         CHECK FOR REPEATS            
VALBKS22 CLC   BKLNBK-BOOKLIN(L'BKLNBK,RE),BOWORK1                              
         BNE   *+14                                                             
         CLC   BKLNSPBK-BOOKLIN(L'BKLNSPBK,RE),BODUB1                           
         BE    EXITDUPL                            DUPLICATE FOUND              
         LA    RE,L'DISBOOK(RE)                                                 
         CR    RE,R6                                                            
         BL    VALBKS22                                                         
*                                                                               
         MVC   BKLNBK,BOWORK1     SAVE BOOK DEFN                                
         MVI   BKLNFIL,RPRBKINQ                                                 
*                                                                               
         TM    BKLNBK,RPRBKSES+RPRBKSPJ+RPRBKST2+RPRBKSTP                       
         BNZ   *+14                FOR NON E/P/S/T                              
         CLC   BKLNBK+1(2),TODAYBIN                                             
         BH    EXITIBKE                                                         
*                                                                               
         MVC   BKLNFIL,SAVBKTYP                                                 
         CLI   BODUB1,C' '                                                      
         BNH   VALBKS30            NO SPECIAL BOOK TYPE                         
         MVC   BKLNSPBK,BODUB1                                                  
*                                                                               
VALBKS30 DS    0H                                                               
         ZIC   RE,FVERRNDX         CURSOR KLUGE                                 
         ZIC   RF,PSNLEN                                                        
         LA    RE,1(RF,RE)                                                      
         STC   RE,FVERRNDX                                                      
*                                                                               
         LA    R3,PSNL(R3)         BUMP TO THE NEXT FIELD                       
         LA    R2,L'DISLBL(R2)                                                  
         LA    R6,L'DISBOOK(R6)                                                 
         B     VALBKS10                                                         
         DROP  R3,R6                                                            
*                                                                               
VALBKS40 MVI   FVERRNDX,0          NO MORE INDEX INTO FIELD NEEDED              
*                                                                               
VALBKSX  DS    0H                                                               
         CLI   BKCOUNT,NUMBKS                                                   
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(TOOMNYBK)   TOO MANY BOOKS                          
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY PROJECTION FIELD                                                      
***********************************************************************         
         DS    0H                                                               
DISPROJS NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'DISPROJS'                                                    
*                                                                               
         ZIC   R1,SVPRJFLD                                                      
         LA    R0,1(R1)                                                         
         STC   R0,SVPRJFLD                                                      
         XC    FVIFLD,FVIFLD                                                    
*                                                                               
         SR    RF,RF               LABEL MATCHED COUNT                          
         LA    R3,DISLBLS                                                       
DISPRJ5  OC    0(L'DISLBL,R3),0(R3)                                             
         BZ    DISPRJ10                                                         
         CR    R1,RF               IS THIS THE LABEL WE WANT?                   
         BE    DISPRJ15            YES                                          
         LA    RF,1(RF)                                                         
*                                                                               
DISPRJ10 LA    R3,L'DISLBL(R3)                                                  
         LA    RE,DISLBLS+L'DISLBLS                                             
         CR    R3,RE                                                            
         BNL   DISPRJX             NOTHING LEFT                                 
         B     DISPRJ5                                                          
*                                                                               
DISPRJ15 DS    0H                                                               
         LA    RF,DISUPGDS+L'DISUPGDS                                           
         LA    R4,DISUPGDS                                                      
         CLC   0(L'DISLBL,R3),0(R4)                                             
         BE    *+16                                                             
         LA    R4,L'DISUPGD(R4)                                                 
         CR    R4,RF                                                            
         BL    *-16                                                             
         DC    H'0'                                                             
*                                                                               
         LA    R2,FVIFLD                                                        
         MVC   0(L'DISLBL,R2),0(R4)                                             
         LA    R2,L'DISLBL-1(R2)                                                
         BAS   RE,SHORTEN                                                       
*                                                                               
         MVI   0(R2),C'='          LABEL=EXPRESSION                             
         LA    R2,1(R2)                                                         
*                                                                               
         LA    R4,L'DISLBL(R4)     PARSE OUT 'UPT='                             
         CLC   =C'UPT=',0(R4)                                                   
         BNE   DISPRJ20                                                         
         LA    R4,4(R4)                                                         
*                                                                               
DISPRJ20 MVC   0(L'DISUPGD-L'DISLBL,R2),0(R4)                                   
         LA    R2,L'DISUPGD-1-L'DISLBL(R2)                                      
         BAS   RE,SHORTEN                                                       
         B     DISPRJX                                                          
*                                                                               
SHORTEN  CLI   0(R2),C' '          GETS RID OF THE SPACE PADDING                
         BH    *+10                                                             
         BCTR  R2,0                                                             
         B     SHORTEN                                                          
         LA    R2,1(R2)                                                         
         BR    RE                                                               
*                                                                               
DISPRJX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROJECTION FIELD                                                     
***********************************************************************         
         DS    0H                                                               
VALPROJS NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'VALPROJS'                                                    
*                                                                               
         ZIC   R1,SVPRJFLD                                                      
         LA    R0,1(R1)                                                         
         STC   R0,SVPRJFLD                                                      
*                                                                               
         LTR   R1,R1                    FIRST FIELD?                            
         BNE   *+14                                                             
         NI    MISCFLG1,FF-MF1TMPBT     NO PROJECTIONS YET                      
         MVC   AFVADDR,FVADDR                                                   
*********                                                                       
* COUNT LABELS                                                                  
*********                                                                       
         LA    R3,DISLBLS                                                       
         SR    RE,RE                                                            
*                                                                               
VALPRJ05 OC    0(L'DISLBL,R3),0(R3)   ANY LABEL?                                
         BZ    *+8                                                              
         LA    RE,1(RE)                                                         
         LA    R3,L'DISLBL(R3)                                                  
         LA    RF,DISLBLS+L'DISLBLS                                             
         CR    R3,RF                                                            
         BL    VALPRJ05                                                         
*********                                                                       
* TRY SOME ERROR CHECKING                                                       
*********                                                                       
         LTR   RE,RE               ZERO LABELS?                                 
         BNZ   VALPRJ10            NO                                           
*                                                                               
         CLI   FVILEN,0            ANY PROJECTIONS                              
         BNE   EXITNV              SHOULDN'T BE                                 
         B     VALPRJX                                                          
*                                                                               
VALPRJ10 CH    RE,=H'1'            ONE LABEL?                                   
         BNE   VALPRJ16            NO                                           
*                                                                               
         LTR   R1,R1               FIRST FIELD?                                 
         BNE   VALPRJ12            NO                                           
*                                                                               
         CLI   FVILEN,0            PROJECTION?                                  
         BE    VALPRJX             NO                                           
         OI    MISCFLG1,MF1TMPBT                                                
         B     VALPRJ20            PROCESS THE PROJECTION                       
*                                                                               
VALPRJ12 TM    MISCFLG1,MF1TMPBT   FIRST PROJECTION USED?                       
         BZ    VALPRJ14            NO                                           
*                                                                               
         CLI   FVILEN,0            PROJECTION?                                  
         BE    VALPRJX             NO                                           
         B     EXITNV                                                           
*                                                                               
VALPRJ14 CLI   FVILEN,0            PROJECTION?                                  
         BE    VALPRJ90            NO - NEEDED IT                               
         B     VALPRJ20            PROCESS THE PROJECTION                       
*                                                                               
VALPRJ16 CH    RE,=H'2'            TWO LABELS?                                  
         BE    *+6                 NOT GOOD                                     
         DC    H'0'                                                             
*                                                                               
         LTR   R1,R1               FIRST FIELD?                                 
         BNE   VALPRO18            NO                                           
*                                                                               
         CLI   FVILEN,0            PROJECTION?                                  
         BE    VALPRJ90            NO - NEEDED IT                               
         OI    MISCFLG1,MF1TMPBT                                                
         B     VALPRJ20            PROCESS THE PROJECTION                       
*                                                                               
VALPRO18 CLI   FVILEN,0            PROJECTION?                                  
         BE    VALPRJ90            NO - NEEDED IT                               
         B     VALPRJ20            PROCESS THE PROJECTION                       
         SPACE 2                                                                
*********                                                                       
* PROCESS PROJECTIONS                                                           
*********                                                                       
VALPRJ20 L     RE,AIO4             CLEAR AIO4 FOR USE WITH PARSNIP              
         LA    RF,480                                                           
         XCEFL                                                                  
*                                                                               
         L     R3,AIO4                                                          
         USING PSND,R3                                                          
         GOTO1 VPARSNIP,BODMCB,(FVILEN,FVIFLD),(0,AIO4),('PSNNPARQ',0)          
         CLI   8(R1),0             SHOULD BE 'LABEL=....'                       
         BNE   EXITNV                                                           
*                                                                               
         CLI   PSNLEN,0                                                         
         BE    EXITNV                                                           
         CLI   PSNLEN,L'DISLBL                                                  
         BH    EXITNV                                                           
         XC    BOWORK1,BOWORK1                                                  
         L     RE,PSNCOMP                                                       
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOWORK1(0),0(RE)                                                 
         OC    BOWORK1(L'DISLBL),BCSPACES                                       
*                                                                               
         LA    RE,DISLBLS                                                       
         LA    R0,DISLBLS+L'DISLBLS                                             
         LA    RF,1                                                             
VALPRJ25 CLC   0(L'DISLBL,RE),BOWORK1                                           
         BE    VALPRJ30                                                         
         LA    RF,1(RF)                                                         
         LA    RE,L'DISLBL(RE)                                                  
         CR    RE,R0                                                            
         BL    VALPRJ25                                                         
         MVC   FVMSGNO,=AL2(LBLINBKF)                                           
         B     EXITL               LABEL SHOULD BE ENTERED IN BOOKS FLD         
*                                                                               
VALPRJ30 DS    0H                   CHECK TO BE SURE NOT ALREADY USED           
         LA    R0,DISUPGDS+L'DISUPGDS                                           
         LA    RE,DISUPGDS                                                      
         CLC   BOWORK1(L'DISLBL),0(RE)                                          
         BE    EXITNV                                                           
         LA    RE,L'DISUPGD(RE)                                                 
         CR    RE,R0                                                            
         BL    *-16                                                             
*                                                                               
********                                                                        
* VALID USER BOOK NAME                                                          
********                                                                        
         ZIC   RE,WHICHUPE                                                      
         BCTR  RE,0                                                             
         MH    RE,=Y(L'DISUPGD)                                                 
         LA    RE,DISUPGDS(RE)                                                  
*                                                                               
         MVC   0(L'DISLBL,RE),BOWORK1                                           
         STC   RF,BOOKNUM          NUMBER OF BOOK WE'RE LOOKING AT              
*                                                                               
         ZIC   RE,PSNLEN                                                        
         LA    RE,1(RE)                                                         
         DROP  R3                                                               
         ZIC   RF,FVILEN                                                        
         SR    RF,RE               EXPRESSION LENGTH                            
         LA    R3,FVIFLD(RE)       A(EXPRESSION)                                
*                                                                               
         XC    BOWORK2,BOWORK2     CREATE UPT= EXPRESSION                       
         MVC   BOWORK2+8(4),=C'UPT='                                            
         LR    RE,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BOWORK2+12(0),0(R3)                                              
         LA    RE,4(RE)                                                         
         STC   RE,BOWORK2+5        INPUT LENGTH                                 
         LA    RE,8(RE)                                                         
         STC   RE,BOWORK2          FIELD LENGTH                                 
*                                                                               
         GOTOX (VALUPGQ,AREPRO01),BODMCB,(X'D8',BOWORK2)                        
         BL    EXITL                                                            
*                                                                               
         ZIC   R1,BOOKNUM                                                       
         BCTR  R1,0                                                             
         MH    R1,=Y(L'DISBOOK)                                                 
         LA    R1,DISBOOKS(R1)                                                  
         USING BOOKLIN,R1                                                       
*                                                                               
         OC    BOWORK1+13(3),BOWORK1+13   NEED AN OVERRIDE BOOK FOR NOW         
         BZ    EXITIOBK                                                         
*                                                                               
         MVC   BKLNBK,BOWORK1+13   SAVE OVR BOOK AND                            
         MVC   BKLNUPGD,BOWORK1+1  UPG                                          
         MVC   BKLNSPBK,BOWORK1+29                                              
         MVC   BKLNXBKS,BOWORK1+23                                              
         DROP  R1                                                               
*                                                                               
         ZIC   RE,WHICHUPE                                                      
         BCTR  RE,0                                                             
         MH    RE,=Y(L'DISUPGD)                                                 
         LA    RE,DISUPGDS(RE)                                                  
*                                                                               
         MVC   L'DISLBL(L'DISUPGD-L'DISLBL,RE),BOWORK2+12                       
         B     VALPRJX                                                          
*                                                                               
***************                                                                 
* MISSING A PROJECTION                                                          
***************                                                                 
VALPRJ90 DS    0H                                                               
         MVI   FVERRNDX,0                                                       
*                                                                               
         LA    RE,DISBOOKS                                                      
         LA    RF,DISLBLS                                                       
         LA    R1,7                                                             
*                                                                               
VALPRJ93 OC    0(L'DISLBL,RF),0(RF)   ANY USER-DEFINED BOOK HERE?               
         BZ    VALPRJ96               NO, SKIP TO THE NEXT ONE                  
*                                                                               
         OC    0(L'DISBOOK,RE),0(RE)  YES, DO WE HAVE A PROJ FOR IT?            
         BNZ   VALPRJ96                                                         
         MVC   FVADDR,AFVADDR                                                   
         MVC   FVMSGNO,=AL2(MSUP4UDB)      NO, MISSING UPGRADE EXPR FOR         
         MVC   FVXTRA(L'DISLBL),0(RF)          THIS BOOK                        
         B     EXITL                                                            
*                                                                               
VALPRJ96 LA    RE,L'DISBOOK(RE)                                                 
         LA    RF,L'DISLBL(RF)                                                  
         BCT   R1,VALPRJ93         LOOP UNTIL WE'RE DONE CHECKING               
*                                                                               
VALPRJX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* AFTER VALIDATING THE KEY FIELDS                                               
***********************************************************************         
KLSTKVAL NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**KLKV**'                                                    
*&&DO                                                                           
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
*&&                                                                             
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
         DROP  R2                                                               
*                                                                               
         NI    GSINDSL1,FF-GSIDISCL    YES, WANT RDIS 1ST BEFORE RVAL           
         CLI   CSACT,A#ADD         ARE WE ADDING A RECORD?                      
         BNE   KLKV10                                                           
         TM    MISCFLG1,MF1KYCHG   YES, DID OUR KEY CHANGE?                     
         BZ    KLKV10                  NO, DON'T WANT RDIS AGAIN                
         OI    GSINDSL1,GSIDISCL       YES, WANT RDIS 1ST BEFORE RVAL           
         B     KLKVX                                                            
***************                                                                 
* USE GLOBBER SO WE CAN GET TO THE RIS PROGRAM (PETER)                          
***************                                                                 
KLKV10   CLI   BCPFKEY,PFKYRIS                                                  
         BNE   KLKVX                                                            
*                                                                               
         XC    BOELEM,BOELEM                                                    
         LA    R1,BOELEM                                                        
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'                                                 
         MVC   GLVXFRPR,=C'SEL'                                                 
         MVC   GLVXTOSY,=C'REP'                                                 
         MVC   GLVXTOPR,=C'RIS'                                                 
         OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         DROP  R1                                                               
         L     R6,ACOM                                                          
         USING COMFACSD,R6                                                      
         GOTO1 CGLOBBER,BODMCB,=C'PUTD',BOELEM,14,GLVXCTL                       
         DROP  R6                                                               
*                                                                               
         XC    BOELEM,BOELEM                                                    
         LA    RE,BOELEM                                                        
***************                                                                 
* STATION                                                                       
***************                                                                 
         MVC   0(L'CCONKSTA,RE),CCONKSTA                                        
         LA    RE,L'CCONKSTA(RE)                                                
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNH   *-6                                                              
         MVI   1(RE),C','                                                       
         LA    RE,2(RE)                                                         
***************                                                                 
* OFFICE                                                                        
***************                                                                 
         MVC   0(L'CCONKOFF,RE),CCONKOFF                                        
         MVI   L'CCONKOFF(RE),C','                                              
         LA    RE,L'CCONKOFF+1(RE)                                              
***************                                                                 
* AGENCY & AGENCY OFFICE                                                        
***************                                                                 
         MVC   0(L'CCONKAGY,RE),CCONKAGY                                        
         LA    RE,L'CCONKAGY(RE)                                                
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNH   *-6                                                              
         CLC   CCONKAOF,BCSPACES                                                
         BH    *+12                                                             
         LA    RE,1(RE)                                                         
         B     KLKV15                                                           
         MVI   1(RE),C'-'                                                       
         MVC   2(L'CCONKAOF,RE),CCONKAOF                                        
         LA    RE,L'CCONKAOF+2(RE)                                              
*                                                                               
KLKV15   MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
***************                                                                 
* ADVERTISER                                                                    
***************                                                                 
         MVC   0(L'CCONKADV,RE),CCONKADV                                        
         LA    RE,L'CCONKADV(RE)                                                
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNH   *-6                                                              
         MVI   1(RE),C','                                                       
         LA    RE,2(RE)                                                         
***************                                                                 
* PROPOSAL FLIGHT DATES                                                         
***************                                                                 
         LR    R2,RE               SAVE A(WHERE TO CONTINUE)                    
         GOTO1 VDATCON,BODMCB,(3,CCONDAT),(5,BOWORK1)                           
         MVC   BOWORK1+3(2),=C'15'                                              
         MVC   BOWORK1+8(6),=CL6'(-52W)'                                        
         MVI   BOWORK1+14,C'-'                                                  
         GOTO1 (RF),(R1),(3,CCONDAT+3),(5,BOWORK1+15)                           
         MVC   BOWORK1+18(2),=C'15'                                             
         MVC   BOWORK1+23(6),=CL6'(-52W)'                                       
         GOTO1 VPERVAL,BODMCB,(29,BOWORK1),(0,PERVALST)                         
*                                                                               
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         LR    RE,R2                                                            
         MVC   0(3,RE),PVALCPER                                                 
         MVC   3(7,RE),PVALCPER+5                                               
         MVC   10(3,RE),PVALCPER+14                                             
         DROP  R1                                                               
*                                                                               
         MVI   13(RE),C','                                                      
         LA    RE,14(RE)                                                        
***************                                                                 
* LISTD                                                                         
***************                                                                 
         MVC   0(5,RE),=C'LISTD'                                                
***************                                                                 
* OUT TO GLOBBER AREA                                                           
***************                                                                 
         L     R6,ACOM                                                          
         USING COMFACSD,R6                                                      
         GOTO1 CGLOBBER,BODMCB,=C'PUTD',BOELEM,41,GLRKEY                        
         DROP  R6                                                               
*                                                                               
KLKVX    B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* HARDCODED DAYPART TABLE                                                       
***********************************************************************         
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
* TABLE OF KNOWN OBJECTS TYPES                                                  
***********************************************************************         
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECRD)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFKEY)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
KNOWTAB  DS    0XL(KNOWLQ)                                                      
* KEY PORTION                                                                   
         DC    AL2(00001),AL4(CONDTA)    CONTRACT                               
         DC    AL2(00002),AL4(PRODTA)    PROPOSAL                               
* RECORD (PROTECTED PORTION)                                                    
         DC    AL2(00049),AL4(STLDTA)    SATELLITE - PROTECTED                  
         DC    AL2(00007),AL4(STADTA)    STATION                                
         DC    AL2(00081),AL4(ADTDTA)    ACTIVITY DATES FIELD                   
* RECORD (INPUT PORTION)                                                        
         DC    AL2(00048),AL4(STLDTA)    SATELLITE - UNPROTECTED                
         DC    AL2(00009),AL4(BKSDTA)    BOOKS                                  
         DC    AL2(00010),AL4(PRJDTA)    PROJECTIONS                            
         DC    AL2(00046),AL4(PTPDTA)    PROJECTION TYPE(TP/PAV)                
         DC    AL2(00011),AL4(DMODTA)    DEMOS                                  
         DC    AL2(00012),AL4(LNSDTA)    LENGTHS                                
         DC    AL2(00017),AL4(DPTDTA)    DAYPART                                
         DC    AL2(00018),AL4(DAYDTA)    DAY                                    
         DC    AL2(00045),AL4(TIMDTA)    TIME                                   
         DC    AL2(00019),AL4(DTSDTA)    DATES                                  
         DC    AL2(00047),AL4(CPPDTA)    CPP/M                                  
         DC    AL2(00013),AL4(CSTDTA)    COMPETITIVE STATIONS                   
         DC    AL2(00014),AL4(HIADTA)    HIATUS                                 
         DC    AL2(00015),AL4(TXTDTA)    TEXT                                   
         DC    AL2(00016),AL4(DECDTA)    DECIMAL PRECISION                      
         DC    AL2(00050),AL4(BTPDTA)    BOOK TYPE(INV/TP/PAV)                  
         DC    AL2(00051),AL4(SACDTA)    ONLINE STATION ACCESS                  
         DC    AL2(00078),AL4(DTXDTA)    DAYPART TEXT FLAG                      
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
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
CNTPROFS DS    0CL10                CONTRACT PROFILES                           
         DS    CL1                 PROGRAM #                                    
         DS    CL1                 SPARE                                        
CONPROFS DS    CL8                 PROFILE BITS                                 
*                                                                               
SELPROFS DS    0CL10                CONTRACT PROFILES                           
         DS    CL1                 PROGRAM #                                    
         DS    CL1                 SPARE                                        
SELPROF  DS    CL8                 PROFILE BITS                                 
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1KYCHG EQU   X'80'                - KEY FIELD CHANGED                         
MF1PFRET EQU   X'40'                - RETURNING FROM CALLED SESSION             
MF1MXWRT EQU   X'20'                - NEED MINIO WRT ON DETAIL UPDATE           
MF1CLRPF EQU   X'10'                - CLEAR PRIME FETCH BIT IN BOOKS            
MF1TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
MNIOFLAG DS    XL1                 MINIO FLAG                                   
MNIOCLSQ EQU   X'80'               - A CHANGE WAS MADE, CLOSE MINIO             
*                                                                               
BKCOUNT  DS    X                   BOOK COUNT FOR VALCST                        
*                                                                               
TODAYBIN DS    XL3                                                              
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE AREA                          
*                                                                               
DISCSTS  DS    0XL(NUMCSTA*STLNLENQ) DISPLAY COMPETITIVE STATIONS               
DISCSTA  DS    (NUMCSTA)XL(STLNLENQ)                                            
*                                                                               
DISBOOKS DS    0XL(NUMBKS*BKLNLENQ)  DISPLAY BOOKS AND UPGRADES                 
DISBOOK  DS    (NUMBKS)XL(BKLNLENQ)                                             
*                                                                               
DISLBLS  DS    0XL(NUMBKS*5)         DISPLAY USER DEFINED LABELS                
DISLBL   DS    (NUMBKS)XL5                                                      
*                                                                               
DISDEMOS DS    0XL(NUMDEMS*DMLNLENQ) DISPLAY DEMOS                              
DISDEMO  DS    (NUMDEMS)XL(DMLNLENQ)                                            
         DS    XL(DMLNLENQ)          EOT                                        
*                                                                               
SAVDPTS  DS    0CL(NUMDPTS*DPLNLENQ) SAVED DAYPARTS                             
SAVDPT   DS    (NUMDPTS)XL(DPLNLENQ)                                            
         DS    XL(DPLNLENQ)          EOT                                        
*                                                                               
SAVDYTMS DS    0CL(NUMDPTS*(1+4))    SAVED 1-BYTE DAY/4-BYTE TIMES              
SAVDYTM  DS    (NUMDPTS)XL(1+4)                                                 
         DS    XL(1+4)               EOT                                        
*                                                                               
SAVSEDTS DS    0CL(NUMDPTS*(3+3))    SAVED PWOS JULIAN ST/END DATES             
SAVSEDT  DS    (NUMDPTS)XL(3+3)                                                 
         DS    XL(3+3)               EOT                                        
*                                                                               
SAVSLNS  DS    0CL(6*1)              SAVED 1-BYTE SPOT LENGTHS                  
SAVSLN   DS    6XL1                                                             
*                                                                               
BOOKNUM  DS    XL1                 CURRENT # OF BOOK FOR UPGRADE EXPR           
SVBLKOFF DS    XL2                 SAVED BLOCK OFFSET FOR PROJECTIONS           
SVPRJFLD DS    X                   SAVED PROJECTION FIELD(LINE)                 
*                                                                               
SAVFTM   DS    X                   SAVED FETCH METHOD                           
*                                                                               
SAVBKTYP DS    X                   SAVED BOOK TYPE                              
*                                                                               
DESCFLGS DS    X                   SAVED DESCRIPTION ELEMENT FLAGS              
*                                                                               
SVDPTLIN DS    XL1                 CURRENT NUMBER OF DPT LINE                   
*                                                                               
SAVOPTNS DS    XL1                 OPTIONS                                      
OPTNTXTQ EQU   X'80'                - TEXT BIT                                  
OPTNDECQ EQU   X'40'                - DEMO DECIMAL PRECISION BIT                
*                                                                               
WHICHUPE DS    XL1                 WHICH UPGRADE EXPRESSION                     
DISUPGDS DS    0CL(4*(5+33))        5 BYTES  - LABEL                            
DISUPGD  DS    4CL(5+33)           33 BYTES  - UPGRADE CHAR EXPRESSION          
*                                                                               
PFPENDNG EQU   PFK02               PFKEY FOR PENDING/CHANGE                     
PFAVAIL  EQU   PFK03               PFKEY FOR AVAIL                              
PFPACKGE EQU   PFK04               PFKEY FOR PACKAGE                            
PFMBOOK  EQU   PFK05               PFKEY FOR MULTI-BOOK                         
PFMDEMO  EQU   PFK06               PFKEY FOR MULTI-DEMO                         
PFKYMORE EQU   PFK11               PFKEY FOR MORE                               
PFRETURN EQU   PFK12               PFKEY FOR RETURN                             
PFKYRIS  EQU   PFK14               PFKEY FOR RIS                                
PFMCOST  EQU   PFK15               PFKEY FOR MCOST                              
PFCSTLST EQU   PFK16               PFKEY FOR COST/LIST                          
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
*                                                                               
MINSTAS  DS    0XL(NUMSTAS*STLNLENQ) SAVED STATIONS                             
MINSTA   DS    (NUMSTAS)XL(STLNLENQ)                                            
*                                                                               
MINBKS   DS    0XL(NUMBKS*BKLNLENQ)  SAVED BOOKS AND UPGRADES                   
MINBK    DS    (NUMBKS)XL(BKLNLENQ)                                             
*                                                                               
MINLBLS  DS    0XL(NUMBKS*5)         SAVED USER DEFINED LABELS                  
MINLBL   DS    (NUMBKS)XL5                                                      
*                                                                               
MINDMOS  DS    0XL(NUMDEMS*DMLNLENQ) SAVED DEMOS                                
MINDMO   DS    (NUMDEMS)XL(DMLNLENQ)                                            
         DS    XL(DMLNLENQ)          EOT                                        
*                                                                               
MINDPTS  DS    0CL(NUMDPTS*DPLNLENQ) SAVED DAYPARTS                             
MINDPT   DS    (NUMDPTS)XL(DPLNLENQ)                                            
         DS    XL(DPLNLENQ)          EOT                                        
*                                                                               
MINDYTMS DS    0CL(NUMDPTS*(1+4))    SAVED 1-BYTE DAY/4-BYTE TIMES              
MINDYTM  DS    (NUMDPTS)XL(1+4)                                                 
         DS    XL(1+4)               EOT                                        
*                                                                               
MINSEDTS DS    0CL(NUMDPTS*(3+3))    SAVED PWOS JULIAN ST/END DATES             
MINSEDT  DS    (NUMDPTS)XL(3+3)                                                 
         DS    XL(3+3)               EOT                                        
*                                                                               
         DS    XL(TWUSER+L'TWUSER-*)   # OF SPARE BEFORE TWSECBLK               
* FAFACTS                                                                       
* RECNTPROF                                                                     
* FASYSLSTD                                                                     
* DDDDEQUS                                                                      
* DDCOMFACS                                                                     
* CTMSGEQUS                                                                     
* FASELIST                                                                      
* FASYSFAC                                                                      
* DDSCANBLKD                                                                    
* DDFLDHDR                                                                      
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE RECNTPROF                                                      
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE DDDDEQUS                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTMSGEQUS                                                      
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016REPRO11S  05/15/97'                                      
         END                                                                    
