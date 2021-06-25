*          DATA SET REPRO10S   AT LEVEL 024 AS OF 05/01/02                      
*&&      SET   NOP=N                                                            
*PHASE T80A10A                                                                  
T80A10   TITLE 'REPRO10 - PROPROSAL RECORDS - REPORT OVERLAY'                   
***********************************************************************         
* NOTES-                                                                        
*                                                                               
*    PROGRAM IS UNFIT FOR CORERESIDENCEY.  DUE TO PRNTBUFF WHICH                
*      WOULD NEED TO BE MOVED TO THE PROGRAM AREA.                              
***********************************************************************         
PRO10    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T80A10*,R7,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
*                                                                               
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         L     RA,ATWA                                                          
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
DTDEF    EQU   0                                                                
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
TABLEOO  DS    0H                                                               
         DC    AL1(OKEY),AL1(0,0,0),AL4(EXITOK)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(EXITOK)                                 
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFKEY)                                  
         DC    AL1(OREP),AL1(0,0,0),AL4(REPORT)                                 
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                                
***********************************************************************         
INIT     DS    0H                                                               
         OI    TWASRVH+1,X'01'     SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    TWASRVH+6,X'80'                                                  
         OI    GCINDS1,GCIPROT     UNPROT ON NTRSES                             
         OI    GSINDSL1,GSIXKEY    NO ENTER DETAILS                             
         MVI   LSTSINDS,0                                                       
*                                                                               
         LA    R1,BCPARM                                                        
         XC    BCPARM(4),BCPARM                                                 
         MVC   BCPARM+4(4),=X'D9000A00'  SET FOR CORE RESIDENT PHASES           
*                                                                               
         MVI   BCPARM+7,QCENTER                                                 
         GOTO1 VCOLY,BCPARM                                                     
         MVC   CENTER,0(R1)                                                     
*                                                                               
         MVI   BCPARM+7,QUNDRLIN                                                
         GOTO1 (RF),(R1)                                                        
         MVC   UNDERLIN,0(R1)                                                   
*                                                                               
         MVI   BCPARM+7,QCHOPPER                                                
         GOTO1 (RF),(R1)                                                        
         MVC   CHOPPER,0(R1)                                                    
*                                                                               
         MVC   ATSAR,AGTSAR                                                     
*                                                                               
         CLI   ASONOFF,ASOFF                                                    
         BNE   INITX                                                            
*                                                                               
         L     RF,TWMASTC                                                       
         L     RF,MCVLOADR-MASTD(RF)                                            
         GOTOX (RF),BODMCB,=CL8'T00A7D',RR=BORELO                               
         ICM   RF,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ATSAR            LOAD TSAROFF                                 
*                                                                               
INITX    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* REPORT OBJECT          - ADDED IN FOR THE REPORT OBJECT             *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
REPORT   LM    R0,R3,SVPARMS                                                    
         USING FDRRECD,R2                                                       
         LA    RF,REPTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
REPTABL  DC    AL1(RPFIRST),AL1(0,0,0),AL4(REPFRST)                             
         DC    AL1(RDO),AL1(0,0,0),AL4(REPDO)                                   
         DC    AL1(RPLAST),AL1(0,0,0),AL4(REPLAST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR REPORT  OBJECT                                       *         
***********************************************************************         
         SPACE 1                                                                
REPFRST  DS    0H                                                               
         MVC   INSYSID,=CL2'RE'                                                 
         MVC   INPRGID,=CL2'SE'                                                 
         MVC   INJCLID,=CL2'SE'                                                 
         MVI   INPRTY1,C' '                                                     
         MVI   INPRTY2,C' '                                                     
*                                                                               
         L     R3,AREP                                                          
         USING REPD,R3                                                          
         MVI   REPCLASS,C'A'                                                    
         MVC   REPRLH,=Y(24)                                                    
         MVI   REPCOPY,1                                                        
         MVC   REPDESC,=CL20'SEL WKSHT'                                         
         MVC   REPMAKER(1),INSYSID                                              
         MVC   REPMAKER+1(2),INPRGID                                            
         DROP  R3                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO THE REPORT                                                       *         
***********************************************************************         
         SPACE 1                                                                
REPDO    DS    0H                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT                                 
         BNE   REPDO02                                                          
         GOTO1 =A(COUNT),RR=BORELO                                              
         B     EXITOK                                                           
*                                                                               
REPDO02  DS    0H                                                               
         GOTO1 =A(PREP),RR=BORELO                                               
         BL    EXITL                                                            
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* AFTER THE SCREEN IS VALIDATED                                       *         
***********************************************************************         
         SPACE 1                                                                
REPLAST  DS    0H                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT                                 
         BE    REPLST02                                                         
*                                                                               
         ZIC   RE,RCBKS                                                         
         BCTR  RE,0                                                             
         MH    RE,=Y(L'MINBK)                                                   
         AR    RE,RA                                                            
         AH    RE,=Y(MINBKS-TWAD)                                               
         USING BOOKLIN,RE                                                       
         TM    BKLNFLG,RPRBKPFT                                                 
         BZ    EXITPBNF                                                         
         DROP  RE                                                               
*                                                                               
         GOTO1 =A(SETFMT),RR=BORELO                                             
         BL    EXITL                                                            
*                                                                               
         GOTO1 =A(BLDSCTL),RR=BORELO                                            
         BL    EXITL                                                            
*                                                                               
REPLST02 CLI   ASONOFF,ASOFF                                                    
         BE    REPLST04                                                         
         CLI   BCPFKEY,7                                                        
         BE    REPLST04                                                         
         MVC   FVADDR,AOPTFLD                                                   
         MVC   FVMSGNO,=AL2(558)                                                
         B     EXITL                                                            
*                                                                               
REPLST04 DS    0H                                                               
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
DTATABL  DS    0AL1                                                             
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
************ DOING AN ACTION ON A SPECIFIC DATA OBJECT ****************         
***********************************************************************         
DATA10   DS    0H                                                               
         LR    RF,RB               TABLE OF KNOWN OBJECTS                       
         AH    RF,=Y(KNOWTAB-PRO10)                                             
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
CONTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISCON)                                
         DC    AL1(DRVAL),AL1(0,0,0),AL4(VALCON)                                
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
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    VALCONX             YES                                          
*                                                                               
         CLI   FVILEN,0            THIS FIELD IS REQUIRED                       
         BE    EXITNO                                                           
*                                                                               
         GOTOX (VALCONQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*****************                                                               
** MARKET NAME **                                                               
*****************                                                               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RSTAREC,RE                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,CUAALF                                                  
         MVC   RSTAKSTA,CCONKSTA                                                
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(27),IOKEYSAV                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,AIO4                                                          
         USING RSTAREC,RE                                                       
         MVC   EMKTNAME,RSTAMKT    MARKET NAME                                  
         DROP  RE                                                               
*                                                                               
*****************************                                                   
** SALESPERSON TELEPHONE # **                                                   
*****************************                                                   
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RSALREC,RE                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,CUAALF                                                  
         MVC   RSALKSAL,CCONSAL                                                 
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(27),IOKEYSAV                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,AIO4                                                          
         USING RSALREC,RE                                                       
         MVC   ESALTEL,RSALTEL     TELEPHONE NUMBER                             
         DROP  RE                                                               
*                                                                               
VALCONX  OI    FVIIND,FVIVAL       VALIDATED                                    
         MVI   HELPLINE,0                                                       
         MVC   ECONNUM,FVIFLD                                                   
         B     EXITOK                                                           
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
PROTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISPRO)                                
         DC    AL1(DRVAL),AL1(0,0,0),AL4(VALPRO)                                
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
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    VALPROX             YES                                          
*                                                                               
         CLI   FVILEN,0            ANY DATA IN THIS FIELD?                      
         BE    EXITNO              NO, NEED A NUMBER                            
*                                                                               
         GOTOX (VALPROQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*                                                                               
         MVC   BOELEM(L'GSRECKEY),GSRECKEY                                      
         LA    R2,GSRECKEY                                                      
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
         DROP  R2                                                               
*                                                                               
         GOTOX (MNIOINQ,AREPRO01),BOPARM                                        
         GOTO1 =A(RDBKSDMS),RR=BORELO                                           
         MVC   GSRECKEY,BOELEM                                                  
*                                                                               
VALPROX  DS    0H                                                               
         OI    FVIIND,FVIVAL       VALIDATED                                    
         MVC   EPRONUM,FVIFLD                                                   
         B     EXITOK                                                           
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
AGYTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISAGY)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY AGENCY FIELD                                                          
***********************************************************************         
DISAGY   DS    0H                                                               
         MVC   FVIFLD(L'EAGYNAM1),EAGYNAM1                                      
         B     EXITOK                                                           
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
ADVTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISADV)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY ADVERTISER FIELD                                                      
***********************************************************************         
DISADV   DS    0H                                                               
         MVC   FVIFLD(L'EADVNAME),EADVNAME                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PRODUCT                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRDDTA   DS    0H                                                               
         LA    RF,PRDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PRDTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISPRD)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY PRODUCT FIELD                                                         
***********************************************************************         
DISPRD   DS    0H                                                               
         MVC   FVIFLD(L'EPRDNAME),EPRDNAME                                      
         B     EXITOK                                                           
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
SALTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISSAL)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY SALESPERSON FIELD                                                     
***********************************************************************         
DISSAL   DS    0H                                                               
         MVC   FVIFLD(L'ESALNAME),ESALNAME                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BUYER                                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
BYRDTA   DS    0H                                                               
         LA    RF,BYRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BYRTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISBYR)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY BUYER FIELD                                                           
***********************************************************************         
DISBYR   DS    0H                                                               
         MVC   FVIFLD(L'ECONBUYR),ECONBUYR                                      
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
STATBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISSTA)                                
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
* DATA OBJECT FOR SATELLITE STATION                                             
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
STLDTA   DS    0H                                                               
         LA    RF,STLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
STLTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISSTL)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY SATELLITE STATION                                                     
***********************************************************************         
DISSTL   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS-TWAD) PRIMARY STARTION ENTRY                       
         USING STALIN,RE                                                        
         MVI   FVIFLD,C'N'                                                      
         TM    STLNFLG,RPRSTSTL                                                 
         BZ    *+8                                                              
         MVI   FVIFLD,C'Y'                                                      
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
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
FLTTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISFLT)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY FLIGHT FIELD                                                          
***********************************************************************         
DISFLT   DS    0H                                                               
         GOTO1 VDATCON,BODMCB,(3,CCONDAT),(5,FVIFLD)                            
         MVI   FVIFLD+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,CCONDAT+3),(5,FVIFLD+9)                             
         MVC   ECONDATE,FVIFLD                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISK ADDRESS                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DSKDTA   DS    0H                                                               
         LA    RF,DSKTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DSKTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISDSK)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DISK ADDRESS                                                          
***********************************************************************         
DISDSK   DS    0H                                                               
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
DVSTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISDVS)                                
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
DVTTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISDVT)                                
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
* DATA OBJECT FOR COMPETITIVE STATIONS                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
CSTDTA   DS    0H                                                               
         LA    RF,CSTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CSTTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISCST)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY COMPETITIVE STATIONS FIELD                                            
***********************************************************************         
DISCST   DS    0H                                                               
         LR    R5,RA                                                            
         AH    R5,=Y(MINSTAS-TWAD) PRIMARY STARTION ENTRY                       
         USING STALIN,R5                                                        
         LA    R5,STLNLENQ(R5)     SKIP PRIME STATION                           
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
         LA    R5,STLNLENQ(R5)                                                  
         BCT   R0,DISCST5                                                       
         DROP  R5                                                               
*                                                                               
DISCSTX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR REPORT STYLE                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
RTPDTA   DS    0H                                                               
         LA    RF,RTPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RTPTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALRTP)                                
         DC    AL1(DDFLTR),AL1(0,0,0),AL4(DEFRTP)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* SET DEFAULT REPORT STYLE                                                      
***********************************************************************         
DEFRTP   DS    0H                                                               
S        USING RSTYLED,REPSTYLS                                                 
         MVC   FVIFLD(L'RSTYLNAM),S.RSTYLNAM                                    
         MVC   STYLE,S.RSTYLSTL                                                 
         MVC   MYRWDTH,S.RSTYLWID                                               
         DROP  S                                                                
         OI    FVIIND,FVIVAL                                                    
         OI    MISCFLG1,MF1TPCHG   SET TYPE CHANGE                              
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE REPORT STYLE                                                         
***********************************************************************         
VALRTP   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1TPCHG   SET TYPE CHANGE                              
*                                                                               
         LA    RF,REPSTYLS                                                      
         USING RSTYLED,RF                                                       
         CLI   FVILEN,0                                                         
         BE    VALRTP10                                                         
*                                                                               
         ZIC   RE,FVXLEN                                                        
VALRTP5  CLI   0(RF),DDSQ          DSS ONLY STYLES?                             
         BNE   VALRTP6             NO                                           
         TM    CUSTAT,CUSDDS       DDS?                                         
         BNO   EXITNV              NO - THEN DONE                               
         LA    RF,RSTYLLNQ(RF)                                                  
*                                                                               
VALRTP6  CLI   0(RF),EOT                                                        
         BE    EXITNV                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),RSTYLNAM                                               
         BE    VALRTP10                                                         
         LA    RF,RSTYLLNQ(RF)                                                  
         B     VALRTP5                                                          
*                                                                               
VALRTP10 DS    0H                                                               
         MVC   STYLE,RSTYLSTL                                                   
         MVC   MYRWDTH,RSTYLWID                                                 
         MVC   FVIFLD(L'RSTYLNAM),RSTYLNAM                                      
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
*                                         TYPE > 10 MEANS RUN SOON              
*                                         TYPE > 20 MEANS RUN OV                
REPSTYLS DS    0XL(RSTYLLNQ)                                                    
         DC    CL8'QUICK   ',AL1(NAROWTHQ),AL1(RPTQAQ),AL1(0)                   
         DC    CL8'QAV     ',AL1(NAROWTHQ),AL1(RPTQAQ),AL1(PFKYQAV)             
         DC    CL8'PACKAGE ',AL1(NAROWTHQ),AL1(RPTPKQ),AL1(0)                   
         DC    CL8'PKG     ',AL1(NAROWTHQ),AL1(RPTPKQ),AL1(PFKYPKG)             
         DC    CL8'LONG    ',AL1(NORMWTHQ),AL1(RPTLAQ),AL1(0)                   
         DC    CL8'LAV     ',AL1(NORMWTHQ),AL1(RPTLAQ),AL1(PFKYLAV)             
         DC    AL1(DDSQ),XL(RSTYLLNQ-1)'0'                                      
         DC    CL8'COUNT   ',AL1(NORMWTHQ),AL1(RPTCNQ),AL1(0)                   
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR NUMBER OF COPIES                                              
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
COPDTA   DS    0H                                                               
         LA    RF,COPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
COPTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALCOP)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE NUMBER OF COPIES                                                     
***********************************************************************         
VALCOP   DS    0H                                                               
         L     R3,AREP                                                          
         USING REPD,R3                                                          
*                                                                               
         CLI   FVILEN,0                                                         
         BNE   *+16                                                             
         MVI   REPCOPY,1                                                        
         MVI   FVIFLD,C'1'                                                      
         B     VALCOPX                                                          
*                                                                               
         TM    FVIIND,FVINUM       VALID NUMERIC?                               
         BZ    EXITNOTN            NO, ERROR                                    
*                                                                               
         CLC   BCFULL,MAXCOPY      MAXCOPIES?                                   
         BNL   EXITNV              TOO MANY                                     
*                                                                               
         MVC   REPCOPY,BCFULL+3                                                 
*                                                                               
VALCOPX  B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR WHEN FIELD                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
WHNDTA   DS    0H                                                               
         LA    RF,WHNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
WHNTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALWHN)                                
         DC    AL1(DDFLTR),AL1(0,0,0),AL4(DEFWHN)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* SET DEFAULT WHEN                                                              
***********************************************************************         
DEFWHN   DS    0H                                                               
         XC    FVIFLD,FVIFLD                                                    
*                                                                               
         MVC   FVIFLD(4),=C'NOW '                                               
         MVI   FVILEN,3                                                         
         MVI   FVXLEN,2                                                         
*                                                                               
         CLI   STYLE,10                                                         
         BNH   DEFWHN0                                                          
*                                                                               
         MVC   FVIFLD(4),=C'SOON'                                               
         MVI   FVILEN,4                                                         
         MVI   FVXLEN,3                                                         
*                                                                               
         CLI   STYLE,20                                                         
         BNH   DEFWHN0                                                          
*                                                                               
         MVC   FVIFLD(4),=C'OV  '                                               
         MVI   FVILEN,2                                                         
         MVI   FVXLEN,1                                                         
*                                                                               
DEFWHN0  GOTOX ('VALWHEN',AGROUTS)                                              
         BL    EXITL                                                            
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE WHEN                                                                 
***********************************************************************         
VALWHN   DS    0H                                                               
         CLI   BCPFKEY,PFKYQAV                                                  
         BE    DEFWHN                                                           
         CLI   BCPFKEY,PFKYLAV                                                  
         BE    DEFWHN                                                           
         CLI   BCPFKEY,PFKYPKG                                                  
         BE    DEFWHN                                                           
*                                                                               
         GOTOX ('VALWHEN',AGROUTS)                                              
         BL    EXITL                                                            
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR REQUESTOR FIELD                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
REQDTA   DS    0H                                                               
         LA    RF,REQTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
REQTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALREQ)                                
         DC    AL1(DDFLTR),AL1(0,0,0),AL4(DEFREQ)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* SET DEFAULT REQUESTOR                                                         
***********************************************************************         
DEFREQ   DS    0H                                                               
         LA    RF,REQDEFS                                                       
DEFREQ02 CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                WHAT HAPPEND HERE?                           
         CLC   STYLE,0(RF)                                                      
         BE    DEFREQX                                                          
         LA    RF,L'REQDEFS(RF)                                                 
         B     DEFREQ02                                                         
*                                                                               
DEFREQX  MVC   FVIFLD(3),1(RF)                                                  
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE REQUESTOR                                                            
***********************************************************************         
VALREQ   DS    0H                                                               
         L     R3,AREP                                                          
         USING REPD,R3                                                          
         TM    MISCFLG1,MF1TPCHG                                                
         BZ    VALREQ5                                                          
*                                                                               
         LA    RF,REQDEFS                                                       
VALREQ1  CLI   0(RF),0             OUR REQUESTOR?                               
         BE    VALREQ5             NO - DO NOT SWITCH                           
         CLC   1(3,RF),FVIFLD                                                   
         BE    VALREQ2             YES - SWITCH TO NEW TYPE                     
         LA    RF,L'REQDEFS(RF)                                                 
         B     VALREQ1                                                          
*                                                                               
VALREQ2  DS    0H                                                               
         LA    RF,REQDEFS                                                       
VALREQ3  CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN REPORT TYPE                          
         CLC   STYLE,0(RF)                                                      
         BE    VALREQ4                                                          
         LA    RF,L'REQDEFS(RF)                                                 
         B     VALREQ3                                                          
*                                                                               
VALREQ4  MVC   FVIFLD(3),1(RF)                                                  
*                                                                               
VALREQ5  MVC   REPSUBID,FVIFLD                                                  
         MVC   INUSER,FVIFLD                                                    
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
PRINTOPS DS    0XL(8+1)                                                         
         DC    CL8'SOON    ',AL1(11)         DEFAULT WHEN                       
         DC    CL8'NOW     ',AL1(1)                                             
         DC    CL8'DDS     ',AL1(12)                                            
         DC    CL8'OV      ',AL1(13)                                            
         DC    AL1(EOT)                                                         
*                                                                               
REQDEFS  DS    0XL(1+3)            DEFAULT REQUESTORS                           
         DC    AL1(RPTQAQ),C'QAV'                                               
         DC    AL1(RPTPKQ),C'PKG'                                               
         DC    AL1(RPTLAQ),C'LAV'                                               
         DC    AL1(RPTCNQ),C'CNT'                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DESTIONATION                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DSTDTA   DS    0H                                                               
         LA    RF,DSTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DSTTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALDST)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE DESTINATION                                                          
***********************************************************************         
VALDST   DS    0H                                                               
         GOTOX ('VALDEST',AGROUTS)                                              
         BL    EXITL                                                            
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TITLE                                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
RTLDTA   DS    0H                                                               
         LA    RF,RTLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RTLTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALRTL)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE REPORT TITLE                                                         
***********************************************************************         
VALRTL   DS    0H                                                               
         MVC   AERRFLD,FVADDR                                                   
         TM    MISCFLG1,MF1TPCHG                                                
         BZ    VALRTL05                                                         
*                                                                               
         LA    RF,RTLDEFS                                                       
VALRTL1  CLI   0(RF),EOT           OUR REQUESTOR?                               
         BE    VALRTL05            NO - DO NOT SWITCH                           
         CLC   1(20,RF),FVIFLD                                                  
         BE    VALRTL2             YES - SWITCH TO NEW TYPE                     
         LA    RF,L'RTLDEFS(RF)                                                 
         B     VALRTL1                                                          
*                                                                               
VALRTL2  MVI   FVILEN,0                                                         
         MVI   FVIHDR+5,0                                                       
         MVC   FVIFLD,BCSPACES                                                  
*                                                                               
VALRTL05 CLI   FVILEN,0            ANY REPORT TITLE?                            
         BNE   VALRTL30            YES                                          
*                                                                               
         LA    RF,RTLDEFS                                                       
VALRTL20 CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                WHAT HAPPEND HERE?                           
         CLC   STYLE,0(RF)                                                      
         BE    VALRTL24                                                         
         LA    RF,L'RTLDEFS(RF)                                                 
         B     VALRTL20                                                         
VALRTL24 MVC   FVIFLD(L'RTLDEFS-1),1(RF)                                        
*                                                                               
VALRTL30 MVC   REPTITLE,FVIFLD                                                  
         L     RE,AREP                                                          
         USING REPD,RE                                                          
         MVC   REPDESC,REPTITLE                                                 
         DROP  RE                                                               
*                                                                               
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
*                                                                               
RTLDEFS  DS    0XL(1+20)            DEFAULT REPORT TITLES                       
         DC    AL1(RPTQAQ),CL20'QUICK AVAIL'                                    
         DC    AL1(RPTPKQ),CL20'PACKAGE'                                        
         DC    AL1(RPTLAQ),CL20'LONG AVAIL'                                     
         DC    AL1(RPTCNQ),CL20'COUNT REPORT'                                   
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CPP/M OPTIONS                                                 
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
CPPDTA   DS    0H                                                               
         LA    RF,CPPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CPPTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALCPP)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE CPP/M OPTIONS                                                        
***********************************************************************         
VALCPP   DS    0H                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    EXITOK              YES                                          
*                                                                               
         MVI   RCCPP,0                                                          
         NI    RCCOST,FF-RCCTRGQ                                                
*                                                                               
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         MVI   FVIFLD,C'N'                                                      
         B     VALCPPX                                                          
*                                                                               
         L     RE,AIO4                                                          
         XC    0(256,RE),0(RE)                                                  
         GOTO1 VSCANNER,BODMCB,FVIHDR,(X'84',AIO4)                              
         L     R3,AIO4                                                          
*                                                                               
VALCPP10 CLI   0(R3),0             ANY OPTION?                                  
         BE    VALCPPX             NO                                           
*                                                                               
         CLI   1(R3),0             OPTION=X?                                    
         BNE   VALCPPER            YES                                          
*                                                                               
         ZIC   RE,0(R3)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'NONE'                                              
         BNE   VALCPP12                                                         
         MVI   RCCPP,0                                                          
         NI    RCCOST,FF-RCCTRGQ                                                
         XC    FVIFLD,FVIFLD                                                    
         MVI   FVIFLD,C'N'                                                      
         B     VALCPPX             DON'T PROCESS ANY MORE                       
*                                                                               
VALCPP12 EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'Y'                                                 
         BNE   VALCPP14                                                         
         CLI   STYLE,RPTLAQ     IF NOT LONG AVAIL                               
         BE    *+12                                                             
         OI    RCCPP,RCCPCOLQ    DO PRIMARY DEMO CPP (AS COLUMN)                
         B     VALCPP30                                                         
*                                  IF LONG AVAIL                                
         OI    RCCPP,RCCPSTKQ      ALL DEMO CPP'S                               
         B     VALCPP30                                                         
*                                                                               
VALCPP14 EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'B'                                                 
         BNE   VALCPP16                                                         
         OI    RCCPP,RCPBYRQ       SET ON BUYERS CPP                            
         OI    RCCOST,RCCTRGQ      SET ON TRGT RATE                             
         B     VALCPP30                                                         
*                                                                               
VALCPP16 DS    0H                                                               
VALCPPER MVC   FVERRNDX,4(R3)                                                   
         B     EXITNV                                                           
*                                                                               
VALCPP30 DS    0H                                                               
         LA    R3,32(R3)                                                        
         B     VALCPP10                                                         
*                                                                               
VALCPPX  OI    FVIIND,FVIVAL                                                    
         CLI   RCNCBKS,0                                                        
         BZ    EXITOK                                                           
         TM    RCCPP,RCCPSTKQ+RCCPCOLQ                                          
         BNZ   EXITOK                                                           
         MVC   FVMSGNO,=AL2(615)                                                
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TEXT OPTIONS                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
TXTDTA   DS    0H                                                               
         LA    RF,TXTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TXTTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALTXT)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE TEXT OPTIONS                                                         
***********************************************************************         
VALTXT   DS    0H                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    EXITOK              YES                                          
*                                                                               
         MVI   RCTEXT,0                                                         
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         MVC   FVIFLD(4),=CL8'NONE'                                             
         B     VALTXTX                                                          
*                                                                               
         L     RE,AIO4                                                          
         XC    0(256,RE),0(RE)                                                  
         GOTO1 VSCANNER,BODMCB,FVIHDR,(X'84',AIO4)                              
         L     R3,AIO4                                                          
*                                                                               
VALTXT10 CLI   0(R3),0             ANY OPTION?                                  
         BE    VALTXTX             NO                                           
*                                                                               
         CLI   1(R3),0             OPTION=X?                                    
         BNE   VALTXTER            YES                                          
*                                                                               
         ZIC   RE,0(R3)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'NONE'                                              
         BNE   VALTXT12                                                         
         MVI   RCTEXT,0                                                         
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(4),=CL8'NONE'                                             
         B     VALTXTX             DON'T PROCESS ANY MORE                       
*                                                                               
VALTXT12 DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'A'                                                 
         BNE   VALTXT14                                                         
         OI    RCTEXT,RCTALLQ                                                   
         B     VALTXTX             DON'T PROCESS ANY MORE                       
*                                                                               
VALTXT14 DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'M'                                                 
         BNE   VALTXT16                                                         
         OI    RCTEXT,RCTMRKQ                                                   
         B     VALTXT30                                                         
*                                                                               
VALTXT16 DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'S'                                                 
         BNE   VALTXT18                                                         
         OI    RCTEXT,RCTSTAQ                                                   
         B     VALTXT30                                                         
*                                                                               
VALTXT18 DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'F'                                                 
         BNE   VALTXT20                                                         
         OI    RCTEXT,RCTFTNQ                                                   
         B     VALTXT30                                                         
*                                                                               
VALTXT20 DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'I'                                                 
         BNE   VALTXT22                                                         
         OI    RCTEXT,RCTINVQ                                                   
         B     VALTXT30                                                         
*                                                                               
VALTXT22 DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'I+'                                                
         BNE   VALTXT24                                                         
         OI    RCTEXT,RCTINVPQ                                                  
         B     VALTXT30                                                         
*                                                                               
VALTXT24 DS    0H                                                               
*                                                                               
VALTXTER MVC   FVERRNDX,4(R3)                                                   
         B     EXITNV                                                           
*                                                                               
VALTXT30 DS    0H                                                               
         LA    R3,32(R3)                                                        
         B     VALTXT10                                                         
*                                                                               
VALTXTX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DAYPART FILTER                                                
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DPTDTA   DS    0H                                                               
         LA    RF,DPTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DPTTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALDPT)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE DAYPART FILTER                                                       
***********************************************************************         
VALDPT   DS    0H                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    EXITOK              YES                                          
*                                                                               
         MVI   RCNDPTS,0           NO FILTERS                                   
         XC    RCDPTS,RCDPTS       CLEAR OUT THE DPT FILTER                     
*                                                                               
         CLI   FVILEN,0            ANY DAYPART FILTER?                          
         BE    VALDPTX             NO                                           
*                                                                               
         ZIC   RE,FVXLEN                                                        
*****    EX    RE,*+8              DAMN PROGRAMS AREA                           
*****    B     *+10                                                             
         EX    RE,*+4                                                           
         MVC   RCDPTS(0),FVIFLD    COPY THE 1-BYTE DAYPART CODE                 
         MVC   RCNDPTS,FVILEN      NUMBER OF FILTERS                            
*                                                                               
         MVC   BOBYTE1,RCDPTS                                                   
         GOTO1 =A(DPTOUT),RR=BORELO                                             
         MVC   RCDPTF3,BOFULL1                                                  
*                                                                               
VALDPTX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMPETITION OPTION                                            
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
CMPDTA   DS    0H                                                               
         LA    RF,CMPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CMPTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALCMP)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE COMPETITION OPTION                                                   
***********************************************************************         
VALCMP   DS    0H                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    EXITOK              YES                                          
*                                                                               
         NI    RCFLAGS1,FF-RCF1COMP                                             
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         MVI   FVIFLD,C'N'                                                      
         B     VALCMPX                                                          
*                                                                               
         CLI   FVIFLD,C'N'                                                      
         BE    VALCMPX                                                          
*                                                                               
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    RCFLAGS1,RCF1COMP                                                
*                                                                               
VALCMPX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COVERSHEET                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
CVRDTA   DS    0H                                                               
         LA    RF,CVRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CVRTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALCVR)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE COVERSHEET FIELD                                                     
***********************************************************************         
VALCVR   DS    0H                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    EXITOK              YES                                          
*                                                                               
         NI    RCFLAGS2,FF-RCF2COVR                                             
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         MVI   FVIFLD,C'N'                                                      
         B     VALCVRX                                                          
*                                                                               
         CLI   FVIFLD,C'N'                                                      
         BE    VALCVRX                                                          
*                                                                               
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    RCFLAGS2,RCF2COVR                                                
*                                                                               
VALCVRX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DOUBLE SPACE                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DBLDTA   DS    0H                                                               
         LA    RF,DBLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DBLTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALDBL)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE DOUPLE SPACE FIELD                                                   
***********************************************************************         
VALDBL   DS    0H                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    EXITOK              YES                                          
*                                                                               
         CLI   SELPROFS,RREPQSEL                                                
         BE    VALDBL0                                                          
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
VALDBL0  DS    0H                                                               
*                                                                               
         NI    RCFLAGS2,FF-RCF2DBSP                                             
         CLI   FVILEN,0                                                         
         BNE   VALDBL10                                                         
*                                                                               
         TM    SELPROF+SELDBSPB,SELDBSPA     DEFAULT TO Y?                      
         BO    *+12                          YES                                
         MVI   FVIFLD,C'N'                                                      
         B     VALDBLX                                                          
*                                                                               
         OI    RCFLAGS2,RCF2DBSP                                                
         MVI   FVIFLD,C'Y'                                                      
         B     VALDBLX                                                          
*                                                                               
VALDBL10 CLI   FVIFLD,C'N'                                                      
         BE    VALDBLX                                                          
*                                                                               
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    RCFLAGS2,RCF2DBSP                                                
*                                                                               
VALDBLX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PORTRAIT                                                      
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PORDTA   DS    0H                                                               
         LA    RF,PORTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PORTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALPOR)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE PORTRAIT FIELD                                                       
***********************************************************************         
LNDSCPLQ EQU   60                                                               
PORTRTLQ EQU   60                  SILLY OPTION                                 
PORTWTHQ EQU   80                                                               
NAROWTHQ EQU   110                                                              
NORMWTHQ EQU   132                                                              
*                                                                               
VALPOR   DS    0H                                                               
         L     R3,AREP                                                          
         USING REPD,R3                                                          
         NI    RCFLAGS2,FF-RCF2PORT                                             
         MVI   REPMAXL,LNDSCPLQ                                                 
*                                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    EXITOK              YES                                          
*                                                                               
         TM    MISCFLG1,MF1TPCHG   REPORT TYPE CHANGED?                         
         BZ    *+12                                                             
         TM    FVIIND,FVIVAL                                                    
         BNZ   VALPOR01                                                         
*                                                                               
         CLI   FVILEN,0                                                         
         BNE   VALPOR02                                                         
VALPOR01 MVI   FVIFLD,C'P'                                                      
         CLI   MYRWDTH,PORTWTHQ                                                 
         BE    VALPORX                                                          
         MVI   FVIFLD,C'N'                                                      
         CLI   MYRWDTH,NAROWTHQ                                                 
         BE    VALPORX                                                          
         MVI   FVIFLD,C'S'                                                      
         CLI   MYRWDTH,NORMWTHQ                                                 
         BE    VALPORX                                                          
         MVI   MYRWDTH,NORMWTHQ                                                 
         B     VALPORX                                                          
*                                                                               
VALPOR02 DS    0H                                                               
         OC    FVIFLD,BCSPACES                                                  
         CLI   FVIFLD,C'P'                                                      
         BNE   VALPOR04                                                         
         MVI   MYRWDTH,PORTWTHQ                                                 
         OI    RCFLAGS2,RCF2PORT                                                
         MVI   REPMAXL,PORTRTLQ                                                 
         B     VALPORX                                                          
VALPOR04 DS    0H                                                               
         CLI   FVIFLD,C'N'                                                      
         BNE   VALPOR06                                                         
         MVI   MYRWDTH,NAROWTHQ                                                 
         B     VALPORX                                                          
VALPOR06 DS    0H                                                               
         CLI   FVIFLD,C'S'                                                      
         BNE   VALPOR08                                                         
         MVI   MYRWDTH,NORMWTHQ                                                 
         B     VALPORX                                                          
VALPOR08 DS    0H                                                               
         B     EXITNV                                                           
*                                                                               
VALPORX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INVENTORY OPTION                                              
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
INVDTA   DS    0H                                                               
         LA    RF,INVTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
INVTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALINV)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE INVENTORY OPTION                                                     
***********************************************************************         
VALINV   DS    0H                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    EXITOK              YES                                          
*                                                                               
         NI    RCFLAGS1,FF-RCF1INV                                              
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         MVI   FVIFLD,C'N'                                                      
         B     VALINVX                                                          
*                                                                               
         CLI   FVIFLD,C'N'                                                      
         BE    VALINVX                                                          
*                                                                               
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    RCFLAGS1,RCF1INV                                                 
*                                                                               
VALINVX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COST OPTION                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
COSDTA   DS    0H                                                               
         LA    RF,COSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
COSTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALCOS)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE COST OPTION                                                          
***********************************************************************         
VALCOS   DS    0H                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    EXITOK              YES                                          
*                                                                               
         OI    RCCOST,RCCNEGQ+RCCCOLQ                                           
         MVI   RCNCSTS,1           DEFAULT TO FIRST COST                        
         XC    RCCSTS,RCCSTS                                                    
         MVI   RCCSTS,1                                                         
*                                                                               
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         MVI   FVIFLD,C'1'                                                      
         B     VALCOSX                                                          
*                                                                               
         ZIC   R1,FVILEN            LENGTH                                      
         LA    R2,FVIFLD                                                        
*                                                                               
         CLI   0(R2),C'N'           COST=N?                                     
         BNE   VALCOS02                                                         
         TM    RCCPP,RCCPSTKQ+RCCPCOLQ                                          
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(614)                                                
         B     EXITL                                                            
         NI    RCCOST,X'FF'-RCCCOLQ                                             
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    VALCOSX                                                          
         LA    R2,1(R2)                                                         
*                                                                               
VALCOS02 DS    0H                                                               
         STC   R1,BOBYTE1                                                       
         CLI   BOBYTE1,NUMCSTS                                                  
         BH    EXITNV                                                           
*                                                                               
         XC    RCCSTS,RCCSTS       CLEAR ALL                                    
         LA    R4,RCCSTS                                                        
         MVI   RCNCSTS,0           COUNTER                                      
*                                                                               
VALCOS04 DS    0H                                                               
         CLI   0(R2),C'1'                                                       
         BL    VALCOSE                                                          
         CLI   0(R2),C'4'          ONLY 4 COSTS                                 
         BH    VALCOSE                                                          
*                                                                               
         MVC   BOBYTE1,0(R2)                                                    
         NI    BOBYTE1,X'0F'                                                    
*                                                                               
         ZIC   RE,BOBYTE1          VERIFY ITS VALID                             
         BCTR  RE,0                                                             
         MH    RE,=Y(L'MINCOST)                                                 
         AH    RE,=Y(MINCOSTS-TWAD)                                             
         AR    RE,RA                                                            
         USING CSTLIN,RE                                                        
*                                                                               
         ZIC   R0,RCNBKS                                                        
         LA    RF,RCBKS                                                         
VALCOS06 DS    0H                                                               
         CLI   CSLNLBK,0           UNLINKED                                     
         BE    VALCOS10            YES - VAILD                                  
         CLC   CSLNLBK,0(RF)       FOR PRIME BOOK?                              
         BE    VALCOS10            YES - VALID                                  
         LA    RF,1(RF)                                                         
         BCT   R0,VALCOS06                                                      
         DROP  RE                                                               
*                                                                               
VALCOSE  LA    RE,FVIFLD                                                        
         SR    R2,RE                                                            
         STC   R2,FVERRNDX                                                      
         MVC   FVMSGNO,=AL2(635)                                                
         B     EXITL                                                            
*                                                                               
VALCOS10 MVC   0(1,R4),BOBYTE1     MOVE BINARY COST CODE INTO RCCSTS            
         LA    R4,1(R4)                                                         
         ZIC   RF,RCNCSTS          BUMP COST COUNTER                            
         LA    RF,1(RF)                                                         
         STC   RF,RCNCSTS                                                       
*                                                                               
         LA    R2,1(R2)            NEXT INPUT NUMBER                            
         BCT   R1,VALCOS04                                                      
*                                                                               
VALCOSX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SHARE/LEVEL OPTION                                            
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SHLDTA   DS    0H                                                               
         LA    RF,SHLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SHLTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALSHL)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE SHARE/LEVEL OPTION                                                   
***********************************************************************         
VALSHL   DS    0H                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    EXITOK              YES                                          
*                                                                               
         NI    RCFLAGS1,FF-(RCF1SHR+RCF1PUT)                                    
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         MVI   FVIFLD,C'N'                                                      
         B     VALSHLX                                                          
*                                                                               
         CLI   FVIFLD,C'N'                                                      
         BE    VALSHLX                                                          
*                                                                               
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    RCFLAGS1,RCF1SHR+RCF1PUT                                         
*                                                                               
VALSHLX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SORT OPTION                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SRTDTA   DS    0H                                                               
         LA    RF,SRTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SRTTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALSRT)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE SORT OPTION                                                          
***********************************************************************         
VALSRT   DS    0H                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    EXITOK              YES                                          
*                                                                               
         XC    SORTREQ,SORTREQ                                                  
*                                                                               
         CLI   SELPROFS,RREPQSEL                                                
         BE    VALSRT0                                                          
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
VALSRT0  DS    0H                                                               
*                                                                               
         MVI   BOBYTE1,0                                                        
         CLI   FVILEN,0             ANY INPUT?                                  
         BNE   VSR01                YES                                         
         MVC   FVIFLD(7),=C'DPT,INV'                                            
         MVI   FVIHDR+5,7                                                       
         MVI   FVILEN,7                                                         
         MVI   FVXLEN,6                                                         
         TM    SELPROF+SELSDTMB,SELSDTMA     BLAIR DEFAULT?                     
         BNO   VSR01                                                            
         MVC   FVIFLD(8),=C'DP,DT,IN'                                           
         MVI   FVIHDR+5,8                                                       
         MVI   FVILEN,8                                                         
         MVI   FVXLEN,7                                                         
*                                                                               
VSR01    DS    0H                                                               
         OI    MISCFLG1,MF1DPTSR                                                
         CLC   FVIFLD(3),=C'DPT,INV'   DAYPART FIRST?                           
         BE    VSR02                   YES                                      
         CLI   RCNDPTS,1               FILTERING ON ONE DAYPART?                
         BE    VSR02                   YES                                      
         NI    MISCFLG1,FF-MF1DPTSR                                             
*                                                                               
VSR02    DS    0H                                                               
         L     RE,AIO4                                                          
         XC    0(256,RE),0(RE)                                                  
         GOTO1 VSCANNER,BODMCB,FVIHDR,(X'82',AIO4)                              
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)            NUMBER OF LINES                            
         BZ    VSR40                                                            
         L     R3,AIO4                                                          
         LA    R2,SORTREQ                                                       
         CLI   0(R2),0             FIND FIRST EMPTY SLOT IN SORTREQ             
         BE    VSR04                                                            
         LA    R2,1(R2)                                                         
         B     *-12                                                             
*                                                                               
VSR04    DS    0H                                                               
         L     R4,=A(SORTWDS)      CHECK AGAINST TABLE                          
         A     R4,BORELO                                                        
         CLI   22(R3),C' '         NO RIGHT HAND VALUES ALLOWED                 
         BH    VSR06B                                                           
*                                                                               
VSR06    DS    0H                                                               
         CLI   0(R4),X'FF'         EOL                                          
         BNE   *+14                                                             
VSR06B   DS    0H                                                               
         MVC   FVERRNDX,4(R3)                                                   
         B     EXITNV                                                           
*                                                                               
         CLC   12(4,R3),0(R4)                                                   
         BE    VSR08                                                            
         LA    R4,SORTWDL(R4)                                                   
         B     VSR06                                                            
*                                                                               
VSR08    DS    0H                                                               
         LA    RE,SORTREQ                                                       
         CR    RE,R2                                                            
         BE    VSR12                                                            
VSR10    DS    0H                                                               
         CLC   0(1,RE),4(R4)                                                    
         BE    VSR06B                                                           
         LA    RE,1(RE)                                                         
         CR    RE,R2                                                            
         BL    VSR10                                                            
*                                                                               
VSR12    DS    0H                                                               
         MVC   0(1,R2),4(R4)         SET INTERNAL SORT CODE                     
         CLI   0(R2),7             IF INVENTORY NUMBER                          
         BNE   *+8                                                              
         OI    BOBYTE1,X'80'         KEEP TRACK OF THAT                         
*                                                                               
         LA    R2,1(R2)                                                         
         LA    R3,32(R3)                                                        
         BCT   R0,VSR04                                                         
*                                                                               
VSR40    DS    0H                                                               
         ZIC   RE,FVILEN                                                        
         LA    RE,FVIFLD(RE)                                                    
         TM    BOBYTE1,X'80'         HAVE WE DONE INVENTORY NUMBER?             
         BNZ   *+14                                                             
         MVI   SORTREQ+L'SORTREQ-3,7       AWAYS INCLUDE INVENTORY #            
         MVC   0(4,RE),=C',INV'                                                 
*                                                                               
         MVI   SORTREQ+L'SORTREQ-2,8       ALWAYS SORT ON SEQ NO                
         MVI   SORTREQ+L'SORTREQ-1,X'FF'   SET EOL                              
*                                                                               
VALSRTX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
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
OPTTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALOPT)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE OPTIONS                                                              
***********************************************************************         
VALOPT   DS    0H                                                               
         GOTO1 =A(VALOPTNS),RR=BORELO                                           
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR HELP LINES                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
HLPDTA   DS    0H                                                               
         LA    RF,HLPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
HLPTBL   DC    AL1(DRDIS),AL1(0,0,0),AL4(DISHLP)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY HELP LINES                                                            
***********************************************************************         
DISHLP   DS    0H                                                               
         CLI   HELPLINE,HLPNUMMX                                                
         BH    DISHLPX                                                          
*                                                                               
         ZIC   R1,HELPLINE                                                      
         LA    R0,1(R1)                                                         
         STC   R0,HELPLINE                                                      
*                                                                               
         MH    R1,=Y(L'HELPTEXT)                                                
         A     R1,=A(HELPTEXT)                                                  
         A     R1,BORELO                                                        
         MVC   FVIFLD(L'HELPTEXT),0(R1)                                         
*                                                                               
DISHLPX  B     EXITOK                                                           
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
SCRNTBL  DC    AL1(SMOD),AL1(0,0,0),AL4(MODSCR)                                 
         DC    AL1(EOT)                                                         
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
MODSCR40 DS    0H                                                               
         CLM   RE,3,=AL2(54)       REPORT TYPE?                                 
         BNE   MODSCR44            NO                                           
*                                                                               
         CLI   BCPFKEY,PFKYQAV                                                  
         BE    MODSCR41                                                         
         CLI   BCPFKEY,PFKYPKG                                                  
         BE    MODSCR41                                                         
         CLI   BCPFKEY,PFKYLAV                                                  
         BE    MODSCR41                                                         
         B     MODSCRNX                                                         
*                                                                               
MODSCR41 L     RF,=A(REPSTYLS)                                                  
         A     RF,BORELO                                                        
         USING RSTYLED,RF                                                       
MODSCR42 CLI   0(RF),EOT                                                        
         BE    MODSCRNX                                                         
         CLC   BCPFKEY,RSTYLPFK                                                 
         BE    MODSCR43                                                         
         LA    RF,RSTYLLNQ(RF)                                                  
         B     MODSCR42                                                         
*                                                                               
MODSCR43 ZIC   R1,0(R5)                                                         
         SH    R1,=H'8'                                                         
         TM    1(R5),X'02'                                                      
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         CLM   R1,1,=AL1(RSTYLLNQ-1)                                            
         BNH   *+8                                                              
         LA    R1,RSTYLLNQ-1                                                    
         BCTR  R1,0                                                             
*****    EX    R1,*+8              DAMN PROGRAMS AREA                           
*****    B     *+10                                                             
         EX    R1,*+4                                                           
         MVC   8(0,R5),0(RF)                                                    
         DROP  RF                                                               
*                                                                               
         LA    RE,9(R1,R5)                                                      
         CLI   0(RE),C' '                                                       
         BNL   *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RF,8(R5)                                                         
         SR    RE,RF                                                            
         STC   RE,5(R5)                                                         
*                                                                               
         OI    6(R5),X'80'                                                      
         NI    4(R5),FF-X'20'                                                   
         B     MODSCRNX                                                         
*                                                                               
MODSCR44 SR    R0,R0               SEE IF WE HAVE DATA FOR THIS FIELD           
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
*****    EX    R1,*+8              DAMN PROGRAMS AREA                           
*****    B     *+10                                                             
         EX    R1,*+4                                                           
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
         DC    AL2(00054),AL4(FLDPROT)   REPORT TYPE                            
         DC    AL2(EOT)                                                         
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
         DC    AL1(PFUSER),AL1(0,0,0),AL4(USRPFK)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* CAN SET THE RECORD FOR THE PFKEY                                              
***********************************************************************         
RECPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),PFKYREQ                                                    
         BE    RECPFKX                                                          
         CLI   0(RE),PFKYDWNL                                                   
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'Download'                                         
         B     RECPFKX                                                          
         B     NOTPFK                                                           
*                                                                               
RECPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE ACTION FOR THE PFKEY                                              
***********************************************************************         
ACTPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),PFKYREQ                                                    
         BE    ACTPFKX                                                          
         B     NOTPFK              BTWN PF3 AND PF6, NO ACTION WANTED           
*                                                                               
ACTPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE USER NAME FOR THE PFKEY                                           
***********************************************************************         
USRPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),PFKYQAV                                                    
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'Qav'                                              
         B     USRPFKX                                                          
         CLI   0(RE),PFKYLAV                                                    
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'Lav'                                              
         B     USRPFKX                                                          
         CLI   0(RE),PFKYPKG                                                    
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'Pkg'                                              
         B     USRPFKX                                                          
         CLI   0(RE),PFKYRUN                                                    
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'Run Rpt'                                          
         B     USRPFKX                                                          
         CLI   0(RE),PFKYRET                                                    
         BNE   *+14                                                             
         MVC   FVIFLD(8),=CL8'Return'                                           
         B     USRPFKX                                                          
         B     NOTPFK                                                           
*                                                                               
USRPFKX  B     EXITOK                                                           
***********************************************************************         
* PFKEY DEFINITION (RECORD, ACTION, OR USER) NOT WANTED                         
***********************************************************************         
NOTPFK   OI    SVPARMS3,X'80'                                                   
         B     EXITOK                                                           
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
XITR2    XIT1  REGS=(R2)                                                        
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
EXITIOBK MVC   FVMSGNO,=AL2(INVOVBOK)                                           
         B     EXITL               INVALID OVERRIDE BOOK                        
EXITIBKE MVC   FVMSGNO,=Y(INVBKEXP)                                             
         B     EXITL               INVALID BOOK EXPRESSION                      
EXITNMOR MVC   FVMSGNO,=AL2(NMOREPRO)                                           
         B     EXITL               NO MORE PROPOSALS FOR THIS CONTRACT          
EXITDUPL MVC   FVMSGNO,=AL2(401)                                                
         B     EXITL               DUPLICATE ENTRY NOT ALLOWED                  
EXITPBNF MVC   FVMSGNO,=AL2(598)                                                
         B     EXITL               PRIME BOOK NOT FETCHED                       
*                                                                               
DIE      DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* SETBKDMS  - SET BOOKS AND DEMOS FOR FETCH                                     
***********************************************************************         
SETBKDMS NTR1                                                                   
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
*                                                                               
         ZIC   R0,RCNBKS           BOOKS                                        
         LA    R6,RCBKS                                                         
         LA    R3,RFTCBKS                                                       
*                                                                               
STBKDM5  DS    0H                                                               
         ZIC   RE,0(R6)                                                         
         BCTR  RE,0                                                             
         MH    RE,=Y(L'MINBK)                                                   
         AR    RE,RA                                                            
         AH    RE,=Y(MINBKS-TWAD)                                               
         USING BOOKLIN,RE                                                       
         OC    BKLNUPGD,BKLNUPGD   UPGRADE?                                     
         BNZ   STBKDM10            YES - SKIP                                   
*                                                                               
         MVC   0(L'BKLNFBK,R3),BKLNFBK                                          
         LA    R3,L'RFTCBKS(R3)                                                 
         DROP  RE                                                               
*                                                                               
STBKDM10 DS    0H                                                               
         LA    R6,1(R6)                                                         
         BCT   R0,STBKDM5                                                       
*                                                                               
         ZIC   R0,RCNDMOS          DEMOS                                        
         LA    R6,RCDEMS                                                        
         LA    R3,RFTCDEMS                                                      
*                                                                               
STBKDM20 DS    0H                                                               
         ZIC   RE,0(R6)                                                         
         BCTR  RE,0                                                             
         MH    RE,=Y(L'MINDMO)                                                  
         AR    RE,RA                                                            
         AH    RE,=Y(MINDMOS-TWAD)                                              
         USING DEMOLIN,RE                                                       
*                                                                               
         MVC   0(L'DMLNDEMO,R3),DMLNDEMO                                        
         MVI   0(R3),0             ZAP FIRST BYTE - KLUGE                       
         LA    R3,L'RFTCDEMS(R3)                                                
         DROP  RE                                                               
*                                                                               
STBKDM30 DS    0H                                                               
         LA    R6,1(R6)                                                         
         BCT   R0,STBKDM20                                                      
*                                                                               
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        SETDVALS - SET DEMO VALUES                                             
*                   P1 IS A(CLUSTER HEADER ELEM)                                
*                   P2 IS A(DEMO AREA)                                          
*                   RCBKIOR CONTAINS BKLNIORD                                   
***********************************************************************         
SETDVALS NTR1                                                                   
         L     R6,0(R1)                                                         
         L     R2,4(R1)                                                         
         XC    0(NUMDEMS*4*3,R2),0(R2)                                          
*                                                                               
SDV02    DS    0H                                                               
         BAS   RE,NEXTEL           GET DEMO ELEM                                
         BNE   SDV18                                                            
*                                                                               
         USING RPRDVELD,R6                                                      
         CLC   RCBKIOR,RPRDVBNM    IS IT FOR RIGHT BOOK?                        
         BNE   SDV02               NO, SKIP                                     
*                                                                               
         XC    BOELEM,BOELEM       MOVE DEMO ELEM TO BOELEM                     
         ZIC   RF,1(R6)             (MAKES SURE INCOMPLETE                      
         BCTR  RF,0                  ELEMS DON'T CAUSE PROBLEMS)                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)                                                  
         LA    R6,BOELEM                                                        
*                                                                               
         LA    R4,RCDEMS           ACTIVE DEMO LIST                             
*                                                                               
SDV15    DS    0H                                                               
         CLI   0(R4),0             EOL                                          
         BE    SDV18                                                            
*                                                                               
         ZIC   RF,0(R4)            INTERNAL DEMO CODE IS POSITION               
         BCTR  RF,0                                                             
         MH    RF,=H'12'           NOTE- VALUES ASSUMED TO BE THERE             
*                                        (DEMO,SHARE,HUT)                       
         LA    RF,RPRDVDMO(RF)     FIRST DEMO VALUE                             
         MVC   0(12,R2),0(RF)      SAVE IN WHICHEVER DVALS                      
         LA    R2,12(R2)                                                        
         LA    R4,1(R4)                                                         
         B     SDV15                                                            
*                                                                               
SDV18    DS    0H                                                               
*                                                                               
SDVX     DS    0H                                                               
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        SETCOSTS - SET COST VALUES                                             
*                   P1  IS A(CLUSTER)                                           
*                   COSTIV1-4   INTERNAL SEQUENCE                               
*                   COSTDV1-4   DISPLAY SEQ                                     
***********************************************************************         
SETCOSTS NTR1                                                                   
         XC    COSTVALS,COSTVALS                                                
*                                  FIRST GET COSTS IN INTERNAL SEQ              
         L     R6,0(R1)                                                         
         USING RPRDTELD,R6                                                      
         MVC   COSTIV1,RPRDTSC1    COST 1 SUBMIT                                
         TM    RCCOST,RCCSUBQ                                                   
         BO    *+10                                                             
         MVC   COSTIV1,RPRDTNC1    OR NEGOTIATED                                
*                                                                               
         MVI   ELCODE,RPRCSELQ                                                  
         BAS   RE,NEXTEL           GET SUPPLEMENTAL COST ELEM                   
         BNE   SCV08                                                            
*                                                                               
         USING RPRCSELD,R6                                                      
         TM    RCCOST,RCCSUBQ      SUBMITTED                                    
         BNO   SCV06                                                            
         MVC   COSTIV2,RPRCSSC2                                                 
         MVC   COSTIV3,RPRCSSC3                                                 
         MVC   COSTIV4,RPRCSSC4                                                 
         B     SCV08                                                            
*                                                                               
SCV06    DS    0H                                                               
         MVC   COSTIV2,RPRCSNC2    NEGOTIATED                                   
         MVC   COSTIV3,RPRCSNC3                                                 
         MVC   COSTIV4,RPRCSNC4                                                 
         DROP  R6                                                               
*                                                                               
SCV08    DS    0H                  NOW SET IN DISPLAY ORDER                     
         LA    R2,RCCSTS           HAS INTERNAL VALUES IN DISP ORDER            
         LA    R3,COSTDV1                                                       
         LA    R0,NUMCSTS                                                       
*                                                                               
SCV09    DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,1,0(R2)                                                       
         BZ    SCV12                                                            
         SLL   RF,2                4 BYTES PER VALUE                            
         LA    RF,COSTIV1-4(RF)                                                 
         MVC   0(4,R3),0(RF)       SET IN DISPLAY ORDER                         
*                                                                               
SCV12    DS    0H                                                               
         LA    R3,4(R3)            NEXT DISPLAY ORDER SLOT                      
         LA    R2,1(R2)                                                         
         BCT   R0,SCV09                                                         
*                                                                               
SCVX     DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
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
         LA    R3,T2BUFF           SECOND TSAR BUFFER                           
         STCM  R1,1,TS2ACTN        REQUESTED ACTION                             
         TM    LSTSINDS,LSTSIRES   TEST TEMPEST BUFFER RESTORED                 
         BNZ   TSAR04                                                           
*                                                                               
         CLI   ASONOFF,ASOFF                                                    
         BNE   TSAR00                                                           
*                                                                               
         MVC   B2.TSABUF,TBUFFADR  SET UP A(TSAR BUFFER)                        
         MVC   B2.TSAREC,TBUFFLEN  LENGTH OF BUFFER                             
         MVI   B2.TSOFFACT,TSAINI  SET INITIALISE                               
         B     TSAR01                                                           
*                                                                               
TSAR00   DS    0H                                                               
         OI    B2.TSIND2,TSI2BUF2    USING TSAR BUFFER 2                        
         MVI   B2.TSACTN,TSAINI      SET INITIALISE                             
*                                                                               
TSAR01   DS    0H                                                               
         MVC   B2.TSACOM,ACOM         SET A(COMFACS)                            
         MVI   B2.TSINDS,TSINODSK     SET TO CORE ONLY                          
         MVI   B2.TSKEYL,T2SKEYLQ     SET KEY LENGTH                            
         NI    B2.TSRECI,FF-TSRVAR    SET FIXED                                 
         MVC   B2.TSRECL,=Y(T2SRECLQ) SET MAXIMUM RECORD LENGTH                 
*                                                                               
         CLI   TS2ACTN,TSASAV      WANT TO SAVE BEFORE RESTORING?               
         BE    TSARX                                                            
TSAR02   GOTOX ATSAR,B2.TSARD      CALL TO INITIALISE/RESTORE                   
         BNE   TSARDIE             ABEND                                        
         OI    LSTSINDS,LSTSIINI+LSTSIRES                                       
*                                                                               
TSAR04   DS    0H                                                               
         LA    R0,T2REC                                                         
         ST    R0,B2.TSAREC        SET A(RECORD)                                
*                                                                               
         CLI   ASONOFF,ASOFF       SET ACTION NUMBER                            
         BE    *+14                                                             
         MVC   B2.TSACTN,TS2ACTN                                                
         B     *+10                                                             
         MVC   B2.TSOFFACT,TS2ACTN                                              
*                                                                               
         CLI   TS2ACTN,TSAINI      EXPLICIT INITIALISE?                         
         BE    TSARX                                                            
         CLI   TS2ACTN,TSARES      EXPLICIT RESTORE?                            
         BE    TSARX                                                            
         CLI   TS2ACTN,TSASAV      SAVE?                                        
         BNE   TSAR06              NO                                           
         NI    LSTSINDS,FF-LSTSIRES                                             
         GOTOX ATSAR,B2.TSARD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TSAR06   MVC   B2.TSRNUM,T2NUM     SET TSAR NUMBER                              
         GOTOX ATSAR,B2.TSARD                                                   
         MVC   T2NUM,B2.TSRNUM     SET RECORD LIST NUMBER                       
         BE    TSAR10                                                           
*                                                                               
         CLI   TS2ACTN,TSAADD      TEST ADDING                                  
         BE    TSAR08                                                           
         CLI   TS2ACTN,TSARDH      TEST READ-HIGH/NEXT                          
         BE    TSAR08                                                           
         CLI   TS2ACTN,TSANXT                                                   
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
*                                                                               
         DROP  B2                                                               
         EJECT                                                                  
***********************************************************************         
*        SETFTCH - SET COMMON FETCH CONTROLS                                    
***********************************************************************         
SETFTCH  NTR1                                                                   
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
*                                                                               
         LR    R0,R4               CLEAR BLOCK                                  
         LA    R1,RFTBLKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   RFTACOM,ACOM                    A(COMFACS)                       
         MVC   RFTAIO1,AIO1                    A(2K IO AREA)                    
         MVC   RFTAIO2,AIO6                    A(2K IO AREA)                    
         MVC   RFTAWRK,AIO2                    A(6K WORK AREA)                  
*                                              USES AIO2,AIO3, & AIO4           
         MVC   RFTCREP,CUAALF                           REP CODE                
         MVI   RFTAMODE,RFTATXTQ           FETCH TEXT                           
         MVI   RFTCNTL,RFTCTXTQ                                                 
*                                                                               
         LR    R1,RA                                                            
         AH    R1,=Y(MINSTAS-TWAD)                                              
         LA    R0,L'MINSTAS(R1)                                                 
         USING STALIN,R1                                                        
*                                                                               
STFT06   DS    0H                                                               
         CLC   ESTATION(4),STLNSTA                                              
         BE    STFT08                                                           
*                                                                               
         LA    R1,STLNLENQ(R1)     NEXT IN TABLE                                
         CR    R1,R0                                                            
         BL    STFT06                                                           
         DC    H'0'                STATION MISSING                              
*                                                                               
STFT08   DS    0H                                                               
         MVC   RFTCSTAT(4),STLNSTA      STATION                                 
         MVI   RFTCSTAT+4,C'T'                                                  
         TM    STLNFLG,RPRSTSTL                                                 
         BZ    *+8                                                              
         MVI   RFTCSTAT+4,C'1'                                                  
         DROP  R1                                                               
*                                                                               
         MVI   RFTCSRC,C'N'                ALWAYS NEILSEN ???                   
         MVI   RFTCTXTW,FTCHWDTH           WIDTH OF FETCH RETURN                
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* UNDEMO - THIS ROUTINE WILL SMASH BOWORK1 & AIO5                               
*   INPUT   PARAMETER 1  - A(DEMOLIN)                                           
*           PARAMETER 2  - A(OUTPUT AREA)                                       
*                                                                               
***********************************************************************         
MUNDEMO  NTR1                                                                   
         L     R2,0(R1)                                                         
         USING DEMOLIN,R2                                                       
         L     R3,4(R1)                                                         
*                                                                               
         L     R6,AIO5                                                          
         USING DBLOCK,R6                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOM                                                    
         MVI   DBSELMED,C'T'                                                    
         LA    RE,BOWORK1                                                       
         XC    BOWORK1(50),BOWORK1                                              
         MVC   BOWORK1(L'DMLNDEMO),DMLNDEMO                                     
         CLI   1(RE),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(RE),C'I'                                                       
         DROP  R6                                                               
*                                                                               
         GOTO1 VDEMOCON,BODMCB,(1,BOWORK1),(9,(R3)),(0,AIO5)                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* UNBOOK - THIS ROUTINE WILL SMASH BOWORK1 & BOWORK2                            
*   INPUT   PARAMETER 1  - A(BOOKLIN)                                           
*           PARAMETER 2  - A(OUTPUT AREA)                                       
*                                                                               
***********************************************************************         
MUNBOOK  NTR1                                                                   
         L     R2,0(R1)                                                         
         USING BOOKLIN,R2                                                       
         L     R3,4(R1)                                                         
*                                                                               
         OC    BKLNUPGD,BKLNUPGD   USER PROJECTION?                             
         BZ    MUNBK6              NO                                           
*                                                                               
         ZIC   R1,BKLNIORD                                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(L'MINLBL)                                                  
         AR    R1,RA                                                            
         AH    R1,=Y(MINLBLS-TWAD)                                              
         MVC   0(L'MINLBL,R3),0(R1)                                             
         B     MUNBKX                                                           
*                                                                               
MUNBK6   XC    BOWORK2,BOWORK2                                                  
         MVI   BOWORK2,28                                                       
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(3),BKLNBK    JUST SHOW THE 1ST (PRIMARY BOOK)            
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(2),=X'0B07'      PUT OUT SOURCE                           
         MVC   BOELEM+2(1),BKLNSPBK                                             
         GOTOX (UNBOOKQ,AREPRO01),BODMCB,(1,BOWORK1),BOWORK2,          X        
               (C'L',BOELEM),(C'+',=CL6' ')                                     
*                                                                               
         ZIC   RE,BOWORK2                                                       
         LA    RE,BOWORK2(RE)                                                   
         TM    BOWORK2+1,X'02'       EXT FIELD HDR?                             
         BNE   *+8                                                              
         SH    RE,=H'8'                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         CLI   0(RE),C')'                                                       
         BNE   MUNBK8                                                           
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     MUNBK8                                                           
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
MUNBK8   DS    0H                                                               
         LA    RF,BOWORK2+8                                                     
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),BOWORK2+8                                                
*                                                                               
MUNBKX   B     EXITOK                                                           
         DROP  R2                                                               
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
* NEXTEL                                                                        
***********************************************************************         
         GETEL R6,=Y(RPROR1ST-RPROHDRD),ELCODE                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
MAXCOPY  DC    F'10'                                                            
DASHES   DS    0CL132                                                           
         DC    C'----------------------------------'                            
         DC    C'----------------------------------'                            
         DC    C'----------------------------------'                            
         DC    C'----------------------------------'                            
BITTAB   DC    X'0040201008040201'                                              
FF       EQU   X'FF'                                                            
DDSQ     EQU   FF                  DDS ONLY MARKER                              
FTCHWDTH EQU   70                  WIDTH OF FETCHED TEXT                        
BUFFLNS  EQU   70                  NUMBER OF LINES IN PRNTBUFF                  
PGMWIDQ  EQU   16                                                               
DTMWIDQ  EQU   23                                                               
*SARPGS  EQU   43                                                               
TSARPGS  EQU   86                                                               
*                                                                               
*        EQUATES FOR FORMAT CONTROLS                                            
*                                                                               
FMDTMQ   EQU   1                   DAY TIME                                     
FMDPTQ   EQU   3                   DAYPART                                      
FMPRGQ   EQU   7                   PROGRAM                                      
FMSTAQ   EQU   2                   STATION                                      
FMTAGSQ  EQU   8                   TAGS (FOR DEMO LINE DESCRIPTIONS)            
FMSPTSQ  EQU   9                   SPOTS                                        
FMCOSTQ  EQU   6                   COST                                         
FMCPPQ   EQU   5                   CPP                                          
FMDEMSQ  EQU   4                   DEMOS                                        
FMINVQ   EQU   10                  INVENTORY NUMBER                             
FMTRGTRQ EQU   11                  TRGT RATE                                    
FMBYRCPQ EQU   12                  BUYER'S CPP                                  
FMCDESCQ EQU   13                  COST DESCRIPTION                             
*                                                                               
PFKYREQ  EQU   PFK01                                                            
PFKYQAV  EQU   PFK03                                                            
PFKYLAV  EQU   PFK04                                                            
PFKYPKG  EQU   PFK05                                                            
PFKYDWNL EQU   PFK06                                                            
PFKYRUN  EQU   PFK07                                                            
PFKYRET  EQU   PFK12                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE OPTIONS                                                              
***********************************************************************         
VALOPTNS NTR1  BASE=*,LABEL=*                                                   
         MVC   AOPTFLD,FVADDR                                                   
*                                                                               
         NI    RCFLAGS1,FF-RCF1RND                                              
         TM    SAVOPTNS,OPTNDECQ   DISPLAY DEMO PRECISION?                      
         BNZ   *+8                 YES, DON'T ROUND                             
         OI    RCFLAGS1,RCF1RND    NO,ROUND                                     
*                                                                               
         MVI   RCNCBKS,0                                                        
         NI    RCFLAGS1,FF-RCF1KEEP                                             
         NI    RCFLAGS2,FF-(RCF2SPTS+RCF2NDPT+RCF2EFDT)                         
*                                                                               
         XC    FLTADDDT,FLTADDDT                                                
         XC    FLTPHCDT,FLTPHCDT                                                
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
         BE    VOPT02                                                           
         L     R1,8(R1)                                                         
         LA    R1,0(R1)                                                         
         LA    R0,FVIFLD                                                        
         SR    R1,R0                                                            
         STC   R1,FVERRNDX         INDEX OF WHERE ERROR IS                      
         B     EXITNV                                                           
*                                                                               
VOPT02   CLI   4(R1),1                                                          
         BL    EXITNV                                                           
         L     R3,AIO4             R3 = A(1ST PARSNIP FIELD)                    
         USING PSND,R3                                                          
*                                                                               
VOPT04   CLI   PSNTAG,0            ANY MORE FIELDS?                             
         BE    VOPT90              NO                                           
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
         BCTR  R1,0                                                             
*                                                                               
         CLI   STYLE,RPTCNQ        COUNT REPORT?                                
         BE    VOP40               YES                                          
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'ROUND' NOTE RCROUND IS SET EARLIER FROM             
         BNE   VOP06               HEADER IN BLDTABS                            
*                                                                               
         OI    RCFLAGS1,RCF1RND     DEFAULT  TO ROUND                           
         OC    PSNVAL,PSNVAL        ANY VALUE?                                  
         BZ    VOP50                NO                                          
         LA    R3,PSNL(R3)          BUMP TO THE NEXT FIELD                      
         OC    PSNVAL,PSNVAL                                                    
         BNZ   EXITNV                                                           
         CLI   PSNLEN,1                                                         
         BNE   EXITNV                                                           
         L     RF,PSNCOMP                                                       
         CLI   0(RF),C'Y'                                                       
         BE    VOP50                                                            
         CLI   0(RF),C'N'                                                       
         BNE   EXITNV                                                           
         NI    RCFLAGS1,FF-RCF1RND   DON'T ROUND                                
         B     VOP50                                                            
*                                                                               
VOP06    DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'SPOTS'   SPOTS MEANS DISPLAY ONLY                   
         BNE   VOP07                 LINES THAT HAVE SPOTS                      
         OI    RCFLAGS2,RCF2SPTS                                                
         OC    PSNVAL,PSNVAL        ANY VALUE?                                  
         BZ    VOP50                NO                                          
         LA    R3,PSNL(R3)          BUMP TO THE NEXT FIELD                      
         OC    PSNVAL,PSNVAL                                                    
         BNZ   EXITNV                                                           
         CLI   PSNLEN,1                                                         
         BNE   EXITNV                                                           
         L     RF,PSNCOMP                                                       
         CLI   0(RF),C'Y'                                                       
         BE    VOP50                                                            
         CLI   0(RF),C'N'                                                       
         BNE   EXITNV                                                           
         NI    RCFLAGS2,FF-RCF2SPTS                                             
         B     VOP50                                                            
*                                                                               
VOP07    DS    0H                  DEMO OVERRIDES                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'DEMOS'  NOTE- DEMOS=235 MEANS SHOW 2ND,             
         BNE   VOP08                3RD, AND 5TH (PLUS PRIME FIRST)             
*                                                                               
         OC    PSNVAL,PSNVAL        ANY VALUE?                                  
         BZ    EXITNV               NO                                          
         LA    R3,PSNL(R3)          BUMP TO THE NEXT FIELD                      
         OC    PSNVAL,PSNVAL                                                    
         BNZ   EXITNV                                                           
         CLI   PSNLEN,NUMDEMS                                                   
         BH    EXITNV                                                           
         ZIC   R1,PSNLEN            LENGTH                                      
         L     R2,PSNCOMP                                                       
*                                                                               
         XC    RCDEMS+1(L'RCDEMS-1),RCDEMS+1  CLEAR DEMOS EXCEPT PRIME          
         LA    R4,RCDEMS+1                                                      
         MVI   RCNDMOS,1           DEMO COUNTER                                 
*                                                                               
         CLI   0(R2),C'1'          PRIME MUST BE FIRST                          
         BNE   VOP07A                                                           
         LA    R2,1(R2)                                                         
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    VOP50               ONLY PRIME                                   
*                                                                               
VOP07A   DS    0H                                                               
         LA    R5,FVIFLD                                                        
         LR    RF,R2                                                            
         SR    RF,R5                                                            
         STC   RF,FVERRNDX                                                      
*                                                                               
         CLI   0(R2),C'2'                                                       
         BL    EXITNV                                                           
         CLI   0(R2),C'7'          ONLY 7 DEMOS                                 
         BH    EXITNV                                                           
*                                                                               
         MVC   BOBYTE1,0(R2)                                                    
         NI    BOBYTE1,X'0F'                                                    
         LR    R5,RA                                                            
         AH    R5,=Y(MINDMOS-TWAD)                                              
         LA    RF,L'MINDMOS(R5)                                                 
         USING DEMOLIN,R5                                                       
*                                                                               
VOP07B   DS    0H                                                               
         OC    0(L'DMLNLENQ,R5),0(R5)                                           
         BZ    VOP07C                                                           
*                                                                               
         CLC   DMLNDORD,BOBYTE1    DISPLAY ORDER MATCH?                         
         BE    VOP07D              YES                                          
VOP07C   LA    R5,DMLNLENQ(R5)                                                  
         CR    R5,RF                                                            
         BL    VOP07B                                                           
         B     EXITNV              DEMO NOT PRESENT IN THIS PROPOSAL            
*                                                                               
VOP07D   DS    0H                                                               
         LA    RE,RCDEMS           CHECK FOR REPEATS                            
         ZIC   RF,RCNDMOS          ALWAYS > 1                                   
VOP07E   DS    0H                                                               
         CLC   DMLNIORD,0(RE)      MATCH?                                       
         BE    EXITNV                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,VOP07E                                                        
*                                                                               
VOP07F   DS    0H                                                               
         MVC   0(1,R4),DMLNIORD    MOVE INTERNAL TO SLOT IN RCDEMS              
         LA    R4,1(R4)                                                         
         ZIC   RF,RCNDMOS          BUMP DEMO COUNTER                            
         LA    RF,1(RF)                                                         
         STC   RF,RCNDMOS                                                       
         DROP  R5                                                               
*                                                                               
         LA    R2,1(R2)            NEXT INPUT NUMBER                            
         BCT   R1,VOP07A                                                        
         B     VOP50                                                            
*                                                                               
VOP08    DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'CBK'    CPP= OPTION? (LIKE DEMO=)                   
         BNE   VOP09                                                            
         CLI   STYLE,RPTLAQ         LONG AVAIL?                                 
         BNE   VOP09                NO - OPTION NOT VALID                       
*                                                                               
         OC    PSNVAL,PSNVAL        ANY VALUE?                                  
         BZ    EXITNV               NO                                          
         LA    R3,PSNL(R3)          BUMP TO THE NEXT FIELD                      
         OC    PSNVAL,PSNVAL                                                    
         BNZ   EXITNV                                                           
         CLI   PSNLEN,NUMBKS                                                    
         BH    EXITNV                                                           
         ZIC   R1,PSNLEN            LENGTH                                      
         L     R2,PSNCOMP                                                       
*                                                                               
         XC    RCCBKS,RCCBKS         CLEAR CPP BOOKS                            
         MVI   RCNCBKS,0             BOOK COUNTER                               
         LA    R4,RCCBKS                                                        
*                                                                               
VOP08A   DS    0H                                                               
         LA    R5,FVIFLD                                                        
         LR    RF,R2                                                            
         SR    RF,R5                                                            
         STC   RF,FVERRNDX                                                      
*                                                                               
         CLI   0(R2),C'1'                                                       
         BL    EXITNV                                                           
         CLI   0(R2),C'7'          ONLY 7 BOOKS                                 
         BH    EXITNV                                                           
*                                                                               
         MVC   BOBYTE1,0(R2)                                                    
         NI    BOBYTE1,X'0F'                                                    
         LR    R5,RA                                                            
         AH    R5,=Y(MINBKS-TWAD)                                               
         LA    RF,L'MINBKS(R5)                                                  
         USING BOOKLIN,R5                                                       
*                                                                               
VOP08B   DS    0H                                                               
         OC    0(L'BKLNLENQ,R5),0(R5)                                           
         BZ    VOP08C                                                           
*                                                                               
         CLC   BKLNDORD,BOBYTE1    DISPLAY ORDER MATCH?                         
         BE    VOP08D              YES                                          
VOP08C   LA    R5,BKLNLENQ(R5)                                                  
         CR    R5,RF                                                            
         BL    VOP08B                                                           
         B     EXITNV              BOOK NOT PRESENT IN THIS PROPOSAL            
*                                                                               
VOP08D   DS    0H                                                               
         LA    RE,RCCBKS           CHECK FOR REPEATS                            
         ZIC   RF,RCNCBKS                                                       
         LTR   RF,RF                                                            
         BZ    VOP08F                                                           
VOP08E   DS    0H                                                               
         CLC   BKLNIORD,0(RE)      MATCH?                                       
         BE    EXITNV                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,VOP08E                                                        
*                                                                               
VOP08F   DS    0H                                                               
         MVC   0(1,R4),BKLNIORD    MOVE INTERNAL TO SLOT IN RCBKS               
         LA    R4,1(R4)                                                         
         ZIC   RF,RCNCBKS          BUMP DEMO COUNTER                            
         LA    RF,1(RF)                                                         
         STC   RF,RCNCBKS                                                       
         DROP  R5                                                               
*                                                                               
         LA    R2,1(R2)            NEXT INPUT NUMBER                            
         BCT   R1,VOP08A                                                        
         B     VOP50                                                            
*                                                                               
VOP09    DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'DPT'   SPECIAL DAYPART SORT                         
         BNE   VOP10                                                            
*                                                                               
         OC    PSNVAL,PSNVAL        ANY VALUE?                                  
         BZ    EXITNV               NO                                          
         LA    R3,PSNL(R3)          BUMP TO THE NEXT FIELD                      
         OC    PSNVAL,PSNVAL                                                    
         BNZ   EXITNV                                                           
         CLI   PSNLEN,26                                                        
         BH    EXITNV                                                           
         ZIC   R1,PSNLEN           LENGTH                                       
         L     R2,PSNCOMP                                                       
*                                                                               
         BCTR  R1,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DPTSEQS(0),0(R2)     SAVE STRING OF 1-BYTE DPTS                  
         B     VOP50                                                            
*                                                                               
VOP10    DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'KEEP'  KEEP (OR KEPT) OPTION                        
         BNE   VOP11                                                            
         OI    RCFLAGS1,RCF1KEEP                                                
         B     VOP50                                                            
*                                                                               
VOP11    DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'BK'    BK= OPTION? (LIKE DEMO=)                     
         BNE   VOP12                                                            
         CLI   STYLE,RPTLAQ        LONG AVAIL?                                  
         BNE   VOP12               NO - OPTION NOT VALID                        
*                                                                               
         OC    PSNVAL,PSNVAL        ANY VALUE?                                  
         BZ    EXITNV               NO                                          
         LA    R3,PSNL(R3)          BUMP TO THE NEXT FIELD                      
         OC    PSNVAL,PSNVAL                                                    
         BNZ   EXITNV                                                           
         CLI   PSNLEN,NUMBKS                                                    
         BH    EXITNV                                                           
         ZIC   R1,PSNLEN            LENGTH                                      
         L     R2,PSNCOMP                                                       
*                                                                               
         XC    RCBKS+1(L'RCBKS-1),RCBKS+1  CLEAR BOOKS EXCEPT PRIME             
         LA    R4,RCBKS+1                                                       
         MVI   RCNBKS,1                    BOOK COUNTER                         
*                                                                               
         CLI   0(R2),C'1'          PRIME MUST BE FIRST                          
         BNE   VOP11A                                                           
         LA    R2,1(R2)                                                         
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    VOP50               ONLY PRIME                                   
*                                                                               
VOP11A   DS    0H                                                               
         LA    R5,FVIFLD                                                        
         LR    RF,R2                                                            
         SR    RF,R5                                                            
         STC   RF,FVERRNDX                                                      
*                                                                               
         CLI   0(R2),C'2'                                                       
         BL    EXITNV                                                           
         CLI   0(R2),C'7'          ONLY 7 BOOKS                                 
         BH    EXITNV                                                           
*                                                                               
         MVC   BOBYTE1,0(R2)                                                    
         NI    BOBYTE1,X'0F'                                                    
         LR    R5,RA                                                            
         AH    R5,=Y(MINBKS-TWAD)                                               
         LA    RF,L'MINBKS(R5)                                                  
         USING BOOKLIN,R5                                                       
*                                                                               
VOP11B   DS    0H                                                               
         OC    0(L'BKLNLENQ,R5),0(R5)                                           
         BZ    VOP11C                                                           
*                                                                               
         CLC   BKLNDORD,BOBYTE1    DISPLAY ORDER MATCH?                         
         BE    VOP11D              YES                                          
VOP11C   LA    R5,BKLNLENQ(R5)                                                  
         CR    R5,RF                                                            
         BL    VOP11B                                                           
         B     EXITNV              BOOK NOT PRESENT IN THIS PROPOSAL            
*                                                                               
VOP11D   DS    0H                                                               
         LA    RE,RCBKS            CHECK FOR REPEATS                            
         ZIC   RF,RCNBKS           ALWAYS > 1                                   
VOP11E   DS    0H                                                               
         CLC   BKLNIORD,0(RE)      MATCH?                                       
         BE    EXITNV                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,VOP11E                                                        
*                                                                               
VOP11F   DS    0H                                                               
         MVC   0(1,R4),BKLNIORD    MOVE INTERNAL TO SLOT IN RCBKS               
         LA    R4,1(R4)                                                         
         ZIC   RF,RCNBKS           BUMP BOOK COUNTER                            
         LA    RF,1(RF)                                                         
         STC   RF,RCNBKS                                                        
         DROP  R5                                                               
*                                                                               
         LA    R2,1(R2)            NEXT INPUT NUMBER                            
         BCT   R1,VOP11A                                                        
         B     VOP50                                                            
*                                                                               
VOP12    DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'XDPT'  NO DAYPART COLUMN OPTION                     
         BNE   VOP13                                                            
         OI    RCFLAGS2,RCF2NDPT                                                
         B     VOP50                                                            
*                                                                               
VOP13    DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'EFFDT' SHOW EFFECTIVE DATES                         
         BNE   VOP14                                                            
         OI    RCFLAGS2,RCF2EFDT                                                
         B     VOP50                                                            
*                                                                               
VOP14    DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'ETXT'  MARKET & STATION TEXT AT END                 
         BNE   VOP15                                                            
         OI    RCFLAGS2,RCF2ETXT                                                
         B     VOP50                                                            
*                                                                               
VOP15    DS    0H                                                               
*                                                                               
         B     VOPERR                                                           
VOP40    DS    0H                  SPECIAL COUNT OPTIONS                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'ADD'   "ADD=DATE"?                                  
         BNE   VOP42               NO                                           
         CLI   INWHEN,INWNOVNT     OVERNIGHT?                                   
         BE    *+14                YES                                          
         MVC   FVMSGNO,=AL2(697)                                                
         B     EXITL                                                            
*                                                                               
         OC    PSNVAL,PSNVAL        ANY VALUE?                                  
         BZ    EXITNV               NO                                          
         LA    R3,PSNL(R3)          BUMP TO THE NEXT FIELD                      
         OC    PSNVAL,PSNVAL                                                    
         BNZ   EXITNV                                                           
*                                                                               
         GOTO1 VPERVAL,BODMCB,(PSNLEN,PSNCOMP),BOWORK2                          
         TM    4(R1),X'03'                                                      
         BNZ   EXITNV                                                           
*                                                                               
VALEDTD  USING PERVALD,BOWORK2                                                  
         TM    VALEDTD.PVALASSM,PVALASY+PVALASM+PVALASD                         
         BO    EXITNV                                                           
*                                                                               
         GOTO1 VDATCON,BODMCB,(0,VALEDTD.PVALESTA),(19,BOFULL1)                 
         GOTO1 VDATCON,BODMCB,(0,VALEDTD.PVALEEND),(19,BOFULL2)                 
*                                                                               
         MVC   FLTADDDT(3),BOFULL1                                              
*                                                                               
         TM    VALEDTD.PVALASSM,PVALAEY+PVALAEM+PVALAED                         
         BO    *+14                END DATE WAS COMPLETELY ASSUMED              
         BNZ   EXITNV              SHOULD ENTER A COMPLETE MMMDD/YY?            
         MVC   FLTADDDT+3(3),BOFULL2                                            
*                                                                               
         B     VOP50                                                            
         DROP  VALEDTD                                                          
*                                                                               
VOP42    DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'PHC'   "PHC=DATE"?                                  
         BNE   VOP44               NO                                           
         CLI   INWHEN,INWNOVNT     OVERNIGHT?                                   
         BE    *+14                YES                                          
         MVC   FVMSGNO,=AL2(697)                                                
         B     EXITL                                                            
*                                                                               
         OC    PSNVAL,PSNVAL        ANY VALUE?                                  
         BZ    EXITNV               NO                                          
         LA    R3,PSNL(R3)          BUMP TO THE NEXT FIELD                      
         OC    PSNVAL,PSNVAL                                                    
         BNZ   EXITNV                                                           
*                                                                               
         GOTO1 VPERVAL,BODMCB,(PSNLEN,PSNCOMP),BOWORK2                          
         TM    4(R1),X'03'                                                      
         BNZ   EXITNV                                                           
*                                                                               
VALEDTD  USING PERVALD,BOWORK2                                                  
         TM    VALEDTD.PVALASSM,PVALASY+PVALASM+PVALASD                         
         BO    EXITNV                                                           
*                                                                               
         GOTO1 VDATCON,BODMCB,(0,VALEDTD.PVALESTA),(19,BOFULL1)                 
         GOTO1 VDATCON,BODMCB,(0,VALEDTD.PVALEEND),(19,BOFULL2)                 
*                                                                               
         MVC   FLTPHCDT(3),BOFULL1                                              
*                                                                               
         TM    VALEDTD.PVALASSM,PVALAEY+PVALAEM+PVALAED                         
         BO    *+14                END DATE WAS COMPLETELY ASSUMED              
         BNZ   EXITNV              SHOULD ENTER A COMPLETE MMMDD/YY?            
         MVC   FLTPHCDT+3(3),BOFULL2                                            
*                                                                               
         B     VOP50                                                            
         DROP  VALEDTD                                                          
*                                                                               
VOP44    DS    0H                                                               
*                                                                               
VOPERR   DS    0H                                                               
         MVC   FVMSGNO,=AL2(INVLOPTN)   OPTION NOT AVAILABLE                    
         B     EXITL                                                            
*                                                                               
VOP50    DS    0H                                                               
         ZIC   RE,FVERRNDX         CURSOR KLUGE                                 
         ZIC   RF,PSNLEN                                                        
         LA    RE,1(RF,RE)                                                      
         STC   RE,FVERRNDX                                                      
*                                                                               
         LA    R3,PSNL(R3)         BUMP TO THE NEXT FIELD                       
         B     VOPT04                                                           
         DROP  R3                                                               
*                                                                               
VOPT90   MVI   FVERRNDX,0          NO MORE INDEX INTO FIELD NEEDED              
*                                                                               
VALOPTX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE TOTALS                                                                 
***********************************************************************         
UPDTOTS  NTR1  BASE=*,LABEL=*                                                   
         L     RE,AMELEM           NEW DAYPART?                                 
         CLC   LASTDPT,RPRDTDPT-RPRDTELD(RE)                                    
         BE    UPTOT01             NO                                           
*                                                                               
         MVC   LASTDPT,RPRDTDPT-RPRDTELD(RE)                                    
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(DPTTOTS-TWAD)                                              
         USING TOTSD,RE                                                         
         ZAP   TOTCOST1,=P'0'                                                   
         ZAP   TOTCOST2,=P'0'                                                   
         ZAP   TOTCOST3,=P'0'                                                   
         ZAP   TOTCOST4,=P'0'                                                   
         ZAP   TOTSPOTS,=P'0'                                                   
         ZAP   TOTLINES,=P'0'                                                   
         LA    RF,TOTRTGS                                                       
         ZIC   R0,NUMDEMS                                                       
*                                                                               
UPTOT00  ZAP   0(L'TOTRTGS,RF),=P'0'                                            
         LA    RF,L'TOTRTGS(RF)                                                 
         BCT   R0,UPTOT00                                                       
         DROP  RE                                                               
*                                                                               
UPTOT01  DS    0H                                                               
         OC    LSPOTS,LSPOTS       ANY SPOTS?                                   
         BZ    UPTOTX              NO - NOTHING TO DO                           
*                                                                               
         LR    R6,RA               GRAND TOTALS                                 
         AH    R6,=Y(GRDTOTS-TWAD)                                              
         USING TOTSD,R6                                                         
*                                                                               
         AP    TOTLINES,=P'1'                                                   
*                                                                               
         LH    R1,LSPOTS                                                        
         CVD   R1,BODUB1                                                        
         AP    TOTSPOTS,BODUB1                                                  
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         L     R1,LCOST1                                                        
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         AP    TOTCOST1,PCKOF16B                                                
*                                                                               
         L     R1,LCOST2                                                        
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         AP    TOTCOST2,PCKOF16B                                                
*                                                                               
         L     R1,LCOST3                                                        
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         AP    TOTCOST3,PCKOF16B                                                
*                                                                               
         L     R1,LCOST4                                                        
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         AP    TOTCOST4,PCKOF16B                                                
*                                                                               
         LA    R2,TOTRTGS                                                       
         LA    R3,LRTGS                                                         
         ZIC   R0,RCNDMOS                                                       
         LTR   R0,R0                                                            
         BZ    UPTOT20                                                          
*                                                                               
UPTOT02  DS    0H                                                               
         MVC   BOFULL1,0(R3)                                                    
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    RCFLAGS1,RCF1RND    ROUNDING?                                    
         BNO   *+16                                                             
         SRP   BODUB1,64-1,5       YES                                          
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         AP    0(L'TOTRTGS,R2),PCKOF16B                                         
         LA    R2,L'TOTRTGS(R2)                                                 
         LA    R3,L'LRTG(R3)                                                    
         BCT   R0,UPTOT02                                                       
*                                                                               
         DROP  R6                                                               
UPTOT20  DS    0H                                                               
         TM    MISCFLG1,MF1DPTSR   SORTED BY DAYPART?                           
         BZ    UPTOTX              NO - NO NEED FOR THIS THEN                   
*                                                                               
         LR    R6,RA               GRAND TOTALS                                 
         AH    R6,=Y(DPTTOTS-TWAD)                                              
         USING TOTSD,R6                                                         
*                                                                               
         AP    TOTLINES,=P'1'                                                   
*                                                                               
         LH    R1,LSPOTS                                                        
         CVD   R1,BODUB1                                                        
         AP    TOTSPOTS,BODUB1                                                  
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         L     R1,LCOST1                                                        
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         AP    TOTCOST1,PCKOF16B                                                
*                                                                               
         L     R1,LCOST2                                                        
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         AP    TOTCOST2,PCKOF16B                                                
*                                                                               
         L     R1,LCOST3                                                        
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         AP    TOTCOST3,PCKOF16B                                                
*                                                                               
         L     R1,LCOST4                                                        
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         AP    TOTCOST4,PCKOF16B                                                
*                                                                               
         LA    R2,TOTRTGS                                                       
         LA    R3,LRTGS                                                         
         ZIC   R0,RCNDMOS                                                       
         LTR   R0,R0                                                            
         BZ    UPTOT30                                                          
*                                                                               
UPTOT26  DS    0H                                                               
         MVC   BOFULL1,0(R3)                                                    
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    RCFLAGS1,RCF1RND    ROUNDING?                                    
         BNO   *+16                                                             
         SRP   BODUB1,64-1,5       YES                                          
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         AP    0(L'TOTRTGS,R2),PCKOF16B                                         
         LA    R2,L'TOTRTGS(R2)                                                 
         LA    R3,L'LRTG(R3)                                                    
         BCT   R0,UPTOT26                                                       
*                                                                               
         DROP  R6                                                               
UPTOT30  DS    0H                                                               
*                                                                               
UPTOTX   B     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT TOTALS                                                                  
*    P1 BYTE1   - TYPE OF TOTAL                                                 
*                    0 - PACKAGE                                                
*                    1 - DAYPART                                                
*                                                                               
*    P2 BYTE2-3 - A(TOTALS AREA)  DESCRIBED BY TOTSD                            
*                                                                               
*  BECAUSE THIS ROUTNINE IS FOR THE PACKAGE VIEW ONLY ONE BOOK                  
*  CAN BE HANDLED.                                                              
***********************************************************************         
PRNTTOTS NTR1  BASE=*,LABEL=*                                                   
         L     R4,0(R1)                                                         
         USING TOTSD,R4                                                         
         MVC   ATEMP1,0(R1)        KEEP THIS ARROUND                            
*                                                                               
         L     R2,APRNTBFF                                                      
         L     RE,APRNTBFF         CLEAR THE AREA TO SPACES                     
         LH    RF,=Y(BUFFLNS*132)                                               
         SR    R0,R0                                                            
         L     R1,=XL4'40000000'                                                
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,APRNTBFF                                                      
         ICM   R2,15,RTGPOS        WHERE RATINGS GO                             
         BZ    PRTOT14             NOWHERE TO PRINT THEM                        
         LA    R3,TOTRTGS                                                       
         ZIC   R5,RCNDMOS                                                       
         LTR   R5,R5                                                            
         BZ    PRTOT14             NONE TO PRINT                                
*                                                                               
PRTOT11  ZAP   PCKOF16B,0(L'TOTRTGS,R3)                                         
*                                                                               
         TM    RCFLAGS1,RCF1RND     ROUNDING?                                   
         BO    PRTOT12              YES                                         
         EDIT  PCKOF16B,(7,0(R2)),1,ZERO=NOBLANK,WRK=BOWORK1,DUB=BODUB1         
         B     PRTOT13                                                          
*                                                                               
PRTOT12  DS    0H                                                               
         SRP   PCKOF16B,64-1,5                                                  
         EDIT  PCKOF16B,(7,0(R2)),ZERO=NOBLANK,WRK=BOWORK1,DUB=BODUB1           
*                                                                               
PRTOT13  DS    0H                                                               
         LA    R3,L'TOTRTGS(R3)                                                 
         LA    R2,8(R2)                                                         
         BCT   R5,PRTOT11                                                       
*                                                                               
PRTOT14  DS    0H                  DISPLAY TOTAL SPOTS                          
         L     R2,SPTPOS                                                        
         BCTR  R2,0                                                             
         EDIT  TOTSPOTS,(6,0(R2)),ZERO=NOBLANK,WRK=BOWORK1,DUB=BODUB1           
*                                                                               
         CLI   RCNCSTS,0                                                        
         BE    PRTOT80             NONE TO PRINT                                
*                                                                               
PRTOT16  DS    0H                                                               
         OC    CSTPOS,CSTPOS       COST COLUMN?                                 
         BZ    PRTOT17             NO                                           
         OC    CPPPOS,CPPPOS       YES - IS CPP?                                
         BNZ   PRTOT20             YES                                          
         B     PRTOT30             NO                                           
*                                                                               
PRTOT17  DS    0H                  COST NOT A COLUMN                            
         OC    CPPPOS,CPPPOS       IS CPP?                                      
         BNZ   PRTOT40             YES                                          
         B     PRTOT50             NO - NEITHER HAS A COLUMN                    
*                                                                               
PRTOT20  DS    0H                  BOTH COST & CPP ARE CLUMNS                   
         L     R2,CSTPOS                                                        
         LA    R3,TOTCOST1                                                      
         LA    R6,RCCSTS                                                        
         ZIC   R5,RCNCSTS                                                       
*                                                                               
PRTOT22  DS    0H                  COST                                         
         ZAP   PCKOF16B,0(L'TOTCOST1,R3)                                        
         EDIT  PCKOF16B,(12,0(R2)),2,ZERO=NOBLANK,WRK=BOWORK1,         X        
               DUB=BODUB1                                                       
*                                                                               
         CLI   RCNCSTS,1           NEED COST DESCRIPTION?                       
         BNH   PRTOT24             NO                                           
*                                                                               
         L     RF,CDSCPOS          GET POSITION                                 
         S     RF,CSTPOS                                                        
         LA    RF,0(RF,R2)                                                      
*                                                                               
         ZIC   RE,0(R6)                                                         
         BCTR  RE,0                                                             
         MH    RE,=Y(L'MINCOST)                                                 
         AH    RE,=Y(MINCOSTS-TWAD)                                             
         AR    RE,RA                                                            
         USING CSTLIN,RE                                                        
         MVC   0(L'CSLNLBL,RF),CSLNLBL                                          
         DROP  RE                                                               
*                                                                               
PRTOT24  DS    0H                                                               
         ZAP   PCKOF08B,TOTRTGS    PRIME RATING                                 
         BAS   RE,DIVPACKD         CPP = COST / RATING                          
         BE    PRTOT26                                                          
         ZAP   PCKOF16B,=P'0'                                                   
*                                                                               
PRTOT26  DS    0H                                                               
         L     RE,CPPPOS                                                        
         S     RE,CSTPOS                                                        
         LA    RE,0(RE,R2)                                                      
         EDIT  (P16,PCKOF16B),(7,0(RE)),2,WRK=BOWORK1,                 X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
         LA    R3,L'TOTCOST1(R3)                                                
         LA    R6,1(R6)                                                         
         LA    R2,132(R2)                                                       
         BCT   R5,PRTOT22                                                       
         B     PRTOT80                                                          
*                                                                               
PRTOT30  DS    0H                  COST COLUMN ONLY                             
         L     R2,CSTPOS                                                        
         LA    R3,TOTCOST1                                                      
         LA    R6,RCCSTS                                                        
         ZIC   R5,RCNCSTS                                                       
*                                                                               
PRTOT32  DS    0H                  COST                                         
         ZAP   PCKOF16B,0(L'TOTCOST1,R3)                                        
         EDIT  PCKOF16B,(12,0(R2)),2,ZERO=NOBLANK,WRK=BOWORK1,         X        
               DUB=BODUB1                                                       
*                                                                               
         CLI   RCNCSTS,1           NEED COST DESCRIPTION?                       
         BNH   PRTOT34             NO                                           
*                                                                               
         L     RF,CDSCPOS          GET POSITION                                 
         S     RF,CSTPOS                                                        
         LA    RF,0(RF,R2)                                                      
*                                                                               
         ZIC   RE,0(R6)                                                         
         BCTR  RE,0                                                             
         MH    RE,=Y(L'MINCOST)                                                 
         AH    RE,=Y(MINCOSTS-TWAD)                                             
         AR    RE,RA                                                            
         USING CSTLIN,RE                                                        
         MVC   0(L'CSLNLBL,RF),CSLNLBL                                          
         DROP  RE                                                               
*                                                                               
PRTOT34  DS    0H                                                               
         L     RF,PGMPOS           GET POSITION                                 
         S     RF,CSTPOS                                                        
         LA    RF,0(RF,R2)                                                      
         MVC   0(5,RF),=C'CPP/M'                                                
         LA    RF,6(RF)                                                         
*                                                                               
         ZAP   PCKOF08B,TOTRTGS    PRIME RATING                                 
         BAS   RE,DIVPACKD         CPP = COST / RATING                          
         BE    PRTOT38                                                          
         ZAP   PCKOF16B,=P'0'                                                   
*                                                                               
PRTOT38  DS    0H                                                               
         EDIT  PCKOF16B,(12,0(RF)),2,WRK=BOWORK1,DUB=BODUB1,           X        
               ZERO=NOBLANK                                                     
*                                                                               
         LA    R3,L'TOTCOST1(R3)                                                
         LA    R6,1(R6)                                                         
         LA    R2,132(R2)                                                       
         BCT   R5,PRTOT32                                                       
         B     PRTOT80                                                          
*                                                                               
PRTOT40  DS    0H                  CPP COLUMN ONLY                              
         L     R2,PGMPOS                                                        
         LA    R3,TOTCOST1                                                      
         LA    R6,RCCSTS                                                        
         ZIC   R5,RCNCSTS                                                       
*                                                                               
PRTOT42  DS    0H                                                               
         MVC   0(4,R2),=C'COST'                                                 
         ZAP   PCKOF16B,0(L'TOTCOST1,R3)                                        
         EDIT  PCKOF16B,(12,6(R2)),2,ZERO=NOBLANK,WRK=BOWORK1,         X        
               DUB=BODUB1                                                       
*                                                                               
         CLI   RCNCSTS,1           NEED COST DESCRIPTION?                       
         BNH   PRTOT44             NO                                           
*                                                                               
         L     RF,CDSCPOS          GET POSITION                                 
         S     RF,PGMPOS                                                        
         LA    RF,0(RF,R2)                                                      
*                                                                               
         ZIC   RE,0(R6)                                                         
         BCTR  RE,0                                                             
         MH    RE,=Y(L'MINCOST)                                                 
         AH    RE,=Y(MINCOSTS-TWAD)                                             
         AR    RE,RA                                                            
         USING CSTLIN,RE                                                        
         MVC   0(L'CSLNLBL,RF),CSLNLBL                                          
         DROP  RE                                                               
*                                                                               
PRTOT44  DS    0H                                                               
         ZAP   PCKOF08B,TOTRTGS    PRIME RATING                                 
         BAS   RE,DIVPACKD         CPP = COST / RATING                          
         BE    PRTOT46                                                          
         ZAP   PCKOF16B,=P'0'                                                   
*                                                                               
PRTOT46  DS    0H                                                               
         L     RE,CPPPOS                                                        
         S     RE,PGMPOS                                                        
         SH    RE,=H'6'                                                         
         LA    RE,0(RE,R2)                                                      
         EDIT  (P16,PCKOF16B),(7,0(RE)),2,WRK=BOWORK1,                 X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
         LA    R3,L'TOTCOST1(R3)                                                
         LA    R6,1(R6)                                                         
         LA    R2,132(R2)                                                       
         BCT   R5,PRTOT42                                                       
         B     PRTOT80                                                          
*                                                                               
PRTOT50  DS    0H                  NEITHER COST NOR CPP COLUMN                  
         L     R2,PGMPOS                                                        
         LA    R3,TOTCOST1                                                      
         LA    R6,RCCSTS                                                        
         ZIC   R5,RCNCSTS                                                       
*                                                                               
PRTOT52  DS    0H                                                               
         MVC   0(4,R2),=C'COST'                                                 
         ZAP   PCKOF16B,0(L'TOTCOST1,R3)                                        
         EDIT  PCKOF16B,(12,6(R2)),2,ZERO=NOBLANK,WRK=BOWORK1,         X        
               DUB=BODUB1                                                       
*                                                                               
         CLI   RCNCSTS,1           NEED COST DESCRIPTION?                       
         BNH   PRTOT54             NO                                           
*                                                                               
         L     RF,CDSCPOS          GET POSITION                                 
         S     RF,PGMPOS                                                        
         LA    RF,0(RF,R2)                                                      
*                                                                               
         ZIC   RE,0(R6)                                                         
         BCTR  RE,0                                                             
         MH    RE,=Y(L'MINCOST)                                                 
         AH    RE,=Y(MINCOSTS-TWAD)                                             
         AR    RE,RA                                                            
         USING CSTLIN,RE                                                        
         MVC   0(L'CSLNLBL,RF),CSLNLBL                                          
         DROP  RE                                                               
*                                                                               
PRTOT54  DS    0H                                                               
         LA    R2,132(R2)                                                       
         MVC   1(6,R2),=C'-CPP/M'                                               
*                                                                               
         ZAP   PCKOF08B,TOTRTGS    PRIME RATING                                 
         BAS   RE,DIVPACKD         CPP = COST / RATING                          
         BE    PRTOT56                                                          
         ZAP   PCKOF16B,=P'0'                                                   
*                                                                               
PRTOT56  DS    0H                                                               
         EDIT  (P16,PCKOF16B),(10,8(R2)),2,WRK=BOWORK1,                X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
         LA    R3,L'TOTCOST1(R3)                                                
         LA    R6,1(R6)                                                         
         LA    R2,132(R2)                                                       
         BCT   R5,PRTOT52                                                       
         B     PRTOT80                                                          
*                                                                               
PRTOT80  DS    0H                                                               
         L     R1,AREP             SKIP A LINE                                  
         GOTO1 VREPORT,(R1)                                                     
*                                                                               
         SR    R4,R4                                                            
         L     RF,APRNTBFF                                                      
*                                                                               
PRTOT82  DS    0H                  CALCULATE BOX LENGTH                         
         LR    R1,RF                                                            
         LA    RF,131(RF)                                                       
PRTOT84  DS    0H                                                               
         CLI   0(RF),C' '                                                       
         BH    PRTOT86                                                          
         BCTR  RF,0                                                             
         CR    RF,R1                                                            
         BH    PRTOT84                                                          
         DC    H'0'                HOW DID THIS HAPPEN?                         
*                                                                               
PRTOT86  DS    0H                                                               
         SR    RF,R1                                                            
         CR    RF,R4                                                            
         BL    *+6                                                              
         LR    R4,RF                                                            
*                                                                               
         LA    RF,132(R1)          NEXT PRINT LINE                              
         CLC   0(132,RF),BCSPACES  ANYTHING TO CHECK?                           
         BH    PRTOT82             YES                                          
*                                                                               
         L     R3,AREP                                                          
         USING REPD,R3                                                          
         LA    R2,REPP1                                                         
         MVI   9(R2),C'*'                                                       
         SH    R4,=H'8'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   10(0,R2),9(R2)                                                   
*                                                                               
         CLI   ATEMP1,0                                                         
         BNE   PRTOT90                                                          
         MVC   11(17,R2),=C' PACKAGE TOTALS '                                   
         B     PRTOT100                                                         
*                                                                               
PRTOT90  DS    0H                                                               
         CLI   ATEMP1,1                                                         
         BE    *+6                                                              
         DC    H'0'                EH?                                          
*                                                                               
         L     R6,AMELEM                                                        
         MVC   11(20,R2),=C' DAYPART ??? TOTALS '                               
         MVC   BOBYTE1,LASTDPT                                                  
         GOTO1 =A(DPTOUT),RR=BORELO                                             
         MVC   20(3,R2),BOFULL1                                                 
*                                                                               
PRTOT100 DS    0H                                                               
         MVI   REPALLN,6                                                        
         L     R1,AREP             PRINT TOP OF BOX                             
         GOTO1 VREPORT,(R1)                                                     
         MVI   REPALLN,0                                                        
*                                                                               
         L     R2,APRNTBFF                                                      
PRTOT102 DS    0H                                                               
         CLC   BCSPACES,0(R2)                                                   
         BNL   PRTOT104                                                         
         MVC   REPP1,0(R2)                                                      
         MVI   REPP1+9,C'*'                                                     
         LA    RE,REPP1(R4)                                                     
         MVI   10(RE),C'*'                                                      
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
         LA    R2,132(R2)                                                       
         B     PRTOT102                                                         
*                                                                               
PRTOT104 DS    0H                                                               
         LA    R2,REPP1                                                         
         MVI   9(R2),C'*'                                                       
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   10(0,R2),9(R2)                                                   
*                                                                               
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*                                                                               
         CLI   ATEMP1,0            PACKAGE TOTALS?                              
         BE    PRTOTX              YES - DON'T SKIP A LINE                      
*                                                                               
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*                                                                               
PRTOTX   DS    0H                                                               
         B     EXITOK                                                           
         DROP  R4,R3                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READS THE BOOK AND DEMO ELEMENTS INTO SAVBKS, SAVLBLS, SAVDMOS                
*                                       MINBKS, MINLBLS, MINDMOS                
***********************************************************************         
         DS    0D                                                               
RDBKSDMS NTR1  BASE=*,LABEL=*                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
***************                                                                 
* DESCRIPTION ELEMENT                                                           
***************                                                                 
         MVI   SAVOPTNS,0                                                       
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
         MVC   DESCFLGS,RPRDSFLG                                                
         MVC   ECONLENS,BCSPACES                                                
*                                                                               
         LA    RE,ECONLENS                                                      
         LA    RF,RPRDSSEC                                                      
         OC    0(L'RPRDSSEC,RF),0(RF)                                           
         BZ    RDBDDSX                                                          
*                                                                               
         LA    R0,NUMLENS          MAX OF 6 LENGTHS IN CONTRACT                 
RDBDDS10 CLI   0(RF),0                ANY MORE LENGTHS?                         
         BE    RDBDDS30                                                         
*                                                                               
         ZIC   R1,0(RF)            SHOW THE LENGTH                              
         CVD   R1,BODUB1                                                        
         UNPK  0(3,RE),BODUB1                                                   
         OI    2(RE),X'F0'                                                      
*                                                                               
         CLI   0(RE),C'0'          2 CHAR LENGTH?                               
         BNE   RDBDDS20                                                         
         MVC   0(1,RE),1(RE)       YES                                          
         MVC   1(1,RE),2(RE)                                                    
         LA    RE,2(RE)                                                         
         B     *+8                                                              
*                                                                               
RDBDDS20 LA    RE,3(RE)            NO, 3 CHAR LENGTH                            
         MVI   0(RE),C','                                                       
*                                                                               
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,RDBDDS10         SHOW ALL THE LENGTHS                         
*                                                                               
RDBDDS30 BCTR  RE,0                REMOVE THE LAST COMMA                        
         MVI   0(RE),C' '                                                       
*                                                                               
RDBDDSX  DS    0H                                                               
***************                                                                 
* BOOK ELEMENT(S)                                                               
***************                                                                 
         LR    RE,RA                                                            
         AH    RE,=Y(MINBKS-TWAD)                                               
         LA    RF,L'MINBKS                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINLBL-TWAD)                                               
         LA    RF,L'MINLBLS                                                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
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
         ZIC   R1,RPRBKIOR         INTERNAL ORDER NUMBER                        
         BCTR  R1,0                                                             
         LR    RE,R1                                                            
         MH    R1,=Y(L'MINBK)                                                   
         LA    R1,0(RA,R1)                                                      
         AH    R1,=Y(MINBKS-TWAD)                                               
         USING BOOKLIN,R1                                                       
*                                                                               
         CLI   RPRBKLEN,RPRBKOVQ   USER DEFINED BOOK?                           
         BH    RDBDBK20            YES                                          
*********                                                                       
* REGULAR BOOK                                                                  
*********                                                                       
         MVC   BKLNIORD,RPRBKIOR   INTERNAL ORDER #                             
         MVC   BKLNDORD,RPRBKDOR   DISPLAY ORDER #                              
         MVC   BKLNFLG,RPRBKFLG    FLAGS                                        
         MVC   BKLNBK,RPRBKSTT     BOOK                                         
         MVC   BKLNSPBK,RPRBKBKT   SPECIAL BOOK TYPE                            
         MVC   BKLNFIL,RPRBKFIL    BOOK SOURCE(I/T/P/4)                         
         B     RDBDBK50                                                         
*********                                                                       
* USER-DEFINED BOOK                                                             
*********                                                                       
RDBDBK20 DS    0H                                                               
         MVC   BKLNIORD,RPRBKIOR   INTERNAL ORDER #                             
         MVC   BKLNDORD,RPRBKDOR   DISPLAY ORDER #                              
         MVC   BKLNFLG,RPRBKFLG    FLAGS                                        
         MVC   BKLNBK,RPRBKSTT     BOOK                                         
         MVC   BKLNSPBK,RPRBKBKT   SPECIAL BOOK TYPE                            
         MVC   BKLNFIL,RPRBKFIL    BOOK SOURCE(I/T/P/4)                         
         MVC   BKLNUPGD,RPRBKBKS   UPGRADE FORMULA                              
         MVC   BKLNXBKS,RPRBKXBK   EXTRA BASE BOOKS                             
         DROP  R1                                                               
*                                                                               
         MH    RE,=Y(L'MINLBL)                                                  
         LA    RE,0(RA,RE)                                                      
         AH    RE,=Y(MINLBL-TWAD)                                               
         MVC   0(L'MINLBL,RE),RPRBKUDF  SAVE THE LABEL                          
*                                                                               
RDBDBK50 BAS   RE,MINIOSEQ                                                      
         BE    RDBDBK10                                                         
*                                                                               
RDBDBKX  DS    0H                                                               
***************                                                                 
* DEMO ELEMENT(S)                                                               
***************                                                                 
         LR    RE,RA                                                            
         AH    RE,=Y(MINDMOS-TWAD)                                              
         LA    RF,L'MINDMOS                                                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
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
         ZIC   R1,RPRDMIOR         INTERNAL ORDER NUMBER                        
         BCTR  R1,0                                                             
         MH    R1,=Y(L'MINDMO)                                                  
         LA    R1,0(RA,R1)                                                      
         AH    R1,=Y(MINDMOS-TWAD)                                              
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
RDBDDMX  DS    0H                                                               
         DROP  R6                                                               
***************                                                                 
* DAYPART ELEMENT(S)                                                            
***************                                                                 
         LR    RE,RA                                                            
         AH    RE,=Y(MINDPTS-TWAD)                                              
         LR    R2,RE                                                            
         USING DPTLIN,R2                                                        
         LA    RF,L'MINDPTS                                                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDPELQ    GET THE DAYPART ELEMENT                      
         BAS   RE,MINIOHI                                                       
         BNE   RDBDDPX                                                          
*                                                                               
RDBDDP10 L     R6,MINELEM                                                       
         USING RPRDPELD,R6                                                      
*                                                                               
         MVC   DPLNDPT,RPRDPDPT                                                 
         MVC   DPLNFLG,RPRDPFLG                                                 
         MVC   DPLNCPP,RPRDPTAB                                                 
         DROP  R2                                                               
*                                                                               
         LA    R2,L'MINDPT(R2)                                                  
         BAS   RE,MINIOSEQ                                                      
         BNE   RDBDDPX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         CLI   0(R6),RPRDPELQ      DAYPART ELEMENT STILL?                       
         BE    RDBDDP10                                                         
*                                                                               
RDBDDPX  DS    0H                                                               
***************                                                                 
* STATION ELEMENT(S)                                                            
***************                                                                 
         LR    RE,RA                                                            
         AH    RE,=Y(MINSTAS-TWAD)                                              
         LA    RF,L'MINSTAS                                                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
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
         MH    R1,=Y(L'MINSTA)                                                  
         LA    R1,0(RA,R1)                                                      
         AH    R1,=Y(MINSTAS-TWAD)                                              
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
RDBDSTX  DS    0H                                                               
         DROP  R6                                                               
*                                                                               
*********                                                                       
* COSTS *                                                                       
*********                                                                       
         LR    RE,RA                                                            
         AH    RE,=Y(MINCOSTS-TWAD)                                             
         LA    RF,L'MINCOSTS                                                    
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R1,1                                                             
         LR    RE,RA                                                            
         AH    RE,=Y(MINCOSTS-TWAD)                                             
         LA    RF,L'MINCOSTS(RE)                                                
         USING CSTLIN,RE                                                        
RDBDCS02 DS    0H                                                               
         STC   R1,CSLNIORD                                                      
         MVC   CSLNLBL(4),=C'COST'                                              
         STC   R1,CSLNLBL+4                                                     
         OI    CSLNLBL+4,C'0'                                                   
         LA    R1,1(R1)                                                         
         LA    RE,CSLNLENQ(RE)                                                  
         CR    RE,RF                                                            
         BL    RDBDCS02                                                         
         SR    R1,R1                                                            
         DROP  RE                                                               
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRCHELQ    GET THE COST HEADER                          
         BAS   RE,MINIOHI                                                       
         BNE   RDBDCSX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRCHELD,R6                                                      
*                                                                               
RDBDCS10 CLI   0(R6),RPRCHELQ                                                   
         BNE   RDBDCSX                                                          
*                                                                               
         ZIC   R1,RPRCHSEQ         INTERNAL ORDER NUMBER                        
         BCTR  R1,0                                                             
         MH    R1,=Y(L'MINCOST)                                                 
         LA    R1,0(RA,R1)                                                      
         AH    R1,=Y(MINCOSTS-TWAD)                                             
         USING CSTLIN,R1                                                        
*                                                                               
         MVC   CSLNIORD,RPRCHSEQ                                                
         MVC   CSLNLBK,RPRCHBK                                                  
*                                                                               
         XC    CSLNLBL,CSLNLBL                                                  
*                                                                               
         ZIC   RE,RPRCHLEN                                                      
         SH    RE,=Y(RPRCHOVQ+1)                                                
         LTR   RE,RE                                                            
         BNP   RDBDCS12                                                         
         CH    RE,=Y(L'CSLNLBL-1)                                               
         BNH   *+8                                                              
         LA    RE,(L'CSLNLBL-1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CSLNLBL(0),RPRCHLBL                                              
         DROP  R1                                                               
*                                                                               
RDBDCS12 BAS   RE,MINIOSEQ                                                      
         BE    RDBDCS10                                                         
*                                                                               
RDBDCSX  DS    0H                                                               
         DROP  R6,R5                                                            
****************************                                                    
* INITIALIZE REPORT VALUES *                                                    
****************************                                                    
         XC    RCBKS,RCBKS         INLITIALZE BOOK LIST                         
         MVI   RCNBKS,0                                                         
         SR    RE,RE                                                            
         LR    R5,RA                                                            
         AH    R5,=Y(MINBKS-TWAD)                                               
         LA    RF,L'MINBKS(R5)                                                  
         USING BOOKLIN,R5                                                       
*                                                                               
RDBDRB2  OC    0(BKLNLENQ,R5),0(R5)                                             
         BZ    RDBDRB4                                                          
*                                                                               
         LA    RE,1(RE)                                                         
         ZIC   R1,BKLNDORD                                                      
         BCTR  R1,0                                                             
         LA    R1,RCBKS(R1)                                                     
         MVC   0(1,R1),BKLNIORD                                                 
*                                                                               
RDBDRB4  LA    R5,BKLNLENQ(R5)                                                  
         CR    R5,RF                                                            
         BL    RDBDRB2                                                          
         STC   RE,RCNBKS                                                        
         DROP  R5                                                               
*                                                                               
         CLI   STYLE,RPTLAQ        LONG AVAIL  LOOPBUG                          
         BE    RDBDRBX             YES                                          
         MVI   RCNBKS,1            NO, EVERYTHING ELSE HAS 1 BOOK               
*                                                                               
RDBDRBX  DS    0H                                                               
*                                                                               
         XC    RCDEMS,RCDEMS                                                    
         MVI   RCNDMOS,0                                                        
         SR    RE,RE                                                            
         LR    R5,RA                                                            
         AH    R5,=Y(MINDMOS-TWAD)                                              
         LA    RF,L'MINDMOS(R5)                                                 
         USING DEMOLIN,R5                                                       
*                                                                               
RDBDRD2  OC    0(DMLNLENQ,R5),0(R5)                                             
         BZ    RDBDRD4                                                          
*                                                                               
         LA    RE,1(RE)                                                         
         ZIC   R1,DMLNDORD                                                      
         BCTR  R1,0                                                             
         LA    R1,RCDEMS(R1)                                                    
         MVC   0(1,R1),DMLNIORD                                                 
*                                                                               
RDBDRD4  LA    R5,DMLNLENQ(R5)                                                  
         CR    R5,RF                                                            
         BL    RDBDRD2                                                          
         STC   RE,RCNDMOS                                                       
         DROP  R5                                                               
*                                                                               
         CLI   STYLE,RPTLAQ        LONG AVAIL                                   
         BE    RDBDRDX             YES                                          
         MVI   RCNDMOS,1           NO, EVERYTHING DEFAULTS TO 1 DEMO            
*                                                                               
RDBDRDX  DS    0H                                                               
*                                                                               
         XC    DPTSEQS,DPTSEQS                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(MINDPTS-TWAD)                                              
         LA    RF,L'MINDPTS(RE)                                                 
         USING DPTLIN,RE                                                        
         LA    R5,DPTSEQS                                                       
*                                                                               
RDBDRDP2 MVC   0(1,R5),DPLNDPT                                                  
         LA    R5,1(R5)                                                         
         LA    RE,DPLNLENQ(RE)                                                  
         CR    RE,RF                                                            
         BL    RDBDRDP2                                                         
         DROP  RE                                                               
*                                                                               
RDBKDMX  B     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SETFMT - SET FORMAT CONTROLS                                           
*                                                                               
*        NOTE- THIS ROUTINE REALLY NEEDS TO BE BUTTONED UP                      
*                                                                               
***********************************************************************         
SETFMT   NTR1  BASE=*,LABEL=*                                                   
         XC    FMTCTL,FMTCTL                                                    
         LA    R2,FMTCTL                                                        
         MVI   BOBYTE1,0                                                        
*                                                                               
         TM    RCFLAGS1,RCF1INV    SHOW INVENTORY FIRST?                        
         BNO   *+12                NO                                           
         MVI   0(R2),FMINVQ        SHOW INV #                                   
         LA    R2,1(R2)                                                         
*                                                                               
         MVI   0(R2),FMDTMQ        DAY/TIME                                     
         MVI   1(R2),FMPRGQ        PROGRAM                                      
         LA    R2,2(R2)                                                         
*                                                                               
         CLI   RCNDPTS,1           FITLERING ON 1 DAYPART?                      
         BE    SFMT01              YES                                          
         TM    RCFLAGS2,RCF2NDPT   SHOWING DAYPART?                             
         BNZ   SFMT01              NO                                           
         MVI   0(R2),FMDPTQ        SHOW DAYPART                                 
         LA    R2,1(R2)                                                         
*                                                                               
SFMT01   TM    RCFLAGS1,RCF1COMP   INCLUDE COMP STATIONS?                       
         BNO   *+12                NO                                           
         MVI   0(R2),FMSTAQ        SHOW STATION                                 
         LA    R2,1(R2)                                                         
*                                                                               
         CLI   STYLE,RPTPKQ        PACKAGE?                                     
         BNE   *+12                NO                                           
         MVI   0(R2),FMSPTSQ       SHOW SPOTS                                   
         LA    R2,1(R2)                                                         
*                                                                               
         SR    RF,RF               DO WE NEED TAGS FOR DEMO LINES?              
         CLI   RCNBKS,1            IF MORE THAN 1 BOOK                          
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         TM    RCCPP,RCCPSTKQ      OR CPPS FOR ALL DEMOS                        
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         TM    RCFLAGS1,RCF1SHR    OR SHARES                                    
         BNO   *+8                                                              
         LA    RF,1(RF)                                                         
         TM    RCFLAGS1,RCF1PUT    OR PUTS                                      
         BNO   *+8                                                              
         LA    RF,1(RF)                                                         
*                                                                               
         STC   RF,RCNTAGS                                                       
         LTR   RF,RF               ANY TAGS?                                    
         BNP   *+12                NO                                           
         MVI   0(R2),FMTAGSQ       YES - TAGS FOR DEMO LINES                    
         LA    R2,1(R2)                                                         
*                                                                               
         MVI   0(R2),FMDEMSQ       DEMOS                                        
         LA    R2,1(R2)                                                         
*                                                                               
         TM    RCCOST,RCCCOLQ      COST AS A COLUMN?                            
         BZ    *+16                NO                                           
         MVI   0(R2),FMCOSTQ       COST                                         
         LA    R2,1(R2)                                                         
         OI    BOBYTE1,X'80'       SET MAY NEED COST DESCRIPTION                
*                                                                               
         TM    RCCPP,RCCPCOLQ      CPP COLUMN ONLY IF SHOWING                   
         BZ    *+16                PRIME DEMO CPP                               
         MVI   0(R2),FMCPPQ        CPP                                          
         LA    R2,1(R2)                                                         
         OI    BOBYTE1,X'80'       SET MAY NEED COST DESCRIPTION                
*                                                                               
         TM    RCCPP,RCPBYRQ       BUYERS CPP AS COLUMN                         
         BZ    *+12                PRIME DEMO CPP OR BUYER'S                    
         MVI   0(R2),FMBYRCPQ      BUYERS CPP                                   
         LA    R2,1(R2)                                                         
*                                                                               
         TM    RCCOST,RCCTRGQ      TRGT RATE                                    
         BZ    *+12                                                             
         MVI   0(R2),FMTRGTRQ                                                   
         LA    R2,1(R2)                                                         
*                                                                               
         CLI   STYLE,RPTPKQ        PACKAGE?                                     
         BE    *+12                YES                                          
         TM    BOBYTE1,X'80'       IF HAD COST OR CPP COLS                      
         BZ    SFMT04                                                           
         CLI   RCNCSTS,1           AND HAVE  MORE THAN ONE COST TYPE            
         BNH   SFMT04                                                           
         MVI   0(R2),FMCDESCQ      NEED COST DESCRIPTION                        
         LA    R2,1(R2)                                                         
*                                                                               
SFMT04   DS    0H                                                               
         MVI   0(R2),X'FF'         EOL                                          
*                                  CALCULATE REPORT WIDTH                       
         OI    HKHFLGS,HKHCALQ     SET HOOK ROUTINE TO CALC WIDTH               
         GOTO1 =A(HEDHOOK),RR=BORELO                                            
         NI    HKHFLGS,FF-HKHCALQ     RESET TO NORMAL                           
*                                                                               
         CLC   MYRWDTH,RPTWID      TOO WIDE?                                    
         BH    *+14                NO                                           
         MVC   FVMSGNO,=AL2(557)                                                
         B     EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DPTOUT - GET 3-BYTE DPT CODE                                           
*                 INPUT IN BOBYTE1, OUTPUT IN BOFULL1(3)                        
***********************************************************************         
         DS    0D                                                               
DPTOUT   NTR1  BASE=*,LABEL=*                                                   
         OI    BOBYTE1,C' '        UPPERCASE                                    
*                                                                               
DPTOUT10 DS    0H                                                               
         TM    CCONFLG1,CCONDPMQ   USES HARDCODED TABLE?                        
         BO    DPTOUT60            YES                                          
*                                                                               
****************************************                                        
** READ DAYPART RECORD FOR VALIDATION **                                        
****************************************                                        
*                                                                               
         MVC   BODMCB(2),CUAALF         GET PARENT REP                          
         GOTOX (GETPRNT,AREPRO01),BODMCB                                        
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RRDPKEY,RE                                                       
         MVI   RRDPKTYP,RRDPKIDQ                                                
         MVC   RRDPKREP,BODMCB                                                  
         MVC   RRDPKDPT,BOBYTE1                                                 
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RRDPKEY),IOKEYSAV                                        
         BE    *+16                                                             
         L     RE,=A(DPTUKNX)                                                   
         A     RE,BORELO                                                        
         B     DPTOUT70                                                         
*                                                                               
         L     R1,=AL4(XOREPFIL+XOGET+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,AIO4                                                          
         LA    RE,RRDPELEM-RRDPREC(RE)                                          
         CLI   0(RE),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                LOST DAYPART ELEMENT                         
         USING RRDPELEM,RE                                                      
         MVC   BOFULL1(3),RRDPSNAM                                              
         DROP  RE                                                               
*                                                                               
         L     RE,=A(DPTTABLE)                                                  
         A     RE,BORELO                                                        
         B     DPTOUTX                                                          
*                                                                               
*************************************************                               
** READ HARDCODED DAYPART TABLE FOR VALIDATION **                               
*************************************************                               
DPTOUT60 DS    0H                  VALIDATE THE DAYPART CODE                    
         L     RE,=A(DPTTABLE)                                                  
         A     RE,BORELO                                                        
*                                                                               
DPTOUT65 CLI   0(RE),X'FF'         DID WE HIT THE END OF DPTTABLE?              
         BE    DPTOUT70            YES, INVALID DAYPART CODE                    
         CLC   BOBYTE1,0(RE)                                                    
         BE    DPTOUT70                                                         
         LA    RE,L'DPTTABLE(RE)                                                
         B     DPTOUT65                                                         
*                                                                               
DPTOUT70 DS    0H                                                               
         MVC   BOFULL1(3),1(RE)                                                 
*                                                                               
DPTOUTX  DS    0H                                                               
         CLI   0(RE),X'FF'                                                      
         BE    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT THE REPORT                                                              
***********************************************************************         
T2       USING T2SLST,T2LST                                                     
T2I      USING T2ILST,T2LST                                                     
         DS    0D                                                               
PREP     NTR1  BASE=*,LABEL=*                                                   
         L     R3,AREP                                                          
         USING REPD,R3                                                          
********************                                                            
* INTIALIZE REPORT *                                                            
********************                                                            
         CLI   ASONOFF,ASOFF                                                    
         BNE   PREPI00                                                          
*                                                                               
         LA    R0,TSARPGS          GET OFFLINE TSAR                             
         SLL   R0,12                                                            
*                                                                               
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                GETMAIN FAILED RC IN (RF)                    
                                                                                
         STCM  R1,15,TBUFFADR      SAVE UP A(TSAR BUFFER)                       
         STCM  R0,15,TBUFFLEN      SAVE LENGTH OF BUFFER                        
*                                                                               
PREPI00  L     R0,=A(PRNTBUFF)                                                  
         A     R0,BORELO                                                        
         ST    R0,APRNTBFF                                                      
*                                                                               
         L     RE,=A(REPHOOKS)                                                  
         A     RE,BORELO                                                        
         ST    RE,REPAUSR                                                       
         MVI   REPHEADH,1                                                       
*                                                                               
         MVC   REPSUBPG,MYRWDTH                                                 
*                                                                               
         LR    RE,RA               CLEAR TOTALS                                 
         AH    RE,=Y(GRDTOTS-TWAD)                                              
         USING TOTSD,RE                                                         
         ZAP   TOTCOST1,=P'0'                                                   
         ZAP   TOTCOST2,=P'0'                                                   
         ZAP   TOTCOST3,=P'0'                                                   
         ZAP   TOTCOST4,=P'0'                                                   
         ZAP   TOTSPOTS,=P'0'                                                   
         ZAP   TOTLINES,=P'0'                                                   
         LA    RF,TOTRTGS                                                       
         ZIC   R0,NUMDEMS                                                       
*                                                                               
PREPI02  ZAP   0(L'TOTRTGS,RF),=P'0'                                            
         LA    RF,L'TOTRTGS(RF)                                                 
         BCT   R0,PREPI02                                                       
         DROP  RE                                                               
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(DPTTOTS-TWAD)                                              
         USING TOTSD,RE                                                         
         ZAP   TOTCOST1,=P'0'                                                   
         ZAP   TOTCOST2,=P'0'                                                   
         ZAP   TOTCOST3,=P'0'                                                   
         ZAP   TOTCOST4,=P'0'                                                   
         ZAP   TOTSPOTS,=P'0'                                                   
         ZAP   TOTLINES,=P'0'                                                   
         LA    RF,TOTRTGS                                                       
         ZIC   R0,NUMDEMS                                                       
*                                                                               
PREPI04  ZAP   0(L'TOTRTGS,RF),=P'0'                                            
         LA    RF,L'TOTRTGS(RF)                                                 
         BCT   R0,PREPI04                                                       
         DROP  RE                                                               
*                                                                               
         MVI   LASTDPT,0                                                        
         NI    MISCFLG1,FF-MF1LINES    SET NO LINES                             
*                                                                               
*******************                                                             
* GENERATE REPORT *                                                             
*******************                                                             
         OI    HKHFLGS,HKHBLNKQ    BLANK PAGE                                   
*                                                                               
         TM    RCFLAGS2,RCF2COVR   COVER PAGE?                                  
         BNO   PREPCVRX            NO                                           
         GOTO1 =A(COVERPG),RR=BORELO                                            
         MVI   REPFHEAD,C'Y'                                                    
PREPCVRX DS    0H                                                               
         NI    HKHFLGS,FF-HKHBLNKQ    BLANK PAGE                                
*                                                                               
         TM    RCFLAGS2,RCF2ETXT                                                
         BO    PREPTXTX                                                         
*                                                                               
         OI    HKHFLGS,HKHNCOLQ       NO COLUMN HEADINGS                        
*                                                                               
         GOTO1 =A(MKTCOMS),RR=BORELO  MARKET COMMENTS                           
         GOTO1 =A(STACOMS),RR=BORELO  STATION COMMENTS                          
*                                                                               
         NI    HKHFLGS,FF-HKHNCOLQ                                              
         MVI   REPFHEAD,C'Y'                                                    
PREPTXTX DS    0H                                                               
*                                                                               
*******************                                                             
* DETAIL LINES    *                                                             
*******************                                                             
         XC    T2LST,T2LST         INITIALIZE QQQ                               
         GOTOX TSAR2,TSAINI                                                     
*                                                                               
         XC    LINECNTR,LINECNTR   BUILD TABLES                                 
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ                                                 
         BAS   RE,MINIOHI                                                       
         BNE   PREPX               MINIO HIGH IS UNHAPPY                        
         B     PREP00B                                                          
*                                                                               
PREP00   BAS   RE,MINIOSEQ                                                      
         BNE   PREP10A                                                          
*                                                                               
PREP00B  DS    0H                                                               
         GOTO1 =A(BLDSKEY),BODMCB,RR=BORELO                                     
         BNE   PREP00                                                           
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         XC    T2LST,T2LST         ADD REPORT KEY                               
         MVI   T2.T2SKTYP,T2SKTYPQ                                              
         MVC   T2.T2SKYCON,SORTKEY                                              
         MVI   T2.T2SMNKEY,RPRDTELQ                                             
         MVC   T2.T2SMNKEY+1(L'T2SMNKEY-1),2(R6)                                
         GOTOX TSAR2,TSAADD                                                     
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(624)                                                
         B     EXITL                                                            
*                                                                               
         XC    T2LST,T2LST         ADD INVETORY NUMBER KEY                      
         MVI   T2I.T2IKTYP,T2IKTYPQ                                             
         MVC   T2I.T2IKSTA#,RPRDTSTA                                            
         MVC   T2I.T2IKINV#,RPRDTINM                                            
         MVC   T2SVKEY,T2KEY                                                    
         GOTOX TSAR2,TSARDH                                                     
         BE    PREP02                                                           
*                                                                               
         XC    T2LST,T2LST                                                      
         MVC   T2KEY(L'T2SVKEY),T2SVKEY                                         
         GOTOX TSAR2,TSAADD                                                     
         BNL   PREP04                                                           
         MVC   FVMSGNO,=AL2(624)                                                
         B     EXITL                                                            
*                                                                               
PREP02   DS    0H                  INVENTORY NUMBER EXISTS SET FLAG             
         OI    T2I.T2IFLGS,T2IF1REP                                             
         GOTOX TSAR2,TSAWRT                                                     
*                                                                               
PREP04   DS    0H                                                               
         B     PREP00                                                           
         DROP  R6                                                               
*                                                                               
PREP10A  OC    LINECNTR,LINECNTR   PROCESS SORT TABLE                           
         BZ    PREPX               NO LINES ADDED                               
*                                                                               
         XC    T2SVKEY,T2SVKEY                                                  
         XC    T2LST,T2LST                                                      
         MVI   T2.T2SKTYP,T2SKTYPQ                                              
         GOTOX TSAR2,TSARDH                                                     
**       BNL   PREPX     ***  READHI RETURNS FUNNY THINGS                       
         B     PREP10B                                                          
*                                                                               
PREP10   DS    0H                                                               
         GOTOX TSAR2,TSANXT                                                     
         BNE   PREP90                                                           
*                                                                               
PREP10B  DS    0H                                                               
         CLI   T2.T2SKTYP,T2SKTYPQ                                              
         BNE   PREP90                                                           
*                                                                               
         MVC   T2SVKEY,T2KEY                                                    
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'T2SMNKEY),T2.T2SMNKEY                                  
         MVC   SVMINKY,MINEKEY                                                  
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         CLI   0(R6),RPRDTELQ      GOT A DETAIL DESCRIPTION ELEM?               
         BNE   PREP10              NO                                           
         CLC   2(L'T2SMNKEY-1,R6),T2.T2SMNKEY+1                                 
         BNE   PREP10                                                           
*                                                                               
         CLI   LASTDPT,0                                                        
         BNE   *+10                                                             
         MVC   LASTDPT,RPRDTDPT                                                 
*                                                                               
         ST    R6,AMELEM                                                        
         NI    MISCFLG1,FF-MF1INVRP                                             
*                                                                               
         XC    T2LST,T2LST         GET INVENTORY KEY                            
         MVI   T2I.T2IKTYP,T2IKTYPQ                                             
         MVC   T2I.T2IKSTA#,RPRDTSTA                                            
         MVC   T2I.T2IKINV#,RPRDTINM                                            
         GOTOX TSAR2,TSARDH                                                     
         BE    *+6                 SHOULD HAVE BEEN THERE                       
         DC    H'0'                                                             
*                                                                               
         TM    T2I.T2IFLGS,T2IF1REP                                             
         BZ    *+8                                                              
         OI    MISCFLG1,MF1INVRP                                                
*                                                                               
         MVC   T2KEY(L'T2SVKEY),T2SVKEY                                         
         GOTOX TSAR2,TSARDH                                                     
         BE    *+6                 SHOULD HAVE BEEN THERE                       
         DC    H'0'                                                             
*                                                                               
         CLI   STYLE,RPTPKQ        PACKAGE REPORT?                              
         BNE   PREP12              NO                                           
*                                                                               
         TM    MISCFLG1,MF1DPTSR   SORTED BY DAYPART?                           
         BZ    PREP12              NO - DON'T BOTHER WITH DPTTOTS               
*                                                                               
         CLC   RPRDTDPT,LASTDPT    NEW DAYPART?                                 
         BE    PREP12              NO                                           
*                                                                               
         LR    R0,RA               PRINT DAYPART TOTALS                         
         AH    R0,=Y(DPTTOTS-TWAD)                                              
         GOTO1 =A(PRNTTOTS),BODMCB,(1,(R0)),RR=BORELO                           
*                                                                               
PREP12   DS    0H                  CHECK IF WE SHOULD GET DEMO DATA             
         CLI   STYLE,RPTLAQ        LONG AVAIL?                                  
         BNE   PREP13              NO - DON'T GET DEMOS                         
         TM    CUSTAT,CUSDDS       DDS TERMINAL?                                
         BNZ   *+12                YES - GET DEMOS                              
         CLI   ASONOFF,ASOFF       RUNNING OFFLINE?                             
         BNE   PREP13              NO - DON'T GET DEMOS                         
*                                                                               
         LR    R2,RA                                                            
         AH    R2,=Y(MINSTAS-TWAD)                                              
         MVC   ELPARMS(2),MINELEML                                              
         MVC   ELPARMS+2(4),AMELEM                                              
         GOTOX (DTLNFTQ,AREPRO01),BODMCB,(R2),                         X        
               (X'C0',MINBKS-MINSTAS(R2)),(0,MINDMOS-MINSTAS(R2)),     X        
               (C'R',ELPARMS)                                                   
*                                                                               
PREP13   DS    0H                                                               
         GOTO1 =A(PRNTLIN),RR=BORELO                                            
         OI    MISCFLG1,MF1LINES    SET LINES                                   
*                                                                               
         CLI   STYLE,RPTPKQ        PACKAGE REPORT?                              
         BNE   PREP20              NO                                           
*                                                                               
         GOTO1 =A(UPDTOTS),RR=BORELO                                            
*                                                                               
PREP20   DS    0H                                                               
         B     PREP10                                                           
         DROP  R6                                                               
*                                                                               
PREP90   DS    0H                                                               
         CLI   STYLE,RPTPKQ        PACKAGE REPORT?                              
         BNE   PREP100             NO                                           
         TM    MISCFLG1,MF1LINES   LINES?                                       
         BZ    PREP100             NO                                           
*                                                                               
         TM    MISCFLG1,MF1DPTSR   SORTED BY DAYPART?                           
         BZ    PREP92              NO - DON'T BOTHER WITH DPTTOTS               
*                                                                               
         LR    R0,RA               PRINT DAYPART TOTALS                         
         AH    R0,=Y(DPTTOTS-TWAD)                                              
         GOTO1 =A(PRNTTOTS),BODMCB,(1,(R0)),RR=BORELO                           
*                                                                               
PREP92   DS    0H                                                               
         LR    R0,RA               PRINT PACKAGE TOTALS                         
         AH    R0,=Y(GRDTOTS-TWAD)                                              
         GOTO1 =A(PRNTTOTS),BODMCB,(0,(R0)),RR=BORELO                           
*                                                                               
PREP100  DS    0H                                                               
*                                                                               
PREPX    DS    0H                                                               
         TM    RCFLAGS2,RCF2ETXT                                                
         BNO   PREPETXX                                                         
*                                                                               
         MVI   REPFHEAD,C'Y'                                                    
         OI    HKHFLGS,HKHNCOLQ       NO COLUMN HEADINGS                        
*                                                                               
         GOTO1 =A(MKTCOMS),RR=BORELO  MARKET COMMENTS                           
         GOTO1 =A(STACOMS),RR=BORELO  STATION COMMENTS                          
*                                                                               
         NI    HKHFLGS,FF-HKHNCOLQ                                              
PREPETXX DS    0H                                                               
         OC    TBUFFADR,TBUFFADR                                                
         BZ    EXITOK                                                           
*                                                                               
         L     R1,TBUFFADR                                                      
         LH    R0,TBUFFLEN                                                      
         FREEMAIN RC,A=(1),LV=(0)                                               
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                FREEMAIN FAILED RC IN (RF)                   
         DROP  R5,R3,T2,T2I                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BLDSCTL - BUILD SORT CONTROL IN SORTCTL (CODE,CUME LEN,ADDR)           
*                  SORTREQ HAS SELECTED SORT OPTIONS                            
***********************************************************************         
         DS    0D                                                               
BLDSCTL  NTR1  BASE=*,LABEL=*                                                   
         XC    SORTCTL,SORTCTL                                                  
         NI    RCFLAGS1,FF-RCF1DPTL   DPT TOTALS SWITCH                         
         LA    R2,SORTCTL                                                       
         LA    R3,SORTREQ                                                       
         SR    R0,R0                                                            
*                                                                               
BSC04    DS    0H                                                               
         CLI   0(R3),X'FF'         END OF SORT CODES                            
         BE    BSC20                                                            
         CLI   0(R3),0             SKIP 0'S                                     
         BE    BSC12                                                            
*                                                                               
         CLI   0(R3),3             IF DAYPART SORT                              
         BNE   BSC05                                                            
         CLI   STYLE,RPTPKQ       AND PACKAGE REQ                               
         BNE   BSC05                                                            
         OI    RCFLAGS1,RCF1DPTL   DPT TOTALS SWITCH                            
*                                                                               
BSC05    DS    0H                                                               
         L     RF,=A(BSTTAB)                                                    
         A     RF,BORELO                                                        
*                                                                               
BSC06    DS    0H                                                               
         CLI   0(RF),X'FF'         EOL                                          
         BNE   *+6                                                              
         DC    H'0'                SHOULD NOT HAPPEN                            
         CLC   0(1,R3),1(RF)                                                    
         BE    BSC10                                                            
         LA    RF,2(RF)            NEXT TABLE ENTRY                             
         B     BSC06                                                            
*                                                                               
BSC10    DS    0H                                                               
         ZIC   R1,0(RF)            LENGTH OF FIELD                              
         AR    R0,R1               CUMULATIVE LENGTH                            
*                                                                               
         MVC   1(1,R2),1(RF)       MOVE ENTRY (WITHOUT LENGTH)                  
         STC   R0,2(R2)            AND SET CUMULATIVE LENGTH IN NEXT            
*                                  (WHERE THE DATA WILL GO)                     
         LA    R2,2(R2)            NEXT OUTPUT                                  
*                                                                               
BSC12    DS    0H                                                               
         LA    R3,1(R3)            NEXT SORTREQ                                 
         B     BSC04                                                            
*                                                                               
BSC20    DS    0H                                                               
         CLI   0(R2),L'SORTKEY     SAVE SORT KEY LENGTH                         
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(634)                                                
         B     EXITL                                                            
         MVI   0(R2),X'FF'         SET EOL                                      
*                                                                               
BSCX     DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT A COVERPG                                                               
***********************************************************************         
         DS    0D                                                               
COVERPG  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AREP                                                          
         USING REPD,R6                                                          
*                                                                               
         CLI   SELPROFS,RREPQSEL                                                
         BE    CVR00                                                            
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
CVR00    DS    0H                                                               
         ZIC   R0,REPMAXL                                                       
         SH    R0,=H'10'           SUBTRACT LINES WE USE                        
         TM    MISCFLG1,MF1TMPBT   DOUBLE LINES?                                
         BZ    *+8                 NO                                           
         SH    R0,=H'10'                                                        
*                                                                               
         SH    R0,=H'20'           SUBTRACT 10 HEADLINES                        
         BNP   CVR04                                                            
         SRL   R0,1                                                             
         LTR   R0,R0                                                            
         BZ    CVR04                                                            
CVR02    DS    0H                                                               
         MVI   REPP1,0                                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
         BCT   R0,CVR02            VERT. CENTERING LENGTH                       
*                                                                               
CVR04    DS    0H                                                               
         ZIC   R2,MYRWDTH          HORZ. CENTERING WIDTH                        
*                                  HORZ. RULE                                   
         MVI   REPP1,BORDCHAR                                                   
         MVC   REPP1+1(RTMARGIN),REPP1                                          
         OC    REPP1,BCSPACES                                                   
         GOTO1 CENTER,BODMCB,REPP1,(R2)                                         
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*--------------------------------  STATION/MARKET                               
         LA    R3,REPP1                                                         
         TM    SELPROF+SELDBCVB,SELDBCVA     DOUBLE SPACE COVERSHEET?           
         BZ    *+12                          NO                                 
         BAS   R4,DOSPACE          <-- NOTE R4!!!!!!!!                          
         LA    R3,REPP2                                                         
*                                                                               
         MVC   1(L'ESTATION,R3),ESTATION                                        
         LA    RE,1+L'ESTATION-1(R3)                                            
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         MVI   1(RE),C'/'                                                       
         MVC   2(L'EMKTNAME,RE),EMKTNAME                                        
         OC    0(L'REPP1,R3),BCSPACES                                           
         LA    R0,RTMARGIN-1                                                    
         GOTO1 CENTER,BODMCB,1(R3),(R0)                                         
*                                                                               
         MVI   0(R3),BORDCHAR                                                   
         MVI   RTMARGIN(R3),BORDCHAR                                            
         GOTO1 CENTER,BODMCB,(R3),(R2)                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*--------------------------------  AGENCY NAME                                  
         LA    R3,REPP1                                                         
         TM    SELPROF+SELDBCVB,SELDBCVA     DOUBLE SPACE COVERSHEET?           
         BZ    *+12                          NO                                 
         BAS   R4,DOSPACE          <-- NOTE R4!!!!!!!!                          
         LA    R3,REPP2                                                         
*                                                                               
         MVI   0(R3),BORDCHAR                                                   
         MVC   LBLCOLMN(6,R3),=C'AGENCY'                                        
         MVC   VALCOLMN(L'EAGYNAM1,R3),EAGYNAM1                                 
         MVI   RTMARGIN(R3),BORDCHAR                                            
         OC    0(L'REPP1,R3),BCSPACES                                           
         GOTO1 CENTER,BODMCB,(R3),(R2)                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*--------------------------------  BUYER NAME                                   
         LA    R3,REPP1                                                         
         TM    SELPROF+SELDBCVB,SELDBCVA     DOUBLE SPACE COVERSHEET?           
         BZ    *+12                          NO                                 
         BAS   R4,DOSPACE          <-- NOTE R4!!!!!!!!                          
         LA    R3,REPP2                                                         
*                                                                               
         MVI   0(R3),BORDCHAR                                                   
         MVC   LBLCOLMN(5,R3),=C'BUYER'                                         
         MVC   VALCOLMN(L'ECONBUYR,R3),ECONBUYR                                 
         MVI   RTMARGIN(R3),BORDCHAR                                            
         OC    0(L'REPP1,R3),BCSPACES                                           
         GOTO1 CENTER,BODMCB,(R3),(R2)                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*--------------------------------  ADVERTISER NAME                              
         LA    R3,REPP1                                                         
         TM    SELPROF+SELDBCVB,SELDBCVA     DOUBLE SPACE COVERSHEET?           
         BZ    *+12                          NO                                 
         BAS   R4,DOSPACE          <-- NOTE R4!!!!!!!!                          
         LA    R3,REPP2                                                         
*                                                                               
         MVI   0(R3),BORDCHAR                                                   
         MVC   LBLCOLMN(10,R3),=C'ADVERTISER'                                   
         MVC   VALCOLMN(L'EADVNAME,R3),EADVNAME                                 
         MVI   RTMARGIN(R3),BORDCHAR                                            
         OC    0(L'REPP1,R3),BCSPACES                                           
         GOTO1 CENTER,BODMCB,(R3),(R2)                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*--------------------------------  PRODUCT NAME                                 
         LA    R3,REPP1                                                         
         TM    SELPROF+SELDBCVB,SELDBCVA     DOUBLE SPACE COVERSHEET?           
         BZ    *+12                          NO                                 
         BAS   R4,DOSPACE          <-- NOTE R4!!!!!!!!                          
         LA    R3,REPP2                                                         
*                                                                               
         MVI   0(R3),BORDCHAR                                                   
         MVC   LBLCOLMN(7,R3),=C'PRODUCT'                                       
         MVC   VALCOLMN(L'EPRDNAME,R3),EPRDNAME                                 
         MVI   RTMARGIN(R3),BORDCHAR                                            
         OC    0(L'REPP1,R3),BCSPACES                                           
         GOTO1 CENTER,BODMCB,(R3),(R2)                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*--------------------------------  FLIGHT DATE                                  
         LA    R3,REPP1                                                         
         TM    SELPROF+SELDBCVB,SELDBCVA     DOUBLE SPACE COVERSHEET?           
         BZ    *+12                          NO                                 
         BAS   R4,DOSPACE          <-- NOTE R4!!!!!!!!                          
         LA    R3,REPP2                                                         
*                                                                               
         MVI   0(R3),BORDCHAR                                                   
         MVC   LBLCOLMN(12,R3),=C'FLIGHT DATES'                                 
         MVC   VALCOLMN(L'ECONDATE,R3),ECONDATE                                 
         MVI   RTMARGIN(R3),BORDCHAR                                            
         OC    0(L'REPP1,R3),BCSPACES                                           
         GOTO1 CENTER,BODMCB,(R3),(R2)                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*--------------------------------  SALESPERSON NAME                             
         LA    R3,REPP1                                                         
         TM    SELPROF+SELDBCVB,SELDBCVA     DOUBLE SPACE COVERSHEET?           
         BZ    *+12                          NO                                 
         BAS   R4,DOSPACE          <-- NOTE R4!!!!!!!!                          
         LA    R3,REPP2                                                         
*                                                                               
         MVI   0(R3),BORDCHAR                                                   
         MVC   LBLCOLMN(11,R3),=C'SALESPERSON'                                  
         MVC   VALCOLMN(L'ESALNAME,R3),ESALNAME                                 
         MVI   RTMARGIN(R3),BORDCHAR                                            
         OC    0(L'REPP1,R3),BCSPACES                                           
         GOTO1 CENTER,BODMCB,(R3),(R2)                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*--------------------------------  TELEPHONE #                                  
         LA    R3,REPP1                                                         
         TM    SELPROF+SELDBCVB,SELDBCVA     DOUBLE SPACE COVERSHEET?           
         BZ    *+12                          NO                                 
         BAS   R4,DOSPACE          <-- NOTE R4!!!!!!!!                          
         LA    R3,REPP2                                                         
*                                                                               
         MVI   0(R3),BORDCHAR                                                   
         MVC   LBLCOLMN(9,R3),=C'TELEPHONE'                                     
         MVC   VALCOLMN(L'ESALTEL,R3),ESALTEL                                   
         MVI   RTMARGIN(R3),BORDCHAR                                            
         OC    0(L'REPP1,R3),BCSPACES                                           
         GOTO1 CENTER,BODMCB,(R3),(R2)                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*--------------------------------  REF#                                         
         LA    R3,REPP1                                                         
         TM    SELPROF+SELDBCVB,SELDBCVA     DOUBLE SPACE COVERSHEET?           
         BZ    *+12                          NO                                 
         BAS   R4,DOSPACE          <-- NOTE R4!!!!!!!!                          
         LA    R3,REPP2                                                         
*                                                                               
         MVI   0(R3),BORDCHAR                                                   
         MVI   RTMARGIN(R3),BORDCHAR                                            
         MVC   LBLCOLMN(L'REFNUMLB,R3),REFNUMLB                                 
         MVC   VALCOLMN(L'ECONNUM,R3),ECONNUM                                   
         LA    RE,VALCOLMN+L'ECONNUM-1(R3)                                      
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(L'EPRONUM,RE),EPRONUM                                          
         LA    RE,VALCOLMN+L'ECONNUM+L'EPRONUM+4(R3)                            
*++++++++++++++++++++++++++++++++  RATING SOURCE                                
         MVC   0(3,RE),=C'???'                                                  
         CLI   CSOURCE,C'N'        NSI?                                         
         BNE   *+10                                                             
         MVC   0(3,RE),=C'NSI'                                                  
*++++++++++++++++++++++++++++++++  TODAYS DATE                                  
         LA    RE,6(RE)                                                         
         LR    R0,RE                                                            
         GOTO1 VDATCON,BODMCB,(5,0),(11,(R0))                                   
         OC    0(L'REPP1,R3),BCSPACES                                           
         GOTO1 CENTER,BODMCB,(R3),(R2)                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*--------------------------------  HORZ. RULE                                   
         LA    R3,REPP1                                                         
         TM    SELPROF+SELDBCVB,SELDBCVA     DOUBLE SPACE COVERSHEET?           
         BZ    *+12                          NO                                 
         BAS   R4,DOSPACE          <-- NOTE R4!!!!!!!!                          
         LA    R3,REPP2                                                         
*                                                                               
         MVI   0(R3),BORDCHAR                                                   
         MVC   1(RTMARGIN,R3),0(R3)                                             
         OC    0(L'REPP1,R3),BCSPACES                                           
         GOTO1 CENTER,BODMCB,(R3),(R2)                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*--------------------------------                                               
         B     EXITOK                                                           
         SPACE 2                                                                
DOSPACE  DS    0H                  DO BLANK COVERSHEET LINE                     
         MVI   0(R3),BORDCHAR                                                   
         MVI   RTMARGIN(R3),BORDCHAR                                            
         GOTO1 CENTER,BODMCB,(R3),(R2)                                          
         BR    R4                  <-- NOTE R4!!!!!!!!!!!!!!!!!!!!!!            
*                                                                               
         SPACE 2                                                                
         DROP  R6                                                               
BORDCHAR EQU   C'*'                THE BOX                                      
LBLCOLMN EQU   02                                                               
VALCOLMN EQU   20                                                               
RTMARGIN EQU   50                                                               
REFNUMLB DC    C'REF#'                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LINE PRINTING                                                                 
***********************************************************************         
PRNTLIN  NTR1  BASE=*,LABEL=*                                                   
         L     R0,APRNTBFF                                                      
         ST    R0,APRNTLOC                                                      
         LH    R1,=Y(BUFFLNS*132)                                               
         SR    RE,RE                                                            
         L     RF,=XL4'40000000'                                                
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,AMELEM                                                        
         USING RPRDTELD,R6                                                      
         LA    R4,FMTCTL                                                        
*                                                                               
         MVI   RGTLNS,0            CLEAR RIGHT AND LEFT LINE COUNTERS           
         MVI   LFTLNS,0                                                         
         XC    SVDPOS,SVDPOS       CLEAR SAVED COLUMN POSITIONS                 
*                                                                               
         GOTO1 =A(SETCOSTS),BODMCB,(R6),RR=BORELO                               
*                                                                               
PRTL02   DS    0H                                                               
         CLI   0(R4),X'FF'         END OF FMT CONTROLS                          
         BE    PRTL10                                                           
         CLI   0(R4),0             SKIP EMPTIES                                 
         BE    PRTL03                                                           
*                                                                               
         ZIC   RF,0(R4)                                                         
         SLL   RF,2                                                             
         LA    RF,PRTLBR-4(RF)                                                  
         BASR  RE,RF                                                            
*                                                                               
PRTL03   DS    0H                                                               
         LA    R4,1(R4)            NEXT                                         
         B     PRTL02                                                           
*                                                                               
PRTL10   DS    0H                  DEMOS FOR EACH BOOK                          
*                                  NOW FLUSH LINE BUFFER                        
         GOTO1 =A(PRTDEMS),RR=BORELO                                            
*                                  NOW FLUSH LINE BUFFER                        
         ZAP   BODUB1,PCKOF06B     PACKED LINE COUNTER => XL1                   
         CVB   R0,BODUB1                                                        
         STC   R0,RGTLNS                                                        
*                                  NOW FLUSH LINE BUFFER                        
         ZIC   R0,LFTLNS          USE GREATER OF LEFT OR RIGHT                  
         CLC   LFTLNS,RGTLNS      LINE COUNTS FOR BCT                           
         BNL   *+8                                                              
         IC    R0,RGTLNS                                                        
         LTR   R0,R0                                                            
         BP    *+8                                                              
         LA    R0,1                                                             
*                                                                               
         CLM   R0,3,=Y(BUFFLNS)    SCREWY COUNT?                                
         BNH   *+8                 NO                                           
         LA    R0,BUFFLNS                                                       
*                                                                               
         L     R3,AREP                                                          
         USING REPD,R3                                                          
         L     R2,APRNTBFF                                                      
         STC   R0,REPALLN         ALLOWLIN LEAVE ROOM FOR WHOLE BLOCK           
*                                                                               
PRTL12   DS    0H                                                               
         MVC   REPP1,0(R2)                                                      
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
         MVI   REPALLN,0                                                        
         LA    R2,132(R2)                                                       
         BCT   R0,PRTL12                                                        
         DROP  R3                                                               
*                                                                               
         BAS   RE,USRCOM           GET USER COMMENTS                            
         BAS   RE,INVCOM           GET INVENTORY COMMENTS                       
*                                                                               
         TM    RCFLAGS2,RCF2DBSP   DOUBLE SPACE?                                
         BO    PRTL13                                                           
         CLI   RCNCSTS,1           IF MULTI-COSTS                               
         BNH   PRTL14                                                           
         CLI   RCNBKS,1            AND NOT MULTI-BOOKS                          
         BH    PRTL14               SPACE AFTER                                 
PRTL13   DS    0H                                                               
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*                                                                               
PRTL14   DS    0H                  SAVE VALUES FOR THIS LINE                    
         XC    LINVALS(LNVALSLQ),LINVALS                                        
         MVC   LSPOTS,RPRDTTSP   SPOTS                                          
*                                                                               
         XC    LCOST1,LCOST1                                                    
         TM    COSTDV1,X'80'       N/A?                                         
         BNZ   *+10                YES                                          
         MVC   LCOST1,COSTDV1                                                   
*                                                                               
         XC    LCOST2,LCOST2                                                    
         TM    COSTDV2,X'80'       N/A?                                         
         BNZ   *+10                YES                                          
         MVC   LCOST2,COSTDV2                                                   
*                                                                               
         XC    LCOST3,LCOST3                                                    
         TM    COSTDV3,X'80'       N/A?                                         
         BNZ   *+10                YES                                          
         MVC   LCOST3,COSTDV3                                                   
*                                                                               
         XC    LCOST4,LCOST4                                                    
         TM    COSTDV4,X'80'       N/A?                                         
         BNZ   *+10                YES                                          
         MVC   LCOST4,COSTDV4                                                   
*                                                                               
         LA    R2,SUDVALS          SUBMIT RATINGS                               
         TM    RCRTG,RCRSUBQ                                                    
         BO    *+8                                                              
         LA    R2,NGDVALS          OR NEGOTIATED                                
         OC    NGDVALS,NGDVALS     IF NO NEG'D                                  
         BNZ   *+8                                                              
         LA    R2,SUDVALS          DEFAULT TO SUBMITTED                         
         LA    R3,LRTGS                                                         
         ZIC   R5,RCNDMOS                                                       
         LTR   R5,R5                                                            
         BZ    PRTL18              NO DEMOS?, DONE                              
*                                                                               
PRTL16   DS    0H                                                               
         MVC   0(L'LRTG,R3),0(R2)                                               
         LA    R2,12(R2)            NEXT DEMO                                   
         LA    R3,L'LRTG(R3)                                                    
         BCT   R5,PRTL16                                                        
*                                                                               
PRTL18   DS    0H                                                               
*                                                                               
PRTLX    DS    0H                                                               
         B     EXITOK                                                           
         DROP  R6                                                               
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
PRTLBR   DS    0F                  FMTLIN BRANCH TABLE                          
         B     PRDAYTM             1                                            
         B     PRSTATN             2                                            
         B     PRDAYPT             3                                            
         B     PRDEMOS             4                                            
         B     PRCPP               5                                            
         B     PRCOST              6                                            
         B     PRPROG              7                                            
         B     PRTAGS              8                                            
         B     PRSPOTS             9                                            
         B     PRINVN              10                                           
         B     PRTRGTR             11                                           
         B     PRBYRCP             12                                           
         B     PRCDESC             13                                           
         EJECT                                                                  
**********************************************************************          
* PRINT THE DAY TIMES                                                           
**********************************************************************          
PRDAYTM  NTR1                                                                   
         L     R2,APRNTLOC         ADVANCE PRINT POSITION                       
         LA    R0,DTMWIDQ+1(R2)                                                 
         ST    R0,APRNTLOC                                                      
*                                                                               
         MVI   TMPLNS,1                                                         
         L     R6,AMELEM                                                        
         MVI   ELCODE,RPRAVELQ     AVAIL DAY/TIMES?                             
         BAS   RE,NEXTEL                                                        
         BE    PRDTAV00            YES                                          
*                                                                               
**********************                                                          
* STANDARD DAY/TIMES *                                                          
**********************                                                          
         L     R6,AMELEM                                                        
         USING RPRDTELD,R6                                                      
         GOTO1 VDAYUNPK,BODMCB,RPRDTDAY,(R2)     FIRST DAY/TIME                 
         GOTO1 VUNTIME,BODMCB,RPRDTSTM,12(R2)                                   
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,RPRDYELQ     NOW GET SECONDARY DAY/TIME LINES             
         BAS   RE,NEXTEL                                                        
         BNE   PRDT20                                                           
*                                                                               
         ZIC   R1,1(R6)            CALC NUMBER OF ENTRIES                       
         SH    R1,=Y(RPRDYOVQ)                                                  
         SR    R0,R0                                                            
         D     R0,=F'5'            5 BYTES PER ENTRY                            
         LA    R6,RPRDYDTM-RPRDYELD(R6)                                         
         LR    R0,R1               COUNT FOR BCT                                
*                                                                               
PRDT10   DS    0H                  FORMAT DAY/TIME                              
         LA    R2,132(R2)                                                       
         GOTO1 VDAYUNPK,BODMCB,0(R6),(R2)                                       
         GOTO1 VUNTIME,BODMCB,1(R6),12(R2)                                      
*                                                                               
         ZIC   RE,TMPLNS           COUNT LINES                                  
         LA    RE,1(RE)                                                         
         STC   RE,TMPLNS                                                        
*                                                                               
         LA    R6,5(R6)            NEXT ENTRY                                   
         BCT   R0,PRDT10                                                        
*                                                                               
PRDT20   DS    0H                                                               
         B     PRDTX                                                            
*                                                                               
*******************                                                             
* AVAIL DAY/TIMES *                                                             
*******************                                                             
PRDTAV00 DS    0H                                                               
         ZIC   R1,1(R6)            CALC NUMBER OF ENTRIES                       
         SH    R1,=Y(RPRAVOVQ)                                                  
         SR    R0,R0                                                            
         D     R0,=AL4(L'RPRAVALS)                                              
         LA    R6,RPRAVALS-RPRAVELD(R6)                                         
         LR    R0,R1               COUNT FOR BCT                                
         BCTR  R0,0                SKIPPING FIRST ENTRY                         
*                                                                               
         MVC   0(11,R2),0(R6)                                                   
         MVC   12(11,R2),L'RPRAVDAY(R6)                                         
         LTR   R0,R0                                                            
         BNP   PRDTAV20                                                         
*                                                                               
PRDTAV10 DS    0H                  FORMAT DAY/TIME                              
         LA    R6,L'RPRAVALS(R6)   NEXT ENTRY                                   
         LA    R2,132(R2)          NEXT LINE                                    
*                                                                               
         MVC   0(11,R2),0(R6)                                                   
         MVC   12(11,R2),L'RPRAVDAY(R6)                                         
*                                                                               
         ZIC   RE,TMPLNS           COUNT LINES                                  
         LA    RE,1(RE)                                                         
         STC   RE,TMPLNS                                                        
*                                                                               
         BCT   R0,PRDTAV10                                                      
*                                                                               
PRDTAV20 DS    0H                                                               
*                                                                               
PRDTX    DS    0H                                                               
         CLC   LFTLNS,TMPLNS   USE LARGER OF LINES USED SO FAR                  
         BNL   *+10            AND USED HERE FOR DAY/TIME                       
         MVC   LFTLNS,TMPLNS                                                    
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT THE DAYPART IN LONG FORM                                                
**********************************************************************          
PRDAYPT  NTR1                                                                   
         L     R2,APRNTLOC         ADVANCE PRINT POSITION                       
         LA    R0,4(R2)                                                         
         ST    R0,APRNTLOC                                                      
*                                                                               
         L     R6,AMELEM                                                        
         USING RPRDTELD,R6                                                      
         MVC   BOBYTE1,RPRDTDPT                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(DPTOUT),RR=BORELO                                             
         MVC   0(3,R2),BOFULL1                                                  
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT THE PROGRAM NAME                                                        
**********************************************************************          
PRPROG   NTR1                                                                   
         L     R2,APRNTLOC         ADVANCE PRINT POSITION                       
         LA    R0,PGMWIDQ+1(R2)                                                 
         ST    R0,APRNTLOC                                                      
*                                                                               
         ST    R2,PGMPOS           SAVE PROGRAM POSITION                        
         MVI   TMPLNS,0                                                         
         L     R6,AMELEM                                                        
         USING RPRDTELD,R6                                                      
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,RPRDTPRG                                                    
         GOTO1 =A(GETTEXT),BODMCB,('RPRTXPRQ',(R4)),RR=BORELO                   
         ZIC   R4,0(R1)            TEXT LENGTH                                  
         LTR   R4,R4                                                            
         BZ    PRPRG20                                                          
         DROP  R6                                                               
*                                                                               
         GOTO1 CHOPPER,BODMCB,((R4),AIO1),('PGMWIDQ',0(R2)),(C'P',4)            
         ZIC   RF,11(R1)           BUMP LINE COUNTER                            
         ZIC   RE,TMPLNS                                                        
         AR    RE,RF                                                            
         STC   RE,TMPLNS                                                        
*                                                                               
         ZIC   RF,11(R1)           BUMP DOWN TO NEXT LINE                       
         MH    RF,=Y(132)                                                       
         AR    R2,RF                                                            
*                                                                               
         MVI   ELCODE,RPRPRELQ     NOW GET SECONDARY PROGRAM LINES              
         BAS   RE,NEXTEL                                                        
         BNE   PRPRG20                                                          
*                                                                               
         ZIC   R0,1(R6)            CALC NUMBER OF ENTRIES                       
         SH    R0,=Y(RPRPROVQ)                                                  
         SRL   R0,1                2 BYTES PER ENTRY                            
         LA    R6,RPRPRPRG-RPRPRELD(R6)                                         
*                                                                               
PRPRG10  DS    0H                                                               
         SR    R4,R4                                                            
         ICM   R4,3,0(R6)                                                       
         GOTO1 =A(GETTEXT),BODMCB,('RPRTXPRQ',(R4)),RR=BORELO                   
         ZIC   R4,0(R1)            TEXT LENGTH                                  
         LTR   R4,R4                                                            
         BZ    PRPRG14                                                          
*                                                                               
         GOTO1 CHOPPER,BODMCB,((R4),AIO1),('PGMWIDQ',0(R2)),(C'P',4)            
         ZIC   RF,11(R1)           BUMP LINE COUNTER                            
         ZIC   RE,TMPLNS                                                        
         AR    RE,RF                                                            
         STC   RE,TMPLNS                                                        
*                                                                               
         ZIC   RF,11(R1)           BUMP DOWN TO NEXT LINE                       
         MH    RF,=Y(132)                                                       
         AR    R2,RF                                                            
*                                                                               
PRPRG14  DS    0H                                                               
         LA    R6,2(R6)            NEXT ENTRY                                   
         BCT   R0,PRPRG10                                                       
*                                                                               
PRPRG20  DS    0H                                                               
         L     R6,AMELEM           PRINT INVENTORY EFFECTIVE DATE               
         USING RPRDTELD,R6                                                      
         TM    RCFLAGS2,RCF2EFDT                                                
         BNZ   *+12                                                             
         TM    MISCFLG1,MF1INVRP  INVENTORY NUMBER USED ELSEWHERE?              
         BZ    PRPRGIX            NO                                            
*                                                                               
         OC    RPRDTEFF,RPRDTEFF   EFFECTIVE START DATE?                        
         BZ    PRPRGIX             NO                                           
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         GOTO1 VDATCON,BODMCB,(8,RPRDTEFF),(5,BOWORK1)                          
*                                                                               
         OC    RPRDTEEF,RPRDTEEF   END DATE ?                                   
         BZ    PRPRGI2             NO                                           
*                                                                               
         MVI   BOWORK1+8,C'-'                                                   
         GOTO1 VDATCON,BODMCB,(8,RPRDTEEF),(5,BOWORK1+9)                        
*                                                                               
         CLC   BOWORK1+6(2),BOWORK1+15             SAME YEAR?                   
         BNE   PRPRGI4                             NO                           
         MVC   BOWORK1+5(L'BOWORK1-8),BOWORK1+8    YES - MMMDD-MMMDD/YY         
*                                                                               
PRPRGI2  DS   0H                   SINGLE DATE LINE                             
         MVC   0(PGMWIDQ,R2),BOWORK1                                            
*                                                                               
         LA    R2,132(R2)                                                       
         ZIC   RF,TMPLNS           BUMP NUMBER OF LINES USED                    
         LA    RF,1(RF)                                                         
         STC   RF,TMPLNS                                                        
         B     PRPRGIX                                                          
*                                                                               
PRPRGI4  DS    0H                  TWO DATE LINES(START & END)                  
         MVC   0(9,R2),BOWORK1                                                  
         MVC   132(8,R2),BOWORK1+9                                              
         LA    R2,264(R2)                                                       
         ZIC   RF,TMPLNS                                                        
         LA    RF,2(RF)                                                         
         STC   RF,TMPLNS                                                        
*                                                                               
PRPRGIX  DS    0H                                                               
         DROP  R6                                                               
*                                                                               
         CLC   LFTLNS,TMPLNS   USE LARGER OF LINES USED SO FAR                  
         BNL   *+10            AND USED HERE FOR PROGRAM NAME                   
         MVC   LFTLNS,TMPLNS                                                    
*                                                                               
PRPRGX   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT STATION NAME                                                            
**********************************************************************          
PRSTATN  NTR1                                                                   
         L     R2,APRNTLOC         ADVANCE PRINT POSITION                       
         LA    R0,L'STLNSTA+1(R2)                                               
         ST    R0,APRNTLOC                                                      
*                                                                               
         L     R6,AMELEM                                                        
         USING RPRDTELD,R6                                                      
*                                                                               
         ZIC   R1,RPRDTSTA                                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(L'MINSTA)                                                  
         AR    R1,RA                                                            
         AH    R1,=Y(MINSTAS-TWAD)                                              
         USING STALIN,R1                                                        
*                                                                               
         MVC   0(L'STLNSTA,R2),STLNSTA                                          
         TM    STLNFLG,RPRSTSTL      SATELLITE                                  
         BZ    *+8                                                              
         MVI   4(R2),C'1'                                                       
         DROP  R1                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT ROW TAGS - ACTUALLY SAVE POSITION FOR FMTDEMS                           
**********************************************************************          
PRTAGS   NTR1                                                                   
         L     R2,APRNTLOC         ADVANCE PRINT POSITION                       
         LA    R0,9(R2)                                                         
         ST    R0,APRNTLOC                                                      
*                                                                               
         ST    R2,RCTAGPOS         TAGS - SAVE POSITION                         
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT COST DECRIPTIONS - ACTUALLY THIS IS JUST TO SAVE THE POSITION           
**********************************************************************          
PRCDESC  NTR1                                                                   
         L     R2,APRNTLOC         ADVANCE PRINT POSITION                       
         LA    R0,L'CSLNLBL(R2)                                                 
         ST    R0,APRNTLOC                                                      
*                                                                               
         ST    R2,CDSCPOS          SAVE COST DESC COL POSITION                  
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT THE INVENTORY NUMBER                                                    
**********************************************************************          
PRINVN   NTR1                                                                   
         L     R2,APRNTLOC         ADVANCE PRINT POSITION                       
         LA    R0,5(R2)                                                         
         ST    R0,APRNTLOC                                                      
*                                                                               
         L     R6,AMELEM                                                        
         USING RPRDTELD,R6                                                      
         MVC   0(4,R2),RPRDTINM     INVENTORY NUMBER                            
         DROP  R6                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT THE SOTS AND SAVE POSITION                                              
**********************************************************************          
PRSPOTS  NTR1                                                                   
         L     R2,APRNTLOC         ADVANCE PRINT POSITION                       
         LA    R0,6(R2)                                                         
         ST    R0,APRNTLOC                                                      
*                                                                               
         ST    R2,SPTPOS           SAVE SPOT COLUMN POSITION                    
         L     R6,AMELEM                                                        
         USING RPRDTELD,R6                                                      
         EDIT  (B2,RPRDTTSP),(5,0(R2)),ZERO=NOBLANK,WRK=BOWORK1,       X        
               DUB=BODUB1                                                       
         DROP  R6                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT COST - ACTUALLY THIS IS JUST TO SAVE THE POSITION                       
**********************************************************************          
PRCOST   NTR1                                                                   
         L     R2,APRNTLOC         ADVANCE PRINT POSITION                       
         LA    R0,13(R2)                                                        
         ST    R0,APRNTLOC                                                      
*                                                                               
         ST    R2,CSTPOS           SAVE COST COLUMN POSITION                    
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT CPP ACTUALLY JUST SAVE ITS POSITION                                     
**********************************************************************          
PRCPP    NTR1                                                                   
         L     R2,APRNTLOC         ADVANCE PRINT POSITION                       
         LA    R0,8(R2)                                                         
         ST    R0,APRNTLOC                                                      
*                                                                               
         ST    R2,CPPPOS           SAVE CPP COLUMN POSITION                     
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT TARGET RATING - ACTUALLY JUST SAVE ITS POSITION                         
**********************************************************************          
PRTRGTR  NTR1                                                                   
         L     R2,APRNTLOC         ADVANCE PRINT POSITION                       
         LA    R0,9(R2)                                                         
         ST    R0,APRNTLOC                                                      
*                                                                               
         ST    R2,TRGTRPOS         SAVE TARGET RATE COLUMN POSITION             
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT BUYER CPP - ACTUALLY JUST SAVE ITS POSITION                             
**********************************************************************          
PRBYRCP  NTR1                                                                   
         L     R2,APRNTLOC         ADVANCE PRINT POSITION                       
         LA    R0,9(R2)                                                         
         ST    R0,APRNTLOC                                                      
*                                                                               
         ST    R2,BCPPPOS          SAVE TARGET RATE COLUMN POSITION             
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* PRINT DEMOS - ACTUALLY SAVE SOME POSITIONS                                    
**********************************************************************          
PRDEMOS  NTR1                                                                   
         L     R2,APRNTLOC         ADVANCE PRINT POSITION                       
         ST    R2,RTGPOS           SAVE RTG COLUMN POSITION                     
         ST    R2,RCDEMPOS         SAVE DEMO POS (NEED HERE TOO)                
*                                  CALCULATE USING TAB AND DEMO VAL             
         ZIC   R1,RCNDMOS          FOR EACH ACTIVE DEMO                         
         MH    R1,=H'8'            8 SPACES                                     
         AR    R2,R1                                                            
         ST    R2,EODPOS           POSITION AT END OF DEMOS                     
*                                  (USED IF NO COST COLUMN)                     
         ST    R2,APRNTLOC                                                      
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* CHECK COST/BOOK COMBO                                                         
*    R1 CONTAINS INDEX INTO RCCSTS                                              
*          **  NOTE- THERE WILL ONLY BE 1 BOOK HERE BECAUSE                     
*          **  OTHERWISE HANDLED IN FMTDEM, SO LOOK ONLY AT RCBKS(1)            
**********************************************************************          
FMCCHK   NTR1                                                                   
         ZIC   RF,RCBKS            BOOK SUPPRESSED FOR THIS LINE?               
         LA    RF,BITTAB(RF)                                                    
         L     RE,AMELEM                                                        
         MVC   BOBYTE1,(RPRDTBKS-RPRDTELD)(RE)                                  
         NC    BOBYTE1,0(RF)                                                    
         CLC   BOBYTE1,0(RF)                                                    
         BE    EXITL               YES                                          
*                                                                               
         LA    RE,RCCSTS(R1)       COST NUMBER                                  
         ZIC   RF,0(RE)                                                         
         BCTR  RF,0                                                             
         MH    RF,=Y(L'MINCOST)                                                 
         AR    RF,RA                                                            
         AH    RF,=Y(MINCOSTS-TWAD)                                             
         USING CSTLIN,RF                                                        
         CLI   CSLNLBK,0           IS THERE A BOOK?                             
         BE    EXITOK              NO                                           
*                                                                               
         CLC   RCBKS(1),CSLNLBK    SAME BOOK?                                   
         BE    EXITOK              YES                                          
         B     EXITL               NO                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
*        INVCOM - PRINT INVENTORY COMMENTS                                      
***********************************************************************         
INVCOM   NTR1                                                                   
         TM    RCTEXT,RCTINVQ      DO WE NEED INV COMMENTS?                     
         BNO   INVCX               NO, DONE                                     
*                                                                               
         MVI   BOBYTE1,0                                                        
         L     R6,AMELEM                                                        
         USING RPRDTELD,R6                                                      
*                                                                               
         CLI   RPRDTINM,C'A'       ANY INV NUMBER                               
         BL    INVCX               NO, OUT                                      
         MVC   SAVINV,RPRDTINM     YES, SAVE IT                                 
*                                                                               
         OI    MISCFLG1,MF1TMPBT   ASSUME ALL                                   
*                                                                               
         TM    RCTEXT,RCTINVPQ     IF DOING ALL INV TEXTS                       
         BO    INVC04              DON'T BOTHER WITH ELEMENT                    
*                                                                               
         LR    RE,RA               CHECK DAYPART FLAGS                          
         AH    RE,=Y(MINDPTS-TWAD)                                              
         LA    RF,L'MINDPTS(RE)                                                 
         USING DPTLIN,RE                                                        
*                                                                               
INVC01   DS    0H                                                               
         CLC   DPLNDPT,RPRDTDPT                                                 
         BNE   *+16                                                             
         TM    DPLNFLG,RPRDPFAI    ALL INVENOTRY TEXT?                          
         BO    INVC04              YES - DON'T BOTHER WITH ELEM                 
         B     INVC02                                                           
*                                                                               
         LA    RE,DPLNLENQ(RE)                                                  
         CR    RE,RF                                                            
         BL    INVC01                                                           
*                                                                               
INVC02   DS    0H                                                               
         MVI   ELCODE,RPRITELQ                                                  
         BAS   RE,NEXTEL                                                        
         BNE   INVC20                                                           
*                                                                               
         ZIC   R0,1(R6)                                                         
         LR    RE,R0                                                            
         SH    R0,=Y(RPRIT1ST-RPRITELD)  R0 HAS NUMBER OF TEXT #'S              
         BNP   INVCX                                                            
*                                                                               
         CLC   =XL2'FFFF',RPRIT1ST-RPRITELD(R6)                                 
         BE    INVC03              SPECIAL ALL MARKER                           
*                                                                               
         NI    MISCFLG1,FF-MF1TMPBT                                             
*                                                                               
         AR    RE,R6                                                            
         ST    RE,ATEMP1                   END OF TEXT #'S                      
         LA    RE,RPRIT1ST-RPRITELD(R6)    FIRST TEXT #                         
         ST    RE,ATEMP2                   NEXT TEXT #                          
*                                                                               
INVC03   L     R6,AMELEM                   RESTORE DETAIL ELEMENT               
*                                                                               
INVC04   DS    0H                                                               
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
*                                                                               
         BAS   RE,SETFTCH          SET COMMON FETCH CONTROLS                    
         BAS   RE,SETBKDMS         SET BOOKS & DEMOS                            
         LA    RF,INVHOOK          SET HOOK ROUTINE                             
         ST    RF,RFTHOOKA                                                      
         MVI   RFTCTXTT,RFTCTXIQ    GET INV TEXT                                
         L     RE,ATEMP2                                                        
         TM    MISCFLG1,MF1TMPBT   IF DOING ALL INV TEXTS                       
         BO    *+10                LEAVE TEXT# NULL                             
         MVC   RFTCTXT#,0(RE)      TEXT NUMBER                                  
         MVC   RFTCINV,SAVINV      INV NUMBER                                   
*                                                                               
         ZIC   RE,RPRDTSTA         GET STATION CALL LETTERS                     
         BCTR  RE,0                                                             
         MH    RE,=Y(L'MINSTA)                                                  
         AR    RE,RA                                                            
         AH    RE,=Y(MINSTAS-TWAD)                                              
         USING STALIN,RE                                                        
*                                                                               
         MVC   RFTCSTAT(4),STLNSTA                                              
         MVI   RFTCSTAT+4,C'T'                                                  
         TM    STLNFLG,RPRSTSTL                                                 
         BZ    *+8                                                              
         MVI   RFTCSTAT+4,C'1'                                                  
         DROP  RE                                                               
*                                                                               
         OC    RPRDTEFF,RPRDTEFF   START DATE                                   
         BZ    INVC10                                                           
         GOTO1 VDATCON,BODMCB,(8,RPRDTEFF),(2,RFTCEFST)                         
*                                                                               
INVC10   DS    0H                                                               
         OC    RPRDTEEF,RPRDTEEF   END DATE                                     
         BZ    INVC11                                                           
         GOTO1 VDATCON,BODMCB,(8,RPRDTEEF),(2,RFTCEFEN)                         
         DROP  R6                                                               
*                                                                               
INVC11   DS    0H                                                               
         GOTO1 VFETCH,BODMCB,RFTBLKD                                            
*                                                                               
         MVI   BOBYTE1,1                                                        
         TM    MISCFLG1,MF1TMPBT   IF DOING ALLINV TEXTS                        
         BO    INVC20              DON'T LOOP BACK                              
         L     RE,ATEMP2           BUMP TO NEXT TEXT NUMBER                     
         LA    RE,2(RE)                                                         
         ST    RE,ATEMP2                                                        
         CLC   ATEMP2,ATEMP1       TO THE END?                                  
         BL    INVC04                                                           
*                                                                               
INVC20   DS    0H                                                               
         CLI   BOBYTE1,0              IF DID ANY LINES                          
         BE    INVCX                                                            
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)        THEN SPACE AFTER                             
*                                                                               
INVCX    DS    0H                                                               
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
*        INVOOK - FETCH HOOK FOR INV TEXT                                       
***********************************************************************         
INVHOOK  NTR1                                                                   
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
         L     R5,AREP                                                          
         USING REPD,R5                                                          
*                                                                               
         L     R3,RFTFTX1A         A(FIRST LINE)                                
         SR    R0,R0                                                            
         ICM   R0,1,RFTFTX1N       LINE COUNT                                   
         BZ    INVH04                                                           
*                                                                               
INVH02   DS    0H                                                               
         MVC   REPP1+4(FTCHWDTH),0(R3)                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*                                                                               
         LA    R3,132(R3)                                                       
         BCT   R0,INVH02                                                        
*                                                                               
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*                                                                               
INVH04   DS    0H                                                               
         B     EXITOK                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        USRCOM - PRINT USER COMMENTS                                           
***********************************************************************         
USRCOM   NTR1                                                                   
         L     R6,AMELEM                                                        
         L     R3,AREP                                                          
         USING REPD,R3                                                          
         MVI   ELCODE,RPRUTELQ                                                  
         MVI   BOBYTE1,0                                                        
*                                                                               
USRC02   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   USRC20                                                           
*                                                                               
         USING RPRUTELD,R6                                                      
         ZIC   RF,1(R6)                                                         
         SH    RF,=Y(RPRUTTXT-RPRUTELD)                                         
         BNP   USRC02                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   REPP1+4(0),RPRUTTXT                                              
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
         DROP  R3,R6                                                            
*                                                                               
         MVI   BOBYTE1,1                                                        
         B     USRC02                                                           
*                                                                               
USRC20   DS    0H                                                               
         CLI   BOBYTE1,0           IF DID ANY LINES                             
         BE    USRCX                                                            
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)        SPACE AFTER                                  
*                                                                               
USRCX    DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        FMTDEMS - FORMAT DEMOS FOR EACH BOOK                                   
*                - R3 POINTS TO RPRDTEL                                         
***********************************************************************         
         DS    0D                                                               
PRTDEMS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,CSTPOS           COST POSITION                                
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         L     R0,EODPOS           IF NONE USE END OF DEMO POS                  
         S     R0,RCDEMPOS         MINUS DEMO POSITION                          
         ST    R0,DEMTOCST         DISP IN LINE FROM DEMO TO COST               
*                                                                               
         LA    R3,RCBKS            LIST OF ACTIVE BOOKS                         
         ZAP   PCKOF06B,=P'0'      PACKED LINE COUNTER (NO FREE REG)            
         NI    MISCFLG1,X'FF'-MF1CSTPR                                          
*                                                                               
PRDM00   DS    0H                                                               
         L     R6,AMELEM                                                        
         USING RPRDTELD,R6                                                      
         MVC   RCBKIOR,0(R3)       INTERNAL ORDER NUMBER                        
*                                                                               
         ZIC   R1,RCBKIOR                                                       
         LA    R1,BITTAB(R1)                                                    
         MVC   BOBYTE1,RPRDTBKS    BIT LIST OF SUPPRESSED BOOKS                 
         NC    BOBYTE1,0(R1)                                                    
*                                                                               
         CLC   BOBYTE1,0(R1)       BOOK SUPPRESSED?                             
         BE    PRDMNBK             YES                                          
*                                                                               
         MVC   RCDEMPBK,RCDEMPOS   SAVE DEM POS ON 1ST LINE FOR BOOK            
         L     R0,RCDEMPOS         GET COST POS ON FIRST BOOK LINE              
         A     R0,DEMTOCST                                                      
         ST    R0,RCCSTPOS                                                      
         MVC   RCCPPPOS,CPPPOS                                                  
*                                                                               
*****************************                                                   
** 1ST LINE IS DEMO VALUES **                                                   
*****************************                                                   
         ZIC   R4,RCBKIOR          GET BOOK TABLE ENTRY                         
         BCTR  R4,0                                                             
         MH    R4,=Y(L'MINBK)                                                   
         AR    R4,RA                                                            
         AH    R4,=Y(MINBKS-TWAD)                                               
*                                                                               
         CLI   RCNTAGS,0           ANY TAGS?                                    
         BE    PRDMD02             NO - SKIP BOOK NAME                          
*                                                                               
         L     R2,RCTAGPOS         POSITION FOR TAG                             
         GOTO1 MUNBOOK,BODMCB,(R4),(R2)                                         
         LA    R2,132(R2)          FOR NEXT LINE                                
         ST    R2,RCTAGPOS                                                      
*                                                                               
PRDMD02  DS    0H                                                               
         L     R6,AMELEM                                                        
*                                                                               
         MVI   ELCODE,RPRDVELQ     SUBMIT DEMO VALS                             
         GOTO1 SETDVALS,BODMCB,(R6),SUDVALS                                     
         MVI   ELCODE,RPRNDELQ     NEGOTIATED DEMO VALUES                       
         GOTO1 SETDVALS,BODMCB,(R6),NGDVALS                                     
*                                                                               
         LA    RF,SUDVALS          USE SUBMIT RTGS                              
         TM    RCRTG,RCRSUBQ                                                    
         BO    PRDMD04                                                          
         OC    NGDVALS,NGDVALS     FORGET NEGOTIATED IF NOT THERE               
         BZ    PRDMD04                                                          
         LA    RF,NGDVALS          OR NEGOTIATED                                
*                                                                               
PRDMD04  DS    0H                                                               
         MVC   WKDVALS,0(RF)                                                    
         OC    WKDVALS,WKDVALS     ANY DEMOS                                    
         BNZ   PRDMD06             YES                                          
         L     R2,RCDEMPOS         NO, SET ASTERISKS                            
         ZIC   R0,RCNDMOS          NUMBER OF ACTIVE DEMOS                       
*                                                                               
PRDMD05  DS    0H                                                               
         MVI   6(R2),C'*'                                                       
         LA    R2,8(R2)                                                         
         BCT   R0,PRDMD05                                                       
         B     PRDMDX                                                           
*                                                                               
PRDMD06  DS    0H                                                               
         L     R2,RCDEMPOS         FIRST DEMO POSTITION                         
         LA    R4,WKDVALS                                                       
         ZIC   R0,RCNDMOS          NUMBER OF ACTIVE DEMOS                       
*                                                                               
PRDMD08  DS    0H                                                               
         MVC   WDEM,0(R4)                                                       
         NI    WDEM,X'7F'          STRIP HOB                                    
         BAS   RE,EDITDEM          RTGS EDITOR                                  
         LA    R4,12(R4)                                                        
         LA    R2,8(R2)                                                         
         BCT   R0,PRDMD08                                                       
*                                                                               
PRDMDX   DS    0H                                                               
*******************************                                                 
** BUYERS CPP (IF REQUESTED) **                                                 
*******************************                                                 
         TM    RCCPP,RCPBYRQ                                                    
         BZ    PRDMBCX                                                          
*                                                                               
         L     R6,AMELEM                                                        
         ICM   R0,15,RPRDTTAB                                                   
         L     R4,BCPPPOS                                                       
         S     R4,RTGPOS                                                        
         A     R4,RCDEMPBK                                                      
         EDIT  (R0),(8,0(R4)),2,WRK=BOWORK1,DUB=BODUB1,ZERO=NOBLANK             
*                                                                               
PRDMBCX  DS    0H                                                               
********************************                                                
** TARGET RATE (IF REQUESTED) **   CALCULATE USING TAB AND DEMO VAL             
********************************   RATING WITHOUT THE OVERRIDE BIT              
         TM    RCCOST,RCCTRGQ                                                   
         BZ    PRDMTRX                                                          
*                                                                               
         MVC   BOFULL1,WKDVALS                                                  
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    RCFLAGS1,RCF1RND                                                 
         BNO   *+16                                                             
         SRP   BODUB1,64-1,5                                                    
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF08B,BODUB1     RATING VALUE                                 
*                                                                               
         L     R6,AMELEM           BUYER'S CPP                                  
         ICM   R0,15,RPRDTTAB                                                   
         CVD   R0,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF08B                                                
         SRP   PCKOF16B,64-1,0                                                  
         L     R4,TRGTRPOS                                                      
         S     R4,RTGPOS                                                        
         A     R4,RCDEMPBK                                                      
         EDIT  (P16,PCKOF16B),(8,0(R4)),2,WRK=BOWORK1,                 X        
               DUB=BODUB1,ZERO=NOBLANK                                          
PRDMTRX  DS    0H                                                               
***********************                                                         
** BUMP TO NEXT LINE **                                                         
***********************                                                         
         L     R2,RCDEMPOS         BUMP TO NEXT LINES DEMO POS                  
         LA    R2,132(R2)                                                       
         ST    R2,RCDEMPOS                                                      
         AP    PCKOF06B,=P'1'      BUMP LINE COUNTER                            
***************************************                                         
** NEXT LINE IS SHARE (IF REQUESTED) **                                         
***************************************                                         
         TM    RCFLAGS1,RCF1SHR    DOING SHARES?                                
         BNO   PRDMSX              NO                                           
*                                                                               
         CLI   RCNTAGS,0           DOING TAGS?                                  
         BE    PRDMS02             NO                                           
*                                                                               
         L     R2,RCTAGPOS         POSITION FOR TAGS                            
         MVC   0(7,R2),=C' -SHARE'                                              
         LA    R2,132(R2)          FOR NEXT LINE                                
         ST    R2,RCTAGPOS                                                      
*                                                                               
PRDMS02  DS    0H                                                               
         L     R2,RCDEMPOS         SLOT FOR FIRST DEMO                          
         ZIC   R0,RCNDMOS          NUMBER OF ACTIVE DEMOS                       
         LA    R4,WKDVALS                                                       
*                                                                               
PRDMS04  DS    0H                                                               
         MVC   WDEM,4(R4)          SHARES AT +8                                 
         NI    WDEM,X'7F'          STRIP HOB                                    
         BAS   RE,EDITDEM                                                       
*                                                                               
         LA    R4,12(R4)                                                        
         LA    R2,8(R2)            NEXT DEMO POS                                
         BCT   R0,PRDMS04                                                       
*                                                                               
         L     R2,RCDEMPOS         BUMP TO NEXT LINES DEMOS                     
         LA    R2,132(R2)                                                       
         ST    R2,RCDEMPOS                                                      
         AP    PCKOF06B,=P'1'       BUMP LINE COUNTER                           
*                                                                               
PRDMSX   DS    0H                                                               
*****************************************                                       
** NEXT LINE IS HUT/PUT (IF REQUESTED) **                                       
*****************************************                                       
         TM    RCFLAGS1,RCF1PUT    DOING HUTS/PUTS?                             
         BZ    PRDMPX              NO                                           
*                                                                               
         CLI   RCNTAGS,0           DOING TAGS?                                  
         BE    PRDMP02             NO                                           
*                                                                               
         L     R2,RCTAGPOS         POSITION FOR TAGS                            
         MVC   0(5,R2),=C' -HPT'                                                
         LA    R2,132(R2)          FOR NEXT LINE                                
         ST    R2,RCTAGPOS                                                      
*                                                                               
PRDMP02  DS    0H                                                               
         L     R2,RCDEMPOS         SLOT FOR FIRST DEMO                          
         ZIC   R0,RCNDMOS          NUMBER OF ACTIVE DEMOS                       
         LA    R4,WKDVALS          SAVED SUBMIT DEM VALS                        
*                                                                               
PRDMP04  DS    0H                                                               
         MVC   WDEM,8(R4)          PUTS AT +8                                   
         NI    WDEM,X'7F'          STRIP HOB                                    
         BAS   RE,EDITDEM                                                       
*                                                                               
         LA    R4,12(R4)                                                        
         LA    R2,8(R2)            NEXT DEMO POS                                
         BCT   R0,PRDMP04                                                       
*                                                                               
         L     R2,RCDEMPOS         BUMP TO NEXT LINES DEMOS                     
         LA    R2,132(R2)                                                       
         ST    R2,RCDEMPOS                                                      
         AP    PCKOF06B,=P'1'      BUMP LINE COUNTER                            
*                                                                               
PRDMPX   DS    0H                                                               
**********************************************                                  
** NEXT LINE(S) ARE CPP'S, COSTS, AND DESCS **                                  
**********************************************                                  
         TM    RCCPP,RCCPSTKQ      DOING CPP STACKED?                           
         BNO   PRDMC02             NO                                           
*                                                                               
         CLI   RCNTAGS,0           DOING TAGS?                                  
         BE    PRDMC02             NO                                           
*                                                                               
         L     R2,RCTAGPOS                                                      
         MVC   0(8,R2),=C' -CPP/M '                                             
**       LA    R2,132(R2)                                                       
**       ST    R2,RCTAGPOS                                                      
*                                                                               
PRDMC02  DS    0H                                                               
         LA    R4,RCCSTS           1ST COST (DISPLAY ORDER)                     
         ZIC   R0,RCNCSTS                                                       
         NI    MISCFLG1,FF-MF1TMPBT                                             
*                                                                               
         TM    RCCPP,RCCPSTKQ+RCCPCOLQ  DOING CPP?                              
         BNZ   PRDMC04                  YES                                     
         TM    RCCOST,RCCCOLQ           COST COLUMN?                            
         BNO   PRDMC05                  NO                                      
*                                                                               
PRDMC04  DS    0H                                                               
         BAS   RE,EDITCOST         CPP AND COST AND DESC ROUTINE                
         LA    R4,1(R4)            NEXT COST                                    
         BCT   R0,PRDMC04                                                       
*                                                                               
PRDMC05  DS    0H                  SEE IF ANY COST LEFT TO PRINT                
         OI    MISCFLG1,MF1CSTPR                                                
*                                                                               
PRDMC06  DS    0H                                                               
         L     R2,RCTAGPOS                                                      
         CLI   RCNTAGS,0           ANY TAGS?                                    
         BNH   *+10                NO                                           
         MVC   0(8,R2),BCSPACES    YES - REMOVE EXTRA TAGS                      
*                                                                               
PRDMC07  DS    0H                                                               
         L     RF,RCDEMPOS                                                      
         A     RF,DEMTOCST         POINT TO COST                                
         CLC   0(20,RF),BCSPACES    IF ANY MUST GO TO NEXT LINE                 
         BNH   PRDMC08                                                          
         L     R2,RCDEMPOS         NEXT LINE                                    
         LA    R2,132(R2)                                                       
         ST    R2,RCDEMPOS                                                      
         L     R2,RCTAGPOS         TAG POSITIONER ALSO                          
         LA    R2,132(R2)                                                       
         ST    R2,RCTAGPOS                                                      
         AP    PCKOF06B,=P'1'      BUMP LINE COUNTER                            
         B     PRDMC07                                                          
*                                                                               
PRDMC08  DS    0H                                                               
         TM    RCCPP,RCCPSTKQ      STACKING CPPS?                               
         BO    PRDMC10             YES?                                         
*                                                                               
PRDMC09  DS    0H                                                               
         L     R0,RCCSTPOS                                                      
         S     R0,DEMTOCST         GET START OF LINE WITH LATEST COST           
         C     R0,RCDEMPOS         IS IT BELOW CURRENT LINE                     
         BNH   PRDMC10                                                          
         L     R2,RCDEMPOS         NO, BUMP TO NEXT DEM POS                     
         LA    R2,132(R2)                                                       
         ST    R2,RCDEMPOS                                                      
         AP    PCKOF06B,=P'1'      BUMP LINE COUNTER                            
         B     PRDMC09                                                          
*                                                                               
PRDMC10  DS    0H                                                               
**************************                                                      
** FINISH WITH FOOTNOTE **                                                      
**************************                                                      
         BAS   RE,GETFOOT          GET FOOTNOTE FOR BOOK/PRIME DEMO             
*                                                                               
         CLI   RCNTAGS,1           IF ANY TAGS (OTHER THAN BOOK                 
         BH    *+12                (WHICH IS ALWAYS #1)                         
         TM    RCTEXT,RCTFTNQ      OR IF DOING FOOTNOTES                        
         BNO   PRDMX10             SPACE BETWEEN BOOKS                          
*                                                                               
         L     R2,RCDEMPOS         SPACE BETWEEN BOOKS                          
         LA    R2,132(R2)                                                       
         ST    R2,RCDEMPOS                                                      
         L     R2,RCTAGPOS                                                      
         LA    R2,132(R2)                                                       
         ST    R2,RCTAGPOS                                                      
         AP    PCKOF06B,=P'1'      BUMP LINE COUNTER                            
*                                                                               
PRDMX10  DS    0H                                                               
**********************                                                          
** NEXT ACTIVE BOOK **                                                          
**********************                                                          
PRDMNBK  DS    0H                                                               
         LA    R3,1(R3)                                                         
         ZIC   RF,RCNBKS                                                        
         LA    RF,RCBKS(RF)        END OF LIST                                  
         CR    R3,RF                                                            
         BL    PRDM00                                                           
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EDIT RATING VALUES - R2 POINTS TO OUTPUT AREA                                 
***********************************************************************         
EDITDEM  NTR1                      EDIT RATINGS                                 
         TM    RCFLAGS1,RCF1RND    ROUND RATING?                                
         BO    EDEM02                                                           
         EDIT  (B4,WDEM),(7,0(R2)),1,WRK=BOWORK1,DUB=BODUB1                     
         B     EDITDEMX                                                         
*                                                                               
EDEM02   DS    0H                  DO ROUNDING                                  
         L     R3,WDEM                                                          
         CVD   R3,BODUB1                                                        
         ZAP   PCKOF08B,BODUB1                                                  
         SRP   PCKOF08B,64-1,5     ROUND THE # OFF INSTEAD                      
         EDIT  (P8,PCKOF08B),(7,0(R2)),ALIGN=LEFT,WRK=BOWORK1,         X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
EDITDEMX DS    0H                                                               
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        FDCSTS- FMTDEM'S COSTS ROUTINE                                         
*                INTERNAL COST NUMBER IN R4                                     
***********************************************************************         
EDITCOST NTR1                                                                   
         ZIC   RF,0(R4)            GET COST TABLE ENTRY                         
         BCTR  RF,0                                                             
         MH    RF,=Y(L'MINCOST)                                                 
         AR    RF,RA                                                            
         AH    RF,=Y(MINCOSTS-TWAD)                                             
         USING CSTLIN,RF                                                        
*                                                                               
         CLI   CSLNLBK,0           INTERNAL BOOK NUMBER                         
         BNE   ECOST10                                                          
         TM    MISCFLG1,MF1CSTPR   COST PRINTED FOR THIS LINE?                  
         BZ    ECOST20                                                          
         TM    RCCPP,RCCPSTKQ      LONG AVAIL CPP?                              
         BZ    ECOSTX              NO                                           
         CLI   RCNCBKS,0           CPP BOOK LIST?                               
         BZ    ECOST20             NO                                           
*                                                                               
         ZIC   R0,RCNCBKS                                                       
         LA    R1,RCCBKS                                                        
         CLC   RCBKIOR,0(R1)       BOOK REQUESTED?                              
         BE    ECOST20             YES                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         B     ECOSTX              NO - SKIP THIS COST                          
*                                                                               
ECOST10  CLC   CSLNLBK,RCBKIOR     IF = TO CURRENT BOOK, OK                     
         BE    ECOST20                                                          
         B     ECOSTX              ELSE SKIP THIS COST                          
         DROP  RF                                                               
*                                                                               
ECOST20  DS    0H                                                               
         ZIC   RF,0(R4)            GET COST VALUE                               
         SLL   RF,2                                                             
         LA    RF,COSTIV1-4(RF)                                                 
         MVC   BOFULL2,0(RF)       SET VALUEIN FULL                             
*********                                                                       
** CPP **                                                                       
*********                                                                       
         TM    RCCPP,RCCPSTKQ+RCCPCOLQ  DOING CPP?                              
         BZ    ECOST40                  NO                                      
*                                                                               
         ZIC   RE,RCBKIOR          CHECK IF CPP SURPRESSED                      
         BCTR  RE,0                                                             
         MH    RE,=Y(L'MINBK)                                                   
         AR    RE,RA                                                            
         AH    RE,=Y(MINBKS-TWAD)                                               
         USING BOOKLIN,RE                                                       
         TM    BKLNBK,RPRBKSSP     ON BOOK?                                     
         BO    ECOST40             YES                                          
         DROP  RE                                                               
*                                                                               
         ZIC   R3,RCNDMOS                                                       
         LA    R6,RCDEMS                                                        
         LA    R5,WKDVALS                                                       
         L     R2,RCDEMPOS                                                      
         TM    RCCPP,RCCPCOLQ                                                   
         BNO   ECOST30                                                          
         L     R2,RCCPPPOS                                                      
*                                                                               
ECOST30  DS    0H                                                               
         ZIC   RE,0(R6)                                                         
         BCTR  RE,0                                                             
         MH    RE,=Y(L'MINDMO)                                                  
         AR    RE,RA                                                            
         AH    RE,=Y(MINDMOS-TWAD)                                              
         USING DEMOLIN,RE                                                       
         TM    DMLNDEMO,RPRDMB1S   ON DEMO?                                     
         BO    ECOST38             YES                                          
         DROP  RE                                                               
*                                                                               
         MVC   BOFULL1,0(R5)       RATING WITHOUT THE OVERRIDE BIT              
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    RCFLAGS1,RCF1RND                                                 
         BNO   *+16                                                             
         SRP   BODUB1,64-1,5                                                    
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF08B,BODUB1     RATING VALUE                                 
*                                                                               
         TM    BOFULL2,X'80'       N/A?                                         
         BZ    *+14                NO                                           
         MVC   7-3(3,R2),=C'N/A'                                                
         B     ECOST38                                                          
*                                                                               
         L     R0,BOFULL2                                                       
         CVD   R0,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         BAS   RE,DIVPACKD         CPP = COST / RATING                          
         BE    ECOST32                                                          
         ZAP   PCKOF16B,=P'0'                                                   
*                                                                               
ECOST32  DS    0H                                                               
         EDIT  (P16,PCKOF16B),(7,0(R2)),2,WRK=BOWORK1,                 X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
ECOST38  DS    0H                                                               
         TM    RCCPP,RCCPCOLQ      CPP COLUMN?                                  
         BO    ECOST39             YES - JUST DO PRIME DEMO                     
*                                                                               
         LA    R2,8(R2)            NEXT DEMO POS                                
         LA    R5,12(R5)                                                        
         LA    R6,1(R6)                                                         
         BCT   R3,ECOST30                                                       
         B     ECOST40                                                          
*                                                                               
ECOST39  DS    0H                                                               
         L     R2,RCCPPPOS                                                      
         LA    R2,132(R2)                                                       
         ST    R2,RCCPPPOS                                                      
*                                                                               
ECOST40  DS    0H                                                               
         L     RF,RCDEMPOS         COST ON CURRENT LINE                         
         A     RF,DEMTOCST                                                      
         TM    RCCPP,RCCPSTKQ                                                   
         BO    ECOST42                                                          
         L     RF,RCCSTPOS         BUMP TO NEXT COST LINE                       
*                                                                               
ECOST42  DS    0H                                                               
         TM    RCCOST,RCCCOLQ      COST COLUMN?                                 
         BNO   ECOST44             NO                                           
*                                                                               
         TM    BOFULL2,X'80'       N/A?                                         
         BZ    *+14                NO                                           
         MVC   12-3(3,RF),=C'N/A'                                               
         B     ECOST44                                                          
*                                                                               
         EDIT  BOFULL2,(12,0(RF)),2,WRK=BOWORK1,DUB=BODUB1                      
*                                                                               
ECOST44  DS    0H                                                               
         CLI   RCNCSTS,1           MULTIPLE COSTS?                              
         BNH   ECOST50             NO                                           
*                                                                               
         LR    R3,RF               DO COST DESCRIPTION                          
         A     R3,CDSCPOS                                                       
         L     R0,CSTPOS           COST COLUMN                                  
         LTR   R0,R0               IF NONE                                      
         BNZ   *+8                                                              
         L     R0,EODPOS           USE END OF DEMO POS                          
         SR    R3,R0               POINTS TO DESC POSITION                      
*                                                                               
         ZIC   RF,0(R4)            GET DESCRIPTION                              
         BCTR  RF,0                                                             
         MH    RF,=Y(L'MINCOST)                                                 
         AR    RF,RA                                                            
         AH    RF,=Y(MINCOSTS-TWAD)                                             
         USING CSTLIN,RF                                                        
         MVC   0(L'CSLNLBL,R3),CSLNLBL                                          
         DROP  RF                                                               
*                                                                               
ECOST50  DS    0H                                                               
         TM    RCCPP,RCCPSTKQ      STACKED CPP'S?                               
         BO    ECOST52             YES                                          
*                                                                               
         L     R2,RCCSTPOS         BUMP TO NEXT COST LINE                       
         LA    R2,132(R2)                                                       
         ST    R2,RCCSTPOS                                                      
         B     ECOST54                                                          
*                                                                               
ECOST52  DS    0H                  DOING STACKED CPP'S                          
         L     R2,RCDEMPOS         BUMP TO NEXT DEMO LINE                       
         LA    R2,132(R2)                                                       
         ST    R2,RCDEMPOS                                                      
         L     R2,RCTAGPOS         AND TAG POS                                  
         LA    R2,132(R2)                                                       
         ST    R2,RCTAGPOS                                                      
         AP    PCKOF06B,=P'1'                                                   
*                                                                               
ECOST54  DS    0H                                                               
*                                                                               
ECOSTX   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
*        GETFOOT- GET FOOTNOTE FOR BOOK/PRIME DEMO                              
*                 ON ENTRY R2 POINTS TO LINE FOR FOOTNOTE                       
***********************************************************************         
GETFOOT  NTR1                                                                   
         TM    RCTEXT,RCTFTNQ      DOING ANY FOOTNOTES?                         
         BNO   GTFX                NO, DONE                                     
*                                                                               
         L     R6,AMELEM                                                        
         USING RPRDTELD,R6                                                      
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
*                                                                               
         XC    SAVINV,SAVINV                                                    
         CLI   RPRDTINM,C'A'       ANY INV NUMBER?                              
         BL    *+10                NO, DONE                                     
         MVC   SAVINV,RPRDTINM     YES, SAVE IT                                 
*                                                                               
         BAS   RE,SETFTCH          SET COMMON FETCH CONTROLS                    
         LA    RF,GTFHOOK          SET HOOK ROUTINE                             
         ST    RF,RFTHOOKA                                                      
         MVI   RFTCNTL,RFTCHDRQ+RFTCDEMQ               DATA FLAGS               
         OI    RFTCNTL,RFTCFTNQ     & FOOTNOTE TEST                             
*                                                                               
         ZIC   RF,RCBKIOR          CHECK IF CPP SURPRESSED                      
         BCTR  RF,0                                                             
         MH    RF,=Y(L'MINBK)                                                   
         AR    RF,RA                                                            
         AH    RF,=Y(MINBKS-TWAD)                                               
         USING BOOKLIN,RF                                                       
         MVC   RFTCBKS(L'BKLNFBK),BKLNFBK                                       
         DROP  RF                                                               
*                                                                               
         ZIC   RF,RCDEMS                                                        
         BCTR  RF,0                                                             
         MH    RF,=Y(L'MINDMO)                                                  
         AR    RF,RA                                                            
         AH    RF,=Y(MINDMOS-TWAD)                                              
         USING DEMOLIN,RF                                                       
         MVC   RFTCDEMS,DMLNDEMO                                                
         MVI   RFTCDEMS,0           ZAP FIRST BYTE - KLUGE                      
         DROP  RF                                                               
*                                                                               
         OC    SAVINV,SAVINV        INVENTORY FETCH?                            
         BZ    GTF10                NO                                          
******************                                                              
* INVETORY FETCH                                                                
******************                                                              
         MVI   RFTAMODE,RFTAMSTQ    MASTER MODE                                 
         MVC   RFTCINV,SAVINV       INV NUMBER                                  
*                                                                               
         OC    RPRDTEFF,RPRDTEFF    START DATE                                  
         BZ    GTF06                                                            
         GOTO1 VDATCON,BODMCB,(8,RPRDTEFF),(2,RFTCEFST)                         
*                                                                               
GTF06    DS    0H                                                               
         OC    RPRDTEEF,RPRDTEEF                                                
         BZ    GTF07                                                            
         GOTO1 VDATCON,BODMCB,(8,RPRDTEEF),(2,RFTCEFEN)                         
*                                                                               
GTF07    DS    0H                                                               
         B     GTF50                                                            
**********************                                                          
** DAY/TIME REFETCH **                                                          
**********************                                                          
GTF10    DS    0H                                                               
         MVI   RFTAMODE,RFTADIRQ                        FETCH MODE              
         CLI   RFTCBKFL,RPRBKINQ                                                
         BNE   *+8                                                              
         MVI   RFTCBKFL,RPRBKTPQ                                                
*                                                                               
         MVC   RFTCDTMS+1(1),RPRDTDAY                   PRIME DAYS              
         MVC   RFTCDTMS+2(4),RPRDTSTM                   PRIME TIMES             
*                                                                               
         L     R2,AMELEM                                                        
GTF12    CLI   0(R2),0                                                          
         BE    GTF18                                                            
         CLI   0(R2),RPRDYELQ      SECONDARY DAYS/TIMES                         
         BE    GTF14                                                            
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     GTF12                                                            
*                                                                               
GTF14    DS    0H                                                               
         ZIC   RE,1(R2)                                                         
         LA    RE,0(RE,R2)         END OF ELEM                                  
         LA    RF,RFTCDTMS+RFTCDTLQ                                             
         LA    R2,RPRDYDTM-RPRDYELD(R2)                                         
GTF16    CR    R2,RE                                                            
         BNL   GTF18                                                            
         MVC   1(L'RPRDYDTM,RF),0(R2)                                           
         LA    R2,L'RPRDYDTM(R2)                                                
         LA    RF,RFTCDTLQ(RF)                                                  
         LA    R0,RFTCDTMS+(8*RFTCDTLQ)                                         
         CR    RF,R0                                                            
         BL    GTF16                                                            
*                                                                               
GTF18    DS    0H                                                               
*                                                                               
GTF50    DS    0H                                                               
         GOTO1 VFETCH,BODMCB,RFTBLKD                                            
*                                                                               
GTFX     DS    0H                                                               
         B     EXITOK                                                           
         DROP  R4,R6                                                            
         SPACE 2                                                                
***********************************************************************         
*       GTFHOOK - FETCH HOOK FOR FOOTNOTE                                       
***********************************************************************         
GTFHOOK  NTR1                                                                   
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
         OC    RFTERR,RFTERR                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RFTMODE,RFTNBKQ     NEW BOOK DATA?                               
         BNO   GTFHX               NO                                           
*                                                                               
         ZAP   BODUB1,PCKOF06B     USE RGTLNSP TO POSITION TO                   
         CVB   R2,BODUB1           LINE FOR FOOTNOTE                            
         MH    R2,=H'132'                                                       
         A     R2,APRNTBFF                                                      
         A     R2,FOOTDSP                                                       
*                                                                               
         CLI   RFTFUPGR,C' '       IF HAVE ANY UPGRADE NOTE                     
         BNH   GTFH02                                                           
         MVC   0(L'RFTFUPGR,R2),RFTFUPGR   PRINT IT NOW                         
         B     GTFH20                      AND FORGET OTHER TEXT                
*                                                                               
GTFH02   DS    0H                                                               
         CLI   RFTFTX1N,0          ANY LINES?                                   
         BE    GTFH50              FETCH SAYS NO                                
         L     RE,RFTFTX1A                                                      
         LA    RE,131(RE)                                                       
GTFH04   CLI   0(RE),C' '                                                       
         BH    GTFH06                                                           
         BCTR  RE,0                                                             
         C     RE,RFTFTX1A                                                      
         BNL   GTFH04                                                           
         B     GTFH50                                                           
*                                                                               
GTFH06   L     RF,RFTFTX1A                                                      
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   7(0,R2),0(RF)       MOVE TEXT                                    
*                                                                               
GTFH20   DS    0H                                                               
         AP    PCKOF06B,=P'1'       BUMP LINE COUNTER                           
         L     R2,RCTAGPOS         BUMP POSITIONERS                             
         LA    R2,132(R2)                                                       
         ST    R2,RCTAGPOS                                                      
         L     R2,RCDEMPOS                                                      
         LA    R2,132(R2)                                                       
         ST    R2,RCDEMPOS                                                      
*                                                                               
GTFH50   DS    0H                                                               
GTFHX    DS    0H                                                               
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REPORT HOOK ROUTINES                                                          
***********************************************************************         
REPHOOKS NTR1  BASE=*,LABEL=*                                                   
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(HOOKSN)                                                
         BNH   *+6                 UNKNOWN ROUTINE                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         L     RF,HOOKS(RF)                                                     
         GOTO1 (RF),RR=BORELO                                                   
         B     EXITOK                                                           
*                                                                               
HOOKS    DS    0A                                                               
         DC    A(HEDHOOK)          HEDHOOK ROUTINE                              
*                                                                               
HOOKSN   EQU   (*-HOOKS)/4                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* HEDHOOK ROUTINE                                                               
*********************************************************************           
         DS    0D                                                               
HEDHOOK  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AREP                                                          
         USING REPD,R3                                                          
*                                                                               
         TM    HKHFLGS,HKHBLNKQ    BLANK PAGE?                                  
         BO    HHOOKX              YES - EXIT                                   
*                                                                               
         TM    HKHFLGS,HKHCALQ     IN CALCULATE WIDTH MODE                      
         BO    HKH10               SKIP A LOT                                   
*                                                                               
         MVC   REPH1(L'BCORGNAM),BCORGNAM                                       
*                                                                               
         ZIC   R2,MYRWDTH                                                       
         SH    R2,=H'11'                                                        
         LA    R2,REPH1(R2)                                                     
         MVC   0(4,R2),=C'Page'                                                 
         ICM   R0,3,REPPAGE                                                     
         BCTR  R0,0                                                             
         EDIT  (R0),(3,5(R2)),ZERO=NOBLANK,DUB=BODUB1,WRK=BOWORK1               
*                                                                               
         LA    R2,REPH2                                                         
         ST    R2,FSTHEAD                                                       
*                                                                               
         CLI   MYRWDTH,132                                                      
         BNE   *+12                                                             
         LA    R2,41(R2)           TITLE IF 132                                 
         B     HKH02                                                            
*                                                                               
         CLI   MYRWDTH,110                                                      
         BNE   *+12                                                             
         LA    R2,30(R2)           TITLE IF 110                                 
         B     HKH02                                                            
*                                                                               
         CLI   MYRWDTH,80                                                       
         BNE   *+12                                                             
         LA    R2,30(R2)           TITLE IF 80                                  
         B     HKH02                                                            
*                                                                               
         B     HKH03               NO TITLE                                     
*                                                                               
HKH02    DS    0H                                                               
         LR    R4,R2               TODAYS DATE ABOVE TITLE                      
         SH    R4,=H'132'                                                       
         GOTO1 VDATCON,BODMCB,(5,0),(11,(R4))                                   
         OC    0(50,R4),BCSPACES                                                
         GOTO1 CENTER,BODMCB,(R4),50                                            
*                                                                               
         MVC   0(50,R2),REPTITLE                                                
         OC    0(50,R2),BCSPACES                                                
         GOTO1 CENTER,BODMCB,(R2),50                                            
         GOTO1 UNDERLIN,BODMCB,(50,0(R2)),132(R2)                               
*                                                                               
HKH03    L     R2,FSTHEAD                                                       
         MVC   (132*00)+000(05,R2),=C'BUYER'                                    
         MVC   (132*00)+008(20,R2),ECONBUYR                                     
         MVC   (132*01)+000(06,R2),=C'AGENCY'                                   
         MVC   (132*01)+008(20,R2),EAGYNAM1                                     
         MVC   (132*02)+000(06,R2),=C'ADVTSR'                                   
         MVC   (132*02)+008(20,R2),EADVNAME                                     
         MVC   (132*03)+000(07,R2),=C'PRODUCT'                                  
         MVC   (132*03)+008(20,R2),EPRDNAME                                     
         MVC   (132*04)+000(06,R2),=C'FLIGHT'                                   
         MVC   (132*04)+008(17,R2),ECONDATE                                     
         MVC   (132*05)+000(08,R2),=C'LENGTHS '                                 
         MVC   (132*05)+008(25,R2),ECONLENS                                     
*                                                                               
         CLI   RCNDPTS,1           1 DAYPART FILTER                             
         BNE   *+16                NO                                           
         MVC   (132*07)+000(07,R2),=C'DAYPART'                                  
         MVC   (132*07)+008(03,R2),RCDPTF3                                      
*                                                                               
         CLI   RCNBKS,1            IF ONLY ONE BOOK                             
         BNE   HKHBKX                                                           
         MVC   (132*06)+000(05,R2),=C'BOOK '                                    
         ZIC   R0,RCBKS                                                         
         BCTR  R0,0                                                             
         MH    R0,=Y(L'MINBK)                                                   
         AR    R0,RA                                                            
         AH    R0,=Y(MINBKS-TWAD)                                               
         LA    R4,(132*06)+008(R2)                                              
         GOTO1 MUNBOOK,BODMCB,(R0),(R4)                                         
*                                                                               
HKHBKX   DS    0H                                                               
*                                                                               
         CLI   RCNCSTS,1           ONE COST?                                    
         BNE   HKHCSX              NO                                           
*                                                                               
         TM    RCCOST,RCCCOLQ      DOING COSTS?                                 
         BZ    HKHCSX              NO                                           
*                                  YES - FIND IT A HOME                         
         MVI   BOBYTE1,C'N'                                                     
         LA    R4,(132*06)(R2)     BOOK LINE                                    
         CLI   0(R4),C' '          EMPTY?                                       
         BNH   HKHCS08             YES                                          
         LA    R4,(132*07)(R2)     DAYPART FILTER LINE?                         
         CLI   0(R4),C' '          EMPTY?                                       
         BNH   HKHCS08             YES                                          
*                                                                               
         MVI   BOBYTE1,C'Y'                                                     
         CLI   MYRWDTH,80                                                       
         BNE   *+12                                                             
         LA    R4,(132*06)+40(R2)                                               
         B     HKHCS08                                                          
*                                                                               
         CLI   MYRWDTH,110                                                      
         BNE   *+12                                                             
         LA    R4,(132*06)+40(R2)                                               
         B     HKHCS08                                                          
*                                                                               
         CLI   MYRWDTH,132                                                      
         BNE   *+12                                                             
         LA    R4,(132*06)+51(R2)  OR AFTER BOOK                                
         B     HKHCS08                                                          
*                                                                               
HKHCS08  DS    0H                                                               
         ZIC   RE,RCCSTS           BINARY COST CODE                             
         BCTR  RE,R0                                                            
         MH    RE,=Y(L'MINCOST)                                                 
         AR    RE,RA                                                            
         AH    RE,=Y(MINCOSTS-TWAD)                                             
         CLI   0(RE),0             NO TABLE ENTRY FOR THIS COST                 
         BE    HKHCSX              SO SKIP DESC                                 
         MVC   0(5,R4),=C'COST '                                                
         CLI   BOBYTE1,C'N'                                                     
         BNE   *+14                                                             
         MVC   08(18,R4),CSLNLBL-CSTLIN(RE)                                     
         B     *+10                                                             
         MVC   13(18,R4),CSLNLBL-CSTLIN(RE)                                     
*                                                                               
HKHCSX   DS    0H                                                               
*                                                                               
HKH09    DS    0H                                                               
         CLI   MYRWDTH,132                                                      
         BNE   HKH09A                                                           
*                                  132 POS                                      
         MVC   (132*02)+051(07,R2),=C'STATION'                                  
         MVC   (132*02)+064(06,R2),ESTATION                                     
         MVC   (132*02)+071(20,R2),EMKTNAME                                     
         MVC   (132*03)+051(11,R2),=C'SALESPERSON'                              
         MVC   (132*03)+064(20,R2),ESALNAME                                     
         MVC   (132*04)+064(12,R2),ESALTEL                                      
         LA    RE,(132*05)+051(R2) REF# HERE                                    
         LA    R2,(132*06)+051(R2) HEADER COMMENTS HERE                         
         B     HKH09X                                                           
*                                                                               
HKH09A   DS    0H                                                               
         CLI   MYRWDTH,110                                                      
         BNE   HKH09B                                                           
*                                  110 POS                                      
         MVC   (132*02)+040(07,R2),=C'STATION'                                  
         MVC   (132*02)+053(06,R2),ESTATION                                     
         MVC   (132*02)+060(20,R2),EMKTNAME                                     
         MVC   (132*03)+040(11,R2),=C'SALESPERSON'                              
         MVC   (132*03)+053(20,R2),ESALNAME                                     
         MVC   (132*04)+053(12,R2),ESALTEL                                      
         LA    RE,(132*05)+040(R2) REF# HERE                                    
         LA    R2,(132*06)+040(R2) HEADER COMMENTS HERE                         
         B     HKH09X                                                           
*                                                                               
HKH09B   DS    0H                                                               
         CLI   MYRWDTH,80                                                       
         BNE   HKH09C                                                           
*                                  80 POS                                       
         MVC   (132*02)+040(07,R2),=C'STATION'                                  
         MVC   (132*02)+053(06,R2),ESTATION                                     
         MVC   (132*02)+060(20,R2),EMKTNAME                                     
         MVC   (132*03)+040(11,R2),=C'SALESPERSON'                              
         MVC   (132*03)+053(20,R2),ESALNAME                                     
         MVC   (132*04)+053(12,R2),ESALTEL                                      
         LA    RE,(132*05)+040(R2) REF# HERE                                    
         LA    R2,(132*06)+040(R2) HEADER COMMENTS HERE                         
         B     HKH09X                                                           
*                                                                               
HKH09C   DS    0H                                                               
*                                                                               
HKH09X   DS    0H                                                               
         MVC   0(7,RE),=CL7'REF#'                                               
         MVC   13(8,RE),ECONNUM                                                 
         LA    RE,13+8(RE)                                                      
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(03,RE),EPRONUM                                                 
*                                                                               
         BAS   RE,HDRUPGD          HEADER UPGRADES                              
*                                                                               
         CLI   0(R2),C' '                                                       
         BNH   *+8                                                              
         LA    R2,132(R2)                                                       
*                                                                               
         BAS   RE,HDRCOM           HEADER COMMENTS                              
*                                                                               
HKH10    DS    0H                                                               
         TM    HKHFLGS,HKHNCOLQ                                                 
         BO    HHOOKX                                                           
*                                                                               
         LA    R2,REPH9           NOTE- ONLY 11-13 ARE ACTUALLY USED            
*                                 HERE NOW, BUT DISPS ARE FROM REPH9            
         DROP  R3                                                               
         LA    R3,FMTCTL                                                        
*                                                                               
HKH20    DS    0H                                                               
         CLI   0(R3),X'FF'         END OF FMT CONTROLS                          
         BE    HKH22                                                            
         CLI   0(R3),0             SKIP EMPTIES                                 
         BE    HKH21                                                            
*                                                                               
         ZIC   RF,0(R3)                                                         
         SLL   RF,2                                                             
         LA    RF,HKHBR-4(RF)                                                   
         BASR  RE,RF                                                            
*                                                                               
HKH21    DS    0H                                                               
         LA    R3,1(R3)            NEXT                                         
         B     HKH20                                                            
*                                                                               
HKH22    DS    0H                                                               
         L     R3,AREP                                                          
         USING REPD,R3                                                          
         LA    R0,REPH9            CALC REPORT WIDTH                            
         SR    R2,R0                                                            
         STC   R2,RPTWID                                                        
HHOOKX   DS    0H                                                               
         B     EXITOK                                                           
         DROP  R3                                                               
*                                                                               
HKHBR    DS    0F                                                               
         B     HKHDAYTM                                                         
         B     HKHSTATN                                                         
         B     HKHDAYPT                                                         
         B     HKHDEMOS                                                         
         B     HKHCPP                                                           
         B     HKHCOST                                                          
         B     HKHPROG                                                          
         B     HKHTAGS                                                          
         B     HKHSPOTS                                                         
         B     HKHINVN                                                          
         B     HKHTRGTR                                                         
         B     HKHBYRCP                                                         
         B     HKHCDSCP                                                         
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
HKHDAYTM DS    0H                                                               
         MVC   (132*03)+000(3,R2),=C'DAY'                                       
         MVC   (132*04)+000(3,R2),DASHES                                        
         MVC   (132*03)+0012(4,R2),=C'TIME'                                     
         MVC   (132*04)+0012(4,R2),DASHES                                       
         LA    R2,DTMWIDQ+1(R2)                                                 
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
HKHDAYPT DS    0H                                                               
         MVC   (132*03)+000(3,R2),=C'DPT'                                       
         MVC   (132*04)+000(3,R2),DASHES                                        
         LA    R2,4(R2)                                                         
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
HKHPROG  DS    0H                                                               
         MVC   (132*03)+000(7,R2),=C'PROGRAM'                                   
         MVC   (132*04)+000(12,R2),DASHES                                       
         LA    R2,PGMWIDQ+1(R2)  WIDTH OF PROGRAM FIELD                         
*                                                                               
         L     R0,AREP                                                          
         AH    R0,=Y(REPH9-REPD)                                                
         LR    RF,R2                                                            
         SR    RF,R0                                                            
         ST    RF,FOOTDSP          FOOTNOTE TO PRINT TO RIGHT OF PGM            
*                                                                               
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
HKHSTATN DS    0H                                                               
         MVC   (132*03)+000(5,R2),=C'STATN'                                     
         MVC   (132*04)+000(5,R2),DASHES                                        
         LA    R2,6(R2)                                                         
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
HKHTAGS  DS    0H                                                               
         LA    R2,9(R2)            JUST LEAVE ROOM                              
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
HKHSPOTS DS    0H                                                               
         MVC   (132*03)+000(5,R2),=C'SPOTS'                                     
         MVC   (132*04)+000(5,R2),DASHES                                        
         LA    R2,6(R2)                                                         
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
HKHINVN  DS    0H                                                               
         MVC   (132*03)+000(3,R2),=C'INV'                                       
         MVC   (132*04)+000(4,R2),DASHES                                        
         LA    R2,5(R2)                                                         
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
HKHTRGTR DS    0H                                                               
         MVC   (132*02)+001(6,R2),=C'TARGET'                                    
         MVC   (132*03)+002(4,R2),=C'RATE'                                      
         MVC   (132*04)+000(8,R2),DASHES                                        
         LA    R2,9(R2)                                                         
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
HKHBYRCP DS    0H                                                               
         MVC   (132*02)+002(6,R2),=C'BUYERS'                                    
         MVC   (132*03)+002(6,R2),=C' CPP/M '                                   
         MVC   (132*04)+002(6,R2),DASHES                                        
         LA    R2,9(R2)                                                         
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
HKHCOST  DS    0H                                                               
         MVC   (132*03)+000(04,R2),=C'COST  '                                   
         MVC   (132*04)+000(12,R2),DASHES                                       
*                                                                               
         LA    R2,13(R2)                                                        
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
HKHCPP   DS    0H                                                               
         MVC   (132*03)+000(5,R2),=C'CPP/M'                                     
         MVC   (132*04)+000(7,R2),DASHES                                        
*                                                                               
         LA    R2,8(R2)                                                         
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
HKHCDSCP DS    0H                                                               
         LA    R2,19(R2)          JUST LEAVE ROOM, NO ACTUAL HEADLINE           
         BR    RE                                                               
         SPACE 2                                                                
**********************************************************************          
         SPACE 1                                                                
HKHDEMOS NTR1                                                                   
         LA    R4,RCDEMS           ACTIVE DEMO LIST                             
         ZIC   R5,RCNDMOS                                                       
*                                                                               
HKHDM4   DS    0H                                                               
         ZIC   R0,0(R4)            NO, LOOK FOR DEMO TABLE ENTRY                
         BCTR  R0,0                                                             
         MH    R0,=Y(L'MINDMO)                                                  
         AR    R0,RA                                                            
         AH    R0,=Y(MINDMOS-TWAD)                                              
         XC    BOWORK2,BOWORK2                                                  
         GOTO1 MUNDEMO,BODMCB,(R0),BOWORK2                                      
*                                                                               
         MVC   (132*03)(7,R2),BOWORK2                                           
         MVC   (132*04)(7,R2),DASHES                                            
*                                                                               
HKHDM12  DS    0H                                                               
         LA    R2,8(R2)            NEXT POSITION                                
         LA    R4,1(R4)            NEXT ACTIVE DEMO                             
         BCT   R5,HKHDM4                                                        
*                                                                               
HKHDM14  DS    0H                                                               
         B     XITR2                                                            
         EJECT                                                                  
***********************************************************************         
*        HDRCOM - PRINT HEADER COMMENTS                                         
*                 R2 POINTS TO PLACE FOR THEM                                   
***********************************************************************         
HDRCOM   NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         LA    RF,BOELEM           USE WORK ELEM                                
         ST    RF,MINELEM                                                       
         MVC   MINMAXEL,=Y(L'BOELEM)   MUST SET RIGHT MAX LENGTH                
         MVI   MINFILTL,1                                                       
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRCMELQ                                                 
*                                                                               
         BAS   RE,MINIOHI                                                       
         B     *+8           ANY ERROR IS ELEM NOT FOUND                        
HDRC02   DS    0H                                                               
         BAS   RE,MINIOSEQ                                                      
         BNE   HDRC20                                                           
*                                                                               
         LA    R3,BOELEM                                                        
         USING RPRCMELD,R3                                                      
         ZIC   RF,1(R3)                                                         
         SH    RF,=Y(RPRCMTXT-RPRCMELD)                                         
         BNP   HDRCX                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),RPRCMTXT                                                 
         LA    R2,132(R2)                                                       
*                                                                               
         B     HDRC02                                                           
*                                                                               
HDRC20   DS    0H                                                               
HDRCX    DS    0H                                                               
         L     R1,AIO7             USE 2ND HALF OF IO7 FOR MINIO ELEM           
         LA    R1,MINBLKL(R1)                                                   
         ST    R1,MINELEM          A(AREA FOR ELEM OR CLUSTER)                  
         MVC   MINMAXEL,=Y(IOAREALN-MINBLKL)  MAXLEN(ELEM OR CLUSTER)           
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        HDRUPGD - PRINT UPGRADE EXPRESSIONS                                    
***********************************************************************         
HDRUPGD  NTR1                                                                   
**PFOL   TM    RCTEXT,RCTFTNQ      DOING ANY FOOTNOTES?                         
**PFOL   BNO   HDUPX               NO, DONE                                     
*                                                                               
         L     R2,FSTHEAD          FIND WHERE                                   
         L     RF,AREP                                                          
         LA    RF,REPHA-REPD(RF)                                                
*                                                                               
         LA    R2,(132*5)(R2)                                                   
HDUP01   CLI   0(R2),C' '                                                       
         BNH   HDUP01B                                                          
         LA    R2,132(R2)                                                       
         CR    R2,RF                                                            
         BL    HDUP01                                                           
         B     HDUPX               NOWHERE TO PRINT IT                          
*                                                                               
HDUP01B  DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         LA    RF,BOELEM           USE WORK ELEM                                
         ST    RF,MINELEM                                                       
         MVC   MINMAXEL,=Y(L'BOELEM)   MUST SET RIGHT MAX LENGTH                
         MVI   MINFILTL,1                                                       
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ                                                 
*                                                                               
         BAS   RE,MINIOHI                                                       
         B     *+8                                                              
HDUP02   DS    0H                                                               
         BAS   RE,MINIOSEQ                                                      
         BNE   HDUP30                                                           
*                                                                               
         LA    R3,BOELEM                                                        
         USING RPRBKELD,R3                                                      
         CLI   RPRBKLEN,RPRBKOVQ   USER-DEFINED? (TEST ELEM LENGTH)             
         BNH   HDUP10              NO                                           
*                                                                               
         ZIC   R0,RCNBKS           NUMBER OF BOOKS                              
         LA    RE,RCBKS                                                         
HDUP04   DS    0H                                                               
         CLC   RPRBKIOR,0(RE)      BOOK BEING SHOWN?                            
         BE    HDUP06              YES                                          
         LA    RE,1(RE)                                                         
         BCT   R0,HDUP04                                                        
         B     HDUP10              NO                                           
*                                                                               
HDUP06   MVC   0(L'RPRBKUDF,R2),RPRBKUDF          LABEL                         
         LA    RE,L'RPRBKUDF(R2)                                                
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         MVI   1(RE),C'='                                                       
*                                                                               
         ZIC   RF,RPRBKLEN                COPY THE UPGRADE                      
         SH    RF,=Y(RPRBKUOQ+1)                                                
         BM    HDUP10                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RE),RPRBKUPG                                                 
*                                                                               
         LA    R2,132(R2)                 NEXT LINE                             
*                                                                               
HDUP10   DS    0H                                                               
         B     HDUP02                                                           
         DROP  R3                                                               
*                                                                               
HDUP30   DS    0H                                                               
         L     R1,AIO7             USE 2ND HALF OF IO7 FOR MINIO ELEM           
         LA    R1,MINBLKL(R1)                                                   
         ST    R1,MINELEM          A(AREA FOR ELEM OR CLUSTER)                  
         MVC   MINMAXEL,=Y(IOAREALN-MINBLKL)  MAXLEN(ELEM OR CLUSTER)           
*                                                                               
HDUPX    DS    0H                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        MKTCOM - PRINT MARKET COMMENTS                                         
***********************************************************************         
         DS    0D                                                               
MKTCOMS  NTR1  BASE=*,LABEL=*                                                   
         TM    RCTEXT,RCTMRKQ      DOING MARKET COMMENTS?                       
         BNO   MKTCX                                                            
*                                                                               
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
*                                                                               
         BAS   RE,SETFTCH          SET COMMON FETCH CONTROLS                    
         LA    RF,MKTHOOK          SET HOOK ROUTINE                             
         ST    RF,RFTHOOKA                                                      
         MVI   RFTCTXTT,RFTCTXMQ   GET MKT TEXT                                 
*                                  GET CALL LETTERS                             
         LR    R2,RA                                                            
         AH    R2,=Y(MINSTAS-TWAD)                                              
         LA    R3,L'MINSTAS(R2)                                                 
         USING STALIN,R2                                                        
*                                                                               
MKTC04   DS    0H                                                               
         MVC   RFTCSTAT,STLNSTA     STATION                                     
         MVI   RFTCSTAT+4,C'T'                                                  
         TM    STLNFLG,RPRSTSTL      SATELLITE                                  
         BZ    *+8                                                              
         MVI   RFTCSTAT+4,C'1'                                                  
*                                                                               
         GOTO1 VFETCH,BODMCB,RFTBLKD                                            
*                                                                               
         TM    RCFLAGS1,RCF1COMP                                                
         BZ    MKTCX                                                            
         LA    R2,STLNLENQ(R2)                                                  
         CR    R2,R3                                                            
         BL    MKTC04                                                           
         DROP  R2                                                               
*                                                                               
MKTCX    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
*        MKTHOOK - FETCH HOOK FOR MKT TEXT                                      
***********************************************************************         
         SPACE 1                                                                
MKTHOOK  NTR1                                                                   
         L     R5,AREP                                                          
         USING REPD,R5                                                          
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
*                                                                               
         L     R3,RFTFTX1A         A(FIRST LINE)                                
         SR    R0,R0                                                            
         ICM   R0,1,RFTFTX1N       LINE COUNT                                   
         BZ    MKH04                                                            
*                                                                               
MKH02    DS    0H                                                               
         MVC   REPP1+4(FTCHWDTH),0(R3)                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*                                                                               
         LA    R3,132(R3)                                                       
         BCT   R0,MKH02                                                         
*                                                                               
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)        SPACE AFTER                                  
*                                                                               
MKH04    DS    0H                                                               
         B     EXITOK                                                           
         DROP  R4,R5                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        STACOMS - FOR STATIONS BEING PRINTED                                   
***********************************************************************         
         DS    0D                                                               
STACOMS  NTR1  BASE=*,LABEL=*                                                   
         TM    RCTEXT,RCTSTAQ      DOING STATION COMMENTS?                      
         BNO   STACX                                                            
*                                                                               
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
*                                                                               
         BAS   RE,SETFTCH          SET COMMON FETCH CONTROLS                    
         LA    RF,STSHOOK          SET HOOK ROUTINE                             
         ST    RF,RFTHOOKA                                                      
         MVI   RFTCTXTT,RFTCTXSQ   GET STA TEXT                                 
*                                  GET CALL LETTERS                             
         LR    R2,RA                                                            
         AH    R2,=Y(MINSTAS-TWAD)                                              
         LA    R3,L'MINSTAS(R2)                                                 
         USING STALIN,R2                                                        
*                                                                               
STAC04   DS    0H                                                               
         MVC   RFTCSTAT,STLNSTA     STATION                                     
         MVI   RFTCSTAT+4,C'T'                                                  
         TM    STLNFLG,RPRSTSTL      SATELLITE                                  
         BZ    *+8                                                              
         MVI   RFTCSTAT+4,C'1'                                                  
*                                                                               
         GOTO1 VFETCH,BODMCB,RFTBLKD                                            
*                                                                               
         TM    RCFLAGS1,RCF1COMP                                                
         BZ    STACX                                                            
         LA    R2,STLNLENQ(R2)                                                  
         CR    R2,R3                                                            
         BL    STAC04                                                           
         DROP  R2                                                               
*                                                                               
STACX    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
*        STAHOOK - FETCH HOOK FOR STA TEXT                                      
***********************************************************************         
STSHOOK  NTR1                                                                   
         L     R5,AREP                                                          
         USING REPD,R5                                                          
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
*                                                                               
         L     R3,RFTFTX1A         A(FIRST LINE)                                
         SR    R0,R0                                                            
         ICM   R0,1,RFTFTX1N       LINE COUNT                                   
         BZ    STSH4                                                            
*                                                                               
STSH2    DS    0H                                                               
         MVC   REPP1+4(FTCHWDTH),0(R3)                                          
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*                                                                               
         LA    R3,132(R3)                                                       
         BCT   R0,STSH2                                                         
*                                                                               
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)        SPACE AFTER                                  
*                                                                               
STSH4    DS    0H                                                               
         B     EXITOK                                                           
         DROP  R4,R5                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BLDSKEY - BUILD SORT KEY FOR LINE DATA - RETURN IN SORTKEY             
***********************************************************************         
         DS    0D                                                               
BLDSKEY  NTR1  BASE=*,LABEL=*                                                   
*                                  APPLY ANY FILTERS                            
         L     R2,AIO7                                                          
         L     R2,MINELEM-MINBLKD(R2)                                           
         USING RPRDTELD,R2                                                      
*                                                                               
         TM    RCFLAGS1,RCF1KEEP   SHOWING ONLY 'KEPT' LINES?                   
         BZ    BSKF08              NO                                           
         TM    RPRDTFL1,RPRDTF1H   TEST 'KEPT'                                  
         BZ    BSKNO               NO, SKIP                                     
*                                                                               
BSKF08   DS    0H                                                               
         CLI   RCNDPTS,0           DAYPART FILTER                               
         BE    BSKF10              NO                                           
*                                                                               
         ZIC   R0,RCNDPTS                                                       
         LA    RE,RCDPTS                                                        
*                                                                               
BSKF09   CLC   RPRDTDPT,0(RE)                                                   
         BE    BSKF10                                                           
         LA    RE,1(RE)                                                         
         BCT   R0,BSKF09                                                        
         B     BSKNO                                                            
*                                                                               
BSKF10   DS    0H                                                               
         TM    RCFLAGS1,RCF1COMP   INCLUDE COMPETITION?                         
         BO    BSKF12              YES                                          
         CLI   RPRDTSTA,1          NO, DO ONLY MASTER STATION                   
         BNE   BSKNO                                                            
*                                                                               
BSKF12   DS    0H                                                               
         TM    RCFLAGS2,RCF2SPTS   ONLY LINES WITH SPOTS?                       
         BNO   BSKF14              NO                                           
         OC    RPRDTTSP,RPRDTTSP                                                
         BZ    BSKNO                                                            
         DROP  R2                                                               
*                                                                               
BSKF14   DS    0H                                                               
         LA    R3,SORTCTL          POINT TO SORT CONTROL TABLE                  
*                                                                               
BSK02    DS    0H                                                               
         CLI   0(R3),X'FF'         EOL                                          
         BE    BSKYES                                                           
         L     R2,AIO7                                                          
         L     R2,MINELEM-MINBLKD(R2)                                           
         ZIC   RF,1(R3)            CODE                                         
         SLL   RF,2                                                             
         LA    RF,BSKBR-4(RF)                                                   
         BASR  RE,RF                                                            
         B     BSK04                                                            
*                                                                               
BSKBR    DS    0F                                                               
         B     BSKDAYTM                                                         
         B     BSKSTATN                                                         
         B     BSKDAYPT                                                         
         B     BSKDEMO                                                          
         B     BSKCPP                                                           
         B     BSKCOST                                                          
         B     BSKINV                                                           
         B     BSKSEQN                                                          
*                                                                               
BSK04    DS    0H                                                               
         LA    R3,2(R3)            NEXT SORTCTL ENTRY                           
         B     BSK02                                                            
*                                                                               
BSKYES   B     EXITOK                                                           
BSKNO    B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD SORT KEY ROUTINES                                                       
***********************************************************************         
BSKDAYTM NTR1                      DAY/TIME                                     
         USING RPRDTELD,R2                                                      
         ZIC   RF,0(R3)            DISP INTO KEY                                
         LA    RF,SORTKEY(RF)                                                   
         MVC   0(1,RF),RPRDTDAY    DAY                                          
*                                                                               
         XI    0(RF),DAYMASK                                                    
         TM    0(RF),DAYMASK       SAT/SUN ONLY?                                
         BNO   BSKDYTM5            NO                                           
         XI    0(RF),B'00000011'   FLIP THEM                                    
*                                                                               
BSKDYTM5 ZICM  R0,RPRDTTIM,2                                                    
         SH    R0,TIMESHFT                                                      
         CH    R0,=XL2'000F'                                                    
         BH    *+8                                                              
         AH    R0,=XL2'9600'                                                    
         STCM  R0,3,1(RF)                                                       
         B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
DAYMASK  EQU   B'11111100'         -MASK TO SORT DAYS                           
TIMESHFT DC    XL2'1CB0'           -TIME SHIFT                                  
         EJECT                                                                  
***********************************************************************         
         SPACE                                                                  
BSKSTATN NTR1                      STATION                                      
         USING RPRDTELD,R2                                                      
         ZIC   RF,0(R3)            DISP INTO KEY                                
         LA    RF,SORTKEY(RF)                                                   
*                                                                               
         ZIC   R1,RPRDTSTA                                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(L'MINSTA)                                                  
         LA    R1,0(R1,RA)                                                      
         AH    R1,=Y(MINSTAS-TWAD)                                              
         USING STALIN,R1                                                        
         MVC   0(L'STLNSTA,RF),STLNSTA      STATION                             
         DROP  R1                                                               
*                                                                               
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
         SPACE                                                                  
BSKDAYPT NTR1                      DAYPART                                      
         USING RPRDTELD,R2                                                      
         ZIC   RF,0(R3)            DISP INTO KEY                                
         LA    RF,SORTKEY(RF)                                                   
*                                  FIRST TRY ANY REQUESTED SORT                 
         LA    RE,DPTSEQS                                                       
         SR    R1,R1                                                            
*                                                                               
BSKDP05  CLC   RPRDTDPT,0(RE)                                                   
         BE    BSKDP10                                                          
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         LA    R0,DPTSEQS+L'DPTSEQS                                             
         CR    RE,R0                                                            
         BL    BSKDP05                                                          
*XXXX    MVI   0(RF),X'FF'         NOT FOUND                                    
         MVC   0(1,RF),RPRDTDPT                                                 
         B     BSKDPX                                                           
*                                                                               
BSKDP10  STC   R1,0(RF)                                                         
*                                                                               
BSKDPX   B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
         SPACE                                                                  
BSKDEMO  NTR1                      DEMO VALUE                                   
         MVC   RCBKIOR,RCBKS        PRIMARY BOOK                                
         MVI   ELCODE,RPRDVELQ     SUBMIT RTGS                                  
         TM    RCRTG,RCRSUBQ                                                    
         BO    *+8                                                              
         MVI   ELCODE,RPRNDELQ      OR NEGOTIATED                               
         GOTO1 SETDVALS,BODMCB,(R2),WKDVALS                                     
*                                                                               
         CLI   ELCODE,RPRNDELQ     IF WERE LOOKING FOR NEG                      
         BNE   BSKDEM4                                                          
         OC    WKDVALS,WKDVALS     AND FOUND NOTHING                            
         BNZ   BSKDEM4                                                          
         MVI   ELCODE,RPRDVELQ     USE SUBMIT                                   
         GOTO1 SETDVALS,BODMCB,(R2),WKDVALS                                     
*                                                                               
BSKDEM4  DS    0H                                                               
         MVC   BOFULL1,WKDVALS                                                  
         NI    BOFULL1,X'7F'          STRIP HOB                                 
         XC    BOFULL1,=X'FFFFFFFF'   DESCENDING SORT                           
*                                                                               
BSKDEM8  DS    0H                                                               
         ZIC   RF,0(R3)            DISP INTO KEY                                
         LA    RF,SORTKEY(RF)                                                   
         MVC   0(4,RF),BOFULL1                                                  
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
         SPACE                                                                  
BSKCPP   NTR1                      CPP                                          
         USING RPRDTELD,R2                                                      
*                                                                               
         MVC   RCBKIOR,RCBKS        PRIMARY BOOK                                
         MVI   ELCODE,RPRDVELQ     SUBMIT RTGS                                  
         TM    RCRTG,RCRSUBQ                                                    
         BO    *+8                                                              
         MVI   ELCODE,RPRNDELQ      OR NEGOTIATED                               
         GOTO1 SETDVALS,BODMCB,(R2),WKDVALS                                     
*                                                                               
         CLI   ELCODE,RPRNDELQ     IF WERE LOOKING FOR NEG                      
         BNE   BSKCPP4                                                          
         OC    WKDVALS,WKDVALS     AND FOUND NOTHING                            
         BNZ   BSKCPP4                                                          
         MVI   ELCODE,RPRDVELQ     USE SUBMIT                                   
         GOTO1 SETDVALS,BODMCB,(R2),WKDVALS                                     
*                                                                               
         GOTO1 =A(SETCOSTS),BODMCB,(R2),RR=BORELO                               
*                                                                               
BSKCPP4  DS    0H                                                               
         SR    R0,R0                                                            
         TM    COSTDV1,X'80'       N/A?                                         
         BZ    *+12                NO                                           
         ICM   R0,15,COSTDV1                                                    
         B     BSKCPP8                                                          
*                                                                               
         MVC   BOFULL1,WKDVALS                                                  
         NI    BOFULL1,X'7F'          STRIP HOB                                 
         L     R0,BOFULL1                                                       
         CVD   R0,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ                                                
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5                                                    
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF08B,BODUB1                                                  
*                                                                               
         ICM   R0,15,COSTDV1                                                    
         CVD   R0,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         SR    R0,R0                                                            
         BAS   RE,DIVPACKD         CPP = COST / RATING                          
         BNE   BSKCPP8                                                          
*                                                                               
         ZAP   BODUB1,PCKOF16B                                                  
         CVB   R0,BODUB1                                                        
*                                                                               
BSKCPP8  ZIC   RF,0(R3)            DISP INTO KEY                                
         LA    RF,SORTKEY(RF)                                                   
         STCM  R0,15,0(RF)         CPP                                          
*                                                                               
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
         SPACE                                                                  
BSKCOST  NTR1                      COST                                         
         GOTO1 =A(SETCOSTS),BODMCB,(R2),RR=BORELO                               
*                                                                               
         ZIC   RF,0(R3)            DISP INTO KEY                                
         LA    RF,SORTKEY(RF)                                                   
         MVC   0(4,RF),COSTDV1       SORT ON FIRST DISPLAY COST                 
         XC    0(4,RF),=X'7FFFFFFF'  DESCENDING SORT - N/A STAYS                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
         SPACE                                                                  
BSKINV   NTR1                      INVENTORY NUMBER                             
         USING RPRDTELD,R2                                                      
         ZIC   RF,0(R3)            DISP INTO KEY                                
         LA    RF,SORTKEY(RF)                                                   
         MVC   0(4,RF),RPRDTINM    INVENTORY NUMBER                             
         MVC   RCINVNM,RPRDTINM     SAVE IT                                     
         MVC   4(1,RF),RPRDTSTA    STATION CODE                                 
         MVC   5(3,RF),RPRDTEFF    EFFECTIVE DATE                               
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
         SPACE                                                                  
BSKSEQN  NTR1                      SEQUENCE NUMBER                              
         USING RPRDTELD,R2                                                      
         ZIC   RF,0(R3)            DISP INTO KEY                                
         LA    RF,SORTKEY(RF)                                                   
         MVC   0(1,RF),RPRDTSEQ    SEQUENCE NUMBER                              
         LH    R1,LINECNTR                                                      
         LA    R1,1(R1)                                                         
         STH   R1,LINECNTR                                                      
         MVC   1(2,RF),LINECNTR                                                 
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GETTEXT - GET TEXT ITEM                                                
*   INPUT                                                                       
*              PARAMETER 1  BYTE1    - TEXT TYPE                                
*              PARAMETER 1  BYTE3-4  - TEXT NUMBER                              
*   OUTPUT                                                                      
*              PARAMETER 1  BYTE1    - LENGTH OF TEXT                           
*              AIO1                  - CONTAINS TEXT                            
*                                                                               
***********************************************************************         
GETTEXT  NTR1  BASE=*,LABEL=*                                                   
         OC    2(2,R1),2(R1)       IF NO TEXT NUMBER                            
         BZ    GETTEXTX            RETURN WITHOUT COMMENT                       
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         LA    RF,BOELEM           USE BOELEM                                   
         ST    RF,MINELEM                                                       
         MVC   MINMAXEL,=Y(L'BOELEM)  AND SET RIGHT MAX LENGTH                  
*                                                                               
         MVC   BOBYTE1,0(R1)       SAVE TEXT TYPE                               
         MVC   BOHALF1,2(R1)         AND NUMBER                                 
         LR    R2,R1                                                            
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRTXELQ                                                 
         MVC   MINEKEY+1(1),BOBYTE1                                             
         MVC   MINEKEY+6(2),BOHALF1                                             
         BAS   RE,MINIOHI                                                       
         BNE   GETTXTX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRTXELD,R6                                                      
         CLI   0(R6),RPRTXELQ                                                   
         BNE   GETTXTX                                                          
         CLC   RPRTXTYP,BOBYTE1                                                 
         BNE   GETTXTX                                                          
         CLC   RPRTXSEQ,BOHALF1                                                 
         BNE   GETTXTX                                                          
*                                                                               
         ZIC   RF,1(R6)                                                         
         SH    RF,=Y(RPRTXOVQ)                                                  
         BNP   GETTXTX                                                          
         STC   RF,0(R2)                                                         
         L     RE,AIO1             TEXT INTO AIO1                               
         XC    0(256,RE),0(RE)                                                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),RPRTXTXT                                                 
         DROP  R6                                                               
*                                                                               
GETTXTX  DS    0H                                                               
         L     RF,AIO7             NEED TO RESTORE THIS                         
         LA    RF,MINBLKL(RF)                                                   
         ST    RF,MINELEM                                                       
         MVC   MINMAXEL,=Y(IOAREALN-MINBLKL)                                    
*                                                                               
GETTEXTX B     EXITOK                                                           
         DROP  R5                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRODUCE A COUNT REPORT                                                        
*                                                                               
*   PCKOF06B  IS SALESPERSON COUNT                                              
*   PCKOF08B  IS OFFICE COUNT                                                   
*   PCKOF16B  IS REP COUNT                                                      
*                                                                               
*   BOFULL1   IS THE PREVIOUS SALESPERSON CODE                                  
*   BOFULL2   IS THE PREVIOUS OFFICE CODE                                       
*   ATEMP1    IS THE CURRENT OFFICE CODE                                        
*                                                                               
***********************************************************************         
K        USING RPROKEY,IOKEY                                                    
SK       USING RPROKEY,IOKEYSAV                                                 
*                                                                               
COUNT    NTR1  BASE=*,LABEL=*                                                   
         L     R3,AREP                                                          
         USING REPD,R3                                                          
*                                                                               
         L     RE,=A(CNTHOOKS)                                                  
         A     RE,BORELO                                                        
         ST    RE,REPAUSR                                                       
         MVI   REPHEADH,1                                                       
*                                                                               
         XC    BOFULL1,BOFULL1                                                  
         XC    BOFULL2,BOFULL2                                                  
         ZAP   PCKOF06B,=P'0'                                                   
         ZAP   PCKOF08B,=P'0'                                                   
         ZAP   PCKOF16B,=P'0'                                                   
*                                                                               
         XC    K.RPROKEY,K.RPROKEY                                              
         MVI   K.RPROOTYP,RPROOTYQ                                              
         MVI   K.RPROOSTY,RPROOSBQ                                              
         MVC   K.RPROORCD,CUAALF                                                
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
PC00     CLC   IOKEY(RPROOOFF-RPROKEY),IOKEYSAV                                 
         BNE   PCX                                                              
*                                                                               
         CLI   ASONOFF,ASOFF                                                    
         BNE   PC10                                                             
*                                                                               
         OC    FLTADDDT,FLTADDDT   ADD DATE FILTER?                             
         BNZ   *+14                                                             
         OC    FLTPHCDT,FLTPHCDT   CHANGE DATE FILTER?                          
         BZ    PC10                                                             
*                                                                               
         BAS   RE,GETFRST          GET FIRST RECORD                             
         BNE   PCSEQ               ERROR - SKIP THIS RECORD                     
*                                                                               
         OC    FLTADDDT,FLTADDDT   ADD DATE FILTER?                             
         BZ    PC04                                                             
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
         USING RPRACELD,R6                                                      
PC01     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    PCSEQ               NO ADD DATE, NEXT RECORD                     
         CLI   0(R6),RPRACELQ                                                   
         BNE   PC01                                                             
         CLI   RPRACTYP,RPRACADD                                                
         BNE   PC01                                                             
*                                                                               
         CLC   RPRACDAT,FLTADDDT                                                
         BL    PCSEQ                                                            
         OC    FLTADDDT+3(3),FLTADDDT+3                                         
         BZ    *+14                                                             
         CLC   RPRACDAT,FLTADDDT+3                                              
         BH    PCSEQ                                                            
*                                                                               
PC04     DS    0H                                                               
         OC    FLTPHCDT,FLTPHCDT   CHANGE DATE FILTER?                          
         BZ    PC06                                                             
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
         USING RPRACELD,R6                                                      
PC05     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    PCSEQ               NO PHC DATE, NEXT RECORD                     
         CLI   0(R6),RPRACELQ                                                   
         BNE   PC05                                                             
         CLI   RPRACTYP,RPRACPHC                                                
         BNE   PC05                                                             
*                                                                               
         CLC   RPRACDAT,FLTPHCDT                                                
         BL    PCSEQ                                                            
         OC    FLTPHCDT+3(3),FLTPHCDT+3                                         
         BZ    *+14                                                             
         CLC   RPRACDAT,FLTPHCDT+3                                              
         BH    PCSEQ                                                            
*                                                                               
PC06     DS    0H                                                               
*                                                                               
PC10     OC    BOFULL1,BOFULL1     ANY PREVIOUS SALESPERSON?                    
         BZ    PC12                NO                                           
*                                                                               
         CLC   K.RPROOSAL,BOFULL1  SAME SALESPERSON AS BEFORE?                  
         BE    PC12                YES                                          
*                                                                               
         GOTO1 READSAL,BODMCB,BOFULL1,REPP1                                     
         LA    RE,REPP1+L'RSALNAME                                              
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         MVI   1(RE),C'('                                                       
         MVC   2(L'RPROOSAL,RE),BOFULL1                                         
         MVI   2+L'RPROOSAL(RE),C')'                                            
         EDIT  PCKOF06B,(10,REPP1+50),WRK=BOWORK1,DUB=BODUB1                    
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
         ZAP   PCKOF06B,=P'0'                                                   
*                                                                               
PC12     DS    0H                                                               
         OC    BOFULL2,BOFULL2     ANY PREVIOUS OFFICE?                         
         BZ    PC13                NO                                           
*                                                                               
         CLC   K.RPROOOFF,BOFULL2  SAME OFFICE AS BEFORE?                       
         BE    PC14                YES                                          
*                                                                               
         MVC   REPP1+5(L'RPROOOFF),BOFULL2                                      
         MVC   REPP1+6+L'RPROOOFF(7),=C'TOTAL: '                                
         LA    RE,REPP1+6+L'RPROOOFF+8                                          
         EDIT  PCKOF08B,(10,0(RE)),WRK=BOWORK1,DUB=BODUB1                       
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
         ZAP   PCKOF08B,=P'0'                                                   
         MVI   REPFHEAD,C'Y'                                                    
PC13     MVC   REPP1(8),=CL30'OFFICE:'                                          
         MVC   ATEMP1(L'RPROOOFF),K.RPROOOFF                                    
         LA    R0,ATEMP1                                                        
         GOTO1 READOFF,BODMCB,(R0),REPP1+8                                      
         LA    RE,REPP1+8+L'ROFFNAME                                            
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         MVI   1(RE),C'('                                                       
         MVC   2(L'RPROOOFF,RE),K.RPROOOFF                                      
         MVI   2+L'RPROOOFF(RE),C')'                                            
         MVI   REPP2,0                                                          
         MVC   REPP3(20),=CL20'SALESPERSON'                                     
         MVC   REPP3+50(20),=CL20'PROPOSALS'                                    
         MVC   REPP4(20),DASHES                                                 
         MVC   REPP4+50(20),DASHES                                              
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*                                                                               
PC14     DS    0H                                                               
         MVC   BOFULL1(L'RPROOSAL),K.RPROOSAL                                   
         MVC   BOFULL2(L'RPROOOFF),K.RPROOOFF                                   
         AP    PCKOF06B,=P'1'                                                   
         AP    PCKOF08B,=P'1'                                                   
         AP    PCKOF16B,=P'1'                                                   
*                                                                               
PCSEQ    DS    0H                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOSEQ)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BE    PC00                                                             
         DC    H'0'                                                             
*                                                                               
PCX      DS    0H                                                               
         GOTO1 READSAL,BODMCB,BOFULL1,REPP1                                     
         LA    RE,REPP1+L'RSALNAME                                              
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         MVI   1(RE),C'('                                                       
         MVC   2(L'RPROOSAL,RE),BOFULL1                                         
         MVI   2+L'RPROOSAL(RE),C')'                                            
         EDIT  PCKOF06B,(10,REPP1+50),WRK=BOWORK1,DUB=BODUB1                    
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*                                                                               
         MVC   REPP1+5(L'RPROOOFF),BOFULL2                                      
         MVC   REPP1+6+L'RPROOOFF(7),=C'TOTAL: '                                
         LA    RE,REPP1+6+L'RPROOOFF+8                                          
         EDIT  PCKOF08B,(10,0(RE)),WRK=BOWORK1,DUB=BODUB1                       
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
         MVI   REPP1,C'-'                                                       
         MVC   REPP1+1(80-1),REPP1                                              
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
*                                                                               
         EDIT  PCKOF16B,(10,REPP1),WRK=BOWORK1,DUB=BODUB1                       
         MVC   REPP1+11(16),=CL16'TOTAL PROPOSALS'                              
         L     R1,AREP                                                          
         GOTO1 VREPORT,(R1)                                                     
         B     EXITOK                                                           
         DROP  R3,K,SK                                                          
         EJECT                                                                  
*************************************************************                   
* READSAL - READ SALESPERSON NAME AND RESTORE READ SEQUENCE                     
*                                                                               
* INPUT:                                                                        
*          P1 - SALES PERSON CODE                                               
*          P2 - WHERE TO PUT THE NAME                                           
*         KEY - KEY FOR SEQUENCE RESTORE                                        
*                                                                               
*************************************************************                   
READSAL  NTR1                                                                   
         L     R2,0(R1)            OFFICE CODE                                  
         L     R3,4(R1)            PRINT AREA                                   
*                                                                               
         CLI   ASONOFF,ASOFF                                                    
         BE    *+14                                                             
         MVC   0(L'RSALNAME,R3),=CL(40)'NO LOOKUP'                              
         B     EXITOK                                                           
*                                                                               
         MVC   0(L'RSALNAME,R3),=CL(40)'UNKNOWN'                                
         MVC   BOWORK1(L'IOKEY),IOKEY                                           
*                                                                               
         XC    IOKEY,IOKEY                                                      
SP       USING RSALREC,IOKEY                                                    
         MVI   SP.RSALKTYP,X'06'                                                
         MVC   SP.RSALKREP,CUAALF                                               
         MVC   SP.RSALKSAL,0(R2)                                                
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RSALKEY),IOKEYSAV                                        
         BNE   RDSAL10                                                          
         DROP  SP                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGET)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO4                                                          
         USING RSALREC,R4                                                       
         MVC   0(L'RSALNAME,R3),RSALNAME                                        
         DROP  R4                                                               
*                                                                               
RDSAL10  DS    0H                  RESTORE SEQUENCE                             
         MVC   IOKEY,BOWORK1                                                    
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(RPROOOFF-RPROKEY),IOKEYSAV                                 
         BE    *+6                                                              
         DC    H'0'                WHAT HAPPENED?                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*************************************************************                   
* READOFF - READ OFFICE NAME AND RESTORE READ SEQUENCE                          
*                                                                               
* INPUT:                                                                        
*          P1 - OFFICE CODE                                                     
*          P2 - WHERE TO PUT THE NAME                                           
*         KEY - KEY FOR SEQUENCE RESTORE                                        
*                                                                               
*************************************************************                   
READOFF  NTR1                                                                   
         L     R2,0(R1)            OFFICE CODE                                  
         L     R3,4(R1)            PRINT AREA                                   
*                                                                               
         CLI   ASONOFF,ASOFF                                                    
         BE    *+14                                                             
         MVC   0(L'RSALNAME,R3),=CL(40)'NO LOOKUP'                              
         B     EXITOK                                                           
*                                                                               
         MVC   BOWORK1(L'IOKEY),IOKEY                                           
         MVC   0(L'ROFFNAME,R3),=CL(40)'UNKNOWN'                                
*                                                                               
         XC    IOKEY,IOKEY                                                      
OF       USING ROFFREC,IOKEY                                                    
         MVI   OF.ROFFKTYP,X'04'                                                
         MVC   OF.ROFFKREP,CUAALF                                               
         MVC   OF.ROFFKOFF,0(R2)                                                
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'ROFFKEY),IOKEYSAV                                        
         BNE   RDOFF10                                                          
         DROP  OF                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGET)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO4                                                          
         USING ROFFREC,R4                                                       
         MVC   0(L'ROFFNAME,R3),ROFFNAME                                        
         DROP  R4                                                               
*                                                                               
RDOFF10  DS    0H                  RESTORE SEQUENCE                             
         MVC   IOKEY,BOWORK1                                                    
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(RPROOOFF-RPROKEY),IOKEYSAV                                 
         BE    *+6                                                              
         DC    H'0'                WHAT HAPPENED?                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET THE FIRST RECORD IN THE SET                                               
***********************************************************************         
GETFRST  NTR1                                                                   
         MVC   BOWORK1(L'IOKEY),IOKEY                                           
         XC    IOKEY,IOKEY                                                      
K        USING RPROKEY,IOKEY                                                    
P        USING RPROKEY,BOWORK1                                                  
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
         MVC   IOKEY(L'RPROKEY),BOWORK1                                         
         ICM   R1,15,=AL4(XIO11+XOREPDIR+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                 CAN'T RESTORE SEQUENCE                       
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'RPROKEY),BOWORK1                                         
         BE    *+6                 CAN'T RESTORE SEQUENCE                       
         DC    H'0'                                                             
*                                                                               
         LTR   R5,R5                                                            
         BZ    EXITOK                                                           
         B     EXITL                                                            
         EJECT                                                                  
*************************************************************                   
* COUNT HEADHOOK                                                                
*************************************************************                   
CNTHOOKS NTR1                                                                   
         L     R3,AREP                                                          
         USING REPD,R3                                                          
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(1)                                                     
         BNE   EXITOK                                                           
         MVC   REPH1(L'BCORGNAM),BCORGNAM                                       
*                                                                               
         LA    R2,REPH2+41                                                      
         MVC   0(50,R2),REPTITLE                                                
         OC    0(50,R2),BCSPACES                                                
         GOTO1 CENTER,BODMCB,(R2),50                                            
         GOTO1 UNDERLIN,BODMCB,(50,0(R2)),132(R2)                               
*                                                                               
         LA    R2,REPH1+100                                                     
         MVC   (00*132)+000(32,R2),=CL32'DONAVAN DATA SYSTEMS'                  
         MVC   (01*132)+000(10,R2),=CL32'REQUESTED:'                            
         LA    R0,(01*132)+011(R2)                                              
         GOTO1 VDATCON,BODMCB,(5,0),(11,(R0))                                   
         LA    R2,(02*132)+000(R2)                                              
*                                                                               
         OC    FLTADDDT,FLTADDDT                                                
         BZ    CNTHK03                                                          
*                                                                               
         MVC   000(04,R2),=C'ADD:'                                              
         LA    R0,005(R2)                                                       
         GOTO1 VDATCON,BODMCB,(8,FLTADDDT),(11,(R0))                            
         MVI   013(R2),C'-'                                                     
*                                                                               
         OC    FLTADDDT+3(3),FLTADDDT+3                                         
         BZ    CNTHK02                                                          
*                                                                               
         LA    R0,014(R2)                                                       
         GOTO1 (RF),(R1),(8,FLTADDDT+3),(11,(R0))                               
*                                                                               
CNTHK02  DS    0H                                                               
         LA    R2,(01*132)+000(R2)                                              
CNTHK03  DS    0H                                                               
         OC    FLTPHCDT,FLTPHCDT                                                
         BZ    CNTHK05                                                          
*                                                                               
         MVC   000(04,R2),=C'PHC:'                                              
         LA    R0,005(R2)                                                       
         GOTO1 VDATCON,BODMCB,(8,FLTPHCDT),(11,(R0))                            
         MVI   013(R2),C'-'                                                     
*                                                                               
         OC    FLTPHCDT+3(3),FLTPHCDT+3                                         
         BZ    CNTHK04                                                          
*                                                                               
         LA    R0,014(R2)                                                       
         GOTO1 (RF),(R1),(8,FLTPHCDT+3),(11,(R0))                               
*                                                                               
CNTHK04  DS    0H                                                               
         LA    R2,(01*132)+000(R2)                                              
CNTHK05  DS    0H                                                               
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SORT WORDS TABLE   WORD(4),CODE(1)                                            
***********************************************************************         
SORTWDS  DS    0X                                                               
         DC    C'DTIM',AL1(01)                                                  
         DC    C'DTM ',AL1(01)                                                  
         DC    C'DT  ',AL1(01)                                                  
         DC    C'STA ',AL1(02)                                                  
         DC    C'ST  ',AL1(02)                                                  
         DC    C'S   ',AL1(02)                                                  
         DC    C'DPT ',AL1(03)                                                  
         DC    C'DP  ',AL1(03)                                                  
         DC    C'RTG ',AL1(04)                                                  
         DC    C'RT  ',AL1(04)                                                  
         DC    C'R   ',AL1(04)                                                  
         DC    C'DEMO',AL1(04)                                                  
         DC    C'DEM ',AL1(04)                                                  
         DC    C'DE  ',AL1(04)                                                  
         DC    C'DMO ',AL1(04)                                                  
         DC    C'DM  ',AL1(04)                                                  
         DC    C'CPP ',AL1(05)                                                  
         DC    C'CP  ',AL1(05)                                                  
         DC    C'COST',AL1(06)                                                  
         DC    C'COS ',AL1(06)                                                  
         DC    C'CO  ',AL1(06)                                                  
         DC    C'CST ',AL1(06)                                                  
         DC    C'CS  ',AL1(06)                                                  
         DC    C'INV ',AL1(07)                                                  
         DC    C'IN  ',AL1(07)                                                  
         DC    C'I   ',AL1(07)                                                  
         DC    X'FF'                                                            
SORTWDL  EQU   4+1                                                              
         EJECT                                                                  
***********************************************************************         
* SORT OPTION TABLE     LENGTH(1),CODE(1)                                       
***********************************************************************         
BSTTAB   DS    0X                                                               
         DC    AL1(03,01)          DAY/TIME                                     
         DC    AL1(05,02)          STATION                                      
         DC    AL1(01,03)          DAYPART                                      
         DC    AL1(04,04)          RATING                                       
         DC    AL1(04,05)          CPP                                          
         DC    AL1(04,06)          COST                                         
         DC    AL1(08,07)          INVENTORY NUMBER                             
         DC    AL1(03,08)          SEQUENCE                                     
         DC    X'FF'                                                            
BSTTABL  EQU   6                                                                
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
DPTUKNX  DC    X'FF',C'???'        END OF TABLE                                 
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
KNOWTAB  DS    0XL(KNOWLQ)                                                      
* KEY PORTION                                                                   
         DC    AL2(00001),AL4(CONDTA)    CONTRACT                               
         DC    AL2(00002),AL4(PRODTA)    PROPOSAL                               
* RECORD (PROTECTED PORTION)                                                    
         DC    AL2(00003),AL4(AGYDTA)    AGENCY                                 
         DC    AL2(00004),AL4(ADVDTA)    ADVERTISER                             
         DC    AL2(00005),AL4(PRDDTA)    PRODUCT                                
         DC    AL2(00006),AL4(SALDTA)    SALESPERSON                            
         DC    AL2(00049),AL4(STLDTA)    SATELLITE - PROTECTED                  
         DC    AL2(00020),AL4(BYRDTA)    BUYER                                  
         DC    AL2(00007),AL4(STADTA)    STATION                                
         DC    AL2(00008),AL4(FLTDTA)    FLIGHT DATES                           
         DC    AL2(00021),AL4(DSKDTA)    DISK ADDRESS                           
         DC    AL2(00039),AL4(DVSDTA)    DEV SALESPERSON                        
         DC    AL2(00040),AL4(DVTDTA)    DEV CONTRACT TYPE                      
         DC    AL2(00053),AL4(CSTDTA)    COMPETITIVE STATIONS                   
* RECORD (INPUT PORTION)                                                        
         DC    AL2(00054),AL4(RTPDTA)    REPORT STYLE                           
         DC    AL2(00055),AL4(COPDTA)    NUMBER OF COPIES                       
         DC    AL2(00073),AL4(WHNDTA)    RUN WHEN                               
         DC    AL2(00074),AL4(REQDTA)    REQUESTOR                              
         DC    AL2(00056),AL4(DSTDTA)    DESTINATION                            
         DC    AL2(00057),AL4(RTLDTA)    REPORT TITLE                           
         DC    AL2(00058),AL4(CPPDTA)    CPP OPTIONS                            
         DC    AL2(00059),AL4(TXTDTA)    TEXT OPTIONS                           
         DC    AL2(00060),AL4(DPTDTA)    DAYPART FILTER                         
         DC    AL2(00061),AL4(CMPDTA)    COMPETITION                            
         DC    AL2(00062),AL4(INVDTA)    INVENTORY CODE                         
         DC    AL2(00066),AL4(COSDTA)    COST                                   
         DC    AL2(00063),AL4(SHLDTA)    SHARES/LEVELS                          
         DC    AL2(00064),AL4(SRTDTA)    SORT                                   
         DC    AL2(00065),AL4(OPTDTA)    OPTIONS                                
         DC    AL2(00075),AL4(CVRDTA)    COVERSHEET                             
         DC    AL2(00076),AL4(DBLDTA)    DOUBLE SPACE                           
         DC    AL2(00077),AL4(PORDTA)    PORTRAIT                               
         DC    AL2(00081),AL4(HLPDTA)    HELP FIELD                             
*                                                                               
         DC    AL2(EOT)                                                         
               EJECT                                                            
***********************************************************************         
* HELP TEXT LINES                                                               
***********************************************************************         
HELPTEXT DS    0CL80                                                            
         DC    CL80'Y=Yes,B=Buyer''s and Target Rate'                           
         DC    CL80'A=All,M=Market,S=Station,F=Footnote'                        
         DC    CL80'I=Inventory,I+=All Inventory'                               
         DC    CL80'P=80, N=110, S=132'                                         
HLPNUMMX EQU   (*-HELPTEXT)/L'HELPTEXT                                          
               EJECT                                                            
***********************************************************************         
* PRINTLINE(PAGE) BUFFER                                                        
***********************************************************************         
PRNTBUFF DS    0X                                                               
         PRINT OFF                                                              
         DC    (BUFFLNS)CL(132)' '                                              
         PRINT ON                                                               
         DC    X'FF'                                                            
***********************************************************************         
* REPORT STYLE DSECT                                                            
***********************************************************************         
RSTYLED  DSECT                                                                  
RSTYLNAM DS    CL8                 NAME                                         
RSTYLWID DS    XL1                 DEFAULT REPORT WIDTH                         
RSTYLSTL DS    XL1                 STYLE EQUATE                                 
RSTYLPFK DS    XL1                 ASSOCIATED PFKEY                             
RSTYLLNQ EQU   *-RSTYLED                                                        
*                                                                               
***********************************************************************         
* KNOWN DATA OBJECTS DSECT                                                      
***********************************************************************         
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
CENTER   DS    A                                                                
UNDERLIN DS    A                                                                
CHOPPER  DS    A                                                                
ATSAR    DS    A                                                                
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
AFRREL   DS    A                                                                
AERRFLD  DS    A                                                                
FSTHEAD  DS    A                                                                
FOOTDSP  DS    A                                                                
AMELEM   DS    A                   MINELEM                                      
APRNTBFF DS    A                   PRNTBUFF                                     
APRNTLOC DS    A                   LOCATION IN PRINTLINE                        
ATEMP1   DS    A                                                                
ATEMP2   DS    A                                                                
AOPTFLD  DS    A                                                                
*                                                                               
TBUFFADR DS    A                                                                
TBUFFLEN DS    H                                                                
*                                                                               
LINECNTR DS    H                                                                
*                                                                               
HELPLINE DS    X                   WHICH HELP LINE WERE ON                      
*                                                                               
FLTADDDT DS    XL6                 ADD DATE FILTER                              
FLTPHCDT DS    XL6                 PRO HEADER CHANGE DATE FILTER                
*                                                                               
ELPARMS  DS    XL6 - TO PASS TO REPRO26                                         
ELCODE   DS    X                                                                
*                                                                               
SVMINKY  DS    XL7                                                              
*                                                                               
SELPROFS DS    0CL10                CONTRACT PROFILES                           
         DS    CL1                 PROGRAM #                                    
         DS    CL1                 SPARE                                        
SELPROF  DS    CL8                 PROFILE BITS                                 
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1KYCHG EQU   X'80'                - KEY FIELD CHANGED                         
MF1TPCHG EQU   X'40'                - REPORT TYPE CHANGED                       
MF1LINES EQU   X'20'                - REPORT HAS SOME LINES                     
MF1INVRP EQU   X'10'                - REPEATED INVENTORY # ON LINE              
MF1CSTPR EQU   X'08'                - COST PRINTED                              
MF1DPTSR EQU   X'04'                - SORTED BY DAYPART FIRST                   
MF1TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
STYLE    DS    X                   REPORT STYLE (> 10 CANNOT RUN NOW)           
RPTQAQ   EQU   1                   QUICK AVAIL                                  
RPTPKQ   EQU   2                   PACKAGE                                      
RPTLAQ   EQU   11                  LONG AVAIL                                   
RPTCNQ   EQU   22                  COUNT REPORT                                 
*                                                                               
PWHEN    DS    X                                                                
PREQ     DS    CL3                                                              
REPTITLE DS    CL70                                                             
*                                                                               
ECONDATE DS    CL17                                                             
ECONLENS DS    CL25                                                             
ECONNUM  DS    CL8                                                              
EPRONUM  DS    CL3                                                              
ESALTEL  DS    CL12                                                             
EMKTNAME DS    CL20                                                             
*                                                                               
DESCFLGS DS    X                   SAVED DESCRIPTION ELEMENT FLAGS              
*                                                                               
SAVOPTNS DS    XL1                 OPTIONS                                      
OPTNTXTQ EQU   X'80'                - TEXT BIT                                  
OPTNDECQ EQU   X'40'                - DEMO DECIMAL PRECISION BIT                
*                                                                               
PCKOF06B DS    PL6                 PACKED OF 6  BYTES                           
PCKOF08B DS    PL8                 PACKED OF 8  BYTES                           
PCKOF16B DS    PL16                PACKED OF 16 BYTES                           
*                                                                               
         EJECT                                                                  
******************                                                              
** FROM REPRP10 **                                                              
******************                                                              
MYRWDTH  DS    X                   REPORT WIDTH                                 
RPTWID   DS    X                                                                
RCBKIOR  DS    X                                                                
RCINVNM  DS    CL4                                                              
*                                                                               
RCNBKS   DS    X                                                                
RCBKS    DS    XL(NUMBKS+1)        ACTIVE BOOK LIST (DISPLAY ORDER)             
RCNDMOS  DS    X                                                                
RCDEMS   DS    XL(NUMDEMS+1)       ACTIVE DEMO LIST        ""                   
RCNCSTS  DS    X                                                                
RCCSTS   DS    XL(NUMCSTS+1)       ACTIVE COST LIST        ""                   
RCNTAGS  DS    X                                                                
RCNCBKS  DS    X                                                                
RCCBKS   DS    XL(NUMBKS+1)        ACTIVE CPP BOOK LIST    ""                   
RCNDPTS  DS    X                                                                
RCDPTS   DS    XL(10)              DAYPART FILTERS                              
RCDPTF3  DS    CL3                 LONG FORM OF FIRST FILTER                    
*                                                                               
RGTLNS   DS    X                                                                
LFTLNS   DS    X                                                                
TMPLNS   DS    X                                                                
*                                                                               
SAVINV   DS    CL4                                                              
*                                                                               
HKHFLGS  DS    XL1                  HEAD HOOK FLAGS                             
HKHCALQ  EQU   X'80'                - CALCULATE WIDTH MODE                      
HKHNCOLQ EQU   X'40'                - NO COLUMN HEADINGS ON THIS PAGE           
HKHBLNKQ EQU   X'20'                - BLANK PAGE                                
*                                                                               
RCCPP    DS    CL1                 P=PRIMARY DEMO CPP, A=ALL DEMOS              
RCCPCOLQ EQU   X'80'               PRIME DEMO ONLY (CPP AS COL)                 
RCCPSTKQ EQU   X'40'               ALL DEMOS (CPP STACKED)                      
RCPBYRQ  EQU   X'20'               BUYER'S                                      
*                                                                               
RCCOST   DS    XL1                                                              
RCCSUBQ  EQU   X'80'               SUBMITTED                                    
RCCNEGQ  EQU   X'40'               NEGOTIATED                                   
RCCTRGQ  EQU   X'20'               TARGET                                       
RCCCOLQ  EQU   X'10'               COST COLUMN                                  
*                                                                               
RCRTG    DS    XL1                                                              
RCRSUBQ  EQU   X'80'               SUBMITTED                                    
RCRNEGQ  EQU   X'40'               NEGOTIATED                                   
*                                                                               
RCTEXT   DS    XL1                 TEXT OPTIONS                                 
RCTALLQ  EQU   X'FF'               ALL TEXT                                     
RCTINVQ  EQU   X'80'               INVENTORY TEXT                               
RCTSTAQ  EQU   X'40'               STATION TEXT                                 
RCTMRKQ  EQU   X'20'               MARKET TEXT                                  
RCTFTNQ  EQU   X'08'               FOOTNOTE TEXT                                
RCTINVPQ EQU   X'04'+RCTINVQ       ALL INVENTORY TEXT(I+)                       
*                                                                               
RCFLAGS1 DS    XL1                 REPORT SWITCHES 1                            
RCF1COMP EQU   X'80'                - COMPETITION OPTION                        
RCF1SHR  EQU   X'40'                - SHARE OPTION                              
RCF1PUT  EQU   X'20'                - PUT(LEVEL) OPTION                         
RCF1INV  EQU   X'10'                - INVENTORY NUMBER OPTION                   
RCF1RATE EQU   X'08'                - COST OPTION                               
RCF1DPTL EQU   X'04'                - DAYPART TOTAL SWITCH                      
RCF1RND  EQU   X'02'                - ROUND RATING                              
RCF1KEEP EQU   X'01'                - SHOW ONLY KEPT LINES                      
*                                                                               
RCFLAGS2 DS    XL1                 REPORT SWITCHES 2                            
RCF2SPTS EQU   X'80'                - SHOW ONLY LINES WITH SPOTS                
RCF2NDPT EQU   X'40'                - DON'T SHOW DAYPART COLUMN                 
RCF2ETXT EQU   X'20'                - MARKET & STATION TEXT AT END              
RCF2DBSP EQU   X'10'                - DOUBLE SPACE THE REPORT                   
RCF2COVR EQU   X'08'                - COVERSHEET OPTION                         
RCF2PORT EQU   X'04'                - PORTRAIT OPTION                           
RCF2EFDT EQU   X'02'                - SHOW EFFECTIVE DATES                      
*                                                                               
LASTDPT  DS    CL1                                                              
DPTSEQS  DS    CL26                DAYPART SORTING TABLE                        
         DS    0F                                                               
*                                                                               
WDEM     DS    F                   WORKING DEMOVALUE                            
*                                                                               
SUDVALS  DS    XL(4*3*NUMDEMS)       SUBMIT DEMO VALS                           
NGDVALS  DS    XL(4*3*NUMDEMS)       NEGOTIATED DEM VALS                        
WKDVALS  DS    XL(4*3*NUMDEMS)       WORK AREA FOR DEMVALS                      
*                                                                               
COSTVALS DS    0XL(8*NUMCSTS)                                                   
COSTIV1  DS    F                   VALUES IN INTERNAL SEQUENCE                  
COSTIV2  DS    F                                                                
COSTIV3  DS    F                                                                
COSTIV4  DS    F                                                                
COSTDV1  DS    F                   VALUES IN DISPLAY SEQUENCE                   
COSTDV2  DS    F                                                                
COSTDV3  DS    F                                                                
COSTDV4  DS    F                                                                
*                                                                               
LINVALS  DS    0F                  LINE VALUES TO CREATE TOTALS                 
         DS    XL2                 KEEP ALIGHNMENT                              
LSPOTS   DS    XL2                                                              
LCOST1   DS    F                                                                
LCOST2   DS    F                                                                
LCOST3   DS    F                                                                
LCOST4   DS    F                                                                
LRTGS    DS    XL(4*NUMDEMS)                                                    
LRTG     DS    (NUMDEMS)F                                                       
LNVALSLQ EQU   *-LINVALS                                                        
*                                                                               
RCTAGPOS DS    A                   ROW TAG                                      
RCDEMPOS DS    A                   DEMO                                         
RCDEMPBK DS    A                   PRIME BOOK POS                               
RCCSTPOS DS    A                                                                
RCCPPPOS DS    A                                                                
DEMTOCST DS    F                   DISPLACEMNT TO COST                          
*                                                                               
SVDPOS   DS    0XL(4*9)            SAVED COL POSITIONS (IN 1ST LINE)            
CPPPOS   DS    A                   CPP                                          
CSTPOS   DS    A                   RATE                                         
CDSCPOS  DS    A                   COST DESCRIPTION                             
RTGPOS   DS    A                   RATING                                       
SPTPOS   DS    A                   SPOTS                                        
PGMPOS   DS    A                   PROGRAM                                      
EODPOS   DS    A                                                                
BCPPPOS  DS    A                   BUYER CPP                                    
TRGTRPOS DS    A                   TARGET RATE                                  
*                                                                               
MAXSRTS  EQU   7                                                                
SORTREQ  DS    XL(MAXSRTS+1)                                                    
SORTCTL  DS    XL(MAXSRTS*2+1)                                                  
SORTKEY  DS    XL(36)                                                           
FMTCTL   DS    XL(20)                                                           
         EJECT                                                                  
*****************************                                                   
** STUFF FOR TSAR BUFFER 2 **                                                   
*****************************                                                   
T2SVKEY  DS    XL(T2SKEYLQ)        LAST T2KEY READ                              
LSTSINDS DS    XL1                 TSAR BUFFER 2 INDICATORS                     
LSTSIINI EQU   X'80'               INITIALISED                                  
LSTSIRES EQU   X'40'               RESTORED                                     
LSTSLOWP DS    XL1                 LOW TSAR PAGE NUMBER                         
LSTSNUMP DS    XL1                 NUMBER OF PAGES ALLOCATED                    
*                                                                               
TS2ACTN  DS    XL1                 REQUESTED ACTION                             
*                                                                               
T2LST    DS    XL(T2SLSTLQ)        TSAR2 RECORD BUFFER                          
         ORG   T2LST                                                            
T2NUM    DS    XL2                                                              
T2REC    DS    0X                                                               
T2KEY    DS    0X                                                               
         ORG                                                                    
*                                                                               
T2BUFF   DS    XL(TSARDL)          BUFFER FOR SECOND TSAR BUFFER                
*                                                                               
         EJECT                                                                  
*                                  LENGTHS FROM T2SLST USED FOR TSAR2           
T2SLST   DSECT                     SORT KEY TSAR RECORD                         
T2SNUM   DS    XL2                 RECORD NUMBER                                
T2SREC   DS    0X                                                               
T2SKEY   DS    0X                                                               
T2SKTYP  DS    XL1                 TSAR RECORD TYPE                             
T2SKTYPQ EQU   1                   TSAR RECORD TYPE EQUATE                      
T2SKYCON DS    XL(L'SORTKEY)       ACTUAL SORT KEY                              
T2SKYSEQ DS    XL1                 SEQUENCE NUMBER                              
T2SKEYLQ EQU   *-T2SKEY            LENGTH OF KEY                                
T2SMNKEY DS    XL7                 MINIO ELEMENT KEY                            
T2SRECLQ EQU   *-T2SREC            LENGTH OF TSAR RECORD                        
T2SLSTLQ EQU   *-T2SLST            LENGTH OF TSAR RECORD                        
*                                                                               
T2ILST   DSECT                     INVENTORY TSAR RECORD                        
T2INUM   DS    XL2                 RECORD NUMBER                                
T2IREC   DS    0X                                                               
T2IKEY   DS    XL(T2SKEYLQ)                                                     
         ORG   T2IKEY                                                           
T2IKTYP  DS    XL1                 TSAR RECORD TYPE                             
T2IKTYPQ EQU   2                   TSAR RECORD TYPE EQUATE                      
T2IKSTA# DS    XL1                 STATION INTERNAL ORDER #                     
T2IKINV# DS    CL4                 INVENTORY #                                  
T2IKEYLQ EQU   *-T2IKEY            LENGTH OF KEY                                
         ORG                                                                    
T2IFLGS  DS    XL1                 INVENTORY # FLAGS                            
T2IF1REP EQU   X'80'                - INVENTORY REPEATED                        
T2IRECLQ EQU   *-T2IREC            LENGTH OF TSAR RECORD                        
T2ILSTLQ EQU   *-T2ILST            LENGTH OF TSAR RECORD                        
*                                                                               
         EJECT                                                                  
TOTSD    DSECT                     TOTALS DSECT                                 
TOTCOST1 DS    PL16                GRAND TOTAL COST 1                           
TOTCOST2 DS    PL16                GRAND TOTAL COST 2                           
TOTCOST3 DS    PL16                GRAND TOTAL COST 3                           
TOTCOST4 DS    PL16                GRAND TOTAL COST 4                           
TOTSPOTS DS    PL6                 GRAND TOTAL SPOTS                            
TOTLINES DS    PL3                 GRAND TOTAL NUMBER OF DETAILS                
TOTRTGS  DS    (NUMDEMS)PL8        GRAND TOTAL RATING                           
TOTSLENQ EQU   *-TOTSD                                                          
         EJECT                                                                  
       ++INCLUDE REPROLN                                                        
NUMLENS  EQU   6                                                                
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
MINCOSTS DS    0XL(NUMCSTS*CSLNLENQ) SAVED COSTS                                
MINCOST  DS    (NUMCSTS)XL(CSLNLENQ)                                            
*                                                                               
MINDPTS  DS    0CL(NUMDPTS*DPLNLENQ) SAVED DAYPARTS                             
MINDPT   DS    (NUMDPTS)XL(DPLNLENQ)                                            
         DS    XL(DPLNLENQ)          EOT                                        
*                                                                               
GRDTOTS  DS    XL(TOTSLENQ)        GRAND TOTALS                                 
DPTTOTS  DS    XL(TOTSLENQ)        DAYPART TOTALS                               
*                                                                               
         DS    XL(TWUSER+L'TWUSER-*)   # OF SPARE BEFORE TWSECBLK               
* REFETCHD                                                                      
         PRINT OFF                                                              
       ++INCLUDE REFETCHD                                                       
         PRINT ON                                                               
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
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
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
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024REPRO10S  05/01/02'                                      
         END                                                                    
