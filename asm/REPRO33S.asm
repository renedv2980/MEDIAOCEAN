*          DATA SET REPRO33S   AT LEVEL 021 AS OF 08/20/97                      
*&&      SET   NOP=N                                                            
*PHASE T80A33A                                                                  
T80A33   TITLE 'REPRO33 - REP PROPOSALS WORK MAINTENANCE'                       
PRO33    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REPRO33*,R7,RR=RE                                              
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
A#WRKADD EQU   18                  WORK/ADD ACTION EQUATE                       
KYDEF    EQU   C'3'                                                             
DTDEF    EQU   C'3'                                                             
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
         L     RF,=A(TABLEOO)          KNOWN OBJECTS                            
         A     RF,BORELO                                                        
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
         LR    RE,RA                                                            
         AH    RE,=Y(APPLID-TWAD)                                               
         CLC   =C'WRKADD',0(RE)                                                 
         BE    INIT02                                                           
*                                                                               
         LA    RF,L'TWUSER                                                      
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
         LR    RE,RA                                                            
         AH    RE,=Y(APPLID-TWAD)                                               
         MVC   0(6,RE),=C'WRKADD'                                               
*                                                                               
INIT02   DS    0H                                                               
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
         NI    MISCFLG1,X'FF'-MF1KYCHG                                          
         LR    RE,RA                                                            
         AH    RE,=Y(SEQINKEY-TWAD)                                             
         MVI   0(RE),0             CLEAR OUT THE SEQ NUMBER FIRST               
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
         NI    GSINDSL1,FF-GSIDISCL    YES, WANT RDIS 1ST BEFORE RVAL           
         CLI   CSACT,A#WRKADD      ARE WE ADDING A RECORD?                      
         BNE   KLKVX                                                            
         TM    MISCFLG1,MF1KYCHG   YES, DID OUR KEY CHANGE?                     
         BZ    KLKVX                   NO, DON'T WANT RDIS AGAIN                
         OI    GSINDSL1,GSIDISCL       YES, WANT RDIS 1ST BEFORE RVAL           
*                                                                               
KLKVX    B     EXITOK                                                           
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE THE I/O CALL TO ADD THE RECORD                                         
***********************************************************************         
RFRADD   DS    0H                                                               
         GOTO1 =A(R1STRADD),BODMCB,(R9),RR=BORELO                               
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER THE I/O CALL TO ADD THE RECORD                                          
***********************************************************************         
RLRADD   DS    0H                                                               
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
         CLI   SELPROFS,RREPQSEL                                                
         BE    DFDDIS0                                                          
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
DFDDIS0  DS    0H                                                               
*                                                                               
         BAS   RE,RDCRTICL         READ CRITICAL INFORMATION                    
*                                                                               
         TM    MISCFLG1,MF1PFRET   RETURNING FROM CALLED SESSION?               
         BNZ   EXITL                                                            
*                                                                               
         MVI   ONPRGLIN,0          ON WHAT PROGRAM LINE                         
         MVI   ONSDTLIN,0          ON WHAT SECONDARY DAY/TIME LINE              
         MVI   ONADTLIN,0          ON WHAT AVAIL DAY/TIME LINE                  
         MVI   ONDMORTG,0          ON WHAT DEMO RATING                          
         MVI   ONBOOKLN,0          ON WHAT BOOK FIELD                           
         MVI   ONPRBKLN,0          ON WHAT BOOK FIELD                           
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         TM    MISCFLG1,MF1KYCHG   YES, DID OUR KEY CHANGE?                     
         BNZ   DFDDIS2             YES                                          
         BAS   RE,FROMAIO5                                                      
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         MVC   SVKYDYPT,RPRDTDPT                                                
         DROP  R6                                                               
         B     DFDDISX                                                          
*                                                                               
DFDDIS2  DS    0H                                                               
         L     R0,MINELEM          CLEAR OUT MINELEM                            
         LH    R1,MINMAXEL                                                      
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SVDMOELM,SVDMOELM                                                
*                                                                               
         CLC   SVKYINVT,BCSPACES   INVENTORY # GIVEN??                          
         BNH   DFDDIS3             NO                                           
         GOTO1 =A(HDRFETCH),BODMCB,RR=BORELO                                    
         B     DFDDIS5                                                          
*                                                                               
DFDDIS3  L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         MVI   RPRDTEL,RPRDTELQ                                                 
         MVI   RPRDTLEN,RPRDTOVQ   LENGTH W/O WEEKS                             
         MVC   RPRDTINM,SVKYINVT   INVENTORY NUMBER                             
         MVC   RPRDTEFF,SVKYEFDT   EFFECTIVE DATES                              
         MVC   RPRDTSLN,SAVSLNS    FIRST LENGTH IS DEFAULT                      
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         MVC   RPRDTSTA,0(RE)      INTERNAL STATION CODE                        
         LR    RE,RA                                                            
         AH    RE,=Y(MINCSEL-TWAD)                                              
         XC    0(L'MINCSEL,RE),0(RE)                                            
         DROP  R6                                                               
*                                                                               
DFDDIS5  DS    0H                                                               
         MVC   MINELEML,=Y(RPRDTOVQ)                                            
****     BAS   RE,INTOAIO5                                                      
*                                                                               
DFDDISX  B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* BEFORE VALIDATING THE DATA FIELDS                                             
***********************************************************************         
DFDVAL   DS    0H                                                               
         BAS   RE,RDCRTICL                                                      
*                                                                               
         MVI   ONPRGLIN,0          ON WHAT PROGRAM LINE                         
         MVI   ONSDTLIN,0          ON WHAT SECONDARY DAY/TIME LINE              
         MVI   ONADTLIN,0          ON WHAT AVAIL DAY/TIME LINE                  
         MVI   ONDMORTG,0          ON WHAT DEMO RATING                          
         MVI   ONBOOKLN,0          ON WHAT BOOK FIELD                           
         MVI   ONPRBKLN,0          ON WHAT BOOK FIELD                           
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         L     R0,MINELEM          CLEAR OUT MINELEM                            
         LH    R1,MINMAXEL                                                      
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SVDMOELM,SVDMOELM                                                
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         MVI   RPRDTEL,RPRDTELQ                                                 
         MVI   RPRDTLEN,RPRDTOVQ   LENGTH W/O WEEKS                             
         MVC   RPRDTINM,SVKYINVT   INVENTORY NUMBER                             
         MVC   RPRDTEFF,SVKYEFDT   EFFECTIVE DATES                              
         MVC   RPRDTSLN,SAVSLNS    FIRST LENGTH IS DEFAULT                      
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         MVC   RPRDTSTA,0(RE)      INTERNAL STATION CODE                        
         DROP  R6                                                               
*                                                                               
         MVC   MINELEML,=Y(RPRDTOVQ)                                            
         BAS   RE,INTOAIO5                                                      
DFDVALX  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
************ DOING AN ACTION ON A SPECIFIC DATA OBJECT ****************         
***********************************************************************         
DATA10   DS    0H                                                               
         LR    RF,RB               TABLE OF KNOWN OBJECTS                       
         AH    RF,=Y(KNOWTAB-PRO33)                                             
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
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    EXITSLCK                                                         
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
         BAS   RE,RDCRTICL         READ CRITICAL INFORMATION                    
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
***********************************************************************         
* DATA OBJECT FOR DEVELOPMENT SALESPERSON                                       
***********************************************************************         
DVSDTA   DS    0H                                                               
         MVC   FVIFLD(L'EDVSNAME),EDVSNAME                                      
         B     EXITOK                                                           
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
         MVC   FVIFLD(L'SAVSTA),STLNSTA                                         
         DROP  R1                                                               
*                                                                               
DISSTAX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE STATION FIELD                                                        
***********************************************************************         
VALSTA   DS    0H                                                               
         CLI   FVILEN,0            ANY STATION?                                 
         BNE   VALSTA00                                                         
*                                                                               
**       TM    MISCFLG1,MF1KYCHG   NO, WAS THE KEY CHANGED?                     
**       BZ    EXITNO                                                           
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
         STC   R1,0(RE)              SAVE INTERNAL ORDER # OF STATION           
*                                                                               
VALSTAX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INVENTORY ID                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
INVDTA   DS    0H                                                               
         LA    RF,INVTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
INVTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALINV)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE INVENTORY ID FIELD                                                   
***********************************************************************         
VALINV   DS    0H                                                               
         XC    SVKYINVT,SVKYINVT                                                
*                                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         CLI   FVILEN,0            ANY INVENTORY ID NUMBER?                     
         BE    VALINVX                                                          
         MVC   SVKYINVT,FVIFLD     YES                                          
*                                                                               
VALINVX  OI    FVIIND,FVIVAL       FIELD HAS BEEN VALIDATED                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EFFECTIVE DATES FIELD                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
EDTDTA   DS    0H                                                               
         LA    RF,EDTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
EDTTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALEDT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE EFFECTIVE DATES FIELD                                                
***********************************************************************         
VALEDT   DS    0H                                                               
         XC    SVKYEFDT,SVKYEFDT                                                
*                                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         CLI   FVILEN,0            ANY EFFECTIVE DATE(S)?                       
         BNE   VALEDT4             YES, THERE ARE                               
         CLC   SVKYINVT,BCSPACES   NO, ANY INVENTORY ID NUMBER?                 
         BNH   VALEDTX                 NO                                       
         LR    RE,RA                                                            
         AH    RE,=Y(SVHDEFDT-TWAD)                                             
         OC    0(L'SVHDEFDT,RE),0(RE)                                           
         BZ    VALEDT2                                                          
*                                                                               
         MVC   SVKYEFDT,0(RE)                                                   
         XC    0(L'SVHDEFDT,RE),0(RE)                                           
         OI    FVIIND,FVIVAL                                                    
         B     DISEDT                                                           
*                                                                               
VALEDT2  XC    0(L'SVHDEFDT,RE),0(RE)                                           
         GOTO1 VDATCON,BODMCB,(3,CCONDAT),(2,SVFLEFSD)                          
         GOTO1 VDATCON,BODMCB,(3,CCONDAT+3),(2,SVFLEFED)                        
         B     VALEDTX                 YES, MISSING EFFECTIVE DATE(S)           
*                                                                               
VALEDT4  DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVHDEFDT-TWAD)                                             
         XC    0(L'SVHDEFDT,RE),0(RE)                                           
*                                                                               
         CLC   SVKYINVT,BCSPACES   ANY INVENTORY ID NUMBER?                     
         BNH   EXITNV              NONE, THEN CAN'T HAVE EFFECTIVE DATE         
*                                                                               
         GOTO1 VPERVAL,BODMCB,(FVILEN,FVIFLD),PERVALST                          
         TM    4(R1),X'03'                                                      
         BNZ   EXITNV                                                           
*                                                                               
VALEDTD  USING PERVALD,PERVALST                                                 
         TM    VALEDTD.PVALASSM,PVALASY+PVALASM+PVALASD                         
         BO    EXITNV                                                           
*                                                                               
         GOTO1 VDATCON,BODMCB,(0,VALEDTD.PVALESTA),(19,BOFULL1)                 
         GOTO1 VDATCON,BODMCB,(0,VALEDTD.PVALEEND),(19,BOFULL2)                 
*                                                                               
         MVC   SVKYEFDT(3),BOFULL1                                              
*                                                                               
         TM    VALEDTD.PVALASSM,PVALAEY+PVALAEM+PVALAED                         
         BO    VALEDTX             END DATE WAS COMPLETELY ASSUMED              
         BNZ   EXITNV              SHOULD ENTER A COMPLETE MMMDD/YY?            
         MVC   SVKYEFDT+3(3),BOFULL2                                            
         DROP  VALEDTD                                                          
*                                                                               
VALEDTX  OI    FVIIND,FVIVAL       FIELD HAS BEEN VALIDATED                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY EFFECTIVE DATES FIELD                                                 
***********************************************************************         
DISEDT   DS    0H                                                               
         OC    SVKYEFDT(3),SVKYEFDT            EFFECTIVE DATE START?            
         BZ    DISEDTX                         NONE                             
*                                                                               
         GOTO1 VDATCON,BODMCB,(8,SVKYEFDT),(17,FVIFLD)                          
*                                                                               
         OC    SVKYEFDT+3(3),SVKYEFDT+3                                         
         BZ    DISEDTX                         NONE                             
         MVI   FVIFLD+8,C'-'                                                    
         GOTO1 VDATCON,BODMCB,(8,SVKYEFDT+3),(17,FVIFLD+9)                      
*                                                                               
DISEDTX  B     EXITOK                                                           
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
         MVI   SVKYDYPT,0                                                       
         CLI   FVILEN,0            THIS FIELD IS REQUIRED                       
         BE    EXITNO                                                           
*                                                                               
VALDPT00 TM    CCONFLG1,CCONDPMQ   USES HARDCODED TABLE?                        
         BO    VALDPT60            YES                                          
*                                                                               
         MVC   BODMCB(2),CUAALF    GET PARENT REP                               
         GOTOX (GETPRNT,AREPRO01),BODMCB                                        
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RRDPKEY,RE                                                       
         MVI   RRDPKTYP,RRDPKIDQ                                                
         MVC   RRDPKREP,BODMCB                                                  
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
         MVC   0(L'DPTINKEY,RE),FVIFLD                                          
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
         GOTO1 =A(VALDYTIM),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
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
*        DC    AL1(DVAL),AL1(0,0,0),AL4(VALSEQ)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY SEQUENCE FIELD                                                        
***********************************************************************         
DISSEQ   DS    0H                                                               
         MVC   FVIFLD(L'SVKYSEQN),SVKYSEQN                                      
DISSEQX  B     EXITOK                                                           
         EJECT                                                                  
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRG)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY PROGRAM FIELD                                                         
***********************************************************************         
DISPRG   DS    0H                                                               
         ZIC   R1,ONPRGLIN                                                      
         LA    R1,1(R1)                                                         
         STC   R1,ONPRGLIN                                                      
*                                                                               
         L     R5,AIO7             SO WE DON'T RUIN OUR CLUSTER                 
         USING MINBLKD,R5                                                       
         LA    RE,BOELEM              WE'LL USE BOELEM                          
         ST    RE,MINELEM                                                       
         MVC   MINMAXEL,=Y(L'BOELEM)                                            
*                                                                               
         BCTR  R1,0                                                             
         MH    R1,=Y(L'PRGLST)                                                  
         LA    R1,PRGLIST(R1)                                                   
         OC    0(L'PRGLST,R1),0(R1)                                             
         BZ    DISPRGX                                                          
*                                                                               
         XC    MINEKEY,MINEKEY     FIND PROGRAM TEXT ELEMENT                    
         MVI   MINEKEY,RPRTXELQ                                                 
         MVI   MINEKEY+1,RPRTXPRQ                                               
         MVC   MINEKEY+6(L'PRGLST),0(R1)                                        
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
DISPRGX  L     R1,AIO7             NEED TO RESTORE THIS                         
         LA    R1,MINBLKL(R1)                                                   
         ST    R1,MINELEM                                                       
         MVC   MINMAXEL,=Y(IOAREALN-MINBLKL)                                    
         B     EXITOK                                                           
         DROP  R5,R6                                                            
***********************************************************************         
* VALIDATE PROGRAM FIELD                                                        
***********************************************************************         
VALPRG   DS    0H                                                               
         ZIC   R1,ONPRGLIN         BUMP INDEX TO PROGRAM TEXT                   
         LA    R1,1(R1)                                                         
         STC   R1,ONPRGLIN                                                      
*                                                                               
         OC    FVIFLD,BCSPACES                                                  
*                                                                               
         L     R5,AIO7             SO WE DON'T RUIN OUR CLUSTER                 
         USING MINBLKD,R5                                                       
         LA    RE,BOELEM              WE'LL USE BOELEM                          
         ST    RE,MINELEM                                                       
         MVC   MINMAXEL,=Y(L'BOELEM)                                            
*                                                                               
         CLI   FVILEN,0            ANY PROGRAM?                                 
         BNE   VALPRG00                                                         
         XC    BOHALF1,BOHALF1     NONE                                         
         CLI   ONPRGLIN,1          ARE WE ON THE 1ST PROGRAM?                   
         BNE   VALPRG40            NO, CLEAR OUT THE PROGRAM THEN               
         B     EXITNO              YES, NEED AT LEAST THE 1ST PROGRAM           
*                                                                               
VALPRG00 XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRTXELQ    TEXT ELEMENT                                 
         MVI   MINEKEY+1,RPRTXPRQ  PROGRAM TEXT                                 
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRTXELD,R6                                                      
         ZIC   R1,FVXLEN           L(PROGRAM) - 1                               
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   VALPRG30                                                         
*                                                                               
VALPRG10 CLI   RPRTXEL,RPRTXELQ    DID WE GET A TEXT ELEMENT?                   
         BNE   VALPRG30                                                         
         CLI   RPRTXTYP,RPRTXPRQ      THAT IS PROGRAM TEXT?                     
         BNE   VALPRG30                                                         
*                                                                               
         MVC   BOHALF1,RPRTXSEQ    SAVE THE SEQUENCE NUMBER AROUND              
         ZIC   RE,RPRTXLEN                                                      
         SH    RE,=Y(RPRTXOVQ+1)                                                
         BM    VALPRG20                                                         
         CR    R1,RE               SAME LENGTH?                                 
         BNE   VALPRG20            NO, CHECK NEXT ONE                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),RPRTXTXT  THIS PROGRAM TEXT EXISTS ALREADY?            
         BE    VALPRG40            YES, STORE IT APPROPRIATELY                  
*                                                                               
VALPRG20 BAS   RE,MINIOSEQ                                                      
         BE    VALPRG10                                                         
***************                                                                 
* PROGRAM TEXT DOES NOT EXIST, NEED TO ADD IT                                   
***************                                                                 
VALPRG30 XC    RPRTXEL(L'BOELEM),RPRTXEL                                        
         MVI   RPRTXEL,RPRTXELQ                                                 
         MVI   RPRTXTYP,RPRTXPRQ                                                
         LH    RE,BOHALF1                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,RPRTXSEQ                                                    
         STCM  RE,3,BOHALF1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RPRTXTXT(0),FVIFLD                                               
         AH    R1,=Y(RPRTXOVQ+1)                                                
         STC   R1,RPRTXLEN                                                      
*                                                                               
         BAS   RE,MINIOADD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
***************                                                                 
* UPDATE THE LIST OF PROGRAMS ATTACHED TO THIS DETAIL                           
***************                                                                 
VALPRG40 ZIC   R1,ONPRGLIN                                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(L'PRGLST)                                                  
         LA    R1,PRGLIST(R1)                                                   
         MVC   0(L'BOHALF1,R1),BOHALF1                                          
*                                                                               
VALPRGX  L     R1,AIO7             NEED TO RESTORE THIS                         
         LA    R1,MINBLKL(R1)                                                   
         ST    R1,MINELEM                                                       
         MVC   MINMAXEL,=Y(IOAREALN-MINBLKL)                                    
         B     EXITOK                                                           
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SECONDARY DAY/TIME FIELD                                      
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SDTDTA   DS    0H                                                               
         LA    RF,SDTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SDTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSDT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSDT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY SECONDARY NDAY/TIME FIELD                                             
***********************************************************************         
DISSDT   DS    0H                                                               
         ZIC   R6,ONSDTLIN                                                      
         LA    R6,1(R6)                                                         
         STC   R6,ONSDTLIN                                                      
*                                                                               
         BCTR  R6,0                                                             
         MH    R6,=Y(L'SDTLST)                                                  
         LA    R6,SDTLIST(R6)                                                   
         OC    0(L'SDTLST,R6),0(R6)                                             
         BZ    DISSDTX                                                          
*                                                                               
         GOTO1 VDAYUNPK,BODMCB,0(R6),FVIFLD                                     
*                                                                               
         LA    RE,FVIFLD                                                        
DISSDT10 CLI   0(RE),C' '          CONVERT '/'S TO ','S                         
         BNH   DISSDT20                                                         
         CLI   0(RE),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         B     DISSDT10                                                         
*                                                                               
DISSDT20 LA    R4,FVIFLD+9         FIND WHERE TO PUT THE TIMES                  
DISSDT30 CLI   0(R4),C' '                                                       
         BH    DISSDT40                                                         
         BCTR  R4,0                                                             
         B     DISSDT30                                                         
DISSDT40 LA    R4,1(R4)                                                         
*                                                                               
DISSDT50 MVI   0(R4),C'/'          SEPARATE DAYS AND TIMES WITH A C'/'          
         LA    R4,1(R4)                                                         
*                                                                               
         GOTO1 VUNTIME,BODMCB,1(R6),0(R4)                                       
*                                                                               
DISSDTX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE SECONDARY DAY/TIME FIELD                                             
***********************************************************************         
VALSDT   DS    0H                                                               
         ZIC   R6,ONSDTLIN                                                      
         LA    R6,1(R6)                                                         
         STC   R6,ONSDTLIN                                                      
*                                                                               
         BCTR  R6,0                CLEAR OUT THE ENTRY BY DEFAULT               
         MH    R6,=Y(L'SDTLST)                                                  
         LA    R6,SDTLIST(R6)                                                   
         XC    0(L'SDTLST,R6),0(R6)                                             
*                                                                               
         CLI   FVILEN,0            ANY DAY/TIME HERE?                           
         BE    VALSDTX             NO, CLEARED OUT ALREADY                      
*                                                                               
VALSDT00 L     RE,AIO4                                                          
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
         GOTO1 VDAYVAL,BODMCB,(0(R3),12(R3)),0(R6),BOBYTE1                      
         CLI   0(R4),0             DAYVAL COMPLAINS IF INPUT > 11               
         BE    EXITNV                                                           
*                                                                               
         LA    R3,32(R3)                                                        
*                                                                               
         CLI   1(R3),0                                                          
         BNE   EXITNV                                                           
         CLI   0(R3),0                                                          
         BNE   *+14                                                             
         MVC   FVERRNDX,4(R3)     WE NEED THE TIME PORTION ALSO                 
         B     EXITNO                                                           
*                                                                               
         GOTO1 VTIMVAL,BODMCB,(0(R3),12(R3)),1(R6)                              
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BNE   *+14                                                             
         MVC   FVERRNDX,4(R3)     POINT TO THE TIME PORTION                     
         B     EXITNV                 THAT'S IN ERROR                           
*                                                                               
VALSDTX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR AVAIL DAY/TIME FIELD                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
ADTDTA   DS    0H                                                               
         LA    RF,ADTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
ADTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISADT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY AVAIL DAY/TIME FIELD                                                  
***********************************************************************         
DISADT   DS    0H                                                               
         ZIC   R6,ONADTLIN                                                      
         LA    R6,1(R6)                                                         
         STC   R6,ONADTLIN                                                      
*                                                                               
         BCTR  R6,0                                                             
         MH    R6,=Y(L'ADTLST)                                                  
         LA    R6,ADTLIST(R6)                                                   
         OC    0(L'ADTLST,R6),0(R6)                                             
         BZ    DISADTX                                                          
*                                                                               
         CLC   0(L'RPRAVDAY,R6),BCSPACES                                        
         BH    *+16                                                             
         MVI   FVIFLD,C'/'                                                      
         LA    R4,FVIFLD+1                                                      
         B     DISADT2                                                          
*                                                                               
         MVC   FVIFLD(L'RPRAVDAY),0(R6)                                         
         LA    R4,FVIFLD+L'RPRAVDAY                                             
         CLI   0(R4),C' '                                                       
         BH    *+10                                                             
         BCTR  R4,0                                                             
         B     *-10                                                             
         CLC   L'RPRAVDAY(L'RPRAVTIM,R6),BCSPACES                               
         BNH   DISADTX                                                          
         MVI   1(R4),C'/'                                                       
         LA    R4,2(R4)                                                         
DISADT2  MVC   0(L'RPRAVTIM,R4),L'RPRAVDAY(R6)                                  
*                                                                               
DISADTX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE AVAIL DAY/TIME FIELD                                                 
***********************************************************************         
VALADT   DS    0H                                                               
         ZIC   R6,ONADTLIN                                                      
         LA    R6,1(R6)                                                         
         STC   R6,ONADTLIN                                                      
*                                                                               
         BCTR  R6,0                CLEAR OUT THE ENTRY BY DEFAULT               
         MH    R6,=Y(L'ADTLST)                                                  
         LA    R6,ADTLIST(R6)                                                   
         XC    0(L'ADTLST,R6),0(R6)                                             
*                                                                               
         CLI   FVILEN,0            ANY DAY/TIME HERE?                           
         BE    VALSDTX             NO, CLEARED OUT ALREADY                      
*                                                                               
VALADT00 L     RE,AIO4                                                          
         LA    RF,480                                                           
         XCEFL                                                                  
*                                                                               
         GOTO1 VSCANNER,BODMCB,FVIHDR,(X'83',AIO4),C',=/='                      
*                                                                               
         CLI   4(R1),1                                                          
         BL    EXITNO              MUST HAVE 1 COMPONENT                        
         CLI   4(R1),2                                                          
         BH    EXITNV              BUT NOT MORE THAN 2                          
*                                                                               
         L     R3,AIO4                                                          
         CLI   1(R3),0                                                          
         BNE   EXITNV                                                           
         CLI   0(R3),0                                                          
         BE    *+10                                                             
         MVC   0(L'RPRAVDAY,R6),12(R3)                                          
         LA    R3,32(R3)                                                        
         CLI   1(R3),0                                                          
         BNE   EXITNV                                                           
         CLI   0(R3),0                                                          
         BE    *+10                                                             
         MVC   L'RPRAVDAY(L'RPRAVTIM,R6),12(R3)                                 
*                                                                               
VALADTX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LENGTH FIELD                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LENDTA   DS    0H                                                               
         LA    RF,LENTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LENTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLEN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLEN)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY LENGTH FIELD                                                          
***********************************************************************         
DISLEN   DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         EDIT  (B1,RPRDTSLN),(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,        X        
               DUB=BODUB1                                                       
*                                                                               
DISLENX  B     EXITOK                                                           
         DROP  R5,R6                                                            
***********************************************************************         
* VALIDATE LENGTH FIELD                                                         
***********************************************************************         
VALLEN   DS    0H                                                               
         CLI   FVILEN,0            THIS FIELD IS REQUIRED                       
         BE    EXITNO                                                           
*                                                                               
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
*                                                                               
         L     R0,BCFULL                                                        
         CH    R0,=H'255'          SPOT LENGTH SHOULD FIT IN 1 BYTE             
         BH    EXITILEN                                                         
         CLI   BCFULL+3,0          AND CAN'T BE 0 SECONDS                       
         BE    EXITILEN                                                         
*                                                                               
         LA    RE,SAVSLNS                                                       
VALLEN10 CLC   0(L'SAVSLN,RE),BCFULL+3   SEE IF ONE OF OUR CHOSEN SPOT          
         BE    VALLEN20                    LENGTHS FOR THIS PROPOSAL            
         LA    RE,L'SAVSLN(RE)                                                  
         LA    RF,SAVSLNS+L'SAVSLNS                                             
         CR    RE,RF                                                            
         BL    VALLEN10                                                         
         B     EXITILEN                                                         
*                                                                               
VALLEN20 L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
         USING RPRDTELD,R6                                                      
         MVC   RPRDTSLN,BCFULL+3                                                
*                                                                               
VALLENX  B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
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
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
*                                                                               
         TM    RPRDTNC1,X'80'      N/A?                                         
         BZ    *+14                NO                                           
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
         CLI   FVILEN,0            THIS FIELD IS REQUIRED                       
         BE    EXITNO                                                           
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
         USING RPRDTELD,R6                                                      
*                                                                               
         CLC   =C'NA',FVIFLD                                                    
         BNE   *+14                                                             
         MVC   RPRDTNC1,=X'80000000'                                            
         B     VALCSTX                                                          
*                                                                               
         ZIC   R0,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,FVIFLD,(R0)                                      
         CLI   0(R1),X'FF'                                                      
         BE    EXITNOTN            NOT NUMERIC                                  
*                                                                               
         MVC   RPRDTNC1,BODMCB+4                                                
*                                                                               
VALCSTX  B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMMENT                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
COMDTA   DS    0H                                                               
         LA    RF,COMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
COMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOM)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY COMMENT FIELD                                                         
***********************************************************************         
DISCOM   DS    0H                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
*                                                                               
DISCOM10 CLI   0(R6),0                                                          
         BE    DISCOMX                                                          
*                                                                               
         USING RPRUTELD,R6                                                      
         CLI   RPRUTEL,RPRUTELQ    DO WE HAVE A COMMENT ELEMENT?                
         BNE   DISCOM20            NO                                           
         CLI   SVPARMS2+3,12       1ST COMMENT LINE (FIELD #12)?                
         BNE   DISCOM15                                                         
         CLI   RPRUTLIN,1          YES, DO WE HAVE ONE FOR LINE 1?              
         BE    DISCOM30                 YES                                     
         B     DISCOM20                                                         
*                                                                               
DISCOM15 CLI   RPRUTLIN,2          DO WE HAVE IT FOR LINE 2?                    
         BE    DISCOM30            YES                                          
*                                                                               
DISCOM20 ZIC   R0,1(R6)            SKIP TO THE NEXT ELEMENT                     
         AR    R6,R0                                                            
         B     DISCOM10                                                         
*                                                                               
DISCOM30 ZIC   R1,RPRUTLEN                                                      
         SH    R1,=Y(RPRUTOVQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),RPRUTTXT  SHOW THE TEXT                                
*                                                                               
DISCOMX  B     EXITOK                                                           
         DROP  R6                                                               
***********************************************************************         
* VALIDATE COMMENT FIELD                                                        
***********************************************************************         
VALCOM   DS    0H                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
*                                                                               
         SR    R0,R0                                                            
         NI    MISCFLG1,X'FF'-MF1TMPBT                                          
VALCOM10 CLI   0(R6),0                                                          
         BNE   *+12                                                             
VALCOM12 OI    MISCFLG1,MF1TMPBT                                                
         B     VALCOM20                                                         
*                                                                               
         CLI   0(R6),RPRUTELQ      COMMENT WILL BE UNDER USER TEXT              
         BE    VALCOM16                                                         
         BH    VALCOM12                                                         
VALCOM14 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VALCOM10                                                         
*                                                                               
         USING RPRUTELD,R6                                                      
VALCOM16 CLI   SVPARMS2+3,12                                                    
         BNE   VALCOM18                                                         
         CLI   RPRUTLIN,1                                                       
         BNE   VALCOM14                                                         
         B     VALCOM20                                                         
VALCOM18 CLI   RPRUTLIN,2                                                       
         BNE   VALCOM14                                                         
*                                                                               
VALCOM20 CLI   FVILEN,0            ANY COMMENT HERE?                            
         BNE   VALCOM30            YES                                          
         TM    MISCFLG1,MF1TMPBT   NO, DO WE HAVE ONE BEFORE?                   
         BNZ   VALCOMX                 NO, NOTHING TO DO HERE THEN              
         B     VALCOM50                YES, DELETE IT THEN                      
*                                                                               
VALCOMD  USING RPRUTELD,BOELEM                                                  
VALCOM30 XC    BOELEM,BOELEM             SET UP HOW CHANGED OR NEW ONE          
         MVI   VALCOMD.RPRUTEL,RPRUTELQ     WILL LOOK                           
*                                                                               
         TM    MISCFLG1,MF1TMPBT                                                
         BNZ   VALCOM40                                                         
         ZIC   RE,RPRUTLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),RPRUTEL                                                
*                                                                               
VALCOM40 MVI   VALCOMD.RPRUTLIN,1    SPECIFY LINE 1 OR 2                        
         CLI   SVPARMS2+3,12                                                    
         BE    *+8                                                              
         MVI   VALCOMD.RPRUTLIN,2                                               
*                                                                               
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   VALCOMD.RPRUTTXT(0),FVIFLD                                       
         AH    RE,=Y(RPRUTOVQ+1)                                                
         STC   RE,VALCOMD.RPRUTLEN                                              
*                                                                               
VALCOM50 TM    MISCFLG1,MF1TMPBT                                                
         BNZ   VALCOM60                                                         
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
VALCOM60 CLI   FVILEN,0                                                         
         BE    VALCOMX                                                          
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
*                                                                               
VALCOMX  B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PRIME BOOK                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PBKDTA   DS    0H                                                               
         OC    SAVLBL,SAVLBL       DO WE HAVE A USER DEFINED BOOK?              
         BZ    *+14                                                             
         MVC   FVIFLD(L'SAVLBL),SAVLBL                                          
         B     PBKDTAX                                                          
*                                                                               
         LA    R1,SAVBKS                                                        
         USING BOOKLIN,R1                                                       
         XC    BOWORK2,BOWORK2                                                  
         MVC   BOWORK2(L'FVIHDR),FVIHDR                                         
         ZIC   RE,BOWORK2                                                       
         LA    RE,5(RE)                                                         
         STC   RE,BOWORK2                                                       
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(3),BKLNBK    JUST SHOW THE 1ST (PRIMARY BOOK)            
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(2),=X'0B07'      PUT OUT SOURCE                           
         MVC   BOELEM+2(1),BKLNSPBK                                             
         DROP  R1                                                               
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
         BNE   PBKDTA05                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     PBKDTA05                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
PBKDTA05 DS    0H                                                               
         LA    RF,BOWORK2+8                                                     
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),BOWORK2+8                                              
*                                                                               
PBKDTAX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PROJECTION                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRJDTA   DS    0H                                                               
         OC    SAVLBL,SAVLBL       DO WE HAVE A USER DEFINED BOOK?              
         BZ    PRJDTAX                                                          
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ                                                 
         MVC   MINEKEY+1(L'RPRBKIOR),SAVBK+(BKLNIORD-BOOKLIN)                   
         BAS   RE,MINIOHI                                                       
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRBKELD,R6                                                      
         ZIC   R1,RPRBKLEN                                                      
         SH    R1,=Y(RPRBKUOQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),RPRBKUPG                                               
*                                                                               
PRJDTAX  B     EXITOK                                                           
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BOOKS'S HEADING                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
HXBKDTA  DS    0H                                                               
         LA    RF,HXBKTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
HXBKTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISHXBK)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY HEADING FOR THE RATING FIELD                                          
***********************************************************************         
DISHXBK  DS    0H                                                               
         GOTO1 =A(DISHDXBK),RR=BORELO                                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PRINT BOOK FIELD                                              
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
XBKDTA   DS    0H                                                               
         LA    RF,XBKTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
XBKTBL   DS    0AL1                                                             
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISXBK)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALXBK)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY PRINT BOOK FIELD                                                      
***********************************************************************         
DISXBK   DS    0H                                                               
         GOTO1 =A(DISXBOOK),RR=BORELO                                           
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE PRINT BOOK FIELD                                                     
***********************************************************************         
VALXBK   DS    0H                                                               
         B     EXITNV                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RATING'S HEADING                                              
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
HRTGDTA  DS    0H                                                               
         LA    RF,HRTGTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
HRTGTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISHRTG)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY HEADING FOR THE RATING FIELD                                          
***********************************************************************         
DISHRTG  DS    0H                                                               
         ZIC   R1,ONDMORTG         ON WHICH DEMO RATING                         
         LA    R1,1(R1)                                                         
         STC   R1,ONDMORTG                                                      
*                                                                               
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVDMO)                                                  
         LA    R1,SAVDMOS(R1)                                                   
         USING DEMOLIN,R1                                                       
*                                                                               
         CLI   0(R1),0                                                          
         BE    DISHRTGX                                                         
*                                                                               
         L     R6,AIO6                                                          
         USING DBLOCK,R6                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOM                                                    
         MVI   DBSELMED,C'T'                                                    
         LA    R5,BOWORK1                                                       
         XC    BOWORK1(50),BOWORK1                                              
         MVC   BOWORK1(L'DMLNDEMO),DMLNDEMO                                     
         DROP  R1                                                               
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         DROP  R6                                                               
*                                                                               
         GOTO1 VDEMOCON,BODMCB,(1,BOWORK1),(9,FVIFLD),(0,AIO6)                  
*                                                                               
DISHRTGX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RATING FIELD                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
RTGDTA   DS    0H                                                               
         LA    RF,RTGTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RTGTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRTG)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRTG)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY RATING FIELD                                                          
***********************************************************************         
DISRTG   DS    0H                                                               
B        USING BOOKLIN,SAVBK                                                    
         OC    B.BKLNUPGD,B.BKLNUPGD   UPGRADE?                                 
         BNZ   *+16                                                             
         TM    SELPROF+SELDMOVB,SELDMOVA                                        
         BZ    *+8                                                              
         OI    FVATRB,FVAPROT                                                   
         DROP  B                                                                
*                                                                               
         CLI   SVDMOELM,0                                                       
         BE    DISRTGX                                                          
*                                                                               
DISRTGD  USING RPRDVELD,SVDMOELM                                                
         ZIC   R1,ONDMORTG         FIND INTERNAL ORDER NUMBER OF DEMO           
         BCTR  R1,0                                                             
         LR    R3,R1                                                            
         MH    R3,=Y(L'SAVDMO)                                                  
         LA    R3,SAVDMOS(R3)                                                   
         USING DEMOLIN,R3                                                       
         ZIC   R1,DMLNIORD                                                      
         DROP  R3                                                               
*                                                                               
         LTR   R1,R1               ANY DEMO HERE?                               
         BE    DISRTGX             NO, NOTHING TO SHOW THEN                     
*                                                                               
         MVI   FVIFLD,C'0'         IF NONE THEN SHOW 0                          
*                                                                               
         BCTR  R1,0                FIND A(DEMO VALUE) WE WANT                   
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,DISRTGD.RPRDVDMO(R1)                                          
*                                                                               
         LR    RE,R1               ARE WE BEYOND THE ELEMENT?                   
         LA    R0,SVDMOELM                                                      
         SR    RE,R0                                                            
         CLM   RE,1,DISRTGD.RPRDVLEN                                            
         BNL   DISRTGX             YES                                          
*                                                                               
         MVC   BOFULL1,0(R1)                                                    
         LA    R4,FVIFLD                                                        
         TM    BOFULL1,X'80'       IS THIS A DEMO OVERRIDE?                     
         BZ    DISRTG10                                                         
         MVI   0(R4),C'*'          YES, THEN SHOW AN '*' BEFORE RATING          
         LA    R4,1(R4)                                                         
         NI    BOFULL1,X'FF'-X'80'   GET RID OF DEMO OVERRIDE BIT               
*                                                                               
DISRTG10 TM    SAVOPTNS,OPTNDECQ   DISPLAY DEMO PRECISION?                      
         BZ    DISRTG20            NO                                           
         EDIT  (B4,BOFULL1),(9,0(R4)),1,ALIGN=LEFT,WRK=BOWORK1,        X        
               DUB=BODUB1,ZERO=NOBLANK                                          
         B     DISRTG30                                                         
*                                                                               
DISRTG20 L     R3,BOFULL1          DON'T DISPLAY DEMO PRECISION                 
         CVD   R3,BODUB1                                                        
         ZAP   PCKOF08B,BODUB1                                                  
         SRP   PCKOF08B,64-1,5     ROUND THE # OFF INSTEAD                      
         EDIT  (P8,PCKOF08B),(9,0(R4)),ALIGN=LEFT,WRK=BOWORK1,         X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
DISRTG30 DS    0H                                                               
*                                                                               
DISRTGX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE RATING FIELD                                                         
***********************************************************************         
VALRTG   DS    0H                                                               
         GOTO1 =A(VALRATNG),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
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
         DC    AL1(SXITOUT),AL1(0,0,0),AL4(NTRXOUT)                             
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
         OI    MISCFLG1,MF1PFCOM                                                
NTRINX   B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS COMIMG BACK FROM CALLED SESSION                                       
***********************************************************************         
         PUSH  USING                                                            
NTRXIN   DS    0H                                                               
         OI    MISCFLG1,MF1PFRET                                                
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS GOING BACK TO CALLER SESSION                                          
***********************************************************************         
         PUSH  USING                                                            
NTRXOUT  DS    0H                                                               
         LA    RF,SDATA+6                                                       
         MVI   0(RF),RPRDTELQ                                                   
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         MVC   RPRDTSTA-RPRDTELD-1(L'RPRDTSTA,RF),0(RE)                         
         MVC   RPRDTDPT-RPRDTELD-1(L'RPRDTDPT,RF),DPTINKEY-SVSTATN(RE)          
         MVC   RPRDTDAY-RPRDTELD-1(L'RPRDTDAY,RF),DAYINKEY-SVSTATN(RE)          
         MVC   RPRDTTIM-RPRDTELD-1(L'RPRDTTIM,RF),PTMINMKY-SVSTATN(RE)          
         MVC   RPRDTSEQ-RPRDTELD-1(L'RPRDTSEQ,RF),SEQINKEY-SVSTATN(RE)          
         MVC   RPRDTFL1-RPRDTELD-1(L'TIMINKEY,RF),TIMINKEY-SVSTATN(RE)          
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
         MVI   GSSKCODE,KYDEF      WORK/ADD KEY SCREEN                          
*                                                                               
SETKSCRX B     EXITOK                                                           
***********************************************************************         
* SET THE DATA SCREEN CODE                                                      
***********************************************************************         
SETMSCR  DS    0H                                                               
         MVI   GSSMCODE,DTDEF      WORK/ADD DATA SCREEN                         
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
         CLI   0(RE),PFNEXT        NEXT?                                        
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
* READS THE CRITICAL INFORMATION FROM THE PROPOSAL RECORD                       
***********************************************************************         
RDCRTICL NTR1                                                                   
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
         USING BOOKLIN,R1                                                       
*                                                                               
         CLI   RPRBKLEN,RPRBKOVQ   USER DEFINED BOOK?                           
         BH    RDBDBK20            YES                                          
*********                                                                       
* REGULAR BOOK                                                                  
*********                                                                       
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
* BOOK EXTENSION ELEMENT(S)                                                     
***************                                                                 
         LR    RE,RA                                                            
         AH    RE,=Y(MINXBKS-TWAD)                                              
         XC    0(L'MINXBKS,RE),0(RE)                                            
         XC    SAVXBKS,SAVXBKS                                                  
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBXELQ    GET THE BOOK EXTENSION ELEMENT               
         BAS   RE,MINIOHI                                                       
         BNE   RDBDBXX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRBXELD,R6                                                      
*                                                                               
RDBDBX10 CLI   0(R6),RPRBXELQ                                                   
         BNE   RDBDBX20                                                         
*                                                                               
         ZIC   R1,RPRBXIOR         INTERNAL ORDER NUMBER                        
         BCTR  R1,0                                                             
         MH    R1,=Y(L'MINXBK)                                                  
         AR    R1,RA                                                            
         AH    R1,=Y(MINXBKS-TWAD)                                              
         USING XBOKLIN,R1                                                       
         MVC   XBLNDPT,RPRBXDPT                                                 
         MVC   XBLNFLG,RPRBXFLG                                                 
         DROP  R1                                                               
*                                                                               
RDBDBX16 BAS   RE,MINIOSEQ                                                      
         BE    RDBDBX10                                                         
*                                                                               
RDBDBX20 DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINXBKS-TWAD)                                              
         MVC   SAVXBKS,0(RE)                                                    
*                                                                               
RDBDBXX  DS    0H                                                               
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
RDBDDP00 LR    RE,RA                                                            
         AH    RE,=Y(MINDPTS-TWAD)                                              
         XC    0(L'MINDPTS,RE),0(RE)                                            
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDPELQ    GET THE DAYPART ELEMENT                      
         BAS   RE,MINIOHI                                                       
         BNE   RDBDDPX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDPELD,R6                                                      
         LR    R1,RA                                                            
         AH    R1,=Y(MINDPTS-TWAD)                                              
*                                                                               
RDBDDP10 CLI   0(R6),RPRDPELQ      DISPLAY ORDER NUMBER                         
         BNE   RDBDDPX                                                          
         MVC   0(L'RPRDPDPT,R1),RPRDPDPT                                        
*&&DO                                                                           
         LA    RE,CSARDPT                                                       
RDBDDP15 CLC   0(L'RPRDPDPT,RE),RPRDPDPT                                        
         BE    RDBDDP20                                                         
         LA    RE,3(RE)                                                         
         LA    RF,CSARDPT+L'CSARDPT                                             
         CR    RE,RF                                                            
         BL    RDBDDP15                                                         
         B     RDBDDP30                                                         
*                                                                               
RDBDDP20 ZICM  RF,1(RE),2                                                       
         STCM  RF,15,1(R1)                                                      
*&&                                                                             
RDBDDP20 MVC   1(L'RPRDPTAB,R1),RPRDPTAB                                        
*                                                                               
RDBDDP30 LA    R1,L'MINDPT(R1)                                                  
         BAS   RE,MINIOSEQ                                                      
         BE    RDBDDP10                                                         
*                                                                               
RDBDDPX  DS    0H                                                               
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
         MVC   STLNSTA,RPRSTSTA                                                 
         MVC   STLNFLG,RPRSTFLG                                                 
         MVC   STLNIORD,RPRSTICD                                                
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
**************************                                                      
** COST HEADER ELEMENTS **                                                      
**************************                                                      
RDBDCH0  LR    RF,RA                                                            
         AH    RF,=Y(MINCOSTS-TWAD)                                             
         XC    0(L'MINCOSTS,RF),0(RF)                                           
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRCHELQ    GET THE COST HEADER                          
         BAS   RE,MINIOHI                                                       
         BNE   RDBDCHX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRCHELD,R6                                                      
*                                                                               
RDBDCH10 CLI   0(R6),RPRCHELQ                                                   
         BNE   RDBDCHX                                                          
*                                                                               
         ZIC   RE,RPRCHSEQ                                                      
         CH    RE,=Y(NUMCSTS)                                                   
         BH    RDBDCH20                                                         
*                                                                               
         BCTR  RE,0                                                             
         MH    RE,=Y(L'SAVRATC)                                                 
         LA    RE,SAVRATCS(RE)                                                  
         USING RFTCRTES,RE                                                      
*                                                                               
         MVC   RFTCRTCD,RPRCHRTC                                                
         MVC   RFTCQQTR,RPRCHQTR                                                
         MVC   RFTCQYR,RPRCHYR                                                  
         NI    RFTCQYR,X'7F'                                                    
         MVC   RFTCSLN,RPRCHSLN                                                 
         DROP  RE                                                               
*                                                                               
         ZIC   RE,RPRCHSEQ                                                      
         BCTR  RE,0                                                             
         MH    RE,=Y(L'MINCOST)                                                 
         AR    RE,RF                                                            
         USING CSTLIN,RE                                                        
         ZIC   R1,RPRCHLEN                                                      
         BCTR  R1,0                                                             
         CH    R1,=Y(20-1)         MAX LABEL EX LEN                             
         BNH   *+8                                                              
         LA    R1,(20-1)                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSLNLBL,RPRCHLBL                                                 
         MVC   CSLNLBK,RPRCHBK                                                  
         MVC   CSLNIORD,RPRCHSEQ                                                
         MVC   CSLNPBC,RPRCHPBC                                                 
         DROP  R6,RE                                                            
*                                                                               
RDBDCH20 BAS   RE,MINIOSEQ                                                      
         BE    RDBDCH10                                                         
*                                                                               
RDBDCHX  DS    0H                                                               
*                                                                               
RDBKDMX  B     EXITOK                                                           
         DROP  R5                                                               
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
         L     RE,AIO5                                                          
         ZICM  RF,RPRORLEN-RPROKEY(RE),2                                        
         SH    RF,=Y(RPROR1ST-RPROKEY)     L'CLUSTER                            
         STCM  RF,3,MINELEML                                                    
         B     EXITOK                                                           
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
*                                                                               
* ON EXIT:     SVMINEKY            MINIO ELEMENT KET SET BY CALLER              
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
EXITIDPT MVC   FVMSGNO,=AL2(INVDYPRT)                                           
         B     EXITL               EXIT WITH INVALID DAYPART                    
EXITILEN MVC   FVMSGNO,=AL2(INVSPLEN)                                           
         B     EXITL               EXIT WITH INVALID LENGTH                     
EXITPF12 MVC   FVMSGNO,=AL2(SELKYCHG)                                           
         B     EXITL               EXIT WITH SEL KEY WAS CHANGED - ...          
EXITIBKE MVC   FVMSGNO,=Y(INVBKEXP)                                             
         B     EXITL               INVALID BOOK EXPRESSION                      
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
* SHOW BOOK HEADINGS                                                            
***********************************************************************         
DISHDXBK NTR1  BASE=*,LABEL=*                                                   
         ZIC   R1,ONBOOKLN         ON WHICH BOOK                                
         LA    R1,1(R1)                                                         
         STC   R1,ONBOOKLN                                                      
*                                                                               
         BCTR  R1,0                                                             
         LR    R2,R1                                                            
         MH    R1,=Y(L'SAVBK)                                                   
         LA    R1,SAVBKS(R1)                                                    
         MH    R2,=Y(L'SAVLBL)                                                  
         LA    R2,SAVLBLS(R2)                                                   
*                                                                               
         OC    0(BKLNLENQ,R1),0(R1)                                             
         BE    DISHXBKX                                                         
*                                                                               
         OC    0(L'SAVLBL,R2),0(R2)             USER DEFINED BOOK?              
         BZ    *+14                             NO                              
         MVC   FVIFLD(L'SAVLBL),0(R2)                                           
         B     DISHXBKX                                                         
*                                                                               
         USING BOOKLIN,R1                                                       
         XC    BOWORK2,BOWORK2                                                  
         MVC   BOWORK2(L'FVIHDR),FVIHDR                                         
         ZIC   RE,BOWORK2                                                       
         LA    RE,5(RE)                                                         
         STC   RE,BOWORK2                                                       
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(3),BKLNBK    JUST SHOW THE 1ST (PRIMARY BOOK)            
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(2),=X'0B07'      PUT OUT SOURCE                           
         MVC   BOELEM+2(1),BKLNSPBK                                             
         DROP  R1                                                               
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
         BNE   DSHXBK05                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     DSHXBK05                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
DSHXBK05 DS    0H                                                               
         LA    RF,BOWORK2+8                                                     
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),BOWORK2+8                                              
*                                                                               
DISHXBKX B     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SHOW BOOK HEADINGS                                                            
***********************************************************************         
DISXBOOK NTR1  BASE=*,LABEL=*                                                   
         ZIC   R1,ONPRBKLN         ON WHICH BOOK                                
         LA    R1,1(R1)                                                         
         STC   R1,ONPRBKLN                                                      
*                                                                               
         OI    FVATRB,FVAPROT                                                   
*                                                                               
         BCTR  R1,0                                                             
         LR    R2,R1                                                            
         MH    R1,=Y(L'SAVBK)                                                   
         LA    R1,SAVBKS(R1)                                                    
         USING BOOKLIN,R1                                                       
*                                                                               
         OC    0(BKLNLENQ,R1),0(R1)                                             
         BE    DISXBKX                                                          
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
         USING RPRDTELD,R6                                                      
*                                                                               
         MVI   FVIFLD,C'Y'                                                      
         CLI   RPRDTBKS,0          ANY BOOKS SUPRRESSED FOR REPORTING?          
         BE    DISXBKX                                                          
*                                                                               
         LA    RE,X'80'            CALCULATE MASK USED FOR THIS BOOK            
         ZIC   RF,BKLNIORD                                                      
         SRL   RE,0(RF)                                                         
         STC   RE,BOBYTE1                                                       
*                                                                               
         NC    BOBYTE1,RPRDTBKS    IS THIS MASKED BIT ON?                       
         BZ    DISXBKX             NO, BOOK IS NOT SUPPRESSED                   
*                                                                               
         MVI   FVIFLD,C'N'                                                      
*                                                                               
DISXBKX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         DROP  R6,R1                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FETCH HEADER DATA FOR GIVEN INVETORY # & EFFECTIVE DATES                      
***********************************************************************         
         DS    0H                                                               
HDRFETCH NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'HDRFETCH'                                                    
*                                                                               
         GOTOX (SAVVALQ,AREPRO01),BOPARM  SAVE TIA - INCASE OF OVERRUN          
*                                                                               
         LA    R0,FETCHBLK         CLEAR THE BLOCK                              
         LH    R1,=Y(RFTBLKL)                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
         MVC   RFTACOM,ACOM                    A(COMFACS)                       
         MVC   RFTAIO1,AIO1                    A(2K IO AREA)                    
         MVC   RFTAIO2,AIO6                    A(2K IO AREA)                    
         MVC   RFTAWRK,AIO2                    A(6K WORK AREA)                  
*                                              USES AIO2,AIO3, & AIO4           
         MVC   RFTCREP,CUAALF                           REP CODE                
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         ZIC   R1,0(RE)            LAST STATION USED BY USER                    
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R1,SAVSTAS(R1)                                                   
         USING STALIN,R1                                                        
         MVC   RFTCSTAT,STLNSTA               STATION CALL LETTERS              
         CLI   RFTCSTAT+4,C' '                                                  
         BH    *+8                                                              
         MVI   RFTCSTAT+4,C'T'                                                  
         MVI   RFTCSRC,C'N'                   DEMO SOURCE                       
         MVC   RFTCINV,SVKYINVT                                                 
*                                                                               
         OC    SVKYEFDT(3),SVKYEFDT                                             
         BZ    HDRF10                                                           
         GOTO1 VDATCON,BODMCB,(8,SVKYEFDT),(2,RFTCEFST)    EFF START &          
HDRF10   OC    SVKYEFDT+3(3),SVKYEFDT+3                                         
         BZ    HDRF15                                                           
         GOTO1 VDATCON,BODMCB,(8,SVKYEFDT),(2,RFTCEFEN)    END DATES            
HDRF15   DS    0H                                                               
*                                                                               
         LA    RE,RFTCRTES                                                      
         LA    RF,SAVRATCS                                                      
         LA    R0,SAVRATCS+L'SAVRATCS                                           
HDRF20   DS    0H                                                               
         OC    0(L'SAVRATC,RF),0(RF)           ANY RATE CARD?                   
         BZ    *+14                            NO                               
         MVC   0(RFTCRTSL,RE),0(RF)                                             
         LA    RE,RFTCRTSL(RE)                                                  
         LA    RF,RFTCRTSL(RF)                                                  
         CR    RF,R0                                                            
         BL    HDRF20                                                           
*                                                                               
         MVI   RFTAMODE,RFTAMSTQ                        FETCH MODE              
         MVI   RFTCNTL,RFTCHDRQ+RFTCRTEQ                DATA FLAGS              
         LA    RE,FTCHHOOK                                                      
         STCM  RE,15,RFTHOOKA                           HOOK ROUTINE            
         SR    R6,R6                                                            
         GOTO1 VFETCH,BODMCB,FETCHBLK                                           
*                                                                               
         GOTOX (RESVALQ,AREPRO01),BOPARM RESTORE TIA                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* HOOK FOR THE FETCH ROUTINE TO ADD NEW DETAIL CLUSTERS                         
***********************************************************************         
FTCHHOOK NTR1                                                                   
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
         OC    RFTERR,RFTERR                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         TM    RFTMODE,RFTNHDRQ    NEW HEADER?                                  
         BZ    FHOOKX              NO                                           
*                                                                               
         OC    SVKYEFDT,SVKYEFDT   EFFECTIVE DATES?                             
         BNZ   FHIP0               YES                                          
*                                                                               
         CLC   RFTFEFST,SVFLEFSD            START/START.                        
         BL    DOFFLT10            STARTS BEFORE THE FILTER                     
         BE    FHIP0               START IS THE SAME                            
*                                  STARTS AFTER THE FILTER                      
*                                                                               
DOFFLT5  CLC   RFTFEFST,SVFLEFED            START/END                           
         BH    FHOOKX              STARTS AFTER END OF FILTER                   
         BE    FHIP0               START IS THE END OF FILTER                   
         BL    FHIP0               STARTS DURING THE FILTER                     
*                                                                               
DOFFLT10 OC    RFTFEFEN,RFTFEFEN            ANY END?                            
         BZ    FHIP0               NO                                           
         CLC   RFTFEFEN,SVFLEFSD            END/START                           
         BL    FHOOKX              ENDS BEFORE THE FILTER                       
         BE    FHIP0               END IS THE START OF FILTER                   
         BH    FHIP0               ENDS AFTER THE START OF FILTER               
********                                                                        
* PROCESS PROGRAM NAMES                                                         
********                                                                        
FHIP0    DS    0H                                                               
         OI    RFTRETRN,RFTRXITQ                                                
         LA    R3,RFTFPGMS                                                      
         LA    R2,SAVPRGNS                                                      
         XC    SAVPRGNS,SAVPRGNS                                                
         OC    RFTFPGMS,RFTFPGMS                                                
         BZ    FHIPX               NOT GOOD!!!                                  
*                                                                               
         USING SAVPRGND,R2                                                      
         SR    RE,RE               PROGRAM NAME COUNT                           
FHIP5    CLI   0(R3),0             ANY NAME ?                                   
         BE    FHIP10              NO                                           
         CH    RE,=H'8'            LAST                                         
         BE    FHIP10              YES                                          
         LA    RE,1(RE)                                                         
         LA    RF,L'RFTFPGMS-1(R3) LAST CHARACTER OF NAME                       
         CLI   0(RF),C' '          SIGN. CHAR?                                  
         BH    *+8                 YES                                          
         BCT   RF,*-8                                                           
*                                                                               
         SR    RF,R3               LENGTH OF STRING                             
         LA    RF,1(RF)            CORRECTION FACTOR                            
         STC   RF,SAVPRGLN         SAVE IT IN THE TABLE                         
         LA    R3,L'RFTFPGMS(R3)                                                
         LA    R2,SAVPRGNQ(R2)                                                  
         B     FHIP5               CHECK NEXT NAME                              
         DROP  R2                                                               
*                                                                               
FHIP10   STC   RE,PRGCOUNT                                                      
         STC   RE,BOBYTE1          PROGRAMS LEFT TO MATCH                       
         XC    BOHALF1,BOHALF1     CLEAR THE SAVED SEQUENCE #                   
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRTXELQ    TEXT ELEMENT                                 
         MVI   MINEKEY+1,RPRTXPRQ  PROGRAM TYPE TEXT                            
         BAS   RE,MINIOHI                                                       
         BNE   FHIP30              ADD ALL THE PROGRAM NAMES                    
         L     R6,MINELEM                                                       
         USING RPRTXELD,R6                                                      
FHIP12   CLI   0(R6),RPRTXELQ      TEXT ELEMENT?                                
         BNE   FHIP30              NO - ADD THE NAMES                           
         CLI   2(R6),RPRTXPRQ      PROGRAM TEXT?                                
         BNE   FHIP30              NO - ADD THE NAMES                           
*                                                                               
         MVC   BOHALF1,RPRTXSEQ    SAVE THE SEQUENCE NUMBER FOR ADDING          
         ZIC   R1,RPRTXLEN         ELEMENT LENGTH                               
         LA    RE,RPRTXOVQ         ELEMENT OVERHEAD                             
         SR    R1,RE               TEXT LENGTH                                  
         LA    R3,RFTFPGMS                                                      
         LA    R2,SAVPRGNS                                                      
         USING SAVPRGND,R2                                                      
*                                                                               
         ZIC   R0,PRGCOUNT         NUMBER OF PROGRAM NAMES                      
FHIP14   ZIC   RE,SAVPRGLN         PROGRAM NAME LENGTH                          
         OC    SAVPRGCD,SAVPRGCD   ALREADY MATCHED?                             
         BNZ   FHIP16              YES - SKIP                                   
*                                                                               
         CR    RE,R1               LENGTH MATCH?                                
         BNE   FHIP16              NO                                           
         BCTR  R1,0                GET EX TEXT LENGTH                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),RPRTXTXT    TEXT MATCH?                                  
         BNE   FHIP16              NO                                           
*                                                                               
         ZIC   R1,BOBYTE1          CORRECT MATCHES LEFT COUNT                   
         BCTR  R1,0                                                             
         STC   R1,BOBYTE1                                                       
         MVC   SAVPRGCD,RPRTXSEQ   SAVE THE TEXT SEQUENCE #                     
         LTR   R1,R1               ANY LEFT TO MATCH?                           
         BZ    FHIPX               NO                                           
*                                                                               
FHIP16   LA    R2,SAVPRGNQ(R2)     NEXT PROGRAM NAME                            
         LA    R3,L'RFTFPGMS(R3)                                                
         BCT   R0,FHIP14                                                        
*                                                                               
FHIP18   BAS   RE,MINIOSEQ         NO MATCHES FOR THIS ELEMENT                  
         BNE   FHIP30              EOR - ADD PROGRAM NAMES                      
         B     FHIP12                                                           
         DROP  R2,R6                                                            
*                                                                               
FHIP30   DS    0H                  ADD REMAINING PROGRAM NAMES                  
         LA    R3,RFTFPGMS                                                      
         LA    R2,SAVPRGNS                                                      
         USING SAVPRGND,R2                                                      
         ZIC   R0,BOBYTE1                                                       
*                                                                               
FHIP33   OC    SAVPRGCD,SAVPRGCD   ALREADY MATCHED?                             
         BZ    FHIP35              NO                                           
         LA    R2,SAVPRGNQ(R2)     NEXT PROGRAM NAME                            
         LA    R3,L'RFTFPGMS(R3)                                                
         B     FHIP33                                                           
*                                                                               
FHIP35   L     R6,MINELEM                                                       
         USING RPRTXELD,R6                                                      
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRTXEL,RPRTXELQ                                                 
         ZIC   RE,SAVPRGLN         TEXT LENGTH                                  
         LA    R1,RPRTXOVQ(RE)     ELEMENT LENGTH                               
         STC   R1,RPRTXLEN                                                      
         MVI   RPRTXTYP,RPRTXPRQ PROGRAM TYPE                                   
         LH    R1,BOHALF1          LOAD SAVED SEQ. #                            
         LA    R1,1(R1)                                                         
         STCM  R1,3,RPRTXSEQ       NEW SEQUENCE #                               
         STCM  R1,3,SAVPRGCD                                                    
         STH   R1,BOHALF1                                                       
         BCTR  RE,0                EX TEXT LENGTH                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RPRTXTXT(0),0(R3)   TEXT                                         
         BAS   RE,MINIOADD         ADD THE ELEMENT                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FHIP37   LA    R2,SAVPRGNQ(R2)     NEXT PROGRAM NAME                            
         LA    R3,L'RFTFPGMS(R3)                                                
         BCT   R0,FHIP33                                                        
         DROP  R2,R6                                                            
*                                                                               
FHIPX    DS    0H                                                               
         LA    R2,SAVPRGNS                                                      
         USING SAVPRGND,R2                                                      
         LA    R3,PRGLIST                                                       
         XC    PRGLIST,PRGLIST                                                  
*                                                                               
FHPL0    DS    0H                                                               
         OC    SAVPRGCD,SAVPRGCD                                                
         BZ    *+14                                                             
         MVC   0(L'PRGLST,R3),SAVPRGCD                                          
         LA    R3,L'PRGLST(R3)                                                  
         LA    R2,SAVPRGNQ(R2)                                                  
         LA    R0,PRGLIST+L'PRGLIST                                             
         CR    R3,R0                                                            
         BNL   *+14                                                             
         LA    R0,SAVPRGNS+L'SAVPRGNS                                           
         CR    R2,R0                                                            
         BL    FHPL0                                                            
         DROP  R2                                                               
*                                                                               
*===================================================                            
         EJECT                                                                  
**************************************                                          
* ADD DETAIL ELEMENT CLUSTER                                                    
**************************************                                          
*    DETAIL ELEMENT                                                             
********                                                                        
         L     R0,MINELEM          CLEAR OUT MINELEM                            
         LH    R1,MINMAXEL                                                      
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         MVI   RPRDTEL,RPRDTELQ                                                 
         MVI   RPRDTLEN,RPRDTOVQ   LENGTH W/O WEEKS                             
         LA    R2,RFTFDTMS                                                      
         USING RFTFDTDY,R2                                                      
         MVC   RPRDTDAY,RFTFDTDY   DAYS                                         
         MVC   RPRDTSTM,RFTFDTST   START TIME                                   
         MVC   RPRDTETM,RFTFDTEN   END TIME                                     
*                                                                               
         GOTO1 VDAYUNPK,BODMCB,RFTFDTDY,SVKYDYTM                                
         LA    RE,SVKYDYTM                                                      
FHKDT02  CLI   0(RE),C' '          CONVERT '/'S TO ','S                         
         BNH   FHKDT04                                                          
         CLI   0(RE),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         B     FHKDT02                                                          
*                                                                               
FHKDT04  LA    RE,SVKYDYTM+9       FIND WHERE TO PUT THE TIMES                  
FHKDT06  CLI   0(RE),C' '                                                       
         BH    FHKDT08                                                          
         BCTR  RE,0                                                             
         B     FHKDT06                                                          
FHKDT08  LA    RE,1(RE)                                                         
*                                                                               
         MVI   0(RE),C'/'          SEPARATE DAYS AND TIMES WITH A C'/'          
         LA    RE,1(RE)                                                         
         LR    R0,RE                                                            
*                                                                               
         GOTO1 VUNTIME,BODMCB,RFTFDTST,(R0)                                     
         DROP  R2                                                               
         XC    SDTLIST,SDTLIST                                                  
         MVC   SDTLIST,RFTFDTMS+L'SDTLST                                        
*                                                                               
         GOTOX (PCKTIMQ,AREPRO01),BODMCB,RFTFDTST,RFTFDTEN,RPRDTTIM             
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         MVC   RPRDTSTA,0(RE)      INTERNAL STATION CODE                        
         GOTO1 VDATCON,BODMCB,(2,RFTFEFST),(19,RPRDTEFF)                        
         GOTO1 VDATCON,BODMCB,(2,RFTFEFEN),(19,RPRDTEEF)                        
         LR    RE,RA                                                            
         AH    RE,=Y(SVHDEFDT-TWAD)                                             
         MVC   0(L'SVHDEFDT,RE),RPRDTEFF                                        
*                                                                               
FHIDT18  DS    0H                                                               
         MVC   RPRDTINM,RFTFINV    INVENTORY NUMBER                             
         MVC   RPRDTSTM,RFTFDTST   FIRST START TIME IN DETAIL                   
         MVC   RPRDTETM,RFTFDTEN   FIRST END TIME IN DETAIL                     
         MVC   RPRDTPRG,SAVPRGNS+(SAVPRGCD-SAVPRGND)                            
         MVC   RPRDTSLN,SAVSLNS    FIRST LENGTH IS DEFAULT                      
*                                                                               
*                                                                               
* CHECK FOR FIRST COST                                                          
*                                                                               
         OC    SAVRATC,SAVRATC     ANY RATE CARD?                               
         BZ    FHIDT30             NO - NO COST THEN                            
*                                                                               
         LA    RE,RFTFRTES                                                      
         LA    R0,RFTFRTEN                                                      
FHIDT20  DS    0H                                                               
         OC    0(RFTFRTSL,RE),0(RE)                                             
         BZ    FHIDT30                                                          
         CLC   SAVRATC,0(RE)       MATCH?                                       
         BE    FHIDT22             YES                                          
         LA    RE,RFTFRTSL(RE)                                                  
         BCT   R0,FHIDT20                                                       
         B     FHIDT30                                                          
*                                                                               
FHIDT22  DS    0H                                                               
         MVC   RPRDTNC1,RFTFRRTE-RFTFRTES(RE)                                   
         NI    RPRDTNC1,X'7F'       CLEAR N/A FLAG ON LARGE COST                
*                                                                               
FHIDT30  DS    0H                                                               
         ZIC   RE,1(R6)            SKIP OVER ELEM                               
         AR    R6,RE                                                            
*                                                                               
*===================================================                            
         SPACE 2                                                                
********                                                                        
*    SUPLEMENTAL DAY/TIME ELEMENTS                                              
********                                                                        
         LA    R3,RFTFDTMS+L'RFTFDTMS                                           
         USING RPRDYELD,R6                                                      
         OC    0(L'RFTFDTMS,R3),0(R3)                                           
         BZ    FHISDXX             NO DATA - DONE                               
*                                                                               
         MVI   RPRDYEL,RPRDYELQ    ELEMENT CODE                                 
*                                                                               
         LA    R0,7                MAX OF 7 EXTRA DAY TIMES                     
         LA    R2,RPRDYDTM         DAY/TIME LIST                                
*                                                                               
FHISD5   MVC   0(L'RPRDYDTM,R2),0(R3)                                           
         LA    R2,L'RPRDYDTM(R2)                                                
         LA    R3,L'RFTFDTMS(R3)                                                
         OC    0(L'RFTFDTMS,R3),0(R3)                                           
         BZ    FHISDX              NO DATA - DONE                               
         BCT   R0,FHISD5                                                        
*                                                                               
FHISDX   DS    0H                                                               
         SR    R2,R6               ELEMENT LENGTH                               
         STC   R2,RPRDYLEN                                                      
*                                                                               
         ZIC   RE,1(R6)            SKIP OVER ELEM                               
         AR    R6,RE                                                            
FHISDXX  DS    0H                                                               
*                                                                               
*===================================================                            
         SPACE 2                                                                
********                                                                        
*    AVAIL DAY/TIMES                                                            
********                                                                        
FHIAV    DS    0H                                                               
         XC    ADTLIST,ADTLIST                                                  
*                                                                               
         OC    RFTFAVLS(RFTFAVNQ*L'RFTFAVLS),RFTFAVLS                           
         BZ    FHIAVX              NONE - ADD INV DAYTIMES??                    
*                                                                               
         LA    R2,RFTFAVLS                                                      
         LA    R0,RFTFAVLS+(RFTFAVNQ*L'RFTFAVLS)                                
FHIAV1   CLI   0(R2),C'/'          PARSE CHARACTER?                             
         BNE   *+8                                                              
         MVI   0(R2),C'\'          YES - REPLACE IT                             
         LA    R2,1(R2)                                                         
         CR    R2,R0                                                            
         BL    FHIAV1                                                           
*                                                                               
         MVC   ADTLIST,RFTFAVLS                                                 
*                                                                               
         LA    R2,RFTFAVLS                                                      
         USING RPRAVELD,R6                                                      
         LA    R3,RPRAVALS                                                      
         MVI   RPRAVEL,RPRAVELQ                                                 
*                                                                               
FHIAV2   OC    0(L'RFTFAVLS,R2),0(R2)                                           
         BZ    FHIAV4                                                           
*                                                                               
         MVC   0(L'RPRAVALS,R3),0(R2)                                           
         LA    R3,L'RPRAVALS(R3)                                                
         LA    R2,L'RFTFAVLS(R2)                                                
         CR    R2,R0                                                            
         BL    FHIAV2                                                           
*                                                                               
FHIAV4   DS    0H                                                               
         SR    R3,R6               ELEMENT LENGTH                               
         STC   R3,RPRAVLEN                                                      
         ZIC   RE,1(R6)            SKIP OVER ELEM                               
         AR    R6,RE                                                            
FHIAVX   DS    0H                                                               
*                                                                               
*===================================================                            
         SPACE 2                                                                
********                                                                        
*    SUPLEMENTAL PROGRAM ELEMENTS                                               
********                                                                        
         LA    R2,SAVPRGNS+L'SAVPRGN         SECOND NAME                        
         USING SAVPRGND,R2                                                      
         USING RPRPRELD,R6                                                      
*                                                                               
         OC    SAVPRGCD,SAVPRGCD   ANY TEXT ATTACHED?                           
         BZ    FHISPXX             NO                                           
*                                                                               
         MVI   RPRPREL,RPRPRELQ    ELEMENT CODE                                 
*                                                                               
         LA    R3,RPRPRPRG         PROGRAM LINKS LIST                           
         LA    R0,7                MAX OF 7 EXTRA NAMES                         
FHISP5   MVC   0(L'RPRPRPRG,R3),SAVPRGCD                                        
         LA    R2,SAVPRGNQ(R2)                                                  
         LA    R3,L'RPRPRPRG(R3)                                                
         OC    SAVPRGCD,SAVPRGCD   ANY TEXT ATTACHED?                           
         BZ    FHISPX              NO                                           
         BCT   R0,FHISP5                                                        
*                                                                               
FHISPX   DS    0H                                                               
         SR    R3,R6               ELEMENT LENGTH                               
         STC   R3,RPRPRLEN                                                      
         DROP  R2                                                               
         ZIC   RE,1(R6)            SKIP OVER ELEM                               
         AR    R6,RE                                                            
FHISPXX  DS    0H                                                               
*                                                                               
*===================================================                            
********                                                                        
*    SUPPLEMENTAL COSTS                                                         
********                                                                        
         OC    RFTFRTES(RFTFRTEN*RFTFRTSL),RFTFRTES                             
         BZ    FHICSX                                                           
*                                                                               
         LA    R2,SAVRATCS+L'SAVRATC        SUPPLEMENTAL RATE CARDS?            
         OC    0(3*L'SAVRATC,R2),0(R2)                                          
         BZ    FHICS30                      NO                                  
*                                                                               
         USING RPRCSELD,R6                                                      
         MVI   RPRCSEL,RPRCSELQ                                                 
         MVI   RPRCSLEN,RPRCSLNQ                                                
         LA    R3,RPRCSNC2                                                      
         DROP  R6                                                               
*                                                                               
FHICS08  DS    0H                                                               
         OC    0(L'SAVRATC,R2),0(R2)        ANY RATE CARD?                      
         BZ    FHICS20                      NO                                  
*                                                                               
         LA    RE,RFTFRTES                                                      
         LA    R0,RFTFRTEN                                                      
FHICS10  DS    0H                                                               
         OC    0(RFTFRTSL,RE),0(RE)                                             
         BZ    FHICS20                                                          
         CLC   0(L'SAVRATC,R2),0(RE)        MATCH?                              
         BE    FHICS12                      YES                                 
         LA    RE,RFTFRTSL(RE)                                                  
         BCT   R0,FHICS10                                                       
         B     FHICS20                                                          
*                                                                               
FHICS12  DS    0H                                                               
         MVC   0(L'RPRCSNC2,R3),RFTFRRTE-RFTFRTES(RE)                           
         NI    0(R3),X'7F'           CLEAR N/A FLAG ON LARGE COST               
*                                                                               
FHICS20  DS    0H                                                               
         LA    R2,L'SAVRATC(R2)                                                 
         LA    R3,RPRCSNC3-RPRCSNC2(R3)                                         
         LA    R0,SAVRATCS+L'SAVRATCS                                           
         CR    R2,R0                                                            
         BL    FHICS08                                                          
*                                                                               
FHICS30  DS    0H                  NOW DO PERCENTAGES                           
*                                                                               
FHICSX   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINCSEL-TWAD)                                              
         MVC   0(L'MINCSEL,RE),0(R6)                                            
*                                                                               
         ZIC   RE,1(R6)            SKIP OVER ELEM                               
         AR    R6,RE                                                            
*                                                                               
         SPACE 2                                                                
FHIX     DS    0H                                                               
FHOOKX   B     EXITOK                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BEFORE THE I/O CALL TO ADD THE RECORD                                         
*                                                                               
* AIO5 STILL CONTAINS OUR FAKE RECORD WITH MINIO CLUSTER                        
***********************************************************************         
R1STRADD DS    0H                                                               
         NMOD1 0,**RADD**                                                       
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
*                                                                               
         L     R6,AIO5                                                          
         OC    RPRORLEN-RPROHDRD(2,R6),RPRORLEN-RPROHDRD(R6)                    
         BNZ   *+10                                                             
         MVC   RPRORLEN-RPROHDRD(2,R6),=Y(RPRDTOVQ+(RPROR1ST-RPROHDRD))         
*                                                                               
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
         USING RPRDTELD,R6                                                      
         MVI   RPRDTEL,RPRDTELQ                                                 
         MVI   RPRDTLEN,RPRDTOVQ               LENGTH W/O WEEKS                 
         MVC   RPRDTINM,SVKYINVT               INVENTORY NUMBER                 
         MVC   RPRDTEFF(L'SVKYEFDT),SVKYEFDT   EFFECTIVE DATES                  
         MVC   RPRDTPRG,PRGLST                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(DPTINKEY-TWAD)                                             
         MVC   RPRDTDPT,0(RE)                                                   
         MVC   RPRDTDAY,DAYINKEY-DPTINKEY(RE)                                   
         MVC   RPRDTSTM(4),TIMINKEY-DPTINKEY(RE)                                
         MVC   RPRDTTIM,PTMINMKY-DPTINKEY(RE)                                   
         MVI   RPRDTSEQ,1                                                       
*                                                                               
         LR    RF,RE               GET BUYER CPP                                
         LR    RE,RA                                                            
         AH    RE,=Y(MINDPTS-TWAD)                                              
         LA    R0,L'MINDPTS(RE)                                                 
RFRA02   CLC   0(1,RE),0(RF)                                                    
         BE    RFRA06                                                           
         LA    RE,L'MINDPT(RE)                                                  
         CR    RE,R0                                                            
         BL    RFRA02                                                           
*                                                                               
         LA    RE,CSARDPT          BUYER CPP FROM CSARDPTS                      
RFRA04   CLC   0(1,RE),0(RF)                                                    
         BE    RFRA06                                                           
         LA    RE,5(RE)                                                         
         LA    R0,CSARDPT+L'CSARDPT                                             
         CR    RE,R0                                                            
         BL    RFRA04                                                           
         XC    BOFULL1,BOFULL1                                                  
         LA    RE,BOFULL1-1                                                     
*                                                                               
RFRA06   MVC   RPRDTTAB,1(RE)                                                   
*                                                                               
         LA    R1,1                                                             
         LA    R3,SAVXBKS          SET SUPRESSED BOOKS                          
         USING XBOKLIN,R3                                                       
RFRA12   DS    0H                                                               
         OC    XBLNDPT,XBLNDPT     ANY DAYPARTS?                                
         BZ    RFRA16              NO - NOTHING TO DO                           
*                                                                               
         LA    RE,XBLNDPT                                                       
         LA    RF,L'XBLNDPT(RE)                                                 
*                                                                               
RFRA14   DS    0H                                                               
         CLC   RPRDTDPT,0(RE)      DAYPART IN MATCH?                            
         BE    RFRA15              YES - SUPPRESS                               
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BL    RFRA14                                                           
         B     RFRA16              NOT FOUND, DON'T SUPPRESS                    
*                                                                               
RFRA15   LA    RF,X'80'                                                         
         LR    RE,R1                                                            
         SRL   RF,0(RE)                                                         
         STC   RF,BOBYTE1          GET CORRECT MASK FOR BOOK                    
         OC    RPRDTBKS,BOBYTE1                                                 
*                                                                               
RFRA16   DS    0H                                                               
         LA    R1,1(R1)                                                         
         LA    R3,L'SAVXBK(R3)                                                  
         LA    R0,SAVXBKS+L'SAVXBKS                                             
         CR    R3,R0                                                            
         BL    RFRA12                                                           
         DROP  R3                                                               
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         MVC   RPRDTSTA,0(RE)      INTERNAL STATION CODE                        
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),RPRDTSTA                                 
***************                                                                 
* SUPPLEMENTAL DAY/TIME ELEMENT                                                 
***************                                                                 
RFRADY00 L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
*                                                                               
         SR    R0,R0                                                            
         NI    MISCFLG1,X'FF'-MF1TMPBT                                          
RFRADY10 CLI   0(R6),0             ANY SUPP DAY/TIME ELEMENT BEFORE?            
         BNE   *+12                                                             
RFRADY15 OI    MISCFLG1,MF1TMPBT   NO                                           
         B     RFRADY20                                                         
*                                                                               
         CLI   0(R6),RPRDYELQ                                                   
         BE    RFRADY20                                                         
         BH    RFRADY15                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RFRADY10                                                         
*                                                                               
RFRADY20 OC    SDTLIST,SDTLIST     ANY 2NDARY DAY/TIMES?                        
         BNZ   RFRADY30            YES                                          
         TM    MISCFLG1,MF1TMPBT   NO, ANY BEFORE?                              
         BNZ   RFRADYX                 NO, NOTHING TO WORRY ABOUT THEN          
         B     RFRADY50                YES, DELETE IT THEN                      
*                                                                               
         USING RPRDYELD,R6                                                      
RFRADY30 XC    BOELEM,BOELEM                                                    
         MVI   BOELEM,RPRDYELQ                                                  
         TM    MISCFLG1,MF1TMPBT   NEED A NEW SUPP DAY/TIME ELEMENT?            
         BNZ   RFRADY40            YES                                          
         ZIC   RE,RPRDYLEN         COPY ELEMENT OVER IF THERE BEFORE            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),RPRDYEL                                                
*                                                                               
RFRADYD  USING RPRDYELD,BOELEM                                                  
RFRADY40 LA    R2,SDTLIST            SET UP HOW CHANGED OR NEW ONE              
         LA    RE,RFRADYD.RPRDYDTM      WILL LOOK                               
         LA    R0,SDTLIST+L'SDTLIST                                             
RFRADY45 OC    0(L'SDTLST,R2),0(R2)                                             
         BZ    *+14                                                             
         MVC   0(L'SDTLST,RE),0(R2)                                             
         LA    RE,L'SDTLST(RE)                                                  
*                                                                               
         LA    R2,L'SDTLST(R2)                                                  
         CR    R2,R0                                                            
         BL    RFRADY45                                                         
*                                                                               
         LA    RF,RFRADYD.RPRDYEL                                               
         SR    RE,RF                                                            
         STC   RE,RFRADYD.RPRDYLEN                                              
*                                                                               
         CLC   RPRDYLEN,RFRADYD.RPRDYLEN  SAME LENGTH?                          
         BNE   RFRADY50                   NO                                    
         BCTR  RE,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RPRDYEL(0),RFRADYD.RPRDYEL  YES                                  
         B     RFRADYX                                                          
*                                                                               
RFRADY50 TM    MISCFLG1,MF1TMPBT                                                
         BNZ   RFRADY60                                                         
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
RFRADY60 OC    SDTLIST,SDTLIST                                                  
         BZ    RFRADYX                                                          
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
*                                                                               
RFRADYX  DS    0H                                                               
***************                                                                 
* AVAIL DAY/TIME ELEMENT                                                        
***************                                                                 
RFRWDA00 L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
*                                                                               
         SR    R0,R0                                                            
RFRWDA10 CLI   0(R6),0             ANY SUPP DAY/TIME ELEMENT BEFORE?            
         BE    RFRWDA20                                                         
*                                                                               
         CLI   0(R6),RPRAVELQ                                                   
         BH    RFRWDA20                                                         
         BE    RFRWDA15            YES, THEN REMOVE IT                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RFRWDA10                                                         
*                                                                               
RFRWDA15 GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
RFRWDA20 OC    ADTLIST,ADTLIST     ANY AVAIL DAY/TIMES?                         
         BZ    RFRWDAX             NO                                           
*                                                                               
RFRWDAD  USING RPRAVELD,BOELEM                                                  
         XC    BOELEM,BOELEM                                                    
         MVI   RFRWDAD.RPRAVEL,RPRAVELQ                                         
         LA    R2,ADTLIST            SET UP HOW CHANGED OR NEW ONE              
         LA    RE,RFRWDAD.RPRAVALS      WILL LOOK                               
         LA    R0,ADTLIST+L'ADTLIST                                             
RFRWDA30 OC    0(L'ADTLST,R2),0(R2)                                             
         BZ    *+14                                                             
         MVC   0(L'ADTLST,RE),0(R2)                                             
         LA    RE,L'ADTLST(RE)                                                  
*                                                                               
         LA    R2,L'ADTLST(R2)                                                  
         CR    R2,R0                                                            
         BL    RFRWDA30                                                         
*                                                                               
         LA    RF,RFRWDAD.RPRAVEL                                               
         SR    RE,RF                                                            
         STC   RE,RFRWDAD.RPRAVLEN                                              
*                                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
*                                                                               
RFRWDAX  DS    0H                                                               
***************                                                                 
* SUPPLEMENTAL PROGRAM ELEMENT                                                  
***************                                                                 
RFRAPR00 L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
*                                                                               
         SR    R0,R0                                                            
         NI    MISCFLG1,X'FF'-MF1TMPBT                                          
RFRAPR10 CLI   0(R6),0             ANY SUPP PROGRAM ELEMENT BEFORE?             
         BNE   *+12                                                             
RFRAPR15 OI    MISCFLG1,MF1TMPBT   NO                                           
         B     RFRAPR20                                                         
*                                                                               
         CLI   0(R6),RPRPRELQ                                                   
         BE    RFRAPR20                                                         
         BH    RFRAPR15                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RFRAPR10                                                         
*                                                                               
RFRAPR20 OC    PRGLIST+L'PRGLST(L'PRGLIST-L'PRGLST),PRGLIST+L'PRGLST            
         BNZ   RFRAPR30            WE HAVE SUPP PROGRAM TEXT                    
         TM    MISCFLG1,MF1TMPBT   NO, ANY BEFORE?                              
         BNZ   RFRAPRX                 NO, NOTHING TO WORRY ABOUT THEN          
         B     RFRAPR50                YES, DELETE IT THEN                      
*                                                                               
         USING RPRPRELD,R6                                                      
RFRAPR30 XC    BOELEM,BOELEM                                                    
         MVI   BOELEM,RPRPRELQ                                                  
         TM    MISCFLG1,MF1TMPBT   NEED A NEW SUPP PROGRAM ELEMENT?             
         BNZ   RFRAPR40            YES                                          
         ZIC   RE,RPRPRLEN         COPY ELEMENT OVER IF THERE BEFORE            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),RPRPREL                                                
*                                                                               
RFRAPRD  USING RPRPRELD,BOELEM                                                  
RFRAPR40 LA    R2,PRGLIST+L'PRGLST   SET UP HOW CHANGED OR NEW ONE              
         LA    RE,RFRAPRD.RPRPRPRG      WILL LOOK                               
         LA    R0,PRGLIST+L'PRGLIST                                             
RFRAPR45 OC    0(L'PRGLST,R2),0(R2)                                             
         BZ    *+14                                                             
         MVC   0(L'PRGLST,RE),0(R2)                                             
         LA    RE,L'PRGLST(RE)                                                  
*                                                                               
         LA    R2,L'PRGLST(R2)                                                  
         CR    R2,R0                                                            
         BL    RFRAPR45                                                         
*                                                                               
         LA    RF,RFRAPRD.RPRPREL                                               
         SR    RE,RF                                                            
         STC   RE,RFRAPRD.RPRPRLEN                                              
*                                                                               
         CLC   RPRPRLEN,RFRAPRD.RPRPRLEN  SAME LENGTH?                          
         BNE   RFRAPR50                   NO                                    
         BCTR  RE,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RPRPREL(0),RFRAPRD.RPRPREL  YES                                  
         B     RFRAPRX                                                          
*                                                                               
RFRAPR50 TM    MISCFLG1,MF1TMPBT                                                
         BNZ   RFRAPR60                                                         
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
RFRAPR60 OC    PRGLIST+L'PRGLST(L'PRGLIST-L'PRGLST),PRGLIST+L'PRGLST            
         BZ    RFRAPRX                                                          
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
*                                                                               
RFRAPRX  DS    0H                                                               
******************                                                              
** COST ELEMENT **                                                              
******************                                                              
         XC    BOELEM,BOELEM                                                    
         LR    RE,RA                                                            
         AH    RE,=Y(MINCSEL-TWAD)                                              
         MVC   BOELEM(L'MINCSEL),0(RE)                                          
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         USING RPRDTELD,R6                                                      
         MVC   BOFULL1,RPRDTNC1                                                 
         OC    BOFULL1,BOFULL1                                                  
         BZ    RFRACS20            NO COST SKIP LINE                            
         TM    BOFULL1,X'80'       N/A?                                         
         BNZ   RFRACS20            YES - SKIP LINE                              
         DROP  R6                                                               
*                                                                               
         MVI   BOELEM,RPRCSELQ                                                  
         MVI   BOELEM+1,RPRCSLNQ                                                
*                                                                               
         LA    R0,NUMCSTS-1                                                     
         LA    R4,BOELEM+(RPRCSSC2-RPRCSELD)                                    
         LR    RE,RA                                                            
         AH    RE,=Y(MINCOSTS-TWAD)                                             
         LA    RE,L'MINCOST(RE)                                                 
         USING CSTLIN,RE                                                        
*                                                                               
RFRACS16 DS    0H                                                               
         CLI   CSLNPBC,0           % OF BASE COST?                              
         BE    RFRACS18            NO                                           
*                                                                               
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         MP    PCKOF16B,=P'100'                                                 
         ZIC   R1,CSLNPBC                                                       
         CVD   R1,BODUB1                                                        
         MP    PCKOF16B,BODUB1                                                  
         DP    PCKOF16B,=PL8'100'                                               
         MVC   BODUB1,PCKOF16B                                                  
         SRP   BODUB1,64-2,5                                                    
*                                                                               
         SRP   BODUB1,64-2,5                                                    
         CVB   R1,BODUB1                                                        
         MH    R1,=H'100'          MAKE IT DOLLARS                              
         ST    R1,L'RPRCSSC2(R4)                                                
*                                                                               
RFRACS18 DS    0H                                                               
         LA    RE,L'MINCOST(RE)                                                 
         LA    R4,L'RPRCSSC2+L'RPRCSNC2(R4)                                     
         BCT   R0,RFRACS16                                                      
         DROP  RE                                                               
*                                                                               
RFRACS20 DS    0H                                                               
         CLI   BOELEM+1,RPRCSLNQ                                                
         BNE   RFRACSX                                                          
*                                                                               
         L     R6,AIO5             WHERE DOES IT GO?                            
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
*                                                                               
RFRACS22 DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    RFRACS24                                                         
         CLI   0(R6),RPRCSELQ                                                   
         BNL   RFRACS24                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RFRACS22                                                         
*                                                                               
RFRACS24 DS    0H                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
*                                                                               
RFRACSX  DS    0H                                                               
***************                                                                 
* NEED TO ADD THE MINIO CLUSTER TO THE RECORD                                   
***************                                                                 
RFRADDX  BAS   RE,FROMAIO5                                                      
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
RFRADDX1 DS    0H                                                               
         BAS   RE,MINIOADD                                                      
         BE    RFRADDX2                                                         
*                                                                               
         ZIC   R1,RPRDTSEQ                                                      
         LA    R1,1(R1)                                                         
         STC   R1,RPRDTSEQ                                                      
         CH    R1,=H'255'                                                       
         BNH   RFRADDX1                                                         
         DC    H'0'                DIE IF NO MORE SEQ NUMBERS AVAILABLE         
*                                                                               
RFRADDX2 EDIT  (B1,RPRDTSEQ),SVKYSEQN,FILL=0,WRK=BOWORK1,DUB=BODUB1             
         LR    RE,RA                                                            
         AH    RE,=Y(SEQINKEY-TWAD)                                             
         MVC   0(L'SEQINKEY,RE),RPRDTSEQ                                        
         DROP  R6                                                               
*                                                                               
RFRADDXX DS    0H                                                               
***************                                                                 
* DEMO VALUE ELEMENT                                                            
***************                                                                 
RFRADM   DS    0H                                                               
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
         GOTOX (DTLNFTQ,AREPRO01),BODMCB,SAVSTAS,(X'80',SAVBKS),       X        
               SAVDMOS,BOWORK1                                                  
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ                                                 
         MVC   MINEKEY+1(7),BOWORK1                                             
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,INTOAIO5                                                      
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)    FIRST ELEMENT                         
         USING RPRDVELD,R6                                                      
RFRADM2  CLI   0(R6),RPRDVELQ      DEMO VALUE ELEMENT?                          
         BNE   *+14                NO                                           
         CLC   RPRDVBNM,SAVBK+(BKLNIORD-BOOKLIN)                                
         BE    RFRADM4             YES                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF MINIO CLUSTER?                        
         BNE   RFRADM2             NO                                           
*                                                                               
         CLI   SVDMOELM,0          USER OVERRIDES ON ADD?                       
         BNE   RFRADM20            YES                                          
         B     RFRADMX                                                          
*                                                                               
RFRADM4  CLI   SVDMOELM,0          USER OVERRIDES ON ADD?                       
         BNE   RFRADM6             YES                                          
*                                                                               
         ZIC   R1,RPRDVLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVDMOELM(0),0(R6)   YES, MAKE A COPY OF IT                       
         B     RFRADMX                                                          
*                                                                               
RFRADM6  DS    0H                  SAVE & DELETE EXISTING ELEMENT               
         XC    BOELEM,BOELEM                                                    
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)                                                  
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
         LA    R3,BOELEM+RPRDVOVQ                                               
         LA    R2,SVDMOELM+RPRDVOVQ                                             
RFRADM10 TM    0(R2),X'80'                   SHARES BASED ON OVERRIDES          
         BZ    RFRADM15                                                         
         NI    0(R2),FF-X'80'                                                   
         L     RE,0(R2)                      RTG                                
         CVD   RE,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         SRP   PCKOF16B,3,0                                                     
*                                                                               
         MVC   BOFULL1,8(R3)                 LEVEL                              
         NI    BOFULL1,X'FF'-X'80'                                              
         L     RE,BOFULL1                                                       
         CVD   RE,BODUB1                                                        
         ZAP   PCKOF08B,BODUB1                                                  
*                                                                               
         BAS   RE,DIVPACKD                   SHR = RTG / LVL                    
         BNE   RFRADM15            DIVIDING BY 0, LEAVE SHR ALONE               
         SRP   PCKOF16B,64-1,5                                                  
*                                                                               
         ZAP   BODUB1,PCKOF16B                                                  
         CVB   RE,BODUB1                                                        
         ST    RE,4(R3)            STORE SHARE VALUE IN CORRECT PLACE           
         OI    4(R3),X'80'         SHR DEFINITELY AN OVERRIDE                   
         MVC   0(4,R3),0(R2)                                                    
         OI    0(R3),X'80'         RTG DEFINITELY AN OVERRIDE                   
*                                                                               
RFRADM15 LA    R3,L'RPRDVDMO(R3)                                                
         LA    R2,L'RPRDVDMO(R2)                                                
         ZIC   R1,SVDMOELM+1                                                    
         LA    R1,SVDMOELM(R1)                                                  
         CR    R2,R1                                                            
         BL    RFRADM10                                                         
*                                                                               
         SR    R1,R1               STORE NEW ELEMENT                            
         IC    R1,SVDMOELM+1                                                    
         CLC   SVDMOELM+1(1),BOELEM+1                                           
         BNL   *+10                                                             
         IC    R1,BOELEM+1                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVDMOELM(0),BOELEM                                               
         LA    R1,1(R1)                                                         
         STC   R1,SVDMOELM+1                                                    
*                                                                               
RFRADM20 DS    0H                                                               
         L     R6,AIO5                    WRITE ELEMENT AT CORRECT POS.         
         LA    R6,RPROR1ST-RPROKEY(R6)    FIRST ELEMENT                         
N        USING RPRDVELD,SVDMOELM                                                
*                                                                               
RFRADM30 CLI   0(R6),0                    END OF CLUSTER?                       
         BE    RFRADM32                   YES - ADD DEMO VALUE ELEM             
         ZIC   RE,1(R6)                   ELEMENT LENGTH                        
         AR    R6,RE                                                            
         CLI   0(R6),RPRDVELQ             DEMO VALUE ELEMENT?                   
         BL    RFRADM30                   NO - SKIP                             
         BH    RFRADM32                   NO - ADD HERE                         
*                                                                               
         USING RPRDVELD,R6                                                      
         CLC   RPRDVBNM,N.RPRDVBNM        BOOK MATCH?                           
         BL    RFRADM30                   NO                                    
         DROP  R6,N                                                             
*                                                                               
RFRADM32 GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),SVDMOELM,0(R6)              
*                                                                               
RFRADMX  DS    0H                                                               
         BAS   RE,FROMAIO5                                                      
         BAS   RE,MINIOWRT                                                      
*                                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE DAY/TIME FIELD                                                       
***********************************************************************         
VALDYTIM DS    0H                                                               
         NMOD1 0,**VDTM**                                                       
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
**       TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
***      BNZ   *+8                                                              
**       OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
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
         CLI   BOBYTE2,0           DAYVAL COMPLAINS IF INPUT > 11               
         BE    EXITNV                                                           
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(DAYINKEY-TWAD)                                             
         MVC   0(L'DAYINKEY,RE),BOBYTE2                                         
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
         LR    R6,RA                                                            
         AH    R6,=Y(TIMINKEY-TWAD)                                             
         MVC   0(L'TIMINKEY,R6),BOFULL1                                         
         GOTOX (PCKTIMQ,AREPRO01),BODMCB,0(R6),2(R6),                  X        
               PTMINMKY-TIMINKEY(R6)                                            
*                                                                               
         MVC   SVKYDYTM,FVIFLD                                                  
*                                                                               
VALDTMX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*          DATA SET REPRO13    AT LEVEL 083 AS OF 04/24/96                      
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
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BZ    *+12                NO                                           
         CLI   CSACT,A#WRKADD      YES, ARE WE ADDING?                          
         BNE   VALRTGX                  NO NEED TO CHANGE ANYTHING              
*                                                                               
         ZIC   R1,ONDMORTG         FIND INTERNAL ORDER NUMBER OF DEMO           
         BCTR  R1,0                                                             
         LR    R3,R1                                                            
         MH    R3,=Y(L'SAVDMO)                                                  
         LA    R3,SAVDMOS(R3)                                                   
         USING DEMOLIN,R3                                                       
         ZIC   R1,DMLNIORD                                                      
         DROP  R3                                                               
*                                                                               
         LTR   R1,R1               ANY DEMO HERE?                               
         BE    VALRTGX             NONE                                         
*                                                                               
         CLI   FVILEN,0            REQUIRED ENTRY                               
         BE    VALRTGX                                                          
*                                                                               
VALRTGD  USING RPRDVELD,SVDMOELM                                                
         CLI   VALRTGD.RPRDVEL,0                                                
         BNE   VALRTG10                                                         
         MVI   VALRTGD.RPRDVEL,RPRDVELQ                                         
         MVI   VALRTGD.RPRDVLEN,RPRDVOVQ                                        
         MVC   VALRTGD.RPRDVBNM,SAVBK+(BKLNIORD-BOOKLIN)                        
*                                                                               
VALRTG10 BCTR  R1,0                R3 = A(DEMO VALUE) IN SVDMOELM               
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R3,VALRTGD.RPRDVDMO(R1)                                          
*                                                                               
         ZIC   R0,FVILEN                                                        
         LA    R4,FVIFLD                                                        
         CLI   FVIFLD,C'*'                                                      
         BNE   *+12                                                             
         LA    R4,1(R4)                                                         
         SH    R0,=H'1'                                                         
*                                                                               
         GOTO1 VCASHVAL,BODMCB,(1,(R4)),(R0)    1 DECIMAL PLACE                 
         CLI   0(R1),X'FF'                                                      
         BE    EXITNOTN            NOT NUMERIC                                  
*                                                                               
         MVC   BOFULL1,0(R3)       MAKE A COPY OF OLD RATING                    
         MVC   0(4,R3),BODMCB+4    JUST CHANGE THE RATING PORTION               
         NI    BOFULL1,X'FF'-X'80'   CHECK TO SEE IF WE OVERRODE RATING         
***      CLC   BOFULL1,BODMCB+4                                                 
***      BE    *+8                                                              
         OI    0(R3),X'80'         DEFINITELY AN OVERRIDE                       
*                                                                               
VALRTG50 LA    R3,L'RPRDVDMO(R3)   R3 = A(ELEMENT BEYOND THAT DEMO)             
         LR    R1,R3                                                            
         LA    R0,VALRTGD.RPRDVEL                                               
         SR    R1,R0                                                            
*                                                                               
         CLM   R1,1,VALRTGD.RPRDVLEN   IS LENGTH GREATER NOW?                   
         BNH   *+8                                                              
         STC   R1,VALRTGD.RPRDVLEN     YES, STORE IT AS SUCH                    
         DROP  VALRTGD                                                          
*                                                                               
VALRTGX  B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY SUPPRESS BOOKS FIELD                                                  
***********************************************************************         
DISSUPBK DS    0H                                                               
         NMOD1 0,**DSBK**                                                       
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
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
         USING RPRDTELD,R6                                                      
*                                                                               
         CLI   RPRDTBKS,0          ANY BOOKS SUPRRESSED FOR REPORTING?          
         BE    DISSBKX                                                          
*                                                                               
         LA    R3,FVIFLD                                                        
         LA    R4,SAVBKS                                                        
         LA    R5,SAVLBLS                                                       
         USING BOOKLIN,R4                                                       
*                                                                               
DISSBK10 CLI   0(R4),0             NO MORE BOOKS?                               
         BE    DISSBK50            THAT'S ALL                                   
*                                                                               
         LA    RE,X'80'            CALCULATE MASK USED FOR THIS BOOK            
         ZIC   R1,0(R4)                                                         
         SRL   RE,0(R1)                                                         
         STC   RE,BOBYTE1                                                       
*                                                                               
         NC    BOBYTE1,RPRDTBKS    IS THIS MASKED BIT ON?                       
         BZ    DISSBK40            NO, BOOK IS NOT SUPPRESSED                   
*                                                                               
         OC    0(L'SAVLBL,R5),0(R5)                                             
         BZ    DISSBK20                                                         
         MVC   0(L'SAVLBL,R3),0(R5)   SUPPRESSED BOOK IS A LABEL                
         LA    R3,L'SAVLBL(R3)                                                  
         B     DISSBK30                                                         
*                                                                               
DISSBK20 DS    0H                                                               
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
         BNE   DISBKS22                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     DISBKS22                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
DISBKS22 LA    RF,BOWORK1+8                                                     
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),BOWORK1+8                                                
         LA    R3,1(RE,R3)                                                      
*                                                                               
DISSBK30 CLI   0(R3),C' '          SHORTEN LINE AS MUCH AS POSSIBLE             
         BH    DISSBK35                                                         
         BCTR  R3,0                                                             
         B     DISSBK30                                                         
DISSBK35 LA    R3,1(R3)            AND PUT A COMMA TO SEPARATE                  
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
DISSBK40 LA    R4,L'SAVBK(R4)                                                   
         LA    R5,L'SAVLBL(R5)                                                  
         LA    RE,SAVBKS+L'SAVBKS                                               
         CR    R4,RE                                                            
         BL    DISSBK10                                                         
*                                                                               
DISSBK50 BCTR  R3,0                GET RID OF LAST COMMA                        
         MVI   0(R3),C' '                                                       
*                                                                               
DISSBKX  B     EXITOK                                                           
         DROP  R6,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUPPRESS BOOKS FIELD                                                 
***********************************************************************         
VALSUPBK DS    0H                                                               
         NMOD1 0,**VSBK**                                                       
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
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         USING RPRDTELD,R6                                                      
         MVI   RPRDTBKS,0                                                       
*                                                                               
         CLI   FVILEN,0            ANY SUPRESSED BOOKS?                         
         BE    VALSBKX             NONE                                         
*                                                                               
         L     RE,AIO4             CLEAR FOR PARSNIP                            
         LA    RF,480                                                           
         XCEFL                                                                  
*                                                                               
         GOTO1 VPARSNIP,BODMCB,(FVILEN,FVIFLD),(8,AIO4),('PSNAOKQ',0)           
         CLI   4(R1),1                                                          
         BL    EXITIBKE                                                         
         CLI   4(R1),7                                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(TOOMNYBK)   TOO MANY BOOKS                          
         B     EXITL                                                            
*                                                                               
         L     R3,AIO4             R3 = A(1ST PARSNIP FIELD)                    
         USING PSND,R3                                                          
*                                                                               
VALSBK10 CLI   PSNTAG,0            ANY MORE FIELDS?                             
         BE    VALSBK40            NO                                           
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
         BZ    VALSBK11            NO                                           
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
VALSBK11 GOTO1 VBOOKVAL,BODMCB,(C'N',BOWORK2),(1,BOWORK1),             X        
               (C'B',VSCANNER),BODUB1                                           
         CLI   4(R1),0             GOOD BOOK?                                   
         BNE   VALSBK20            YES                                          
***************                                                                 
* BOOK IS A USER-DEFINED BOOK                                                   
***************                                                                 
         CLI   PSNTAG,C'F'         REGUALR FIELD COMPONENT                      
         BNE   EXITIBKE                                                         
         CLI   PSNLEN,3            MAKE SURE USER DEFINED BTWN 3-5 CHR          
         BL    EXITIBKE                                                         
         CLI   PSNLEN,L'SAVLBL                                                  
         BH    EXITIBKE                                                         
*                                                                               
         LA    RE,SAVBKS           SAVE THE LABEL                               
         LA    R0,SAVBKS+L'SAVBKS                                               
         LA    RF,SAVLBLS                                                       
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         L     R5,PSNCOMP                                                       
*                                                                               
VALSBK12 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),0(R5)                                                    
         BE    VALSBK14                                                         
VALSBK13 LA    RF,L'SAVLBL(RF)                                                  
         LA    RE,L'SAVBK(RE)                                                   
         CR    RE,R0                                                            
         BL    VALSBK12                                                         
         B     EXITIBKE                                                         
*                                                                               
VALSBK14 CLI   PSNLEN,L'SAVLBL                                                  
         BNL   VALSBK24                                                         
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNE   VALSBK13                                                         
*                                                                               
         B     VALSBK24                                                         
***************                                                                 
* BOOK IS A REGULAR BOOK                                                        
***************                                                                 
VALSBK20 LA    RE,SAVBKS           BOOK SHOULD BE ONE OF OURS                   
         LA    RF,SAVBKS+L'SAVBKS                                               
         USING BOOKLIN,RE                                                       
VALSBK22 CLC   BKLNBK,BOWORK1                                                   
         BNE   *+24                                                             
         CLC   BKLNSPBK,BODUB1                                                  
         BNE   *+14                                                             
         OC    BKLNUPGD,BKLNUPGD                                                
         BZ    VALSBK24                                                         
         LA    RE,L'SAVBK(RE)                                                   
         CR    RE,RF                                                            
         BL    VALSBK22                                                         
         B     EXITIBKE                                                         
*                                                                               
VALSBK24 LA    R1,X'80'                                                         
         ZIC   RF,BKLNIORD         INTERNAL ORDER NUMBER OF BOOK                
         SRL   R1,0(RF)                                                         
         STC   R1,BOBYTE1          GOT CORRECT MASK FOR BOOK                    
         OC    RPRDTBKS,BOBYTE1    SUPPRESS THIS BOOK                           
         DROP  RE                                                               
*                                                                               
VALSBK30 DS    0H                                                               
         ZIC   RE,FVERRNDX         CURSOR KLUGE                                 
         ZIC   RF,PSNLEN                                                        
         LA    RE,1(RF,RE)                                                      
         STC   RE,FVERRNDX                                                      
*                                                                               
         LA    R3,PSNL(R3)         BUMP TO THE NEXT FIELD                       
         B     VALSBK10                                                         
         DROP  R3                                                               
*                                                                               
VALSBK40 MVI   FVERRNDX,0          NO MORE INDEX INTO FIELD NEEDED              
*                                                                               
VALSBKX  B     EXITOK                                                           
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN OBJECTS                                                        
***********************************************************************         
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECRD)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFKEY)                                  
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
KNOWTAB  DS    0XL(KNOWLQ)                                                      
* KEY PORTION                                                                   
         DC    AL2(00001),AL4(CONDTA)    CONTRACT                               
         DC    AL2(00002),AL4(PRODTA)    PROPOSAL                               
         DC    AL2(00008),AL4(STADTA)    STATION                                
         DC    AL2(00090),AL4(INVDTA)    INVENTORY NUMBER                       
         DC    AL2(00091),AL4(EDTDTA)    EFFECTIVE DATES                        
* KEY (PROTECTED PORTION)                                                       
         DC    AL2(00003),AL4(AGYDTA)    AGENCY                                 
         DC    AL2(00004),AL4(ADVDTA)    ADVERTISER                             
         DC    AL2(00005),AL4(PRDDTA)    PRODUCT                                
         DC    AL2(00006),AL4(SALDTA)    SALESPERSON                            
         DC    AL2(00007),AL4(BYRDTA)    BUYER                                  
         DC    AL2(00009),AL4(FLTDTA)    FLIGHT DATES                           
         DC    AL2(00010),AL4(DSKDTA)    DISK ADDRESS                           
         DC    AL2(00087),AL4(DVSDTA)    DEV SALESPERSON                        
         DC    AL2(00088),AL4(DVTDTA)    DEV TYPE                               
* RECORD (INPUT PORTION)                                                        
         DC    AL2(00099),AL4(DPTDTA)    DAYPART                                
         DC    AL2(00100),AL4(DTMDTA)    DAY/TIME                               
         DC    AL2(00101),AL4(SEQDTA)    SEQUENCE #                             
         DC    AL2(00076),AL4(PRGDTA)    SECONDARY PROGRAM                      
         DC    AL2(00075),AL4(SDTDTA)    SECONDARY DAY/TIME                     
         DC    AL2(00089),AL4(ADTDTA)    AVAIL DAY/TIME                         
         DC    AL2(00086),AL4(LENDTA)    LENGTH                                 
         DC    AL2(00074),AL4(CSTDTA)    COST                                   
         DC    AL2(00012),AL4(COMDTA)    COMMENT                                
         DC    AL2(00028),AL4(COMDTA)    COMMENT LINE #2                        
         DC    AL2(00078),AL4(PBKDTA)    PRIMARY BOOK                           
         DC    AL2(00081),AL4(PRJDTA)    PROJECTION                             
         DC    AL2(00146),AL4(HXBKDTA)   BOOK NAME                              
         DC    AL2(00147),AL4(XBKDTA)    BOOK PRINT FIELD                       
         DC    AL2(00079),AL4(HRTGDTA)   DEMO NAME                              
         DC    AL2(00080),AL4(RTGDTA)    DEMO RATING                            
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
SELPROFS DS    0CL10                CONTRACT PROFILES                           
         DS    CL1                 PROGRAM #                                    
         DS    CL1                 SPARE                                        
SELPROF  DS    CL8                 PROFILE BITS                                 
*                                                                               
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS, SET #1                  
MF1KYCHG EQU   X'80'                - KEY FIELD CHANGED                         
MF1PFRET EQU   X'40'                - RETURNING FROM CALLED SESSION             
MF1PFCOM EQU   X'20'                - COMING FROM A CALLER SESSION              
MF1TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
PRIMEBK  DS    X                   INTERNAL ORD # OF DISPLAYED BOOK             
PRIMEDM  DS    X                   INTERNAL ORD # OF DISPLAYED DEMO             
*                                                                               
PFTEXT   EQU   PFK01               PFKEY FOR TEXT                               
PFNEXT   EQU   PFK11               PFKEY FOR NEXT                               
PFRETURN EQU   PFK12               PFKEY FOR RETURN                             
*                                                                               
PCKOF06B DS    PL6                 PACKED OF 6  BYTES                           
PCKOF08B DS    PL8                 PACKED OF 8  BYTES                           
PCKOF16B DS    PL16                PACKED OF 16 BYTES                           
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE AREA                          
*                                                                               
SVKYINVT DS    CL4                 INVENTORY ID NUMBER                          
SVKYEFDT DS    XL6                 EFFECTIVE DATES (2 PWOS JULIAN)              
SVKYDYPT DS    CL1                 DAYPART BUILT BY NTRIN                       
SVKYDYTM DS    CL19                DAY/TIME BUILT BY NTRIN                      
SVKYSEQN DS    CL3                 SEQUENCE NUMBER BY NTRIN                     
*                                                                               
SVFLEFSD DS    XL2                 SAVED FLIGHT START DATE                      
SVFLEFED DS    XL2                 SAVED FLIGHT END DATE                        
*                                                                               
SAVSTAS  DS    0XL(NUMSTAS*STLNLENQ)                                            
SAVSTA   DS    (NUMSTAS)XL(STLNLENQ)                                            
*                                                                               
SAVBKS   DS    0XL(NUMBKS*(BKLNLENQ))                                           
SAVBK    DS    (NUMBKS)XL(BKLNLENQ)                                             
*                                                                               
SAVLBLS  DS    0CL(NUMBKS*5)       LABELS FOR USER DEFINED BKS (MINIO)          
SAVLBL   DS    (NUMBKS)CL5          - NULL: EMPTY OR LABEL                      
*                                                                               
SAVDMOS  DS    0XL(NUMDEMS*DMLNLENQ)                                            
SAVDMO   DS    (NUMDEMS)XL(DMLNLENQ)                                            
*                                                                               
SAVSLNS  DS    0CL(6*1)            SAVED 1-BYTE SPOT LENGTHS                    
SAVSLN   DS    6XL1                                                             
*                                                                               
SAVRATCS DS    0XL((NUMCSTS)*RFTCRTSL)    RATE CARD                             
SAVRATC  DS    (NUMCSTS)XL(RFTCRTSL)                                            
*                                                                               
SAVOPTNS DS    XL1                 OPTIONS                                      
OPTNTXTQ EQU   X'80'                - TEXT BIT                                  
OPTNDECQ EQU   X'40'                - DEMO DECIMAL PRECISION BIT                
*                                                                               
ONPRGLIN DS    XL1                 ON WHICH PROGRAM LINE                        
ONSDTLIN DS    XL1                 ON WHICH SECONDARY DAY/TIME LINE             
ONADTLIN DS    XL1                 ON WHICH AVAIL DAY/TIME LINE                 
ONDMORTG DS    XL1                 ON WHICH DEMO RATING                         
ONBOOKLN DS    XL1                 ON WHICH DEMO RATING                         
ONPRBKLN DS    XL1                 ON WHICH DEMO RATING                         
*                                                                               
SDTLIST  DS    0XL(7*5)            LIST OF DAY/TIME                             
SDTLST   DS    7XL5                                                             
*                                                                               
ADTLIST  DS    0XL(8*22)           LIST OF AVAIL DAY/TIME                       
ADTLST   DS    8XL22                                                            
*                                                                               
PRGLIST  DS    0XL(8*2)            LIST OF PROGRAM LINK #                       
PRGLST   DS    8XL2                                                             
*                                                                               
SVDMOELM DS    XL(L'BOELEM)        SAVED DEMO ELEMENT                           
*                                                                               
SVMINEKY DS    XL(L'RPROKMEL)      SAVED MINIO ELEMENT KEY                      
*                                                                               
SAVPRGNS DS    0CL(8*SAVPRGNQ)     SAVED PROGRAM NAMES                          
SAVPRGN  DS    8CL(SAVPRGNQ)                                                    
PRGCOUNT DS    X                   COUNT OF PROGRAM NAMES                       
*                                                                               
TPUBBSQ  EQU   X'48'               NSI TIME PERIOD SOURCE                       
TTUBBSQ  EQU   X'50'               NSI TYPICAL TIME PERIOD SOURCE               
PAVUBBSQ EQU   X'40'               NSI PAV SOURCE                               
*                                                                               
SAVXBKS  DS    0XL(NUMBKS*XBLNLENQ)   BOOK EXTENSION                            
SAVXBK   DS    (NUMBKS)XL(XBLNLENQ)                                             
*                                                                               
FTCHUPGD DS    XL(1+(7*(11+14)))       FETCH UPGRADE EXPR BLOCK                 
*                                                                               
FETCHBLK DS    XL(RFTBLKL)         FETCH BLOCK                                  
         SPACE 2                                                                
SAVPRGND DSECT                     SAVED PROGRAM NAME DSECT                     
SAVPRGLN DS    X                   LENGTH OF STRING                             
SAVPRGCD DS    XL2                 TEXT ELEMENT CODE                            
SAVPRGNQ EQU   *-SAVPRGND                                                       
         EJECT                                                                  
* REPROWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REPROWORK                                                      
         PRINT ON                                                               
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
APPLID   DS    XL6                                                              
***********************************                                             
* SAVE AREA EXCEPT BETWEEN NTRSES                                               
***********************************                                             
MINSTAS  DS    0XL(NUMSTAS*STLNLENQ)                                            
MINSTA   DS    (NUMSTAS)XL(STLNLENQ)                                            
*                                                                               
MINBKS   DS    0XL(NUMBKS*(BKLNLENQ))                                           
MINBK    DS    (NUMBKS)XL(BKLNLENQ)                                             
*                                                                               
MINLBLS  DS    0CL(NUMBKS*5)       LABELS FOR USER DEFINED BKS (MINIO)          
MINLBL   DS    (NUMBKS)CL5          - NULL: EMPTY OR LABEL                      
*                                                                               
MINDMOS  DS    0XL(NUMDEMS*DMLNLENQ)                                            
MINDMO   DS    (NUMDEMS)XL(DMLNLENQ)                                            
*                                                                               
MINDPTS  DS    0CL(NUMDPTS*(1+4))   - 1 BYTE DAYPART CODE                       
MINDPT   DS    (NUMDPTS)CL(1+4)     - 4 BYTE BYR CPP                            
*                                                                               
MINCOSTS DS    0XL(NUMCSTS*CSLNLENQ)                                            
MINCOST  DS    (NUMCSTS)XL(CSLNLENQ)                                            
*                                                                               
DPTINKEY DS    CL1                 DAYPART IN THE KEY SCREEN                    
DAYINKEY DS    XL1                 DAY BITS IN KEY SCREEN                       
TIMINKEY DS    XL4                 TIMES IN KEY SCREEN                          
SEQINKEY DS    XL1                 SEQUENCE NUMBER IN KEY SCREEN                
PTMINMKY DS    XL2                 PACKED TIME FOR MINIO ELEM KEY               
*                                                                               
SVHDEFDT DS    XL6                 EFFECTIVE DATES (2 PWOS JULIAN)              
*                                                                               
MINXBKS  DS    0XL(NUMBKS*XBLNLENQ)   BOOK EXTENSION                            
MINXBK   DS    (NUMBKS)XL(XBLNLENQ)                                             
*                                                                               
MINCSEL  DS    XL(RPRCSLNQ)                                                     
******                                                                          
         DS    XL(TWUSER+L'TWUSER-*)   # OF SPARE BEFORE TWSECBLK               
         DS    0X                                                               
         EJECT                                                                  
       ++INCLUDE REPROLN                                                        
         EJECT                                                                  
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
**PAN#1  DC    CL21'021REPRO33S  08/20/97'                                      
         END                                                                    
