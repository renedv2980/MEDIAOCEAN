*          DATA SET REPRO23    AT LEVEL 033 AS OF 07/10/00                      
*&&      SET   NOP=N                                                            
*PHASE T80A23A                                                                  
T80A23   TITLE 'REPRO23 - REP PROPOSALS WORKSHEET'                              
**********************************************************************          
*  HISTORY OF CHANGES                                                *          
*    JUN28/00 (BU ) REMOVE REFERENCES TO GLV1GOTO PER MEL HERTZIG    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
PRO23    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REPRO23*,R7,RR=RE                                              
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
         LR    RE,RA                                                            
         AH    RE,=Y(TWAD-SVVWFLG1)                                             
         MVC   VIEWFLG1,0(RE)                                                   
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
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                            
*                                                                               
* P1 HOLDS EQUATED VERB                                                         
***********************************************************************         
OBJECT   L     R1,SVPARMS                                                       
         LR    RF,RB                                                            
         AH    RF,=Y(TABLEOO-PRO23)                                             
         B     ITER                                                             
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                                
***********************************************************************         
INIT     DS    0H                                                               
         GOTO1 =A(INITPRG),RR=BORELO                                            
         DC    H'0'                                                             
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
         NI    MISCFLG1,X'FF'-MF1KYCHG-MF1VWCHG                                 
*                                                                               
         CLI   BCPFKEY,PFKRFCST    COST REFRESH?                                
         BNE   *+8                 NO                                           
         OI    MISCFLG1,MF1VWCHG                                                
*                                                                               
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
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    EXITOK                                                           
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
         GOTO1 =A(D1STDVAL),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
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
         DC    AL1(DDIS),AL1(0,0,0),AL4(DLDDIS)      DISPLAY                    
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER DISPLAYING THE DATA FIELDS                                              
***********************************************************************         
DLDDIS   DS    0H                                                               
         CLC   SVPARMS4,ATLST                                                   
         BE    EXITOK                                                           
*                                                                               
         TM    MISCFLG2,MF2ADPRO   PROPOSAL ADDED?                              
         BZ    *+18                                                             
         MVI   FVOMTYP,C'I'                                                     
         MVC   FVMSGNO,=AL2(93)                                                 
         B     EXITL                                                            
*                                                                               
         TM    MISCFLG1,MF1CSERR   COST CALCULATION ERROR?                      
         BZ    EXITOK              NO                                           
         MVC   FVMSGNO,=AL2(698)                                                
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* AFTER VALIDATING THE DATA FIELDS                                              
***********************************************************************         
DLDVAL   DS    0H                                                               
         GOTO1 =A(DLSTDVAL),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
************ DOING AN ACTION ON A SPECIFIC DATA OBJECT ****************         
***********************************************************************         
DATA10   DS    0H                                                               
         LR    RF,RB               TABLE OF KNOWN OBJECTS                       
         AH    RF,=Y(KNOWTAB-PRO23)                                             
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
         CLI   BCPFKEY,PFKSCRL                                                  
         BNE   VALCON0                                                          
         OI    MISCFLG2,MF2SCRL                                                 
         MVI   BCPFKEY,0                                                        
         NI    BCINDS1,FF-BCIANYPF                                              
*                                                                               
VALCON0  TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
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
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
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
         MVC   FVIFLD(L'STLNSTA),STLNSTA-STALIN(R1)                             
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
         LA    RE,SAVSTAS                                                       
         USING STALIN,RE                                                        
VALSTA10 CLC   STLNSTA,FVIFLD                                                   
         BE    VALSTA20                                                         
         LA    RE,L'SAVSTA(RE)                                                  
         LA    RF,SAVSTAS+L'SAVSTAS                                             
         CR    RE,RF                                                            
         BL    VALSTA10                                                         
*                                                                               
         LA    RE,SAVSTAS                                                       
         TM    MISCFLG1,MF1PFRET   ERROR, RETURNING FROM CALLED SESS?           
         BZ    EXITNV                     NO, ERROR                             
         MVC   FVIFLD(L'STLNSTA),STLNSTA  YES - DEFAULT TO PRIME STA            
*                                                                               
VALSTA20 LR    RF,RA                                                            
         AH    RF,=Y(SVSTATN-TWAD)                                              
         MVC   0(1,RF),STLNIORD      SAVE INTERNAL ORDER # OF STATION           
         DROP  RE                                                               
*                                                                               
VALSTAX  OI    FVIIND,FVIVAL                                                    
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
OPTTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALOPT)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
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
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
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
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
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
* DATA OBJECT FOR SORT                                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SRTDTA   DS    0H                                                               
         LA    RF,SRTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SRTTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALSRT)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE SORT FIELD                                                           
***********************************************************************         
VALSRT   DS    0H                                                               
         GOTO1 =A(VALSORT),BODMCB,(R9),RR=BORELO                                
         BL    EXITL                                                            
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
DPTTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALDPT)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE DAYPART FILTER FIELD                                                 
***********************************************************************         
VALDPT   DS    0H                                                               
         TM    FVIIND,FVIVAL                                                    
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1VWCHG   THE VIEW WAS CHANGED                         
*                                                                               
         XC    FLTDPTS,FLTDPTS     CLEAR OUT THE DPT FILTER                     
         MVI   NFLTDPTS,0          NUMBER OF FILTERS                            
*                                                                               
         CLI   FVILEN,0            ANY DAYPART FILTER?                          
         BE    VALDPTX             NO                                           
         CLI   FVILEN,L'FLTDPTS    TOO MANY                                     
         BH    EXITNV              YES                                          
*                                                                               
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLTDPTS(0),FVIFLD   COPY THE 1-BYTE DAYPART CODE                 
         MVC   NFLTDPTS,FVILEN     NUMBER OF FILTERS                            
*                                                                               
VALDPTX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR VIEW                                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
VEWDTA   DS    0H                                                               
         LA    RF,VEWTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
VEWTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALVEW)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE VIEW FIELD                                                           
***********************************************************************         
VALVEW   DS    0H                                                               
         GOTO1 =A(VALVIEWF),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
         B     EXITOK                                                           
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
         NI    FVATRB,FF-FVAPROT                                                
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   *+8                                                              
         OI    FVATRB,FVAPROT                                                   
*                                                                               
         GOTO1 =A(DISCOMNT),BODMCB,(R9),(RC),RR=BORELO                          
         BL    EXITL                                                            
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE COMMENT FIELD                                                        
***********************************************************************         
VALCOM   DS    0H                                                               
         NI    FVATRB,FF-FVAPROT                                                
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   *+8                                                              
         OI    FVATRB,FVAPROT                                                   
*                                                                               
         GOTO1 =A(VALCOMNT),BODMCB,(R9),(RC),RR=BORELO                          
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TOTAL RATING                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
TRTDTA   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTRTG-TWAD)                                             
         ZAP   PCKOF16B,0(L'GRNDTRTG,RE)                                        
         TM    FILTFLG1,FF1SHRS+FF1LVLS                                         
         BZ    *+10                                                             
         ZAP   PCKOF16B,GRNDTDMO-GRNDTRTG(L'GRNDTDMO,RE)                        
*                                                                               
         TM    SAVOPTNS,OPTNDECQ   DISPLAY DEMO PRECISION?                      
         BZ    TRTDTA10                                                         
         EDIT  (P16,PCKOF16B),(9,FVIFLD),1,ALIGN=LEFT,WRK=BOWORK1,     X        
               DUB=BODUB1,ZERO=NOBLANK                                          
         B     TRTDTAX                                                          
*                                                                               
TRTDTA10 SRP   PCKOF16B,64-1,5     ROUND THE # OFF INSTEAD                      
         EDIT  (P16,PCKOF16B),(9,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,       X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
TRTDTAX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TOTAL COST                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
TCSDTA   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCST-TWAD)                                             
         ZAP   PCKOF16B,0(L'GRNDTCST,RE)                                        
*                                                                               
         EDIT  (P16,PCKOF16B),(17,FVIFLD),2,ALIGN=LEFT,WRK=BOWORK1,    X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
TCSDTAX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TOTAL NEED RATE                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
TNRDTA   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCPP-TWAD)                                             
         ZAP   PCKOF16B,0(L'GRNDTCPP,RE)                                        
         ZAP   PCKOF08B,GRNDTDTL-GRNDTCPP(L'GRNDTDTL,RE)                        
*                                                                               
         BAS   RE,DIVPACKD         AVERAGE BUYER'S CPP                          
         BE    TNRDTA10                                                         
         MVI   FVIFLD,C'*'                                                      
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
         B     TNRDTAX                                                          
*                                                                               
TNRDTA10 SRP   PCKOF16B,64-1,5                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTRTG-TWAD)                                             
         ZAP   PCKOF08B,0(L'GRNDTRTG,RE)                                        
*                                                                               
         MP    PCKOF16B,PCKOF08B                                                
         SRP   PCKOF16B,64-1,5                                                  
*                                                                               
         EDIT  (P16,PCKOF16B),(11,FVIFLD),2,ALIGN=LEFT,WRK=BOWORK1,    X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
TNRDTAX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TOTAL CPP                                                     
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
TCPDTA   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCST-TWAD)                                             
         ZAP   PCKOF16B,0(L'GRNDTCST,RE)                                        
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTRTG-TWAD)                                             
         ZAP   PCKOF08B,0(L'GRNDTRTG,RE)                                        
*                                                                               
         BAS   RE,DIVPACKD         CPP = COST / RATING                          
         BE    TCPDTA10            NOT DIVIDING BY 0                            
         MVI   FVIFLD,C'*'         DIV BY 0, '****' OUT THE CPP                 
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
         B     TCPDTAX                                                          
*                                                                               
TCPDTA10 EDIT  (P16,PCKOF16B),(9,FVIFLD),2,ALIGN=LEFT,WRK=BOWORK1,     X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
TCPDTAX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TOTAL BUYER CPP                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
TBCDTA   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCPP-TWAD)                                             
         ZAP   PCKOF16B,0(L'GRNDTCPP,RE)                                        
         ZAP   PCKOF08B,GRNDTDTL-GRNDTCPP(L'GRNDTDTL,RE)                        
*                                                                               
         BAS   RE,DIVPACKD         AVERAGE BUYER'S CPP                          
         BE    TBCDTA10                                                         
         MVI   FVIFLD,C'*'                                                      
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
         B     TBCDTAX                                                          
*                                                                               
TBCDTA10 SRP   PCKOF16B,64-1,5                                                  
         EDIT  (P16,PCKOF16B),(9,FVIFLD),2,ALIGN=LEFT,WRK=BOWORK1,     X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
TBCDTAX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TOTAL SPOTS                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
TSPDTA   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTSPT-TWAD)                                             
         ZAP   PCKOF06B,0(L'GRNDTSPT,RE)                                        
*                                                                               
         EDIT  (P6,PCKOF06B),(6,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,        X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
TSPDTAX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST STATION FIELD                                            
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
* DISPLAY STATION FIELD                                                         
***********************************************************************         
DISLSTA  DS    0H                                                               
         USING TLSTD,R2                                                         
         CLC   LSROWREP,=H'1'                                                   
         BNE   DISLSTAX                                                         
*                                                                               
         ZIC   R1,TLSDTSTA                                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R1,SAVSTAS(R1)                                                   
         MVC   FVIFLD(L'STLNSTA),STLNSTA-STALIN(R1)                             
         OI    FVATRB,FVAPROT                                                   
*                                                                               
DISLSTAX B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DAY/TIME FIELD                                                
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LDTMDTA  DS    0H                                                               
         LA    RF,LDTMTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LDTMTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLDTM)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLDTM)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DAY/TIME FIELD                                                        
***********************************************************************         
DISLDTM  DS    0H                                                               
         GOTO1 =A(DISLDYTM),RR=BORELO                                           
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE DAY/TIME FIELD                                                       
***********************************************************************         
VALLDTM  DS    0H                                                               
         GOTO1 =A(VALDYTIM),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PROGRAM FIELD                                                 
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LPRGDTA  DS    0H                                                               
         LA    RF,LPRGTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LPRGTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLPRG)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLPRG)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY PROGRAM FIELD                                                         
***********************************************************************         
DISLPRG  DS    0H                                                               
         NI    FVATRB,FF-FVAPROT                                                
         CLC   LSROWREP,=H'1'                                                   
         BE    *+12                                                             
         OI    FVATRB,FVAPROT                                                   
         B     DSLPRGX                                                          
*                                                                               
         USING TLSTD,R2                                                         
         OC    TLRDTPRG,TLRDTPRG   ANY PROGRAM TEXT NUMBER?                     
         BZ    DSLPRGX             NONE, NOTHING TO DISPLAY                     
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRTXELQ    TEXT ELEMENT                                 
         MVI   MINEKEY+1,RPRTXPRQ  PROGRAM TEXT                                 
         MVC   MINEKEY+RPRTXSEQ-RPRTXELD-1(L'TLRDTPRG),TLRDTPRG                 
         BAS   RE,MINIOHI                                                       
         BE    DSLPRG20                                                         
DSLPRG10 MVI   FVIFLD,C'*'         PROGRAM DOES NOT EXIST, PUT STARS            
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
         B     DSLPRGX                                                          
*                                                                               
DSLPRG20 L     R6,MINELEM                                                       
         USING RPRTXELD,R6                                                      
         CLI   RPRTXEL,RPRTXELQ    DID WE GET A TEXT ELEMENT?                   
         BNE   DSLPRG10                                                         
         CLI   RPRTXTYP,RPRTXPRQ      THAT IS PROGRAM TEXT?                     
         BNE   DSLPRG10                                                         
         CLC   TLRDTPRG,RPRTXSEQ      AND SEQ NUMBER IS THE SAME?               
         BNE   DSLPRG10                                                         
         ZIC   R1,RPRTXLEN                                                      
         SH    R1,=Y(RPRTXOVQ+1)                                                
         BM    DSLPRG10                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),RPRTXTXT  SHOW THIS PROGRAM TEXT                       
*                                                                               
DSLPRGX  OI    FVIIND,FVIVAL       MARK FIELD AS VALIDATED                      
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVSTATN-TWAD)                                              
         CLI   0(RE),0                                                          
         BNE   EXITOK                                                           
         OI    FVATRB,FVAPROT                                                   
         B     EXITOK                                                           
         DROP  R2,R5,R6                                                         
***********************************************************************         
* VALIDATE PROGRAM FIELD                                                        
***********************************************************************         
VALLPRG  DS    0H                                                               
         USING TLSTD,R2                                                         
         CLI   FVILEN,0            REQUIRES A PROGRAM                           
         BE    EXITNO                                                           
*                                                                               
         TM    FVIIND,FVIVAL       WAS THIS FIELD CHANGED?                      
         BNZ   VALLPRGX            NO                                           
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRTXELQ    TEXT ELEMENT                                 
         MVI   MINEKEY+1,RPRTXPRQ  PROGRAM TEXT                                 
         L     R6,MINELEM                                                       
         USING RPRTXELD,R6                                                      
         ZIC   R1,FVXLEN           L(PROGRAM) - 1                               
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   VLLPRG50                                                         
*                                                                               
VLLPRG10 CLI   RPRTXEL,RPRTXELQ    DID WE GET A TEXT ELEMENT?                   
         BNE   VLLPRG50                                                         
         CLI   RPRTXTYP,RPRTXPRQ      THAT IS PROGRAM TEXT?                     
         BNE   VLLPRG50                                                         
*                                                                               
         MVC   BOHALF1,RPRTXSEQ    SAVE THE SEQUENCE NUMBER AROUND              
         ZIC   RE,RPRTXLEN                                                      
         SH    RE,=Y(RPRTXOVQ+1)                                                
         BM    VLLPRG40                                                         
         CR    R1,RE               SAME LENGTH?                                 
         BNE   VLLPRG40            NO, CHECK NEXT ONE                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),RPRTXTXT  THIS PROGRAM TEXT EXISTS ALREADY?            
         BNE   VLLPRG40            NO, CHECK NEXT PROGRAM TEXT ELEM             
         B     VLLPRG60                                                         
*                                                                               
VLLPRG40 BAS   RE,MINIOSEQ                                                      
         BE    VLLPRG10                                                         
***************                                                                 
* PROGRAM TEXT DOES NOT EXIST, NEED TO ADD IT                                   
***************                                                                 
VLLPRG50 XC    RPRTXEL(L'BOELEM),RPRTXEL                                        
         MVI   RPRTXEL,RPRTXELQ                                                 
         MVI   RPRTXTYP,RPRTXPRQ                                                
         LH    RE,BOHALF1                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,RPRTXSEQ                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RPRTXTXT(0),FVIFLD                                               
         AH    R1,=Y(RPRTXOVQ+1)                                                
         STC   R1,RPRTXLEN                                                      
*                                                                               
         BAS   RE,MINIOADD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VLLPRG60 DS    0H                                                               
         MVC   TLRDTPRG,RPRTXSEQ   UPDATE TSAR RECORD                           
         CLI   WHATVIEW,VWMCOST    MCOST?                                       
         BNE   VALLPRGX            NO - NO NEED TO UPDATE CLUSTER               
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,(R2)   GET CLUSTER FOR THE TSAR RECORD           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         MVC   RPRDTPRG,TLRDTPRG                                                
*                                                                               
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALLPRGX OI    FVIIND,FVIVAL       MARK FIELD AS VALIDATED                      
         B     EXITOK                                                           
         DROP  R2,R5,R6                                                         
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLDPT)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DAYPART FIELD                                                         
***********************************************************************         
DISLDPT  DS    0H                                                               
         GOTO1 =A(DISLDYPT),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE DAYPART FIELD                                                        
***********************************************************************         
VALLDPT  DS    0H                                                               
         GOTO1 =A(VALLDYPT),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLCST)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY COST FIELD                                                            
***********************************************************************         
DISLCST  DS    0H                                                               
         USING TLSTD,R2                                                         
         CLC   LSROWREP,=H'1'      LINKED TSAR LINE 1?                          
         BE    *+12                                                             
         OI    FVATRB,FVAPROT                                                   
         B     DSLCSTX                                                          
*                                                                               
         TM    TLRDTNCU,X'80'      N/A?                                         
         BZ    *+14                NO                                           
         MVC   FVIFLD(2),=C'NA'                                                 
         B     DSLCSTX                                                          
*                                                                               
         EDIT  (B4,TLRDTNCU),(17,FVIFLD),2,DUB=BODUB1,ALIGN=LEFT,      X        
               WRK=BOWORK1,ZERO=NOBLANK                                         
*                                                                               
DSLCSTX  B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* VALIDATE COST FIELD                                                           
***********************************************************************         
VALLCST  DS    0H                                                               
         USING TLSTD,R2                                                         
         CLC   =C'NA',FVIFLD                                                    
         BNE   *+14                                                             
         MVC   TLRDTNCU,=X'80000000'                                            
         B     VALLCST0                                                         
*                                                                               
         ZIC   R0,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,FVIFLD,(R0)                                      
         CLI   0(R1),X'FF'                                                      
         BE    EXITNOTN            NOT NUMERIC                                  
         MVC   TLRDTNCU,BODMCB+4   <=== GRNDTCST UPDATED IN UPDREC              
*                                                                               
VALLCST0 DS    0H                                                               
         OI    TLRCFLG1,TCF1COST   THE COST WAS CHANGED                         
         TM    WHATSORT+SRTCPPB,SRTCPPA                                         
         BNO   *+8                                                              
         OI    TLRCFLG1,TCF1KCHG   YES, TSAR KEY DEFINITELY CHANGED             
*                                                                               
         TM    WHATSORT+SRTCSTB,SRTCSTA                                         
         BNO   *+8                                                              
         OI    TLRCFLG1,TCF1KCHG   YES, TSAR KEY DEFINITELY CHANGED             
*                                                                               
         CLI   WHICHCST,0                                                       
         BE    VALLCSTX            COST 1 HANDLED IN DLDVAL                     
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,(R2)   GET CLUSTER FOR THE TSAR RECORD           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,INTOAIO5         SO WE CAN USE RECUP LATER                    
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         XC    BOELEM,BOELEM                                                    
*                                                                               
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
VALCST2  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    VALCST6             YES, NO SUPLEMENTAL COST ELEMENT             
*                                                                               
         CLI   0(R6),RPRCSELQ      SUPLEMENTAL COST ELEMENT?                    
         BL    VALCST2             NO - NOT THERE YET                           
         BH    VALCST6             NO - PAST IT                                 
*                                                                               
         ZIC   R1,1(R6)            COPY AND DELETE                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)                                                  
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
VALCST6  MVI   BOELEM,RPRCSELQ                                                  
         MVI   BOELEM+1,RPRCSLNQ                                                
*                                                                               
         ZIC   R1,WHICHCST         GET SUPLEMENTAL COST                         
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRCSSC2+L'RPRCSNC2)                                     
         LA    R1,BOELEM+(RPRCSSC2-RPRCSELD)(R1)                                
         MVC   0(L'RPRCSSC2,R1),TLRDTSCU                                        
         MVC   L'RPRCSSC2(L'RPRCSNC2,R1),TLRDTNCU                               
*                                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
         BAS   RE,FROMAIO5                                                      
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALLCSTX B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MULTI-COST FIELD                                              
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LMCSDTA  DS    0H                                                               
         LA    RF,LMCSTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LMCSTBL  DC    AL1(DHED),AL1(0,0,0),AL4(HEDLMCS)                                
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISLMCS)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLMCS)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY MULTI-COST FIELDHEADLINE FOR THE COLUMN                               
***********************************************************************         
HEDLMCS  DS    0H                                                               
         GOTO1 =A(MCSTHDNG),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
***********************************************************************         
* DISPLAY MULTI-COST FIELD                                                      
***********************************************************************         
DISLMCS  DS    0H                                                               
         GOTO1 =A(DISMCOST),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE MULTI-COST FIELD                                                     
***********************************************************************         
VALLMCS  DS    0H                                                               
         GOTO1 =A(VALMCOST),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
         B     EXITOK                                                           
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLRTG)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEMO FIELD HEADLINE FOR THE COLUMN                                    
***********************************************************************         
HEDLRTG  DS    0H                                                               
         GOTO1 =A(RTNGHDNG),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
***********************************************************************         
* DISPLAY RATING FIELD                                                          
***********************************************************************         
DISLRTG  DS    0H                                                               
         GOTO1 =A(DISRATNG),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE RATING FIELD                                                         
***********************************************************************         
VALLRTG  DS    0H                                                               
         GOTO1 =A(VALRATNG),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLCPP)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY CPP FIELD                                                             
***********************************************************************         
DISLCPP  DS    0H                                                               
         GOTO1 =A(DISCSTPP),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE CPP FIELD                                                            
***********************************************************************         
VALLCPP  DS    0H                                                               
         GOTO1 =A(VALCSTPP),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BUYER CPP FIELD                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LBCPDTA  DS    0H                                                               
         LA    RF,LBCPTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LBCPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLBCP)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLBCP)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY BUYER CPP FIELD                                                       
***********************************************************************         
DISLBCP  DS    0H                                                               
         USING TLSTD,R2                                                         
         EDIT  (B4,TLRDTTAB),(7,FVIFLD),2,ALIGN=LEFT,DUB=BODUB1,       X        
               ZERO=NOBLANK,WRK=BOWORK1                                         
DSLBCPX  B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* VALIDATE BUYER CPP FIELD                                                      
***********************************************************************         
VALLBCP  DS    0H                                                               
         USING TLSTD,R2                                                         
         OI    TLRCFLG1,TCF1BCPP   THE BUYER'S CPP WAS CHANGED                  
*                                                                               
         ZIC   R0,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,FVIFLD,(R0)                                      
         CLI   0(R1),X'FF'                                                      
         BE    EXITNOTN            NOT NUMERIC                                  
         MVC   TLRDTTAB,BODMCB+4                                                
*                                                                               
VALLBCPX B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR NEED RATE FIELD                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LNRTDTA  DS    0H                                                               
         LA    RF,LNRTTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LNRTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLNRT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLNRT)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY NEED RATE FIELD                                                       
***********************************************************************         
DISLNRT  DS    0H                                                               
         USING TLSTD,R2                                                         
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         ICM   R0,15,PREVRTG       RATING                                       
         CVD   R0,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         ICM   R0,15,TLRDTTAB      BUYER'S CPP                                  
         CVD   R0,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF06B                                                
         SRP   PCKOF16B,64-1,0                                                  
         EDIT  (P16,PCKOF16B),(8,FVIFLD),2,ALIGN=LEFT,WRK=BOWORK1,     X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
DSLNRTX  B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* VALIDATE NEED RATE FIELD                                                      
***********************************************************************         
VALLNRT  DS    0H                                                               
         GOTO1 =A(VALNEDRT),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
         B     EXITOK                                                           
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLSPT)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY SPOTS FIELD                                                           
***********************************************************************         
DISLSPT  DS    0H                                                               
         USING TLSTD,R2                                                         
         EDIT  (B2,TLRDTTSP),(4,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,        X        
               DUB=BODUB1,ZERO=NOBLANK                                          
DSLSPTX  B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* VALIDATE SPOTS FIELD                                                          
***********************************************************************         
VALLSPT  DS    0H                                                               
         USING TLSTD,R2                                                         
         TM    FVIIND,FVINUM       IS THIS A VALID NUMERIC FIELD?               
         BZ    EXITNV              NO                                           
         L     R1,BCFULL                                                        
         STCM  R1,3,TLRDTTSP                                                    
*                                                                               
VALLSPTX B     EXITOK                                                           
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
         AH    RF,=Y(LISTABL-PRO23)                                             
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
* SET KEY COLUMNS                                                               
***********************************************************************         
KEYCLM1  DS    0H                                                               
         GOTO1 =A(DEFCLMNS),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISE FOR LIST                                                           
***********************************************************************         
INITL1   DS    0H                                                               
         OI    LSSTAT1,LSSSEL+LSSTSAR   SELECTABLE LIST OF TSAR RECS            
         OI    LSSTAT2,LSSIUPD          WE WANT TO DO OUR OWN UPDATES           
         MVI   LSSUBLEN,2               LENGTH OF SUB-ACTION FIELD              
         NI    LSSTAT2,X'FF'-LSSADD   NOT VALID TO ADD NEW LIST LINES           
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
         LR    RE,RA               YES, CLEAR OUT THE GRAND TOTALS              
         AH    RE,=Y(GRNDTCST-TWAD)                                             
         ZAP   0(L'GRNDTCST,RE),=P'0'                                           
         ZAP   GRNDTRTG-GRNDTCST(L'GRNDTRTG,RE),=P'0'                           
         ZAP   GRNDTDMO-GRNDTCST(L'GRNDTDMO,RE),=P'0'                           
         ZAP   GRNDTSPT-GRNDTCST(L'GRNDTSPT,RE),=P'0'                           
         ZAP   GRNDTCPP-GRNDTCST(L'GRNDTCPP,RE),=P'0'                           
         ZAP   GRNDTDTL-GRNDTCST(L'GRNDTDTL,RE),=P'0'                           
         NI    MISCFLG1,FF-MF1CSERR                                             
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'SVMINEKY),SVMINEKY                                     
         BAS   RE,MINIOHI                                                       
         BNE   EXITL               MINIO HIGH IS UNHAPPY                        
         B     NML20                                                            
         DROP  R5                                                               
***********************************************************************         
* NEXT FOR LIST                                                                 
***********************************************************************         
NLST1    DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'SVMINEKY),SVMINEKY                                     
         BAS   RE,MINIOHI                                                       
         BNE   NMLXNO              MINIO HIGH IS UNHAPPY                        
*                                                                               
NML10    BAS   RE,MINIOSEQ                                                      
         BNE   NMLXNO                                                           
*                                                                               
NML20    L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         CLI   0(R6),RPRDTELQ      GOT A DETAIL DESCRIPTION ELEM?               
         BNE   NML10               NO                                           
*                                                                               
         CLI   BCPFKEY,PFKRFCST    REFRESH COST?                                
         BNE   *+8                 NO                                           
         BAS   RE,REFCOST                                                       
*                                                                               
         TM    FILTFLG1,FF1HIDN    DISPLAY HIDDEN LINES?                        
         BZ    NML21               NO                                           
         TM    RPRDTFL1,RPRDTF1H   HIDDEN?                                      
         BZ    NML10               NO - NEXT DETAIL                             
         B     NML22                                                            
*                                                                               
NML21    TM    RPRDTFL1,RPRDTF1H   HIDDEN?                                      
         BNZ   NML10               YES - NEXT DETAIL                            
         B     NML22                                                            
*                                                                               
NML22    LR    RE,RA               DO WE HAVE A STATION FILTER?                 
         AH    RE,=Y(SVSTATN-TWAD)                                              
         CLI   0(RE),0                                                          
         BE    *+14                                                             
         CLC   RPRDTSTA,0(RE)      YES, MATCH?                                  
         BNE   NML10                    NO, CHECK NEXT DETAIL                   
*                                                                               
         CLI   NFLTDPTS,0          DO WE HAVE A DAYPART FILTER?                 
         BE    NML24               NO                                           
         ZIC   R0,NFLTDPTS                                                      
         LA    RE,FLTDPTS                                                       
NML23    CLC   RPRDTDPT,0(RE)      MATCH ON FILTER?                             
         BE    NML24               YES                                          
         LA    RE,1(RE)                                                         
         BCT   R0,NML23                                                         
         B     NML10               NO                                           
*                                                                               
NML24    TM    FILTFLG1,FF1SPOTS   FILTER FOR THOSE WITH SPOTS?                 
         BZ    *+14                NO                                           
         OC    RPRDTTSP,RPRDTTSP   YES, ANY SPOTS FOR THIS DETAIL?              
         BZ    NML10                    NONE                                    
*                                                                               
NML30    MVI   SVMINEKY,RPRDTELQ                                                
         MVC   SVMINEKY+1(L'SVMINEKY-1),RPRDTSTA                                
NMLX     B     EXITOK                                                           
         DROP  R5,R6                                                            
NMLXNO   DS    0H                                                               
         CLI   BCPFKEY,PFKRFCST    REFRESH COST?                                
         BNE   *+8                 NO                                           
         BAS   RE,MINIOCLS                                                      
         B     EXITL                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE COSTS FOR EACH LINE                                                    
***********************************************************************         
REFCOST  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         BAS   RE,INTOAIO5                                                      
*                                                                               
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         USING RPRDTELD,R6                                                      
         MVC   BOFULL1,RPRDTNC1                                                 
         OC    BOFULL1,BOFULL1                                                  
         BZ    RFCSA50             NO COST SKIP LINE                            
         TM    BOFULL1,X'80'       N/A?                                         
         BNZ   RFCSA50             YES - SKIP LINE                              
         DROP  R6                                                               
*                                                                               
         XC    BOELEM,BOELEM                                                    
RFCSA10  CLI   0(R6),0                                                          
         BE    RFCSA14                                                          
         CLI   0(R6),RPRCSELQ                                                   
         BE    RFCSA12                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RFCSA10                                                          
*                                                                               
RFCSA12  DS    0H                  COPY AND DELETE ELEMENT                      
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)                                                  
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
RFCSA14  DS    0H                                                               
         MVI   BOELEM,RPRCSELQ                                                  
         MVI   BOELEM+1,RPRCSLNQ                                                
*                                                                               
         LA    R0,NUMCSTS-1                                                     
         LA    R4,BOELEM+(RPRCSNC2-RPRCSELD)                                    
         LR    RE,RA                                                            
         AH    RE,=Y(MINCOSTS-TWAD)                                             
         LA    RE,L'MINCOST(RE)                                                 
         USING CSTLIN,RE                                                        
*                                                                               
RFCSA16  DS    0H                                                               
         CLI   CSLNPBC,0           % OF BASE COST?                              
         BE    RFCSA18             NO                                           
         TM    0(R4),X'80'         N/A?                                         
         BNZ   RFCSA18             YES                                          
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
         CP    BODUB1,=P'2000000000'                                            
         BNH   *+14                                                             
         OI    MISCFLG1,MF1CSERR                                                
         ZAP   BODUB1,=P'0'                                                     
*                                                                               
         SRP   BODUB1,64-2,5                                                    
         CVB   R1,BODUB1                                                        
         MH    R1,=H'100'          MAKE IT DOLLARS                              
         ST    R1,0(R4)                                                         
*                                                                               
RFCSA18  DS    0H                                                               
         LA    RE,L'MINCOST(RE)                                                 
         LA    R4,RPRCSNC3-RPRCSNC2(R4)                                         
         BCT   R0,RFCSA16                                                       
         DROP  RE                                                               
*                                                                               
         L     R6,AIO5             WHERE DOES IT GO?                            
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
*                                                                               
RFCSA20  DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    RFCSA22                                                          
         CLI   0(R6),RPRCSELQ                                                   
         BNL   RFCSA22                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RFCSA20                                                          
*                                                                               
RFCSA22  DS    0H                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
         BAS   RE,FROMAIO5                                                      
         BAS   RE,MINIOWRT                                                      
*                                                                               
         XC    MINEKEY,MINEKEY     SOMETHING SCREWY HERE                        
         MVI   MINEKEY,RPRDTELQ                                                 
         L     R6,MINELEM                                                       
         MVC   MINEKEY+1(7),2(R6)                                               
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RFCSA50  DS    0H                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
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
         GOTO1 =A(TSARFILE),BODMCB,(R9),RR=BORELO                               
         B     EXITOK                                                           
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
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         OC    TLNUM,TLNUM         NO RECORD HERE?                              
         BZ    UPDRECX             NONE, NOTHING TO UPDATE                      
         MVC   TLROLRTG,TLRNWRTG                                                
         GOTOX ('TSARIO',AGROUTS),TSAPUT  NO, NEED TO WRT TSAR RECORD           
UPDRECX  B     EXITOK                                                           
         DROP  R2                                                               
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
         XC    BOCURSOR,BOCURSOR                                                
*                                                                               
         OI    SNINDS1,SNIPARMS    SO WE CAN GET DNTR                           
         MVC   SDATA(6),=C'WORUPD'                                              
*                                                                               
         CLI   BCPFKEY,PFPROPSL    GOING TO PRO/CHA?                            
         BE    NTROUTX             YES, DON'T NEED ANYMORE DATA                 
*                                                                               
         L     R4,ATLST                                                         
         USING TLSTD,R4                                                         
         MVC   SDATA+6(L'RPROKMEL),TLRDTELC                                     
         MVC   SDATA+6+L'RPROKMEL(L'RPRDTSTM*2),TLRDTSTM                        
         MVC   SDATA+6+L'RPROKMEL+L'RPRDTSTM*2(L'PRIMEBK*2),PRIMEBK             
         DROP  R4                                                               
NTROUTX  B     EXITOK                                                           
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
         GOTO1 =A(NTRXITIN),BODMCB,(R9),RR=BORELO                               
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
         CLI   CSACT,A#UPDATE                                                   
         BNE   SETKSCRX                                                         
         MVI   GSSKCODE,C'X'       UPDATE KEY SCREEN                            
*                                                                               
SETKSCRX B     EXITOK                                                           
***********************************************************************         
* SET THE MAINT SCREEN CODE                                                     
***********************************************************************         
SETMSCR  DS    0H                                                               
         TM    BCINDS1,BCINACT     NEW ACTION?                                  
         BNZ   SETMSCR0            YES, WE NEED TO DO THE SCREEN THEN           
         TM    MISCFLG1,MF1KYCHG   DID THE KEY CHANGE?                          
         BNZ   SETMSCR0            YES, WE NEED TO DO THE SCREEN THEN           
         TM    MISCFLG1,MF1VWCHG   DO WE NEED TO CHANGE VIEWS?                  
         BZ    SETMSCRX            NO                                           
*                                                                               
SETMSCR0 CLC   WHATVIEW,GSSMCODE   YES, IS IT THE SAME VIEW?                    
         BNE   STMSCR80            NO                                           
*                                                                               
         CLI   GSSMCODE,VWAVAIL                                                 
         BNE   *+12                                                             
         MVI   GSSMCODE,VWAVAIL2                                                
         B     STMSCR90                                                         
*                                                                               
         CLI   GSSMCODE,VWMCOST                                                 
         BNE   *+12                                                             
         MVI   GSSMCODE,VWMCOST2                                                
         B     STMSCR90                                                         
*                                                                               
         CLI   GSSMCODE,VWMBOOK                                                 
         BNE   *+12                                                             
         MVI   GSSMCODE,VWMBOOK2                                                
         B     STMSCR90                                                         
*                                                                               
         CLI   GSSMCODE,VWMDEMO                                                 
         BNE   *+12                                                             
         MVI   GSSMCODE,VWMDEMO2                                                
         B     STMSCR90                                                         
*                                                                               
         CLI   GSSMCODE,VWPACKGE                                                
         BNE   *+12                                                             
         MVI   GSSMCODE,VWPACKG2                                                
         B     STMSCR90                                                         
*                                                                               
STMSCR80 MVC   GSSMCODE,WHATVIEW                                                
STMSCR90 MVI   GSSMPAGE,1                                                       
         CLI   WHATVIEW,VWPACKGE                                                
         BNE   STMSCR99                                                         
         CLI   NFLTDPTS,0                                                       
         BE    STMSCR99                                                         
         MVI   GSSMPAGE,2                                                       
STMSCR99 XC    GCLASKEY,GCLASKEY   SO IT DOESN'T GO TO RVAL IN GEFIL02          
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
* SUB-ACTION OBJECT                                                             
* -----------------                                                             
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
SUBACT   DS    0H                                                               
         GOTO1 =A(SUBACTN),BODMCB,(R9),RR=BORELO                                
         BL    EXITL                                                            
         BH    EXITH                                                            
         B     EXITOK                                                           
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
         CLI   0(RE),PFPROPSL      PROPOSAL?                                    
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Pro'                                              
         B     RECPFKX                                                          
*                                                                               
         CLI   0(RE),PFPENDNG      PENDING                                      
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Pend'                                             
         B     RECPFKX                                                          
*                                                                               
         CLI   0(RE),PFCSTLST      COST/LIST                                    
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Cost'                                             
         B     RECPFKX                                                          
*                                                                               
         CLI   0(RE),PFWOKADD      WORK/ADD                                     
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Work'                                             
         B     RECPFKX                                                          
*                                                                               
         CLI   0(RE),PFDOWNLD      PRO/DOWNLOAD                                 
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Pro'                                              
         B     RECPFKX                                                          
*                                                                               
         B     NOTPFK              EVERYTHING ELSE - NO RECORD                  
*                                                                               
RECPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE ACTION FOR THE PFKEY                                              
***********************************************************************         
ACTPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),PFPRINT       PRINT?                                       
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Rpt'                                              
         B     ACTPFKX                                                          
*                                                                               
         CLI   0(RE),PFPROPSL      PROPOSAL?                                    
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Cha'                                              
         B     ACTPFKX                                                          
*                                                                               
         CLI   0(RE),PFPENDNG      PENDING?                                     
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Cha'                                              
         B     ACTPFKX                                                          
*                                                                               
         CLI   0(RE),PFCSTLST      COST/LIST?                                   
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'List'                                             
         B     ACTPFKX                                                          
*                                                                               
         CLI   0(RE),PFWOKADD      WORK?ADD?                                    
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Add'                                              
         B     ACTPFKX                                                          
*                                                                               
         CLI   0(RE),PFDOWNLD      PRO/DOWNLOAD                                 
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Download'                                         
         B     RECPFKX                                                          
*                                                                               
         B     NOTPFK              NO ACTION FOR EVERYTHING ELSE                
*                                                                               
ACTPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE USER DEFINED NAME FOR THE PFKEY                                   
***********************************************************************         
USRPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),PFAVAIL       AVAIL?                                       
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Ava'                                              
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFPACKGE      PACKAGE?                                     
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Pack'                                             
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFMBOOK       MBOOK?                                       
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Mbk'                                              
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFMDEMO       MDEMO?                                       
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Mdem'                                             
         B     USRPFKX                                                          
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
         CLI   0(RE),13            MORE?                                        
         BNE   *+8                 NO                                           
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFKYRIS       RIS?                                         
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'RIS'                                              
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFMCOST       MCOST?                                       
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Mcst'                                             
         B     USRPFKX                                                          
*                                                                               
         CLI   0(RE),PFKRFCST      REFRESH COST?                                
         BNE   *+14                NO                                           
         MVC   FVIFLD(8),=CL8'Refresh'                                          
         B     USRPFKX                                                          
*                                                                               
         B     NOTPFK              NO ACTION FOR EVERYTHING ELSE                
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
* THIS ROUTINE READS IN THE MINIO CLUSTER INTO MINELEM OF WHICH THE             
* TSAR RECORD IS BUILT FROM                                                     
*                                                                               
* ON ENTRY:    PARAM 1             A(TSAR RECORD)                               
*                                                                               
* ON EXIT:     CC                  EQ = READ MINIO ELEMENT OKAY                 
*                                  NE = COULDN'T FIND THE MINIO ELEMENT         
***********************************************************************         
GCLSTTSR NTR1                                                                   
         L     R2,0(R1)                                                         
         USING TLSTD,R2                                                         
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY     GET CLUSTER FOR THIS DETAIL                  
         MVI   MINEKEY,RPRDTELQ                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),TLSDTSTA                                 
         BAS   RE,MINIOHI                                                       
         BNE   EXITL                                                            
*                                                                               
         L     R6,MINELEM          MAKE SURE WE GOT THE RIGHT CLUSTER           
         USING RPRDTELD,R6                                                      
         CLI   RPRDTEL,RPRDTELQ                                                 
         BNE   EXITL                                                            
         CLC   RPRDTSTA(L'RPROKMEL-1),TLSDTSTA                                  
         BNE   EXITL                                                            
         B     EXITOK                                                           
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS WHATVIEW BASED ON GSSMCODE                                  
***********************************************************************         
SETVIEW  NTR1                                                                   
         CLI   GSSMCODE,VWAVAIL                                                 
         BE    *+12                                                             
         CLI   GSSMCODE,VWAVAIL2                                                
         BNE   *+12                                                             
         MVI   WHATVIEW,VWAVAIL                                                 
         B     SETVIEWX                                                         
*                                                                               
         CLI   GSSMCODE,VWMBOOK                                                 
         BE    *+12                                                             
         CLI   GSSMCODE,VWMBOOK2                                                
         BNE   *+12                                                             
         MVI   WHATVIEW,VWMBOOK                                                 
         B     SETVIEWX                                                         
*                                                                               
         CLI   GSSMCODE,VWMDEMO                                                 
         BE    *+12                                                             
         CLI   GSSMCODE,VWMDEMO2                                                
         BNE   *+12                                                             
         MVI   WHATVIEW,VWMDEMO                                                 
         B     SETVIEWX                                                         
*                                                                               
         CLI   GSSMCODE,VWPACKGE                                                
         BE    *+12                                                             
         CLI   GSSMCODE,VWPACKG2                                                
         BNE   *+12                                                             
         MVI   WHATVIEW,VWPACKGE                                                
         B     SETVIEWX                                                         
*                                                                               
         CLI   GSSMCODE,VWMCOST                                                 
         BE    *+12                                                             
         CLI   GSSMCODE,VWMCOST2                                                
         BNE   *+12                                                             
         MVI   WHATVIEW,VWMCOST                                                 
         B     SETVIEWX                                                         
*                                                                               
SETVIEWX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK THE WHATVIEW AND VIEWFLG1 TO SEE IF WE NEED MULTIPLE TSAR               
*   LINES.                                                                      
*                                                                               
*  RETURNS CC    NOT EQUAL     - SINGLE LINE DISPLAY                            
*                EQUAL         - MUTLILINE DISPLAY                              
*                                                                               
*          BOBYTE1             - NUMBER OF LINES IN DISPLAY                     
***********************************************************************         
CHKVIEW  NTR1                                                                   
         LA    RE,VWMLTAB                                                       
CKVW5    CLI   0(RE),EOT                                                        
         BE    CKVWSGL                                                          
         CLC   WHATVIEW,0(RE)                                                   
         BNE   CKVW10                                                           
*                                                                               
         MVC   BOBYTE1,1(RE)                                                    
         NC    BOBYTE1,VIEWFLG1                                                 
         CLC   BOBYTE1,1(RE)                                                    
         BE    CKVWML                                                           
*                                                                               
CKVW10   LA    RE,L'VWMLTAB(RE)                                                 
         B     CKVW5                                                            
*                                                                               
CKVWSGL  MVI   BOBYTE1,1           SINGLE LINE VIEW                             
         B     EXITL                                                            
*                                                                               
CKVWML   MVC   BOBYTE1,2(RE)       MULTILINE VIEW                               
         B     EXITOK                                                           
*                                                                               
VWMLTAB  DS    0CL3                MUTILINE VIEW TAB                            
         DC    AL1(VWMCOST),AL1(0),AL1(3)                                       
         DC    AL1(VWMBOOK),AL1(VF1MLDMO+VF1SFNOT),AL1(4)                       
         DC    AL1(VWMBOOK),AL1(VF1SFNOT),AL1(2)                                
         DC    AL1(VWMBOOK),AL1(VF1MLDMO),AL1(4)                                
*        DC    AL1(VWMBOOK),AL1(0),AL1(1)                                       
         DC    AL1(EOT)                                                         
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
* REORGANIZATION OF  SAVBKS, SAVLBLS, & SAVDMOS  BASED ON PRIME                 
***********************************************************************         
REORGPRM NTR1                                                                   
***      CLI   PRIMEBK,1           DO WE HAVE TO?                               
***      BE    REORG50             NO                                           
         LA    RE,SAVBKS                                                        
         USING BOOKLIN,RE                                                       
         CLC   PRIMEBK,BKLNDORD                                                 
         BE    REORG50                                                          
         DROP  RE                                                               
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
REORG50  DS    0H                                                               
***      CLI   PRIMEDM,1                                                        
***      BE    REORGX                                                           
         LA    RE,SAVDMOS                                                       
         USING DEMOLIN,RE                                                       
         CLC   PRIMEDM,DMLNDORD                                                 
         BE    REORGX                                                           
         DROP  RE                                                               
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
FLTXX    MVI   SVPARMS,DFLTX       EXIT NOT WANTED FOR FILTER                   
         B     EXITOK                                                           
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
*                                                                               
EXITERR  TM    MINIOFLG,MNIOFCLS      DO WE NEED TO CLOSE MINIO?                
         BZ    EXITL                                                            
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    EXITL                                                            
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
* INITIALIZATION                                                                
***********************************************************************         
         DS    0D                                                               
         USING *,RB                                                             
INITPRG  DS    0H                                                               
         LR    RB,RF                                                            
         B     *+12                                                             
         DC    CL8'**INIT**'                                                    
*                                                                               
         OI    TWASRVH+1,X'01'     SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    TWASRVH+6,X'80'                                                  
         OI    GCINDS1,GCIPROT             UNPROT ON NTRSES                     
         OI    GSINDSL1,GSINOIO+GSIXKEY    WE'LL DO THE IO'S                    
         OI    LSSTAT1,LSSBALL     BUILD ALL OF LIST IN ONE GO                  
*                                                                               
         MVI   MINIOFLG,0                                                       
*                                                                               
         L     R6,ACOM                                                          
         USING COMFACSD,R6                                                      
         NI    MISCFLG1,X'FF'-MF1GLOBR                                          
         GOTO1 CGLOBBER,BODMCB,=C'GETD',BOWORK1,24,GLVXCTL                      
         CLI   8(R1),GLEGNF        GLOBAL NOT FOUND?                            
         BE    INIT10              NOPE, SKIP DELETE                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),BODMCB,=C'DELE',,,GLVXCTL                                   
         DROP  R6                                                               
*                                                                               
INITD    USING GLVXFRSY,BOWORK1                                                 
         CLC   =C'REPSWP',INITD.GLVXFRSY                                        
         BNE   *+8                                                              
         OI    MISCFLG1,MF1GLOBR   CAME FROM GLOBBER                            
         DROP  INITD                                                            
*                                                                               
         XC    TWASRV,TWASRV                                                    
         MVC   TWASRV(3),=C'=RE'                                                
         OI    TWASRVH+6,X'80'                                                  
         MVI   TWASRVH+5,3                                                      
*                                                                               
INIT10   LR    RE,RA                    ANY CONTRACT?                           
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         OC    0(L'SVCONNUM,RE),0(RE)                                           
         BZ    *+12                     NO                                      
         CLI   SVPRONUM-SVCONNUM(RE),0                                          
         BNE   INITX                    YES, LEAVE IT ALONE THEN                
*                                                                               
         ZAP   GRNDTCST-SVCONNUM(L'GRNDTCST,RE),=P'0'                           
         ZAP   GRNDTRTG-SVCONNUM(L'GRNDTRTG,RE),=P'0'                           
         ZAP   GRNDTDMO-SVCONNUM(L'GRNDTDMO,RE),=P'0'                           
         ZAP   GRNDTSPT-SVCONNUM(L'GRNDTSPT,RE),=P'0'                           
         ZAP   GRNDTCPP-SVCONNUM(L'GRNDTCPP,RE),=P'0'                           
         ZAP   GRNDTDTL-SVCONNUM(L'GRNDTDTL,RE),=P'0'                           
*                                                                               
INITX    B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR PROPOSAL NUMBER X                                             
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
         DS    0D                                                               
         USING *,RB                                                             
PRODTA   DS    0H                                                               
         LR    RB,RF                                                            
         B     *+12                                                             
         DC    CL8'**VPRO**'                                                    
*                                                                               
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
         NI    MISCFLG2,FF-MF2ADPRO                                             
         TM    BCINDS1,BCINACT     IS THIS THE FIRST TIME?                      
         BZ    VALPRO10            NO                                           
         TM    GCINDS2,GCINTRS     JUST NTRSED?                                 
         BZ    VALPRO10            NO                                           
         CLI   PSREC,R#PEND        FROM PENDING?                                
         BNE   VALPRO10            NO                                           
*                                                                               
         MVI   BCPFKEY,0                                                        
*                                                                               
         GOTO1 VCOLY,BODMCB,(X'28',0),(0,0)                                     
         CLI   BODMCB+4,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,BODMCB                                                        
         GOTO1 (RF),BODMCB,(R9)                                                 
         BL    EXITL                                                            
*                                                                               
         OI    MISCFLG2,MF2ADPRO   PROPOSAL ADDED                               
*                                                                               
         MVC   BOBYTE1,BPRONUM                                                  
         XI    BOBYTE1,FF                                                       
         EDIT  (B1,BOBYTE1),(3,FVIFLD),ALIGN=LEFT,ZERO=NOBLANK,        +        
               DUB=BODUB1,WRK=BOWORK1                                           
*                                                                               
         STC   R0,FVILEN                                                        
         BCTR  R0,0                                                             
         STC   R0,FVXLEN                                                        
         OI    FVIIND,FVINUM                                                    
         NI    FVIIND,FF-FVIVAL                                                 
*                                                                               
VALPRO10 DS    0H                                                               
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
* NUMBER OF LINES FIELD                                                         
***********************************************************************         
         DS    0D                                                               
         USING *,RB                                                             
LINEDTA  DS    0H                                                               
         LR    RB,RF                                                            
         B     *+12                                                             
         DC    CL8'LINEDTA'                                                     
*                                                                               
         LA    RF,LINETBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LINETBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLINE)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY LINE COUNT FIELD                                                      
***********************************************************************         
DISLINE  DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTDTL-TWAD)                                             
         TM    2(RE),X'0C'         CHECK FOR PACKED                             
         BNO   DISLINEX                                                         
*                                                                               
         EDIT  (P3,0(RE)),(8,FVIFLD),ZERO=NOBLANK,WRK=BOWORK1,         X        
               DUB=BODUB1                                                       
DISLINEX B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCCESS SUBACTION FIELD                                                      
***********************************************************************         
SUBACTN  DS    0H                                                               
         NMOD1 0,*OSUBA**                                                       
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
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    EXITH                                                            
*                                                                               
         OI    LSSCIND2,LSSCIPAG                                                
         L     R2,SVPARMS3                                                      
         ST    R2,FVADDR                                                        
*                                                                               
SUBACT02 DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    SUBACTH                                                          
*                                                                               
         CLI   8(R2),C'*'                                                       
         BE    SUBACTH                                                          
*                                                                               
         OI    MISCFLG2,MF2TLSBA                                                
         ST    R2,SBACURSR                                                      
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'DEL'     SELECT FOR DELETE?                           
         BNE   SUBACT10                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         GOTO1 =A(DELSUBAC),BODMCB,(R9),RR=BORELO                               
         B     SUBACTX             DON'T DO ANY MORE PROCESSING FOR IT          
*                                                                               
SUBACT10 DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'KEEP'    SELECT FOR DELETE?                           
         BNE   SUBACT20                                                         
         TM    FILTFLG1,FF1HIDN    HIDDEN VIEW?                                 
         BNZ   EXITNV              YES                                          
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
         GOTO1 =A(HIDSUBAC),BODMCB,(R9),RR=BORELO                               
         B     SUBACTX             DON'T DO ANY MORE PROCESSING FOR IT          
*                                                                               
SUBACT20 DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'UNKEEP'  SELECT FOR DELETE?                           
         BNE   SUBACT30                                                         
         TM    FILTFLG1,FF1HIDN    HIDDEN VIEW?                                 
         BZ    EXITNV              YES                                          
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         GOTO1 =A(UNHSUBAC),BODMCB,(R9),RR=BORELO                               
         B     SUBACTX             DON'T DO ANY MORE PROCESSING FOR IT          
*                                                                               
SUBACT30 DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL8'X'     SELECT FOR REFETCH?                          
         BNE   SUBACT40                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         GOTOX (DTLNRFQ,AREPRO01),BODMCB,SAVSTAS,SAVBKS,SAVDMOS,       X        
               TLSDTSTA                                                         
         DROP  R2                                                               
*                                                                               
         OI    LSLNIND1,LSLNIUPD   DO UPDATE                                    
         OI    LSSCIND1,LSSCIINP                                                
         OI    LSSCIND2,LSSCIPAG   RE-DISPLAY PAGE                              
         OI    GCINDS2,GCIANYCH    FOOL CONTROLLER THAT WE HAVE CHANGE          
*                                                                               
         B     SUBACTX             DON'T DO ANY MORE PROCESSING FOR IT          
*                                                                               
SUBACT40 DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'ZZZZ'    SELECT FOR ALL TEXT?                         
         BNE   SUBACT50                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         GOTO1 =A(TXTSUBAC),BODMCB,(R9),RR=BORELO                               
*                                                                               
         OI    LSLNIND1,LSLNIUPD   DO UPDATE                                    
         OI    LSSCIND1,LSSCIINP                                                
         OI    LSSCIND2,LSSCIPAG   RE-DISPLAY PAGE                              
         OI    GCINDS2,GCIANYCH    FOOL CONTROLLER THAT WE HAVE CHANGE          
*                                                                               
         B     SUBACTX             DON'T DO ANY MORE PROCESSING FOR IT          
*                                                                               
SUBACT50 DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'%%%%'    REFRESH ALL COSTS?                           
         BNE   SUBACT60                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         GOTO1 =A(PBCSUBAC),BODMCB,(R9),RR=BORELO                               
         BL    EXITL                                                            
*                                                                               
         B     SUBACTX             DON'T DO ANY MORE PROCESSING FOR IT          
*                                                                               
SUBACT60 DS    0H                                                               
*                                                                               
SUBACTH  TM    GCINDS2,GCIANYCH    ANY CHANGES ON THE SCREEN?                   
         BZ    EXITH               NO                                           
         BAS   RE,MINIOCLS                                                      
         B     EXITH                                                            
*                                                                               
SUBACTX  DS    0H                                                               
         BAS   RE,MINIOCLS                                                      
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
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
         MVI   VIEWFLG1,0                                                       
         CLI   WHATVIEW,VWMBOOK                                                 
         BNE   *+8                                                              
         OI    VIEWFLG1,VF1SFNOT   SHOW SMALL FOOTNOTES                         
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
         CLC   0(0,RE),=CL8'SHRS'                                               
         BNE   *+20                                                             
         TM    FILTFLG1,FF1LVLS                                                 
         BNZ   *+12                                                             
         OI    FILTFLG1,FF1SHRS                                                 
         B     VALOPT30                                                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'SHARES'                                             
         BNE   *+20                                                             
         TM    FILTFLG1,FF1LVLS                                                 
         BNZ   *+12                                                             
         OI    FILTFLG1,FF1SHRS                                                 
         B     VALOPT30                                                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'LVLS'                                               
         BNE   *+20                                                             
         TM    FILTFLG1,FF1SHRS                                                 
         BNZ   *+12                                                             
         OI    FILTFLG1,FF1LVLS                                                 
         B     VALOPT30                                                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'LEVELS'                                             
         BNE   *+20                                                             
         TM    FILTFLG1,FF1SHRS                                                 
         BNZ   *+12                                                             
         OI    FILTFLG1,FF1LVLS                                                 
         B     VALOPT30                                                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'KEEP'                                               
         BNE   *+12                                                             
         OI    FILTFLG1,FF1HIDN                                                 
         B     VALOPT30                                                         
*                                                                               
         CH    R1,=H'4'                                                         
         BH    VALOPT12                                                         
         LR    RF,R1                                                            
         BCTR  RF,0                LOOKING FOR 'COST' + DIGIT                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'COST'                                               
         BNE   VALOPT12                                                         
         LA    RF,1(RF,RE)                                                      
         CLI   0(RF),C'1'                                                       
         BL    EXITNV                                                           
         CLI   0(RF),C'4'                                                       
         BH    EXITNV                                                           
         ZIC   R0,0(RF)                                                         
         N     R0,=F'7'                                                         
         BCTR  R0,0                                                             
         STC   R0,WHICHCST                                                      
         B     VALOPT30                                                         
*                                                                               
VALOPT12 DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'STACK'                                              
         BNE   VALOPT14                                                         
         CLI   WHATVIEW,VWMBOOK                                                 
         BNE   *+8                                                              
         OI    VIEWFLG1,VF1MLDMO    SHOW MULTILINE DEMO                         
         B     VALOPT30                                                         
*                                                                               
VALOPT14 DS    0H                                                               
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'NOFNOTE'                                            
         BNE   VALOPT16                                                         
         CLI   WHATVIEW,VWMBOOK                                                 
         BNE   *+8                                                              
         NI    VIEWFLG1,FF-VF1SFNOT    NO FOOTNOTES                             
         B     VALOPT30                                                         
*                                                                               
VALOPT16 DS    0H                                                               
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'LFNOTE'                                             
         BNE   VALOPT20                                                         
         CLI   WHATVIEW,VWMBOOK                                                 
         BNE   *+8                                                              
         OI    VIEWFLG1,VF1BFNOT    SHOW BIG FOOTNOTE                           
         B     VALOPT30                                                         
*                                                                               
VALOPT20 DS    0H                                                               
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'KEY'                                                
         BNE   *+12                                                             
         OI    MISCFLG1,MF1MNKY                                                 
         B     VALOPT30                                                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=CL8'DPT'                                                
         BNE   VALOPT28                                                         
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
         CLI   PSNLEN,L'LDPTSEQS   TOO MANY?                                    
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
VALOPT28 DS    0H                                                               
         MVC   FVMSGNO,=AL2(INVLOPTN)   OPTION NOT AVAILABLE                    
         B     EXITL                                                            
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
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(TWAD-SVVWFLG1)                                             
         MVC   0(1,RE),VIEWFLG1                                                 
*                                                                               
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE VIEW FIELD                                                           
***********************************************************************         
VALVIEWF DS    0H                                                               
         NMOD1 0,**VVEW**                                                       
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
         CLI   BCPFKEY,0           PFKEY HIT?                                   
         BE    VALVEW50            NO, ENTER KEY                                
*                                                                               
         CLI   BCPFKEY,PFAVAIL                                                  
         BNE   VALVEW10                                                         
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(8),=CL8'AVAIL'                                            
         B     VALVEW40                                                         
*                                                                               
VALVEW10 CLI   BCPFKEY,PFPACKGE                                                 
         BNE   VALVEW12                                                         
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(8),=CL8'PACKAGE'                                          
         B     VALVEW40                                                         
*                                                                               
VALVEW12 CLI   BCPFKEY,PFMBOOK                                                  
         BNE   VALVEW14                                                         
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(8),=CL8'MBOOK'                                            
         B     VALVEW40                                                         
*                                                                               
VALVEW14 CLI   BCPFKEY,PFMDEMO                                                  
         BNE   VALVEW16                                                         
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(8),=CL8'MDEMO'                                            
         B     VALVEW40                                                         
*                                                                               
VALVEW16 CLI   BCPFKEY,PFMCOST                                                  
         BNE   VALVEW50                                                         
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(8),=CL8'MCOST'                                            
*                                                                               
VALVEW40 MVI   FVILEN,8                                                         
         MVI   FVXLEN,7                                                         
         B     VALVEW60                                                         
*                                                                               
VALVEW50 TM    FVIIND,FVIVAL                                                    
         BNZ   *+8                                                              
VALVEW60 OI    MISCFLG1,MF1VWCHG   THE VIEW WAS CHANGED                         
*                                                                               
         MVI   WHATVIEW,VWAVAIL    DEFAULT IS THE AVAIL VIEW                    
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         MVC   FVIFLD(5),=C'AVAIL'                                              
         B     VALVEWX                                                          
*                                                                               
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL8'AVAIL'                                            
         BE    VALVEWX                                                          
         MVI   WHATVIEW,VWPACKGE                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL8'PACKAGE'                                          
         BE    VALVEWX                                                          
         MVI   WHATVIEW,VWMDEMO                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL8'MDEMO'                                            
         BE    VALVEWX                                                          
         MVI   WHATVIEW,VWMCOST                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL8'MCOST'                                            
         BE    VALVEWX                                                          
         MVI   WHATVIEW,VWMBOOK                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL8'MBOOK'                                            
         BNE   EXITNV                                                           
*                                                                               
VALVEWX  OI    FVIIND,FVIVAL       MARK FIELD AS VALIDATED                      
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
         BAS   RE,CHKVIEW                                                       
         BNE   DEFCL#1                                                          
         OI    LSSTAT3,LS3RFIX                                                  
         XC    LSROWLIN,LSROWLIN                                                
         MVC   LSROWLIN+1,BOBYTE1                                               
         MVC   LSCOLLIN,=AL2(80)                                                
         OI    LSSTAT1,LSSMUROW                                                 
*                                                                               
DEFCL#1  CLI   SELPROFS,RREPQSEL                                                
         BE    DEFCL0                                                           
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
DEFCL0   DS    0H                                                               
*                                                                               
***********************************                                             
* THESE 3 OR 4 FIXED COLUMNS ARE COMMON IN ALL VIEWS                            
***********************************                                             
         LA    RF,LSFIXCLM         RF = A(1ST FIXED COLUMN)                     
         USING DCTABD,RF                                                        
*                                                                               
         LR    RE,RA               DO WE HAVE A STATION IN THE KEY?             
         AH    RE,=Y(SVSTATN-TWAD)                                              
         CLI   0(RE),0                                                          
         BNE   DMLC00              YES                                          
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(82)    STATION COLUMN                               
         LA    RF,DCTABL(RF)                                                    
*                                                                               
DMLC00   CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(18)    DAY/TIME FIELD                               
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         LR    RE,RA               DO WE HAVE A STATION IN THE KEY?             
         AH    RE,=Y(SVSTATN-TWAD)                                              
         CLI   0(RE),0                                                          
         BNE   DMLC02              YES                                          
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(85)    SHORTEN PROGRAM COLUMN                       
         LA    RF,DCTABL(RF)                                                    
         MVC   LSFIXNUM,=AL2(4)    # OF FIXED COLUMNS (5 W/ DAYPART)            
         B     DMLC04                                                           
*                                                                               
DMLC02   CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(19)    REGULAR PROGRAM FIELD                        
         LA    RF,DCTABL(RF)                                                    
         MVC   LSFIXNUM,=AL2(3)    # OF FIXED COLUMNS (4 W/ DAYPART)            
*                                                                               
DMLC04   CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(20)    DAYPART FIELD                                
         LA    RF,DCTABL(RF)       SO WE CAN TACK MORE ON                       
***************                                                                 
* AVAIL VIEW ONLY HAS FIXED COLUMNS (BUT WE CAN ONLY HAVE A MAX OF 8            
* FIXED COLUMNS, SO WE'LL MAKE AT LEAST TWO VARIABLE COLUMNS)                   
***************                                                                 
         CLI   WHATVIEW,0                                                       
         BE    *+12                                                             
         CLI   WHATVIEW,VWAVAIL    AVAIL VIEW?                                  
         BNE   DMLC10                                                           
*                                                                               
         ZICM  R1,LSFIXNUM,2                                                    
         LA    R1,3(R1)            3 MORE FIXED COLUMNS                         
         STCM  R1,3,LSFIXNUM                                                    
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(24)    COST FIELD                                   
         LA    RF,DCTABL(RF)                                                    
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(51)    DEMO RATING FIELD                            
         LA    RF,DCTABL(RF)                                                    
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(61)    CPP FIELD                                    
*                                                                               
         MVC   LSVARNUM,=AL2(2)    NUMBER OF VARIABLE COLUMNS                   
         LA    RF,LSVARCLM         RF = A(1ST VARIABLE COLUMN)                  
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(26)    TAB CPP FIELD                                
         LA    RF,DCTABL(RF)                                                    
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(23)    NEED RATE FIELD                              
         B     DMLCX                                                            
***************                                                                 
* PACKAGE VIEW ONLY HAS FIXED COLUMNS                                           
***************                                                                 
DMLC10   CLI   WHATVIEW,VWPACKGE                                                
         BNE   DMLC20                                                           
         MVC   LSVARNUM,=AL2(4)    NUMBER OF VARIABLE COLUMNS                   
         LA    RF,LSVARCLM         RF = A(1ST VARIABLE COLUMN)                  
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(27)    SPOTS FIELD                                  
         LA    RF,DCTABL(RF)                                                    
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(24)    COST FIELD                                   
         LA    RF,DCTABL(RF)                                                    
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(51)    RATING FIELD                                 
         LA    RF,DCTABL(RF)                                                    
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(61)    CPP FIELD                                    
         B     DMLCX                                                            
***************                                                                 
* MULTI-DEMO VIEW HAS FIXED AND VARIABLE                                        
***************                                                                 
DMLC20   CLI   WHATVIEW,VWMDEMO                                                 
         BNE   DMLC30                                                           
*                                                                               
         ZICM  R1,LSFIXNUM,2                                                    
         LA    R1,3(R1)            3 MORE FIXED COLUMNS                         
         STCM  R1,3,LSFIXNUM                                                    
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(24)    COST FIELD                                   
         LA    RF,DCTABL(RF)                                                    
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(51)    RATING FIELD                                 
         LA    RF,DCTABL(RF)                                                    
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(61)    CPP FIELD                                    
         LA    RF,DCTABL(RF)       SO WE CAN TACK MORE ON                       
*                                                                               
         LA    RF,LSVARCLM           RF = A(1ST VARIABLE COLUMN)                
         LA    RE,SAVDMOS+L'SAVDMO   RE = A(2ND DEMO)                           
         LA    R1,PRMDEMOQ+1         FIELD EQUATE FOR 2ND DEMO FIELD            
         LA    R2,PRMDCPPQ+1         FIELD EQUATE FOR 2ND DEMO CPP FLD          
         SR    R3,R3                 R3 = NUMBER OF VARIABLE COLUMNS            
DMLC23   OC    0(L'SAVDMO,RE),0(RE)  ANY DEMO DEFINED HERE?                     
         BZ    DMLC26                NO                                         
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         STCM  R1,3,DCTFLD#        DEMO FIELD                                   
         LA    RF,DCTABL(RF)                                                    
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         STCM  R2,3,DCTFLD#        DEMO'S CPP FIELD                             
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         LA    R3,2(R3)            WE PUT TWO MORE VARIABLE COLUMNS             
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         LA    RE,L'SAVDMO(RE)                                                  
         LA    R0,SAVDMOS+L'SAVDMOS                                             
         CR    RE,R0                                                            
         BL    DMLC23              LOOP BACK UNTIL ALL DEMOS ARE DONE           
*                                                                               
DMLC26   STCM  R3,3,LSVARNUM       NUMBER OF VARIABLE COLUMNS                   
         B     DMLCX                                                            
***************                                                                 
* MULTI-BOOK VIEW                                                               
***************                                                                 
DMLC30   CLI   WHATVIEW,VWMBOOK                                                 
         BNE   DMLC40                                                           
*                                                                               
         TM    SELPROF+SELMBCB,SELMBCA                                          
         BZ    DMLC32              NO COST COLUMN                               
*                                                                               
         ZICM  R1,LSFIXNUM,2                                                    
         LA    R1,1(R1)            1 MORE FIXED COLUMN                          
         STCM  R1,3,LSFIXNUM                                                    
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(24)    COST FIELD                                   
         LA    RF,DCTABL(RF)                                                    
*                                                                               
DMLC32   DS    0H                                                               
         ZICM  R1,LSFIXNUM,2                                                    
         LA    R1,1(R1)            1 MORE FIXED COLUMN                          
         STCM  R1,3,LSFIXNUM                                                    
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(PRMBKQ)  FIRST BOOK                                 
         TM    VIEWFLG1,VF1BFNOT     BIG FIELD?                                 
         BNO   *+10                  NO                                         
         MVC   DCTFLD#,=AL2(PRMBK2Q) FIRST BOOK                                 
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         LA    RF,LSVARCLM           RF = A(1ST VARIABLE COLUMN)                
         LA    RE,SAVBKS+L'SAVBK     RE = A(2ND BOOK)                           
         LA    R1,PRMBKQ+1           FIELD EQUATE FOR 2ND BOOK FIELD            
         TM    VIEWFLG1,VF1BFNOT     BIG FIELD?                                 
         BNO   *+8                   NO                                         
         LA    R1,PRMBK2Q+1          FIELD EQUATE FOR 2ND BOOK FIELD            
         SR    R2,R2                 R2 = NUMBER OF VARIABLE COLUMNS            
*                                                                               
DMLC33   OC    0(L'SAVBK,RE),0(RE)   ANY BOOK DEFINED HERE?                     
         BZ    DMLC36                NO                                         
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         STCM  R1,3,DCTFLD#        BOOK FIELD                                   
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         LA    R2,1(R2)            WE PUT ONE MORE VARIABLE COLUMNS             
         LA    R1,1(R1)                                                         
         LA    RE,L'SAVBK(RE)                                                   
         LA    R0,SAVBKS+L'SAVBKS                                               
         CR    RE,R0                                                            
         BL    DMLC33              LOOP BACK UNTIL ALL DEMOS ARE DONE           
*                                                                               
DMLC36   STCM  R2,3,LSVARNUM       NUMBER OF VARIABLE COLUMNS                   
         B     DMLCX                                                            
*                                                                               
DMLC40   CLI   WHATVIEW,VWMCOST                                                 
         BNE   DMLC50                                                           
*                                                                               
         ZICM  R1,LSFIXNUM,2                                                    
         LA    R1,4(R1)            4 MORE FIXED COLUMNS                         
         STCM  R1,3,LSFIXNUM                                                    
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(102)                                                
         LA    RF,DCTABL(RF)                                                    
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(103)                                                
         LA    RF,DCTABL(RF)                                                    
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(104)                                                
         LA    RF,DCTABL(RF)                                                    
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN                                                
         MVC   DCTFLD#,=AL2(105)                                                
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         XC    LSVARNUM,LSVARNUM       NUMBER OF VARIABLE COLUMNS               
         B     DMLCX                                                            
*                                                                               
DMLC50   DS    0H                                                               
DMLCX    B     EXITOK                                                           
         DROP  RF                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY COMMENT FIELD                                                     
***********************************************************************         
DISCOMNT DS    0H                                                               
         NMOD1 0,**DCOM**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
         L     RC,4(R1)                                                         
         USING OVERWRKD,RC                                                      
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRCMELQ                                                 
         MVI   MINEKEY+1,1                                                      
         CLI   SVPARMS2+3,12       1ST COMMENT LINE (FIELD #12)                 
         BE    *+8                                                              
         MVI   MINEKEY+1,2         2ND COMMENT LINE (FIELD #28)                 
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   DISCOMX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRCMELD,R6                                                      
         CLI   RPRCMEL,RPRCMELQ    DO WE HAVE A COMMENT ELEMENT?                
         BNE   DISCOMX             NO                                           
         CLI   SVPARMS2+3,12       1ST COMMENT LINE (FIELD #12)?                
         BNE   *+12                                                             
         CLI   RPRCMLIN,1          YES, DO WE HAVE ONE FOR LINE 1?              
         BNE   DISCOMX             NO                                           
*                                                                               
         ZIC   R1,RPRCMLEN                                                      
         SH    R1,=Y(RPRCMOVQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),RPRCMTXT  SHOW THE TEXT                                
*                                                                               
DISCOMX  OI    FVIIND,FVIVAL       MARK FIELD AS VALIDATED                      
         B     EXITOK                                                           
         DROP  R5,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY COMMENT FIELD                                                    
***********************************************************************         
VALCOMNT DS    0H                                                               
         NMOD1 0,**VCOM**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
         L     RC,4(R1)                                                         
         USING OVERWRKD,RC                                                      
*                                                                               
         TM    FVIIND,FVIVAL       WAS THIS COMMENT CHANGED?                    
         BNZ   VALCOMX             NO                                           
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRCMELQ                                                 
         MVI   MINEKEY+1,1                                                      
         CLI   SVPARMS2+3,12       1ST COMMENT LINE (FIELD #12)?                
         BE    *+8                                                              
         MVI   MINEKEY+1,2         NO, 2ND COMMENT LINE (FIELD #28)             
         L     R6,MINELEM                                                       
         USING RPRCMELD,R6                                                      
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   VALCOM50            COMMENT ELEMENT DOES NOT EXIST               
*                                                                               
         CLI   RPRCMEL,RPRCMELQ    DO WE HAVE A COMMENT ELEMENT?                
         BNE   VALCOM50            NO                                           
         CLI   SVPARMS2+3,12       1ST COMMENT LINE (FIELD #12)?                
         BNE   VALCOM10                                                         
         CLI   RPRCMLIN,1          YES, DO WE HAVE ONE FOR LINE 1?              
         BNE   VALCOM50            NO                                           
***************                                                                 
* COMMENT ELEMENT EXISTS FOR THIS LINE                                          
***************                                                                 
VALCOM10 CLI   FVILEN,0            NO COMMENT TEXT FOR THIS LINE?               
         BNE   *+12                                                             
         BAS   RE,MINIODEL         NONE, DON'T NEED THIS ELEM ANYMORE           
         B     VALCOMX                                                          
*                                                                               
         XC    RPRCMTXT(L'BOELEM),RPRCMTXT                                      
         ZIC   R1,FVXLEN           UPDATE THE ELEMENT                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RPRCMTXT(0),FVIFLD                                               
         AH    R1,=Y(RPRCMOVQ+1)                                                
         STC   R1,RPRCMLEN                                                      
         BAS   RE,MINIOWRT                                                      
         B     VALCOMX                                                          
***************                                                                 
* COMMENT ELEMENT DID NOT EXIST FOR THIS LINE                                   
***************                                                                 
VALCOM50 CLI   FVILEN,0            NO INPUT IN THIS FIELD?                      
         BE    VALCOMX             NONE, THEN NOTHING TO ADD                    
*                                                                               
         XC    RPRCMEL(L'BOELEM),RPRCMEL                                        
         MVI   RPRCMEL,RPRCMELQ                                                 
         MVI   RPRCMLIN,1                                                       
         CLI   SVPARMS2+3,12       1ST COMMENT LINE (FIELD #12)?                
         BE    *+8                                                              
         MVI   RPRCMLIN,2          NO, 2ND COMMENT LINE (FIELD #28)             
*                                                                               
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RPRCMTXT(0),FVIFLD                                               
         AH    R1,=Y(RPRCMOVQ+1)                                                
         STC   R1,RPRCMLEN         LENGTH OF MINIO ELEMENT                      
*                                                                               
         BAS   RE,MINIOADD                                                      
         BE    *+6                                                              
         DC    H'0'                DUPLICATE? CAN'T BE                          
*                                                                               
VALCOMX  OI    FVIIND,FVIVAL       MARK FIELD AS VALIDATED                      
         B     EXITOK                                                           
         DROP  R5,R6                                                            
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
         TM    MISCFLG2,MF2PROCH    COMING FROM PRO/CHANGE?                     
         BZ    *+8                  NO                                          
         MVI   FVILEN,0                                                         
*                                                                               
VALBOK00 MVI   PRIMEBK,1                                                        
         CLI   FVILEN,0            ANY BOOK ENTERED HERE?                       
         BNE   VALBOK10            YES, VALIDATE IT                             
*                                                                               
         OC    SAVLBL,SAVLBL       NO, SHOW WHAT THE PRIMARY BOOK IS            
         BZ    *+14                                                             
         MVC   BOWORK2+8(L'SAVLBL),SAVLBL    USER DEFINED                       
         B     VALBOK05                                                         
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
         BNE   VALBOK05                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     VALBOK05                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
VALBOK05 DS    0H                                                               
         LA    RF,BOWORK2+8                                                     
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),BOWORK2+8                                              
         OI    FVIIND,FVIVAL                                                    
         OI    FVOIND,FVOXMT                                                    
         B     VALBOKX                                                          
*                                                                               
VALBOK10 DS    0H                                                               
         GOTO1 VBOOKVAL,BODMCB,(C'N',FVIHDR),(1,BOWORK1),              X        
               (C'B',VSCANNER),BODUB1                                           
         CLI   4(R1),0             GOOD BOOK?                                   
         BE    VALBOK50            NO, COULD BE USER-DEFINED BOOK               
*                                                                               
         LR    R2,RA               FIND THE PRIME BOOK NUMBER                   
         AH    R2,=Y(MINBKS-TWAD)                                               
         USING BOOKLIN,R2                                                       
         LA    R1,1                START WITH THE FIRST ONE                     
*                                                                               
VALBOK20 CLC   BKLNBK,BOWORK1      MATCH ON THIS 3-BYTE BOOK?                   
         BNE   *+24                NO                                           
         CLC   BKLNSPBK,BODUB1     MATCH ON SPECIAL BOOK?                       
         BNE   *+14                NO                                           
         OC    BKLNUPGD,BKLNUPGD   UPGRADE?                                     
         BZ    VALBOK60            NO                                           
         LA    R1,1(R1)                                                         
         LA    R2,L'MINBK(R2)      BUMP TO NEXT MINBKS ENTRY                    
         LR    R0,RA                                                            
         AH    R0,=Y(MINBKS+L'MINBKS-TWAD)                                      
         CR    R2,R0                                                            
         BL    VALBOK20                                                         
         B     EXITNV              BOOK IS NOT PART OF OUR BOOKS LIST           
         DROP  R2                                                               
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
         BL    VALBOK55                                                         
         TM    MISCFLG1,MF1PFRET                                                
         BZ    EXITNV              BOOK IS NOT PART OF OUR BOOKS LIST           
         MVI   FVILEN,0                                                         
         B     VALBOK00                                                         
*                                                                               
VALBOK60 STC   R1,PRIMEBK          SAVE # OF WHERE IN THE DISPLAY LIST          
*                                                                               
VALBOKX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY MULTI-COST FIELD HEADLINE FOR THE COLUMN                              
***********************************************************************         
MCSTHDNG DS    0H                                                               
         NMOD1 0,**HMCS**                                                       
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
         TM    MISCFLG1,MF1PFRET   COMING BACK FROM A SESSION                   
         BNZ   HDLMCS00            YES, DON'T BOTHER                            
*                                                                               
         CLI   SVPARMS2+3,PRMCOSTQ  FIRST RATING COLUMN?                        
         BNE   HDLMCS00                                                         
         BAS   RE,REORGPRM          YES, MUST REORDER 1ST TIME                  
*                                                                               
HDLMCS00 DS    0H                                                               
         L     R2,SVPARMS5                                                      
         ZIC   R3,SVPARMS2+3       FIELD NUMBER                                 
         SH    R3,=Y(PRMCOSTQ)     OFFSETED BY THE PRIME COST                   
*                                                                               
         MVC   0(8,R2),=CL20'COST'                                              
         LA    R0,1(R3)                                                         
         EDIT  (R0),(1,4(R2)),WRK=BOWORK1,DUB=BODUB1                            
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINCOSTS-TWAD)                                             
         MH    R3,=Y(L'MINCOST)                                                 
         LA    R3,0(R3,RE)                                                      
         USING CSTLIN,R3                                                        
*                                                                               
         OC    CSLNLBL,CSLNLBL                                                  
         BZ    *+10                                                             
         MVC   0(8,R2),CSLNLBL                                                  
*                                                                               
         L     R0,SVPARMS6                                                      
         GOTO1 MCSBOOK,BODMCB,(R3),(R0)                                         
         DROP  R3                                                               
*                                                                               
HEDLMCSX B     EXITOK                                                           
***********************************************************************         
* DISPLAY MULTICOST BOOK NAME                                                   
*                                                                               
* INPUT    P1 - A(COST)                                                         
*          P2 - A(DISPLAY AREA)                                                 
*                                                                               
* OUTPUT   BOOK FOR DISPLAY IS PLACED IN DISPLAY AREA                           
*                                                                               
* NOTE - BOWORK1 GETS CLOBBERED                                                 
***********************************************************************         
MCSBOOK  NTR1                                                                   
         L     R6,0(R1)                                                         
         USING CSTLIN,R6                                                        
         L     R5,4(R1)                                                         
*                                                                               
         LA    RE,SAVBKS                                                        
         USING BOOKLIN,RE                                                       
         LA    RF,SAVLBLS                                                       
         CLI   CSLNLBK,0           ANY BOOK LINK?                               
         BE    MCSBK4              NO USE PRIME BOOK                            
*                                                                               
MCSBK2   CLC   CSLNLBK,BKLNIORD    INTERNAL ORDER MATCH?                        
         BE    MCSBK4              YES                                          
         LA    RE,L'SAVBK(RE)                                                   
         LA    RF,L'SAVLBL(RF)                                                  
         LA    R0,SAVBKS+L'SAVBKS                                               
         CR    RE,R0                                                            
         BL    MCSBK2                                                           
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
MCSBK4   OC    0(L'SAVLBL,RF),0(RF)       LABEL?                                
         BZ    *+14                       NO                                    
         MVC   0(L'SAVLBL,R5),0(RF)                                             
         B     MCSBOOKX                                                         
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(3),BKLNBK                                                
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(2),=X'0B07'      PUT OUT SOURCE                           
         MVC   BOELEM+2(1),BKLNSPBK                                             
         DROP  RE                                                               
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
         BNE   MCSBK6                                                           
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     MCSBK6                                                           
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
MCSBK6   DS    0H                                                               
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),FVIFLD                                                   
*                                                                               
MCSBOOKX B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY MULTI COST FIELD                                                      
***********************************************************************         
DISMCOST DS    0H                                                               
         NMOD1 0,**DMCS**                                                       
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
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,(R2)  GET CLUSTER FOR THE TSAR RECORD            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM          USE MOST RECENT COST                         
         USING RPRDTELD,R6                                                      
         MVC   TLRDTSC1,RPRDTSC1                                                
         MVC   TLRDTNC1,RPRDTNC1                                                
         DROP  R6                                                               
*                                                                               
         ZIC   R3,SVPARMS2+3         YES, FIND OUT WHICH BOOK FIELD             
         SH    R3,=Y(PRMCOSTQ)         BY SUBTRACTING PRIME EQUATE              
*                                                                               
         CLC   LSROWREP,=H'1'      LINKED TSAR LINE 1?                          
         BNE   DISMCS20            NO                                           
******************                                                              
** DISPLAY COST **                                                              
******************                                                              
         LTR   R3,R3               PRIME COST?                                  
         BNZ   *+14                NO                                           
         MVC   BOFULL1,TLRDTNC1                                                 
         B     DISMCS10                                                         
*                                                                               
         XC    BOFULL1,BOFULL1                                                  
         L     R6,MINELEM                                                       
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
DISMCS5  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    DISMCS10            YES, NO SUPLEMENTAL COST ELEMENT             
*                                                                               
         CLI   0(R6),RPRCSELQ      SUPLEMENTAL COST ELEMENT?                    
         BNE   DISMCS5             NO                                           
*                                                                               
         BCTR  R3,0                                                             
         MH    R3,=Y(L'RPRCSSC2+L'RPRCSNC2)                                     
         LA    R3,RPRCSSC2-RPRCSELD(R3,R6)                                      
         MVC   BOFULL1,L'RPRCSSC2(R3)                                           
*                                                                               
DISMCS10 DS    0H                                                               
         TM    BOFULL1,X'80'       N/A?                                         
         BZ    *+14                                                             
         MVC   FVIFLD(2),=C'NA'                                                 
         B     DISMCSX                                                          
*                                                                               
         EDIT  BOFULL1,(17,FVIFLD),2,ALIGN=LEFT,WRK=BOWORK1,DUB=BODUB1          
         B     DISMCSX                                                          
********************                                                            
** DISPLAY RATING **                                                            
********************                                                            
DISMCS20 DS    0H                                                               
         CLC   LSROWREP,=H'2'      LINKED TSAR LINE 2?                          
         BNE   DISMCS30            NO                                           
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(MINCOSTS-TWAD)                                             
         LR    R0,R3                                                            
         MH    R0,=Y(L'MINCOST)                                                 
         AR    RF,R0                                                            
         USING CSTLIN,RF                                                        
*                                                                               
         MVC   BOBYTE1,SAVBK       DEFAULT INTERNAL ORDER #                     
*                                                                               
         CLI   CSLNLBK,0                                                        
         BE    *+10                                                             
         MVC   BOBYTE1,CSLNLBK     INTERNAL ORDER # FROM COST HEADER            
         DROP  RF                                                               
*                                                                               
         ZIC   R0,BOBYTE1                                                       
         BCTR  R0,0                                                             
         MH    R0,=Y(L'MINBK)                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(MINBKS-TWAD)                                               
         AR    RF,R0                                                            
*                                                                               
         USING BOOKLIN,RF                                                       
         OC    BKLNUPGD,BKLNUPGD   UPGRADE?                                     
         BNZ   *+16                                                             
         TM    SELPROF+SELDMOVB,SELDMOVA                                        
         BZ    *+8                                                              
         OI    FVATRB,FVAPROT                                                   
         DROP  RF                                                               
*                                                                               
         MVI   FVIFLD,C'0'         IF NONE THEN SHOW 0                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDVELD,R6                                                      
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
DSMCRT10 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    DSMCRTX                                                          
*                                                                               
         CLI   RPRDVEL,RPRDVELQ    DEMO VALUE ELEMENT?                          
         BNE   DSMCRT10                                                         
         CLC   RPRDVBNM,BOBYTE1    INTERNAL ORDER NUMBER THE SAME?              
         BNE   DSMCRT10            NO, WE NEED A DIFFERENT BOOK                 
*                                                                               
         MVC   BOBYTE1,SAVDMO+(DMLNIORD-DEMOLIN)                                
*                                                                               
         ZIC   R1,BOBYTE1                                                       
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,RPRDVDMO(R1)                                                  
*                                                                               
         LR    RE,R1               ARE WE BEYOND THE ELEMENT?                   
         LA    R0,RPRDVEL                                                       
         SR    RE,R0                                                            
         CLM   RE,1,RPRDVLEN                                                    
         BNL   DSMCRTX             YES, NOTHING TO SHOW                         
*                                                                               
         TM    FILTFLG1,FF1LVLS    DISPLAY LEVELS?                              
         BZ    *+12                NO                                           
         LA    R1,8(R1)                                                         
         B     DSMCRT12                                                         
         TM    FILTFLG1,FF1SHRS    DISPLAY SHARES?                              
         BZ    *+8                 NO                                           
         LA    R1,4(R1)                                                         
DSMCRT12 MVC   BOFULL1,0(R1)                                                    
         LA    R4,FVIFLD                                                        
         TM    BOFULL1,X'80'       IS THIS A DEMO OVERRIDE?                     
         BZ    DSMCRT15                                                         
         MVI   0(R4),C'*'          YES, THEN SHOW AN '*' BEFORE RATING          
         LA    R4,1(R4)                                                         
         NI    BOFULL1,X'FF'-X'80'   GET RID OF DEMO OVERRIDE BIT               
*                                                                               
DSMCRT15 TM    SAVOPTNS,OPTNDECQ   DISPLAY DEMO PRECISION?                      
         BZ    DSMCRT20            NO                                           
         EDIT  (B4,BOFULL1),(5,0(R4)),1,ALIGN=LEFT,WRK=BOWORK1,        X        
               DUB=BODUB1,ZERO=NOBLANK                                          
         MVC   PREVRTG,BOFULL1     SAVE SO WE CAN CALCULATE CPP LATER           
         B     DSMCRTX                                                          
*                                                                               
DSMCRT20 L     R3,BOFULL1          DON'T DISPLAY DEMO PRECISION                 
         CVD   R3,BODUB1                                                        
         ZAP   PCKOF08B,BODUB1                                                  
         SRP   PCKOF08B,64-1,5     ROUND THE # OFF INSTEAD                      
         EDIT  (P8,PCKOF08B),(5,0(R4)),ALIGN=LEFT,WRK=BOWORK1,         X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
DSMCRTX  B     DISMCSX                                                          
*****************                                                               
** DISPLAY CPP **                                                               
*****************                                                               
DISMCS30 DS    0H                                                               
         CLC   LSROWREP,=H'3'      LINKED TSAR LINE 3?                          
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(MINCOSTS-TWAD)                                             
         LR    R0,R3                                                            
         MH    R0,=Y(L'MINCOST)                                                 
         AR    RF,R0                                                            
         USING CSTLIN,RF                                                        
*                                                                               
         MVC   BOBYTE1,SAVBK       DEFAULT INTERNAL ORDER #                     
*                                                                               
         CLI   CSLNLBK,0                                                        
         BE    *+10                                                             
         MVC   BOBYTE1,CSLNLBK     INTERNAL ORDER # FROM COST HEADER            
         XC    PREVRTG,PREVRTG                                                  
         DROP  RF                                                               
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDVELD,R6                                                      
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
DSMCCP10 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    DSMCCP20            YES, NO RATING                               
*                                                                               
         CLI   RPRDVEL,RPRDVELQ    DEMO VALUE ELEMENT?                          
         BNE   DSMCCP10                                                         
         CLC   RPRDVBNM,BOBYTE1    SAME INTERNAL ORDER # ?                      
         BNE   DSMCCP10            NO, KEEP SEARCHING FOR IT                    
*                                                                               
         MVC   BOBYTE1,SAVDMO+(DMLNIORD-DEMOLIN)                                
*                                                                               
         ZIC   R1,BOBYTE1                                                       
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,RPRDVDMO(R1)                                                  
*                                                                               
         LR    RE,R1               ARE WE BEYOND THE ELEMENT?                   
         LA    R0,RPRDVEL                                                       
         SR    RE,R0                                                            
         CLM   RE,1,RPRDVLEN                                                    
         BNL   DSMCCP20            YES, NEED DEMO RATING TO CHANGE COST         
*                                                                               
         MVC   BOFULL1,0(R1)       RATING WITHOUT THE OVERRIDE BIT              
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ                                                
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5                                                    
         SRP   BODUB1,1,0                                                       
         CVB   R1,BODUB1                                                        
         STCM  R1,15,PREVRTG       NOW WE HAVE A RATING TO BASE WITH            
*                                                                               
DSMCCP20 ICM   R0,15,PREVRTG       RATING                                       
         CVD   R0,BODUB1                                                        
         ZAP   PCKOF08B,BODUB1                                                  
*                                                                               
         LTR   R3,R3               PRIME COST?                                  
         BNZ   *+14                NO                                           
         MVC   BOFULL1,TLRDTNC1                                                 
         B     DSMCCP24                                                         
*                                                                               
         XC    BOFULL1,BOFULL1                                                  
         L     R6,MINELEM                                                       
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
DSMCCP22 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    DSMCCP24            YES, NO SUPLEMENTAL COST ELEMENT             
*                                                                               
         CLI   0(R6),RPRCSELQ      SUPLEMENTAL COST ELEMENT?                    
         BNE   DSMCCP22            NO                                           
*                                                                               
         BCTR  R3,0                                                             
         MH    R3,=Y(L'RPRCSSC2+L'RPRCSNC2)                                     
         LA    R3,RPRCSSC2-RPRCSELD(R3,R6)                                      
         MVC   BOFULL1,L'RPRCSSC2(R3)                                           
*                                                                               
DSMCCP24 DS    0H                  NEGOTIATED COST                              
         TM    BOFULL1,X'80'       N/A?                                         
         BZ    *+14                NO                                           
         MVC   FVIFLD(2),=C'NA'                                                 
         B     DSMCCPX                                                          
*                                                                               
         L     R0,BOFULL1                                                       
         CVD   R0,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         BAS   RE,DIVPACKD         CPP = COST / RATING                          
         BE    DSMCCP30                                                         
         MVI   FVIFLD,C'*'         YES, '****' OUT THE CPP                      
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
         B     DSMCCPX                                                          
*                                                                               
DSMCCP30 EDIT  (P16,PCKOF16B),(7,FVIFLD),2,WRK=BOWORK1,ALIGN=LEFT,     X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
DSMCCPX  DS    0H                                                               
*                                                                               
DISMCSX  B     EXITOK                                                           
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE MULTI COST FIELD                                                     
***********************************************************************         
VALMCOST DS    0H                                                               
         NMOD1 0,**VMCS**                                                       
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
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,(R2)  GET CLUSTER FOR THE TSAR RECORD            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,INTOAIO5         SO WE CAN USE RECUP LATER                    
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         XC    BOELEM,BOELEM                                                    
*                                                                               
         ZIC   R3,SVPARMS2+3         YES, FIND OUT WHICH BOOK FIELD             
         SH    R3,=Y(PRMCOSTQ)         BY SUBTRACTING PRIME EQUATE              
         CLC   LSROWREP,=H'1'      LINKED TSAR LINE 1?                          
         BNE   VALMCS20            NO                                           
*******************                                                             
** VALIDATE COST **                                                             
*******************                                                             
         CLI   FVIFLD,C'%'         REFRESH REQUEST?                             
         BNE   VALMCS06                                                         
*                                                                               
         LTR   R3,R3               FIRST COST?                                  
         BZ    VALMCSX             YES - SKIP                                   
*                                                                               
         TM    TLRDTNC1,X'80'      N/A?                                         
         BNZ   VALMCSX             YES - SKIP                                   
*                                                                               
         LR    R0,RA                                                            
         AH    R0,=Y(MINCOSTS-TWAD)                                             
         LR    R4,R3                                                            
         MH    R4,=Y(L'MINCOST)                                                 
         AR    R4,R0                                                            
         USING CSTLIN,R4                                                        
*                                                                               
         CLI   CSLNPBC,0           % OF BASE COST?                              
         BE    VALMCSX             NO                                           
*                                                                               
VALMCS02 ZIC   R0,1(R6)            BUMP TO THE NEXT ELEMENT IN CLUSTER          
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    VALMCS04            YES, NO SUPLEMENTAL COST ELEMENT             
*                                                                               
         CLI   0(R6),RPRCSELQ      SUPLEMENTAL COST ELEMENT?                    
         BL    VALMCS02            NO - NOT THERE YET                           
         BH    VALMCS04            NO - PAST IT                                 
*                                                                               
         ZIC   R1,1(R6)            COPY AND DELETE                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)                                                  
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
VALMCS04 MVI   BOELEM,RPRCSELQ                                                  
         MVI   BOELEM+1,RPRCSLNQ                                                
*                                                                               
         BCTR  R3,0                                                             
         MH    R3,=Y(L'RPRCSSC2+L'RPRCSNC2)                                     
         LA    R3,BOELEM+(RPRCSSC2-RPRCSELD)(R3)                                
*                                                                               
         ICM   R1,15,TLRDTNC1                                                   
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
         CP    BODUB1,=P'2000000000'                                            
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(699)                                                
         B     EXITL                                                            
*                                                                               
         SRP   BODUB1,64-2,5                                                    
         CVB   R1,BODUB1                                                        
         MH    R1,=H'100'          MAKE IT DOLLARS                              
         ST    R1,L'RPRCSSC2(R3)                                                
         DROP  R4                                                               
*                                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
         BAS   RE,FROMAIO5                                                      
         BAS   RE,MINIOWRT                                                      
         B     VALMCSX                                                          
*                                                                               
VALMCS06 CLC   =C'NA',FVIFLD                                                    
         BNE   *+14                                                             
         MVC   BOFULL1,=X'80000000'                                             
         B     VALMCS07                                                         
*                                                                               
         ZIC   R0,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,FVIFLD,(R0)                                      
         CLI   0(R1),X'FF'                                                      
         BE    EXITNOTN            NOT NUMERIC                                  
         MVC   BOFULL1,BODMCB+4                                                 
*                                                                               
VALMCS07 DS    0H                                                               
         CLM   R3,1,WHICHCST                                                    
         BNE   VALMCS08                                                         
*                                                                               
         MVC   TLRDTNCU,BOFULL1                                                 
         OI    TLRCFLG1,TCF1COST   THE COST WAS CHANGED                         
         TM    WHATSORT+SRTCPPB,SRTCPPA                                         
         BNO   *+8                                                              
         OI    TLRCFLG1,TCF1KCHG   YES, TSAR KEY DEFINITELY CHANGED             
*                                                                               
         TM    WHATSORT+SRTCSTB,SRTCSTA                                         
         BNO   *+8                                                              
         OI    TLRCFLG1,TCF1KCHG   YES, TSAR KEY DEFINITELY CHANGED             
*                                                                               
VALMCS08 DS    0H                                                               
         LTR   R3,R3               PRIMARY COST?                                
         BNZ   VALMCS10            NO                                           
*                                                                               
         MVC   TLRDTNC1,BOFULL1    UPDREC DOES MINIO WRT                        
         L     R6,MINELEM          USE MOST RECENT COST                         
         USING RPRDTELD,R6                                                      
         MVC   RPRDTSC1,TLRDTSC1                                                
         MVC   RPRDTNC1,TLRDTNC1                                                
         B     VALMCS14                                                         
         DROP  R6                                                               
*                                                                               
VALMCS10 ZIC   R0,1(R6)            BUMP TO THE NEXT ELEMENT IN CLUSTER          
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    VALMCS12            YES, NO SUPLEMENTAL COST ELEMENT             
*                                                                               
         CLI   0(R6),RPRCSELQ      SUPLEMENTAL COST ELEMENT?                    
         BL    VALMCS10            NO - NOT THERE YET                           
         BH    VALMCS12            NO - PAST IT                                 
*                                                                               
         ZIC   R1,1(R6)            COPY AND DELETE                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)                                                  
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
VALMCS12 MVI   BOELEM,RPRCSELQ                                                  
         MVI   BOELEM+1,RPRCSLNQ                                                
*                                                                               
         BCTR  R3,0                                                             
         MH    R3,=Y(L'RPRCSSC2+L'RPRCSNC2)                                     
         LA    R1,BOELEM+(RPRCSSC2-RPRCSELD)(R3)                                
         MVC   L'RPRCSSC2(L'RPRCSNC2,R1),BOFULL1                                
*                                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
         BAS   RE,FROMAIO5                                                      
VALMCS14 BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VALMCSX                                                          
*********************                                                           
** VALIDATE RATING **                                                           
*********************                                                           
VALMCS20 DS    0H                                                               
         CLC   LSROWREP,=H'2'      LINKED TSAR LINE 2?                          
         BNE   VALMCS30            NO                                           
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(MINCOSTS-TWAD)                                             
         LR    R0,R3                                                            
         MH    R0,=Y(L'MINCOST)                                                 
         AR    RF,R0                                                            
         USING CSTLIN,RF                                                        
*                                                                               
         MVC   BOBYTE1,SAVBK       DEFAULT INTERNAL ORDER #                     
*                                                                               
         CLI   CSLNLBK,0                                                        
         BE    *+10                                                             
         MVC   BOBYTE1,CSLNLBK     INTERNAL ORDER # FROM COST HEADER            
         DROP  RF                                                               
*                                                                               
         XC    BODMCB(6*L'BODMCB),BODMCB      PARAMS FOR A FETCH                
*                                                                               
         LA    RF,SAVBKS                                                        
         USING BOOKLIN,RF                                                       
VLMCRT3  CLC   BOBYTE1,BKLNIORD                                                 
         BE    VLMCRT5                                                          
         LA    RF,L'SAVBK(RF)                                                   
         LA    R0,SAVBKS+L'SAVBKS                                               
         CR    RF,R0                                                            
         BL    VLMCRT3                                                          
         DC    H'0'                                                             
         DROP  RF                                                               
*                                                                               
VLMCRT5  DS    0H                                                               
         ST    RF,BODMCB+12                                                     
         NI    MISCFLG2,FF-MF2TMPBT   NOT PRIME BOOK                            
         LA    R0,SAVBKS                                                        
         CR    RF,R0                                                            
         BNE   *+8                                                              
         OI    MISCFLG2,MF2TMPBT      PRIME BOOK                                
*                                                                               
         USING RPRDVELD,R6                                                      
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
VLMCRT10 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BNE   VLMCRT15                                                         
         OI    MISCFLG1,MF1TMPBT   YES, WE NEED A DEMO VALUE ELEMENT            
         LR    R0,R6                                                            
         LA    R6,BOELEM              BUILD ELEM TO BE ADDED TO CLUSTER         
         XC    BOELEM,BOELEM                                                    
         MVI   RPRDVEL,RPRDVELQ                                                 
         MVC   RPRDVBNM,BOBYTE1    INTERNAL ORDER NUMBER OF BOOK                
         MVI   RPRDVLEN,RPRDVOVQ   SINCE NO DEMO VALUES, MINIMUM LENGTH         
         LR    R6,R0                                                            
         B     VLMCRT20                                                         
*                                                                               
VLMCRT15 CLI   0(R6),RPRDVELQ      DEMO VALUE ELEMENT?                          
         BNE   VLMCRT10                                                         
         CLC   RPRDVBNM,BOBYTE1    INTERNAL ORDER NUMBER THE SAME?              
         BNE   VLMCRT10            NO, WE NEED A DIFFERENT BOOK                 
         XC    BOELEM,BOELEM                                                    
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)     COPY ELEM IN CASE IT GETS BIGGER             
*                                                                               
VLMCRTD  USING RPRDVELD,BOELEM                                                  
VLMCRT20 DS    0H                                                               
         MVC   BOBYTE1,SAVDMO+(DMLNIORD-DEMOLIN)                                
         LA    R0,SAVDMO+(DMLNDEMO-DEMOLIN)                                     
         ST    R0,BODMCB+16                                                     
*                                                                               
         OI    FVIFLD,C' '         UPPERCASE                                    
         CLI   FVIFLD,C'X'         CHECK FOR FETCH                              
         BE    VLMCRT30                                                         
         SPACE 2                                                                
***************************                                                     
** HANDLE USER OVERRIDES **                                                     
***************************                                                     
         ZIC   R0,FVILEN                                                        
         LA    R4,FVIFLD                                                        
         CLI   FVIFLD,C'*'         CHECK FOR OVERRIDE '*'                       
         BNE   VLMCRT22                                                         
         LA    R4,1(R4)                                                         
         SH    R0,=H'1'                                                         
*                                                                               
VLMCRT22 GOTO1 VCASHVAL,BODMCB,(1,(R4)),(R0)    1 DECIMAL PLACE                 
         CLI   0(R1),X'FF'                                                      
         BE    EXITNOTN            NOT NUMERIC                                  
*                                                                               
         ZIC   R1,BOBYTE1          STORE DEMO VALUE IN CORRECT POSTION          
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,VLMCRTD.RPRDVDMO(R1)                                          
         LR    RF,R1               KEEP A COPY OF THIS ADDRESS                  
*                                                                               
         TM    FILTFLG1,FF1LVLS    DISPLAY LEVELS?                              
         BZ    *+12                NO                                           
         LA    R1,8(R1)                                                         
         B     VLMCRT24                                                         
*                                                                               
         TM    FILTFLG1,FF1SHRS    DISPLAY SHARES?                              
         BZ    *+8                 NO                                           
         LA    R1,4(R1)                                                         
*                                                                               
VLMCRT24 DS    0H                                                               
         TM    MISCFLG2,MF2TMPBT      PRIME BOOK                                
         BZ    VLMCRT25               NO SOME OTHER BOOK                        
         MVC   TLROLDMO,0(R1)      YES, SAVE WHAT IT USED TO BE                 
         MVC   TLRNWDMO,BODMCB+4        AND WHAT IT WILL BE                     
         OI    TLRNWDMO,X'80'      DEFINITELY AN OVERRIDE                       
*                                                                               
         TM    FILTFLG1,FF1SHRS+FF1LVLS                                         
         BNZ   VLMCRT25                                                         
         MVC   TLROLRTG,0(R1)      YES, SAVE WHAT IT USED TO BE                 
         MVC   TLRNWRTG,BODMCB+4        AND WHAT IT WILL BE                     
         OI    TLRNWRTG,X'80'      DEFINITELY AN OVERRIDE                       
*                                                                               
VLMCRT25 DS    0H                                                               
         MVC   0(4,R1),BODMCB+4    JUST CHANGE THE CHANGED DEMO                 
         OI    0(R1),X'80'         DEFINITELY AN OVERRIDE                       
***************                                                                 
* HAVE TO CALCULATE THE RATING/SHARE BASED ON WHAT CHANGED                      
***************                                                                 
         TM    FILTFLG1,FF1SHRS+FF1LVLS    CHANGED THE RATING?                  
         BNZ   VLMCRT26                    NO                                   
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
         BNE   VLMCRT50            DIVIDING BY 0, LEAVE SHR ALONE               
         LR    RF,R0                                                            
         SRP   PCKOF16B,64-1,5                                                  
*                                                                               
         ZAP   BODUB1,PCKOF16B                                                  
         CVB   RE,BODUB1                                                        
         ST    RE,4(RF)            STORE SHARE VALUE IN CORRECT PLACE           
         OI    4(RF),X'80'         DEFINITELY AN OVERRIDE                       
         B     VLMCRT50                                                         
*********                                                                       
* RATING = SHARE * LEVEL                                                        
*********                                                                       
VLMCRT26 L     RE,BODMCB+4                                                      
         CVD   RE,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         MVC   BOFULL1,4(RF)       SHARE VALUE                                  
         TM    FILTFLG1,FF1LVLS    DID WE CHANGE LEVEL VALUE?                   
         BNZ   *+10                YES, WE GOT SHARE TO MULT WITH               
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
         TM    MISCFLG2,MF2TMPBT   PRIME BOOK?                                  
         BZ    VLMCRT27            NO, SOME OTHER BOOK                          
         MVC   TLROLRTG,0(RF)      YES, SAVE WHAT IT USED TO BE                 
         STCM  RE,15,TLRNWRTG         AND WHAT IT WILL BE                       
         OI    TLRNWRTG,X'80'      DEFINITELY AN OVERRIDE                       
*                                                                               
VLMCRT27 DS    0H                                                               
         ST    RE,0(RF)            STORE RATING VALUE IN CORRECT PLACE          
         OI    0(RF),X'80'         DEFINITELY AN OVERRIDE                       
         B     VLMCRT50                                                         
         EJECT                                                                  
****************************                                                    
** USER REQUEST FOR FETCH **                                                    
****************************                                                    
VLMCRT30 ZIC   R1,TLRDTSTA                                                      
         SR    R0,R0                                                            
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R0,SAVSTAS+(STLNSTA-STALIN)(R1)      A(STATION TEXT)             
         ST    R0,BODMCB                                                        
*                                                                               
         CLC   TLRDTINM,BCSPACES                                                
         BNH   VLMCRT32                                                         
*                                                                               
         LA    R0,TLRDTINM         A(INVENTORY #)                               
         ST    R0,BODMCB+4                                                      
*                                                                               
         LA    R0,TLRDTEFF         A(EFFECTIVE DATES)                           
         ST    R0,BODMCB+8                                                      
         B     VLMCRT33                                                         
*                                                                               
VLMCRT32 DS    0H                                                               
         L     RE,AIO5                                                          
         LA    RE,RPROR1ST-RPROHDRD(RE)                                         
         ST    RE,BODMCB+4                                                      
         MVI   BODMCB+4,X'80'                                                   
*                                                                               
VLMCRT33 GOTOX (FETCHQ,AREPRO01),BODMCB                                         
*                                                                               
         ZIC   R1,BOBYTE1          STORE DEMO VALUE IN CORRECT POSTION          
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,VLMCRTD.RPRDVDMO(R1)                                          
*                                                                               
         TM    FILTFLG1,FF1LVLS    DISPLAY LEVELS?                              
         BZ    *+16                NO                                           
         LA    RE,8(R1)                                                         
         LA    RF,BODMCB+8                                                      
         B     VLMCRT34                                                         
*                                                                               
         TM    FILTFLG1,FF1SHRS    DISPLAY SHARES?                              
         BZ    *+16                NO                                           
         LA    RE,4(R1)                                                         
         LA    RF,BODMCB+4                                                      
         B     VLMCRT34                                                         
*                                                                               
         LA    RE,0(R1)            DISPLAY RATINGS OTHERWISE                    
         LA    RF,BODMCB                                                        
VLMCRT34 DS    0H                                                               
         TM    MISCFLG2,MF2TMPBT   PRIME BOOK?                                  
         BZ    VLMCRT36            NO, SOME OTHER BOOK                          
         MVC   TLROLDMO,0(RE)      YES, SAVE WHAT IT USED TO BE                 
         MVC   TLRNWDMO,0(RF)           AND WHAT IT WILL BE                     
*                                                                               
         TM    FILTFLG1,FF1SHRS+FF1LVLS                                         
         BNZ   VLMCRT36                                                         
         MVC   TLROLRTG,0(RE)      YES, SAVE WHAT IT USED TO BE                 
         MVC   TLRNWRTG,0(RF)           AND WHAT IT WILL BE                     
*                                                                               
VLMCRT36 DS    0H                                                               
         MVC   0(L'RPRDVDMO,R1),BODMCB    COPY RTG/SHR/HPT FROM FETCH           
         SPACE 2                                                                
*                                                                               
VLMCRT50 LA    R1,L'RPRDVDMO(R1)   R1 = A(ELEMENT BEYOND THAT DEMO)             
         LA    R0,VLMCRTD.RPRDVEL                                               
         SR    R1,R0                                                            
         CLM   R1,1,VLMCRTD.RPRDVLEN                                            
         BL    *+8                                                              
         STC   R1,VLMCRTD.RPRDVLEN                                              
*                                                                               
         TM    MISCFLG1,MF1TMPBT   DO WE NEED A NEW DEMO VALUE ELEMENT?         
         BNZ   VLMCRT56            YES, ADD IT                                  
*                                                                               
         CLM   R1,1,RPRDVLEN       SAME LENGTH AS BEFORE?                       
         BNE   VLMCRT54            NO, NEED TO REMOVE AND ADD                   
         ZIC   R1,RPRDVLEN         YES                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RPRDVEL,VLMCRTD.RPRDVEL                                          
         B     VLMCRT58                                                         
         DROP  VLMCRTD                                                          
*                                                                               
VLMCRT54 GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
VLMCRT56 GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
*                                                                               
VLMCRT58 BAS   RE,FROMAIO5                                                      
         BAS   RE,MINIOWRT                                                      
*                                                                               
VLMCRT60 TM    WHATSORT+SRTRTGB,SRTRTGA                                         
         BNO   *+8                                                              
         OI    TLRCFLG1,TCF1KCHG   YES, TSAR KEY DEFINITELY CHANGED             
*                                                                               
         TM    WHATSORT+SRTCPPB,SRTCPPA                                         
         BNO   *+8                                                              
         OI    TLRCFLG1,TCF1KCHG   YES, TSAR KEY DEFINITELY CHANGED             
*                                                                               
VLMCRTX  B     VALMCSX                                                          
******************                                                              
** VALIDATE CPP **                                                              
******************                                                              
VALMCS30 DS    0H                                                               
         CLC   LSROWREP,=H'3'      LINKED TSAR LINE 3?                          
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(MINCOSTS-TWAD)                                             
         LR    R0,R3                                                            
         MH    R0,=Y(L'MINCOST)                                                 
         AR    RF,R0                                                            
         USING CSTLIN,RF                                                        
*                                                                               
         MVC   BOBYTE1,SAVBK       DEFAULT INTERNAL ORDER #                     
*                                                                               
         CLI   CSLNLBK,0                                                        
         BE    *+10                                                             
         MVC   BOBYTE1,CSLNLBK     INTERNAL ORDER # FROM COST HEADER            
         XC    PREVRTG,PREVRTG                                                  
         DROP  RF                                                               
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDVELD,R6                                                      
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
VLMCCP10 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    EXITNV              YES, NO RATING                               
*                                                                               
         CLI   RPRDVEL,RPRDVELQ    DEMO VALUE ELEMENT?                          
         BNE   VLMCCP10                                                         
         CLC   RPRDVBNM,BOBYTE1    SAME INTERNAL ORDER # ?                      
         BNE   VLMCCP10            NO, KEEP SEARCHING FOR IT                    
*                                                                               
         MVC   BOBYTE1,SAVDMO+(DMLNIORD-DEMOLIN)                                
*                                                                               
         ZIC   R1,BOBYTE1                                                       
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,RPRDVDMO(R1)                                                  
*                                                                               
         LR    RE,R1               ARE WE BEYOND THE ELEMENT?                   
         LA    R0,RPRDVEL                                                       
         SR    RE,R0                                                            
         CLM   RE,1,RPRDVLEN                                                    
         BNL   EXITNV              YES, NEED DEMO RATING TO CHANGE COST         
*                                                                               
         MVC   BOFULL1,0(R1)       RATING WITHOUT THE OVERRIDE BIT              
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ                                                
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5                                                    
         SRP   BODUB1,1,0                                                       
         CVB   R1,BODUB1                                                        
         STCM  R1,15,PREVRTG       NOW WE HAVE A RATING TO BASE WITH            
*                                                                               
         CLC   =C'NA',FVIFLD      N/A?                                          
         BNE   *+12                                                             
         L     R1,=X'80000000'                                                  
         B     VLMCCP14                                                         
*                                                                               
         ZIC   R0,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,FVIFLD,(R0)                                      
         CLI   0(R1),X'FF'                                                      
         BE    EXITNOTN            NOT NUMERIC                                  
*                                                                               
         MVC   BOFULL1,BODMCB+4                                                 
*                                                                               
         ICM   R0,15,PREVRTG       COST = CPP * RATING                          
         CVD   R0,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         SRP   PCKOF16B,64-1,5                                                  
         ZAP   BODUB1,PCKOF16B                                                  
         CVB   R1,BODUB1                                                        
*                                                                               
VLMCCP14 DS    0H                                                               
         CLM   R3,1,WHICHCST                                                    
         BNE   *+8                                                              
         STCM  R1,15,TLRDTNCU                                                   
*                                                                               
         LTR   R3,R3               PRIME COST?                                  
         BNZ   VLMCCP21            NO                                           
*                                                                               
         STCM  R1,15,TLRDTNC1                                                   
         L     R6,MINELEM          USE MOST RECENT COST                         
         USING RPRDTELD,R6                                                      
         MVC   RPRDTSC1,TLRDTSC1                                                
         MVC   RPRDTNC1,TLRDTNC1                                                
         B     VLMCCP28                                                         
         DROP  R6                                                               
*                                                                               
VLMCCP21 L     R6,MINELEM                                                       
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
VLMCCP22 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    VLMCCP24            YES, NO SUPLEMENTAL COST ELEMENT             
*                                                                               
         CLI   0(R6),RPRCSELQ      SUPLEMENTAL COST ELEMENT?                    
         BNE   VLMCCP22            NO                                           
*                                                                               
         BCTR  R3,0                                                             
         MH    R3,=Y(L'RPRCSSC2+L'RPRCSNC2)                                     
         LA    R3,RPRCSSC2-RPRCSELD(R3,R6)                                      
         STCM  R1,15,L'RPRCSSC2(R3)                                             
         B     VLMCCP28                                                         
*                                                                               
VLMCCP24 DS    0H                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
VLMCCP26 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    *+12                YES - PUT NEW ELEMENT HERE                   
         CLI   0(R6),RPRCSELQ      SUPLEMENTAL COST ELEMENT?                    
         BNH   VLMCCP26            NO                                           
*                                                                               
         XC    BOELEM,BOELEM                                                    
         LR    RF,R6                                                            
         LA    R6,BOELEM                                                        
         USING RPRCSELD,R6                                                      
         MVI   RPRCSEL,RPRCSELQ                                                 
         MVI   RPRCSLEN,RPRCSLNQ                                                
         BCTR  R3,0                                                             
         MH    R3,=Y(L'RPRCSSC2+L'RPRCSNC2)                                     
         LA    R3,RPRCSSC2-RPRCSELD(R3,R6)                                      
         STCM  R1,15,L'RPRCSSC2(R3)                                             
*                                                                               
         LR    R6,RF                                                            
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
         BAS   RE,FROMAIO5                                                      
*                                                                               
VLMCCP28 DS    0H                                                               
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VLMCCPX  DS    0H                                                               
*                                                                               
VALMCSX  B     EXITOK                                                           
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY DEMO FIELD HEADLINE FOR THE COLUMN                                    
***********************************************************************         
RTNGHDNG DS    0H                                                               
         NMOD1 0,**HRTG**                                                       
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
         TM    MISCFLG1,MF1PFRET   COMING BACK FROM A SESSION                   
         BNZ   HDLRTG00            YES, DON'T BOTHER                            
*                                                                               
         CLI   SVPARMS2+3,PRMDEMOQ  FIRST RATING COLUMN?                        
         BE    HDLRTG0A                                                         
         CLI   SVPARMS2+3,PRMBKQ                                                
         BE    HDLRTG0A                                                         
         CLI   SVPARMS2+3,PRMBK2Q                                               
         BNE   HDLRTG00                                                         
*                                                                               
HDLRTG0A BAS   RE,REORGPRM          YES, MUST REORDER 1ST TIME                  
*                                                                               
HDLRTG00 L     R1,SVPARMS6                                                      
         MVI   0(R1),C'-'                                                       
         MVC   1(6,R1),0(R1)                                                    
*                                                                               
         L     R2,SVPARMS5                                                      
         ZIC   R1,SVPARMS2+3       FIELD NUMBER                                 
*                                                                               
         CLI   SVPARMS2+3,PRMDEMOQ BOOK?                                        
         BL    HDLRTG02            YES                                          
         CLI   SVPARMS2+3,PRMBK2Q  DEMO?                                        
         BL    HDLRTG10            YES                                          
***************                                                                 
* BOOK NAME FOR THE RATING COLUMN                                               
***************                                                                 
         SH    R1,=Y(PRMBK2Q)      OFFSETED BY THE BIG PRIME BOOK               
         LR    R3,R1                                                            
         B     HDLRTG04                                                         
*                                                                               
HDLRTG02 SH    R1,=Y(PRMBKQ)       OFFSETED BY THE PRIME BOOK                   
         LR    R3,R1                                                            
*                                                                               
HDLRTG04 MH    R3,=Y(L'SAVBK)                                                   
         LA    R3,SAVBKS(R3)                                                    
         LR    R4,R1                                                            
         MH    R4,=Y(L'SAVLBL)                                                  
         LA    R4,SAVLBLS(R4)                                                   
*                                                                               
         OC    0(L'SAVLBL,R4),0(R4)   USER DEFINED BOOK?                        
         BZ    *+14                                                             
         MVC   0(5,R2),0(R4)                                                    
         B     HEDLRTGX                                                         
*                                                                               
         USING BOOKLIN,R3                                                       
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(3),BKLNBK                                                
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(2),=X'0B07'      PUT OUT SOURCE                           
         MVC   BOELEM+2(1),BKLNSPBK                                             
         LA    RE,20+8                                                          
         TM    FVIHDR+1,X'02'       EXT FIELD HDR?                              
         BNO   *+8                                                              
         AH    RE,=H'8'                                                         
         STC   RE,FVIHDR                                                        
         DROP  R3                                                               
         GOTOX (UNBOOKQ,AREPRO01),BODMCB,(1,BOWORK1),FVIHDR,           X        
               (C'L',BOELEM),(C'+',=CL6' ')                                     
*                                                                               
         ZIC   RE,FVIHDR                                                        
         LA    RE,FVIHDR(RE)                                                    
         TM    FVIHDR+1,X'02'       EXT FIELD HDR?                              
         BNO   *+8                                                              
         SH    RE,=H'8'                                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         CLI   0(RE),C')'                                                       
         BNE   HDLRTG08                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     HDLRTG08                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
HDLRTG08 DS    0H                                                               
         LA    RF,FVIHDR+8                                                      
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),FVIHDR+8                                                 
         B     HEDLRTGX                                                         
***************                                                                 
* DEMO NAME FOR THE RATING COLUMN                                               
***************                                                                 
HDLRTG10 SH    R1,=Y(PRMDEMOQ)     OFFSETED BY THE PRIME DEMO                   
         LR    R3,R1                                                            
         MH    R3,=Y(L'SAVDMO)                                                  
         LA    R3,SAVDMOS(R3)                                                   
         USING DEMOLIN,R3                                                       
*                                                                               
         L     R6,AIO5                                                          
         USING DBLOCK,R6                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOM                                                    
         MVI   DBSELMED,C'T'                                                    
         LA    R5,BOWORK1                                                       
         XC    BOWORK1(50),BOWORK1                                              
         MVC   BOWORK1(L'DMLNDEMO),DMLNDEMO                                     
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         DROP  R6,R3                                                            
*                                                                               
         GOTO1 VDEMOCON,BODMCB,(1,BOWORK1),(9,0(R2)),(0,AIO5)                   
*                                                                               
HEDLRTGX B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY RATING FIELD                                                          
***********************************************************************         
DISRATNG DS    0H                                                               
         NMOD1 0,**DRTG**                                                       
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
         BAS   RE,REORGPRM         REORG  SAVBKS, SAVLBLS, & SAVDMOS            
*                                                                               
         USING TLSTD,R2                                                         
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         TM    VIEWFLG1,VF1MLDMO                                                
         BO    *+12                                                             
         TM    VIEWFLG1,VF1SFNOT   FOOTNOTES?                                   
         BZ    DSLRTG00            NO                                           
         BAS   RE,CHKVIEW          LAST TSAR LINE?                              
         CLC   BOBYTE1,LSROWREP+1                                               
         BE    DISFNOTE            YES - DISPLAY FOOTNOTE                       
*                                                                               
DSLRTG00 GOTO1 GCLSTTSR,BODMCB,(R2)  GET CLUSTER FOR THE TSAR RECORD            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1                 PRIME IS DEFAULT                           
         CLI   SVPARMS2+3,PRMDEMOQ   BOOK FIELD HERE?                           
         BL    DSLRTG02              YES                                        
         CLI   SVPARMS2+3,PRMBK2Q    DEMO FIELD HERE?                           
         BL    DSLRTG04              YES                                        
         IC    R1,SVPARMS2+3         BIG BOOK FIELD                             
         SH    R1,=Y(PRMBK2Q)                                                   
         B     DSLRTG04                                                         
DSLRTG02 IC    R1,SVPARMS2+3         YES, FIND OUT WHICH BOOK FIELD             
         SH    R1,=Y(PRMBKQ)         BY SUBTRACTING PRIME EQUATE                
DSLRTG04 LR    R3,R1                                                            
         MH    R3,=Y(L'SAVBK)                                                   
         LA    R3,SAVBKS(R3)                                                    
         USING BOOKLIN,R3                                                       
         MVC   BOBYTE1,BKLNIORD    INTERNAL ORDER NUMBER OF THE BOOK            
         OC    BKLNUPGD,BKLNUPGD   UPGRADE?                                     
         BNZ   *+16                                                             
         TM    SELPROF+SELDMOVB,SELDMOVA                                        
         BZ    *+8                                                              
         OI    FVATRB,FVAPROT                                                   
         DROP  R3                                                               
*                                                                               
         XC    PREVRTG,PREVRTG     NO RATING YET                                
         L     R6,MINELEM                                                       
         USING RPRDVELD,R6                                                      
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
DSLRTG10 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         MVI   FVIFLD,C'0'         IF NONE THEN SHOW 0                          
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    DISLRTGX                                                         
*                                                                               
         CLI   RPRDVEL,RPRDVELQ    DEMO VALUE ELEMENT?                          
         BNE   DSLRTG10                                                         
         CLC   RPRDVBNM,BOBYTE1    INTERNAL ORDER NUMBER THE SAME?              
         BNE   DSLRTG10            NO, WE NEED A DIFFERENT BOOK                 
*                                                                               
         SR    R1,R1                 PRIME IS DEFAULT                           
         CLI   SVPARMS2+3,PRMDEMOQ   DEMO FIELD HERE?                           
         BL    *+20                  NO, WE GOT A BOOK FIELD                    
         CLI   SVPARMS2+3,PRMBK2Q    BIG BOOK FIELD?                            
         BNL   *+12                  NO, WE GOT A DEMO FIELD                    
         IC    R1,SVPARMS2+3         YES, FIND OUT WHICH DEMO FIELD             
         SH    R1,=Y(PRMDEMOQ)       BY SUBTRACTING PRIME EQUATE                
         LR    R3,R1                                                            
         MH    R3,=Y(L'SAVDMO)                                                  
         LA    R3,SAVDMOS(R3)                                                   
         MVC   BOBYTE1,DMLNIORD-DEMOLIN(R3)                                     
*                                                                               
         ZIC   R1,BOBYTE1                                                       
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,RPRDVDMO(R1)                                                  
*                                                                               
         LR    RE,R1               ARE WE BEYOND THE ELEMENT?                   
         LA    R0,RPRDVEL                                                       
         SR    RE,R0                                                            
         CLM   RE,1,RPRDVLEN                                                    
         BNL   DISLRTGX            YES, NOTHING TO SHOW                         
*                                                                               
         CLC   LSROWREP,=H'2'      SHARE LINE?                                  
         BNE   *+12                NO                                           
         LA    R1,4(R1)                                                         
         B     DISRTG12                                                         
         CLC   LSROWREP,=H'3'      LEVEL LINE?                                  
         BNE   *+12                NO                                           
         LA    R1,8(R1)                                                         
         B     DISRTG12                                                         
         TM    FILTFLG1,FF1LVLS    DISPLAY LEVELS?                              
         BZ    *+12                NO                                           
         LA    R1,8(R1)                                                         
         B     DISRTG12                                                         
         TM    FILTFLG1,FF1SHRS    DISPLAY SHARES?                              
         BZ    *+8                 NO                                           
         LA    R1,4(R1)                                                         
DISRTG12 MVC   BOFULL1,0(R1)                                                    
         LA    R4,FVIFLD                                                        
         MVI   BOBYTE1,0           JRD                                          
         TM    BOFULL1,X'80'       IS THIS A DEMO OVERRIDE?                     
         BZ    DSLRTG15                                                         
         MVI   BOBYTE1,C'*'                                                     
**JRD    MVI   0(R4),C'*'          YES, THEN SHOW AN '*' BEFORE RATING          
**JRD    LA    R4,1(R4)                                                         
         NI    BOFULL1,X'FF'-X'80'   GET RID OF DEMO OVERRIDE BIT               
*                                                                               
DSLRTG15 TM    SAVOPTNS,OPTNDECQ   DISPLAY DEMO PRECISION?                      
         BZ    DSLRTG20            NO                                           
         EDIT  (B4,BOFULL1),(5,0(R4)),1,WRK=BOWORK1,ALIGN=LEFT,        X        
               DUB=BODUB1,ZERO=NOBLANK                                          
         MVC   PREVRTG,BOFULL1     SAVE SO WE CAN CALCULATE CPP LATER           
         B     DSLRTG30                                                         
*                                                                               
DSLRTG20 L     R3,BOFULL1          DON'T DISPLAY DEMO PRECISION                 
         CVD   R3,BODUB1                                                        
         ZAP   PCKOF08B,BODUB1                                                  
         SRP   PCKOF08B,64-1,5     ROUND THE # OFF INSTEAD                      
         EDIT  (P8,PCKOF08B),(5,0(R4)),WRK=BOWORK1,,ALIGN=LEFT,        X        
               DUB=BODUB1,ZERO=NOBLANK                                          
         SRP   PCKOF08B,1,0                                                     
         ZAP   BODUB1,PCKOF08B                                                  
         CVB   R1,BODUB1                                                        
         STCM  R1,15,PREVRTG       SAVE SO WE CAN CALCULATE CPP LATER           
*                                                                               
DSLRTG30 DS    0H                                                               
         CLI   BOBYTE1,C'*'        JRD                                          
         BNE   DISLRTGX                                                         
*                                                                               
         MVC   BOWORK1(5),0(R4)                                                 
         MVI   0(R4),C'*'                                                       
*                                                                               
         CLI   BOWORK1,C' '                                                     
         BNH   DISLRTGX                                                         
*                                                                               
         MVC   1(4,R4),BOWORK1                                                  
*                                                                               
DISLRTGX B     EXITOK                                                           
         DROP  R2,R5,R6                                                         
***********************************************************************         
* DISPLAY MBOOK FOOTNOTE                                                        
***********************************************************************         
DISFNOTE DS    0H                                                               
         OI    FVATRB,FVAPROT                                                   
         MVC   FVIFLD(20),=CL20' '                                              
*                                                                               
         SR    R1,R1                 PRIME IS DEFAULT                           
         CLI   SVPARMS2+3,PRMDEMOQ   BOOK FIELD HERE?                           
         BL    DSLFNT02              YES                                        
         CLI   SVPARMS2+3,PRMBK2Q    DEMO FIELD HERE?                           
         BL    DSLFNT04              YES                                        
         IC    R1,SVPARMS2+3         BIG BOOK FIELD                             
         SH    R1,=Y(PRMBK2Q)                                                   
         B     DSLFNT04                                                         
DSLFNT02 IC    R1,SVPARMS2+3         YES, FIND OUT WHICH BOOK FIELD             
         SH    R1,=Y(PRMBKQ)         BY SUBTRACTING PRIME EQUATE                
DSLFNT04 DS    0H                                                               
         MH    R1,=Y(L'SAVBK)                                                   
         LA    R1,SAVBKS(R1)                                                    
         USING BOOKLIN,R1                                                       
         ZIC   R3,BKLNIORD           INTERNAL ORDER NUMBER OF THE BOOK          
         DROP  R1                                                               
*                                                                               
         L     RE,AIOREC                                                        
DSLFNT06 CLI   0(RE),0                                                          
         BE    DSLFNTX                                                          
         CLM   R3,1,1(RE)           BOOK MATCH?                                 
         BE    DSLFNT08                                                         
         ZIC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         B     DSLFNT06                                                         
*                                                                               
DSLFNT08 DS    0H                                                               
*                                                                               
         ZIC   RF,0(RE)                                                         
         SH    RF,=H'6'            5+1 FOR EX                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),5(RE)                                                  
*                                                                               
         CLI   SVPARMS2+3,PRMBK2Q  BIG FOOTNOTES?                               
         BNL   DSLFNTX             YES                                          
*                                                                               
         CLI   FVIFLD+7,C'/'                                                    
         BNE   *+8                                                              
         MVI   FVIFLD+6,C'/'                                                    
*                                                                               
DSLFNTX  B     EXITOK                                                           
         LTORG                                                                  
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
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,(R2)   GET CLUSTER FOR THE TSAR RECORD           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,INTOAIO5         SO WE CAN USE RECUP LATER                    
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         USING RPRDVELD,R6                                                      
         NI    MISCFLG1,X'FF'-MF1TMPBT  USED TO FLG IF WE NEED DEMO VAL         
*                                                                               
         SR    R1,R1                 PRIME IS DEFAULT                           
         CLI   SVPARMS2+3,PRMDEMOQ   BOOK FIELD HERE?                           
         BL    VLLRTG02              YES                                        
         CLI   SVPARMS2+3,PRMBK2Q    DEMO FIELD HERE?                           
         BL    VLLRTG04              YES                                        
         IC    R1,SVPARMS2+3         BIG BOOK FIELD                             
         SH    R1,=Y(PRMBK2Q)                                                   
         B     VLLRTG04                                                         
VLLRTG02 IC    R1,SVPARMS2+3         YES, FIND OUT WHICH BOOK FIELD             
         SH    R1,=Y(PRMBKQ)         BY SUBTRACTING PRIME EQUATE                
VLLRTG04 LR    R3,R1                                                            
         MH    R3,=Y(L'SAVBK)                                                   
         LA    R3,SAVBKS(R3)                                                    
         USING BOOKLIN,R3                                                       
         MVC   BOBYTE2,BKLNIORD    INTERNAL ORDER NUMBER OF BOOK                
*                                                                               
         XC    BODMCB(6*L'BODMCB),BODMCB      PARAMS FOR A FETCH                
*                                                                               
         ST    R3,BODMCB+12        BOOKLINE                                     
         DROP  R3                                                               
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
         MVC   RPRDVBNM,BOBYTE2    INTERNAL ORDER NUMBER OF BOOK                
         MVI   RPRDVLEN,RPRDVOVQ   SINCE NO DEMO VALUES, MINIMUM LENGTH         
         LR    R6,R0                                                            
         B     VLLRTG20                                                         
*                                                                               
VLLRTG15 CLI   RPRDVEL,RPRDVELQ    DEMO VALUE ELEMENT?                          
         BNE   VLLRTG10                                                         
         CLC   RPRDVBNM,BOBYTE2    SAME AS INTERNAL ORD # OF BOOK?              
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
         SR    R1,R1                 PRIME IS DEFAULT                           
         CLI   SVPARMS2+3,PRMDEMOQ   DEMO FIELD HERE?                           
         BL    *+20                  NO, WE GOT A BOOK FIELD                    
         CLI   SVPARMS2+3,PRMBK2Q    BIG BOOK FIELD?                            
         BNL   *+12                  NO, WE GOT A DEMO FIELD                    
         IC    R1,SVPARMS2+3         YES, FIND OUT WHICH DEMO FIELD             
         SH    R1,=Y(PRMDEMOQ)       BY SUBTRACTING PRIME EQUATE                
         LR    R3,R1                                                            
         MH    R3,=Y(L'SAVDMO)                                                  
         LA    R3,SAVDMOS(R3)                                                   
         USING DEMOLIN,R3                                                       
         MVC   BOBYTE1,DMLNIORD                                                 
         LA    R0,DMLNDEMO         DEMO ADDRESS FOR FETCH                       
         ST    R0,BODMCB+16                                                     
         DROP  R3                                                               
*                                                                               
         OI    FVIFLD,C' '         UPPERCASE                                    
         CLI   FVIFLD,C'X'         CHECK FOR FETCH                              
         BE    VLLRTG30                                                         
         CLI   FVIFLD,C'+'         CHECK FOR PRINT BOOK                         
         BE    VLLRTG50                                                         
         CLI   FVIFLD,C'-'         CHECK FOR SUPPRESS BOOK                      
         BE    VLLRTG50                                                         
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
         ZIC   R1,BOBYTE1          STORE DEMO VALUE IN CORRECT POSTION          
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,VLLRTGD.RPRDVDMO(R1)                                          
         LR    RF,R1               KEEP A COPY OF THIS ADDRESS                  
*                                                                               
         CLC   LSROWREP,=H'2'      SHARE LINE?                                  
         BNE   *+12                NO                                           
         LA    R1,4(R1)                                                         
         B     VLLRTG24                                                         
*                                                                               
         CLC   LSROWREP,=H'3'      LEVEL LINE?                                  
         BNE   *+12                NO                                           
         LA    R1,8(R1)                                                         
         B     VLLRTG24                                                         
*                                                                               
         TM    FILTFLG1,FF1LVLS    DISPLAY LEVELS?                              
         BZ    *+12                NO                                           
         LA    R1,8(R1)                                                         
         B     VLLRTG24                                                         
*                                                                               
         TM    FILTFLG1,FF1SHRS    DISPLAY SHARES?                              
         BZ    *+8                 NO                                           
         LA    R1,4(R1)                                                         
*                                                                               
VLLRTG24 DS    0H                                                               
         CLI   SVPARMS2+3,PRMBKQ     AFFECTING PRIME BOOK                       
         BE    *+20                                                             
         CLI   SVPARMS2+3,PRMBK2Q                                               
         BE    *+12                                                             
         CLI   SVPARMS2+3,PRMDEMOQ          OR PRIME DEMO FIELD                 
         BNE   VLLRTG25            NO, SOME OTHER RATING                        
         MVC   TLROLDMO,0(R1)      YES, SAVE WHAT IT USED TO BE                 
         MVC   TLRNWDMO,BODMCB+4        AND WHAT IT WILL BE                     
         OI    TLRNWDMO,X'80'      DEFINITELY AN OVERRIDE                       
*                                                                               
         CLC   LSROWREP,=H'1'      RATING LINE?                                 
         BNE   VLLRTG25            NO                                           
*                                                                               
         TM    FILTFLG1,FF1SHRS+FF1LVLS                                         
         BNZ   VLLRTG25                                                         
         MVC   TLROLRTG,0(R1)      YES, SAVE WHAT IT USED TO BE                 
         MVC   TLRNWRTG,BODMCB+4        AND WHAT IT WILL BE                     
         OI    TLRNWRTG,X'80'      DEFINITELY AN OVERRIDE                       
*                                                                               
VLLRTG25 DS    0H                                                               
         MVC   0(4,R1),BODMCB+4    JUST CHANGE THE CHANGED DEMO                 
         OI    0(R1),X'80'         DEFINITELY AN OVERRIDE                       
***************                                                                 
* HAVE TO CALCULATE THE RATING/SHARE BASED ON WHAT CHANGED                      
***************                                                                 
         CLC   LSROWREP,=H'1'              RATING LINE?                         
         BNE   VLLRTG26                    NO                                   
         TM    FILTFLG1,FF1SHRS+FF1LVLS    CHANGED THE RATING?                  
         BNZ   VLLRTG26                    NO                                   
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
         CLC   LSROWREP,=H'3'      LEVEL LINE?                                  
         BE    *+18                YES                                          
         TM    FILTFLG1,FF1LVLS    DID WE CHANGE LEVEL VALUE?                   
         BNZ   *+10                YES, WE GOT SHARE TO MULT WITH               
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
         CLI   SVPARMS2+3,PRMBKQ     AFFECTING PRIME BOOK                       
         BE    *+20                                                             
         CLI   SVPARMS2+3,PRMBK2Q                                               
         BE    *+12                                                             
         CLI   SVPARMS2+3,PRMDEMOQ          OR PRIME DEMO FIELD                 
         BNE   VLLRTG27            NO, SOME OTHER RATING                        
         MVC   TLROLRTG,0(RF)      YES, SAVE WHAT IT USED TO BE                 
         STCM  RE,15,TLRNWRTG         AND WHAT IT WILL BE                       
         OI    TLRNWRTG,X'80'      DEFINITELY AN OVERRIDE                       
*                                                                               
VLLRTG27 DS    0H                                                               
         ST    RE,0(RF)            STORE RATING VALUE IN CORRECT PLACE          
         OI    0(RF),X'80'         DEFINITELY AN OVERRIDE                       
*                                                                               
VLLRTG29 LR    R1,RF               RE-ESTABLISH R1 TO BEG OF DEMO               
         B     VLLRTG40                                                         
         EJECT                                                                  
****************************                                                    
** USER REQUEST FOR FETCH **                                                    
****************************                                                    
VLLRTG30 ZIC   R1,TLRDTSTA                                                      
         SR    R0,R0                                                            
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R0,SAVSTAS+(STLNSTA-STALIN)(R1)      A(STATION TEXT)             
         ST    R0,BODMCB                                                        
*                                                                               
         CLC   TLRDTINM,BCSPACES                                                
         BNH   VLLRTG32                                                         
*                                                                               
         LA    R0,TLRDTINM         A(INVENTORY #)                               
         ST    R0,BODMCB+4                                                      
*                                                                               
         LA    R0,TLRDTEFF         A(EFFECTIVE DATES)                           
         ST    R0,BODMCB+8                                                      
         B     VLLRTG33                                                         
*                                                                               
VLLRTG32 DS    0H                                                               
         L     RE,AIO5                                                          
         LA    RE,RPROR1ST-RPROHDRD(RE)                                         
         ST    RE,BODMCB+4                                                      
         MVI   BODMCB+4,X'80'                                                   
*                                                                               
VLLRTG33 GOTOX (FETCHQ,AREPRO01),BODMCB                                         
*                                                                               
         ZIC   R1,BOBYTE1          STORE DEMO VALUE IN CORRECT POSTION          
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,VLLRTGD.RPRDVDMO(R1)                                          
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
         TM    FILTFLG1,FF1LVLS    DISPLAY LEVELS?                              
         BZ    *+16                NO                                           
         LA    RE,8(R1)                                                         
         LA    RF,BODMCB+8                                                      
         B     VLLRTG34                                                         
*                                                                               
         TM    FILTFLG1,FF1SHRS    DISPLAY SHARES?                              
         BZ    *+16                NO                                           
         LA    RE,4(R1)                                                         
         LA    RF,BODMCB+4                                                      
         B     VLLRTG34                                                         
*                                                                               
         LA    RE,0(R1)            DISPLAY RATINGS OTHERWISE                    
         LA    RF,BODMCB                                                        
VLLRTG34 DS    0H                                                               
         CLI   SVPARMS2+3,PRMBKQ     AFFECTING PRIME BOOK                       
         BE    *+20                                                             
         CLI   SVPARMS2+3,PRMBK2Q    AFFECTING PRIME BOOK                       
         BE    *+12                                                             
         CLI   SVPARMS2+3,PRMDEMOQ          OR PRIME DEMO FIELD                 
         BNE   VLLRTG36            NO, SOME OTHER RATING                        
         MVC   TLROLDMO,0(RE)      YES, SAVE WHAT IT USED TO BE                 
         MVC   TLRNWDMO,0(RF)           AND WHAT IT WILL BE                     
*                                                                               
         CLC   LSROWREP,=H'1'      RATING LINE?                                 
         BNE   VLLRTG36            NO                                           
*                                                                               
         TM    FILTFLG1,FF1SHRS+FF1LVLS                                         
         BNZ   VLLRTG36                                                         
         MVC   TLROLRTG,0(RE)      YES, SAVE WHAT IT USED TO BE                 
         MVC   TLRNWRTG,0(RF)           AND WHAT IT WILL BE                     
*                                                                               
VLLRTG36 DS    0H                                                               
         MVC   0(L'RPRDVDMO,R1),BODMCB    COPY RTG/SHR/HPT FROM FETCH           
         SPACE 2                                                                
*********************************                                               
** COMMON DEMO VALUE CHANGE CODE *                                              
*********************************                                               
VLLRTG40 LA    R1,L'RPRDVDMO(R1)   R1 = A(ELEMENT BEYOND THAT DEMO)             
         LA    R0,VLLRTGD.RPRDVEL                                               
         SR    R1,R0                                                            
         CLM   R1,1,VLLRTGD.RPRDVLEN                                            
         BL    *+8                                                              
         STC   R1,VLLRTGD.RPRDVLEN                                              
*                                                                               
         TM    MISCFLG1,MF1TMPBT   DO WE NEED A NEW DEMO VALUE ELEMENT?         
         BNZ   VLLRTG46            YES, ADD IT                                  
*                                                                               
         CLM   R1,1,RPRDVLEN       SAME LENGTH AS BEFORE?                       
         BNE   VLLRTG44            NO, NEED TO REMOVE AND ADD                   
         ZIC   R1,RPRDVLEN         YES                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RPRDVEL,VLLRTGD.RPRDVEL                                          
         B     VLLRTG48                                                         
         DROP  VLLRTGD                                                          
*                                                                               
VLLRTG44 DS    0H                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
VLLRTG46 DS    0H                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
VLLRTG48 DS    0H                                                               
         B     VLLRTG60                                                         
         DROP  R6                                                               
         EJECT                                                                  
**************************************                                          
** USER REQUEST FOR PRINTING CHANGE **                                          
**************************************                                          
VLLRTG50 DS    0H                                                               
         L     RE,AIO5                                                          
         LA    RE,RPROR1ST-RPROKEY(RE)                                          
         USING RPRDTELD,RE                                                      
*                                                                               
         LA    RF,X'80'                                                         
         ZIC   R1,BOBYTE2          INTERNAL ORDER # OF BOOK                     
         SRL   RF,0(R1)                                                         
         STC   RF,BOBYTE1          GOT CORRECT MASK FOR BOOK                    
*                                                                               
         CLI   FVIFLD,C'+'                                                      
         BNE   VLLRTG52                                                         
*                                                                               
         XI    BOBYTE1,X'FF'                                                    
         NC    RPRDTBKS,BOBYTE1    PRINT THIS BOOK                              
         B     VLLRTG58                                                         
*                                                                               
VLLRTG52 DS    0H                                                               
         CLI   FVIFLD,C'-'                                                      
         BNE   VLLRTG54                                                         
*                                                                               
         OC    RPRDTBKS,BOBYTE1    SUPPRESS THIS BOOK                           
         B     VLLRTG58                                                         
*                                                                               
VLLRTG54 DS    0H                                                               
*                                                                               
VLLRTG58 DS    0H                                                               
         MVC   TLRDTBKS,RPRDTBKS                                                
         DROP  RE                                                               
         EJECT                                                                  
***************                                                                 
** FINISH UP **                                                                 
***************                                                                 
VLLRTG60 BAS   RE,FROMAIO5                                                      
         BAS   RE,MINIOWRT                                                      
*                                                                               
         TM    WHATSORT+SRTRTGB,SRTRTGA                                         
         BNO   *+8                                                              
         OI    TLRCFLG1,TCF1KCHG   YES, TSAR KEY DEFINITELY CHANGED             
*                                                                               
         TM    WHATSORT+SRTCPPB,SRTCPPA                                         
         BNO   *+8                                                              
         OI    TLRCFLG1,TCF1KCHG   YES, TSAR KEY DEFINITELY CHANGED             
*                                                                               
VALLRTGX B     EXITOK                                                           
         DROP  R2,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE NEED RATE FIELD                                                      
***********************************************************************         
VALNEDRT DS    0H                                                               
         NMOD1 0,**VNRT**                                                       
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
         TM    TLRCFLG1,TCF1BCPP                                                
         BZ    VLLNRT00                                                         
         CLI   FVILEN,0            USER BLANKED OUT FIELD TO CONTINUE?          
         BE    VALLNRTX            YES, IGNORE IT THEN                          
         B     TABCHNGD            NO, ERROR: BUYER'S CPP WAS CHANGED           
*                                                                               
VLLNRT00 L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,(R2)   GET CLUSTER FOR THE TSAR RECORD           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDVELD,R6                                                      
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
VLLNRT10 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    EXITNV              YES, NEED DEMO RATING                        
*                                                                               
         CLI   RPRDVEL,RPRDVELQ    DEMO VALUE ELEMENT?                          
         BNE   VLLNRT10                                                         
         CLC   RPRDVBNM,SAVBK      SAME INTERNAL ORDER # AS PRIME BOOK?         
         BNE   VLLNRT10            NO, KEEP SEARCHING FOR IT                    
*                                                                               
         ZIC   R1,SAVDMO+(DMLNIORD-DEMOLIN)                                     
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,RPRDVDMO(R1)                                                  
*                                                                               
         LR    RE,R1               ARE WE BEYOND THE ELEMENT?                   
         LA    R0,RPRDVEL                                                       
         SR    RE,R0                                                            
         CLM   RE,1,RPRDVLEN                                                    
         BNL   EXITNV              YES, NEED DEMO RATING TO CHANGE COST         
*                                                                               
         MVC   BOFULL1,0(R1)                                                    
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1                                                       
*                                                                               
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ                                                
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5                                                    
         SRP   BODUB1,1,0                                                       
         CVB   R1,BODUB1                                                        
         STCM  R1,15,PREVRTG       NOW WE HAVE A RATING TO BASE WITH            
*                                                                               
         LTR   R1,R1               ZERO?                                        
         BZ    VALLNRTX            YES, NEED DEMO RATING TO CHANGE COST         
*                                                                               
         ZIC   R0,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,FVIFLD,(R0)                                      
         CLI   0(R1),X'FF'                                                      
         BE    EXITNOTN            NOT NUMERIC                                  
         MVC   BOFULL1,BODMCB+4                                                 
*                                                                               
         ICM   R1,15,BOFULL1       TAB CPP = NEED RATE / RATING                 
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         SRP   PCKOF16B,2,0        MULTIPLY BY 100 TO GET PRECISION             
*                                                                               
         ICM   R1,15,PREVRTG                                                    
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         DP    PCKOF16B,PCKOF06B                                                
*                                                                               
         SRP   PCKOF16B(10),64-1,5   DIVIDE BY 10 AND ROUND                     
*                                                                               
         ZAP   BODUB1,PCKOF16B(10)                                              
         CVB   R1,BODUB1                                                        
         STCM  R1,15,TLRDTTAB      WE GOT THE TAB CPP                           
*                                                                               
VALLNRTX B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET UP TSAR FROM FILE 1                                                       
***********************************************************************         
TSARFILE DS    0H                                                               
         NMOD1 0,**TSRF**                                                       
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
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         XC    TLRAVLS,TLRAVLS                                                  
         L     R6,MINELEM                                                       
TSRAV1   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    TSRAVX                                                           
         CLI   0(R6),RPRAVELQ                                                   
         BNE   TSRAV1                                                           
*                                                                               
         USING RPRAVELD,R6                                                      
         MVC   TLRAVLS,RPRAVALS                                                 
         DROP  R6                                                               
TSRAVX   DS    0H                                                               
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
*                                                                               
         MVC   TLRLEN,=Y(TLRLNQ)                                                
         MVC   TLSDTSTA(L'RPROKMEL-1),2(R6)  KEY OF ELEM W/O ELCODE             
         MVC   TLRDTELC,0(R6)                ELCODE                             
*                                                                               
         ZIC   R1,1(R6)            R1 = L(ELEMENT W/O LENGTH OR ELCODE)         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TLRDTSTA(0),2(R6)                                                
*                                                                               
         MVC   TLRDTSCU,TLRDTSC1                                                
         MVC   TLRDTNCU,TLRDTNC1                                                
*                                                                               
         CLI   WHICHCST,0                                                       
         BE    TSRFIL04                                                         
*                                                                               
         XC    TLRDTSCU,TLRDTSCU   ASSUME ZERO                                  
         XC    TLRDTNCU,TLRDTNCU                                                
*                                                                               
         L     R6,MINELEM                                                       
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
TSRFIL02 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    TSRFIL04            YES, NO SUPLEMENTAL COST ELEMENT             
*                                                                               
         CLI   0(R6),RPRCSELQ      SUPLEMENTAL COST ELEMENT?                    
         BNE   TSRFIL02            NO                                           
*                                                                               
         ZIC   R1,WHICHCST         GET SUPLEMENTAL COST                         
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRCSSC2+L'RPRCSNC2)                                     
         LA    R1,RPRCSSC2-RPRCSELD(R1,R6)                                      
         MVC   TLRDTSCU,0(R1)                                                   
         MVC   TLRDTNCU,L'RPRCSSC2(R1)                                          
*                                                                               
TSRFIL04 DS    0H                                                               
         MVC   TLRDTSCO,TLRDTSCU   OLD COSTS                                    
         MVC   TLRDTNCO,TLRDTNCU                                                
*                                                                               
         ICM   R1,15,TLRDTTAB      CALCULATE TOTAL BUYER CPP                    
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCPP-TWAD)                                             
         AP    0(L'GRNDTCPP,RE),PCKOF16B                                        
*                                                                               
         LR    RE,RA               CALCULATE GRAND TOTAL # OF DETAILS           
         AH    RE,=Y(GRNDTDTL-TWAD)                                             
         AP    0(L'GRNDTDTL,RE),=P'1'                                           
*                                                                               
         ZICM  R1,TLRDTTSP,2       CALCULATE GRAND TOTAL SPOTS                  
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1     COPY THE NUMBER OF SPOTS                     
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTSPT-TWAD)                                             
         AP    0(L'GRNDTSPT,RE),BODUB1                                          
*                                                                               
         ICM   R1,15,TLRDTNCU      CALCULATE GRAND TOTAL COST                   
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCST-TWAD)                                             
*                                                                               
         TM    TLRDTNCU,X'80'      N/A?                                         
         BNZ   *+10                YES - SKIP                                   
         AP    0(L'GRNDTCST,RE),PCKOF16B                                        
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDVELD,R6                                                      
         SR    R0,R0                                                            
*                                                                               
TSRFIL05 IC    R0,1(R6)            <-- BUMP TO NEXT ELEMENT                     
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    TSARFILX            YES, NO DEMO RATING                          
*                                                                               
         CLI   RPRDVEL,RPRDVELQ    DEMO VALUE ELEMENT?                          
         BNE   TSRFIL05                                                         
         CLC   RPRDVBNM,SAVBK      SAME INTERNAL ORDER # AS PRIME BOOK?         
         BNE   TSRFIL05            NO, KEEP SEARCHING FOR IT                    
*                                                                               
         ZIC   R1,SAVDMOS+(DMLNIORD-DEMOLIN)                                    
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,RPRDVDMO(R1)                                                  
*                                                                               
         LR    RE,R1               ARE WE BEYOND THE ELEMENT?                   
         LA    R0,RPRDVEL                                                       
         SR    RE,R0                                                            
         CLM   RE,1,RPRDVLEN                                                    
         BNL   TSARFILX            YES, NO DEMO RATING                          
*                                                                               
         MVC   TLROLRTG,0(R1)      WE HAVE A RATING                             
         MVC   TLRNWRTG,0(R1)                                                   
*                                                                               
         TM    FILTFLG1,FF1LVLS    DISPLAY LEVELS?                              
         BZ    *+12                NO                                           
         LA    R1,8(R1)                                                         
         B     TSRFIL06                                                         
         TM    FILTFLG1,FF1SHRS    DISPLAY SHARES?                              
         BZ    *+8                 NO                                           
         LA    R1,4(R1)                                                         
*                                                                               
TSRFIL06 MVC   TLROLDMO,0(R1)      WE HAVE A DEMO (RTG/SHR/LVL)                 
         MVC   TLRNWDMO,0(R1)                                                   
*                                                                               
         MVC   BOFULL1,TLRNWDMO    RATING WITHOUT THE OVERRIDE BIT              
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ   ROUNDING?                                    
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5       YES                                          
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         ZICM  R1,TLRDTTSP,2       NUMBER OF SPOTS                              
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF06B                                                
*                                                                               
         LR    RE,RA               CALCULATE THE TOTAL DEMO POINTS              
         AH    RE,=Y(GRNDTDMO-TWAD)                                             
         AP    0(L'GRNDTDMO,RE),PCKOF16B                                        
*                                                                               
         MVC   BOFULL1,TLRNWRTG    RATING WITHOUT THE OVERRIDE BIT              
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ   ROUNDING?                                    
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5       YES                                          
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF06B                                                
*                                                                               
         LR    RE,RA               CALCULATE THE TOTAL RATING POINTS            
         AH    RE,=Y(GRNDTRTG-TWAD)                                             
         AP    0(L'GRNDTRTG,RE),PCKOF16B                                        
*                                                                               
TSARFILX DS    0H                  BUILD TSAR KEY AND EXIT                      
         GOTO1 =A(BTSRKEY),BODMCB,(R9),RR=BORELO                                
         B     EXITOK                                                           
         DROP  R6,R3                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD TSAR KEY FROM DATA IN ATLST                                             
***********************************************************************         
BTSRKEY  DS    0H                                                               
         NMOD1 0,**BTSK**                                                       
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
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         LA    R3,SORTCTL          POINT TO SORT CONTROL TABLE                  
*                                                                               
BSK02    DS    0H                                                               
         CLI   0(R3),X'FF'         EOL                                          
         BE    BSKYES                                                           
         L     R2,ATLST                                                         
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
*&&DO*&& B     BSKSEQN                                                          
*                                                                               
BSK04    DS    0H                                                               
         LA    R3,2(R3)            NEXT SORTCTL ENTRY                           
         B     BSK02                                                            
*                                                                               
BSKYES   MVC   TLKSRT,SORTKEY                                                   
         B     EXITOK                                                           
         DROP  R2                                                               
BSKNO    B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD SORT KEY ROUTINES                                                       
***********************************************************************         
BSKDAYTM NTR1                      DAY/TIME                                     
         USING TLSTD,R2                                                         
         ZIC   RF,0(R3)            DISP INTO KEY                                
         LA    RF,SORTKEY(RF)                                                   
         MVC   0(1,RF),TLRDTDAY    DAY                                          
*                                                                               
         XI    0(RF),DAYMASK                                                    
         TM    0(RF),DAYMASK       SAT/SUN ONLY?                                
         BNO   BSKDYTM5            NO                                           
         XI    0(RF),B'00000011'   FLIP THEM                                    
*                                                                               
BSKDYTM5 ZICM  R0,TLRDTTIM,2                                                    
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
         USING TLSTD,R2                                                         
         ZIC   RF,0(R3)            DISP INTO KEY                                
         LA    RF,SORTKEY(RF)                                                   
*                                                                               
         ZIC   R1,TLRDTSTA                                                      
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
         USING TLSTD,R2                                                         
         ZIC   RF,0(R3)            DISP INTO KEY                                
         LA    RF,SORTKEY(RF)                                                   
*                                  FIRST TRY ANY REQUESTED SORT                 
         LA    RE,LDPTSEQS                                                      
         SR    R1,R1                                                            
*                                                                               
BSKDP05  CLC   TLRDTDPT,0(RE)                                                   
         BE    BSKDP10                                                          
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         LA    R0,LDPTSEQS+L'LDPTSEQS                                           
         CR    RE,R0                                                            
         BL    BSKDP05                                                          
*XXXX    MVI   0(RF),X'FF'         NOT FOUND                                    
         MVC   0(1,RF),TLRDTDPT                                                 
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
         USING TLSTD,R2                                                         
         MVC   BOFULL1,TLRNWRTG                                                 
         NI    BOFULL1,X'7F'          STRIP HOB                                 
         XC    BOFULL1,=X'FFFFFFFF'   DESCENDING SORT                           
*                                                                               
         ZIC   RF,0(R3)               DISP INTO KEY                             
         LA    RF,SORTKEY(RF)                                                   
         MVC   0(4,RF),BOFULL1                                                  
*                                                                               
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
         SPACE                                                                  
BSKCPP   NTR1                      CPP                                          
         USING TLSTD,R2                                                         
         TM    TLRDTNCU,X'80'      N/A?                                         
         BZ    *+12                NO                                           
         ICM   R0,15,TLRDTNCU                                                   
         B     BSKCPP8                                                          
*                                                                               
         MVC   BOFULL1,TLRNWRTG                                                 
         NI    BOFULL1,X'7F'          STRIP HOB                                 
         L     R0,BOFULL1                                                       
         CVD   R0,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ                                                
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5                                                    
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF08B,BODUB1                                                  
*                                                                               
         ICM   R0,15,TLRDTNCU                                                   
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
BSKCPPX  DS    0H                                                               
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
         SPACE                                                                  
BSKCOST  NTR1                        COST                                       
         USING TLSTD,R2                                                         
         ZIC   RF,0(R3)              DISP INTO KEY                              
         LA    RF,SORTKEY(RF)                                                   
         MVC   0(4,RF),TLRDTNCU      SORT ON FIRST DISPLAY COST                 
         XC    0(4,RF),=X'7FFFFFFF'  DESCENDING SORT - N/A LAST                 
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
         SPACE                                                                  
BSKINV   NTR1                      INVENTORY NUMBER                             
         USING TLSTD,R2                                                         
         ZIC   RF,0(R3)            DISP INTO KEY                                
         LA    RF,SORTKEY(RF)                                                   
         MVC   0(4,RF),TLRDTINM    INVENTORY NUMBER                             
         MVC   4(1,RF),TLRDTSTA    STATION CODE                                 
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*&&DO                                                                           
         SPACE                                                                  
BSKSEQN  NTR1                      SEQUENCE NUMBER                              
         LA    RF,SORTKEY                                                       
K        USING TLKSRT,RF                                                        
         USING TLSTD,R2                                                         
         MVC   K.TLKDTSQ#,TLKDTSQ#    DETAIL LINE NUMBER                        
         MVC   K.TLKLINE#,TLKLINE#    SEQUENCE NUMBER                           
         B     EXITOK                                                           
         DROP  R2,K                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* VALIDATE SORT FIELD                                                           
***********************************************************************         
VALSORT  DS    0H                                                               
         NMOD1 0,**VSRT**                                                       
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
         BE    VALSRT0                                                          
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
VALSRT0  DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1VWCHG   NO, THE VIEW WAS CHANGED                     
*                                                                               
         XC    SORTREQ,SORTREQ                                                  
         XC    WHATSORT,WHATSORT                                                
*                                                                               
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
         MVC   0(1,R2),4(R4)       SET INTERNAL SORT CODE                       
         ZIC   RF,5(R4)                                                         
         LA    RF,WHATSORT(RF)     SET SORT BIT ON                              
         OC    0(1,RF),6(R4)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,32(R3)                                                        
         BCT   R0,VSR04                                                         
*                                                                               
VSR40    DS    0H                                                               
*&&DO*&& MVI   SORTREQ+L'SORTREQ-2,8       ALWAYS SORT ON SEQ NO                
         MVI   SORTREQ+L'SORTREQ-1,X'FF'   SET EOL                              
*                                                                               
         XC    SORTCTL,SORTCTL     BUILD SORT CONTROL SEQUENCE                  
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
         CLI   0(R2),L'TLKSRT      SAVE SORT KEY LENGTH                         
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(634)                                                
         B     EXITL                                                            
*                                                                               
         MVI   0(R2),X'FF'         SET EOL                                      
*                                                                               
VALSRTX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
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
VALDMO00 MVI   PRIMEDM,1                                                        
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
VALDMO30 STC   R1,PRIMEDM          SAVE # OF WHERE IN THE DISPLAY LIST          
*                                                                               
VALDMOX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY DAY/TIME FIELD                                                        
***********************************************************************         
DISLDYTM NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**DLDT**'                                                    
*                                                                               
         NI    FVATRB,FF-FVAPROT                                                
         CLC   LSROWREP,=H'1'                                                   
         BE    *+12                                                             
         OI    FVATRB,FVAPROT                                                   
         B     DSLDTMX                                                          
*                                                                               
         USING TLSTD,R2                                                         
         TM    MISCFLG1,MF1MNKY    DISPLAY MINIO KEYS?                          
         BNZ   DSLDTMAX            YES                                          
*                                                                               
         OC    TLRAVLS,TLRAVLS                                                  
         BZ    DSLDTMAX                                                         
*                                                                               
         OI    FVATRB,FVAPROT                                                   
         CLC   TLRAVDAY,BCSPACES                                                
         BH    *+16                                                             
         MVI   FVIFLD,C'/'                                                      
         LA    R4,FVIFLD+1                                                      
         B     DSLDTMA2                                                         
*                                                                               
         MVC   FVIFLD(L'TLRAVDAY),TLRAVDAY                                      
         LA    R4,FVIFLD+L'TLRAVDAY                                             
         CLI   0(R4),C' '                                                       
         BH    *+10                                                             
         BCTR  R4,0                                                             
         B     *-10                                                             
         CLC   TLRAVTIM,BCSPACES                                                
         BNH   DSLDTMX                                                          
         MVI   1(R4),C'/'                                                       
         LA    R4,2(R4)                                                         
DSLDTMA2 MVC   0(L'TLRAVTIM,R4),TLRAVTIM                                        
         B     DSLDTMX                                                          
*                                                                               
DSLDTMAX LA    R4,FVIFLD                 YES, WHERE TO START DISPLAYING         
*                                                                               
DSLDTM00 CLI   TLRDTDAY,0                        ANY DAYS FOR THIS REC?         
         BE    DSLDTM50                          NO                             
         GOTO1 VDAYUNPK,BODMCB,TLRDTDAY,FVIFLD   YES                            
*                                                                               
         LA    RE,FVIFLD                                                        
DSLDTM10 CLI   0(RE),C' '          CONVERT '/'S TO ','S                         
         BNH   DSLDTM20                                                         
         CLI   0(RE),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         B     DSLDTM10                                                         
*                                                                               
DSLDTM20 LA    R4,FVIFLD+9         FIND WHERE TO PUT THE TIMES                  
DSLDTM30 CLI   0(R4),C' '                                                       
         BH    DSLDTM40                                                         
         BCTR  R4,0                                                             
         B     DSLDTM30                                                         
DSLDTM40 LA    R4,1(R4)                                                         
*                                                                               
DSLDTM50 MVI   0(R4),C'/'          SEPARATE DAYS AND TIMES WITH A C'/'          
         LA    R4,1(R4)                                                         
*                                                                               
         OC    TLRDTSTM(L'TLRDTSTM*2),TLRDTSTM  ANY TIMES FOR THE LINE?         
         BZ    DSLDTMX                                                          
*                                                                               
         GOTO1 VUNTIME,BODMCB,TLRDTSTM,0(R4)                                    
*                                                                               
DSLDTMX  B     EXITOK                                                           
         DROP  R2                                                               
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
         USING TLSTD,R2                                                         
         CLI   FVILEN,0            REQUIRES A DAY/TIME                          
         BE    EXITNO                                                           
*                                                                               
         L     RE,AIO5                                                          
         LA    RF,480                                                           
         XCEFL                                                                  
*                                                                               
         GOTO1 VSCANNER,BODMCB,FVIHDR,(X'83',AIO5),C',=/='                      
         SPACE 2                                                                
*******************************                                                 
* PROCESS MINIO KEY DAY/TIMES *                                                 
*******************************                                                 
         CLI   4(R1),2             SHOULD HAVE 2 COMPONENTS                     
         BNE   EXITNV                NO MORE, NO LESS                           
*                                                                               
         L     R3,AIO5                                                          
         CLI   1(R3),0                                                          
         BNE   EXITNV                                                           
         CLI   0(R3),0                                                          
         BE    VLLDTM10                                                         
         GOTO1 VDAYVAL,BODMCB,(0(R3),12(R3)),TLRDTDAY,BOBYTE1                   
         CLI   TLRDTDAY,0          DAYVAL COMPLAINS IF INPUT > 11               
         BE    EXITNV                                                           
*                                                                               
VLLDTM10 LA    R3,32(R3)                                                        
*                                                                               
         CLI   1(R3),0                                                          
         BNE   EXITNV                                                           
         CLI   0(R3),0                                                          
         BE    VLLDTM20                                                         
*                                                                               
         GOTO1 VTIMVAL,BODMCB,(0(R3),12(R3)),TLRDTSTM                           
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BNE   *+14                                                             
         MVC   FVERRNDX,4(R3)     POINT TO THE TIME PORTION                     
         B     EXITNV                 THAT'S IN ERROR                           
*                                                                               
         GOTOX (PCKTIMQ,AREPRO01),BODMCB,TLRDTSTM,TLRDTETM,TLRDTTIM             
*                                                                               
VLLDTM20 DS    0H                                                               
         TM    WHATSORT+SRTDTMB,SRTDTMA                                         
         BNO   *+8                                                              
         OI    TLRCFLG1,TCF1KCHG   YES, TSAR KEY DEFINITELY CHANGED             
*                                                                               
VALLDTMX DS    0H                                                               
         OI    MISCFLG2,MF2TLSBA                                                
         B     EXITOK                                                           
         DROP  R2                                                               
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
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY CPP FIELD                                                             
***********************************************************************         
DISCSTPP DS    0H                                                               
         NMOD1 0,**DCPP**                                                       
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
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         XC    PREVRTG,PREVRTG                                                  
         GOTO1 GCLSTTSR,BODMCB,(R2)   GET CLUSTER FOR THE TSAR RECORD           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDVELD,R6                                                      
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
DSLCPP10 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    DSLCPP20            YES, NO RATING                               
*                                                                               
         CLI   RPRDVEL,RPRDVELQ    DEMO VALUE ELEMENT?                          
         BNE   DSLCPP10                                                         
         CLC   RPRDVBNM,SAVBK      SAME INTERNAL ORDER # AS PRIME BOOK?         
         BNE   DSLCPP10            NO, KEEP SEARCHING FOR IT                    
*                                                                               
         SR    R1,R1               PRIME IS DEFAULT                             
         IC    R1,SVPARMS2+3       FIND OUT WHICH DEMO'S CPP FIELD              
         SH    R1,=Y(PRMDCPPQ)     BY SUBTRACTING PRIME EQUATE                  
         MH    R1,=Y(L'SAVDMO)                                                  
         LA    R1,SAVDMOS(R1)                                                   
         MVC   BOBYTE1,DMLNIORD-DEMOLIN(R1)                                     
*                                                                               
         ZIC   R1,BOBYTE1                                                       
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,RPRDVDMO(R1)                                                  
*                                                                               
         LR    RE,R1               ARE WE BEYOND THE ELEMENT?                   
         LA    R0,RPRDVEL                                                       
         SR    RE,R0                                                            
         CLM   RE,1,RPRDVLEN                                                    
         BNL   DSLCPP20            YES, NEED DEMO RATING TO CHANGE COST         
*                                                                               
         MVC   BOFULL1,0(R1)       RATING WITHOUT THE OVERRIDE BIT              
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ                                                
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5                                                    
         SRP   BODUB1,1,0                                                       
         CVB   R1,BODUB1                                                        
         STCM  R1,15,PREVRTG       NOW WE HAVE A RATING TO BASE WITH            
*                                                                               
DSLCPP20 DS    0H                                                               
         TM    TLRDTNCU,X'80'      N/A?                                         
         BZ    *+14                                                             
         MVC   FVIFLD(2),=C'NA'                                                 
         B     DISLCPPX                                                         
*                                                                               
         ICM   R0,15,PREVRTG       RATING                                       
         CVD   R0,BODUB1                                                        
         ZAP   PCKOF08B,BODUB1                                                  
*                                                                               
         ICM   R0,15,TLRDTNCU      NEGOTIATED COST                              
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
DISLCPPX B     EXITOK                                                           
         DROP  R2,R5,R6                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE CPP FIELD                                                            
***********************************************************************         
VALCSTPP DS    0H                                                               
         NMOD1 0,**VCPP**                                                       
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
         TM    TLRCFLG1,TCF1COST   WAS THE COST CHANGED?                        
         BZ    VLLCPP00            NO                                           
         CLI   FVILEN,0            USER BLANKED OUT FIELD TO CONTINUE?          
         BE    VALLCPPX            YES                                          
         B     EXITCCHG            NO, COST WAS CHANGED                         
*                                                                               
VLLCPP00 L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,(R2)   GET CLUSTER FOR THE TSAR RECORD           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDVELD,R6                                                      
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
VLLCPP10 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    EXITNV              YES, NEED DEMO RATING TO CHANGE COST         
*                                                                               
         CLI   RPRDVEL,RPRDVELQ    DEMO VALUE ELEMENT?                          
         BNE   VLLCPP10                                                         
         CLC   RPRDVBNM,SAVBK      SAME INTERNAL ORDER # AS PRIME BOOK?         
         BNE   VLLCPP10            NO, KEEP SEARCHING FOR IT                    
*                                                                               
         SR    R1,R1               PRIME IS DEFAULT                             
         IC    R1,SVPARMS2+3       FIND OUT WHICH DEMO'S CPP FIELD              
         SH    R1,=Y(PRMDCPPQ)     BY SUBTRACTING PRIME EQUATE                  
         MH    R1,=Y(L'SAVDMO)                                                  
         LA    R1,SAVDMOS(R1)                                                   
         MVC   BOBYTE1,DMLNIORD-DEMOLIN(R1)                                     
*                                                                               
         ZIC   R1,BOBYTE1                                                       
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,RPRDVDMO(R1)                                                  
*                                                                               
         LR    RE,R1               ARE WE BEYOND THE ELEMENT?                   
         LA    R0,RPRDVEL                                                       
         SR    RE,R0                                                            
         CLM   RE,1,RPRDVLEN                                                    
         BNL   EXITNV              YES, NEED DEMO RATING TO CHANGE COST         
*                                                                               
         MVC   BOFULL1,0(R1)       RATING WITHOUT THE OVERRIDE BIT              
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ                                                
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5                                                    
         SRP   BODUB1,1,0                                                       
         CVB   R1,BODUB1                                                        
         STCM  R1,15,PREVRTG       NOW WE HAVE A RATING TO BASE WITH            
*                                                                               
         CLC   =C'NA',FVIFLD      N/A?                                          
         BNE   *+12                NO                                           
         L     R1,=X'80000000'                                                  
         B     VALLCPP2                                                         
*                                                                               
         ZIC   R0,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,FVIFLD,(R0)                                      
         CLI   0(R1),X'FF'                                                      
         BE    EXITNOTN            NOT NUMERIC                                  
*                                                                               
         MVC   BOFULL1,BODMCB+4                                                 
*                                                                               
         ICM   R1,15,PREVRTG       COST = CPP * RATING                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         SRP   PCKOF16B,64-1,5                                                  
         ZAP   BODUB1,PCKOF16B                                                  
         CVB   R1,BODUB1                                                        
*                                                                               
VALLCPP2 DS    0H                                                               
         STCM  R1,15,TLRDTNCU                                                   
*                                                                               
         OI    TLRCFLG1,TCF1CPP    WE CHANGED A CPP ON THIS LINE                
         TM    WHATSORT+SRTCPPB,SRTCPPA                                         
         BNO   *+8                                                              
         OI    TLRCFLG1,TCF1KCHG   YES, TSAR KEY DEFINITELY CHANGED             
*                                                                               
         TM    WHATSORT+SRTCSTB,SRTCSTA                                         
         BNO   *+8                                                              
         OI    TLRCFLG1,TCF1KCHG   YES, TSAR KEY DEFINITELY CHANGED             
*                                                                               
         CLI   WHICHCST,0                                                       
         BE    VALLCPPX            COST 1 HANDLED IN DLDVAL                     
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,(R2)   GET CLUSTER FOR THE TSAR RECORD           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,INTOAIO5         SO WE CAN USE RECUP LATER                    
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         XC    BOELEM,BOELEM                                                    
*                                                                               
         SR    R0,R0               BUMP TO THE NEXT ELEMENT IN CLUSTER          
VALCPP12 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    VALCPP16            YES, NO SUPLEMENTAL COST ELEMENT             
*                                                                               
         CLI   0(R6),RPRCSELQ      SUPLEMENTAL COST ELEMENT?                    
         BL    VALCPP12            NO - NOT THERE YET                           
         BH    VALCPP16            NO - PAST IT                                 
*                                                                               
         ZIC   R1,1(R6)            COPY AND DELETE                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)                                                  
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
VALCPP16 MVI   BOELEM,RPRCSELQ                                                  
         MVI   BOELEM+1,RPRCSLNQ                                                
*                                                                               
         ZIC   R1,WHICHCST         GET SUPLEMENTAL COST                         
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRCSSC2+L'RPRCSNC2)                                     
         LA    R1,BOELEM+(RPRCSSC2-RPRCSELD)(R1)                                
         MVC   0(L'RPRCSSC2,R1),TLRDTSCU                                        
         MVC   L'RPRCSSC2(L'RPRCSNC2,R1),TLRDTNCU                               
*                                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
         BAS   RE,FROMAIO5                                                      
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALLCPPX B     EXITOK                                                           
         DROP  R2,R5,R6                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY DAYPART FIELD                                                         
***********************************************************************         
DISLDYPT DS    0H                                                               
         NMOD1 0,**DDPT**                                                       
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
         NI    FVATRB,FF-FVAPROT                                                
         CLC   LSROWREP,=H'1'                                                   
         BE    *+12                                                             
         OI    FVATRB,FVAPROT                                                   
         B     DSLDPTX                                                          
*                                                                               
         USING TLSTD,R2                                                         
         MVC   FVIFLD(L'TLRDTDPT),TLRDTDPT                                      
         CLI   NFLTDPTS,0                                                       
         BE    DSLDPTX                                                          
         OI    FVATRB,FVAPROT                                                   
DSLDPTX  B     EXITOK                                                           
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE DAYPART FIELD                                                        
***********************************************************************         
VALLDYPT DS    0H                                                               
         NMOD1 0,**VDPT**                                                       
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
         CLI   FVILEN,0            ANY DAYPART CODE ENTERED ON LINE?            
         BE    EXITNO                                                           
*                                                                               
VLLDPT00 DS    0H                                                               
         TM    CCONFLG1,CCONDPMQ   USES HARDCODED TABLE?                        
         BO    VLLDPT60            YES                                          
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
         BNE   VLLDPT60            NO DAYPART RECORDS                           
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
         B     VLLDPT90                                                         
*                                                                               
*************************************************                               
** READ HARDCODED DAYPART TABLE FOR VALIDATION **                               
*************************************************                               
VLLDPT60 DS    0H                                                               
         LR    RE,RB                                                            
         AH    RE,=Y(DPTTABLE-PRO23)                                            
         ZIC   R3,FVXLEN              R3 = LENGTH OF INPUT -1                   
*                                                                               
VLLDPT65 CLI   0(RE),X'FF'         DID WE HIT THE END OF DPTTABLE?              
         BE    EXITNV              YES, INVALID DAYPART CODE                    
*                                                                               
         EX    R3,*+8              VALID 1-BYTE DAYPART CODE?                   
         B     *+10                                                             
         CLC   FVIFLD(0),3(RE)                                                  
         BE    VLLDPT90                                                         
         LA    RE,L'DPTTABLE(RE)                                                
         B     VLLDPT65                                                         
*                                                                               
VLLDPT90 MVC   TLRDTDPT,FVIFLD     SAVE THE DAYPART ENTERED                     
*                                                                               
         LA    RE,SAVDPTS          BUYER CPP FROM SAVDPTS                       
VLLDPT92 CLC   0(L'TLRDTDPT,RE),FVIFLD                                          
         BE    VLLDPT96                                                         
         LA    RE,L'SAVDPT(RE)                                                  
         LA    RF,SAVDPTS+L'SAVDPTS                                             
         CR    RE,RF                                                            
         BL    VLLDPT92                                                         
*                                                                               
         LA    RE,CSARDPT          BUYER CPP FROM CSARDPTS                      
VLLDPT94 CLC   0(L'TLRDTDPT,RE),FVIFLD                                          
         BE    VLLDPT96                                                         
         LA    RE,5(RE)                                                         
         LA    RF,CSARDPT+L'CSARDPT                                             
         CR    RE,RF                                                            
         BL    VLLDPT94                                                         
         XC    BOFULL1,BOFULL1                                                  
         LA    RE,BOFULL1-1                                                     
*                                                                               
VLLDPT96 MVC   TLRDTTAB,1(RE)                                                   
*                                                                               
         TM    WHATSORT+SRTDPTB,SRTDPTA                                         
         BNO   *+8                                                              
         OI    TLRCFLG1,TCF1KCHG   YES, TSAR KEY DEFINITELY CHANGED             
*                                                                               
VALLDPTX B     EXITOK                                                           
         DROP  R2                                                               
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
*                                                                               
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
***************                                                                 
* USE GLOBBER SO WE CAN GET TO THE PRP PROGRAM (GRANT)                          
***************                                                                 
         CLI   BCPFKEY,PFPRINT                                                  
         BNE   KLKV10                                                           
*                                                                               
         L     R6,ACOM                                                          
         USING COMFACSD,R6                                                      
         XC    BOELEM,BOELEM                                                    
         LA    R1,BOELEM                                                        
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'                                                 
         MVC   GLVXFRPR,=C'SEL'                                                 
         MVC   GLVXTOSY,=C'REP'                                                 
         MVC   GLVXTOPR,=C'SWP'                                                 
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R1                                                               
         GOTO1 CGLOBBER,BODMCB,=C'PUTD',BOELEM,14,GLVXCTL                       
*                                                                               
         XC    BOELEM,BOELEM                                                    
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         ZAP   BOWORK1(5),=P'99999999'                                          
         ZAP   BOWORK1+10(5),=P'0'                                              
         MVO   BOWORK1+10(5),0(4,RE)                                            
         SP    BOWORK1(5),BOWORK1+10(5)                                         
         OI    BOWORK1+4,X'0F'                                                  
         UNPK  BOELEM(8),BOWORK1(5)                                             
         MVI   BOELEM+8,C','                                                    
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVPRONUM-TWAD)                                             
         MVC   BOBYTE1,0(RE)                                                    
         XI    BOBYTE1,X'FF'                                                    
         EDIT  (B1,BOBYTE1),(3,BOELEM+9),FILL=0,DUB=BODUB1,WRK=BOWORK1          
         GOTO1 CGLOBBER,BODMCB,=C'PUTD',BOELEM,12,GLRKEY                        
*                                                                               
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(8),=CL8'QUICK'                                            
         CLI   WHATVIEW,VWPACKGE                                                
         BNE   *+10                                                             
         MVC   BOELEM(8),=CL8'PACKAGE'                                          
         GOTO1 CGLOBBER,BODMCB,=C'PUTD',BOELEM,8,GLVXREC                        
*                                                                               
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(6),=C'REPORT'                                             
         GOTO1 CGLOBBER,BODMCB,=C'PUTD',BOELEM,6,GLVXACT                        
*                                                                               
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(7),=CL7'NOW,QAV'                                          
         CLI   WHATVIEW,VWPACKGE                                                
         BNE   *+10                                                             
         MVC   BOELEM(7),=CL7'NOW,PKG'                                          
         GOTO1 CGLOBBER,BODMCB,=C'PUTD',BOELEM,7,GLRWHEN                        
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
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
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
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         CLI   SELPROFS,RREPQSEL                                                
         BE    DFDDIS00                                                         
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
*                                                                               
DFDDIS00 DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         OC    0(L'SVCONNUM,RE),0(RE)     ANY CONTRACT NUMBER?                  
         BZ    DFDDISX                    NO                                    
         CLI   SVPRONUM-SVCONNUM(RE),0    ANY PROPOSAL NUMBER?                  
         BZ    DFDDISX                    NO                                    
*                                                                               
         OC    SAVBKS,SAVBKS                                                    
         BNZ   DFDDIS01                                                         
*                                                                               
         LA    R1,GSRECKEY         YES, RESET INFO                              
         USING RPROKEY,R1                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         MVC   RPROKCON,0(RE)                                                   
         MVC   RPROKPRO,SVPRONUM-SVCONNUM(RE)                                   
         DROP  R1                                                               
         GOTOX (MNIOINQ,AREPRO01),BOPARM INITIALIZE MINIO                       
         GOTO1 =A(RDBKSDMS),BODMCB,(R9),RR=BORELO                               
*                                                                               
DFDDIS01 CLI   TWAACCS,C'$'               STATION IS USER                       
         BNE   *+18                                                             
         CLC   SVPARMS4,ATLST             LIST LINE?                            
         BE    DFDDLN                     YES                                   
         B     DFDDIS30                                                         
*                                                                               
         CLC   SVPARMS4,ATLST             LIST LINE?                            
         BE    DFDDLN                     YES                                   
         SPACE 2                                                                
****************************************************                            
** CHECK IF FETCH REQUIRED FOR STATIONS & DAYPART **                            
****************************************************                            
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ                                                 
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DFDDIS02 L     R6,MINELEM                                                       
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
         B     DFDDIS02                                                         
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
**********************************************                                  
** DO PRIME BOOK FETCH FOR ALL DETAIL LINES **                                  
**********************************************                                  
DFDDIS20 DS    0H                                                               
         TM    SAVBKS+(BKLNFLG-BOOKLIN),RPRBKPFT                                
         BNZ   DFDDIS30                             ALREADY DONE                
*                                                                               
         GOTOX (PRBKFTQ,AREPRO01),BODMCB,SAVSTAS,SAVBKS,SAVDMOS                 
         EJECT                                                                  
***************                                                                 
* DESCRIPTION ELEMENT                                                           
***************                                                                 
DFDDIS30 MVI   SAVOPTNS,0                                                       
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
         TM    MISCFLG1,MF1PFRET                                                
         BNZ   DFDDISX                                                          
         BAS   RE,REORGPRM         REORG  SAVBKS, SAVLBLS, & SAVDMOS            
         B     DFDDISX             ALL DONE FRO SCREEN                          
         EJECT                                                                  
**********************************************************                      
** DO FETCH FOR ALL BOOKS AND DEMOS ON THIS DETAIL LINE **                      
**********************************************************                      
DFDDLN   DS    0H                                                               
*                                                                               
         TM    MISCFLG2,MF2CURST                                                
         BNZ   DFDDLN5                                                          
         LH    R1,LS1STINP                                                      
         A     R1,ATWA                                                          
         ST    R1,BOCURSOR                                                      
         OI    MISCFLG2,MF2CURST                                                
*                                                                               
DFDDLN5  DS    0H                                                               
         OC    SBACURSR,SBACURSR                                                
         BZ    *+10                                                             
         MVC   BOCURSOR,SBACURSR                                                
*                                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         CLI   WHATVIEW,VWMBOOK                                                 
         BNE   DFDDLN10                                                         
*                                                                               
         GOTOX (DTLNFTQ,AREPRO01),BODMCB,SAVSTAS,(X'40',SAVBKS),       x        
               SAVDMOS,TLSDTSTA                                                 
         B     DFDDLN20                                                         
*                                                                               
DFDDLN10 GOTOX (DTLNFTQ,AREPRO01),BODMCB,SAVSTAS,SAVBKS,               X        
               SAVDMOS,TLSDTSTA                                                 
*                                                                               
DFDDLN20 DS    0H                                                               
**********************************************************                      
         SPACE 2                                                                
DFDDISX  B     EXITOK                                                           
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BEFORE VALIDATING THE DATA FIELDS                                             
***********************************************************************         
D1STDVAL DS    0H                                                               
         NMOD1 0,**DFDV**                                                       
         L     R9,0(R1)                                                         
         USING WORKD,R9                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         CLI   SELPROFS,RREPQSEL                                                
         BE    DFDVAL0                                                          
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
DFDVAL0  DS    0H                                                               
*                                                                               
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
         BAS   RE,REORGPRM         REORG  SAVBKS, SAVLBLS, & SAVDMOS            
*                                                                               
DFDVALX  B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS COMIMG BACK FROM CALLED SESSION                                       
***********************************************************************         
NTRXITIN DS    0H                                                               
         NMOD1 0,**NXIN**                                                       
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
         BAS   RE,SETVIEW                                                       
*                                                                               
         LA    R1,GSRECKEY                                                      
         USING RPROKEY,R1                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         MVC   RPROKCON,0(RE)                                                   
         MVC   RPROKPRO,SVPRONUM-SVCONNUM(RE)                                   
         DROP  R1                                                               
         GOTOX (MNIOINQ,AREPRO01),BOPARM                                        
*                                                                               
         GOTO1 =A(RDBKSDMS),BODMCB,(R9),RR=BORELO                               
         OI    MISCFLG1,MF1PFRET                                                
*                                                                               
         L     R2,SVPARMS+8                                                     
         USING SSAVD,R2                                                         
         GOTOX AGEN,BODMCB,OKEY,KVAL,GSRECKEY                                   
         BAS   RE,REORGPRM                                                      
*                                                                               
         CLI   NSREC,R#WORK                                                     
         BNE   *+12                                                             
         CLI   NSACT,18            WORK/ADD?                                    
         BE    NTRXINRF            YES                                          
*                                                                               
         CLC   =C'COSLSTY',SDATA   COST/LIST?                                   
         BE    NTRXINRF            YES                                          
*                                                                               
         CLC   =C'PROCHAY',SDATA   PRO/CHANGE?                                  
         BNE   *+12                                                             
         OI    MISCFLG2,MF2PROCH   YES                                          
         B     NTRXINRF                                                         
*                                                                               
         B     NTRXIN00                                                         
*                                                                               
NTRXINRF DS    0H                                                               
         MVI   GSFRPEL+FRPTYPE-FRPELD,FRPTRFRS  LIST REFRESH PFKEY              
         OI    LSSCIND1,LSSCIBLD+LSSCICLM       REBUILD LIST                    
***FA    MVC   BOBYTE1,NSREC                                                    
         MVI   NSREC,O#DIS                                                      
         GOTO1 =A(DEFCLMNS),BODMCB,(R9),RR=BORELO                               
***FA    MVC   NSREC,BOBYTE1                                                    
         B     NTRXIN50                                                         
         DROP  R2                                                               
*                                                                               
NTRXIN00 DS    0H                                                               
         OI    LSSCIND2,LSSCIPAG                                                
*                                                                               
         L     R4,ATLST                                                         
         USING TLSTD,R4                                                         
         OC    TLSDTSTA(L'RPROKMEL-1),TLSDTSTA                                  
         BNZ   NTRXIN05                                                         
         LA    RE,64(RA)                                                        
         SR    R0,R0                                                            
NTRXIN01 IC    R0,0(RE)                                                         
         TM    1(RE),X'02'         EXTENDED FIELD HEADER?                       
         BNZ   *+10                                                             
NTRXIN02 AR    RE,R0               BUMP TO THE NEXT FIELD                       
         B     NTRXIN01                                                         
*                                                                               
         LR    RF,RE                                                            
         AR    RF,R0                                                            
         SH    RF,=H'2'                                                         
         CLC   =X'000A',0(RF)      STATION FIELD?                               
         BNE   NTRXIN02            NO, BUMP UNTIL WE FIND IT                    
         NI    4(RE),X'FF'-X'20'   YES, TAKE OFF PREVIOUSLY VALIDATED           
         B     EXITOK                                                           
*                                                                               
NTRXIN05 L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 GCLSTTSR,BODMCB,ATLST                                            
         BE    *+6                                                              
         DC    H'0'                                                             
************************                                                        
* UPDATE AVAIL DAY/TIME                                                         
************************                                                        
         XC    TLRAVLS,TLRAVLS                                                  
         L     R6,MINELEM                                                       
NTRAV1   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NTRAVX                                                           
         CLI   0(R6),RPRAVELQ                                                   
         BNE   NTRAV1                                                           
*                                                                               
         USING RPRAVELD,R6                                                      
         MVC   TLRAVLS,RPRAVALS                                                 
         DROP  R6                                                               
NTRAVX   DS    0H                                                               
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
*                                                                               
***************                                                                 
* UPDATE PRIME RATING IN TSAR ENTRY                                             
***************                                                                 
NTRXIRT  DS    0H                                                               
         L     R6,MINELEM                                                       
         XC    TLRNWRTG,TLRNWRTG                                                
*                                                                               
NTRXPR2  ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             EOF                                          
         BE    NTRXIRTX            YES - SKIP RATING CODE                       
         CLI   0(R6),RPRDVELQ                                                   
         BNE   NTRXPR2                                                          
*                                                                               
         USING RPRDVELD,R6                                                      
         CLC   RPRDVBNM,SAVBKS+(BKLNIORD-BOOKLIN)                               
         BNE   NTRXPR2                                                          
*                                                                               
         ZIC   R1,SAVDMOS+(DMLNIORD-DEMOLIN)                                    
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,RPRDVDMO(R1)                                                  
*                                                                               
         LR    RE,R1               ARE WE BEYOND THE ELEMENT?                   
         LA    R0,RPRDVEL                                                       
         SR    RE,R0                                                            
         CLM   RE,1,RPRDVLEN                                                    
         BNL   NTRXIPRX            YES, NO DEMO RATING                          
*                                                                               
         MVC   TLRNWRTG,0(R1)                                                   
*                                                                               
         TM    FILTFLG1,FF1LVLS    DISPLAY LEVELS?                              
         BZ    *+12                NO                                           
         LA    R1,8(R1)                                                         
         B     NTRXPR6                                                          
         TM    FILTFLG1,FF1SHRS    DISPLAY SHARES?                              
         BZ    *+8                 NO                                           
         LA    R1,4(R1)                                                         
*                                                                               
NTRXPR6  DS    0H                                                               
         MVC   TLRNWDMO,0(R1)                                                   
*                                                                               
NTRXIPRX DS    0H                                                               
*********                                                                       
* UPDATE GRAND TOTAL RATING                                                     
*********                                                                       
NTRXITR  L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
*                                                                               
         MVC   BOFULL1,TLROLRTG    OLD DEMO RATING                              
         NI    BOFULL1,X'FF'-X'80'   WITHOUT THE DEMO OVERRIDE BIT              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ   ROUNDING?                                    
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5       YES                                          
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         ZICM  R1,TLRDTTSP,2       OLD NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF06B                                                
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTRTG-TWAD)                                             
         SP    0(L'GRNDTRTG,RE),PCKOF16B                                        
*                                                                               
         MVC   BOFULL1,TLRNWRTG    RATING WITHOUT DEMO OVERRIDE BIT             
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1          ADD IN THE NEW RATING                        
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ     ROUNDING?                                  
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5         YES                                        
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         ZICM  R1,RPRDTTSP,2       NEW NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF06B                                                
         AP    0(L'GRNDTRTG,RE),PCKOF16B                                        
*                                                                               
NTRXITRX DS    0H                                                               
*                                                                               
NTRXIRTX DS    0H                                                               
***************                                                                 
* UPDATE GRAND TOTAL COST                                                       
***************                                                                 
NTRXINTC DS    0H                                                               
         L     R6,MINELEM                                                       
         CLC   TLRDTNCU,TLRDTNC1   WAS IT COST1?                                
         BNE   NTRXITCX            NO SKIP THIS                                 
*                                                                               
         TM    TLRDTNCU,X'80'      N/A?                                         
         BNZ   NTRXITC4            YES                                          
*                                                                               
         ICM   R1,15,TLRDTNCU      MINUS OUT OLD COST                           
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         ZICM  R1,TLRDTTSP,2       OLD NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCST-TWAD)                                             
         SP    0(L'GRNDTCST,RE),PCKOF16B                                        
*                                                                               
NTRXITC4 DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCST-TWAD)                                             
*                                                                               
         MVC   TLRDTNCU,RPRDTNC1                                                
         MVC   TLRDTSCU,RPRDTSC1                                                
         MVC   TLRDTNCO,RPRDTNC1                                                
         MVC   TLRDTSCO,RPRDTSC1                                                
*                                                                               
         TM    RPRDTNC1,X'80'      N/A?                                         
         BNZ   NTRXITCX            YES                                          
*                                                                               
         ICM   R1,15,RPRDTNC1      ADD IN NEW COST !!!TOTAL!!!!                 
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         ZICM  R1,RPRDTTSP,2       NEW NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         AP    0(L'GRNDTCST,RE),PCKOF16B                                        
*                                                                               
NTRXITCX DS    0H                                                               
***************                                                                 
* UPDATE GRAND TOTAL SPOTS                                                      
***************                                                                 
NTRXITS  ZICM  R1,TLRDTTSP,2       MINUS OUT OLD SPOTS                          
         CVD   R1,BODUB1                                                        
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTSPT-TWAD)                                             
         SP    0(L'GRNDTSPT,RE),BODUB1                                          
*                                                                               
         ZICM  R1,RPRDTTSP,2       ADD IN NEW SPOTS                             
         CVD   R1,BODUB1                                                        
         AP    0(L'GRNDTSPT,RE),BODUB1                                          
*                                                                               
NTRXITSX DS    0H                                                               
***************                                                                 
* UPDATE THE TSAR ENTRY                                                         
***************                                                                 
         MVC   TLRDTFL1(TLRDTWKS-TLRDTFL1),RPRDTFL1                             
         DROP  R5,R6                                                            
*                                                                               
         TM    WHATSORT+SRTCPPB,SRTCPPA      KEY CHANGE POSSIBLE?               
         BO    NTRXIN10                      YES                                
         TM    WHATSORT+SRTRTGB,SRTRTGA                                         
         BO    NTRXIN10                      YES                                
         TM    WHATSORT+SRTCSTB,SRTCSTA                                         
         BO    NTRXIN10                      YES                                
*                                                                               
         GOTOX ('TSARIO',AGROUTS),TSAPUT     NO                                 
         B     NTRXIN40                                                         
         DROP  R4                                                               
*                                                                               
NTRXIN10 DS    0H                                                               
         GOTO1 =A(BTSRKEY),BODMCB,(R9),RR=BORELO                                
         ICM   R1,7,=XL4'FFFFFFFF'                                              
         ICM   R1,8,=AL1(AGLPTITM)                                              
         GOTOX (0,AGENLST)                                                      
         OI    LSLTIND1,LSLTISHU   SET SHUFFLE REQUIRED                         
*                                                                               
NTRXIN40 DS    0H                                                               
*                                                                               
NTRXIN50 OI    MISCFLG1,MF1PFRET                                                
*                                                                               
NTRXINX  B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DELETE SUB ACTION                                                             
***********************************************************************         
DELSUBAC DS    0H                                                               
         NMOD1 0,**DSBA**                                                       
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
         OI    LSLNIND1,LSLNIUPD   DO UPDATE                                    
         OI    LSSCIND1,LSSCIINP                                                
         OI    LSSCIND2,LSSCIPAG   RE-DISPLAY PAGE                              
         OI    LSLNIND1,LSLNIDEL   YES, DELETE IT FROM TSAR                     
         OI    GCINDS2,GCIANYCH    FOOL CONTROLLER THAT WE HAVE CHANGE          
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,ATLST                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*********                                                                       
* UPDATE GRAND TOTAL RATING                                                     
*********                                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
*                                                                               
         MVC   BOFULL1,TLROLRTG    OLD DEMO RATING                              
         NI    BOFULL1,X'FF'-X'80'   WITHOUT THE DEMO OVERRIDE BIT              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ   ROUNDING?                                    
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5       YES                                          
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         ZICM  R1,TLRDTTSP,2       OLD NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF06B                                                
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTRTG-TWAD)                                             
         SP    0(L'GRNDTRTG,RE),PCKOF16B                                        
*********                                                                       
* UPDATE GRAND TOTAL DEMO (RTG/SHR/LVL)                                         
*********                                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
*                                                                               
         MVC   BOFULL1,TLROLDMO    OLD DEMO (RTG/SHR/LVL)                       
         NI    BOFULL1,X'FF'-X'80'   WITHOUT THE DEMO OVERRIDE BIT              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ   ROUNDING?                                    
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5       YES                                          
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         ZICM  R1,TLRDTTSP,2       OLD NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF06B                                                
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTDMO-TWAD)                                             
         SP    0(L'GRNDTDMO,RE),PCKOF16B                                        
***************                                                                 
* UPDATE GRAND TOTAL CPP                                                        
***************                                                                 
         ICM   R1,15,TLRDTTAB                                                   
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCPP-TWAD)                                             
         SP    0(L'GRNDTCPP,RE),PCKOF16B                                        
*                                                                               
         LR    RE,RA               ONE LESS DETAIL                              
         AH    RE,=Y(GRNDTDTL-TWAD)                                             
         SP    0(L'GRNDTDTL,RE),=P'1'                                           
***************                                                                 
* UPDATE GRAND TOTAL COST                                                       
***************                                                                 
         TM    TLRDTNCU,X'80'      N/A?                                         
         BNZ   DELSBA10            YES                                          
*                                                                               
         ICM   R1,15,TLRDTNCU      MINUS OUT OLD COST                           
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         ZICM  R1,TLRDTTSP,2       OLD NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCST-TWAD)                                             
         SP    0(L'GRNDTCST,RE),PCKOF16B                                        
DELSBA10 DS    0H                                                               
***************                                                                 
* UPDATE GRAND TOTAL SPOTS                                                      
***************                                                                 
         ZICM  R1,TLRDTTSP,2       MINUS OUT OLD SPOTS                          
         CVD   R1,BODUB1                                                        
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTSPT-TWAD)                                             
         SP    0(L'GRNDTSPT,RE),BODUB1                                          
***************                                                                 
* FINALLY DELETE THE MINIO ELEMENT                                              
***************                                                                 
         BAS   RE,MINIODEL                                                      
*                                                                               
         OI    MISCFLG1,MF1MLDEL   SET MULTILINE DEL                            
*                                                                               
         OI    TLRCFLG1,TCF1DELD   TSAR ENTRY MARKED FOR DELETION               
         B     EXITOK              DON'T DO ANY MORE PROCESSING FOR IT          
         DROP  R2,R5                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* HIDE SUB ACTION                                                               
***********************************************************************         
HIDSUBAC DS    0H                                                               
         NMOD1 0,**HSBA**                                                       
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
         OI    LSLNIND1,LSLNIUPD   DO UPDATE                                    
         OI    LSSCIND1,LSSCIINP                                                
         OI    LSSCIND2,LSSCIPAG   RE-DISPLAY PAGE                              
         OI    LSLNIND1,LSLNIDEL   YES, DELETE IT FROM TSAR                     
         OI    GCINDS2,GCIANYCH    FOOL CONTROLLER THAT WE HAVE CHANGE          
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,ATLST                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*********                                                                       
* UPDATE GRAND TOTAL RATING                                                     
*********                                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
*                                                                               
         MVC   BOFULL1,TLROLRTG    OLD DEMO RATING                              
         NI    BOFULL1,X'FF'-X'80'   WITHOUT THE DEMO OVERRIDE BIT              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ   ROUNDING?                                    
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5       YES                                          
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         ZICM  R1,TLRDTTSP,2       OLD NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF06B                                                
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTRTG-TWAD)                                             
         SP    0(L'GRNDTRTG,RE),PCKOF16B                                        
***************                                                                 
* UPDATE GRAND TOTAL CPP                                                        
***************                                                                 
         ICM   R1,15,TLRDTTAB                                                   
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCPP-TWAD)                                             
         SP    0(L'GRNDTCPP,RE),PCKOF16B                                        
*                                                                               
         LR    RE,RA               ONE LESS DETAIL                              
         AH    RE,=Y(GRNDTDTL-TWAD)                                             
         SP    0(L'GRNDTDTL,RE),=P'1'                                           
***************                                                                 
* UPDATE GRAND TOTAL COST                                                       
***************                                                                 
         TM    TLRDTNCU,X'80'      N/A?                                         
         BNZ   HIDSBA10            YES                                          
*                                                                               
         ICM   R1,15,TLRDTNCU      MINUS OUT OLD COST                           
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         ZICM  R1,TLRDTTSP,2       OLD NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCST-TWAD)                                             
         SP    0(L'GRNDTCST,RE),PCKOF16B                                        
HIDSBA10 DS    0H                                                               
***************                                                                 
* UPDATE GRAND TOTAL SPOTS                                                      
***************                                                                 
         ZICM  R1,TLRDTTSP,2       MINUS OUT OLD SPOTS                          
         CVD   R1,BODUB1                                                        
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTSPT-TWAD)                                             
         SP    0(L'GRNDTSPT,RE),BODUB1                                          
***************                                                                 
* FINALLY WRITE  THE MINIO ELEMENT                                              
***************                                                                 
         OI    RPRDTFL1,RPRDTF1H   HIDE THE LINE                                
*                                                                               
         OI    MISCFLG1,MF1MLDEL   SET MULTILINE DEL                            
*                                                                               
         BAS   RE,MINIOWRT                                                      
         B     EXITOK              DON'T DO ANY MORE PROCESSING FOR IT          
         DROP  R2,R5,R6                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UNHIDE SUB ACTION                                                             
***********************************************************************         
UNHSUBAC DS    0H                                                               
         NMOD1 0,**USBA**                                                       
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
         OI    LSLNIND1,LSLNIUPD   DO UPDATE                                    
         OI    LSSCIND1,LSSCIINP                                                
         OI    LSSCIND2,LSSCIPAG   RE-DISPLAY PAGE                              
         OI    LSLNIND1,LSLNIDEL   YES, DELETE IT FROM TSAR                     
         OI    GCINDS2,GCIANYCH    FOOL CONTROLLER THAT WE HAVE CHANGE          
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,ATLST                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*********                                                                       
* UPDATE GRAND TOTAL RATING                                                     
*********                                                                       
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
*                                                                               
         MVC   BOFULL1,TLROLRTG    OLD DEMO RATING                              
         NI    BOFULL1,X'FF'-X'80'   WITHOUT THE DEMO OVERRIDE BIT              
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ   ROUNDING?                                    
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5       YES                                          
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         ZICM  R1,TLRDTTSP,2       OLD NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF06B                                                
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTRTG-TWAD)                                             
         SP    0(L'GRNDTRTG,RE),PCKOF16B                                        
***************                                                                 
* UPDATE GRAND TOTAL CPP                                                        
***************                                                                 
         ICM   R1,15,TLRDTTAB                                                   
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCPP-TWAD)                                             
         SP    0(L'GRNDTCPP,RE),PCKOF16B                                        
*                                                                               
         LR    RE,RA               ONE LESS DETAIL                              
         AH    RE,=Y(GRNDTDTL-TWAD)                                             
         SP    0(L'GRNDTDTL,RE),=P'1'                                           
***************                                                                 
* UPDATE GRAND TOTAL COST                                                       
***************                                                                 
         TM    TLRDTNCU,X'80'      N/A?                                         
         BNZ   UNHSBA10            YES                                          
*                                                                               
         ICM   R1,15,TLRDTNCU      MINUS OUT OLD COST                           
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         ZICM  R1,TLRDTTSP,2       OLD NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCST-TWAD)                                             
         SP    0(L'GRNDTCST,RE),PCKOF16B                                        
UNHSBA10 DS    0H                                                               
***************                                                                 
* UPDATE GRAND TOTAL SPOTS                                                      
***************                                                                 
         ZICM  R1,TLRDTTSP,2       MINUS OUT OLD SPOTS                          
         CVD   R1,BODUB1                                                        
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTSPT-TWAD)                                             
         SP    0(L'GRNDTSPT,RE),BODUB1                                          
***************                                                                 
* FINALLY WRITE  THE MINIO ELEMENT                                              
***************                                                                 
         NI    RPRDTFL1,FF-RPRDTF1H   UNHIDE THE LINE                           
*                                                                               
         OI    MISCFLG1,MF1MLDEL   SET MULTILINE DEL                            
*                                                                               
         BAS   RE,MINIOWRT                                                      
         B     EXITOK              DON'T DO ANY MORE PROCESSING FOR IT          
         DROP  R2,R5,R6                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TEXT SUB ACTION                                                               
***********************************************************************         
TXTSUBAC DS    0H                                                               
         NMOD1 0,*TXSBA**                                                       
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
         GOTO1 GCLSTTSR,BODMCB,ATLST                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,INTOAIO5                                                      
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
TXSBA02  CLI   0(R6),0             END OF ELEM?                                 
         BE    TXSBA06             YES                                          
         CLI   0(R6),RPRITELQ      INV TEXT ELEM?                               
         BE    TXSBA04             YES                                          
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     TXSBA02                                                          
*                                                                               
TXSBA04  GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),(R6)                        
*                                                                               
TXSBA06  XC    BOELEM,BOELEM       CREATE SKEL ELEMENT                          
         MVI   BOELEM,RPRITELQ     ELEMENT CODE                                 
         MVI   BOELEM+1,RPRITOVQ+L'RPRIT1ST                                     
         MVC   BOELEM+3(2),=XL2'FFFF'                                           
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
TXSBA08  CLI   0(R6),0             END OF ELEM?                                 
         BE    TXSBA10             YES                                          
         CLI   0(R6),RPRITELQ      INV TEXT ELEM?                               
         BNE   *+6                 SHOULD HAVE DELETED THIS                     
         DC    H'0'                                                             
         BNL   TXSBA10                                                          
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     TXSBA08                                                          
*                                                                               
TXSBA10  LA    R0,BOELEM                                                        
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),(R0),(R6)                   
*                                                                               
         BAS   RE,FROMAIO5                                                      
         BAS   RE,MINIOWRT                                                      
         B     EXITOK              DON'T DO ANY MORE PROCESSING FOR IT          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REFRESH ALL COSTS SUBACTION                                                   
***********************************************************************         
PBCSUBAC DS    0H                                                               
         NMOD1 0,*PBCBA**                                                       
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
         OI    LSLNIND1,LSLNIUPD   DO UPDATE                                    
         OI    LSSCIND1,LSSCIINP                                                
         OI    LSSCIND2,LSSCIPAG   RE-DISPLAY PAGE                              
         OI    GCINDS2,GCIANYCH    FOOL CONTROLLER THAT WE HAVE CHANGE          
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,ATLST                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,INTOAIO5                                                      
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
         USING RPRDTELD,R6                                                      
         MVC   BOFULL1,RPRDTNC1                                                 
         OC    BOFULL1,BOFULL1                                                  
         BZ    PBCSA50             NO COST SKIP LINE                            
         TM    BOFULL1,X'80'       N/A?                                         
         BNZ   PBCSA50             YES - SKIP LINE                              
         DROP  R6                                                               
*                                                                               
         XC    BOELEM,BOELEM                                                    
PBCSA10  CLI   0(R6),0                                                          
         BE    PBCSA14                                                          
         CLI   0(R6),RPRCSELQ                                                   
         BE    PBCSA12                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PBCSA10                                                          
*                                                                               
PBCSA12  DS    0H                  COPY AND DELETE ELEMENT                      
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)                                                  
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),0(R6)                       
*                                                                               
PBCSA14  DS    0H                                                               
         MVI   BOELEM,RPRCSELQ                                                  
         MVI   BOELEM+1,RPRCSLNQ                                                
*                                                                               
         LA    R0,NUMCSTS-1                                                     
         LA    R4,BOELEM+(RPRCSNC2-RPRCSELD)                                    
         LR    RE,RA                                                            
         AH    RE,=Y(MINCOSTS-TWAD)                                             
         LA    RE,L'MINCOST(RE)                                                 
         USING CSTLIN,RE                                                        
*                                                                               
PBCSA16  DS    0H                                                               
         CLI   CSLNPBC,0           % OF BASE COST?                              
         BE    PBCSA18             NO                                           
         TM    0(R4),X'80'         N/A?                                         
         BNZ   PBCSA18             YES                                          
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
         CP    BODUB1,=P'2000000000'                                            
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(699)                                                
         B     EXITL                                                            
*                                                                               
         SRP   BODUB1,64-2,5                                                    
         CVB   R1,BODUB1                                                        
         MH    R1,=H'100'          MAKE IT DOLLARS                              
         ST    R1,0(R4)                                                         
*                                                                               
         LA    R1,4                                                             
         SR    R1,R0                                                            
         CLM   R1,1,WHICHCST                                                    
         BNE   PBCSA18                                                          
*                                                                               
         L     R1,ATLST                                                         
         USING TLSTD,R1                                                         
         MVC   TLRDTNCU,0(R4)                                                   
         DROP  R1                                                               
*                                                                               
PBCSA18  DS    0H                                                               
         LA    RE,L'MINCOST(RE)                                                 
         LA    R4,RPRCSNC3-RPRCSNC2(R4)                                         
         BCT   R0,PBCSA16                                                       
         DROP  RE                                                               
*                                                                               
         L     R6,AIO5             WHERE DOES IT GO?                            
         LA    R6,RPROR1ST-RPROKEY(R6)                                          
*                                                                               
PBCSA20  DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    PBCSA22                                                          
         CLI   0(R6),RPRCSELQ                                                   
         BNL   PBCSA22                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PBCSA20                                                          
*                                                                               
PBCSA22  DS    0H                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),BOELEM,0(R6)                
         BAS   RE,FROMAIO5                                                      
         BAS   RE,MINIOWRT                                                      
*                                                                               
PBCSA50  DS    0H                                                               
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* AFTER VALIDATING THE DATA FIELDS                                              
***********************************************************************         
DLSTDVAL DS    0H                                                               
         NMOD1 0,**DLDV**                                                       
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
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         CLC   SVPARMS4,ATLST                                                   
         BNE   DLDVALX                                                          
*                                                                               
         TM    MISCFLG2,MF2TLSBA       SUBACTION PROCESSED? OR DTM              
         BO    DLDV0                   YES - SKIP                               
         TM    MISCFLG2,MF2SCRL        SCROLL PFKEY?                            
         BNO   DLDV0                   NO                                       
*                                                                               
         NI    LSSCIND1,FF-LSSCIINP    SCROLL TO NEXT SCREEN                    
*                                                                               
DLDV0    DS    0H                                                               
         GOTO1 =A(BTSRKEY),BODMCB,(R9),RR=BORELO                                
*                                                                               
DLDV20   L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         TM    TLRCFLG1,TCF1DELD   MARKED FOR DELETION?                         
         BNZ   DLDVALX             YES, SHOULDN'T FIND IT                       
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,(R2)   GET CLUSTER FOR THE TSAR RECORD           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
*                                                                               
         CLI   WHATVIEW,VWMCOST                                                 
         BE    DLDV38                                                           
*                                                                               
***************                                                                 
* UPDATE GRAND TOTAL RATING                                                     
***************                                                                 
DLDV30   MVC   BOFULL1,TLROLRTG    RATING WITHOUT DEMO OVERRIDE BIT             
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1          MINUS OUT THE OLD RATING                     
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ     ROUNDING?                                  
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5         YES                                        
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         ZICM  R1,RPRDTTSP,2       OLD NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF06B                                                
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTRTG-TWAD)                                             
         SP    0(L'GRNDTRTG,RE),PCKOF16B                                        
*                                                                               
         MVC   BOFULL1,TLRNWRTG    RATING WITHOUT DEMO OVERRIDE BIT             
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1          ADD IN THE NEW RATING                        
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ     ROUNDING?                                  
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5         YES                                        
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         ZICM  R1,TLRDTTSP,2       NEW NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF06B                                                
         AP    0(L'GRNDTRTG,RE),PCKOF16B                                        
***************                                                                 
* UPDATE GRAND TOTAL DEMO                                                       
***************                                                                 
         MVC   BOFULL1,TLROLDMO    RATING WITHOUT DEMO OVERRIDE BIT             
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1          MINUS OUT THE OLD DEMO                       
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ     ROUNDING?                                  
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5         YES                                        
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         ZICM  R1,RPRDTTSP,2       OLD NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF06B                                                
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTDMO-TWAD)                                             
         SP    0(L'GRNDTDMO,RE),PCKOF16B                                        
*                                                                               
         MVC   BOFULL1,TLRNWDMO    RATING WITHOUT DEMO OVERRIDE BIT             
         NI    BOFULL1,X'FF'-X'80'                                              
         L     R1,BOFULL1          ADD IN THE NEW DEMO                          
         CVD   R1,BODUB1                                                        
         TM    SAVOPTNS,OPTNDECQ     ROUNDING?                                  
         BNZ   *+16                                                             
         SRP   BODUB1,64-1,5         YES                                        
         SRP   BODUB1,1,0                                                       
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         ZICM  R1,TLRDTTSP,2       NEW NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
*                                                                               
         MP    PCKOF16B,PCKOF06B                                                
         AP    0(L'GRNDTDMO,RE),PCKOF16B                                        
***************                                                                 
* UPDATE BUYER'S CPP                                                            
***************                                                                 
         ICM   R1,15,RPRDTTAB                                                   
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCPP-TWAD)                                             
         SP    0(L'GRNDTCPP,RE),PCKOF16B                                        
*                                                                               
         ICM   R1,15,TLRDTTAB                                                   
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         AP    0(L'GRNDTCPP,RE),PCKOF16B                                        
***************                                                                 
* UPDATE GRAND TOTAL COST                                                       
***************                                                                 
         TM    TLRDTNCO,X'80'      N/A?                                         
         BNZ   DLDV34              YES                                          
*                                                                               
         ICM   R1,15,TLRDTNCO                                                   
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         ZICM  R1,RPRDTTSP,2       OLD NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCST-TWAD)                                             
         SP    0(L'GRNDTCST,RE),PCKOF16B                                        
*                                                                               
DLDV34   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTCST-TWAD)                                             
*                                                                               
         MVC   TLRDTNCO,TLRDTNCU   NEW COST == OLD COSTS                        
         MVC   TLRDTSCO,TLRDTSCU                                                
*                                                                               
         TM    TLRDTNCU,X'80'      N/A?                                         
         BNZ   DLDV36              YES                                          
*                                                                               
         ICM   R1,15,TLRDTNCU      ADD IN NEW COST                              
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
         ZICM  R1,TLRDTTSP,2       NEW NUMBER OF SPOTS                          
         CVD   R1,BODUB1                                                        
         ZAP   PCKOF06B,BODUB1                                                  
         MP    PCKOF16B,PCKOF06B                                                
         AP    0(L'GRNDTCST,RE),PCKOF16B                                        
DLDV36   DS    0H                                                               
***************                                                                 
* UPDATE GRAND TOTAL SPOTS                                                      
***************                                                                 
         ZICM  R1,RPRDTTSP,2       MINUS OUT OLD SPOTS                          
         CVD   R1,BODUB1                                                        
         LR    RE,RA                                                            
         AH    RE,=Y(GRNDTSPT-TWAD)                                             
         SP    0(L'GRNDTSPT,RE),BODUB1                                          
         ZICM  R1,TLRDTTSP,2       ADD IN NEW SPOTS                             
         CVD   R1,BODUB1                                                        
         AP    0(L'GRNDTSPT,RE),BODUB1                                          
***************                                                                 
         CLI   WHICHCST,0                                                       
         BNE   *+16                                                             
         MVC   TLRDTSC1,TLRDTSCU                                                
         MVC   TLRDTNC1,TLRDTNCU                                                
*                                                                               
DLDV38   DS    0H                                                               
         CLC   TLSDTSTA(L'RPROKMEL-1),TLRDTSTA   DID MINIO KEY CHANGE?          
         BNE   DLDV40                            YES                            
***************                                                                 
* NO CHANGE IN THE MINIO KEY                                                    
***************                                                                 
         CLC   RPRDTFL1(TLRDTWKS-TLRDTFL1),TLRDTFL1                             
         BE    DLDVALX             SAME DATA, NO NEED TO MINIOWRT               
         MVC   RPRDTFL1(TLRDTWKS-TLRDTFL1),TLRDTFL1                             
         BAS   RE,MINIOWRT                                                      
         B     DLDVALX                                                          
***************                                                                 
* CHANGE WAS MADE THAT AFFECTS THE MINIO KEY                                    
***************                                                                 
DLDV40   DS    0H                                                               
         MVC   RPRDTFL1(TLRDTWKS-TLRDTFL1),TLRDTFL1                             
*                                                                               
         CLC   RPRDTDPT,TLRDTDPT   DAYPART CHANGED?                             
         BE    DLDV48              NO                                           
*                                                                               
         LA    R1,1                RESET SUPPRESS BOOKS                         
         LR    RE,RA                                                            
         AH    RE,=Y(MINXBKS-TWAD)                                              
         USING XBOKLIN,RE                                                       
DLDV41   DS    0H                                                               
         LA    RF,X'80'                                                         
         SRL   RF,0(R1)                                                         
         STC   RF,BOBYTE1          SUPPRESS MASK                                
         MVI   BOBYTE2,FF                                                       
         XC    BOBYTE2,BOBYTE1                                                  
*                                                                               
         OC    XBLNDPT,XBLNDPT     ANY DAYPARTS?                                
         BZ    DLDV47              NO - NOTHING TO DO                           
*                                                                               
         LA    RF,XBLNDPT                                                       
         LA    R0,L'XBLNDPT(RF)                                                 
DLDV42   DS    0H                                                               
         CLC   RPRDTDPT,0(RF)      OLD DAYPART MATCH?                           
         BE    DLDV43              YES - CHECK FOR NEW DAYPART TOO              
         LA    RF,1(RF)                                                         
         CR    RF,R0                                                            
         BL    DLDV42                                                           
         B     DLDV45                                                           
*                                                                               
DLDV43   DS    0H                                                               
         LA    RF,XBLNDPT                                                       
         LA    R0,L'XBLNDPT(RF)                                                 
DLDV44   DS    0H                                                               
         CLC   TLRDTDPT,0(RF)      NEW DAYPART MATCH?                           
         BE    DLDV47              YES - PRESERVE FLAGS                         
         LA    RF,1(RF)                                                         
         CR    RF,R0                                                            
         BL    DLDV44                                                           
*                                                                               
         NC    RPRDTBKS,BOBYTE2    PRINT BOOK FOR OLD DPT                       
         B     DLDV47                                                           
*                                                                               
DLDV45   DS    0H                                                               
         LA    RF,XBLNDPT                                                       
         LA    R0,L'XBLNDPT(RF)                                                 
*                                                                               
DLDV46   DS    0H                                                               
         CLC   TLRDTDPT,0(RF)      NEW DAYPART MATCH?                           
         BE    DLDV46A             YES - CHECK OLD DPT TOO                      
         LA    RF,1(RF)                                                         
         CR    RF,R0                                                            
         BL    DLDV46                                                           
         B     DLDV47                                                           
*                                                                               
DLDV46A  DS    0H                                                               
         LA    RF,XBLNDPT                                                       
         LA    R0,L'XBLNDPT(RF)                                                 
*                                                                               
DLDV46B  DS    0H                                                               
         CLC   RPRDTDPT,0(RF)      OLD DAYPART MATCH?                           
         BE    DLDV47              YES - PRESERVE FLAGS                         
         LA    RF,1(RF)                                                         
         CR    RF,R0                                                            
         BL    DLDV46B                                                          
*                                                                               
         OC    RPRDTBKS,BOBYTE1    SUPPRESS BOOK                                
*                                                                               
DLDV47   DS    0H                                                               
         LA    RE,L'MINXBK(RE)                                                  
         LA    R1,1(R1)                                                         
         CLM   R1,1,=AL1(NUMBKS)                                                
         BNH   DLDV41                                                           
         DROP  RE                                                               
*                                                                               
DLDV48   MVC   RPRDTSTA(L'RPROKMEL-1),TLRDTSTA   NEW MINIO KEY                  
*                                                                               
DLDV49   BAS   RE,MINIOADD         ADD NEW DETAIL                               
         BE    DLDV50                                                           
         ZIC   R1,RPRDTSEQ         BUMP SEQUENCE NUMBER IF DUPLICATE            
         LA    R1,1(R1)                                                         
         STC   R1,RPRDTSEQ                                                      
         STC   R1,TLRDTSEQ                                                      
         B     DLDV49                                                           
*                                                                               
DLDV50   GOTO1 GCLSTTSR,BODMCB,(R2)  GET CLUSTER FOR THE TSAR RECORD            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,MINIODEL           SO WE CAN DELETE IT                        
*                                                                               
         MVC   TLSDTSTA(L'RPROKMEL-1),TLRDTSTA                                  
*                                                                               
DLDVALX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* HARD CODED DAYPART TABLE                                                      
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
* SORT WORDS TABLE   WORD(4),CODE(1),WHATSORT BYTE(1),WHATSORT BIT(1)           
***********************************************************************         
SORTWDS  DS    0X                                                               
         DC    C'DTIM',AL1(01),AL1(SRTDTMB),AL1(SRTDTMA)                        
         DC    C'DTM ',AL1(01),AL1(SRTDTMB),AL1(SRTDTMA)                        
         DC    C'DT  ',AL1(01),AL1(SRTDTMB),AL1(SRTDTMA)                        
         DC    C'STA ',AL1(02),AL1(SRTSTAB),AL1(SRTSTAA)                        
         DC    C'ST  ',AL1(02),AL1(SRTSTAB),AL1(SRTSTAA)                        
         DC    C'S   ',AL1(02),AL1(SRTSTAB),AL1(SRTSTAA)                        
         DC    C'DPT ',AL1(03),AL1(SRTDPTB),AL1(SRTDPTA)                        
         DC    C'DP  ',AL1(03),AL1(SRTDPTB),AL1(SRTDPTA)                        
         DC    C'RTG ',AL1(04),AL1(SRTRTGB),AL1(SRTRTGA)                        
         DC    C'RT  ',AL1(04),AL1(SRTRTGB),AL1(SRTRTGA)                        
         DC    C'R   ',AL1(04),AL1(SRTRTGB),AL1(SRTRTGA)                        
         DC    C'DEMO',AL1(04),AL1(SRTRTGB),AL1(SRTRTGA)                        
         DC    C'DEM ',AL1(04),AL1(SRTRTGB),AL1(SRTRTGA)                        
         DC    C'DE  ',AL1(04),AL1(SRTRTGB),AL1(SRTRTGA)                        
         DC    C'DMO ',AL1(04),AL1(SRTRTGB),AL1(SRTRTGA)                        
         DC    C'DM  ',AL1(04),AL1(SRTRTGB),AL1(SRTRTGA)                        
         DC    C'CPP ',AL1(05),AL1(SRTCPPB),AL1(SRTCPPA)                        
         DC    C'CP  ',AL1(05),AL1(SRTCPPB),AL1(SRTCPPA)                        
         DC    C'COST',AL1(06),AL1(SRTCSTB),AL1(SRTCSTA)                        
         DC    C'COS ',AL1(06),AL1(SRTCSTB),AL1(SRTCSTA)                        
         DC    C'CO  ',AL1(06),AL1(SRTCSTB),AL1(SRTCSTA)                        
         DC    C'CST ',AL1(06),AL1(SRTCSTB),AL1(SRTCSTA)                        
         DC    C'CS  ',AL1(06),AL1(SRTCSTB),AL1(SRTCSTA)                        
         DC    C'INV ',AL1(07),AL1(SRTINVB),AL1(SRTINVA)                        
         DC    C'IN  ',AL1(07),AL1(SRTINVB),AL1(SRTINVA)                        
         DC    C'I   ',AL1(07),AL1(SRTINVB),AL1(SRTINVA)                        
         DC    X'FF'                                                            
SORTWDL  EQU   4+1+1+1                                                          
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
         DC    AL1(05,07)          INVENTORY NUMBER                             
         DC    AL1(03,08)          SEQUENCE                                     
         DC    X'FF'                                                            
BSTTABL  EQU   6                                                                
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECRD)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFKEY)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(SUBACT)                              
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
LISTABL  DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LDEFCLM),AL1(0,0,1),AL4(DEFCLM1)                             
         DC    AL1(LKEYCLM),AL1(0,0,1),AL4(DEFCLM1)                             
         DC    AL1(LTSARDIR),AL1(0,0,1),AL4(TSARDIR1)                           
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
         DC    AL1(LUPDDIR),AL1(0,0,1),AL4(UPDDIR1)                             
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
*                                                                               
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
         DC    AL2(00011),AL4(OPTDTA)    OPTIONS                                
         DC    AL2(00013),AL4(BOKDTA)    BOOK                                   
         DC    AL2(00014),AL4(DMODTA)    DEMO                                   
         DC    AL2(00015),AL4(SRTDTA)    SORT                                   
         DC    AL2(00016),AL4(DPTDTA)    DAYPART                                
         DC    AL2(00017),AL4(VEWDTA)    VIEW                                   
* KEY (PROTECTED PORTION)                                                       
         DC    AL2(00010),AL4(LINEDTA)   RECORD COUNT                           
* RECORD (UNPROTECTED PORTION)                                                  
         DC    AL2(00012),AL4(COMDTA)    COMMENT                                
         DC    AL2(00028),AL4(COMDTA)    COMMENT LINE #2                        
         DC    AL2(00029),AL4(TRTDTA)    TOTAL RATING                           
         DC    AL2(00030),AL4(TCSDTA)    TOTAL COST                             
         DC    AL2(00031),AL4(TCPDTA)    TOTAL CPP                              
         DC    AL2(00032),AL4(TSPDTA)    TOTAL SPOTS                            
* RECORD (PROTECTED PORTION)                                                    
         DC    AL2(00083),AL4(TNRDTA)    TOTAL NEED RATE                        
         DC    AL2(00084),AL4(TBCDTA)    TOTAL BUYER'S CPP                      
* RECORD (LIST INPUT PORTION)                                                   
         DC    AL2(00082),AL4(LSTADTA)   STATION                                
         DC    AL2(00018),AL4(LDTMDTA)   DAY/TIME                               
         DC    AL2(00019),AL4(LPRGDTA)   PROGRAM                                
         DC    AL2(00085),AL4(LPRGDTA)   SHORTENED PROGRAM                      
         DC    AL2(00020),AL4(LDPTDTA)   DAYPART                                
         DC    AL2(00024),AL4(LCSTDTA)   COST                                   
         DC    AL2(00026),AL4(LBCPDTA)   TAB CPP                                
         DC    AL2(00023),AL4(LNRTDTA)   NEED RATE                              
         DC    AL2(00027),AL4(LSPTDTA)   SPOTS                                  
* BOOK FIELDS                                                                   
         DC    AL2(00041),AL4(LRTGDTA)   BOOK #1'S RATING                       
         DC    AL2(00042),AL4(LRTGDTA)   BOOK #2'S RATING                       
         DC    AL2(00043),AL4(LRTGDTA)   BOOK #3'S RATING                       
         DC    AL2(00044),AL4(LRTGDTA)   BOOK #4'S RATING                       
         DC    AL2(00045),AL4(LRTGDTA)   BOOK #5'S RATING                       
         DC    AL2(00046),AL4(LRTGDTA)   BOOK #6'S RATING                       
         DC    AL2(00047),AL4(LRTGDTA)   BOOK #7'S RATING                       
* BIG BOOK FIELDS                                                               
         DC    AL2(00111),AL4(LRTGDTA)   BOOK #1'S RATING                       
         DC    AL2(00112),AL4(LRTGDTA)   BOOK #2'S RATING                       
         DC    AL2(00113),AL4(LRTGDTA)   BOOK #3'S RATING                       
         DC    AL2(00114),AL4(LRTGDTA)   BOOK #4'S RATING                       
         DC    AL2(00115),AL4(LRTGDTA)   BOOK #5'S RATING                       
         DC    AL2(00116),AL4(LRTGDTA)   BOOK #6'S RATING                       
         DC    AL2(00117),AL4(LRTGDTA)   BOOK #7'S RATING                       
* DEMO FIELDS                                                                   
         DC    AL2(00051),AL4(LRTGDTA)   DEMO #1'S RATING                       
         DC    AL2(00052),AL4(LRTGDTA)   DEMO #2'S RATING                       
         DC    AL2(00053),AL4(LRTGDTA)   DEMO #3'S RATING                       
         DC    AL2(00054),AL4(LRTGDTA)   DEMO #4'S RATING                       
         DC    AL2(00055),AL4(LRTGDTA)   DEMO #5'S RATING                       
         DC    AL2(00056),AL4(LRTGDTA)   DEMO #6'S RATING                       
         DC    AL2(00057),AL4(LRTGDTA)   DEMO #7'S RATING                       
* CPP FIELDS                                                                    
         DC    AL2(00061),AL4(LCPPDTA)   DEMO #1'S CPP                          
         DC    AL2(00062),AL4(LCPPDTA)   DEMO #2'S CPP                          
         DC    AL2(00063),AL4(LCPPDTA)   DEMO #3'S CPP                          
         DC    AL2(00064),AL4(LCPPDTA)   DEMO #4'S CPP                          
         DC    AL2(00065),AL4(LCPPDTA)   DEMO #5'S CPP                          
         DC    AL2(00066),AL4(LCPPDTA)   DEMO #6'S CPP                          
         DC    AL2(00067),AL4(LCPPDTA)   DEMO #7'S CPP                          
* MCOST FIELDS                                                                  
         DC    AL2(00102),AL4(LMCSDTA)   COST #1                                
         DC    AL2(00103),AL4(LMCSDTA)   COST #2                                
         DC    AL2(00104),AL4(LMCSDTA)   COST #3                                
         DC    AL2(00105),AL4(LMCSDTA)   COST #4                                
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
SBACURSR DS    A                                                                
ESCLEN   DS    XL1                                                              
ESCCHAR  DS    XL2                                                              
SELPROFS DS    0CL10                CONTRACT PROFILES                           
         DS    CL1                 PROGRAM #                                    
         DS    CL1                 SPARE                                        
SELPROF  DS    CL8                 PROFILE BITS                                 
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS, SET #1                  
MF1KYCHG EQU   X'80'                - KEY FIELD CHANGED                         
MF1PFRET EQU   X'40'                - RETURNING FROM CALLED SESSION             
MF1VWCHG EQU   X'20'                - VIEW WAS CHANGED                          
MF1CSERR EQU   X'10'                - ERROR REFRESHING COST                     
MF1MLDEL EQU   X'08'                - MULTILINE DELETE                          
MF1MNKY  EQU   X'04'                - SHOW MINIO KEYS NOT AVL DAY/TIME          
MF1GLOBR EQU   X'02'                - CAME BACK FROM GLOBBER                    
MF1TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
MISCFLG2 DS    XL1                 MISCELLANEOUS FLAGS, SET #2                  
MF2PROCH EQU   X'80'                - RETURNING FROM PRO/CHANGE W/YES           
MF2TLSBA EQU   X'40'                - TSAR LINE SUB-ACT DON'T SCROLL            
MF2SCRL  EQU   X'20'                - PROCESS SCREEN THEN SCROLL                
MF2CURST EQU   X'10'                - CURSOR POSITION HAS BEEN SET              
MF2ADPRO EQU   X'08'                - PROPOSAL ADDED THIS TIME                  
MF2TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
MINIOFLG DS    XL1                 MINIO MISC FLAGS                             
MNIOFCLS EQU   X'80'                - NEED TO CLOSE MINIO                       
*                                                                               
FILTFLG1 DS    XL1                 FILTER FLAGS                                 
FF1SPOTS EQU   X'80'                - FILTER FOR LINES WITH SPOTS               
FF1SHRS  EQU   X'40'                - RATINGS ARE SHARES                        
FF1LVLS  EQU   X'20'                - RATINGS ARE LEVELS                        
FF1HIDN  EQU   X'10'                - SHOW HIDDEN DETAIL LINES                  
*                                                                               
LDPTSEQS DS    CL26                DAYPART LIST SEQUENCE TABLE                  
*                                                                               
PRIMEBK  DS    X                   INTERNAL ORD # OF DISPLAYED BOOK             
PRIMEDM  DS    X                   INTERNAL ORD # OF DISPLAYED DEMO             
*                                                                               
WHICHCST DS    X                   ZERO BASED COST WE'RE USING                  
*                                                                               
VIEWFLG1 DS    CL1                 VIEW FLAGS                                   
VF1MLDMO EQU   X'80'                - MULTILINE DEMO                            
VF1SFNOT EQU   X'40'                - SHOW FOOTNOTE                             
VF1BFNOT EQU   X'20'+VF1SFNOT       - SHOW BIG FOOTNOTE                         
*                                                                               
WHATVIEW DS    CL1                 WHAT VIEW WE WANT TO SEE                     
VWAVAIL  EQU   C'A'                 - AVAIL VIEW                                
VWAVAIL2 EQU   C'X'                 - 2ND AVAIL VIEW                            
*WMBOOK  EQU   C'B'                 - MULTI-BOOK VIEW                           
*WMBOOK2 EQU   C'Y'                 - 2ND MULTI-BOOK VIEW                       
VWMBOOK  EQU   C'F'                 - MULTI-BOOK VIEW W/STACK                   
VWMBOOK2 EQU   C'G'                 - 2ND MULTI-BOOK VIEW W/STACK               
VWMDEMO  EQU   C'D'                 - MULTI-DEMO VIEW                           
VWMDEMO2 EQU   C'Z'                 - 2ND MULTI-DEMO VIEW                       
VWPACKGE EQU   C'P'                 - PACKAGE VIEW                              
VWPACKG2 EQU   C'Q'                 - 2ND PACKAGE VIEW                          
VWMCOST  EQU   C'M'                 - MULTI-COST VIEW                           
VWMCOST2 EQU   C'N'                 - 2ND MULTI-COST VIEW                       
*                                                                               
WHATSORT DS    XL2                 WHAT SORTS ARE SELECTED                      
SRTCPPB  EQU   0                   CPP SORT                                     
SRTCPPA  EQU   X'80'                                                            
SRTCSTB  EQU   0                   COST SORT                                    
SRTCSTA  EQU   X'40'                                                            
SRTRTGB  EQU   0                   DEMO RATING SORT                             
SRTRTGA  EQU   X'20'                                                            
SRTINVB  EQU   0                   INVENTORY NUMBER SORT                        
SRTINVA  EQU   X'10'                                                            
SRTDPTB  EQU   0                   DAYPART SEQUENCE # SORT                      
SRTDPTA  EQU   X'08'                                                            
SRTSTAB  EQU   0                   STATION CALL LETTER SORT                     
SRTSTAA  EQU   X'04'                                                            
SRTDTMB  EQU   0                   DAYTIME SORT                                 
SRTDTMA  EQU   X'02'                                                            
SRTSEQB  EQU   0                   SEQUENCE NUMBER SORT                         
SRTSEQA  EQU   X'01'                                                            
*                                                                               
MAXSRTS  EQU   7                                                                
SORTREQ  DS    XL(MAXSRTS+1)                                                    
SORTCTL  DS    XL(MAXSRTS*2+1)                                                  
SORTKEY  DS    XL(36)                                                           
*                                                                               
FLTDPTS  DS    CL8                 DAYPART FILTER                               
NFLTDPTS DS    XL1                 NUMBER OF DAYPART FILTERS                    
*                                                                               
PRMBKQ   EQU   41                  FIELD NUMBER FOR PRIME BOOK                  
PRMBK2Q  EQU   111                 FIELD NUMBER FOR BIG PRIME BOOK              
PRMDEMOQ EQU   51                  FIELD NUMBER FOR PRIME DEMO                  
PRMDCPPQ EQU   61                  FIELD NUMBER FOR PRIME DEMO'S CPP            
PRMCOSTQ EQU   102                 FIELD NUMBER FOR PRIME COST                  
*                                                                               
PFPRINT  EQU   PFK01               PFKEY FOR PRINT                              
PFPROPSL EQU   PFK02               PFKEY FOR PROPOSAL                           
PFAVAIL  EQU   PFK03               PFKEY FOR AVAIL                              
PFPACKGE EQU   PFK04               PFKEY FOR PACKAGE                            
PFMBOOK  EQU   PFK05               PFKEY FOR MULTI-BOOK                         
PFMDEMO  EQU   PFK06               PFKEY FOR MULTI-DEMO                         
PFKYUP   EQU   PFK07               PFKEY FOR SCROLL UP                          
PFKYDOWN EQU   PFK08               PFKEY FOR SCROLL DOWN                        
PFKYLEFT EQU   PFK09               PFKEY FOR SCROLL LEFT                        
PFKYRGHT EQU   PFK10               PFKEY FOR SCROLL RIGHT                       
PFPENDNG EQU   PFK11               PFKEY FOR PENDING                            
PFRETURN EQU   PFK12               PFKEY FOR RETURN                             
*              PFK13               PFKEY FOR MORE                               
PFKYRIS  EQU   PFK14               PFKEY FOR RIS                                
PFMCOST  EQU   PFK15               PFKEY FOR MCOST                              
PFCSTLST EQU   PFK16               PFKEY FOR COST/LIST                          
PFWOKADD EQU   PFK17               PFKEY FOR WORK/ADD                           
PFKRFCST EQU   PFK18               PFKEY FOR REFRESHING COSTS                   
PFDOWNLD EQU   PFK19               PFKEY FOR PROPOSAL/DOWNLOAD                  
PFKSCRL  EQU   PFK08               PFKEY FOR SCROLL                             
*                                                                               
PREVRTG  DS    XL4                 PREVIOUS RATING                              
*                                                                               
PCKOF06B DS    PL6                 PACKED OF 6  BYTES                           
PCKOF08B DS    PL8                 PACKED OF 8  BYTES                           
PCKOF16B DS    PL16                PACKED OF 16 BYTES                           
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE AREA                          
*                                                                               
SAVOPTNS DS    XL1                 OPTIONS                                      
OPTNTXTQ EQU   X'80'                - TEXT BIT                                  
OPTNDECQ EQU   X'40'                - DEMO DECIMAL PRECISION BIT                
*                                                                               
SAVSTAS  DS    0XL(NUMSTAS*STLNLENQ)     SAVED STATION LINES                    
SAVSTA   DS    (NUMSTAS)XL(STLNLENQ)                                            
*                                                                               
SAVBKS   DS    0XL(NUMBKS*BKLNLENQ)     SAVED BOOKLINES                         
SAVBK    DS    (NUMBKS)XL(BKLNLENQ)                                             
*                                                                               
SAVLBLS  DS    0CL(NUMBKS*5)     SAVED LABELS FOR USER DEFINED BOOKS            
SAVLBL   DS    (NUMBKS)CL5                 - NULL: EMPTY OR LABEL               
*                                                                               
SAVDMOS  DS    0CL(NUMDEMS*(DMLNLENQ))   SAVED DEMOLINES                        
SAVDMO   DS    (NUMDEMS)CL(DMLNLENQ)                                            
*                                                                               
SAVDPTS  DS    0CL(8*(1+4))         - 1 BYTE DAYPART CODE                       
SAVDPT   DS    8CL(1+4)             - 4 BYTE BYR CPP                            
*                                                                               
SAVSLNS  DS    0CL(6*1)            SAVED 1-BYTE SPOT LENGTHS                    
SAVSLN   DS    6XL1                                                             
*                                                                               
SAVXBKS  DS    0XL(NUMBKS*XBLNLENQ)   BOOK EXTENSION                            
SAVXBK   DS    (NUMBKS)XL(XBLNLENQ)                                             
*                                                                               
SVRECDA  DS    XL(L'GSRECDA)                                                    
SVMINEKY DS    XL(L'RPROKMEL)      SAVED MINIO ELEMENT KEY                      
         EJECT                                                                  
       ++INCLUDE REPROLN                                                        
         EJECT                                                                  
* REPROWORK                                                                     
       ++INCLUDE REPROWORK                                                      
TWAD     DSECT                                                                  
***********************************                                             
* SAVE AREA - SAVED/RESTORED BETWEEN NTRSES                                     
***********************************                                             
         ORG   SVMORE                                                           
SVVWFLG1 DS    XL1                 SAVE THE VIEWFLAGS                           
GRNDTCST DS    PL16                GRAND TOTAL COST                             
GRNDTRTG DS    PL8                 GRAND TOTAL RATING                           
GRNDTDMO DS    PL8                 GRAND TOTAL DEMO (RTG/SHR/LVL)               
GRNDTSPT DS    PL6                 GRAND TOTAL SPOTS                            
GRNDTCPP DS    PL16                GRAND TOTAL COST PER POINT                   
GRNDTDTL DS    PL3                 GRAND TOTAL NUMBER OF DETAILS                
         ORG   TWUSER                                                           
***********************************                                             
* SAVE AREA EXCEPT BETWEEN NTRSES                                               
***********************************                                             
MINSTAS  DS    0XL(NUMSTAS*STLNLENQ)  SAVED STATION LINES                       
MINSTA   DS    (NUMSTAS)XL(STLNLENQ)                                            
*                                                                               
MINBKS   DS    0XL(NUMBKS*(BKLNLENQ))      SAVED BOOKLINES                      
MINBK    DS    (NUMBKS)XL(BKLNLENQ)                                             
*                                                                               
MINLBLS  DS    0CL(NUMBKS*5)    LABELS FOR USER DEFINED BKS (MINIO)             
MINLBL   DS    (NUMBKS)CL5         - NULL: EMPTY OR LABEL                       
*                                                                               
MINDMOS  DS    0CL(NUMDEMS*(DMLNLENQ))      SAVED DEMOLINES                     
MINDMO   DS    (NUMDEMS)CL(DMLNLENQ)                                            
*                                                                               
MINDPTS  DS    0CL(NUMDPTS*(1+4))         - 1 BYTE DAYPART CODE                 
MINDPT   DS    (NUMDPTS)CL(1+4)             - 4 BYTE BYR CPP                    
*                                                                               
MINCOSTS DS    0XL(NUMCSTS*CSLNLENQ)                                            
MINCOST  DS    (NUMCSTS)XL(CSLNLENQ)                                            
*                                                                               
MINTSRKY DS    CL(L'TLKSRT)                                                     
*                                                                               
MINXBKS  DS    0XL(NUMBKS*XBLNLENQ)   BOOK EXTENSION                            
MINXBK   DS    (NUMBKS)XL(XBLNLENQ)                                             
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
***       NCLUDE FAFACTS                                                        
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
         EJECT                                                                  
TLSTD    DSECT                                                                  
         ORG   TLUSER                                                           
TLRECD   DS    0X                                                               
* THIS TLS PORTION IS SO WE CAN GET TO THE ORIGINAL MINIO RECORD                
TLSDTSTA DS    XL1                 STATION INTERNAL CODE                        
TLSDTDPT DS    CL1                 DAYPART CODE                                 
TLSDTDAY DS    XL1                 BIT 0=SPARE 1=MON .. 7=SUN                   
TLSDTTIM DS    XL2                 START TIME SLL 4 BITS W/ 1/4 HRS             
TLSDTSEQ DS    XL1                 SEQUENCE NUMBER                              
TLSDTSPR DS    XL1                 SPARE                                        
*                                                                               
TLRCFLG1 DS    XL1                 WHAT WAS CHANGED FLAG                        
TCF1COST EQU   X'80'                - THE COST WAS CHANGED                      
TCF1CPP  EQU   X'40'                - A CPP WAS CHANGED                         
TCF1BCPP EQU   X'20'                - BUYER'S CPP WAS CHANGED                   
TCF1KCHG EQU   X'02'                - TSAR KEY WAS CHANGED                      
TCF1DELD EQU   X'01'                - TSAR ENTRY MARKED FOR DELETION            
*                                                                               
TLROLRTG DS    XL4                 OLD RATING FOR PRIME BOOK/DEMO               
TLRNWRTG DS    XL4                 NEW RATING FOR PRIME BOOK/DEMO               
TLROLDMO DS    XL4                 OLD DEMO (RTG/SHR/LVL)                       
TLRNWDMO DS    XL4                 NEW DEMO (RTG/SHR/LVL)                       
*                                                                               
TLRDTELC DS    XL1                 ELEMENT CODE                                 
TLRDTSTA DS    XL1                 STATION INTERNAL CODE                        
TLRDTDPT DS    CL1                 DAYPART CODE                                 
TLRDTDAY DS    XL1                 BIT 0=SPARE 1=MON .. 7=SUN                   
TLRDTTIM DS    XL2                 START TIME SLL 4 BITS W/ 1/4 HRS             
TLRDTSEQ DS    XL1                 SEQUENCE NUMBER                              
TLRDTSPR DS    XL1                 SPARE                                        
*                                                                               
TLRDTFL1 DS    XL1                 FLAG                                         
TLRDTF1H EQU   X'80'                - HIDDEN LINE                               
TLRDTMLN EQU   X'40'                - MULTILINE DISPLAY                         
TLRDTINM DS    CL4                 INVENTORY NUMBER                             
TLRDTSTM DS    XL2                 START TIME                                   
TLRDTETM DS    XL2                 END   TIME                                   
TLRDTTAB DS    XL4                 TAB (CPP) FOR THIS DETAIL                    
TLRDTSC1 DS    XL4                 COST 1 SUBMITTED                             
TLRDTNC1 DS    XL4                 COST 1 NEGOTIATED                            
TLRDTPRG DS    XL2                 PROGRAM # (LINK TO PROGRAM ELEM)             
TLRDTBKS DS    XL1                 BIT 0=SPARE 1=BK1 .. 7=BK7                   
*                                    IF BIT IS ON, BOOK IS SUPPRESSED           
TLRDTTSP DS    XL2                 TOTAL NUMBER OF SPOTS                        
TLRDTEFF DS    XL3                 EFFECTIVE START DATE (PWOS JULIAN)           
TLRDTEEF DS    XL3                 EFFECTIVE END DATE (PWOS JULIAN)             
TLRDTSLN DS    XL1                 SECONDS LENGTH                               
TLRDTWKS DS    (MAXWKS)XL1         SPOTS PER WEEK (1 BYTE/WEEK)                 
*                                                                               
TLRDTSCU DS    XL4                 SUBMITTED COST PER USER REQ                  
TLRDTNCU DS    XL4                 NEGOTIATED COST PER USER REQ                 
TLRDTSCO DS    XL4                 OLD SUBMITTED COST PER USER REQ              
TLRDTNCO DS    XL4                 OLD NEGOTIATED COST PER USER REQ             
*                                                                               
TLRAVLS  DS    0CL22               AVAIL DAY/TIMES                              
TLRAVDAY DS    CL11                                                             
TLRAVTIM DS    CL11                                                             
*                                                                               
TLRDT0X  DS    0X                                                               
*                                                                               
TLRLNQ   EQU   *-TLSTD             LENGTH OF TSAR RECORD                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033REPRO23   07/10/00'                                      
         END                                                                    
