*          DATA SET REPRO19    AT LEVEL 009 AS OF 02/12/99                      
*&&      SET   NOP=N                                                            
*PHASE T80A19A                                                                  
T80A19   TITLE 'REPRO19 - REP PROPOSAL DOWNLOAD'                                
PRO19    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REPRO19*,R7,RR=RE                                              
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
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                            
*                                                                               
* P1 HOLDS EQUATED VERB                                                         
***********************************************************************         
OBJECT   L     R1,SVPARMS                                                       
         LR    RF,RB                                                            
         AH    RF,=Y(TABLEOO-PRO19)                                             
         B     ITER                                                             
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
         DC    AL1(KDIS),AL1(0,0,0),AL4(KFKDIS)      DISPLAY                    
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE DISPLAYING THE KEY FIELDS                                              
***********************************************************************         
KFKDIS   DS    0H                                                               
         MVI   WHCHHELP,0                                                       
         B     EXITOK                                                           
***********************************************************************         
* BEFORE VALIDATING THE KEY FIELDS                                              
***********************************************************************         
KFKVAL   DS    0H                                                               
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
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
*                                                                               
         MVC   GCLASKEY,GSRECKEY                                                
         B     EXITOK                                                           
         DROP  R2                                                               
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
* AFTER THE SCREEN IS VALIDATED                                       *         
***********************************************************************         
         SPACE 1                                                                
REPLAST  DS    0H                                                               
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
         CLC   SVPARMS4,ATLST      LIST LINE?                                   
         BNE   EXITOK              NO                                           
*                                                                               
         OC    SAVBKS,SAVBKS       IN CASE OF WIERDNESS                         
         BNZ   DFDD00                                                           
*                                                                               
         GOTO1 =A(RDBKSDMS),BODMCB,(R9),RR=BORELO                               
*                                                                               
DFDD00   DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         GOTO1 GCLSTTSR,BODMCB,(R2)  GET CLUSTER FOR THE TSAR RECORD            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    CUSTAT,CUSDDS       DDS TERMINAL?                                
         BNZ   *+12                YES - GET DEMOS                              
         CLI   ASONOFF,ASOFF       RUNNING OFFLINE?                             
         BNE   DFDD02              NO - DON'T GET DEMOS                         
*                                                                               
         MVC   ELPARMS(2),MINELEML                                              
         MVC   ELPARMS+2(4),MINELEM                                             
*                                                                               
         GOTOX (DTLNFTQ,AREPRO01),BODMCB,(0,SAVSTAS),                  X        
               (X'C0',SAVBKS),(0,SAVDMOS),(C'R',ELPARMS)                        
*                                                                               
DFDD02   DS    0H                                                               
         L     RE,AIO5             COPY THE ENTIRE CLUSTER                      
         LA    RF,IOAREALN                                                      
         L     R0,MINELEM                                                       
         LH    R1,MINMAXEL                                                      
         MVCL  RE,R0                                                            
         DROP  R5                                                               
*                                                                               
         B     EXITOK              JRD                                          
         EJECT                                                                  
***********************************************************************         
************ DOING AN ACTION ON A SPECIFIC DATA OBJECT ****************         
***********************************************************************         
DATA10   DS    0H                                                               
         LR    RF,RB               TABLE OF KNOWN OBJECTS                       
         AH    RF,=Y(KNOWTAB-PRO19)                                             
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
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
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
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWHN)                                 
         DC    AL1(KDIS),AL1(0,0,0),AL4(DEFWHN)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* DEFAULT WHEN VALUE                                                            
***********************************************************************         
DEFWHN  DS    0H                                                                
         MVC   FVIFLD(4),=C'SOON'                                               
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE WHEN                                                                 
***********************************************************************         
VALWHN   DS    0H                                                               
         CLI   FVILEN,0                                                         
         BE    DEFWHN                                                           
*                                                                               
         GOTOX ('VALWHEN',AGROUTS)                                              
         BL    EXITL                                                            
         CLI   INWHEN,INWNNOW       TRY AN NOT FILL UP THE PQ RANDOMLY          
         BE    EXITNV                                                           
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALREQ)                                 
         DC    AL1(KDIS),AL1(0,0,0),AL4(DEFREQ)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* DEFAULT REQUESTOR VALUE                                                       
***********************************************************************         
DEFREQ   DS    0H                                                               
         MVC   FVIFLD(3),=C'DWN'                                                
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE REQUESTOR                                                            
***********************************************************************         
VALREQ   DS    0H                                                               
         CLI   FVILEN,0                                                         
         BE    DEFREQ                                                           
*                                                                               
         L     R3,AREP                                                          
         USING REPD,R3                                                          
         MVC   REPSUBID,FVIFLD                                                  
         MVC   INUSER,FVIFLD                                                    
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DESTINATION                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DSTDTA   DS    0H                                                               
         LA    RF,DSTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DSTTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALDST)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDST)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCPP)                                 
         DC    AL1(KDIS),AL1(0,0,0),AL4(DEFCPP)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* DEFAULT CPP OPTION VALUE                                                      
***********************************************************************         
DEFCPP   DS    0H                                                               
         MVI   FVIFLD,C'N'                                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE CPP/M OPTIONS                                                        
***********************************************************************         
VALCPP   DS    0H                                                               
         NI    RCFLAGS1,FF-(RCF1CPP+RCF1BCPP)                                   
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
         NI    RCFLAGS1,FF-(RCF1CPP+RCF1BCPP)                                   
         XC    FVIFLD,FVIFLD                                                    
         MVI   FVIFLD,C'N'                                                      
         B     VALCPPX             DON'T PROCESS ANY MORE                       
*                                                                               
VALCPP12 EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'Y'                                                 
         BNE   VALCPP14                                                         
         OI    RCFLAGS1,RCF1CPP                                                 
         B     VALCPP30                                                         
*                                                                               
VALCPP14 EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'B'                                                 
         BNE   VALCPP16                                                         
         OI    RCFLAGS1,RCF1BCPP                                                
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
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DAYPART FILTER                                                
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DPFDTA   DS    0H                                                               
         LA    RF,DPFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DPFTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALDPF)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDPF)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE DAYPART FILTER                                                       
***********************************************************************         
VALDPF   DS    0H                                                               
         MVI   RCNDPTS,0           NO FILTERS                                   
         XC    RCDPTS,RCDPTS       CLEAR OUT THE DPT FILTER                     
*                                                                               
         CLI   FVILEN,0            ANY DAYPART FILTER?                          
         BE    VALDPFX             NO                                           
*                                                                               
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RCDPTS(0),FVIFLD    COPY THE 1-BYTE DAYPART CODE                 
         MVC   RCNDPTS,FVILEN      NUMBER OF FILTERS                            
*                                                                               
VALDPFX  OI    FVIIND,FVIVAL                                                    
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCMP)                                 
         DC    AL1(KDIS),AL1(0,0,0),AL4(DEFCMP)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* DEFAULT COMPETITION OPTION                                                    
***********************************************************************         
DEFCMP   DS    0H                                                               
         MVI   FVIFLD,C'N'                                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE COMPETITION OPTION                                                   
***********************************************************************         
VALCMP   DS    0H                                                               
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALINV)                                 
         DC    AL1(KDIS),AL1(0,0,0),AL4(DEFINV)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* DEFAULT INVENTORY OPTION                                                      
***********************************************************************         
DEFINV   DS    0H                                                               
         MVI   FVIFLD,C'N'                                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE INVENTORY OPTION                                                     
***********************************************************************         
VALINV   DS    0H                                                               
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
* DATA OBJECT FOR DAYPART COLUMN OPTION                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DPTDTA   DS    0H                                                               
         LA    RF,DPTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DPTTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALDPT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDPT)                                 
         DC    AL1(KDIS),AL1(0,0,0),AL4(DEFDPT)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* DEFAULT DAYPART OPTION                                                        
***********************************************************************         
DEFDPT   DS    0H                                                               
         MVI   FVIFLD,C'Y'                                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE DAYPART OPTION                                                       
***********************************************************************         
VALDPT   DS    0H                                                               
         NI    RCFLAGS2,FF-RCF2NDPT                                             
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         MVI   FVIFLD,C'Y'                                                      
         B     VALDPTX                                                          
*                                                                               
         CLI   FVIFLD,C'Y'                                                      
         BE    VALDPTX                                                          
*                                                                               
         CLI   FVIFLD,C'N'                                                      
         BNE   EXITNV                                                           
         OI    RCFLAGS2,RCF2NDPT                                                
*                                                                               
VALDPTX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SPOTS COLUMN OPTION                                           
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SPTDTA   DS    0H                                                               
         LA    RF,SPTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SPTTBL   DC    AL1(DRVAL),AL1(0,0,0),AL4(VALSPT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSPT)                                 
         DC    AL1(KDIS),AL1(0,0,0),AL4(DEFSPT)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* DEFAULT SPOTS OPTION                                                          
***********************************************************************         
DEFSPT   DS    0H                                                               
         MVI   FVIFLD,C'N'                                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE SPOTS OPTION                                                         
***********************************************************************         
VALSPT   DS    0H                                                               
         NI    RCFLAGS2,FF-RCF2SPTS                                             
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         MVI   FVIFLD,C'N'                                                      
         B     VALSPTX                                                          
*                                                                               
         CLI   FVIFLD,C'N'                                                      
         BE    VALSPTX                                                          
*                                                                               
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    RCFLAGS2,RCF2SPTS                                                
*                                                                               
VALSPTX  OI    FVIIND,FVIVAL                                                    
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOS)                                 
         DC    AL1(KDIS),AL1(0,0,0),AL4(DEFCOS)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* DEFAULT COMPETITION OPTION                                                    
***********************************************************************         
DEFCOS   DS    0H                                                               
         MVI   FVIFLD,C'1'                                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE COST OPTION                                                          
***********************************************************************         
VALCOS   DS    0H                                                               
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
         TM    RCFLAGS1,RCF1CPP                                                 
         BZ    VALCOSX                                                          
         MVC   FVMSGNO,=AL2(614)                                                
         B     EXITL                                                            
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSHL)                                 
         DC    AL1(KDIS),AL1(0,0,0),AL4(DEFSHL)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* DEFAULT COMPETITION OPTION                                                    
***********************************************************************         
DEFSHL   DS    0H                                                               
         MVI   FVIFLD,C'N'                                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE SHARE/LEVEL OPTION                                                   
***********************************************************************         
VALSHL   DS    0H                                                               
         NI    RCFLAGS1,FF-(RCF1SHR+RCF1PUT)                                    
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         MVI   FVIFLD,C'N'                                                      
         B     VALSHLX                                                          
*                                                                               
         CLI   FVIFLD,C'N'                                                      
         BE    VALSHLX                                                          
*                                                                               
         CLI   FVIFLD,C'S'                                                      
         BNE   *+12                                                             
         OI    RCFLAGS1,RCF1SHR                                                 
         B     VALSHLX                                                          
*                                                                               
         CLI   FVIFLD,C'L'                                                      
         BNE   *+12                                                             
         OI    RCFLAGS1,RCF1PUT                                                 
         B     VALSHLX                                                          
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSRT)                                 
         DC    AL1(KDIS),AL1(0,0,0),AL4(DEFSRT)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* DEFAULT SORT OPTION                                                           
***********************************************************************         
DEFSRT   DS    0H                                                               
         CLI   SELPROFS,RREPQSEL                                                
         BE    DEFSRT0                                                          
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
DEFSRT0  DS    0H                                                               
*                                                                               
         MVC   FVIFLD(7),=C'DPT,INV'                                            
         TM    SELPROF+SELSDTMB,SELSDTMA     DTM DEFAULT?                       
         BNO   *+10                                                             
         MVC   FVIFLD(8),=C'DTIM,INV'                                           
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE SORT OPTION                                                          
***********************************************************************         
VALSRT   DS    0H                                                               
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
         TM    SELPROF+SELSDTMB,SELSDTMA     DTM DEFAULT?                       
         BNO   VSR01                                                            
         MVC   FVIFLD(8),=C'DTIM,INV'                                           
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOPT)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
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
* DATA OBJECT FOR HELP FIELD                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
HELPDTA  DS    0H                                                               
         LA    RF,HELPTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
HELPTBL  DS    0H                                                               
         DC    AL1(KDIS),AL1(0,0,0),AL4(DEFHELP)                                
         DC    AL1(DSET),AL1(0,0,0),AL4(FLTXX)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* DEFAULT HELP TEXT                                                             
***********************************************************************         
DEFHELP  DS    0H                                                               
         CLI   WHCHHELP,HLPNUMMX                                                
         BH    DEFHELPX                                                         
*                                                                               
         ZIC   R1,WHCHHELP                                                      
         LA    R0,1(R1)                                                         
         STC   R0,WHCHHELP                                                      
*                                                                               
         MH    R1,=Y(L'HELPTEXT)                                                
         A     R1,=A(HELPTEXT)                                                  
         A     R1,BORELO                                                        
         MVC   FVIFLD(L'HELPTEXT),0(R1)                                         
*                                                                               
DEFHELPX DS    0H                                                               
         B     EXITOK                                                           
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
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DAY/TIME FIELD                                                        
***********************************************************************         
DISLDTM  DS    0H                                                               
         L     R2,ATLST                                                         
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
DSLDTMX  OC    FVIFLD,BCSPACES                                                  
         B     EXITOK                                                           
         DROP  R2                                                               
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY PROGRAM FIELD                                                         
***********************************************************************         
DISLPRG  DS    0H                                                               
         L     R2,ATLST                                                         
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
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         MVC   FVIFLD(L'TLRDTDPT),TLRDTDPT                                      
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INVENTORY ID                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
LINVDTA  DS    0H                                                               
         LA    RF,LINVTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LINVTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLINV)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY INVENTORY ID FIELD                                                    
***********************************************************************         
DISLINV   DS    0H                                                              
          L     R2,ATLST                                                        
          USING TLSTD,R2                                                        
          MVC   FVIFLD(4),TLRDTINM                                              
          DROP R2                                                               
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY COST FIELD                                                            
***********************************************************************         
DISLCST  DS    0H                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         LA    RE,TLRDTNC1                                                      
*                                                                               
         ZICM  R1,SVPARMS2+2,2                                                  
         SH    R1,=H'102'                                                       
         LTR   R1,R1                                                            
         BNP   DISLCST2                                                         
*                                                                               
         BCTR  R1,0                                                             
         MH    R1,=Y(L'TLRDTNC1)                                                
         LA    RE,TLRDTNC2(R1)                                                  
*                                                                               
DISLCST2 DS    0H                                                               
         TM    0(RE),X'80'         N/A?                                         
**DWN    BZ    *+14                NO                                           
**DWN    MVC   FVIFLD(2),=C'NA'                                                 
         BZ    *+12                NO                                           
         MVI   FVIFLD,C'0'                                                      
         B     DSLCSTX                                                          
*                                                                               
         EDIT  (B4,0(RE)),(17,FVIFLD),2,DUB=BODUB1,ALIGN=LEFT,         X        
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
LRTGTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLRTG)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY RATING FIELD                                                          
***********************************************************************         
DISLRTG  DS    0H                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
*                                                                               
         LA    R3,SAVBKS                                                        
         LA    R4,SAVDMOS                                                       
*                                                                               
         CLC   =H'120',SVPARMS2+2   SHARE?                                      
         BE    *+14                 YES                                         
         CLC   =H'121',SVPARMS2+2   LEVEL?                                      
         BNE   DSLRTG01             NO                                          
*                                                                               
         ZIC   R3,LASTBKDM                                                      
         MH    R3,=Y(L'SAVBK)                                                   
         LA    R3,SAVBK(R3)                                                     
*                                                                               
         ZIC   R4,LASTBKDM+1                                                    
         MH    R4,=Y(L'SAVDMO)                                                  
         LA    R4,SAVDMO(R4)                                                    
         B     DSLRTG02                                                         
*                                                                               
DSLRTG01 CLC   =H'90',SVPARMS2+2   PRIME BOOK/DEMO?                             
         BNE   *+18                NO                                           
         MVI   WHICHBK,0                                                        
         XC    LASTBKDM,LASTBKDM                                                
         B     DSLRTG02                                                         
*                                                                               
         ZIC   R3,WHICHBK                                                       
         CLC   =H'91',SVPARMS2+2   NEXT BOOK?                                   
         BNE   *+12                NO                                           
         LA    R3,1(R3)                                                         
         STC   R3,WHICHBK                                                       
*                                                                               
         LA    RF,RCBKS(R3)                                                     
         ZIC   R3,0(RF)                                                         
         BCTR  R3,0                                                             
         STC   R3,LASTBKDM                                                      
         MH    R3,=Y(L'SAVBK)                                                   
         LA    R3,SAVBK(R3)                                                     
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,SVPARMS2+2                                                  
         SH    R4,=H'91'                                                        
         LA    RF,RCDEMS(R4)                                                    
         ZIC   R4,0(RF)                                                         
         BCTR  R4,0                                                             
         STC   R4,LASTBKDM+1                                                    
         MH    R4,=Y(L'SAVDMO)                                                  
         LA    R4,SAVDMO(R4)                                                    
*                                                                               
DSLRTG02 DS    0H                                                               
         USING BOOKLIN,R3                                                       
         MVC   BOBYTE1,BKLNIORD    INTERNAL ORDER NUMBER OF THE BOOK            
         DROP  R3                                                               
*                                                                               
         XC    PREVRTG,PREVRTG     NO RATING YET                                
         L     R6,AIO5                                                          
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
         MVC   BOBYTE1,DMLNIORD-DEMOLIN(R4)                                     
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
         CLC   =H'120',SVPARMS2+2  SHARE?                                       
         BNE   *+12                NO                                           
         LA    R1,4(R1)                                                         
         B     DISRTG12                                                         
*                                                                               
         CLC   =H'121',SVPARMS2+2  LEVEL?                                       
         BNE   *+12                NO                                           
         LA    R1,8(R1)                                                         
         B     DISRTG12                                                         
*                                                                               
DISRTG12 MVC   BOFULL1,0(R1)                                                    
         LA    R4,FVIFLD                                                        
         NI    BOFULL1,X'FF'-X'80'   GET RID OF DEMO OVERRIDE BIT               
*                                                                               
         TM    RCFLAGS1,RCF1RND    ROUND RATING?                                
         BNZ   DSLRTG15            YES                                          
*                                                                               
         EDIT  (B4,BOFULL1),(5,0(R4)),1,WRK=BOWORK1,ALIGN=LEFT,        X        
               DUB=BODUB1,ZERO=NOBLANK                                          
         MVC   PREVRTG,BOFULL1     SAVE SO WE CAN CALCULATE CPP LATER           
         B     DISLRTGX                                                         
*                                                                               
DSLRTG15 L     R3,BOFULL1          DON'T DISPLAY DEMO PRECISION                 
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
DISLRTGX B     EXITOK                                                           
         DROP  R2,R6                                                            
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY CPP FIELD                                                             
***********************************************************************         
DISLCPP  DS    0H                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
*                                                                               
         XC    PREVRTG,PREVRTG                                                  
         L     R6,AIO5                                                          
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
         LA    R1,SAVDMOS                                                       
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
         LA    RE,TLRDTNC1                                                      
*                                                                               
         ZICM  R1,SVPARMS2+2,2                                                  
         SH    R1,=H'106'                                                       
         LTR   R1,R1                                                            
         BNP   DSLCPP22                                                         
*                                                                               
         BCTR  R1,0                                                             
         MH    R1,=Y(L'TLRDTNC1)                                                
         LA    RE,TLRDTNC2(R1)                                                  
*                                                                               
DSLCPP22 DS    0H                                                               
         TM    0(RE),X'80'         N/A?                                         
**DWN    BZ    *+14                                                             
**DWN    MVC   FVIFLD(2),=C'NA'                                                 
         BZ    *+12                                                             
         MVI   FVIFLD,C'0'                                                      
         B     DISLCPPX                                                         
*                                                                               
         ICM   R0,15,PREVRTG       RATING                                       
         CVD   R0,BODUB1                                                        
         ZAP   PCKOF08B,BODUB1                                                  
*                                                                               
         ICM   R0,15,0(RE)         NEGOTIATED COST                              
         CVD   R0,BODUB1                                                        
         ZAP   PCKOF16B,BODUB1                                                  
*                                                                               
         BAS   RE,DIVPACKD         CPP = COST / RATING                          
         BE    DSLCPP30                                                         
**DWN    MVI   FVIFLD,C'*'         YES, '****' OUT THE CPP                      
         MVI   FVIFLD,C'0'                                                      
         B     DISLCPPX                                                         
*                                                                               
DSLCPP30 EDIT  (P16,PCKOF16B),(7,FVIFLD),2,WRK=BOWORK1,ALIGN=LEFT,     X        
               DUB=BODUB1,ZERO=NOBLANK                                          
*                                                                               
DISLCPPX B     EXITOK                                                           
         DROP  R2,R6                                                            
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY BUYER CPP FIELD                                                       
***********************************************************************         
DISLBCP  DS    0H                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         EDIT  (B4,TLRDTTAB),(7,FVIFLD),2,ALIGN=LEFT,DUB=BODUB1,       X        
               ZERO=NOBLANK,WRK=BOWORK1                                         
DSLBCPX  B     EXITOK                                                           
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY NEED RATE FIELD                                                       
***********************************************************************         
DISLNRT  DS    0H                                                               
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
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
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         EDIT  (B2,TLRDTTSP),(4,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,        X        
               DUB=BODUB1,ZERO=NOBLANK                                          
DSLSPTX  B     EXITOK                                                           
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
         AH    RF,=Y(LISTABL-PRO19)                                             
         USING OBJTABD,RF                                                       
LITER    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK                                                           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    LITER02             MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     LITER               ITERATE TABLE                                
*                                                                               
LITER02  DS    0H                                                               
         ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR LIST                                                           
***********************************************************************         
FTFLST1  DS    0H                                                               
         XC    SVMINEKY,SVMINEKY                                                
         MVI   SVMINEKY,RPRDTELQ   NEED A DETAIL DESCRIPTION ELEMENT            
         OI    LSSTAT1,LSSTSAR          SELECTABLE LIST OF TSAR RECS            
         OI    LSSTAT2,LSSIUPD          WE WANT TO DO OUR OWN UPDATES           
         NI    LSSTAT2,X'FF'-LSSADD   NOT VALID TO ADD NEW LIST LINES           
*                                                                               
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
         XC    SORTCTL,SORTCTL                                                  
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
         CLI   0(R2),L'SORTKEY     SAVE SORT KEY LENGTH                         
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(634)                                                
         B     EXITL                                                            
         MVI   0(R2),X'FF'         SET EOL                                      
*                                                                               
BSCX     DS    0H                                                               
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
         TM    RCFLAGS1,RCF1KEEP   DISPLAY KEPT LINES?                          
         BZ    NML21               NO                                           
         TM    RPRDTFL1,RPRDTF1H   KEPT?                                        
         BZ    NML10               NO - NEXT DETAIL                             
         B     NML22                                                            
*                                                                               
NML21    TM    RPRDTFL1,RPRDTF1H   HIDDEN?                                      
         BNZ   NML10               YES - NEXT DETAIL                            
         B     NML22                                                            
*                                                                               
NML22    DS    0H                  DO WE HAVE A STATION FILTER?                 
         TM    RCFLAGS1,RCF1COMP                                                
         BNZ   *+12                                                             
         CLI   RPRDTSTA,1          YES, PRIME STATION?                          
         BNE   NML10                    NO, CHECK NEXT DETAIL                   
*                                                                               
         CLI   RCNDPTS,0           DO WE HAVE A DAYPART FILTER?                 
         BE    NML24               NO                                           
         ZIC   R0,RCNDPTS                                                       
         LA    RE,RCDPTS                                                        
NML23    CLC   RPRDTDPT,0(RE)      MATCH ON FILTER?                             
         BE    NML24               YES                                          
         LA    RE,1(RE)                                                         
         BCT   R0,NML23                                                         
         B     NML10               NO                                           
*                                                                               
NML24    TM    RCFLAGS2,RCF2FSPT   FILTER FOR THOSE WITH SPOTS?                 
         BZ    *+14                NO                                           
         OC    RPRDTTSP,RPRDTTSP   YES, ANY SPOTS FOR THIS DETAIL?              
         BZ    NML10                    NONE                                    
*                                                                               
NML30    MVI   SVMINEKY,RPRDTELQ                                                
         MVC   SVMINEKY+1(L'SVMINEKY-1),RPRDTSTA                                
NMLX     B     EXITOK                                                           
         DROP  R5,R6                                                            
NMLXNO   DS    0H                                                               
         B     EXITL                                                            
*                                                                               
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
         XC    TLRDTNC2,TLRDTNC2   ASSUME ZERO                                  
         XC    TLRDTNC3,TLRDTNC3                                                
         XC    TLRDTNC4,TLRDTNC4                                                
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
         USING RPRCSELD,R6                                                      
         MVC   TLRDTNC2,RPRCSNC2                                                
         MVC   TLRDTNC3,RPRCSNC3                                                
         MVC   TLRDTNC4,RPRCSNC4                                                
*                                                                               
TSRFIL04 DS    0H                                                               
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
         MVC   TLRNWRTG,0(R1)                                                   
*                                                                               
TSARFILX DS    0H                  BUILD TSAR KEY AND EXIT                      
         GOTO1 =A(BTSRKEY),BODMCB,(R9),RR=BORELO                                
         B     EXITOK                                                           
         DROP  R6,R3                                                            
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
         XC    GCLASKEY,GCLASKEY   SO IT DOESN'T GO TO RVAL IN GEFIL02          
         B     EXITOK                                                           
***********************************************************************         
* SET THE MAINT SCREEN CODE                                                     
***********************************************************************         
SETMSCR  DS    0H                                                               
         MVI   GSSMCODE,0                                                       
         XC    GCLASKEY,GCLASKEY   SO IT DOESN'T GO TO RVAL IN GEFIL02          
         B     EXITOK                                                           
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
EXITERR  B     EXITL                                                            
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
INITX    B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DOWNLOAD OBJECT                                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
         DS    0D                                                               
         USING *,RB                                                             
DOWNLD   LR    RB,RF                                                            
         LM    R0,R3,SVPARMS                                                    
         LA    RF,DWNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DWNTBL   DC    AL1(DAPPCOL),AL1(0,0,0),AL4(DEFCLM1)                             
         DC    AL1(DAPPFLT),AL1(0,0,0),AL4(EXITOK)                              
         DC    AL1(DSCREEN),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(DLDO),AL1(0,0,0),AL4(EXITH)                                  
         DC    AL1(DSETCOLS),AL1(0,0,0),AL4(SETCOLS)                            
         DC    AL1(DPQINIT),AL1(0,0,0),AL4(DWNINIT)                             
         DC    AL1(EOT)                                                         
         SPACE 3                                                                
***********************************************************************         
* INTITIALIZE DOWNLOAD REPORT PARMS                                             
***********************************************************************         
DWNINIT  DS    0H                                                               
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
         MVC   REPDESC,=CL20'PRO/DOWNLD'                                        
         MVC   REPMAKER(1),INSYSID                                              
         MVC   REPMAKER+1(2),INPRGID                                            
         DROP  R3                                                               
         B     EXITOK                                                           
***********************************************************************         
* SET DEFAULT COLUMNS                                                           
***********************************************************************         
DEFCLM1  DS    0H                                                               
         OI    DLINDS,DLBALL       BUILD ALL OF LIST IN ONE GO                  
*                                                                               
         LA    RF,LSVARCLM         RF = A(1ST FIXED COLUMN)                     
         USING DCTABD,RF                                                        
         SR    R1,R1                                                            
*                                                                               
         MVC   DCTFLD#,=AL2(87)    STATION COLUMN                               
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
         TM    RCFLAGS2,RCF2NDPT                                                
         BNZ   *+18                                                             
         MVC   DCTFLD#,=AL2(98)    DAYPART FIELD                                
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
         TM    RCFLAGS1,RCF1INV                                                 
         BZ    *+18                                                             
         MVC   DCTFLD#,=AL2(110)   INVENTORY NUMBER FIELD                       
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
         MVC   DCTFLD#,=AL2(88)    DAY/TIME FIELD                               
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
         MVC   DCTFLD#,=AL2(89)    REGULAR PROGRAM FIELD                        
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
         TM    RCFLAGS2,RCF2SPTS                                                
         BZ    *+22                                                             
         OI    DCTINDS1,X'08'                                                   
         MVC   DCTFLD#,=AL2(101)   SPOTS FIELD                                  
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
         CLI   RCNBKS,0                                                         
         BE    DEFCOL30                                                         
         CLI   RCNDMOS,0                                                        
         BE    DEFCOL30                                                         
*                                                                               
         OI    DCTINDS1,X'08'                                                   
         MVC   DCTFLD#,=AL2(90)    PRIME RATING FIELD ALWAYS FIRST              
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
         TM    RCFLAGS1,RCF1SHR                                                 
         BZ    DEFCOL06                                                         
*                                                                               
         OI    DCTINDS1,X'08'                                                   
         MVC   DCTFLD#,=AL2(120)                                                
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
DEFCOL06 TM    RCFLAGS1,RCF1PUT                                                 
         BZ    DEFCOL08                                                         
*                                                                               
         OI    DCTINDS1,X'08'                                                   
         MVC   DCTFLD#,=AL2(121)                                                
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
DEFCOL08 CLI   RCNBKS,1            MULTIBOOK?                                   
         BH    *+12                YES                                          
         CLI   RCNDMOS,1           MULTIDEMO?                                   
         BNH   DEFCOL30            NO                                           
*                                                                               
         ZIC   R2,RCNBKS                                                        
         CLI   RCNDMOS,1           ONE DEMO?                                    
         BE    DEFCOL20            YES, NEXT BOOK                               
*                                                                               
         LA    RE,92               SKIP PRIME BOOK/DEMO                         
         ZIC   R3,RCNDMOS                                                       
         BCTR  R3,0                                                             
         B     DEFCOL12                                                         
*                                                                               
DEFCOL10 LA    RE,91               BOOK LEVEL LOOP                              
         ZIC   R3,RCNDMOS                                                       
DEFCOL12 DS    0H                  DEMO LEVEL LOOP                              
         OI    DCTINDS1,X'08'                                                   
         STCM  RE,3,DCTFLD#                                                     
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
         TM    RCFLAGS1,RCF1SHR                                                 
         BZ    DEFCOL14                                                         
*                                                                               
         OI    DCTINDS1,X'08'                                                   
         MVC   DCTFLD#,=AL2(120)                                                
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
DEFCOL14 TM    RCFLAGS1,RCF1PUT                                                 
         BZ    DEFCOL16                                                         
*                                                                               
         OI    DCTINDS1,X'08'                                                   
         MVC   DCTFLD#,=AL2(121)                                                
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
DEFCOL16 LA    RE,1(RE)                                                         
         BCT   R3,DEFCOL12         NEXT DEMO                                    
*                                                                               
DEFCOL20 DS    0H                  NEXT BOOK                                    
         BCT   R2,DEFCOL10                                                      
*                                                                               
DEFCOL30 DS    0H                                                               
         CLI   RCNCSTS,0                                                        
         BE    DEFCOL36                                                         
*                                                                               
         ZIC   R2,RCNCSTS                                                       
         LA    R3,RCCSTS                                                        
DEFCOL32 DS    0H                                                               
         ZIC   RE,0(R3)                                                         
         LA    RE,101(RE)                                                       
         OI    DCTINDS1,X'08'                                                   
         STCM  RE,3,DCTFLD#        COST FIELD                                   
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
         CLI   RCNBKS,1            MUTIBOOK?                                    
         BNE   DEFCOL34            YES, NO CPP                                  
         CLI   RCNDMOS,1           MULTIDEMO?                                   
         BNE   DEFCOL34            YES, NO CPP                                  
         TM    RCFLAGS1,RCF1CPP                                                 
         BZ    DEFCOL34                                                         
*                                                                               
         LA    RE,4(RE)                                                         
         OI    DCTINDS1,X'08'                                                   
         STCM  RE,3,DCTFLD#        CPP FIELD                                    
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
DEFCOL34 DS    0H                                                               
         LA    R3,1(R3)                                                         
         BCT   R2,DEFCOL32                                                      
*                                                                               
DEFCOL36 DS    0H                                                               
         TM    RCFLAGS1,RCF1BCPP                                                
         BZ    DEFCOL40                                                         
*                                                                               
         OI    DCTINDS1,X'08'                                                   
         MVC   DCTFLD#,=AL2(99)    TAB CPP FIELD                                
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
         CLI   RCNBKS,1            MUTIBOOK?                                    
         BNE   DEFCOL40            YES, NO NEED RATE                            
         CLI   RCNDMOS,1           MULTIDEMO?                                   
         BNE   DEFCOL40            YES, NO NEED RATE                            
*                                                                               
         OI    DCTINDS1,X'08'                                                   
         MVC   DCTFLD#,=AL2(100)   NEED RATE FIELD                              
         LA    RF,DCTABL(RF)                                                    
         LA    R1,1(R1)                                                         
*                                                                               
DEFCOL40 DS    0H                                                               
         STCM  R1,3,LSVARNUM                                                    
         XC    LSFIXNUM,LSFIXNUM                                                
*                                                                               
         CH    R1,=Y(VARMAX)       TOO WIDE?                                    
         BNH   EXITOK              NO                                           
         MVC   FVMSGNO,=AL2(557)                                                
         B     EXITL                                                            
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* SET COLUMNS - THIS CALL FALLS OUTSIDE THE OO MODEL                            
***********************************************************************         
SETCOLS  DS    0H                                                               
         L     R2,SVPARMS3                                                      
         L     R4,SVPARMS4                                                      
         USING FDRELD,R4                                                        
         ZICM  R1,FDRNUM,2                                                      
         DROP  R4                                                               
*                                                                               
         LA    RF,COLTAB                                                        
SETCOL02 DS    0H                                                               
         CLC   =AL2(EOT),0(RF)                                                  
         BE    EXITOK              UNCHANGED COLUMN HEADING                     
         CLM   R1,3,0(RF)                                                       
         BE    SETCOL04                                                         
         LA    RF,6(RF)                                                         
         B     SETCOL02                                                         
*                                                                               
SETCOL04 DS    0H                                                               
         ICM   RF,15,2(RF)                                                      
         A     RF,BORELO                                                        
         BR    RF                                                               
*                                                                               
COLTAB   DS    0H                                                               
         DC    AL2(0099),AL4(SETBCP)                                            
         DC    AL2(0090),AL4(SETBK)                                             
         DC    AL2(0091),AL4(SETBK)                                             
         DC    AL2(0092),AL4(SETBK)                                             
         DC    AL2(0093),AL4(SETBK)                                             
         DC    AL2(0094),AL4(SETBK)                                             
         DC    AL2(0095),AL4(SETBK)                                             
         DC    AL2(0096),AL4(SETBK)                                             
         DC    AL2(0097),AL4(SETBK)                                             
         DC    AL2(0087),AL4(SETSTA)                                            
         DC    AL2(0102),AL4(SETCST)                                            
         DC    AL2(0103),AL4(SETCST)                                            
         DC    AL2(0104),AL4(SETCST)                                            
         DC    AL2(0105),AL4(SETCST)                                            
         DC    AL2(0106),AL4(SETCPP)                                            
         DC    AL2(0107),AL4(SETCPP)                                            
         DC    AL2(0108),AL4(SETCPP)                                            
         DC    AL2(0109),AL4(SETCPP)                                            
         DC    AL2(0110),AL4(SETINV)                                            
         DC    AL2(0120),AL4(SETSHR)                                            
         DC    AL2(0121),AL4(SETPUT)                                            
         DC    AL2(EOT)                                                         
*                                                                               
***************                                                                 
SETBCP   DS    0H                                                               
         MVC   0(9,R2),=C'Byr Cpp/M'                                            
         B     EXITOK                                                           
*                                                                               
***************                                                                 
SETSTA   DS    0H                                                               
         MVC   0(7,R2),=C'Station'                                              
         B     EXITOK                                                           
*                                                                               
***************                                                                 
SETCPP   DS    0H                                                               
         MVC   0(5,R2),=C'Cpp/M'                                                
         B     EXITOK                                                           
*                                                                               
***************                                                                 
SETINV   DS    0H                                                               
         MVC   0(5,R2),=C'Inv #'                                                
         B     EXITOK                                                           
*                                                                               
***************                                                                 
SETSHR   DS    0H                                                               
         MVC   0(3,R2),=C'Shr'                                                  
         B     EXITOK                                                           
*                                                                               
***************                                                                 
SETPUT   DS    0H                                                               
         MVC   0(3,R2),=C'Lvl'                                                  
         B     EXITOK                                                           
*                                                                               
***************                                                                 
SETCST   DS    0H                                                               
         LR    R0,R1               MAY NEED THIS                                
         SH    R1,=H'102'                                                       
         MH    R1,=Y(L'MINCOST)                                                 
         AR    R1,RA                                                            
         AH    R1,=Y(MINCOSTS-TWAD)                                             
         USING CSTLIN,R1                                                        
         CLC   CSLNLBL,BCSPACES                                                 
         BNH   SETCST10                                                         
*                                                                               
         MVC   0(L'CSLNLBL,R2),CSLNLBL                                          
*                                                                               
         LA    RE,30(R2)                                                        
         CLI   0(RE),C' '                                                       
         BH    *+14                                                             
         MVI   0(RE),C' '                                                       
         BCTR  RE,0                                                             
         B     *-14                                                             
*                                                                               
         B     EXITOK                                                           
*                                                                               
SETCST10 SH    R0,=H'101'                                                       
         MVC   0(5,R2),=C'COST '                                                
         EDIT  (R0),(1,5(R2)),WRK=BOWORK1,DUB=BODUB1                            
         B     EXITOK                                                           
***************                                                                 
* BOOK/DEMO NAME FOR THE RATING COLUMN                                          
***************                                                                 
SETBK    DS    0H                                                               
         LA    R3,SAVBKS                                                        
         LA    R4,SAVDMOS                                                       
         LA    R5,SAVLBLS                                                       
*                                                                               
         CH    R1,=H'90'           FIRST BOOK?                                  
         BNE   *+12                NO                                           
         MVI   WHICHBK,0                                                        
         B     SETBK10                                                          
*                                                                               
         ZIC   RE,WHICHBK                                                       
         CH    R1,=H'91'           NEXT BOOK?                                   
         BNE   *+12                NO                                           
         LA    RE,1(RE)                                                         
         STC   RE,WHICHBK                                                       
*                                                                               
         LA    RF,RCBKS(RE)                                                     
         ZIC   R3,0(RF)                                                         
         BCTR  R3,0                                                             
         MH    R3,=Y(L'SAVBK)                                                   
         LA    R3,SAVBK(R3)                                                     
*                                                                               
         LA    RF,RCBKS(RE)                                                     
         ZIC   R5,0(RF)                                                         
         BCTR  R5,0                                                             
         MH    R5,=Y(L'SAVLBL)                                                  
         LA    R5,SAVLBL(R5)                                                    
*                                                                               
         SH    R1,=H'91'                                                        
         LA    RF,RCDEMS(R1)                                                    
         ZIC   R1,0(RF)                                                         
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVDMO)                                                  
         LA    R4,SAVDMO(R1)                                                    
*                                                                               
SETBK10  DS    0H                                                               
         OC    0(L'SAVLBL,R5),0(R5)          USER DEFINED BOOK?                 
         BZ    *+18                                                             
         MVC   0(5,R2),0(R5)                                                    
         LA    R2,5(R2)                                                         
         B     SETDEM                                                           
*                                                                               
         OC    0(L'SAVBK,R3),0(R3)                                              
         BZ    EXITOK                                                           
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
         BNE   SETBK12                                                          
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     SETBK12                                                          
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
SETBK12  DS    0H                                                               
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         SR    RF,RE                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),BCSPACES                                                 
*                                                                               
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),FVIFLD                                                   
         LA    R2,1(RE,R2)                                                      
*                                                                               
SETDEM   DS    0H                                                               
         MVI   0(R2),C'/'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         USING DEMOLIN,R4                                                       
*                                                                               
         L     R6,AIO3                                                          
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
         DROP  R6,R4                                                            
*                                                                               
         GOTO1 VDEMOCON,BODMCB,(1,BOWORK1),(9,0(R2)),(0,AIO3)                   
*                                                                               
         B     EXITOK                                                           
         SPACE 3                                                                
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
BSKYES   DS    0H                                                               
         MVC   TLKSRT,SORTKEY                                                   
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
         MH    R1,=Y(L'SAVSTA)                                                  
         LA    R1,SAVSTAS(R1)                                                   
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
         MVC   0(1,RF),TLRDTDPT      NOT FOUND                                  
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
         TM    TLRDTNC1,X'80'      N/A?                                         
         BZ    *+12                NO                                           
         ICM   R0,15,TLRDTNC1                                                   
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
         ICM   R0,15,TLRDTNC1                                                   
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
         MVC   0(4,RF),TLRDTNC1      SORT ON FIRST DISPLAY COST                 
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
*          DATA SET REPRO10    AT LEVEL 009 AS OF 05/02/97                      
***********************************************************************         
* VALIDATE OPTIONS                                                              
***********************************************************************         
VALOPTNS NTR1  BASE=*,LABEL=*                                                   
         NI    RCFLAGS1,FF-RCF1RND                                              
         TM    SAVOPTNS,OPTNDECQ   DISPLAY DEMO PRECISION?                      
         BNZ   *+8                 YES, DON'T ROUND                             
         OI    RCFLAGS1,RCF1RND    NO,ROUND                                     
*                                                                               
         NI    RCFLAGS1,FF-RCF1KEEP                                             
         NI    RCFLAGS2,FF-RCF2FSPT                                             
*                                                                               
*                                                                               
         XC    LDPTSEQS,LDPTSEQS                                                
         LA    RF,LDPTSEQS                                                      
         LA    RE,SAVDPTS                                                       
VOPT00   MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,L'SAVDPT(RE)                                                  
         LA    R0,SAVDPTS+L'SAVDPTS                                             
         CR    RE,R0                                                            
         BL    VOPT00                                                           
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
         OI    RCFLAGS2,RCF2FSPT                                                
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
         NI    RCFLAGS2,FF-RCF2FSPT                                             
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
         ZIC   R5,BOBYTE1                                                       
         BCTR  R5,0                                                             
         MH    R5,=Y(L'SAVDMO)                                                  
         LA    R5,SAVDMOS(R5)                                                   
*                                                                               
         OC    0(L'DMLNLENQ,R5),0(R5)                                           
         BZ    EXITNV              NO DEMO                                      
*                                                                               
         LA    R5,RCDEMS           CHECK FOR REPEATS                            
         ZIC   RF,RCNDMOS          ALWAYS > 1                                   
VOP07E   DS    0H                                                               
         CLC   BOBYTE1,0(R5)       MATCH?                                       
         BE    EXITNV                                                           
         LA    R5,1(R5)                                                         
         BCT   RF,VOP07E                                                        
*                                                                               
VOP07F   DS    0H                                                               
         MVC   0(1,R4),BOBYTE1     MOVE INTERNAL TO SLOT IN RCDEMS              
         LA    R4,1(R4)                                                         
         ZIC   RF,RCNDMOS          BUMP DEMO COUNTER                            
         LA    RF,1(RF)                                                         
         STC   RF,RCNDMOS                                                       
*                                                                               
         LA    R2,1(R2)            NEXT INPUT NUMBER                            
         BCT   R1,VOP07A                                                        
         B     VOP50                                                            
*                                                                               
VOP08    DS    0H                                                               
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
         MVC   LDPTSEQS(0),0(R2)     SAVE STRING OF 1-BYTE DPTS                 
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
         ZIC   R5,BOBYTE1                                                       
         BCTR  R5,0                                                             
         MH    R5,=Y(L'SAVBK)                                                   
         LA    R5,SAVBKS(R5)                                                    
*                                                                               
         OC    0(L'BKLNLENQ,R5),0(R5)                                           
         BZ    EXITNV              NO BOOK                                      
*                                                                               
         LA    R5,RCBKS            CHECK FOR REPEATS                            
         ZIC   RF,RCNBKS           ALWAYS > 1                                   
VOP11E   DS    0H                                                               
         CLC   BOBYTE1,0(R5)       MATCH?                                       
         BE    EXITNV                                                           
         LA    R5,1(R5)                                                         
         BCT   RF,VOP11E                                                        
*                                                                               
VOP11F   DS    0H                                                               
         MVC   0(1,R4),BOBYTE1     MOVE INTERNAL TO SLOT IN RCBKS               
         LA    R4,1(R4)                                                         
         ZIC   RF,RCNBKS           BUMP BOOK COUNTER                            
         LA    RF,1(RF)                                                         
         STC   RF,RCNBKS                                                        
*                                                                               
         LA    R2,1(R2)            NEXT INPUT NUMBER                            
         BCT   R1,VOP11A                                                        
         B     VOP50                                                            
*                                                                               
VOP12    DS    0H                                                               
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
* READS THE BOOK AND DEMO ELEMENTS INTO SAVBKS, SAVLBLS, SAVDMOS                
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
*                                                                               
         MVI   SAVOPTNS,0                                                       
         XC    SAVSLNS,SAVSLNS                                                  
*                                                                               
***************                                                                 
* DESCRIPTION ELEMENT                                                           
***************                                                                 
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
***************                                                                 
* BOOK ELEMENT(S)                                                               
***************                                                                 
         XC    SAVBKS,SAVBKS                                                    
         XC    SAVLBLS,SAVLBLS                                                  
         XC    RCBKS,RCBKS         INLITIALZE BOOK LIST                         
         MVI   RCNBKS,0                                                         
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ    GET THE BOOK ELEMENT                         
         BAS   RE,MINIOHI                                                       
         BNE   RDBDBKX                                                          
*                                                                               
         MVI   RCNBKS,1                                                         
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRBKELD,R6                                                      
*                                                                               
RDBDBK10 CLI   0(R6),RPRBKELQ                                                   
         BNE   RDBDBKX                                                          
*                                                                               
         ZIC   R1,RPRBKDOR         DISPLAY ORDER NUMBER                         
         BCTR  R1,0                                                             
         LA    R1,RCBKS(R1)                                                     
         MVC   0(1,R1),RPRBKDOR                                                 
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
RDBDBKX  DS    0H                                                               
         DROP  R6                                                               
***************                                                                 
* DEMO ELEMENT(S)                                                               
***************                                                                 
         XC    SAVDMOS,SAVDMOS                                                  
         XC    RCDEMS,RCDEMS                                                    
         MVI   RCNDMOS,0                                                        
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDMELQ    GET THE DEMO ELEMENT                         
         BAS   RE,MINIOHI                                                       
         BNE   RDBDDMX                                                          
*                                                                               
         MVI   RCNDMOS,1                                                        
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDMELD,R6                                                      
*                                                                               
RDBDDM10 CLI   0(R6),RPRDMELQ                                                   
         BNE   RDBDDMX                                                          
*                                                                               
         ZIC   R1,RPRDMDOR         DISPLAY ORDER NUMBER                         
         BCTR  R1,0                                                             
         LA    R1,RCDEMS(R1)                                                    
         MVC   0(1,R1),RPRDMDOR                                                 
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
RDBDDMX  DS    0H                                                               
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
* HELP TEXT TABLE                                                               
***********************************************************************         
HELPTEXT DS    0CL80                                                            
         DC    CL80'Y=YES INCLUDE COMPETITIVE DETAIL LINES'                     
         DC    CL80'Y=YES, B=BUYER''S AND TARGET RATE'                          
         DC    CL80'Y=YES INCLUDE INVENTORY COLUMN'                             
         DC    CL80'Y=YES INCLUDE DAYPART COLUMN'                               
         DC    CL80'Y=YES INCLUDE SPOTS COLUMN'                                 
         DC    CL80'S=SHARES, L=HUT/PUT LEVELS, Y=SHARES AND HUT/PUT'           
HLPNUMMX EQU   (*-HELPTEXT)/L'HELPTEXT                                          
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
*        DC    AL1(ORECH),AL1(0,0,0),AL4(RECRD)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(OREP),AL1(0,0,0),AL4(REPORT)                                 
         DC    AL1(ODLOAD),AL1(0,0,0),AL4(DOWNLD)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,0),AL4(FTFLST1)                            
         DC    AL1(LTSARDIR),AL1(0,0,0),AL4(TSARDIR1)                           
         DC    AL1(LTSARFIL),AL1(0,0,0),AL4(TSARFIL1)                           
         DC    AL1(EOT)                                                         
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
KNOWTAB  DS    0XL(KNOWLQ)                                                      
* KEY (UNPROTECTED PORTION)                                                     
         DC    AL2(00001),AL4(CONDTA)    CONTRACT                               
         DC    AL2(00002),AL4(PRODTA)    PROPOSAL                               
         DC    AL2(00084),AL4(WHNDTA)    RUN WHEN                               
         DC    AL2(00085),AL4(REQDTA)    REQUESTOR                              
         DC    AL2(00086),AL4(DSTDTA)    DESTINATION                            
         DC    AL2(00111),AL4(INVDTA)    INVENTORY CODE                         
         DC    AL2(00112),AL4(CMPDTA)    COMPETITION                            
         DC    AL2(00113),AL4(COSDTA)    COST                                   
         DC    AL2(00114),AL4(DPFDTA)    DAYPART FILTER                         
         DC    AL2(00115),AL4(OPTDTA)    OPTIONS                                
         DC    AL2(00116),AL4(SRTDTA)    SORT                                   
         DC    AL2(00117),AL4(CPPDTA)    CPP OPTIONS                            
         DC    AL2(00118),AL4(DPTDTA)    DAYPART COLUMN                         
         DC    AL2(00119),AL4(SPTDTA)    SPOTS COLUMN                           
         DC    AL2(00122),AL4(SHLDTA)    SHARE/LEVEL COLUMN                     
* REPORT FIELDS                                                                 
         DC    AL2(00087),AL4(LSTADTA)   STATION                                
         DC    AL2(00088),AL4(LDTMDTA)   DAY/TIME                               
         DC    AL2(00089),AL4(LPRGDTA)   PROGRAM                                
         DC    AL2(00098),AL4(LDPTDTA)   DAYPART                                
         DC    AL2(00099),AL4(LBCPDTA)   TAB CPP                                
         DC    AL2(00100),AL4(LNRTDTA)   NEED RATE                              
         DC    AL2(00101),AL4(LSPTDTA)   SPOTS                                  
         DC    AL2(00110),AL4(LINVDTA)   INVENTORY NUMBER                       
* BOOK FIELDS                                                                   
         DC    AL2(00090),AL4(LRTGDTA)   PRIME BOOK/DEMO                        
         DC    AL2(00091),AL4(LRTGDTA)   BOOKS FIRST DEMO                       
         DC    AL2(00092),AL4(LRTGDTA)   - SECOND DEMO                          
         DC    AL2(00093),AL4(LRTGDTA)   - THRID DEMO                           
         DC    AL2(00094),AL4(LRTGDTA)   - FOURTH DEMO                          
         DC    AL2(00095),AL4(LRTGDTA)   - FIFTH DEMO                           
         DC    AL2(00096),AL4(LRTGDTA)   - SIXTH DEMO                           
         DC    AL2(00097),AL4(LRTGDTA)   - SEVENTH DEMO                         
*                                                                               
         DC    AL2(00120),AL4(LRTGDTA)   - SHARE FOR DEMO                       
         DC    AL2(00121),AL4(LRTGDTA)   - LEVEL FOR DEMO                       
* MCOST FIELDS                                                                  
         DC    AL2(00102),AL4(LCSTDTA)   COST #1                                
         DC    AL2(00103),AL4(LCSTDTA)   COST #2                                
         DC    AL2(00104),AL4(LCSTDTA)   COST #3                                
         DC    AL2(00105),AL4(LCSTDTA)   COST #4                                
* CPP FIELDS                                                                    
         DC    AL2(00106),AL4(LCPPDTA)   COST #1'S CPP                          
         DC    AL2(00107),AL4(LCPPDTA)   COST #2'S CPP                          
         DC    AL2(00108),AL4(LCPPDTA)   COST #3'S CPP                          
         DC    AL2(00109),AL4(LCPPDTA)   COST #4'S CPP                          
* OTHER FIELDS                                                                  
         DC    AL2(00123),AL4(HELPDTA)   HELP FIELD                             
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
ELPARMS  DS    XL6 - TO PASS TO REPRO26                                         
ESCLEN   DS    XL1                                                              
ESCCHAR  DS    XL2                                                              
SELPROFS DS    0CL10                CONTRACT PROFILES                           
         DS    CL1                 PROGRAM #                                    
         DS    CL1                 SPARE                                        
SELPROF  DS    CL8                 PROFILE BITS                                 
*                                                                               
WHICHBK  DS    X                                                                
WHICHLN  DS    X                                                                
*                                                                               
LASTBKDM DS    XL2                                                              
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
RCFLAGS1 DS    XL1                 REPORT SWITCHES 1                            
RCF1COMP EQU   X'80'                - COMPETITION OPTION                        
RCF1SHR  EQU   X'40'                - SHARE OPTION                              
RCF1PUT  EQU   X'20'                - PUT(LEVEL) OPTION                         
RCF1INV  EQU   X'10'                - INVENTORY NUMBER OPTION                   
RCF1CPP  EQU   X'08'                - CPP COLUMN                                
RCF1BCPP EQU   X'04'                - BUYERS CPP                                
RCF1RND  EQU   X'02'                - ROUND RATING                              
RCF1KEEP EQU   X'01'                - SHOW ONLY KEPT LINES                      
*                                                                               
RCFLAGS2 DS    XL1                 REPORT SWITCHES 2                            
RCF2SPTS EQU   X'80'                - SHOW ONLY LINES WITH SPOTS                
RCF2NDPT EQU   X'40'                - DON'T SHOW DAYPART COLUMN                 
RCF2FSPT EQU   X'20'                - FILTER ON SPOTS                           
**       EQU   X'10'                - DOUBLE SPACE THE REPORT                   
**       EQU   X'08'                - COVERSHEET OPTION                         
**       EQU   X'04'                - PORTRAIT OPTION                           
**       EQU   X'02'                - SHOW EFFECTIVE DATES                      
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
MF2TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
MINIOFLG DS    XL1                 MINIO MISC FLAGS                             
MNIOFCLS EQU   X'80'                - NEED TO CLOSE MINIO                       
*                                                                               
FILTFLG1 DS    XL1                 FILTER FLAGS                                 
**1SPOTS EQU   X'80'                - FILTER FOR LINES WITH SPOTS               
FF1SHRS  EQU   X'40'                - RATINGS ARE SHARES                        
FF1LVLS  EQU   X'20'                - RATINGS ARE LEVELS                        
**1HIDN  EQU   X'10'                - SHOW HIDDEN DETAIL LINES                  
*                                                                               
LDPTSEQS DS    CL26                DAYPART LIST SEQUENCE TABLE                  
*                                                                               
PRIMEBK  DS    X                   INTERNAL ORD # OF DISPLAYED BOOK             
PRIMEDM  DS    X                   INTERNAL ORD # OF DISPLAYED DEMO             
*                                                                               
WHICHCST DS    X                   ZERO BASED COST WE'RE USING                  
WHCHHELP DS    X                   ZERO BASED HELP TEXT LINE                    
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
SVRECDA  DS    XL(L'GSRECDA)                                                    
SVMINEKY DS    XL(L'RPROKMEL)      SAVED MINIO ELEMENT KEY                      
         EJECT                                                                  
       ++INCLUDE REPROLN                                                        
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
         ORG   TWUSER                                                           
***********************************                                             
* SAVE AREA EXCEPT BETWEEN NTRSES                                               
***********************************                                             
MINCOSTS DS    0XL(NUMCSTS*CSLNLENQ)                                            
MINCOST  DS    (NUMCSTS)XL(CSLNLENQ)                                            
*                                                                               
         DS    XL(TWUSER+L'TWUSER-*)   # OF SPARE BEFORE TWSECBLK               
         DS    0X                                                               
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
TLRDTNC2 DS    XL4                 COST 2                                       
TLRDTNC3 DS    XL4                 COST 3                                       
TLRDTNC4 DS    XL4                 COST 4                                       
*                                                                               
TLRNWRTG DS    XL4                 NEW RATING FOR PRIME BOOK/DEMO               
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
**PAN#1  DC    CL21'009REPRO19   02/12/99'                                      
         END                                                                    
