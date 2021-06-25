*          DATA SET REPRO22B   AT LEVEL 014 AS OF 05/01/02                      
*&&      SET   NOP=N                                                            
*PHASE T80A22B                                                                  
T80A22   TITLE 'REPRO22 - OBJECT VERSION OF RECORD RECORDS'                     
**********************************************************************          
*  HISTORY OF CHANGES                                                *          
*    JUN28/00 (BU ) REMOVE REFERENCES TO GLV1GOTO PER MEL HERTZIG    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
PRO22    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REPRO22*,R7,RR=RE                                              
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
NUMDEMS  EQU   7                                                                
NUMBKS   EQU   7                                                                
NUMDPTS  EQU   8                                                                
         SPACE 1                                                                
*****************************                                                   
** KEY SCREEN CODE EQUATES **                                                   
*****************************                                                   
KYADD    EQU   C'A'                ADD KEY SCREEN                               
KYDEF    EQU   0                   DEFAULT KEY SCREEN                           
         SPACE 1                                                                
******************************                                                  
** DATA SCREEN CODE EQUATES **                                                  
******************************                                                  
*TADD    EQU   C'A'                ADD DATA SCREEN                              
DTADD    EQU   C'X'                ADD DATA SCREEN                              
DTCMNT   EQU   C'C'                COMMENT DATA SCREEN                          
DTDEF    EQU   0                   DEFAULT DATA SCREEN                          
         SPACE 1                                                                
**************************                                                      
** PENDING ACTION CODES **                                                      
**************************                                                      
A#CMNT   EQU   16                  COMMENT ACTION                               
         EJECT                                                                  
***********************************************************************         
INIT     DS    0H                                                               
         GOTO1 =A(INITPRG),RR=BORELO                                            
         DC    H'0'                                                             
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                            
*                                                                               
* P1 HOLDS EQUATED VERB                                                         
***********************************************************************         
OBJECT   DS    0H                                                               
         L     R1,SVPARMS                                                       
         L     RF,=A(TABLEOO)       KNOWN OBJECTS                               
         A     RF,BORELO                                                        
         B     ITER                                                             
         SPACE 2                                                                
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
KEYTABL  DS    0H                                                               
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* AFTER WE'VE DONE EVERYTHING TO THE KEY FIELDS ON THE SCREEN                   
***********************************************************************         
KEYLAST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KLTABL   DS    0H                                                               
         DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER VALIDATING THE KEY FIELDS                                               
***********************************************************************         
KLKVAL   DS    0H                                                               
         CLI   BCPFKEY,PFKYRIS                                                  
         BE    CALLRIS                                                          
*                                                                               
         USING RCONPTYP,R2          OF COURSE IT WAS ALREADY READ               
         MVI   RCONPTYP,X'8C'       IN VALCON(VIA AREPRO01).                    
         MVC   RCONPREP,CUAALF                                                  
         MVC   RCONPCON,CCONNUM                                                 
         MVC   RCONPCON,=X'DEADBEEF'                                            
         DROP  R2                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CALL RIS PROGRAM VIA GLOBBER                                                  
***********************************************************************         
CALLRIS  DS    0H                                                               
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
         B     CRIS15                                                           
         MVI   1(RE),C'-'                                                       
         MVC   2(L'CCONKAOF,RE),CCONKAOF                                        
         LA    RE,L'CCONKAOF+2(RE)                                              
*                                                                               
CRIS15   MVI   0(RE),C','                                                       
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
         L     RD,BCSVRD           GET ALL THE WAT OUT                          
         B     EXITL                                                            
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
         SPACE 2                                                                
***********************************************************************         
* BEFORE THE CONTROLLER CALLS THE I/O ACTION                                    
***********************************************************************         
RECFRST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,RFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RFTABL   DS    0H                                                               
         DC    AL1(RADD),AL1(0,0,0),AL4(RFRADD)      ADD                        
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE THE I/O CALL TO ADD THE RECORD                                         
***********************************************************************         
RFRADD   DS    0H                                                               
         OC    AHDRCON,AHDRCON                                                  
         BZ    CALLCON                                                          
*                                                                               
         OI    GSINDSL1,GSINOIO                                                 
         XC    MYDSKA,MYDSKA                                                    
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RCONPTYP,RE          OF COURSE IT WAS ALREADY READ               
         MVI   RCONPTYP,X'8C'       IN VALCON(VIA AREPRO01).                    
         MVC   RCONPREP,CUAALF                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO11+XOREPDIR+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RCONKEY),IOKEYSAV                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MYDSKA,IOKEY+L'RCONKEY+L'RCONCNTL                                
*                                                                               
         ICM   R1,15,=AL4(XIO11+XOREPFIL+XOGETRUP)                              
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R0,AIO3                                                          
         LH    R1,=Y(IOAREALN)                                                  
         L     RE,AIOREC           OLD CONTRACT                                 
         LH    RF,=Y(IOAREALN)                                                  
         MVCL  R0,RE               FOR RECORD LAST RECORD ADD                   
*                                                                               
         LR    R6,R2                                                            
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   ELCODE,X'01'        GET CONTRACT DESC. ELEMENT                   
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                REQ'D                                        
*                                                                               
         ZIC   RE,1(R6) ELEMENT LENGTH                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BOWORK2(0),0(R6)    SAVE THE ELEMENT                             
         LA    R5,BOWORK2                                                       
         USING RCONELEM,R5                                                      
*                                                                               
*---------------------------------------------------------------------*         
* BUILD PENDING SARX ELEMENT                                                    
*                                                                               
*   WILL NEED TO SET THE FOLLOWING AFTER GLOBBER CALL TO CONTRACT:              
*        1.  RSARXSRC                                                           
*---------------------------------------------------------------------*         
         LA    R6,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         USING RSARXEL,R6                                                       
         MVI   RSARXCO,X'12'                         ELEMENT CODE               
         MVI   RSARXLEN,RSARXLTH                     ELEMENT LENGTH             
*                                                    ENTRY DATE TODAY           
         GOTO1 VDATCON,BODMCB,(5,0),(3,RSARXEDT)                                
         MVC   RSARXDEM,SAVDMOS                      DEMOS                      
         MVC   RSARXSRC,RCONRTGS                     RATING SOURCE              
*                                                                               
         LA    RF,SAVDPTS                            DAYPARTS                   
         LA    RE,RSARXDPT                                                      
         XC    RSARXDPT,RSARXDPT                                                
         LA    R0,6                                  6 DAYPARTS IN SARX         
RFRA010  MVC   0(1,RE),0(RF)                                                    
         LA    RF,L'SAVDPT(RF)                                                  
         LA    RE,3(RE)                                                         
         BCT   R0,RFRA010                                                       
*                                                                               
         LA    RF,SAVBKS                             BOOKS                      
         LA    RE,RSARXBKS                                                      
         XC    RSARXBKS,RSARXBKS                                                
         LA    R0,6                                  6 BOOKS IN SARX            
RFRA020  OC    0(L'SAVBK,RF),0(RF)                                              
         BZ    RFRA022                               NO MORE BOOKS              
         CLI   0(RF),0                               USER DEF. LABEL?           
         BNE   *+14                                   - NOT IN SARX             
         MVC   0(3,RE),2(RF)                                                    
         LA    RE,3(RE)                                                         
         LA    RF,L'SAVBK(RF)                                                   
         BCT   R0,RFRA020                                                       
*                                                                               
RFRA022  DS    0H                                                               
         LA    RF,SAVSLN                             LENGTHS                    
         LA    RE,RSARXRFR                           6 LENGTHS IN SARX          
         LA    R0,6                                                             
RFRA030  XC    0(2,RE),0(RE)                         CLEAR OLD LENGTH           
         MVC   1(1,RE),0(RF)                         STORE NEW LENGTH           
         LA    RE,2(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,RFRA030                                                       
*                                                                               
         MVC   RSARXBGT,SAVMKTB                      MARKET BUDGET              
         OI    SAVSARF1,X'04'                        SET EXPNASION ELS          
         MVC   RSARXFLG,SAVSARF1                     COPY FLAGS                 
         MVC   RSARXFL2,SAVSARF2                                                
         MVC   RSARXSHG,SAVSHGL                      SHARE GOAL                 
*                                                    LAST UPDATE TODAY          
         GOTO1 VDATCON,BODMCB,(5,0),(3,RSARXLAD)                                
         DROP  R6,R5                                                            
*                                                                               
         LR    R6,R2                                                            
         LA    R6,RCONELEM-RCONKEY(R6)               WHERE DOES IT GO?          
RFRA040  CLI   0(R6),0             END OF RECORD?                               
         BE    RFRA042             YES                                          
         CLI   0(R6),X'12'         IS THIS THE PLACE?                           
         BNL   RFRA042             YES                                          
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     RFRA040                                                          
*                                                                               
RFRA042  DS    0H                                                               
         LA    R0,BOELEM                                                        
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',(R2)),(R0),(R6)                   
         SPACE 2                                                                
*---------------------------------------------------------------------*         
* EXPANSION ELEMENTS                                                            
*                                                                               
*     BOOKS ELEMENT                                                             
*---------------------------------------------------------------------*         
         LA    R6,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         USING RCPRBKEL,R6                                                      
         MVI   RCPRBKCO,RCPRBKEQ   ELEMENT CODE                                 
*                                                                               
         LA    R0,NUMBKS                                                        
         LA    RE,SAVBKS                                                        
         LA    RF,RCPRBKBK                                                      
RFRABK02 OC    0(L'SAVBK,RE),0(RE) END OF BOOKS?                                
         BZ    RFRABK04            YES                                          
         MVC   0(L'RCPRBKBK,RF),0(RE)                                           
         LA    RF,L'RCPRBKBK(RF)                                                
         LA    RE,L'SAVBK(RE)                                                   
         BCT   R0,RFRABK02                                                      
*                                                                               
RFRABK04 SR    RF,R6               ELEMENT LENGTH                               
         STC   RF,RCPRBKLN                                                      
         DROP  R6                                                               
*                                                                               
         LR    R6,R2                                                            
         LA    R6,RCONELEM-RCONKEY(R6)               WHERE DOES IT GO?          
RFRABK06 CLI   0(R6),0             END OF RECORD?                               
         BE    RFRABK08            YES                                          
         CLI   0(R6),RCPRBKEQ      IS THIS THE PLACE?                           
         BNL   RFRABK08            YES                                          
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     RFRABK06                                                         
*                                                                               
RFRABK08 DS    0H                                                               
         LA    R0,BOELEM                                                        
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',(R2)),(R0),(R6)                   
         SPACE 1                                                                
*---------------------------------------------------------------------*         
*     DAYPARTS & CPP ELEMENT                                                    
*        NEED TO BUILD SECOND TABLE TO "SCRUNCH"                                
*---------------------------------------------------------------------*         
         LA    R6,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         USING RCPRDPEL,R6                                                      
         MVI   RCPRDPCO,RCPRDPEQ                                                
         TM    MISCFLG1,MF1DPTMU   DAYPART MENU?                                
         BZ    *+8                 NO                                           
         OI    RCPRDPFL,RCPRDPMQ                                                
*                                                                               
         LA    RE,SAVDPTS                                                       
         LA    RF,RCPRDPDP                                                      
         L     R5,AIO1                                                          
         XC    0(L'SAVDPTS,R5),0(R5)   BUILD NEW LIST HERE                      
*                                                                               
RFRADP12 OC    0(L'SAVDPT,RE),0(RE)    ANY DAYPART?                             
         BZ    RFRADP14                NO - SKIP                                
         MVC   0(L'RCPRDPDP,RF),0(RE)                                           
         MVC   0(L'SAVDPT,R5),0(RE)                                             
         LA    R5,L'SAVDPT(R5)                                                  
         LA    RF,L'RCPRDPDP(RF)                                                
*                                                                               
RFRADP14 LA    RE,L'SAVDPT(RE)                                                  
         LA    R0,SAVDPTS+L'SAVDPTS                                             
         CR    RE,R0                                                            
         BL    RFRADP12                                                         
*                                                                               
         L     R5,AIO1                                                          
         MVC   SAVDPTS,0(R5)       "SCRUNCHED" TABLE                            
         SR    RF,R6               ELEMENT LENGTH                               
         STC   RF,RCPRDPLN                                                      
         DROP  R6                                                               
*                                                                               
         LR    R6,R2                                                            
         LA    R6,RCONELEM-RCONKEY(R6)               WHERE DOES IT GO?          
RFRADP16 CLI   0(R6),0             END OF RECORD?                               
         BE    RFRADP18            YES                                          
         CLI   0(R6),RCPRDPEQ      IS THIS THE PLACE?                           
         BNL   RFRADP18            YES                                          
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     RFRADP16                                                         
*                                                                               
RFRADP18 DS    0H                                                               
         LA    R0,BOELEM                                                        
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',(R2)),(R0),(R6)                   
         SPACE 2                                                                
*---------------------------------------------------------------------*         
*     COMMENT ELEMENTS                                                          
*---------------------------------------------------------------------*         
         LA    R0,SAVCOMS+(3*L'SAVCOM)                                          
         LA    R1,L'SAVCOMS-(3*L'SAVCOM)                                        
         LR    RE,RA                                                            
         AH    RE,=Y((MINCOMS+(3*L'MINCOM))-TWAD)                               
         LA    RF,L'MINCOMS-(3*L'MINCOM)                                        
         MVCL  R0,RE                                                            
*                                                                               
         LA    R5,SAVCOMS+L'SAVCOMS                                             
RFRACM16 OC    0(L'SAVCOM,R5),0(R5)     ANY COMMENT HERE??                      
         BZ    RFRACM22                 NO                                      
*                                                                               
         XC    BOELEM,BOELEM                                                    
         LA    R6,BOELEM                                                        
         MVI   0(R6),X'11'         ELEMENT CODE                                 
         ZIC   RE,0(R5)            COMMENT EX LEN                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R6),1(R5)                                                    
         LA    RE,3(RE)            ELEMENT LENGTH                               
         STC   RE,1(R6)                                                         
         LR    R6,R2                                                            
         LA    R6,RCONELEM-RCONKEY(R6)               WHERE DOES IT GO?          
RFRACM18 CLI   0(R6),0             END OF RECORD?                               
         BE    RFRACM20            YES                                          
         CLI   0(R6),X'11'         IS THIS THE PLACE?                           
         BNL   RFRACM20            YES                                          
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     RFRACM18                                                         
*                                                                               
RFRACM20 DS    0H                                                               
         LA    R0,BOELEM                                                        
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',(R2)),(R0),(R6)                   
*                                                                               
RFRACM22 SH    R5,=Y(L'SAVCOM)     PREV COMMENT                                 
         LA    RE,SAVCOMS                                                       
         CR    R5,RE                                                            
         BNL   RFRACM16                                                         
*                                                                               
         GOTO1 =A(SPREDFOR),RR=BORELO                                           
*                                                                               
         ICM   R1,15,=AL4(XIO11+XOREPFIL+XOPUT)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,SAVMKTB                                                    
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1+1(1),SAVSHGL                                             
         MH    RF,BOWORK1                                                       
         XC    BOWORK1,BOWORK1                                                  
         MVI   BOWORK1+3,100                                                    
         D     RE,BOWORK1                                                       
         STCM  RF,15,SAVSTAB                                                    
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CALL CONTRACT VIA GLOBBER TO ADD A RECORD FOR US                              
***********************************************************************         
CALLCON  DS    0H                                                               
         XC    BLOCK,BLOCK                                                      
         LA    RE,BLOCK                                                         
         USING GLVXFRSY,RE                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'SEL'    SELLERS WORKSHEET                            
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'CON'    CONTRACT PROGRAM                             
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  RE                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     R6,ACOM                                                          
         USING COMFACSD,R6                                                      
*                                                                               
         GOTO1 CGLOBBER,BODMCB,=C'PUTD',BLOCK,14,GLVXCTL                        
         CLI   BODMCB+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    BOELEM,BOELEM                                                    
         LA    R5,BOELEM                                                        
         USING RCAUTOD,R5                                                       
         MVC   RCAUSTAT,CCONKSTA                                                
         MVC   RCAUAGY,CCONKAGY                                                 
         MVC   RCAUAGOF,CCONKAOF                                                
         MVC   RCAUBUYR,ECONBUYR                                                
         MVC   RCAUADV,CCONKADV                                                 
         MVC   RCAUPRD,EPRDNAME                                                 
         MVC   RCAUSAL,CCONSAL                                                  
         MVC   RCAUDVSL,CCONDVS                                                 
         MVC   RCAUDVCT,CCONDVT                                                 
         MVC   RCAUTYPE,CCONTYPE                                                
         GOTO1 VDATCON,BODMCB,(3,CCONDAT),(2,RCAUFLT)                           
         GOTO1 (RF),(R1),(3,CCONDAT+3),(2,RCAUFLT+2)                            
         DROP  R5                                                               
*                                                                               
         GOTO1 CGLOBBER,BODMCB,=C'PUTD',BOELEM,RCAUELLQ,GLRCAUTO                
         CLI   BODMCB+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RD,BCSVRD                                                        
         B     EXITL                                                            
         DROP  R6                                                               
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
         DC    AL1(RADD),AL1(0,0,0),AL4(RLRADD)      ADD                        
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER THE I/O CALL TO ADD THE RECORD                                          
*                                                                               
* CHECK FOR CHANGE TO ANY CONTRACT KEY                                          
*        ADD THE PASSIVE KEYS                                                   
*            ON ENTRY AIO3 CONTAINS OLD CONTRACT(SEE RFRWRT)                    
*            USE AIO1 FOR OLD KEYS                                              
*            USE AIO2 FOR NEW KEYS                                              
*                                                                               
***********************************************************************         
RLRADD   DS    0H                                                               
         NI    GSINDSL1,FF-GSINOIO                                              
*                                                                               
         MVC   BOWORK1(4),ACOM                                                  
         MVC   BOWORK1+4(2),CUAALF                                              
*                                                                               
         GOTOX (RFCONKY,VREPFACS),BODMCB,AIO3,(R2),MYDSKA,BOWORK1               
*                                                                               
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
         SPACE 2                                                                
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
         SPACE 2                                                                
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
*                                                                               
*  R2 - RECORD(AT CORRECT LEVEL)                                                
***********************************************************************         
DFDDIS   DS    0H                                                               
         MVI   SVDPTLIN,0          NOT ON ANY DAYPART LINE YET                  
         MVI   SVCOMLIN,0          NOT ON ANY COMMENT LINE YET                  
         B     EXITOK                                                           
***********************************************************************         
* BEFORE VALIDATING THE DATA FIELDS                                             
***********************************************************************         
DFDVAL   DS    0H                                                               
         CLI   CNTPROFS,RREPQCNT                                                
         BE    DFDVAL0                                                          
*                                                                               
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQCNT',CNTPROFS)                 
*                                                                               
DFDVAL0  DS    0H                                                               
         MVI   SVDPTLIN,0          NOT ON ANY DAYPART LINE YET                  
         MVI   SVCOMLIN,0          NOT ON ANY COMMENT LINE YET                  
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
DLTABL   DS    0H                                                               
         DC    AL1(DDIS),AL1(0,0,0),AL4(DLDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
*                                                                               
***********************************************************************         
* AFTER DISPLAYING THE DATA FIELDS                                              
***********************************************************************         
DLDDIS   DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* AFTER VALIDATING THE DATA FIELDS                                              
***********************************************************************         
DLDVAL   DS    0H                                                               
         MVC   FVADDR,XFVADDR                                                   
         OC    AHDRCON,AHDRCON                                                  
         BNZ   *+12                                                            
         TM    MISCFLG1,MF1NOCON                                                
         BZ    EXITENTR                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
************ DOING AN ACTION ON A SPECIFIC DATA OBJECT ****************         
***********************************************************************         
DATA10   DS    0H                                                               
         LR    RF,RB               TABLE OF KNOWN OBJECTS                       
         AH    RF,=Y(KNOWTAB-PRO22)                                             
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
         DROP  RF                                                               
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
         USING RCONRTYP,R2                                                      
         ZAP   BOWORK1+20(5),=P'0'        EDIT USES 17 BYTES OF WORK            
         MVO   BOWORK1+20(5),RCONRCON                                           
         DROP  R2                                                               
         EDIT  (P5,BOWORK1+20),(8,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,      X        
               DUB=BODUB1                                                       
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE A CONTRACT FIELD                                                     
***********************************************************************         
VALCON   DS    0H                                                               
         CLC   CUAALF,=C'NK'       KATZ NATIONAL?                               
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(602)   YES - USE CONTRACT                           
         B     EXITL                                                            
*                                                                               
         NI    MISCFLG1,FF-MF1NOCON                                             
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNTR-TWAD)                                             
         XC    0(L'SVCONNTR,RE),0(RE)                                           
*                                                                               
         OC    AHDRERR,AHDRERR                                                  
         BZ    *+14                                                             
         MVC   FVMSGNO,AHDRERR                                                  
         B     EXITL               AUTO HEADER ERROR VIA CONTACT PROG           
*                                                                               
         TM    GLOBFLG,GBFBRW      BROWSE GLOBBER?                              
         BNO   VALCON00            YES                                          
*                                                                               
         TM    GLOBFLG,GBFBCAN     BROWSE CANCEL?                               
         BNO   *+18                NO                                           
         MVC   FVMSGNO,=AL2(162)                                                
         MVI   FVOMTYP,C'I'                                                     
         B     EXITL                                                            
*                                                                               
         TM    GLOBFLG,GBFBBAD     BROWSE BAD RETURN CALL?                      
         BNO   *+14                NO                                           
         MVC   FVMSGNO,=AL2(725)                                                
         B     EXITL                                                            
*                                                                               
         TM    GLOBFLG,GBFBNRF     BROWSE NO RECORDS FOUND?                     
         BNO   *+14                NO                                           
         MVC   FVMSGNO,=AL2(633)                                                
         B     EXITL                                                            
*                                                                               
VALCON00 OC    AHDRCON,AHDRCON                                                  
         BNZ   VALCON99                                                         
*                                                                               
         CLI   FVILEN,0                                                         
         BZ    *+12                IF NO INPUT                                  
         TM    FVIIND,FVIVAL                                                    
         BZ    VALCON02            OR OLD INPUT                                 
*                                                                               
         OI    MISCFLG1,MF1NOCON   SET NO CONTRACT                              
         B     VALCONX                                                          
*                                                                               
VALCON02 DS    0H                                                               
         GOTOX (VALCONQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*                                                                               
         GOTO1 =A(READCON),RR=BORELO                                            
*                                                                               
         XC    FVIFLD,FVIFLD       CLEAR CONTRACT                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         XC    0(L'SVCONNUM,RE),0(RE)                                           
*                                                                               
         B     VALCONX                                                          
*                                                                               
VALCON99 DS    0H                                                               
         OI    MISCFLG1,MF1KYCHG   THIS KEY FIELD WAS CHANGED                   
         EDIT  AHDRCON,(8,FVIFLD),ALIGN=LEFT,DUB=BODUB1,WRK=BOWORK1             
         STC   R0,FVILEN                                                        
         BCTR  R0,0                                                             
         STC   R0,FVXLEN                                                        
         OI    FVIIND,FVINUM                                                    
*                                                                               
         GOTOX (VALCONQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNTR-TWAD)                                             
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FVIFLD                                                   
         OC    0(L'SVCONNTR,RE),BCSPACES                                        
*                                                                               
VALCONX  DS    0H                                                               
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* PASS THE CONTRACT FIELD TO SOMEONE                                            
***********************************************************************         
NTRCON   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNTR-TWAD)                                             
         MVC   FVIFLD(8),0(RE)                                                  
NTRCONX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CONTRACT TYPE                                                 
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
TYPDTA   DS    0H                                                               
         LA    RF,TYPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TYPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTYP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTYP)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY TYPE FIELD                                                            
***********************************************************************         
DISTYP   DS    0H                                                               
         MVC   FVIFLD(L'CCONTYPE),CCONTYPE                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE TYPE FIELD                                                           
***********************************************************************         
VALTYP   DS    0H                                                               
         TM    MISCFLG1,MF1NOCON   CONTRACT OMMITED?                            
         BZ    DISTYP              NO DISPLAY                                   
*                                                                               
         GOTO1 =A(VALTYPE),BODMCB,RR=BORELO                                     
         BL    EXITL                                                            
         B     EXITOK                                                           
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
AGYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAGY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAGY)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY AGENCY FIELD                                                          
***********************************************************************         
DISAGY   DS    0H                                                               
         CLI   CSACT,A#ADD         IS THE ACTION ADD?                           
         BE    DISAGY5             YES - DISPLAY CODES                          
*                                                                               
         MVC   FVIFLD(L'EAGYNAM1),EAGYNAM1                                      
         B     DISAGYX                                                          
*                                                                               
DISAGY5  DS    0H                                                               
         MVC   FVIFLD(L'CCONKAGY),CCONKAGY                                      
         CLC   CCONKAOF,BCSPACES                                                
         BE    DISAGYX                                                          
*                                                                               
         LA    RE,FVIFLD+L'CCONKAGY                                             
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BNH   *-6                                                              
         MVI   1(RE),C'-'                                                       
         LA    RE,2(RE)                                                         
         MVC   0(L'CCONKAOF,RE),CCONKAOF                                        
*                                                                               
DISAGYX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE AGENCY FIELD                                                         
***********************************************************************         
VALAGY   DS    0H                                                               
         TM    MISCFLG1,MF1NOCON   CONTRACT OMMITED?                            
         BZ    DISAGY              NO DISPLAY                                   
*                                                                               
         GOTO1 =A(VALAGNCY),BODMCB,RR=BORELO                                    
         BL    EXITL                                                            
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
ADVTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISADV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADV)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY ADVERTISER FIELD                                                      
***********************************************************************         
DISADV   DS    0H                                                               
         CLI   CSACT,A#ADD         IS THE ACTION ADD?                           
         BE    DISADV5             YES - DISPLAY CODES                          
*                                                                               
         MVC   FVIFLD(L'EADVNAME),EADVNAME                                      
         B     DISADVX                                                          
*                                                                               
DISADV5  DS    0H                                                               
         MVC   FVIFLD(L'CCONKADV),CCONKADV                                      
*                                                                               
DISADVX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE ADVERTISER FIELD                                                     
***********************************************************************         
VALADV   DS    0H                                                               
         TM    MISCFLG1,MF1NOCON   CONTRACT OMMITED?                            
         BZ    DISADV              NO - DISPLAY                                 
*                                                                               
         CLI   FVILEN,0            ANY INPUT?                                   
         BE    INVADVX             NO - INVALID                                 
*                                                                               
         CLI   FVIFLD,C'='         BROWSE REQUEST?                              
         BNE   VALADV1                                                          
*                                                                               
         TM    MISCFLG1,MF1GLOB    FROM GLOBBER?                                
         BZ    VALADV0A            NO                                           
         TM    GLOBFLG,GBFBRW      BROWSE?                                      
         BNO   VALADV0A            NO                                           
         TM    GLOBFLG,GBFBADV     ADVERTISER?                                  
         BNO   VALADV0A            NO                                           
*                                                                               
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(L'BRWRET),BRWRET                                          
         OC    FVIFLD,BCSPACES                                                  
         LA    RE,FVIFLD+L'BRWRET                                               
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         STC   RE,FVXLEN                                                        
         LA    RE,1(RE)                                                         
         STC   RE,FVILEN                                                        
         B     VALADV1                                                          
*                                                                               
VALADV0A GOTO1 =A(GOOBR),BODMCB,C'ADV ',0,RR=BORELO                             
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
VALADV1  CLI   FVILEN,L'RADVKADV   TOO LONG?                                    
         BH    INVADVX             YES                                          
*                                                                               
         OC    FVIFLD(L'RADVKADV),BCSPACES   UPPERCASE                          
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
         BNE   INVADVX             ADVERTISER NOT ON RECORD                     
*                                                                               
         MVC   CCONKADV,FVIFLD                                                  
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGET)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,AIO4                                                          
         MVC   CCONCTGY,RADVCATG-RADVREC(RE)                                    
*                                                                               
VALADVX  B     EXITOK                                                           
*                                                                               
INVADVX  MVC   FVMSGNO,=AL2(INVADV) EXIT WITH INVALID ADVERTISER                
         B     EXITL                                                            
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
PRDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRD)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY PRODUCT FIELD                                                         
***********************************************************************         
DISPRD   DS    0H                                                               
         CLI   CSACT,A#ADD         IS THE ACTION ADD?                           
         BNE   DISPRD5             YES - DISPLAY CODES                          
*                                                                               
         CLC   CCONPRD,BCSPACES                                                 
         BE    DISPRD5                                                          
*                                                                               
         OC    CCONPRD,CCONPRD                                                  
         BE    DISPRD5                                                          
*                                                                               
         MVC   FVIFLD(2),=C'C='                                                 
         MVC   FVIFLD+2(L'CCONPRD),CCONPRD                                      
         B     DISPRDX                                                          
*                                                                               
DISPRD5  MVC   FVIFLD(L'EPRDNAME),EPRDNAME                                      
*                                                                               
DISPRDX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE PRODUCT FIELD                                                        
***********************************************************************         
VALPRD   DS    0H                                                               
         TM    MISCFLG1,MF1NOCON   CONTRACT OMMITED?                            
         BZ    DISPRD              NO - DISPLAY                                 
*                                                                               
         CLI   FVILEN,0            ANY INPUT?                                   
         BE    INVPRDX             NO - INVALID                                 
*                                                                               
         MVC   CCONPRD,BCSPACES                                                 
         OC    FVIFLD(L'RCONEXPR),BCSPACES                                      
         CLC   FVIFLD(2),=C'C='    IS IT A PRODUCT CODE?                        
         BNE   VALPRD10            NO                                           
*                                                                               
         CLI   FVILEN,L'RPRDKPRD+2 TOO LONG?                                    
         BH    INVPRDX             YES                                          
*                                                                               
         LA    RE,IOKEY                                                         
         USING RPRDKEY,RE                                                       
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,CCONKADV                                                
         MVC   RPRDKPRD,FVIFLD+2                                                
         MVC   RPRDKREP,CUAALF                                                  
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RPRDKEY),IOKEYSAV                                        
         BNE   INVPRDX             PRODUCT NOT ON RECORD                        
*                                                                               
VALPRD10 DS    0H                                                               
         MVC   EPRDNAME,FVIFLD                                                  
*                                                                               
VALPRDX  B     EXITOK                                                           
*                                                                               
INVPRDX  MVC   FVMSGNO,=AL2(INVPRD) EXIT WITH INVALID PRODUCT                   
         B     EXITL                                                            
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
SALTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSAL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSAL)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY SALESPERSON FIELD                                                     
***********************************************************************         
DISSAL   DS    0H                                                               
         CLI   CSACT,A#ADD         IS THE ACTION ADD?                           
         BE    DISSAL5             YES - DISPLAY CODES                          
*                                                                               
         MVC   FVIFLD(L'ESALNAME),ESALNAME                                      
         B     DISSALX                                                          
*                                                                               
DISSAL5  DS    0H                                                               
         MVC   FVIFLD(L'CCONSAL),CCONSAL                                        
*                                                                               
DISSALX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE SALESPERSON FIELD                                                    
***********************************************************************         
VALSAL   DS    0H                                                               
         TM    MISCFLG1,MF1NOCON   CONTRACT OMMITED?                            
         BZ    DISSAL              NO - DISPLAY                                 
*                                                                               
         GOTO1 =A(VALSALPR),BODMCB,RR=BORELO                                    
         BL    EXITL                                                            
         B     EXITOK                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BUYER                                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
BUYDTA   DS    0H                                                               
         LA    RF,BUYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BUYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBUY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBUY)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY BUYER FIELD                                                           
***********************************************************************         
DISBUY   DS    0H                                                               
         MVC   FVIFLD(L'ECONBUYR),ECONBUYR                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE BUYER FIELD                                                          
***********************************************************************         
VALBUY   DS    0H                                                               
         TM    MISCFLG1,MF1NOCON   CONTRACT OMMITED?                            
         BZ    DISBUY              NO - DISPLAY                                 
*                                                                               
         MVC   ECONBUYR,FVIFLD                                                  
VALBUYX  B     EXITOK                                                           
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
DISSTA   DS    0H D                                                             
         CLI   CSACT,A#ADD         IS THE ACTION ADD?                           
         BE    DISSTA5             YES - DISPLAY CODES                          
*                                                                               
         MVC   FVIFLD(L'ESTATION),ESTATION                                      
         B     DISSTAX                                                          
*                                                                               
DISSTA5  DS    0H                                                               
         MVC   AFVADDR,FVADDR                                                   
         MVC   FVIFLD(L'CCONKSTA),CCONKSTA                                      
*                                                                               
DISSTAX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE STATION FIELD                                                        
***********************************************************************         
VALSTA   DS    0H                                                               
         TM    MISCFLG1,MF1NOCON   CONTRACT OMMITED?                            
         BZ    DISSTA              NO - DISPLAY                                 
*                                                                               
         DS    0H                                                               
         GOTO1 =A(VALSTATN),BODMCB,RR=BORELO                                    
         BL    EXITL                                                            
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
FLTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFLT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFLT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY FLIGHT FIELD                                                          
***********************************************************************         
DISFLT   DS    0H                                                               
         OI    FVIIND,FVIVAL                                                    
         OC    CCONDAT,CCONDAT                                                  
         BZ    DISFLTX             NOTHING TO DO                                
*                                                                               
         GOTO1 VDATCON,BODMCB,(3,CCONDAT),(5,FVIFLD)                            
         MVI   FVIFLD+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,CCONDAT+3),(5,FVIFLD+9)                             
*                                                                               
         CLI   CSACT,A#ADD         IS IT ADD?                                   
         BNE   DISFLTX             NO                                           
*                                                                               
         TM    MISCFLG1,MF1NEWKY   DO WE WANT THE NEW KEY MESG?                 
         BZ    DISFLTX             NO - CONTINUE                                
         NI    MISCFLG1,X'FF'-MF1NEWKY                                          
*                                                                               
         MVC   FVMSGNO,=AL2(FVFENEW)                                            
         MVC   FVADDR,AFVADDR                                                   
         B     EXITL               EXIT WITH ENTER NEW KEY                      
*                                                                               
DISFLTX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE FLIGHT FIELD                                                         
***********************************************************************         
VALFLT   DS    0H                                                               
         TM    MISCFLG1,MF1NOCON   CONTRACT GIVEN??                             
         BZ    DISFLT              NO - DISPLAY                                 
*                                                                               
         CLI   FVILEN,0            ANY INPUT?                                   
         BE    EXITNV              NO - INVALID                                 
*                                                                               
         GOTO1 VPERVAL,BODMCB,(FVILEN,FVIFLD),(0,BOWORK1)                       
         CLI   4(R1),0             EVERYTHING OK?                               
         BNE   EXITNV              NO                                           
*                                                                               
         LA    R3,CCONDAT                                                       
         MVC   0(6,R3),BOWORK1+28                                               
         MVC   CHARFLT,BOWORK1+44                                               
*                                                                               
* CHECK IF K DATES EXCEED 1 CALENDAR YR (13 BROADCAST MONTHS MAX)               
*                                                                               
         GOTO1 VGTBROAD,BODMCB,(1,BOWORK1+44),BOWORK2,VGETDAY,VADDAY            
         GOTO1 VGTBROAD,BODMCB,(1,BOWORK1+50),BOWORK2+12,VGETDAY,VADDAY         
*                                                                               
         GOTO1 VADDAY,BODMCB,(C'Y',BOWORK2+6),(0,BOWORK2+6),1                   
*                                                                               
         CLC   BOWORK2+18(4),BOWORK2+6                                          
         BNH   *+14                ERROR - DATES CANT EXCEED 1 CAL YR           
         MVC   FVMSGNO,=AL2(49)                                                 
         B     EXITL                                                            
*                                                                               
         LA    RE,IOKEY                                                         
         USING RSTAKEY,RE                                                       
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,CUAALF                                                  
         MVC   RSTAKSTA,CCONKSTA                                                
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RSTAKEY),IOKEYSAV                                        
         BNE   INVSTAX             STATION NOT ON RECORD                        
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGET)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   INVSTAX             STATION NOT ON RECORD                        
*                                                                               
         L     R4,AIO4                                                          
         USING RSTAREC,R4                                                       
         CLC   RSTASTRT,0(R3)                                                   
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(287)                                                
         B     EXITL                                                            
*&&DO                                                                           
         OC    RSTAEND,RSTAEND     IGNORE LEAVE DATE                            
         BZ    VALFLTX                                                          
         CLC   RSTAEND,3(R3)                                                    
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(287)                                                
         B     EXITL                                                            
*&&                                                                             
         DROP  R4                                                               
*                                                                               
VALFLTX  B     EXITOK                                                           
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDVS)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEVELOPMENT SALESPERSON FIELD                                         
***********************************************************************         
DISDVS   DS    0H                                                               
         CLI   CSACT,A#ADD         IS THE ACTION ADD?                           
         BE    DISDVS5             YES - DISPLAY CODES                          
*                                                                               
         MVC   FVIFLD(L'EDVSNAME),EDVSNAME                                      
         B     DISDVSX                                                          
*                                                                               
DISDVS5  MVC   FVIFLD(L'CCONDVS),CCONDVS                                        
*                                                                               
DISDVSX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE DEVELOPMENT SALESPERSON FIELD                                        
***********************************************************************         
VALDVS   DS    0H                                                               
         TM    MISCFLG1,MF1NOCON   CONTRACT OMMITED?                            
         BZ    DISDVS              NO - DISPLAY                                 
*                                                                               
         CLI   FVILEN,0            ANY INPUT?                                   
         BE    VALDVSX             NO                                           
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
         MVC   CCONDVS,FVIFLD                                                   
VALDVSX  B     EXITOK                                                           
*                                                                               
INVDVSX  MVC   FVMSGNO,=AL2(INVDVSAL) EXIT WITH INVALID DEV SALESPERSON         
         B     EXITL                                                            
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDVT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEVELOPMENT TYPE                                                      
***********************************************************************         
DISDVT   DS    0H                                                               
         MVC   FVIFLD(L'CCONDVT),CCONDVT                                        
*                                                                               
DISDVTX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE DEVELOPMENT TYPE                                                     
***********************************************************************         
VALDVT   DS    0H                                                               
         TM    MISCFLG1,MF1NOCON   CONTRACT OMMITED?                            
         BZ    DISDVS              NO - DISPLAY                                 
*                                                                               
         CLI   FVILEN,0                                                         
         BNE   VALDVT5                                                          
         OC    CCONDVS,CCONDVS     DEV SALESPERSON ENTERED?                     
         BNZ   MISSINP             YES - THEN INPUT REQUIRED                    
         B     VALDVTX                                                          
*                                                                               
VALDVT5  DS    0H                                                               
         OC    CCONDVS,CCONDVS     DEV SALESPERSON ENTERED?                     
         BZ    EXITNV              NO - THEN INPUT INVALID                      
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
         MVC   CCONDVT,FVIFLD                                                   
VALDVTX  B     EXITOK                                                           
*                                                                               
INVDVTX  MVC   FVMSGNO,=AL2(INVDVTYP) EXIT WITH INVALID DEV CON TYPE            
         B     EXITL                                                            
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
DISBKS   DS    0H                  (SEE RECNT70)                                
         GOTO1 =A(DISBOOKS),BODMCB,RR=BORELO                                    
         BL    EXITL                                                            
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE BOOKS FIELD                                                          
***********************************************************************         
VALBKS   DS    0H                                                               
         OC    AHDRCON,AHDRCON                                                  
         BNZ   *+12                                                             
         TM    MISCFLG1,MF1NOCON                                                
         BZ    DISBKS                                                           
*                                                                               
         GOTO1 =A(VALBOOKS),BODMCB,RR=BORELO                                    
         BL    EXITL                                                            
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
         OC    SAVDMOS,SAVDMOS                                                  
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
         MVC   BOWORK1(L'SAVDMOS),SAVDMOS                                       
         LA    R3,NUMDEMS                                                       
*                                                                               
DISDMO10 DS    0H                                                               
         OC    0(3,R5),0(R5)       END OF DEMOS                                 
         BZ    DISDMO15                                                         
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         LA    R5,3(R5)                                                         
         BCT   R3,DISDMO10                                                      
         DROP  R6                                                               
*                                                                               
DISDMO15 MVI   0(R5),X'FF'         END OF TABLE                                 
         GOTO1 VDEMOCON,BODMCB,('NUMDEMS',BOWORK1),(9,FVIFLD),(0,AIO5)          
*                                                                               
DISDMOX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE DEMOS FIELD                                                          
***********************************************************************         
VALDMO   DS    0H                                                               
         OC    AHDRCON,AHDRCON                                                  
         BNZ   *+12                                                             
         TM    MISCFLG1,MF1NOCON                                                
         BZ    DISDMO                                                           
*                                                                               
         ZIC   R0,FVILEN                                                        
         LA    RF,FVIFLD                                                        
*                                                                               
         CLC   =C'DR',SAVBKS+2                                                  
         BE    *+14                                                             
         CLC   =C'PP',SAVBKS+2                                                  
         BNE   VALDMO01                                                         
*                                                                               
         LTR   R0,R0                                                            
         BNZ   EXITNV                                                           
         B     VALDMOX                                                          
*                                                                               
VALDMO01 LTR   R0,R0                                                            
         BZ    MISSINP                                                          
*                                                                               
         CLI   0(RF),C'='                                                       
         BNE   *+8                                                              
         MVI   0(RF),C'$'                                                       
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         GOTOX (VALDMOQ,AREPRO01),BODMCB,(C'Y',FVIHDR),                X        
               ('NUMDEMS',SAVDMOS)                                              
         BL    EXITL                                                            
*                                                                               
         LA    R1,SAVDMOS                                                       
VALDMO5  CLI   0(R1),X'FF'         END OF TABLE MARK?                           
         BE    *+12                                                             
         LA    R1,L'SAVDMO(R1)     NEXT DEMO ENTRY                              
         B     VALDMO5                                                          
         MVI   0(R1),0             CLEAR END OF TABLE MARKER                    
*                                                                               
* TEST PROFILE SET TO REQUIRE PRIMARY DEMO                                      
         TM    CONPROFS+CNTPDEMB,CNTPDEMA                                       
         BZ    VALDMOX                                                          
*                                                                               
         LA    R1,SAVDMOS                                                       
         LA    R0,8                                                             
VALDMO10 TM    0(R1),X'40'         PRIMARY DEMO CHECK                           
         BO    VALDMOX                                                          
         LA    R1,3(R1)                                                         
         BCT   R0,VALDMO10                                                      
         MVC   FVMSGNO,=AL2(243)                                                
         B     EXITL                                                            
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
         GOTO1 =A(DISLENS),BODMCB,RR=BORELO                                     
         BL    EXITL                                                            
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE LENGTHS FIELD                                                        
***********************************************************************         
VALLNS   DS    0H                                                               
         OC    AHDRCON,AHDRCON                                                  
         BNZ   *+12                                                             
         TM    MISCFLG1,MF1NOCON                                                
         BZ    DISLNS                                                           
*                                                                               
         GOTO1 =A(VALLENS),BODMCB,RR=BORELO                                     
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MARKET BUDGET                                                 
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
MKTBDTA  DS    0H                                                               
         LA    RF,MKTBTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
MKTBTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISMKTB)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMKTB)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY MARKET BUDGET FIELD                                                   
***********************************************************************         
DISMKTB  DS    0H                                                               
         TM    SAVSARF2,X'80'     'BONUS' BUDGET?                               
         BZ    *+14                NO                                           
         MVC   FVIFLD(8),=CL8'BONUS'                                            
         B     DISMKTBX                                                         
         TM    SAVSARF2,X'40'      'TRADE' BUDGET?                              
         BZ    *+14                NO                                           
         MVC   FVIFLD(8),=CL8'TRADE'                                            
         B     DISMKTBX                                                         
         TM    SAVSARF2,X'20'      'DR' BUDGET?                                 
         BZ    *+14                NO                                           
         MVC   FVIFLD(8),=CL8'DR'                                               
         B     DISMKTBX                                                         
         TM    SAVSARF2,X'10'      'ORDER' BUDGET?                              
         BZ    *+14                NO                                           
         MVC   FVIFLD(8),=CL8'ORDER'                                            
         B     DISMKTBX                                                         
         TM    SAVSARF2,X'08'      'PP' BUDGET?                                 
         BZ    *+14                NO                                           
         MVC   FVIFLD(8),=CL8'PP'                                               
         B     DISMKTBX                                                         
         TM    SAVSARF2,X'04'      'GEN AVAIL' BUDGET?                          
         BZ    *+14                NO                                           
         MVC   FVIFLD(8),=CL8'GEN AVL'                                          
         B     DISMKTBX                                                         
*                                                                               
         TM    SAVSARF1,X'40'      ZERO ENTERED?                                
         BZ    *+12                NO                                           
         MVI   FVIFLD,C'0'                                                      
         B     DISMKTBX                                                         
*                                                                               
         EDIT  SAVMKTB,(8,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,DUB=BODUB1             
DISMKTBX OC    AHDRCON,AHDRCON                                                  
         BZ    *+8                                                              
         OI    FVIIND,FVIVAL       FIELD VALIDATED PREV.                        
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE MARKET BUDGET FIELD                                                  
***********************************************************************         
VALMKTB  DS    0H                                                               
         OC    AHDRCON,AHDRCON                                                  
         BNZ   *+12                                                             
         TM    MISCFLG1,MF1NOCON                                                
         BZ    DISMKTB                                                          
*                                                                               
         TM    FVIIND,FVIVAL       HAS THE FIELD CHANGED?                       
         BNZ   *+8                 NO                                           
         OI    BDGTFLGS,BDGTMBC                                                 
*                                                                               
         CLC   CCONKADV(3),=C'GEN' IF ADV=GEN                                   
         BE    *+24                                                             
         CLC   CCONCTGY,=C'ZZ'      OR CATEGROY=ZZ                              
         BE    *+14                                                             
         CLC   CCONTYPE,=C'N'       OR TYPE=N                                   
         BNE   VALMKTB0                                                         
*                                                                               
         CLI   FVILEN,0            THEN -  BUDGET INPUT NOT ALLOWED             
         BNE   EXITNBGT                                                         
         B     VALMKTBX                                                         
*                                                                               
VALMKTB0 DS    0H                                                               
         NI    MISCFLG2,FF-MF2NOBUD                                             
         CLI   FVILEN,0            ANY INPUT                                    
         BE    VALMKTBX            NO                                           
*                                                                               
         OI    BDGTFLGS,BDGTMBE                                                 
         TM    FVIIND,FVINUM       NUMERIC INPUT?                               
         BNZ   VALMKTB2            YES                                          
         OI    MISCFLG2,MF2NOBUD                                                
*                                                                               
         OC    FVIFLD,BCSPACES                                                  
         ZIC   RE,FVXLEN           INPUT LENGTH                                 
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL8'BONUS'                                            
         BNE   *+12                                                             
         OI    SAVSARF2,X'80'      BONUS BUDGET                                 
         B     VALMKTBX                                                         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL8'TRADE'                                            
         BNE   *+12                                                             
         OI    SAVSARF2,X'40'      TRADE BUDGET                                 
         B     VALMKTBX                                                         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL8'ORDER'                                            
         BNE   *+12                                                             
         OI    SAVSARF2,X'10'      ORDER BUDGET                                 
         B     VALMKTBX                                                         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL8'DR'                                               
         BNE   *+12                                                             
         OI    SAVSARF2,X'20'      DR BUDGET                                    
         B     VALMKTBX                                                         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL8'GEN AVL'                                          
         BNE   *+12                                                             
         OI    SAVSARF2,X'04'      GEN AVAIL BUDGET                             
         B     VALMKTBX                                                         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL8'PP'                                               
         BNE   *+12                                                             
         OI    SAVSARF2,X'08'      PP BUDGET                                    
         B     VALMKTBX                                                         
*                                                                               
         B     EXITNV                                                           
*                                                                               
VALMKTB2 DS    0H                                                               
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,FVIFLD(0)                                                 
         CVB   RE,BODUB1                                                        
         LTR   RE,RE               ENTERED 0?                                   
         BNZ   *+8                 NO                                           
         OI    SAVSARF1,X'40'      MARKET BUDGET OF ZERO ENTERED                
         STCM  RE,15,SAVMKTB                                                    
*                                                                               
VALMKTBX OC    AHDRCON,AHDRCON                                                  
         BZ    *+8                                                              
         OI    FVIIND,FVIVAL       FIELD VALIDATED PREV.                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SHARE GOAL                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SHGLDTA  DS    0H                                                               
         LA    RF,SHGLTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SHGLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSHGL)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSHGL)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY SHARE GOAL FIELD                                                      
***********************************************************************         
DISSHGL  DS    0H                                                               
         MVC   FVIFLD,BCSPACES                                                  
         CLI   SAVSARF2,0                                                       
         BNE   DISSHGLX                                                         
*                                                                               
         TM    SAVSARF1,X'20'      ZERO ENTERED?                                
         BZ    *+12                NO                                           
         MVI   FVIFLD,C'0'                                                      
         B     DISSHGLX                                                         
*                                                                               
         EDIT  SAVSHGL,(5,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,DUB=BODUB1             
DISSHGLX OC    AHDRCON,AHDRCON                                                  
         BZ    *+8                                                              
         OI    FVIIND,FVIVAL       FIELD VALIDATED PREV.                        
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE SHARE GOAL FIELD                                                     
***********************************************************************         
VALSHGL  DS    0H                                                               
         OC    AHDRCON,AHDRCON                                                  
         BNZ   *+12                                                             
         TM    MISCFLG1,MF1NOCON                                                
         BZ    DISSHGL                                                          
*                                                                               
         TM    MISCFLG2,MF2NOBUD                                                
         BO    VALSHGLX                                                         
*                                                                               
         TM    FVIIND,FVIVAL       HAS THE FIELD CHANGED?                       
         BNZ   *+8                 NO                                           
         OI    BDGTFLGS,BDGTSGC                                                 
*                                                                               
         CLC   CCONKADV(3),=C'GEN' IF ADV=GEN                                   
         BE    *+24                                                             
         CLC   CCONCTGY,=C'ZZ'      OR CATEGROY=ZZ                              
         BE    *+14                                                             
         CLC   CCONTYPE,=C'N'       OR TYPE=N                                   
         BNE   VALSHGL5                                                         
*                                                                               
         CLI   FVILEN,0            THEN -  BUDGET INPUT NOT ALLOWED             
         BNE   EXITNBGT                                                         
         B     VALSHGLX                                                         
*                                                                               
VALSHGL5 CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
         OI    BDGTFLGS,BDGTSGE                                                 
*                                                                               
         TM    FVIIND,FVINUM       NUMERIC INPUT?                               
         BZ    EXITNOTN                                                         
*                                                                               
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,FVIFLD(0)                                                 
         CVB   RE,BODUB1                                                        
         CH    RE,=H'100'          GREATER THAN 100%?                           
         BH    EXITNV              YES - INVALID                                
         LTR   RE,RE               ENTERED 0?                                   
         BNZ   *+8                 NO                                           
         OI    SAVSARF1,X'20'      SHARE GOAL OF ZERO ENTERED                   
         STC   RE,SAVSHGL                                                       
                                                                                
*                                                                               
VALSHGLX OC    AHDRCON,AHDRCON                                                  
         BZ    *+8                                                              
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR STATION BUDGET                                                
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
STABDTA  DS    0H                                                               
         LA    RF,STABTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
STABTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTAB)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTAB)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY STATION BUDGET FIELD                                                  
***********************************************************************         
DISSTAB  DS    0H                                                               
         MVC   FVIFLD,BCSPACES                                                  
         CLI   SAVSARF2,0                                                       
         BNE   DISSTABX                                                         
*                                                                               
         TM    SAVSARF1,X'20'+X'40'  ZERO MKTB OR SHGL?                         
         BZ    *+12                  NO                                         
         MVI   FVIFLD,C'0'                                                      
         B     DISSTABX                                                         
*                                                                               
         EDIT  SAVSTAB,(8,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,DUB=BODUB1             
DISSTABX OC    AHDRCON,AHDRCON                                                  
         BZ    *+8                                                              
         OI    FVIIND,FVIVAL       FIELD VALIDATED PREV.                        
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE STATION BUDGET FIELD                                                 
***********************************************************************         
VALSTAB  DS    0H                                                               
         OC    AHDRCON,AHDRCON                                                  
         BNZ   *+12                                                             
         TM    MISCFLG1,MF1NOCON                                                
         BZ    DISSTAB                                                          
*                                                                               
         TM    MISCFLG2,MF2NOBUD                                                
         BO    VALSTABX                                                         
*                                                                               
         CLC   CCONKADV(3),=C'GEN' IF ADV=GEN                                   
         BE    *+24                                                             
         CLC   CCONCTGY,=C'ZZ'      OR CATEGROY=ZZ                              
         BE    *+14                                                             
         CLC   CCONTYPE,=C'N'       OR TYPE=N                                   
         BNE   VALSTAB5                                                         
*                                                                               
         CLI   FVILEN,0            THEN -  BUDGET INPUT NOT ALLOWED             
         BNE   EXITNBGT                                                         
         XC    SAVMKTB,SAVMKTB                                                  
         XC    SAVSTAB,SAVSTAB                                                  
         XC    SAVSHGL,SAVSHGL                                                  
         B     VALSTABX                                                         
*                                                                               
VALSTAB5 CLI   FVILEN,0            ANY INPUT                                    
         BNE   VALSTAB7            YES                                          
         TM    BDGTFLGS,BDGTMBE+BDGTSGE                                         
         BNO   EXITNV              NEED THE MKTB & SHGL                         
         B     VALSTABX                                                         
*                                                                               
VALSTAB7 TM    FVIIND,FVINUM       NUMERIC INPUT?                               
         BZ    EXITNOTN                                                         
*                                                                               
         TM    FVIIND,FVIVAL       HAS THE FIELD CHANGED?                       
         BNZ   *+8                 NO                                           
         OI    BDGTFLGS,BDGTSBC                                                 
*                                                                               
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,FVIFLD(0)                                                 
         CVB   RE,BODUB1                                                        
         STCM  RE,15,SAVSTAB                                                    
*                                                                               
         TM    BDGTFLGS,BDGTSGE    THE SHARE GOAL IS ALWAYS NEEDED              
         BZ    EXITNV                                                           
*                                                                               
VALSTABX OC    AHDRCON,AHDRCON                                                  
         BZ    *+8                                                              
         OI    FVIIND,FVIVAL                                                    
*                                                                               
         CLC   CCONKADV(3),=C'GEN'                                              
         BE    DLDVALX            IF ADV=GEN                                    
         CLC   CCONCTGY,=C'ZZ'                                                  
         BE    DLDVALX              OR CATEGORY=ZZ                              
         CLC   CCONTYPE,=C'N'                                                   
         BE    DLDVALX              0R TYPE=N THEN NOTHING TO DO HERE           
*                                                                               
         OI    SAVSARF1,X'80'      SET MARKET=BUDGET                            
*                                                                               
*                                              BUDGET CALCULATION               
         TM    BDGTFLGS,BDGTMBC    IF MARKET BUDGET CHANGED                     
         BO    DLDVAL10            CALC STATION BUDGET(FOR DISPLAY)             
*                                                                               
         TM    BDGTFLGS,BDGTSBC    IF STATION BUDGET CHANGED                    
         BO    DLDVAL20            CALC. MARKET BUDGET                          
*                                                                               
         TM    BDGTFLGS,BDGTSGC    IF SHARE GOAL CHANGED                        
         BO    DLDVAL10            CALC STATION BUDGET(FOR DISPLAY)             
*                                                                               
         B     *+12                SKIP TEST                                    
DLDVAL10 TM    BDGTFLGS,BDGTMBE    MARKET BUDGET ENTERED?                       
         BZ    DLDVAL20            NO - WAS CLEARED BY USER, RECALC.            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,SAVMKTB                                                    
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1+1(1),SAVSHGL                                             
         MH    RF,BOWORK1                                                       
         XC    BOWORK1,BOWORK1                                                  
         MVI   BOWORK1+3,100                                                    
         D     RE,BOWORK1                                                       
         STCM  RF,15,SAVSTAB                                                    
         B     DLDVALX                                                          
*                                                                               
DLDVAL20 DS    0H                                                               
         TM    SAVSARF1,X'20'      0% SHARE?                                    
         BZ    DLDVAL25            NO                                           
         XC    SAVSTAB,SAVSTAB     SET EVERYTHING TO ZERO                       
         XC    SAVMKTB,SAVMKTB                                                  
         OI    SAVSARF1,X'40'+X'20'                                             
         B     DLDVALX                                                          
*                                                                               
DLDVAL25 DS    0H                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,SAVSTAB                                                    
         MH    RF,=H'100'                                                       
         XC    BOWORK1(24),BOWORK1                                              
         MVC   BOWORK1+3(1),SAVSHGL                                             
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         A     RF,BOWORK1          ADD SHARE FOR HALF-ROUND                     
         D     RE,BOWORK1                                                       
         SRA   RF,1                HALVE AFTER DIVIDE                           
         STCM  RF,15,SAVMKTB       SAVE MARKET DOLLARS                          
         SR    RE,RE               DO MKT$$ REPRODUCE STA$ ENTERED?             
         M     RE,BOWORK1          MULTIPLY BY SHARE                            
         SR    RE,RE                                                            
*                                                                               
*                                  DON'T HALF-ROUND THIS CALCULATION!!          
         D     RE,=F'100'          DIVIDE BY 100                                
         CLM   RF,15,SAVSTAB       SAME VALUE?                                  
         BE    DLDVALX             YES - LEAVE AS IS                            
         ZICM  RF,SAVMKTB,4        NO  - ROUND IT UP AGAIN                      
         LA    RF,1(RF)                                                         
         STCM  RF,15,SAVMKTB       SAVE MARKET DOLLARS AGAIN                    
*                                  DON'T HALF-ROUND THIS CALCULATION!!          
DLDVALX  XC    BDGTFLGS,BDGTFLGS                                                
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DAYPART FIELD                                                         
***********************************************************************         
DISDPT   DS    0H                                                               
         GOTO1 =A(DISDAYPT),BODMCB,RR=BORELO                                    
         BL    EXITL                                                            
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE DAYPART FIELD                                                        
*                                                                               
*   USES HARDCODED DAYPART TABLE IF                                             
*            THE CCONDPMQ BIT IS ON IN CCONFLG1                                 
*        OR                                                                     
*            THERE ARE NO DAYPART RECORDS TO USE                                
*                                                                               
*   USES DAYPART RECORDS IF                                                     
*            THE CCONDPMQ BIT IS OFF IN CCONFLG1                                
*        AND                                                                    
*            THERE ARE DAYPART RECORDS TO USE                                   
*                                                                               
***********************************************************************         
VALDPT   DS    0H                                                               
         OC    AHDRCON,AHDRCON                                                  
         BNZ   *+12                                                             
         TM    MISCFLG1,MF1NOCON                                                
         BZ    DISDPT                                                           
*                                                                               
         GOTO1 =A(VALDAYPT),BODMCB,RR=BORELO                                    
         BL    EXITL                                                            
         B     EXITOK                                                           
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
         LA    R1,SAVDPTS(R1)                                                   
*                                                                               
         OC    0(L'SAVDPT,R1),0(R1)      ANY DAYPART?                           
         BZ    DISCPPX                                                          
         L     R0,1(R1)                                                         
         EDIT  (R0),(8,FVIFLD),2,WRK=BOWORK1,DUB=BODUB1,ALIGN=LEFT              
DISCPPX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE CPP FIELD                                                            
***********************************************************************         
VALCPP   DS    0H                                                               
         OC    AHDRCON,AHDRCON                                                  
         BNZ   *+12                                                             
         TM    MISCFLG1,MF1NOCON                                                
         BZ    DISCPP                                                           
*                                                                               
         ZIC   R1,SVDPTLIN         WE'RE ON A NEW (OR 1ST) DAYPART LINE         
         BCTR  R1,0                RE = A(ENTRY FOR THIS DAYPART)               
         MH    R1,=Y(L'SAVDPT)                                                  
         LA    R2,SAVDPTS(R1)                                                   
*                                                                               
         CLI   FVILEN,0                ANY INPUT ON THIS LINE?                  
         BE    VALCPPX                 NO, OKAY                                 
*                                                                               
         CLI   0(R2),C' '          ANY DAYPART                                  
         BNH   EXITNV                                                           
*                                                                               
         ZIC   R0,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(2,FVIFLD),(R0)                                  
         CLI   0(R1),0                                                          
         BNE   EXITNV                                                           
         CLC   =F'9999999',4(R1)                                                
         BL    EXITNV              WON'T DISPLAY                                
         MVC   1(4,R2),4(R1)                                                    
*                                                                               
VALCPPX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMMENTS                                                      
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
         ZIC   R1,SVCOMLIN         GET DAYPART LINE                             
         LA    R0,1(R1)                                                         
         STC   R0,SVCOMLIN                                                      
*                                                                               
         MH    R1,=Y(L'SAVCOM)                                                  
         LA    RE,SAVCOMS(R1)                                                   
*                                                                               
         OC    0(L'SAVCOM,RE),0(RE)      ANY COMMENT ?                          
         BZ    DISCOMX                   NO                                     
         MVC   FVIFLD(L'SAVCOM),1(RE)                                           
*                                                                               
DISCOMX  DS    0H                                                               
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE COMMENT FIELD                                                        
***********************************************************************         
VALCOM   DS    0H                                                               
         OC    AHDRCON,AHDRCON                                                  
         BNZ   *+12                                                             
         TM    MISCFLG1,MF1NOCON                                                
         BZ    DISCOM                                                           
*                                                                               
         TM    FVIIND,FVIVAL       COMMENTS CHANGED?                            
         BNZ   VALCOM2             NO -  KEEP BUFFERED COMMENTS                 
*                                                                               
         LR    R0,RA               CLEAR COMMENTS, BUT HERE?                    
         AH    R0,=Y(MINCOMS-TWAD)                                              
         LA    R1,L'MINCOMS                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
VALCOM2  ZIC   R1,SVCOMLIN                                                      
         LA    R0,1(R1)                                                         
         STC   R0,SVCOMLIN                                                      
         MH    R1,=Y(L'SAVCOM)                                                  
         LA    RF,SAVCOMS(R1)                                                   
*                                                                               
         CLI   FVILEN,0            ANY INPUT?                                   
         BNZ   VALCOM5             YES                                          
*                                                                               
         CLI   SVCOMLIN,1                                                       
         BNE   VALCOMX                                                          
         TM    CONPROFS+CNTPCOMB,CNTPCOMA                                       
         BNZ   VALDMOX             COMMENT OPTIONAL                             
         B     REQCOMX             1 COMMENT REQUIRED                           
*                                                                               
VALCOM5  ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),FVIFLD                                                   
         STC   RE,0(RF)            STORE COMMENT EX LEN                         
*                                                                               
VALCOMX  DS    0H                                                               
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
*                                                                               
REQCOMX  MVC   FVMSGNO,=AL2(REQCOM) EXIT WITH COMMENT REQUIRED                  
         B     EXITL                                                            
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
DSKTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSK)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DISK ADDRESS                                                          
***********************************************************************         
DISDSK   DS    0H                                                               
         GOTO1 VHEXOUT,BODMCB,GSRECDA,FVIFLD,L'GSRECDA                          
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
NTROUT   DS    0H                                                               
         CLI   SREC,O#MAX          CHECK FOR CONTROLLER                         
         BNH   EXITOK                                                           
*                                                                               
         NI    GSINDSL1,X'FF'-GSINOIO    TURN OFF OUR IO'S                      
         NI    SNINDS1,X'FF'-SNIPARMS    TURN OFF PARAMETER PASSING             
*                                                                               
         CLI   CSACT,A#LST         IS IT LIST                                   
         BE    NTROUTX             YES - EXIT                                   
         CLI   SREC,R#PEND         GOING INTO SAME RECORD?                      
         BNE   NTROUT5             NO - PASS PARAMETERS                         
*                                                                               
P        USING SSAVD,PSSAV         NEED TO CHECK ACTION                         
         CLI   P.SACT,A#LST        PREV ACTION LIST                             
         BE    NTROUTX             YES - NOTHING TO PASS                        
         DROP  P                                                                
*                                                                               
NTROUT5  OI    SNINDS1,SNIPARMS    SO WE CAN GET DNTR                           
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
* PROCESS PARAMETER LIST FOR RETURN FROM PFKEY/SEESION CHANGE                   
***********************************************************************         
         PUSH  USING                                                            
NTRXIN   DS    0H                                                               
         OI    MISCFLG1,MF1PFRET   RETURNING FROM PFKEY                         
NTRXINX  B     EXITOK                                                           
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
         USING SSAVD,R2                                                         
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
         CLI   CSACT,A#ADD         IS THE ACTION 'ADD'?                         
         BNE   *+12                NO                                           
         MVI   GSSKCODE,KYADD                                                   
         B     SETKSCRX                                                         
*                                                                               
         MVI   GSSKCODE,KYDEF      SET TO DEFAULT KEY SCREEN                    
*                                                                               
SETKSCRX B     EXITOK                                                           
***********************************************************************         
* SET THE DATA SCREEN CODE                                                      
***********************************************************************         
SETSCR   DS    0H                                                               
         MVI   GSSMCODE,0          CLEAR THE CODE                               
*                                                                               
         CLI   CSACT,A#CMNT        IS THE ACTION 'COMMENTS' ?                   
         BNE   *+12                NO                                           
         MVI   GSSMCODE,DTCMNT                                                  
         B     SETSCRX                                                          
*                                                                               
         CLI   CSACT,A#ADD         IS THE ACTION 'ADD'?                         
         BNE   *+12                NO                                           
         MVI   GSSMCODE,DTADD                                                   
         B     SETSCRX                                                          
*                                                                               
         MVI   GSSMCODE,DTDEF      SET TO DEFAULT DATA SCREEN                   
*                                                                               
SETSCRX  B     EXITOK                                                           
***********************************************************************         
* MODIFY THE SCREEN FIELDS (PULL OUT THE KEYS FROM AKYFLD)                      
***********************************************************************         
MODSCR   DS    0H                                                               
         GOTO1 =A(MODSCRN),BODMCB,RR=BORELO                                     
         B     EXITOK                                                           
***********************************************************************         
* TABLE OF WANTED KEY OBJECTS                                                   
***********************************************************************         
KNOWTAB2 DS    0XL(KNOWLQ)                                                      
         DC    AL2(00001),AL4(FLDPROT)   CONTRACT                               
         DC    AL2(EOT)                                                         
*                                                                               
FLDPROT  DC    H'0'                DUMMY BRANCH                                 
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
INVAGYX  MVC   FVMSGNO,=AL2(INVAGY)                                             
         B     EXITL               EXIT WITH INVALID AGENCY                     
INVSALX  MVC   FVMSGNO,=AL2(INVSALP)                                            
         B     EXITL               EXIT WITH INVALID SALESPERSON                
INVSTAX  MVC   FVMSGNO,=AL2(INVSTA)                                             
         B     EXITL               EXIT WITH INVALID STATION                    
MOFAGYX  MVC   FVMSGNO,=AL2(MULTIAOF)                                           
         B     EXITL               EXIT WITH MULTI OFFICE AGENCY                
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
EXITRCAE MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITL               EXIT WITH RECORD ALREADY EXISTS              
EXITCRES MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITL               EXIT WITH RECORD CAN'T BE RESTORED           
INVLUPGD MVC   FVMSGNO,=AL2(INVUPGRD)                                           
         B     EXITL               INVALID UPGRADE EXPRESSION                   
AUTOERR  MVC   FVMSGNO,AHDRERR                                                  
         B     EXITL               AUTO HEADER ERROR VIA CONTACT PROG           
EXITNBGT MVC   FVMSGNO,=AL2(592)                                                
         B     EXITL               NO BUDGET DATA ALLOWED                       
*                                                                               
MISSINP  MVC   FVMSGNO,=AL2(CE#MISIF)                                           
         B     EXITL               MISSING INPUT FIELD                          
*                                                                               
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
         OI    TWASRVH+1,X'01'     SRERVICE REQUEST ALWAYS MODIFIED             
         OI    TWASRVH+6,X'80'                                                  
         OI    GCINDS1,GCIPROT             NEVER PROTECT ON NTRSES              
         NI    GSINDSL1,X'FF'-GSINOIO      TURN OFF OUR IO'S                    
         OI    GSINDSL1,GSIXKEY    NO ENTER KEY MESG                            
         CLI   PSREC,R#PRO                                                      
         BNE   INIT01                                                           
         CLI   PSACT,A#ADD                                                      
         BNE   INIT01                                                           
*                                                                               
         GOTOX AGEN,BODMCB,OSES,SRESLVL                                         
*                                                                               
INIT01   DS    0H                                                               
         XC    AHDRCON,AHDRCON     CLEAR GLOBBER CON #                          
         XC    AHDRERR,AHDRERR     CLEAR GLOBBER ERR #                          
         XC    BRWRET,BRWRET       CLEAR BROWSE RETURN VALUE                    
         MVI   GLOBFLG,0                                                        
         NI    MISCFLG1,X'FF'-MF1GLOB                                           
*                                                                               
         L     RF,ACOM                                                          
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),BODMCB,=C'GETD',BOWORK1,24,GLVXCTL                          
         TM    BODMCB+8,X'10'      NO VARIABLES FOUND, SKIP                     
         BNZ   INITX                                                            
*                                                                               
         L     RF,ACOM                                                          
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),BODMCB,=C'DELE',,,GLVXCTL                                   
*                                                                               
         CLC   =C'SEL',BOWORK1+(GLVXFRPR-GLVXFRSY)                              
         BNE   INIT03              NOT FROM SELF                                
*                                                                               
         MVC   BASMSG(45),=CL45'AUTOHEADER ERROR - CONTACT DDS'                 
         OI    BASMSGH+6,X'80'                                                  
         DC    H'0',C'$ABEND'                                                   
*                                                                               
INIT03   DS    0H                                                               
         OI    MISCFLG1,MF1GLOB                                                 
*                                                                               
         XC    TWASRV,TWASRV                                                    
         MVC   TWASRV(3),=C'=RE'                                                
         OI    TWASRVH+6,X'80'                                                  
         MVI   TWASRVH+5,3                                                      
*                                                                               
         CLC   =C'CON',BOWORK1+(GLVXFRPR-GLVXFRSY)                              
         BNE   INIT10              NOT FROM CONTRACT                            
*                                                                               
         OI    GLOBFLG,GBFCON                                                   
*                                                                               
         L     RF,ACOM                                                          
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),BODMCB,=C'GETD',BOELEM,RCAUELLQ,GLRCAUTO                    
         TM    BODMCB+8,X'10'      NO VARIABLES FOUND, SKIP                     
         BNZ   INITX                                                            
*                                                                               
         LA    R6,BOELEM                                                        
         USING RCAUTOD,R6                                                       
*                                                                               
         ZAP   BODUB1,=P'0'                                                     
         MVO   BODUB1,RCAUCON#                                                  
         CVB   R0,BODUB1                                                        
         STCM  R0,15,AHDRCON                                                    
*                                                                               
         MVC   AHDRERR,RCAUERR#                                                 
         OI    GLOBFLG,GBFCADD                                                  
*                                                                               
         OC    AHDRCON,AHDRCON                                                  
         BNZ   INITX                                                            
         OC    AHDRERR,AHDRERR                                                  
         BNZ   INITX                                                            
         DC    H'0'                SOMETHING VERY BAD                           
*                                                                               
INIT10   DS    0H                                                               
         CLC   =C'BRO',BOWORK1+(GLVXFRPR-GLVXFRSY)                              
         BNE   INIT20              NOT FROM BROWSE                              
*                                                                               
         OI    GLOBFLG,GBFBRW                                                   
*                                                                               
         L     RF,ACOM                                                          
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),BODMCB,=C'GETD',BOELEM,GLBRWLNQ,GLRBRWSE                    
         TM    BODMCB+8,X'10'      BROWSE ELEM?                                 
         BZ    *+12                YES                                          
         OI    GLOBFLG,GBFBCAN     SET CANCEL - ROB SAYS DIE, I DON'T           
         B     INITX                                                            
*                                                                               
         L     RF,ACOM                                                          
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),BODMCB,=C'DELE',,,GLRBRWSE                                  
*                                                                               
         LA    R3,BOELEM                                                        
         USING GLBRWKW,R3                                                       
*                                                                               
         CLC   =C'BAD',GLBRWREC  **CASE OF BAD RETURN CALL**                    
         BNE   *+12                                                             
         OI    GLOBFLG,GBFBBAD                                                  
         B     INITX                                                            
*                                                                               
         CLC   =C'CAN',GLBRWREC  **CASE OF CANCEL RETURNED**                    
         BNE   *+12                                                             
         OI    GLOBFLG,GBFBCAN                                                  
         B     INITX                                                            
*                                                                               
         OC    GLBRWKW,BCSPACES                                                 
         CLC   GLBRWKW,BCSPACES     NO RECORD RETURNED?                         
         BNE   *+12                                                             
         OI    GLOBFLG,GBFBNRF      SET NO RECORDS FOUND                        
         B     INITX                                                            
*                                                                               
         MVC   BRWRET,GLBRWKW                                                   
         CLC   =C'AGY',GLBRWREC  **CASE OF AGY REC RETURNED**                   
         BNE   *+12                                                             
         OI    GLOBFLG,GBFBAGY                                                  
         B     INITX                                                            
*                                                                               
         CLC   =C'ADV',GLBRWREC  **CASE OF ADV REC RETURNED**                   
         BNE   *+12                                                             
         OI    GLOBFLG,GBFBADV                                                  
         B     INITX                                                            
*                                                                               
         CLC   =C'SAL',GLBRWREC  **CASE OF SAL REC RETURNED**                   
         BNE   *+12                                                             
         OI    GLOBFLG,GBFBSAL                                                  
         B     INITX                                                            
*                                                                               
         B     INITX                                                            
         DROP  R3                                                               
*                                                                               
INIT20   DS    0H                                                               
INITX    B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY OBJECT                                                                  
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
         DS    0D                                                               
         USING *,RB                                                             
PFKEY    LR    RB,RF                                                            
         B     *+12                                                             
         DC    CL8'**PFKEY**'                                                   
*                                                                               
         LM    R0,R3,SVPARMS                                                    
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
         CLC   CUAALF,=C'NK'       KATZ NATIONAL?                               
         BNE   PFKYES              NO                                           
*                                                                               
         L     RE,8(R1)            YES - NO ADDING PROPOSALS                    
         USING FRPELD,RE                                                        
         CLI   FRPPFK#,PFPROPSL                                                 
         BE    PFKNO                                                            
         CLI   FRPPFK#,PFKWRKUP                                                 
         BE    PFKNO                                                            
         DROP  RE                                                               
*                                                                               
PFKYES   B     EXITOK                                                           
PFKNO    B     EXITL                                                            
***********************************************************************         
* CAN SET THE RECORD FOR THE PFKEY                                              
***********************************************************************         
RECPFK   DS    0H                                                               
         L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),PFKINTR       PENDING INTERNAL KEY?                        
         BNE   RECPFK10            NO - SOME OTHER RECORD                       
*                                                                               
         MVC   FVIFLD(8),=CL8'Pend'                                             
         B     RECPFKX                                                          
*                                                                               
RECPFK10 CLI   0(RE),PFPROPSL      PROPOSAL RECORD PFKEY?                       
         BNE   RECPFK12            NO - SOME OTHER KEY                          
*                                                                               
         MVC   FVIFLD(8),=CL8'Pro'                                              
         B     RECPFKX                                                          
*                                                                               
RECPFK12 CLI   0(RE),PFKWRKUP      WORK RECORD PFKEY?                           
         BNE   RECPFK20            NO - SOME OTHER KEY                          
*                                                                               
         MVC   FVIFLD(8),=CL8'Work'                                             
         B     RECPFKX                                                          
*                                                                               
RECPFK20 CLI   0(RE),PFKYUP                                                     
         BNL   NOTPFK              DON'T OUTPUT RECORD NAME                     
*                                                                               
RECPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE ACTION FOR THE PFKEY                                              
***********************************************************************         
ACTPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),PFKINTR       PENDING INTERNAL KEY?                        
         BNE   ACTPFK10            NO - SOME OTHER RECORD                       
*                                                                               
         CLI   CSACT,A#CMNT        ACTION COMMENTS?                             
         BE    ACTPFK5             YES - PFKEY IS TO CHANGE                     
*                                                                               
         MVC   FVIFLD(8),=CL8'Comments'                                         
         B     ACTPFKX                                                          
*                                                                               
ACTPFK5  MVC   FVIFLD(8),=CL8'Cha'                                              
         B     ACTPFKX                                                          
*                                                                               
ACTPFK10 CLI   0(RE),PFPROPSL      PROPOSAL RECORD PFKEY?                       
         BNE   ACTPFK12            NO - SOME OTHER KEY                          
*                                                                               
         MVC   FVIFLD(8),=CL8'Add'                                              
         B     ACTPFKX                                                          
*                                                                               
ACTPFK12 CLI   0(RE),PFKWRKUP      WORK RECORD PFKEY?                           
         BNE   ACTPFK14            NO - SOME OTHER KEY                          
*                                                                               
         MVC   FVIFLD(8),=CL8'Update'                                           
         B     ACTPFKX                                                          
*                                                                               
ACTPFK14 DS    0H                                                               
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
         CLI   0(RE),PFKYNEXT                                                   
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
         SPACE 4                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ CONTRACT INFO ON ROLLOVER                                                
***********************************************************************         
READCON  NTR1  BASE=*,LABEL=*                                                   
         XC    SAVBKS,SAVBKS                                                    
         XC    SAVDMOS,SAVDMOS                                                  
         XC    SAVDPTS,SAVDPTS                                                  
         XC    SAVMKTB,SAVMKTB                                                  
         MVI   SAVSHGL,0                                                        
         XC    SAVSTAB,SAVSTAB                                                  
         MVI   SAVSARF1,0                                                       
         MVI   SAVSARF2,0                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RCONPTYP,RE          OF COURSE IT WAS ALREADY READ               
         MVI   RCONPTYP,X'8C'       IN VALCON(VIA AREPRO01).                    
         MVC   RCONPREP,CUAALF                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RCONKEY),IOKEYSAV                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPFIL+XOGETRUP)                               
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO4             BUILD FROM THE ROLLOVER CONTRACT             
         LA    R6,RCONELEM-RCONKEY(R2)                                          
         MVI   ELCODE,X'12'      GET SAR ELEMENT (IF ANY)                       
         USING RSARXEL,R6                                                       
         BAS   RE,FIRSTEL                                                       
         BNE   RDCNCM                                                           
         SPACE 2                                                                
******************                                                              
** COPY LENGTHS **                                                              
******************                                                              
         LA    RE,6                   HAS A MAX OF 6 LENGTHS                    
         LA    RF,RSARXRFR                                                      
         LA    R1,SAVSLNS                                                       
RDCNLN10 OC    0(2,RF),0(RF)          ANY MORE LENGTHS?                         
         BZ    RDCNLNX                NO                                        
*                                                                               
         MVC   0(L'SAVSLN,R1),1(RF)   COPY THE LENGTH (1 BYTE)                  
         LA    RF,2(RF)                                                         
         LA    R1,L'SAVSLN(R1)                                                  
         BCT   RE,RDCNLN10                                                      
*                                                                               
RDCNLNX  DS    0H                                                               
         SPACE 2                                                                
******************************                                                  
** COPY BUDGET INFO & FLAGS **                                                  
******************************                                                  
         MVC   SAVMKTB,RSARXBGT                                                 
         MVC   SAVSHGL,RSARXSHG                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,SAVMKTB                                                    
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1+1(1),SAVSHGL                                             
         MH    RF,BOWORK1                                                       
         XC    BOWORK1,BOWORK1                                                  
         MVI   BOWORK1+3,100                                                    
         D     RE,BOWORK1                                                       
         STCM  RF,15,SAVSTAB                                                    
*                                                                               
RDCNBF10 XC    SAVTGRP,SAVTGRP                                                  
         MVC   SAVTGRP+(L'SAVTGRP-L'RSARXGTO)(L'RSARXGTO),RSARXGTO              
         MVC   SAVSARF1,RSARXFLG                                                
         MVC   SAVSARF2,RSARXFL2                                                
*                                                                               
         TM    RSARXFLG,X'04'      PROPOSAL EXPANSION USED?                     
         BO    RDCNEXP             YES                                          
         SPACE 2                                                                
*-------------------------------------                                          
* COPY INFORMATION FROM SARX ELEMENT                                            
*--------------------------------------                                         
         CLC   RSARXBKS(2),=C'DR'  FOR DIRECT RESPONSE - SKIP BOOKS             
         BE    *+14                AND DEMOS                                    
         CLC   RSARXBKS(2),=C'PP'  PAID PROGRAMMING - SKIP BOOKS                
         BNE   RDCNSAR1            AND DEMOS                                    
*                                                                               
         MVC   SAVBKS+2(2),RSARXBKS                                             
         B     RDCNSAR2                                                         
*                                                                               
RDCNSAR1 LA    R0,6                NUMBER OF BOOKS TO COPY                      
         LA    RE,RSARXBKS                                                      
         LA    RF,SAVBKS                                                        
         MVC   2(3,RF),0(RE)       MOVE BOOK                                    
         LA    RF,5(RF)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         MVC   SAVDMOS,RSARXDEM    MOVE DEOMS                                   
*                                                                               
RDCNSAR2 DS    0H                                                               
         LA    R0,6                NUMBER OF DAYPARTS                           
         LA    RE,RSARXDPT                                                      
         LA    RF,SAVDPTS                                                       
         MVC   0(1,RF),0(RE)       MOVE DAYPART                                 
         MVC   3(2,RF),1(RE)       MOVE CPP                                     
         LA    RF,5(RF)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,*-20                                                          
*                                                                               
         OI    CCONFLG1,CCONDPMQ   OLD CONTRACT MUST USE TABLE                  
         B     RDCNSEX                                                          
         SPACE 1                                                                
*--------------------------------------------------                             
* COPY INFORMATION FROM PROPOSAL EXPANSION ELEMENTS                             
*--------------------------------------------------                             
RDCNEXP  CLC   RSARXBKS(2),=C'DR'  FOR DIRECT RESPONSE - SKIP BOOKS             
         BE    *+14                AND DEMOS                                    
         CLC   RSARXBKS(2),=C'PP'  FOR PAID PROGRAMING - SKIP BOOKS             
         BNE   RDCNEXP1            AND DEMOS                                    
*                                                                               
         MVC   SAVBKS+2(2),RSARXBKS                                             
         B     RDCNEXP2                                                         
*                                                                               
RDCNEXP1 MVC   SAVDMOS,RSARXDEM    MOVE DEMOS                                   
*                                                                               
         LA    R6,RCONELEM-RCONKEY(R2)                                          
         MVI   ELCODE,RCPRBKEQ     GET PROPOSAL EXP BOOK ELEMENT                
         USING RCPRBKEL,R6                                                      
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                SHOULD BE HERE                               
         SR    R0,R0                                                            
         ZIC   R1,RCPRBKLN                                                      
         SH    R1,=AL2(RCPRBKOQ)                                                
         SR    RE,RE                                                            
         LA    RE,L'RCPRBKBK                                                    
         DR    R0,RE                                                            
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,RCPRBKBK                                                      
         LA    RF,SAVBKS                                                        
         MVC   0(L'RCPRBKBK,RF),0(RE)   MOVE BOOK OR LABEL                      
         LA    RF,L'RCPRBKBK(RF)                                                
         LA    RE,L'RCPRBKBK(RE)                                                
         BCT   R1,*-14                                                          
*                                                                               
RDCNEXP2 DS    0H                                                               
         LA    R6,RCONELEM-RCONKEY(R2)                                          
         MVI   ELCODE,RCPRDPEQ     GET PROPOSAL EXP DAYPART/CPP ELEM            
         USING RCPRDPEL,R6                                                      
         BAS   RE,FIRSTEL                                                       
         BNE   RDCNSEX                                                          
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,RCPRDPLN                                                      
         SH    R1,=AL2(RCPRDPOX)                                                
         SR    RE,RE                                                            
         LA    RE,L'RCPRDPDP                                                    
         DR    R0,RE                                                            
         LTR   R1,R1                                                            
         BZ    RDCNSEX                                                          
*                                                                               
         LTR   R0,R0               OLD ELEMENT?                                 
         BNZ   RDCNEXP4            NO                                           
         OI    CCONFLG1,CCONDPMQ   OLD ELEMENTS USE HARDCODED TABLE             
         LA    RE,RCPRDPDP-(RCPRDPOQ-RCPRDPOX)                                  
         B     RDCNEXP6                                                         
*                                                                               
RDCNEXP4 LA    RE,RCPRDPDP                                                      
         TM    RCPRDPFL,RCPRDPMQ   USING DAYPART MENU?                          
         BO    RDCNEXP6            YES                                          
         OI    CCONFLG1,CCONDPMQ   NO - USES HARDCODED TABLE                    
*                                                                               
RDCNEXP6 LA    RF,SAVDPTS                                                       
         MVC   0(L'RCPRDPDP,RF),0(RE)   MOVE DAYPART & CPP                      
         LA    RF,L'RCPRDPDP(RF)                                                
         LA    RE,L'RCPRDPDP(RE)                                                
         BCT   R1,*-14                                                          
*                                                                               
RDCNSEX  DS    0H                                                               
**********************                                                          
** PENDING COMMENTS **                                                          
**********************                                                          
RDCNCM   DS    0H                                                               
         LA    R0,SAVCOMS                                                       
         LA    R1,L'SAVCOMS                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
RDCNCM0  LA    R6,RCONELEM-RCONKEY(R2)                                          
         LA    R4,SAVCOM                                                        
         MVI   ELCODE,X'11'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   RDCNCMX             NONE PRESENT                                 
         SR    R5,R5               COMMENT COUNT                                
*                                                                               
RDCNCM05 LA    R5,1(R5)                                                         
         CH    R5,=H'12'           ALL THE COMMENTS WE CAN HANDLE?              
         BH    RDCNCM10            YES                                          
*                                                                               
         ZIC   R1,1(R6)            ELEMENT LENGTH                               
         SH    R1,=H'3'            LESS OVERHEAD + 1 FOR EX                     
         CH    R1,=AL2(L'SAVCOM)   TOO LONG?                                    
         BL    *+8                 NO                                           
         LA    R1,L'SAVCOM-1                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),2(R6)       MOVE THE COMMENT DATA                        
         STC   R1,0(R4)            STORE THE EX LEN                             
         LA    R4,L'SAVCOM(R4)     NEXT SAVED COMMENT                           
         BAS   RE,NEXTEL                                                        
         BE    RDCNCM05                                                         
*                                                                               
RDCNCM10 DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(MINCOMS-TWAD)                                              
         LA    RF,L'MINCOMS                                                     
         LA    R0,SAVCOMS                                                       
         LA    R1,L'SAVCOMS                                                     
         MVCL  RE,R0                                                            
*                                                                               
RDCNCMX  DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1PFRET                                          
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GOOBR - (GO BROWSE) ROUTINE TO INVOKE REP RECORD BROWSER            *         
*  P1 = 4 CHAR REC TYPE (EG: C'AGY ' OR C'ADV ')                      *         
*  P2 = HIGH ORDER BYTE  = RECORD OPTIONS BYTE                        *         
***********************************************************************         
         DS    0D                                                               
         USING *,RB                                                             
GOOBR    DS    0H                                                               
         LR    RB,RF                                                            
         B     *+12                                                             
         DC    CL8'**GOOBR*'                                                    
*                                                                               
*  CREATE BROWSE ELEMENT FOR GLOBBER                                            
*                                                                               
         XC    BOELEM(GLBRWLNQ),BOELEM                                          
         LA    R2,BOELEM                                                        
         USING GLBRWKW,R2                                                       
         ZIC   RF,FVXLEN           LENGTH W/O '='                               
         BCTR  RF,0                FOR EX                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   GLBRWKW(0),FVIFLD+1 COPY THE KEYWORD                             
         MVC   GLBRWREC,0(R1)      COPY THE RECORD                              
         MVC   GLBRWFLG,4(R1)      COPY THE FLAGS                               
         L     RF,ACOM                                                          
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),BODMCB,=C'PUTD',BOELEM,GLBRWLNQ,GLRBRWSE                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
* CREATE GLOBBER CONTROL ELEMENT                                                
*                                                                               
         XC    BOELEM(GLVXLENQ),BOELEM                                          
         LA    R2,BOELEM                                                        
         USING GLVXFRSY,R2                                                      
         MVC   GLVXFRSY,=C'REP'                                                 
         MVC   GLVXFRPR,=C'SEL'                                                 
         MVC   GLVXTOSY,=C'REP'                                                 
         MVC   GLVXTOPR,=C'BRO'                                                 
***>>>   MVI   GLVXFLG1,GLV1GOTO+GLV1SEPS   INVOKE BASE                         
         MVI   GLVXFLG1,GLV1SEPS   INVOKE BASE                                  
         L     RF,ACOM                                                          
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),BODMCB,=C'PUTD',BOELEM,GLVXLENQ,GLVXCTL                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
         L     RD,BCSVRD                                                        
         B     EXITL                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SPREAD THE FORCAST DOLLARS                                                    
*  BOWORK2 == DAYTABLE                                                          
*  BOFULL2 == TOTDAYS                                                           
***********************************************************************         
         DS    0H                                                               
SPREDFOR NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'SPREDFOR'                                                    
*                                                                               
         L     R6,AIOREC           DELETE EXSITING BUCKETS                      
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   ELCODE,X'23'                                                     
SFOR0001 BAS   RE,FIRSTEL                                                       
         BNE   SFOR0002                                                         
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',(R2)),(R6)                        
         B     SFOR0001                                                         
*                                                                               
SFOR0002 L     R6,AIOREC                                                        
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   ELCODE,X'12'        SAR ELEMENT                                  
         BAS   RE,FIRSTEL                                                       
         BE    *+6                 VERY BAD                                     
         DC    H'0'                                                             
         USING RSARXEL,R6                                                       
*                                                                               
*   INITIALIZE WORKSPACE FOR FORECAST SPREADING....                             
*                                                                               
         XC    BOELEM,BOELEM       SET NEW ELEMENT                              
         MVC   BOELEM(2),=X'230A'                                               
*                                                                               
         GOTO1 VDATCON,BODMCB,(5,BOWORK1),(0,BOWORK1)                           
*                                  GET TODAY'S DATE EBCDIC                      
         GOTO1 VGETDAY,BODMCB,BOWORK1,BOWORK1+6                                 
*                                  GET DAY OF WEEK OF TODAY'S DATE              
         ZIC   R2,BODMCB           SAVE DAY OF WEEK RETURNED                    
         BCTR  R2,0                MAKE DAY OF WEEK ZERO/MONDAY REL             
         LNR   R2,R2               NEGATE THE VALUE                             
         GOTO1 VADDAY,BODMCB,BOWORK1,BOWORK1+6,(R2)                             
         GOTO1 VDATCON,BODMCB,(0,BOWORK1+6),(2,BOELEM+4)                        
*                                  INSERT IT INTO NEW 23 ELEMENT                
         BAS   RE,GENDAYS          GENERATE DAYTABLE                            
         BAS   RE,SPREDAYS         GENERATE DAYS WITHIN TABLE                   
SFOR0020 EQU   *                                                                
         SR    RF,RF                                                            
         LA    R2,BOWORK2          ACCUMULATE TOTAL DAYS                        
SFOR0030 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SFOR0040            YES                                          
         ZIC   RE,3(R2)            TAKE DAYS FROM TABLE                         
         AR    RF,RE               ACCUMULATE                                   
         LA    R2,4(R2)            BUMP TO NEXT ENTRY                           
         B     SFOR0030            GO BACK FOR NEXT                             
SFOR0040 EQU   *                                                                
         ST    RF,BOFULL2          SAVE IT FOR LATER                            
         MVC   BOFULL1,RSARXBGT    LOAD MARKET $$ BUDGET FIGURE                 
         L     RF,BOFULL1                                                       
         ZIC   R2,RSARXSHG         ADJUST WITH SHARE GOAL                       
         MR    RE,R2               MULTIPLY MARKET $ BY SHARE GOAL              
*                                     GIVING STATION $$                         
*                                                                               
*   NOW MULTIPLY BY 10 FOR PROPER DECIMAL ALIGNMENT                             
*                                                                               
         M     RE,=F'10'           MULTIPLY BY 10                               
         L     R2,BOFULL2          DIV STA $$ BY TOTAL DAYS                     
*                                     GIVING $$ PER DAY                         
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         AR    RF,R2               ADD TOTDAYS FOR ROUNDING                     
         DR    RE,R2               DIVIDE BY TOTDAYS                            
         SRA   RF,1                DIVIDE BY 2                                  
         ST    RF,BOFULL2          SAVE $$ PER DAY                              
         LA    R2,BOWORK2                                                       
SFOR0050 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SFOR0060            YES - FINISHED                               
         BAS   RE,GENBUCKS         NO  - GEN X'23' FORECAST BUCKET              
         LA    R2,4(R2)            BUMP TO NEXT BUCKET                          
         B     SFOR0050            GO BACK FOR NEXT                             
SFOR0060 EQU   *                                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CREATE DAYTABLE IN BOWORK2                                                    
*                                                                               
*   AIO1 IS USED TO SET UP BROADCAST MONTH ARRAY.                               
***********************************************************************         
GENDAYS  NTR1                                                                   
         L     R2,AIO1             A(DAYTABLE)                                  
         XC    0(256,R2),0(R2)     INITIALIZE TABLE                             
         USING BROADTBL,R2                                                      
GDAY0020 EQU   *                                                                
         GOTO1 VDATCON,BODMCB,(3,CCONDAT),(0,BOWORK2)                           
*                                  CONVERT START DATE TO EBCDIC                 
GDAY0040 EQU   *                                                                
         GOTO1 VGTBROAD,BODMCB,(1,BOWORK2),BOWORK2+6,VGETDAY,VADDAY             
         MVC   BRDWEEKS,BODMCB     INSERT NUMBER OF WEEKS                       
*                                  GET BROADCAST DATES FOR MONTH                
         CLI   BODMCB,X'FF'          ERROR?                                     
         BNE   *+6                 NO                                           
         DC    H'0'                SHOULDN'T HAPPEN!!                           
         GOTO1 VDATCON,BODMCB,(0,BOWORK2+6),(3,BRDSTART)                        
*                                  INSERT START DATE IN TABLE                   
         GOTO1 VDATCON,BODMCB,(0,BOWORK2+12),(3,BRDEND)                         
*                                  INSERT END   DATE IN TABLE                   
         CLC   CCONDAT+3(3),BRDEND                                              
*                                  CONTRACT FLIGHT END REACHED?                 
         BNH   GDAY0060            YES                                          
         GOTO1 VDATCON,BODMCB,(3,BRDEND),(0,BOWORK2+6)                          
*                                  CONVERT END DATE TO EBCDIC                   
         LA    RF,1                DATE INCREMENT                               
         GOTO1 VADDAY,BODMCB,BOWORK2+6,BOWORK2,(RF)                             
*                                  GET NEXT DAY, WHICH IS FIRST                 
*                                     DAY OF NEXT BDCST MONTH                   
         LA    R2,BRDLEN(R2)       BUMP TO NEXT TABLE ENTRY                     
         B     GDAY0040            GO BACK, SET NEXT MONTH                      
GDAY0060 EQU   *                                                                
         XC    BOWORK2(56),BOWORK2       CLEAR THE WORKAREA                     
         L     R2,AIO1             RESET A(BDCST MONTH TABLE)                   
         LA    R3,BOWORK2                                                       
GDAY0080 EQU   *                                                                
         CLI   BRDEND,0            ANY ENTRY?                                   
         BZ    GDAY0100            NO  - FINISHED                               
         MVC   0(2,R3),BRDEND      MOVE BDCST MON END (YM) TO TABLE             
         LA    R2,BRDLEN(R2)       BUMP TO NEXT BDCST MONTH                     
         LA    R3,4(R3)            BUMP TO NEXT DAYTABLE                        
         B     GDAY0080            GO BACK FOR NEXT                             
GDAY0100 EQU   *                                                                
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SPREAD DAYS                                                                   
***********************************************************************         
SPREDAYS NTR1                                                                   
         LA    R2,BOWORK2          A(DAYTABLE)                                  
         L     R3,AIO1             A(BDCST MONTH TABLE)                         
         USING BROADTBL,R3                                                      
         CLC   BRDSTART,CCONDAT    IS FLIGHT START FIRST DAY                    
*                                     OF FIRST BROADCAST MONTH?                 
         BE    SPDA0040            YES                                          
         GOTO1 VDATCON,BODMCB,(3,CCONDAT),(0,BOWORK1)                           
*                                  CONVERT FLIGHT START DATE                    
         CLC   CCONDAT+3(3),BRDEND                                              
*                                  IS FLIGHT END DATE EARLIER                   
*                                     THAN BROADCAST MONTH END DATE?            
         BNL   SPDA0020            NO  -                                        
         GOTO1 VDATCON,BODMCB,(3,CCONDAT+3),(0,BOWORK1+6)                       
*                                  CONVERT FLIGHT END   DATE                    
*                                                                               
*   AT THIS POINT, BOTH FLIGHT START AND END ARE WITHIN THE FIRST               
*     BROADCAST MONTH, SO THAT THE NUMBER OF DAYS CALCULATION IS                
*     DONE FROM FLIGHT START TO FLIGHT END .                                    
*                                                                               
         GOTO1 VPERVERT,BODMCB,BOWORK1,BOWORK1+6                                
         MVC   BOWORK2+2(2),BODMCB+8                                            
*                                  MOVE NUM DAYS TO 1ST TABLE NTRY              
         B     SPDA0100            EXIT ROUTINE                                 
*                                                                               
*   AT THIS POINT, FLIGHT START IS OTHER THAN BEGINNING OF BDCST                
*     MONTH, AND FLIGHT END IS EITHER AFTER THE BDCST MONTH, OR                 
*     THE LAST DAY OF THE MONTH.  NUMBER OF DAYS IS CALCULATED                  
*     FROM FLIGHT START TO BDCST MONTH END.                                     
*                                                                               
SPDA0020 EQU   *                                                                
         GOTO1 VDATCON,BODMCB,(3,BRDEND),(0,BOWORK1+6)                          
*                                  CONVERT FLIGHT END   DATE                    
         GOTO1 VPERVERT,BODMCB,BOWORK1,BOWORK1+6                                
         MVC   BOWORK2+2(2),BODMCB+8                                            
*                                  MOVE NUM DAYS TO 1ST TABLE NTRY              
         B     SPDA0080            FIRST ENTRY DONE                             
SPDA0040 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SPDA0100            YES                                          
         CLC   CCONDAT+3(3),BRDEND                                              
*                                  END OF FLIGHT REACHED?                       
         BL    SPDA0060            YES - PARTIAL MONTH TO DO                    
         ZIC   RF,BRDWEEKS         NO  - CALCULATE DAYS FROM WEEKS              
         SR    RE,RE                                                            
         LA    R1,7                                                             
         MR    RE,R1               MULT WEEKS BY 7                              
         STC   RF,3(R2)            INSERT # DAYS INTO TABLE                     
         B     SPDA0080            GO TO NEXT SLOT                              
SPDA0060 EQU   *                                                                
*                                                                               
*   AT THIS POINT, FLIGHT END IS OTHER THAN END OF BROADCAST                    
*     MONTH.  NUMBER OF DAYS IS CALCULATED FROM BROADCAST MONTH                 
*     START DATE THROUGH FLIGHT END.                                            
*                                                                               
         GOTO1 VDATCON,BODMCB,(3,BRDSTART),(0,BOWORK1)                          
*                                  CONVERT BROADCAST MONTH START                
         GOTO1 VDATCON,BODMCB,(3,CCONDAT+3),(0,BOWORK1+6)                       
*                                  CONVERT FLIGHT END   DATE                    
         GOTO1 VPERVERT,BODMCB,BOWORK1,BOWORK1+6                                
         MVC   2(2,R2),BODMCB+8                                                 
*                                  MOVE NUM DAYS TO LAST TABLE NTRY             
         B     SPDA0100            FINISHED                                     
*                                                                               
SPDA0080 EQU   *                                                                
         LA    R2,4(R2)            BUMP DAYTABLE                                
         LA    R3,BRDLEN(R3)       BUMP BDCST MONTH TABLE                       
         B     SPDA0040            GO BACK FOR NEXT                             
SPDA0100 EQU   *                                                                
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GENERATE BUCKETS                                                              
***********************************************************************         
GENBUCKS NTR1                                                                   
*                                                                               
         MVC   BOELEM+2(2),0(R2)   INSERT MONTH INTO 23 ELT                     
         SR    RE,RE                                                            
         ZIC   RF,3(R2)            NUMBER OF DAYS FOR MONTH *                   
         M     RE,BOFULL2             $$ PER DAY = $$ FOR MONTH                 
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         AH    RF,=H'10'           ADD FOR ROUNDING                             
         D     RE,=F'10'           DIVIDE FOR DECIMAL SCALING                   
         SRA   RF,1                DIVIDE BY 2                                  
         ST    RF,BOFULL1                                                       
         MVC   BOELEM+6(4),BOFULL1 INSERT INTO X'23' ELEMENT                    
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RCONELEM-RCONKEY(R6)               WHERE DOES IT GO?          
GBUC0018 CLI   0(R6),0             END OF RECORD?                               
         BE    GBUC0020            YES                                          
         CLI   0(R6),X'23'         IS THIS THE PLACE?                           
         BH    GBUC0020            YES                                          
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     GBUC0018                                                         
*                                                                               
GBUC0020 DS    0H                                                               
         LA    R0,BOELEM                                                        
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIOREC),(R0),(R6)                 
*                                                                               
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE AGENCY FIELD                                                     
***********************************************************************         
         DS    0H                                                               
VALTYPE  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'VALTYPE'                                                     
*                                                                               
         CLI   CNTPROFS,RREPQCNT                                                
         BE    VALTYP0                                                          
*                                                                               
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQCNT',CNTPROFS)                 
*                                                                               
VALTYP0  DS    0H                                                               
         XC    CCONTYPE,CCONTYPE                                                
*                                                                               
         CLI   FVILEN,0            ANY INPUT?                                   
         BNE   VALTYP2             YES - CHECK IT                               
*                                                                               
         TM    CONPROFS+CNTVTYPB,CNTVTYPA   INPUT REQUIRED?                     
         BZ    VALTYPX             NO                                           
         MVC   FVMSGNO,=AL2(571)                                                
         B     EXITL                                                            
*                                                                               
VALTYP2  DS    0H                                                               
         LA    RE,IOKEY                                                         
         USING RCTYKEY,RE                                                       
         XC    RCTYKEY,RCTYKEY                                                  
         MVI   RCTYKTYP,X'32'                                                   
         MVC   RCTYKREP,CUAALF                                                  
         MVC   RCTYKCTY,FVIFLD                                                  
         MVC   CCONTYPE,FVIFLD                                                  
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RCTYKEY),IOKEYSAV                                        
         BNE   EXITNV              TYPE NOT ON RECORD                           
*                                                                               
VALTYPX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE AGENCY FIELD                                                     
***********************************************************************         
         DS    0H                                                               
VALAGNCY NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'VALAGNCY'                                                    
*                                                                               
         CLI   FVILEN,0            ANY INPUT?                                   
         BE    INVAGYX             NO                                           
*                                                                               
         CLI   FVIFLD,C'='         BROWSE REQUEST?                              
         BNE   VALAGY1                                                          
*                                                                               
         TM    MISCFLG1,MF1GLOB    FROM GLOBBER?                                
         BZ    VALAGY0A            NO                                           
         TM    GLOBFLG,GBFBRW      BROWSE?                                      
         BNO   VALAGY0A            NO                                           
         TM    GLOBFLG,GBFBAGY     AGENCY?                                      
         BNO   VALAGY0A            NO                                           
*                                                                               
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(L'BRWRET),BRWRET                                          
         OC    FVIFLD,BCSPACES                                                  
         LA    RE,FVIFLD+L'BRWRET                                               
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         STC   RE,FVXLEN                                                        
         LA    RE,1(RE)                                                         
         STC   RE,FVILEN                                                        
         B     VALAGY1                                                          
*                                                                               
VALAGY0A GOTO1 =A(GOOBR),BODMCB,C'AGY ',0,RR=BORELO                             
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
VALAGY1  CLI   FVILEN,L'RAGYKAGY+1+L'RAGYKAOF   TOO LONG?                       
         BH    INVAGYX             YES                                          
*                                                                               
         NI    MISCFLG1,X'FF'-MF1TMPBT   SET AGENCY OFFICE NOT GIVEN            
         LA    RE,FVIFLD                                                        
VALAGY5  CLI   0(RE),C' '                                                       
         BE    VALAGY10            AGENCY ONLY                                  
         CLI   0(RE),C'-'                                                       
         BE    VALAGY8                                                          
         LA    RE,1(RE)                                                         
         B     VALAGY5                                                          
*                                                                               
VALAGY8  OI    MISCFLG1,MF1TMPBT   SET AGENCY OFFICE GIVEN                      
*                                                                               
VALAGY10 LA    R1,1(RE)            AGENCY POSITION FOR LATER                    
         LA    RF,FVIFLD                                                        
         SR    RE,RF               DATA LENGTH                                  
         BNP   INVAGYX                                                          
         STC   RE,BOBYTE1                                                       
         CLI   BOBYTE1,L'RAGYKAGY  TOO LONG?                                    
         BH    INVAGYX             YES                                          
         TM    MISCFLG1,MF1TMPBT   AGENCY OFFICE GIVEN?                         
         BZ    VALAGY16            NO                                           
*                                                                               
         LR    RE,R1               CHECK AGENCY OFFICE                          
         CLC   0(L'RAGYKAOF,RE),BCSPACES                                        
         BE    INVAGYX                                                          
*                                                                               
VALAGY12 CLI   0(RE),C' '                                                       
         BE    VALAGY14                                                         
         LA    RE,1(RE)                                                         
         B     VALAGY12                                                         
*                                                                               
VALAGY14 DS    0H                                                               
         SR    RE,R1               DATA LENGTH                                  
         STC   RE,BOBYTE2                                                       
         CLI   BOBYTE2,L'RAGYKAOF  TOO LONG?                                    
         BH    INVAGYX             YES                                          
         B     VALAGY20            NO                                           
*                                                                               
VALAGY16 CLC   BOBYTE1,FVILEN      EXTRA GARBAGE?                               
         BNE   INVAGYX             YES                                          
         B     VALAGY20                                                         
*                                                                               
VALAGY20 OC    FVIFLD(L'RAGYKAGY+1+L'RAGYKAOF),BCSPACES                         
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
         MVC   CCONKAGY,RAGYKAGY                                                
         MVC   CCONKAOF,RAGYKAOF                                                
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RAGYKEY),IOKEYSAV                                        
         BNE   INVAGYX             AGENCY NOT ON RECORD                         
*                                                                               
         TM    MISCFLG1,MF1TMPBT   WAS THE OFFICE GIVEN                         
         BO    VALAGYX             YES                                          
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOSEQ)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ SEQ                     
*                                                                               
         CLC   IOKEY+RAGYKREP-RAGYKEY(L'RAGYKREP),CUAALF                        
         BNE   VALAGYX                                                          
         CLC   IOKEY(RAGYKAOF-RAGYKEY),IOKEYSAV                                 
         BE    MOFAGYX             NEED AGENCY OFFICE                           
*                                                                               
VALAGYX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE SALESPERSON FIELD                                                
***********************************************************************         
         DS    0H                                                               
VALSALPR NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'VALSALPR'                                                    
*                                                                               
         CLI   FVILEN,0            ANY INPUT?                                   
         BE    INVSALX             NO - INVALID                                 
*                                                                               
         CLI   FVIFLD,C'='         BROWSE REQUEST?                              
         BNE   VALSAL1                                                          
*                                                                               
         TM    MISCFLG1,MF1GLOB    FROM GLOBBER?                                
         BZ    VALSAL0A            NO                                           
         TM    GLOBFLG,GBFBRW      BROWSE?                                      
         BNO   VALSAL0A            NO                                           
         TM    GLOBFLG,GBFBSAL     SALESPERSON?                                 
         BNO   VALSAL0A            NO                                           
*                                                                               
         XC    FVIFLD,FVIFLD                                                    
         MVC   FVIFLD(L'BRWRET),BRWRET                                          
         OC    FVIFLD,BCSPACES                                                  
         LA    RE,FVIFLD+L'BRWRET                                               
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         STC   RE,FVXLEN                                                        
         LA    RE,1(RE)                                                         
         STC   RE,FVILEN                                                        
         B     VALSAL1                                                          
*                                                                               
VALSAL0A GOTO1 =A(GOOBR),BODMCB,C'SAL ',(X'80',0),RR=BORELO                     
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
VALSAL1  CLI   FVILEN,L'RSALKSAL   TOO LONG?                                    
         BH    INVSALX             YES                                          
*                                                                               
         OC    FVIFLD(L'RSALKSAL),BCSPACES   UPPERCASE                          
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
         MVC   CCONSAL,FVIFLD                                                   
*                                                                               
VALSALX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE STATION FIELD                                                    
***********************************************************************         
         DS    0H                                                               
VALSTATN NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'VALSTATN'                                                    
*                                                                               
         CLI   FVILEN,0            ANY INPUT?                                   
         BE    INVSTAX             NO - INVALID                                 
         CLI   FVILEN,L'RSTAKSTA   TOO LONG?                                    
         BNH   VSTAN10             NO                                           
         CLI   FVIFLD+4,C'-'                                                    
         BNE   INVSTAX                                                          
         CLI   FVIFLD+5,C' '                                                    
         BNH   INVSTAX                                                          
         MVC   FVIFLD+4(1),FVIFLD+5                                             
         XC    FVIFLD+5(L'FVIFLD-5),FVIFLD+5                                    
         BH    INVSTAX             YES                                          
*                                                                               
VSTAN10  OC    FVIFLD(L'RSTAKSTA),BCSPACES   UPPERCASE                          
         LA    RE,IOKEY                                                         
         USING RSTAKEY,RE                                                       
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,CUAALF                                                  
         MVC   RSTAKSTA,FVIFLD                                                  
         CLI   RSTAKSTA+4,C'T'                                                  
         BNE   *+8                                                              
         MVI   RSTAKSTA+4,C' '                                                  
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         CLC   IOKEY(L'RSTAKEY),IOKEYSAV                                        
         BNE   INVSTAX             STATION NOT ON RECORD                        
*                                                                               
         MVC   CCONKSTA,FVIFLD                                                  
VALSTAX  B     EXITOK                                                           
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
DISBOOKS NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'DISBOOKS'                                                    
*                                                                               
         MVC   XFVADDR,FVADDR      SAVE THIS ADDRESS FOR DFDVAL                 
*                                                                               
         CLI   CSACT,A#ADD         ARE WE ADDING?                               
         BNE   DISBKS10                                                         
         TM    MISCFLG1,MF1KYCHG   KEY CHANGED?                                 
         BZ    DISBKS10            NO                                           
***************                                                                 
* FOR ACTION ADD WHEN THE KEY CHANGED                                           
***************                                                                 
*                                                                               
DISBKS10 DS    0H                                                               
         LA    R2,FVIFLD                                                        
         LA    R3,SAVBKS                                                        
*                                                                               
DISBKS20 CLI   0(R3),0                USER DEFINED BOOK?                        
         BNE   DISBKS30               YES                                       
*                                                                               
         OC    0(L'SAVBK,R3),0(R3)    ANY BOOK?                                 
         BZ    DISBKS60               NO MORE BOOKS                             
*                                                                               
         CLC   =C'DR',2(R3)                                                     
         BE    *+14                                                             
         CLC   =C'PP',2(R3)                                                     
         BNE   DISBKS22                                                         
*                                                                               
         MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(2),2(R3)                                                  
         B     DISBKS60                                                         
*                                                                               
DISBKS22 XC    BOWORK1,BOWORK1                                                  
         MVI   BOWORK1,28                                                       
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
DISBKS30 MVC   0(L'SAVBK,R2),0(R3)   SHOW THE USER-DEFINED LABEL                
*                                                                               
         LA    R2,L'SAVBK-1(R2)      REMOVE AS MUCH SPACES AS WE CAN            
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
         LA    R3,L'SAVBK(R3)                                                   
         LA    RE,SAVBKS+L'SAVBKS                                               
         CR    R3,RE                                                            
         BL    DISBKS20                                                         
*                                                                               
DISBKS60 BCTR  R2,0                REMOVE THE LAST COMMA                        
         MVI   0(R2),C' '                                                       
*                                                                               
DISBKSX  B     EXITOK                                                           
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
         CLI   FVILEN,0                                                         
         BE    MISSINP                                                          
*                                                                               
         MVI   BOBYTE2,0           # OF USER-DEFINED BOOKS                      
         MVI   BKCOUNT,0           # OF BOOKS                                   
*                                                                               
         L     RE,AIO4             CLEAR FOR PARSNIP                            
         LA    RF,480                                                           
         XCEFL                                                                  
*                                                                               
         GOTO1 VPARSNIP,BODMCB,(FVILEN,FVIFLD),(16,AIO4),('PSNAOKQ',0)          
         CLI   8(R1),0                                                          
         BE    VALBKS05                                                         
         L     R1,8(R1)                                                         
         LA    R1,0(R1)                                                         
         LA    R0,FVIFLD                                                        
         SR    R1,R0                                                            
         STC   R1,FVERRNDX         INDEX OF WHERE ERROR IS                      
         B     EXITNV                                                           
*                                                                               
VALBKS05 LA    R6,SAVBKS           R6 = A(1ST 5-BYTE BK OR LABEL)               
         L     R3,AIO4             R3 = A(1ST PARSNIP FIELD)                    
         USING PSND,R3                                                          
*                                                                               
VALBKS10 L     RE,PSNCOMP          CALCULATE WHERE TO POINT CURSOR              
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         STC   RE,FVERRNDX                                                      
*                                                                               
         CLI   PSNTAG,0            ANY MORE FIELDS?                             
         BE    VALBKS40            NO                                           
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
         L     RE,PSNCOMP                                                       
         CLI   PSNLEN,2                                                         
         BNE   VALBKS12                                                         
*                                                                               
         CLC   =C'DR',0(RE)                                                     
         BE    *+14                                                             
         CLC   =C'PP',0(RE)                                                     
         BNE   VALBKS12                                                         
*                                                                               
         MVC   SAVBKS+2(2),0(RE)                                                
         B     VALBKS40                                                         
*                                                                               
VALBKS12 XC    BOWORK2,BOWORK2                                                  
         XC    BODUB1,BODUB1                                                    
         ZIC   R1,PSNLEN                                                        
         STC   R1,BOWORK2+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOWORK2+8(0),0(RE)                                               
         LA    RF,BOWORK2+9(R1)                                                 
*                                                                               
         OC    PSNATTR,PSNATTR     ETHINIC BOOK?                                
         BZ    VALBKS14            NO                                           
*                                                                               
         LA    R3,PSNL(R3)                                                      
         OC    PSNATTR,PSNATTR     ANOTHER ETHINIC BOOK?                        
         BNZ   EXITNV              YES                                          
*                                                                               
         CLI   PSNLEN,1                                                         
         BNE   EXITNV                                                           
*                                                                               
         L     RE,PSNCOMP                                                       
         BCTR  RE,0                WANT THE '(' & ')'                           
         MVC   0(3,RF),0(RE)                                                    
         ZIC   RE,BOWORK2+5                                                     
         LA    RE,3(RE)                                                         
         STC   RE,BOWORK2+5                                                     
*                                                                               
VALBKS14 GOTO1 VBOOKVAL,BODMCB,(C'N',BOWORK2),(1,BOWORK1),             X        
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
         BNE   EXITNV                                                           
*                                                                               
         CLI   PSNLEN,3            MAKE SURE USER DEFINED BTWN 3-5 CHR          
         BL    EXITNV                                                           
         CLI   PSNLEN,L'SAVBK                                                   
         BH    EXITNV                                                           
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
         MVC   0(0,R6),0(RE)                                                    
         OC    0(L'SAVBK,R6),BCSPACES                                           
         B     VALBKS30                                                         
*                                                                               
VALBKS20 DS    0H                                                               
         CLI   BODUB1,C' '                                                      
         BNH   *+10                                                             
         MVC   1(1,R6),BODUB1                                                   
         MVC   2(3,R6),BOWORK1     SAVE THE BOOK DEFINITION                     
*                                                                               
VALBKS30 LA    R3,PSNL(R3)         BUMP TO THE NEXT FIELD                       
         LA    R6,L'SAVBK(R6)                                                   
         B     VALBKS10                                                         
         DROP  R3                                                               
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
* DISPLAY THE LENGTHS FIELD                                                     
***********************************************************************         
         DS    0H                                                               
DISLENS  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*DISLENS'                                                    
*                                                                               
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
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE LENGTHS FIELD                                                    
***********************************************************************         
         DS    0H                                                               
VALLENS  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*VALLENS'                                                    
*                                                                               
         CLI   FVILEN,0                                                         
         BE    MISSINP                                                          
*                                                                               
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
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE DAYPART FIELD                                                     
***********************************************************************         
         DS    0H                                                               
DISDAYPT NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'DISDAYPT'                                                    
*                                                                               
         ZIC   R1,SVDPTLIN         WE'RE ON A NEW (OR 1ST) DAYPART LINE         
         LA    R1,1(R1)                                                         
         STC   R1,SVDPTLIN                                                      
*                                                                               
         BCTR  R1,0                ENTRIES ARE 3-BYTES LONG IN CSARDPT          
         MH    R1,=Y(L'SAVDPT)                                                  
         LA    R1,SAVDPTS(R1)                                                   
*                                                                               
         OC    0(L'SAVDPT,R1),0(R1)      ANY DAYPART?                           
         BZ    DISDPTX                                                          
         MVC   FVIFLD(1),0(R1)     YES, SHOW THE DAYPART                        
*                                                                               
DISDPTX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE DAYPART FIELD                                                    
***********************************************************************         
         DS    0H                                                               
VALDAYPT NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'VALDAYPT'                                                    
*                                                                               
         CLI   SVDPTLIN,0          SAVE ADDRESS OF FIRST LINE                   
         BNZ   *+10                                                             
         MVC   AFVADDR,FVADDR                                                   
*                                                                               
         LA    RE,SAVDPTS                                                       
VALDPT5  CLI   0(RE),0             ANY DAYPART?                                 
         BE    *+14                NO - SKIP                                    
         CLC   FVIFLD(1),0(RE)     CHECK FOR REPEAT DPT?                        
         BE    EXITNV              YES - INVALID                                
         LA    RE,L'SAVDPT(RE)                                                  
         LA    RF,SAVDPTS+L'SAVDPTS                                             
         CR    RE,RF                                                            
         BL    VALDPT5                                                          
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
*                                                                               
         XC    0(L'SAVDPT,RE),0(RE)                                             
         ZIC   R1,SVDPTLIN                                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RECDPT)                                                  
         LA    R1,RECDPTS(R1)                                                   
         XC    0(L'RECDPT,R1),0(R1)                                             
*                                                                               
         CLI   SVDPTLIN,NUMDPTS        ARE WE ON THE LAST DPT LINE?             
         BNE   VALDPTX                 NO - OKAY                                
*                                                                               
         CLI   SAVSARF2,0              SPECIAL BUDGET?                          
         BE    *+12                                                             
         TM    SAVSARF2,X'04'          'GEN AVAIL'?                             
         BZ    VALDPTX                 NO                                       
*                                                                               
         OC    SAVDPTS,SAVDPTS         YES, NEED AT LEAST ONE DPT               
         BNZ   VALDPTX                 MISSING INPUT                            
         MVC   FVADDR,AFVADDR                                                   
         B     EXITNO                  THERE WAS ONE                            
*                                                                               
VALDPT10 TM    CCONFLG1,CCONDPMQ   USES HARDCODED TABLE?                        
         BO    VALDPT60            YES                                          
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
         DROP  RE                                                               
*                                                                               
         ICM   R1,15,=AL4(XIO4+XOREPDIR+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SCREW UP ON THE READ HIGH                    
*                                                                               
         L     RE,BOFULL2          RESTORE ENTRY IN SAVDPTS                     
         CLC   IOKEY(RRDPKDPT-RRDPKEY),IOKEYSAV                                 
         BNE   VALDPT60            NO DAYPART RECORDS                           
*                                                                               
****************************************                                        
** READ DAYPART RECORD FOR VALIDATION **                                        
****************************************                                        
         OI    MISCFLG1,MF1DPTMU   USE MENU                                     
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
*                                                                               
         L     RE,BOFULL2          RESTORE ENTRY IN SAVDPTS                     
         B     VALDPT90                                                         
*                                                                               
*************************************************                               
** READ HARDCODED DAYPART TABLE FOR VALIDATION **                               
*************************************************                               
VALDPT60 NI    MISCFLG1,FF-MF1DPTMU   DON'T USE MENU                            
         LA    R2,DPTTABLE            VALIDATE THE DAYPART CODE                 
         ZIC   R3,FVXLEN              R3 = LENGTH OF INPUT -1                   
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
VALDPT90 MVC   0(1,RE),FVIFLD      COPY THE 1-BYTE DAYPART CODE                 
         ZIC   R1,SVDPTLIN         WE'RE ON A NEW (OR 1ST) DAYPART LINE         
         BCTR  R1,0                RE = A(ENTRY FOR THIS DAYPART)               
         MH    R1,=Y(L'RECDPT)                                                  
         LA    R1,RECDPTS(R1)                                                   
         CLC   0(1,R1),0(RE)                                                    
         BNE   *+10                                                             
         XC    0(L'RECDPT,R1),0(R1)                                             
*                                                                               
VALDPTX  DS    0H                                                               
         B     EXITOK                                                           
*                                                                               
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
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MODIFY THE SCREEN MANUALLY                                                    
***********************************************************************         
         DS    0H                                                               
MODSCRN  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'MODSCRN*'                                                    
*                                                                               
         CLI   CSACT,A#LST                                                      
         BE    MODSCRX                                                          
         TM    BCINDS1,BCINACT     NEW ACTION???                                
         BO    MODSCR5             YES                                          
         CLI   CSACT,A#ADD                                                      
         BNE   MODSCR5                                                          
*                                                                               
         B     MODSCRX                                                          
*                                                                               
MODSCR5  DS    0H                                                               
         L     R0,ATWA             CLEAR COMMENTS, BUT HERE?                    
         AH    R0,=Y(MINCOMS-TWAD)                                              
         LA    R1,L'MINCOMS                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
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
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN OBJECT TYPES                                                   
***********************************************************************         
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECRD)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFKEY)                                  
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
KNOWTAB  DS    0XL(KNOWLQ)                                                      
* KEY PORTION                                                                   
         DC    AL2(00001),AL4(CONDTA)    CONTRACT                               
*                                                                               
* ADD SUB KEY                                                                   
         DC    AL2(00036),AL4(AGYDTA)    AGENCY                                 
         DC    AL2(00032),AL4(AGYDTA)    AGENCY FILTER                          
         DC    AL2(00003),AL4(ADVDTA)    ADVERTISER                             
         DC    AL2(00004),AL4(PRDDTA)    PRODUCT                                
         DC    AL2(00005),AL4(SALDTA)    SALESPERSON                            
         DC    AL2(00006),AL4(BUYDTA)    BUYER                                  
         DC    AL2(00007),AL4(STADTA)    STATION                                
         DC    AL2(00008),AL4(FLTDTA)    FLIGHT DATES                           
         DC    AL2(00038),AL4(FLTDTA)    FLIGHT DATES FILTER                    
         DC    AL2(00028),AL4(DVSDTA)    DEVELOPMENT SALESPERSON                
         DC    AL2(00029),AL4(DVTDTA)    DEVELOPMENT CONTRACT TYPE              
         DC    AL2(00010),AL4(TYPDTA)    CONTRACT TYPE                          
*                                                                               
* NON ADD SUPPLEMENTAL DATA FIELDS                                              
         DC    AL2(00042),AL4(AGYDTA)    AGENCY                                 
         DC    AL2(00043),AL4(ADVDTA)    ADVERTISER                             
         DC    AL2(00044),AL4(PRDDTA)    PRODUCT                                
         DC    AL2(00045),AL4(SALDTA)    SALESPERSON                            
         DC    AL2(00046),AL4(DVSDTA)    DEVELOPMENT SALESPERSON                
         DC    AL2(00047),AL4(DVTDTA)    DEVELOPMENT CONTRACT TYPE              
         DC    AL2(00048),AL4(STADTA)    STATION                                
         DC    AL2(00033),AL4(TYPDTA)    CONTRACT TYPE                          
*                                                                               
*                                                                               
* RECORD (INPUT) PORTION  ADD/CHA/DIS                                           
         DC    AL2(00090),AL4(BKSDTA)    BOOKS                                  
         DC    AL2(00011),AL4(DMODTA)    DEMOS                                  
         DC    AL2(00091),AL4(LNSDTA)    LENGTHS                                
         DC    AL2(00013),AL4(MKTBDTA)   MARKET BUDGET                          
         DC    AL2(00014),AL4(SHGLDTA)   SHARE GOAL                             
         DC    AL2(00015),AL4(STABDTA)   STATION BUDGET                         
         DC    AL2(00017),AL4(DPTDTA)    DAYPART                                
         DC    AL2(00018),AL4(CPPDTA)    CPP                                    
*                                                                               
* RECORD PORTION  COMMENTS                                                      
         DC    AL2(00019),AL4(COMDTA)    COMMENTS                               
*                                                                               
* DDS ONLY PORTION                                                              
         DC    AL2(00099),AL4(DSKDTA)    DISK ADDRESS                           
*                                                                               
         DC    AL2(EOT)                                                         
***********************************************************************         
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                                       
***********************************************************************         
         SPACE 1                                                                
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
XFVADDR  DS    A                                                                
AFVADDR  DS    A                   FIELD ADDRESS 1 - FOR ERRORS                 
AFVADDR2 DS    A                   FIELD ADDRESS 2                              
AFVADDR3 DS    A                   FIELD ADDRESS 3                              
MYDSKA   DS    A                   MY DISK ADRESS, THANX ALAN                   
AHDRCON  DS    XL4                 AUTOHEADER CONTRACT #                        
AHDRERR  DS    XL2                 AUTOHEADER ERROR #                           
BRWRET   DS    CL10                BROWSE RETURN AREA                           
ESCLEN   DS    XL1                                                              
ESCCHAR  DS    XL2                                                              
ELCODE   DS    XL1                                                              
BKCOUNT  DS    XL1                                                              
CNTPROFS DS    0CL10                CONTRACT PROFILES                           
         DS    CL1                 PROGRAM #                                    
         DS    CL1                 SPARE                                        
CONPROFS DS    CL8                 PROFILE BITS                                 
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1KYCHG EQU   X'80'               - KEY FIELD CHANGED                          
MF1NOCON EQU   X'40'               - CONTRACT # OMMITED                         
MF1PFRET EQU   X'20'               - RETURNING FORM PFKEY                       
MF1GLOB  EQU   X'10'               - GLOBBER INPUT                              
MF1NEWKY EQU   X'08'               - WE WANT NEW KEY MESSAGE ON ADD             
MF1ADDRD EQU   X'04'               - THIS IS A REDISPLAY ON ADD                 
MF1DPTMU EQU   X'02'               - USE DAYPART MENU                           
MF1TMPBT EQU   X'01'               - TEMPORARY BIT (USED BY ANYONE)             
*                                                                               
MISCFLG2 DS    XL1                 MISCELLANEOUS FLAGS                          
MF2NOBUD EQU   X'80'               - NO BUDGET DOLLARS                          
MF2FLTCH EQU   X'40'               - FLIGHT DATE CHANGED                        
MF2TMPBT EQU   X'01'               - TEMPORARY BIT (USED BY ANYONE)             
*                                                                               
GLOBFLG  DS    XL1                 FLAGS TO FOR GLOBBER RETURNS                 
GBFCON   EQU   X'80'               - FROM AUTOHEADER ADD                        
GBFBRW   EQU   X'40'               - FROM BROWSE                                
GBFBBAD  EQU   X'20'               - FROM BROWSE - BAD RETURN CALL              
GBFBNRF  EQU   X'10'               - FROM BROWSE - NO RECORDS FOND              
GBFBCAN  EQU   X'08'               - FROM BROWSE - CANCEL'D                     
GBFBAGY  EQU   X'04'               - FROM BROWSE AGENCY                         
GBFBADV  EQU   X'02'               - FROM BROWSE ADVERTISER                     
GBFBSAL  EQU   X'01'               - FROM BROWSE SALESPERSON                    
GBFCADD  EQU   X'01'               - FROM CONTRACT/ADD                          
*                                                                               
BDGTFLGS DS    XL1                 FLAGS FOR DOING BUDGET CALCULATION           
BDGTSBC  EQU   X'08'               - STATION BUDGET CHANGED                     
BDGTMBC  EQU   X'20'               - MARKET BUDGET CHANGED                      
BDGTMBE  EQU   X'02'               - MARKET BUDGET ENTERED                      
BDGTSGC  EQU   X'10'               - SHARE GOAL CHANGED                         
BDGTSGE  EQU   X'01'               - SHARE GOAL ENTERED                         
GRPFLG1  DS    XL1                 FLAGS FOR DOING GRP CALCULATIONS             
GF1TGRP  EQU   X'80'               - TOTAL GRP ENTERED                          
GF1CPP   EQU   X'40'               - CPP ENTERED                                
GF1GRP   EQU   X'20'               - GRP ENTERED                                
GF1PDPT  EQU   X'10'               - %DPT ENTERED                               
GF1BUD   EQU   X'08'               - BUDGET ENTERED                             
GF1TGNZ  EQU   X'04'               - TOTAL GRP NON ZERO                         
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE AREA                          
*                                                                               
SAVBKS   DS    0XL(NUMBKS*5)       SAVED 3-BYTE OR 5 BYTE LABEL                 
SAVBK    DS    (NUMBKS)XL(5)        - NULL: EMPTY OR LABEL                      
*                                                                               
SAVDMOS  DS    0CL(NUMDEMS*3)      SAVED 3-BYTE DEMO                            
SAVDMO   DS    (NUMDEMS)CL3                                                     
ENDDMO   DS    CL3                 EXTRA FOR END OF TABLE                       
*                                                                               
SAVSLNS  DS    0CL(6*1)            SAVED 1-BYTE SPOT LENGTHS                    
SAVSLN   DS    6XL1                                                             
*                                                                               
SVDPTLIN DS    XL1                 CURRENT NUMBER OF DPT LINE                   
*                                                                               
SAVDPTS  DS    0CL(NUMDPTS*5)      SAVED 1-BYTE DPT/4-BYTE CPP                  
SAVDPT   DS    (NUMDPTS)XL5                                                     
*                                                                               
RECDPTS  DS    0CL(NUMDPTS*5)      DPT/CPP INFO FROM RECORD                     
RECDPT   DS    (NUMDPTS)XL5                                                     
*                                                                               
SAVGRPS  DS    0XL(NUMDPTS*(1+4+4))  SAVED 1 BYTE %DPT, 4 BYTES GRP             
SAVGRP   DS    (NUMDPTS)XL(1+4+4)     4 BYTES BUDGET                            
*                                                                               
SAVTGRP  DS    XL4                 SAVED TOTAL GRPS                             
*                                                                               
SVCOMLIN DS    XL1                 CURRENT NUMBER OF COMMENT LINE               
*                                                                               
SAVCOMS  DS    0CL(12*61)          SAVED 60 BYTE COMMENTS + LEN                 
SAVCOM   DS    12CL61                                                           
*                                                                               
SAVMKTB  DS    XL4                 SAVED MARKET BUDGET                          
SAVSHGL  DS    X                   SAVED SHARE GOAL                             
SAVSTAB  DS    XL4                 SAVED STATION BUDGET                         
SAVSARF1 DS    X                   SAVED CONTRACT SAR FLAGS                     
SAVSARF2 DS    X                   SAVED CONTRACT SAR FLAGS 2                   
*                                                                               
SAVBUYR  DS    CL(L'ECONBUYR)      SAVED CONTRACT BUYER                         
SAVFLGHT DS    CL(L'CCONDAT)       SAVED FLIGHT DATES                           
CHARFLT  DS    CL12                SAVED FLIGHT DATES(EBCDIC)                   
         DS    D                                                                
BLOCK    DS    480C                                                             
GLOBKEY  DS    CL48                                                             
GLOBDA   DS    CL4                                                              
*                                                                               
         EJECT                                                                  
PFKWRKUP EQU   PFK04               PFKEY FOR WORK/UPDATE                        
PFKINTR  EQU   PFK03               PFKEY FOR PENDING ACTIONS                    
PFPROPSL EQU   PFK02               PFKEY FOR PROPOSAL                           
PFKYUP   EQU   PFK07               PFKEY FOR SCROLL UP                          
PFKYDOWN EQU   PFK08               PFKEY FOR SCROLL DOWN                        
PFKYLEFT EQU   PFK09               PFKEY FOR SCROLL LEFT                        
PFKYRGHT EQU   PFK10               PFKEY FOR SCROLL RIGHT                       
PFKYNEXT EQU   PFK11               PFKEY FOR NEXT(FROM LIST)                    
PFKYQUIT EQU   PFK12               PFKEY FOR QUIT                               
PFKYRIS  EQU   PFK14               PFKEY FOR RIS                                
         EJECT                                                                  
* REPROWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REPROWORK                                                      
         PRINT ON                                                               
TWAD     DSECT                                                                  
***********************************                                             
* SAVED BETWEEN NTRSES                                                          
***********************************                                             
         ORG   SVMORE                                                           
SVCONNTR DS    CL8                 SAVED CONTRACT FOR NTR                       
*                                                                               
         ORG   TWUSER                                                           
***********************************                                             
* SAVE AREA EXCEPT BETWEEN NTRSES - USED DURING ADD                             
***********************************                                             
MINCOMS  DS    0CL(12*61)          SAVED 60 BYTE COMMENTS + LEN                 
MINCOM   DS    12CL61                                                           
*                                                                               
         EJECT                                                                  
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    XL3                 BINARY MONTH START DATE                      
BRDEND   DS    XL3                 BINARY MONTH END   DATE                      
BRDWEEKS DS    XL1                 NUM WEEKS IN PERIOD                          
BRDLEN   EQU   *-BRDSTART          LENGTH OF ENTRY                              
         EJECT                                                                  
* RECNTAUTOD                                                                    
         PRINT OFF                                                              
       ++INCLUDE RECNTAUTOD                                                     
         PRINT ON                                                               
* REGLBRW                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGLBRW                                                        
         PRINT ON                                                               
* RECNTPROF                                                                     
         PRINT OFF                                                              
       ++INCLUDE RECNTPROF                                                      
         PRINT ON                                                               
* DDGLVXCTLD + DDGLOBEQUS                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
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
**PAN#1  DC    CL21'014REPRO22B  05/01/02'                                      
         END                                                                    
