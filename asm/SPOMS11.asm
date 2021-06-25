*          DATA SET SPOMS11    AT LEVEL 004 AS OF 12/20/20                      
*PHASE T23411B                                                                  
T23411   TITLE 'SPOMS11 - ORDER PSTATUS'                                        
T23411   CSECT                                                                  
         PRINT NOGEN                                                            
BGN      NMOD1 0,*T23411*,R7,RR=R3                                              
*                                                                               
         L     RC,0(R1)                   STANDARD CODING                       
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA             BASE SCREEN + OUR SCREEN              
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE                R5=A(LOCAL SAVED STORAGE)             
         USING LSSD,R5                                                          
*                                                                               
         LA    R2,CONACTH                 CURSOR TO ACTION FIELD                
         CLI   CALLSP,0                   DID WE PFKEY INTO PSTATUS?            
         BE    INVLFLD                    NO - ERROR                            
*                                                                               
         ST    R3,RELO                    RELO                                  
         ST    RC,BASERC                                                        
         BAS   RE,GETPF                   GET PFKEYS                            
*                                                                               
         L     RF,ACOMFACS                                                      
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF)                                   
*                                                                               
         LA    R3,RELOTAB                                                       
INIT3    CLI   0(R3),X'FF'                                                      
         BE    INIT5                                                            
         ICM   RF,15,0(R3)                                                      
         A     RF,RELO                                                          
         ICM   RE,15,4(R3)                                                      
         LA    RE,LSSD(RE)                                                      
         STCM  RF,15,0(RE)                                                      
         LA    R3,L'RELOTAB(R3)                                                 
         B     INIT3                                                            
         LA    R2,OPSMED                                                        
*                                                                               
INIT5    CLI   MODE,DISPREC               DISPLAY RECORD?                       
         BE    DR                         YES                                   
         CLI   MODE,VALREC                VALIDATE RECORD?                      
         BE    VR                         YES                                   
         CLI   MODE,XRECPUT               VALIDATE RECORD?                      
         BE    XRPUT                      YES                                   
*                                                                               
XIT      XIT1                                                                   
***********************************************************************         
* DISPLAY THE RECORD                                                  *         
***********************************************************************         
DR       OI    OPSMEDH+1,X'20'            PROTECT                               
         OI    OPSORDRH+1,X'20'           PROTECT                               
         OI    OPSCLTH+1,X'20'            PROTECT                               
         OI    OPSPRDH+1,X'20'            PROTECT                               
         OI    OPSESTH+1,X'20'            PROTECT                               
         OI    OPSFLTH+1,X'20'            PROTECT                               
         OI    OPSMKTH+1,X'20'            PROTECT                               
         OI    OPSSTAH+1,X'20'            PROTECT                               
*                                                                               
         LA    R2,OPSTDOLH                TOTAL DOLLAR                          
         MVI   5(R2),L'OPSTDOL            TOTAL DOLLAR LENGTH                   
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPID                             
         CLI   8(R1),0                    HAVE TOTAL DOLLAR?                    
         JNE   NEEDFLDS                   NO - ERROR                            
         OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         LA    R2,OPSTSPTH                TOTAL SPOTS                           
         MVI   5(R2),L'OPSTSPT            TOTAL SPOTS LENGTH                    
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVDQU                              
         CLI   8(R1),0                    HAVE TOTAL SPOTS?                     
         JNE   NEEDFLDS                   NO - ERROR                            
         OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         LA    R2,OPSCSTAH                STATUS                                
         MVI   5(R2),L'OPSCSTA            STATUS LENGTH                         
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVPGM                              
         CLI   8(R1),0                    HAVE STATUS?                          
         JNE   NEEDFLDS                   NO - ERROR                            
         OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'03'               SUPPLEMENTARY ID ELEMENT              
         BAS   RE,GETEL                   HAVE AN X'03' ELEMENT?                
         BNE   DR10                       NO                                    
*                                                                               
         USING  DOSPELD,R6                SUPPLEMENTARY ID ELEM DSECT           
         EDIT  (B2,DOSPVER#),(3,OPSCSND),FILL=0                                 
         OI    OPSCSNDH+6,X'80'           TRANSMIT                              
         EDIT  (B1,DOSPREVN),(3,OPSCREV),FILL=0                                 
         OI    OPSCREVH+6,X'80'           TRANSMIT                              
*                                                                               
         CLI   DOSPDEST,C'R'              DESTINATION REP?                      
         BNE   *+10                       NO                                    
         MVC   OPSCDST(3),=C'REP'         YES - MOVE REP TO SCREEN              
         CLI   DOSPDEST,C'S'              DESTINATION STA?                      
         BNE   *+10                       NO                                    
         MVC   OPSCDST(3),=C'STA'         YES - MOVE STA TO SCREEN              
         OI    OPSCDSTH+6,X'80'           TRANSMIT                              
*                                                                               
         CLI   DOSPMTHD,C'I'              EDI?                                  
         BNE   *+10                       NO                                    
         MVC   OPSCMTH,=C'EDI'            YES                                   
         CLI   DOSPMTHD,C'F'              FAX?                                  
         BNE   *+10                       NO                                    
         MVC   OPSCMTH,=C'FAX'            YES                                   
         CLI   DOSPMTHD,C'E'              EMAIL?                                
         BNE   *+10                       NO                                    
         MVC   OPSCMTH,=C'EML'            YES                                   
         OI    OPSCMTHH+6,X'80'           TRANSMIT                              
         DROP  R6                         DROP R6                               
*                                                                               
DR10     L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'12'               NEW STATUS LAYOUT ELEMENT             
         BAS   RE,GETEL                   HAVE A X'12' ELEMENT?                 
         BNE   DR25                       NO                                    
*                                                                               
         USING DOSTELD,R6                 NEW STATUS LAYOUT ELEM DSECT          
         CLI   DOSTSTAT,DDLVRD            DELIVERED?                            
         BE    DR15                       YES                                   
         CLI   DOSTSTAT,DFXDLVD           FAX DELIVERED?                        
         BE    DR15                       YES                                   
         CLI   DOSTSTAT,DEMDLVD           E-MAIL DELIVERED?                     
         BNE   DR20                       NO                                    
*                                                                               
DR15     GOTO1 HEXOUT,DMCB,DOSTTIME,WORK,L'DOSTTIME                             
         MVC   OPSCDLV(2),WORK            HOUR                                  
         MVI   OPSCDLV+2,C'.'             "."                                   
         MVC   OPSCDLV+3(2),WORK+2        MINUTE                                
         OI    OPSCDLVH+6,X'80'           TRANSMIT                              
*                                                                               
         LA    R2,OPSCDATH                IN CASE OF ERROR                      
         BAS   RE,NEXTEL                  HAVE ANOTHER X'12' ELEMENT?           
         BNE   INVSTATS                   NO - ERROR                            
         CLI   DOSTSTAT,DSENT             SENT?                                 
         BNE   INVSTATS                   NO - ERROR                            
*                                                                               
DR20     GOTO1 DATCON,DMCB,(8,DOSTDATE),(11,OPSCDAT)                            
         OI    OPSCDATH+6,X'80'           TRANSMIT                              
*                                                                               
         GOTO1 HEXOUT,DMCB,DOSTTIME,WORK,L'DOSTTIME                             
         MVC   OPSCTIM(2),WORK            HOUR                                  
         MVI   OPSCTIM+2,C'.'             "."                                   
         MVC   OPSCTIM+3(2),WORK+2        MINUTE                                
         OI    OPSCTIMH+6,X'80'           TRANSMIT                              
         DROP  R6                         DROP R6                               
*                                                                               
DR25     L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'50'               WHERE'D IT GO ELEMENT                 
         BAS   RE,GETEL                   HAVE A X'50' ELEMENT?                 
         BNE   DRX                        NO                                    
*                                                                               
         USING DOWIGELD,R6                NEW STATUS LAYOUT ELEM DSECT          
         MVC   OPSCRTE,DOWIGRPP           ROUTE                                 
         OI    OPSCRTEH+6,X'80'           TRANSMIT                              
         DROP  R6                         DROP R6                               
*                                                                               
DRX      LA    R2,OPSPDATH                DATE                                  
         MVC   8(5,R2),=C'TODAY'          TODAY                                 
         MVI   5(R2),5                    INPUT LENGTH OF 5                     
         LA    R2,OPSPTIMH                TIME                                  
         MVC   8(3,R2),=C'NOW'            NOW                                   
         MVI   5(R2),3                    INPUT LENGTH OF 3                     
         B     XIT                        EXIT                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
VR       LA    R2,OPSPSTAH                ORDER STATUS TO PATCH TO              
         TM    OPSPSTAH+1,X'20'           HAS ORDER BEEN PATCHED?               
         BNZ   ERRPF12                    YES - MUST PF12                       
         OC    OPSPSTA,SPACES                                                   
*                                                                               
         CLI   5(R2),0                    HAVE ANY INPUT?                       
         BE    NEEDFLDS                   NO - ERROR                            
*                                                                               
         L     R3,AORDSTAB                R3=ORDER STATUS TABLE                 
         USING ORDD,R3                    ORDER STATUS DSECT                    
*                                                                               
VR010    CLI   ORDSTAT,X'FF'              END OF TABLE?                         
         BE    INVLFLD                    YES - INVALID STATUS                  
         CLC   ORDDFLT,8(R2)              MATCH ON STATUS?                      
         BE    VR020                      YES                                   
         LA    R3,ORDDLNQ(R3)             NEXT TABLE ENTRY                      
         B     VR010                      CHECK NEXT TABLE ENTRY                
VR020    OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,DOIDELQ             X'01' ELEMENT                         
         BAS   RE,GETEL                   GET X'01' ELEMENT                     
         BE    *+6                        HAVE ONE?                             
         DC    H'0'                       NO - DEATH                            
*                                                                               
         TM    ORDFLG1,ORDMEDR            STATUS ONLY MEDIA R?                  
         JZ    VR025                                                            
         CLI   OPSMED,C'R'                ARE WE MEDIA R?                       
         JNE   ERRINVMD                    NO, SEND ERROR                       
*                                                                               
VR025    L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,DOSTELQ             X'12' ELEMENT                         
         BAS   RE,GETEL                   HAVE A X'12' ELEM                     
         BNE   ERRNTSNT                   NEVER SENT CANT PATCH                 
         ST    R6,ADOSTEL                                                       
*                                                                               
         TM    ORDFLG1,ORDUNDO+ORDDELET   UNDO / UNSENT?                        
         JZ    VR030                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,DOSTELQ                                                   
         BRAS  RE,GETEL                                                         
*                                                                               
VR027    GOTOR RECUP,DMCB,(C'S',AIO),(R6)    REMOVE IT                          
         TM    ORDFLG1,ORDDELET           UNSENT                                
         JZ    VRX                        NO, JUST REMOVE FIRST                 
*                                                                               
         L     R6,AIO                     R6 = ORDER RECORD                     
         USING DAREORDD,R6                                                      
         MVC   BAGYMD,DOKAGMD             SAVE AGENCY/MEDIA                     
         MVC   BINORDER,DOKORDER          SAVE ORDER NUMBER                     
         MVC   DATADISP,=H'24'            SET TO SPOTFILE                       
                                                                                
         L     R6,AIO                     R6 = ORDER RECORD                     
         MVI   ELCODE,DOIDELQ             X'01' ELEMENT                         
         JAS   RE,GETEL                   HAVE AN X'01' ELEMENT?                
         JNE   VR028                      DUMP IF NOT FOUND                     
         USING DOIDELD,R6                 ID ELEMENT DSECT                      
         MVC   FULL(3),DOIDBYR            BUYER                                 
         MVC   SVSTATN,DOISTA             STATION                               
         DROP  R6                                                               
*-----------------------------------*                                           
* MAKE GOOD RECORD PROCESSING       *                                           
*-----------------------------------*                                           
VR028    CLI   SVSTATN,X'E8'              CABLE?                                
         BL    VR028B                                                           
*                                                                               
         MVC   DATADISP,=H'42'            SET THE DATA DISP FOR XSPOT           
*                                                                               
         BRAS  RE,RDXSPT                  READ MAKE GOOD - XSPOT                
         JNE   VR029                                                            
VR028A   BRAS  RE,CHKMG00                 CHECK MAKE GOOD RECORD                
         JNE   ERRNTAMG                   ACTIVE MAKEGOOD RECORD                
         BRAS  R4,RDXSPT10                READ NEXT SEQ XSPOT RECORD            
         JE    VR028A                                                           
         J     VR029                                                            
*                                                                               
VR028B   BRAS  RE,RDSPOT                  READ SPOT FILE                        
         JNE   VR029                                                            
VR028C   BRAS  RE,CHKMG00                 CHECK MAKE GOOD RECORD                
         JNE   ERRNTAMG                   ACTIVE MAKEGOOD RECORD                
         BRAS  R4,RDSPOT10                READ NEXT SEQ SPOT RECORD             
         JE    VR028C                                                           
*-----------------------------------*                                           
* DARE ORDER RECORD PROCESSING      *                                           
*-----------------------------------*                                           
VR029    L     R6,AIO                  DARE ORDER RECORD                        
         MVC   DATADISP,=H'24'         USUALLY SPOTFILE                         
         MVI   ELCODE,DOIDELQ          PRIMARY ID ELEMENT                       
         JAS   RE,GETEL                HAVE ONE?                                
         JNE   *+10                    NO                                       
         USING DOIDELD,R6                                                       
         XC    DOIDCON,DOIDCON         CLEAR CONTRACT FIELD                     
*                                                                               
         L     R6,AIO                  DARE ORDER RECORD                        
         MVI   ELCODE,DOSPELQ          SUPP ID ELEM                             
         JAS   RE,GETEL                                                         
         JNE   VR029A                                                           
         USING DOSPELD,R6                                                       
         NI    DOSPFLG1,X'FF'-DOSPCFCM CLEAR DOSPCFCM FLAG                      
         XC    DOSPSPTS,DOSPSPTS       CLEAR TOTAL SPOTS                        
         ZAP   DOSPTOTL,=P'0'          CLEAR TOTAL DOLLARS                      
         XC    DOSPREVN,DOSPREVN       CLEAR REVISION NUMBER                    
         CLI   DOSPLEN,DOSPLNQ         SEE IF WE HAVE OTHER DATA                
         JNH   VR029A                                                           
         XC    DOSPDEST,DOSPDEST       CLEAR NEW DEST OF ORDER                  
         XC    DOSPMTHD,DOSPMTHD       CLEAR NEW METHOD OF ORDER                
         XC    DOSPIDST,DOSPIDST       CLEAR DEST OF ORDER ON INI SEND          
         XC    DOSPIMTH,DOSPIMTH       CLEAR METHOD USED FOR DEST               
         CLI   DOSPLEN,DOSPTLNQ        SEE IF WE HAVE OTHER DATA                
         JNH   VR029A                                                           
         XC    DOSPFSTA,DOSPFSTA       CLEAR STATION OF ORDER                   
         XC    DOSPFAID,DOSPFAID       CLEAR FIRST AIR DATE                     
         XC    DOSPVER#,DOSPVER#       CLEAR ORDER VERSION NUMBER               
         XC    DOSPFLG2,DOSPFLG2       CLEAR 2ND SET OF FLAGS                   
*                                                                               
VR029A   L     R6,AIO                  DARE ORDER RECORD                        
         XC    DMCB(24),DMCB           CLEAR DMCB                               
         USING DAREORDD,R6                                                      
                                                                                
         CLI   DOKCMT,0                                                         
         JNE   *+12                                                             
         LA    R4,DODTAB2              NOT A COMMENT - DELETE FEW ELEM          
         J     *+8                                                              
         LA    R4,DODTAB               SUPPLEMENT RECORD - DEL ALL ELEM         
VR029B   CLI   0(R4),0                                                          
         JE    VR029D1                                                          
         MVC   ELCODE,0(R4)            ELEM CODE TO BE DELETED                  
VR029C   L     R6,AIO                                                           
         JAS   RE,GETEL                FIND THE ELEM CODE                       
         JNE   VR029D                                                           
         GOTOR RECUP,DMCB,(C'S',AIO),(R6),0                                     
         J     VR029C                                                           
VR029D   LA    R4,DODTABL(R4)          BUMP TO NEXT ELEM CODE IN TABLE          
         J     VR029B                                                           
*------------------------------------------------------------------*            
* DARE  DARE ORDER SUPPLEMENT RECORDS (DOKCMT != 0) PROCESSING                  
*------------------------------------------------------------------*            
VR029D1  BRAS  RE,DOSREC00                                                      
*-----------------------------------*                                           
* ORDER HISTORY RECORD PROCESSING   *                                           
*-----------------------------------*                                           
VR029E   MVC   DATADISP,=H'42'         SET DISP TO XSPFILE                      
         BRAS  RE,RDXOHI               READ ORDER HISTORY - XSPOT               
         JNE   VR029M                                                           
*                                                                               
VR029F   XC    DMCB(24),DMCB           CLEAR DMCB                               
         LA    R4,OHDTAB               OHIS ELEMENTS TO DELETE                  
VR029I   CLI   0(R4),0                 LOOP THRU ALL ELEMENTS AND DEL           
         JE    VR029L                                                           
         MVC   ELCODE,0(R4)                                                     
VR029J   L     R6,AIO3                                                          
         JAS   RE,GETEL                                                         
         JNE   VR029K                                                           
         GOTOR RECUP,DMCB,(C'T',AIO3),(R6),0 DELETE ELEMENT                     
         J     VR029J                                                           
VR029K   LA    R4,OHDTABL(R4)          BUMP TO NEXT ELEM CODE                   
         J     VR029I                                                           
                                                                                
VR029L   GOTO1 DATAMGR,DMCB,(X'80',DMPUT),=C'XSPFIL',SVDA,AIO3,DMWRK            
         BRAS  R4,RDXOHI10             READ NEXT SEQ OHIS RECORD                
         JE    VR029F                                                           
                                                                                
VR029M   MVC   DATADISP,=H'24'         RESTORE TO SPOTFILE                      
         J     VRX                     EXIT                                     
*                                                                               
VR030    TM    ORDRFLG,ORDEDI             ROUTE MUST BE EDI?                    
         JZ    VR040                                                            
         CLC   OPSCMTH,=C'EDI'            CURRENT ROUTE IS EDI?                 
         BNE   ERRTNEDI                   NO - ERROR                            
*                                                                               
VR040    TM    ORDRFLG,ORDFAX             ROUTE MUST BE FAX?                    
         JZ    VR050                                                            
         CLC   OPSCMTH,=C'FAX'            CURRENT ROUTE IS FAX?                 
         BNE   ERRTNFAX                   NO - ERROR                            
*                                                                               
VR050    TM    ORDRFLG,ORDEML             ROUTE MUST BE EML?                    
         JZ    VR060                                                            
         CLC   OPSCMTH,=C'EML'            CURRENT ROUTE IS EML?                 
         BNE   ERRTNOE                    NO - ERROR                            
*                                                                               
VR060    L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,DOIDELQ             X'01' ELEMENT                         
         BAS   RE,GETEL                   GET X'01' ELEMENT                     
*                                                                               
         USING DOIDELD,R6                 PRIMARY ID ELEMENT DSECT              
         TM    ORDFLG1,ORDNDCON           MUST HAVE CONTRACT?'                  
         JZ    VR070                                                            
         CLC   DOIDCON,SPACES             HAVE REP CONTRACT NUMBER?             
         BNH   ERRCONT                    NO - ERROR                            
*                                                                               
VR070    LA    R6,ELEM                    BUILD NEW STATUS ELEMENT HERE         
         XC    ELEM,ELEM                  CLEAR THE ELEMENT                     
         USING DOSTELD,R6                 STATUS ELEMENT DSECT                  
         MVI   DOSTEL,DOSTELQ             X'12' ELEMENT                         
         MVI   DOSTLEN,DOSTLNQ            STANDARD LENGTH                       
         MVC   DOSTSTAT,ORDSTAT           STATUS                                
*                                                                               
         LA    R2,OPSPDATH                DATE                                  
         CLI   5(R2),0                    HAVE ANY INPUT?                       
         BE    NEEDFLDS                   NO - ERROR                            
*                                                                               
         CLC   =C'TODAY',8(R2)            DATE = TODAY?                         
         BE    VR080                      YES                                   
         GOTO1 DATVAL,DMCB,(0,8(R2)),DUB  NO - VALIDATE MMMDD/YY                
         OC    0(4,R1),0(R1)              VALID DATE?                           
         BZ    INVLFLD                    NO                                    
         GOTO1 DATCON,DMCB,(0,DUB),(19,DOSTDATE)                                
         B     VR090                      RE-DISPLAY DATE                       
*                                                                               
VR080    GOTO1 DATCON,DMCB,(5,0),(19,DOSTDATE)                                  
*                                                                               
VR090    XC    8(8,R2),8(R2)              CLEAR DATE                            
         MVI   5(R2),8                    INPUT LENGTH OF 8                     
         GOTO1 DATCON,DMCB,(8,DOSTDATE),(11,8(R2))                              
         OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         LA    R2,OPSPTIMH                TIME                                  
         CLI   5(R2),0                    HAVE ANY INPUT?                       
         BE    NEEDFLDS                   NO - ERROR                            
*                                                                               
         CLC   =C'NOW',8(R2)              TIME = NOW?                           
         BE    VR110                      YES                                   
         MVC   FULL(2),8(R2)              HH                                    
         MVC   FULL+2(2),11(R2)           MM                                    
         CLI   10(R2),C'.'                C'.'?                                 
         BNE   INVLFLD                    NO - INVALID SYNTAX                   
*                                                                               
         LA    RF,FULL                    HHMM                                  
         LA    R4,4                       4 DIGITS                              
*                                                                               
VR100    CLI   0(RF),C'0'                 LESS THAN C'0'?                       
         BL    INVLFLD                    YES - ERROR                           
         CLI   0(RF),C'9'                 HIGHER THAN C'9'?                     
         BH    INVLFLD                    YES - ERROR                           
         LA    RF,1(RF)                   BUMP TO NEXT DIGIT                    
         BCT   R4,VR100                   CHECK NEXT DIGIT                      
         CLC   FULL(2),=C'23'             GREATER THAN 23 HOURS?                
         BH    INVLFLD                    YES - ERROR                           
         CLC   FULL+2(2),=C'59'           GREATER THAN 59 MINUTES?              
         BH    INVLFLD                    YES - ERROR                           
         GOTO1 HEXIN,DMCB,FULL,DOSTTIME,4 SET DOSTTIME                          
         B     VR120                      DONE W/TIME                           
*                                                                               
VR110    THMS  DDSTIME=YES                GET DDS TIME                          
         STCM  R0,15,PACKOF4B             6:00 AM                               
         ST    R1,FULL                    HHMMSS+                               
         AP    PACKOF4B,FULL              ADD HHMMSS+ TO 6AM                    
         ICM   R1,15,PACKOF4B             DDS TIME                              
         SRL   R1,12                      GET RID OF SECONDS AND SIGN           
         STCM  R1,3,DOSTTIME              HHMM                                  
         GOTO1 HEXOUT,DMCB,DOSTTIME,WORK,L'DOSTTIME                             
         MVC   8(2,R2),WORK               HH                                    
         MVI   10(R2),C'.'                C'.'                                  
         MVC   11(2,R2),WORK+2            MM                                    
         MVI   5(R2),5                    CHANGE INPUT LENGTH                   
VR120    OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         TM    ORDFLG1,ORDHASBT           HAS STATUS BITS?                      
         JZ    VR130                       NO                                   
         MVI   DOSTLEN,DOSTLNQ3                                                 
         CLI   OPSMED,C'R'                ONLY FOR MEDIA R                      
         JNE   VR130                                                            
         MVC   DOSTTYPE,ORDSTBIT                                                
*                                                                               
VR130    CLI   DOSTSTAT,DEMDLVD           STATUS IS EMAIL DELIVERED?            
         JNE   VR140                                                            
         L     RF,ADOSTEL                                                       
         CLI   DOSTSTAT-DOSTELD(RF),DEMSENT   PREVIOUS EMAIL SENT?              
         JNE   INVLFLD                                                          
         MVI   DOSTLEN,DOSTLNQ6           YES, THEN SET EMAIL ELEMENT           
         MVC   DOSTDRPP,=CL8'EMAIL'                                             
                                                                                
VR140    CLI   DOSTSTAT,QFAXCNCL                                                
         JE    VR145                                                            
         CLI   DOSTSTAT,DFXDLVD           AND STATUS IS DELIVERED?              
         JNE   VR150                                                            
         MVC   HALF,FFS                                                         
VR145    L     RF,ADOSTEL                                                       
         CLI   DOSTSTAT-DOSTELD(RF),DFXSENT   PREVIOUS FAX SENT?                
         JNE   INVLFLD                                                          
         J     VR175                                                            
*                                                                               
VR150    LA    R2,OPSPUIDH                USER ID                               
         OI    6(R2),X'80'                TRANSMIT                              
         TM    ORDFLG1,ORDHASID           MUST HAVE ID?                         
         JNZ   VR160                                                            
         CLI   5(R2),0                    ANY INPUT?                            
         JNE   INVLFLD                    YES - ERROR                           
*                                                                               
VR160    TM    ORDFLG1,ORDHASID           MUST HAVE ID?                         
         JZ    VR200                                                            
         CLI   5(R2),0                    ANY INPUT?                            
         JE    NEEDFLDS                   NO - ERROR                            
*                                                                               
         CLI   OPSMED,C'R'                MEDIA R?                              
         JNE   VR170                      NO                                    
         MVI   DOSTLEN,DOSTLNQ6           LENGTH FOR REP PREFIX/OFFICE          
         MVC   DOSTDRPP(10),8(R2)         DELIVERED REP PREFIX/OFFICE           
         OC    DOSTDRPP,SPACES            SPACE PAD                             
         B     VR200                                                            
*                                                                               
VR170    BAS   RE,GETUSRID                GET USER ID                           
VR175    MVI   DOSTLEN,DOSTLNQ2           LENGTH W/USER ID                      
         MVC   DOSTIDNM,HALF              USER-ID                               
         DROP  R6,R3                      DROP R6                               
*                                                                               
VR200    L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,DOSTELQ             X'12' ELEMENT                         
         BAS   RE,GETEL                   GET FIRST X'12' ELEM                  
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
*&&DO                                                                           
         L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,DOXMTELQ            X'11' ELEMENT                         
         BAS   RE,GETEL                   GET FIRST X'11' ELEM                  
         BNE   VRX                        NOT THERE                             
*                                                                               
         USING DOXMTELD,R6                TRANSMISSION ELEM DSECT               
         LA    R4,ELEM                    X'12' ELEMENT JUST ADDED              
         USING DOSTELD,R4                 X'12' ELEMENT DSECT                   
         MVC   DOXMTSTA,DOSTSTAT          STATUS                                
         MVC   DOXMTSTD,DOSTDATE          DATE                                  
         MVC   DOXMTSTT,DOSTTIME          TIME                                  
         DROP  R4,R6                      DROP USINGS                           
*&&                                                                             
VRX      OI    OPSPSTAH+1,X'20'           PROTECT                               
         OI    OPSPDATH+1,X'20'           PROTECT                               
         OI    OPSPTIMH+1,X'20'           PROTECT                               
         OI    OPSPUIDH+1,X'20'           PROTECT                               
         B     XIT                                                              
*                                                                               
*--------------------------------------*                                        
* READ MAKE GOOD CABLE - XSPOT FILE    *                                        
*--------------------------------------*                                        
RDXSPT   LR    R4,RE                                                            
         LA    R6,KEY                     R6 = KEY                              
         XC    KEY,KEY                    CLEAR THE KEY                         
         USING MNXKEY,R6                  DARE MAKEGOOD DSECT                   
         MVI   MNXKTYPE,MNXKTYPQ          X'0D'                                 
         MVI   MNXKSBTY,MNXKSBTQ          X'36'                                 
         MVC   MNXKAGMD,BAGYMD            A/M                                   
         MVC   MNXKORDR,BINORDER          ORDER NUMBER                          
         MVC   KEYSAVE,KEY                SAVE KEY                              
         GOTO1 DATAMGR,DMCB,(X'00',DMRDHI),=C'XSPDIR ',KEY,KEY,0                
         J     RDXSPT20                                                         
RDXSPT10 LA    R6,KEY                     R6 = KEY                              
         GOTO1 DATAMGR,DMCB,(X'00',DMRSEQ),=C'XSPDIR ',KEY,KEY,0                
RDXSPT20 CLC   KEY(MNXKGRP-MNXKEY),KEYSAVE                                      
         JNE   NE                                                               
         GOTO1 DATAMGR,DMCB,DMGET,=C'XSPFIL ',MNXKDA,AIO2,DMWRK                 
         J     EQ                                                               
*--------------------------------------*                                        
* READ MAKE GOOD RECORD - SPOT FILE    *                                        
*--------------------------------------*                                        
RDSPOT   LR    R4,RE                                                            
         LA    R6,KEY                     R6 = KEY                              
         XC    KEY,KEY                    CLEAR THE KEY                         
         USING DAREMGND,R6                DARE MAKEGOOD DSECT                   
         MVI   MNKTYPE,MNKTYPQ            X'0D'                                 
         MVI   MNKSUBTY,MNKSTYPQ          X'36'                                 
         MVC   MNKAGMD,BAGYMD             A/M                                   
         MVC   MNKBYR,FULL                BUYER                                 
         MVC   MNKORDER,BINORDER          ORDER NUMBER                          
         MVC   KEYSAVE,KEY                PASS BACK DELETED RECORDS             
         GOTO1 DATAMGR,DMCB,(X'00',DMRDHI),=C'SPTDIR ',KEY,KEY,0                
         J     RDSPOT20                                                         
RDSPOT10 LA    R6,KEY                     R6 = KEY                              
         GOTO1 DATAMGR,DMCB,(X'00',DMRSEQ),=C'SPTDIR ',KEY,KEY,0                
RDSPOT20 CLC   KEY(MNKGROUP-MNKEY),KEYSAVE            KEY MATCHES?              
         JNE   NE                         NO - NO ORDER FOUND                   
         GOTO1 DATAMGR,DMCB,DMGET,=C'SPTFIL ',MNKDSKAD,AIO2,DMWRK               
         J     EQ                                                               
*--------------------------------------*                                        
* READ ORDER HISTORY - XSPOT FILE      *                                        
*--------------------------------------*                                        
RDXOHI   LR    R4,RE                                                            
         LA    R6,KEY                  R6 = KEY                                 
         XC    KEY,KEY                 CLEAR THE KEY                            
         USING OHISRECD,R6             DARE MAKEGOOD DSECT                      
         MVI   OHISTYP,OHISTYQ         TYPE                                     
         MVI   OHISSTYP,OHISSTYQ       SUB-TYPE                                 
         MVC   OHISORD,BINORDER        ORDER NUMBER                             
         MVC   OHISBKAM,BAGYMD         A/M                                      
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),=C'XSPDIR',KEY,KEY,0                 
         J     RDXOHI20                                                         
                                                                                
RDXOHI10 LA    R6,KEY                  R6 = KEY                                 
         GOTO1 DATAMGR,DMCB,(X'80',DMRSEQ),=C'XSPDIR',KEY,KEY,0                 
RDXOHI20 CLC   KEY(OHISBKCL-OHISKEY),KEYSAVE            KEY MATCHES?            
         JNE   NE                      NO - NO ORDER FOUND                      
         MVC   SVDA,OHISDDA                                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMGET),=C'XSPFIL',SVDA,AIO3,DMWRK            
         J     EQ                                                               
*----------------------------------------------------------*                    
* DARE ORDER SUPPLEMENT RECORDS (DOKCMT != 0) PROCESSING                        
*----------------------------------------------------------*                    
DOSREC00 ST    RE,SAVERE                                                        
         LR    R4,RE                                                            
         LA    R6,KEY                     R6 = KEY                              
         XC    KEY,KEY                    CLEAR THE KEY                         
         USING DAREORDD,R6                DARE MAKEGOOD DSECT                   
         L     R4,AIO                                                           
         MVC   KEY(12),0(R4)                                                    
         MVC   KEYSAVE,KEY                PASS BACK DELETED RECORDS             
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),=C'SPTDIR ',KEY,KEY,0                
         J     DOSREC20                                                         
DOSREC10 LA    R6,KEY                     R6 = KEY                              
         GOTO1 DATAMGR,DMCB,(X'80',DMRSEQ),=C'SPTDIR ',KEY,KEY,0                
DOSREC20 CLC   KEY(12),KEYSAVE            KEY MATCHES?                          
         JNE   DOSRECXX                   EXIT ROUTINE                          
         MVC   SVDA,DOKCMT+2                                                    
         GOTO1 DATAMGR,DMCB,(X'80',DMGET),=C'SPTFIL ',SVDA,AIO3,DMWRK           
         L     R6,AIO3                 DARE ORDER RECORD                        
         CLI   DOKCMT,0                                                         
         JE    DOSREC10                READ NEXT RECORD                         
         LA    R4,DODTAB               DELETE ELEM                              
DOSREC30 CLI   0(R4),0                                                          
         JE    DOSREC45                READ NEXT RECORD                         
         MVC   ELCODE,0(R4)            ELEM CODE TO BE DELETED                  
DOSREC40 L     R6,AIO3                                                          
         JAS   RE,GETEL                FIND THE ELEM CODE                       
         JNE   DOSREC50                                                         
         GOTOR RECUP,DMCB,(C'S',AIO3),(R6),0                                    
         J     DOSREC40                                                         
DOSREC45 XC    DMCB(24),DMCB           CLEAR DMCB                               
         GOTO1 DATAMGR,DMCB,(X'80',DMPUT),=C'SPTFIL ',SVDA,AIO3,DMWRK           
         J     DOSREC10                                                         
DOSREC50 LA    R4,DODTABL(R4)          BUMP TO NEXT ELEM CODE IN TABLE          
         J     DOSREC30                                                         
DOSRECXX L     RE,SAVERE                                                        
         BR    RE                                                               
*--------------------------------------*                                        
* MAKE GOOD RECORD CHECK               *                                        
*--------------------------------------*                                        
CHKMG00  LR    R4,RE               SAVE RE                                      
         USING MNSTELD,R6                                                       
         L     R6,AIO2             R6-MAKE GOOD NOTICE RECORD                   
         MVI   ELCODE,MNSTELQ      MAKEGOOD GROUP STATUS ELEMENT-X'05'          
         JAS   RE,GETEL            HAVE ONE?                                    
         JNE   EQ                  NO                                           
CHKMG10  CLI   MNSTSTAT,MNSTNEW    STATUS=NEW                                   
         JE    NE                  NEXT                                         
         CLI   MNSTSTAT,MNSTAMND   STATUS=AMENDED                               
         JE    NE                  NEXT                                         
         CLI   MNSTSTAT,MNSTAPP    STATUS=APPROVED                              
         JE    NE                  NEXT                                         
         CLI   MNSTSTAT,MNSTSAPP   STATUS=SELF-APPROVED                         
         JE    NE                  NEXT                                         
         CLI   MNSTSTAT,MNSTHOLD   STATUS=ON-HOLD                               
         JE    NE                  NEXT                                         
         CLI   MNSTSTAT,MNSTGOIN   STATUS=GOING TO BE OKAYED                    
         JE    NE                  NEXT                                         
         CLI   MNSTSTAT,MNSTDELV   STATUS=DELIVERED                             
         JNE   EQ                  NEXT                                         
         JAS   RE,NEXTEL                                                        
         JE    CHKMG10             NO                                           
         J     EQ                                                               
*                                                                               
EQ       CR    RB,RB                                                            
         J     *+6                                                              
NE       CR    RB,0                                                             
         LR    RE,R4               SAVE RE                                      
         BR    RE                                                               
*                                                                               
SAVERE   DS    F                                                                
*                                                                               
OHDTAB   DS    0H                    ORDER HISTORY DELETE TABLE                 
         DC    AL1(OBDELEMQ)                                                    
OHDTABL  EQU   *-OHDTAB                                                         
         DC    AL1(OHSPTELQ)                                                    
         DC    AL1(OHDOLELQ)                                                    
         DC    AL1(OHCOVELQ)                                                    
         DC    AL1(OHTAXELQ)                                                    
         DC    XL1'15'                                                          
         DC    XL1'16'                                                          
         DC    XL1'17'                                                          
         DC    XL1'18'                                                          
         DC    AL1(0)                                                           
*                                                                               
DODTAB   DS    0H                    DARE ORDER DELETE TABLE                    
         DC    AL1(DOIDELQ)                                                     
DODTABL  EQU   *-DODTAB                                                         
         DC    AL1(DOI2ELQ)                                                     
         DC    AL1(DOCBLELQ)                                                    
         DC    AL1(DOBY2ELQ)                                                    
         DC    AL1(DOCM2ELQ)                                                    
         DC    AL1(DRURLELQ)                                                    
         DC    AL1(DRLINELQ)                                                    
         DC    AL1(DOVPRELQ)                                                    
         DC    AL1(DOEMLELQ)                                                    
         DC    AL1(DOWHOELQ)                                                    
         DC    AL1(DOEMXELQ)                                                    
         DC    AL1(DOEGTELQ)                                                    
         DC    AL1(DOSAPELQ)                                                    
         DC    AL1(DOPSELQ)                                                     
         DC    AL1(DOPNELQ)                                                     
         DC    AL1(DOPPELQ)                                                     
         DC    AL1(DOPMELQ)                                                     
DODTAB2  DC    AL1(DORPELQ2)                                                    
         DC    AL1(DORPELQ)                                                     
         DC    AL1(DOXMTELQ)                                                    
         DC    AL1(DOSTELQ)                                                     
         DC    AL1(COLELQ)                                                      
         DC    AL1(MGCOLELQ)                                                    
         DC    AL1(DOBUYELQ)                                                    
         DC    AL1(DODEMELQ)                                                    
         DC    AL1(DOCOMELQ)                                                    
         DC    AL1(DOWIGELQ)                                                    
         DC    AL1(0)                                                           
**************                                                                  
* PATCHES ORDER TO UNSENT STATUS                                                
*  - REMOVES ALL X'11' ELEMENT                                                  
*  - REMOVES ALL X'12' ELEMENT                                                  
*  - RESETS DOIDELEM                                                            
*  - RESETS DOSPELEM                                                            
**************                                                                  
ORDUNSND NTR1                                                                   
         J     XIT                                                              
**************                                                                  
* GETS THE USER ID                                                              
**************                                                                  
GETUSRID NTR1                                                                   
*                                                                               
         MVC   SAVEKEY,KEY                SAVE THE ORDER KEY                    
         XC    KEY,KEY                    LOOK FOR THE USER ID RECORD           
         LA    R4,KEY                     R4 = KEY                              
         USING CTIREC,R4                  USER ID DSECT                         
         MVI   CTIKTYP,CTIKTYPQ           C'I'                                  
         MVC   CTIKID,8(R2)               USER ID                               
         OC    CTIKID,SPACES              SPACE PAD                             
         DROP  R4                         DROP USER ID DSECT                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),AIO2                 
         CLI   8(R1),0                    FOUND THE USER-ID?                    
         BNE   INVLFLD                    NO - ERROR                            
*                                                                               
         L     R6,AIO2                    A(USER-ID) RECORD                     
         USING CTIREC,R6                  USER ID DSECT                         
         LA    R6,CTIDATA                 A(FIRST ELEMENT)                      
         MVI   ELCODE,X'02'               DESCRIPTION ELEMENT                   
         BAS   RE,NEXTEL                  HAVE ONE?                             
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
*                                                                               
         USING CTDSCD,R6                  DESCRIPTION ELEM DSECT                
         MVC   HALF,CTDSC                 USER ID                               
         DROP  R6                         DROP R6                               
*                                                                               
         MVC   KEY(13),SAVEKEY            SAVED KEY                             
*                                                                               
         GOTO1 HIGH                       READ HIGH                             
         CLC   KEY(13),KEYSAVE            FOUND THE KEY?                        
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
*                                                                               
         GOTO1 GETREC                     GET THE RECORD                        
*                                                                               
         B     XIT                        RETURN TO CALLER                      
***********************************************************************         
* XRPUT - ADD/CHANGE THE ORDER PATCH RECORD                           *         
***********************************************************************         
XRPUT    XC    ELEM,ELEM                  CLEAR ELEM                            
         LA    R4,ELEM                    R4 = ELEM                             
         USING DOPSTATD,R4                STATUS PATCH ELEM DSECT               
         MVI   DOPSEL,DOPSELQ             X'01' ELEMENT                         
         MVI   DOPSLEN,DOPSLNQ            ELEMENT LENGTH                        
*                                                                               
         L     R6,AIO                     ORDER RECORD                          
         MVI   ELCODE,DOSTELQ             X'12' ELEMENT                         
         BAS   RE,GETEL                   HAVE A X'12' ELEMENT?                 
         BNE   XRXIT                      NO - EXIT                             
*                                                                               
         USING DOSTELD,R6                 STATUS ELEMENT DSECT                  
         MVC   DOPSNST,DOSTSTAT           NEW STATUS                            
         MVC   DOPSNDT,DOSTDATE           NEW DATE                              
         MVC   DOPSNTM,DOSTTIME           NEW TIME                              
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(19,DOPSDAT)                                   
*                                                                               
         THMS  DDSTIME=YES                GET DDS TIME                          
         STCM  R0,15,PACKOF4B             6:00 AM                               
         ST    R1,FULL                    HHMMSS+                               
         AP    PACKOF4B,FULL              ADD HHMMSS+ TO 6AM                    
         ICM   R1,15,PACKOF4B             DDS TIME                              
         SRL   R1,12                      GET RID OF SECONDS AND SIGN           
         STCM  R1,3,DOPSTIM               HHMM                                  
*                                                                               
         BAS   RE,NEXTEL                  HAVE ANOTHER X'12' ELEMENT?           
         BNE   XR05                       NO                                    
*                                                                               
         MVC   DOPSOST,DOSTSTAT           OLD STATUS                            
         MVC   DOPSODT,DOSTDATE           OLD DATE                              
         MVC   DOPSOTM,DOSTTIME           OLD TIME                              
         DROP  R6                         DROP ORDER REC USING                  
*                                                                               
XR05     GOTO1 GETFACT,DMCB,0             CALL GETFACT                          
         L     R1,0(R1)                   A(INPUT BLOCK)                        
         USING FACTSD,R1                  FACTD DSECT                           
         MVC   DOPSPID,FAPASSWD           PID OF PATCHER                        
         DROP  R1                         DROP R1                               
*                                                                               
         GOTO1 GETFACT,DMCB,(X'80',0),F#UTLD                                    
         L     R1,0(R1)                   A(INPUT BLOCK)                        
         USING F@UTLD,R1                  FACTD DSECT                           
         CLC   F@TICKET,SPACES            HAVE A TICKET?                        
         BNH   *+14                       NO                                    
         MVI   DOPSLEN,DOPSLNQ2           LONGER ELEMENT LENGTH                 
         MVC   DOPSTIC,F@TICKET           TICKET NUMBER                         
         DROP  R1                         DROP R1                               
*                                                                               
         MVC   AIO,AIO2                   ADD/GET RECORD INTO AIO2              
         LA    R6,KEY                     ORDER KEY                             
         USING DOKEY,R6                   ORDER PATCH KEY                       
         MVI   DOKCMT,DOKPSTAT            ORDER/PATCH RECORD TYPE               
*                                                                               
         GOTO1 HIGH                       READ HIGH                             
         CLC   KEY(13),KEYSAVE            KEY ALREADY EXISTS?                   
         BE    XR10                       YES - GO UPDATE RECORD                
*                                                                               
         L     R6,AIO                     BUILD RECORD IN AIO2                  
         MVC   DOKEY,KEYSAVE              COPY THE KEY                          
         MVC   DORFRST(DOPSLNQ2),DOPSEL   MOVE THE X'01' ELEMENT IN             
         LLC   R1,DOPSLEN                 X'71' ELEMENT LENGTH                  
         AHI   R1,DORFRST-DOKEY           ADD RECORD KEY LENGTH                 
         STCM  R1,3,DORLEN                RECORD LENGTH                         
         MVC   DORAGY,AGENCY              AGENCY                                
         GOTO1 ADDREC                     ADD THE RECORD                        
         B     XRXIT                      EXIT                                  
*                                                                               
XR10     GOTO1 GETREC                     GET THE RECORD                        
*                                                                               
         L     R6,AIO2                    RECORD IN AIO2                        
         LA    R6,24(R6)                  PUT X'01' ELEMENT HERE                
*                                                                               
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
*                                                                               
         GOTO1 PUTREC                     PUT THE RECORD BACK                   
*                                                                               
XRXIT    MVC   AIO,AIO1                   RESTORE AIO                           
         B     XIT                        EXIT                                  
         DROP  R4                         DROP R4                               
***********************************************************************         
* GET THE PFKEY INFORMATION                                           *         
***********************************************************************         
GETPF    NTR1                                                                   
*                                                                               
         GOTO1 INITIAL,DMCB,PFTABLE       INITIALIZE THE PFKEYS                 
*                                                                               
         B     XIT                        RETURN                                
***********************************************************************         
* RELOTAB                                                             *         
***********************************************************************         
RELOTAB  DS    0D                                                               
         DC    AL4(ORDSTAB),AL4(AORDSTAB-LSSD)                                  
         DC    X'FF'                                                            
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                        *         
***********************************************************************         
PFRTNQ   EQU   12                         RETURN                                
*                                                                               
PFTABLE  DS    0C                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,PFRTNQ,PFTRPROG,0,0,0)                               
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                            LTORG                                 
         DROP  R7,RB                                                            
***********************************************************************         
* GENERAL ERROR MESSAGES                                                        
***********************************************************************         
INVLFLD  MVI   GERROR1,INVALID                                                  
         J     ERREXIT                                                          
*                                                                               
ERRINVMD LHI   RF,1023             INVALID MEDIA                                
         J     ER2EXIT             GO PROCESS THE ERROR                         
*                                                                               
ERRCONT  LHI   RF,1034             CONTRACT NOT FOUND                           
         J     ER2EXIT             GO PROCESS THE ERROR                         
*                                                                               
ERRTNEDI LHI   RF,1393             CURRENT ROUTE MUST BE EDI                    
         J     ER2EXIT             GO PROCESS THE ERROR                         
*                                                                               
ERRTNFAX LHI   RF,1403             CURRENT ROUTE MUST BE FAX                    
         J     ER2EXIT             GO PROCESS THE ERROR                         
*                                                                               
ERRTNOE  LHI   RF,1404             CURRENT ROUTE MUST BE OE                     
         J     ER2EXIT             GO PROCESS THE ERROR                         
*                                                                               
ERRNTSNT LHI   RF,1394             ORDER NEVER SENT, WHY PATCH??                
         J     ER2EXIT             GO PROCESS THE ERROR                         
*                                                                               
ERRNTAMG LHI   RF,1476             ACTIVE MAKEGOOD - CANNOT PATCH               
         J     ER2EXIT             GO PROCESS THE ERROR                         
*                                                                               
ER2EXIT  STCM  RF,3,GERROR         ERROR NUMBER                                 
         L     R1,ATIOB            SET CURSOR ADDRESS                           
         USING TIOBD,R1            TRANSLATOR DSECT                             
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R1                  DROP R1                                      
         MVI   GETMSYS,2           SPOT MESSAGES                                
         J     ERREXIT                                                          
***********************************************************************         
* ERROR MESSAGES (SYSTEM 23)                                                    
***********************************************************************         
ERRNOORD MVI   GERROR1,215         CANNOT FIND THAT ORDER                       
         J     ERREXIT                                                          
*                                                                               
ERRPF12  MVI   GERROR1,241         CANNOT FIND THAT ORDER                       
         J     ERREXIT                                                          
*                                                                               
INVLDPF  MVI   GERROR1,ERINVPFK    INVALID PF KEY                               
         J     ERREXIT                                                          
*                                                                               
INVSTATS MVI   GERROR1,9           SENT STATUS MUST FOLLOW DELIVERED            
         J     ERREXIT                                                          
***********************************************************************         
* GENERAL INFO MESSAGES                                                         
***********************************************************************         
NEEDFLDS MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         J     INFEXIT                                                          
*                                                                               
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
***********************************************************************         
* ORDER STATUS TABLE                                                            
***********************************************************************         
ORDSTAB  DS    0H                                                               
ORDUNSNT DC    AL1(0),CL6'UNSENT',X'080000'                                     
ORDUNDOS DC    AL1(0),CL6'UNDO  ',X'040000'                                     
ORDDLVRD DC    AL1(DDLVRD),CL6'DLVRED',X'408000'                                
ORDEMLD  DC    AL1(DEMDLVD),CL6'EMDLVD',X'002000'                               
ORDFAXD  DC    AL1(DFXDLVD),CL6'FXDLVD',X'004000'                               
ORDFAXC  DC    AL1(QFAXCNCL),CL6'FAXCAN',X'004000'                              
ORDAPPR  DC    AL1(QAPP),CL6'OPENED',X'808000'                                  
ORDCNFM  DC    AL1(QCFMD),CL6'CNFRMD',X'A08020'                                 
ORDCANCF DC    AL1(QCFMD),CL6'CANCFM',X'B08021'                                 
ORDRJCT  DC    AL1(QRJCT),CL6'RJCTED',X'008000'                                 
***ORDAMND  DC    AL1(QRJCT),CL6'AMEND',X'008000'                               
ORDNODA  DC    AL1(QNODARE),CL6'NTDARE',X'000000'                               
ORDUNDA  DC    AL1(QUNDARE),CL6'UNDARD',X'000000'                               
ORDRCAP  DC    AL1(QRCLAPPR),CL6'RCLAPP',X'008000'     ** RCL - APP  **         
**ORDNRCF  DC    AL1(QRCLCONF),CL6'RCLCNF',X'008000' **NOTRCL - CFMD **         
**ORDNRRJ  DC    AL1(QRCLREJD),CL6'RCLRJC',X'008000' **NOTRCL - REJ  **         
ORDRCTR  DC    AL1(QRCLTRNS),CL6'RCLXMT',X'008000'     ** RCL - XMIT **         
ORDRCWP  DC    AL1(QRCLWIP),CL6'RCLWIP',X'008000'      ** RCL - WIP  **         
ORDRCRD  DC    AL1(QRCLDELN),CL6'RCLDEL',X'008000'     ** RCL - DLVD **         
*                                                                               
FFS      DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                              
*                                                                               
DMRDHI   DC    C'DMRDHI  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMGET    DC    C'GETREC  '                                                      
DMPUT    DC    C'PUTREC  '                                                      
***********************************************************************         
* LOCAL SAVED STORAGE                                                           
***********************************************************************         
LSSD     DSECT                                                                  
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
SAVEKEY  DS    XL13                SAVED KEY                                    
BASERC   DS    A                   BASE RC                                      
RELO     DS    A                   A(RELO)                                      
VGLOBBER DS    A                   A(GLOBBER)                                   
AORDSTAB DS    A                   A(ORDER TYPE TABLE)                          
ADOSTEL  DS    A                   A(FIRST DOSTEL ELEMENT)                      
*                                                                               
BINORDER DS    XL4                 ORDER NUMBER IN BINARY                       
SVDA     DS    XL4                 D/A                                          
DMWRK    DS    12D                 DMWORK AREA                                  
SVSTATN  DS    XL3                 SAVE STATION                                 
         ORG   LSSD+L'SYSSPARE                                                  
***********************************************************************         
* DSECT TO COVER ORDER STATUS TABLE                                             
***********************************************************************         
ORDD     DSECT                                                                  
ORDSTAT  DS    XL1                 ORDER STATUS                                 
ORDDFLT  DS    CL6                 DEFAULT CODE TO DISPLAY                      
*                                                                               
ORDFLG1  DS    X                   FLAG 1                                       
ORDNDCON EQU   X'80'               - MUST HAVE CONTRACT                         
ORDHASID EQU   X'40'               - HAS ID FIELD                               
ORDHASBT EQU   X'20'               - HAS SPECIAL STATUS BIT                     
ORDMEDR  EQU   X'10'               - THIS STATUS IS ONLY FOR MEDIA R            
ORDDELET EQU   X'08'               - DELETE ALL STATUS                          
ORDUNDO  EQU   X'04'               - DELETE LAST STATUS                         
*                                                                               
ORDRFLG  DS    X                   ROUTE FLAG                                   
ORDEDI   EQU   X'80'               - ROUTE MUST BE EDI                          
ORDFAX   EQU   X'40'               - ROUTE MUST BE FAX                          
ORDEML   EQU   X'20'               - ROUTE MUST BE OE                           
*                                                                               
ORDSTBIT DS    X                   STATUS BIT                                   
ORDDLNQ  EQU   *-ORDD                                                           
*                                                                               
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSB1D          (OUR PATCHSTA SCREEN)                        
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDCOMFACSD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* SPOMSWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPOMSWORKD                                                     
         PRINT ON                                                               
* SPGENDRMKN                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRMKN                                                     
         PRINT ON                                                               
* SPGENDRORD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRORD                                                     
         PRINT ON                                                               
* SPGENORHIS                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENORHIS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPOMS11   12/20/20'                                      
         END                                                                    
