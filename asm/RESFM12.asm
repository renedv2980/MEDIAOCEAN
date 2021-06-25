*          DATA SET RESFM12    AT LEVEL 048 AS OF 03/16/05                      
*PHASE T81812A,*                                                                
         TITLE 'T81812 - RESFM12 - PROPOSAL HEADER PRINT'                       
*********************************************************************           
*                                                                   *           
*        RESFM12 --- PROPOSAL PRINTER                               *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* MAR27/89 (MRR) --- FORCE INVENTORY UPGRADE COMMENT OUT            *           
*                                                                   *           
* MAR31/89 (MRR) --- FIX CPP/CPM CALCULATIONS SO THAT THEY REFLECT  *           
*                     THE TRUE NUMBER OF SPOTS                      *           
*                                                                   *           
* APR03/89 (MRR) --- FIX CPP/CPM TOTAL LINE                         *           
*                                                                   *           
* APR14/89 (MRR) --- FIX PACKAGE TOTALS ON MULTIPLE PACKAGES        *           
*                    FIX BOOK LABELING CODE W/ USE OF 'NONE'        *           
*                    FIX (MAKE WORK) DEMO OVERRIDES                 *           
*                                                                   *           
* APR22/89 (MRR) --- CLEAN-UP                                       *           
*                    -FIX BOOK CODE                                 *           
*                    -ALL TEXT OPTION IS PRODUCING ALL TEXT, NEEDS  *           
*                     FILTERING                                     *           
*                    -MAKE SURE WE DON'T REPORT DELETED ITEMS       *           
*                    -MISC COSMETIC CHANGES                         *           
*                                                                   *           
* MAY08/89 (MRR) --- ALLOW FOR ZERO DEMO OVERRIDES                  *           
*                                                                   *           
* MAY12/89 (MRR) --- MAYBE SEMI-FINAL CLEAN-UP, MOVE LEN COLUMN     *           
*                     OVER NEXT TO COST, FLIP TEXT OPTION, NEW      *           
*                     REPORT OPTIONS SCREEN                         *           
*                                                                   *           
* AUG02/89 (MRR) --- >MAKE HEADLINE CALLS-AFFL-CHANN MATCH AVAIL    *           
*                      SUPPRESS CHANNEL NUMBER IFF CH = 1           *           
*                    >IF OPTION G IS 'Y' THEN WE SUPPRESS RATES,    *           
*                      WE STILL DO AND WANT PACKAGE TOTAL, JUST     *           
*                      LABEL IT FOR THIS CASE.  ALSO CHECK FOR NO   *           
*                      RATES SO THAT THE TOTAL IS ZERO.             *           
*                                                                   *           
* AUG20/89 (MRR) --- >MOVE MARKET, STATION, ET AL RIGHT TO MATCH    *           
*                     THE AVAIL(AS PER REQUEST FROM UNIVISION)      *           
*                                                                   *           
* SEP20/89 (MRR) --- >MAKE SPOT TOTALS FOR PROPOSAL CORRECT         *           
*                                                                   *           
* NOV01/89 (MRR) --- >SUPPRESS AUTO UPGRADE COMMENT ON NON PJ BOOKS *           
*                                                                   *           
* JAN15/90 (MRR) --- >MAKE INVENTORY I/O NON-UPDATE                 *           
*                                                                   *           
* MAR13/90 (MRR) --- >REMOVE OPTIONS I/J AND RELABLE THE REMAINING  *           
*                      OPTIONS.  LOOK FOR T=A AS TEXT=ALL AND       *           
*                      PROCESS IT.  ALLOW FOR SOON PROCESSING.      *           
*                                                                   *           
* 07MAY90  (EFJ) --- REMOVED 'SOURCE' FIELD (1 LEVEL BACK)          *           
*                                                                   *           
* MAY09/90 (MRR) --- >RATINGS DEMO OVERRIDES NEEDED TO BE *10       *           
*                    >FIX SUPPLIED TEXT FILTERING FOR P/E BOOKS     *           
*                                                                   *           
* OCT05/90 (MRR) --- >IMPLEMENT OPTION 'L' 1-DECIMAL DEMOS          *           
*                                                                   *           
* 09NOV90  (EFJ) ---> USE NEW GENCON FEATURE TO PASS REPORT-NAME    *           
*                     (CON NUMBER) INSTEAD OF 'REAV'                *           
*                   > TAKE OUT USELESS LINE OF CODE THAT ONLY       *           
*                     FUCKED THINGS UP                              *           
*                                                                   *           
* 14NOV90  (EFJ) ---> FIX MANUAL UPGRADE NOT PRINTING ON PROPOSAL   *           
*                     AND NOT PRINTING FOR P-BOOKS                  *           
*                                                                   *           
* 04DEC90  (EFJ) ---> PUT MARKET-NAME ON COVER SHEET                *           
*                                                                   *           
* 17SEP93  (BU ) ---> EXPAND SPOT COUNT DISPLAY BY 1 CHAR IF > 99   *           
*                                                                   *           
* 07JUN94  (BU ) ---> CORRECT SPOT AVERAGE CALCULATION              *           
*                                                                   *           
* 16APR96  (RHV) ---  SUPPORT 34 BYTE AGY ADDRESS FIELDS            *           
*                                                                   *           
*                     ***  END TOMBSTONE  ***                       *           
*********************************************************************           
*                                                                               
T81812   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1812**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE 1                                                                
         ST    RB,SAVERB                                                        
         ST    R7,SAVER7                                                        
         SPACE 1                                                                
         L     R3,=A(FORMDEMS)                                                  
         A     R3,RELO                                                          
         ST    R3,AFORMDEM                                                      
         L     R3,=A(FORMCPM)                                                   
         A     R3,RELO                                                          
         ST    R3,AFORMCPM                                                      
         L     R3,=A(ADDEM)                                                     
         A     R3,RELO                                                          
         ST    R3,AADDEM                                                        
         SPACE 1                                                                
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
         SPACE 2                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
*                                                                               
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
****************************************************************                
* IO1 = HEADER RECORD, THEN PACKAGE HEADER OR DETAIL RECORD    *                
* IO2 = INVENTORY RECORD                                       *                
* IO3 = TEXT RECORD                                            *                
****************************************************************                
         SPACE 3                                                                
****************************************************************                
****************************************************************                
*              VALIDATE KEY                                    *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VKEY     EQU   *                                                                
         SPACE                                                                  
         LA    RE,SYSSPARE                                                      
         L     RF,=AL4(4096-(SYSSPARE-SYSD))                                    
         XCEF                                                                   
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE CONTRACT  (REQUIRED)                                   *          
*====================================================================*          
         SPACE 1                                                                
         LA    R2,PRTCONH                                                       
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         GOTO1 VALICON,DMCB,(R2)                                                
         OI    4(R2),X'20'         INDICATE PREVIOUSLY VALIDATED                
         LA    R2,PRTSTNH          POINT TO CALL LETTERS FIELD                  
         GOTO1 DISPCON,DMCB,(R2)                                                
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE TYPE (REQUIRED)                                        *          
*       ALWAYS PROPOSALS                                             *          
*====================================================================*          
         SPACE 1                                                                
VK10     DS    0H                                                               
         OI    GENSTAT3,NOCLRSPK   TELL GENCON SETTING OWN SPOOL KEY            
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   SPOOLKEY+1(11),SPACES                                            
         MVC   SPOOLKEY+1(8),PRTCON                                             
         MVI   ETYPE,C'P'                                                       
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE SOURCE  (REQUIRED)                                     *          
*        I=INVENTORY,  S=SID                                         *          
*====================================================================*          
         SPACE 1                                                                
         MVI   ESOURCE,C'I'        ALWAYS                                       
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE HEADER NUMBER  (REQUIRED)                              *          
*====================================================================*          
         SPACE 1                                                                
VK40     MVC   RERROR,=AL2(MISSING)                                             
         LA    R2,PRTHDRH                                                       
         TM    4(R2),X'20'                                                      
         BO    VK50                                                             
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERREND                                                           
         STC   R0,XHDRNUM                                                       
         MVC   EHDRNUM,8(R2)                                                    
         OI    4(R2),X'20'                                                      
VK50     DS    0H                                                               
         MVC   AIO,AIO1            PUT HEADER INTO IO1                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RPRPKEY,R6                                                       
         MVI   RPRPKTYP,X'16'                                                   
         MVC   RPRPKREP,AGENCY                                                  
         MVC   RPRPKCON,CCONNUM                                                 
         MVC   RPRPKPRP,XHDRNUM                                                 
         MVC   RPRPKSRC,ESOURCE                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    VK70                                                             
         MVC   RERROR,=AL2(NOTFOUND)                                            
         LA    R2,PRTCONH                                                       
         B     ERREND                                                           
         SPACE 4                                                                
VK70     DS    0H                                                               
*                                                                               
*        THIS CODE WILL VALIDATE A 1-BYTE ZERO-INTENSITY FIELD                  
*        SO THAT THE REPORT DOESN'T GET GENERATED IMMEDIATELY UPON              
*        ENTRY TO THE SCREEN.                                                   
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE, NO TESTING                       
         BE    VK75                                                             
*                                                                               
         LA    R2,PRTVALH                                                       
         CLI   8(R2),C'X'                                                       
         BE    VK75                                                             
         MVI   8(R2),C'X'                                                       
         MVI   5(R2),1                                                          
* NEXT INSTRUCTION REMOVED IN THE BELIEF THAT IT IS FUCKING STUPID              
*         OI    1(R1),X'01'         FIELD HAS BEEN MODIFIED                     
         OI    6(R2),X'80'         TRANSMIT                                     
         SPACE 1                                                                
         LA    R2,PRTOPAH                                                       
         MVC   RERROR,=H'2'        PLEASE ENTER FIELDS AS REQUIRED              
         MVI   RMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         B     ERREND                                                           
         SPACE 1                                                                
VK75     MVI   OPS,C'N'            ALL DEFAULTS ARE N                           
         MVC   OPS+1(19),OPS                                                    
         LA    R4,OPS                                                           
         SPACE 1                                                                
         LA    R2,PRTOPAH                                                       
         LA    R3,PRTLSTH                                                       
         SR    R0,R0                                                            
         SPACE 1                                                                
VK80     CR    R2,R3                                                            
         BNL   VK90                                                             
         SPACE 1                                                                
         BAS   RE,CHECKOPT                                                      
         LA    R4,1(R4)            NEXT OPTION                                  
         BAS   RE,NEXTUF           GET NEXT UNPROTECTED FIELD                   
         B     VK80                                                             
         SPACE 1                                                                
VK90     EQU   *                                                                
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
CHECKOPT DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    COX                                                              
         CLI   8(R2),C'N'                                                       
         BE    COX                                                              
         CLI   8(R2),C'Y'                                                       
         BE    *+14                                                             
         MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
         MVI   0(R4),C'Y'                                                       
         B     COX                                                              
         SPACE 1                                                                
COX      BR    RE                                                               
         SPACE 1                                                                
NEXTUF   IC    R0,0(R2)            GET NEXT UNPROTECTED FIELD                   
         AR    R2,R0                                                            
         TM    1(R2),X'20'         PROTECTED                                    
         BO    NEXTUF                                                           
         BR    RE                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              PRINT REPORT                                    *                
****************************************************************                
         SPACE 1                                                                
PREP     DS    0H                                                               
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         XC    DETAC,DETAC         CLEAR OUT ACCUMULATORS                       
         XC    PKGAC,PKGAC                                                      
         XC    PROPAC,PROPAC                                                    
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         MVC   8(2,R2),CCONKOFF                                                 
         GOTO1 VALIOFF             GET OFFICE ADDRESS FOR HEADLINE              
         SPACE 1                                                                
         MVC   AIO,AIO1            PUT HEADER INTO IO1                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RPRPKEY,R6                                                       
         MVI   RPRPKTYP,X'16'                                                   
         MVC   RPRPKREP,AGENCY                                                  
         MVC   RPRPKCON,CCONNUM                                                 
         MVC   RPRPKPRP,XHDRNUM                                                 
         MVC   RPRPKSRC,ESOURCE                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         MVC   PRPKEY,KEY          SAVE PROPOSAL KEY                            
         GOTO1 GETREC                                                           
         BAS   RE,EXTHDR               EXTRACT INFO FROM HEADER                 
         GOTO1 =A(SETHEADS),DMCB,(RC),RR=RELO      SET HEADLINES ONCE           
         SPACE 1                                                                
         CLI   OPTB,C'Y'           Y=PRINT COVERSHEET                           
         BNE   PR10                                                             
         MVI   RCSUBPRG,0                                                       
         GOTO1 =A(COVERSHT),DMCB,(RC),RR=RELO                                   
PR10     MVI   RCSUBPRG,1                                                       
         CLI   OPTI,C'Y'           Y=PRINT MARKET & STATION TEXT                
         BNE   PR30                                                             
         BAS   RE,MSTEXT           GET MKT & STA TEXT AND PRINT                 
         SPACE 1                                                                
PR20     MVC   AIO,AIO1            PUT DETAIL RECORD IN IO1                     
         MVC   KEY(27),PRPKEY      RESTORE DATAMGR SEQUENCE                     
         GOTO1 READ                                                             
         SPACE 1                                                                
PR30     MVC   PRPKEY,KEY          SAVED PROPOSAL KEY                           
         GOTO1 SEQ                                                              
         CLC   KEY(23),PRPKEY      CHECK CHANGE OF PROPOSAL                     
         BE    PR40                                                             
         BAS   RE,PACKTOT          CALCULATE AND PRINT PACKAGE TOTALS           
         SPACE 1                                                                
         MVI   SPACING,2                                                        
         CLI   OPTA,C'Y'           Y=TRIPLE SPACE                               
         BNE   *+8                                                              
         MVI   SPACING,3                                                        
         GOTO1 SPOOL,DMCB,(R8)     SPACE AFTER PACKAGE TOTALS                   
         SPACE 1                                                                
         BAS   RE,PROPTOT          CALCULATE AND PRINT PROPOSAL TOTALS          
         B     XIT                 THAT'S ALL, FOLKS                            
         SPACE 1                                                                
PR40     GOTO1 GETREC                                                           
         CLC   KEY(25),PRPKEY      CHECK CHANGE OF PACKAGE                      
         BE    PR60                NO                                           
         OC    PRPKEY+23(2),PRPKEY+23    WAS LAST RECORD A HEADER               
         BZ    PR50                YES, SO NO PACKAGE TOTALS TO DO YET          
         BAS   RE,PACKTOT          COMPUTE AND PRINT PACKAGE TOTALS             
         SPACE 1                                                                
         MVI   SPACING,2                                                        
         CLI   OPTA,C'Y'           Y=TRIPLE SPACE                               
         BNE   *+8                                                              
         MVI   SPACING,3                                                        
         GOTO1 SPOOL,DMCB,(R8)     SPACE AFTER PACKAGE TOTALS                   
         SPACE 1                                                                
PR50     MVC   PKGCODE,RPRPKPLN                                                 
*        MVI   PKGTSPT,0           TOTAL SPOTS IN THE T PACKAGE                 
         SPACE 1                                                                
PR60     MVC   PRPKEY,KEY          SAVE DETAIL RECORD KEY                       
         CLI   RPRPKDET,0          DO WE NOW HAVE A PKG HEADER                  
         BNE   PR70                NO                                           
         BAS   RE,EXTPKG           EXTRACT PACKAGE HEADER DETAILS               
         B     PR30                                                             
         SPACE 1                                                                
PR70     BAS   RE,EXTDET           EXTRACT DETAILS FROM DETAIL RECORD           
         CLC   INVNO(3),=C'MAN'                                                 
         BE    PR80                                                             
         CLI   INVNO+2,0                                                        
         BE    PR90                                                             
         SPACE 1                                                                
         BAS   RE,INV              INVENTORY                                    
         B     PR100                                                            
         SPACE 1                                                                
PR80     BAS   RE,MAN              MANUAL                                       
         B     PR100                                                            
         SPACE 1                                                                
PR90     BAS   RE,PURE             PURE                                         
         SPACE 1                                                                
PR100    EQU   *                                                                
         MVI   SPACING,2                                                        
         CLI   OPTA,C'Y'           Y=TRIPLE SPACE                               
         BNE   PR110                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
PR110    EQU   *                                                                
         GOTO1 SPOOL,DMCB,(R8)     SPACE AFTER EACH DETAIL LINE                 
         B     PR20                                                             
         DROP  R6                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE CONTROLS READING OF INVENTORY RECORDS.                *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
INV      NTR1                                                                   
         MVC   AIO,AIO2            PUT INVENTORY RECORD IN IO2                  
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
         XC    KEY,KEY             BUILD KEY FOR INVENTORY HEADER               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP    USE PARENT REP, NOT AGENCY                   
         MVC   RINVKSTA,CCONKSTA                                                
         MVI   RINVKSTA+4,C'T'                                                  
         CLI   INVSAT,0            SATELLITE OPTION                             
         BE    *+10                                                             
         MVC   RINVKSTA+4(1),INVSAT                                             
         MVC   RINVKINV,INVNO                                                   
         MVC   RINVKSTD,INVDT                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BNE   XIT                                                              
         SPACE 2                                                                
INV20    MVC   INVKEY,KEY                                                       
         OC    INVDT,INVDT         IF DATE IS SPECIFIED,                        
         BZ    INV30                                                            
         CLC   INVDT,RINVKSTD         IT MUST MATCH                             
         BNE   XIT                                                              
         SPACE 2                                                                
INV30    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,INVDTP           DAY TIME PROGRAM TO P1-P3                    
         CLC   P(44),SPACES        DID NOT PASS EFFECTIVE DATE TEST             
         BE    INV120              IF THESE ARE NOW SPACES                      
         BAS   RE,ANYOVER                                                       
         BAS   RE,DAYTIME                                                       
         CLC   P3(15),SPACES                                                    
         BE    INV40                                                            
         CLC   P2(15),SPACES                                                    
         BNE   INV40                                                            
         MVC   P2(15),P3                                                        
         MVC   P3(15),SPACES                                                    
         SPACE 2                                                                
INV40    BAS   RE,HOWMANY                                                       
         XC    COMPTEXT,COMPTEXT                                                
*                         NOW READ AN INVENTORY DETAIL                          
         OC    CBOOKS(3),CBOOKS    IF NO HEADER BOOKS                           
         BZ    INV90               MUST STILL PRINT LEN AND SPOTS               
         SPACE 1                                                                
         LA    R1,CBOOKS           USE BOOKS                                    
         OC    DETBOOKS(3),DETBOOKS    BUT DETBOOKS OVERRIDES HDR BOOKS         
         BZ    *+8                                                              
         LA    R1,DETBOOKS         OR DETBOOKS                                  
         SPACE 1                                                                
         MVI   CPMSW,C'Y'                                                       
         TM    0(R1),X'80'                                                      
         BNO   *+8                                                              
         MVI   CPMSW,C'N'                                                       
         SPACE 1                                                                
         MVC   RINVKSRC(3),0(R1)   BOOK                                         
         SPACE 1                                                                
         L     RE,=A(SVCLST)       CONVERSION TABLE                             
         A     RE,RELO                                                          
         SPACE 1                                                                
INV50    CLC   3(1,RE),0(R1)       FROM BOOKVAL FORMAT TO SRC FORMAT            
         BE    INV60                                                            
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   INV50                                                            
         DC    H'0'                                                             
INV60    MVC   RINVKSRC,2(RE)                                                   
         SPACE 1                                                                
         BAS   RE,ANYBK                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     TEST ANY INVENTORY DETAIL                    
         BNE   INV110                                                           
         TM    KEY+27,X'80'        DELTED?                                      
         BNZ   INV110              YES-SKIP IT                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,DIGDEM           GOT A HIT                                    
         MVC   P+51(2),ACTCODE     OUTPUT PR CODE                               
         GOTO1 =A(BOOKOUT),DMCB,(RC),RR=RELO                                    
INV90    EQU   *                                                                
         BAS   RE,INVLINES                                                      
         BAS   RE,FORMAT                                                        
         BAS   RE,DINVTEXT                                                      
         CLI   OPTK,C'Y'           Y=SUPPRESS AUTO UPGRADE COMMENT              
         BE    INV110                                                           
         CLI   UPPRINT,C'Y'        PRINT UPGRADE?                               
         BE    INV95                                                            
         CLC   ACTCODE(2),=C'PJ'   UPGRADE COMMENT ONLY ON PJ'S                 
*         BNE   INV110                                                          
         BE    INV95               ALWAYS PRINT COMMENT                         
         OC    DETBOOKS(3),DETBOOKS   DETBOOKS OVERRIDES HDR BOOKS              
         BZ    *+16                                                             
         TM    DETBOOKS,X'04'                                                   
         BZ    INV110                                                           
         B     INV95                                                            
         TM    CBOOKS,X'04'          P-BOOK? - ALWAYS PRINT                     
         BZ    INV110                                                           
INV95    BAS   RE,UPOUT                                                         
         SPACE 1                                                                
INV110   EQU   *                                                                
         BAS   RE,ANYLEFT                                                       
         BAS   RE,TEXT                                                          
         B     XIT                                                              
         SPACE 2                                                                
INV120   EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(20),KEYSAVE                                                  
         BNE   XIT                                                              
         CLI   RINVKSRC,0                                                       
         BE    INV20                                                            
         B     INV120                                                           
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        INVLINES - CALC NUMBER OF LINES FOR THIS INVENTORY BOOK     *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
INVLINES NTR1                                                                   
*                                                                               
         ZIC   R1,MINLINES                                                      
         CLI   CNUMDEM,0                                                        
         BE    INVL10                                                           
         CLI   OPTH,C'N'                                                        
         BE    INVL10                                                           
         CLC   RATE(4),=F'0'                                                    
         BE    INVL10                                                           
         LA    R1,1(R1)                                                         
INVL10   EQU   *                                                                
         L     R6,AIO2                                                          
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   INVL20                                                           
         LA    R1,2(R1)                                                         
INVL20   EQU   *                                                                
         LA    R6,DETPJ                                                         
         OC    0(14,R6),0(R6)                                                   
         BZ    INVL30                                                           
         LA    R1,1(R1)                                                         
INVL30   EQU   *                                                                
         STC   R1,ALLOWLIN                                                      
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINES TO HANDLE PURE PROPOSALS                     *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
PURE     NTR1                                                                   
         MVC   AIO,AIO2            PUT PURE RECORD INTO IO2                     
         BAS   RE,ANYOVER                                                       
         LA    R2,CBOOKS                                                        
         LA    R5,DETBOOKS                                                      
         SPACE 2                                                                
         LR    R1,R2                                                            
         OC    0(3,R5),0(R5)                                                    
         BZ    *+6                                                              
         LR    R1,R5                                                            
         MVI   CPMSW,C'Y'                                                       
         TM    0(R1),X'80'                                                      
         BNO   *+8                                                              
         MVI   CPMSW,C'N'                                                       
         SPACE 1                                                                
         MVC   CBLOCK(2),INVNO        PURE NUMBER                               
         MVC   CBLOCK+6(1),INVSAT     SATELITTE                                 
         MVC   CBLOCK+7(2),1(R1)      BOOK                                      
         MVC   CBLOCK+10(60),SPACES                                             
         MVC   DATADISP,=H'23'     USE DEMO FILE DATA DISP                      
         SPACE 1                                                                
         GOTO1 DISPDTT             GET DAY/TIME TITLE FROM PURE RECORD          
         SPACE 1                                                                
         MVC   DATADISP,=H'34'     RESTORE TO REP DATADISP                      
         MVC   P(44),SPACES                                                     
         MVC   P(3),CBLOCK+10          DAY                                      
         MVC   P+4(6),CBLOCK+20        TIME                                     
         MVC   P+17(27),CBLOCK+40      TITLE                                    
         SPACE 1                                                                
         BAS   RE,ANYOVER          GET ANY OVERRIDES                            
         BAS   RE,DAYTIME                                                       
         SPACE 1                                                                
         OC    DET0B,DET0B         ANY BOOK LABEL                               
         BZ    *+14                                                             
         MVC   P+44(5),DET0B       USE IT INSTEAD OF BOOK                       
         B     PUR40                                                            
         SPACE 1                                                                
         L     RE,=A(SVCLST)                                                    
         A     RE,RELO                                                          
PUR20    CLC   0(1,R1),3(RE)                                                    
         BE    PUR30                                                            
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   PUR20                                                            
         DC    H'0'                                                             
PUR30    CLI   1(RE),C' '          MAY NEED TO PRECEDE BOOK                     
         BE    *+10                                                             
         MVC   P+44(1),1(RE)       WITH P,T,S OR E                              
         SPACE 1                                                                
         MVC   DUB,1(R1)                                                        
         GOTO1 DATCON,DMCB,(3,DUB),(8,WORK)                                     
         MVC   P+45(3),WORK                                                     
         CLI   DUB+1,0                                                          
         BNE   *+10                                                             
         MVC   P+45(3),=C'EST'                                                  
         MVC   P+48(2),WORK+6                                                   
         SPACE 1                                                                
PUR40    MVC   DATADISP,=H'23'     GET DEMOS FROM PURE RECORD                   
         BAS   RE,DIGDEM                                                        
         BAS   RE,FORMAT                                                        
         MVC   DATADISP,=H'34'     RESTORE TO REP DATADISP                      
         BAS   RE,TEXT                                                          
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO HANDLE MANUAL PROPOSALS                    *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
MAN      NTR1                                                                   
         BAS   RE,ANYOVER                                                       
         BAS   RE,DAYTIME                                                       
         LA    R2,CBOOKS                                                        
         OC    DETBOOKS(3),DETBOOKS                                             
         BZ    *+8                                                              
         LA    R2,DETBOOKS                                                      
         MVI   CPMSW,C'Y'                                                       
         TM    0(R2),X'80'                                                      
         BNO   *+8                                                              
         MVI   CPMSW,C'N'                                                       
         SPACE 1                                                                
         OC    DET0B,DET0B         ANY BOOK LABEL                               
         BZ    *+14                                                             
         MVC   P+45(5),DET0B       USE IT INSTEAD OF BOOK                       
         B     MAN30                                                            
         SPACE 1                                                                
         L     RE,=A(SVCLST)                                                    
         A     RE,RELO                                                          
MAN10    CLC   0(1,R2),3(RE)                                                    
         BE    MAN20                                                            
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   MAN10                                                            
         DC    H'0'                                                             
MAN20    CLI   1(RE),C' '          MAY NEED TO PRECEDE BOOK                     
         BE    *+10                                                             
         MVC   P+44(1),1(RE)       WITH P,T,S OR E                              
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(3,1(R2)),(8,WORK)                                   
         MVC   P+45(3),WORK                                                     
         CLI   2(R2),0                                                          
         BNE   *+10                                                             
         MVC   P+45(3),=C'EST'                                                  
         MVC   P+48(2),WORK+6                                                   
         SPACE 1                                                                
MAN30    MVI   P+52,C'*'                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,TEXT                                                          
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* ROUTINE TO PRINT AUTO UPGRADE COMMENT                              *          
*  (AUPOUT IS DDUPOUT)                                               *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
UPOUT    NTR1                                                                   
         LA    R6,DETPJ            LOOK FOR UPGRADE ELEMENT                     
         OC    0(14,R6),0(R6)      (DETAIL OVERRIDES HEADER)                    
         BZ    OUX                                                              
         SPACE 1                                                                
         GOTO1 VUPOUT,DMCB,(R6),P+9                                             
         BAS   RE,SPLAT                                                         
OUX      B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* ROUTINE TO PRINT MARKET AND STATION TEXT                           *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
MSTEXT   NTR1                                                                   
         MVC   AIO,AIO3            PUT TEXT RECORDS IN IO3                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP    USE PARENT REP, NOT AGENCY                   
         MVC   RINVKSTA,CCONKSTA                                                
         MVI   RINVKSTA+4,C'T'                                                  
         MVI   RINVKSRC,C'M'                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         SPACE 1                                                                
MS10     CLC   KEY(24),KEYSAVE                                                  
         BNE   MSX                 NOT MARKET OR STATION TEXT                   
         CLI   KEY+24,C'M'         MARKET TEXT                                  
         BE    MS20                                                             
         CLI   KEY+24,C'S'         STATION TEXT                                 
         BNE   MSX                                                              
         SPACE 1                                                                
MS20     BAS   RE,TXTPRNT                                                       
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     MS10                                                             
         SPACE 1                                                                
MSX      MVC   AIO,AIO1            RESET AIO                                    
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINES TO PRINT TEXTS                               *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
TEXT     NTR1                                                                   
*                                                                               
         MVI   DETTEXT,C'N'        Y=MANUAL OR REFERENCED TEXT EXISTS           
         L     R6,AIO1             DETAIL RECORD                                
         MVI   ELCODE,X'08'        TEXT ELEMENT                                 
         BAS   RE,GETEL                                                         
         B     TXT30                                                            
TXT20    EQU   *                                                                
         MVI   ELCODE,X'08'        RE-SET TEXT EL CODE                          
         BAS   RE,NEXTEL                                                        
TXT30    EQU   *                                                                
         BNE   TXT50                                                            
*                                                                               
         USING RAVLTEL,R6                                                       
         CLI   RAVLTTYP,C'T'       SPECIFIC TEXT NUMBER                         
         BNE   TXT40                                                            
         ZICM  R2,RAVLTNUM,2       ZERO IS DO ALL                               
         BNZ   TXT35                                                            
         BAS   RE,GETTEXT                                                       
         XC    COMPTEXT,COMPTEXT                                                
         B     TXT20               SO IT'S ALREADY BEEN PRINTED                 
TXT35    EQU   *                                                                
         MVC   DUB(2),RAVLTDTA     TEXT NO. REFERENCED                          
         LH    R2,DUB                                                           
         BAS   RE,GETTEXT          GO AND GET IT AND PRINT                      
         MVI   ELCODE,X'08'                                                     
         MVI   DETTEXT,C'Y'        Y=MANUAL OR REFERENCED TEXT EXISTS           
         B     TXT20                                                            
         SPACE 1                                                                
TXT40    CLI   RAVLTTYP,C'M'       MANUAL TEXT                                  
         BNE   TXT20                                                            
         MVI   DETTEXT,C'Y'        Y=MANUAL OR REFERENCED TEXT EXISTS           
         ZIC   R3,RAVLTLEN                                                      
         SH    R3,=H'5'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+7(0),RAVLTDTA     DIG IT OUT                                   
         BAS   RE,SPLAT            AND PRINT                                    
         B     TXT20                                                            
         SPACE 1                                                                
TXT50    OC    COMPTEXT,COMPTEXT   DID WE PRINT COMPULSORY TEXT                 
         BZ    TXT60                                                            
         LH    R2,COMPTEXT         NO - SO DO IT NOW                            
         BAS   RE,GETTEXT                                                       
         SPACE 1                                                                
TXT60    CLI   DETTEXT,C'Y'        Y=MANUAL OR REFERENCED TEXT EXISTS           
         BNE   TXTX                                                             
         BAS   RE,SPLAT            ENSURE A SPACE AFTER MANUAL TEXT             
TXTX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO READ TEXT RECORDS AND PRINT                *          
*   ON ENTRY, R2 HAS TEXT NUMBER                                     *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
GETTEXT  NTR1                                                                   
         MVC   AIO,AIO3            PUT TEXT RECORD IN IO3                       
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
         L     R1,AIO2                                                          
         MVC   KEY(27),0(R1)                                                    
         MVI   RINVKSRC,X'FF'                                                   
         STCM  R2,3,RINVKTXT       TEXT NUMBER (0 IS TX=ALL)                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
GT10     EQU   *                                                                
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GTXIT                                                            
         LTR   R2,R2               0=PRINT ALL AVAILABLE TEXT                   
         BZ    GT20                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GTXIT                                                            
GT20     EQU   *                                                                
         MVC   DUB,RINVKTXT        CURRENT TEXT NUMBER                          
         MVI   CKTXTOPT,0          SET CHECK OPT                                
         BAS   RE,TXTPRNT                                                       
         CLC   RINVKTXT,COMPTEXT   SEE IF WE'VE JUST PRINTED                    
         BNE   GT30                COMPULSORY TEXT                              
         XC    COMPTEXT,COMPTEXT                                                
GT30     EQU   *                                                                
         LTR   R2,R2                                                            
         BNZ   GTXIT                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     GT10                                                             
         SPACE 1                                                                
GTXIT    MVC   AIO,AIO1            RESET AIO                                    
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        DINVTEXT --- OUTPUT INVENTORY DEMO TRACK COMMENTS           *          
*                      TT, TP, ET ALLIA                              *          
*--------------------------------------------------------------------*          
DINVTEXT NTR1                                                                   
*                                                                               
         MVC   AIO,AIO2            POINT TO DEMO TRACK                          
         MVI   CKTXTOPT,1          SET DON'T CHECK FLAG                         
         BAS   RE,TXTPRNTA                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
TXTPRNT  NTR1                                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         B     TP01                                                             
*                                                                               
TXTPRNTA NTR1                                                                   
*                                                                               
TP01     EQU   *                                                                
         L     R6,AIO              TEXT RECORD                                  
         USING RINVKEY,R6                                                       
         CLI   CKTXTOPT,1          CHECK DON'T CHECK FLAG                       
         BE    TP05                                                             
         BAS   RE,TXTFILT          IF 'ALL' CHECK FILTERS                       
         BNZ   XIT                 NON-ZERO RETURN IS SKIP THIS RECORD          
TP05     EQU   *                                                                
         CLI   OPTJ,C'Y'           Y=PRINT TEXT & INV. NO.                      
         BNE   TP10                                                             
         CLI   RINVKSRC,X'FF'      IS IT A REG RATIONAL REC?                    
         BE    TP06C               YES                                          
         CLI   RINVKSRC,C'M'       MARKET FACT?                                 
         BE    TP06A               YES                                          
         CLI   RINVKSRC,C'S'       STATION FACT?                                
         BNE   TP10                NO, DEMO COMMENT NO TEXT NUMBER              
TP06A    EQU   *                                                                
         MVC   P(1),RINVKSRC                                                    
         MVC   P+1(1),=C'='                                                     
         B     TP06Z                                                            
TP06C    EQU   *                                                                
         MVC   P(2),=C'T='                                                      
TP06Z    EQU   *                                                                
         EDIT  (2,RINVKTXT),(4,P+2),ALIGN=LEFT                                  
         DROP  R6                                                               
         SPACE 1                                                                
TP10     MVI   ELCODE,X'01'                                                     
         BAS   RE,COUNTEXT                                                      
         BAS   RE,GETEL                                                         
         B     TP30                                                             
*                                                                               
TP20     BAS   RE,NEXTEL                                                        
*                                                                               
TP30     BNE   TP40                                                             
         USING RINVTEL,R6                                                       
         ZIC   R3,RINVTLEN                                                      
         SH    R3,=H'7'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+9(0),RINVTEXT                                                  
         BAS   RE,SPLAT                                                         
         B     TP20                                                             
*                                                                               
TP40     EQU   *                                                                
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        TXTFILT - FILTER TEXT RECORDS WHEN 'ALL' HAVE BEEN REQUESTED*          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
TXTFILT  NTR1                                                                   
*                                                                               
         L     R6,AIO3                                                          
         USING RINVKEY,R6                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   TXTFGOOD            NO FILTER ELEMENT                            
*                                                                               
         CLI   RINVFSRC-RINVFEL(R6),0   SERVICE FILTER                          
         BE    TXTF10                                                           
         CLC   CSOURCE(1),RINVFSRC-RINVFEL(R6)                                  
         BNE   TXTFBAD                                                          
*                                                                               
TXTF10   EQU   *                                                                
         CLI   RINVFYR-RINVFEL(R6),0    BOOK FILTER                             
         BE    TXTF20                                                           
         LA    R1,CBOOKS                                                        
         OC    DETBOOKS(3),DETBOOKS                                             
         BZ    TXTF15                                                           
         LA    R1,DETBOOKS                                                      
TXTF15   EQU   *                                                                
         CLC   1(2,R1),RINVFYR-RINVFEL(R6)                                      
         BNE   TXTFBAD                                                          
*                                                                               
TXTF20   EQU   *                                                                
         LA    R1,CBOOKS                BOOK TYPE                               
         OC    DETBOOKS(3),DETBOOKS                                             
         BZ    TXTF25                                                           
         LA    R1,DETBOOKS                                                      
TXTF25   EQU   *                                                                
         CLI   RINVFBKT-RINVFEL(R6),0   IF NO BOOK TYPE FILTER THEN             
         BNE   TXTF26                    MAKE SURE WE DON'T HAVE                
*****    TM    0(R1),X'26'               A PROJECTION/ESTIMATE/SPC SUR          
*****    BNZ   TXTFBAD                                                          
         B     TXTF30                                                           
TXTF26   EQU   *                                                                
         MVC   DMCB(1),0(R1)                                                    
         NI    DMCB,X'7F'               SUPPRESS CPM BIT                        
         CLC   DMCB(1),RINVFBKT-RINVFEL(R6)                                     
         BNE   TXTFBAD                                                          
*                                                                               
TXTF30   EQU   *                                                                
         CLI   RINVFLOC-RINVFEL(R6),0   LOCAL TEXT ONLY                         
         BE    TXTF40                                                           
TXTF40   EQU   *                                                                
         ZIC   R1,RINVFLEN-RINVFEL(R6)  CHECK FOR DEMO FILTERS                  
         SH    R1,=H'10'                                                        
         BZ    TXTFGOOD            NO DEMOS TO CHECK                            
         ZICM  R2,CNUMDEM,1        NUMBER OF DEMO ON THIS LINE                  
         BZ    TXTFBAD             NO DEMOS, CAN'T USE A FILTERED TXT           
         LA    R3,CDEMOS                                                        
TXTF42   EQU   *                                                                
         LA    R4,RINVFDEM                                                      
         LR    R5,R1                                                            
TXTF44   EQU   *                                                                
         CLC   2(1,R3),0(R4)                                                    
         BE    TXTFGOOD                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,TXTF44                                                        
         LA    R3,3(R3)                                                         
         BCT   R2,TXTF42                                                        
         B     TXTFBAD                                                          
*                                                                               
*        TEXT FILTER ROUTINE EXIT                                               
*                                                                               
TXTFGOOD EQU   *                                                                
         LA    R0,0                                                             
TXTFEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         B     XIT                                                              
TXTFBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TXTFEXIT                                                         
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO ENSURE TEXTS WILL FIT                      *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
COUNTEXT NTR1                                                                   
         LA    R3,1                                                             
         BAS   RE,GETEL                                                         
         B     CT40                                                             
         SPACE 2                                                                
CT20     BAS   RE,NEXTEL                                                        
         SPACE 2                                                                
CT40     BNE   CT60                COUNT TEXT ELEMENTS                          
         LA    R3,1(R3)            IN R3                                        
         B     CT20                                                             
         SPACE 2                                                                
CT60     STC   R3,ALLOWLIN                                                      
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE EXTRACTS DETAILS FROM THE PROPOSAL HEADER RECORD.     *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
EXTHDR   NTR1                                                                   
         SPACE 1                                                                
         L     R6,AIO1             HEADER RECORD                                
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RAVLELEM,R6                                                      
         MVC   CDEMOS,RAVLDEM                                                   
         LA    R2,CDEMOS+21                                                     
         LA    R3,8                                                             
         SPACE 2                                                                
EH10     OC    0(3,R2),0(R2)       COUNT DEMOS                                  
         BNZ   EH20                                                             
         SH    R2,=H'3'                                                         
         BCT   R3,EH10                                                          
         SPACE 1                                                                
EH20     STC   R3,CNUMDEM                                                       
         SPACE 1                                                                
         MVC   CBOOKS,RAVLBKS                                                   
         MVI   CNUMBKS,0                                                        
         OC    CBOOKS(3),CBOOKS    ZERO OR                                      
         BZ    *+8                                                              
         MVI   CNUMBKS,1           1 BOOK ALLOWED FOR PROPOSALS                 
         SPACE 2                                                                
         MVC   CSOURCE,RAVLSRC                                                  
         MVC   WEEKS,RAVLWKS                                                    
         MVC   PKGCOST,RAVLCOST                                                 
         SPACE 1                                                                
         OC    RAVLDATO,RAVLDATO                                                
         BZ    EH30                                                             
         GOTO1 DATCON,DMCB,(3,RAVLDATO),(2,PRPDATES)                            
         GOTO1 (RF),(R1),(3,RAVLDATO+3),(2,PRPDATES+2)                          
         B     EH40                                                             
         SPACE 1                                                                
EH30     GOTO1 DATCON,DMCB,(3,CCONDAT),(2,PRPDATES)                             
         GOTO1 (RF),(R1),(3,CCONDAT+3),(2,PRPDATES+2)                           
         SPACE 1                                                                
EH40     MVC   HDRLEN,RAVLPLEN                                                  
         MVC   HCPP,RAVLCPP                                                     
         SPACE 1                                                                
         MVC   COMMENT,SPACES                                                   
         L     R6,AIO1                                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL            LOOK FOR COMMENTS                            
         BNE   EH60                                                             
         USING RAVLCEL,R6                                                       
         ZIC   R3,RAVLCLEN                                                      
         SH    R3,=H'3'                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   COMMENT(0),RAVLCOMM                                              
         BAS   RE,NEXTEL                                                        
         BNE   EH60                                                             
         ZIC   R3,RAVLCLEN                                                      
         SH    R3,=H'3'                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   COMMENT+60(0),RAVLCOMM                                           
         SPACE 1                                                                
EH60     XC    HDRPJ,HDRPJ                                                      
         L     R6,AIO1                                                          
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL            LOOK FOR HEADER UPGRADE ELEMENT              
         BNE   EH70                                                             
         MVC   HDRPJ,0(R6)         SAVE ELEMENT                                 
         SPACE 1                                                                
EH70     XC    HDR0B,HDR0B                                                      
         L     R6,AIO1                                                          
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL            LOOK FOR HDR BOOK LABELING ELEMENT           
         BNE   EH80                                                             
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HDR0B(0),2(R6)         SAVE LABEL                                
         SPACE 1                                                                
EH80     L     R6,AIO1                                                          
         MVI   ELCODE,X'0D'                                                     
         BAS   RE,GETEL            LOOK FOR HEADER SID ELEMENT                  
         BNE   XIT                                                              
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE EXTRACTS DETAILS FROM A DETAIL RECORD.                *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
EXTDET   NTR1                                                                   
         SPACE 1                                                                
** FIRST, PUT HEADER SELECTIONS (IF ANY) INTO DETAIL FIELDS                     
*  THEN, OVERRIDE THEM WITH DETAIL SELECTIONS (IF ANY)                          
         SPACE 1                                                                
         MVC   DETLEN,HDRLEN                                                    
         MVC   DETPJ,HDRPJ                                                      
         MVC   DET0B,HDR0B                                                      
*                                                                               
         XC    ACTDEMOS,ACTDEMOS                                                
         L     R6,AIO1             DETAIL RECORD                                
         USING RPRPKEY,R6                                                       
         MVC   XDETNUM,RPRPKDET    SAVE DETAIL NUMBER                           
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RPRPDEL,R6                                                       
         MVC   INVNO,RPRPDINV                                                   
         MVC   INVDT,RPRPDATE                                                   
         MVC   RATE,RPRPDRTE                                                    
         CLI   RPRPDSEC,0                                                       
         BE    *+10                                                             
         MVC   DETLEN,RPRPDSEC                                                  
         MVC   INVSAT,RPRPDSAT                                                  
         MVC   DNUM,RPRPDNUM                                                    
         MVC   DNC,RPRPDNC                                                      
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL            LOOK FOR DETAIL UPGRADE ELEMENT              
         BNE   ED20                                                             
         USING RAVLNEL,R6                                                       
         OC    RAVLNBKS(12),RAVLNBKS  DID THEY LEAVE THE FIELD BLANK?           
         BZ    ED20                YES -- USE DEFAULT                           
         CLC   =C'NONE',RAVLNOPS   DO THEY WANT TO IGNORE THE DEFAULT?          
         BNE   ED15                NO                                           
         XC    DETPJ,DETPJ         YES -- CLEAR THE UPGRADE FIELD               
         XC    DET0B,DET0B                                                      
         B     ED20                                                             
ED15     EQU   *                                                                
         MVC   DETPJ,RAVLNEL       USE THE HEADER UPGRADE                       
         DROP  R6                                                               
*                                                                               
ED20     EQU   *                                                                
         L     R6,AIO1                                                          
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL            LOOK FOR DET BOOK LABELING ELEMENT           
         BNE   ED25                                                             
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DET0B(0),2(R6)         SAVE LABEL                                
         B     ED30                                                             
ED25     EQU   *                                                                
         L     R6,AIO1                                                          
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   ED30                                                             
ED26     EQU   *                                                                
         CLI   2(R6),7                BOOK OVERRIDE                             
         BNE   ED27                                                             
         XC    DET0B,DET0B                                                      
         B     ED30                                                             
ED27     EQU   *                                                                
         BAS   RE,NEXTEL                                                        
         BE    ED26                                                             
*                                                                               
ED30     EQU   *                                                                
         L     R6,AIO1                                                          
         MVI   ELCODE,X'0D'                                                     
         BAS   RE,GETEL            LOOK FOR DETAIL SID ELEMENT                  
         BNE   XIT                                                              
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE EXTRACTS DETAILS FROM A PACKAGE HEADER RECORD.        *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
EXTPKG   NTR1                                                                   
         SPACE 1                                                                
         L     R6,AIO1             PKG HEADER                                   
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RPRPPEL,R6                                                       
         MVC   PKGTSPT,RPRPPSPT                                                 
         ZICM  R0,PKGTSPT,1                                                     
         BNZ   XIT                                                              
         MVI   PKGTSPT,1                                                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO CHECK FOR OVERRIDES                        *          
*--------------------------------------------------------------------*          
         SPACE 3                                                                
ANYOVER  NTR1                                                                   
         XC    DETDEMOS,DETDEMOS   PRESET                                       
         XC    DETBOOKS,DETBOOKS                                                
         L     R6,AIO1             PROPOSAL DETAIL RECORD                       
         MVI   ELCODE,X'06'        LOOK FOR OVERRIDE ELEMENTS                   
         BAS   RE,GETEL                                                         
         B     AO20                                                             
         SPACE 2                                                                
AO10     BAS   RE,NEXTEL                                                        
         SPACE 2                                                                
AO20     BNE   XIT                                                              
         USING RAVLOEL,R6                                                       
         ZIC   R5,RAVLOLEN                                                      
         MVC   WORK,SPACES                                                      
         SH    R5,=H'4'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),RAVLODTA    DIG OUT VARIABLE DATA                        
         LA    R5,1(R5)                                                         
         ZIC   R2,RAVLOTYP                                                      
         BCTR  R2,0                                                             
         SLL   R2,2                AS A RESULT OF OVERRIDE TYPE,                
         B     BRANCH(R2)          GO TO APPROPRIATE ROUTINE                    
         SPACE 2                                                                
BRANCH   B     AO50                DAY                                          
         B     AO60                TIME                                         
         B     AO70                PROGRAM TITLE                                
         B     AO80                DEMOS                                        
         B     AO10                CODES - NOT USED                             
         B     AO10                CPM - NOT USED                               
         B     AO100               BOOK                                         
         EJECT                                                                  
*              SPECIFIC OVERRIDE ROUTINES                                       
         SPACE 2                                                                
AO50     MVC   DYTM(16),WORK       DAY                                          
         B     AO10                                                             
         SPACE 1                                                                
AO60     MVC   DYTM+16(16),WORK    TIME                                         
         B     AO10                                                             
         SPACE 1                                                                
*                                  PROGRAM                                      
AO70     GOTO1 CHOPPER,DMCB,(48,WORK),(27,P+17),(C'P',3)                        
         B     AO10                                                             
         SPACE 1                                                                
AO80     CLI   CNUMDEM,0           DEMOS                                        
         BE    AO10                IF NO REAL DEMOS, SKIP OVERRIDES             
         LA    R3,BLOCK                                                         
         MVC   0(80,R3),SPACES                                                  
         MVC   0(48,R3),WORK                                                    
         LA    R4,80(R3)                                                        
         XC    0(256,R4),0(R4)                                                  
         GOTO1 SCANNER,DMCB,(C'C',(R3)),(8,(R4))                                
         ZICM  R5,DMCB+4,1                                                      
         BZ    AO10                                                             
         LA    R1,CDEMOS                                                        
         LA    R3,DETDEMOS                                                      
         SPACE 1                                                                
AO90     EQU   *                                                                
         ICM   R0,15,4(R4)                                                      
         BNZ   AO91                                                             
         CLI   12(R4),C'0'                                                      
         BNE   AO91                                                             
         L     R0,=F'-1'                                                        
         B     AO92                                                             
AO91     EQU   *                                                                
         CLI   PRTOPL,C'Y'         OVERRIDES W/ WHOLE NUMBER?                   
         BE    AO92                YUP,                                         
         MH    R0,=H'10'            ELSE * 10 TO MATCH 1 DECIMAL                
AO92     EQU   *                                                                
         ST    R0,0(R3)                                                         
         LA    R1,3(R1)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,32(R4)                                                        
         BCT   R5,AO90                                                          
         SPACE 1                                                                
         B     AO10                                                             
         SPACE 1                                                                
AO100    XC    DETBOOKS,DETBOOKS   BOOKS - ONLY 1 BOOK FOR PROPOSALS            
         ZIC   R2,CSOURCE                                                       
         SPACE 1                                                                
         XC    WORK(8),WORK        BUILD A PHONY SCREEN HEADER                  
         ZIC   RE,RAVLOLEN                                                      
         SH    RE,=H'3'                                                         
         STC   RE,WORK+5           LENGTH OF INPUT                              
         BCTR  RE,0                LESS 1 FOR EXECUTE                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),RAVLODTA                                               
         OC    WORK+8(20),SPACES                                                
         SPACE 1                                                                
         GOTO1 BOOKVAL,DMCB,((R2),WORK),(1,DUB),SCANNER                         
         CLI   DMCB+4,1                                                         
         BNE   AO10                                                             
         MVC   DETBOOKS(3),DUB                                                  
         B     AO10                                                             
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO GET DAY/TIME/PROGRAMMING FROM INV HEADER              
*                                                                               
* DYTM IS SET UP AS - DYTM(16)   = DAY                                          
*                     DYTM+16(16)= TIME                                         
*                     DYTM+32(8) = PROPOSAL DETAIL NUMBER                       
*                     DYTM+40(8) = INVENTORY NUMBER                             
*                                                                               
* ANY DAY AND/OR TIME OVERRIDES GO INTO DYTM ALSO (SEE ANYOVER), AND            
* THEN IT IS ALL SQUASHED AND CHOPPED INTO P & P2 (SEE DAYTIME).                
*--------------------------------------------------------------------*          
         SPACE 2                                                                
INVDTP   NTR1                                                                   
         L     R6,AIO2             INVENTORY RECORD                             
         USING RINVKEY,R6                                                       
         MVC   DYTM+32(16),SPACES                                               
         CLI   OPTJ,C'Y'           Y=PRINT TEXT AND INV. NO.                    
         BNE   ID10                                                             
         MVC   DYTM+32(2),=C'P='                                                
         EDIT  (1,XDETNUM),(3,DYTM+34),ALIGN=LEFT                               
         MVC   DYTM+40(2),=C'I='                                                
         EDIT  (1,RINVKQTR),(2,DYTM+42),FILL=0                                  
         MVC   DYTM+44(2),RINVKDAY                                              
         CLI   DYTM+45,C'0'                                                     
         BNE   *+8                                                              
         MVI   DYTM+45,C' '                                                     
         DROP  R6                                                               
         SPACE 1                                                                
ID10     MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RINVPEL,R6                                                       
         MVC   DYTM(32),SPACES               DAY/TIME                           
         OC    RINVPADY,RINVPADY   IS THERE AVAIL DAY OVERRIDE                  
         BZ    ID20                                                             
         GOTO1 UNDAY,DMCB,RINVPADY,DYTM      YES, USE IT                        
         B     ID30                                                             
ID20     GOTO1 UNDAY,DMCB,RINVPDAY,DYTM                                         
         SPACE 1                                                                
ID30     OC    RINVPATM,RINVPATM   IS THERE AVAIL TIME OVERRIDE                 
         BZ    ID40                                                             
         GOTO1 UNTIME,DMCB,RINVPATM,DYTM+16     YES, USE IT                     
         B     ID50                                                             
ID40     GOTO1 UNTIME,DMCB,RINVPTIM,DYTM+16                                     
         SPACE 1                                                                
ID50     OC    DYTM,SPACES                                                      
         SPACE 2                                                                
         ZIC   R5,RINVPLEN                   PROGRAM                            
         SH    R5,=H'40'                                                        
         GOTO1 CHOPPER,DMCB,((R5),RINVPROG),(27,P+17),(C'P',3)                  
         SPACE 2                                                                
         MVC   WORK(15),=C'(ENDS MMMDD/YY)'                                     
         OC    RINVPEFF+2(2),RINVPEFF+2      IF INVENTORY ENDS BEFORE           
         BZ    ID60                             START OF PROPOSAL               
         CLC   RINVPEFF+2(2),PRPDATES           IT DOESN'T QUALIFY              
         BL    IDNO                                                             
         SPACE 2                                                                
ID60     OC    RINVPEFF(2),RINVPEFF          IF INVENTORY STARTS AFTER          
         BZ    ID70                             END OF PROPOSAL                 
         CLC   RINVPEFF(2),PRPDATES+2           IT DOESN'T QUALIFY              
         BH    IDNO                                                             
         SPACE 2                                                                
ID70     CLC   RINVPEFF(2),RINVPEFF+2        IF START=END ITS AN ONLY           
         BE    ID80                                                             
         CLC   RINVPEFF(2),PRPDATES          IF INVENTORY STARTS AFTER          
         BH    ID90                             PROPOSAL, ITS A FROM            
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    XIT                                                              
         CLC   RINVPEFF+2(2),PRPDATES+2      IF INVENTORY ENDS BEFORE           
         BNL   XIT                              PROPOSAL, ITS AN ENDS           
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(8,WORK+6)                            
         B     ID100                                                            
         SPACE 2                                                                
ID80     MVC   WORK+9(5),=C' ONLY'                                              
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(8,WORK+1)                              
         B     ID100                                                            
         SPACE 2                                                                
ID90     MVC   WORK+1(4),=C'EFF.'                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(8,WORK+6)                              
         SPACE 2                                                                
ID100    MVC   P3(15),WORK                                                      
         B     XIT                                                              
         SPACE 2                                                                
IDNO     MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE COMPRESSES THE ORIGINAL OR OVERRIDE DAY/TIME EXPRESSION          
* AND THE AVAIL DETAIL NUMBER AND THE INVENTORY NUMBER ( IF OPTJ=Y)             
* INTO P (AND P2 IF NECESSARY), EACH 16 CHARACTERS LONG.                        
*--------------------------------------------------------------------*          
         SPACE 2                                                                
DAYTIME  NTR1                                                                   
         OC    DYTM,SPACES                                                      
         GOTO1 SQUASHER,DMCB,DYTM,48                                            
         GOTO1 CHOPPER,DMCB,(48,DYTM),(16,P),(C'P',2)                           
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO HANDLE TOTALS FOR PACKAGE                             
*--------------------------------------------------------------------*          
         SPACE 3                                                                
PACKTOT  NTR1                                                                   
         OC    PKGAC,PKGAC                                                      
         BZ    PTX90                                                            
         CLI   PKGCODE+1,X'FF'                                                  
         BNE   PT05                                                             
         GOTO1 =A(ADJEM),DMCB,(RC),RR=RELO   ADJUST ACCUMULATORS                
PT05     EQU   *                                                                
*                                                                               
         CLI   DNC,C'T'                                                         
         BNE   PT05A                                                            
         ZIC   RF,PKGTSPT                                                       
         ST    RF,PKGAC+0                                                       
PT05A    EQU   *                                                                
         CLI   OPTF,C'Y'           Y=PRINT SUBTOTALS PER PACKAGE                
         BNE   PTX90                                                            
*                                                                               
         MVI   ALLOWLIN,4                                                       
         LA    R2,PKGAC                                                         
         BAS   RE,CHICKEN                                                       
         ICM   RF,15,0(R2)                                                      
         BNZ   PT07                                                             
         CLI   DNC,C'T'                                                         
         BNE   PT07                                                             
         ZIC   RF,PKGTSPT                                                       
         ST    RF,0(R2)                                                         
PT07     EQU   *                                                                
         MVC   P+17(18),=C'NON-PACKAGE TOTALS'                                  
         CLI   PKGCODE+1,X'FF'                                                  
         BE    PT20                                                             
         CLI   PKGCODE+1,C'T'      FOR T PACKAGES                               
         BNE   PT10                                                             
PT10     MVC   P+17(18),=C'PACKAGE A TOTALS  '                                  
         MVC   P+25(1),PKGCODE                                                  
         SPACE 1                                                                
PT20     MVC   OPTION,OPTG         Y=SHOW RATES                                 
         SPACE 1                                                                
         MVC   P+45(5),=C'TOTAL'                                                
         SR    R5,R5               DON'T SHOW SPOT LEN                          
         GOTO1 AFORMDEM,DMCB,(RC)                                               
         BAS   RE,FORMSPOT                                                      
         BAS   RE,SPLAT                                                         
         SPACE 2                                                                
PTX90    EQU   *                                                                
         GOTO1 =A(PKTOPR),DMCB,(RC),RR=RELO  ADD PKG ACCUMS TO PROPOSAL         
         XC    PKGAC,PKGAC                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE PROPOSAL TOTALS                                
         SPACE 3                                                                
PROPTOT  NTR1                                                                   
         OC    PKGCOST,PKGCOST     USE HEADER COST IF NOT ZERO                  
         BZ    PTOT10              ELSE USE ACCUM TOTAL FROM DETAILS            
         MVC   PROPAC+4(4),PKGCOST                                              
         SPACE 2                                                                
PTOT10   DS    0H                                                               
         CLI   OPTC,C'Y'           SUPPRESS TOTALS?                             
         BNE   PTOT15              NO                                           
         CLI   OPTD,C'Y'           SUPPRESS AVE. DEMO DELIVERY?                 
         BNE   PTOT15              NO                                           
         CLI   OPTE,C'Y'           SUPPRESS TOTAL CPP?                          
         BE    XIT                 YES -- NOTHING TO PRINT                      
         SPACE 2                                                                
PTOT15   MVI   ALLOWLIN,6                                                       
         LA    R2,PROPAC                                                        
         BAS   RE,CHICKEN                                                       
         MVC   P+17(19),=C'TOTALS FOR PROPOSAL'                                 
         ICM   RF,15,0(R2)                                                      
         BNZ   PTOT17                                                           
         CLI   DNC,C'T'                                                         
         BNE   PTOT17                                                           
         ZIC   RF,PKGTSPT                                                       
         ST    RF,0(R2)                                                         
PTOT17   EQU   *                                                                
         MVI   OPTION,C'Y'                                                      
         CLI   OPTC,C'Y'           Y=SUPPRESS PROPOSAL TOTALS                   
         BE    PTOT20                                                           
         SPACE 2                                                                
         SR    R5,R5               DON'T SHOW SPOT LEN                          
         GOTO1 AFORMDEM,DMCB,(RC)                                               
         BAS   RE,FORMSPOT                                                      
         MVC   P+45(8),=C'PROPOSAL'                                             
         CLI   OPTG,C'Y'                                                        
         BE    PTOT18                                                           
         OC    4(4,R2),4(R2)       IS THERE A TOTAL COST?                       
         BZ    PTOT18              NO, SKIP LABEL                               
         LA    R5,P+63                                                          
         ZIC   RF,CNUMDEM                                                       
         MH    RF,=H'7'                                                         
         AR    R5,RF                                                            
         MVC   0(4,R5),=C'COST'                                                 
PTOT18   EQU   *                                                                
         BAS   RE,SPLAT                                                         
         SPACE 2                                                                
PTOT20   CLI   OPTD,C'Y'           Y=SUPPRESS SPOT AVERAGE                      
         BE    PTOT30                                                           
         GOTO1 =A(SPOTAVE),DMCB,(RC),RR=RELO                                    
         BAS   RE,SPLAT                                                         
         SPACE 2                                                                
PTOT30   CLI   OPTE,C'Y'           Y=SUPPRESS CPMS                              
         BE    PTOT40                                                           
         SPACE 2                                                                
         MVI   DETAC,1             THIS FORCES TOTAL CPMS TO COME OUT           
         GOTO1 AFORMCPM,DMCB,(RC)                                               
         MVC   P+45(7),=C'CPP/CPM'                                              
         BAS   RE,SPLAT                                                         
         SPACE 2                                                                
PTOT40   BAS   RE,CHICKEN                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL PRINTING                                      
         SPACE 3                                                                
SPLAT    NTR1                                                                   
         MVC   DMWORK(44),P2       SAVE LEFT HAND DATA                          
         MVC   DMWORK+44(44),P3                                                 
         MVC   P2(44),SPACES                                                    
         MVC   P3(44),SPACES                                                    
         MVI   SPACING,1           ALWAYS PRINT A SINGLE LINE                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(44),DMWORK        AND RESTORE LEFT HAND A LINE UP              
         MVC   P2(44),DMWORK+44                                                 
         ZICM  R2,MINLINES,1                                                    
         BZ    XIT                                                              
         BCTR  R2,0                                                             
         STC   R2,MINLINES                                                      
         B     XIT                                                              
         SPACE 2                                                                
HOWMANY  NTR1                                                                   
         LA    R2,P                SET UP MINIMUM TO PRINT                      
         LA    R3,6                                                             
         SR    R4,R4                                                            
         SPACE 2                                                                
HOWMANY2 CLC   0(44,R2),SPACES     CHECK DATA ON LEFT                           
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         LA    R2,132(R2)                                                       
         BCT   R3,HOWMANY2                                                      
         STC   R4,MINLINES         STORE IN MINLINES                            
         STC   R4,ALLOWLIN                                                      
         B     XIT                                                              
         SPACE 2                                                                
ANYLEFT  NTR1                                                                   
         ZICM  R2,MINLINES,1       ENSURE ENOUGH LINES PRINT                    
         BZ    XIT                                                              
         BAS   RE,SPLAT                                                         
         BCT   R2,*-4                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL FORMATTING DETAIL LINES                       
*                  (SPOTS, DEMO VALUES, AND COSTS)                              
*                                                                               
* ACCUMS ARE SET UP AS +0(4) = # OF SPOTS                                       
*                      +4(4) = RATE                                             
*                      +8(4) = DEMO DELIVERY                                    
         SPACE 3                                                                
FORMAT   NTR1                                                                   
         LA    R2,DETAC+8                                                       
         LA    R3,ACTDEMOS                                                      
         OC    DETDEMOS,DETDEMOS                                                
         BZ    *+8                                                              
         MVI   P+52,C'*'                                                        
         LA    R4,DETDEMOS                                                      
         LA    R5,CDEMOS                                                        
         ZIC   R6,CNUMDEM                                                       
         CLI   CNUMDEM,0           MAY BE NO DEMOS                              
         BE    FM50                                                             
         SPACE 2                                                                
FM20     MVC   0(4,R2),0(R3)       PUT DEMOS INTO LINE 1                        
         OC    0(4,R4),0(R4)                                                    
         BZ    *+10                                                             
         MVC   0(4,R2),0(R4)                                                    
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,3(R5)                                                         
         BCT   R6,FM20                                                          
         SPACE 1                                                                
FM50     LA    R2,DETAC                                                         
         MVC   4(4,R2),RATE                                                     
         CLI   DNC,C'T'                                                         
         BE    FM90                                                             
         ZIC   R3,DNUM                                                          
         CLI   DNUM,99             MORE THAN 99 SPOTS?                          
         BH    FM52                YES                                          
         MVC   P+55(5),=C'(NNW)'                                                
         EDIT  (R3),(2,P+56)                                                    
         CLI   P+56,C' '                                                        
         BNE   FM55                                                             
         MVC   P+55(2),=C' ('                                                   
         B     FM55                                                             
FM52     EQU   *                                                                
         MVC   P+54(6),=C'(NNNW)'  EXTEND ONE CHAR LEFT                         
         EDIT  (R3),(3,P+55)       EDIT LARGER NUMBER                           
FM55     EQU   *                                                                
         MVC   P+58(1),DNC         X OR W                                       
         CLI   DNC,C'W'                                                         
         BE    FM60                                                             
         ST    R3,0(R2)                                                         
         B     FM70                                                             
         SPACE 1                                                                
FM60     ZIC   R5,WEEKS            ASSUME PROPOSAL WEEKS                        
         MR    R4,R3                                                            
         ST    R5,0(R2)            R5=TOTAL SPOTS                               
FM70     MVC   OPTION,OPTG         Y=SHOW RATES                                 
         LA    R5,1                SHOW SPOT LEN                                
         GOTO1 AFORMDEM,DMCB,(RC)                                               
         SPACE 1                                                                
         BAS   RE,SPLAT            PRINT (SPOTS) DEMOS (RATE)                   
         CLI   OPTH,C'Y'           Y=PRINT SPOT CPP FOR 'W' LINES               
         BNE   FM80                                                             
         GOTO1 AFORMCPM,DMCB,(RC)                                               
         BNZ   FM80                                                             
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
FM80     GOTO1 AADDEM,DMCB,(RC)                                                 
         B     XIT                                                              
         SPACE 2                                                                
FM90     EQU   *                                                                
         MVI   DETAC+3,1                                                        
         MVC   OPTION,OPTG         Y=SHOW RATES                                 
         LA    R5,1                SHOW SPOT LEN                                
         GOTO1 AFORMDEM,DMCB,(RC)                                               
         SPACE 1                                                                
         BAS   RE,SPLAT                                                         
         GOTO1 AADDEM,DMCB,(RC)                                                 
         MVI   DETAC+3,0                                                        
         B     XIT                                                              
         EJECT                                                                  
ANYUP    NTR1                                                                   
         MVI   UPPRINT,C'N'                                                     
         LA    R6,DETPJ            LOOK FOR UPGRADE ELEMENT                     
         OC    0(14,R6),0(R6)      (DETAIL OVERRIDES HEADER)                    
         BZ    XIT         ***AVAILS LOOKS FOR UPGRADE ON INVENTORY***          
*                                  DO WE NEED TO DO THIS ALSO?????****          
         SPACE 1                                                                
         MVC   P+51(2),=C'* '    DON'T SHOW PROGRAM CONDE ON UPGRADE            
*                                  FROM PRP AND APPLY THIS IF FOUND             
         GOTO1 DEMUP,DMCB,(R4),(R6),ACOMFACS                                    
         MVI   UPPRINT,C'Y'                                                     
         SPACE 1                                                                
         USING RAVLNEL,R6                                                       
         CLI   RAVLNTYP,3          HUT/PUT                                      
         BE    AU10                                                             
         CLI   RAVLNTYP,6                                                       
         BNE   XIT                                                              
         SPACE 1                                                                
AU10     OC    RAVLNOP1,RAVLNOP1                                                
         BZ    XIT                                                              
         OC    RAVLNOP2,RAVLNOP2                                                
         BNZ   XIT                                                              
         MVC   RAVLNOP2,CBOOKS+1   ADJUST UP ELEMENT TO SHOW FROM BOOK          
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 3                                                                
*              SEE IF UPGRADE CAUSES ANOTHER BOOK TO BE READ                    
         SPACE 3                                                                
ANYBK    NTR1                                                                   
         LA    R6,DETPJ            LOOK FOR UPGRADE ELEMENT                     
         OC    0(14,R6),0(R6)      (DETAIL OVERRIDES HEADER)                    
         BZ    XIT                                                              
         SPACE 1                                                                
         USING RAVLNEL,R6                                                       
         CLI   RAVLNTYP,3          INTERESTED IN HUT/PUT                        
         BNE   XIT                                                              
         CLI   RAVLNOP2,70         WHOSE 2'ND OPERAND IS A BOOK                 
         BL    XIT                                                              
         USING RINVKEY,R4                                                       
         MVC   RINVKBK,RAVLNOP2                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT CHICKEN TRACKS                                  
         SPACE 3                                                                
CHICKEN  NTR1                                                                   
         MVI   P+45,C'-'                                                        
         ZIC   R3,CNUMDEM                                                       
         MH    R3,=H'7'                                                         
         LA    R3,29(R3)                                                        
         CLI   OPTG,C'Y'           Y=PRINT RATES                                
         BE    CHICK10                                                          
         OC    4(4,R2),4(R2)                                                    
         BNZ   CHICK10                                                          
         SH    R3,=H'15'                                                        
CHICK10  EQU   *                                                                
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+46(0),P+45                                                     
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO DIG OUT DEMO VALUES                                  
         SPACE 3                                                                
DIGDEM   NTR1                                                                   
         L     R4,AIO2             INVENTORY RECORD                             
         AH    R4,DATADISP                                                      
         BAS   RE,ANYUP                                                         
         LA    R5,BLOCK                                                         
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         L     R4,AIO2             INVENTORY RECORD                             
         ST    R4,DBAREC                                                        
         AH    R4,DATADISP                                                      
         ST    R4,DBAQUART                                                      
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         CLI   PRTOPL,C'Y'        'Y' MEANS WHOLE NUMBER                        
         BE    DD02A                                                            
         MVI   DBXTTRP,X'01'      RTG = 1 DECIMAL                               
         MVI   DBXTTSP,X'01'      SHR = 1 DECIMAL                               
         MVI   DBXTTIP,X'02'      IMP = 00'S                                    
         B     DD02B                                                            
DD02A    EQU   *                                                                
         MVI   DBXTTRP,X'00'      RTG = 0 DECIMAL                               
         MVI   DBXTTSP,X'00'      SHR = 0 DECIMAL                               
         MVI   DBXTTIP,X'03'      IMP = 000'S                                   
DD02B    EQU   *                                                                
         DROP  R1                                                               
*                                                                               
         LA    R2,CDEMOS                                                        
         LA    R3,ACTDEMOS                                                      
         ZIC   R1,CNUMDEM                                                       
         MH    R1,=H'3'                                                         
         LA    R1,0(R2,R1)                                                      
         MVI   0(R1),X'FF'                                                      
         CLI   CNUMDEM,0           MAY BE NO DEMOS                              
         BE    DD30                                                             
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCK,(R3)                              
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
         OC    DETPJ(14),DETPJ                                                  
         BNZ   DD05                                                             
         L     R6,AIO2                                                          
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   DD05                                                             
         MVC   DETPJ(14),0(R6)                                                  
DD05     EQU   *                                                                
*                                                                               
         ZIC   R0,CNUMDEM          NUMBER OF DEMOS FOR BCT LOOP                 
         L     R6,AIO2             INVENTORY RECORD                             
         MVI   ELCODE,X'CD'                                                     
         MVI   MULTSW,C'N'                                                      
         BAS   RE,GETEL                                                         
         BNE   DD10                                                             
         USING RINVCEL,R6                                                       
         TM    RINVCTYP,X'80'                                                   
         BNO   DD10                                                             
         MVI   MULTSW,C'Y'                                                      
         SPACE 2                                                                
DD10     CLI   MULTSW,C'Y'         IF MULTI-PROGRAM INVENTORY,                  
         BNE   DD20                                                             
         CLC   1(2,R2),=X'D701'    DONT SHOW HUT                                
         BNE   *+10                                                             
         XC    0(4,R3),0(R3)                                                    
         CLC   1(2,R2),=X'E201'    OR SHARE                                     
         BNE   *+10                                                             
         XC    0(4,R3),0(R3)                                                    
         CLC   1(2,R2),=X'C301'    OR CHARE                                     
         BNE   *+10                                                             
         XC    0(4,R3),0(R3)                                                    
         CLC   1(2,R2),=X'E701'    OR TSHARE                                    
         BNE   *+10                                                             
         XC    0(4,R3),0(R3)                                                    
         SPACE 2                                                                
DD20     LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,DD10                                                          
         SPACE 2                                                                
DD30     MVC   ACTCODE,SPACES                                                   
         L     R6,AIO2             INVENTORY RECORD                             
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING RINVCEL,R6                                                       
         MVC   ACTCODE,RINVCODE                                                 
         OC    RINVCTXT,RINVCTXT                                                
         BZ    *+10                                                             
         MVC   COMPTEXT,RINVCTXT                                                
         B     XIT                                                              
         EJECT                                                                  
FORMSPOT NTR1                                                                   
         OC    0(4,R2),0(R2)                                                    
         BZ    XIT                                                              
         EDIT  (4,0(R2)),(4,P+54)                                               
         B     XIT                                                              
         EJECT                                                                  
HEDSPECS DS    0H                                                               
         SPACE 1                                                                
         SPROG 1                                                                
         SSPEC H7,95,PAGE                                                       
         SSPEC H13,1,C'DAY/TIME'                                                
         SSPEC H14,1,C'--------'                                                
         SSPEC H13,18,C'PROGRAM/ADJACENCY'                                      
         SSPEC H14,18,C'-----------------'                                      
         SSPEC H13,46,C'BOOK CODE SPOTS'                                        
         SSPEC H14,46,C'---- ---- -----'                                        
         DS    X'00'                                                            
         SPACE 3                                                                
HOOK     NTR1                                                                   
*                                                                               
         L     RB,SAVERB                                                        
         L     R7,SAVER7                                                        
*                                                                               
         CLI   RCSUBPRG,0                                                       
         BE    HOOKX                                                            
         MVC   H1+28(L'ESALNAME),ESALNAME     SALESPERSON NAME                  
         MVC   H2+28(L'EOFFADD1),EOFFADD1     OFFICE ADDRESS                    
         MVC   H3+28(L'EOFFADD2),EOFFADD2                                       
         MVC   H3+48(L'EOFFSTT),EOFFSTT                                         
         GOTO1 SQUASHER,DMCB,H3+28,22                                           
         MVC   H4+28(L'EOFFZIP),EOFFZIP                                         
         MVC   H5+28(L'ESALTEL),ESALTEL       SALESPERSON TELEPHONE             
         SPACE 1                                                                
         MVC   H1+73(L'ECONBUYR),ECONBUYR     BUYER NAME                        
         MVC   H2+73(L'EAGYNAM2),EAGYNAM2     AGENCY NAME                       
         MVC   H3+73(L'EAGYADD1),EAGYADD1     AND ADDRESS                       
         MVC   H4+73(L'EAGYADD2),EAGYADD2                                       
         MVC   H5+73(L'EAGYCITY),EAGYCITY                                       
         MVC   H5+95(L'EAGYSTAT),EAGYSTAT                                       
         GOTO1 SQUASHER,DMCB,H4+73,24                                           
         MVC   H6+73(L'EAGYZIP),EAGYZIP                                         
         SPACE 1                                                                
         MVC   H7(L'HDLN7),HDLN7                                                
         MVC   H8(L'HDLN8),HDLN8                                                
         SPACE 1                                                                
         MVC   H10(60),COMMENT                                                  
         MVC   H11(60),COMMENT+60                                               
         SPACE 1                                                                
         OC    PKGCOST,PKGCOST     IF NO COST, SKIP                             
         BZ    HK10                                                             
         MVC   H10+73(22),=C'TOTAL PROPOSAL PRICE $'                            
         EDIT  (4,PKGCOST),(7,H10+95),ALIGN=LEFT                                
         SPACE 1                                                                
HK10     MVC   H13+63(L'DEMHD1),DEMHD1     MAKE DEMO HEADS                      
         MVC   H14+63(L'DEMHD2),DEMHD2     SEEM CENTERED                        
HOOKX    B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
ERREND   GOTO1 MYERROR             DO A GETTXT CALL                             
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*-----------------------------------------------------------*                   
*        COVERSHT --- PRODUCE COVER SHEET                   *                   
*-----------------------------------------------------------*                   
COVERSHT NMOD1 0,*CVRSHT*                                                       
         L     RC,0(R1)                                                         
         SPACE 1                                                                
         LA    R3,20               R3 = NUMBER OF LINES TO SKIP                 
COVER10  MVI   P,0                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BCT   R3,COVER10                                                       
*                                                                               
         MVC   P+34(2),=C'* '      TOP ROW OF ASTERISKS                         
         MVC   P+36(32),P+34                                                    
         MVI   P+68,C'*'                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   WORK(33),SPACES                                                  
         GOTO1 DATCON,DMCB,(5,0),(8,WORK)                                       
         BAS   RE,FANCY                                                         
*                                                                               
         MVI   WORK,C'#'                                                        
         MVC   WORK+1(7),ECONNUM                                                
         MVI   WORK+8,C'-'                                                      
         MVC   WORK+10(3),EHDRNUM                                               
         GOTO1 SQUASHER,DMCB,WORK,12                                            
         BAS   RE,FANCY                                                         
*                                                                               
         MVC   WORK(L'EAGYNAM2),EAGYNAM2                                        
         BAS   RE,FANCY                                                         
*                                                                               
         MVC   WORK(L'ECONBUYR),ECONBUYR                                        
         BAS   RE,FANCY                                                         
*                                                                               
         MVC   WORK(L'EADVNAME),EADVNAME                                        
         BAS   RE,FANCY                                                         
*                                                                               
         MVC   WORK(L'EPRDNAME),EPRDNAME                                        
         BAS   RE,FANCY                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,PRPDATES),(8,WORK)                                
         MVI   WORK+9,C'-'                                                      
         GOTO1 DATCON,DMCB,(2,PRPDATES+2),(8,WORK+11)                           
         GOTO1 SQUASHER,DMCB,WORK,19                                            
         BAS   RE,FANCY                                                         
*                                                                               
         MVC   WORK(L'ESTATION),ESTATION                                        
         CLI   WORK+3,C' '                                                      
         BNE   COVER20                                                          
         MVC   WORK+3(3),=C'-TV'                                                
         B     COVER25                                                          
COVER20  EQU   *                                                                
         MVC   WORK+4(3),=C'-TV'                                                
COVER25  EQU   *                                                                
         BAS   RE,FANCY                                                         
*                                                                               
         MVC   WORK(L'EMKTNAME),EMKTNAME                                        
         BAS   RE,FANCY                                                         
*                                                                               
         MVC   WORK(L'ESALNAME),ESALNAME                                        
         BAS   RE,FANCY                                                         
*                                                                               
         MVC   P+34(2),=C'* '      BOTTOM ROW OF ASTERISKS                      
         MVC   P+36(32),P+34                                                    
         MVI   P+68,C'*'                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'Y'                                                    
         XIT1                                                                   
         SPACE 2                                                                
FANCY    NTR1                                                                   
         OC    WORK(33),SPACES                                                  
         GOTO1 CENTER,DMCB,WORK,33                                              
         MVI   P+34,C'*'                                                        
         MVC   P+36(33),WORK                                                    
         MVI   P+68,C'*'                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   WORK(33),SPACES                                                  
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE SETS UP THE H7 AND H8 AND THE DEMO HEADLINES          *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
SETHEADS NMOD1 0,**SETHD*                                                       
         L     RC,0(R1)                                                         
         SPACE 1                                                                
         MVC   HDLN7(L'HDLN7),SPACES                                            
         MVC   HDLN8(L'HDLN8),SPACES                                            
         MVC   DEMHD1(L'DEMHD1),SPACES                                          
         MVC   DEMHD2(L'DEMHD2),SPACES                                          
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(5,0),(8,HDLN7)   TODAY'S DATE                       
         MVI   HDLN8,C'#'                                                       
         MVC   HDLN8+1(L'ECONNUM),ECONNUM    CONTRACT NUMBER                    
         MVI   HDLN8+8,C'-'                                                     
         MVC   HDLN8+10(L'EHDRNUM),EHDRNUM   HEADER NUMBER                      
         GOTO1 SQUASHER,DMCB,HDLN8,12                                           
         SPACE 1                                                                
         MVC   HDLN7+17(L'EADVNAME),EADVNAME ADVERTISER                         
         MVC   HDLN8+17(L'EPRDNAME),EPRDNAME PRODUCT                            
         SPACE 1                                                                
*   PRPDATES IS CONTRACT DATES(DEFAULT) OR HEADER DATES (OVERRIDE)              
         GOTO1 DATCON,DMCB,(2,PRPDATES),(8,HDLN7+38)                            
         MVI   HDLN7+47,C'-'                                                    
         GOTO1 DATCON,DMCB,(2,PRPDATES+2),(8,HDLN7+49)                          
         MVC   HDLN7+58(10),=C'- NN WEEKS'                                      
         EDIT  (1,WEEKS),(2,HDLN7+60)                                           
         CLI   WEEKS,1                                                          
         BNE   *+8                                                              
         MVI   HDLN7+67,C' '                                                    
         GOTO1 SQUASHER,DMCB,HDLN7+38,32                                        
         SPACE 1                                                                
         MVC   HDLN8+38(3),=C'ARB' SERVICE                                      
         CLI   CSOURCE,C'A'                                                     
         BE    SH10                                                             
         MVC   HDLN8+38(3),=C'NSI'                                              
         CLI   CSOURCE,C'N'                                                     
         BE    SH10                                                             
         MVC   HDLN8+38(3),=C'SRC'                                              
         CLI   CSOURCE,C'S'                                                     
         BE    SH10                                                             
         MVC   HDLN8+38(3),SPACES                                               
SH10     DS    0H                                                               
         CLI   CNUMBKS,0           MAY BE NO BOOK                               
         BE    SH40                                                             
         LA    R3,HDLN8+42         BOOK                                         
         OC    HDR0B,HDR0B         ANY BOOK LABEL                               
         BZ    *+14                NO                                           
         MVC   0(L'HDR0B,R3),HDR0B YES -- USE IT INSTEAD OF BOOK                
         B     SH40                                                             
         SPACE 1                                                                
         LA    R2,CBOOKS                                                        
         L     RE,=A(SVCLST)                                                    
         A     RE,RELO                                                          
SH20     CLC   0(1,R2),3(RE)                                                    
         BE    SH30                                                             
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   SH20                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
SH30     CLI   1(RE),C' '                                                       
         BE    *+14                                                             
         MVC   0(1,R3),1(RE)       PRECEDE WITH P,T,S OR E                      
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
         SR    R1,R1                                                            
         IC    R1,CBOOKS+2         NOW DO MONTH                                 
         MH    R1,=H'3'                                                         
         LA    R1,MONTH(R1)                                                     
         MVC   0(3,R3),0(R1)                                                    
         LA    R3,3(R3)                                                         
         SPACE 1                                                                
         SR    R1,R1                                                            
         IC    R1,CBOOKS+1         AND YEAR                                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   0(2,R3),DUB+1                                                    
         SPACE 1                                                                
SH40     MVC   HDLN7+85(L'EMKTNAME),EMKTNAME    MARKET NAME                     
         MVC   HDLN8+85(L'ESTATION),ESTATION    STATION                         
         LA    R2,HDLN8+88                                                      
         CLI   0(R2),C' '                                                       
         BE    SH45                                                             
         LA    R2,1(R2)                                                         
SH45     EQU   *                                                                
         MVC   0(3,R2),=C'-TV'                                                  
         MVI   3(R2),C'/'                                                       
         MVC   4(3,R2),ESTAAFFL                 AFFILIATE                       
         CLC   ESTACHAN(2),=C'1 '                                               
         BE    SH46                                                             
         MVI   7(R2),C'/'                                                       
         MVC   8(4,R2),ESTACHAN                 CHANNEL                         
SH46     EQU   *                                                                
         SPACE 2                                                                
         LA    R5,BLOCK                                                         
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         LA    R2,CDEMOS                                                        
         ZIC   R4,CNUMDEM                                                       
         LA    R3,DEMHD1                                                        
         CLI   CNUMDEM,0           SKIP DEMOCON IF NO DEMOS                     
         BE    SH60                                                             
         SPACE 1                                                                
SH50     CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         GOTO1 DEMOCON,DMCB,(R2),(5,WORK),(0,DBLOCK)                            
         DROP  R5                                                               
         CLI   1(R2),C'I'                                                       
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
         SPACE 1                                                                
         MVC   0(5,R3),WORK                                                     
         MVC   66(5,R3),WORK+5                                                  
         CLC   66(5,R3),=C'(IMP)'                                               
         BNE   *+10                                                             
         MVC   66(5,R3),=C'(000)'                                               
         LA    R2,3(R2)                                                         
         LA    R3,7(R3)                                                         
         BCT   R4,SH50                                                          
         SPACE 1                                                                
SH60     EQU   *                                                                
         MVC   2(3,R3),=C'LEN'                                                  
         MVC   68(3,R3),=C'---'                                                 
         CLI   OPTG,C'Y'           Y=SHOW RATES                                 
         BNE   SHX                                                              
         MVC   10(4,R3),=C'COST'                                                
         MVC   76(4,R3),=C'----'                                                
         SPACE 1                                                                
SHX      XIT1                                                                   
         SPACE 2                                                                
MONTH    DC    C'ESTJANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                       
         EJECT                                                                  
*              ROUTINE TO FORMAT DEMO NUMBERS                                   
         SPACE 3                                                                
FORMDEMS NMOD1 0,**FRMDEM                                                       
         L     RC,0(R1)                                                         
*                                  R2=A(ACCUMS)                                 
         SPACE 2                                                                
         LR    R6,R2                                                            
         LA    R2,8(R2)                                                         
         LA    R3,P+61                                                          
         ZIC   R4,CNUMDEM                                                       
         CLI   CNUMDEM,0           MAY BE NO DEMOS                              
         BE    FD6                                                              
         SPACE 2                                                                
FD2      OC    0(4,R2),0(R2)                                                    
         BZ    FD4                                                              
         CLC   0(4,R2),=F'-1'                                                   
         BE    FD4                                                              
*                                                                               
         CLI   PRTOPL,C'Y'                                                      
         BE    FD3                                                              
         EDIT  (4,0(R2)),(7,0(R3)),1                                            
         B     FD4                                                              
FD3      EQU   *                                                                
         EDIT  (4,0(R2)),(7,0(R3))                                              
*                                                                               
FD4      EQU   *                                                                
         LA    R2,4(R2)                                                         
         LA    R3,7(R3)                                                         
         BCT   R4,FD2                                                           
         SPACE 1                                                                
FD6      EQU   *                                                                
         LA    R3,3(R3)                                                         
         LTR   R5,R5               CHCEK DON'T SHOW LEN FLAG                    
         BZ    FD7                                                              
         EDIT  (1,DETLEN),(3,0(R3))                                             
FD7      EQU   *                                                                
         CLI   OPTION,C'Y'         Y= SHOW RATES                                
         BNE   FDX                                                              
         EDIT  (4,4(R6)),(8,4(R3))                                              
FDX      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO OUTPUT CPMS                                           
         SPACE 3                                                                
FORMCPM  NMOD1 0,**FRMCPM                                                       
         L     RC,0(R1)                                                         
*                                                                               
         ICM   RF,15,DETAC                                                      
         BNZ   CPM1                                                             
         CLI   DNC,C'T'                                                         
         BNE   CPMBAD                                                           
         ZICM  RF,PKGTSPT,1                                                     
         BZ    CPMBAD                                                           
*                                                                               
CPM1     EQU   *                                                                
         MVC   FULL,4(R2)          TUCK AWAY SPOT COST                          
         ICM   R0,15,FULL                                                       
         BZ    CPMBAD                                                           
*                                                                               
         LA    R2,8(R2)                                                         
         LA    R3,P+62                                                          
         LA    R4,CDEMOS                                                        
         ZIC   R6,CNUMDEM                                                       
         CLI   CNUMDEM,0           MAY BE NO DEMOS                              
         BE    CPMBAD                                                           
*                                                                               
CPM2     EQU   *                                                                
         CLC   0(4,R2),=F'-1'                                                   
         BE    CPM6                                                             
         TM    0(R4),X'80'         SKIP IF CPM SUPPRESSION                      
         BO    CPM6                                                             
         CLI   1(R4),C'S'          OR IF SHARE                                  
         BE    CPM6                                                             
         CLI   1(R4),C'P'          OR IF HUT                                    
         BE    CPM6                                                             
         CLI   1(R4),C'C'                                                       
         BE    CPM6                                                             
         CLI   1(R4),C'X'          OR IF TSHARE                                 
         BE    CPM6                                                             
         OC    0(4,R2),0(R2)       NO DEMO                                      
         BZ    CPM6                                                             
         ICM   RF,15,FULL                                                       
         BZ    CPMBAD                                                           
         CLI   PRTOPL,C'Y'         DEMOS WHOLE NUMBER?                          
         BE    CPM3                YES                                          
         M     RE,=F'10'           NO, DO COST*10 TO COMPENSATE                 
CPM3     EQU   *                                                                
         M     RE,=F'200'                                                       
         D     RE,0(R2)                                                         
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'9999'         ALWAYS ROOM FOR $99.99                       
         BH    CPM4                                                             
         EDIT  (RF),(6,0(R3)),2,FLOAT=$                                         
         B     CPM6                                                             
         SPACE 2                                                                
CPM4     SR    RE,RE                                                            
         AH    RF,=H'50'                                                        
         D     RE,=F'100'                                                       
         C     RF,=F'99999'                                                     
         BH    CPM6                                                             
         EDIT  (RF),(6,0(R3)),FLOAT=$                                           
         SPACE 2                                                                
CPM6     LA    R2,4(R2)                                                         
         LA    R3,7(R3)                                                         
         LA    R4,3(R4)                                                         
         BCT   R6,CPM2                                                          
*                                                                               
*        FORMAT CPM REPORT LINE EXIT                                            
*                                                                               
CPMGOOD  EQU   *                                                                
         LA    R0,0                                                             
CPMEXIT  EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
CPMBAD   EQU   *                                                                
         LA    R0,1                                                             
         B     CPMEXIT                                                          
         EJECT                                                                  
*              ROUTINE TO OUTPUT SPOT AVERAGE                                   
         SPACE 3                                                                
SPOTAVE  NMOD1 0,**SPOTAV                                                       
         L     RC,0(R1)                                                         
         SPACE 1                                                                
         OC    0(4,R2),0(R2)                                                    
         BZ    SAX                                                              
         MVC   P+45(9),=C'SPOT AVE.'                                            
         LA    R3,8(R2)                                                         
         LA    R4,P+61                                                          
         ZIC   R5,CNUMDEM                                                       
         CLI   CNUMDEM,0           MAY BE NO DEMOS                              
         BE    SAX                                                              
         SPACE 2                                                                
SA10     L     RE,0(R3)                                                         
         SRDA  RE,31                                                            
         D     RE,0(R2)                                                         
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(6,0(R4)),1                                                 
         LA    R3,4(R3)                                                         
         LA    R4,7(R4)                                                         
         BCT   R5,SA10                                                          
SAX      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO ADD ACCUMULATORS DOWN                                 
         SPACE 3                                                                
ADDEM    NMOD1 0,**ADDEM                                                        
         LA    R3,DETAC+4          MULTIPLY RATE & DEMOS X NO. OF SPOTS         
         LA    R4,9                                                             
         SPACE 2                                                                
ADDEM2   L     R6,0(R3)                                                         
         MH    R6,DETAC+2                                                       
         ST    R6,0(R3)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,ADDEM2                                                        
         SPACE 1                                                                
         LA    R2,DETAC+8                                                       
         LA    R3,CDEMOS                                                        
         ZIC   R4,CNUMDEM                                                       
         CLI   CNUMDEM,0                                                        
         BE    ADDEM3Y                                                          
         SPACE 2                                                                
ADDEM3   CLI   1(R3),C'S'          TAKE OUT SHARE AND HUT NOW                   
         BE    ADDEM3X             SO THEY DONT ADD DOWN                        
         CLI   1(R3),C'P'                                                       
         BE    ADDEM3X                                                          
         CLI   1(R3),C'C'                                                       
         BE    ADDEM3X                                                          
         CLI   1(R3),C'X'                                                       
         BNE   *+10                                                             
ADDEM3X  XC    0(4,R2),0(R2)                                                    
         SPACE 1                                                                
         LA    R2,4(R2)                                                         
         LA    R3,3(R3)                                                         
         BCT   R4,ADDEM3                                                        
         SPACE 1                                                                
ADDEM3Y  LA    R2,DETAC                                                         
         LA    R3,PKGAC                                                         
         LA    R5,10                                                            
         SPACE 2                                                                
ADDEM4   L     R1,0(R2)            NOW ADD TO PACKAGE & PROPOSAL ACCUMS         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,ADDEM4                                                        
         SPACE 1                                                                
         XC    DETAC,DETAC                                                      
         XIT1                                                                   
         EJECT                                                                  
*              OTHER ACCUMULATOR ROUTINES                                       
         SPACE 3                                                                
ADJEM    NMOD1 0,**ADJEM                                                        
         LA    R2,PKGAC            ROUTINE TO ADJUST ORBIT                      
         LA    R3,10                                                            
         L     R4,0(R2)            NUMBER OF PROGRAMS IN ORBIT (R4)             
         LTR   R4,R4                                                            
         BZ    ADJEMX                                                           
         ZICM  R5,PKGTSPT,1        TOTAL SPOTS IN PACKAGE                       
         BNZ   ADJEM2                                                           
         LR    R5,R4                                                            
         SPACE 2                                                                
ADJEM2   L     R1,0(R2)                                                         
         MR    R0,R5                                                            
         SLDA  R0,1                                                             
         DR    R0,R4                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R3,ADJEM2                                                        
ADJEMX   XIT1                                                                   
         SPACE 2                                                                
PKTOPR   NMOD1 0,**PKTOPR                                                       
         LA    R2,PKGAC                                                         
         LA    R3,PROPAC                                                        
         LA    R4,10                                                            
         SPACE 2                                                                
PKTOPR2  L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,PKTOPR2                                                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        OUTPUT BOOK EXPRESSION                                                 
*                                                                               
BOOKOUT  NMOD1 0,**BKOUT*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    R4,CBOOKS                                                        
         OC    DETBOOKS(3),DETBOOKS                                             
         BZ    BOUT05                                                           
         LA    R4,DETBOOKS                                                      
BOUT05   EQU   *                                                                
         LA    R5,DET0B                                                         
*                                                                               
         CLI   0(R5),0                                                          
         BE    BOUT10                                                           
         CLI   0(R5),C' '                                                       
         BE    BOUT10                                                           
         MVC   P+45(5),0(R5)       LOAD BOOK LABEL                              
         B     BOUTEXIT                                                         
*                                                                               
BOUT10   EQU   *                                                                
         ZIC   R1,2(R4)                                                         
         MH    R1,=H'3'                                                         
         L     R0,=A(BOUTMON)                                                   
         A     R0,RELO                                                          
         AR    R1,R0                                                            
         MVC   P+45(3),0(R1)                                                    
         L     R1,=A(SVCLST)                                                    
         A     R1,RELO                                                          
BOUT30   EQU   *                                                                
         CLC   0(1,R4),3(R1)                                                    
         BE    BOUT50                                                           
         LA    R1,L'SVCLST(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   BOUT30                                                           
         DC    H'0'                                                             
*                                                                               
BOUT50   EQU   *                                                                
         CLI   1(R1),C' '                                                       
         BE    BOUT60                                                           
         MVC   P+44(1),1(R1)       PRECEDE WITH P,T,S OR E                      
BOUT60   EQU   *                                                                
         CLI   P+43,C' '           IF THERE IS ENOUGH ROOM                      
         BE    BOUT70                                                           
         MVI   DUB,C' '                                                         
         MVC   DUB+1(3),P+44                                                    
         MVC   P+43(4),DUB         ELSE SHOW EMMYY OR TMMYY                     
*                                                                               
BOUT70   EQU   *                                                                
         ZIC   R1,1(R4)                                                         
         CVD   R1,DUB                                                           
         UNPK  P+48(2),DUB                                                      
         OI    P+49,X'F0'                                                       
*                                                                               
*        BOOKOUT EXIT                                                           
*                                                                               
BOUTEXIT EQU   *                                                                
         XIT1                                                                   
         SPACE 2                                                                
BOUTMON  DC    C'ESTJANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                       
         EJECT                                                                  
         SPACE 2                                                                
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMD3D                                                       
         PRINT ON                                                               
         EJECT                                                                  
RAVLD    DSECT                                                                  
         SPACE 1                                                                
       ++INCLUDE REGENAVLN                                                      
         EJECT                                                                  
RPRPD    DSECT                                                                  
         SPACE 1                                                                
       ++INCLUDE REGENPRPN                                                      
         EJECT                                                                  
RINVD    DSECT                                                                  
         SPACE 1                                                                
       ++INCLUDE REGENINV                                                       
         EJECT                                                                  
*  DEDEMFILE                                                                    
*  DEDBLOCK  (DBLOCKD DSECT)                                                    
*  DEDBEXTRAD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
         PRINT ON                                                               
*  RESFMWORKD                                                                   
         SPACE 1                                                                
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*        WORK AREA                                                              
*                                                                               
SAVERE   DS    F                   SAVE REGISTER E FOR BAL                      
SAVERB   DS    F                   SAVE ADDRESS-ABILITY REGISTER                
SAVER7   DS    F                   SAVE ADDRESS-ABILITY REGISTER                
AFORMDEM DS    A                                                                
AFORMCPM DS    A                                                                
AADDEM   DS    A                                                                
PKGCOST  DS    F                   TOTAL PROPOSAL COST (RAVLCOST)               
*                                                                               
         DS    0F                                                               
DETAC    DS    CL40                DETAIL ACCUMULATOR                           
PKGAC    DS    CL40                PACKAGE ACCUMULATOR                          
PROPAC   DS    CL40                PROPOSAL ACCUMULATOR                         
COMPTEXT DS    H                   COMPULSORY (FORCED) TEXT FROM INV            
*                                                                               
PRPKEY   DS    CL27                PROPOSAL KEY                                 
INVKEY   DS    CL27                INVENTORY KEY                                
PKGCODE  DS    CL2                 PACKAGE CODE (RPRPKPLN)                      
PKGTSPT  DS    CL1                 TOTAL SPOTS IN T PACKAGE (PRPRPSPT)          
INVNO    DS    CL3                 INVENTORY NUMBER (RPRPDINV)                  
INVDT    DS    CL3                 INVENTORY DATE (RPRPDATE)                    
INVSAT   DS    CL1                 SATELLITE (RPRPDSAT)                         
DETBOOKS DS    CL5                 BOOKS FROM DETAIL (OVRRIDES HDR)             
DETDEMOS DS    CL33                DEMOS VALUES FRM DETAIL-OVRIDES HDR          
CPMSW    DS    CL1                 SUPPRESS CPM SWITCH                          
ACTCODE  DS    CL2                 PROGRAM/BREAK CODE (RINVCODE)                
ACTDEMOS DS    CL33                ACTUAL DEMO VALUES                           
DETTEXT  DS    CL1                 Y=MANUAL OR REFERENCED TEXT EXISTS           
WEEKS    DS    CL1                 NUMBER OF WEEKS (RAVLWKS)                    
PRPDATES DS    CL4                 DATES OF CONTRACT OR RAVLDATO                
*                                  (RAVLDATO OVERRIDES CONTRACT DATES)          
HDRLEN   DS    CL1                 LENGTH FROM HEADER (RAVLPLEN)                
DETLEN   DS    CL1                 LEN FROM DET (RPRPDSEC)-OVRRIDES HDR         
HDRPJ    DS    CL14                PJ ELEMENT FROM HEADER (X'05')               
DETPJ    DS    CL14                PJ ELEM FRM DET - OVERRIDES HDR              
HDR0B    DS    CL5                 HDR LABEL FROM X'0B' ELEM                    
DET0B    DS    CL5                 DET LABEL FROM X'0B' - OVERRIDES HDR         
HCPP     DS    CL4                 COST PER POINT FROM HDR (RAVLHCPP)           
RATE     DS    CL4                 RATE $ (RPRPDRTE)                            
DNUM     DS    CL1                 NUMBER OF SPOTS (RPRPDNUM)                   
DNC      DS    CL1                 W, X OR T (RPRPDNC)                          
DYTM     DS    CL48                DAY/TIME/(AVAIL NO./INV NO.)                 
OPS      DS    0CL20               PRINT OPTIONS-ALL DEFAULTS ARE N             
OPTA     DS    CL1                 Y=TRIPLE SPACE, N=DOUBLE                     
OPTB     DS    CL1                 Y=PRINT COVER SHEET, N=DON'T                 
OPTC     DS    CL1                 Y=SUPPRESS TOTALS, N=PRINT                   
OPTD     DS    CL1                 Y=SUPPRESS AVE DEMO DELIVERY, N=PRNT         
OPTE     DS    CL1                 Y=SUPPRESS TOTAL CPP, N=PRINT                
OPTF     DS    CL1                 Y=PRINT SUBTOTALS/PKG, N=SUPPRESS            
OPTG     DS    CL1                 Y=PRINT RATES, N=SUPPRESS                    
OPTH     DS    CL1                 Y=PRINT SPOT CPP, N=DON'T                    
OPTI     DS    CL1                 Y=PRINT MKT/STN TEXT, N=DON'T                
OPTJ     DS    CL1                 Y=PRINT INV & TEXT NO., N=DON'T              
OPTK     DS    CL1                 Y=SUPPRESS AUTO UPGRADE CMT, N=DON'T         
OPTL     DS    CL1                 Y=WHOLE NUMBER DEMOS, N=1 DECIMAL            
OPTM     DS    CL1                 SPARE                                        
OPTN     DS    CL1                 SPARE                                        
OPTO     DS    CL1                 SPARE                                        
OPTP     DS    CL1                 SPARE                                        
OPTQ     DS    CL1                 SPARE                                        
OPTR     DS    CL1                 SPARE                                        
OPTS     DS    CL1                 SPARE                                        
OPTT     DS    CL1                 SPARE                                        
MINLINES DS    CL1                 MINIMUM LINES TO PRINT                       
MULTSW   DS    CL1                 COMBO (RINVCTYP=X'80)                        
UPPRINT  DS    CL1                 AUTO UPGRADE COMMENT TO PRINT                
CKTXTOPT DS    CL1                 CHECK OPTIONS IN TEXT FLAG                   
COMMENT  DS    CL120               HEADER COMMENT (RAVLCEL)                     
HDLN7    DS    CL105               DON'T OVERWRITE PAGE NUMBER                  
HDLN8    DS    CL132                                                            
DEMHD1   DS    CL66                DEMO CATEGORY HEADLINE 1                     
DEMHD2   DS    CL66                DEMO CATEGORY HEADLINE 2                     
DBEXTRA1 DS    CL128               DEMO BLOCK EXTRA DATA                        
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048RESFM12   03/16/05'                                      
         END                                                                    
