*          DATA SET RESFM13    AT LEVEL 006 AS OF 03/16/05                      
*PHASE T81813A,*                                                                
*INCLUDE UPOUT                                                                  
         TITLE 'T81813 (RESFM13) - AVAIL PRINTER'                               
*********************************************************************           
*                                                                   *           
*        RESFM13 --- AVAIL PRINTER                                  *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* APR05/89 (MRR) --- INITIAL RELEASE                                *           
*                                                                   *           
* JUL27/89 (MRR) --- FIX STATION/AFFIL/CHANNEL PRINTING             *           
*                    FIX DEMO GROUP SPACING/PAGING                  *           
*                    MAKE OPTION 'E' WORK, SUPPRESS RATE            *           
*                                                                   *           
* AUG03/89 (MRR) --- IF STATION = 1, SUPPRESS IN HEADLINE           *           
*                    FIX DEMO, SPOT LEN AND COST SPACING            *           
*                    CHANGE SPOT LEN FROM 'LEN/ROMAN' TO 'ROMAN/LEN'*           
*                    MAKE COST FIELD 6 CHARS FROM 8                 *           
*                                                                   *           
* 08/29/89  PJS  --- ALWAYS DISPLAY BASIC (BOOK) LINE.              *           
*                    (INSTEAD OF COLLAPSING SHARE/HPT LINES ON TOP  *           
*                     OF BASIC LINE IF NO BASIC DEMO DATA)          *           
*                                                                   *           
* SEP13/89 (MRR) --- FIX TEXT FILTERING FOR BOOK TYPE DEALING WITH  *           
*                     SERVICE                                       *           
*                                                                   *           
* OCT31/89 (MRR) --- FIX CPP/CPM ON MANUAL LINES.  TOO MUCH SPACING *           
*                     WITH OPTH=Y AND NO COMMENTS TO PRINT          *           
*                                                                   *           
* DEC20/89 (MRR) --- MANUALLY ENTERED SHARES NEED NOT BE MULTIPLIED *           
*                     BY 10                                         *           
*                                                                   *           
* DEC29/89 (MRR) --- COMMENTS/RATIONALE TAKE OVER THE INVENTORY     *           
*                     NAME AND EFFECTIVE DATES WHEN THERE ARE NO    *           
*                     DEMO LINES TO PRINT                           *           
*                                                                   *           
* JAN15/90 (MRR) --- REMOVE OPTIONS H AND I (CONTROL TEXT PRINTING) *           
*                     AND MOVE WHAT WAS J,K,L TO H,I,J.  LOOK FOR   *           
*                     T=0 AND USE THAT AS TEXT=ALL.                 *           
*                                                                   *           
* FEB22/90 (MRR) --- FIX TEXT FILTERING AND ALLOW 'SOON' GENERATION *           
*                     OF A REPORT, FORCE REPORT TO SOON IFF NON-DDS *           
*                     TERMINAL AND > 100 AVAIL LINES                *           
*                                                                   *           
* FEB27/90 (MRR) --- BUG IN -'ING OUT A BOOK WHEN COMPARING THE     *           
*                     BOOK NAMES AND ONE IS SPACES AND THE OTHER IS *           
*                     NULLS                                         *           
*                                                                   *           
* MAR14/90 (MRR) --- SMALL CLEAN UP                                 *           
*                                                                   *           
* 07MAY90  (EFJ) --- REMOVED 'SOURCE' FIELD                         *           
*                                                                   *           
* MAY07/90 (MRR) --- >COMMENTS WERE OVERTLY FILTERED                *           
*                    >UPGRADE COMMENT NOT PRINTING ON MANUAL UPGRADE*           
*                      AND ELSE WHERE NEEDED                        *           
*                    >CPM NOT PRINTING FOR 2ND RATE ON 2ND+ BOOK    *           
*                                                                   *           
* MAY15/90 (MRR) ---> RATE NOW PRINTING ON EVERY BOOK LINE          *           
*                   > UPGRADE NOT PRINTING FOR P-BOOKS              *           
*                                                                   *           
* JUN04/90 (MRR) ---> SUPPRESS SPOT LEN ON 1ST LINE WHEN IT COMES   *           
*                      LATER WITH COSTS                             *           
*                   > PROPER FILTERING OF NSI E/P BOOKS             *           
*                   > CHANGE CODE TO '*' ON AVAIL SUPPLIED UPGRADES *           
*                   > PRINT MANUAL COMMENT W/ T=ALL                 *           
*                   > RATE IS PRINTING FOR ALL BOOKS W/ CPM OPTION  *           
*                   > CHANGE SPOT + MODIFIED PRINTING TO '.N' FROM  *           
*                      ROMAN                                        *           
*                                                                   *           
* JUN08/90 (MRR) ---> BOOKS AND NO DEMOS DUMPS                      *           
*                   > BOOK AND DEMO W/ '=' IS BROKE                 *           
*                   > PRINT RATES EVEN IF NO VALID BOOKS            *           
*                                                                   *           
* OCT05/90 (MRR) ---> IMPLEMENT OPTION 'K' 1 DECIMAL DEMOS          *           
*                                                                   *           
*                                                                   *           
* 09NOV90  (EFJ) ---> USE NEW GENCON FEATURE TO PASS REPORT-NAME    *           
*                     (CON NUMBER) INSTEAD OF 'REAV'                *           
*                   > TAKE OUT USELESS LINE OF CODE THAT ONLY       *           
*                     MESSED THINGS UP.                             *           
*                                                                   *           
* 04DEC90  (EFJ) ---> PUT MARKET-NAME ON COVER SHEET                *           
*                                                                   *           
* MAY08/91 (MRR) --- >SHARE LINE FOR 'PA' TRACKS WAS PRINTING WRONG *           
*                                                                   *           
* DEC10/91 (MRR) --- >DEMO OVERRIDES WERE BEING USED FOR ALL BOOKS  *           
*                      WHEN DOING CPP/CPMS                          *           
*                                                                   *           
* AUG12/92 (BU ) --- >SET SERVICE OF PLUS'D IN BOOK TO THAT OF SAR  *           
*                     RATHER THAN ORDER HEADER. (RTN MBKSP35)       *           
*                                                                   *           
* DEC09/94 (SKU) --- >FIX BUG OF CHECKING NONE AGAINST BOOKS        *           
*                                                                   *           
* OCT12/95 (BU ) --- > FIX UT/TP DEMO FILE ACCESS PROBLEM           *           
*                                                                   *           
* AUG16/96 (BU ) --- LOSING ZIP CODE:  FIX IT.                      *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
T81813   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1813**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
         ST    RB,SAVEDRB                                                       
         ST    R7,SAVED2RB                                                      
*                                                                               
         L     R3,=A(BOOKOUT)                                                   
         A     R3,RELO                                                          
         ST    R3,ABOOKOUT                                                      
         L     R3,=A(DEMNUMS)                                                   
         A     R3,RELO                                                          
         ST    R3,ADEMNUMS                                                      
*                                                                               
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
* IO1 = HEADER RECORD, THEN DETAIL RECORD                      *                
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
*                                                                               
         LA    RE,SYSSPARE                                                      
         L     RF,=AL4(4096-(SYSSPARE-SYSD))                                    
         XCEF                                                                   
*                                                                               
*====================================================================*          
*    VALIDATE CONTRACT  (REQUIRED)                                   *          
*====================================================================*          
*                                                                               
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
*       ALWAYS AVAIL                                                 *          
*====================================================================*          
         SPACE 1                                                                
VK10     DS    0H                                                               
         OI    GENSTAT3,NOCLRSPK   TELL GENCON SETTING OWN SPOOL KEY            
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   SPOOLKEY+1(11),SPACES                                            
         MVC   SPOOLKEY+1(8),PRTCON                                             
         MVI   ETYPE,C'A'                                                       
         SPACE 1                                                                
*====================================================================*          
*    VALIDATE SOURCE  (REQUIRED)                                     *          
*        I=INVENTORY,  S=SID                                         *          
*====================================================================*          
         SPACE 1                                                                
         MVI   ESOURCE,C'I'                                                     
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
VK50     EQU   *                                                                
         MVC   AIO,AIO1            PUT HEADER INTO IO1                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAVLKEY,R6                                                       
         MVI   RAVLKTYP,X'14'                                                   
         MVC   RAVLKREP,AGENCY                                                  
         MVC   RAVLKCON,CCONNUM                                                 
         MVC   RAVLKAVN,XHDRNUM                                                 
         MVC   RAVLKSRC,ESOURCE                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    VK70                                                             
         MVC   RERROR,=AL2(NOTFOUND)                                            
         LA    R2,PRTCONH                                                       
         B     ERREND                                                           
         DROP  R6                                                               
         SPACE 2                                                                
VK70     EQU   *                                                                
*                                                                               
*        THIS CODE WILL VALIDATE A 1-BYTE ZERO-INTENSITY FIELD                  
*        SO THAT THE REPORT DOESN'T GET GENERATED IMMEDIATELY UPON              
*        ENTRY TO THE SCREEN.                                                   
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    VK75                                                             
*                                                                               
VK71     EQU   *                                                                
         LA    R2,PRTVALH                                                       
         CLI   8(R2),C'X'                                                       
         BE    VK75                                                             
         MVI   8(R2),C'X'                                                       
         MVI   5(R2),1                                                          
* NEXT INSTRUCTION REMOVED, IT'S TOTALLY FUCKED!!!                              
*         OI    1(R1),X'01'         FIELD HAS BEEN MODIFIED                     
         OI    6(R2),X'80'         TRANSMIT                                     
         SPACE 1                                                                
         LA    R2,PRTOPAH                                                       
         MVC   RERROR,=H'28'       ENTER DATA                                   
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
         CLI   OFFLINE,C'Y'        DON'T FORCE TO SOON IF ALREADY               
         BE    VK100                OFFLINE                                     
         CLI   DDS,C'Y'            DDS TERMINAL?                                
         BE    VK100                                                            
         CLC   CONWHEN(4),=C'NOW,' ONLY CHECK IF NOW                            
         BNE   VK100                                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAVLKEY,R6                                                       
         MVI   RAVLKTYP,X'14'                                                   
         MVC   RAVLKREP,AGENCY                                                  
         MVC   RAVLKCON,CCONNUM                                                 
         MVC   RAVLKAVN,XHDRNUM                                                 
         DROP  R6                                                               
         SR    R2,R2                     USE R2 AS A COUNTER                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                      THIS HIGH WILL GET THE HEADER          
VK95     EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         LA    R2,1(R2)                                                         
         CLC   KEY(25),KEYSAVE                                                  
         BE    VK95                                                             
         C     R2,=F'101'                                                       
         BNH   VK100                                                            
*                                                                               
         MVC   WORK(3),CONWHEN+4         SAVE OFF ID                            
         MVC   CONWHEN(5),=C'SOON,'                                             
         MVC   CONWHEN+5(3),WORK         AND PUT IT BACK                        
         XC    WORK(3),WORK                                                     
         LA    R2,CONWHENH                                                      
         MVI   4(R2),X'84'                                                      
         MVI   5(R2),8                                                          
         OI    6(R2),X'81'                                                      
         MVI   7(R2),8                                                          
         MVI   WHEN,X'20'                                                       
         MVI   TWAWHEN,2                                                        
VK100    EQU   *                                                                
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
*              PRINT REPORT                                    *                
****************************************************************                
         SPACE 1                                                                
PREP     EQU   *                                                                
*                                                                               
PREP01   EQU   *                                                                
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         MVC   8(2,R2),CCONKOFF                                                 
         GOTO1 VALIOFF             GET OFFICE ADDRESS FOR HEADLINE              
*                                                                               
         MVC   AIO,AIO1            PUT HEADER INTO IO1                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAVLKEY,R6                                                       
         MVI   RAVLKTYP,X'14'                                                   
         MVC   RAVLKREP,AGENCY                                                  
         MVC   RAVLKCON,CCONNUM                                                 
         MVC   RAVLKAVN,XHDRNUM                                                 
         MVC   RAVLKSRC,ESOURCE                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    PREP10                                                           
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
PREP10   EQU   *                                                                
         GOTO1 GETREC                                                           
         BAS   RE,EXTHDR               EXTRACT INFO FROM HEADER                 
         GOTO1 =A(SETHEADS),DMCB,(RC),RR=RELO                                   
         SPACE 1                                                                
         CLI   OPTB,C'Y'           Y=PRINT COVERSHEET                           
         BNE   PREP20                                                           
         MVI   RCSUBPRG,0                                                       
         GOTO1 =A(COVERSHT),DMCB,(RC),RR=RELO                                   
PREP20   EQU   *                                                                
         MVI   RCSUBPRG,1                                                       
         CLI   OPTH,C'Y'           Y=PRINT MARKET & STATION TEXT                
         BNE   PREP30                                                           
         BAS   RE,MSTEXT           GET MKT & STA TEXT AND PRINT                 
PREP30   EQU   *                                                                
         BAS   RE,HOWMANY                                                       
*                                                                               
*        LOOP FOR AVAIL DETAIL RECORDS                                          
*                                                                               
PREP100  EQU   *                                                                
         MVC   AIO,AIO1            PUT DETAIL RECORD IN IO1                     
         L     R1,AIO1                                                          
         MVC   KEY(27),0(R1)       RESTORE DATAMGR SEQUENCE                     
         GOTO1 READ                                                             
         GOTO1 SEQ                                                              
         L     R1,AIO1                                                          
         CLC   KEY(26),0(R1)       CHECK CHANGE OF AVAIL                        
         BNE   XIT                 THAT'S ALL, FOLKS                            
         GOTO1 GETREC                                                           
         BAS   RE,EXTDET           EXTRACT DETAILS FROM DETAIL RECORD           
*                                                                               
         GOTO1 =A(MERGEBKS),DMCB,(RC),RR=RELO                                   
         GOTO1 =A(FORMTOP),DMCB,(RC),RR=RELO                                    
*                                                                               
         CLC   INVNO(3),=C'MAN'                                                 
         BNE   PREP150                                                          
         BAS   RE,MAN              MANUAL                                       
         B     PREP190                                                          
PREP150  EQU   *                                                                
*                                                                               
         CLI   INVNO+2,0                                                        
         BNE   PREP160                                                          
         BAS   RE,PURE             PURE                                         
         B     PREP190                                                          
PREP160  EQU   *                                                                
*                                                                               
         BAS   RE,INV              INVENTORY                                    
*                                                                               
PREP190  EQU   *                                                                
         MVI   SPACING,2                                                        
         CLI   OPTA,C'Y'           Y=TRIPLE SPACE                               
         BNE   PREP195                                                          
         GOTO1 SPOOL,DMCB,(R8)     EXTRA SPACE AFTER EACH DETAIL LINE           
PREP195  EQU   *                                                                
         GOTO1 SPOOL,DMCB,(R8)     SPACE AFTER EACH DETAIL LINE                 
*                                                                               
         B     PREP100             ...LOOP                                      
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE CONTROLS READING OF INVENTORY RECORDS.                *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
INV      NTR1                                                                   
*                                                                               
         MVC   AIO,AIO2            PUT INVENTORY RECORD IN IO2                  
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
         XC    KEY,KEY             BUILD KEY FOR INVENTORY HEADER               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP    USE PARENT REP, NOT AGENCY                   
         MVC   RINVKSTA,CCONKSTA                                                
         MVI   RINVKSTA+4,C'T'                                                  
         CLI   INVSAT,0            SATELLITE OPTION                             
         BE    INV10                                                            
         MVC   RINVKSTA+4(1),INVSAT                                             
INV10    EQU   *                                                                
         MVC   RINVKINV,INVNO                                                   
         MVC   RINVKSTD,INVDT                                                   
INV15    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BNE   XIT                                                              
*                                                                               
INV20    EQU   *                                                                
         CLI   RINVKSRC,0          MAKE SURE ITS A HEADER                       
         BE    INV25                                                            
         DC    H'0'                                                             
INV25    EQU   *                                                                
         OC    INVDT,INVDT         IF DATE IS SPECIFIED,                        
         BZ    INV30                                                            
         CLC   INVDT,RINVKSTD      IT MUST MATCH                                
         BE    INV30               IF NOT, GET TO NEXT INV HEADER               
INV29    EQU   *                                                                
         MVC   RINVKSRC(3),=X'FFFFFF'                                           
         B     INV15                                                            
*                                                                               
INV30    EQU   *                                                                
         TM    KEY+27,X'80'                                                     
         BNZ   XIT                                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 =A(INVDTP),DMCB,(RC),RR=RELO                                     
         CLC   P(44),SPACES         DID NOT PASS EFFECTIVE DATE TEST            
         BE    INV29                IF THESE ARE NOW SPACES                     
*                                                                               
         BAS   RE,ANYOVER                                                       
         BAS   RE,DAYTIME                                                       
         CLC   P3(15),SPACES                                                    
         BE    INV40                                                            
         CLC   P2(15),SPACES                                                    
         BNE   INV40                                                            
         MVC   P2(15),P3                                                        
         MVC   P3(15),SPACES                                                    
*                                                                               
INV40    EQU   *                                                                
         LA    R2,BOOKLIST                                                      
         ZIC   R3,NUMBKS                                                        
         XC    COMPTEXT,COMPTEXT                                                
*                                                                               
         CLI   NUMBKS,0                                                         
         BNE   INV50                                                            
         LA    R3,1                FAKE 1 FOR NO BOOKS                          
         BAS   RE,FORMAT                                                        
         B     INV200                                                           
INV50    EQU   *                                                                
         CLI   OPTG,C'Y'          DO ALL BOOKS/DEMOS CPP/CPM                    
         BNE   INV60                                                            
         MVI   CPMSW,C'Y'                                                       
         MVI   RATESW,C'Y'                                                      
INV60    EQU   *                                                                
*                                                                               
*        LOOP READ AND PROCESS FOR INVENTORY DETAIL RECORDS                     
*                                                                               
INV100   EQU   *                                                                
         XC    KEY,KEY                                                          
         L     R1,AIO2                                                          
         MVC   KEY(24),0(R1)                                                    
*                                                                               
         LR    R1,R2                                                            
         CLI   OPTG,C'Y'          DO ALL BOOKS/DEMOS CPP/CPM                    
         BE    INV110                                                           
         MVI   CPMSW,C'N'                                                       
         TM    0(R1),X'80'                                                      
         BO    INV110                                                           
         MVI   CPMSW,C'Y'                                                       
         MVI   RATESW,C'Y'                                                      
*                                                                               
INV110   EQU   *                                                                
         MVC   RINVKSRC(3),0(R1)   BOOK                                         
         L     RE,=A(SVCLST)       CONVERSION TABLE                             
         A     RE,RELO                                                          
INV120   EQU   *                                                                
         CLC   3(1,RE),0(R1)       FROM BOOKVAL FORMAT TO SRC FORMAT            
         BE    INV150                                                           
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   INV120                                                           
         B     INV200                                                           
*                                                                               
INV150   EQU   *                                                                
         XC    GRPS,GRPS                                                        
         XC    SHARES,SHARES                                                    
         XC    LEVELS,LEVELS                                                    
         MVC   RINVKSRC,2(RE)                                                   
         MVI   DATASRC,C'I'                                                     
         BAS   RE,ANYBK                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   INV200                                                           
         TM    KEY+27,X'80'                                                     
         BNZ   INV200                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         CLI   CNUMDEM,0                                                        
         BE    INV153                                                           
         BAS   RE,DIGDEM                                                        
         BNE   INV200                                                           
*                                                                               
INV153   EQU   *                                                                
         SR    R5,R5                     USE R5 AS UPGRADE FLAG                 
         OC    AUPEL(4),AUPEL            FLIP CODE IF UPGRADE                   
         BZ    INV154                                                           
         CLI   UPTYPE,C'A'               AVAIL UPGRADE, NOT INV                 
         BE    INV155                                                           
INV154   EQU   *                                                                
         MVC   P+INVCODOF(2),ACTCODE     OTPUT PR CODE                          
         B     INV160                                                           
INV155   EQU   *                                                                
         LA    R5,1                                                             
         MVC   P+INVCODOF(2),=C'* '                                             
*                                                                               
INV160   EQU   *                                                                
         GOTO1 ABOOKOUT,DMCB,(RC)                                               
*                                                                               
INV190   EQU   *                                                                
         BAS   RE,FORMAT                                                        
         LTR   R5,R5                                                            
         BNZ   INV192                                                           
         BAS   RE,DINVTEXT                                                      
*                                                                               
INV192   EQU   *                                                                
         OC    AUPEL(4),AUPEL      IF NO UPGRADE EL, NO EXP TO PRINT            
         BZ    INV197                                                           
         CLI   AVLUPED,1           UPGRADED ON AVL OR INV?                      
         BE    INV195              1=ON AVAIL, ALWAYS PRINT                     
         CLC   ACTCODE(2),=C'PJ'   OUTPUT PR CODE                               
         BE    INV195                                                           
         TM    0(R2),X'04'         P-BOOK?                                      
         BZ    INV197              IF SO, ALWAYS PRINT                          
INV195   EQU   *                                                                
         BAS   RE,UPOUT                                                         
INV197   EQU   *                                                                
         MVI   RATESW,C'N'                                                      
*                                                                               
INV200   EQU   *                                                                
         LA    R2,3(R2)            POINT TO NEXT BOOK                           
         LA    R4,2(R4)                                                         
         ZIC   R1,BOOKNO                                                        
         LA    R1,1(R1)                                                         
         STC   R1,BOOKNO                                                        
         BCT   R3,INV100           LOOP                                         
*                                                                               
         CLC   P(44),SPACES                                                     
         BE    INV220                                                           
         MVI   MINLINES,0                                                       
         CLI   RATESW,C'N'                                                      
         BE    INV210                                                           
         BAS   RE,FORMAT                                                        
         CLC   P(44),SPACES                                                     
         BE    INV220                                                           
INV210   EQU   *                                                                
         BAS   RE,SPLAT                                                         
         CLC   P(44),SPACES                                                     
         BE    INV220                                                           
         BAS   RE,SPLAT                                                         
         CLC   P(44),SPACES                                                     
         BE    INV220                                                           
         BAS   RE,SPLAT                                                         
INV220   EQU   *                                                                
         BAS   RE,ANYLEFT                                                       
         BAS   RE,TEXT                                                          
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINES TO HANDLE PURE AVAILS                        *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
PURE     NTR1                                                                   
*                                                                               
         MVC   AIO,AIO2          PUT PURE REC IN IO2                            
         XC    DYTM,DYTM                                                        
****     BAS   RE,ANYOVER                                                       
         LA    R2,BOOKLIST                                                      
         ZIC   R3,NUMMBKS                                                       
         MVI   MYSWITCH,C'N'                                                    
         MVI   BOOKNO,1                                                         
*                                                                               
PUR10    EQU   *                                                                
         LR    R1,R2                                                            
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
         MVC   DYTM+00(3),CBLOCK+10    DAY                                      
         MVC   DYTM+16(6),CBLOCK+20    TIME                                     
         SPACE 1                                                                
         BAS   RE,ANYOVER          GET ANY OVERRIDES                            
         BAS   RE,DAYTIME                                                       
         SPACE 1                                                                
         OC    HDR0B,HDR0B         ANY BOOK LABEL                               
         BZ    *+14                                                             
         MVC   P+44(5),HDR0B       USE IT INSTEAD OF BOOK                       
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
         SPACE 1                                                                
PUR50    EQU   *                                                                
         LA    R2,3(R2)                                                         
         CLI   BOOKNO,0                                                         
         BE    PUR90                                                            
         ZIC   R1,BOOKNO                                                        
         LA    R1,1(R1)                                                         
         STC   R1,BOOKNO                                                        
         OC    0(3,R2),0(R2)                                                    
         BZ    PUR90                                                            
         CLI   0(R2),X'FF'                                                      
         BE    PUR90                                                            
         BCT   R3,PUR10                                                         
PUR90    EQU   *                                                                
         MVI   RATESW,C'N'                                                      
         BAS   RE,TEXT                                                          
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO HANDLE MANUAL AVAILS                       *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
MAN      NTR1                                                                   
         BAS   RE,ANYOVER                                                       
         BAS   RE,DAYTIME                                                       
         CLC   P,SPACES    IS THERE A HOLE (NO DAY/TIME/PROG OVRRIDE)           
         BE    MAN100              YES SO SKIP BOOKS ETC                        
*                                                                               
         MVC   ACTDEMOS,MANDEMOS                                                
         XC    GRPS,GRPS                                                        
         XC    SHARES,SHARES                                                    
         XC    LEVELS,LEVELS                                                    
*                                                                               
         LA    R2,BOOKLIST                                                      
         CLI   OPTG,C'Y'           CPP/CPM REPORT OPTION?                       
         BE    MAN20               YES                                          
         MVI   CPMSW,C'N'                                                       
         TM    0(R2),X'80'                                                      
         BO    MAN30                                                            
MAN20    EQU   *                                                                
         MVI   CPMSW,C'Y'                                                       
MAN30    EQU   *                                                                
         OC    0(3,R2),0(R2)       ARE THERE ANY BOOKS AT ALL                   
         BZ    MAN50               NO, SKIP BOOKOUT                             
         MVI   BOOKNO,1                                                         
         GOTO1 ABOOKOUT,DMCB,(RC)                                               
MAN50    EQU   *                                                                
         BAS   RE,FORMAT                                                        
         MVI   RATESW,C'N'                                                      
         SPACE 1                                                                
MAN100   BAS   RE,TEXT                                                          
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* ROUTINE TO PRINT AUTO UPGRADE COMMENT                              *          
*                                                                    *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
UPOUT    NTR1                                                                   
         ZICM  R2,AUPEL,4          ARE WE ADDRESSING UPGRADE ELEMENT            
         BZ    XIT                                                              
         CLI   OPTJ,C'Y'                                                        
         BE    XIT                                                              
         BAS   RE,SPLAT                                                         
         GOTO1 =V(UPOUT),DMCB,(R2),P+9,RR=RELO                                  
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
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
MS20     EQU   *                                                                
         MVI   CKTXTOPT,0          SET CHECK OPT FLAG                           
         BAS   RE,TXTPRNT                                                       
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
         MVI   ELCODE,X'08'        TEXT ELEMENT                                 
         BAS   RE,NEXTEL                                                        
TXT30    EQU   *                                                                
         BNE   TXT50                                                            
*                                                                               
         USING RAVLTEL,R6                                                       
         CLI   RAVLTTYP,C'T'       SPECIFIC TEXT NUMBER                         
         BNE   TXT40                                                            
         ZICM  R2,RAVLTNUM,2       ZERO IS DO ALL                               
         BNZ   TXT35               NON-ZERO IS DO SPECIFIC                      
         BAS   RE,GETTEXT          TELL DOWNSTREAM ROUTINE DO DO ALL            
         XC    COMPTEXT,COMPTEXT   WE MUST HAVE PRINTED COMPULSORY TEXT         
         B     TXT20               LOOP FOR MANUAL(S)                           
         SPACE 1                                                                
TXT35    MVC   DUB(2),RAVLTDTA     TEXT NO. REFERENCED                          
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
GTXT10   EQU   *                                                                
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GTXIT                                                            
         LTR   R2,R2               0=PRINT ALL AVAILABLE TEXT                   
         BZ    GTXT20                                                           
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GTXIT                                                            
GTXT20   EQU   *                                                                
         MVC   DUB,RINVKTXT        CURRENT TEXT NUMBER                          
         MVI   CKTXTOPT,0          SET CHECK OPT FLAG                           
         BAS   RE,TXTPRNT                                                       
         CLC   RINVKTXT,COMPTEXT   SEE IF WE'VE JUST PRINTED                    
         BNE   GTXT30              COMPULSORY TEXTS                             
         XC    COMPTEXT,COMPTEXT                                                
GTXT30   EQU   *                                                                
         LTR   R2,R2               0=PRINT ALL AVAILABLE TEXT                   
         BNZ   GTXIT                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     GTXT10                                                           
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
         MVI   CKTXTOPT,1          SET DON'T CHECK OPT FLAG                     
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
         CLI   CKTXTOPT,1          CHECK DON'T CHECK OPT FLAG                   
         BE    TP05                                                             
**       ZICM  R0,DUB,2            DUB=ZERO ON ALL TEXT                         
**       BNZ   TP05                                                             
         BAS   RE,TXTFILT                                                       
         BNZ   XIT                                                              
TP05     EQU   *                                                                
         CLI   OPTI,C'Y'           Y=PRINT TEXT & INV. NO.                      
         BNE   TP10                                                             
         CLI   RINVKSRC,X'FF'      NORMAL RATIONAL RECORD                       
         BE    TP06C                                                            
         CLI   RINVKSRC,C'M'       MARKET FACT TEXT?                            
         BE    TP06A               YES, DO THAT                                 
         CLI   RINVKSRC,C'S'       STATION FACT TEXT?                           
         BNE   TP10                NO, IT'S DEMO TEXT, NO TEXT NUMBER           
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
         SPACE 2                                                                
TP20     BAS   RE,NEXTEL                                                        
         SPACE 2                                                                
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
         DROP  R6                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        TXTFILT - FILTER TEXT RECORDS WHEN 'ALL' HAVE BEEN REQUESTED*          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
TXTFILT  NTR1                                                                   
*                                                                               
         L     R6,AIO3                                                          
         USING RINVFEL,R6                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   TXTFGOOD            NO FILTER ELEMENT                            
*                                                                               
         CLI   RINVFSRC,0          SERVICE FILTER                               
         BE    TXTF10                                                           
         CLC   CSOURCE(1),RINVFSRC                                              
         BNE   TXTFBAD                                                          
*                                                                               
TXTF10   EQU   *                                                                
         CLI   RINVFYR,0           BOOK FILTER                                  
         BE    TXTF20                                                           
         LA    R1,BOOKLIST                                                      
         ZIC   R2,NUMBKS                                                        
         LTR   R2,R2                                                            
         BZ    TXTFBAD             NO BOOKS TO FILTER ON                        
TXTF15   EQU   *                                                                
         CLC   1(2,R1),RINVFYR                                                  
         BE    TXTF17                                                           
TXTF16   EQU   *                                                                
         LA    R1,3(R1)                                                         
         BCT   R2,TXTF15                                                        
         B     TXTFBAD                                                          
TXTF17   EQU   *                                                                
         CLI   RINVFBKT,0          BOOK TYPE                                    
         BNE   TXTF18                                                           
*******  TM    0(R1),X'26'         ESTIMATED/PROJECTED/SPC SURVEY?              
*******  BNZ   TXTF16              YES, SKIP IT                                 
         B     TXTF20              NOPE, TAKE IT                                
TXTF18   EQU   *                                                                
         MVC   DMCB(1),0(R1)                                                    
         NI    DMCB,X'3E'          CLEAR SUPPRESS CPM BIT AND SVC BITS          
         MVC   DMCB+1(1),RINVFBKT                                               
         NI    DMCB+1,X'3E'        CLEAR SUPPRESS CPM BIT AND SVC BITS          
         CLC   DMCB+1(1),DMCB                                                   
         BNE   TXTF16                                                           
*                                                                               
TXTF20   EQU   *                                                                
         CLI   RINVFLOC,0          LOCAL TEXT ONLY                              
         BE    TXTF40                                                           
* ------                                                                        
TXTF40   EQU   *                                                                
         ZIC   R1,RINVFLEN         CHECK FOR DEMO FILTERS                       
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
         DROP  R6                                                               
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
* THIS ROUTINE EXTRACTS DETAILS FROM THE AVAIL HEADER RECORD.        *          
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
         OC    CDEMOS(3),CDEMOS                                                 
         BNZ   EH05                                                             
         MVI   CNUMDEM,0                                                        
         MVI   CNUMBKS,0                                                        
         B     EH30                                                             
*                                                                               
EH05     EQU   *                                                                
         LA    R2,CDEMOS+21                                                     
         LA    R3,8                                                             
         SPACE 2                                                                
EH10     OC    0(3,R2),0(R2)       COUNT DEMOS                                  
         BNZ   EH20                                                             
         SH    R2,=H'3'                                                         
         BCT   R3,EH10                                                          
*                                                                               
EH20     EQU   *                                                                
         STC   R3,CNUMDEM                                                       
*                                                                               
         MVC   CBOOKS,RAVLBKS      COUNT BOOKS                                  
         MVI   CNUMBKS,0                                                        
         LA    R2,CBOOKS+15                                                     
         LA    R3,6                                                             
EH25     EQU   *                                                                
         OC    0(3,R2),0(R2)                                                    
         BNZ   EH26                                                             
         SH    R2,=H'3'                                                         
         BCT   R3,EH25                                                          
EH26     EQU   *                                                                
         STC   R3,CNUMBKS                                                       
*                                                                               
EH30     EQU   *                                                                
         MVC   FORMULAS,RAVLRFRM                                                
         LA    R2,FORMULAS+10                                                   
         LA    R3,6                                                             
EH32     EQU   *                                                                
         OC    0(2,R2),0(R2)       COUNT FORMULAS                               
         BNZ   EH34                                                             
         SH    R2,=H'2'                                                         
         BCT   R3,EH32                                                          
EH34     EQU   *                                                                
         STC   R3,NUMFORMS                                                      
*                                                                               
         MVC   CSOURCE,RAVLSRC                                                  
         MVC   WEEKS,RAVLWKS                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,CCONDAT),(2,AVLDATES)                             
         GOTO1 (RF),(R1),(3,CCONDAT+3),(2,AVLDATES+2)                           
*                                                                               
         MVC   HDRLEN,RAVLPLEN                                                  
         MVC   HCPP,RAVLCPP                                                     
*                                                                               
         DROP  R6                                                               
*                                                                               
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
         DROP  R6                                                               
         SPACE 1                                                                
EH60     EQU   *                                                                
         XC    HDRPJ(28),HDRPJ                                                  
         L     R6,AIO1                                                          
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL            LOOK FOR HEADER UPGRADE ELEMENT              
         BNE   EH70                                                             
         MVC   HDRPJ(14),0(R6)     SAVE ELEMENT                                 
         BAS   RE,NEXTEL                                                        
         BNE   EH70                                                             
         MVC   HDRPJ+14(14),0(R6)                                               
         SPACE 1                                                                
EH70     EQU   *                                                                
         XC    HDR0B,HDR0B                                                      
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
*                                                                               
         MVI   RATESW,C'Y'                                                      
*                                                                               
         XC    ACTDEMOS,ACTDEMOS                                                
         L     R6,AIO1             DETAIL RECORD                                
         USING RAVLKEY,R6                                                       
         MVC   XDETNUM,RAVLKDET    SAVE DETAIL NUMBER                           
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    EDET10                                                           
         DC    H'0'                                                             
EDET10   EQU   *                                                                
*                                                                               
         USING RAVLDEL,R6          COVER DETAIL RECORD 01 ELEMENT               
         MVC   INVNO,RAVLDINV                                                   
         MVC   INVDT,RAVLDATE                                                   
         MVC   AVLRATES(24),RAVLDRTE                                            
         MVC   INVSAT,RAVLDSAT                                                  
*                                                                               
         XC    DFORMS,DFORMS                                                    
         MVI   DNUMFORM,0                                                       
*                                                                               
         MVC   DFORMS,RAVLDFRM                                                  
         LA    R2,DFORMS+10                                                     
         LA    R3,6                                                             
EDET15   EQU   *                                                                
         OC    0(2,R2),0(R2)       COUNT FORMULAS                               
         BNZ   EDET16                                                           
         SH    R2,=H'2'                                                         
         BCT   R3,EDET15                                                        
EDET16   EQU   *                                                                
         STC   R3,DNUMFORM                                                      
*                                                                               
         XC    DETPJ(28),DETPJ                                                  
         L     R6,AIO1                                                          
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL            LOOK FOR DETAIL UPGRADE ELEMENT              
         BNE   EDET20                                                           
         USING RAVLNEL,R6                                                       
         MVC   DETPJ(14),RAVLNEL   LOAD THE FIRST DETAIL UPGRADE                
         BAS   RE,NEXTEL                                                        
         BNE   EDET20                                                           
         MVC   DETPJ+14(14),RAVLNEL                                             
         DROP  R6                                                               
*                                                                               
EDET20   EQU   *                                                                
         L     R6,AIO1                                                          
         MVI   ELCODE,X'0D'                                                     
         BAS   RE,GETEL            LOOK FOR DETAIL SID ELEMENT                  
         BNE   XIT                                                              
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO CHECK FOR OVERRIDES                        *          
*--------------------------------------------------------------------*          
         SPACE 3                                                                
ANYOVER  NTR1                                                                   
         XC    MANDEMOS,MANDEMOS   PRESET                                       
         XC    MANBOOKS,MANBOOKS                                                
         MVI   NUMMBKS,0                                                        
         MVC   MANCPM,=8C'Y'                                                    
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
         SPACE                                                                  
         DROP  R6                                                               
         SPACE 2                                                                
BRANCH   B     AO50                DAY                                          
         B     AO60                TIME                                         
         B     AO70                PROGRAM TITLE                                
         B     AO80                DEMOS                                        
         B     AO90                CODES                                        
         B     AO100               CPM                                          
         B     AO10                BOOK-NONE                                    
         EJECT                                                                  
*              SPECIFIC OVERRIDE ROUTINES                                       
         SPACE 2                                                                
*                                                                               
*        OVERRIDE FOR DAY                                                       
*                                                                               
AO50     EQU   *                                                                
         MVC   DYTM(16),WORK       DAY                                          
         B     AO10                                                             
*                                                                               
*        OVERRIDE FOR TIME                                                      
*                                                                               
AO60     EQU   *                                                                
         MVC   DYTM+16(16),WORK    TIME                                         
         B     AO10                                                             
*                                                                               
*        OVERRIDE FOR PROGRAM                                                   
*                                                                               
AO70     EQU   *                                                                
         GOTO1 CHOPPER,DMCB,(48,WORK),(27,P+17),(C'P',3)                        
         B     AO10                                                             
*                                                                               
*        OVERRIDE FOR DEMO                                                      
*                                                                               
AO80     EQU   *                                                                
         CLI   CNUMDEM,0           DEMOS                                        
         BE    AO10                IF NO REAL DEMOS, SKIP OVERRIDES             
         LA    R3,BLOCK                                                         
         MVC   0(80,R3),SPACES                                                  
         MVC   0(48,R3),WORK                                                    
         LA    R4,80(R3)                                                        
         XC    0(256,R4),0(R4)                                                  
         GOTO1 SCANNER,DMCB,(C'C',(R3)),(8,(R4))                                
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    AO10                                                             
         LA    R1,CDEMOS                                                        
         LA    R3,MANDEMOS                                                      
*                                                                               
AO82     EQU   *                                                                
         L     R0,4(R4)                                                         
         LTR   R0,R0                                                            
         BNZ   AO825                                                            
         CLI   12(R4),C'0'                                                      
         BNE   AO828                                                            
         L     R0,=F'-1'                                                        
         B     AO828                                                            
AO825    EQU   *                                                                
         CLI   PRTOPK,C'Y'         OVERRIDE WITH WHOLE NUMBERS?                 
         BE    AO828               - YES                                        
         MH    R0,=H'10'           ELSE, * 10 TO SIMULATE 1 DECIMAL             
AO828    EQU   *                                                                
*                                                                               
         ST    R0,0(R3)                                                         
         LA    R1,3(R1)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,32(R4)                                                        
*                                                                               
         BCT   R5,AO82             LOOP                                         
         B     AO10                                                             
*                                                                               
*        OVERRIDE FOR CODES                                                     
*                                                                               
AO90     EQU   *                                                                
         MVC   0(2,R3),12(R4)                                                   
         LA    R3,2(R3)                                                         
         LA    R4,32(R4)                                                        
         BCT   R5,AO90                                                          
         B     AO10                                                             
*                                                                               
*        OVERRIDE FOR CPM                                                       
*                                                                               
AO100    EQU   *                                                                
         BAS   RE,SCANIT                                                        
         CH    R5,=H'1'                                                         
         BL    AO10                                                             
         LA    R3,MANCPM                                                        
         BH    AO110                                                            
         MVC   MANCPM(8),=8C'N'                                                 
         B     AO10                                                             
*                                                                               
AO110    EQU   *                                                                
         MVC   0(1,R3),12(R4)                                                   
         LA    R3,1(R3)                                                         
         LA    R4,32(R4)                                                        
         BCT   R5,AO110                                                         
         B     AO10                                                             
*                                                                               
*        OVERRIDE FOR BOOK                                                      
*                                                                               
AO120    EQU   *                                                                
         B     AO10                                                             
*                                                                               
*        SCANIT --- SCANNER INTERFACE FOR OVERRIDE ROUTINES                     
*                                                                               
SCANIT   NTR1                                                                   
*                                                                               
         L     R3,AIO3                                                          
         MVC   0(80,R3),SPACES                                                  
         MVC   0(48,R3),WORK                                                    
         MVC   DMCB+8(4),=C',=/='                                               
         LA    R4,80(R3)                                                        
         GOTO1 SCANNER,DMCB,(C'C',(R3)),(8,(R4))                                
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         XIT1  REGS=(R4,R5)                                                     
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE COMPRESSES THE ORIGINAL OR OVERRIDE DAY/TIME EXPRESSION          
* AND THE AVAIL DETAIL NUMBER AND THE INVENTORY NUMBER ( IF OPTI=Y)             
* INTO P (AND P2 IF NECESSARY), EACH 16 CHARACTERS LONG.                        
*--------------------------------------------------------------------*          
         SPACE 2                                                                
DAYTIME  NTR1                                                                   
         OC    DYTM,SPACES                                                      
         GOTO1 SQUASHER,DMCB,DYTM,48                                            
         GOTO1 CHOPPER,DMCB,(48,DYTM),(16,P),(C'P',2)                           
         B     XIT                                                              
         EJECT                                                                  
HOWMANY  NTR1                                                                   
         LA    R2,P                SET UP MINIMUM TO PRINT                      
         LA    R3,4                                                             
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
         ZIC   R2,MINLINES         ENSURE ENOUGH LINES PRINT                    
         LTR   R2,R2                                                            
         BZ    XIT                                                              
         BAS   RE,SPLAT                                                         
         BCT   R2,*-4                                                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        FORMAT --- ROUTINE TO CONTROL FORMATTING DETAIL LINE(S)                
*                                                                               
FORMAT   NTR1                                                                   
*                                                                               
         ZIC   R0,ALLOWLIN                                                      
         LTR   R0,R0                                                            
         BNZ   FORM10                                                           
*                                                                               
         MVI   ALLOWLIN,3                                                       
         CLI   OPTE,C'Y'                                                        
         BE    FORM10                                                           
         CLI   OPTF,C'Y'                                                        
         BE    FORM10                                                           
         CLC   AVLRATES(4),=X'00000000'                                         
         BE    FORM10                                                           
         ZIC   R0,ALLOWLIN                                                      
         ZIC   RF,NUMFORMS                                                      
         AR    R0,RF                                                            
         STC   R0,ALLOWLIN                                                      
FORM10   EQU   *                                                                
*                                                                               
         CLI   CNUMDEM,0           DEMOS ARE OPTIONAL                           
         BE    FORM20                                                           
*                                                                               
         LA    R2,6                                                             
         LA    R3,FORMLIST                                                      
FORM15   EQU   *                                                                
         MVC   LINSW(1),0(R3)                                                   
         GOTO1 =A(EDITDEMO),DMCB,(RC),RR=RELO                                   
         LA    R3,1(R3)                                                         
         BCT   R2,FORM15                                                        
FORM20   EQU   *                                                                
*                                                                               
         LA    R3,AVLRATES                                                      
*                                                                               
         LA    R2,DFORMS                                                        
         ZIC   R4,DNUMFORM                                                      
         LTR   R4,R4                                                            
         BNZ   FORM30                                                           
*                                                                               
         LA    R2,FORMULAS                                                      
         ZIC   R4,NUMFORMS                                                      
         LTR   R4,R4                                                            
         BZ    XIT                                                              
*                                                                               
FORM30   EQU   *                                                                
         CLI   OPTE,C'Y'                                                        
         BE    FORM35                                                           
         BAS   RE,FORMLEN          EDIT LENGTH AND RATE                         
         CLI   OPTF,C'Y'                                                        
         BE    FORM35                                                           
         CLI   CPMSW,C'Y'                                                       
         BE    FORM50                                                           
FORM35   EQU   *                                                                
         CLC   P+60(70),SPACES                                                  
         BE    FORM60                                                           
         BAS   RE,SPLAT                                                         
         B     FORM60                                                           
*                                                                               
FORM50   EQU   *                                                                
         CLI   CNUMDEM,0           IF NO DEMOS, DON'T DO CPP                    
         BE    FORM60                                                           
         GOTO1 =A(CPP),DMCB,(RC),RR=RELO                                        
         CLC   P+LINDATOF(70),SPACES                                            
         BE    FORM60                                                           
         BAS   RE,SPLAT                                                         
*                                                                               
FORM60   EQU   *                                                                
         LA    R2,2(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,FORM30                                                        
         BAS   RE,SPLAT                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
*        THE LIST OF LIST TYPE FOR EDITDEMO TO PRODUCE                          
*                                                                               
FORMLIST DC    C'IRS2L4'           BASIC/GRP/SHRS/ /LEVS/                       
         DS    0F                                                               
         EJECT                                                                  
*                                                                               
*        FORMLEN --- ROUTINE TO FORMAT LENGTH AND RATE                          
*                                                                               
FORMLEN  NTR1                                                                   
*                                                                               
         CLI   RATESW,C'Y'         CHECK SWITCH                                 
         BNE   XIT                 YES                                          
*                                                                               
FLEN10   EQU   *                                                                
         L     R4,0(R3)                                                         
         LTR   R4,R4                                                            
         BZ    XIT                                                              
*                                                                               
         ZIC   R3,CNUMDEM          FIND TO EDIT LEN AND COST                    
         LA    R0,DMCOLWTH                                                      
         STH   R0,HALF                                                          
         MH    R3,HALF                                                          
         LA    R3,P+LINDATOF(R3)                                                
*                                                                               
         ZIC   RE,1(R2)                                                         
         EDIT  (RE),(3,0(R3)),ALIGN=LEFT                                        
         ZIC   R1,0(R2)            GET LEN MODIFIER NUMBER                      
         LTR   R1,R1                                                            
         BZ    FLEN40                                                           
         LR    RE,R3                                                            
FLEN20   EQU   *                                                                
         CLI   0(RE),C' '                                                       
         BE    FLEN30                                                           
         CLI   0(RE),0                                                          
         BE    FLEN30                                                           
         LA    RE,1(RE)                                                         
         B     FLEN20                                                           
FLEN30   EQU   *                                                                
         MVI   0(RE),C'.'                                                       
         MVC   1(1,RE),0(R2)                                                    
         OI    1(RE),X'F0'                                                      
*                                                                               
FLEN40   EQU   *                                                                
         EDIT  (R4),(6,4(R3))                                                   
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS UPGRADES                                                       
*                                                                               
ANYUP    NTR1                                                                   
         XC    AUPEL,AUPEL                                                      
         XC    AVLUPED,AVLUPED                                                  
         MVI   UPTYPE,0                                                         
*                                                                               
         XC    DMCB(20),DMCB       STORE UP TO 4 UPGRADE ADDRS                  
         LA    R1,DMCB                                                          
*                                                                               
         CLI   HDRPJ,0             ANY HEADER UPGRADES?                         
         BE    ANYUP1C             NO, LOOK FOR DETAIL UPGRADES                 
         CLC   DETPJ+2(4),=C'NONE' CHECK FOR DETAIL REMOVAL                     
         BE    ANYUP1A             NONE MEANS SKIP IT                           
         CLI   HDRUP1FG,0          SKIP 1ST HDR UPGRADE FOR THIS ITEM           
         BNE   ANYUP1A              NON-ZERO MEANS YES                          
         LA    RF,HDRPJ                                                         
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
ANYUP1A  EQU   *                                                                
         CLI   HDRPJ+14,0          SECOND HEADER UPGRADE                        
         BE    ANYUP1C             NO                                           
         CLC   DETPJ+16(4),=C'NONE' CHECK FOR DETAIL REMOVAL                    
         BE    ANYUP1C              YES, SKIP IT                                
         CLI   HDRUP2FG,0          SKIP 2ND HDR UPGRADE FOR THIS ITEM           
         BNE   ANYUP1C              NON-ZERO MEANS YES                          
         LA    RF,HDRPJ+14                                                      
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
ANYUP1C  EQU   *                                                                
         CLI   DETPJ,0             ANY DETAIL UPGRADES?                         
         BE    ANYUP1Z             NO                                           
         CLI   DETPJ+2,0           IF ZERO, IT'S A PLACE HOLDER                 
         BE    ANYUP1E                                                          
         CLC   DETPJ+2(4),=C'NONE'                                              
         BE    ANYUP1E             SKIP IT IF WE ALREADY USED IT                
         LA    RF,DETPJ                                                         
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
ANYUP1E  EQU   *                                                                
         CLI   DETPJ+14,0          2ND DETAIL UPGRADE LOADED?                   
         BE    ANYUP1Z             NO, MOVE ON                                  
         CLI   DETPJ+16,0          ANYTHING THER                                
         BE    ANYUP1Z             NO SKIP IT                                   
         CLC   DETPJ+16(4),=C'NONE'  SKIP IT IF ALREADY USED                    
         BE    ANYUP1Z                                                          
         LA    RF,DETPJ+14                                                      
         ST    RF,0(R1)                                                         
ANYUP1Z  EQU   *                                                                
         LA    R1,DMCB                                                          
         L     R6,0(R1)            LOOK FOR UPGRADE ELEMENT                     
         B     ANYUP4                                                           
*                                                                               
ANYUP2   EQU   *                                                                
         LA    R1,4(R1)                                                         
         L     R6,0(R1)            LOOK FOR NEXT UPGRADE ELEMENT                
*                                                                               
ANYUP4   EQU   *                                                                
         LTR   R6,R6                                                            
         BZ    ANYUP6                                                           
         CLI   0(R6),X'05'         UPGRADE ELEMENT ?                            
         BNE   ANYUP6              NO                                           
         BAS   RE,BOOKMISS                                                      
         BE    ANYUP2                                                           
         MVI   P+58,C'*'                                                        
         MVI   UPTYPE,C'A'                                                      
         MVC   P+INVCODOF(2),SPACES (DONT SHOW PROGRAM CODE ON                  
*                                     UPGRADES AT AVAIL TIME)                   
*                                     AND APPLY THIS IF FOUND                   
         CLI   AVTYPE,C'P'                                                      
         BNE   ANYUP4B                                                          
         BAS   RE,ANYUPPAV         GET PAV DATA                                 
         SPACE 1                                                                
ANYUP4B  EQU   *                                                                
         LR    RF,R4                                                            
         S     RF,=F'34'                                                        
         NI    11(RF),X'FF'-X'40'  BECAUSE OF UT/TP PROBLEM                     
         GOTO1 DEMUP,DMCB,(R4),(R6),ACOMFACS                                    
         LR    RF,R4                                                            
         S     RF,=F'34'                                                        
         OI    11(RF),X'40'        BECAUSE OF UT/TP PROBLEM                     
         ST    R6,AUPEL                                                         
         MVI   AVLUPED,1                                                        
         USING RAVLNEL,R6                                                       
         CLI   RAVLNTYP,3                                                       
         BE    ANYUP5                                                           
         CLI   RAVLNTYP,6                                                       
         BNE   XIT                                                              
         SPACE 1                                                                
ANYUP5   OC    RAVLNOP1,RAVLNOP1                                                
         BZ    XIT                                                              
         OC    RAVLNOP2,RAVLNOP2                                                
         BNZ   XIT                                                              
         ZIC   R1,BOOKNO                                                        
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,BOOKLIST(R1)                                                  
         MVC   RAVLNOP2,1(R1)      ADJUST UP ELEMENT TO SHOW FROM BOOK          
         B     XIT                                                              
*                                                                               
         DROP  R6                                                               
         SPACE 1                                                                
ANYUP6   L     R6,AIO2                                                          
         CLI   AVTYPE,C'I'                                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVI   UPTYPE,C'I'                                                      
         ST    R6,AUPEL                                                         
         L     R6,AIO2                                                          
         MVI   ELCODE,X'CD'        LOOK FOR CODE ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING RINVCEL,R6                                                       
         CLC   RINVCODE,=C'PJ'     ONLY SHOW INV UPGRADES WHEN CODE IS          
         BE    ANYUP7              PJ                                           
         BNE   XIT                                                              
         DROP  R6                                                               
*                                                                               
ANYUP7   EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SEE IF UPGRADE QUALIFIES FOR BOOK                     
         SPACE 3                                                                
BOOKMISS EQU   *                                                                
         XC    WORK(1),WORK                                                     
         CLI   BOOKNO,8                                                         
         BH    BKMS10                                                           
         ZIC   RF,BOOKNO                                                        
         BCTR  RF,0                                                             
         LA    RF,BITLIST(RF)                                                   
         USING RAVLNEL,R6                                                       
         MVC   WORK(1),RAVLNBKS                                                 
         NC    WORK(1),0(RF)                                                    
         DROP  R6                                                               
BKMS10   EQU   *                                                                
         CLI   WORK,0                                                           
         BR    RE                                                               
         SPACE 1                                                                
BITLIST  DC    X'8040201008040201'                                              
         EJECT                                                                  
*                                                                               
*        GET PAV DATA AND MAKE IT LOOK LIKE INV DATA FOR UPGRADE                
*                                                                               
ANYUPPAV NTR1                                                                   
*                                                                               
         LA    R5,BLOCK                                                         
         USING DBLOCK,R5                                                        
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'PAV'                                                   
         MVC   DBAREC,AIO2                                                      
         ST    R4,DBAQUART                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         CLI   PRTOPK,C'Y'        'Y' MEANS WHOLE NUMBER                        
         BE    AUP10A                                                           
         MVI   DBXTTRP,X'01'      RTG = 1 DECIMAL                               
         MVI   DBXTTSP,X'01'      SHR = 1 DECIMAL                               
         MVI   DBXTTIP,X'02'      IMP = 00'S                                    
         B     AUP10B                                                           
AUP10A   EQU   *                                                                
         MVI   DBXTTRP,X'00'      RTG = 0 DECIMAL                               
         MVI   DBXTTSP,X'00'      SHR = 0 DECIMAL                               
         MVI   DBXTTIP,X'03'      IMP = 000'S                                   
AUP10B   EQU   *                                                                
         DROP  R1                                                               
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CDEMOUT                                                       
         GOTO1 (RF),DMCB,(C'P',DEMOSHR),DBLOCK,HOMSHARE                         
         DROP  RE                                                               
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         LA    RE,WORK             BUILD DUMMY UPGRADE ELEMENT                  
         USING RAVLNEL,RE                                                       
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVI   RAVLNTYP,4                                                       
         MVI   RAVLNBKS,C'P'                                                    
         MVI   RAVLNCAT,C'P'                                                    
         MVC   RAVLNOP1,=X'FFFF'   GET OLD HPTS                                 
         MVC   RAVLNOP2,=H'1'                                                   
         XC    DUB,DUB                                                          
         LR    RF,R4                                                            
         S     RF,=F'34'                                                        
         NI    11(RF),X'FF'-X'40'  BECAUSE OF UT/TP PROBLEM                     
         GOTO1 DEMUP,DMCB,(R4),WORK,ACOMFACS,DUB,HOMSHARE                       
         LR    RF,R4                                                            
         S     RF,=F'34'                                                        
         OI    11(RF),X'40'        BECAUSE OF UT/TP PROBLEM                     
*                                                                               
         B     XIT                                                              
         DROP  R5,RE                                                            
         EJECT                                                                  
*              SEE IF UPGRADE CAUSES ANOTHER BOOK TO BE READ                    
         SPACE 3                                                                
ANYBK    NTR1                                                                   
         L     R6,AIO              LOOK FOR UPGRADE ELEMENT                     
         LA    R6,34(R6)                                                        
         MVI   ELCODE,X'05'                                                     
         BAS   RE,FIRSTEL                                                       
         B     ANYBK4                                                           
         SPACE 1                                                                
ANYBK2   BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
ANYBK4   BNE   XIT                                                              
         BAS   RE,BOOKMISS                                                      
         BE    ANYBK2                                                           
         USING RAVLNEL,R6                                                       
         CLI   RAVLNTYP,6          INTERESTED IN HPT UPGRADES OR                
         BE    ANYBK6                                                           
         CLI   RAVLNTYP,3          IN HUT/PUT                                   
         BNE   XIT                                                              
         CLI   RAVLNOP2,70         WHOSE 2'ND OPERAND IS A BOOK                 
         BL    XIT                                                              
         CLI   ANYOPT,C'P'                                                      
         BE    ANYBK6                                                           
         USING RINVKEY,R4                                                       
         MVC   RINVKBK,RAVLNOP2                                                 
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
         USING PRKEY,R4                                                         
ANYBK6   MVC   PRBOOK,RAVLNOP2                                                  
         B     XIT                                                              
         DROP  R4,R6                                                            
*                                                                               
*        DEMO SHARE DATA                                                        
*                                                                               
DEMOSHR  DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINES TO DIG OUT DEMO VALUES                                  
         SPACE 3                                                                
DIGDEM   NTR1                                                                   
*                                                                               
         L     R4,AIO2                                                          
         AH    R4,DATADISP                                                      
         BAS   RE,ANYUP                                                         
         L     R4,AIO2                                                          
         AH    R4,DATADISP                                                      
         MVC   ACTCODE,SPACES                                                   
         L     R6,AIO2                                                          
         MVI   ELCODE,X'CD'                                                     
         MVI   MULTSW,C'N'                                                      
         BAS   RE,GETEL                                                         
         BNE   DDEM10                                                           
*                                                                               
         USING RINVCEL,R6                                                       
         MVC   ACTCODE,RINVCODE                                                 
         OC    RINVCTXT,RINVCTXT                                                
         BZ    DDEM05                                                           
         MVC   COMPTEXT,RINVCTXT                                                
DDEM05   TM    RINVCTYP,X'80'                                                   
         BNO   DDEM10                                                           
         MVI   MULTSW,C'Y'                                                      
         DROP  R6                                                               
*                                                                               
DDEM10   EQU   *                                                                
         ICM   R6,15,AUPEL                                                      
         BNZ   DDEM15                                                           
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   DDEM15                                                           
         ST    R6,AUPEL                                                         
DDEM15   EQU   *                                                                
*                                                                               
         LA    R3,ACTDEMOS         LOOK UP BASIC NUMBERS                        
         MVI   LINSW,C'I'                                                       
         GOTO1 ADEMNUMS,DMCB,(RC)                                               
         CLI   BOOKNO,1                                                         
         BNE   DDEM40                                                           
         LA    R2,MANDEMOS         APPLY OVERRIDES                              
         ZIC   R0,CNUMDEM                                                       
*                                                                               
DDEM20   EQU   *                                                                
         OC    0(4,R2),0(R2)                                                    
         BZ    DDEM25                                                           
         MVC   0(4,R3),0(R2)                                                    
DDEM25   LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,DDEM20                                                        
*                                                                               
DDEM40   EQU   *                                                                
         XC    GRPS,GRPS           OPTION TO SHOW POINTS                        
         CLI   OPTD,C'Y'                                                        
         BNE   DDEM60                                                           
         MVI   LINSW,C'R'                                                       
         LA    R3,GRPS                                                          
         GOTO1 ADEMNUMS,DMCB,(RC)                                               
*                                                                               
DDEM60   EQU   *                                                                
         XC    SHARES,SHARES       SHARE AND                                    
         XC    LEVELS,LEVELS       H/P/T LINES                                  
         CLI   OPTC,C'Y'           ARE SUPPRESSED OPTIONALLY                    
         BE    YES                                                              
         CLI   MULTSW,C'Y'         OR ON COMBOS                                 
         BE    YES                                                              
         CLI   BOOKNO,1            OR ON BOOK 1                                 
         BNE   DDEM80                                                           
         OC    MANDEMOS,MANDEMOS      WHEN DEMOS ARE OVERRIDDEN                 
         BNZ   YES                                                              
*                                                                               
DDEM80   EQU   *                                                                
         CLC   ACTCODE,=C'PA'      IF PROGRAM CODE IS PA                        
         BE    DDEM100             PRINT SHARES AND LEVELS-DO RECALC            
*                                                                               
         OC    AUPEL,AUPEL         BUT SUPPRESS IF OTHER UPGRADES               
         BNZ   YES                                                              
         CLC   ACTCODE,=C'ES'      OR IF PROGRAM CODE IS ES                     
         BE    YES                                                              
         CLC   ACTCODE,=C'PE'      OR PE                                        
         BE    YES                                                              
         CLC   ACTCODE,=C'PJ'      OR PJ                                        
         BE    YES                                                              
*                                                                               
DDEM100  EQU   *                                                                
         MVI   LINSW,C'L'          GET LEVELS                                   
         LA    R3,LEVELS                                                        
         GOTO1 ADEMNUMS,DMCB,(RC)                                               
*                                                                               
         MVI   LINSW,C'S'                                                       
****     CLC   ACTCODE,=C'PA'      IF PROGRAM CODE IS PA, RECALCULATE           
****     BNE   DDEM140                                                          
****                                                                            
****120  BAS   RE,RECALC                                                        
****     B     DDEM160                                                          
*                                                                               
DDEM140  LA    R3,SHARES           OTHERWISE FIGURE THEM OUT                    
         GOTO1 ADEMNUMS,DMCB,(RC)                                               
*                                                                               
DDEM160  EQU   *                                                                
         CLI   OPTD,C'Y'                                                        
         BNE   YES                                                              
         MVI   LINSW,C'4'          AND LEVEL LINE FOR GRPS                      
         LA    R3,LEVELS+32                                                     
         GOTO1 ADEMNUMS,DMCB,(RC)                                               
*                                                                               
         MVI   LINSW,C'2'          SECOND SHARE                                 
         CLC   ACTCODE,=C'PA'      IF PROGRAM CODE IS PA, RECALCULATE           
         BNE   DDEM190                                                          
*                                                                               
DDEM180  BAS   RE,RECALC                                                        
         B     DDEM200                                                          
*                                                                               
DDEM190  LA    R3,SHARES+32        SECOND SHARE                                 
         GOTO1 ADEMNUMS,DMCB,(RC)                                               
DDEM200  B     YES                                                              
*                                                                               
*                                                                               
*                                                                               
NO       LA    R1,1                                                             
         B     *+6                                                              
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*  RECALCULATE SHARES FOR PA                                                    
*                (RATING X 1000)                                                
*                -------------- =  SHARE (THEN ROUND)                           
*                   LEVEL                                                       
         SPACE 2                                                                
RECALC   NTR1                                                                   
         CLI   LINSW,C'S'                                                       
         BNE   RC20                                                             
         ZIC   R4,CNUMDEM                                                       
         LA    R3,SHARES                                                        
         LA    RE,LEVELS                                                        
         LA    R5,ACTDEMOS                                                      
         B     RC30                                                             
*                                                                               
RC20     CLI   LINSW,C'2'          2ND SHARE AND LEVEL LINE FOR GRPS            
         BNE   RCX                                                              
         ZIC   R4,CNUMDEM                                                       
         LA    R3,SHARES+32                                                     
         LA    RE,LEVELS+32                                                     
         LA    R5,GRPS                                                          
*                                                                               
RC30     OC    0(4,RE),0(RE)       SKIP CALC IF LEVEL IS 0                      
         BZ    RC40                                                             
         L     R1,0(R5)            RATING                                       
         MH    R1,=H'1000'                                                      
         SR    R0,R0                                                            
         D     R0,0(RE)                                                         
*                          NOW ROUND                                            
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         ST    R1,0(R3)                                                         
*                                                                               
RC40     LA    R3,4(R3)                                                         
         LA    R5,4(R5)                                                         
         LA    RE,4(RE)                                                         
         BCT   R4,RC30                                                          
*                                                                               
RCX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL PRINTING                                      
         SPACE 3                                                                
SPLAT    NTR1                                                                   
*                                                                               
         L     RB,SAVEDRB                                                       
         L     R7,SAVED2RB                                                      
*                                                                               
         MVC   DMWORK(44),P2       SAVE LEFT HAND DATA                          
         MVC   DMWORK+44(44),P3                                                 
         MVC   P2(44),SPACES                                                    
         MVC   P3(44),SPACES                                                    
         MVI   SPACING,1           ALWAYS PRINT A SINGLE LINE                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(44),DMWORK        AND RESTORE LEFT HAND A LINE UP              
         MVC   P2(44),DMWORK+44                                                 
         ZIC   R2,MINLINES                                                      
         LTR   R2,R2                                                            
         BZ    XIT                                                              
         BCTR  R2,0                                                             
         STC   R2,MINLINES                                                      
         B     XIT                                                              
         EJECT                                                                  
HEDSPECS DS    0H                                                               
         SPACE 1                                                                
         SPROG 1                                                                
         SSPEC H10,86,PAGE                                                      
         SSPEC H13,1,C'DAY/TIME'                                                
         SSPEC H14,1,C'--------'                                                
         SSPEC H13,18,C'PROGRAM/ADJACENCY'                                      
         SSPEC H14,18,C'-----------------'                                      
         SSPEC H13,46,C'BOOK CODE'                                              
         SSPEC H14,46,C'---- ----'                                              
         DS    X'00'                                                            
         SPACE 3                                                                
HOOK     NTR1                                                                   
*                                                                               
         L     RB,SAVEDRB                                                       
         L     R7,SAVED2RB                                                      
*                                                                               
         CLI   RCSUBPRG,0                                                       
         BE    HOOKX                                                            
*                                                                               
         MVC   H1+28(L'ESALNAME),ESALNAME     SALESPERSON NAME                  
         MVC   H2+28(L'EOFFADD1),EOFFADD1     OFFICE ADDRESS                    
         MVC   H3+28(L'EOFFADD2),EOFFADD2                                       
         MVC   H3+48(L'EOFFSTT),EOFFSTT                                         
         GOTO1 SQUASHER,DMCB,H3+28,22                                           
         MVC   H4+28(L'EOFFZIP),EOFFZIP                                         
         MVC   H5+28(L'ESALTEL),ESALTEL       SALESPERSON TELEPHONE             
*                                                                               
         MVC   H1+73(L'ECONBUYR),ECONBUYR     BUYER NAME                        
         MVC   H2+73(L'EAGYNAM2),EAGYNAM2     AGENCY NAME                       
         MVC   H3+73(L'EAGYADD1),EAGYADD1     AND ADDRESS                       
         MVC   H4+73(L'EAGYADD2),EAGYADD2                                       
         MVC   H5+73(L'EAGYCITY),EAGYCITY                                       
         MVC   H5+95(L'EAGYSTAT),EAGYSTAT                                       
         GOTO1 SQUASHER,DMCB,H5+73,24                                           
         MVC   H6+73(L'EAGYZIP),EAGYZIP                                         
*                                                                               
         MVC   H7(L'HDLN7),HDLN7                                                
         MVC   H8(L'HDLN8),HDLN8                                                
*                                                                               
         MVC   H10(60),COMMENT                                                  
         MVC   H11(60),COMMENT+60                                               
*                                                                               
         MVC   H13+LINDATOF+1(L'DEMHD1),DEMHD1    MAKE DEMO HEADS               
         MVC   H14+LINDATOF+1(L'DEMHD2),DEMHD2    SEEM CENTERED                 
*                                                                               
HOOKX    EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
ERREND   GOTO1 MYERROR             DO A GETTXT CALL                             
         SPACE 2                                                                
RELO     DS    A                                                                
DMCOLWTH EQU   7               WIDTH OF A DEMO DATA COL(W/ 1 SPACE)             
LINDESOF EQU   49              SECONDARD DATA LINE CODE/LABEL OFFSET            
INVCODOF EQU   51              PRIMARY DATA LINE CODE OFFSET                    
LINDATOF EQU   55              ALL LINES DATA START OFFSET                      
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
* THIS ROUTINE SETS UP THE H7 AND H8 AND THE DEMO HEADLINES          *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
SETHEADS NMOD1 0,**SETHD*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         MVC   HDLN7(L'HDLN7),SPACES                                            
         MVC   HDLN8(L'HDLN8),SPACES                                            
         MVC   DEMHD1(L'DEMHD1),SPACES                                          
         MVC   DEMHD2(L'DEMHD2),SPACES                                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(8,HDLN7)   TODAY'S DATE                       
         MVI   HDLN8,C'#'                                                       
         MVC   HDLN8+1(L'ECONNUM),ECONNUM    CONTRACT NUMBER                    
         MVI   HDLN8+8,C'-'                                                     
         MVC   HDLN8+10(L'EHDRNUM),EHDRNUM   HEADER NUMBER                      
         GOTO1 SQUASHER,DMCB,HDLN8,12                                           
*                                                                               
         MVC   HDLN7+17(L'EADVNAME),EADVNAME ADVERTISER                         
         MVC   HDLN8+17(L'EPRDNAME),EPRDNAME PRODUCT                            
*                                                                               
*   AVLDATES IS CONTRACT DATES(DEFAULT) OR HEADER DATES (OVERRIDE)              
*                                                                               
         GOTO1 DATCON,DMCB,(2,AVLDATES),(8,HDLN7+38)                            
         MVI   HDLN7+47,C'-'                                                    
         GOTO1 DATCON,DMCB,(2,AVLDATES+2),(8,HDLN7+49)                          
         MVC   HDLN7+58(10),=C'- NN WEEKS'                                      
         EDIT  (1,WEEKS),(2,HDLN7+60)                                           
         CLI   WEEKS,1                                                          
         BNE   SETH10                                                           
         MVI   HDLN7+67,C' '                                                    
SETH10   GOTO1 SQUASHER,DMCB,HDLN7+38,32                                        
*                                                                               
         MVC   HDLN8+38(3),=C'ARB' SERVICE                                      
         CLI   CSOURCE,C'A'                                                     
         BE    SETH20                                                           
         MVC   HDLN8+38(3),=C'NSI'                                              
         CLI   CSOURCE,C'N'                                                     
         BE    SETH20                                                           
         MVC   HDLN8+38(3),=C'SRC'                                              
         CLI   CSOURCE,C'S'                                                     
         BE    SETH20                                                           
         MVC   HDLN8+38(3),SPACES                                               
SETH20   EQU   *                                                                
         CLI   CNUMBKS,0           MAY BE NO BOOK                               
         BE    SETH60                                                           
SETH30   EQU   *                                                                
         LA    R2,CBOOKS           HEADER BOOKS LIST                            
         LA    R3,HDLN8+42         BOOK PLACE IN HEADLINE                       
         LA    R4,HDR0B            BOOK NAME OVERRIDE LIST                      
         ZIC   R5,CNUMBKS                                                       
SETH35   EQU   *                                                                
         CLI   0(R4),0                                                          
         BE    SETH37                                                           
         CLI   0(R4),C' '                                                       
         BE    SETH37                                                           
*                                                                               
         MVC   0(5,R3),0(R4)       MOVE IN BOOK NAME OVERRIDE                   
         B     SETH57                                                           
SETH37   EQU   *                                                                
         L     RE,=A(SVCLST)                                                    
         A     RE,RELO                                                          
SETH40   EQU   *                                                                
         CLC   0(1,R2),3(RE)                                                    
         BE    SETH50                                                           
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   SETH40                                                           
         DC    H'0'                                                             
*                                                                               
SETH50   EQU   *                                                                
         CLI   1(RE),C' '                                                       
         BE    SETH55                                                           
         MVC   0(1,R3),1(RE)       PRECEDE WITH P,T,S OR E                      
         LA    R3,1(R3)                                                         
*                                                                               
SETH55   EQU   *                                                                
         SR    R1,R1                                                            
         IC    R1,2(R2)            NOW DO MONTH                                 
         MH    R1,=H'3'                                                         
         LA    R1,MONTH(R1)                                                     
         MVC   0(3,R3),0(R1)                                                    
         LA    R3,3(R3)                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R2)            AND YEAR                                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   0(2,R3),DUB+1                                                    
*                                                                               
SETH57   EQU   *                                                                
         CLI   0(R3),C' '                                                       
         BE    SETH58                                                           
         LA    R3,1(R3)                                                         
         B     SETH57                                                           
SETH58   EQU   *                                                                
         LA    R4,5(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R2,3(R2)                                                         
         BCT   R5,SETH35                                                        
*                                                                               
SETH60   EQU   *                                                                
         MVC   HDLN7+85(L'EMKTNAME),EMKTNAME    MARKET NAME                     
         MVC   HDLN8+85(L'ESTATION),ESTATION    STATION                         
         LA    R2,HDLN8+88                                                      
         CLI   0(R2),C' '                                                       
         BE    SETH65                                                           
         LA    R2,1(R2)                                                         
SETH65   EQU   *                                                                
         MVC   0(3,R2),=C'-TV'                                                  
         MVI   3(R2),C'/'                                                       
         MVC   4(3,R2),ESTAAFFL                 AFFILIATE                       
         CLC   ESTACHAN(2),=C'1 '                                               
         BE    SETH66                                                           
         MVI   7(R2),C'/'                                                       
         MVC   8(4,R2),ESTACHAN                 CHANNEL                         
SETH66   EQU   *                                                                
*                                                                               
         LA    R5,BLOCK                                                         
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         LA    R2,CDEMOS                                                        
         ZIC   R4,CNUMDEM                                                       
         LA    R3,DEMHD1                                                        
         CLI   CNUMDEM,0           SKIP DEMOCON IF NO DEMOS                     
         BE    SETH140                                                          
*                                                                               
SETH100  EQU   *                                                                
         CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   SETH110                                                          
         MVI   1(R2),C'I'                                                       
SETH110  EQU   *                                                                
         GOTO1 DEMOCON,DMCB,(R2),(5,WORK),(0,DBLOCK)                            
         CLI   1(R2),C'I'                                                       
         BNE   SETH120                                                          
         MVI   1(R2),C'T'                                                       
SETH120  EQU   *                                                                
         MVC   0(5,R3),WORK                                                     
         MVC   70(5,R3),WORK+5                                                  
         CLC   70(5,R3),=C'(IMP)'                                               
         BNE   SETH130                                                          
         MVC   70(5,R3),=C'(000)'                                               
SETH130  EQU   *                                                                
         LA    R2,3(R2)                                                         
         LA    R3,DMCOLWTH(R3)                                                  
         BCT   R4,SETH100                                                       
SETH140  EQU   *                                                                
*                                                                               
         CLI   NUMFORMS,0          AT THIS POINT WE CAN ONLY                    
         BE    SETH200              CHECK NUMBER OF FORMULAS                    
         BCTR  R3,0                                                             
         MVC   00(10,R3),=C'LEN   COST'                                         
         MVC   70(10,R3),=C'---   ----'                                         
*                           123 123456                                          
SETH200  EQU   *                                                                
         DROP  R5                                                               
*                                                                               
*        SETHEADS EXIT                                                          
*                                                                               
SETHEXIT EQU   *                                                                
         XIT1                                                                   
*                                                                               
*        MONTH TEXTS                                                            
*                                                                               
MONTH    DC    C'ESTJANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        THIS ROUTINE PRODUCES THE COVER SHEET                                  
*                                                                               
COVERSHT NMOD1 0,*CVRSHT*                                                       
         L     RC,0(R1)                                                         
*                                                                               
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
         GOTO1 DATCON,DMCB,(2,AVLDATES),(8,WORK)                                
         MVI   WORK+9,C'-'                                                      
         GOTO1 DATCON,DMCB,(2,AVLDATES+2),(8,WORK+11)                           
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
*                                                                               
*        FANCY BOX ROUTINE                                                      
*                                                                               
FANCY    EQU   *                                                                
         ST    RE,SAVERE                                                        
         OC    WORK(33),SPACES                                                  
         GOTO1 CENTER,DMCB,WORK,33                                              
         MVI   P+34,C'*'                                                        
         MVC   P+36(33),WORK                                                    
         MVI   P+68,C'*'                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   WORK(33),SPACES                                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        CPP -  ROUTINE TO EDIT CPP/CPM                                         
*                                                                               
CPP      NMOD1 0,***CPP**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         MVC   FULL,0(R3)                                                       
         OC    FULL,FULL                                                        
         BZ    CPPEXIT                                                          
         LA    R2,ACTDEMOS                                                      
         ST    R2,CPPHOLD                                                       
         LA    R2,MANDEMOS                                                      
         ST    R2,CPPHOLD+4                                                     
         LA    R3,P+LINDATOF                                                    
         LA    R4,MANCPM                                                        
         LA    R5,CDEMOS                                                        
         ZIC   R6,CNUMDEM                                                       
*                                                                               
CPP2     EQU   *                                                                
         L     R2,CPPHOLD+4                                                     
         OC    0(4,R2),0(R2)                                                    
         BNZ   CPP3                                                             
         L     R2,CPPHOLD                                                       
CPP3     EQU   *                                                                
         CLC   0(4,R2),=F'-1'                                                   
         BE    CPP8                                                             
         CLI   OPTG,C'Y'           OPT-G SAYS DO ALL CPP/CPM(S)                 
         BE    CPP3A                                                            
         TM    0(R5),X'20'         COLUMN INDICATED?                            
         BNO   CPP8                 NOT ON IS NO                                
CPP3A    EQU   *                                                                
         CLI   0(R4),C'N'          OR OVERRIDE CPM=NO                           
         BE    CPP8                                                             
         CLI   1(R5),C'T'          ONLY INTERESTED IN CPM                       
         BE    CPP4                                                             
         CLI   1(R5),C'R'          OR CPP                                       
         BE    CPP4                                                             
         CLI   1(R5),C'D'                                                       
         BE    CPP4                                                             
         CLI   1(R5),C'A'                                                       
         BNE   CPP8                                                             
*                                                                               
CPP4     EQU   *                                                                
         OC    0(4,R2),0(R2)                                                    
         BZ    CPP8                                                             
         L     R1,FULL             COMPUTE CPP/CPM                              
         CLI   PRTOPK,C'Y'                                                      
         BE    CPP5                                                             
         M     R0,=F'10'                                                        
CPP5     EQU   *                                                                
         M     R0,=F'200'                                                       
         D     R0,0(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    RF,R1                                                            
         EDIT  (RF),(6,(R3)),2,FLOAT=$                                          
         CH    RF,=H'10000'                                                     
         BL    CPP8                                                             
         LA    RF,50(RF)                                                        
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         EDIT  (RF),(6,(R3)),FLOAT=$                                            
*                                                                               
CPP8     EQU   *                                                                
         L     R2,CPPHOLD                                                       
         LA    R2,4(R2)                                                         
         ST    R2,CPPHOLD                                                       
         L     R2,CPPHOLD+4                                                     
         LA    R2,4(R2)                                                         
         ST    R2,CPPHOLD+4                                                     
         LA    R3,DMCOLWTH(R3)                                                  
         LA    R4,1(R4)                                                         
         LA    R5,3(R5)                                                         
         BCT   R6,CPP2                                                          
*                                                                               
         XC    MANDEMOS,MANDEMOS                                                
*                                                                               
CPPEXIT  EQU   *                                                                
         XIT1                                                                   
         SPACE 3                                                                
         DS    0F                                                               
CPPHOLD  DS    2F                                                               
         EJECT                                                                  
*                                                                               
*        OUTPUT BOOK EXPRESSION                                                 
*                                                                               
BOOKOUT  NMOD1 0,**BKOUT*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         ZIC   R2,BOOKNO                                                        
         BCTR  R2,0                                                             
         LR    R3,R2                                                            
         MH    R2,=H'3'                                                         
         MH    R3,=H'5'                                                         
         LA    R4,BOOKLIST(R2)                                                  
         LA    R5,USE0B(R3)                                                     
*                                                                               
         CLI   0(R5),0                                                          
         BE    BOUT10                                                           
         CLI   0(R5),C' '                                                       
         BE    BOUT10                                                           
         MVC   P+45(5),0(R5)       LOAD BOOK LABEL                              
         B     BOUT90                                                           
*                                                                               
BOUT10   EQU   *                                                                
         ZIC   R1,2(R4)                                                         
         MH    R1,=H'3'                                                         
         L     R0,=A(MONTH)                                                     
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
BOUT90   EQU   *                                                                
         CLI   RATESW,C'Y'         SHOW LEN IF NOT COMING LATER                 
         BNE   BOUTEXIT                                                         
         OC    AVLRATES(24),AVLRATES                                            
         BNZ   BOUTEXIT                                                         
         ZIC   R2,DFORMS+1                                                      
         LTR   R2,R2                                                            
         BNZ   BOUT95                                                           
         ZIC   R2,FORMULAS+1                                                    
BOUT95   EQU   *                                                                
         ZIC   R3,CNUMDEM                                                       
         LA    R0,DMCOLWTH                                                      
         STH   R0,HALF                                                          
         MH    R3,HALF                                                          
         LA    R3,P+LINDATOF(R3)                                                
         EDIT  (R2),(3,0(R3))                                                   
*                                                                               
*        BOOKOUT EXIT                                                           
*                                                                               
BOUTEXIT EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO EDIT DEMOS                                            
         SPACE 2                                                                
EDITDEMO NMOD1 0,*EDDEMO*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    R2,ACTDEMOS                                                      
         CLI   LINSW,C'I'          BASIC LINE                                   
         BE    ED40                YES (CH FROM ED20 08/29/89  PJS)             
*                                                                               
         MVC   P+LINDESOF(4),=C'RTG.'    DESCRIBE LINE TYPE                     
         LA    R2,GRPS                                                          
         CLI   LINSW,C'R'                                                       
         BE    ED20                                                             
*                                                                               
         MVC   P+LINDESOF(5),=C'SHARE'                                          
         CLI   OPTD,C'Y'                                                        
         BNE   ED10                                                             
         MVC   P+LINDESOF(5),=C'T-SHR'                                          
ED10     EQU   *                                                                
         LA    R2,SHARES                                                        
         CLI   LINSW,C'S'                                                       
         BE    ED20                                                             
*                                                                               
         MVC   P+LINDESOF(5),=C'H/P/T'                                          
         CLI   OPTD,C'Y'                                                        
         BNE   ED15                                                             
         MVC   P+LINDESOF(5),=C'TOTAL'                                          
ED15     EQU   *                                                                
         LA    R2,LEVELS                                                        
         CLI   LINSW,C'L'                                                       
         BE    ED20                                                             
         MVC   P+LINDESOF(5),=C'SHARE'                                          
         LA    R2,SHARES+32                                                     
         CLI   LINSW,C'2'                                                       
         BE    ED20                                                             
         MVC   P+LINDESOF(5),=C'HT/PT'                                          
         LA    R2,LEVELS+32                                                     
*                                                                               
ED20     EQU   *                                                                
         OC    0(32,R2),0(R2)                                                   
         BNZ   ED40                                                             
         MVC   P+LINDESOF(5),SPACES                                             
         B     EDEXIT                                                           
*                                                                               
ED40     EQU   *                                                                
         LA    R3,P+LINDATOF                                                    
         ZIC   R4,CNUMDEM                                                       
*                                                                               
ED50     EQU   *                                                                
         OC    0(4,R2),0(R2)                                                    
         BZ    ED55                                                             
         CLC   0(4,R2),=F'-1'                                                   
         BNE   ED51                                                             
         XC    0(4,R2),0(R2)                                                    
ED51     EQU   *                                                                
         CLI   PRTOPK,C'Y'                                                      
         BE    ED52                                                             
         EDIT  (4,(R2)),(6,(R3)),1                                              
         B     ED53                                                             
ED52     EQU   *                                                                
         EDIT  (4,(R2)),(6,(R3))                                                
ED53     EQU   *                                                                
         CLI   LINSW,C'S'                                                       
         BNE   ED55                                                             
         MVI   6(R3),C'%'          PERCENT AFTER SHARE                          
ED55     EQU   *                                                                
         LA    R2,4(R2)                                                         
         LA    R3,DMCOLWTH(R3)                                                  
         BCT   R4,ED50                                                          
*                                                                               
         BAS   RE,SPLAT                                                         
*                                                                               
EDEXIT   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE GETS ACTUAL NUMBERS                                      
         SPACE 2                                                                
* AT ENTRY, R3 POINTS TO OUTPUT AREA, R4 POINTS TO FIRST ELEMENT ON             
* INVENTORY RECORD                                                              
*                                                                               
DEMNUMS  NMOD1 0,*DEMNUM*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         BAS   RE,EFFDEMS                                                       
*                                                                               
         LA    R2,EDEMOS                                                        
         ZIC   R1,CNUMDEM                                                       
         LR    R0,R1               SET NUMBER OF DEMOS AS COUNTER               
         MH    R1,=H'3'                                                         
         LA    R1,0(R2,R1)         POINT TO END OF DEMO LIST                    
         MVI   0(R1),X'FF'         SET EOL MARKER FOR DEMOUT                    
*                                                                               
         LA    R5,BLOCK                                                         
         USING DBLOCK,R5                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         L     R4,AIO2                                                          
         ST    R4,DBAREC                                                        
         AH    R4,DATADISP                                                      
         ST    R4,DBAQUART                                                      
         MVC   DBSELAGY,AGENCY                                                  
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         CLI   PRTOPK,C'Y'        'Y' MEANS WHOLE NUMBER                        
         BE    DEMN05A                                                          
         MVI   DBXTTRP,X'01'      RTG = 1 DECIMAL                               
         MVI   DBXTTSP,X'01'      SHR = 1 DECIMAL                               
         MVI   DBXTTIP,X'02'      IMP = 00'S                                    
         B     DEMN05B                                                          
DEMN05A  EQU   *                                                                
         MVI   DBXTTRP,X'00'      RTG = 0 DECIMAL                               
         MVI   DBXTTSP,X'00'      SHR = 0 DECIMAL                               
         MVI   DBXTTIP,X'03'      IMP = 000'S                                   
DEMN05B  EQU   *                                                                
         DROP  R1                                                               
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CDEMOUT                                                       
         GOTO1 (RF),DMCB,(C'L',(R2)),DBLOCK,(R3)                                
         CLI   DBERROR,0                                                        
         BE    DEMN10                                                           
         DC    H'0'                                                             
*                                                                               
DEMN10   EQU   *                                                                
         CLI   1(R2),C'-'                                                       
         BE    DEMN60                                                           
         CLI   MULTSW,C'Y'         IF MULTI-PROGRAM INVENTORY,                  
         BNE   DEMN60                                                           
         CLI   1(R2),C'P'          DONT SHOW HUTS/PUTS                          
         BNE   DEMN20                                                           
         XC    0(4,R3),0(R3)                                                    
DEMN20   CLI   1(R2),C'S'          OR SHARES                                    
         BNE   DEMN30                                                           
         XC    0(4,R3),0(R3)                                                    
DEMN30   CLC   1(2,R2),=X'C301'    OR CHARE                                     
         BNE   DEMN40                                                           
         XC    0(4,R3),0(R3)                                                    
DEMN40   CLC   1(2,R2),=X'E701'    OR TSHARE                                    
         BNE   DEMN60                                                           
         XC    0(4,R3),0(R3)                                                    
*                                                                               
DEMN60   EQU   *                                                                
         LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,DEMN10                                                        
*                                                                               
DEMNEXIT EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R5,RE                                                            
         EJECT                                                                  
*              WORK OUT EFFECTIVE DEMO TYPES FOR DIFFERENT LINES                
         SPACE 3                                                                
EFFDEMS  NTR1                                                                   
         MVC   EDEMOS,CDEMOS                                                    
         CLI   LINSW,C'I'                                                       
         BE    DEMNEXIT                                                         
         LA    R3,EDEMOS                                                        
         ZIC   R0,CNUMDEM                                                       
         SPACE 1                                                                
EFF2     MVC   WORK(1),1(R3)       COLUMN TYPE                                  
         MVC   WORK+1(1),LINSW     LINE TYPE                                    
         LA    R1,EFFTABLE                                                      
         SPACE 1                                                                
EFF4     CLI   0(R1),X'FF'                                                      
         BE    EFF6                                                             
         CLC   WORK(2),0(R1)                                                    
         BE    EFF6                                                             
         LA    R1,3(R1)                                                         
         B     EFF4                                                             
         SPACE 1                                                                
EFF6     MVC   1(1,R3),2(R1)       SUBSTITUTE TYPE                              
         LA    R3,3(R3)                                                         
         BCT   R0,EFF2                                                          
         B     DEMNEXIT                                                         
         SPACE 1                                                                
EFFTABLE DS    0H                                                               
         DC    C'TRR'                                                           
         DC    C'TSX'                                                           
         DC    C'TLQ'                                                           
         DC    C'RSS'                                                           
         DC    C'RLP'                                                           
         DC    C'T2S'                                                           
         DC    C'T4P'                                                           
         DC    X'FFFF',C'-'                                                     
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINE TO MERGE HEADER AND DETAIL BOOK LISTS         *          
*--------------------------------------------------------------------*          
         SPACE 3                                                                
MERGEBKS NMOD1 0,*MRGBKS*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         XC    BOOKLIST,BOOKLIST   CLEAR BOOKLIST AND RENAME LIST               
         XC    USE0B,USE0B                                                      
         MVI   HDRUP1FG,0                                                       
         MVI   HDRUP2FG,0                                                       
*                                                                               
         MVI   BOOKNO,1                                                         
         MVI   NUMBKS,0                                                         
*                                                                               
         BAS   RE,SETDET0B                                                      
*                                                                               
*        GET TO AVAIL DETAIL LINE MAIN ELEMENT                                  
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,RAVLELEM-RAVLREC(R6)                                          
MBKS05   EQU   *                                                                
         CLI   0(R6),X'01'                                                      
         BE    MBKS10                                                           
         CLI   0(R6),0                                                          
         BE    MBKSEXIT                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     MBKS05                                                           
MBKS10   EQU   *                                                                
         USING RAVLDEL,R6                                                       
*                                                                               
         CLC   =C'NONE',RAVLDBKS   NO BOOKS FOR THIS DETAIL ITEM                
         BE    MBKSEXIT                                                         
*                                                                               
         CLI   CNUMBKS,0           NO HEADER BOOKS                              
         BE    MBKS50              TRY LOADING DETAIL                           
*                                                                               
         CLC   RAVLDBKS(3),=X'000000'                                           
         BE    MBKS20              NO DETAIL BOOKS, LOAD HEADER'S               
*                                                                               
         CLI   RAVLDBKF,0          BOOK FILTER OR MODIFIER                      
         BNE   MBKS20              NON-ZERO IS TAKE HEAD'S LIST                 
         MVI   HDRUP1FG,1          SKIP HEADER UPGRADE 1                        
         MVI   HDRUP2FG,1          SKIP HEADER UPGRADE 2                        
         B     MBKS50              AND ONLY TAKE THE DETAIL LIST                
*                                                                               
MBKS20   EQU   *                                                                
         BAS   RE,MBKSHEAD         LOAD HEADER'S BOOKS                          
         BAS   RE,MBKSREM          REMOVE -'ED BOOKS                            
         BAS   RE,MBKSUP           CHECK HEADER UPGRADES                        
         BAS   RE,MBKSCOM          COMPRESS BOOK LIST                           
MBKS50   EQU   *                                                                
         BAS   RE,MBKSPLUS         LOAD DETAIL'S BOOKS                          
*                                                                               
*        MERGEBOOKS EXIT                                                        
*                                                                               
MBKSEXIT EQU   *                                                                
         XIT1                                                                   
         SPACE 3                                                                
*                                                                               
*        MBKSHEAD --- LOAD HEADER'S BOOKS AND RENAMES                           
*                                                                               
MBKSHEAD EQU   *                                                                
*                                                                               
         LA    R1,CBOOKS                                                        
         LA    R2,BOOKLIST                                                      
         LA    R3,HDR0B                                                         
         LA    R4,USE0B                                                         
*                                                                               
         ZIC   R0,CNUMBKS                                                       
         STC   R0,NUMBKS                                                        
MBKSH10  EQU   *                                                                
         MVC   0(3,R2),0(R1)                                                    
         MVC   0(5,R4),0(R3)                                                    
         LA    R1,3(R1)                                                         
         LA    R2,3(R2)                                                         
         LA    R3,5(R3)                                                         
         LA    R4,5(R4)                                                         
         BCT   R0,MBKSH10                                                       
*                                                                               
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
*        REMOVE -'ED BOOKS FROM BOOKLIST AND RENAME LIST                        
*                                                                               
MBKSREM  EQU   *                                                                
*                                                                               
         LA    R3,RAVLDBKS                                                      
         LA    R4,RAVLDBKF                                                      
         LA    R5,WORK                                                          
         MVC   FULL(1),NUMBKS      SAVE NUMBER OF BOOKS                         
*                                                                               
         LA    RF,6                LOOP FOR 6 DETAIL BOOKS                      
MBKSR10  EQU   *                                                                
         CLI   0(R4),C'-'                                                       
         BNE   MBKSR60                                                          
         CLC   0(3,R3),=X'000000'                                               
         BE    MBKSR60                                                          
         ZIC   R0,NUMBKS           LOOP THRU HEADER'S BOOKS                     
         LA    R1,BOOKLIST                                                      
         LA    R2,USE0B                                                         
MBKSR20  EQU   *                                                                
         CLC   0(3,R1),0(R3)                                                    
         BE    MBKSR30                                                          
         CLC   1(2,R1),1(R3)                                                    
         BNE   MBKSR40                                                          
         MVC   FULL+1(1),0(R1)                                                  
         OI    FULL+1,X'80'                                                     
         MVC   FULL+2(1),0(R3)                                                  
         OI    FULL+2,X'80'                                                     
         CLC   FULL+1(1),FULL+2                                                 
         BNE   MBKSR40                                                          
MBKSR30  EQU   *                                                                
         CLC   0(5,R2),0(R5)                                                    
         BE    MBKSR50                                                          
         OC    0(5,R5),0(R5)                                                    
         BNZ   MBKSR40                                                          
         CLC   0(5,R2),SPACES                                                   
         BE    MBKSR50                                                          
MBKSR40  EQU   *                                                                
         LA    R1,3(R1)                                                         
         LA    R2,5(R2)                                                         
         BCT   R0,MBKSR20                                                       
         B     MBKSR60             -'ED BOOK NO LONGER IN HEADER                
MBKSR50  EQU   *                                                                
         MVC   0(3,R1),=X'000000'      ZERO -'ED BOOK, COMPRESS LATER           
         MVC   0(5,R2),=X'0000000000'   ALSO CLEAR RENAME                       
         ZIC   R1,NUMBKS                                                        
         BCTR  R1,0                                                             
         STC   R1,NUMBKS           REDUCE BOOK COUNT                            
MBKSR60  EQU   *                                                                
         LA    R3,3(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,5(R5)                                                         
         BCT   RF,MBKSR10          LOOP                                         
*                                                                               
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
*        CHECK HEADER UPGRADES FOR REMOVAL                                      
*                                                                               
MBKSUP   EQU   *                                                                
*                                                                               
         LA    R1,HDRUP1FG                                                      
         LA    R2,HDRPJ                                                         
         LA    R3,2                CHECK FOR 2 UPGRADES                         
MBKSUP10 EQU   *                                                                
         CLI   0(R2),0                                                          
         BE    MBKSUP50                                                         
         LA    R4,BOOKLIST                                                      
         LA    R5,X'80'                                                         
         STC   R5,0(R1)                                                         
MBKSUP20 EQU   *                                                                
         CLC   0(3,R4),=X'000000'                                               
         BNE   MBKSUP30                                                         
         ZIC   R0,2(R2)                                                         
         CR    R0,R5                                                            
         BE    MBKSUP50            FOUND IT, LEAVE NON-ZERO                     
MBKSUP30 EQU   *                                                                
         SRL   R5,1                                                             
         STC   R5,0(R1)                                                         
         LA    R4,3(R4)                                                         
         CLI   0(R1),X'02'                                                      
         BNE   MBKSUP20            LOOP TO NEXT BOOK                            
         MVI   0(R1),0             CHECKED ALL, CLEAR FLAG                      
MBKSUP50 EQU   *                                                                
         LA    R1,1(R1)                                                         
         LA    R2,14(R2)                                                        
         BCT   R3,MBKSUP10         LOOP FOR 2 UPGRADES                          
*                                                                               
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
*        COMPRESS BOOKLIST AFTER '-' OPERATION                                  
*                                                                               
MBKSCOM  EQU   *                                                                
         LA    R1,BOOKLIST                                                      
         LA    R2,USE0B                                                         
*                                                                               
         ZIC   RF,FULL              USE SAVED LONGER LIST NUMBER                
         LTR   RF,RF                                                            
         BNZ   MBKSC10                                                          
         BR    RE                                                               
*                                                                               
MBKSC10  EQU   *                                                                
         CLC   0(3,R1),=X'000000'                                               
         BNE   MBKSC30                                                          
         LR    R3,RF                                                            
         LR    R4,R1                                                            
         LR    R5,R2                                                            
MBKSC20  EQU   *                                                                
         MVC   0(3,R4),3(R4)                                                    
         MVC   0(5,R5),5(R5)                                                    
         LA    R4,3(R4)                                                         
         LA    R5,5(R5)                                                         
         BCT   R3,MBKSC20                                                       
         MVC   0(3,R4),=X'000000'                                               
         MVC   0(5,R5),=X'0000000000'                                           
MBKSC30  EQU   *                                                                
         LA    R1,3(R1)                                                         
         LA    R2,5(R2)                                                         
         BCT   RF,MBKSC10                                                       
*                                                                               
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
*        ADD IN +'ED IN BOOKS OR JUST LOAD DETAIL'S BOOKS                       
*                                                                               
MBKSPLUS EQU   *                                                                
         LA    R1,BOOKLIST                                                      
         LA    R2,USE0B                                                         
MBKSP10  EQU   *                                                                
         CLC   0(3,R1),=X'000000'  PUT R1 AT FIRST OPEN SPOT IN LIST            
         BE    MBKSP20                                                          
         LA    R1,3(R1)                                                         
         LA    R2,5(R2)                                                         
         B     MBKSP10                                                          
MBKSP20  EQU   *                                                                
*                                                                               
         LA    R3,RAVLDBKS                                                      
         LA    R4,RAVLDBKF                                                      
         LA    R5,WORK                                                          
         LA    R0,6                                                             
MBKSP30  EQU   *                                                                
         CLI   0(R4),C'+'                                                       
         BE    MBKSP35                                                          
         CLI   0(R4),0                                                          
         BNE   MBKSP40                                                          
MBKSP35  EQU   *                                                                
         CLC   0(3,R3),=X'000000'                                               
         BE    MBKSP40                                                          
*                                                                               
*  PLUS'D IN BOOK CONTAINS BIT SETTINGS REPRESENTING THE SERVICE                
*    SPECIFIED IN THE ORDER HEADER RATHER THAN IN THE SAR.  THIS                
*    MUST BE RESET SO THAT THE DEMO TRACK(S) RETRIEVED FOR THE                  
*    PLUS'D IN BOOK COME FROM THE CORRECT SERVICE.                              
*                                                                               
         MVC   0(3,R1),0(R3)       ADD PLUS'D IN BOOK TO LIST                   
         NI    0(R1),X'BE'         TURN OFF SERVICE AND                         
*                                     SPANISH DEMOS BITS                        
         CLI   CSOURCE,C'A'        ARBITRON?                                    
         BE    MBKSP39             YES - NO BITS SET                            
         OI    0(R1),X'40'         NO  - SET SERV = NSI OR SRC                  
         CLI   CSOURCE,C'N'        NSI?                                         
         BE    MBKSP39             YES - X'40' IS SUFFICIENT                    
         OI    0(R1),X'41'         NO  - MUST BE SRC - SET                      
*                                    SPANISH DEMO BIT                           
MBKSP39  EQU   *                                                                
         MVC   0(5,R2),0(R5)                                                    
         LA    R1,3(R1)                                                         
         LA    R2,5(R2)                                                         
         ZIC   RF,NUMBKS                                                        
         LA    RF,1(RF)                                                         
         STC   RF,NUMBKS                                                        
MBKSP40  EQU   *                                                                
         LA    R3,3(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,5(R5)                                                         
         BCT   R0,MBKSP30                                                       
*                                                                               
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
*  SETDET0B --- MOVE THIS DETAIL RECORD'S RENAME ELEMENT                        
*                INTO WORK                                                      
*                                                                               
SETDET0B EQU   *                                                                
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         L     RF,AIO1                                                          
         LA    RF,RAVLELEM-RAVLREC(RF)                                          
SD0B10   EQU   *                                                                
         CLI   0(RF),X'0B'                                                      
         BE    SD0B20                                                           
         CLI   0(RF),0                                                          
         BE    SD0BEXIT                                                         
         ZIC   R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     SD0B10                                                           
*                                                                               
SD0B20   EQU   *                                                                
         ZIC   R1,1(RF)                                                         
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,SD0B30                                                        
         B     SD0BEXIT                                                         
SD0B30   MVC   WORK(0),2(RF)                                                    
*                                                                               
SD0BEXIT EQU   *                                                                
         BR    RE                                                               
         SPACE 3                                                                
         DROP  R6                                                               
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
INVDTP   NMOD1 0,*INVDTP*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R6,AIO2             INVENTORY RECORD                             
         USING RINVKEY,R6                                                       
         MVC   DYTM+32(16),SPACES                                               
         CLI   OPTI,C'Y'           Y=PRINT TEXT AND INV. NO.                    
         BNE   ID10                                                             
         MVC   DYTM+32(2),=C'A='                                                
         EDIT  (1,XDETNUM),(3,DYTM+34),ALIGN=LEFT                               
         MVC   DYTM+40(2),=C'I='                                                
         EDIT  (1,RINVKQTR),(2,DYTM+42),FILL=0                                  
         MVC   DYTM+44(2),RINVKDAY                                              
         CLI   DYTM+45,C'0'                                                     
         BNE   ID10                                                             
         MVI   DYTM+45,C' '                                                     
         DROP  R6                                                               
*                                                                               
ID10     EQU   *                                                                
         LA    R6,34(R6)                                                        
ID11     EQU   *                                                                
         CLI   0(R6),X'01'                                                      
         BE    ID15                                                             
         CLI   0(R6),X'00'                                                      
         BNE   ID12                                                             
         DC    H'0'                                                             
ID12     EQU   *                                                                
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ID11                                                             
ID15     EQU   *                                                                
         USING RINVPEL,R6                                                       
         MVC   DYTM(32),SPACES               DAY/TIME                           
         OC    RINVPADY,RINVPADY   IS THERE AVAIL DAY OVERRIDE                  
         BZ    ID20                                                             
         GOTO1 UNDAY,DMCB,RINVPADY,DYTM      YES, USE IT                        
         B     ID30                                                             
ID20     EQU   *                                                                
         OC    RINVPDAY,RINVPDAY                                                
         BZ    ID30                                                             
         GOTO1 UNDAY,DMCB,RINVPDAY,DYTM                                         
*                                                                               
ID30     EQU   *                                                                
         OC    RINVPATM,RINVPATM   IS THERE AVAIL TIME OVERRIDE                 
         BZ    ID40                                                             
         GOTO1 UNTIME,DMCB,RINVPATM,DYTM+16     YES, USE IT                     
         B     ID50                                                             
ID40     EQU   *                                                                
         GOTO1 UNTIME,DMCB,RINVPTIM,DYTM+16                                     
*                                                                               
ID50     EQU   *                                                                
         OC    DYTM,SPACES                                                      
*                                                                               
         ZIC   R5,RINVPLEN                   PROGRAM                            
         SH    R5,=H'40'                                                        
         GOTO1 CHOPPER,DMCB,((R5),RINVPROG),(27,P+17),(C'P',3)                  
*                                                                               
         MVC   WORK(15),=C'(ENDS MMMDD/YY)'                                     
         OC    RINVPEFF+2(2),RINVPEFF+2      IF INVENTORY ENDS BEFORE           
         BZ    ID60                             START OF CONTRACT               
         CLC   RINVPEFF+2(2),AVLDATES           IT DOESN'T QUALIFY              
         BL    IDNO                                                             
*                                                                               
ID60     EQU   *                                                                
         OC    RINVPEFF(2),RINVPEFF          IF INVENTORY STARTS AFTER          
         BZ    ID70                             END OF CONTRACT                 
         CLC   RINVPEFF(2),AVLDATES+2           IT DOESN'T QUALIFY              
         BH    IDNO                                                             
*                                                                               
ID70     EQU   *                                                                
         CLC   RINVPEFF(2),RINVPEFF+2        IF START=END ITS AN ONLY           
         BE    ID80                                                             
         CLC   RINVPEFF(2),AVLDATES          IF INVENTORY STARTS AFTER          
         BH    ID90                             CONTRACT, ITS A FROM            
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    IDEXIT                                                           
         CLC   RINVPEFF+2(2),AVLDATES+2      IF INVENTORY ENDS BEFORE           
         BNL   IDEXIT                           CONTRACT, ITS AN ENDS           
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(8,WORK+6)                            
         B     ID100                                                            
*                                                                               
ID80     EQU   *                                                                
         MVC   WORK+9(5),=C' ONLY'                                              
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(8,WORK+1)                              
         B     ID100                                                            
*                                                                               
ID90     EQU   *                                                                
         MVC   WORK+1(4),=C'EFF.'                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(8,WORK+6)                              
*                                                                               
ID100    EQU   *                                                                
         MVC   P3(15),WORK                                                      
         B     IDEXIT                                                           
*                                                                               
IDNO     EQU   *                                                                
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
*                                                                               
*        INVDTP EXIT                                                            
*                                                                               
IDEXIT   EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        FORMTOP --- FORCE TOP-OF-FORM IF WE NEED TO FOR THIS AVAIL             
*                     DETAIL LINE                                               
*                                                                               
*        NUMBER OF LINES NEEDED FOR THIS AVAIL DETAIL ITEM =                    
*                                                                               
*        MANUAL ITEM NEEDS 3 LINE                                               
*                                                                               
*        PURE / TP / PAV / INVENTORY:                                           
*                                                                               
*        (NUMBER-OF-BOOKS * (2 OR 4(WITH SHARE AND H/P/T)) +                    
*        (NUMBER-OF-BOOKS-MARKED-AS-$ * NUMBER-OF-COSTS/SPOTLENS) +             
*        (NUMBER-OF-BOOKS-THAT-WILL-HAVE-AN-UPGRADE-COMMENT)                    
*                                                                               
*        WHERE NUMBER-OF-BOOKS IS THE NUMBER OF BOOKS THAT ACTUALLY             
*        HAVE DATA                                                              
*                                                                               
FORMTOP  NMOD1 0,*FRMTOP*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         CLI   INVNO+2,0                                                        
         BNE   FTOP05                                                           
         LA    R1,1                                                             
         B     FTOP100                                                          
*                                                                               
FTOP05   EQU   *                                                                
         SR    R5,R5                                                            
         ZIC   R4,NUMBKS                                                        
         LTR   R4,R4                                                            
         BZ    FTOP20                                                           
         MVI   WORK,0                                                           
FTOP10   EQU   *                                                                
         BAS   RE,FTOPGET          DATA REC FOR THIS INV/BOOK?                  
         BNZ   FTOP15              NON-ZERO IS NO                               
         LA    R5,2(R5)                                                         
         CLI   WORK,0              WORK=0 IS EST/PROJ BOOK                      
         BNE   FTOP15                                                           
         LA    R5,2(R5)                                                         
FTOP15   EQU   *                                                                
         CLI   OPTC,C'Y'           SUPPRESS SHARE AND H/P/T                     
         BE    FTOP16                                                           
         LA    R5,2(R5)                                                         
FTOP16   EQU   *                                                                
         BCT   R4,FTOP10           R1 HAS NUMBER OF BOOK DETAIL LINES           
FTOP20   EQU   *                                                                
         LR    R1,R5                                                            
*                                                                               
         SR    R2,R2                                                            
         CLI   OPTE,C'Y'           SUPPRESS RATES, ET AL                        
         BE    FTOP80              YES, SKIP THIS SECTION                       
         ZIC   R3,NUMBKS                                                        
         LTR   R3,R3                                                            
         BZ    FTOP70                                                           
         LA    R4,BOOKLIST                                                      
FTOP50   EQU   *                                                                
         TM    0(R4),X'80'                                                      
         BO    FTOP60                                                           
         LA    R2,1(R2)                                                         
FTOP60   EQU   *                                                                
         LA    R4,3(R4)                                                         
         BCT   R3,FTOP50           R2 HAS NUMBER OF COST BOOKS                  
         ZIC   R3,NUMFORMS                                                      
         STH   R3,DUB                                                           
         MH    R2,DUB              R2 HAS NUMBER OF COST LINES                  
FTOP70   EQU   *                                                                
         LTR   R2,R2               IF STILL ZERO AT THIS POINT CHECK            
         BNZ   FTOP80              NUMBER OF RATES ON THIS DETAIL               
*                                                                               
         LA    R3,AVLRATES                                                      
         LA    R4,4                                                             
FTOP75   EQU   *                                                                
         OC    0(4,R3),0(R3)                                                    
         BZ    FTOP80                                                           
         CLC   0(4,R3),SPACES                                                   
         BE    FTOP80                                                           
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,FTOP75                                                        
FTOP80   EQU   *                                                                
*                                                                               
         SR    R3,R3                                                            
*                                  DETERMIN NUMBER OF UPGRADE COMMENTS          
         MH    R3,=H'2'            R3 HAS NUMBER OF UPGRADE LINES               
FTOP100  EQU   *                                                                
*                                                                               
         AR    R1,R2               ADD PARTS                                    
         AR    R1,R3                                                            
         LA    R1,2(R1)            TWO LINE SEPERATOR                           
         STC   R1,ALLOWLIN                                                      
         ZIC   R0,LINE             SEE IF WE NEED NEW PAGE                      
         AR    R1,R0                                                            
         ZIC   R0,MAXLINES                                                      
         CR    R0,R1                                                            
         BH    FTOP120                                                          
         MVI   FORCEHED,C'Y'                                                    
         LA    R1,1                                                             
         STC   R1,ALLOWLIN                                                      
FTOP120  EQU   *                                                                
*                                                                               
*        FORMTOP EXIT                                                           
*                                                                               
FTOPEXIT EQU   *                                                                
         XIT1                                                                   
         SPACE 3                                                                
*                                                                               
*        FTOPGET --- SEE IF THERE IS A 'DATA' RECORD TO PRINT                   
*                                                                               
FTOPGET  NTR1                                                                   
*                                                                               
         MVC   ELEM(27),KEY                                                     
         XC    KEY(27),KEY                                                      
*                                                                               
         MVI   WORK,0              ZERO IS NO SHARES+H/P/T LINES                
*                                                                               
         CLI   INVNO+2,0                                                        
         BE    FGET100             PURE ITEM                                    
*        --- INVENTORY ITEM                                                     
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP                                                 
         MVC   RINVKSTA,CCONKSTA                                                
         MVI   RINVKSTA+4,C'T'                                                  
         CLI   INVSAT,0                                                         
         BE    FGET50                                                           
         MVC   RINVKSTA+4(1),INVSAT                                             
FGET50   EQU   *                                                                
         MVC   RINVKINV,INVNO                                                   
         MVC   RINVKSTD,INVDT                                                   
FGET60   EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BNE   FGETBAD                                                          
         OC    INVDT,INVDT                                                      
         BZ    FGET70                                                           
         CLC   INVDT,RINVKSTD                                                   
         BE    FGET70                                                           
         MVC   RINVKSRC(3),=X'FFFFFF'                                           
         B     FGET60                                                           
FGET70   EQU   *                                                                
         LR    R2,R4                                                            
         BCTR  R2,0                                                             
         MH    R2,=H'3'                                                         
         LA    R2,BOOKLIST(R2)                                                  
         CLC   0(3,R2),=X'000000'                                               
         BE    FGETBAD                                                          
         CLC   0(3,R2),SPACES                                                   
         BE    FGETBAD                                                          
         MVC   RINVKSRC(3),0(R2)                                                
         L     RE,=A(SVCLST)                                                    
         A     RE,RELO                                                          
FGET80   EQU   *                                                                
         CLC   3(1,RE),0(R2)                                                    
         BE    FGET85                                                           
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   FGET80                                                           
         DC    H'0'                                                             
FGET85   EQU   *                                                                
         MVC   RINVKSRC(1),2(RE)                                                
         DROP  R6                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   FGETBAD                                                          
         LA    R3,FGETITAB                                                      
         LA    R0,6                                                             
FGET90   EQU   *                                                                
         CLC   2(1,RE),0(R3)       PROJ OR EST TRACK                            
         BE    FGET100             YES, GET OUT                                 
         LA    R3,1(R3)                                                         
         BCT   R0,FGET90                                                        
         L     R5,AIO                                                           
         L     R6,AIO2                                                          
         MVC   AIO,AIO2            NOT A PJ OR EST, BUT CHECK REC               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC               FOR A 'CODE'                                
         ST    R5,AIO                                                           
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
         BNE   FGETGOOD                                                         
         USING RINVCEL,R6                                                       
         CLC   RINVCODE(2),SPACES                                               
         BE    FGETGOOD                                                         
         OC    RINVCODE(2),RINVCODE                                             
         BZ    FGETGOOD                                                         
         DROP  R6                                                               
*                                                                               
FGET100  EQU   *                                                                
         MVI   WORK,1              NEED AN EXTRA 2 LINES FOR FOOTNOTE           
*                                                                               
*        FORMTOP RECORD GET/CHECKER EXIT                                        
*                                                                               
FGETGOOD EQU   *                                                                
         SR    R2,R2                                                            
FGETEXIT EQU   *                                                                
         MVC   KEY(27),ELEM                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         L     RE,SAVERE                                                        
         LTR   R2,R2                                                            
         B     FTOPEXIT                                                         
FGETBAD  EQU   *                                                                
         LA    R2,1                                                             
         B     FGETEXIT                                                         
*                                                                               
*  INVENTORY 3 OR 5 LINE TYPE TABLE                                             
*                                                                               
FGETITAB EQU   *                                                                
         DC    C'BOUERX'           ARB/NSI/SRC PROJ,EST                         
         EJECT                                                                  
         SPACE 2                                                                
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DDCOMFACS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
*        PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMD6D                                                       
         EJECT                                                                  
RAVLD    DSECT                                                                  
         SPACE 1                                                                
       ++INCLUDE REGENAVLN                                                      
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
*               WORK AREA                                                       
*                                                                               
         DS    0F                                                               
INVNO    DS    CL3                 INVENTORY NUMBER                             
INVDT    DS    CL3                 INVENTORY DATE                               
INVSAT   DS    CL1                 SATELLITE                                    
CPMSW    DS    CL1                 SUPPRESS CPM SWITCH                          
DATASRC  DS    CL1                 DATA SOURCE-INV,PURE,TP,MAN                  
ACTCODE  DS    CL2                 PROGRAM/BREAK CODE (RINVCODE)                
ACTDEMOS DS    CL33                ACTUAL DEMO VALUES                           
EDEMOS   DS    CL66                EDITED DEMOS                                 
GRPS     DS    CL33                                                             
SHARES   DS    CL66                                                             
LEVELS   DS    CL66                                                             
MANDEMOS DS    CL66                MANUAL DEMO VALUES                           
MANBOOKS DS    CL24                                                             
NUMMBKS  DS    CL1                                                              
MANCPM   DS    CL8                                                              
NUMBKS   DS    CL1                                                              
BOOKNO   DS    CL1                 POSITION OF CURRENT BOOK IN LIST             
BOOKLIST DS    CL36                BOOKS TO USE FOR THIS DETAIL LINE            
FORMULAS DS    CL12                                                             
NUMFORMS DS    CL1                                                              
DFORMS   DS    CL12                                                             
DNUMFORM DS    CL1                                                              
DETTEXT  DS    CL1                 Y=MANUAL OR REFERENCED TEXT EXISTS           
WEEKS    DS    CL1                 NUMBER OF WEEKS (RAVLWKS)                    
AVLDATES DS    CL4                 DATES OF CONTRACT OR RAVLDATO                
*                                  (RAVLDATO OVERRIDES CONTRACT DATES)          
HDRLEN   DS    CL1                 LENGTH FROM HEADER (RAVLPLEN)                
DETLEN   DS    CL1                 LEN FROM DET (RPRPDSEC)-OVRRIDES HDR         
HDRPJ    DS    CL14                PJ ELEMENT FROM HEADER (X'05')               
         DS    CL14                   SECOND ELEMENT FROM HEADER                
DETPJ    DS    CL14                PJ ELEM FRM DET - OVERRIDES HDR              
         DS    CL14                   SECOND ELEMENT FROM DETAIL                
HDR0B    DS    CL30                HDR LABEL FROM X'0B' ELEM                    
USE0B    DS    CL60                MERGED LABEL '0B' EL FROM HDR+DET            
HCPP     DS    CL4                 COST PER POINT FROM HDR (RAVLHCPP)           
DNUM     DS    CL1                 NUMBER OF SPOTS (RPRPDNUM)                   
DNC      DS    CL1                 W, X OR T (RPRPDNC)                          
DYTM     DS    CL48                DAY/TIME/(AVAIL NO./INV NO.)                 
OPS      DS    0CL20               PRINT OPTIONS-ALL DEFAULTS ARE N             
OPTA     DS    CL1                 Y=TRIPLE SPACE, N=DOUBLE                     
OPTB     DS    CL1                 Y=PRINT COVER SHEET, N=DON'T                 
OPTC     DS    CL1                 Y=SUPPRESS SHARES HPT'S, N=PRINT             
OPTD     DS    CL1                 Y=PRINT RTG UNDER IMP, N=PRINT               
OPTE     DS    CL1                 Y=SUPPRESS RATES, N=PRINT                    
OPTF     DS    CL1                 Y=PRINT SPECIFIED CPM/CPP N=SUPPRESS         
OPTG     DS    CL1                 Y=PRINT ALL CPM/CPP, N=SUPPRESS              
OPTH     DS    CL1                 Y=PRINT MKT & STA TEXT, N=DON'T              
OPTI     DS    CL1                 Y=PRINT INV & TEXT NUMS, N=DON'T             
OPTJ     DS    CL1                 Y=SUPPRESS AUTO UPGRADE CMT, N=DON'T         
OPTK     DS    CL1                 Y=WHOLE NUMBER DEMOS                         
OPTL     DS    CL1                 SPARE                                        
OPTM     DS    CL1                 SPARE                                        
OPTN     DS    CL1                 SPARE                                        
OPTO     DS    CL1                 SPARE                                        
OPTP     DS    CL1                 SPARE                                        
OPTQ     DS    CL1                 SPARE                                        
AVLUPED  DS    CL1                 1=UPGRADE EXPRESSION ON THE AVAIL            
CKTXTOPT DS    CL1                 CHECK OPTIONS IN TEXT FLAG                   
MINLINES DS    CL1                 MINIMUM LINES TO PRINT                       
MULTSW   DS    CL1                 COMBO (RINVCTYP=X'80)                        
LINSW    DS    CL1                                                              
RATESW   DS    CL1                                                              
MYSWITCH DS    CL1                                                              
UPTYPE   DS    CL1                                                              
AVTYPE   DS    CL1                                                              
ANYOPT   DS    CL1                                                              
HDRUP1FG DS    CL1                                                              
HDRUP2FG DS    CL1                                                              
COMMENT  DS    CL120               HEADER COMMENT (RAVLCEL)                     
HDLN7    DS    CL110               DON'T OVERWRITE PAGE NUMBER                  
HDLN8    DS    CL132                                                            
DEMHD1   DS    CL70                DEMO CATEGORY HEADLINE 1                     
DEMHD2   DS    CL70                DEMO CATEGORY HEADLINE 2                     
         DS    0H                                                               
COMPTEXT DS    H                   COMPULSORY (FORCED) TEXT FROM INV            
         DS    0F                                                               
SAVERE   DS    F                   SAVE REGISTER E FOR BAL                      
SAVEDRB  DS    F                   SAVED BASE REG                               
SAVED2RB DS    F                   SAVED 2ND BASE REG                           
ABOOKOUT DS    A                                                                
ADEMNUMS DS    A                                                                
SVDXDA   DS    F                   FOR PAV FILE READ                            
AUPEL    DS    F                                                                
AVLRATES DS    6F                  DETAIL LINE RATES                            
HOMSHARE DS    3F                                                               
DBEXTRA1 DS    CL128                                                            
         EJECT                                                                  
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006RESFM13   03/16/05'                                      
         END                                                                    
