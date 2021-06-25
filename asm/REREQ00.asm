*          DATA SET REREQ00    AT LEVEL 088 AS OF 10/18/11                      
*PHASE T80700A,*                                                                
*INCLUDE GETDAY                                                                 
*INCLUDE GETIDS                                                                 
         SPACE 2                                                                
         MACRO                                                                  
&TAG     ZERO  &ADDR,&LEN,&BYTE=X'0'                                            
.*                                                                              
         AIF   ('&LEN' EQ '0').ZERO99                                           
.*                                                                              
         LA    RE,&ADDR-1                                                       
         LA    RF,&LEN                                                          
         IC    R0,=&BYTE                                                        
Z&SYSNDX STC   R0,0(RF,RE)                                                      
         BCT   RF,Z&SYSNDX                                                      
.ZERO99  ANOP                                                                   
         MEND                                                                   
REQ0     TITLE 'T80700 - REREQ00 - NEW REP REQUEST BASE MODULE'                 
***********************************************************************         
*                                                                     *         
*    REREQ00 (T80700) --- REQUEST PROGRAM -  BASE                     *         
*                                                                     *         
*        INPUT PARAMETERS (FROM MONITOR)                              *         
*              P1 = X'TAGYB'                                          *         
*                   AL3(16 BYTE TRANSIN INFO)                         *         
*              P2 = A(TCBTWA)                                         *         
*              P3 = A(SYSFACS)                                        *         
*              P4 = A(TCBTIA)                                         *         
*              P5 = A(COMFACS)                                        *         
*              P6 = A(EXTRA INFO LIST)  (COUNTRY INFO)                *         
*                                                                     *         
*---------------------------------------------------------------------*         
*  MODULES IN REQUEST SYSTEM                                          *         
*        REREQ00 - BASE                                               *         
*        REREQ01 - SCREEN LOADER (LIST)/SCREEN BUILDER (ADD)          *         
*        REREQ02 - ADD PROCESS. VALIDATE FIELDS/REQUESTS              *         
*              ADD REQUEST FOR SOON/OVERNIGHT PROCESS                 *         
*        REREQ03 - LIST/CHANGE PROCESSING                             *         
*        REREQ10 - FIELD TABLE DEFINITION. (CALLED BY REREQ01)        *         
*                                                                     *         
*  OTHER DATASETS OF INTEREST                                         *         
*        REREQTWA -- TWA DSECT (SCREENS, SAVE FIELDS, ETC)            *         
*        REREQWRK -- TEMPORARY WORK AREA DSECT                        *         
*        REREQDEF -- REQUEST DEFINITIONS.  (MACRO CALLS)              *         
*                     NOW IN 4 PARTS, REREQDEF1/DEF2/DEF3/DEF4        *         
*        REREQFLD -- FIELDS FOR DYNAMIC BUILDER.                      *         
*              SPLIT INTO SEPARATE DATASET BECAUSE FIELDS ARE         *         
*              UPPER/LOWER CASE.  (EASIER TO EDIT)                    *         
*                                                                     *         
*---------------------------------------------------------------------*         
*  TO ADD A NEW REQUEST:                                              *         
*        ADD ENTRY TO REREQDEF(PART 1 (REPORT ID 00 -> 2Z))           *         
*                             (PART 2 (REPORT ID 50 -> 9Z))           *         
*                             (PART 3 (REPORT ID A0 -> JJ))           *         
*                             (PART 3 (REPORT ID KK -> ZZ))           *         
*                             (PART 4 (REPORT ID 30 -> 4Z))           *         
*                                                                     *         
*        ADD DEFAULT VALUES TO REREQ10/11.  BY CONVENTION, EACH       *         
*              REQUEST HAS AN EQUATE 'DFRXX' WHERE XX=REPORT NUMBER   *         
*              EQUATE PLACED ABOVE DEFAULT LIST (MANY REQUESTS        *         
*              HAVE SAME DEFAULTS).  REQUESTS HAVING NO DEFAULTS      *         
*              PLACE EQUATE ABOVE NULL LIST.  THIS MAKES FOR NO       *         
*              QUESTIONS ON WHAT REPORT HAS WHAT DEFAULTS             *         
*                                                                     *         
*        IF REQUEST HAS OPTION FIELDS, BE SURE TO UPDATE OPTION       *         
*              TABLES IN REREQ02.                                     *         
*                                                                     *         
*        ASSEMBLE REREQ00, 10. (AND REREQ02 IF OPTIONS CHANGED)       *         
*                                                                     *         
*        NOTE: MACRO PROFILES ON 'RQDEF' CALL SET HI-INTENSITY FOR    *         
*              SOON/OVERNIGHT REQUIRED FIELDS (HIS/HIO)               *         
*            **ADDITIONALLY, SHORT FORM MACRO PROFILES ARE SIS/SIO    *         
*              WHILE LONG FORM ARE HIS/HIO                            *         
*                                                                     *         
*              FATAB USES AN INCLUDE DATASET FAREQREP                 *         
*                THIS LIST SHOULD BE ALL REQUESTS DEFINED IN          *         
*                SYSTEM.  NOTE THAT RRG REPORTS DO NOT NEED           *         
*                THEIR OWN ENTRY, BUT THE RRG CLASS MUST              *         
*                HAVE AN ENTRY.  IF REQUEST REQUIRES A NEW            *         
*                FAREQREP ENTRY, CO-ORDINATE THE FATAB CATALOGING     *         
*                AND FACPAK LINK WITH THE POWERS THAT BE.             *         
*                                                                     *         
*--------------------                                                 *         
*  TO ADD NEW FIELDS:                                                 *         
*        ADD FIELD EQUATE TO REREQWRK.  NAMING CONVENTION:            *         
*              'Q' FIELD = FIELD NUMBER IN REREQFLD DATASET (REREQ01) *         
*              'V' FIELD = VALIDATION ROUTINE NUMBER (REREQ02)        *         
*              'I' FIELD = INPUT/OUTPUT FIELD.                        *         
*                          IFLD3XX WHERE XX=FIELD LENGTH.             *         
*              'CMT' FIELD = TEXT COMMENTS (PROTECTED LITERALS)       *         
*                                                                     *         
*        ADD FIELD DEFINITION ENTRY TO REREQFLD.                      *         
*                                                                     *         
*        UPDATE FIELD VALIDATION ADDRESS LIST (FLDVAL) WITH           *         
*              A(PROCESS ROUTINE) FOR THE FIELD (THE 'V' NUMBER)      *         
*                                                                     *         
*        ASSEMBLE REREQ00, 01, 02, 10, 11.                            *         
*                                                                     *         
*        NOTE ON FIELD VALIDATION:  EACH FIELD VALIDATION ROUTINE     *         
*              BASICALY STANDS ALONE.  THEY KNOW ABOUT REQUEST CARD   *         
*              FIELDS (FILLED IN ONLY IF VALIDATED PRIOR TO CURRENT   *         
*              CALLING CURRENT ROUTINE).  THEY CANNOT 'FIND' OTHER    *         
*              SCREEN FIELDS TO CHECK CONTENTS (IE CROSS-EDITS).      *         
*              ALL CROSS-EDITING MUST BE DONE AS AN EXTRA             *         
*              VALIDATION ROUTINE.                                    *         
*                                                                     *         
*--------------------                                                 *         
*  TO ADD EXTRA VALIDATION ROUTINE (WHOLE CARD EDIT)                  *         
*        DEFINE ROUTINE IN REREQ02 EXTRA VALIDATION ROUTINE LIST      *         
*              (XVAL)                                                 *         
*                                                                     *         
*        CODE 'PROC=NN' PARAMETER ON RQDEF CALL FOR REQUEST.          *         
*              NN = VALIDATION ROUTINE NUMBER IN REREQ02.             *         
*                                                                     *         
*        ASSEMBLE REREQ02, 10.                                        *         
*                                                                     *         
*        NOTE ON EXTRA VALIDATIONS:                                   *         
*              USE 'PUTCURS' ROUTINE IN REREQ02 TO 'FIND' FIELDS ON   *         
*              REQUEST SCREEN.                                        *         
*                                                                     *         
*---------------------------------------------------------------------*         
* UPDATE HISTORY:                                                     *         
* --------------                                                      *         
*                                                                     *         
* HISTORY PRIOR TO 1995 HAS  BEEN DELETED                             *         
*                                                                     *         
*                                                                     *         
* MAR22/95 (BU ) --->OPTION LINE (LINE 3) MOVED TO ANOTHER SCREEN     *         
*                                                                     *         
* AUG01/96 (BU ) --->ADD FOURTH TABLE OVERLAY                         *         
*                                                                     *         
* JUL08/98 (BOB) --->MOVE RFP BUFFERS TO TIA                          *         
*                                                                     *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
*                                                                               
T80700   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWORK,T80700,RR=R2,CLEAR=YES                                     
         PRINT NOGEN                                                            
*                                                                               
         LR    R9,RC                                                            
         USING REQWRK,R9           R9=A(W/S)                                    
*                                                                               
*                                                                               
         LR    RF,R9               SET A(REQWRK AREA)                           
         AH    RF,=Y(DIOWORK)      SET A(IOWORK W/IN DSECT)                     
*                                                                               
         MVC   0(4,RF),=C'*IO*'    INSERT FLAG                                  
         LA    RF,4(RF)            BUMP PAST FLAG                               
         ST    RF,AIOADDR2         SAVE ADDRESS OF IOWORK                       
*                                                                               
         ST    R2,RELO                                                          
         ST    R9,ATEMP                                                         
         SPACE                                                                  
         LM    R2,R4,0(R1)                                                      
*                                                                               
         LR    RA,R3               A(SCREEN)                                    
         USING TWAD,RA                                                          
         ST    R1,APARM                                                         
         ST    RA,ASAVE                                                         
         ST    RB,ABASE                                                         
*                                                                               
         L     RF,16(R1)                                                        
         ST    RF,ACOMFACS                                                      
         ST    RF,RFBLOCK          SAVE A(COMFACS) IN REPFACS                   
         MVC   RFBLOCK+4(2),TWAAGY SAVE AGENCY IN REPFACS                       
*                                                                               
*- INITIALIZE RUN                                                               
         BAS   RE,INITIAL                                                       
         BNZ   ERROR                                                            
*                                  ESTABLISH A(REPFACS)                         
         GOTO1 CALLOV,DMCB,0,X'D9000AAC',0                                      
         MVC   AREPFACS,0(R1)      EXTERNAL ROUTINE, MISC. SUBROUTINES          
*                                                                               
*                                                                               
*- VALIDATE BASE SCREEN INPUT.                                                  
         GOTO1 =A(VALINPUT),P1,(R9),RR=RELO   VALIDATE BASE FIELD               
         BNZ   ERROR                                                            
*                                                                               
*- CHECK FOR LOCAL PROCESSING (MENU)                                            
         CLI   ACTION,EQMENU                                                    
         BE    MAIN110                                                          
         CLI   ACTION,EQNEXT                                                    
         BNE   MAIN120                                                          
         CLI   LASTACT,EQMENU                                                   
         BNE   MAIN120                                                          
*                                                                               
MAIN110  BAS   RE,BASEPROC                                                      
         BNZ   ERROR                                                            
         B     OKEXIT                                                           
*                                                                               
*- NEED NEW SCREEN?                                                             
MAIN120  EQU   *                                                                
         CLI   NEWSCRN,0                                                        
         BNE   MAIN150             NO                                           
*                                                                               
         MVI   GOPROC,0            ASSUME WE NEED RQST INPUT                    
*                                                                               
         GOTO1 AGETOVLY,P1,('OVBLDSCR',0)                                       
*                                                                               
         CLI   NOMASTER,0          MASTER REP ERROR FLAG SET?                   
         BE    MAIN124             NO  - PROCEED                                
MAIN121  EQU   *                                                                
         CLI   NOMASTER,1          'PROHIBIT-MASTER' FLAG SET?                  
         BNE   MAIN122             NO  - MUST BE SOON ERROR                     
         MVI   NOMASTER,0          YES - RESET FLAG                             
         L     RE,=A(BLOKMAST)     SET UP ERROR MESSAGE                         
         A     RE,RELO                                                          
         LA    RF,L'BLOKMAST                                                    
         STM   RE,RF,AERROR                                                     
         MVI   NEWSCRN,0                                                        
         XC    SCRNBILT,SCRNBILT                                                
         XC    LASTACT,LASTACT                                                  
*                                                                               
         NI    RQSNUMH+4,II0C      TURN OFF PREVALID BIT                        
         NI    RQSWHENH+4,II0C                                                  
*                                                                               
         B     ERROR                                                            
*                                                                               
MAIN122  EQU   *                                                                
         MVI   NOMASTER,0          YES - RESET FLAG                             
         L     RE,=A(SOONMAST)     SET UP ERROR MESSAGE                         
         A     RE,RELO                                                          
         LA    RF,L'SOONMAST                                                    
         STM   RE,RF,AERROR                                                     
         XC    LASTACT,LASTACT                                                  
*                                                                               
         NI    RQSNUMH+4,II0C      TURN OFF PREVALID BIT                        
         NI    RQSWHENH+4,II0C                                                  
*                                                                               
         B     ERROR                                                            
*                                                                               
MAIN124  EQU   *                                                                
*                                                                               
         MVI   NEWSCRN,1           NOT NEW ANY MORE                             
*                                                                               
         CLI   GOPROC,0            NEED RQST INPUT?                             
         BNE   MAIN150             NO.  CALL PROCESS OVLY                       
*                                                                               
         LA    RE,ENTRDATA                                                      
         LA    RF,L'ENTRDATA                                                    
         STM   RE,RF,AERROR                                                     
*                                                                               
*- POSITION CURSOR AT 1ST UNPROTECTED FIELD AFTER HEADING LINES                 
         LA    R1,RQSLAST          SCREENS GET BUILT HERE                       
         SR    RF,RF                                                            
MAIN130  TM    1(R1),PROTECT       PROTECTED?                                   
         BZ    MAIN140                                                          
         IC    RF,0(R1)                                                         
         AR    R1,RF                                                            
         B     MAIN130                                                          
MAIN140  ST    R1,CURSPOS                                                       
         B     OKEXIT                                                           
         SPACE 1                                                                
*                                                                               
*- SPLIT FOR ADD/CHANGE OR OTHER PROCESS.                                       
MAIN150  EQU   *                                                                
         ZERO  USERWORK,USERWRKL   CLEAR USERWORK FOR SUBOVLYS                  
*                                                                               
         CLI   ACTION,EQADD                                                     
         BE    MAIN155                                                          
         CLI   ACTION,EQCHA                                                     
         BNE   MAIN160                                                          
MAIN155  GOTO1 AGETOVLY,P1,('OVADD',0)                                          
*                                  EDIT + REQ RECORD BUILD                      
         BNZ   ERROR                                                            
         GOTO1 AGETOVLY,P1,('OVSEC',0)                                          
*                                  'SECURITY' CHECK + REQ RECD O/P              
         BNZ   ERROR                                                            
         B     OKEXIT                                                           
*                                                                               
*- CALL OTHER PROCESS OVERLAY FOR OTHER ACTIONS                                 
MAIN160  EQU   *                                                                
         XC    DAONSCRN,DAONSCRN   NO MORE CURRENT D/A                          
*                                                                               
         GOTO1 AGETOVLY,P1,('OVPROC',0)                                         
         BNZ   ERROR                                                            
*                                                                               
*- CHECK FOR SELECT AFTER LIST                                                  
         CLI   SELECTSW,0                                                       
         BE    OKEXIT              NO SELECT                                    
*                                                                               
         MVI   NEWSCRN,0           FORCE NEW SCREEN                             
         MVI   ACTION,EQDISP       NEW INTERNAL ACTION                          
         B     MAIN120                                                          
*                                                                               
*- GOOD EXIT.                                                                   
*  NO BEEP ON GOOD RETURN                                                       
OKEXIT   EQU   *                                                                
         MVI   ERRCNTL,ECNOBEEP                                                 
         B     EXIT                                                             
*                                                                               
*- ERROR EXIT.                                                                  
*  LEAVE LAST ACTION ALONE.                                                     
ERROR    EQU   *                                                                
         CLI   NOMASTER,0          MASTER REP ERROR FLAG SET?                   
         BNE   MAIN121             YUP, DO THAT FIRST                           
         B     EXIT10                                                           
         SPACE                                                                  
*                                                                               
*- SET TRANSMIT BIT ON MESSAGE LINE                                             
*  AND POSITION CURSOR TO FIELD DESIGNATED BY CURSPOS.                          
*                                                                               
*  SAVE CURRENT ACTION.  EXCEPTION: ACTIONS REQUIRING PRIOR                     
*     ACTION (IE: NEXT) MUST NOT ALTER LAST ACTION CODE.                        
*                                                                               
EXIT     EQU   *                                                                
         L     R1,AACTNTRY                                                      
         CLC   =XL2'00',ACTPREV1(R1)    PRIOR ACTIONS?                          
         BNE   EXIT10                                                           
*                                                                               
         MVC   LASTACT,ACTION                                                   
EXIT10   BAS   RE,DOMSG            PUT OUT MSG                                  
         FOUT  RQSMSGH             ALWAYS TRANSMIT MESSAGE FIELD                
*                                                                               
         L     R1,CURSPOS          PUT CURSOR AT THIS FIELD                     
         LTR   R1,R1                                                            
         BNZ   EXIT20                                                           
         LA    R1,RQSACTH          DEFAULT TO ACTION IF 0                       
EXIT20   OI    6(R1),OI1C                                                       
*                                                                               
EXXMOD   XMOD1                                                                  
         SPACE 2                                                                
EXXIT    XIT1                      GENERIC EXIT                                 
         TITLE 'REREQ00 -- LOCAL PROCESSING -- MENU DISPLAY'                    
*                                                                               
*- BASEPROC -- HANDLE LOCAL ACTIONS                                             
*                                                                               
BASEPROC NTR1                                                                   
         L     RE,=A(REQTBL)       MENU USES BASE TBL                           
         A     RE,RELO                                                          
         ST    RE,AREQTBLC                                                      
*                                                                               
*- CLEAR REPORT ID AND EXPANSION FOR MENU                                       
         GOTO1 ACLRSCRN,P1,RQSNUMH,RQSNUMXH                                     
*                                                                               
*                                                                               
*- MUST BE MENU OR NEXT MENU TO BE HERE                                         
         CLI   ACTION,EQMENU                                                    
         BE    BPROC100                                                         
         CLI   ACTION,EQNEXT                                                    
         BNE   BPROC020                                                         
         CLI   LASTACT,EQMENU                                                   
         BE    BPROC100                                                         
BPROC020 DC    H'0'                WE DON'T BELONG HERE.                        
*                                                                               
*- LOAD IN MENU SCREEN                                                          
BPROC100 EQU   *                                                                
         CLI   SCRNBILT,SCMENU                                                  
         BE    BPROC120            SCREEN ALREADY LOADED.                       
*                                                                               
         LA    R2,RQSLAST                                                       
         LA    R4,SCMENU                                                        
         GOTO1 CALLOV,P1,((R4),(R2)),0                                          
         CLI   P2,X'FF'                                                         
         BNE   *+6                                                              
         DC    H'0'                ERROR LOADING SCREEN                         
         XC    SCRNBILT,SCRNBILT                                                
         MVI   SCRNBILT,SCMENU                                                  
*                                                                               
BPROC120 EQU   *                                                                
         GOTO1 ACLRSCRN,P1,MNU1STH,MNULAST-1                                    
*                                                                               
         LA    R2,MNU1STH-LMNULINE                                              
         MVI   FULL,2              COLUMN INDICATOR (0,1,2)                     
*                                                                               
*- IF MENU ACTION, START LOOKING FROM BEGINNING OF REQTBL                       
*  ELSE USE TBLDISP TO FIND A(NEXT REQUEST TO LIST)                             
         L     R4,AREQTBLC                                                      
         A     R4,TBLDISP          <<===* NOTE R4 = A(REQTBL ENTRY)             
*                                                                               
         CLI   ACTION,EQNEXT                                                    
         BE    BPROC200                                                         
*                                                                               
         GOTO1 FINDREQ,P1,0                                                     
         BZ    *+6                                                              
         DC    H'0'                NO MATCH? HOW DID WE PASS IPT VAL?           
*                                                                               
*- TOP OF LOOP                                                                  
BPROC180 L     R4,P1               A(ENTRY)                                     
*                                                                               
*- TABLE ENTRY FILTERS                                                          
BPROC200 EQU   *                                                                
*                                                                               
         XC    RPTNUM,RPTNUM       MENU USES RPTNUM AS STARTING POINT           
*                                                                               
         CLC   =C'DOWN',RQSOUT     MENU FOR 'DOWNLOAD' REPORTS?                 
         BNE   BPROC205            NO                                           
         TM    RQCNTL2(R4),RQ2DWNLD                                             
*                                  YES - DOWNLOADABLE REPORT?                   
         BZ    BPROC500            NO  - SKIP IT                                
BPROC205 EQU   *                                                                
*                                                                               
         TM    RQCNTL1(R4),RQ1XMENU   EXCLUDE FROM MENU?                        
         BZ    BPROC210                                                         
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TUBES MAY ASK FOR 'ALL'                  
         BNE   BPROC500                                                         
         CLC   RQSOPT(3),=C'ALL'                                                
         BNE   BPROC500                                                         
*                                                                               
BPROC210 EQU   *                                                                
         TM    RQCNTL1(R4),RQ1DDS     LIST DDS REPORTS ONLY IF DDS TUBE         
         BZ    BPROC220                                                         
*                                                                               
         CLI   TWAOFFC,C'*'                                                     
         BNE   BPROC500                                                         
*                                                                               
*- CHECK FOR OPTIONAL FILTERS                                                   
*  OPTION FLD KEYWORDS:                                                         
*        OVER                      LIST OVERNIGHTS ONLY                         
*        SOON                      LIST SOON REPORTS ONLY                       
*        MARKER                    LIST FILE MARKERS ONLY                       
*                                                                               
BPROC220 EQU   *                                                                
         LA    RF,RQSOPTH                                                       
         CLI   5(RF),0                                                          
         BE    BPROC300            NO OPTION FILTERS                            
*                                                                               
         LA    R1,OPTTBL           OPTION TABLE                                 
BPROC230 CLI   0(R1),0                                                          
         BE    BPROC300            REQTBL ENTRY NOT FILTERED OUT                
*                                                                               
         CLC   OPTMIN(1,R1),5(RF)  MIN COMPARE LEN -VS- IPT LEN                 
         BH    BPROC240            CAN'T BE THIS OPTION                         
*                                                                               
         ZIC   RE,5(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,OPTCHECK         MATCH ON OPTION KEYWORD?                     
         BE    BPROC250                                                         
*                                                                               
BPROC240 LA    R1,OPTLNTRY(R1)     NEXT ENTRY                                   
         B     BPROC230                                                         
*                                                                               
OPTCHECK CLC   8(0,RF),OPTKEYWD(R1)    KEYWORD COMPARE                          
*                                                                               
OPTTM    TM    RQCNTL1(R4),X'0'        REQTBL ENTRY MATCHES FILTER BIT?         
*                                                                               
BPROC250 EQU   *                                                                
         ZIC   RE,OPTMASK(R1)      PICK UP MASK BYTE FROM TBL                   
         EX    RE,OPTTM                                                         
         BNO   BPROC500            FAILED                                       
*                                                                               
*- REQTBL ENTRY PASSES ALL FILTERS.                                             
*  FIND SCREEN OUTPUT ADDRESS AND DISPLAY RPT DESCRIPTION                       
*  (REPORTS LISTED 3-UP, LEFT TO RIGTH, TOP TO BOTTOM)                          
BPROC300 EQU   *                                                                
         ZIC   RE,FULL             BUMP COLUMN COUNTER                          
         LA    RE,1(RE)                                                         
         STC   RE,FULL                                                          
*                                                                               
         CLI   FULL,3              USED LAST COLUMN ON SCREEN?                  
         BL    BPROC350            NO. MOVE OUT DATA                            
*                                                                               
         MVI   FULL,0              POINT TO 1ST COLUMN                          
*                                                                               
         LA    R2,LMNULINE(R2)     POINT TO NEXT SCREEN LINE                    
         LA    RF,MNULAST                                                       
         CR    R2,RF                                                            
         BNL   BPROC800            OUT OF ROOM.  SET UP FOR NEXT                
*                                                                               
*- (COLUMN * 26 BYTES/COLUMN) + START OF LINE = OUTPUT ADDRESS.                 
BPROC350 EQU   *                                                                
         ZIC   RE,FULL             COLUMN (0,1,2)                               
         MH    RE,=H'26'           * 26 BYTES/COL=LINE DISP                     
         LA    RE,8(RE,R2)         ACTUAL SCREEN ADDRESS                        
*                                                                               
         MVC   0(1,RE),RQPREFIX(R4) PREFIX, IF ANY                              
         CLI   0(RE),C'R'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '          BLANK OUT 'R' ON DISPLAY                     
*                                                                               
         MVC   1(2,RE),RQALPHA(R4) ALPHA ID                                     
         MVI   3(RE),C'-'                                                       
         MVC   4(20,RE),RQNAME(R4) NAME TO SCREEN LINE                          
         FOUT  (R2)                                                             
*                                                                               
*- GET NEXT REQUEST FROM TABLE.  STOP IF ALL DONE.                              
BPROC500 EQU   *                                                                
         L     RF,AREQTBLC         FIND DISPLACEMENT FOR FINDREQ                
         SR    R4,RF                                                            
         ST    R4,TBLDISP                                                       
         GOTO1 FINDREQ,P1,TBLDISP                                               
         BZ    BPROC180                                                         
*                                                                               
*- NO MORE TO LIST.                                                             
         MVC   RQSACT,SPACES       BLANK OUT THE ACTION                         
         XC    TBLDISP,TBLDISP                                                  
         MVI   LASTACT,0           PREVENT MORE NEXTING                         
         NI    RQSNUMH+4,II0C      TURN OFF RPT NUMBER PREVALID                 
         LA    RE,NOMORE                                                        
         LA    RF,L'NOMORE                                                      
         B     BPROC900                                                         
*                                                                               
*- MORE TO LIST                                                                 
BPROC800 EQU   *                                                                
         L     RF,AREQTBLC         SAVE DISPLACEMENT FOR NEXT TIME              
         SR    R4,RF                                                            
         ST    R4,TBLDISP                                                       
         MVI   LISTMORE,1          MORE TO SEE                                  
         MVC   RQSACT(8),=CL8'NEXT' SEED THE 'NEXT'                             
         OI    RQSACTH+6,X'01'     AVOID 'NO DATA' MSG                          
         LA    RE,MOREMSG                                                       
         LA    RF,L'MOREMSG                                                     
*                                                                               
BPROC900 STM   RE,RF,AERROR        ADDR/LEN OF MSG TEXT                         
         MVI   ERRCNTL,ECNOBEEP    INFORMATIONAL ONLY                           
         LA    R2,RQSACTH                                                       
         FOUT  (R2)                                                             
         ST    R2,CURSPOS                                                       
         SR    R0,R0                                                            
         B     EXXIT                                                            
         TITLE 'REREQ00 -- MISC SUBROUTINES'                                    
*                                                                               
*- DOMSG -- MOVE MSG TO MSG WINDOW                                              
*                                                                               
DOMSG    NTR1  BASE=ABASE                                                       
         TM    ERRCNTL,ECNOBEEP    SUPPRESS TERMINAL BEEP?                      
         BO    DOMSG20             YES                                          
*                                                                               
         L     R1,APARM                                                         
         L     R1,0(R1)            A(TIOB FROM MONITOR)                         
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBALRM   BEEP FOR ERRORS                              
         DROP  R1                                                               
*                                                                               
DOMSG20  EQU   *                                                                
         NI    ERRCNTL,(X'FF'-ECNOBEEP)    TURN OFF NO BEEP BIT.                
*                                                                               
*- USE MSG OR SYSTEM MSG?                                                       
         TM    ERRCNTL,ECMSGNUM                                                 
         BO    DOMSG40             SYSTEM MSG.                                  
*                                                                               
         LM    RE,RF,AERROR                                                     
         LTR   RE,RE                                                            
         BZ    DOMSGX              NO MESSAGE TO MOVE                           
*                                                                               
         LTR   RF,RF               LENGTH OF MSG....EXIT IF 0                   
         BZ    DOMSGX                                                           
         BCTR  RF,0                                                             
         EX    RF,MSGMOVE                                                       
         B     DOMSGX                                                           
*                                                                               
*- READ SYSTEM MSG FROM FILE.                                                   
DOMSG40  EQU   *                                                                
         ZIC   R2,ERRDATA          1 BYTE MSG NUMBER                            
         GOTO1 GETMSG,P1,((R2),RQSMSG),(08,DMCB),DATAMGR                        
*                                                                               
DOMSGX   EQU   *                                                                
         XC    AERROR,AERROR       CLEAR FOR NEXT CALL                          
         XIT1                                                                   
         SPACE                                                                  
MSGMOVE  MVC   RQSMSG(0),0(RE)     ERR MSG TO TOP OF SCREEN                     
         EJECT                                                                  
*                                                                               
*- FINDREQ -- SEARCH REQTBL FOR REQUEST MATCHING FILTERS                        
*                                                                               
*  INPUT:  P1 = F'0' - FIND 1ST REQUEST                                         
*               A(CURRENT REQUEST DISP) - FIND NEXT RQST.                       
*                                                                               
*          RPTID - REPORT ID FILTER (PREFIX,NUMBER)                             
*                                                                               
*  RETURN: P1 = A(REQTBL ENTRY) IF MATCH                                        
*               SET TO 0 IF NO MATCH FOUND (IN REMAINING TBL)                   
*          P2 = REQTBL DISPLACEMENT OF MATCHING RQST (NOT AN ADDRESS)           
*                                                                               
*  CC:     ZERO = FOUND, ^0 = NOT FOUND                                         
*                                                                               
FINDREQ  NTR1  BASE=ABASE                                                       
         L     R1,AREQTBLC                                                      
         ICM   RF,15,P1                                                         
         BZ    FRQ020              GET FIRST                                    
*                                                                               
         A     R1,0(RF)            ADD DISP TO CURRENT ENTRY                    
         B     FRQ200              AND GET NEXT                                 
*                                                                               
FRQ020   EQU   *                                                                
         OC    RPTPREFX,RPTPREFX   ANY PREFIX FILTER                            
         BZ    FRQ040                                                           
         CLC   RPTPREFX,RQPREFIX(R1)   MATCH TBL?                               
         BNE   FRQ200                  NO. NEXT ENTRY                           
*                                                                               
FRQ040   EQU   *                                                                
         OC    RPTNUM,RPTNUM       ANY REPORT NUMER FILTER?                     
         BZ    FRQ060                                                           
         CLC   RPTNUM,RQALPHA(R1)   MATCH TBL?                                  
         BNE   FRQ200               NO. NEXT ENTRY                              
*                                                                               
FRQ060   EQU   *                   FOUND A MATCH!                               
         ST    R1,P1               A(REQTBL ENTRY)                              
         S     R1,AREQTBLC                                                      
         ST    R1,P2               REQTBL DISPLACEMENT                          
*                                                                               
         SR    R0,R0               0 CC                                         
         B     FRQEXIT                                                          
*                                                                               
FRQ200   EQU   *                   FIND NEXT ENTRY IN REQTBL                    
         SR    RF,RF                                                            
         ICM   RF,3,RQLNTRY(R1)    2 BYTE ENTRY LENGTH                          
         AR    R1,RF                                                            
         CLC   =XL2'00',0(R1)      END OF TABLE?                                
         BNE   FRQ020              NO  - GO BACK FOR NEXT                       
*                                                                               
         XC    P1(8),P1            PASS BACK 0 ADDRESS/DISPLACEMENT             
         LTR   RD,RD               ^0 CC                                        
FRQEXIT  EQU   *                                                                
         B     EXXIT                                                            
         EJECT                                                                  
*                                                                               
*- GETOVLY -- PASS CONTROL TO A SUB-OVERLAY.  IF OVERLAY IS NOT                 
*             ALREADY LOADED, LOAD IT IN FROM DISK (LET FAKPAK                  
*             FIND ADDRESS).  ADDRESS AND OVERLAY NUMBER SAVED                  
*             FOR POSSIBLE NEXT CALL.                                           
*                                                                               
* INPUT: P1 -- BYTE 1 = OVERLAY # TO LOAD                                       
*              BYTES 2-4 = A(LOAD POINT) OR 0 (USE FACPAK LOAD POINT)           
*        LASTOVLY - OVERLAY NUMBER FROM LAST CALL                               
*        AOVLY    - OVERLAY LOAD POINT (SET FROM LAST CALL)                     
*                                                                               
GETOVLY  NTR1  BASE=ABASE                                                       
*                                                                               
*- OVERLAY ALREADY IN CORE FROM PREVIOUS CALL?                                  
         CLC   LASTOVLY,0(R1)      IS OVERLAY IN CORE                           
         BE    GETOV200            YES                                          
*                                                                               
         MVC   LASTOVLY,0(R1)      SAVE NEW OVERLAY NUMBER                      
*                                                                               
         GOTO1 CALLOV,0(R1),,0                                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                CAN'T LOAD OVLY FROM DISK                    
*                                                                               
         MVC   AOVLY,0(R1)         SAVE A(OVERLAY)                              
*                                                                               
*- TRANSFER CONTROL TO SUB-OVERLAY. PASS A(WRK AREA) IN P1                      
GETOV200 EQU   *                                                                
         GOTO1 AOVLY,0(R1),(R9)                                                 
         B     EXXIT                                                            
         EJECT                                                                  
*                                                                               
*- CLRSCRN -- LABEL = ACLRSCRN -- CLEAR SCREEN TO BLANKS                        
*                                                                               
*  INPUT:  P1 - A(1ST FIELD TO CLEAR)                                           
*          P2 - A(LAST FIELD TO CLEAR)  (INCLUSIVE)                             
*                                                                               
CLRSCRN  NTR1  BASE=ABASE                                                       
         LM    R2,R3,0(R1)         R2 = A(1ST), R3 = A(LAST)                    
CLRSCR20 CR    R2,R3                                                            
         BH    CLRSCR50            DONE                                         
*                                                                               
         ZIC   RF,0(R2)            DETERMINE ACTUAL FIELD LENGTH                
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                0 LENGTH FIELD                               
*                                                                               
         LA    R1,8+1              HEADER LEN + 1 FOR 'EX'                      
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         LA    R1,8(R1)            EXTENDED HEADER                              
         SR    RF,R1               RF = FIELD LENGTH                            
*                                                                               
         EX    RF,CLRSCR99         FILL WITH BLANKS                             
         FOUT  (R2)                XMIT THE FIELD                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT FIELD                                   
         CLI   0(R2),0                                                          
         BE    CLRSCR50            GET OUT ON END OF SCREEN                     
         B     CLRSCR20                                                         
CLRSCR50 XIT1                                                                   
         SPACE                                                                  
CLRSCR99 MVC   8(0,R2),SPACES                                                   
         EJECT                                                                  
*                                                                               
*- SETREP -- POINT TO REP FILE/DIRECTORY                                        
SETREP   NTR1  BASE=ABASE                                                       
         MVC   FILENAME,=CL8'REPFILE'                                           
         MVC   DIRNAME,=CL8'REPDIR'                                             
         B     EXXIT                                                            
         SPACE 2                                                                
*                                                                               
*- SETCNTL -- POINT TO CONTROL FILE                                             
SETCNTL  NTR1  BASE=ABASE                                                       
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVC   DIRNAME,=CL8'CTFILE'                                             
         B     EXXIT                                                            
         SPACE 2                                                                
*                                                                               
*- SETREQ -- POINT TO REQUEST FILE (NO DIRECTORY)                               
SETREQ   NTR1  BASE=ABASE                                                       
         MVC   FILENAME,=CL8'REPREQ'                                            
         MVC   DIRNAME,=CL8'REPREQ'                                             
         B     EXXIT                                                            
         TITLE 'REREQ00 -- DATAMGR INTERFACE'                                   
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
READ     NTR1  BASE=ABASE                                                       
         MVC   COMMAND,=CL8'DMREAD'                                             
         B     DIRCTRY                                                          
         SPACE 2                                                                
CHAIN    NTR1  BASE=ABASE                                                       
         MVC   COMMAND,=CL8'DMRDIR'                                             
         B     DIRCTRY                                                          
         SPACE 2                                                                
SEQ      NTR1  BASE=ABASE                                                       
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     NTR1  BASE=ABASE                                                       
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
ADD      NTR1  BASE=ABASE                                                       
         MVC   COMMAND,=CL8'DMADD '                                             
         B     DIRCTRY                                                          
         SPACE 2                                                                
WRITE    NTR1  BASE=ABASE                                                       
         MVC   COMMAND,=CL8'DMWRT '                                             
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRY  EQU   *                                                                
         IC    R4,DMINBTS                                                       
         IC    R3,TERMNAL                                                       
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         LA    R5,KEYSAVE          A(INPUT)                                     
         LA    R6,KEY              A(OUTPUT)                                    
*                                                                               
         CLC   DIRNAME,=CL8'REPDIR'                                             
         BE    DIRCTRY5                                                         
*                                                                               
         L     R6,AIOWORK          READ INTO RECORD FOR OTHER DIRS              
*                                                                               
*- PASS P6 DIRECTLY FROM APPLICATION                                            
DIRCTRY5 EQU   *                                                                
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),DIRNAME,(R5),(R6),((R3),0)           
*                                                                               
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
GETREC   NTR1  BASE=ABASE                                                       
         MVC   COMMAND,=CL8'GETREC'                                             
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   NTR1  BASE=ABASE                                                       
         MVC   COMMAND,=CL8'PUTREC'                                             
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   NTR1  BASE=ABASE                                                       
         MVC   COMMAND,=CL8'ADDREC'                                             
         B     FILE                                                             
         SPACE 2                                                                
FILE     EQU   *                                                                
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R3,TERMNAL                                                       
         IC    R4,DMINBTS                                                       
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),FILENAME,                   X        
               (R2),AIOWORK,((R3),DMWORK),0                                     
         SPACE 2                                                                
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    EXXIT                                                            
         SPACE 2                                                                
*                                                                               
*- EXIT WITH DMGR ERROR ON SCREEN                                               
DMERRS   EQU   *                                                                
         MVC   DMCB+20(4),DATAMGR                                               
         GOTO1 GETMSG,DMCB+12,(0,RQSMSG),(8,DMCB)                               
         LTR   RD,RD                                                            
         B     EXXIT               ^0 CC                                        
         TITLE 'REREQ00 -- INITIAL'                                             
*                                                                               
*- REQUEST INITIAL.                                                             
*  IF 1ST TIME IN, CLEAN OUT REQTWA                                             
*                                                                               
*  INPUT: R2-R4 ARE P1-P3 FROM MONITOR                                          
*                                                                               
INITIAL  NTR1                                                                   
*                                                                               
*- PICK UP TRANSIN INFO (1ST/LAST/NUMBER FIELDS ENTERED)                        
         LR    R6,R3                         STORE A(FIRST INPUT FLD)           
         AH    R6,0(R2)                                                         
         ST    R6,AFIRSTF                                                       
         LR    R6,R3                         STORE A(LAST INPUT FLD)            
         AH    R6,2(R2)                                                         
         ST    R6,ALASTF                                                        
         MVC   COUNTF,4(R2)                  STORE INPUT FLD COUNT              
*                                                                               
*- PICK UP SYSFACS LIST                                                         
         MVC   FACLIST(28),0(R4)                 STORE FACILITY LIST            
         LA    R6,REPWORK                    STORE TERM NUM IN DMCB             
         ST    R6,DMCB+16                                                       
         MVC   DMCB+16(1),0(R3)                                                 
*                                                                               
*- RELOCATE COMMON ROUTINES                                                     
         LA    RE,RTNADDRS         ROUTINE ADDRESS LIST                         
         LA    RF,ACOMMON          PUT RELOCATED ADDRS HERE                     
         LA    R0,(LACOMMON/4)     # OF ROUTINES                                
         SR    R1,R1               INDEX REG                                    
*                                                                               
INIT100  EQU   *                                                                
         L     R2,0(R1,RE)         PICK UP ADDR FROM LIST                       
         A     R2,RELO             RELOCATE IT                                  
         ST    R2,0(R1,RF)         AND SAVE IN WRK AREA                         
         LA    R1,4(R1)            UP THE INDEX                                 
         BCT   R0,INIT100                                                       
*                                                                               
*- INITIALIZE DATAMGR WORK AREA                                                 
         MVC   TERMNAL,TWATASK     TERMINAL (TASK?) NUMBER                      
         MVI   DMINBTS,0                                                        
         MVI   DMOUTBTS,0                                                       
*                                                                               
         L     RE,AIOADDR2         DEFAULT IO BUFFER                            
         ST    RE,AIOWORK                                                       
*                                                                               
         L     RF,APARM            SET A(RFP) & A(MINIO) BUFFERS                
         L     RF,12(RF)           BUFFERS RESIDE IN TIA                        
*                                  ADDRESS IN 4TH INPUT PARM                    
*                                                                               
         ST    RF,AIORFP           A(RFPBLK)                                    
*                                                                               
         AH    RF,=Y(RFPBLTLN)     EXTENDED BUFFER LENGTH (8K)                  
         ST    RF,AIOMINIO         A(MINIO IOAREA)                              
*                                                                               
         AH    RF,=Y(5*1024)       MINIO IOAREA LENGTH (5K)                     
         ST    RF,ARFPTAB          A(MINIO RECORD AREA) (1K)                    
*                                                                               
         GOTO1 ASETREP             POINT TO REPFILE/DIR                         
*                                                                               
         MVC   SAVEMSG,RQSMSG      J.I.C. WE WANT TO SEE LAST MSG               
         XC    RQSMSG,RQSMSG                                                    
*                                                                               
         MVI   SELECTSW,0                                                       
*                                                                               
         CLI   IAMAMAST,0                                                       
         BNE   INITOK                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),TWAAGY    INSERT REP CODE                              
         GOTO1 DIRHIGH                                                          
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    INIT200             YES                                          
         DC    H'0'                NO REP RECORD ON FILE?                       
INIT200  EQU   *                                                                
         GOTO1 FILREAD                                                          
*                                                                               
         L     RE,AIOADDR2         SET A(IOWORK)                                
         USING RREPREC,RE                                                       
*                                                                               
         MVC   FRIDTEST,RREPPROF+3 FRIDAY TEST FLAG                             
         MVC   DAILYPAC,RREPPROF+27       DAILY PACING FLAG                     
         CLC   RREPMAST,=X'FFFF'   MASTER REP?                                  
         BNE   INIT215             NO                                           
         MVI   IAMAMAST,C'Y'                                                    
         B     INIT220                                                          
INIT215  EQU   *                                                                
         MVI   IAMAMAST,C'N'                                                    
INIT220  EQU   *                                                                
         XC    PROFDATA,PROFDATA   INITIALIZE PROFILES                          
         LA    R6,RREPELEM         A(DESCRIPTIVE ELEMENT)                       
*                                                                               
         DROP  RE                                                               
*                                                                               
INIT230  EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    INIT270             YES - NO PROFILES                            
         CLI   0(R6),4             PROFILE ELEMENT?                             
         BE    INIT240             YES - PROCESS                                
         ZIC   RF,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,RF                                                            
         B     INIT230             GO BACK FOR NEXT                             
INIT240  EQU   *                                                                
         USING RREPPGMP,R6                                                      
         ZIC   RF,RREPPGM#         LOOP CONTROL                                 
         LA    R6,RREPPGM1                                                      
         USING RREPPGM1,R6                                                      
INIT250  EQU   *                                                                
         CLI   RREPPGM1,RREPQREQ   REQUEST PROFILE?                             
         BE    INIT260             YES                                          
         LA    R6,RREPPGML(R6)     BUMP TO NEXT ELEMENT                         
         BCT   RF,INIT250          GO BACK FOR NEXT                             
         B     INIT270             NOT FOUND                                    
INIT260  EQU   *                                                                
         MVC   PROFDATA,RREPPGM1   SAVE PROGRAM PROFILES                        
         DROP  R6                                                               
INIT270  EQU   *                                                                
*                                                                               
*- END OF REQUEST INITIAL                                                       
INITOK   EQU   *                                                                
         SR    R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- ROUTINES IN BASE AVAILABLE TO SUB-OVERLAYS.                                  
*                                                                               
*--->  ORDER OF FIELDS MUST MATCH ORDER OF LABELS IN WORK AREA  <---            
*                                                                               
RTNADDRS DS    0F                                                               
         DC    A(REQTBL)           AREQTBLC - REQUEST DEFINITION TBL            
         DC    A(REQTBL)           AREQTBL1 - REQUEST DEFINITION TBL            
         DC    A(REQTBL)           AREQTBL2 - REQUEST DEFINITION TBL            
         DC    A(REQTBL)           AREQTBL3 - REQUEST DEFINITION TBL            
         DC    A(REQTBL)           AREQTBL4 - REQUEST DEFINITION TBL            
         DC    A(REQTBL)           AREQTBL5 - REQUEST DEFINITION TBL            
         DC    A(GETOVLY)          OVERLAY READER/CALLER                        
         DC    A(READ)             DIRREAD                                      
         DC    A(CHAIN)            DIRCHAIN                                     
         DC    A(HIGH)             DIRHIGH                                      
         DC    A(SEQ)              DIRSEQ                                       
         DC    A(ADD)              DIRADD                                       
         DC    A(WRITE)            DIRWRIT                                      
         DC    A(GETREC)           FILREAD                                      
         DC    A(PUTREC)           FILWRIT                                      
         DC    A(ADDREC)           FILADD                                       
         DC    A(CLRSCRN)          CLEAR SCREEN FIELDS TO BLANKS                
         DC    A(SETREP)                                                        
         DC    A(SETREQ)                                                        
         DC    A(SETCNTL)                                                       
         DC    A(FINDREQ)          GET NEXT REQTBL ENTRY                        
         DC    A(DOMSG)                                                         
#RTNADDR EQU   (*-RTNADDRS)/4                                                   
         TITLE 'REREQ00 -- ERROR MESSAGES AND INTERNAL TABLES'                  
*                                                                               
*- ERROR MESSAGES                                                               
*                                                                               
ENTRDATA DC    C'ENTER REQUEST DATA.'                                           
BADACT   DC    C'INVALID ACTION.'                                               
BADRPT   DC    C'INVALID REPORT ID.  USE ''MENU'' ACTION TO LIST VALID X        
               REPORTS'                                                         
BLOKMAST DC    C'''MASTER'' REP MAY NOT REQUEST THIS REPORT'                    
SOONMAST DC    C'''MASTER'' REP MAY NOT REQUEST THIS REPORT SOON'               
MOREMSG  DC    C'MORE TO LIST.  PRESS ENTER FOR NEXT PAGE.'                     
NOMORE   DC    C'NO MORE TO LIST.  ENTER NEXT REQUEST.'                         
MISSING  DC    C'THIS FIELD IS REQUIRED FOR THIS REQUEST'                       
BADOFF   DC    C'INVALID OFFICE CODE'                                           
BADDEST  DC    C'INVALID DESTINATION'                                           
BADOUT   DC    C'INVALID OUTPUT CODE'                                           
BADWHEN  DC    C'CHOICES ARE ''OV'' (OVERNIGHT) OR ''SOON''.'                   
WHENCONF DC    C'THIS REPORT MAY NOT BE RUN '                                   
CANTCHA  DC    C'CAN''T ALTER THIS FIELD ON CHANGE ACTION.'                     
BADSPACE DC    C'INVALID VALUE.  ENTER ''1'', ''2'', OR ''3''.'                 
NOSPCHGE DC    C'''SPACING'' OPTION INACTIVE.  RESET TO ''1''.'                 
NOCONSPC DC    C'CONTRACT PRINT ONLY PERMITS SINGLE SPACING'                    
BADGROUP DC    C'INVALID GROUP NAME'                                            
BADCOMBO DC    C'INVALID VALUE.  ENTER ''Y'', ''N'', OR '' ''.'                 
BADRQSTR DC    C'REQUESTOR MUST CONTAIN AT LEAST 3 CHARACTERS.'                 
CSPNOUSE DC    C'NOT AUTHORIZED FOR THIS REPORT'                                
         DS    0H                                                               
         SPACE                                                                  
*                                                                               
*- MESSAGE NUMBERS FROM CONTROL FILE                                            
SECLOCK  EQU   55                  SECURITY LOCKOUT                             
         SPACE 2                                                                
*                                                                               
*- OPTION TABLE                                                                 
*                                                                               
OPTKEYWD EQU   0                   KEYWORD                                      
OPTMIN   EQU   8                   MINIMUM LENGTH OF INPUT                      
OPTMASK  EQU   9                   REQTBL CNTL BYTE 1 MASK                      
*                                                                               
OPTLNTRY EQU   10                  LENGTH OF ENTRY                              
OPTKYMAX EQU   8                   MAX LENGTH OF KEYWORD                        
*                                                                               
OPTTBL   DS    0H                                                               
         DC    CL8'OVER    ',X'1',AL1(RQ1OVRNT)                                 
         DC    CL8'SOON    ',X'1',AL1(RQ1SOON)                                  
         DC    CL8'MARKER  ',X'1',AL1(RQ1FMARK)                                 
         DC    X'00'               EOT                                          
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*- ACTION TABLE                                                                 
*                                                                               
ACTKEYWD EQU   0                   KEYWORD                                      
ACTMIN   EQU   8                   MINIMUM LENGTH OF INPUT                      
ACTIACT  EQU   9                   INTERNAL ACTION CODE                         
ACTPREV1 EQU   10                  PREVIOUS ACTION 1                            
ACTPREV2 EQU   11                  PREVIOUS ACTION 2                            
ACTCNTL  EQU   12                  CONTROL BITS. ON = FLD REQUIRED              
ACRPTID  EQU   X'80'                - REPORT ID                                 
ACRQSTR  EQU   X'40'                - REQUESTOR                                 
ACOFFICE EQU   X'20'                - OFFICE                                    
*                                                                               
ACTLNTRY EQU   13                  LENGTH OF ENTRY                              
ACTKYMAX EQU   8                   MAX LENGTH OF KEYWORD                        
*                                                                               
ACTTBL   DS    0H                                                               
         DC    CL8'MENU    ',X'1',AL1(EQMENU),X'0',X'0',X'0'                    
*                                                                               
         DC    CL8'LIST    ',X'1',AL1(EQLIST),X'0',X'0',X'0'                    
*                                                                               
*****    DC    CL8'TOTAL   ',X'1',AL1(EQTOT),X'0',X'0',X'0'                     
*                                                                               
         DC    CL8'NEXT    ',X'1',AL1(EQNEXT),AL1(EQMENU,EQLIST),X'0'           
*                                                                               
         DC    CL8'ADD     ',X'1',AL1(EQADD),X'0',X'0'                          
         DC    AL1(ACRPTID+ACRQSTR+ACOFFICE)                                    
*                                                                               
         DC    CL8'CHANGE  ',X'1',AL1(EQCHA),AL1(EQADD,0),X'0'                  
*                                                                               
         DC    X'00'               EOT                                          
         SPACE 2                                                                
SPACES   DC    CL80' '                                                          
         EJECT                                                                  
         LTORG                                                                  
         DS    0H                                                               
         TITLE 'REREQ00 -- VALIDATE BASE SCREEN INPUT'                          
*                                                                               
*- VALINPUT -- VALIDATE BASE SCREEN FIELDS                                      
VALINPUT NMOD1 0,**VIPT                                                         
*                                                                               
         MVI   NOMASTER,0                                                       
*                                                                               
*- VALIDATE ACTION FIELD                                                        
         XC    ACTION,ACTION       INTERNAL ACTION FIELD                        
         LA    R2,RQSACTH                                                       
         L     RE,=A(BADACT)       INVALID ACTION MSG                           
         A     RE,RELO                                                          
         LA    RF,L'BADACT                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         CLI   5(R2),0                                                          
         BE    VIERR               ACTION IS REQUIRED                           
*                                                                               
         L     R1,=A(ACTTBL)       ACTION TABLE                                 
         A     R1,RELO                                                          
VI0020   CLI   0(R1),0                                                          
         BE    VIERR               ACTION NOT IN TABLE                          
*                                                                               
         CLC   ACTMIN(1,R1),5(R2)  MIN COMPARE LEN -VS- IPT LEN                 
         BH    VI0040              CAN'T BE THIS ACTION                         
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,ACTCHECK         MATCH ON ACTION KEYWORD?                     
         BE    VI0050                                                           
*                                                                               
VI0040   LA    R1,ACTLNTRY(R1)     NEXT ACTION ENTRY                            
         B     VI0020                                                           
*                                                                               
*- FOUND CURRENT ACTION.  PICK UP INTERNAL ACTION CODE                          
*  THEN CHECK FOR VALID PRIOR ACTION                                            
VI0050   EQU   *                                                                
         ST    R1,AACTNTRY         SAVE A(ACTION TBL ENTRY)                     
*                                                                               
         MVC   ACTION,ACTIACT(R1)  INTERNAL ACTION CODE                         
*                                                                               
*- CAN'T NEXT UNLESS THERE IS MORE TO SEE.                                      
         CLI   ACTION,EQNEXT                                                    
         BNE   VI0055                                                           
         CLI   LISTMORE,0                                                       
         BE    VI0060              SEQUENCE ERROR                               
*                                                                               
*- CAN'T CHANGE UNLESS A REQUEST IS ON SCREEN                                   
*  CAN'T CHANGE 'WHEN' ON CHANGE                                                
VI0055   EQU   *                                                                
         CLI   ACTION,EQCHA                                                     
         BNE   VI0057                                                           
         OC    DAONSCRN,DAONSCRN                                                
         BZ    VI0060              SEQUENCE ERROR                               
*                                                                               
         TM    RQSWHENH+4,II1C                                                  
         BO    VI0057                                                           
*                                                                               
         L     RE,=A(CANTCHA)      INVALID ACTION MSG                           
         A     RE,RELO                                                          
         LA    RF,L'CANTCHA                                                     
         STM   RE,RF,AERROR                                                     
*                                                                               
VI0057   EQU   *                                                                
         CLI   ACTPREV1(R1),0      PREVIOUS ACTION NEEDED?                      
         BZ    VI0100              NO                                           
*                                                                               
         CLC   LASTACT,ACTPREV1(R1)                                             
         BE    VI0100              OK.                                          
*                                                                               
         CLI   ACTPREV2(R1),0      2ND PREV ACTION?                             
         BZ    VI0060              ERROR                                        
*                                                                               
         CLC   LASTACT,ACTPREV2(R1)                                             
         BE    VI0100              ALSO OK.                                     
*                                                                               
*- SEQUENCE OF ACTION ERROR.  BUILD SPECIFIC MESSAGE.                           
VI0060   EQU   *                                                                
         XC    AERROR,AERROR       DOING MSG HERE                               
*                                                                               
         LR    R3,R1               SAVE FROM GOTO1 MACRO                        
*                                                                               
         LA    R4,RQSMSG           MESSAGE LINE ON SCREEN                       
         MVC   0(6,R4),=C'ERROR.'                                               
         LA    R4,8(R4)                                                         
         GOTO1 TEXTACT,P1,(ACTPREV1(R3),(R4))                                   
         ZIC   RF,P1                                                            
         AR    R4,RF                                                            
*                                                                               
         CLI   ACTPREV2(R3),0                                                   
         BE    VI0070                                                           
*                                                                               
         MVC   0(4,R4),=C' OR '                                                 
         LA    R4,4(R4)                                                         
         GOTO1 TEXTACT,P1,(ACTPREV2(R3),(R4))                                   
         ZIC   RF,P1                                                            
         AR    R4,RF                                                            
VI0070   EQU   *                                                                
         MVC   0(14,R4),=C' MUST PRECEED '                                      
         LA    R4,14(R4)                                                        
         GOTO1 TEXTACT,P1,(ACTION,(R4))                                         
         B     VIERR                                                            
         SPACE 2                                                                
*                                                                               
*- DO WE NEED A NEW SCREEN?                                                     
*                                                                               
VI0100   EQU   *                                                                
         MVI   LISTMORE,0          PREVENT SPURIOUS NEXTING                     
*                                                                               
*- ACTIONS THAT REQUIRE PRIOR ACTIONS NEVER NEED A NEW SCREEN                   
VI0105   EQU   *                                                                
         MVI   NEWSCRN,1           ASSUME SCREEN ALREADY UP                     
*                                                                               
         L     RF,AACTNTRY                                                      
         CLC   =XL2'00',ACTPREV1(RF)   PREV ACTION(S)?                          
         BNE   VI0120                  YES.                                     
*                                                                               
         CLC   ACTION,LASTACT                                                   
         BNE   VI0110                                                           
*                                                                               
         CLC   RQSNUM,SCRNBILT     CURRENT RPT ID=SCREEN WE BUILT?              
         BNE   VI0110                                                           
*                                                                               
         B     VI0120              SCREEN ALREADY UP. RE-USE IT                 
*                                                                               
VI0110   EQU   *                                                                
         MVI   NEWSCRN,0           ACTION CHANGED. NEED NEW SCREEN.             
         SPACE 2                                                                
*                                                                               
*- VALIDATE REPORT ID.                                                          
*                                                                               
VI0120   EQU   *                                                                
         L     RF,AREQTBLC         BUILD A(REQTBL ENTRY) FROM                   
         A     RF,TBLDISP            START ADDRESS + DISPLACEMENT               
         ST    RF,AREQNTRY                                                      
*                                                                               
         LA    R2,RQSNUMH                                                       
         TM    4(R2),II1C          PREVIOUSLY VALID?                            
         BO    VI0300                                                           
*                                                                               
         MVI   NEWSCRN,0           NEED NEW SCREEN                              
*                                                                               
         GOTO1 ACLRSCRN,P1,RQSNUMXH,RQSNUMXH  BLANK EXPANSION                   
*                                                                               
         L     RE,=A(BADRPT)       INVLALID REPORT MSG                          
         A     RE,RELO                                                          
         LA    RF,L'BADRPT                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VI0150              GO VALIDATE REPORT ID                        
*                                                                               
*- CLEAR REPORT EXPANSION AND TWA REPORT ID FIELD.                              
         GOTO1 ACLRSCRN,P1,RQSNUMXH,RQSNUMXH                                    
         XC    RPTID,RPTID         0 ID IN THE TWA.                             
*                                                                               
         L     RF,AACTNTRY         DOES THIS ACTION REQUIRE RPT ID              
         TM    ACTCNTL(RF),ACRPTID                                              
         BZ    VI0280              NO.                                          
         B     VIERR               ERROR.  MISSING REQUIRED FIELD.              
*                                                                               
*- REPORT ID IS 1 BYTE (OPTIONAL) PREFIX AND 2 BYTE REPORT NUMBER               
*  USER MAY GIVE JUST PREFIX FOR 'LISTING' TYPE ACTIONS (IT'S A FILTER)         
*  REPORT NUMBER MUST BE GIVEN IF ACTION REQUIRES IT.                           
*                                                                               
*  INPUT EDIT RULES:                                                            
*        3 BYTE INPUT IS FORMAT PNN (P=PREFIX, N=NUMBER)                        
*        2 BYTE INPUT IS NN (NO PREFIX)                                         
*        1 BYTE INPUT IS P  (ONLY PREFIX)                                       
*                                                                               
VI0150   EQU   *                                                                
         CLI   5(R2),2             2 CHAR INPUT HAS NO PREFIX                   
         BE    VI0160                                                           
         MVC   RPTPREFX,8(R2)                                                   
*                                                                               
VI0160   LA    RF,8(R2)                                                         
         CLI   5(R2),2                                                          
         BL    VI0170              PREFIX ONLY.                                 
         BE    VI0165                                                           
         LA    RF,1(RF)            SKIP PREFIX ON 3 CHAR INPUT                  
VI0165   MVC   RPTNUM,0(RF)        PICK UP REPORT NUMBER                        
*                                                                               
VI0170   EQU   *                   MAKE SURE RPT ID ENTERED IF REQUIRED         
         L     RF,AACTNTRY                                                      
         TM    ACTCNTL(RF),ACRPTID                                              
         BZ    VI0180              NOT REQUIRED                                 
         OC    RPTNUM,RPTNUM                                                    
         BZ    VIERR               REQUIRED BUT NOT GIVEN                       
*                                                                               
*- IF REPORT NUMBER GIVEN WITHOUT PREFIX,  DEFAULT PREFIX TO 'R'                
VI0180   EQU   *                                                                
         OC    RPTNUM,RPTNUM                                                    
         BZ    VI0190              LEAVE PREFIX ALONE                           
*                                                                               
         OC    RPTPREFX,RPTPREFX                                                
         BNZ   VI0190              USE GIVEN PREFIX                             
*                                                                               
         MVI   RPTPREFX,C'R'       USE DEFAULT PREFIX                           
*                                                                               
*- LOOK THRU REQUEST TABLE TO FIND 1ST MATCHING REQUEST                         
VI0190   GOTO1 AFINDREQ,P1,0                                                    
         BZ    VI0200                                                           
*                                                                               
*- NO MATCH ON GIVEN RPT ID.  IF WE ARE MENU ACTION, 0 OUT RPT ID               
*  (LIST ALL REPORTS) AND CONTINUE.  ERROR FOR OTHER ACTIONS.                   
         CLI   ACTION,EQMENU                                                    
         BNE   VIERR                                                            
         OC    RPTID,RPTID                                                      
         BZ    VIERR               ALREADY 0.  DON'T LOOP!                      
         XC    RPTID,RPTID                                                      
         B     VI0190              GO FIND 1ST ENTRY                            
*                                                                               
*- CHECK DDS-ONLY REQUEST STATUS                                                
VI0200   EQU   *                                                                
         L     R1,P1               A(TBL ENTRY)                                 
         TM    RQCNTL1(R1),RQ1DDS  DDS REQUEST?                                 
         BZ    VI0220                                                           
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   VIERR                                                            
*                                                                               
*- SAVE A(REQTBL) ENTRY AND DISPLACEMENT FOR NEXT PASS                          
VI0220   L     R1,P1                                                            
         ST    R1,AREQNTRY         SAVE A(REQTBL ENTRY)                         
         L     RF,AREQTBLC                                                      
         SR    R1,RF                                                            
         ST    R1,TBLDISP          SAVE AS DISPLACEMENT IN TWA                  
*                                                                               
*- DISPLAY REPORT NAME                                                          
         LA    RE,RQNAME(R1,RF)    A(20 BYTE NAME)                              
         MVC   RQSNUMX,0(RE)                                                    
         FOUT  RQSNUMXH                                                         
*                                                                               
VI0280   OI    4(R2),II1C          TURN ON PREVALID                             
         SPACE                                                                  
*                                                                               
*- VALIDATE REQUESTOR AND COMP S/P SCREEN REQUEST                               
*                                                                               
VI0300   EQU   *                                                                
         LA    R2,RQSNAMEH                                                      
         L     RE,=A(MISSING)      REQUIRED FIELD MISSING MSG                   
         A     RE,RELO                                                          
         LA    RF,L'MISSING                                                     
         STM   RE,RF,AERROR                                                     
*                                                                               
         CLI   5(R2),0             ANY INPUT                                    
         BNE   VI0320                                                           
*                                                                               
         L     RF,AACTNTRY         DOES THIS ACTION REQUIRE REQUESTOR           
         TM    ACTCNTL(RF),ACRQSTR                                              
         BNZ   VIERR               YES. ERROR.                                  
VI0320   EQU   *                                                                
         CLI   5(R2),3             AT LEAST THREE CHARACTERS?                   
*                                                                               
***                                                                             
****>>>  B     VI0340              UNTIL 3 CHARS REQUIRED.                      
***                                                                             
*                                                                               
         BNL   VI0340              YES - PROCEED                                
         CLC   =C'LI',RQSACT       NO  - LIST ACTION?                           
         BE    VI0340              YES - PERMIT IT                              
         CLC   =C'ME',RQSACT       NO  - MENU ACTION?                           
         BE    VI0340              YES - PERMIT IT                              
         CLC   =C'NE',RQSACT       NO  - MENU/NEXT ACTION?                      
         BE    VI0340              YES - PERMIT IT                              
         L     RE,=A(BADRQSTR)     NO  - FIELD REQUIRES 3 CHAR MIN              
         A     RE,RELO                                                          
         LA    RF,L'BADRQSTR                                                    
         STM   RE,RF,AERROR                                                     
         B     VIERR               ERROR EXIT                                   
         SPACE                                                                  
*                                                                               
*- VALIDATE OFFICE                                                              
*                                                                               
VI0340   EQU   *                                                                
         CLC   TWAAGY,=C'NU'       CLEAR CHANNEL?                               
         BE    VI0345              YES - PERMIT PS REPORT                       
         CLC   TWAAGY,=C'B3'       EJOR/TEST?                                   
         BE    VI0345              YES - PERMIT PS REPORT                       
         CLC   RQSNUM(2),=C'PS'    NO  - COMPENSATION S/P REPORT?               
         BE    VI0342              YES - NOT AUTHORIZED                         
         CLC   RQSNUM(3),=C'RPS'   COMPENSATION S/P REPORT?                     
         BNE   VI0345              NO  - AUTHORIZED                             
VI0342   EQU   *                   NOT AUTHORIZED                               
         LA    R2,RQSNUMH                                                       
         L     RE,=A(CSPNOUSE)     REPORT NOT AUTHORIZED FOR USER               
         A     RE,RELO                                                          
         LA    RF,L'CSPNOUSE                                                    
         STM   RE,RF,AERROR                                                     
*                                                                               
         B     VIERR               ERROR.                                       
*                                                                               
VI0345   EQU   *                                                                
         XC    IREQOFF,IREQOFF                                                  
*                                                                               
         LA    R2,RQSOFFH                                                       
         L     RE,=A(MISSING)      REQUIRED FIELD MISSING MSG                   
         A     RE,RELO                                                          
         LA    RF,L'MISSING                                                     
         STM   RE,RF,AERROR                                                     
*                                                                               
         CLI   5(R2),0             ANY INPUT                                    
         BNE   VI0350                                                           
*                                                                               
         L     RF,AACTNTRY         DOES THIS ACTION REQUIRE INPUT               
         TM    ACTCNTL(RF),ACOFFICE                                             
         BNZ   VIERR               YES. ERROR.                                  
         B     VI0400              SKIP OTHER OFFICE PROCESSING                 
*                                                                               
VI0350   EQU   *                                                                
         L     RE,=A(BADOFF)       INVALID OFFICE                               
         A     RE,RELO                                                          
         LA    RF,L'BADOFF                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         MVC   IREQOFF,RQSOFF                                                   
         OC    IREQOFF(2),=C'  '    CONVERT X'0' TO BLANKS                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),IREQOFF                                                
         GOTO1 DIRREAD                                                          
         BNZ   VIEXIT              DMGR ERROR                                   
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VIERR                                                            
*                                                                               
*- CHECK FOR LIMITED OFFICE ACCESS                                              
*                                                                               
*  ** NOTE: STATION SECURITY CHECKED AFTER SCREEN HAS BEEN VALIDATED.           
*                                                                               
         L     RE,AREQNTRY                                                      
         TM    RQCNTL1(RE),RQ1OFLMT    LIMIT TEST REQUIRED?                     
         BZ    VI0400                  NO                                       
*                                                                               
         MVI   ERRDATA,SECLOCK     SECURITY LOCKOUT                             
         MVI   ERRCNTL,ECMSGNUM    MESSAGE FROM FILE                            
*                                                                               
*- OUTSIDE OFFICE FOR A REP?                                                    
         CLC   =C'O=',TWAACCS      TEST FOR OFFICE LIMITED ACCESS               
         BNE   VI0400                                                           
*                                                                               
         TM    TWAAUTH,X'80'       TERMINAL HAS ACCESS TO ALL OFFICES?          
         BO    VI0400              YES                                          
*                                                                               
         CLC   IREQOFF,TWAACCS+2   NO, MUST MATCH RESTRICTED OFFICE             
         BNE   VIERR                                                            
         SPACE 2                                                                
*                                                                               
*- VALIDATE DESTINATION ID NAME                                                 
VI0400   EQU   *                                                                
         XC    IDEST,IDEST                                                      
*                                                                               
         LA    R2,RQSDESTH                                                      
*                                                                               
*- CAN'T ALTER DESTINATION ON CHANGE.                                           
         CLI   ACTION,EQCHA                                                     
         BNE   VI0405                                                           
         TM    4(R2),II1C          ALTERED?                                     
         BO    VI0405                                                           
         L     RE,=A(CANTCHA)      SET UP MESSAGE AND CURSPOS                   
         A     RE,RELO                                                          
         LA    RF,L'CANTCHA                                                     
         STM   RE,RF,AERROR                                                     
         B     VIERR2                                                           
*                                                                               
VI0405   CLI   5(R2),0                                                          
         BE    VI0490              NO INPUT                                     
*                                                                               
         MVI   RFPSTAT,0                                                        
         CLC   =C'FILE',8(R2)      'FILE' INDICATES RFP USAGE                   
         BNE   VI0408                                                           
*                                                                               
         L     RE,=A(BADGROUP)     SET UP MESSAGE AND CURSPOS                   
         A     RE,RELO                                                          
         LA    RF,L'BADGROUP                                                    
         STM   RE,RF,AERROR                                                     
*                                                                               
         MVI   QRFPMODE,QRFPINIT                                                
         GOTO1 AGETOVLY,P1,('OVRFP',0)                                          
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         ZIC   R1,RQSOUTH+5                                                     
         SH    R1,=H'1'                                                         
         BM    VI0406                                                           
         EX    R1,*+4                                                           
         MVC   QRFPWORK(0),RQSOUT  VALIDATE GROUP NAME                          
         OC    QRFPWORK,SPACES2                                                 
         MVI   QRFPMODE,QRFPGVAL                                                
         MVI   RFPSTAT,RFPINUSE                                                 
         GOTO1 AGETOVLY,P1,('OVRFP',0)                                          
         CLI   QRFPMODE,QRFPOK                                                  
         BE    VI0406                                                           
         LA    R2,RQSOUTH                                                       
         B     VIERR               INVALID GROUP                                
*                                                                               
VI0406   DS    0H                                                               
         OI    RQSDESTH+4,II1C     SET PREVIOUSLY CORRECT                       
         OI    RQSOUTH+4,II1C      SET PREVIOUSLY CORRECT                       
         B     VI0600              VALIDATE 'WHEN'                              
*                                                                               
VI0408   DS    0H                                                               
         L     RE,=A(BADDEST)      SET UP MESSAGE AND CURSPOS                   
         A     RE,RELO                                                          
         LA    RF,L'BADDEST                                                     
         STM   RE,RF,AERROR                                                     
*                                                                               
         GOTO1 ASETCNTL            SET UP FOR CONTROL FILE READ                 
*                                                                               
         OC    8(10,R2),=CL10' '   CONVERT NULL TO BLANKS                       
*                                                                               
*- READ ID RECORD FROM CONTROL FILE FOR TWA ORIGIN                              
*  OR GET DDS/SJR ID RECORD.                                                    
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,TWAUSRID     ID # FROM TWA (ORIGIN)                      
*                                                                               
         CLC   =CL10'DDS',8(R2)                                                 
         BE    VI0410                                                           
         CLC   =CL10'SJR',8(R2)                                                 
         BNE   VI0420                                                           
*                                                                               
VI0410   MVC   CTIKID(10),8(R2)     DDS/SJR ONLY                                
*                                                                               
         DROP  R4                                                               
*                                                                               
VI0420   GOTO1 DIRREAD                                                          
         CLI   P3,X'10'            KEY NOT FOUND                                
         BE    VIERR                                                            
         CLI   P3,0                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,DATAMGR                                                       
         L     RE,AIOADDR2         SET A(IOWORK)                                
         ST    RE,P1               SET A(IOWORK) IN PARAM1                      
         MVI   P1,C'D'             SET 1ST BYTE OF PARAM1                       
*                                                                               
         PRINT GEN                                                              
         GOTO1 =V(GETIDS),P1,,0,(R5),RR=RELO                                    
         PRINT NOGEN                                                            
****>>>> GOTO1 =V(GETIDS),P1,(C'D',IOWORK),0,(R5),RR=RELO                       
         CLI   P1,0                                                             
         BE    VIERR               NO DESTS FOUND                               
         CLI   P1,X'FF'                                                         
         BNE   *+6                 DISK ERROR                                   
         DC    H'0'                                                             
         ZIC   R5,P1                NUMBER OF DESTS                             
         L     R6,P2                ADDR OF BLOCK OF DESTS                      
*                                                                               
*- MATCH USER'S INPUT TO DESTINATION ELEMENTS                                   
VI0440   CLI   0(R6),X'FF'         END OF TABLE                                 
         BE    VIERR                                                            
         CLC   8(10,R2),0(R6)                                                   
         BNE   VI0460                                                           
         MVC   IDEST,10(R6)        DEST ID NUMBER, INTERNAL FORMAT              
*                                                                               
         GOTO1 ASETREP             POINT BACK TO REP FILE                       
*                                                                               
         B     VI0490                                                           
*                                                                               
VI0460   LA    R6,12(R6)                                                        
         BCT   R5,VI0440                                                        
         B     VIERR                                                            
*                                                                               
VI0490   EQU   *                                                                
         OI    RQSDESTH+4,II1C     SET PREVIOUSLY CORRECT                       
         SPACE 2                                                                
*                                                                               
*- VALIDATE OUTPUT TYPE                                                         
VI0500   EQU     *                                                              
         XC    IOUT,IOUT                                                        
*                                                                               
         LA    R2,RQSOUTH                                                       
*                                                                               
*- CAN'T ALTER OUTPUT ON CHANGE.                                                
         CLI   ACTION,EQCHA                                                     
         BNE   VI0505                                                           
         TM    4(R2),II1C          ALTERED?                                     
         BO    VI0505                                                           
         L     RE,=A(CANTCHA)      SET UP MESSAGE AND CURSPOS                   
         A     RE,RELO                                                          
         LA    RF,L'CANTCHA                                                     
         STM   RE,RF,AERROR                                                     
         B     VIERR2                                                           
*                                                                               
VI0505   CLI   5(R2),0                                                          
         BE    VI0590            NO INPUT                                       
*                                                                               
         L     RE,=A(BADOUT)     SET UP MESSAGE AND CURSPOS                     
         A     RE,RELO                                                          
         LA    RF,L'BADOUT                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         CLC   8(4,R2),=C'DOWN'    DOWNLOAD REQUEST?                            
         BNE   VI0507              NO  - SKIP CHECKING                          
         L     RE,AREQNTRY         A(REQUEST ENTRY)                             
         TM    RQCNTL2(RE),RQ2DWNLD          TEST CONTROL                       
         BNO   VIERR               NOT ON = DOWNLOAD NOT ALLOWED                
*                                                                               
VI0507   EQU   *                                                                
         GOTO1 ASETCNTL            SET UP FOR CNTL FILE READ                    
*                                                                               
         MVC   IOUT,8(R2)          PICK UP SCREEN IPT                           
         OC    IOUT,=CL6' '        AND PAD WITH BLANKS                          
*                                                                               
         LA    R4,KEY              READ OUTPUT TYPE RECORD                      
         USING CTOKEY,R4                                                        
         XC    CTOKEY,CTOKEY                                                    
         MVC   CTOKID,=CL10' '     10 BYTES ON FILE                             
         MVI   CTOKEY,C'O'                                                      
         MVC   CTOKID(L'IOUT),IOUT   6 BYTES ON REQ REC                         
         DROP  R4                                                               
         GOTO1 DIRREAD,P1                                                       
         BNZ   VIEXIT              DMGR ERROR                                   
*                                                                               
         TM    P3,X'10'            REC NOT FOUND?                               
         BO    VIERR                                                            
*                                                                               
****>>>> LA    R6,IOWORK+28                                                     
*                                                                               
         L     R6,AIOADDR2         SET A(IOWORK AREA)                           
         LA    R6,28(R6)           DISPLACE                                     
VI0520   CLI   0(R6),X'38'         FIND OUTPUT DETAIL ELEMENT                   
         BE    VI0530                                                           
         CLI   0(R6),0             END OF REC                                   
         BE    VIERR                                                            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VI0520                                                           
*                                                                               
VI0530   DS    0H                                                               
         USING CTOUTD,R6                                                        
         TM    CTOUTSTA,X'80'      SEE IF REQUESTABLE OUTPUT TYPE               
         BZ    VIERR               NO                                           
*                                                                               
         GOTO1 ASETREP             POINT BACK TO REP FILE                       
         DROP  R6                                                               
*                                                                               
VI0590   EQU   *                                                                
         OI    RQSOUTH+4,II1C      SET PREVIOUSLY CORRECT                       
*                                                                               
*- VALIDATE WHEN....CAN BE OVERNIGHT (OV) OR SOON. BLANK=OVERNIGHT              
VI0600   EQU   *                                                                
         LA    R2,RQSWHENH                                                      
         L     RE,=A(BADWHEN)                                                   
         A     RE,RELO                                                          
         LA    RF,L'BADWHEN                                                     
         STM   RE,RF,AERROR                                                     
*                                                                               
         MVI   WHEN,EQOVRN         OVERNIGHT                                    
         CLI   5(R2),0                                                          
         BE    VI0620              BLANK = OVERNIGHT                            
         CLC   =C'OV',8(R2)                                                     
         BE    VI0620                                                           
*                                                                               
         MVI   WHEN,EQSOON         SOON                                         
         CLC   =C'SOON',8(R2)                                                   
         BNE   VIERR               INVALID INPUT.                               
*                                                                               
*- IF 'WHEN' HAS CHANGED, WE NEED A NEW SCREEN                                  
VI0620   TM    4(R2),II1C                                                       
         BO    VI0625                                                           
         CLI   ACTION,EQNEXT       ...BUT NOT IF 'NEXT'                         
         BE    VI0625                                                           
*                                                                               
         MVI   NEWSCRN,0           FORCE NEW SCREEN                             
         XC    SCRNBILT,SCRNBILT                                                
VI0625   OI    4(R2),II1C                                                       
*                                                                               
*- CHECK FOR CONFLICT BETWEEN REPORT AND 'WHEN' (ADDS ONLY)                     
         CLI   ACTION,EQADD                                                     
         BNE   VI0650              SKIP THE TEST.                               
*                                                                               
         L     RE,=A(WHENCONF)                                                  
         A     RE,RELO                                                          
         LA    RF,L'WHENCONF                                                    
         STM   RE,RF,AERROR                                                     
*                                                                               
         L     RE,AREQNTRY                                                      
         CLI   WHEN,EQSOON         ARE WE RUNNING SOON?                         
         BNE   VI0630                                                           
         TM    RQCNTL1(RE),RQ1SOON                                              
         BO    VI0625A             OK                                           
*                                                                               
         MVC   RQSMSG+L'WHENCONF(4),=C'SOON'                                    
         B     VIERR                                                            
*                                                                               
VI0625A  EQU   *                                                                
         TM    RQCNTL2(RE),RQ2MASOV  IS 'MASTER/OVERNIGHT ONLY' SET             
         BZ    VI0650                NO  - PROCEED                              
         CLI   IAMAMAST,C'Y'       YES - AM I A MASTER?                         
         BNE   VI0650              NO  - I AM NOT: PROCEED                      
         MVI   NOMASTER,2                                                       
         B     VIERR                                                            
*                                                                               
VI0630   EQU   *                                                                
         CLI   WHEN,EQOVRN         ARE WE RUNNING OVERNIGHT                     
         BNE   VI0650                                                           
         TM    RQCNTL1(RE),RQ1OVRNT                                             
         BO    VI0650              OK                                           
*                                                                               
         MVC   RQSMSG+L'WHENCONF(9),=C'OVERNIGHT'                               
         B     VIERR                                                            
*                                                                               
*- ADD ACTION CAN USE REQUEST DEFINITION FROM LAST TIME.                        
VI0650   EQU   *                                                                
         CLI   ACTION,EQADD                                                     
         BNE   VI0660                                                           
         LA    RE,REQDEF                                                        
         ST    RE,AREQTBLC                                                      
*                                                                               
*- VALIDATE OPTIONAL SPACING                                                    
*                                                                               
VI0660   EQU   *                                                                
         SPACE 2                                                                
*                                                                               
         LA    R2,RQSSPCEH                                                      
         L     RE,=A(BADSPACE)     INVALID SPACING MESSAGE                      
         A     RE,RELO                                                          
         LA    RF,L'BADSPACE                                                    
         STM   RE,RF,AERROR                                                     
*                                                                               
         LA    RF,RQSNUMH          REPORT NUMBER FIELD                          
         CLI   5(RF),2             TWO CHARACTER REPORT #?                      
         BNE   VI0661              NOT 2 CHARACTERS                             
         CLC   =C'10',8(RF)        CONTRACT PRINT (10 REPORT)?                  
         BE    VI0663              YES - ONLY PERMIT SINGLE SPACING             
         B     VI0664              NO  - CONTINUE PROCESSING                    
VI0661   EQU   *                                                                
         CLI   5(RF),3             THREE CHARACTER REPORT #?                    
         BNE   VI0664              NO                                           
         CLC   =C'R10',8(RF)       CONTRACT PRINT (R10 REPORT)?                 
         BE    VI0663              YES - ONLY PERMIT SINGLE SPACING             
         B     VI0664              NO  - CONTINUE PROCESSING                    
VI0663   EQU   *                                                                
         CLI   8(R2),C'1'          SINGLE SPACED?                               
         BE    VI0666              YES                                          
         L     RE,=A(NOCONSPC)                                                  
         A     RE,RELO                                                          
         LA    RF,L'NOCONSPC                                                    
         STM   RE,RF,AERROR                                                     
         B     VIERR                                                            
*                                                                               
VI0664   EQU   *                                                                
         CLI   5(R2),0             ANY INPUT                                    
         BE    VI0670              NO                                           
*                                                                               
****>>>  CLI   TWAOFFC,C'*'        **TEST**                                     
****>>>  BE    VI0666              **TEST**                                     
****>>>  CLI   8(R2),C'1'          **TEST**                                     
****>>>  BE    VI0666              **TEST**                                     
****>>>  L     RE,=A(NOSPCHGE)     **TEST**                                     
****>>>  A     RE,RELO             **TEST**                                     
****>>>  LA    RF,L'NOSPCHGE       **TEST**                                     
****>>>  STM   RE,RF,AERROR        **TEST**                                     
****>>>  B     VIERR               **TEST**                                     
VI0666   EQU   *                                                                
         CLI   8(R2),C'0'          TEST NUMERIC                                 
         BL    VIERR               < X'F0' = NON-NUMERIC: REJECT                
         CLI   8(R2),C'3'          IS VALUE 1, 2, OR 3?                         
         BH    VIERR               YES - REJECT AS ERROR                        
*                                                                               
VI0670   EQU   *                                                                
         SPACE 2                                                                
*                                                                               
*   COMBINE COMBOS OPTION IS NOW A KEYWORD IN NEW OPTIONS FIELD.                
*                                                                               
***      LA    R2,RQSCMBOH                                                      
***      L     RE,=A(BADCOMBO)     INVALID COMBO SUPPRESS OPTION                
***      A     RE,RELO                                                          
***      LA    RF,L'BADCOMBO                                                    
***      STM   RE,RF,AERROR                                                     
***      CLI   RQSCMBO,C'Y'        YES?                                         
***      BE    VI0680                                                           
***      CLI   RQSCMBO,C'N'        NO ?                                         
***      BE    VI0680                                                           
***      CLI   RQSCMBO,X'00'       NULL=NO?                                     
***      BE    VI0680                                                           
***      CLI   RQSCMBO,C' '        SPACE=NO?                                    
***      BNE   VIERR                                                            
*                                                                               
VI0680   EQU   *                                                                
*- FUTURE OPTIONS GO HERE                                                       
VIGOOD   SR    R0,R0                                                            
         B     VIEXIT                                                           
         SPACE 2                                                                
*                                                                               
*- BASE SCREEN INPUT ERRORS ARE SERIOUS.                                        
*  MUST VALIDATE ALL FIELDS ON NEXT PASS.                                       
VIERR    EQU   *                                                                
         MVI   NEWSCRN,0                                                        
         XC    SCRNBILT,SCRNBILT                                                
         XC    LASTACT,LASTACT                                                  
*                                                                               
         NI    RQSNUMH+4,II0C      TURN OFF PREVALID BIT                        
         NI    RQSWHENH+4,II0C                                                  
*                                                                               
VIERR2   ST    R2,CURSPOS          LESS SEVERE ERRS COME HERE                   
         LTR   RD,RD               BAD CC                                       
VIEXIT   EQU   *                                                                
         XIT1                                                                   
         SPACE                                                                  
ACTCHECK CLC   RQSACT(0),ACTKEYWD(R1)                                           
         EJECT                                                                  
*                                                                               
*- TEXTACT -- CONVERT INTERNAL ACTION CODE TO TEXT.                             
*  INPUT: P1 = BYTE 1 - INTERNAL ACTION CODE                                    
*              BYTE 2-4 - A(TEXT OUTPUT AREA)                                   
*                                                                               
*  RETURN: P1 = BYTE 1 - LENGTH OF TEXT MOVED TO OUTPUT AREA                    
*                                                                               
TEXTACT  NTR1                                                                   
         L     R1,=A(ACTTBL)                                                    
         A     R1,RELO                                                          
TXACT20  CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                INVALID INTERNAL ACTION                      
         CLC   ACTIACT(1,R1),P1                                                 
         BE    TXACT40                                                          
         LA    R1,ACTLNTRY(R1)                                                  
         B     TXACT20                                                          
*                                                                               
TXACT40  EQU   *                                                                
         LA    RF,1                                                             
         SR    RE,RE               COUNT NUMBER BYTES MOVED                     
         L     R2,P1               A(OUTPUT AREA)                               
         LA    R1,ACTKEYWD(R1)     TEXT KEYWORD                                 
         LA    R0,ACTKYMAX         MAX KEYWORD LENGTH                           
TXACT50  EQU   *                                                                
         CLI   0(R1),C' '                                                       
         BE    TXACT60             STOP ON BLANK                                
         MVC   0(1,R2),0(R1)                                                    
         AR    R1,RF               NEXT KEYWORD BYTE                            
         AR    R2,RF               NEXT OUTPUT BYTE                             
         AR    RE,RF               BUMP NUMBER CHARS MOVED                      
         BCT   R0,TXACT50                                                       
*                                                                               
TXACT60  STC   RE,P1               PASS BACK # CHARS MOVED                      
         XIT1                                                                   
         SPACE 2                                                                
SPACES2  DC    CL80' '                                                          
         LTORG                                                                  
         TITLE 'REREQ00 -- BOGUS FIELD DEF MACROS -- TAKE UP NO SPACE'          
*                                                                               
*- BASE VERSION OF FIELD DEF MACRO -- NO-OPS THAT TAKE NO SPACE                 
*  FOR FIELDS.  ('SHORT' FORMAT TABLE)                                          
*                                                                               
         MACRO                                                                  
         GFLD  &FLD,&ROW,&COL,&FMT=0,&CNTL=0,&RTN=NONE,&EXT=0                   
.*                                                                              
.GETOUT  ANOP                                                                   
         MEND                                                                   
         SPACE 2                                                                
*                                                                               
*- BASE VERSION OF LINE DEF MACRO -- NO-OPS THAT TAKE NO SPACE                  
*  FOR FIELDS.  ('SHORT' FORMAT TABLE)                                          
*                                                                               
         MACRO                                                                  
&TAG     GENLN &NAME,&ROW,&COL=0,&LBL=Y,&IPT=0,&XP=0,&OV=N,&SOON=N,    X        
               &FMT=0,&NUM=N,&LAST=N,&VAL=N,&EXT=0                              
.*                                                                              
.OUT     ANOP                                                                   
         MEND                                                                   
         SPACE 2                                                                
*                                                                               
*- BASE VERSION OF FILTER SCREEN DEF MACRO -- NO-OPS THAT TAKE NO SPACE         
*  FOR FIELDS.  ('SHORT' FORMAT TABLE)                                          
*                                                                               
         MACRO                                                                  
         STDFT &HIO=NNNNNNNNNNNNNNNNNNN,&HIS=NNNNNNNNNNNNNNNNNNN                
.*                                                                              
.STDF10  ANOP                                                                   
         MEND                                                                   
         MACRO                                                                  
         SHDFT &SIO=NNNNNNNNNNNNN,&SIS=NNNNNNNNNNNNN                            
.*                                                                              
.SHDF10  ANOP                                                                   
         MEND                                                                   
         TITLE 'REREQ00 -- RQDEF MACRO -- REQUEST DEFINITION'                   
*                                                                               
*- MACRO TO DEFINE BASIC REQTBL ENTRY                                           
*  BASE VERSION -- DO NOT DEFINE ANY DEFAULT VALUES                             
*                                                                               
         MACRO                                                                  
         RQDEF &ID=,&NAME=,&PROC=0,&OVR=Y,&SOON=Y,&DDS=N,&MENU=Y,      X        
               &MARK=N,&XREQ=N,&OFFLMT=Y,&PREFIX=R,&CID=,&RRG=,        X        
               &ENDEF=N,&DOWN=Y,&RRGEN=N,&MASTR=Y,&FRIDAY=N,&BUYRD=N            
.*                                                                              
         LCLC  &LBL,&CNTL,&CDID,&DFLT,&CNTL2                                    
.*                                                                              
&LBL     SETC  '&PREFIX.&ID'                                                    
&DFLT    SETC  'DF&LBL'                                                         
.*                                                                              
.* END OF DEFINITION?                                                           
.* GENERATE LAST FIELD ENTRY THEN END OF RQST DEF LABEL.                        
         AIF   ('&ENDEF' EQ 'N').RQD00                                          
         GENLN LAST=Y                                                           
&LBL.X   EQU   *                                                                
         MEXIT                                                                  
.*                                                                              
.* ANALYZE CALL                                                                 
.*                                                                              
.RQD00   ANOP                                                                   
.*                                                                              
.* SET REQUEST CONTROL1 BITS                                                    
&CNTL    SETC  '0'                                                              
         AIF   ('&OVR' EQ 'N').RQD20                                            
&CNTL    SETC  'RQ1OVRNT+&CNTL'                                                 
.*                                                                              
.RQD20   ANOP                                                                   
         AIF   ('&SOON' EQ 'N').RQD30                                           
&CNTL    SETC  'RQ1SOON+&CNTL'                                                  
.*                                                                              
.RQD30   ANOP                                                                   
         AIF   ('&DDS' EQ 'N').RQD40                                            
&CNTL    SETC  'RQ1DDS+&CNTL'                                                   
.*                                                                              
.RQD40   ANOP                                                                   
         AIF   ('&MENU' EQ 'Y').RQD50                                           
&CNTL    SETC  'RQ1XMENU+&CNTL'                                                 
.*                                                                              
.RQD50   ANOP                                                                   
         AIF   ('&MARK' EQ 'N').RQD60                                           
&CNTL    SETC  'RQ1FMARK+&CNTL'                                                 
.*                                                                              
.RQD60   ANOP                                                                   
         AIF   ('&XREQ' EQ 'N').RQD70                                           
&CNTL    SETC  'RQ1XREQ+&CNTL'                                                  
.*                                                                              
.RQD70   ANOP                                                                   
         AIF   ('&OFFLMT' EQ 'N').RQD80                                         
&CNTL    SETC  'RQ1OFLMT+&CNTL'                                                 
.*                                                                              
.RQD80   ANOP                                                                   
.*                                                                              
.* SET REQUEST CONTROL2 BITS                                                    
.*                                                                              
&CNTL2   SETC  '0'                                                              
         AIF   ('&DOWN' EQ 'N').RQD82                                           
&CNTL2   SETC  'RQ2DWNLD+&CNTL2'                                                
.*                                                                              
.RQD82   ANOP                                                                   
.*                                                                              
         AIF   ('&RRGEN' EQ 'N').RQD84                                          
&CNTL2   SETC  'RQ2RRG+&CNTL2'                                                  
.*                                                                              
.RQD84   ANOP                                                                   
.*                                                                              
         AIF   ('&MASTR' EQ 'Y').RQD86A                                         
         AIF   ('&MASTR' EQ 'O').RQD86B                                         
         AGO   .RQD86Z                                                          
.*                                                                              
.RQD86A  ANOP                                                                   
&CNTL2   SETC  'RQ2MASTR+&CNTL2'                                                
         AGO   .RQD86Z                                                          
.*                                                                              
.RQD86B  ANOP                                                                   
&CNTL2   SETC  'RQ2MASTR+RQ2MASOV+&CNTL2'                                       
.*                                                                              
.RQD86Z  ANOP                                                                   
.*                                                                              
         AIF   ('&FRIDAY' EQ 'N').RQD87                                         
&CNTL2   SETC  'RQ2FRIDY+&CNTL2'                                                
.*                                                                              
.RQD87   ANOP                                                                   
.*                                                                              
         AIF   ('&BUYRD' EQ 'N').RQD88                                          
&CNTL2   SETC  'RQ2BYRED+&CNTL2'                                                
.*                                                                              
.RQD88   ANOP                                                                   
.*                                                                              
.* DEFINE TABLE ENTRY                                                           
&LBL     DC    AL2(&LBL.X-*)       LENGTH OF ENTRY                              
         DC    CL1'&PREFIX'        PREFIX CHARACTER                             
         DC    CL2'&ID'            ALPHA ID                                     
         DC    AL3(0)              DEFAULT VALUES                               
         DC    AL1(&PROC)          POST-VALIDATION PROCESS RTN NUMBER           
         DC    AL1(&CNTL)                       CONTROL 1                       
         DC    AL1(&CNTL2)                      CONTROL 2                       
.*                                                                              
&CDID    SETC  '&ID'                                                            
         AIF   (T'&CID EQ 'O').RQD90                                            
&CDID    SETC  '&CID'                                                           
.*                                                                              
.RQD90   ANOP                                                                   
         DC    CL2'&CDID'          RQST CARD ID                                 
.*                                                                              
         AIF   (T'&RRG EQ 'O').RQD100                                           
         AIF   ('&RRG' EQ 'Y').RQD95                                            
.*                                                                              
         DC    CL2'&RRG'           RRG ID                                       
         AGO   .RQD120                                                          
.*                                                                              
.RQD95   ANOP                                                                   
         DC    CL2'&ID'            RRD ID                                       
         AGO   .RQD120                                                          
.*                                                                              
.RQD100  ANOP                                                                   
         DC    XL2'00'             NO RRG ID                                    
.*                                                                              
.RQD120  ANOP                                                                   
         DC    CL20&NAME           MENU NAME                                    
         MEND                                                                   
         TITLE 'REREQ00 -- REQUEST DEFINITION TABLE'                            
         PRINT GEN                                                              
       ++INCLUDE REREQDEF1                                                      
       ++INCLUDE REREQDEF5                                                      
       ++INCLUDE REREQDEF2                                                      
       ++INCLUDE REREQDEF6                                                      
       ++INCLUDE REREQDEF3                                                      
       ++INCLUDE REREQDEF4                                                      
         DC    XL2'00'                                                          
         PRINT NOGEN                                                            
         TITLE 'REREQ00 -- TERMINAL I/O BLOCK DSECT  (TIOB)'                    
       ++INCLUDE FATIOB                                                         
         TITLE 'REREQ00 -- REREQTWA SCREEN WORK AREA'                           
         PRINT GEN                                                              
       ++INCLUDE REREQTWA          SCREEN MAP                                   
         TITLE 'REREQ00 -- REREQWRK TEMPORARY WORK AREA'                        
         PRINT GEN                                                              
       ++INCLUDE REREQWRK          TEMPORARY WORK AREA                          
         EJECT                                                                  
         ORG   IOWORK                                                           
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
*                                                                               
*- CONTROL FILE RECORD DSECTS FOLLOW  (CTGENFILE)                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*GERFPIOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE GERFPIOD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'088REREQ00   10/18/11'                                      
         END                                                                    
