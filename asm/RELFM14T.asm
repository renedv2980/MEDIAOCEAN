*          DATA SET RELFM14T   AT LEVEL 052 AS OF 02/11/04                      
*PHASE T80414A,*                                                                
*INCLUDE XSORT                                                                  
*INCLUDE NUMVAL                                                                 
*******E GETFACT                                                                
*******E DATCON                                                                 
         TITLE 'T80414 - RELFM14 - FILE STATION RECORD '                        
**********************************************************************          
*                                                                    *          
*       RELFM14 (T80414) --- REP FILE STATION RECORD                 *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY: (HISTORY PRIOR TO 1/97 HAS BEEN DELETED. BILL)     *          
*                                                                    *          
*                                                                    *          
* 06MAR97  SKU USE DDDARETAB FOR TV REPS INSTEAD OF REPREPS          *          
*                                                                    *          
* 08AUG97  SKU ADD PBS TO PRIMARY NETWORK AFFILIATION TABLE          *          
*                                                                    *          
* 15OCT97  SKU USE OPTION 5 FOR DARE REVISION METHOD OVERRIDE        *          
*                                                                    *          
* 23OCT97  BU  ACCEPT OLD/NEW REP OF DRK/UNK/NON                     *          
*                                                                    *          
* 24OCT97  BU  REQUIRE JOIN DATE TO BE MONDAY                        *          
*                                                                    *          
* 07NOV97  BU  ADD AFFILIATE = CBL                                   *          
*                                                                    *          
* 19DEC97  BU  ADD PASSIVE KEYS X'8301'/X'8302'                      *          
*                                                                    *          
* 08DEC98 ASTE ADD PASSIVE KEYS X'8303'                              *          
*                                                                    *          
* 12FEB98  BU  DON'T PERMIT ALTERNATE CALENDAR FLAG TO BE TURNED OFF *          
*                                                                    *          
* 22APR98  BU  RSTAOPTA:  LAST POSITION = BIAS EC RADIO STATION!     *          
*                                                                    *          
* 29APR98  BU  CHECK COMBO STATIONS IN PARENT:  ALLOW ONLY ONCE      *          
*                                                                    *          
* 30APR98  BU  FIX 'NON-PARTICIPATING COMBO' PASSIVE PROBLEM         *          
*                                                                    *          
* 11JUN98  BU  ADD AFFILIATE = PAX                                   *          
*                                                                    *          
* 24JUN98  SKU ADD FORMAT O FOR PAXON                                *          
*                                                                    *          
* 29JUN98  SKU ALLOW RADIO STATIONS TO BE ONLINE                     *          
*                                                                    *          
* 16JUL98  JRD REORGANIZE, PROBABLY FOR THE WORST                    *          
*              ADD MASTER/SUBSIDARY STATION CONTROL                  *          
*                                                                    *          
* 20OCT98  NRK DISPLAY CREATE AND LAST UPDATE DATES ON STA AND STAD  *          
*              SCREENS.                                              *          
*                                                                    *          
* 16DEC98  JRD DISABLE MASTER/SUB CONTROL TO GET THE MODULE IN       *          
*              ADD INVENTORY BOOK LIST FIELDS TO STAD                *          
*                                                                    *          
* 13JAN99  RHV ENTERPRISE II (H) FORMAT                              *          
*                                                                    *          
* 10FEB99  BU  UPGRADE FOR MASTER STATION                            *          
*                                                                    *          
* 12MAR99  BU  PERMIT OWNER CHANGE ONLY ON FRIDAY IF FILE PROFILE    *          
*              2 = X'20'                                             *          
*                                                                    *          
* 17MAR99  BU  PROHIBIT CHANGE IF LFM PROFILE 15 IS ON (BYTE 2=X'02')*          
*                                                                    *          
* 20MAY99  BU  ENHANCE MASTER ADDITION DATE CHECKING                 *          
*                                                                    *          
* 18JUN99  BU  PROFILE + MASTER VALIDATION IF LOCAL SIGNON           *          
*                                                                    *          
* 17JUN99  AST ALLOW USER TO LEAVE MARKET BLANK IF MARKET CODE       *          
*                IS PRESENT, USE MKT CODE NAME DEFAULT               *          
*                                                                    *          
* 04AUG99  SKU SPACE PAD TEAM CODE IN OFFTEAM VALIDATION             *          
*                                                                    *          
* 13SEP99  RHV ADD VCI STAR PLUS FORMAT S                            *          
*                                                                    *          
* 14SEP99  BU  SUPPRESS GROUP/SUBGROUP ON MASTER ADD                 *          
*                                                                    *          
* 18NOV99  RHV MORE OPTION BITS                                      *          
*                                                                    *          
* 19NOV99  BU  STAD: HISTORICAL TAKEOVER DATE                        *          
*                                                                    *          
* 09DEC99  BU  ENHANCE TESTAN RETURN                                 *          
*                                                                    *          
* 20SEP00  BU  ADD VCI STARS II AS EC                                *          
*                                                                    *          
* 02NOV00  BU  OVERRIDE MASTER STATION VALIDITY CHECKS, LEAVE DATE   *          
*                                                                    *          
* 16NOV00  BU  KEEP CLEARCHANNEL OUT OF MASTER STATION               *          
*                                                                    *          
* 02JAN01  ABOB ADD STATION ID NUMBER                                           
*                                                                    *          
* 24APR01  BU  ADD AFFILIATE = GEM (TELEMUNDO)                       *          
*                                                                    *          
* 25SEP01  BU  BACK BILL DATE DISAPPEARANCE FIX                      *          
*                                                                    *          
* 31OCT01  BU  ADD WIDE ORBIT TRAFFIC 'O'                            *          
*                                                                    *          
* 20NOV01  BU  ADD AFFILIATE = MUN (TELEMUNDO)                       *          
*                                                                    *          
* 07FEB02  HQ  ADD SIGN ON PASSIVE KEY X'8307'                       *          
*                                                                    *          
* 20MAR02  BU  ADD E-MAIL ADDRESSES TO MAIN SCREEN                   *          
*                                                                    *          
* 08MAY02  BU  CONFIRM VIA WEB MODIFICATIONS                         *          
*                                                                    *          
* 09JUL02  BU  ADD TRAFFIC CODES 'T' AND 'U'                         *          
*                                                                    *          
* 09JUL02  BU  ADD AFFILIATE CODE 'TF ' (TELEFUTURA)                 *          
*                                                                    *          
* 26JUL02  BU  ADD DEMO MKT CODE TO RECORD                           *          
*                                                                    *          
* 08OCT02  BU  ADD TRAFFIC CODES 'L' (NEW MARKETRON)                 *          
*                                                                    *          
* 13NOV02  BU  ADD UNIQUE ID PROCESSING                              *          
*              PLUS PASSIVE KEY X'8308'                              *          
*                                                                    *          
* 06JUN03  BU  ADD TRAFFIC CODE  'I' (NEW OSI)                       *          
*                                                                    *          
* 26JUN03  BU  TRAP UNIQUE ID CHANGE HIST, ENHANCE PASSIVE CHECK     *          
*                                                                    *          
* 30JUN03  BU  UNIQUE ID VALIDATION: CROSS-COMPANY CHECK             *          
*                                                                    *          
* 20OCT03  MN  PERSONAL ID FIELDS ON STADATE DISPLAY                 *          
*                                                                    *          
* 05DEC03  BU  VENDOR FIELD ON STADATE DISPLAY                       *          
*                                                                    *          
* 15DEC03  BU  TEST VENDOR 'ETRANST' ADDED                           *          
*                                                                    *          
* 10FEB04  BU  ADD TRAFFIC CODES 'Z' (VSS)                           *          
*                                                                    *          
*                    **  END TOMBSTONE  **                           *          
**********************************************************************          
T80414   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 126,T80414,R9,R8,RR=R3                                           
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
         ST    R3,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
         MVC   KEY,BKEY                                                         
         MVI   KEY+27,0                                                         
         MVC   KEY+28(4),BSVDA                                                  
*                                                                               
         OC    TWASINON,TWASINON   GLOBBER PROCESSING?                          
         BZ    MAIN0020            NO                                           
         MVC   FMSGEN,SPACES       SET AREA TO SPACE                            
         MVC   FMSGEN(20),=C'MASTER MAINTENANCE -'                              
         MVC   FMSGEN+21(20),TWAREPNM                                           
*                                  LOAD PRIMARY NAME                            
         CLC   TWAREPN2,SPACES     ADD WILL HAVE 2NDARY NAME                    
         BNH   MAIN0008            NO 2NDARY NAME                               
         MVC   FMSGEN+21(20),TWAREPN2                                           
MAIN0008 EQU   *                                                                
         LA    RF,FMSGEN+42                                                     
MAIN0010 EQU   *                                                                
         CLI   0(RF),X'40'         BYTE = SPACE?                                
         BNE   MAIN0015            NO                                           
         BCTR  RF,0                YES - BACK UP 1 SLOT                         
         B     MAIN0010            GO BACK AND CHECK NEXT                       
MAIN0015 EQU   *                                                                
         LA    RF,2(RF)            LEAVE SPACE                                  
         MVC   0(4,RF),=C'FILE'                                                 
         LA    R2,LFMRECH          SET RECORD/ACTION TO PROTECTED               
         OI    1(R2),X'20'                                                      
         FOUT  (R2)                                                             
         LA    R2,LFMACTH          SET RECORD/ACTION TO PROTECTED               
         OI    1(R2),X'20'                                                      
         FOUT  (R2)                                                             
MAIN0020 EQU   *                                                                
         LA    R0,REC                                                           
         ST    R0,AIOAREA                                                       
*                                                                               
         LA    R2,FMSLAST          POINT TO FIRST TITLE                         
         CLI   BREC,2                                                           
         BE    STA                                                              
         DC    H'0'                                                             
         EJECT                                                                  
STA      DS    0H                                                               
         CLI   BFMTSW,0            DISPLAY?                                     
         BNE   EDIT                NO  -                                        
*                                  YES -                                        
         BAS   RE,GETREC                                                        
STAD0020 DS    0H                                                               
         MVC   SVCLDT,RSTACLDT     SAVE CLOSED DATE                             
         MVC   SVHIST,RSTAHIST     SAVE HISTORY DATE                            
*                                                                               
* CHECK WHICH SCREEN TO DISPLAY                                                 
*                                                                               
* NOTE: ANY RECORD WHICH STARTS WITH 'STA' SHOULD BE                            
*  ADDED TO THE CHECK AT THE BEGINNING OF UPDCTRL                               
*                                                                               
         CLC   LFMREC(4),=C'STAC'  STATION CONTROL                              
         BNE   *+12                                                             
         L     RF,=A(DISCTRL)                                                   
         B     STAD0038                                                         
*                                                                               
         CLC   MASTER,=X'FFFF'     MASTER REP?                                  
         BNE   STAD0030            NO                                           
*                                                                               
         GOTO1 VGETEL,DMCB,(X'51',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          ACTIVE REP FOUND?                            
         BNE   STAD0022            YES                                          
         GOTO1 VGETEL,DMCB,(X'52',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          PREVIOUS ACTIVE REP FOUND?                   
         BNE   STAD0022            YES                                          
         DC    H'0'                REALLY SHOULDN'T HAPPEN                      
*                                                                               
STAD0022 DS    0H                  READ SUBSIDARY REP STATION RECORD            
         L     RE,DMCB+8           ADDRESS OF ELEMENT                           
         MVC   KEY(L'RSTAKEY),RSTAKEY                                           
K        USING RSTAKEY,KEY                                                      
         MVC   K.RSTAKREP,RSTAMCRC-RSTAMCEL(RE)                                 
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                UM?                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
STAD0030 DS    0H                                                               
         CLC   LFMREC(3),=C'COM'   COMPETITIVE                                  
         BNE   *+12                                                             
         L     RF,=A(DISCOMP)                                                   
         B     STAD0038                                                         
*                                                                               
*MN                                                                             
         CLC   LFMREC(4),=C'STAD'  STATION DATES                                
         BNE   STAD0032                                                         
         L     RF,=A(DISDATE)                                                   
         A     RF,RELO             RELOCATE BRANCH ADRESS                       
         BASR  RE,RF                                                            
                                                                                
         L     RF,=A(DISPID)                                                    
         A     RF,RELO             RELOCATE BRANCH ADRESS                       
         BASR  RE,RF                                                            
                                                                                
         L     RF,=A(DISPVEND)                                                  
         B     STAD0038                                                         
STAD0032 EQU   *                                                                
*MN                                                                             
*                                                                               
         CLC   LFMREC(4),=C'OFFT'  OFFICE TEAM                                  
         BNE   *+12                                                             
         L     RF,=A(DISOFTM)                                                   
         B     STAD0038                                                         
*                                                                               
         CLC   LFMREC(4),=C'STAU'  DDS ID/UID LISTING                           
         BNE   *+12                                                             
         L     RF,=A(DISDDSID)                                                  
         B     STAD0038                                                         
*                                                                               
         L     RF,=A(DISSTA)       DEFAULT STATION SCREEN                       
         B     STAD0038                                                         
*                                                                               
*  CALL THE CORRECT DISPLAY ROUTINE                                             
*                                                                               
STAD0038 DS    0H                                                               
         A     RF,RELO             RELOCATE BRANCH ADRESS                       
         BASR  RE,RF                                                            
         CLC   TWAREPN2,SPACES     GLOBBER 'MASTER ADD'?                        
         BNH   EXXMOD              NO  -                                        
         MVC   LFMACT(3),=C'ADD'   YES - CHANGE NEW ACTION                      
         LA    R2,FMSJDH           JOIN DATE                                    
         NI    4(R2),X'FF'-X'20'   TURN OFF PREVIOUSLY VALID                    
         LA    R2,FMSLDH           LEAVE DATE                                   
         NI    4(R2),X'FF'-X'20'   TURN OFF PREVIOUSLY VALID                    
         B     EXXMOD                                                           
         EJECT                                                                  
**********************************************************************          
*                                                                               
* FOR ACTION CHANGE, EXISTING RECORD MUST BE READ AND THE '02' ELS              
* PRESERVED SINCE THEY ARE MAINTAINED ON A SEPARATE SCREEN.  FIRST              
* ELEMENT IS ZEROED SINCE CODE REBUILDS ELEMENT FROM THE BEGINNING.             
* ANY OTHER ('03','04','05','06','07','08','09') ELEMENTS ARE DELETED           
* BEFORE EDIT STARTS.                                                           
* NOTE - NEW ACTIVITY ELEMENT 'F1' IS PRESERVED ALSO                            
* NOTE - '04' ELS ARE PRESERVED ALSO.  NOW PROCESSED ON A NEW SCREEN.           
*                                                                               
**********************************************************************          
EDIT     DS    0H                                                               
         CLC   MASTER,=X'FFFF'     MASTER REP?                                  
         BNE   EDIT0000            NO                                           
*                                                                               
         MVC   LFMMSG(L'MASTERR),MASTERR                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
MASTERR  DC    C'CANNOT CHANGE STATION AT MASTER LEVEL'                         
*                                                                               
EDIT0000 DS    0H                                                               
*                                                                               
*   IF GLOBBER GOT US HERE, WE CAME FROM THE MASTER.                            
*                                                                               
         OC    TWASINON,TWASINON   GLOBBER GOT US HERE?                         
         BNZ   STAE0005            YES - CHANGE IS PERMITTED                    
         TM    SVPGPBIT+1,X'02'    NO  - CHANGE PROHIBITED FOR SUB?             
         BNO   STAE0005            NO                                           
*                                                                               
         MVC   LFMMSG(L'NOCHGAD),NOCHGAD                                        
         CLI   BACT,C'A'           ADD REQUEST?                                 
         BE    EDIT0005            YES                                          
         MVC   LFMMSG(L'NOCHGCH),NOCHGCH                                        
EDIT0005 EQU   *                                                                
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
NOCHGCH  DC    C'CANNOT CHANGE STATION AT SUBSIDIARY LEVEL'                     
NOCHGAD  DC    C'CANNOT ADD STATION AT SUBSIDIARY LEVEL'                        
*                                                                               
STAE0005 DS    0H                                                               
         L     R7,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,R7                                                      
         GOTO1 CGETFACT,DMCB,WORK2                                              
         DROP  R7                                                               
*                                                                               
         LA    RF,WORK2                                                         
         USING FACTSD,RF                                                        
         MVC   USERLUID,FASYM      GET LUID                                     
         DROP  RF                                                               
*                                                                               
* CHECK WHICH SCREEN TO EDIT                                                    
*                                                                               
         CLC   LFMREC(3),=C'COM'   COMPETITIVE                                  
         BNE   *+12                                                             
         L     RF,=A(EDITCOMP)                                                  
         B     STAE0010                                                         
*                                                                               
****     CLC   LFMREC(4),=C'STAC'  STATION CONTROL - NO CHANGE ACTION           
****     BNE   *+12                                                             
****     L     RF,=A(EDITCTRL)                                                  
****     B     STAE0010                                                         
*                                                                               
*MN                                                                             
         CLC   LFMREC(4),=C'STAD'  STATION DATES                                
         BNE   STAE0007                                                         
         L     RF,=A(EDITDATE)                                                  
         A     RF,RELO             RELOCATE BRANCH ADRESS                       
         BASR  RE,RF                                                            
         BNZ   STAE0015            ERROR RETURN                                 
                                                                                
         L     RF,=A(EDITPID)                                                   
         A     RF,RELO             RELOCATE BRANCH ADRESS                       
         BASR  RE,RF                                                            
                                                                                
         L     RF,=A(EDITVEND)                                                  
         B     STAE0010                                                         
STAE0007 EQU   *                                                                
*MN                                                                             
*                                                                               
         CLC   LFMREC(4),=C'OFFT'  OFFICE TEAM                                  
         BNE   *+12                                                             
         L     RF,=A(EDITOFTM)                                                  
         B     STAE0010                                                         
*                                                                               
         B     STAE0020             DEFAULT STATION SCREEN                      
*                                                                               
*  CALL THE CORRECT EDIT ROUTINE                                                
*                                                                               
STAE0010 DS    0H                                                               
         A     RF,RELO             RELOCATE BRANCH ADRESS                       
         BASR  RE,RF                                                            
         BZ    FLFILE              CLEAN EDIT - GO REWRITE RECORD               
*                                                                               
* ERROR RETURNED - SEND TO SCREEN                                               
*                                                                               
STAE0015 DS    0H                                                               
         L     R2,DUB              RESET A(ERROR SCREEN FIELD)                  
         L     R3,DUB+4            RESET ERROR CODE                             
*                                                                               
         C     R3,=A(-1)           ERROR = MONTH AFTER EOM DATE?                
         BE    FLERR15                                                          
         C     R3,=A(-2)           ERROR = NO EOM RECORD?                       
         BE    FLERR16                                                          
         C     R3,=A(-3)           ERROR = OTHER ERROR IN EDIT DATE             
         BE    FLERR14                                                          
         C     R3,=A(-4)           ERROR = COMP. STATION CHANGE INV.            
         BE    FLERR19                                                          
         C     R3,=A(-5)           ERROR = HIST T/O DATE ERROR                  
         BE    FLERR24                                                          
         C     R3,=A(-6)           ERROR = HIST T/O DATE NOT MONDAY             
         BE    FLERR25                                                          
         C     R3,=A(-7)           ERROR = TAKE INV = Y OR BLNK                 
         BE    FLERR26                                                          
         C     R3,=A(-8)           ERROR = VENDOR NOT RECOGNIZED                
         BE    FLERR33                                                          
         B     ERROR                                                            
         EJECT                                                                  
*********************************************************************           
*  EDITING OF THE DEFAULT SCREEN IS HANDLED HERE                                
*********************************************************************           
STAE0020 DS    0H                                                               
         MVC   KEYSAVE2,KEY        SAVE KEY OF THIS STATION                     
         GOTO1 =A(SETSUBS),RR=Y    ESTABLISH A SUBSIDIARY LIST                  
         MVC   KEY,KEYSAVE2        RESTORE KEY                                  
*                                                                               
         CLI   BACT,C'A'           'ADD' REQUEST?                               
         BE    STAE0040            YES - DON'T TRY TO RETRIEVE KEY              
*                                                                               
         GOTO1 HIGH                NO  - GET THE KEY                            
         CLC   KEY(27),KEYSAVE2    KEY FOUND?                                   
         BE    *+6                 YES -                                        
         DC    H'0'                NO  - SHOULDN'T HAPPEN                       
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         MVI   BYTE4,0             CLEAR STORAGE                                
         GOTO1 VGETEL,DMCB,(X'08',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          NO EXTRA EXTENDED ELEMENT                    
         BE    STAE0035                                                         
         L     R5,DMCB+8                                                        
         USING RSTAXXEL,R5                                                      
*                                                                               
         MVC   BYTE4,RSTAOPTA      SAVE ALTERNATE OPTIONS FIELD                 
*                                                                               
         DROP  R5                                                               
STAE0035 EQU   *                                                                
         GOTO1 VDELELEM,DMCB,(3,REC)     COMMENT ELEMENTS                       
         GOTO1 VDELELEM,DMCB,(5,REC)     EXTENDED DESCRIP. ELEMENT              
         GOTO1 VDELELEM,DMCB,(6,REC)     SIGN ON ID'S ELEMENT                   
         GOTO1 VDELELEM,DMCB,(7,REC)     TWX ELEMENT                            
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'25',REC) EMAIL ELEMENTS                         
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         GOTO1 VDELELEM,DMCB,(8,REC)     EXTRA DESCRIP. ELEMENT                 
         GOTO1 VDELELEM,DMCB,(9,REC)     COMBINED STATION ELEMENT               
         GOTO1 =A(SAV0AELS),DMCB,(RC),(RA),RR=Y                                 
*                                  SAVE ANY COMBO ELEMENTS                      
         GOTO1 VDELELEM,DMCB,(X'0A',REC) NEW COMBINED STATION ELEMENT           
         GOTO1 VDELELEM,DMCB,(X'0B',REC) STATION COMMENT ELEMENT                
         GOTO1 VDELELEM,DMCB,(X'0C',REC) FORMER REP/NEW REP ELEMENT             
         GOTO1 VDELELEM,DMCB,(X'11',REC) DEMO MARKET ELEMENT                    
         LA    R4,REC+34           POINT R4 AT FIRST ELEMENT                    
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8              ZERO OUT '01' ELEMENT DATA                   
         B     STAE0080                                                         
         XC    2(0,R4),2(R4)                                                    
         SPACE 1                                                                
STAE0040 EQU   *                                                                
         CLC   TWAREPN2,SPACES     'MASTER ADD'? 2NDARY NAME IF YES             
         BNH   STAE0060            NO  -                                        
         MVC   REC+20(2),REPALPHA  YES - REPLACE REPCODE IN RECORD              
STAE0060 EQU   *                                                                
         MVC   REC+34(2),=X'0153'                                               
         MVC   REC+27(2),=Y(117)                                                
         SPACE 1                                                                
STAE0080 MVC   RSTAPROF,ZEROS                                                   
         CLI   BACT,C'A'                                                        
         BE    STAE0100                                                         
         MVC   RSTACLDT,SVCLDT     RESTORE SAVED CLOSE DATE                     
         MVC   RSTAHIST,SVHIST     RESTORE SAVED HISTORY DATE                   
STAE0100 EQU   *                                                                
*                                                                               
* CONTRACT TO STATION                                                           
         LA    R2,FMSCSTH                                                       
         MVI   RSTASTAT,0                                                       
         CLI   5(R2),0                                                          
         BE    STAE0160                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'NO '                                                  
         BNE   STAE0120                                                         
         OI    RSTASTAT,X'08'                                                   
         B     STAE0160                                                         
STAE0120 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'YES'                                                  
         BNE   FLERR2                                                           
STAE0160 DS    0H                                                               
*                                                                               
* RECAP                                                                         
STAE0200 LA    R2,FMSCAPH                                                       
         CLI   5(R2),0                                                          
         BE    STAE0280                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'NO '                                                  
         BNE   STAE0240                                                         
         OI    RSTASTAT,X'04'                                                   
         B     STAE0280                                                         
STAE0240 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'YES'                                                  
         BNE   FLERR2                                                           
STAE0280 DS    0H                                                               
*                                                                               
* CHANNEL                                                                       
         LA    R2,FMSCHH                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         TM    4(R2),X'08'         VALID NUM                                    
         BZ    FLERR3                                                           
         CLI   BKEY+26,C' '        CHECK TV                                     
         BE    STAE0300            YES                                          
         CLI   BKEY+26,C'L'        CHECK TV                                     
         BNE   STAE0320            NO                                           
STAE0300 EQU   *                                                                
         CLI   5(R2),2                                                          
         BH    FLERR2                                                           
         B     STAE0360                                                         
STAE0320 CLI   5(R2),3                                                          
         BL    FLERR2                                                           
         CLI   5(R2),4                                                          
         BH    FLERR2                                                           
STAE0360 BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    FLERR3                                                           
         STCM  R0,3,RSTACHAN                                                    
*                                                                               
* GROUP/SUBGROUP                                                                
         LA    R2,FMSGSGH                                                       
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         LA    R3,NOCHGERR                                                      
*                                                                               
*   FOLLOWING TEST PERMITS A CHANGE TO GROUP/SUBGROUP IF THE FINAL              
*        PROFILE BIT FOR 'FILE' IS ON.  THIS IS A TEMPORARY TEST                
*        TO FACILITATE CONVERSION OF KATZ STATION RECORDS, AND                  
*        SHOULD BE REMOVED WHEN THAT CHORE IS COMPLETED.                        
*                                                                               
         TM    SVPGPBIT+7,X'01'    TEST VERY LAST BIT OF PROFILE                
         BO    STAE0380            ON  - SKIP GROUP/SUBGRP TEST                 
         CLI   BACT,C'A'           TEST ADD                                     
         BE    *+14                                                             
         CLC   SVGRUP,WORK       TEST SAME GROUP                                
         BNE   ERROR               NO - ERROR                                   
STAE0380 EQU   *                                                                
         MVC   RSTAGRUP,WORK                                                    
         MVC   SVGRUP,RSTAGRUP                                                  
         SPACE 1                                                                
         XC    KEY,KEY             VALIDATE GROUP ON FILE                       
         MVI   KEY,7                                                            
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RSTAGRUP                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    STAE0400                                                         
         LA    R3,NOGRPERR                                                      
         B     ERROR                                                            
STAE0400 DS    0H                                                               
*                                                                               
* TRAFFIC FORMAT                                                                
         LA    R2,FMSTRFH                                                       
         ST    R2,FULL             SAVE A(R2) FOR RECEIVING ID EDIT             
         CLI   5(R2),0                                                          
         BE    STAE0520                                                         
         BAS   RE,EDITTRAF                                                      
         BNZ   FLERR2                                                           
* CROSS CHECK THAT ONLY FORMAT 'A' USED FOR RADIO (BAND A/F/C)                  
         CLI   BKEY+26,C' '        DEFAULT TELEVISION                           
         BE    STAE0440                                                         
         CLI   BKEY+26,C'L'        LOW POWER TELEVISION                         
         BE    STAE0440                                                         
         CLI   BKEY+26,C'T'        EXPLICIT TELEVISION                          
         BE    STAE0440                                                         
*                                                                               
* RELEASE FORMAT RESTRICTIONS ON RADIO STATIONS                                 
*                                                                               
*        CLI   8(R2),C'A'          ALLOW ONLY 'A' OR ' ' FOR RADIO              
*        BE    STAE0480                                                         
*        CLI   8(R2),C' '                                                       
*        BE    STAE0480                                                         
*        BNE   FLERR2                                                           
*                                                                               
         B     STAE0480                                                         
*                                                                               
STAE0440 CLI   8(R2),C'A'          DON'T ALLOW 'A' FOR TV                       
         BE    FLERR2                                                           
*                                                                               
STAE0480 MVC   RSTATRAF,8(R2)                                                   
STAE0520 DS    0H                                                               
*                                                                               
* TVB REGION                                                                    
         LA    R2,FMSTVBH                                                       
         CLI   5(R2),0                                                          
         BE    STAE0600                                                         
         L     R4,=A(TVBLST)                                                    
         A     R4,RELO                                                          
STAE0560 CLC   0(2,R4),8(R2)                                                    
         BE    *+20                                                             
         LA    R4,20(R4)                                                        
         CLI   0(R4),X'FF'                                                      
         BNE   *-18                                                             
*                                                                               
         B     FLERR2                                                           
         MVC   RSTATVB,8(R2)                                                    
STAE0600 DS    0H                                                               
*                                                                               
* JOIN DATE                                                                     
         LA    R2,FMSJDH                                                        
         XC    WORK,WORK                                                        
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    FLERR2                                                           
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
         CLI   DMCB,1              MONDAY RETURNED?                             
         BNE   FLERR17                                                          
*                                                                               
* LEAVE DATE                                                                    
         LA    R2,FMSLDH                                                        
         CLI   5(R2),0                                                          
         BE    STAE0640                                                         
         GOTO1 VDATVAL,(R1),(0,8(R2)),WORK+6                                    
         OC    0(4,R1),0(R1)                                                    
         BZ    FLERR2                                                           
STAE0640 GOTO1 VDATCON,DMCB,WORK,(3,RSTASTRT)                                   
         MVC   MSTRSTRT,WORK       SAVE START DATE                              
         OC    WORK+6(6),WORK+6                                                 
         BZ    STAE0650                                                         
*                                                                               
         GOTO1 (RF),(R1),WORK+6,(3,RSTAEND)                                     
         CLC   RSTASTRT,RSTAEND    START DATE BEFORE END DATE?                  
         BH    FLERR23             NO  - ERROR                                  
***>>>   B     STAE0680                                                         
*                                                                               
STAE0650 DS    0H                                                               
         CLC   RSTAKREP(2),=C'NU'                                               
         BE    STAE0680                                                         
*                                  KEEP CLEARCHANNEL FROM MASTER STN            
         CLC   MASTER,SPACES       VALIDATE CONTROL FOR MASTER/SUB              
         BNH   STAE0680                                                         
*                                                                               
*                                  MASTER ADD?                                  
***      CLC   TWAREPN2,SPACES     ADD WILL HAVE 2NDARY NAME                    
***      BH    STAE0680            2NDARY NAME - DON'T VALIDATE                 
*                                                                               
         OC    TWASINON,TWASINON   GLOBBER PROCESSING?                          
         BNZ   STAE0660            YES - DO 'VALCTRL' - NO PROFILE              
         TM    SVPGPBIT+1,X'01'    SUB IN USE: SKIP VAL CONTROL?                
         BNO   STAE0680            YES - SKIP                                   
STAE0660 EQU   *                                                                
         MVI   NOUPDATE,0          SET 'MASTER UPDATE NEEDED' FLAG              
         LA    R2,FMSJDH           JOIN DATE                                    
         TM    4(R2),X'20'         PREVIOUSLY VALID SET?                        
         BNO   STAE0670            NO  - DO DATE CHECKING                       
         LA    R2,FMSLDH           LEAVE DATE                                   
         TM    4(R2),X'20'         PREVIOUSLY VALID SET?                        
         BNO   STAE0670            NO  - DO DATE CHECKING                       
         MVI   NOUPDATE,1          YES - SET 'NO MASTER UPDATE' FLAG            
         B     STAE0680            SKIP DATE CHECKING                           
STAE0670 EQU   *                                                                
         GOTO1 =A(VALCTRL),RR=Y                                                 
         BE    STAE0680                                                         
*                                                                               
         TM    SVPGPBIT+2,X'40'    IGNORE OVERLAP CHECK?                        
         BO    STAE0680            YES                                          
*                                                                               
         CLI   BYTE,1              DATE OVERLAP ERROR?                          
         BNE   FLERR20             NO  - EXIT WITH OTHER MESSAGE                
         LA    R2,FMSJDH           YES - SET A(JOIN DATE FIELD)                 
         B     FLERR22             EXIT WITH OVERLAP MESSAGE                    
*                                                                               
STAE0680 DS    0H                                                               
*                                                                               
* OWNER                                                                         
         LA    R2,FMSOWNH                                                       
         CLI   5(R2),0             ANYTHING IN OWNER FIELD?                     
         BNE   STAE0690            YES                                          
         TM    SVPGPBIT+1,X'10'    NO  - OWNER REQUIRED?                        
         BO    FLERR11             YES - ERROR                                  
         TM    SVPGPBIT+1,X'04'    PROFILE = CHANGE ONLY ON FRIDAY?             
         BNO   STAE0720            NO  - OKAY AS ENTERED                        
         CLI   BACT,C'A'           YES - ADD ACTION? (NEW STATION?)             
         BE    STAE0720            YES - PERMIT CHANGE AS 'ADD'                 
         TM    4(R2),X'20'         NO  - HAS FIELD BEEN CHANGED?                
         BO    STAE0720            NO  - FIELD IS UNCHANGED                     
         GOTO1 VDATCON,DMCB,(5,WORK),(0,WORK)                                   
*                                  INSERT TODAY'S DATE EFFECTIVE                
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
*EST     MVI   DMCB,1              TEST                                         
         CLI   DMCB,5              FRIDAY RETURNED?                             
         BNE   FLERR21                                                          
         B     STAE0720            NO  - OWNER NOT REQUIRED                     
STAE0690 EQU   *                                                                
         TM    4(R2),X'20'         NO  - HAS FIELD BEEN CHANGED?                
         BO    STAE0710            NO  - FIELD IS UNCHANGED                     
         TM    SVPGPBIT+1,X'04'    PROFILE = CHANGE ONLY ON FRIDAY?             
         BNO   STAE0710            NO                                           
         CLI   BACT,C'A'           YES - ADD ACTION? (NEW STATION?)             
         BE    STAE0710            YES - PERMIT CHANGE AS 'ADD'                 
         GOTO1 VDATCON,DMCB,(5,WORK),(0,WORK)                                   
*                                  INSERT TODAY'S DATE EFFECTIVE                
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
*ESTA    MVI   DMCB,1              TEST                                         
         CLI   DMCB,5              FRIDAY RETURNED?                             
         BNE   FLERR21                                                          
STAE0710 EQU   *                                                                
         CLI   5(R2),3             MUST INPUT 3 CHARS                           
         BNE   FLERR2                                                           
*                                  READ OWNERSHIP RECORD FOR NAME               
         XC    KEY,KEY                                                          
         MVI   KEY,X'2A'                                                        
         MVC   KEY+22(2),REPALPHA                                               
         MVC   KEY+24(3),8(R2)                                                  
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+12                                                             
         LA    R3,ERRNF            RECORD NOT FOUND                             
         B     ERROR                                                            
*                                                                               
         MVC   RSTAOWN,8(R2)                                                    
STAE0720 DS    0H                                                               
*                                                                               
* RANK                                                                          
         LA    R2,FMSRNKH                                                       
         CLI   5(R2),0                                                          
         BNE   STAE0760                                                         
         LA    R1,SVPGPBIT         PROGRAM PROFILE BITS                         
         TM    0(R1),X'08'         4TH BIT ON = RANK REQUIRED                   
         BZ    STAE0800            NOT REQUIRED                                 
         LR    R1,RA                CHECK FOR DDS TERMINAL                      
         USING TWAD,R1                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    STAE0800            YES - IGNORE TEST                            
         DROP  R1                                                               
         B     FLERR1              REQUIRED = ERROR                             
STAE0760 TM    4(R2),X'08'         VALID NUMERIC                                
         BZ    FLERR3                                                           
         CLI   8(R2),C'7'          MUST BE FROM 1-7                             
         BH    FLERR2                                                           
         MVC   RSTARANK,8(R2)                                                   
STAE0800 DS    0H                                                               
*                                                                               
* STATUS                                                                        
         LA    R2,FMSSTH                                                        
         CLI   5(R2),0                                                          
         BE    STAE0805                                                         
         BAS   RE,SCANSTAT         EDIT STATUS FIELD                            
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
*                                                                               
*                                                                               
STAE0805 DS    0H                                                               
*                                                                               
* EMAIL                                                                         
         CLI   FMSEM1H+5,0         ANY INPUT ?                                  
         BNE   STAE0807            YES                                          
         CLI   FMSOPT+21,C'Y'      CONFIRM VIA WEB?                             
         BNE   STAE0840            NO  - NO EMAIL ADDR NEEDED                   
         LA    R2,FMSEM1H          SET CURSOR ADDRESS                           
         B     FLERR29             YES - ERROR: NEED ADDR                       
STAE0807 EQU   *                                                                
*                                                                               
         LA    R6,FMSEM1H          A(E-MAIL ADDRESSES)                          
         LA    R5,WORK2                                                         
         XC    WORK2(200),WORK2    CLEAR ELEMENT AREA                           
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         GOTO1 CSCANNER,DMCB,(48,(R6)),(3,(R5)),0                               
         CLI   DMCB+4,0                                                         
         BE    FLERR27                                                          
         LA    R5,WORK2            SET A(SCANNER AREA)                          
*                                                                               
STAE0810 DS    0H                                                               
         CLI   0(R5),0                  ANY ENTRY IN SCANNER AREA?              
         BZ    STAE0840                 NO  - FINISHED                          
         XC    WORK2+200(64),WORK2+200                                          
*                                       CLEAR ELEMENT BUILD AREA                
         LA    R3,WORK2+200        BIULD THE ELEMENT                            
         USING RSTAEML,R3                                                       
*                                                                               
         MVI   RSTAEMC,X'25'       ELEMENT CODE                                 
         ZIC   R2,0(R5)            LENGTH OF SCANNER INPUT                      
         BCTR  R2,0                     DECREMENT FOR EX                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   RSTAADD(0),12(R5)   E-MAIL FROM SCANNER TO ELEMENT               
         AHI   R2,4                ADD CODE + LENGTH + FLAG BYTES + EX          
         STC   R2,RSTAEMLN         ELEMENT LENGTH                               
*                                                                               
**       CLI   FMSEM2,C'Y'         SEND AN EMAIL ?                              
**       BNE   *+12                                                             
**       OI    RSTAFLG,RSTAFLGQ    TURN FLAG ON                                 
**       B     *+8                                                              
**       NI    RSTAFLG,X'FF'-RSTAFLGQ                                           
         DROP  R3                                                               
*                                                                               
         GOTO1 VADDELEM,DMCB,REC,WORK2+200                                      
         LA    R5,70(R5)                BUMP TO NEXT SCANNER FIELD              
         B     STAE0810                 GO BACK FOR NEXT                        
*                                                                               
*                                                                               
STAE0840 DS    0H                                                               
*                                                                               
* FORMER REP/NEW REP                                                            
         CLI   FMSNEWH+5,0         ANYTHING ENTERED IN NEW REP FIELD?           
         BE    STAE0870            NO                                           
         CLI   FMSLDH+5,0          YES, IS LEAVE DATE ENTERED?                  
         BNE   STAE0875            YES, EDIT THE FIELDS                         
         LA    R3,300              CAN'T ENTER NEW REP W/O LEAVE DATE           
         LA    R2,FMSNEWH                                                       
         B     MYERROR                                                          
*                                                                               
STAE0870 CLI   FMSFORH+5,0         ANYTHING ENTERED IN FORMER REP FLD?          
         BE    STAE0880            NO                                           
*                                                                               
STAE0875 GOTO1 =A(FNEDT),DMCB,(RC),(RA),RR=Y  EDIT FORMER/NEW REP FLDS          
         BE    STAE0880            NO ERRORS                                    
         LA    R3,102              INVALID REP ERROR                            
         B     MYERROR                                                          
STAE0880 DS    0H                                                               
*                                                                               
* RECEIVING ID                                                                  
         LA    R2,FMSRIDH                                                       
         SPACE 1                                                                
         CLI   RSTATRAF,C'A'       RADIO MUST HAVE 'GRAPH'                      
         BE    *+12                 RECEIVING ID                                
         CLI   RSTATRAF,C'G'       GRAPHNET FORMAT MUST HAVE                    
         BNE   STAE0920            'GRAPH' RECEIVING ID                         
         CLC   8(5,R2),=C'GRAPH'                                                
         BNE   STAE0900            RADIO, NOT 'GRAPH'                           
         CLI   5(R2),5                                                          
         BNE   STAE0900                                                         
         B     STAE0960                                                         
         SPACE 1                                                                
STAE0900 EQU   *                                                                
         CLI   FMSOPT+21,C'Y'      CONFIRM VIA WEB STATION?                     
         BE    STAE0960            YES - ACCEPT WHAT'S IN FIELD                 
         B     FLERR28             NO  - CONSIDER ERROR                         
STAE0920 CLI   5(R2),0                                                          
         BE    STAE1000                                                         
         CLC   8(5,R2),=C'GRAPH'   NON-GRAPHNET FORMAT CAN'T                    
         BE    FLERR2              HAVE 'GRAPH' RECEIVING ID                    
*                                                                               
STAE0960 BAS   RE,MOVE                                                          
         BAS   RE,RIDEDT           EDIT RECEIVING ID FIELD                      
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
*                                                                               
         OC    RSTATRAF,RSTATRAF   IF THERE'S RECEIVING ID,                     
         BNZ   STAE1000            THERE MUST BE A TRAFFIC TYPE                 
         L     R2,FULL             POINT TO TRAFFIC FIELD                       
         B     FLERR1              MISSING INPUT                                
STAE1000 DS    0H                                                               
*                                                                               
* SIGN ON ID'S                                                                  
         LA    R2,FMSSIDH                                                       
         CLI   5(R2),0                                                          
         BE    STAE1040                                                         
*                                                                               
         BAS   RE,SOEDT            EDIT SIGN ON ID FIELD                        
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
STAE1040 DS    0H                                                               
*                                                                               
* PRIMARY AFFILIATE                                                             
         LA    R2,FMSAFH                                                        
         LA    R1,*                                                             
         A     R1,=A(AFFLLIST-(*-4))                                            
STAE1080 DS    0H                                                               
         CLI   0(R1),0                                                          
         BE    STAE1120                                                         
         OI    10(R2),X'40'        SPACE OUT BIN ZERO FOR                       
*                                     TWO-CHARACTER AFFILIATE CODES             
         CLC   0(3,R1),8(R2)                                                    
         BE    STAE1160                                                         
         LA    R1,3(R1)                                                         
         B     STAE1080                                                         
STAE1120 DS    0H                                                               
         CLI   BKEY+26,C' '        TV MUST HAVE AFFIL                           
         BE    FLERR2                                                           
         CLI   5(R2),0             RADIO MAY BE BLANK                           
         BNE   FLERR2                                                           
         OC    8(3,R2),=CL3' '                                                  
STAE1160 MVC   RSTAAFFL,8(R2)                                                   
*                                                                               
* TWX NUMBER                                                                    
         LA    R2,FMSTWXH                                                       
         CLI   5(R2),0                                                          
         BE    STAE1200                                                         
         XC    WORK2(64),WORK2                                                  
         MVC   WORK2(2),=X'0740'                                                
         MVC   WORK2+2(20),8(R2)                                                
         GOTO1 VADDELEM,DMCB,REC,WORK2                                          
STAE1200 DS    0H                                                               
*                                                                               
* UID NUMBER                                                                    
         LA    R2,FMSUIDH                                                       
         CLI   5(R2),0                                                          
         BE    STAE1220                                                         
         TM    FMSUIDH+4,X'20'     PREVIOUSLY VALIDATED?                        
         BO    STAE1220            YES - OKAY                                   
         XC    WORK,WORK           CLEAR STORAGE                                
         GOTO1 VGETEL,DMCB,(X'2A',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          RETRIEVE UID ELEMENT                         
         BE    STAE1205            NOT FOUND                                    
         ZICM  RF,DMCB+8,4         FOUND - TAKE ADDRESS                         
         ZIC   RE,1(RF)            GET LENGTH                                   
         BCTR  RE,0                BACK OFF 1 CHARACTER                         
         EX    RE,STAE1201                                                      
         MVI   WORK,X'2B'          RESET ELEMENT ID                             
         MVI   WORK+1,24           SET ELEMENT LENGTH                           
         GOTO1 VDATCON,DMCB,(5,WORK2),(3,WORK+2)                                
*                                  INSERT TODAY'S DATE                          
         MVC   WORK+05(6),WORK+26  INSERT UID                                   
         MVC   WORK+11(8),USERLUID                                              
*                                  INSERT LUID MAKING CHANGE                    
         GOTO1 VADDELEM,DMCB,REC,WORK                                           
         B     STAE1205                                                         
STAE1201 MVC   WORK+24(0),0(RF)    SAVE UID ELEMENT                             
STAE1205 DS    0H                                                               
         CLC   =C'DELETE',FMSUID   REQUEST TO DROP UID?                         
         BNE   STAE1210            NO                                           
         GOTO1 VDELELEM,DMCB,(X'2A',REC)     DROP OLD UNIQUE ID ELT             
         B     STAE1220                                                         
STAE1210 EQU   *                                                                
         GOTO1 =A(UNIQUEID),DMCB,(RC),(RA),RR=Y                                 
         BZ    STAE1220            NO ERROR - PROCEED                           
         CLI   DUB,2               BAD UID?                                     
         BE    FLERR31                                                          
         CLI   DUB,1               UID ALREADY USED?                            
         BE    FLERR32                                                          
         DC    H'0'                UNRECOGNIZED RETURN                          
STAE1220 DS    0H                                                               
*                                                                               
* LIABILITY COMMENT NUMBER                                                      
         XC    RSTALIAB,RSTALIAB                                                
         LA    R2,FMSLIBH                                                       
         CLI   5(R2),0                                                          
         BNE   STAE1240                                                         
*                                                                               
         CLC   FMSLIC+14(08),=C'UID MKT='                                       
*                                  LIABILITY EXPAND USED FOR UNIQUE ID?         
         BE    STAE1280            YES - DON'T CLEAR IT OUT                     
*                                                                               
         MVC   FMSLIC,SPACES                                                    
         FOUT  FMSLICH                                                          
         B     STAE1280                                                         
*                                                                               
STAE1240 CLI   5(R2),2                                                          
         BNE   FLERR5                                                           
         LA    RE,8(R2)                                                         
         ZIC   RF,5(R2)                                                         
         STM   RE,RF,DMCB                                                       
         GOTO1 =V(NUMVAL),DMCB,RR=Y                                             
         CLI   DMCB,0                                                           
         BNE   FLERR5                                                           
         CLC   DMCB+4(4),=F'0'                                                  
         BL    FLERR5                                                           
         CLC   DMCB+4(4),=F'100'                                                
         BH    FLERR5                                                           
         MVC   RSTALIAB,DMCB+4+3                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING RCMTD,R3                                                         
         MVI   RCMTKTYP,X'2E'      RECORD TYPE                                  
         MVC   RCMTKREP,REPALPHA   REP CODE                                     
         MVC   RCMTKOFF,=X'FFFF'   OFFICE CODE                                  
         MVC   RCMTKCDE(4),=C'LIAB'  LIABILITY COMMENT CODE                     
         MVC   RCMTKCDE+4(2),8(R2)                                              
         OC    RCMTKCDE,SPACES     BLANK PADDED                                 
         DROP  R3                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BE    STAE1280                                                         
         XC    RSTALIAB,RSTALIAB                                                
         LA    R3,283              LIABILITY REC NOT FOUND                      
         B     MYERROR                                                          
*                                                                               
STAE1280 DS    0H                                                               
*                                                                               
* CONTRACT COMMENT                                                              
         LA    R2,FMSCCMH                                                       
         CLI   5(R2),0                                                          
         BE    STAE1320                                                         
         BAS   RE,CMTEDT           EDIT CONTRACT COMMENT                        
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         SPACE 1                                                                
STAE1320 DS    0H                                                               
STAE1400 EQU   *                                                                
*                                                                               
* STATION COMMENTS:                                                             
         LA    R0,2                                                             
         LA    R2,FMSSC1H          A(1ST STATION COMMENT HEADER)                
STAE1440 EQU   *                                                                
         CLI   5(R2),0             ANY VALUE IN FIELD?                          
         BE    STAE1560            NO  - LOOP EACH FIELD                        
         XC    WORK2(100),WORK2    YES - SET UP ELEMENT                         
         MVI   WORK2,X'0B'         INSERT ELEMENT ID                            
         ZIC   RF,5(R2)            INSERT LENGTH OF ELEMENT                     
         LA    RF,2(RF)            ADD TWO FOR ELEMENT INFORMATION              
         STC   RF,WORK2+1          INSERT INTO ELEMENT                          
         LA    RE,3                SET UP TO MOVE COMMENT BY LENGTH             
         SR    RF,RE                                                            
         EX    RF,STAE1480         MOVE BY LENGTH                               
         B     STAE1520                                                         
*                                                                               
STAE1480 MVC   WORK2+2(0),8(R2)    MOVE COMMENT TO ELEMENT                      
*                                                                               
STAE1520 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,REC,WORK2                                          
*                                  ADD COMMENT ELEMENT TO RECORD                
STAE1560 EQU   *                                                                
         ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         BCT   R0,STAE1440         GO BACK FOR NEXT FIELD                       
*                                                                               
STAE1600 EQU   *                                                                
*                                                                               
* RADIO COMBOS                                                                  
         LA    R2,FMSCMH           COMBO FIELD                                  
         CLI   5(R2),0                                                          
         BE    STAE1640                                                         
         CLI   BKEY+26,C'C'        COMBINED STA CAN'T HAVE COMBO FLD            
         BE    FLERR2                                                           
         CLI   BKEY+26,C' '        TV CAN'T HAVE COMBO FLD                      
         BE    FLERR2                                                           
         CLI   BKEY+26,C'T'        TV CAN'T HAVE COMBO FLD                      
         BE    FLERR2                                                           
         CLI   BKEY+26,C'L'        TV CAN'T HAVE COMBO FLD                      
         BE    FLERR2                                                           
         CLI   5(R2),5             INPUT LEN MUST BE 5 OR 6 CHARS               
         BE    *+16                                                             
         CLI   5(R2),6                                                          
         BE    *+20                                                             
         B     FLERR2                                                           
         CLI   8+4(R2),C'C'        IF INPUT LEN=5, MUST BE                      
         BNE   FLERR2                                                           
         B     *+14                                                             
         CLC   =C'-C',8+4(R2)      IF INPUT LEN=6, MUST BE                      
         BNE   FLERR2                                                           
* BUILD STATION RECORD (MAKE SURE COMBO EXISTS)                                 
         LA    R3,STAERR                                                        
         XC    KEY,KEY             VERIFY STATION IS VALID                      
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(4),8(R2)                                                  
         OC    KEY+22(4),SPACES                                                 
         MVI   KEY+26,C'C'                                                      
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   ERROR                                                            
* BUILD X'0A' COMBINED STATION ELEM                                             
         XC    WORK2(256),WORK2                                                 
         MVC   WORK2(2),=X'0A0A'   NEW LENGTH: 10 CHARS                         
         MVC   WORK2+2(5),KEY+22                                                
         GOTO1 VADDELEM,DMCB,REC,WORK2                                          
* EDIT CM1-4                                                                    
STAE1640 DS    0H                                                               
         MVI   PREFSTAT,C'N'       SET PREFERRED STATION FLAG                   
         LA    R2,FMSCM1H          COMBO 1                                      
*                                                                               
* PROGRAMMERS NOTE:                                                             
*     VALUE OF NEXT INSTRUCTION DETERMINES NUMBER OF 'CM' FIELDS                
*      (COMBO STATIONS) THAT WILL BE USED.  THIS NUMBER SHOULD                  
*      CORRESPOND TO THE NUMBER OF UNPROTECTED FIELDS WITHIN                    
*      THE SCREEN.                                                              
*                                                                               
         LA    R4,4                4 COMBO FIELDS                               
STAE1680 CLI   5(R2),0                                                          
         BE    STAE2000                                                         
         CLI   BKEY+26,C'C'        CHECK FOR COMBINED STATION                   
         BNE   FLERR2              ONLY COMBINED STA CAN HAVE 1-4               
*                                                                               
         XC    WORK2(10),WORK2     INITIALIZE COMBINED STA ELT                  
         LA    R3,STAERR                                                        
         XC    KEY,KEY             VERIFY STATION IS VALID                      
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(4),8(R2)                                                  
         OC    KEY+22(4),SPACES                                                 
*                                                                               
         CLI   5(R2),5             INPUT LENGTH MUST BE 5 OR 6                  
         BE    STAE1880                                                         
         BL    FLERR2              LESS THAN 5:  ERROR                          
         CLI   8+5(R2),C'*'        6TH AN ASTERISK?                             
         BNE   STAE1720            NO                                           
         CLI   5(R2),6             YES - ONLY ALLOW 6 CHARS                     
         BNE   FLERR2              ERROR                                        
         MVI   WORK2+7,C'*'        SET PREFERRED STATION                        
         CLI   PREFSTAT,C'N'       FIRST PREFERRED STAT?                        
         BNE   FLERR2              NO  - CAN'T HAVE TWO                         
         MVI   PREFSTAT,C'Y'       YES - SET TO 'ENTERED'                       
         B     STAE1880            PROCESS AS IF 5 CHARACTERS                   
STAE1720 EQU   *                                                                
         CLI   8+5(R2),C'-'        6TH A MINUS?                                 
         BNE   STAE1760            NO                                           
         CLI   5(R2),6             YES - ONLY ALLOW 6 CHARS                     
         BNE   FLERR2              ERROR                                        
         MVI   WORK2+7,C'-'        SET MINUSED STATION                          
         B     STAE1880            PROCESS AS IF 5 CHARACTERS                   
STAE1760 EQU   *                                                                
         CLI   8+6(R2),C'*'        7TH AN ASTERISK?                             
         BNE   STAE1800            NO                                           
         CLI   5(R2),7             YES - ONLY ALLOW 7 CHARS                     
         BNE   FLERR2              ERROR                                        
         MVI   WORK2+7,C'*'        SET PREFERRED STATION                        
         CLI   PREFSTAT,C'N'       FIRST PREFERRED STAT?                        
         BNE   FLERR2              NO  - CAN'T HAVE TWO                         
         MVI   PREFSTAT,C'Y'       YES - SET TO 'ENTERED'                       
         B     STAE1920            PROCESS AS IF 6 CHARACTERS                   
STAE1800 EQU   *                                                                
         CLI   8+6(R2),C'-'        7TH A MINUS?                                 
         BNE   STAE1840            NO                                           
         CLI   5(R2),7             YES - ONLY ALLOW 7 CHARS                     
         BNE   FLERR2              ERROR                                        
         MVI   WORK2+7,C'-'        SET MINUSED STATION                          
         B     STAE1920            PROCESS AS IF 6 CHARACTERS                   
STAE1840 EQU   *                                                                
         CLI   5(R2),6                                                          
         BE    STAE1920                                                         
         B     FLERR2                                                           
*                                                                               
STAE1880 CLI   8+4(R2),C'A'                                                     
         BNE   *+12                                                             
         MVI   KEY+26,C'A'                                                      
         B     STAE1960                                                         
         CLI   8+4(R2),C'F'                                                     
         BNE   FLERR2                                                           
         MVI   KEY+26,C'F'                                                      
         B     STAE1960                                                         
STAE1920 CLC   =C'-A',8+4(R2)                                                   
         BNE   *+12                                                             
         MVI   KEY+26,C'A'                                                      
         B     STAE1960                                                         
         CLC   =C'-F',8+4(R2)                                                   
         BNE   FLERR2                                                           
         MVI   KEY+26,C'F'                                                      
STAE1960 BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   ERROR                                                            
*                                                                               
         MVC   WORK2(2),=X'0A0A'   NEW LENGTH                                   
         MVC   WORK2+2(5),KEY+22                                                
*                                                                               
         GOTO1 VDATCON,DMCB,(5,WORK),(2,WORK2+8)                                
*                                  INSERT TODAY'S DATE EFFECTIVE                
         PRINT GEN                                                              
         GOTO1 VADDELEM,DMCB,REC,WORK2                                          
         PRINT NOGEN                                                            
STAE2000 DS    0H                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVC   KEY(2),=X'8301'     INSERT RECORD TYPE                           
         MVC   KEY+15(2),REPALPHA  INSERT RECORD TYPE                           
         MVC   KEY+17(5),WORK2+2   INSERT CHILD STATION                         
         BAS   RE,HIGH             READ KEY                                     
         B     STAE2020                                                         
STAE2010 EQU   *                                                                
         BAS   RE,SEQ              READ NEXT RECORD                             
STAE2020 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY THROUGH CHILD?                      
         BNE   STAE2030            NO  - ACCEPT KEY                             
         CLC   KEY+22(5),RSTAKSTA  YES - SAME PARENT?                           
         BE    STAE2010            YES - READ NEXT KEY                          
         B     FLERR18                                                          
STAE2030 EQU   *                                                                
         BAS   RE,NEXTUF                                                        
         BCT   R4,STAE1680                                                      
STAE2040 DS    0H                                                               
         OC    COMBAREA,COMBAREA   ANY OLD X'0A' ELTS?                          
         BZ    STAE2080            NO  - ACCEPT NEW ONES                        
         GOTO1 =A(CHK0AELS),DMCB,(RC),(RA),RR=Y                                 
*                                  YES - INSERT OLD DATES                       
****>>>  BNZ   FLERR6                                                           
*                                                                               
*   ABOVE BRANCH TO BE REMOVED WHEN STATIONS CAN BE DELETED.                    
*                                                                               
STAE2080 EQU   *                                                                
*                                                                               
*   ADD/UPDATE STATION ID ELEMENT                                               
*   ABOB ADDED                                                                  
*                                                                               
         GOTO1 =A(SIDNEDT),DMCB,(RC),RR=Y                                       
         BZ    SIDN0001                                                         
*                                                                               
*        ERROR CHECKING                                                         
*                                                                               
         CLI   DMCB,1            INVALID STATION ID NUMBER                      
         BNE   *+12                                                             
         LA    R2,FMSSTNH                                                       
         B     FLERR2                                                           
*                                                                               
SIDN0001 DS    0H                                                               
***************************************************************                 
* EDIT ALL FIELDS ON EXTENDED DESC ELEM                                         
* (DEST FMT, OPTIONS, MKT CODE, DEST ID, INV, A/R INTERFACE CODE,               
*  FAX #, SECONDARY AFFL, TIME ZONE, ELEC CON, LUID, INVOICE)                   
* ALSO BUILD AND ADD X'11' DEMO MARKET CODE, IF ENTERED                         
*                                                                               
         GOTO1 =A(OPTEDT),DMCB,(RC),RR=Y                                        
         BZ    STAE2120                                                         
         LA    R2,FMSRWSH                                                       
         CLI   DMCB,1                                                           
         BE    FLERR2                                                           
*                                                                               
         LA    R2,FMSRDSH                                                       
         CLI   DMCB,2                                                           
         BE    FLERR2                                                           
*                                                                               
         CLI   DMCB,3                                                           
         BE    FLERR2                                                           
         CLI   DMCB,4                                                           
         BE    FLERR4                                                           
         CLI   DMCB,5              ERROR: INTERFACE CODE REQUIRED               
         BNE   *+12                NO                                           
         LA    R2,FMSINTH                                                       
         B     FLERR1                                                           
         CLI   DMCB,6              ERROR: MARKET CODE REQUIRED                  
         BNE   *+12                                                             
         LA    R2,FMSMCDH                                                       
         B     FLERR1                                                           
         CLI   DMCB,7                                                           
         BNE   *+12                                                             
         LA    R2,FMSAF2H                                                       
         B     FLERR2                                                           
         CLI   DMCB,8                                                           
         BNE   *+12                                                             
         LA    R2,FMSTZH                                                        
         B     FLERR2                                                           
         CLI   DMCB,9                                                           
         BNE   *+12                                                             
         LA    R2,FMSECH                                                        
         B     FLERR2                                                           
         CLI   DMCB,10                                                          
         BNE   *+12                                                             
         LA    R2,FMSINVH                                                       
         B     FLERR2                                                           
         CLI   DMCB,11                                                          
         BNE   *+12                                                             
         LA    R2,FMSLUH                                                        
         B     FLERR2                                                           
         CLI   DMCB,12                                                          
         BNE   *+12                                                             
         LA    R2,FMSECH           NOT EC TRAFFIC FORMAT                        
         B     FLERR8                                                           
         CLI   DMCB,13                                                          
         BNE   *+12                                                             
         L     R2,DMCB+4           FAX/MB= NOT NUMERIC                          
         B     FLERR9                                                           
         CLI   DMCB,14                                                          
         BNE   *+12                                                             
         L     R2,DMCB+4           MB= NOT 8 DIGITS NUMERIC                     
         B     FLERR10                                                          
         CLI   DMCB,15                                                          
         BNE   *+12                                                             
         L     R2,DMCB+4           INVALID ID,MB,OR FAX#                        
         B     FLERR12                                                          
         CLI   DMCB,16                                                          
         BNE   *+12                                                             
         L     R2,DMCB+4           CAN'T USE 'GRAPH' IN DEST FIELD              
         B     FLERR13                                                          
         CLI   DMCB,17                                                          
         BNE   *+12                                                             
         L     R2,DMCB+4           INVALID DEMO MARKET CODE                     
         B     FLERR30                                                          
*                                                                               
STAE2120 DS    0H                                                               
* MARKET ***NEW                                                                 
*                                                                               
         TM    SVPGPBIT+2,X'80'    PREVENT ENTERING MARKET NAME?                
         BZ    STAE2125                                                         
*                                                                               
         LA    R2,FMSMCDH                                                       
         CLI   5(R2),0                                                          
         BE    FLERR2                                                           
*                                                                               
         LA    R2,FMSMKH                                                        
         CLI   5(R2),0                                                          
         BE    STAE2128                                                         
*                                                                               
         CLC   FMSMK,FMSMCD+L'FMSMCD+8                                          
         BE    STAE2125                                                         
*                                                                               
         MVC   FMSMK,FMSMCD+L'FMSMCD+8                                          
         MVI   5(R2),L'FMSMK                                                    
*                                                                               
STAE2125 DS    0H                                                               
         LA    R2,FMSMKH                                                        
         CLI   5(R2),0                                                          
         BE    *+14                                                             
         MVC   RSTAMKT,8(R2)                                                    
         B     STAE2130                                                         
*                                                                               
STAE2128 DS    0H                                                               
         LA    R2,FMSMCDH                                                       
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         LA    R2,FMSMKH                                                        
         B     FLERR1                                                           
*                                                                               
* GET MARKET CODE NAME AND PUT IN RSTAMKT                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING RMKTKEY,R3                                                       
         MVI   KEY,X'2B'                                                        
         MVC   RMKTKREP,REPALPHA                                                
         MVC   RMKTKMKT,FMSMCD                                                  
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,REC2                                                          
         LA    R5,REC                                                           
         BAS   RE,MOVEREC          MOVE STA REC TO REC2                         
*                                                                               
         LA    R3,REC                                                           
         USING RMKTREC,R3                                                       
         BAS   RE,GETREC           * HOPEFULLY, MKT REC IS NOT TOO BIG!         
         MVC   SVMKTNM,RMKTNAME                                                 
         DROP  R3                                                               
*                                                                               
*** RESTORE ORIGINAL RECORD                                                     
         LA    R4,REC                                                           
         LA    R5,REC2                                                          
         BAS   RE,XCREC            SWAP REC AND REC2                            
*                                                                               
         LA    R3,REC                                                           
         USING RSTAREC,R3                                                       
         MVC   RSTAMKT,SVMKTNM                                                  
         DROP  R3                                                               
*                                                                               
STAE2130 DS    0H                                                               
         B     FLFILE              BYPASS COMBO STATION FORMAT CHECK            
*                                                                               
SVMKTNM  DS    CL20                SAVE MKT NAME FROM RECORD                    
NOUPDATE DS    CL1                                                              
         DS    0F                                                               
*                                                                               
*    THIS BYPASS ELIMINATES THE CHECK TO ENSURE ALL STATIONS THAT               
*      BELONG TO THE COMBO ARE OF THE SAME FORMAT/RECEIVING ID.                 
*      THIS IS NECESSARY BECAUSE ALL STATIONS CANNOT BE UPDATED                 
*      AT ONE TIME, AND THIS IS A 'YOU CANNOT GET THERE FROM HERE'              
*      SITUATION.  ANOTHER APPROACH MUST BE DEVISED.                            
*                                                                               
*****    BAS   RE,CHECKFMT         CHECK COMBO STATION FORMATS                  
*****    BZ    FLFILE              CC = ZERO = OKAY                             
*****    B     FLERR7              CC NOT = ZERO = ERROR                        
         EJECT                                                                  
********************************************************************            
FLFILE   DS    0H                                                               
*                                                                               
*                                  ADD/UPDATE ACTIVITY ELEMENT                  
         GOTO1 =A(ACTINFO),DMCB,(RC),(RA),RR=Y                                  
         CLI   BACT,C'A'           TEST ADD                                     
         BE    FLFI0060                                                         
* CHANGE - READ REC THEN WRITE NEW                                              
         LA    R4,MYIOAREA                                                      
         LA    R5,REC                                                           
         BAS   RE,MOVEREC          MOVE REC TO MYIOAREA                         
*                                                                               
         MVC   KEY(28),REC         READ ORIGINAL RECORD                         
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,GETREC                                                        
*                                                                               
         LA    R4,REC2                                                          
         LA    R5,REC                                                           
         BAS   RE,MOVEREC          MOVE REC TO REC2                             
*                                                                               
         MVC   KEY(L'RSTAKEY),REC                                               
         MVI   KEY,X'42'           READ PREVIOUS COPY OF STATION RECORD         
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   FLFI0020                                                         
*                                  PREVIOUS RECORD EXISTS, OVERWRITE IT         
         BAS   RE,GETREC                                                        
         LA    R4,REC                                                           
         LA    R5,REC2                                                          
         BAS   RE,MOVEREC          MOVE REC TO REC2                             
         MVI   REC,X'42'                                                        
         BAS   RE,PUTREC           WRITE THE RECORD                             
         B     FLFI0040                                                         
*                                                                               
FLFI0020 DS    0H                  PREVIOUS RECORD DOESN'T EXISTS, ADD          
         MVI   REC,X'42'                                                        
         BAS   RE,ADDREC                                                        
*                                                                               
FLFI0040 DS    0H                                                               
         MVC   KEY(28),REC         READ ORIGINAL RECORD                         
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,GETREC                                                        
*                                                                               
         LA    R4,REC                                                           
         LA    R5,MYIOAREA         MYIOAREA HAS RECORD WITH LATEST CHG          
         BAS   RE,XCREC                                                         
         BAS   RE,PUTREC                                                        
         B     FLFI0080                                                         
*                                                                               
FLFI0060 BAS   RE,ADDREC                                                        
         MVC   BSVDA,KEY           SAVE DISK ADDRESS                            
*                                                                               
FLFI0080 CLI   BREC,2                                                           
         BNE   EXXMOD                                                           
*                                                                               
         GOTO1 =A(STAPSV),RR=Y     HANDLE ALTERNATE KEYS                        
*                                                                               
         CLC   RSTAKREP-RSTAREC+REC(2),=C'NU'                                   
         BE    FLFI0120                                                         
*                                  KEEP CLEARCHANNEL FROM MASTER STN            
         CLC   MASTER,SPACES       IF MASTER/SUB                                
         BNH   FLFI0120                                                         
*                                  MASTER ADD?                                  
         CLC   TWAREPN2,SPACES     ADD WILL HAVE 2NDARY NAME                    
         BNH   FLFI0100            NO 2NDARY NAME                               
         GOTO1 =A(UPDOLDM),RR=Y                                                 
*                                  UPDATE OLD MASTER + CONTROL                  
FLFI0100 DS    0H                                                               
         CLI   NOUPDATE,1          UPDATE NEEDED?                               
         BE    FLFI0120            NO  - SKIP UPDATING - DATES ARE              
*                                     SAME IN 'CHANGE'                          
         GOTO1 =A(UPDCTRL),DMCB,(0,0),RR=Y                                      
*                                  UPDATE MASTER CONTROL                        
FLFI0120 DS    0H                                                               
         LA    R2,LFMLAST          FOR STATION CHANGE - DISPLAY RECORD          
         MVC   REC2K(4),=C'JOIN'   SET REDISPLAY FLAG                           
         B     STAD0020                                                         
         EJECT                                                                  
********************************************************************            
* SUB-ROUTINE TO EDIT STATUS FIELD ON STATION SCREEN                            
********************************************************************            
SCANSTAT NTR1                                                                   
         LA    R3,INVERR                                                        
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         GOTO1 CSCANNER,DMCB,(R2),(4,WORK3)                                     
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         ZIC   R5,DMCB+4                                                        
         LA    R6,WORK3                                                         
         SPACE                                                                  
SS10     EQU   *                                                                
         CLI   0(R6),3             KEYWORD MUST BE AT LEAST 3 BYTES             
         BL    ERROR                                                            
         SPACE                                                                  
         CLI   0(R6),3             VALDATE BOP CHECK OVERRIDE                   
         BH    SS20                                                             
         CLC   12(3,R6),=C'BOP'                                                 
         BNE   SS20                                                             
         ZIC   R1,1(R6)                                                         
         LTR   R1,R1                                                            
         BZ    ERROR                                                            
         CH    R1,=H'2'                                                         
         BH    ERROR                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R6),=C'NO '                                                 
         BNE   ERROR                                                            
         OI    RSTASTAT,X'80'                                                   
         B     SSX                                                              
         SPACE                                                                  
SS20     EQU   *                   VALIDATE CONTRACT LOCKOUT                    
         CLI   0(R6),8                                                          
         BH    ERROR                                                            
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),=C'CONTRACT'                                            
         BNE   SS30                                                             
         ZIC   R1,1(R6)                                                         
         LTR   R1,R1                                                            
         BZ    ERROR                                                            
         CH    R1,=H'2'                                                         
         BH    ERROR                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R6),=C'NO '                                                 
         BNE   ERROR                                                            
         OI    RSTASTAT,X'40'                                                   
         B     SSX                                                              
         SPACE                                                                  
SS30     DS    0H                  VALIDATE ACCESS TO NEW AVAILS                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),=CL8'AVAILS'                                            
         B     SS40                AVAILS=NEW IS SKIPPED!!                      
***>>>   BNE   SS40                DISCONTINUED!!                               
         CLI   1(R6),3                                                          
         BNE   ERROR                                                            
         CLC   22(3,R6),=C'NEW'                                                 
         BNE   ERROR                                                            
         OI    RSTASTAT,X'20'                                                   
         B     SSX                                                              
         SPACE                                                                  
SS40     DS    0H              VALIDATE MANDATORY SCHED GRPS FOR SAR            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),=CL8'BUDGET'                                            
         BNE   ERROR                                                            
         ZIC   R1,1(R6)                                                         
         LTR   R1,R1                                                            
         BZ    ERROR                                                            
         CH    R1,=H'3'                                                         
         BH    ERROR                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R6),=C'YES'                                                 
         BNE   ERROR                                                            
         OI    RSTASTAT,X'10'                                                   
         B     SSX                                                              
         SPACE                                                                  
SSX      EQU   *                                                                
         LA    R6,32(R6)                                                        
         BCT   R5,SS10                                                          
         B     EXXMOD                                                           
         DROP  R7                                                               
         EJECT                                                                  
***************************************************                             
*   SUBROUTINE TO EDIT RECEIVING ID FIELD                                       
***************************************************                             
RIDEDT   NTR1                                                                   
         LA    R5,WORK2                                                         
         USING RSTAXEL,R5                                                       
         MVC   RSTAXEL(2),=X'0514'                                              
         MVC   RSTARSO,WORK                                                     
*                                                                               
         BAS   RE,GETID                                                         
         CLC   HALF,=H'0'          INVALID ID                                   
         BE    FLERR2                                                           
         MVC   RSTARID,HALF                                                     
         MVC   GRAPHLAG,HALF       SAVE ID FOR COMBO TESTING                    
         DROP  R5                                                               
         GOTO1 VADDELEM,DMCB,REC,(R5)                                           
         B     EXXMOD                                                           
         EJECT                                                                  
***************************************************                             
*   SUBROUTINE TO EDIT SIGN-ON ID FIELD                                         
***************************************************                             
SOEDT    NTR1                                                                   
         LA    R6,WORK2+100        R6 POINTS TO BLOCK                           
         LA    R5,WORK2                                                         
         XC    WORK2(100),WORK2    CLEAR ELEMENT AREA                           
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         GOTO1 CSCANNER,DMCB,(R2),(5,(R6)),0                                    
         DROP  R7                                                               
         SPACE 1                                                                
         CLI   DMCB+4,0                                                         
         BE    FLERR2                                                           
         ZIC   R4,DMCB+4                                                        
         SPACE 1                                                                
         USING RSTASOEL,R5                                                      
         MVC   RSTASOEL(2),=X'0611'                                             
SOE10    MVC   WORK,SPACES                                                      
         ZIC   R3,0(R6)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),12(R6)                                                   
         SPACE 1                                                                
         BAS   RE,GETID                                                         
         CLC   HALF,=H'0'                                                       
         BE    FLERR2                                                           
         MVC   RSTASO,12(R6)                                                    
         MVC   RSTASID,HALF                                                     
         DROP  R5                                                               
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,REC,(R5)                                           
         SPACE 1                                                                
         LA    R6,32(R6)                                                        
         BCT   R4,SOE10                                                         
         B     EXXMOD                                                           
         EJECT                                                                  
*&&UK                                                                           
****************************************************************                
* SUB-ROUTINE TO EDIT EMAIL FIELD ON STATION SCREEN                             
****************************************************************                
EMLEDT   NTR1                                                                   
         LA    R3,KEY                                                           
         USING RSTAREC,R3                                                       
*                                                                               
         MVI   RCMTKTYP,X'2E'      RECORD TYPE                                  
         MVC   RCMTKREP,REPALPHA   REP CODE                                     
         MVC   RCMTKOFF,=X'FFFF'   OFFICE CODE                                  
         MVC   RCMTKCDE,10(R2)     COMMENT CODE                                 
         DROP  R3                                                               
         BAS   RE,HIGH                                                          
*&&                                                                             
****************************************************************                
* SUB-ROUTINE TO EDIT CONTRACT COMMENT FIELD ON STATION SCREEN                  
****************************************************************                
CMTEDT   NTR1                                                                   
         CLC   =C'C=',8(R2)        SUPPORT FOR FILE COMMENT                     
         BNE   CE30                                                             
         CLI   5(R2),2                                                          
         BH    *+12                                                             
         LA    R3,278              COMMENT CODE MISSING                         
         B     MYERROR                                                          
*                                                                               
         OC    8(14,R2),SPACES     BLANK PADDED                                 
         CLC   10(8,R2),SPACES                                                  
         BNE   *+12                                                             
         LA    R3,278              COMMENT CODE MISSING                         
         B     MYERROR                                                          
*                                                                               
         CLC   18(4,R2),SPACES                                                  
         BE    *+12                                                             
         LA    R3,279              CMT CDE MUST BE FIRST AND ONLY ENTRY         
         B     MYERROR                                                          
*                                                                               
         MVI   5(R2),10            KEEP ONLY COMMENT CODE                       
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING RCMTD,R3                                                         
         MVI   RCMTKTYP,X'2E'      RECORD TYPE                                  
         MVC   RCMTKREP,REPALPHA   REP CODE                                     
         MVC   RCMTKOFF,=X'FFFF'   OFFICE CODE                                  
         MVC   RCMTKCDE,10(R2)     COMMENT CODE                                 
         DROP  R3                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BE    *+12                                                             
         LA    R3,277              FILE COMMENT REC NOT FOUND                   
         B     MYERROR                                                          
*                                                                               
         LA    R5,WORK2                                                         
         XC    WORK2(100),WORK2                                                 
         USING RSTACEL,R5                                                       
         MVI   RSTACCOD,3                                                       
         MVI   RSTACTYP,C'M'       FILE COMMENT = MANUAL CMT                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSTACCMT(0),8(R2)                                                
         LA    R1,5(R1)            COMPUTE EL LEN                               
         STC   R1,RSTACLEN                                                      
         B     CE200                                                            
         DROP  R5                                                               
*                                                                               
CE30     EQU   *                                                                
         LA    R6,REC2             R6 POINTS TO BLOCK                           
         LA    R5,WORK2                                                         
         XC    WORK2(100),WORK2    CLEAR ELEMENT AREA                           
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         GOTO1 CSCANNER,DMCB,(60,(R2)),(1,(R6)),0                               
         CLI   DMCB+4,0                                                         
         BE    FLERR2                                                           
         SPACE                                                                  
         USING RSTACEL,R5                                                       
         MVI   RSTACCOD,3                                                       
         CLI   1(R6),0                                                          
         BNE   CE100                                                            
         MVI   RSTACTYP,C'M'       UNDIVIDED FIELD IMPLIES MANUAL CMT           
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSTACCMT(0),12(R6)                                               
         LA    R1,5(R1)            COMPUTE EL LEN                               
         STC   R1,RSTACLEN                                                      
         B     CE200                                                            
         SPACE                                                                  
CE100    EQU   *                                                                
         CLI   0(R6),1                                                          
         BNE   FLERR2                                                           
         CLI   12(R6),C'L'         LIBRARY REFERENCE                            
         BNE   FLERR2                                                           
         CLI   1(R6),4             VALUE CANNOT BE G.T. 9999                    
         BH    FLERR2                                                           
         TM    3(R6),X'80'                                                      
         BNO   FLERR2                                                           
         OC    8(4,R6),8(R6)                                                    
         BZ    FLERR2                                                           
         MVI   RSTACTYP,C'L'                                                    
         MVC   RSTACNUM,10(R6)                                                  
         MVI   RSTACLEN,6                                                       
         DROP  R5,R7                                                            
         SPACE                                                                  
CE200    GOTO1 VADDELEM,DMCB,REC,(R5)                                           
         B     EXXMOD                                                           
         EJECT                                                                  
****************************************************************                
*        SUBROUTINE TO EDIT STATION TRAFFIC SYSTEM CODE                         
****************************************************************                
EDITTRAF NTR1                                                                   
         LA    R1,EDTFLIST                                                      
EDTF10   EQU   *                                                                
         CLI   0(R1),0             END OF LIST?                                 
         BE    EDTFBAD                                                          
         CLC   8(1,R2),0(R1)                                                    
         BE    EDTFGOOD                                                         
         LA    R1,1(R1)                                                         
         B     EDTF10                                                           
*                                                                               
*        EDITTRAF EXIT                                                          
*                                                                               
EDTFGOOD EQU   *                                                                
         LA    R0,0                                                             
EDTFEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         B     EXXMOD                                                           
EDTFBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     EDTFEXIT                                                         
*                                                                               
EDTFLIST DC    C'B'          BIAS                                               
         DC    C'C'          COLUMBINE                                          
         DC    C'O'          COLUMBINE/PAXON                                    
         DC    C'G'          GRAPHNET                                           
         DC    C'J'          JDS                                                
         DC    C'K'          KAMAN                                              
         DC    C'H'          KAMAN II                                           
         DC    C'I'          OSI                                                
         DC    C'L'          MARKETRON (NEW)                                    
         DC    C'M'          MARKETRON                                          
         DC    C'P'          WPVI                                               
         DC    C'R'          REP                                                
         DC    C'W'          BIAS COPY DOWN FORMAT                              
         DC    C'V'          VCI:  COLUMBINE PAPER FORMAT                       
         DC    C'S'          VCI STAR PLUS FORMAT                               
         DC    C'T'          VCI/BDE:  COLUMBINE PAPER FORMAT                   
         DC    C'U'          VCI/BDE: STAR PLUS FORMAT                          
         DC    C'A'          RADIO                                              
         DC    C'Z'          VSS (USES ENTERPRISE/KAMAN)                        
         DC    X'00'                                                            
         EJECT                                                                  
****************************************************************                
*        SUBROUTINE TO EXTRACT 2 BYTE ID                                        
****************************************************************                
GETID    NTR1                                                                   
         LA    R4,REC2                                                          
         XC    0(25,R4),0(R4)      BUILD CONTROL FILE KEY                       
         MVI   0(R4),C'I'                                                       
         MVC   15(10,R4),SPACES                                                 
         MVC   15(8,R4),WORK                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R4),(R4),0                  
         CLI   DMCB+8,0                                                         
         BNE   NOID                                                             
         LA    R4,28(R4)                                                        
         SR    R5,R5                                                            
*                                                                               
TESTELE  CLI   0(R4),0                                                          
         BE    NOID                                                             
         CLI   0(R4),2                                                          
         BNE   NEXTELE                                                          
         MVC   HALF,2(R4)          ID FOUND                                     
         B     EXXMOD                                                           
*                                                                               
NEXTELE  IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     TESTELE                                                          
*                                                                               
NOID     MVC   HALF,=H'43'      USE DDS ID AS DEFAULT FOR REGIONAL HQ           
         CLI   BREC,2                                                           
         BNE   NOID5                                                            
         MVC   HALF,=H'0'        FOR STATION RECORD, USE 0, NO DEFAULT          
NOID5    B     EXXMOD                                                           
         EJECT                                                                  
******************************************************************              
*   SUBROUTINE TO VALIDATE/DISPLAY A MARKET RECORD (X'2B RECORD)                
*                                                                               
*   ZERO IS FOUND                                                               
*   NON-ZERO IS NOT FOUND                                                       
******************************************************************              
DISPMKT  NTR1                                                                   
         LA    R4,REC2                                                          
         ST    R4,AIOAREA                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RMKTREC,R6                                                       
         MVI   RMKTKEY,X'2B'                                                    
         MVC   RMKTKREP(2),REPALPHA                                             
         MVC   RMKTKMKT(4),FMSMCD                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DMKTBAD                                                          
*                                                                               
         BAS   RE,GETREC                                                        
         LR    R6,R4                                                            
         LA    R2,FMSMCDH                                                       
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVC   8(20,R2),RMKTNAME                                                
*                                                                               
DMKTGOOD EQU   *                                                                
         SR    R0,R0                                                            
DMKTEXIT EQU   *                                                                
         LA    R4,REC                                                           
         ST    R4,AIOAREA                                                       
         LTR   R0,R0                                                            
         B     EXXMOD                                                           
DMKTBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     DMKTEXIT                                                         
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
* SUBROUTINE TO POINT R2 TO NEXT UNPROTECTED FIELD                              
******************************************************************              
NEXTUF   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         CLI   0(R2),9                                                          
         BE    NEXTUF2                                                          
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUF                                                           
NEXTUF2  CLI   9(R2),0             CHECK FOR LAST                               
         BNE   NEXTUF4                                                          
         CR    R2,R2               IF LAST, SET CC=                             
         BR    RE                                                               
NEXTUF4  LTR   R2,R2               NOT LAST, SET CC NOT=                        
         BR    RE                                                               
         EJECT                                                                  
******************************************************************              
* SUBROUTINE TO MOVE 1000 BYTES TO R4 FROM R5                                   
******************************************************************              
MOVEREC  MVC   000(250,R4),000(R5)                                              
         MVC   250(250,R4),250(R5)                                              
         MVC   500(250,R4),500(R5)                                              
         MVC   750(250,R4),750(R5)                                              
         BR    RE                                                               
         SPACE 2                                                                
********************************************************************            
CHECK    EQU   *                                                                
         TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
         SPACE 2                                                                
******************************************************************              
XCREC    LA    R0,4                                                             
         XC    0(250,R4),0(R5)                                                  
         XC    0(250,R5),0(R4)                                                  
         XC    0(250,R4),0(R5)                                                  
         LA    R4,250(R4)                                                       
         LA    R5,250(R5)                                                       
         BCT   R0,XCREC+4                                                       
         BR    RE                                                               
         SPACE 2                                                                
******************************************************************              
FLERR1   LA    R3,MSSNGERR                                                      
         B     ERROR                                                            
FLERR2   LA    R3,INVERR                                                        
         B     ERROR                                                            
FLERR3   LA    R3,NUMERR                                                        
         B     ERROR                                                            
FLERR4   LA    R3,ERRNF                                                         
         LA    R2,FMSMCDH                                                       
         B     ERROR                                                            
FLERR5   SR    R3,R3                                                            
         MVC   LFMMSG(L'LIABERR),LIABERR                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
LIABERR  DC    C'LIABILITY COMMENT MUST BE 01 THRU 99.'                         
FLERR6   SR    R3,R3                                                            
         LA    R2,FMSCM1H                                                       
         MVC   LFMMSG(L'COMBSTA),COMBSTA                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
COMBSTA  DC    C'ORDER OR NUMBER OF COMBO STATIONS CANNOT BE CHANGED.'          
FLERR7   SR    R3,R3                                                            
         LA    R2,FMSCM1H                                                       
         MVC   LFMMSG(L'COMBTYP),COMBTYP                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
COMBTYP  DC    C'COMBO STATIONS MUST HAVE SAME FORMAT/RECEIVING ID.'            
FLERR8   SR    R3,R3                                                            
         MVC   LFMMSG(L'NOECTYP),NOECTYP                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR9   SR    R3,R3                                                            
         MVC   LFMMSG(L'FAXMBERR),FAXMBERR                                      
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FAXMBERR DC    C'FAX/MB= VALUES MUST BE ALL NUMERIC'                            
FLERR10  SR    R3,R3                                                            
         MVC   LFMMSG(L'MBOXERR),MBOXERR                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
MBOXERR  DC    C'MB= NUMBER MUST BE 8 DIGITS'                                   
FLERR11  SR    R3,R3                                                            
         MVC   LFMMSG(L'NEEDOWN),NEEDOWN                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
NEEDOWN  DC    C'OWNER CODE IS REQUIRED'                                        
FLERR12  SR    R3,R3                                                            
         MVC   LFMMSG(L'DESTERR),DESTERR                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
DESTERR  DC    C'INVALID ID, MAILBOX, OR FAX NUMBER'                            
FLERR13  SR    R3,R3                                                            
         MVC   LFMMSG(L'GRPHERR),GRPHERR                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
GRPHERR  DC    C'DESTINATION ''GRAPH'' NOT ALLOWED IN THIS FIELD'               
FLERR14  SR    R3,R3                                                            
         MVC   LFMMSG(L'DATEERR),DATEERR                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
DATEERR  DC    C'DATE ENTERED INCORRECTLY.  FORMAT IS MON/YEAR'                 
FLERR15  SR    R3,R3                                                            
         MVC   LFMMSG(L'EOMDERR),EOMDERR                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
EOMDERR  DC    C'MONTH STILL ACTIVE.  CANNOT BE CLOSED.       '                 
FLERR16  SR    R3,R3                                                            
         MVC   LFMMSG(L'NOEOM),NOEOM                                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
NOEOM    DC    C'EOM RECORD FOR TODAYS DATE NOT FOUND.'                         
FLERR17  SR    R3,R3                                                            
         MVC   LFMMSG(L'NOTMON),NOTMON                                          
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
NOTMON   DC    C'** ERROR - JOIN DATE MUST BE A MONDAY **'                      
FLERR18  SR    R3,R3                                                            
         MVC   LFMMSG(L'CMBERR),CMBERR                                          
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
CMBERR   DC    C'** ERROR ** COMBO STATION SPECIFIED ALREADY IN USE'            
FLERR19  SR    R3,R3                                                            
         MVC   LFMMSG(L'COMPERR),COMPERR                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
COMPERR  DC    C'** ERROR ** CHANGE TO COMPETITIVES NOT ALLOWED'                
FLERR20  SR    R3,R3                                                            
         MVC   LFMMSG(L'CTRLERR),CTRLERR                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR21  SR    R3,R3                                                            
         MVC   LFMMSG(L'OWNRFRI),OWNRFRI                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR22  SR    R3,R3                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,1               INSERT REC TYPE                              
         MVC   KEY+25(2),HALF      INSERT OVERLAPPING REP CODE                  
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                REP RECORD MUST BE ON FILE                   
         LA    R4,REC2                                                          
         ST    R4,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         LA    R4,REC                                                           
         ST    R4,AIOAREA                                                       
*                                                                               
R        USING RREPREC,REC2                                                     
         MVC   REPOLAP,R.RREPSHRT INSERT SHORT REP NAME                         
         MVC   LFMMSG(DAREOLAP),DATEOLAP                                        
         DROP  R                                                                
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR23  SR    R3,R3                                                            
         MVC   LFMMSG(L'LDPREJD),LDPREJD                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR24  SR    R3,R3                                                            
         MVC   LFMMSG(L'HISTERR),HISTERR                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR25  SR    R3,R3                                                            
         MVC   LFMMSG(L'HISTER2),HISTER2                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR26  SR    R3,R3                                                            
         MVC   LFMMSG(L'INVDTA2),INVDTA2                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR27  SR    R3,R3                                                            
         MVC   LFMMSG(L'EMAILERR),EMAILERR                                      
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR28  SR    R3,R3                                                            
         MVC   LFMMSG(L'GRAPHNET),GRAPHNET                                      
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR29  SR    R3,R3                                                            
         MVC   LFMMSG(L'NOEMAIL),NOEMAIL                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR30  SR    R3,R3                                                            
         MVC   LFMMSG(L'DEMOMRKT),DEMOMRKT                                      
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR31  SR    R3,R3                                                            
         MVC   LFMMSG(L'BADUID),BADUID                                          
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR32  SR    R3,R3                                                            
         MVC   LFMMSG(L'DUPUID),DUPUID                                          
         MVC   LFMMSG+42(2),DUB+1  INSERT REP CODE                              
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FLERR33  SR    R3,R3                                                            
         MVC   LFMMSG(L'VENDRERR),VENDRERR                                      
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
DUPUID   DC    C'** ERROR ** UNIQUE ID ALREADY ASSIGNED TO '                    
BADUID   DC    C'** ERROR ** UNIQUE ID NOT FOUND ON FILE'                       
DEMOMRKT DC    C'DEMO MKT: NUMERIC (1-65535) OR THREE CHARS'                    
NOEMAIL  DC    C'CONFIRM VIA WEB REQUIRES EMAIL ADDRESS'                        
GRAPHNET DC    C'RECEIVING ID MUST BE "GRAPH"'                                  
CTRLERR  DC    C'** ERROR - ANOTHER REP IS ACTIVE, SEE STACTRL RECORD'          
HISTERR  DC    C'DATE ERROR.  FORMAT IS MONTH/DAY/YEAR'                         
HISTER2  DC    C'DATE ERROR.  DATE MUST BE A MONDAY'                            
INVDTA2  DC    C'INDICATOR IS Y OR BLANK'                                       
OWNRFRI  DC    C'** ERROR - OWNER MAY ONLY BE CHANGED ON FRIDAY'                
DATEOLAP DC    C'** ERROR - NEW JOIN DATE OVERLAPS THOSE ON '                   
REPOLAP  DC    CL10' '                                                          
DAREOLAP EQU   *-DATEOLAP                                                       
NOECTYP  DC    C'MUST BE ELECTRONIC CONTRACT TRAFFIC FORMAT.'                   
LDPREJD  DC    C'** ERROR - LEAVE DATE BEFORE JOIN DATE'                        
EMAILERR DC    C'** ERROR - EMAIL ADDRESS TOO LONG: MAX 48'                     
VENDRERR DC    C'** ERROR - VENDOR NOT RECOGNIZED'                              
ZEROS    DC    30C'0'                                                           
BLANKS   DC    CL24' '                                                          
MONTHS   DC    X'01',CL3'JAN'                                                   
         DC    X'02',CL3'FEB'                                                   
         DC    X'03',CL3'MAR'                                                   
         DC    X'04',CL3'APR'                                                   
         DC    X'05',CL3'MAY'                                                   
         DC    X'06',CL3'JUN'                                                   
         DC    X'07',CL3'JUL'                                                   
         DC    X'08',CL3'AUG'                                                   
         DC    X'09',CL3'SEP'                                                   
         DC    X'0A',CL3'OCT'                                                   
         DC    X'0B',CL3'NOV'                                                   
         DC    X'0C',CL3'DEC'                                                   
         DC    X'FF'                                                            
         SPACE 2                                                                
AFFLLIST DC    CL3'ABC'                                                         
         DC    CL3'NBC'                                                         
         DC    CL3'CBS'                                                         
         DC    CL3'IND'                                                         
         DC    CL3'UNI'                                                         
         DC    CL3'TEL'                                                         
         DC    CL3'FOX'                                                         
         DC    CL3'GAL'                                                         
         DC    CL3'UPN'                                                         
         DC    CL3'WBT'                                                         
         DC    CL3'PBS'                                                         
         DC    CL3'CBL'                                                         
         DC    CL3'PAX'                                                         
         DC    CL3'GEM'                                                         
         DC    CL3'MUN'                                                         
         DC    CL3'TF '            TELEFUTURA                                   
         DC    X'00'                                                            
         SPACE 2                                                                
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
NOGRPERR EQU   121                                                              
NOCHGERR EQU   123                                                              
DUPSTERR EQU   125                 DUP STA OR AFFILIATE                         
MANYIND  EQU   74                  TOO MANY INDEPENDENT COMPETING               
NOECTRAF EQU   393                 NOT EC TRAFFIC FORMAT                        
         EJECT                                                                  
**********************************************************************          
* SAME AS GETEL IN BASE, BUT USES OWN IOAREA HERE                               
*        P1=A(RECORD)    BYTE 0 = ELEM CODE SOUGHT BY USER (1ST ELEM            
*                                 ONLY RETURNED) SET TO X'FF' IF NONE           
*        P2=A(3 FULL WORD AREA FOR REGISTERS 3-5 FOR BXLE IN USER)              
**********************************************************************          
MYGETEL  NTR1                                                                   
         L     R2,0(R1)            A(RECORD)                                    
         L     R6,4(R1)            A(STORE AREA)                                
*                                                                               
         LA    R5,34(R2)           POINT TO FIRST ELEMENT                       
         SR    R4,R4                                                            
GET130   CLI   0(R5),0                                                          
         BE    GET150                                                           
         IC    R4,1(R5)                                                         
         AR    R5,R4                                                            
         B     GET130                                                           
GET150   BCTR  R5,R0               BXLE                                         
         SR    R4,R4                                                            
*                                                                               
         LA    R3,34(R2)           1ST ELEM                                     
*                                                                               
GETEL100 CLC   0(1,R1),0(R3)       ELEM CODE?                                   
         BNE   GETEL200                                                         
         STM   R3,R5,0(R6)         BXLE FOR USER                                
GETELXIT B     EXXMOD                                                           
GETEL200 IC    R4,1(R3)            ELEM LEN                                     
         BXLE  R3,R4,GETEL100                                                   
         MVI   0(R1),X'FF'         NOT FOUND INDICATOR                          
         B     GETELXIT                                                         
         EJECT                                                                  
* ELEMENT CODE FOR SEARCH IS IN 'BYTE'                                          
* FIRST ELEMENT ADDRESS (NOT CHECKED) IN R4                                     
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    NEXTELX                                                          
         CLC   BYTE,0(R4)                                                       
         BER   RE                  EXIT WITH CC =                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE               EXIT WITH CC NOT =                           
         BR    RE                                                               
         EJECT                                                                  
**********************************************************                      
* VALIDATE FIELD IS ALPHA/NUMERIC                                               
* FIELD IS DELIMITED BY BLANK,COMMA,OR DASH                                     
* R4 HAS FIELD ADDRESS. ON EXIT HAS STOP CHAR ADDRESS                           
* R5 HAS MAX LENGTH.    ON EXIT HAS CHAR COUNT.                                 
* R1 HAS 'TO' ADDRESS.  ON EXIT HAS NEXT CHAR ADDRESS                           
**********************************************************                      
TESTAN   MVI   BYTE,X'0C'          SET VALID A (X'04') AND N (X'08')            
         LA    R0,1(R5)            GET MAX LEN+1                                
TESTAN1  CLI   0(R4),C' '                                                       
         BE    TESTANX                                                          
         CLI   0(R4),0                                                          
         BE    TESTANX                                                          
         CLI   0(R4),C','                                                       
         BE    TESTANX                                                          
         CLI   0(R4),C'-'                                                       
         BE    TESTANX                                                          
         CLI   0(R4),C'A'                                                       
         BL    TESTAN8             LESS THAN A = ERROR                          
         CLI   0(R4),C'Z'                                                       
         BNH   TESTAN4                                                          
TESTAN2  NI    BYTE,X'08'          FIELD NOT ALPHA                              
         CLI   0(R4),C'0'                                                       
         BL    TESTAN4                                                          
         CLI   0(R4),C'9'                                                       
         BNH   TESTAN6                                                          
TESTAN4  NI    BYTE,X'04'          FIELD NOT NUMERIC                            
TESTAN6  MVC   0(1,R1),0(R4)                                                    
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,TESTAN1                                                       
         B     ERROR                                                            
TESTAN8  EQU   *                                                                
         OI    BYTE,X'80'          SET ERROR RETURN FLAG                        
TESTANX  BCTR  R0,0                ADJUST COUNT                                 
         SR    R5,R0               GIVES CHARACTER COUNT                        
         BR    RE                                                               
         EJECT                                                                  
***************************************************************                 
MYERROR  EQU   *                                                                
         MVI   ERRAREA,X'FF'                                                    
         GOTO1 VGETTXT,DMCB+12,(R3),0,(C'E',DMCB),0,0,0                         
         LTR   R3,R3               IF DATAMGR ERROR, DON'T SET CURPOS           
         BZ    MYERRORX                                                         
         OI    6(R2),X'40'                                                      
MYERRORX B     EXXMOD                                                           
         EJECT                                                                  
*** STORAGE AREA                                                                
AIOSV    DS    A                                                                
*                                                                               
*                   .0.1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.6.7.8.9                    
NEW23ELT DC    XL20'2314000000100040404040404040400000000000'                   
NEW0DELT DC    XL06'0D0600000000'                                               
NEW10ELT DC    XL16'10100000000000000000000000000000'                           
*MN                                                                             
NEW13ELT DC    XL10'130A0000000000000000'                                       
NEW2CELT DC    XL02'2C18'          CONTROL OF ELEMENT                           
         DC    22X'00'             REMAINDER OF ELEMENT                         
*MN                                                                             
*                                                                               
KEYSAVE2 DS    CL27                                                             
PREFSTAT DC    C'N'                                                             
TEMPIDS  DS    CL40                                                             
COMBAREA DS    CL70                SAVE AREA FOR OLD X'0A' ELEMENTS             
*                                  4 X 14 CHARS. LAST ENTRY = DELIM             
*                                  AS OF 11/09/93, ELT = 10 CHARS               
GRAPHLAG DC    XL2'FFFF'           GRAPH FLAG                                   
*                                  X'FFFF' = NO 'X05' ELEMENT                   
*                                  X'0000' - X'05' ELT W/ NO RECVNG ID          
*                                  OTHER:    GRAPHNET/ACE                       
USERDATE DS    XL3                 TODAY'S DATE                                 
USERLUID DS    CL8                 USER'S LUID                                  
EOMDATE  DS    CL2                                                              
MSTRSTRT DS    CL6                 START DATE FOR MASTER UPDATE                 
SUBREPS  DS    CL32                SUBREPS                                      
         DS    0F                                                               
MYIOAREA DS    1000C               FOR STORED COMMENTS                          
         EJECT                                                                  
**********************************************************************          
* INCLUDE RELFMGEN                                                              
* BELOW IS THE EQUIVALENT TO THE CONTENTS OF RELFMGEN WITH THE                  
* EXCEPTION OF THE LTORG                                                        
* IF RELFMGEN IS CHANGED, THE CHANGES MUST BE REFLECTED HERE                    
       ++INCLUDE RELFMINC                                                       
       ++INCLUDE FLDIND                                                         
       ++INCLUDE RGENEROL                                                       
       EJECT                                                                    
       LTORG                                                                    
       EJECT                                                                    
       ++INCLUDE RGENOLD                                                        
       ++INCLUDE RELFMWRK                                                       
**********************************************************************          
         EJECT                                                                  
*                                                                               
         ORG   REC                                                              
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENEOM                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENOWN                                                       
         EJECT                                                                  
         ORG   REC2                                                             
       ++INCLUDE REGENTEM                                                       
         EJECT                                                                  
         ORG   REC2                                                             
       ++INCLUDE REGENOFF                                                       
         EJECT                                                                  
         ORG                                                                    
*              DDCOMFACS                                                        
***      PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE FATWA                                                          
         SPACE 2                                                                
       ++INCLUDE RELFMTWA                                                       
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMFDD                                                       
         ORG   LFMLAST                                                          
       ++INCLUDE RELFME9D                                                       
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMDCD                                                       
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMD0D                                                       
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMD1D                                                       
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMD2D                                                       
         EJECT                                                                  
REC2SECT DSECT                                                                  
       ++INCLUDE REGENMKT                                                       
       ++INCLUDE REGENGRP                                                       
RCMTD    DSECT                     FILE COMMENTS                                
       ++INCLUDE REGENCMT                                                       
***      PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         CSECT                                                                  
* TVB TABLE                                                                     
       ++INCLUDE RETVBTAB                                                       
         EJECT                                                                  
* REP TABLE                                                                     
       ++INCLUDE DDDARETAB                                                      
       ++INCLUDE REPREPS                                                        
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO RETRIEVE REP RECORD, ISOLATE SUBSIDIARIES                          
**********************************************************************          
         DS    0D                                                               
SETSUBS  NTR1  BASE=*,LABEL=*                                                   
         XC    SUBREPS,SUBREPS                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,1               SET FOR REP RECORDS                          
         MVC   KEY+25(2),REPALPHA  SET SIGNON ID                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO  - MUST BE ON FILE                        
         LA    R0,REC2                                                          
         ST    R0,AIOAREA                                                       
         GOTO1 GETREC                                                           
         LA    R0,REC                                                           
         ST    R0,AIOAREA                                                       
R        USING RREPREC,REC2                                                     
         MVC   SUBREPS(2),REPALPHA INSERT SIGNON ID INTO TABLE                  
         CLC   R.RREPMAST,SPACES   MAST/SUB?                                    
         BNH   SSUB0900            NO  - LEAVE TABLE W/SIGNON ONLY              
         CLC   R.RREPMAST,=X'FFFF' MASTER?                                      
         BE    SSUB0020            YES - RETRIEVE THE SUBSIDIARIES              
         XC    KEY,KEY                                                          
         MVI   KEY,1               SET FOR REP RECORDS                          
         MVC   KEY+25(2),R.RREPMAST    SET MASTER ID                            
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO  - MUST BE ON FILE                        
         LA    R0,REC2                                                          
         ST    R0,AIOAREA                                                       
         GOTO1 GETREC              RETRIEVE THE MASTER RECORD                   
         LA    R0,REC                                                           
         ST    R0,AIOAREA                                                       
SSUB0020 EQU   *                                                                
         ZIC   RE,R.RREPSCNT       GET SUBSIDIARY COUNT                         
         SLL   RE,1                DOUBLE THE COUNT (NOW BYTES)                 
         BCTR  RE,0                BACK OFF 1                                   
         LA    RF,R.RREPSCOD       SET A(CODES)                                 
         EX    RE,SSUB0025                                                      
         B     SSUB0040                                                         
SSUB0025 MVC   SUBREPS(0),0(RF)    MOVE CODES BY LENGTH                         
         DROP  R                                                                
SSUB0040 EQU   *                                                                
SSUB0900 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO DISPLAY COMPETING STATIONS SCREEN                                  
**********************************************************************          
         DS    0D                                                               
DISCOMP  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,RSTAELEM                                                      
         LA    R2,FSCCPPH                                                       
CSF20    EQU   *                                                                
         MVI   BYTE,2                                                           
         BAS   RE,NEXTUF                                                        
         CLI   0(R2),9                                                          
         BE    CSFX                                                             
         LA    R0,FSCLSTH                                                       
         CR    R0,R2                                                            
         BE    CSFX                                                             
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   CSF30                                                            
         FOUT  (R2),SPACES,11                                                   
         SPACE 1                                                                
         USING RSTAMKEL,R4                                                      
         MVC   8(4,R2),RSTAMKST    CALL LETTERS                                 
         LA    R5,11(R2)                                                        
         CLI   0(R5),C' '                                                       
         BE    *+8                                                              
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
         MVI   0(R5),C'-'                                                       
         MVC   1(1,R5),RSTAMKST+4  BAND                                         
         CLI   1(R5),C' '                                                       
         BNE   *+8                                                              
         MVI   1(R5),C'T'                                                       
         LA    R5,2(R5)                                                         
         CLC   RSTAMKAF(3),SPACES                                               
         BE    CSF20                                                            
         MVI   0(R5),C'='                                                       
         MVC   1(3,R5),RSTAMKAF    AFFILIATION                                  
         B     CSF20                                                            
         DROP  R4                                                               
         SPACE 1                                                                
CSF30    FOUT  (R2),SPACES,11                                                   
         BAS   RE,NEXTUF                                                        
         BNE   CSF30                                                            
CSFX     B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO DISPLAY STATION CONTROL                                            
**********************************************************************          
         DS    0D                                                               
DISCTRL  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SCTANAMH                                                      
*                                                                               
DCTRL002 ZIC   RE,0(R2)                                                         
         LTR   RF,RE               ZERO MEANS END OF SCREEN                     
         BZ    DCTRL004                                                         
         AHI   RF,-9               8 +1 FOR EX                                  
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         AHI   RF,-8               8 MORE FOR EXTENDED FIELD HEADER             
*                                                                               
         TM    1(R2),X'08'         NORMAL INTENSITY?                            
         BNZ   *+18                NO - SKIP CLEAR                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
         OI    6(R2),X'80'                                                      
         AR    R2,RE                                                            
         B     DCTRL002                                                         
*                                                                               
DCTRL004 DS    0H                                                               
         LA    R6,RSTAELEM                                                      
         USING RSTAMCEL,R6                                                      
         CLI   RSTAMCEL,RSTAMCCQ   CURRENT ELEMENT?                             
         BNE   DCTRL010            NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           GET REPNAME                                  
         MVC   KEY+RREPKREP-RREPKEY(2),RSTAMCRC                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'RREPKEY),KEY                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,REC2                                                          
         ST    R4,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         LA    R4,REC                                                           
         ST    R4,AIOAREA                                                       
*                                                                               
R        USING RREPREC,REC2                                                     
         FOUT  SCTANAMH                                                         
         MVC   SCTANAM,R.RREPNAME                                               
         DROP  R                                                                
*                                                                               
         FOUT  SCTAJDH                                                          
         GOTO1 VDATCON,DMCB,(3,RSTAMCJD),(8,SCTAJD)                             
*                                                                               
         FOUT  SCTAMRKH                                                         
         MVI   SCTAMRK,C' '                                                     
DCTRL008 DS    0H                  SKIP EXTRA CURRENTS                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),RSTAMCCQ                                                   
         BNE   DCTRL010                                                         
         MVI   SCTAMRK,C'+'        SET MORE MAKRER                              
         B     DCTRL008                                                         
*                                                                               
DCTRL010 DS    0H                                                               
         CLI   RSTAMCEL,RSTAMCPQ   PREVIOUS ELEMENT?                            
         BNE   DCTRL020            NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           GET REPNAME                                  
         MVC   KEY+RREPKREP-RREPKEY(2),RSTAMCRC                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'RREPKEY),KEY                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,REC2                                                          
         ST    R4,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         LA    R4,REC                                                           
         ST    R4,AIOAREA                                                       
*                                                                               
R        USING RREPREC,REC2                                                     
         FOUT  SCTPNAMH                                                         
         MVC   SCTPNAM,R.RREPNAME                                               
         DROP  R                                                                
*                                                                               
         FOUT  SCTPJDH                                                          
         FOUT  SCTPLDH                                                          
         GOTO1 VDATCON,DMCB,(3,RSTAMCJD),(8,SCTPJD)                             
         GOTO1 VDATCON,DMCB,(3,RSTAMCLD),(8,SCTPLD)                             
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
DCTRL020 DS    0H                                                               
         LA    R2,SCTHNAMH                                                      
S        USING SCTHNAMH,R2                                                      
*                                                                               
DCTRL022 DS    0H                                                               
         CLI   RSTAMCEL,RSTAMCHQ   HISTORY ELEMENT?                             
         BNE   DCTRL030            NO                                           
         LA    R0,SCTHLSTH                                                      
         CR    R2,R0               PAST END OF HISTORY LINES?                   
         BH    DCTRL030            YES                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           GET REPNAME                                  
         MVC   KEY+RREPKREP-RREPKEY(2),RSTAMCRC                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'RREPKEY),KEY                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,REC2                                                          
         ST    R4,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         LA    R4,REC                                                           
         ST    R4,AIOAREA                                                       
*                                                                               
R        USING RREPREC,REC2                                                     
         FOUT  S.SCTHNAMH                                                       
         MVC   S.SCTHNAM,R.RREPNAME                                             
         DROP  R                                                                
*                                                                               
         FOUT  S.SCTHJDH                                                        
         FOUT  S.SCTHLDH                                                        
         GOTO1 VDATCON,DMCB,(3,RSTAMCJD),(8,S.SCTHJD)                           
         GOTO1 VDATCON,DMCB,(3,RSTAMCLD),(8,S.SCTHLD)                           
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         LA    R2,SCTHNXTH-SCTHNAMH(R2)                                         
         B     DCTRL022                                                         
         DROP  S                                                                
*                                                                               
DCTRL030 DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
******************************************************                          
*   STATION LEAVE DATE ALTERNATE KEY PROCESSING                                 
******************************************************                          
         DS    0D                                                               
STAPSV   NTR1  BASE=*,LABEL=*                                                   
         CLI   BACT,C'A'           TEST ADD/CHANGE                              
         BE    STAP0040            ADD                                          
         LA    R2,FMSLDH           HAS FIELD CHANGED?                           
         TM    4(R2),X'20'         TEST CHANGED                                 
         BO    STAP0060            NO CHANGE - OKAY AS IS                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'82'                         SET RECORD ID                  
         MVC   KEY+25(2),REC2+20                 SET REP CODE                   
         MVC   KEY+17(5),RSTAKSTA-RSTAREC+REC2   SET STN CALL LETTERS           
         MVC   KEY+22(3),RSTAEND-RSTAREC+REC2    SET LEAVE DATE                 
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    STAP0020            YES                                          
         DC    H'0'                NOT FOUND - SHOULDN'T HAPPEN                 
STAP0020 EQU   *                                                                
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        SET DELETE BIT                               
         BAS   RE,WRITE            REWRITE KEY AS DELETE                        
         BAS   RE,CHECK                                                         
STAP0040 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'82'                         SET RECORD ID                  
         MVC   KEY+25(2),REC+20                  SET REP CODE                   
         MVC   KEY+17(5),RSTAKSTA-RSTAREC+REC    SET STN CALL LETTERS           
         MVC   KEY+22(3),RSTAEND-RSTAREC+REC     SET LEAVE DATE                 
         MVC   KEY+28(4),BSVDA     SET DISK ADDRESS                             
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TOTS                                
         BAS   RE,HIGH             CHECK FOR ADD/REWRITE                        
         LA    RF,ADD              SET FOR ADD                                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                 NOT EQUAL IS 'ADD'                           
         LA    RF,WRITE            EQUAL IS 'REWRITE'                           
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF               PERFORM THE FUNCTION                         
         BAS   RE,CHECK                                                         
STAP0060 EQU   *                   MARKET NAME KEY TEST                         
         GOTO1 =A(P3N4KEYS),RR=Y   X'8303' & 'X'8304' PASSIVE REC               
*                                     ALSO X'8306' KEY                          
STAP0070 EQU   *                                                                
         CLI   BACT,C'A'           TEST ADD/CHANGE                              
         BE    STAP0080            ADD                                          
         LA    R2,FMSMKH           HAS FIELD CHANGED?                           
         TM    4(R2),X'20'         TEST CHANGED                                 
         BO    STAP0100            NO CHANGE - OKAY AS IS                       
*                                                                               
*  DELETE OLD PASSIVE POINTER - WILL ALWAYS EXIST                               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'83'                         SET RECORD ID                  
         MVI   KEY+1,X'02'                       SET RECORD SUBID               
         MVC   KEY+2(02),REC2+20                 SET REP CODE                   
         MVC   KEY+4(18),RSTAMKT-RSTAREC+REC2    SET MKT NAME                   
         MVC   KEY+22(5),RSTAKSTA-RSTAREC+REC2   SET STATION CALL LTRS          
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   STAP0080            NO  - SKIP IF NOT FOUND                      
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        SET DELETE BIT                               
         BAS   RE,WRITE            REWRITE KEY AS DELETE                        
         BAS   RE,CHECK                                                         
STAP0080 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'83'                         SET RECORD ID                  
         MVI   KEY+1,X'02'                       SET RECORD SUBID               
         MVC   KEY+2(02),REC+20                  SET REP CODE                   
         MVC   KEY+4(18),RSTAMKT-RSTAREC+REC     SET MKT NAME                   
         MVC   KEY+22(5),RSTAKSTA-RSTAREC+REC    SET STATION CALL LTRS          
         MVC   KEY+28(4),BSVDA     SET DISK ADDRESS                             
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TOTS                                
         BAS   RE,HIGH             CHECK FOR ADD/REWRITE                        
         LA    RF,ADD              SET FOR ADD                                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                 NOT EQUAL IS 'ADD'                           
         LA    RF,WRITE            EQUAL IS 'REWRITE'                           
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF               PERFORM THE FUNCTION                         
         BAS   RE,CHECK                                                         
STAP0100 EQU   *                                                                
         CLI   RSTAKSTA+4-RSTAREC+REC,C'C'                                      
*                                  IS IT A COMBO PARENT?                        
         BNE   STAP0300            NO  - NO PASSIVE X'8301' KEYS                
*                                                                               
         CLI   BACT,C'A'           TEST ADD/CHANGE                              
         BE    STAP0180            ADD                                          
*                                                                               
*  DELETE OLD PASSIVE POINTER - WILL ALWAYS EXIST                               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'83'                         SET RECORD ID                  
         MVI   KEY+1,X'01'                       SET RECORD SUBID               
         MVC   KEY+15(02),REC2+20                SET REP CODE                   
         MVC   KEY+22(5),RSTAKSTA-RSTAREC+REC2   SET PARENT CALL LTRS           
*                                                                               
*        KEY INITIALIZED EXCEPT FOR CHILD CALL LETTERS                          
*                                                                               
         LA    R2,RSTAELEM-RSTAREC+REC2                                         
*                                  SET A(STA X'01' ELT:  OLD REC)               
STAP0120 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    STAP0180            YES                                          
         CLI   0(R2),X'0A'         COMBINED STATION ELT?                        
         BNE   STAP0160            NO  - SKIP TO NEXT ELT                       
         MVC   KEY+17(5),2(R2)     YES - INSERT CHILD CALLS                     
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    STAP0140            YES -                                        
         MVC   KEY(27),KEYSAVE     NO  - RESET KEY                              
         B     STAP0160            SKIP TO NEXT ELT                             
STAP0140 EQU   *                                                                
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        SET DELETE BIT                               
         BAS   RE,WRITE            REWRITE KEY AS DELETE                        
         BAS   RE,CHECK                                                         
STAP0160 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELT                             
         AR    R2,RF                                                            
         B     STAP0120            GO BACK FOR NEXT                             
STAP0180 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'83'                         SET RECORD ID                  
         MVI   KEY+1,X'01'                       SET RECORD SUBID               
         MVC   KEY+15(02),REC+20                 SET REP CODE                   
         MVC   KEY+22(5),RSTAKSTA-RSTAREC+REC    SET PARENT CALL LTRS           
*                                                                               
*        KEY INITIALIZED EXCEPT FOR CHILD CALL LETTERS                          
*                                                                               
         MVC   KEY+28(4),BSVDA     SET DISK ADDRESS                             
         LA    R2,RSTAELEM-RSTAREC+REC                                          
*                                  SET A(STA X'01' ELT:  NEW REC)               
STAP0200 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    STAP0300            YES                                          
         CLI   0(R2),X'0A'         COMBINED STATION ELT?                        
         BNE   STAP0220            NO  - SKIP TO NEXT ELT                       
         CLI   7(R2),C'-'          NON-PARTICIPANT?                             
         BE    STAP0220            YES - SKIP TO NEXT ELT                       
         MVC   KEY+17(5),2(R2)     YES - INSERT CHILD CALLS                     
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TOTS                                
         BAS   RE,HIGH             CHECK FOR ADD/REWRITE                        
         LA    RF,ADD              SET FOR ADD                                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                 NOT EQUAL IS 'ADD'                           
         LA    RF,WRITE            EQUAL IS 'REWRITE'                           
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF               PERFORM THE FUNCTION                         
         BAS   RE,CHECK                                                         
STAP0220 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELT                             
         AR    R2,RF                                                            
         B     STAP0200            GO BACK FOR NEXT                             
*                                                                               
* HANDLE X'8307' SIGN ON PASSIVE KEY                                            
*                                                                               
STAP0300 EQU   *                                                                
         CLI   BACT,C'A'           TEST ADD/CHANGE                              
         BE    STAP0380            ADD                                          
*                                                                               
*  DELETE OLD PASSIVE POINTER - WILL ALWAYS EXIST                               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'83'                         SET RECORD ID                  
         MVI   KEY+1,X'07'                       SET RECORD SUBID               
         MVC   KEY+18(02),REC2+20                SET REP CODE                   
         MVC   KEY+22(5),RSTAKSTA-RSTAREC+REC2   SET STATION CALL LTR           
*                                                                               
         LA    R2,RSTAELEM-RSTAREC+REC2                                         
*                                  SET A(STA X'01' ELT:  OLD REC)               
STAP0320 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    STAP0380            YES                                          
         CLI   0(R2),X'06'         SIGN ON ELT?                                 
         BNE   STAP0360            NO  - SKIP TO NEXT ELT                       
         MVC   KEY+20(2),10(R2)    YES - INSERT SIGN ON ID                      
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    STAP0340            YES - MARK DELETE                            
         MVC   KEY(27),KEYSAVE     NO  - RESET KEY                              
         B     STAP0360            SKIP TO NEXT ELT                             
STAP0340 EQU   *                                                                
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        SET DELETE BIT                               
         BAS   RE,WRITE            REWRITE KEY AS DELETE                        
         BAS   RE,CHECK                                                         
STAP0360 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELT                             
         AR    R2,RF                                                            
         B     STAP0320            GO BACK FOR NEXT                             
STAP0380 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'83'                         SET RECORD ID                  
         MVI   KEY+1,X'07'                       SET RECORD SUBID               
         MVC   KEY+18(02),REC+20                 SET REP CODE                   
         MVC   KEY+22(5),RSTAKSTA-RSTAREC+REC    SET STATION CALL LTR           
*                                                                               
*        KEY INITIALIZED                                                        
*                                                                               
         MVC   KEY+28(4),BSVDA     SET DISK ADDRESS                             
         LA    R2,RSTAELEM-RSTAREC+REC                                          
*                                  SET A(STA X'01' ELT:  NEW REC)               
STAP0400 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    STAP0500            YES                                          
         CLI   0(R2),X'06'         SIGON ON ELT?                                
         BNE   STAP0420            NO  - SKIP TO NEXT ELT                       
         MVC   KEY+20(2),10(R2)    YES - INSERT SIGN ON ID                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TOTS                                
         BAS   RE,HIGH             CHECK FOR ADD/REWRITE                        
         LA    RF,ADD              SET FOR ADD                                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                 NOT EQUAL IS 'ADD'                           
         LA    RF,WRITE            EQUAL IS 'REWRITE'                           
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF               PERFORM THE FUNCTION                         
         BAS   RE,CHECK                                                         
STAP0420 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELT                             
         AR    R2,RF                                                            
         B     STAP0400            GO BACK FOR NEXT                             
STAP0500 EQU   *                                                                
         B     EXXMOD                                                           
*                                  1  =  MASTER-ADD CLEANUP CALL                
         DS    0F                                                               
         LTORG                                                                  
         EJECT                                                                  
******************************************************                          
*   STATION LEAVE DATE ALTERNATE KEY PROCESSING                                 
*        FOR MASTER-ADD MASTER CONTROL RECORD                                   
*        CLEANUP PROCESSING.  ZERO-LEAVE-DATE                                   
*        PASSIVE IN 'REC' IS ALWAYS DELETED, AND                                
*        A PASSIVE WITH A LEAVE DATE IS INSERTED.                               
*        ALL OTHER PASSIVES REMAIN THE SAME.                                    
*                                                                               
******************************************************                          
         DS    0D                                                               
MASTAPSV NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'82'                         SET RECORD ID                  
         MVC   KEY+25(2),REC+20                  SET REP CODE                   
         MVC   KEY+17(5),RSTAKSTA-RSTAREC+REC    SET STN CALL LETTERS           
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    MPSV0020            YES                                          
         DC    H'0'                NOT FOUND - SHOULDN'T HAPPEN                 
MPSV0020 EQU   *                                                                
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        SET DELETE BIT                               
         BAS   RE,WRITE            REWRITE KEY AS DELETE                        
         BAS   RE,CHECK                                                         
MPSV0040 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'82'                         SET RECORD ID                  
         MVC   KEY+25(2),REC+20                  SET REP CODE                   
         MVC   KEY+17(5),RSTAKSTA-RSTAREC+REC    SET STN CALL LETTERS           
         MVC   KEY+22(3),RSTAEND-RSTAREC+REC     SET LEAVE DATE                 
         MVC   KEY+28(4),BSVDA     SET DISK ADDRESS                             
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TOTS                                
         BAS   RE,HIGH             CHECK FOR ADD/REWRITE                        
         LA    RF,ADD              SET FOR ADD                                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                 NOT EQUAL IS 'ADD'                           
         LA    RF,WRITE            EQUAL IS 'REWRITE'                           
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF               PERFORM THE FUNCTION                         
         BAS   RE,CHECK                                                         
MPSV0060 EQU   *                   MARKET NAME KEY TEST                         
         B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO UPDATE PREVIOUS CURRENT STATION RECORD + CONTROL                   
*        CURRENT RECORD LEAVE DATE IS SET TO NEW RECORD                         
*        JOIN DATE - 1.                                                         
**********************************************************************          
         DS    0D                                                               
UPDOLDM  NTR1  BASE=*,LABEL=*                                                   
*                                  SAVE NEW RECORD                              
         LA    R4,MYIOAREA                                                      
         LA    R5,REC                                                           
         BAS   RE,MOVEREC          MOVE REC TO MYIOAREA                         
         MVI   CURFOUND,0          SET 'CURRENT FOUND' FLAG OFF                 
         MVI   READWRIT,0          SET 'READ/WRITE' FLAG OFF                    
*                                                                               
         MVC   KEY(27),MYIOAREA    RESET KEY TO MASTER                          
         MVC   KEY+20(02),MASTER   INSERT MASTER REP CODE                       
         GOTO1 HIGH                RETRIEVE KEY                                 
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   UOLD0100            RECORD NOT FOUND - MUST BE ADDED             
         LA    RF,REC2K            ALTERNATE IO AREA                            
         ST    RF,AIOAREA                                                       
         XC    REC2K(255),REC2K    CLEAR RECEIVING AREA                         
         BAS   RE,GETREC           READ MASTER RECORD                           
*                                                                               
         LA    R4,REC2K            GET CURRENT RECORD REP CODE                  
         LA    R4,RSTAELEM-RSTAREC(R4)                                          
         MVI   READWRIT,1          SET 'READ/WRITE' TO READ FIRST               
*                                                                               
UOLD0020 DS    0H                  FIND CURRENT REP ELEMENTS                    
         CLI   0(R4),0                                                          
         BE    UOLD0080            END OF RECORD - NO CURRENT - EXIT            
         CLI   0(R4),RSTAMCCQ      CURRENT ELEMENT?                             
         BE    UOLD0060            YES - RETRIEVE CURRENT RECORD                
UOLD0040 DS    0H                  FIND CURRENT REP ELEMENTS                    
         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         B     UOLD0020            GO BACK FOR NEXT ELEMENT                     
*                                                                               
UOLD0060 DS    0H                  FIND CURRENT REP ELEMENTS                    
         MVC   KEY+20(2),REPDISP(R4)                                            
*                                  INSERT CURRENT REP INTO KEY                  
         GOTO1 HIGH                RETRIEVE KEY                                 
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+6                 RECORD FOUND                                 
         DC    H'0'                CURRENT NOT ON FILE?                         
         MVI   CURFOUND,1          SET 'CURRENT FOUND' FLAG                     
         LA    RF,REC                                                           
         ST    RF,AIOAREA          RESET A(IOAREA)                              
         XC    REC(255),REC        CLEAR RECEIVING AREA                         
         BAS   RE,GETREC           READ CURRENT RECORD                          
         MVC   DMCB+8(4),=F'-1'                                                 
         GOTO1 VADDAY,DMCB,MSTRSTRT,WORK                                        
         GOTO1 VDATCON,DMCB,WORK,(3,WORK+6)                                     
         LA    RF,REC                                                           
         LA    RF,RSTAEND-RSTAREC(RF)                                           
*                                                                               
         TM    SVPGPBIT+2,X'40'    IGNORE OVERLAP CHECK?                        
         BO    UOLD0070            YES                                          
*                                                                               
         MVC   0(3,RF),WORK+6      INSERT LEAVE DATE                            
         GOTO1 PUTREC              REWRITE STATION W/LEAVE DATE                 
*                                                                               
         GOTO1 =A(MASTAPSV),RR=Y   HANDLE ALTERNATE KEYS                        
*                                                                               
*                                                                               
*   UPDATE MASTER CONTROL RECORD WITH CHANGES OF 'CURRENT' STATION              
*                                                                               
UOLD0070 EQU   *                                                                
         GOTO1 =A(UPDCTRL),DMCB,(READWRIT,0),RR=Y                               
*                                  UPDATE MASTER CONTROL - DON'T                
*                                     REWRITE RECORD YET                        
         MVI   READWRIT,2          SET 'READ/WRITE' TO SKIP REREAD              
         B     UOLD0040            CHECK FOR MORE 'CURRENT'                     
UOLD0080 EQU   *                                                                
         CLI   CURFOUND,0          ANY CURRENT UPDATED?                         
         BE    UOLD0100            NO  - FINISHED                               
         LA    R4,REC              RESTORE NEW RECORD                           
         LA    R5,MYIOAREA                                                      
         BAS   RE,MOVEREC          MOVE REC TO MYIOAREA                         
         MVI   READWRIT,3          SET 'READ/WRITE' TO REWRITE                  
*                                                                               
*   UPDATE MASTER CONTROL RECORD WITH CHANGES OF 'NEW' STATION                  
*                                                                               
         GOTO1 =A(UPDCTRL),DMCB,(READWRIT,0),RR=Y                               
*                                  UPDATE MASTER CONTROL - DON'T                
*                                     REREAD RECORD.  REWRITE                   
*                                        WHEN FINISHED                          
*                                                                               
*   TEST                                                                        
         LA    RF,MYIOAREA                                                      
         LA    RE,REC2                                                          
         LA    R1,REC                                                           
         LA    R2,REC2K                                                         
*   TEST END                                                                    
*                                                                               
*                                                                               
UOLD0100 EQU   *                                                                
         B     EXXMOD                                                           
CURFOUND DS    XL1                                                              
READWRIT DS    XL1                                                              
         DS    0F                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO UPDATE MASTER STATION CONTROL                                      
**********************************************************************          
         DS    0D                                                               
UPDCTRL  NTR1  BASE=*,LABEL=*                                                   
         MVC   UPDFLAG,0(R1)       LOAD SPECIAL FLAG                            
*                                  NON-ZERO:  FROM MASTER/ADD UPDATE            
*                                     1 = READ MASTER, DONT REWRITE             
*                                     2 = DONT READ OR REWRITE                  
*                                     3 = DONT READ, DO REWRITE                 
*                                  ZERO:      FROM REGULAR UPDATE               
         CLC   LFMREC(4),=C'STAD'  IF NOT PLAIN STATION RECORD                  
         BE    UCTL0320             DON'T BOTHER UPDATING MASTER                
         CLC   LFMREC(4),=C'STAC'   CONTROL RECORD                              
         BE    UCTL0320                                                         
         CLC   LFMREC(3),=C'STAC' <- NOTE LENGTH OF 3 NOT 4                     
         BNE   UCTL0320                                                         
*                                                                               
         XC    WORK,WORK                                                        
E        USING RSTAMCEL,WORK                                                    
         MVI   E.RSTAMCEC,RSTAMCHQ                                              
         OC    RSTAEND,RSTAEND     LEAVE DATE?                                  
         BNZ   *+8                 YES                                          
         MVI   E.RSTAMCEC,RSTAMCCQ NO - SET AS CURRENT REP                      
         MVI   E.RSTAMCLN,RSTAMCLQ LENGTH                                       
         MVC   E.RSTAMCRC,RSTAKREP                                              
         CLI   UPDFLAG,0           USE RECORD REP CODE?                         
         BH    UCTL0020            YES                                          
         MVC   E.RSTAMCRC,REPALPHA                                              
UCTL0020 EQU   *                                                                
         MVC   E.RSTAMCJD,RSTASTRT JOIN DATE                                    
         MVC   E.RSTAMCLD,RSTAEND  LEAVE DATE                                   
         DROP  E                                                                
*                                                                               
         CLI   UPDFLAG,1           READ MASTER STATION RECORD?                  
         BH    UCTL0050            NO  - DON'T REREAD                           
         XC    KEY,KEY                                                          
K        USING RSTAKEY,KEY                                                      
         MVI   K.RSTAKTYP,X'02'                                                 
         MVC   K.RSTAKREP,MASTER                                                
         MVC   K.RSTAKSTA,RSTAKSTA                                              
         DROP  K                                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   UCTL0220            RECORD NOT FOUND - ADDREC                    
*                                                                               
         TM    KEY+L'RSTAKEY,X'80'                                              
         BZ    UCTL0040            NOT DELETED KEY, DON'T WRITE                 
*                                                                               
         NI    KEY+L'RSTAKEY,X'FF'-X'80'                                        
         GOTO1 WRITE               UNDELETE KEY                                 
*                                                                               
UCTL0040 DS    0H                                                               
         LA    RE,REC2                                                          
         ST    RE,AIOAREA                                                       
         OI    DMINBTS,X'08'                                                    
         GOTO1 GETREC                                                           
UCTL0050 DS    0H                                                               
         LA    RE,REC                                                           
         ST    RE,AIOAREA                                                       
*                                                                               
         LA    R4,REC2                                                          
         NI    RSTACNTL-RSTAREC(R4),X'FF'-X'80'                                 
         LA    R4,RSTAELEM-RSTAREC(R4)                                          
*                                                                               
UCTL0060 DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    UCTL0200            END OF RECORD                                
         CLI   UPDFLAG,0           USE RECORD REP CODE?                         
         BH    UCTL0080            YES                                          
         CLC   REPALPHA,REPDISP(R4)                                             
         BE    UCTL0120            REPCODE MATCH                                
         B     UCTL0100                                                         
UCTL0080 DS    0H                                                               
         CLC   RSTAKREP,REPDISP(R4)                                             
         BE    UCTL0120            REPCODE MATCH                                
UCTL0100 DS    0H                                                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         B     UCTL0060                                                         
*                                                                               
UCTL0120 DS    0H                                                               
         CLI   0(R4),RSTAMCHQ      OLD = HISTORY ELEMENT?                       
         BNE   UCTL0122            NO                                           
***>>>   MVC   0(RSTAMCLQ,R4),WORK YES - REPLACE OLD WITH NEW                   
         B     UCTL0190            SKIP OVER INSERT LOGIC                       
*                                                                               
UCTL0122 DS    0H                                                               
         CLI   0(R4),RSTAMCPQ      OLD = PREVIOUS ELEMENT?                      
         BNE   UCTL0140            NO                                           
*                                                                               
         CLI   WORK,RSTAMCCQ       NEW ELEMENT TO BE CURRENT?                   
         BE    UCTL0190            YES - LEAVE IT AS CURRENT                    
         MVI   WORK,RSTAMCPQ       NO  - NEW ELEMENT BECOMES PREVIOUS           
***>>>   MVC   0(RSTAMCLQ,R4),WORK YES - REPLACE OLD WITH NEW                   
         B     UCTL0190            SKIP OVER INSERT LOGIC                       
*                                                                               
UCTL0140 DS    0H                  OLD = CURRENT (ALL THAT'S LEFT)              
         OC    RSTAMCLD-RSTAMCEL+WORK(3),RSTAMCLD-RSTAMCEL+WORK                 
*                                  ANY LEAVE DATE IN NEW ELT?                   
         BZ    UCTL0180            NO                                           
         LA    RF,REC2                                                          
         LA    RF,RSTAELEM-RSTAREC(RF)                                          
*                                                                               
UCTL0150 DS    0H                                                               
         CLI   0(RF),0                                                          
         BE    UCTL0152            END OF RECORD                                
         CLI   0(RF),X'53'         HISTORY ELEMENT FOUND?                       
         BE    UCTL0152            YES - FINISHED                               
         ZIC   RE,0(RF)            NO  - CURRENT OR PREVIOUS                    
         LA    RE,1(RE)            YES - BUMP TO PREV OR HIST                   
         STC   RE,0(RF)            PUT IT BACK                                  
         LA    RF,RSTAMCLQ(RF)     BUMP TO NEXT ELEMENT                         
         B     UCTL0150            GO BACK FOR NEXT                             
UCTL0152 DS    0H                                                               
                                                                                
         MVI   WORK,RSTAMCPQ       NEW ELEMENT BECOMES PREVIOUS                 
         B     UCTL0190            GO BACK FOR NEXT                             
UCTL0180 DS    0H                  OLD = CURRENT (ALL THAT'S LEFT)              
         MVI   WORK,RSTAMCCQ       NEW ELEMENT BECOMES CURRENT                  
UCTL0190 DS    0H                  OLD = CURRENT (ALL THAT'S LEFT)              
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
*                                                                               
         GOTO1 VRECUP,DMCB,(C'R',REC2),(R4),(R4)                                
*                                  DELETE THE ORIGINAL ELEMENT                  
*                                                                               
         GOTO1 CHELLO,DMCB,(C'P',REPFILE),REC2,WORK                             
*                                  INSERT THE NEW ELEMENT                       
*                                                                               
         DROP  R7                                                               
*                                                                               
***>>>   MVC   0(RSTAMCLQ,R4),WORK REPLACE OLD WITH NEW                         
         B     UCTL0208            SKIP OVER INSERT LOGIC                       
*                                                                               
UCTL0200 DS    0H                  ADD OF NEW CURRENT - REP NOT                 
*                                     FOUND IN RECORD                           
         CLC   TWAREPN2,SPACES     MASTER UPDATE TO UPDATE CONTROL?             
         BH    UCTL020A            YES - 2NDARY NAME IN USE                     
         CLI   UPDFLAG,0           REGULAR CALL TO UPDATE CONTROL?              
         BNE   UCTL0204            NO  - COMES FROM MASTER ADD                  
*                                                                               
*   FOLLOWING CODE IS NOT DONE IN MASTER ADD.  THE 'PREVIOUS' CODE HAS          
*        ALREADY BEEN ESTABLISHED.  TO DO IT TWICE MOVES 'PREVIOUS' TO          
*        HISTORY ERRONEOUSLY.                                                   
*                                                                               
         LA    RF,REC2                                                          
         LA    RF,RSTAELEM-RSTAREC(RF)                                          
*                                                                               
UCTL0202 DS    0H                                                               
         CLI   0(RF),0                                                          
         BE    UCTL0204            END OF RECORD                                
         CLI   0(RF),X'53'         HISTORY ELEMENT FOUND?                       
         BE    UCTL0204            YES - FINISHED                               
         ZIC   RE,0(RF)            NO  - CURRENT OR PREVIOUS                    
         LA    RE,1(RE)            YES - BUMP TO PREV OR HIST                   
         STC   RE,0(RF)            PUT IT BACK                                  
         LA    RF,RSTAMCLQ(RF)     BUMP TO NEXT ELEMENT                         
         B     UCTL0202            GO BACK FOR NEXT                             
UCTL0204 DS    0H                                                               
         CLI   UPDFLAG,3           MASTER STATION UPDATE PASS?                  
         BNE   UCTL0205            NO  - USE RECUP                              
UCTL020A DS    0H                                                               
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
*                                                                               
         GOTO1 CHELLO,DMCB,(C'P',REPFILE),REC2,WORK                             
*                                                                               
         DROP  R7                                                               
*                                                                               
         B     UCTL0208                                                         
UCTL0205 DS    0H                                                               
         GOTO1 VRECUP,DMCB,(C'R',REC2),WORK,(R4)                                
UCTL0208 DS    0H                                                               
*                                                                               
         CLI   UPDFLAG,1           SKIP REWRITE OF STATION RECORD?              
         BE    UCTL0210            YES - DON'T REWRITE YET                      
         CLI   UPDFLAG,2           SKIP REWRITE OF STATION RECORD?              
         BE    UCTL0210            YES - DON'T REWRITE YET                      
*                                                                               
*   NEW CONTROL RECORD IS IN REC2  -  REREAD OLD CONTROL RECORD                 
*        INTO REC2K TO RESET FOR REWRITE                                        
*                                                                               
         LA    RE,REC2K            SET A(IOAREA) -> REC2K                       
         ST    RE,AIOAREA                                                       
         MVC   KEY(27),REC2        RESET KEY                                    
         GOTO1 HIGH                REREAD KEY TO UPDATE CTL REC                 
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                NOT FOUND?                                   
         GOTO1 GETREC              READ OLD REC INTO REC2K                      
*                                                                               
*   SET TO REWRITE NEW CONTROL RECORD FROM REC2                                 
*                                                                               
         LA    RE,REC2             SET A(IOAREA) -> REC2                        
         ST    RE,AIOAREA                                                       
         GOTO1 PUTREC                                                           
UCTL0210 DS    0H                  NEW RECORD TO BE ADDED                       
         LA    RE,REC                                                           
         ST    RE,AIOAREA                                                       
         B     UCTL0320                                                         
*                                                                               
UCTL0220 DS    0H                  NEW RECORD TO BE ADDED                       
         XC    REC2(256),REC2                                                   
R        USING RSTAREC,REC2                                                     
         MVI   R.RSTAKTYP,X'02'                                                 
         MVC   R.RSTAKREP,MASTER                                                
         MVC   R.RSTAKSTA,RSTAKSTA                                              
         MVC   R.RSTALEN,=Y((RSTAELEM-RSTAREC)+RSTAMCLQ+1)                      
         MVC   R.RSTAELEM(RSTAMCLQ),WORK                                        
         DROP  R                                                                
*                                                                               
         LA    RE,REC2                                                          
         ST    RE,AIOAREA                                                       
         CLI   UPDFLAG,1           SKIP ADD OF STATION RECORD?                  
         BE    UCTL0240            YES - DON'T REWRITE YET                      
         CLI   UPDFLAG,2           SKIP ADD OF STATION RECORD?                  
         BE    UCTL0240            YES - DON'T REWRITE YET                      
         GOTO1 ADDREC                                                           
UCTL0240 DS    0H                                                               
         LA    RE,REC                                                           
         ST    RE,AIOAREA                                                       
UCTL0320 DS    0H                                                               
         B     EXXMOD                                                           
REPFILE  DC    C'REPFILE'                                                       
UPDFLAG  DS    CL1                                                              
REPDISP  EQU   RSTAMCRC-RSTAMCEL                                                
         DS    0F                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO VALIDATE DATE CHANGES AGAINST MASTER STATION CONTROL               
**********************************************************************          
         DS    0D                                                               
VALCTRL  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,0              SET ERROR FLAG OFF                           
         MVI   ENDFLAG,0           SET END DATE FLAG OFF                        
         XC    KEY,KEY                                                          
K        USING RSTAKEY,KEY                                                      
         MVI   K.RSTAKTYP,X'02'                                                 
         MVC   K.RSTAKREP,MASTER                                                
         MVC   K.RSTAKSTA,RSTAKSTA                                              
         DROP  K                                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   VCTRLOK             RECORD NOT FOUND                             
*                                                                               
         LA    RE,REC2                                                          
         ST    RE,AIOAREA                                                       
         GOTO1 GETREC                                                           
         LA    RE,REC                                                           
         ST    RE,AIOAREA                                                       
*                                                                               
*                                  MASTER ADD?                                  
         CLC   TWAREPN2,SPACES     ADD WILL HAVE 2NDARY NAME                    
         BH    VCTL0180            2NDARY NAME - CHECK DATES VS                 
*                                     1ST ENTRY IN RECORD                       
*   CHANGE:  COMPARE JOIN/LEAVE DATE OF NEW ENTRY TO ALL OTHER                  
*        REPS' ENTRIES.  SKIP SAME REP                                          
*                                                                               
         LA    R4,REC2                                                          
         LA    R4,RSTAELEM-RSTAREC(R4)                                          
VCTL0020 DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    VCTL0120            END OF RECORD                                
*                                                                               
         USING RSTAMCEL,R4                                                      
         CLC   REPALPHA,REPDISP(R4)                                             
         BNE   VCTL0040            NO MATCH -                                   
         OC    RSTAMCLD,RSTAMCLD   REPCODE MATCH - ANY END DATE                 
*                                     IN ELEMENT?                               
         BNZ   VCTL0100            YES -                                        
         MVI   ENDFLAG,1           NO  - MAY ACCEPT END DATE                    
         B     VCTL0100                                                         
VCTL0040 EQU   *                                                                
         MVC   OLDSTART(6),RSTAMCJD                                             
*                                  SET CONTROL START/END DATES                  
*                                     FOR COMPARISON                            
         DROP  R4                                                               
         OC    OLDEND,OLDEND       ANY END DATE?                                
         BNZ   VCTL0060            YES                                          
         MVC   OLDEND,=X'FFFFFF'   NO  - SET TO INFINITY                        
VCTL0060 EQU   *                                                                
         MVC   NEWSTART(6),RSTASTRT                                             
*                                  SET STATION START END/DATES                  
*                                     FOR COMPARISON                            
         OC    NEWEND,NEWEND       ANY END DATE?                                
         BNZ   VCTL0080            YES                                          
         MVC   NEWEND,=X'FFFFFF'   NO  - SET TO INFINITY                        
VCTL0080 EQU   *                                                                
         CLC   NEWSTART,OLDEND     NEW RECORD AFTER OLD END?                    
         BH    VCTL0100            YES - NO OVERLAP                             
*                                     CHECK NEXT RECORD                         
         BE    VCTL0200            NO  - EQUAL = OVERLAP ERROR                  
         CLC   NEWEND,OLDSTART     NO  - LESS: NEW END MUST BE                  
*                                     PRIOR TO OLD START DATE                   
         BNL   VCTL0200                                                         
         B     VCTL0100                                                         
*                                                                               
NEWSTART DS    CL3                 NEW RECORD START                             
NEWEND   DS    CL3                 NEW RECORD END                               
OLDSTART DS    CL3                 OLD RECORD START                             
OLDEND   DS    CL3                 OLD RECORD END                               
*                                                                               
VCTL0100 DS    0H                                                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         B     VCTL0020                                                         
*                                                                               
VCTL0120 DS    0H                                                               
         LA    R4,REC2                                                          
         LA    R4,RSTAELEM-RSTAREC(R4)                                          
VCTL0140 DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    VCTRLOK             END OF RECORD                                
         CLC   REPALPHA,REPDISP(R4)                                             
         BE    VCTL0160            REPCODE MATCH - DON'T COMPARE                
*                                     AS THIS WILL BE REPLACED                  
         CLI   0(R4),RSTAMCCQ      CURRENT ELEMENT?                             
         BNE   VCTL0160            NO  - BUMP TO NEXT ELEMENT                   
         CLI   ENDFLAG,0           ELT BEING CHANGED HAVE END DATE?             
         BE    VCTRLBAD            YES - CAN'T CHANGE IT                        
*                                  NO  - BUMP TO NEXT ELEMENT                   
VCTL0160 EQU   *                                                                
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         B     VCTL0140                                                         
VCTL0180 DS    0H                  ADD: CHECK FOR DATE CONTINUITY               
         LA    R4,REC2                                                          
         LA    R4,RSTAELEM-RSTAREC(R4)                                          
         USING RSTAMCEL,R4                                                      
         GOTO1 VDATCON,DMCB,MSTRSTRT,(3,WORK+36)                                
*                                  MASTER START DATE TO BINARY                  
         OC    RSTAMCLD,RSTAMCLD   ANY LEAVE DATE IN MASTER REC?                
         BZ    VCTL0220            NO  - COMPARE AGAINST START DATE             
         CLC   RSTAMCLD,WORK+36    LEAVE DATE BEFORE NEW JOIN DATE?             
         BL    VCTRLOK             YES - ACCEPT DATE                            
VCTL0200 EQU   *                                                                
         MVC   HALF,RSTAMCRC       SAVE OVERLAPPING REP CODE                    
         MVI   BYTE,1              SET ERROR FLAG ON                            
         B     VCTRLBAD            EXIT WITH ERROR                              
VCTL0220 EQU   *                                                                
         MVC   HALF,RSTAMCRC       SAVE OVERLAPPING REP CODE                    
*                                  NO LEAVE DATE:  CHECK VS START DATE          
         CLC   RSTAMCJD,WORK+36    JOIN  DATE BEFORE NEW JOIN DATE?             
         BL    VCTRLOK             YES - ACCEPT DATE                            
         MVI   BYTE,1              SET ERROR FLAG ON                            
         B     VCTRLBAD            EXIT WITH ERROR                              
         DROP  R4                                                               
*                                                                               
VCTRLBAD DS    0H                                                               
         LTR   RB,RB               EXIT NE                                      
         B     VCTRLX                                                           
*                                                                               
VCTRLOK  DS    0H                                                               
         CR    RB,RB               EXIT EQ                                      
VCTRLX   DS    0H                                                               
         B     EXXMOD                                                           
ENDFLAG  DS    CL1                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT STATION DATES INFORMATION                                   
***********************************************************************         
         DS    0D                                                               
DISDATE  NTR1  BASE=*,LABEL=*                                                   
         FOUT  SDTJDTH                                                          
         OC    RSTASTRT,RSTASTRT   ANY START DATE? (MUST BE!)                   
         BNZ   *+6                 YES                                          
         DC    H'0'                NO                                           
         GOTO1 VDATCON,DMCB,(3,RSTASTRT),(5,SDTJDT)                             
         MVC   SDTDDT,SPACES                                                    
         FOUT  SDTDDTH                                                          
         OC    RSTAEND,RSTAEND     ANY END   DATE?                              
         BZ    STAA0008            NO                                           
         GOTO1 VDATCON,DMCB,(3,RSTAEND),(5,SDTDDT)                              
STAA0008 EQU   *                                                                
         MVC   SDTRJD,SPACES                                                    
         LA    R2,SDTRJDH                                                       
         OI    4(R2),X'20'         SET PREVIOUSLY VALID                         
         FOUT  (R2)                                                             
         MVC   SDTRLD,SPACES                                                    
         LA    R2,SDTRLDH                                                       
         OI    4(R2),X'20'         SET PREVIOUSLY VALID                         
         FOUT  (R2)                                                             
         GOTO1 VGETEL,DMCB,(X'0D',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          NO X'0D' JOIN/LEAVE REPORT ELT               
         BE    STAA0015                                                         
         L     R5,DMCB+8                                                        
         USING RSTAJLEL,R5                                                      
         OC    RSTAJOIN,RSTAJOIN   ANY JOIN DATE?                               
         BZ    STAA0010            NO                                           
         XC    WORK,WORK                                                        
         MVC   WORK(2),RSTAJOIN                                                 
         GOTO1 VDATCON,DMCB,(3,WORK),(6,SDTRJD)                                 
STAA0010 EQU   *                                                                
         OC    RSTALEAV,RSTALEAV   ANY LEAVE DATE?                              
         BZ    STAA0015            NO                                           
         XC    WORK,WORK                                                        
         MVC   WORK(2),RSTALEAV                                                 
         GOTO1 VDATCON,DMCB,(3,WORK),(6,SDTRLD)                                 
*                                                                               
         DROP  R5                                                               
*                                                                               
STAA0015 EQU   *                                                                
         MVC   SDTCTR,SPACES                                                    
         LA    R2,SDTCTRH                                                       
         OI    4(R2),X'20'         SET PREVIOUSLY VALID                         
         FOUT  (R2)                                                             
         GOTO1 VGETEL,DMCB,(X'10',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          NO X'10' CONTRACT END DATE ELT               
         BE    STAA0018                                                         
*                                                                               
         L     R5,DMCB+8           R5 -> A(CONTRACT END DATE ELT)               
         USING RSTADTEL,R5                                                      
         OC    RSTADTCE,RSTADTCE   ANY CONTRACT END DATE?                       
         BZ    STAA0018            NO                                           
         XC    WORK,WORK                                                        
         MVC   WORK(L'RSTADTCE),RSTADTCE                                        
         GOTO1 VDATCON,DMCB,(3,WORK),(6,SDTCTR)                                 
         DROP  R5                                                               
*                                                                               
STAA0018 EQU   *                                                                
         FOUT  SDTLDTH             SET FIELD TO TRANSMIT                        
         FOUT  SDTPGMH             SET FIELD TO TRANSMIT                        
         FOUT  SDTLUIH             SET FIELD TO TRANSMIT                        
         MVC   SDTLDT,SPACES                                                    
         MVC   SDTPGM,SPACES                                                    
         MVC   SDTLUI,SPACES                                                    
         GOTO1 VGETEL,DMCB,(X'23',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          NO X'23' ACTIVITY ELEMENT                    
         BE    STAA0100                                                         
         L     R5,DMCB+8                                                        
         USING RSTACOEL,R5                                                      
         GOTO1 VDATCON,DMCB,(3,RSTACODT),(5,SDTLDT)                             
         MVC   SDTLUI(8),RSTACOLU  INSERT LUID OF LAST STADATE                  
         TM    RSTACOFL,X'80'      TAPE ACTUALIZER?                             
         BNO   STAA0030            NO                                           
         MVC   SDTPGM(20),=C'TAPE ACTUALIZER     '                              
         B     STAA0100                                                         
STAA0030 EQU   *                                                                
         TM    RSTACOFL,X'40'      INVOICE PGM?                                 
         BNO   STAA0040            NO                                           
         MVC   SDTPGM(20),=C'INVOICE PROGRAM     '                              
         B     STAA0100                                                         
STAA0040 EQU   *                                                                
         TM    RSTACOFL,X'20'      RE16 REPORT?                                 
         BNO   STAA0060            NO                                           
         MVC   SDTPGM(20),=C'RE16 REPORT         '                              
         B     STAA0100                                                         
STAA0060 EQU   *                                                                
         TM    RSTACOFL,X'10'      FILE PROGRAM?                                
         BNO   STAA0080            NO                                           
         MVC   SDTPGM(20),=C'FILE PROGRAM        '                              
         B     STAA0100                                                         
STAA0080 EQU   *                                                                
         MVC   SDTPGM(20),=C'UNRECOGNIZED MEANS  '                              
*                                                                               
         DROP  R5                                                               
*                                                                               
STAA0100 EQU   *                                                                
         MVC   SDTCDT,SPACES       CLEAR THE CLOSED DATE FIELD                  
         OC    RSTACLDT,RSTACLDT   ANY CLOSED DATE?                             
         BZ    STAA0120            NO                                           
         MVC   WORK(2),RSTACLDT                                                 
         MVC   WORK+2(1),=X'01'                                                 
         GOTO1 VDATCON,DMCB,(3,WORK),(6,SDTCDT)                                 
STAA0120 EQU   *                                                                
         LA    R2,SDTCDTH                                                       
         OI    4(R2),X'20'         SET PREVIOUSLY VALID                         
         FOUT  (R2)                                                             
*                                                                               
* CREATE AND LAST ACTIVITY DATE FIELDS                                          
*                                                                               
         LA    R2,SDTCREH                                                       
         MVC   8(L'SDTCRE,R2),SPACES                                            
         FOUT  (R2)                                                             
         LA    R2,SDTLADH                                                       
         MVC   8(L'SDTLAD,R2),SPACES                                            
         FOUT  (R2)                                                             
         GOTO1 VGETEL,DMCB,(X'F1',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    STAA0160                                                         
*                                                                               
         L     R5,DMCB+8                                                        
         USING RSTACTEL,R5                                                      
         GOTO1 VDATCON,DMCB,(3,RSTACTAD),(11,SDTCRE)                            
         GOTO1 VDATCON,DMCB,(3,RSTACTCD),(11,SDTLAD)                            
         DROP  R5                                                               
STAA0160 EQU   *                                                                
*                                                                               
* HISTORY TAKEOVER DATE FIELD                                                   
*                                                                               
         LA    R2,SDTHISH                                                       
         MVC   8(L'SDTHIS,R2),SPACES                                            
         FOUT  (R2)                                                             
         OC    RSTAHIST,RSTAHIST                                                
         BZ    STAA0200                                                         
         GOTO1 VDATCON,DMCB,(2,RSTAHIST),(11,SDTHIS)                            
STAA0200 EQU   *                                                                
*                                                                               
* INVOICE DATA TAKEOVER FIELD                                                   
*                                                                               
         LA    R2,SDTINVH                                                       
         MVC   8(L'SDTINV,R2),SPACES                                            
         FOUT  (R2)                                                             
         TM    RSTAFLGS,X'80'      TAKE INVOICE DATA SET?                       
         BZ    STAA0210            NO                                           
         MVI   SDTINV,C'Y'         YES                                          
STAA0210 EQU   *                                                                
*                                                                               
* INVENTORY BOOK LIST AND ADDITIONAL TYPES                                      
*                                                                               
         LA    R2,SDTIBLH                                                       
         MVC   8(L'SDTIBL,R2),SPACES                                            
         FOUT  (R2)                                                             
         LA    R2,SDTIBLTH                                                      
         MVC   8(L'SDTIBLT,R2),SPACES                                           
         FOUT  (R2)                                                             
         GOTO1 VGETEL,DMCB,(X'48',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    STAA0220                                                         
*                                                                               
         L     R5,DMCB+8                                                        
         USING RSTABLEL,R5                                                      
         MVC   SDTIBL(L'RSTABLTG),RSTABLTG                                      
         MVC   SDTIBLT,RSTABLTS                                                 
         DROP  R5                                                               
*                                                                               
STAA0220 EQU   *                                                                
*                                                                               
         B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
*MN                                                                             
***********************************************************************         
* ROUTINE TO FORMAT STATION DATES INFORMATION                                   
***********************************************************************         
         DS    0D                                                               
DISPID   NTR1  BASE=*,LABEL=*                                                   
         MVC   SDTPID1,SPACES                                                   
         FOUT  SDTPID1H                                                         
         MVC   SDTPID2,SPACES                                                   
         FOUT  SDTPID2H                                                         
         MVC   SDTPID3,SPACES                                                   
         FOUT  SDTPID3H                                                         
         MVC   SDTPID4,SPACES                                                   
         FOUT  SDTPID4H                                                         
                                                                                
         GOTO1 VGETEL,DMCB,(X'13',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    DISPIDX                                                          
         USING RSTPDTEL,R4                                                      
         L     R4,DMCB+8                                                        
         MVC   SDTPID1,RSTADTPI                                                 
         FOUT  SDTPID1H                                                         
                                                                                
         LA    R2,SDTPID2H                                                      
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
DISP010  CLI   0(R4),0                                                          
         BE    DISPIDX                                                          
         CLI   0(R4),X'13'                                                      
         BNE   DISP020                                                          
         MVC   8(L'RSTADTPI,R2),RSTADTPI                                        
         FOUT  (R2)                                                             
         LA    R2,16(R2)                                                        
DISP020  ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DISP010                                                          
                                                                                
DISPIDX  EQU   *                                                                
         B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
         DROP  R4                                                               
*MN                                                                             
***********************************************************************         
* ROUTINE TO DISPLAY VENDOR INFORMATION                                         
***********************************************************************         
         DS    0D                                                               
DISPVEND NTR1  BASE=*,LABEL=*                                                   
         MVC   SDTVEND,SPACES                                                   
         FOUT  SDTVENDH                                                         
                                                                                
         GOTO1 VGETEL,DMCB,(X'2C',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    DISPVDX                                                          
         USING RSTAETEL,R4                                                      
         L     R4,DMCB+8                                                        
         MVC   SDTVEND,RSTAETVN    DISPLAY VENDOR IDENTIFICATION                
         FOUT  SDTVENDH                                                         
         GOTO1 VDATCON,DMCB,(3,RSTAETDT),(5,SDTVDAT)                            
         FOUT  SDTVDATH            DISPLAY DATE LAST CHANGED                    
         MVC   SDTVLUI,RSTAETLU    DISPLAY LUID MAKING CHANGE                   
         FOUT  SDTVLUIH                                                         
                                                                                
DISPVDX  EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         DROP  R4                                                               
*MN                                                                             
***********************************************************************         
* ROUTINE TO LIST DDS ID/UID HISTORY ELEMENTS                                   
*        REC CONTAINS STATION RECORD                                            
***********************************************************************         
         DS    0D                                                               
DISDDSID NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,CLEARUID                                                      
         LA    R2,UIDUID1H         SET A(1ST LINE, 1ST ID)                      
         MVC   15(7,R2),=C'NONE   '                                             
         GOTO1 VGETEL,DMCB,(X'2A',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          ACTIVE REP FOUND?                            
         BE    DDSI0020            NOT FOUND                                    
         L     R4,DMCB+8           SET A(ELEMENT FOUND)                         
         USING RSTAUIEL,R4                                                      
         MVC   8(6,R2),RSTAUIST    INSERT DDS ID                                
         MVC   15(7,R2),=C'CURRENT'                                             
         DROP  R4                                                               
DDSI0020 EQU   *                                                                
         GOTO1 VGETEL,DMCB,(X'2B',REC),DMCB+8                                   
*                                  LOOK FOR UID HISTORY ELEMENT                 
         CLI   DMCB,X'FF'          ELEMENT FOUND?                               
         BE    DDSI0100            NO  - NO 2B ELEMENTS                         
         L     R4,DMCB+8           SET A(ELEMENT FOUND)                         
         B     DDSI0060                                                         
DDSI0040 EQU   *                                                                
         ZIC   RF,1(R4)            GET ELEMENT LENGTH                           
         AR    R4,RF               BUMP TO NEXT ELEMENT                         
         CLI   0(R4),X'2B'         UID HISTORY ELEMENT?                         
         BNE   DDSI0100            NO  - FINISHED                               
DDSI0060 EQU   *                                                                
         ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF               BUMP TO NEXT FIELD                           
         LA    RE,UIDUIDLH         SET A(LAST FIELD)                            
         CR    R2,RE                                                            
         BNL   DDSI0900            PAST SCREEN                                  
         USING RSTAUHEL,R4                                                      
         MVC   8(6,R2),RSTAUHST    INSERT DDS ID                                
         GOTO1 VDATCON,DMCB,(3,RSTAUHDT),(5,15(R2))                             
         MVC   24(8,R2),RSTAUHLU                                                
         B     DDSI0040            GO BACK FOR NEXT                             
DDSI0100 EQU   *                                                                
DDSI0900 EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
CLEARUID NTR1                                                                   
         LA    R2,UIDUID1H         SET A(1ST LINE, 1ST ID)                      
CUID0020 EQU   *                                                                
         LA    RE,UIDUIDLH         SET A(LAST FIELD)                            
         CR    R2,RE                                                            
         BNL   CUID0900            PAST SCREEN                                  
         MVC   8(24,R2),SPACES                                                  
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     CUID0020                                                         
CUID0900 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT OFFICE/TEAM SCREEN                                          
***********************************************************************         
         DS    0D                                                               
DISOFTM  NTR1  BASE=*,LABEL=*                                                   
         LA    RF,REC2             SET A(ALTERNATE IO AREA)                     
         ST    RF,AIOAREA                                                       
         LA    R2,OTMOF1H          SET A(1ST OFFICE FIELD HEADER)               
         LA    R3,OTMTM1H          SET A(1ST TEAM   FIELD HEADER)               
         LA    R5,OTMOFNH          SET A(1ST OFFICE NAME FIELD)                 
         LA    R7,OTMTMNH          SET A(1ST TEAM   NAME FIELD)                 
         LA    R4,RSTAELEM                                                      
         MVI   BYTE,4                                                           
OFFD0010 BAS   RE,NEXTEL                                                        
         BNE   OFFD0040                                                         
         ST    R4,DUB              SAVE R4 TEMPORARILY                          
         USING RSTAOTEL,R4                                                      
         MVC   8(2,R2),RSTAOTOF    INSERT OFFICE                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         FOUT (R2)                                                              
         XC    KEY,KEY             RETRIEVE OFFICE NAME                         
         MVI   KEY,4                                                            
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),8(R2)     OFFICE CODE FROM SCREEN                      
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   OFFD0020            NOT FOUND - NO NAME?                         
         GOTO1 GETREC                                                           
         MVC   8(20,R5),ROFFNAME   INSERT OFFICE NAME                           
         FOUT  (R5)                                                             
OFFD0020 EQU   *                                                                
         USING RSTAOTEL,R4                                                      
         MVC   8(2,R3),RSTAOTTM    INSERT TEAM                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         FOUT  (R3)                                                             
         XC    KEY,KEY             RETRIEVE TEAM NAME                           
         MVI   KEY,5                                                            
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),8(R3)                                                  
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   OFFD0030            NOT FOUND?  NO NAME??                        
         GOTO1 GETREC                                                           
         MVC   8(10,R7),RTEMDVNM   INSERT DIVISION NAME                         
         MVC   19(10,R7),RTEMNAME  INSERT TEAM     NAME                         
         FOUT  (R7)                                                             
OFFD0030 EQU   *                                                                
         LA    R2,NXTOTFLD(R2)     BUMP TO NEXT OFFICE FIELD                    
         LA    R3,NXTOTFLD(R3)     BUMP TO NEXT TEAM   FIELD                    
         LA    R5,NXTOTFLD(R5)     BUMP TO NEXT OFFICE NAME                     
         LA    R7,NXTOTFLD(R7)     BUMP TO NEXT TEAM   NAME                     
         L     R4,DUB              RESTORE R4                                   
         MVI   BYTE,4              RESET ELEMENT VALUE                          
         B     OFFD0010                                                         
*                                                                               
NXTOTFLD EQU   OTMOF2H-OTMOF1H                                                  
*                                                                               
OFFD0040 EQU   *                                                                
         LA    RF,OTMLSTH          CLEAR TO END OF SCREEN                       
OFFD0050 EQU   *                                                                
         CR    R2,RF               END OF SCREEN REACHED?                       
         BNL   OFFD0060            YES                                          
         ZIC   RE,0(R2)            GET FIELD LENGTH: OFFICE                     
         SH    RE,=H'9'            SUBTRACT LENGTH+1 FOR EX                     
         EX    RE,MVCSPAC2         MOVE SPACE TO FIELD                          
         FOUT  (R2)                                                             
         ZIC   RE,0(R3)            GET FIELD LENGTH: TEAM                       
         SH    RE,=H'9'            SUBTRACT LENGTH+1 FOR EX                     
         EX    RE,MVCSPAC3         MOVE SPACE TO FIELD                          
         FOUT  (R3)                                                             
         ZIC   RE,0(R5)            GET FIELD LENGTH: OFFICE                     
         SH    RE,=H'9'            SUBTRACT LENGTH+1 FOR EX                     
         EX    RE,MVCSPAC5         MOVE SPACE TO FIELD                          
         FOUT  (R5)                                                             
         ZIC   RE,0(R7)            GET FIELD LENGTH: TEAM                       
         SH    RE,=H'9'            SUBTRACT LENGTH+1 FOR EX                     
         EX    RE,MVCSPAC7         MOVE SPACE TO FIELD                          
         FOUT  (R7)                                                             
         LA    R2,NXTOTFLD(R2)     BUMP TO NEXT OFFICE FIELD                    
         LA    R3,NXTOTFLD(R3)     BUMP TO NEXT TEAM   FIELD                    
         LA    R5,NXTOTFLD(R5)     BUMP TO NEXT OFFICE NAME                     
         LA    R7,NXTOTFLD(R7)     BUMP TO NEXT TEAM   NAME                     
         B     OFFD0050            GO BACK FOR NEXT                             
OFFD0060 EQU   *                                                                
         LA    RF,REC                                                           
         ST    RF,AIOAREA          RESET A(ORIGINAL IO AREA)                    
         B     EXXMOD                                                           
*                                                                               
MVCSPAC2 MVC   8(0,R2),SPACES      CLEAR OFFICE                                 
MVCSPAC3 MVC   8(0,R3),SPACES      CLEAR TEAM                                   
MVCSPAC5 MVC   8(0,R5),SPACES      CLEAR OFFICE NAME                            
MVCSPAC7 MVC   8(0,R7),SPACES      CLEAR TEAM   NAME                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO DISPLAY THE DEFAULT STATION SCREEN                                 
*********************************************************************           
         DS    0D                                                               
DISSTA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* MARKET                                                                        
         LA    R2,FMSMKH                                                        
         MVC   8(L'FMSMK,R2),RSTAMKT                                            
         FOUT  (R2)                                                             
*                                                                               
* CONTRACT TO STATION?                                                          
         LA    R2,FMSCSTH                                                       
         MVC   8(3,R2),=C'YES'                                                  
         TM    RSTASTAT,X'08'                                                   
         BZ    *+10                                                             
         MVC   8(3,R2),=C'NO '                                                  
         FOUT  (R2)                                                             
*                                                                               
* RECAP?                                                                        
         LA    R2,FMSCAPH                                                       
         MVC   8(3,R2),=C'YES'                                                  
         TM    RSTASTAT,X'04'                                                   
         BZ    *+10                                                             
         MVC   8(3,R2),=C'NO '                                                  
         FOUT  (R2)                                                             
*                                                                               
* CHANNEL                                                                       
         LA    R2,FMSCHH                                                        
         EDIT  RSTACHAN,(4,8(R2)),ALIGN=LEFT                                    
         FOUT  (R2)                                                             
*                                                                               
* GROUP/SUBGROUP                                                                
         LA    R2,FMSGSGH                                                       
         MVC   8(L'FMSGSG,R2),SPACES                                            
         CLC   REC2K(4),=C'JOIN'   REDISPLAY FLAG FROM MASTER ADD?              
         BE    DISS0020                                                         
         CLC   TWAREPN2,SPACES     MASTER ADD DISPLAY?                          
         BH    DISS0040            YES - DON'T SHOW GROUP/SUBGRP                
DISS0020 EQU   *                                                                
         MVC   8(L'FMSGSG,R2),RSTAGRUP                                          
         MVC   SVGRUP,RSTAGRUP                                                  
         FOUT  (R2)                                                             
*                                                                               
* EXPAND GROUP/SUBGROUP                                                         
         LA    R2,FMSGSGH                                                       
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,7                                                            
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RSTAGRUP                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,REC2                                                          
         ST    R4,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         LA    R4,REC                                                           
         ST    R4,AIOAREA                                                       
         LA    R4,REC2                                                          
         USING RGRPREC,R4                                                       
         MVC   8(20,R2),RGRPNAME                                                
         FOUT  (R2)                                                             
         DROP  R4                                                               
DISS0040 EQU   *                                                                
*                                                                               
* TRAFFIC FORMAT                                                                
         LA    R2,FMSTRFH                                                       
         MVC   8(L'FMSTRF,R2),SPACES                                            
         CLI   RSTATRAF,0                                                       
         BE    *+10                                                             
         MVC   8(L'RSTATRAF,R2),RSTATRAF                                        
         FOUT  (R2)                                                             
*                                                                               
* TVB REGION                                                                    
         LA    R2,FMSTVBH                                                       
         MVC   8(L'FMSTVB,R2),SPACES                                            
         FOUT  (R2)                                                             
         CLI   RSTATVB,0                                                        
         BE    DISS0060                                                         
         L     R4,=A(TVBLST)                                                    
         A     R4,RELO                                                          
*                                                                               
         CLC   0(2,R4),RSTATVB                                                  
         BE    *+18                                                             
         LA    R4,20(R4)                                                        
         CLI   0(R4),X'FF'                                                      
         BNE   *-18                                                             
         DC    H'0'                                                             
*                                                                               
         MVC   8(2,R2),0(R4)                                                    
         FOUT  (R2)                                                             
*                                                                               
* EXPAND TVB NAME                                                               
DISS0060 ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(20,R2),SPACES                                                  
         CLI   RSTATVB,0                                                        
         BE    *+10                                                             
         MVC   8(18,R2),2(R4)                                                   
         FOUT  (R2)                                                             
*                                                                               
* JOIN DATE                                                                     
         LA    R2,FMSJDH                                                        
         MVC   8(L'FMSJD,R2),SPACES                                             
         CLC   REC2K(4),=C'JOIN'   REDISPLAY FLAG FROM MASTER ADD?              
         BE    DISS0080                                                         
         CLC   TWAREPN2,SPACES     MASTER ADD DISPLAY?                          
         BH    DISS0100            YES - DON'T SHOW JOIN/LEAVE DTS              
DISS0080 EQU   *                                                                
         XC    REC2K(4),REC2K      CLEAR REDISPLAY FLAG                         
         GOTO1 VDATCON,DMCB,(3,RSTASTRT),(8,8(R2))                              
DISS0100 EQU   *                                                                
         OI    4(R2),X'20'         SET PREVIOUSLY VALID                         
         FOUT  (R2)                                                             
*                                                                               
* LEAVE DATE                                                                    
         LA    R2,FMSLDH                                                        
         MVC   8(L'FMSLD,R2),SPACES                                             
         OC    RSTAEND,RSTAEND                                                  
         BZ    DISS0120                                                         
         CLC   TWAREPN2,SPACES     MASTER ADD DISPLAY?                          
         BH    DISS0120            YES - DON'T SHOW JOIN/LEAVE DTS              
         GOTO1 VDATCON,DMCB,(3,RSTAEND),(8,8(R2))                               
DISS0120 EQU   *                                                                
         OI    4(R2),X'20'         SET PREVIOUSLY VALID                         
         FOUT  (R2)                                                             
*                                                                               
* OWNER                                                                         
         LA    R2,FMSOWNH          SET A(OWNERSHIP FIELD)                       
         MVC   8(L'FMSOWN,R2),SPACES                                            
*                                  INITIALIZE TO SPACES                         
         OC    RSTAOWN,RSTAOWN     ANY OWNER IN STATION REC?                    
         BZ    *+10                NO  - SET PREVAL AND FOUT                    
         MVC   8(3,R2),RSTAOWN     YES - INSERT OWNER                           
         OI    4(R2),X'20'         SET PREVIOUSLY VALID                         
         FOUT  (R2)                                                             
*                                                                               
* EXPAND OWNER NAME                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         SPACE 1                                                                
         MVC   8(20,R2),SPACES                                                  
         OC    RSTAOWN,RSTAOWN                                                  
         BZ    DISS0180                                                         
* READ OWNERSHIP RECORD FOR NAME                                                
         LA    R4,REC2                                                          
         ST    R4,AIOAREA          GET OWNERSHIP IN REC2                        
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'2A'                                                        
         MVC   KEY+22(2),REPALPHA                                               
         MVC   KEY+24(3),RSTAOWN                                                
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    DISS0140            FOUND                                        
         MVC   8(17,R2),=C'OWNER NOT ON FILE'                                   
         B     DISS0160                                                         
DISS0140 EQU   *                                                                
         BAS   RE,GETREC                                                        
         LA    R4,REC2                                                          
         USING ROWNREC,R4                                                       
         MVC   8(20,R2),ROWNNAME                                                
         DROP  R4                                                               
DISS0160 EQU   *                                                                
         LA    R4,REC              POINT BACK TO STATION RECORD                 
         ST    R4,AIOAREA                                                       
DISS0180 FOUT  (R2)                                                             
*                                                                               
* RANK                                                                          
         LA    R2,FMSRNKH                                                       
         MVC   8(1,R2),SPACES                                                   
         FOUT  (R2)                                                             
         CLI   RSTARANK,0                                                       
         BE    *+10                                                             
         MVC   8(1,R2),RSTARANK                                                 
         FOUT  (R2)                                                             
*                                                                               
* STATUS                                                                        
         LA    R2,FMSSTH                                                        
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                EAR FIELD TO SPACES                        
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         FOUT  (R2)                                                             
         TM    RSTASTAT,X'F1'      ANY STATUS BITS ON (OTHER THAN               
*                        08-CON TO STATION, 04-RECAP, 02-DON'T SEND)            
         BZ    *+8                 NO                                           
         BAS   RE,UNSCAN           DISPLAY STATUS FIELD                         
*                                                                               
* FORMER REP/NEW REP                                                            
         MVC   FMSFOR,SPACES       CLEAR TO SPACES                              
         FOUT  FMSFORH             PUT TO SCREEN                                
         MVC   FMSNEW,SPACES                                                    
         FOUT  FMSNEWH                                                          
*                                                                               
         GOTO1 VGETEL,DMCB,(X'0C',REC),DMCB+8  LOOK FOR ELEMENT                 
         CLI   DMCB,X'FF'          IS THE ELEMENT THERE?                        
         BE    FNFMEXIT            NO, NOTHING TO PUT TO SCREEN                 
*                                                                               
         L     R5,DMCB+8           BEGINNING OF ELEMENT                         
         USING RSTAFNEL,R5                                                      
         MVC   FMSFOR,RSTAFNFO     GET THE FORMER REP                           
         FOUT  FMSFORH             PUT TO SCREEN                                
         MVC   FMSNEW,RSTAFNNE     GET THE NEW REP                              
         FOUT  FMSNEWH                                                          
         DROP  R5                                                               
*                                                                               
FNFMEXIT DS    0H                                                               
*                                                                               
* RECEIVING ID                                                                  
         LA    R2,FMSRIDH                                                       
         BAS   RE,RIDFMT                                                        
*                                                                               
* SIGN ON ID'S                                                                  
         LA    R2,FMSSIDH                                                       
         BAS   RE,SOFMT                                                         
*                                                                               
* PRIMARY AFFILIATE                                                             
         LA    R2,FMSAFH                                                        
         MVC   8(L'RSTAAFFL,R2),RSTAAFFL                                        
         FOUT  (R2)                                                             
*                                                                               
* DISPLAY ALL DATA FROM X'08' ELEM                                              
         BAS   RE,OPTDIS                                                        
*                                                                               
         GOTO1 =A(DISPCOMS),DMCB,(RC),RR=Y                                      
* RADIO COMBOS                                                                  
         FOUT  FMSCMH,SPACES,8                                                  
         FOUT  FMSCM1H,SPACES,8    CLEAR/XMIT COMBO STATION FIELDS              
         FOUT  FMSCM2H,SPACES,8                                                 
         FOUT  FMSCM3H,SPACES,8                                                 
         FOUT  FMSCM4H,SPACES,8                                                 
         FOUT  FMSDC1H,SPACES,8    CLEAR/XMIT COMBO STATION DATES               
         FOUT  FMSDC2H,SPACES,8                                                 
         FOUT  FMSDC3H,SPACES,8                                                 
         FOUT  FMSDC4H,SPACES,8                                                 
         GOTO1 VGETEL,DMCB,(X'0A',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          NO EXTRA EXTENDED ELEMENT                    
         BE    DISS0300                                                         
         L     R5,DMCB+8                                                        
         USING RSTACSEL,R5                                                      
*                                                                               
         CLI   REC+26,C'C'         IS THIS A COMBO STA?                         
         BE    DISS0200            YES - DISPLAY UP TO 4 CHILDREN               
*                                                                               
         MVC   FMSCM(4),RSTACS     NO  - DISPLAY 1 PARENT                       
         MVC   FMSCM+4(2),=C'-C'                                                
         OI    FMSCMH+6,X'80'      XMIT FLD                                     
         B     DISS0300                                                         
DISS0200 DS    0H                                                               
         LA    R2,FMSCM1H          FIRST COMBO FIELD                            
         LA    R3,FMSDC1H          FIRST COMBO DATE                             
DISS0220 MVC   8(4,R2),RSTACS                                                   
         MVI   8+4(R2),C'-'                                                     
         MVC   8+5(1,R2),RSTACS+4                                               
         CLI   RSTACPRF,C'*'       PREFERRED STATION?                           
         BNE   DISS0240            NO                                           
         MVI   8+6(R2),C'*'        YES - SET FLAG                               
DISS0240 EQU   *                                                                
         CLI   RSTACPRF,C'-'       MINUSED STATION?                             
         BNE   DISS0260            NO                                           
         MVI   8+6(R2),C'-'        YES - SET FLAG                               
DISS0260 EQU   *                                                                
         LR    RF,RA               CHECK FOR DDS TERMINAL                       
         USING TWAD,RF                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
*                                                                               
         DROP  RF                                                               
*                                                                               
         BNE   DISS0280            NO                                           
         CLI   1(R5),8             CHECK L(COMBO STATION ELEMENT)               
         BNH   DISS0280            NO DATE IN ELEMENT                           
         OC    8(2,R5),8(R5)       ANY DATE IN ELEMENT?                         
         BZ    DISS0280            NO                                           
         GOTO1 VDATCON,DMCB,(2,RSTACDTE),(5,8(R3))                              
DISS0280 EQU   *                                                                
         FOUT  (R2)                                                             
         FOUT  (R3)                                                             
*                                                                               
         ZIC   R0,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,R0                                                            
         CLI   0(R5),X'0A'         COMBO STATION ELEMENT?                       
         BNE   DISS0300            NO  -                                        
         BAS   RE,NEXTUF           YES - GET NEXT UNPROTECTED FIELD             
         ZIC   R0,0(R3)            BUMP TO NEXT DATE FIELD                      
         AR    R3,R0                  ADD LENGTH OF SCREEN FIELD!               
         B     DISS0220            GO BACK FOR NEXT                             
*                                                                               
         DROP  R5                                                               
DISS0300 DS    0H                                                               
*                                                                               
* TWX FIELD                                                                     
         LA    R2,FMSTWXH                                                       
         MVC   8(L'FMSTWX,R2),SPACES                                            
         FOUT  (R2)                                                             
         GOTO1 VGETEL,DMCB,(X'07',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    DISS0320                                                         
*                                                                               
         L     R5,DMCB+8                                                        
         USING RSTATWXL,R5                                                      
         MVC   8(20,R2),RSTATWX                                                 
         DROP  R5                                                               
*                                                                               
DISS0320 DS    0H                                                               
*                                                                               
* LAST ACTIVITY DATE FIELD                                                      
*                                                                               
         LA    R2,FMSLADH                                                       
         MVC   8(L'FMSLAD,R2),SPACES                                            
         FOUT  (R2)                                                             
         GOTO1 VGETEL,DMCB,(X'F1',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    DISS0330                                                         
*                                                                               
         L     R5,DMCB+8                                                        
         USING RSTACTEL,R5                                                      
         GOTO1 VDATCON,DMCB,(3,RSTACTCD),(11,FMSLAD)                            
         DROP  R5                                                               
*                                                                               
DISS0330 DS    0H                                                               
*                                                                               
* UNIQUE ID FIELD                                                               
*                                                                               
         MVC   FMSLIC,SPACES                                                    
         FOUT  FMSLICH             CLEAR LIABILITY FIELD                        
         LA    R2,FMSUIDH                                                       
         MVC   8(L'FMSUID,R2),SPACES                                            
         FOUT  (R2)                                                             
         GOTO1 VGETEL,DMCB,(X'2A',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    DISS0340                                                         
*                                                                               
         L     R5,DMCB+8                                                        
         USING RSTAUIEL,R5                                                      
         MVC   FMSUID(6),RSTAUIST                                               
         OI    FMSUIDH+4,X'20'     TURN ON PREVIOUSLY VALIDATED                 
         FOUT  (R2)                                                             
         DROP  R5                                                               
         LA    R4,REC2                                                          
         USING CT99RECD,R4                                                      
         XC    0(25,R4),0(R4)      BUILD CONTROL FILE KEY                       
         MVI   CT99KTYP,X'99'                                                   
         MVC   CT99KUID,FMSUID                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R4),(R4),0                  
         CLI   DMCB+8,0                                                         
         BE    DISS0338            MUST BE ON FILE                              
         MVC   FMSLIC+14(08),=C'UID MKT='                                       
         MVC   FMSLIC+22(17),=C'NO LONGER ON FILE'                              
         B     DISS0340                                                         
DISS0338 EQU   *                                                                
         MVC   FMSLIC+14(08),=C'UID MKT='                                       
         LA    RF,CT99DATA                                                      
         USING CRCLD,RF                                                         
         MVC   FMSLIC+22(26),CRCLCTY                                            
         DROP  R4,RF                                                            
*                                                                               
DISS0340 DS    0H                                                               
*                                                                               
* LIABILITY FIELD                                                               
         LA    R2,FMSLIBH                                                       
         MVC   8(L'FMSLIB,R2),SPACES                                            
         FOUT  (R2)                                                             
         OC    RSTALIAB,RSTALIAB   ANY LIABILITY WITH STATION?                  
         BNZ   DISS0342            YES - CAN'T SHOW UID MKT                     
         CLC   FMSLIC+14(08),=C'UID MKT='                                       
*                                  LIABILITY EXPAND USED FOR UNIQUE ID?         
         BE    DISS0345            YES - DON'T CLEAR IT OUT                     
DISS0342 EQU   *                                                                
         MVC   FMSLIC(L'FMSLIC),SPACES                                          
         FOUT  FMSLICH                                                          
         OC    RSTALIAB,RSTALIAB                                                
*        BZ    DISS0360                                                         
         BZ    DISS0345                                                         
         EDIT  (B1,RSTALIAB),(2,8(R2)),FILL=0                                   
*                                                                               
         MVC   FMSLIC(L'FMSLIC),SPACES                                          
         FOUT  FMSLICH                                                          
         XC    MYIOAREA(L'RCMTKCDE),MYIOAREA                                    
         MVC   MYIOAREA(4),=C'LIAB'                                             
         MVC   MYIOAREA+4(2),8(R2)                                              
         GOTO1 =A(DISPFC),DMCB,(RC),(RA),MYIOAREA,(L'FMSLIC,FMSLIC),   X        
               RR=Y                                                             
         FOUT  FMSLICH             XMIT                                         
*                                                                               
*                                                                               
* EMAIL ADDRESS FIELD                                                           
*        FOUT  (R2)                                                             
DISS0345 EQU   *                                                                
*                                                                               
         MVC   FMSEM1(L'FMSEM1),SPACES                                          
*                                       CLEAR PREVIOUS VALUE OF FIELD           
         XC    HALF2,HALF2         CLEAR COUNTER                                
         LA    R5,FMSEM1                SET A(E-MAIL ADDRESSES)                 
         GOTO1 VGETEL,DMCB,(X'25',REC),DMCB+8                                   
         L     R3,DMCB+8                SET A(ELEMENT ADDRESS)                  
         CLI   DMCB,X'FF'               SEPARATE TEST - LEAVE                   
         BE    DISS0360                                                         
         B     DISS0354                                                         
DISS0350 EQU   *                                                                
         ZIC   RF,1(R3)                 BUMP TO NEXT ELEMENT                    
         AR    R3,RF                                                            
         CLI   0(R3),X'25'              NEXT 25 ELEMENT?                        
         BNE   DISS0358                 NO  - FINISHED                          
         CLI   HALF2,3             THREE EMAIL ADDRS SHOWN?                     
         BE    DISS0358            YES - DON'T SHOW MORE                        
         MVI   0(R5),C','               INSERT SEPARATOR                        
         LA    R5,1(R5)                 BUMP TO NEXT POSITION                   
*                                                                               
DISS0354 EQU   *                                                                
         USING RSTAEML,R3                                                       
*                                                                               
         ZIC   R2,RSTAEMLN         ELEMENT LENGTH                               
         SHI   R2,4                MINUS CODE+LENGTH+FLAG+EX                    
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),RSTAADD          MOVE EMAIL ADDRESS BY LEN               
         AR    R5,R2                    BUMP BY LENGTH                          
         LA    R5,1(R5)                 BUMP TO NEXT SPACE                      
         ZIC   RF,HALF2            INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         STC   RF,HALF2                                                         
         B     DISS0350                 GO BACK FOR NEXT ELEMENT                
                                                                                
DISS0358 EQU   *                                                                
**       MVI   FMSEM2,C'N'                                                      
**       TM    RSTAFLG,RSTAFLGQ                                                 
**       BZ    *+8                                                              
**       MVI   FMSEM2,C'Y'                                                      
         FOUT  FMSEM1H                                                          
**       FOUT  FMSEM2H                                                          
         DROP  R3                                                               
         B     STADX                                                            
*                                                                               
*                                                                               
DISS0360 DS    0H                                                               
**       MVI   FMSEM2,C' '                                                      
**       FOUT  FMSEM2H                                                          
         MVC   FMSEM1(L'FMSEM1),SPACES                                          
         FOUT  FMSEM1H                                                          
*                                                                               
STADX    DS    0H                                                               
*                                                                               
* DISPLAY ALL DATA FROM X'0F' STATION ID NUMBER ELEM                            
*        ABOB ADDED                                                             
*                                                                               
         BAS   RE,SIDNDIS                                                       
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*--------------------------------------------------------------------           
* SUB-ROUTINE TO DISPLAY STATUS FIELD ON STATION SCREEN                         
*--------------------------------------------------------------------           
UNSCAN   NTR1                                                                   
         MVC   WORK3(60),SPACES    PRECLEAR FIELD FOR UNSCAN                    
         LA    R7,WORK3                                                         
         SR    R3,R3               COUNT OF FIELDS                              
         SPACE                                                                  
         TM    RSTASTAT,X'80'                                                   
         BZ    US10                                                             
         MVC   0(3,R7),=C'BOP'                                                  
         MVI   10(R7),C'N'                                                      
         LA    R7,20(R7)                                                        
         LA    R3,1(R3)            INCREMENT FIELD COUNT                        
         SPACE                                                                  
US10     EQU   *                                                                
         TM    RSTASTAT,X'40'                                                   
         BZ    US20                                                             
         MVC   0(8,R7),=C'CONTRACT'                                             
         MVI   10(R7),C'N'                                                      
         LA    R7,20(R7)                                                        
         LA    R3,1(R3)                                                         
         SPACE                                                                  
US20     DS    0H                                                               
         TM    RSTASTAT,X'20'                                                   
         B     US30                AVAILS=NEW IS SKIPPED!!                      
***>>>   BZ    US30                                                             
         MVC   0(8,R7),=CL8'AVAILS'                                             
         MVC   10(3,R7),=C'NEW'                                                 
         LA    R7,20(R7)                                                        
         LA    R3,1(R3)                                                         
         SPACE                                                                  
US30     DS    0H                                                               
         TM    RSTASTAT,X'10'                                                   
         BZ    USX                                                              
         MVC   0(8,R7),=CL8'BUDGET'                                             
         MVC   10(3,R7),=C'YES'                                                 
         LA    R7,20(R7)                                                        
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
USX      EQU   *                                                                
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
         GOTO1 CUNSCAN,DMCB,((R3),WORK3),(R2),0                                 
         B     STADX                                                            
         DROP  R7                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
*   SUBROUTINE TO FORMAT RECEIVING ID FIELD                                     
*--------------------------------------------------------------------           
RIDFMT   NTR1                                                                   
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         FOUT  (R2)                                                             
         SPACE 1                                                                
         GOTO1 VGETEL,DMCB,(X'05',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    RIDFMTX             NO EXTRA ELEMENT                             
         SPACE 1                                                                
         L     R5,DMCB+8                                                        
         USING RSTAXEL,R5                                                       
         MVC   8(L'RSTARSO,R2),RSTARSO                                          
         FOUT  (R2)                                                             
RIDFMTX  B     STADX                                                            
         DROP  R5                                                               
         EJECT                                                                  
*--------------------------------------------------------------------           
*  SUBROUTING TO FORMAT SIGN-ON ID FIELD                                        
*--------------------------------------------------------------------           
SOFMT    NTR1                                                                   
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         FOUT  (R2)                                                             
         SPACE 1                                                                
         XC    TEMPIDS(40),TEMPIDS                                              
         LA    R3,TEMPIDS                                                       
         LA    R4,RSTAELEM                                                      
         MVI   BYTE,6              LOOK FOR SIGN-ON ID ELEMENTS                 
SOF10    BAS   RE,NEXTEL                                                        
         BNE   SOF40                                                            
         SPACE 1                                                                
         USING RSTASOEL,R4                                                      
         MVC   0(L'RSTASO,R3),RSTASO                                            
         LA    R3,7(R3)                                                         
SOF20    CLI   0(R3),C' '          GET RID OF EXTRA SPACES AT THE END           
         BNE   SOF30               OF SIGN ON ID                                
         BCTR  R3,0                                                             
         B     SOF20                                                            
         SPACE 1                                                                
SOF30    EQU   *                                                                
         LA    R3,1(R3)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         B     SOF10                                                            
         SPACE 1                                                                
SOF40    BCTR  R3,0                                                             
         CLI   0(R3),C','                                                       
         BNE   *+8                                                              
         MVI   0(R3),C' '                                                       
         MVC   8(L'FMSSID,R2),TEMPIDS                                           
         FOUT  (R2)                                                             
         B     STADX                                                            
         DROP  R4                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*        OPTDIS - DISPLAY ALL FIELDS ON THE STATION RECORD                      
*                 EXTENDED DESCRIPTION ELEMENT                                  
*---------------------------------------------------------------------          
OPTDIS   NTR1                                                                   
         FOUT  FMSOPTH,=C'NNNNNNNNNNNNNNNNN',17  OPTIONS                        
         FOUT  FMSRDSH,SPACES,19              DESTINATION ID                    
         FOUT  FMSDMCH,SPACES,5               DEMO MARKET CODE                  
         FOUT  FMSFAXH,SPACES,19              FAX NUMBER                        
         FOUT  FMSINTH,SPACES,12              A/R INTERFACE CODE                
         FOUT  FMSRWSH,SPACES,2               DEST FMT                          
         FOUT  FMSMCDH,SPACES,4               MKT CODE                          
         ZIC   RF,FMSMCDH                     MKT CD EXPANSION FLD              
         LA    R2,FMSMCDH(RF)                                                   
         FOUT  (R2),SPACES,20                                                   
         FOUT  FMSAF2H,SPACES,3               SECONDARY AFFL                    
         FOUT  FMSTZH,SPACES,2                TIME ZONE                         
         FOUT  FMSECH,=C'NO ',3               ELECTRONIC CON                    
         FOUT  FMSINVH,=C'NO ',3              INVOICE                           
         FOUT  FMSLUH,SPACES,8                LUID                              
*                                                                               
*                                  RETRIEVE DEMO MARKET CODE ELEMENT            
         GOTO1 VGETEL,DMCB,(X'11',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          NO DEMO MARKET CODE ELEMENT                  
         BE    ODIS0060                                                         
         L     R5,DMCB+8                                                        
         USING RSTADMEL,R5                                                      
*                                                                               
         CLI   RSTADMCD,0          ANYTHING IN FIRST BYTE?                      
         BZ    ODIS0020            NO  - EXPAND NUMERIC VALUE IN FIELD          
*                                  YES - ALPHANUMERIC VALUE IN 3 CHARS          
         MVC   FMSDMC(3),RSTADMCD                                               
         B     ODIS0040                                                         
ODIS0020 EQU   *                                                                
         EDIT  RSTADMCD,(5,FMSDMC),ALIGN=LEFT                                   
         FOUT  FMSDMCH                                                          
*                                                                               
         DROP  R5                                                               
*                                                                               
ODIS0040 EQU   *                                                                
*                                                                               
ODIS0060 EQU   *                                                                
         GOTO1 VGETEL,DMCB,(X'08',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          NO EXTRA EXTENDED ELEMENT                    
         BE    ODIS0800                                                         
         L     R5,DMCB+8                                                        
         USING RSTAXXEL,R5                                                      
*                                                                               
         MVC   FMSOPT(9),RSTAOPTS     OPTIONS                                   
         LA    R3,FMSOPT+9                                                      
         LA    R4,RSTAOPTA                                                      
         BAS   RE,DOPROF                                                        
         LA    R3,FMSOPT+17                                                     
         LA    R4,RSTAOPTB                                                      
         BAS   RE,DOPROF                                                        
*                                                                               
* IF THERE IS A 2ND FAX #, WE WILL OVERWIRTE THE DESTINATION FIELD              
* WITH THE 2ND FAX NUMBER                                                       
         MVC   FMSRDS(L'RSTAORDS),RSTAORDS  DESTINATION ID                      
         OC    RSTAOFX2,RSTAOFX2   DO WE HAVE A SECOND FAX #?                   
         BZ    ODIS0120            NO, SKIP                                     
         CLI   RSTAOFX2,X'00'      FIRST BYTE EMPTY?                            
         BNE   ODIS0100            NO  - JUST PUT OUT WHAT'S THERE              
         CLI   RSTAOFX2+1,X'00'    ANYTHING IN SECOND BYTE?                     
         BE    ODIS0100            NO  - JUST PUT OUT WHAT'S THERE              
         ZIC   R3,RSTAOFX2+1       INTERNATIONAL CODE                           
         EDIT  (R3),(3,FMSRDS),FILL=0                                           
         UNPK  WORK(16),RSTAOFX2+3(8)                                           
         ZIC   RF,RSTAOFX2+2       LENGTH OF SIGNIFICANT DIGITS                 
         LA    RE,16               MAXIMUM LENGTH OF FIELD                      
         SR    RE,RF               GET SIGNIFICANT OFFSET                       
         LA    RF,WORK             A(UNPACKED NUMBER)                           
         AR    RF,RE               ADD OFFSET                                   
         ZIC   RE,RSTAOFX2+2       GET LENGTH OF FAX# FIELD AGAIN               
         BCTR  RE,0                DECREMENT FOR EX                             
         EX    RE,ODIS0080         MOVE BY LENGTH                               
         B     ODIS0120                                                         
ODIS0080 MVC   FMSRDS+3(0),0(RF)                                                
*                                                                               
ODIS0100 EQU   *                                                                
         MVC   FMSRDS(L'RSTAOFX2),RSTAOFX2     FAX NUMBER                       
ODIS0120 DS    0H                                                               
         CLI   RSTAOFAX,X'00'      FIRST BYTE EMPTY?                            
         BNE   ODIS0160            NO  - JUST PUT OUT WHAT'S THERE              
         CLI   RSTAOFAX+1,X'00'    ANYTHING IN SECOND BYTE?                     
         BE    ODIS0160            NO  - JUST PUT OUT WHAT'S THERE              
         ZIC   R3,RSTAOFAX+1       INTERNATIONAL CODE                           
         EDIT  (R3),(3,FMSFAX),FILL=0                                           
         UNPK  WORK(16),RSTAOFAX+3(8)                                           
         ZIC   RF,RSTAOFAX+2       LENGTH OF SIGNIFICANT DIGITS                 
         LA    RE,16               MAXIMUM LENGTH OF FIELD                      
         SR    RE,RF               GET SIGNIFICANT OFFSET                       
         LA    RF,WORK             A(UNPACKED NUMBER)                           
         AR    RF,RE               ADD OFFSET                                   
         ZIC   RE,RSTAOFAX+2       GET LENGTH OF FAX# FIELD AGAIN               
         BCTR  RE,0                DECREMENT FOR EX                             
         EX    RE,ODIS0140         MOVE BY LENGTH                               
         B     ODIS0180                                                         
ODIS0140 MVC   FMSFAX+3(0),0(RF)                                                
*                                                                               
ODIS0160 EQU   *                                                                
         MVC   FMSFAX(L'RSTAOFAX),RSTAOFAX     FAX NUMBER                       
ODIS0180 EQU   *                                                                
         MVC   FMSINT(L'RSTAOSI),RSTAOSI       A/R INTERFACE CODE               
         MVC   FMSRWS,RSTARWS                  FMT AND CONTRACT FILTER          
*                                                                               
* MARKET CODE                                                                   
         FOUT  FMSMCDH,SPACES,4    MKT CODE                                     
         ZIC   RF,FMSMCDH                                                       
         LA    R3,FMSMCDH(RF)                                                   
         CLC   RSTAMKTC(4),SPACES                                               
         BE    ODIS0200                                                         
         OC    RSTAMKTC(4),RSTAMKTC                                             
         BZ    ODIS0200                                                         
         MVC   FMSMCD,RSTAMKTC                                                  
         BAS   RE,DISPMKT                                                       
ODIS0200 EQU   *                                                                
         FOUT  (R3)                                                             
*                                                                               
         MVC   FMSAF2,RSTAAFL2                 SECONDARY AFFILIATE              
         MVC   FMSTZ,RSTATZ                    TIME ZONE                        
*                                                                               
         TM    RSTAXOPT,X'80'      ELECTRONIC CON                               
         BZ    ODIS0220            NO                                           
         MVC   FMSEC,=C'YES'                                                    
         TM    RSTAXOPT,X'10'      LOCAL E/C?                                   
         BZ    ODIS0220            NO                                           
         MVC   FMSEC,=C'LOC'       YES                                          
ODIS0220 EQU   *                                                                
*                                                                               
         TM    RSTAXOPT,X'40'                  INVOICE                          
         BZ    *+10                                                             
         MVC   FMSINV,=C'YES'                                                   
*                                                                               
         MVC   FMSLU,RSTALUID                  LUID                             
*                                                                               
ODIS0800 B     STADX                                                            
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO DISPLAY PROFILE BITS                                               
* NOTE: R4 MUST ADDRESS BYTE TO BE DISPLAYED (8 OPTION BITS/BYTE)               
*       R3 MUST ADDRESS 8-BYTE DISPLAY AREA ON SCREEN                           
*                                                                               
DOPROF   NTR1                                                                   
         MVC   0(8,R3),=C'NNNNNNNN'                                             
         TM    0(R4),X'80'                                                      
         BZ    *+8                                                              
         MVI   0(R3),C'Y'                                                       
         TM    0(R4),X'40'                                                      
         BZ    *+8                                                              
         MVI   1(R3),C'Y'                                                       
         TM    0(R4),X'20'                                                      
         BZ    *+8                                                              
         MVI   2(R3),C'Y'                                                       
         TM    0(R4),X'10'                                                      
         BZ    *+8                                                              
         MVI   3(R3),C'Y'                                                       
         TM    0(R4),X'08'                                                      
         BZ    *+8                                                              
         MVI   4(R3),C'Y'                                                       
         TM    0(R4),X'04'                                                      
         BZ    *+8                                                              
         MVI   5(R3),C'Y'                                                       
         TM    0(R4),X'02'                                                      
         BZ    *+8                                                              
         MVI   6(R3),C'Y'                                                       
         TM    0(R4),X'01'                                                      
         BZ    *+8                                                              
         MVI   7(R3),C'Y'                                                       
DOPROFX  B     STADX                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*      SIDNDIS-SUBROUTINE FINDS AND DISPLAYS X'0F' STATION ID NUMBER*           
*              ABOB ADDED                                           *           
*********************************************************************           
SIDNDIS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         FOUT  FMSSTNH,SPACES,8   CLEAR STATION ID FIELD                        
*                                                                               
*GET X'0F' ELEMENT                                                              
         GOTO1 VGETEL,DMCB,(X'0F',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          NO STATION ID NUMBER ELEMENT                 
         BE    SDNDEXT                                                          
         L     R5,DMCB+8                                                        
         USING RSTAINEL,R5                                                      
         ICM   R3,15,RSTAINID    LOAD BINARY ST ID NUMBER                       
*                                                                               
         DROP  R5                                                               
*                                                                               
*CONVERT FROM BINARY TO PRINTABLE CHAR STATION ID NUMBER                        
         EDIT  (R3),FMSSTN,ALIGN=LEFT                                           
*                                                                               
         FOUT  FMSSTNH           DISPLAY STATION ID FIELD                       
*                                                                               
SDNDEXT  DS    0H                EXIT SUBROUTINE                                
         B     EXXMOD                                                           
*      SIDNDIS SUBROUTINE END                                                   
*********************************************************************           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* EDIT COMPETING STATIONS/AFFILIATES                                            
*********************************************************************           
         DS    0D                                                               
EDITCOMP NTR1  BASE=*,LABEL=*                                                   
*                                  GET RECORD AND DELETE ALL                    
         BAS   RE,GETREC           COMPETING STA ELS.                           
         GOTO1 VDELELEM,DMCB,(2,REC)                                            
         LA    R7,RSTAMKEL                                                      
         MVI   BYTE4,X'00'         COUNT COMPETING STATIONS                     
         MVI   BYTE3,X'00'         COUNT COMPETING IND STATIONS                 
         ST    R2,FULL             SAVE A(FIELD BEFORE COMP STA)                
         LA    R2,FSCCPPH                                                       
         SPACE 1                                                                
CSE20    BAS   RE,NEXTUF                                                        
         CLI   0(R2),9                                                          
         BE    CSE50                                                            
         LA    R0,FSCLSTH                                                       
         CR    R0,R2                                                            
         BE    CSE50                                                            
         CLI   5(R2),0                                                          
         BE    CSE20                                                            
         XC    REC2(15),REC2                                                    
         MVC   REC2(2),=X'020F'                                                 
         MVC   REC2+2(8),SPACES                                                 
*                                                                               
         LA    R1,REC2+2                                                        
         LA    R3,INVERR                                                        
         LA    R4,8(R2)                                                         
         LA    R5,4                                                             
         BAS   RE,TESTAN           TEST AND MOVE FIELD                          
         TM    BYTE,X'80'          ERROR                                        
         BO    CSEERR                                                           
         TM    BYTE,X'04'          ALPHA                                        
         BZ    CSEERR                                                           
         CH    R5,=H'3'            AT LEAST 3 CHARS                             
         BL    CSEERR                                                           
         CLI   0(R4),C'-'          CHECK FOR - TO NAME BAND                     
         BNE   CSEERR                                                           
         MVC   REC2+6(1),1(R4)     BAND                                         
         CLC   1(2,R4),=C'A='                                                   
         BE    CSE30                                                            
         CLC   1(2,R4),=C'F='                                                   
         BE    CSE30                                                            
         CLC   1(2,R4),=C'C='      COMBO STATION                                
         BE    CSE30                                                            
         CLI   1(R4),C'A'                                                       
         BE    CSE30                                                            
         CLI   1(R4),C'F'                                                       
         BE    CSE30                                                            
         CLI   1(R4),C'C'          COMBO STATION                                
         BE    CSE30                                                            
         CLI   1(R4),C'N'          RADIO NETWORK                                
         BE    CSE30                                                            
         CLC   1(2,R4),=C'T='                                                   
         BNE   CSEERR                                                           
         MVI   REC2+6,C' '                                                      
         SPACE 1                                                                
CSE30    LA    R4,3(R4)                                                         
         MVC   REC2+7(3),0(R4)     AFFILIATION                                  
         OC    REC2+7(3),=CL3' '                                                
         CLC   REC2+7(3),=CL3' '                                                
         BNE   CSE40               IF RADIO, ANY INPUT OR BLANK IS OK           
         CLI   BKEY+26,C' '        IF TV, MUST PUT IN SOMETHING                 
         BE    CSEERR                                                           
         CLI   BKEY+26,C'L'        IF TV, MUST PUT IN SOMETHING                 
         BE    CSEERR                                                           
CSE40    BAS   RE,DUPCK                                                         
         LTR   R3,R3                                                            
         BNZ   CSEERR                                                           
*                                                                               
         GOTO1 VRECUP,DMCB,(2,REC),REC2,(R7)                                    
         SR    R5,R5                                                            
         IC    R5,REC2+1                                                        
         AR    R7,R5                                                            
         B     CSE20                                                            
*                                                                               
CSE50    MVI   BYTE3,X'00'                                                      
         CLI   BKEY+26,C'L'        FOR TV LOW POWER                             
         BE    CSE51                                                            
         CLI   BKEY+26,C' '        FOR TV                                       
         BNE   STA50                                                            
CSE51    EQU   *                                                                
         CLC   RSTAAFFL,=C'ABC'    MUST INCLUDE STATION                         
         BE    STA50               IN COUNT OF IND STATIONS                     
         CLC   RSTAAFFL,=C'CBS'                                                 
         BE    STA50                                                            
         CLC   RSTAAFFL,=C'NBC'                                                 
         BE    STA50                                                            
         LA    R3,MANYIND                                                       
         SR    RE,RE                                                            
         IC    RE,BYTE3                                                         
         AH    RE,=H'1'                                                         
         STC   RE,BYTE3                                                         
         CH    RE,=H'3'            MAX OF 3 FOR TV                              
         B     STA50               MAX TEST KILLED...                           
****>>>  BNH   STA50                                                            
****>>>  LA    R2,LFMLAST          POINT TO FIRST TITLE                         
****>>>  BAS   RE,NEXTUF           AND THEN TO 1ST UNPROTECTED FIELD            
****>>>  B     ERROR                                                            
         SPACE 1                                                                
STA50    CLI   BACT,C'A'           ADD                                          
         BE    STA100              OK TO ADDREC                                 
*                                                                               
*   TEMPORARY:  ALLOW PAXSON TO CHANGE COMPETITIVE STATIONS                     
*                                                                               
*        CLC   =C'PQ',REPALPHA     PAXSON?                                      
*        BE    STA100              YES - DON'T COMPARE OLD/NEW                  
*                                                                               
*   TEMPORARY:  REMOVE WHEN FINISHED.                                           
*                                                                               
         SPACE 1                                                                
         CLI   BKEY+26,C'L'        DO NOT CHECK RADIO                           
         BE    STA51               FOR DELETED STATIONS                         
         CLI   BKEY+26,C' '        DO NOT CHECK RADIO                           
         BNE   STA100              FOR DELETED STATIONS                         
STA51    EQU   *                                                                
         MVC   KEY,BKEY                                                         
         MVC   KEY+28(4),BSVDA                                                  
         LA    R2,REC2                                                          
         ST    R2,AIOAREA                                                       
         BAS   RE,GETREC           GET OLD REC IN REC2                          
         LA    R2,REC                                                           
         ST    R2,AIOAREA                                                       
         SPACE 1                                                                
         L     R2,FULL                                                          
         XR    R5,R5                                                            
         LA    R6,REC2+34                                                       
STA60    IC    R5,1(R6)                                                         
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R5                                                            
         CLI   0(R6),0                                                          
         BE    STA100              OK TO PUTREC                                 
         CLI   0(R6),2                                                          
         BNE   STA60                                                            
         BAS   RE,NEXTUF                                                        
         SPACE 1                                                                
         MVI   BYTE,2                                                           
         LA    R4,RSTAELEM         CHECK TO NEW REC                             
STA70    BAS   RE,NEXTEL                                                        
         BNE   STA200                                                           
         CLC   2(5,R6),2(R4)       IS OLD COMPETING ON NEW REC                  
         BNE   STA70                                                            
         B     STA60               GET NEXT  OLD                                
         SPACE 1                                                                
STA100   SR    R2,R2                                                            
         IC    R2,BYTE4            NUMBER OF COMPETING                          
         CH    R2,=H'2'                                                         
         BL    STA110                                                           
         GOTO1 =V(XSORT),DMCB,(0,RSTAMKEL),(R2),15,5,2,RR=YES                   
STA110   CR    RB,RB                                                            
         B     STA300                                                           
*                                                                               
STA200   DS    0H                                                               
         ST    R2,DUB                                                           
         MVC   DUB+4,=A(-4)                                                     
         B     STA290                                                           
CSEERR   DS    0H                                                               
         ST    R2,DUB                                                           
         ST    R3,DUB+4                                                         
         B     STA290                                                           
STA290   DS    0H                                                               
         LTR   RB,RB                                                            
STA300   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*----------------------------------------------------------------               
*              CHECK FOR DUPLICATE COMPETITIVES                                 
*----------------------------------------------------------------               
DUPCK    NTR1                                                                   
         LA    R3,DUPSTERR                                                      
         CLC   REC2+2(5),BKEY+22   SAME STATION AND BAND                        
         BE    DUPERX                                                           
         SPACE 1                                                                
DUP10    LA    R4,RSTAELEM                                                      
         MVI   BYTE,2                                                           
         SPACE 1                                                                
DUP20    BAS   RE,NEXTEL                                                        
         BNE   DUP30                                                            
         CLC   REC2+2(5),2(R4)                                                  
         BE    DUPERX                                                           
         SPACE 1                                                                
         B     DUP20                                                            
         SPACE 1                                                                
DUP30    LA    R3,INVERR                                                        
         CLI   BKEY+26,C'L'        KEY IS LOW POWER TV                          
         BE    DUP40                                                            
         CLI   BKEY+26,C' '        KEY IS TV                                    
         BNE   *+16                                                             
         CLI   REC2+6,C' '         COMPETING MUST BE TV                         
         BNE   DUPERX                                                           
         B     DUP40                                                            
         CLI   REC2+6,C' '         KEY IS NOT TV                                
         BE    DUPERX              COMPETING IS TV                              
         SPACE 1                                                                
DUP40    SR    R4,R4                                                            
         IC    R4,BYTE4            COUNT TOTAL COMPETING                        
         AH    R4,=H'1'                                                         
         STC   R4,BYTE4                                                         
         CLI   BKEY+26,C'L'        LOW POWER TV                                 
         BE    DUP41                                                            
         CLI   BKEY+26,C' '        TV                                           
         BNE   DUPX                                                             
DUP41    EQU   *                                                                
         CH    R4,=H'11'           MAX 11 COMPETITIVES FOR TV                   
         BH    DUPERX                                                           
         SPACE 1                                                                
         CLC   REC2+7(3),=C'ABC'   AND COUNT 'IND' COMPETING                    
         BE    DUPX                                                             
         CLC   REC2+7(3),=C'CBS'                                                
         BE    DUPX                                                             
         CLC   REC2+7(3),=C'NBC'                                                
         BE    DUPX                                                             
         LA    R3,MANYIND                                                       
         SR    R4,R4                                                            
         IC    R4,BYTE3                                                         
         AH    R4,=H'1'                                                         
         STC   R4,BYTE3                                                         
         CLI   BKEY+26,C'L'        LOW POWER TV                                 
         BE    DUP42                                                            
         CLI   BKEY+26,C' '        TV                                           
         BNE   *+12                                                             
DUP42    EQU   *                                                                
         CH    R4,=H'3'            MAX OF 3 FOR TV                              
****>>>  BH    DUPERX              TEST KILLED....                              
         SPACE 1                                                                
DUPX     SR    R3,R3                                                            
DUPERX   XIT1  REGS=(R3)                                                        
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* EDIT COMPETING STATIONS/AFFILIATES                                            
*********************************************************************           
***      DS    0D                                                               
***TCTRL NTR1  BASE=*,LABEL=*                                                   
***      BAS   RE,GETREC                                                        
***      B     EXXMOD                                                           
***      LTORG                                                                  
***      EJECT                                                                  
***********************************************************************         
* ROUTINE TO EDIT STATION DATES INFORMATION                                     
*                                                                               
***********************************************************************         
         DS    0D                                                               
EDITDATE NTR1  BASE=*,LABEL=*                                                   
         XC    DUB,DUB             CLEAR RETURN FLAG FIELD                      
         LA    R2,LFMKEYH                                                       
         ST    R2,DUB              SET POSSIBLE ERROR CURSOR POS                
         MVC   KEYSAVE2(27),KEY                                                 
         BAS   RE,GETEOM           RETRIEVE END OF MONTH DATE                   
***>>>   BNZ   STAB0720            ERROR RETURN:  NO EOM RECORD                 
*                                                                               
*   IF NO EOM RECORD WAS FOUND, 'TOTREQ' WILL CONTAIN X'01'                     
*        'TOTREQ' IS USED FOR FLAG PURPOSES ONLY WITHIN THIS MODULE.            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),KEYSAVE2                                                 
         GOTO1 HIGH                RESTORE THE KEY                              
*                                                                               
         BAS   RE,GETREC           RETRIEVE RECORD                              
*                                                                               
         GOTO1 VGETEL,DMCB,(X'0D',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          NO X'0D' JOIN/LEAVE REPORT ELT               
         BE    STAB0001                                                         
         L     RF,DMCB+8           SET A(ELEMENT FOUND)                         
         ZIC   RE,1(RF)            GET LENGTH OF ELEMENT                        
         BCTR  RE,0                BACK DOWN 1 FOR EX                           
         EX    RE,*+8              MOVE X'0D' ELT BY LENGTH                     
         B     *+10                                                             
         MVC   NEW0DELT(0),0(RF)                                                
         GOTO1 VDELELEM,DMCB,(X'0D',REC)                                        
*                                  DELETE JOIN/LEAVE REPORT ELT                 
STAB0001 GOTO1 VGETEL,DMCB,(X'10',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          NO X'10' CONTRACT END DATE ELT               
         BE    STAB0002                                                         
         L     RF,DMCB+8           SET A(ELEMENT FOUND)                         
         ZIC   RE,1(RF)            GET LENGTH OF ELEMENT                        
         BCTR  RE,0                BACK DOWN 1 FOR EX                           
         EX    RE,*+8              MOVE X'10' ELT BY LENGTH                     
         B     *+10                                                             
         MVC   NEW10ELT(0),0(RF)                                                
         GOTO1 VDELELEM,DMCB,(X'10',REC)                                        
*                                  DELETE CONTRACT END DATE ELT                 
STAB0002 EQU   *                                                                
         GOTO1 VGETEL,DMCB,(X'23',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          NO X'23' CLOSEOUT ACTIVITY ELT               
         BE    STAB0004                                                         
         L     RF,DMCB+8           SET A(ELEMENT FOUND)                         
         ZIC   RE,1(RF)            GET LENGTH OF ELEMENT                        
         BCTR  RE,0                BACK DOWN 1 FOR EX                           
         EX    RE,*+8              MOVE X'23' ELT BY LENGTH                     
         B     *+10                                                             
         MVC   NEW23ELT(0),0(RF)                                                
         GOTO1 VDELELEM,DMCB,(X'23',REC)                                        
*                                  DELETE CLOSEOUT ACTIVITY ELT                 
STAB0004 EQU   *                                                                
         L     R7,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,R7                                                      
         GOTO1 CGETFACT,DMCB,WORK2                                              
         DROP  R7                                                               
*                                                                               
         LA    RF,WORK2                                                         
         USING FACTSD,RF                                                        
         MVC   NEW23ELT+7(8),FASYM  INSERT LUID MAKING CHANGE                   
         DROP  RF                                                               
*                                                                               
         LA    R2,SDTCDTH          CLOSE DATE CHANGED?                          
         ST    R2,DUB              SET POSSIBLE ERROR CURSOR POS                
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    STAB0020            YES - LEAVE ALONE                            
         GOTO1 VDATVAL,DMCB,(2,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    STAB0800            EXIT WITH ERROR                              
         CLI   TOTREQ,NOEOMFND     WAS EOM RECORD FOUND?                        
         BE    STAB0720            NO  - CAN'T ENTER CLOSE DATE                 
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+6)                                 
         CLC   EOMDATE,WORK+6      CLOSE DATE AFTER EOM MONTH?                  
         BNH   STAB0700            YES - SET ERROR                              
         MVC   RSTACLDT(2),WORK+6  RESET CLOSE DATE                             
         MVI   NEW23ELT+5,X'10'    SET 'CLOSED BY FILE'                         
         MVC   NEW23ELT+7(8),USERLUID                                           
*                                  INSERT 'CLOSED BY LUID'                      
         GOTO1 VDATCON,DMCB,(5,WORK),(3,WORK+12)                                
         MVC   NEW23ELT+2(3),WORK+12                                            
STAB0020 EQU   *                                                                
         LA    R2,SDTRJDH          REPORT JOIN DATE CHANGED?                    
         ST    R2,DUB              SET POSSIBLE ERROR CURSOR POS                
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    STAB0040            YES - LEAVE ALONE                            
         XC    NEW0DELT+2(2),NEW0DELT+2                                         
*                                  NO  - CLEAR OUT OLD VALUE                    
         CLI   5(R2),0             LENGTH OF INPUT = ZERO?                      
         BE    STAB0040            YES - NOTHING TO PROCESS                     
         GOTO1 VDATVAL,DMCB,(2,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    STAB0800            EXIT WITH ERROR                              
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+6)                                 
         MVC   NEW0DELT+2(2),WORK+6  RESET JOIN DATE IN 0D ELT                  
STAB0040 EQU   *                                                                
         LA    R2,SDTRLDH          REPORT LEAVE DATE CHANGED?                   
         ST    R2,DUB              SET POSSIBLE ERROR CURSOR POS                
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    STAB0050            YES - LEAVE ALONE                            
*                                  NO  - CLEAR OUT OLD VALUE                    
         XC    NEW0DELT+4(2),NEW0DELT+4                                         
         CLI   5(R2),0             LENGTH OF INPUT = ZERO?                      
         BE    STAB0050            YES - NOTHING TO PROCESS                     
         GOTO1 VDATVAL,DMCB,(2,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    STAB0800            EXIT WITH ERROR                              
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+6)                                 
         MVC   NEW0DELT+4(2),WORK+6  RESET LEAVE DATE IN 0D ELT                 
*                                                                               
STAB0050 EQU   *                                                                
         LA    R2,SDTCTRH          CONTRACT END DATE CHANGED?                   
         ST    R2,DUB              SET POSSIBLE ERROR CURSOR POS                
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    STAB0060            YES - LEAVE ALONE                            
*                                  NO  - CLEAR OLD VALUE                        
         XC    NEW10ELT+2(2),NEW10ELT+2                                         
         CLI   5(R2),0             LENGTH OF INPUT = ZERO?                      
         BE    STAB0060            YES - NOTHING TO PROCESS                     
         GOTO1 VDATVAL,DMCB,(2,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    STAB0800            EXIT WITH ERROR                              
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+6)                                 
         MVC   NEW10ELT+2(2),WORK+6  RESET LEAVE DATE IN 0D ELT                 
*                                                                               
STAB0060 EQU   *                                                                
         LA    R2,SDTHISH          HISTORICAL T/O DATE CHANGED?                 
         ST    R2,DUB              SET POSSIBLE ERROR CURSOR POS                
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    STAB0080            YES - LEAVE ALONE                            
         CLI   5(R2),0             LENGTH OF INPUT = ZERO?                      
         BE    STAB0080            YES - NOTHING TO PROCESS                     
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    STAB0820            EXIT WITH ERROR                              
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
         LA    R2,SDTHISH          RESET DUB IN CASE OF ERROR                   
         ST    R2,DUB                                                           
         CLI   DMCB,1              MONDAY RETURNED?                             
         BNE   STAB0840            NO                                           
         GOTO1 VDATCON,DMCB,(0,WORK),(2,WORK+6)                                 
         MVC   RSTAHIST,WORK+6     RESET HIST T/O DATE                          
STAB0080 EQU   *                                                                
         LA    R2,SDTINVH          TAKE INVOICE DATA CHANGED?                   
         ST    R2,DUB              SET POSSIBLE ERROR CURSOR POS                
         TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BO    STAB0120            YES - LEAVE ALONE                            
         CLI   5(R2),0             LENGTH OF INPUT = ZERO?                      
         BE    STAB0120            YES - NOTHING TO PROCESS                     
         CLI   SDTINV,C'Y'         TAKE INVOICE DATA?                           
         BNE   STAB0860            NO                                           
         OI    RSTAFLGS,X'80'      TURN ON TAKE INVOICE FLAG                    
STAB0120 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,REC,NEW23ELT                                       
         GOTO1 VADDELEM,DMCB,REC,NEW0DELT                                       
         GOTO1 VADDELEM,DMCB,REC,NEW10ELT                                       
         B     STAB0900                                                         
STAB0700 EQU   *                                                                
         MVC   DUB+4(4),=A(-1)                                                  
*                                  SET FLAG TO -1                               
         B     STAB0880                                                         
STAB0720 EQU   *                                                                
         MVC   DUB+4(4),=A(-2)                                                  
*                                  SET FLAG TO -2                               
         B     STAB0880                                                         
STAB0800 EQU   *                                                                
         MVC   DUB+4(4),=A(-3)                                                  
*                                  SET FLAG TO -3                               
         B     STAB0880                                                         
STAB0820 EQU   *                                                                
         MVC   DUB+4(4),=A(-5)                                                  
*                                  SET FLAG TO -5                               
         B     STAB0880                                                         
STAB0840 EQU   *                                                                
         MVC   DUB+4(4),=A(-6)                                                  
*                                  SET FLAG TO -6                               
         B     STAB0880                                                         
STAB0860 EQU   *                                                                
         MVC   DUB+4(4),=A(-7)                                                  
*                                  SET FLAG TO -7                               
         B     STAB0880                                                         
STAB0880 EQU   *                                                                
         LTR   RB,RB               SET CC !=                                    
         B     STABX                                                            
STAB0900 EQU   *                                                                
*                                                                               
* INVENTORY BOOK LIST AND ADDITIONAL TYPES                                      
*                                                                               
         GOTO1 VGETEL,DMCB,(X'48',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          ELEMENT NOT FOUND                            
         BE    STAB0904                                                         
*                                                                               
         XC    WORK,WORK           NOTE - WORK IS ONLY 48 BYTES                 
         L     RF,DMCB+8           SET A(ELEMENT FOUND)                         
         ZIC   RE,1(RF)            GET LENGTH OF ELEMENT                        
         BCTR  RE,0                BACK DOWN 1 FOR EX                           
         EX    RE,*+8              MOVE ELEMENT BY LENGTH                       
         B     *+10                                                             
         MVC   WORK(0),0(RF)       SAVE AND DELETE                              
         GOTO1 VDELELEM,DMCB,(X'48',REC)                                        
*                                                                               
STAB0904 EQU   *                                                                
         LA    R2,SDTIBLH                                                       
         CLI   5(R2),0             INPUT?                                       
         BE    STAB0910                                                         
*                                                                               
         LA    R6,WORK             NOTE - WORK IS ONLY 48 BYTES                 
         USING RSTABLEL,R6                                                      
         MVI   RSTABLEC,RSTABLEQ                                                
         MVI   RSTABLLN,RSTABLLQ   NOTE - WORK IS ONLY 48 BYTES                 
         MVC   RSTABLTG,SDTIBL                                                  
         OC    RSTABLTG,SPACES                                                  
         MVC   RSTABLTS,SDTIBLT                                                 
         OC    RSTABLTS,SPACES                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 VADDELEM,DMCB,REC,WORK                                           
*                                                                               
STAB0910 EQU   *                                                                
         CR    RB,RB               SET CC =                                     
*                                                                               
STABX    EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*----------------------------------------------------------------------         
NOEOMFND EQU   1                                                                
EOMFND   EQU   0                                                                
GETEOM   NTR1                                                                   
         MVI   TOTREQ,EOMFND       CLEAR EOM FLAG FOR JOB                       
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,HALF)                                      
         GOTO1 (RF),(R1),(2,HALF),(3,TODAY)                                     
         XC    KEY,KEY             BUILD EOM KEY                                
         MVI   KEY,X'18'                                                        
         MVC   KEY+24(2),REPALPHA  REP                                          
         MVC   KEY+26(1),TODAY     YEAR                                         
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GETE0100            RECORD NOT FOUND:  SET ERROR                 
         BAS   RE,GETREC           GET EOM RECORD                               
         SR    R7,R7                                                            
         LA    R3,REOMDATE                                                      
*                                                                               
GETE0020 CLC   HALF,0(R3)          SEARCH FOR ACCOUNTING MONTH                  
         BNH   GETE0040                                                         
         LA    R7,2(R7)                                                         
         CH    R7,=H'24'                                                        
         BH    GETE0080                                                         
         LA    R3,REOMDATE(R7)                                                  
         B     GETE0020                                                         
*                                                                               
GETE0040 LTR   R7,R7                                                            
         BZ    GETE0060                                                         
         SRA   R7,1                                                             
         STC   R7,WORK+1           MONTH                                        
         MVC   WORK(1),TODAY       YEAR                                         
         B     GETE0120                                                         
*                                                                               
GETE0060 MVI   WORK+1,12           DECEMBER                                     
         ZIC   R1,TODAY            LAST YEAR                                    
         BCTR  R1,0                                                             
         STC   R1,WORK                                                          
         B     GETE0120                                                         
*                                                                               
GETE0080 MVI   WORK+1,1            JANUARY                                      
         ZIC   R1,TODAY            NEXT YEAR                                    
         LA    R1,1(R1)                                                         
         STC   R1,WORK                                                          
         B     GETE0120                                                         
*                                                                               
GETE0100 EQU   *                                                                
         MVI   TOTREQ,NOEOMFND     SET 'EOM NOT FOUND'                          
         LTR   RB,RB               SET CC NOT = ZERO:  ERROR                    
         B     GETE0140                                                         
GETE0120 EQU   *                                                                
         MVC   EOMDATE,WORK        SAVE RESULT OF TEST                          
         SR    R0,R0               SET CC = ZERO                                
GETE0140 EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*MN                                                                             
***********************************************************************         
* ROUTINE TO FORMAT PERSONAL ID INFORMATION                                     
***********************************************************************         
         DS    0D                                                               
         USING GENOLD,RC                                                        
EDITPID  NTR1  BASE=*,LABEL=*                                                   
                                                                                
*        BUILD TABLE OF EXISTING PID'S                                          
                                                                                
         GOTO1 VGETEL,DMCB,(X'13',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    EPID005                                                          
                                                                                
         USING RSTPDTEL,R4                                                      
         L     R4,DMCB+8                                                        
         XC    PIDS,PIDS                                                        
         LA    R2,PIDS                                                          
EPID001  MVC   0(L'RSTADTPI,R2),RSTADTPI                                        
         LA    R2,L'RSTADTPI(R2)                                                
EPID002  ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    EPID005                                                          
         CLI   0(R4),X'13'                                                      
         BE    EPID001                                                          
         B     EPID002                                                          
         DROP  R4                                                               
                                                                                
*        DELETE ALL EXISTING PID ELEMENTS                                       
                                                                                
EPID005  EQU   *                                                                
         GOTO1 VDELELEM,DMCB,(X'13',REC)                                        
                                                                                
*        VALIDATE THAT PID'S ENTERED EXIST ON SECURITY FILE                     
                                                                                
         LA    R2,SDTPID1H                                                      
         LA    R3,4                                                             
EPID010  CLI   5(R2),0                                                          
         BE    EPID015                                                          
         XC    KEY,KEY                                                          
         USING SAPEREC,R4                                                       
         LA    R4,KEY                                                           
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,C'F'                                                     
         MVI   SAPESUB,X'04'                                                    
         XC    2(11,R4),2(R4)                                                   
         MVC   SAPEAGY,REPALPHA                                                 
         MVC   SAPEPID,8(R2)                                                    
         OC    SAPEPID,SPACES                                                   
         DROP  R4                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,REC2,0                   
         LA    R4,REC2                                                          
         CLC   KEY(23),0(R4)                                                    
         BNE   EPIDERR                                                          
EPID015  LA    R2,16(R2)                                                        
         BCT   R3,EPID010                                                       
                                                                                
*        MAKE SURE THERE ARE NO DUPLICATE ENTRIES                               
                                                                                
EPID016  EQU   *                                                                
         LA    R2,SDTPID1H                                                      
         CLI   5(R2),0                                                          
         BE    EPID016A                                                         
         LA    R2,SDTPID2H                                                      
         CLC   SDTPID1,SDTPID2                                                  
         BE    DUPERR                                                           
         LA    R2,SDTPID3H                                                      
         CLC   SDTPID1,SDTPID3                                                  
         BE    DUPERR                                                           
         LA    R2,SDTPID4H                                                      
         CLC   SDTPID1,SDTPID4                                                  
         BE    DUPERR                                                           
                                                                                
EPID016A LA    R2,SDTPID2H                                                      
         CLI   5(R2),0                                                          
         BE    EPID016B                                                         
         LA    R2,SDTPID3H                                                      
         CLC   SDTPID2,SDTPID3                                                  
         BE    DUPERR                                                           
         LA    R2,SDTPID4H                                                      
         CLC   SDTPID2,SDTPID4                                                  
         BE    DUPERR                                                           
                                                                                
EPID016B LA    R2,SDTPID3H                                                      
         CLI   5(R2),0                                                          
         BE    EPID020                                                          
         LA    R2,SDTPID4H                                                      
         CLC   SDTPID3,SDTPID4                                                  
         BE    DUPERR                                                           
                                                                                
*        ADD NEW 13 ELEMENTS                                                    
                                                                                
EPID020  EQU   *                                                                
         LA    R2,SDTPID1H                                                      
         LA    R3,4                                                             
EPID025  CLI   5(R2),0                                                          
         BE    EPID030                                                          
         LA    R4,WORK2                                                         
         MVC   WORK2(L'NEW13ELT),NEW13ELT                                       
         MVC   WORK2+2(8),8(R2)                                                 
         GOTO1 VADDELEM,DMCB,REC,WORK2                                          
EPID030  LA    R2,16(R2)                                                        
         BCT   R3,EPID025                                                       
                                                                                
         XC    SIGNONS,SIGNONS                                                  
         GOTO1 VGETEL,DMCB,(X'06',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    NOSIGNON                                                         
                                                                                
         USING RSTASOEL,R4                                                      
         L     R4,DMCB+8                                                        
         LA    R2,SIGNONS                                                       
EPID035  MVC   0(L'RSTASO,R2),RSTASO                                            
         LA    R2,L'RSTASO(R2)                                                  
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'06'                                                      
         BE    EPID035                                                          
         DROP  R4                                                               
                                                                                
EPID040  EQU   *                                                                
         LA    R3,SIGNONS                                                       
EPID045  OC    0(8,R3),0(R3)                                                    
         BZ    EPID060                                                          
         LA    R5,4                                                             
         LA    R2,PIDS                                                          
EPID050  OC    0(8,R2),0(R2)                                                    
         BZ    EPID058                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'83'                    SET RECORD ID                       
         MVI   KEY+1,X'09'                  SET RECORD SUBID                    
         MVC   KEY+4(2),REPALPHA            SET REP CODE                        
         MVC   KEY+6(8),0(R3)                                                   
         MVC   KEY+14(8),0(R2)              PERSON ID                           
         MVC   KEY+22(5),RSTAKSTA-RSTAREC+REC   SET STATION CALL LTRS           
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   EPID055             NO  - SKIP IF NOT FOUND                      
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        SET DELETE BIT                               
         BAS   RE,WRITE            REWRITE KEY AS DELETE                        
         BAS   RE,CHECK                                                         
EPID055  EQU   *                                                                
EPID058  LA    R2,8(R2)                                                         
         BCT   R5,EPID050                                                       
         LA    R3,8(R3)                                                         
         B     EPID045                                                          
                                                                                
EPID060  LA    R3,SIGNONS                                                       
EPID062  OC    0(8,R3),0(R3)                                                    
         BZ    EPIDXIT                                                          
         LA    R5,4                                                             
         LA    R2,SDTPID1H                                                      
EPID065  CLI   5(R2),0                                                          
         BE    EPID068                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'83'                    SET RECORD ID                       
         MVI   KEY+1,X'09'                  SET RECORD SUBID                    
         MVC   KEY+4(2),REPALPHA            SET REP CODE                        
         MVC   KEY+6(8),0(R3)               SIGN ON ID                          
         MVC   KEY+14(8),8(R2)              PERSON ID                           
         MVC   KEY+22(5),RSTAKSTA-RSTAREC+REC     SET STAT CALL LTRS            
         MVC   KEY+28(4),BSVDA     DISC ADDR OF RECORD                          
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TOTS                                
         BAS   RE,HIGH             CHECK FOR ADD/REWRITE                        
         LA    RF,ADD              SET FOR ADD                                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                 NOT EQUAL IS 'ADD'                           
         LA    RF,WRITE            EQUAL IS 'REWRITE'                           
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF               PERFORM THE FUNCTION                         
         BAS   RE,CHECK                                                         
EPID068  EQU   *                                                                
         LA    R2,16(R2)                                                        
         BCT   R5,EPID065                                                       
         LA    R3,8(R3)                                                         
         B     EPID062                                                          
                                                                                
EPIDXIT  EQU   *                                                                
         B     EXXMOD                                                           
                                                                                
EPIDERR  LA    R3,2                                                             
         B     ERROR                                                            
                                                                                
DUPERR   EQU   *                                                                
         LA    R3,401                                                           
         B     ERROR                                                            
                                                                                
NOSIGNON EQU   *                                                                
         LA    R3,988                                                           
         LA    R2,SDTPID1H                                                      
         B     ERROR                                                            
*                                                                               
SIGNONS  DS    CL40                                                             
PIDS     DS    CL32                                                             
         LTORG                                                                  
*MN                                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT VENDOR INFORMATION                                          
***********************************************************************         
         DS    0D                                                               
         USING GENOLD,RC                                                        
EDITVEND NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTO1 VDELELEM,DMCB,(X'2C',REC)                                        
*                                  DELETE EXISTING VENDOR ELEMENT               
         LA    R2,SDTVENDH                                                      
EDVN025  CLI   5(R2),0             ANY DATA IN FIELD?                           
         BE    EDVN0080            NO                                           
         MVC   VENDWORK,8(R2)      EXTRACT FIELD FROM SCREEN                    
         OC    VENDWORK,SPACES     SET BINARY TO SPACE                          
*                                                                               
*   VENDOR CHECK:  IF A NEW VENDOR IS ESTABLISHED, PLEASE INSERT                
*        THE CODE SET UP BY FRED ROE INTO THE LIST.                             
*                                                                               
         CLC   =C'*TVSCAN ',VENDWORK                                            
         BE    EDVN0040            ACCEPTED                                     
         CLC   =C'ETRANST ',VENDWORK                                            
         BE    EDVN0040            ACCEPTED                                     
         CLC   =C'*TVSCANB',VENDWORK                                            
         BE    EDVN0040            ACCEPTED                                     
         CLC   =C'*1DOMAIN',VENDWORK                                            
         BNE   EDVN0060            ERROR:  NOT RECOGNIZED                       
EDVN0040 EQU   *                                                                
         MVC   NEW2CELT+5(8),VENDWORK                                           
*                                  INSERT VENDOR INTO ELEMENT                   
         GOTO1 VDATCON,DMCB,(5,WORK),(3,NEW2CELT+2)                             
*                                  INSERT TODAY'S DATE INTO ELEMENT             
         L     R7,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,R7                                                      
         GOTO1 CGETFACT,DMCB,WORK2                                              
         DROP  R7                                                               
*                                                                               
         LA    RF,WORK2                                                         
         USING FACTSD,RF                                                        
         MVC   NEW2CELT+13(8),FASYM                                             
*                                  INSERT LUID INTO ELEMENT                     
         DROP  RF                                                               
         GOTO1 VADDELEM,DMCB,REC,NEW2CELT                                       
         B     EDVN0080                                                         
EDVN0060 EQU   *                                                                
         ST    R2,DUB              SET A(FIELD IN ERROR)                        
         MVC   DUB+4,=A(-8)        SET ERROR RETURN CODE                        
         LTR   RB,RB               EXIT CC NOT ZERO                             
         B     EDVN0100                                                         
EDVN0080 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
EDVN0100 EQU   *                                                                
         XIT1                                                                   
                                                                                
VENDWORK DS    CL8                                                              
         LTORG                                                                  
                                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EDIT OFFICE/TEAM SCREEN, GENERATE RECORD DATA            *         
***********************************************************************         
         DS    0D                                                               
EDITOFTM NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,GETREC           GET RECORD                                   
         GOTO1 VDELELEM,DMCB,(4,REC)                                            
*                                  DELETE ALL OFF/TEAM ELEMENTS                 
         LA    R2,OTMOF1H          SET A(1ST OFFICE FIELD HEADER)               
         LA    R3,OTMTM1H          SET A(1ST TEAM   FIELD HEADER)               
         LA    R5,WORK2            SET A(WORK AREA FOR ELEMENT)                 
         USING RSTAOTEL,R5                                                      
         XC    WORK2(100),WORK2    CLEAR IT OUT                                 
         MVI   RSTAOTCO,4          INITIALIZE ELEMENT CODE                      
         MVI   RSTAOTLN,11            AND LENGTH                                
OFFE0020 EQU   *                                                                
         LA    RF,OTMLSTH          SET A(END OF SCREEN)                         
         CR    R2,RF               END OF SCREEN REACHED?                       
         BNL   OFFE0160            YES - FINISHED                               
         CLI   5(R2),0             ANY OFFICE ENTERED?                          
         BNZ   OFFE0040            YES -                                        
         CLI   5(R3),0             NO  - TEAM ENTERED?                          
         BZ    OFFE0120            NO  - CHECK NEXT FIELDS                      
         B     OFFE0080            YES - IT IS ERROR                            
*                                                                               
OFFE0040 EQU   *                   TEST OFFICE ON FILE                          
         XC    KEY,KEY                                                          
         MVI   KEY,4                                                            
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),8(R2)     OFFICE CODE FROM SCREEN                      
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    OFFE0060            FOUND                                        
         ST    R2,DUB              PASS BACK A(SCREEN FIELD)                    
         LA    R3,151              INVALID OFFICE                               
         ST    R3,DUB+4            PASS BACK ERROR CODE                         
         B     OFFE0140                                                         
*                                                                               
OFFE0060 EQU   *                                                                
         MVC   RSTAOTOF,8(R2)      MOVE IN OFFICE                               
*                                                                               
         CLI   5(R3),0             ANY TEAM ENTERED?                            
         BZ    OFFE0080            NO  - ERROR                                  
*                                                                               
*  VERIFY TEAM ON FILE                                                          
         MVI   KEY,5                                                            
         MVC   KEY+25(2),8(R3)                                                  
         OC    KEY+25(2),SPACES                                                 
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    OFFE0100            TEAM RECORD FOUND                            
OFFE0080 EQU   *                                                                
         ST    R3,DUB              PASS BACK A(SCREEN FIELD)                    
         LA    R3,119              TEAM NOT FOUND                               
         ST    R3,DUB+4            PASS BACK ERROR CODE                         
         B     OFFE0140                                                         
*                                                                               
OFFE0100 EQU   *                                                                
         MVC   RSTAOTTM,8(R3)      MOVE IN TEAM                                 
*                                                                               
         DROP  R5                                                               
         GOTO1 VADDELEM,DMCB,REC,(R5)                                           
*                                                                               
OFFE0120 EQU   *                                                                
         LA    R2,NXTOTFLD(R2)     BUMP TO NEXT OFFICE FIELD                    
         LA    R3,NXTOTFLD(R3)     BUMP TO NEXT TEAM   FIELD                    
         B     OFFE0020            GO BACK FOR NEXT FIELD                       
*                                                                               
OFFE0140 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     OFFE0180                                                         
OFFE0160 EQU   *                                                                
         SR    RF,RF               SET CC = ZERO                                
OFFE0180 EQU   *                                                                
         B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        OPTEDT -  EDIT AND LOAD ALL FIELDS ON THE STATION RECORD               
*                   EXTENDED DESCRIPTION ELEMENT                                
**********************************************************************          
         SPACE 1                                                                
OPTEDT   NTR1  BASE=*,LABEL=*                                                   
         LA    R5,WORK2       BUILD X'08' (EXTRA DESCRIPTION ELEMENT)           
         XC    WORK2(80),WORK2                                                  
         USING RSTAXXEL,R5                                                      
         MVC   RSTAXXEL(2),=X'0850'                                             
         MVI   RSTAOPTS,C'N'       CLEAR 9 OPTION FLAGS TO 'N'                  
         MVC   RSTAOPTS+1(8),RSTAOPTS                                           
         MVI   RSTAOPTA,0          CLEAR SECONDARY OPTION FIELD                 
         MVI   RSTAOPTB,0          CLEAR SECONDARY OPTION FIELD                 
*                                                                               
         LA    R2,FMSOPTH                                                       
*                                                                               
OPED0020 DS    0H                                                               
         OC    FMSOPT,SPACES                                                    
         LA    R1,25               LOOP THRU 25 OPTIONS                         
         LA    R4,FMSOPT                                                        
OPED0040 CLI   0(R4),C' '                                                       
         BNE   OPED0060                                                         
         MVI   0(R4),C'N'                                                       
OPED0060 CLI   0(R4),C'N'                                                       
         BE    OPED0080                                                         
         CLI   0(R4),C'Y'                                                       
         BE    OPED0080                                                         
         CH    R1,=H'13'           ALLOW 1, 2 OR 3 FOR OPTION 5                 
         BNE   FLERR2                                                           
         CLI   0(R4),C'1'                                                       
         BL    FLERR2                                                           
         CLI   0(R4),C'3'                                                       
         BH    FLERR2                                                           
OPED0080 LA    R4,1(R4)                                                         
         BCT   R1,OPED0040                                                      
*                                                                               
         MVC   RSTAOPTS(9),FMSOPT  STORE FIRST 9 OPTIONS AS CHARACTER           
         LA    R4,FMSOPT+9                                                      
         LA    R6,RSTAOPTA                                                      
         BAS   RE,PUTPROF          STORE LAST 8 PROFILES                        
         LA    R4,FMSOPT+17                                                     
         LA    R6,RSTAOPTB                                                      
         BAS   RE,PUTPROF          STORE LAST 8 PROFILES                        
         TM    BYTE4,X'20'         ALTERNATE CALENDAR TURNED ON?                
         BNO   OPED0100            NO  - LEAVE IT AS SET                        
         OI    RSTAOPTA,X'20'      YES - MAKE SURE IT'S ON                      
*                                                                               
OPED0100 CLI   RSTAOPTS+2,C'Y'                                                  
         BNE   OPED0120                                                         
         OI    RSTASTAT,X'02'      INDICATE "DON'T SEND" IS ALLOWED             
*                                                                               
*- UNCOUPLE DESTINATION ID FIELD FROM TRAFFIC SYSTEM.                           
OPED0120 EQU   *                                                                
         LA    R2,FMSRDSH          REP OFFICE DESTINATION ID                    
         CLI   5(R2),0             ANY I/P                                      
         BZ    OPED0260                                                         
*                                                                               
         BAS   RE,MOVE                                                          
         BAS   RE,GETID            EDIT RECEIVING ID FIELD                      
         CLC   HALF,=H'0'          VALID ID?                                    
         BE    OPED0140            NO - CHECK IF FAX/MAILBOX NUMBER             
         CLC   =C'GRAPH',FMSRDS                                                 
         BE    OEBAD16                                                          
         MVC   RSTAORDS(8),WORK    YES - SAVE OFF ID                            
         MVC   RSTAORID(2),HALF                                                 
         B     OPED0260                                                         
OPED0140 EQU   *                                                                
         CLI   FMSTRF,C'G'                                                      
         BE    *+12                                                             
         CLI   FMSTRF,C'A'                                                      
         BNE   OEBAD3                                                           
         ZIC   RF,5(R2)            DERIVE LENGTH                                
         LA    RE,8(R2)            SET A(FIELD)                                 
         CLC   =C'MB=',0(RE)       MAIL BOX NUMBER?                             
         BNE   OPED0160            NO                                           
         CH    RF,=H'11'           MAILBOX LENGTH MUST BE 8 (+3 FOR             
*                                     KEYWORD MB=)                              
         BNE   OEBAD14                                                          
         LA    R1,3                YES                                          
         SR    RF,R1               SUBTRACT 3 FROM LENGTH FOR 'MB='             
         LA    RE,3(RE)            BUMP PAST 'MB='                              
OPED0160 EQU   *                                                                
         CLI   0(RE),C'0'          ZERO OR GREATER?                             
         BL    OEBAD15             NO                                           
         CLI   0(RE),C'9'          NINE OR LESS?                                
         BH    OEBAD15             NO                                           
         LA    RE,1(RE)                                                         
         BCT   RF,OPED0160         GO BACK FOR NEXT                             
         CLC   =C'011',8(R2)       INTERNATIONAL FAX NUMBER?                    
         BE    OPED0180            YES - SPECIAL TREATMENT FOR NUMBER           
         MVC   RSTAOFX2(13),FMSRDS NO  - SAVE NUMBER AS ENTERED                 
         B     OPED0240                                                         
OPED0180 EQU   *                                                                
         MVI   RSTAOFX2,0          SET 1ST BYTE TO BINARY ZERO                  
*                                     TO SERVE AS FLAG                          
         MVI   RSTAOFX2+1,11       SET INTERNATIONAL CODE                       
*                                                                               
*   THIS IS REDUNDANT AT THIS TIME, AS '011' IS THE ONLY INTERNAT-              
*        IONAL CODE WE ARE USING.  LATER, IF MORE ARE USED, THIS                
*        BECOMES MORE MEANINGFUL.                                               
*                                                                               
         ZIC   RF,5(R2)            RETRIEVE LENGTH AGAIN                        
         LA    RE,4                SUBTRACT FOR 011 + EX                        
         SR    RF,RE                                                            
         EX    RF,OPED0200         PACK BY LENGTH                               
         B     OPED0220                                                         
OPED0200 PACK  DUB(8),11(0,R2)     PACK NUMBER AFTER 011                        
*                                                                               
OPED0220 EQU   *                                                                
         LA    RF,1(RF)            ADD 1 BACK FROM EX                           
         STC   RF,RSTAOFX2+2       SAVE LENGTH OF DATA FOR EDITING              
*                                     SIGNIFICANT DIGITS                        
         MVC   RSTAOFX2+3(8),DUB   SAVE INTERNATIONAL NUMBER                    
*                                                                               
* IN THE CASE THAT WE HAVE A FAX,MB,ETC. NUMBER IN THE DEST FIELD, WE           
* FORCE THE DEST ID IN THE RSTAREC TO BE 'GRAPH'                                
*                                                                               
OPED0240 EQU   *                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(8),=C'GRAPH   '                                             
         BAS   RE,GETID                                                         
         CLC   HALF,=H'0'                                                       
         BNE   *+6                                                              
         DC    H'0'                'GRAPH' HAD BETTER BE VALID                  
         MVC   RSTAORDS(8),WORK                                                 
         MVC   RSTAORID(2),HALF                                                 
OPED0260 EQU   *                                                                
         LA    R2,FMSDMCH          DEMO MARKET CODE                             
         GOTO1 =A(DEMOMKT),DMCB,(RC),RR=Y                                       
         BNZ   OEBAD17             ERROR RETURN                                 
         LA    R2,FMSFAXH          STATION FAX NUMBER                           
         CLI   5(R2),0                                                          
         BZ    OPED0360                                                         
         ZIC   RF,5(R2)            DERIVE LENGTH                                
         LA    RE,8(R2)            SET A(FIELD)                                 
         CLC   =C'MB=',0(RE)       MAIL BOX NUMBER?                             
         BNE   OPED0280            NO                                           
         CH    RF,=H'11'           MAILBOX LENGTH MUST BE 8 (+3 FOR             
*                                     KEYWORD MB=)                              
         BNE   OEBAD14                                                          
         LA    R1,3                YES                                          
         SR    RF,R1               SUBTRACT 3 FROM LENGTH FOR 'MB='             
         LA    RE,3(RE)            BUMP PAST 'MB='                              
OPED0280 EQU   *                                                                
         CLI   0(RE),C'0'          ZERO OR GREATER?                             
         BL    OEBAD13             NO                                           
         CLI   0(RE),C'9'          NINE OR LESS?                                
         BH    OEBAD13             NO                                           
         LA    RE,1(RE)                                                         
         BCT   RF,OPED0280         GO BACK FOR NEXT                             
         CLC   =C'011',8(R2)       INTERNATIONAL FAX NUMBER?                    
         BE    OPED0300            YES - SPECIAL TREATMENT FOR NUMBER           
         MVC   RSTAOFAX(13),FMSFAX NO  - SAVE NUMBER AS ENTERED                 
         B     OPED0360                                                         
OPED0300 EQU   *                                                                
         MVI   RSTAOFAX,0          SET 1ST BYTE TO BINARY ZERO                  
*                                     TO SERVE AS FLAG                          
         MVI   RSTAOFAX+1,11       SET INTERNATIONAL CODE                       
*                                                                               
*   THIS IS REDUNDANT AT THIS TIME, AS '011' IS THE ONLY INTERNAT-              
*        IONAL CODE WE ARE USING.  LATER, IF MORE ARE USED, THIS                
*        BECOMES MORE MEANINGFUL.                                               
*                                                                               
         ZIC   RF,5(R2)            RETRIEVE LENGTH AGAIN                        
         LA    RE,4                SUBTRACT FOR 011 + EX                        
         SR    RF,RE                                                            
         EX    RF,OPED0320         PACK BY LENGTH                               
         B     OPED0340                                                         
OPED0320 PACK  DUB(8),11(0,R2)     PACK NUMBER AFTER 011                        
*                                                                               
OPED0340 EQU   *                                                                
         LA    RF,1(RF)            ADD 1 BACK FROM EX                           
         STC   RF,RSTAOFAX+2       SAVE LENGTH OF DATA FOR EDITING              
*                                     SIGNIFICANT DIGITS                        
         MVC   RSTAOFAX+3(8),DUB   SAVE INTERNATIONAL NUMBER                    
OPED0360 EQU   *                                                                
*                                                                               
         LA    R2,FMSINTH          STATION INTERFACE CODE                       
         CLI   5(R2),0                                                          
         BNZ   OPED0380                                                         
         LA    R2,SVPGPBIT         PROGRAM PROFILE BITS                         
         TM    0(R2),X'80'         0TH BIT ON = INTERFACE CDE NEEDED            
         BNO   OPED0400            NOT REQUIRED                                 
         LR    R2,RA                CHECK FOR DDS TERMINAL                      
         USING TWAD,R2                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    OPED0400            YES - IGNORE TEST                            
         DROP  R2                                                               
         B     OEBAD5              REQUIRED = ERROR                             
OPED0380 MVC   RSTAOSI(10),FMSINT                                               
OPED0400 EQU   *                                                                
*                                                                               
*- DESTINATION FORMAT/CONTRACT STATUS FILTER REQUIRED IF DEST ID GIVEN.         
         LA    R2,FMSRWSH               DESTINATION FORMAT                      
         CLI   5(R2),0                                                          
         BNE   OPED0420                                                         
         OC    RSTAORID,RSTAORID                                                
         BZ    OPED0480                                                         
         B     OEBAD               REQUIRED, BUT NOT GIVEN.                     
*                                                                               
*- FORMAT MAY BE ANY VALID TRAFFIC SYSTEM OR BLANK (REP FORMAT)                 
OPED0420 EQU   *                                                                
         MVI   RSTARWS,0           ASSUME 0.                                    
*                                                                               
         OC    RSTAOFX2,RSTAOFX2   IF 2ND FAX# ENTERED, DEST FMT                
         BZ    OPED0440            MUST BE A OR G                               
         CLI   FMSRWS,C'G'                                                      
         BE    OPED0440                                                         
         CLI   FMSRWS,C'A'                                                      
         BNE   OEBAD                                                            
*                                                                               
OPED0440 EQU   *                                                                
         CLI   FMSRWS,0            SCREEN INPUT 0 OR BLANK?                     
         BE    OPED0460            YES = LEAVE FORMAT IN ELEM AS 0.             
         CLI   FMSRWS,C' '                                                      
         BE    OPED0460                                                         
*                                                                               
         BAS   RE,EDITTRAF         EDIT FORMAT AS TRAFFIC SYSTEM                
         BNZ   OEBAD                                                            
*                                                                               
         MVC   RSTARWS(1),FMSRWS   DESTINATION FORMAT CODE.                     
*                                                                               
*- 2ND BYTE OF FORMAT MUST BE C/U/B  (CONF/UNCONF/BOTH)                         
OPED0460 EQU   *                                                                
         MVC   RSTAWSCF(1),FMSRWS+1  CONTRACT FILTER BYTE                       
*                                                                               
         CLI   FMSRWS+1,C'C'                                                    
         BE    OPED0480                                                         
         CLI   FMSRWS+1,C'U'                                                    
         BE    OPED0480                                                         
         CLI   FMSRWS+1,C'B'                                                    
         BNE   OEBAD                                                            
*                                                                               
OPED0480 EQU   *                                                                
         LA    R2,FMSMCDH                                                       
         CLI   5(R2),0                                                          
         BNE   OPED0500                                                         
         LA    R2,SVPGPBIT         PROGRAM PROFILE BITS                         
         TM    0(R2),X'40'         1TH BIT ON = MARKET CDE NEEDED               
         BNO   OPED0520            NOT REQUIRED                                 
         LR    R2,RA                CHECK FOR DDS TERMINAL                      
         USING TWAD,R2                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    OPED0520            YES - IGNORE TEST                            
         DROP  R2                                                               
         B     OEBAD6              REQUIRED = ERROR                             
OPED0500 BAS   RE,DISPMKT                                                       
         BNZ   OEBAD4                                                           
         MVC   RSTAMKTC(4),FMSMCD                                               
OPED0520 EQU   *                                                                
*                                                                               
* SECONDARY AFFILIATE                                                           
         LA    R2,FMSAF2H                                                       
         LA    R1,*                                                             
         A     R1,=A(AFFLLIST-(*-4))                                            
         CLI   5(R2),0                                                          
         BE    OPED0540                                                         
*                                                                               
         CLC   0(3,R1),8(R2)                                                    
         BE    *+20                                                             
         LA    R1,3(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   *-18                                                             
         B     OEBAD7                                                           
*                                                                               
         MVC   RSTAAFL2,8(R2)                                                   
OPED0540 DS    0H                                                               
*                                                                               
* TIME ZONE                                                                     
         LA    R2,FMSTZH                                                        
         CLI   5(R2),0                                                          
         BE    OPED0560                                                         
         CLI   5(R2),1                                                          
         BNE   OEBAD8                                                           
         LA    RF,TZTAB                                                         
         LA    R1,4                                                             
*                                                                               
         CLC   8(1,R2),0(RF)                                                    
         BE    *+16                                                             
         LA    RF,1(RF)                                                         
         BCT   R1,*-14                                                          
         B     OEBAD8                                                           
*                                                                               
         MVC   RSTATZ(1),8(R2)                                                  
         B     OPED0560                                                         
TZTAB    DC    C'ECMP'             VALID TIME ZONES                             
OPED0560 DS    0H                                                               
*                                                                               
*                                                                               
* ELECTRONIC CONTRACT                                                           
         LA    R2,FMSECH                                                        
         NI    RSTAXOPT,X'FF'-X'90'                                             
*                                  ASSUME NOT E/C, NOT LOCAL                    
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    OPED0660                                                         
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8              E/C?                                         
         B     *+10                                                             
         CLC   8(0,R2),=C'YES'                                                  
         BE    OPED0580            YES                                          
         EX    R1,*+8              LOCAL E/C?                                   
         B     *+10                                                             
         CLC   8(0,R2),=C'LOC'                                                  
         BE    OPED0570            YES                                          
         EX    R1,*+8              NOT E/C?                                     
         B     *+10                                                             
         CLC   8(0,R2),=C'NO '                                                  
         BE    OPED0660            YES                                          
         B     OEBAD9              ERROR                                        
OPED0570 EQU   *                                                                
         OI    RSTAXOPT,X'10'      LOCAL E/C: TURN ON INDICATOR                 
OPED0580 EQU   *                                                                
         CLI   RSTAKSTA+4,C'A'     RADIO STATION?                               
         BE    OPED0600            YES                                          
         CLI   RSTAKSTA+4,C'F'     RADIO STATION?                               
         BNE   OPED0620            NO  - CHECK TELEVISION                       
OPED0600 EQU   *                                                                
         TM    RSTAOPTA,X'01'      BIAS EC STATION FLAG SET?                    
         BO    OPED0640            YES - ACCEPT THIS INPUT                      
         B     OEBAD12             NO  - RETURN ERROR MESSAGE                   
OPED0620 EQU   *                                                                
         CLI   RSTATRAF,C'B'       BIAS TRAFFIC STATION?                        
         BE    OPED0640            YES                                          
         CLI   RSTATRAF,C'W'       ALTERNATE BIAS TRAFFIC STATION?              
         BE    OPED0640            YES                                          
         CLI   RSTATRAF,C'K'       ENTERPRISE TRAFFIC STATION?                  
         BE    OPED0640            YES                                          
         CLI   RSTATRAF,C'H'       ENTERPRISE TRAFFIC STATION?                  
         BE    OPED0640            YES                                          
         CLI   RSTATRAF,C'Z'       VSS (ENTERPRISE) TRAFFIC STATION?            
         BE    OPED0640            YES                                          
         CLI   RSTATRAF,C'I'       OSI        TRAFFIC STATION?                  
         BE    OPED0640            YES                                          
         CLI   RSTATRAF,C'C'       COLUMBINE  TRAFFIC STATION?                  
         BE    OPED0640            YES                                          
         CLI   RSTATRAF,C'V'       VCI  TRAFFIC STATION?                        
         BE    OPED0640            YES                                          
         CLI   RSTATRAF,C'S'       VCI  STARS II TRAFFIC STATION?               
         BE    OPED0640            YES                                          
         CLI   RSTATRAF,C'T'       VCI/BDE TRAFFIC STATION?                     
         BE    OPED0640            YES                                          
         CLI   RSTATRAF,C'U'       VCI/BDE STARS II TRAFFIC STATION?            
         BE    OPED0640            YES                                          
         CLI   RSTATRAF,C'O'       WIDE ORBIT TRAFFIC STATION?                  
         BE    OPED0640            YES                                          
         CLI   RSTATRAF,C'L'       NEW MARKETRON TRAFFIC STATION?               
         BE    OPED0640            YES                                          
         CLI   RSTATRAF,C'J'       JDS/2000 TRAFFIC STATION?                    
         BNE   OEBAD12             NO  - NOT EC TRAFFIC FORMAT                  
OPED0640 EQU   *                                                                
         OI    RSTAXOPT,X'80'      YES                                          
OPED0660 DS    0H                                                               
*                                                                               
* LUID                                                                          
         LA    R2,FMSLUH                                                        
         CLI   5(R2),0                                                          
         BE    OPED0720                                                         
         CLI   5(R2),8                                                          
         BNE   OEBAD11                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'T'                                                         
         MVC   KEY+7(8),8(R2)                                                   
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,REC2                     
         CLC   REC2(26),KEY                                                     
         BNE   OEBAD11                                                          
* NEED A HOME-BREW GETEL FOR CTFILE RECS                                        
         LA    R1,REC2+28                                                       
         SR    RF,RF                                                            
*                                                                               
OPED0680 CLI   0(R1),0                                                          
         BE    OEBAD11                                                          
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         CLI   0(R1),X'20'                                                      
         BNE   OPED0680                                                         
*                                                                               
         USING CTIDD,R1                                                         
OPED0700 CLC   REC+22(4),CTID                                                   
         BNE   OPED0680                                                         
         MVC   RSTALUID,FMSLU                                                   
OPED0720 DS    0H                                                               
         DROP  R1                                                               
*                                                                               
* INVOICE                                                                       
         LA    R2,FMSINVH                                                       
         NI    RSTAXOPT,X'FF'-X'40'  ASSUME NO                                  
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    OPED0740                                                         
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'YES'                                                  
         BE    OPED0730                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'NO '                                                  
         BE    OPED0740                                                         
         B     OEBAD10                                                          
OPED0730 DS    0H                                                               
         OI    RSTAXOPT,X'40'      YES                                          
OPED0740 DS    0H                                                               
*                                                                               
         DROP  R5                                                               
         GOTO1 VADDELEM,DMCB,REC,(R5)                                           
*                                                                               
OEGOOD   EQU   *                                                                
         LA    R0,0                                                             
OEEXIT   EQU   *                                                                
         STC   R0,DMCB                                                          
         ST    R2,DMCB+4                                                        
         LTR   R0,R0                                                            
         B     EXXMOD                                                           
OEBAD    EQU   *                                                                
         LA    R0,1                                                             
         B     OEEXIT                                                           
OEBAD2   EQU   *                                                                
         LA    R0,2                                                             
         B     OEEXIT                                                           
OEBAD3   EQU   *                                                                
         LA    R0,3                                                             
         B     OEEXIT                                                           
OEBAD4   EQU   *                                                                
         LA    R0,4                                                             
         B     OEEXIT                                                           
OEBAD5   EQU   *                                                                
         LA    R0,5                                                             
         B     OEEXIT                                                           
OEBAD6   EQU   *                                                                
         LA    R0,6                                                             
         B     OEEXIT                                                           
OEBAD7   EQU   *                                                                
         LA    R0,7                                                             
         B     OEEXIT                                                           
OEBAD8   EQU   *                                                                
         LA    R0,8                                                             
         B     OEEXIT                                                           
OEBAD9   EQU   *                                                                
         LA    R0,9                                                             
         B     OEEXIT                                                           
OEBAD10  EQU   *                                                                
         LA    R0,10                                                            
         B     OEEXIT                                                           
OEBAD11  EQU   *                                                                
         LA    R0,11                                                            
         B     OEEXIT                                                           
OEBAD12  EQU   *                                                                
         LA    R0,12               NOT EC TRAFFIC FORMAT                        
         B     OEEXIT                                                           
OEBAD13  EQU   *                                                                
         LA    R0,13               FAX/MB= NOT NUMERIC                          
         B     OEEXIT                                                           
OEBAD14  EQU   *                                                                
         LA    R0,14               MB= MUST BE 8 DIGITS                         
         B     OEEXIT                                                           
OEBAD15  EQU   *                                                                
         LA    R0,15               INVALID ID/MB/FAX#                           
         B     OEEXIT                                                           
OEBAD16  EQU   *                                                                
         LA    R0,16               INVALID ID/MB/FAX#                           
         B     OEEXIT                                                           
OEBAD17  EQU   *                                                                
         LA    R0,17               INVALID DEMO MARKET CODE                     
         B     OEEXIT                                                           
         EJECT                                                                  
*---------------------------------------------------------------------          
* ROUTINE TO CONVERT 8 Y/N TYPE OPTIONS ON SCREEN TO 1 BYTE IN BINARY           
* NOTE: R4 ADDRESSES 8-BYTE SCREEN FIELD WITH OPTIONS                           
*       R6 ADDRESSES BYTE TO HAVE BINARY WRITTEN TO                             
*---------------------------------------------------------------------          
PUTPROF  NTR1                                                                   
         LA    R1,8                     8 BITS                                  
         ZIC   R3,=X'80'                FIRST BIT HEX VALUE                     
PUTP10   CLI   0(R4),C'N'                                                       
         BE    PUTP20                                                           
         STC   R3,HALF                                                          
         OC    0(1,R6),HALF             BIT VALUE INTO RECORD                   
PUTP20   SRL   R3,1                     NEXT BIT VALUE                          
         LA    R4,1(R4)                 NEXT BIT ON SCREEN                      
         BCT   R1,PUTP10                                                        
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO PROCESS A UNIQUE ID NUMBER ENTRY                                   
*       BUILD X'2A' ELEMENT TO STORE IT IF NECESSARY                            
*                                                                               
*       BAD CODE RETURNS CC NOT ZERO                                            
*                                                                               
*                                                                               
***********************************************************************         
UNIQUEID NMOD1 0,*UNIQID*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    R4,REC2                                                          
         USING CT99RECD,R4                                                      
         XC    0(25,R4),0(R4)      BUILD CONTROL FILE KEY                       
         MVI   CT99KTYP,X'99'                                                   
         MVC   CT99KUID,FMSUID                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R4),(R4),0                  
         CLI   DMCB+8,0                                                         
         BNE   UNIQ0800                                                         
         LA    R3,SUBREPS          SUBREP TABLE                                 
UNIQ0020 EQU   *                                                                
         CLI   0(R3),0             END OF TABLE REACHED?                        
         BE    UNIQ0030            YES - OKAY                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'8308'     SET UID PASSIVE KEY TYPE                     
         MVC   KEY+14(2),0(R3)     INSERT REP FROM TABLE                        
         MVC   KEY+16(6),FMSUID    INSERT UNIQUE ID                             
         GOTO1 HIGH                                                             
         CLC   KEY(22),KEYSAVE     UNIQUE ID ALREADY IN USE?                    
         BE    UNIQ0700            YES - ERROR                                  
         LA    R3,2(R3)            BUMP TO NEXT REP                             
         B     UNIQ0020                                                         
UNIQ0030 EQU   *                                                                
         XC    WORK2(64),WORK2                                                  
         MVC   WORK2(2),=X'2A0C'                                                
         MVC   WORK2+2(06),8(R2)                                                
         GOTO1 VDELELEM,DMCB,(X'2A',REC)     DROP OLD UNIQUE ID ELT             
         GOTO1 VADDELEM,DMCB,REC,WORK2       ADD  NEW UNIQUE ID ELT             
         LA    R2,FMSLIBH          ANYTHING IN LIABILITY FIELD?                 
         CLI   5(R2),0             YES - CAN'T USE THIS FIELD                   
         BNE   UNIQ0040                                                         
         MVC   FMSLIC+14(08),=C'UID MKT='                                       
         FOUT  FMSLICH                                                          
UNIQ0040 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
         B     UNIQ0900                                                         
UNIQ0700 EQU   *                                                                
         MVI   DUB,1                                                            
         MVC   DUB+1(2),0(R3)      INSERT REP CODE                              
         LTR   RB,RB               SET CC NOT ZERO                              
         B     UNIQ0900                                                         
UNIQ0800 EQU   *                                                                
         MVI   DUB,2                                                            
         LTR   RB,RB               SET CC NOT ZERO                              
         B     UNIQ0900                                                         
UNIQ0900 EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* ROUTINE TO VALIDATE DEMO MARKET CODE, IF ENTERED, AND TO                      
*       BUILD X'11' ELEMENT TO STORE IT                                         
*                                                                               
*       BAD CODE RETURNS CC NOT ZERO                                            
*                                                                               
*                                                                               
***********************************************************************         
DEMOMKT  NMOD1 0,*DEMOMKT*                                                      
         L     RC,0(R1)                                                         
*                                                                               
         LA    R2,FMSDMCH          SET A(DEMO MKT CODE HDR)                     
         CLI   5(R2),0                                                          
         BE    DEMK0800            NO CODE - EXIT                               
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BO    DEMK0080            YES - PACK AND STORE                         
DEMK0040 EQU   *                   NO  - CHECK ALPHA REQUIREMENTS               
         CLI   5(R2),3             ALPHANUM MUST BE THREE CHARS                 
         BNE   DEMK0900            INVALID FOR ALPHANUM: CC NOT ZERO            
         MVC   DEMKCODE(3),8(R2)   INSERT CODE INTO ELEMENT                     
         B     DEMK0800                                                         
DEMK0080 BAS   RE,DEMKPACK                                                      
         LTR   R0,R0                                                            
         BZ    DEMK0880            TURN AROUND CONDITION CODE: ERROR            
         STCM  R0,3,DEMKCODE+1     MARKET DEMO CODE: LOW ORDER BYTES            
         SRL   R0,16               VALUE TOO BIG? HIGH ORDER BYTES              
         LTR   R0,R0                                                            
         BNZ   DEMK0880            NOT ZERO:  ERROR                             
DEMK0800 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,REC,DEMKELT                                        
*                                  ADD ELEMENT TO RECORD                        
         SR    R0,R0               SET CC = ZERO                                
         B     DEMK0900                                                         
DEMK0880 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
DEMK0900 EQU   *                                                                
         XIT1                                                                   
DEMKELT  DC    X'1108'                                                          
DEMKCODE DC    XL3'000000'                                                      
DEMKFILL DC    XL3'000000'                                                      
*                                                                               
DEMKPACK SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                        OR NON NUMERIC                       
         BCTR  R1,R0                                                            
         EX    R1,VARMPACK                                                      
         CVB   R0,DUB                                                           
         BR    RE                                                               
VARMPACK PACK  DUB,8(0,R2)                                                      
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* ROUTINE TO DISPLAY FIRST LINE OF CONTRACT COMMENT                             
*                                                                               
* INPUT P1: A(COMMENT CODE)                                                     
*                                                                               
*       P2: BYTE 0:   LENGHT OF OUTPUT FIELD                                    
*           BYTE 1-3: A(OUTPUT FIELD)                                           
*                                                                               
***********************************************************************         
DISPFC   NMOD1 0,*DISPFC*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R2,8(R1)            COMMENT CODE                                 
         L     R3,12(R1)           L, OUTPUT FIELD                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCMTD,R6                                                         
         MVI   RCMTKTYP,X'2E'      RECORD TYPE                                  
         MVC   RCMTKREP,REPALPHA   REP CODE                                     
         MVC   RCMTKOFF,=X'FFFF'   OFFICE CODE                                  
         MVC   RCMTKCDE,0(R2)      COMMENT CODE                                 
         OC    RCMTKCDE,SPACES     BLANK PADDED                                 
         DROP  R6                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BNE   DISPFCX                                                          
*                                                                               
         L     R4,AIOAREA          SAVE OFF REC, SO WE WON'T                    
         ST    R4,AIOSV            CLOBBER IT                                   
         LA    R4,MYIOAREA                                                      
         ST    R4,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         GOTO1 MYGETEL,DMCB,(X'02',MYIOAREA),DMCB+8                             
         CLI   DMCB,X'FF'                                                       
         BE    DISPFC50                                                         
*                                                                               
         L     R4,DMCB+8                                                        
         USING RCMTELM2,R4         COMMENT TEXT ELEMENT                         
*                                                                               
DISPFC10 DS    0H                                                               
         CLI   RCMT2LEN,3          GET FIRST NON-BLANK COMMT LINE               
         BH    DISPFC20                                                         
         CLI   RCMT2TXT,C' '                                                    
         BNE   DISPFC20                                                         
         MVI   BYTE,2              COMMENT TEXT ELEMENT                         
         BAS   RE,NEXTEL           R4 HAS ADDRESS OF FIRST ELEMENT              
         BE    DISPFC10                                                         
         B     DISPFC50                                                         
*                                                                               
DISPFC20 DS    0H                                                               
         CLM   R3,8,RCMT2LEN       COMMT FIELD HAS THIS MUCH ROOM               
         BH    DISPFC30                                                         
         LR    R1,R3               BYTE 0 HAS LENGTH                            
         SRL   R1,24               SHIFT LENGHT TO BYTE 3                       
         B     DISPFC45                                                         
DISPFC30 ZIC   R1,RCMT2LEN                                                      
DISPFC45 SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     DISPFC50                                                         
         MVC   0(0,R3),RCMT2TXT                                                 
         DROP  R4                                                               
*                                                                               
DISPFC50 L     R4,AIOSV            RESTORE                                      
         ST    R4,AIOAREA                                                       
*                                                                               
DISPFCX  DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
*                                                                               
P3N4KEYS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BACT,C'A'           TEST ADD/CHANGE                              
         BE    PK30010             ADD                                          
*                                                                               
*  DELETE OLD PASSIVE POINTER - NOT ALWAYS PRESENT!                             
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'83'                         SET RECORD ID                  
         MVI   KEY+1,X'03'                       SET RECORD SUBID               
         MVC   KEY+17(2),RSTAKREP-RSTAREC+REC2   SET REP CODE                   
         MVC   KEY+19(3),RSTAOWN-RSTAREC+REC2    SET OWNER NAME                 
         MVC   KEY+22(5),RSTAKSTA-RSTAREC+REC2   SET STATION CALL LTRS          
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   PK30010             NO  - SKIP IF NOT FOUND                      
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        SET DELETE BIT                               
         BAS   RE,WRITE            REWRITE KEY AS DELETE                        
         BAS   RE,CHECK                                                         
PK30010  EQU   *                                                                
         OC    RSTAOWN-RSTAREC+REC(3),RSTAOWN-RSTAREC+REC                       
         BZ    PK3X                     NO, DON'T ADD P-KEY                     
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'83'                         SET RECORD ID                  
         MVI   KEY+1,X'03'                       SET RECORD SUBID               
         MVC   KEY+17(2),RSTAKREP-RSTAREC+REC    SET REP CODE                   
         MVC   KEY+19(3),RSTAOWN-RSTAREC+REC     SET OWNER NAME                 
         MVC   KEY+22(5),RSTAKSTA-RSTAREC+REC    SET STATION CALL LTRS          
         MVC   KEY+28(4),BSVDA     SET DISK ADDRESS                             
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         BAS   RE,HIGH             CHECK FOR ADD/REWRITE                        
         LA    RF,ADD              SET FOR ADD                                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                 NOT EQUAL IS 'ADD'                           
         LA    RF,WRITE            EQUAL IS 'REWRITE'                           
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF               PERFORM THE FUNCTION                         
         BAS   RE,CHECK                                                         
PK3X     DS    0H                                                               
         CLI   BACT,C'A'           TEST ADD/CHANGE                              
         BE    PK40010             ADD                                          
*                                                                               
*  DELETE OLD PASSIVE POINTER - NOT ALWAYS PRESENT!                             
*                                                                               
         XC    KEY,KEY                                                          
K        USING RST6KEY,KEY                                                      
         MVI   K.RST6KTYP,X'83'                    SET RECORD ID                
         MVI   K.RST6KSTP,X'04'                    SET RECORD SUBID             
         MVC   K.RST6KREP,RSTAKREP-RSTAREC+REC2    SET REP CODE                 
         MVC   K.RST6KOWN,RSTAOWN-RSTAREC+REC2     SET OWNER NAME               
         MVC   K.RST6KSTA,RSTAKSTA-RSTAREC+REC2    SET STATION CALL             
         LA    RF,RSTAELEM-RSTAREC+REC2                                         
PK40002  EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PK40006             YES                                          
         CLI   0(RF),X'08'         EXTENDED DESCRIPTION ELEMENT?                
         BE    PK40004             YES                                          
*                                                                               
         ZIC   R0,1(RF)            NEXT ELEMENT                                 
         AR    RF,R0                                                            
         B     PK40002                                                          
*                                                                               
PK40004  DS    0H                                                               
         MVC   K.RST6KMKT,RSTAMKTC-RSTAXXEL(RF)                                 
         DROP  K                                                                
*                                                                               
PK40006  DS    0H                                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   PK40010             NO  - SKIP IF NOT FOUND                      
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        SET DELETE BIT                               
         BAS   RE,WRITE            REWRITE KEY AS DELETE                        
         BAS   RE,CHECK                                                         
PK40010  EQU   *                                                                
         OC    RSTAOWN-RSTAREC+REC(3),RSTAOWN-RSTAREC+REC                       
         BZ    PK4X                     NO, DON'T ADD P-KEY                     
*                                                                               
         XC    KEY,KEY                                                          
K        USING RST6KEY,KEY                                                      
         MVI   K.RST6KTYP,X'83'                    SET RECORD ID                
         MVI   K.RST6KSTP,X'04'                    SET RECORD SUBID             
         MVC   K.RST6KREP,RSTAKREP-RSTAREC+REC     SET REP CODE                 
         MVC   K.RST6KOWN,RSTAOWN-RSTAREC+REC      SET OWNER NAME               
         MVC   K.RST6KSTA,RSTAKSTA-RSTAREC+REC     SET STATION CALL             
         MVC   KEY+28(4),BSVDA     SET DISK ADDRESS                             
*                                                                               
         LA    RF,RSTAELEM-RSTAREC+REC                                          
PK40012  EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PK4X                YES                                          
         CLI   0(RF),X'08'         EXTENDED DESCRIPTION ELEMENT?                
         BE    PK40014             YES                                          
*                                                                               
         ZIC   R0,1(RF)            NEXT ELEMENT                                 
         AR    RF,R0                                                            
         B     PK40012                                                          
*                                                                               
PK40014  DS    0H                                                               
         MVC   K.RST6KMKT,RSTAMKTC-RSTAXXEL(RF)                                 
         DROP  K                                                                
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         BAS   RE,HIGH             CHECK FOR ADD/REWRITE                        
         LA    RF,ADD              SET FOR ADD                                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                 NOT EQUAL IS 'ADD'                           
         LA    RF,WRITE            EQUAL IS 'REWRITE'                           
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF               PERFORM THE FUNCTION                         
         BAS   RE,CHECK                                                         
PK4X     DS    0H                                                               
         CLI   BACT,C'A'           TEST ADD/CHANGE                              
         BE    PK60010             ADD                                          
***>>>                                                                          
*        PASSIVE KEY 8306                                                       
*                                                                               
*  DELETE OLD PASSIVE POINTER - NOT ALWAYS PRESENT!                             
*                                                                               
         XC    KEY,KEY                                                          
K        USING RST8KEY,KEY                                                      
         MVI   K.RST8KTYP,X'83'                    SET RECORD ID                
         MVI   K.RST8KSTP,X'06'                    SET RECORD SUBID             
         MVC   K.RST8KREP,RSTAKREP-RSTAREC+REC2    SET REP CODE                 
*                                                                               
*                                  MARKET CODE SET WHEN X'08' FOUND             
*                                                                               
         MVC   K.RST8KSTA,RSTAKSTA-RSTAREC+REC2    SET STATION CALL             
         LA    RF,RSTAELEM-RSTAREC+REC2                                         
PK60002  EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PK60006             YES                                          
         CLI   0(RF),X'08'         EXTENDED DESCRIPTION ELEMENT?                
         BE    PK60004             YES                                          
*                                                                               
         ZIC   R0,1(RF)            NEXT ELEMENT                                 
         AR    RF,R0                                                            
         B     PK60002                                                          
*                                                                               
PK60004  DS    0H                                                               
         MVC   K.RST8KMKT,RSTAMKTC-RSTAXXEL(RF)                                 
         DROP  K                                                                
*                                                                               
PK60006  DS    0H                                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   PK60010             NO  - SKIP IF NOT FOUND                      
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        SET DELETE BIT                               
         BAS   RE,WRITE            REWRITE KEY AS DELETE                        
         BAS   RE,CHECK                                                         
PK60010  EQU   *                                                                
*                                                                               
         XC    KEY,KEY                                                          
K        USING RST8KEY,KEY                                                      
         MVI   K.RST8KTYP,X'83'                    SET RECORD ID                
         MVI   K.RST8KSTP,X'06'                    SET RECORD SUBID             
         MVC   K.RST8KREP,RSTAKREP-RSTAREC+REC     SET REP CODE                 
         MVC   K.RST8KSTA,RSTAKSTA-RSTAREC+REC     SET STATION CALL             
         MVC   KEY+28(4),BSVDA     SET DISK ADDRESS                             
*                                                                               
         LA    RF,RSTAELEM-RSTAREC+REC                                          
PK60012  EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PK6X                YES                                          
         CLI   0(RF),X'08'         EXTENDED DESCRIPTION ELEMENT?                
         BE    PK60014             YES                                          
*                                                                               
         ZIC   R0,1(RF)            NEXT ELEMENT                                 
         AR    RF,R0                                                            
         B     PK60012                                                          
*                                                                               
PK60014  DS    0H                                                               
         CLC   RSTAMKTC-RSTAXXEL(4,RF),SPACES                                   
*                                  ANY MARKET CODE IN RECORD?                   
         BNH   PK6X                NO  - EXIT WITH NO KEY                       
         MVC   K.RST8KMKT,RSTAMKTC-RSTAXXEL(RF)                                 
         DROP  K                                                                
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         BAS   RE,HIGH             CHECK FOR ADD/REWRITE                        
         LA    RF,ADD              SET FOR ADD                                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                 NOT EQUAL IS 'ADD'                           
         LA    RF,WRITE            EQUAL IS 'REWRITE'                           
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF               PERFORM THE FUNCTION                         
         BAS   RE,CHECK                                                         
PK6X     DS    0H                                                               
*        PASSIVE KEY 8306                                                       
*****++++++++++                                                                 
*        PASSIVE KEY 8308                                                       
*                                                                               
*  DELETE OLD PASSIVE POINTER - NOT ALWAYS PRESENT!                             
*                                                                               
         XC    KEY,KEY                                                          
K        USING RSTUKEY,KEY                                                      
         MVI   K.RSTUKTYP,X'83'                    SET RECORD ID                
         MVI   K.RSTUKSTP,X'08'                    SET RECORD SUBID             
         MVC   K.RSTUKREP,RSTAKREP-RSTAREC+REC2    SET REP CODE                 
*                                                                               
*                                  UNIQUE ID SET WHEN X'08' FOUND               
*                                                                               
         MVC   K.RSTUKSTA,RSTAKSTA-RSTAREC+REC2    SET STATION CALL             
         LA    RF,RSTAELEM-RSTAREC+REC2                                         
PK80002  EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PK80006             YES                                          
         CLI   0(RF),X'2A'         UNIQUE ID ELEMENT?                           
         BE    PK80004             YES                                          
*                                                                               
         ZIC   R0,1(RF)            NEXT ELEMENT                                 
         AR    RF,R0                                                            
         B     PK80002                                                          
*                                                                               
PK80004  DS    0H                                                               
         MVC   K.RSTUKUID,RSTAUIST-RSTAUIEL(RF)                                 
         DROP  K                                                                
*                                                                               
PK80006  DS    0H                                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   PK80010             NO  - SKIP IF NOT FOUND                      
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        SET DELETE BIT                               
         BAS   RE,WRITE            REWRITE KEY AS DELETE                        
         BAS   RE,CHECK                                                         
PK80010  EQU   *                                                                
*                                                                               
         XC    KEY,KEY                                                          
K        USING RST8KEY,KEY                                                      
         MVI   K.RSTUKTYP,X'83'                    SET RECORD ID                
         MVI   K.RSTUKSTP,X'08'                    SET RECORD SUBID             
         MVC   K.RSTUKREP,RSTAKREP-RSTAREC+REC     SET REP CODE                 
         MVC   K.RSTUKSTA,RSTAKSTA-RSTAREC+REC     SET STATION CALL             
         MVC   KEY+28(4),BSVDA     SET DISK ADDRESS                             
*                                                                               
         LA    RF,RSTAELEM-RSTAREC+REC                                          
PK80012  EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PK8X                YES                                          
         CLI   0(RF),X'2A'         UNIQUE ID ELEMENT?                           
         BE    PK80014             YES                                          
*                                                                               
         ZIC   R0,1(RF)            NEXT ELEMENT                                 
         AR    RF,R0                                                            
         B     PK80012                                                          
*                                                                               
PK80014  DS    0H                                                               
         CLC   RSTAUIST-RSTAUIEL(4,RF),SPACES                                   
*                                  ANY UNIQUE ID IN RECORD?                     
         BNH   PK8X                NO  - EXIT WITH NO KEY                       
         MVC   K.RSTUKUID,RSTAUIST-RSTAUIEL(RF)                                 
         DROP  K                                                                
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         BAS   RE,HIGH             CHECK FOR ADD/REWRITE                        
         LA    RF,ADD              SET FOR ADD                                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                 NOT EQUAL IS 'ADD'                           
         LA    RF,WRITE            EQUAL IS 'REWRITE'                           
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF               PERFORM THE FUNCTION                         
         BAS   RE,CHECK                                                         
PK8X     DS    0H                                                               
*        PASSIVE KEY 8308                                                       
*****++++++++++                                                                 
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
*        SIDNEDT SUROUTINE - EDIT AND BUILD STATION ID NUMBER   *               
*                            ELEMENT.                           *               
*        NOTE: IN CASE OF ERRORS FIRST F OF DMCB WILL HAVE      *               
*              ERROR CODE (EX.1) AND CONDITION CODE WILL BE SET.*               
*                                                               *               
*                            ABOB ADDED                         *               
*****************************************************************               
         SPACE 1                                                                
SIDNEDT  NTR1  BASE=*,LABEL=*                                                   
* DELETE STATION ID NUMBER  ELEMENT                                             
         GOTO1 VDELELEM,DMCB,(X'0F',REC)                                        
*                                                                               
         LA    R2,FMSSTNH          POINT TO STATION ID NUMBER HEADER            
         CLI   5(R2),0             CHECK IF ANYTHING WAS ENTERED                
         BE    SIDN0561            IF NOTHING  EXIT                             
*ELSE                                                                           
         TM    FMSSTNH+4,X'08'     CHECK IF DATA NUMERIC                        
         BZ    SIDER01             IF NOT GIVE ERROR                            
*ELSE                                                                           
         ZIC   RF,5(R2)            GET LENGTH OF ENTERED ID NUMBER              
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)      CONVERT TO PACK DECIMAL                      
         CVB   R1,DUB              CONVERT TO BINERY                            
*                                                                               
         CL    R1,=4X'00'          CHECK IF DATA NOT = 0                        
         BE    SIDER01             IF = GIVE ERROR                              
*                                                                               
         LA    R5,WORK2            SET A(WORK AREA FOR ELEMENT)                 
         USING RSTAINEL,R5                                                      
         XC    WORK2(100),WORK2    CLEAR IT OUT                                 
         MVI   RSTAINEC,X'0F'      ADD ELEMENT CODE                             
         MVI   RSTAINLN,X'0C'      AND LENGTH                                   
         STCM  R1,15,RSTAINID      STORE STATION ID NUMBER                      
*                                                                               
         DROP  R5                                                               
         GOTO1 VADDELEM,DMCB,REC,(R5)                                           
*                                                                               
SIDN0561 DS    0H                  EXIT SUBROUTINE                              
*                                                                               
         LA    R0,0                                                             
SIDNEX   DS    0H                                                               
         STC   R0,DMCB              PUT 0 FOR NO ERRORS                         
         ST    R2,DMCB+4                                                        
         LTR   R0,R0                                                            
         B     EXXMOD                                                           
*                                                                               
SIDER01  DS    0H                  PUT ERROR CODE                               
         LA    R0,1                1 FOR NOT NUMERIC ID NUMBER                  
         B     SIDNEX                                                           
         SPACE 1                                                                
*                                                                               
* END OF STATION ID NUMBER VALIDATION  SUBROUTINE                               
*********************************************************************           
         EJECT                                                                  
**********************************************************************          
* ADDS OR UPDATES ACTIVITY ELEMENT                                              
**********************************************************************          
ACTINFO  NMOD1 0,*ACTINF*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
*                                  GET TODAY'S DATE                             
*                                                                               
* COULDN'T USE 'TODAY' WITH STATION X'F1' DSECT WITHIN A 'USING'.               
* THIS IS DUE TO RSTAREC ORG ON TO REC.  SO BE CAREFUL WHEN REFERENCING         
* ANYTHING IN GENOLD                                                            
*                                                                               
         MVC   USERDATE,TODAY                                                   
*                                                                               
         GOTO1 MYGETEL,DMCB,(X'F1',REC),DMCB+8                                  
         CLI   DMCB,X'FF'          ACTIVITY ELEMENT FOUND?                      
         BE    ACT0100                                                          
*                                  YES, UPDATE ACTIVITY ELEMENT                 
         L     R4,DMCB+8                                                        
         USING RSTACTEL,R4                                                      
         MVC   RSTACTCD,USERDATE   TODAY'S DATE                                 
         MVC   RSTACTCI,USERLUID   LUID THAT MADE THIS CHANGE                   
*                                                                               
         CLI   RSTACTCN,255        BUMP CHANGE COUNTER                          
         BL    ACT0010             MAX OUT AT 255 - RESET IF SO                 
         MVI   RSTACTCN,0                                                       
         B     ACTX                                                             
*                                                                               
ACT0010  DS    0H                                                               
         ZIC   RF,RSTACTCN                                                      
         LA    RF,1(RF)                                                         
         STC   RF,RSTACTCN                                                      
         B     ACTX                                                             
         DROP  R4                                                               
*                                                                               
ACT0100  DS    0H                  NO ACTIVITY ELEMENT FOUND                    
         XC    WORK2(RSTACTLQ),WORK2                                            
         LA    R4,WORK2                                                         
         USING RSTACTEL,R4                                                      
         MVI   RSTACTEC,RSTACTEQ                                                
         MVI   RSTACTLN,RSTACTLQ                                                
         MVC   RSTACTCD,USERDATE   TODAY'S DATE                                 
         MVC   RSTACTCI,USERLUID   LUID THAT MADE THIS CHANGE                   
*                                                                               
         CLI   BACT,C'A'                                                        
         BNE   ACT0150                                                          
*                                  IF ADD,                                      
         MVC   RSTACTAD,USERDATE   TODAY'S DATE                                 
         MVC   RSTACTAI,USERLUID   LUID THAT ADDED THIS                         
         DROP  R4                                                               
*                                                                               
ACT0150  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,REC,WORK2                                          
*                                                                               
ACTX     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*   SUBROUTINE TO EDIT FORMER REP/NEW REP FIELDS -- X'0C' ELEMENT               
**********************************************************************          
FNEDT    NMOD1 0,**FNEDT*,RR=R4                                                 
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
*                                                                               
         LA    R5,WORK2                                                         
         USING RSTAFNEL,R5                                                      
         MVI   RSTAFNEC,X'0C'      BUILD X'0C' ELEMENT                          
         MVI   RSTAFNLN,8                                                       
*                                                                               
         CLI   BKEY+26,C' '        IS THIS TV STATION (BAND IS EMPTY)?          
         BE    FN20                YES                                          
         CLI   BKEY+26,C'L'        IS THIS TV STATION (LOW POWER)?              
         BE    FN20                YES                                          
*                                                                               
         L     R3,=A(RAREPTAB)     LOOK AT RADIO TABLE                          
         AR    R3,R4               ADD RELO                                     
         LA    R6,L'RAREPTAB                                                    
         B     FN30                                                             
*                                                                               
FN20     EQU   *                                                                
         L     R3,=A(REPIDS)       LOOK AT TV TABLE                             
         AR    R3,R4               ADD RELO                                     
         LA    R6,L'REPIDS                                                      
*                                                                               
FN30     EQU   *                                                                
         LA    R2,FMSFORH          FORMER REP FIELD                             
         CLI   5(R2),0             IS ANYTHING THERE?                           
         BNE   *+14                YES                                          
         MVC   RSTAFNFO,SPACES     NO, MOVE IN SPACES                           
         B     FN50                EDIT NEW REP FIELD                           
         OC    FMSFOR,SPACES                                                    
         BAS   RE,VERIREP          VERIFY THE REP                               
         BNE   FNNO                INVALID REP                                  
         MVC   RSTAFNFO,FMSFOR     MOVE IN THE REP                              
*                                                                               
FN50     LA    R2,FMSNEWH          NEW REP FIELD                                
         CLI   5(R2),0             ANYTHING THERE?                              
         BNE   *+14                YES                                          
         MVC   RSTAFNNE,SPACES                                                  
         B     FN100               ADD THE ELEMENT                              
         OC    FMSNEW,SPACES                                                    
         BAS   RE,VERIREP          VERIFY THE REP                               
         BNE   FNNO                INVALID REP                                  
         MVC   RSTAFNNE,FMSNEW     MOVE IN THE REP                              
*                                                                               
         DROP  R5                                                               
FN100    GOTO1 VADDELEM,DMCB,REC,(R5)                                           
         B     FNYES               NO ERROR                                     
         SPACE 3                                                                
*                                                                               
* R6 HAS LENGTH OF EACH ENTRY IN TABLE                                          
*                                                                               
VERIREP  NTR1                                                                   
VERI10   CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    VERI20              YES, INVALID REP                             
         CLC   8(3,R2),0(R3)       CORRECT REP?                                 
         BE    FNYES                                                            
         LA    R3,0(R3,R6)         GO TO NEXT REP                               
         B     VERI10                                                           
VERI20   EQU   *                                                                
         CLC   =C'DRK',8(R2)       'DARK' STATION?                              
         BE    FNYES               YES                                          
         CLC   =C'UNK',8(R2)       'UNKNOWN' STATION?                           
         BE    FNYES               YES                                          
         CLC   =C'NON',8(R2)       'NON' STATION?                               
         BE    FNYES               NO                                           
         B     FNNO                YES, INVALID REP                             
         SPACE 1                                                                
*                                                                               
FNYES    SR    RC,RC                                                            
FNNO     LTR   RC,RC                                                            
         XIT1  REGS=(R2)           SAVE THE CURSOR POSITION                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  CYCLE THROUGH X'0A' ELEMENTS, ENSURING THAT ALL STATIONS REFERENCED          
*    ARE COMPATIBLE AS TO FORMAT/RECEIVING ID                                   
*                                                                               
CHECKFMT NMOD1 0,**CFMT**                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     RE,AIOAREA          SWAP IO AREA ADDRESSES                       
         ST    RE,AIOSV                                                         
         LA    RE,MYIOAREA         TEMPORARY IO AREA                            
         ST    RE,AIOAREA                                                       
         PRINT GEN                                                              
         GOTO1 MYGETEL,DMCB,(X'0A',RSTAREC),DMCB+8                              
         PRINT NOGEN                                                            
         CLI   DMCB,X'FF'          NO X'0A' - FINISHED                          
         BE    CFMT0090            EXIT OKAY                                    
         L     R4,DMCB+8           A(ELEMENT)                                   
CFMT0010 EQU   *                                                                
         MVC   KEY(27),RSTAREC     LOAD ORIGINAL KEY                            
         MVC   KEY+22(5),2(R4)     LOAD NEW CALL LETTERS                        
         B     CFMT0030            PROCESS THIS STATION                         
CFMT0020 EQU   *                                                                
         MVI   BYTE,X'0A'          SET FOR NEXT ELEMENT                         
         BAS   RE,NEXTEL           GET NEXT ELEMENT, IF ANY                     
         BE    CFMT0010            FOUND ONE                                    
         B     CFMT0090            NO MORE - FINISHED                           
CFMT0030 EQU   *                                                                
         BAS   RE,HIGH2            READ KEY                                     
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                 ALREADY CHECKED                              
         DC    H'0'                SHOULD BE PRESENT                            
         BAS   RE,GETREC2          READ RECORD                                  
         BAS   RE,CHECKFM2         CHECK OUT NEW RECORD                         
         BZ    CFMT0020            CC = ZERO = OKAY                             
         LA    RE,1                SET CC NOT = ZERO                            
         B     CFMT0095            NOT OKAY - EXIT                              
CFMT0090 EQU   *                                                                
         SR    RE,RE                                                            
CFMT0095 EQU   *                                                                
         L     RF,AIOSV            RESET AIOAREA                                
         ST    RF,AIOAREA                                                       
         LTR   RE,RE               SET CC FOR RETURN                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  CHECKFM2:  CHECK COMBO STATIONS FOR COMPATIBILITY                            
*                                                                               
CHECKFM2 NTR1                                                                   
         L     R2,AIOAREA          A(STATION RECORD)                            
*                                                                               
         LA    R2,RSTAELEM-RSTAREC(R2)                                          
*                                  LOOK FOR X'05' ELEMENT                       
CFM20010 EQU   *                                                                
         CLI   0(R2),5             EXTENDED DESCRIPT ELEMENT?                   
         BE    CFM20020            YES - PROCESS                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CFM20040            YES - X'05' NOT FOUND - ERROR?               
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     CFM20010            GO BACK FOR NEXT                             
CFM20020 EQU   *                                                                
         CLC   =X'FFFF',GRAPHLAG   ANY ID IN ORIGINAL?                          
         BNE   CFM20030            YES - COMPARE ID'S                           
         OC    10(2,R2),10(R2)     NO  - ANY ID IN COMBO RECORD?                
         BZ    CFM20090            NO  - EXIT OKAY                              
         B     CFM20080            YES - NO MATCH - ERROR                       
CFM20030 EQU   *                                                                
         CLC   GRAPHLAG,10(R2)     COMPARE RECEIVING IDS                        
         BNE   CFM20080            NO MATCH - ERROR                             
         B     CFM20090            MATCH    - EXIT OKAY                         
CFM20040 EQU   *                                                                
         CLC   =X'FFFF',GRAPHLAG   ANY ID IN ORIGINAL?                          
         BE    CFM20090            NO  - EXIT OKAY                              
CFM20080 EQU   *                                                                
         LA    RE,1                SET CC NOT = ZERO FOR ERROR                  
         B     CFM20095                                                         
CFM20090 EQU   *                                                                
         SR    RE,RE               SET CC = ZERO                                
CFM20095 EQU   *                                                                
         LTR   RE,RE                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  LOCAL DATAMAGR CODE                                                          
*                                                                               
HIGH2    MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY2                                                         
*                                                                               
DIRCTRY2 NTR1                                                                   
         IC    R4,DMINBTS                                                       
         IC    R3,TERMNAL                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY,    X        
               ((R3),0),0                                                       
         B     DMCHECK2                                                         
*                                                                               
GETREC2  MVC   COMMAND,=C'GETREC'                                               
         B     FILE2                                                            
*                                                                               
FILE2    NTR1                                                                   
         LA    R1,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R3,TERMNAL                                                       
         IC    R4,DMINBTS                                                       
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'REPFILE',               X        
               (R2),AIOAREA,((R3),DMWORK),0                                     
*                                                                               
DMCHECK2 MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS2                                                          
         XIT1                                                                   
*                                                                               
DMERRS2  L     RD,4(RD)            UNWIND WITHOUT EXIT                          
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3                                                            
         L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(8,DMCB)                            
         OI    6(R2),OI1C                                                       
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
         XMOD1 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  SAV0AELS:  SAVES COMBO STATION ELEMENTS FOR COMPARISON TO ENSURE             
*    ORDER IS NOT CHANGED.                                                      
*                                                                               
SAV0AELS NMOD1 0,**SV0A**                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         XC    COMBAREA,COMBAREA                                                
         LA    R2,COMBAREA         SET A(COMBO ELT AREA)                        
         LA    R1,RSTAELEM         STATION REC DESCRIPTIVE ELEMENT              
SAVA0010 EQU   *                                                                
         CLI   0(R1),X'00'         END OF RECORD?                               
         BE    SAVA0040            YES - FINISHED                               
         ZIC   R3,1(R1)            YES - TAKE LENGTH                            
         CLI   0(R1),X'0A'         COMBO ELEMENT?                               
         BNE   SAVA0020            NO  - SKIP AND LOOK AT NEXT                  
         LR    RF,R3                                                            
         BCTR  RF,0                                                             
         EX    RF,SAVA0030         MOVE FIELD BY LENGTH                         
         AR    R2,R3               BUMP COMPAREA BY LENGTH                      
SAVA0020 EQU   *                                                                
         AR    R1,R3               ADD L(ELT) AND A(ELT)                        
         B     SAVA0010            GO BACK FOR NEXT ELEMENT                     
SAVA0030 MVC   0(0,R2),0(R1)       MOVE X'0A' ELT BY LENGTH                     
SAVA0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  CHK0AELS:  THE ORDER OF THE STATIONS MAY NOT BE CHANGED.  BECAUSE            
*    THE 'PREFERRED STATION' FOR PRINT REPORTS MAY BE CHANGED, THE              
*    PREVIOUSLY VALID BIT CANNOT BE RELIED UPON TO INDICATE THAT THE            
*    STATIONS, ONCE ENTERED, ARE IN THE SAME SEQUENCE.                          
*                                                                               
CHK0AELS NMOD1 0,**CK0A**                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         LA    R1,RSTAELEM         A(DESCRIPTIVE ELT OF STATION REC)            
CHKA0010 EQU   *                                                                
         CLI   0(R1),X'00'         END OF RECORD?                               
         BE    CHKA0050            YES - EXIT OKAY                              
         CLI   0(R1),X'0A'         COMBO STA ELEMENT?                           
         BE    CHKA0020            YES -                                        
CHKA0015 EQU   *                                                                
         ZIC   RF,1(R1)            NO  - BUMP TO NEXT NEW X'0A'                 
         AR    R1,RF                                                            
         B     CHKA0010            GO BACK FOR NEXT                             
CHKA0020 EQU   *                                                                
         LA    R2,COMBAREA         A(OLD X'0A' ELEMENTS)                        
CHKA0021 EQU   *                                                                
         OC    0(4,R2),0(R2)       ANY OLD X'0A' AT LOC?                        
         BZ    CHKA0015            NO  - LOOK FOR NEXT NEW X'0A'                
         CLC   2(5,R1),2(R2)       YES - NEW VS OLD COMBO STATION               
         BE    CHKA0030            FOUND - SET DATES FROM OLD                   
         ZIC   RF,1(R2)            N.F.  - BUMP TO NEXT OLD X'0A'               
         AR    R2,RF                                                            
         B     CHKA0021            GO BACK FOR NEXT OLD                         
CHKA0030 EQU   *                                                                
         CLI   1(R2),8             OLD:  OLD FORMAT? (LEN < 9)                  
         BH    CHKA0040            NO  - NEW FORMAT                             
         XC    8(2,R1),8(R1)       YES - STATION PREVIOUSLY ENTERED,            
*                                     NO DATE IN ELT                            
         B     CHKA0015            GO BACK FOR NEXT NEW                         
CHKA0040 EQU   *                                                                
         MVC   8(2,R1),8(R2)       NEW FORMAT:  USE DATE                        
*                                     FROM OLD ELEMENT                          
         B     CHKA0015            GO BACK FOR NEXT NEW                         
CHKA0050 EQU   *                                                                
         BAS   RE,CHK0AORD         CHECK ORDER                                  
*                                                                               
*   ABOVE TO BE REMOVED WHEN STATIONS CAN BE DELETED                            
*                                                                               
         BNZ   CHKA0060            ERROR FOUND                                  
         SR    R0,R0               NO ERROR = SET CC ZERO                       
         LTR   R0,R0                                                            
CHKA0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  CHK0AORD:  THE ORDER OF THE STATIONS MAY NOT BE CHANGED.  BECAUSE            
*    THE 'PREFERRED STATION' FOR PRINT REPORTS MAY BE CHANGED, THE              
*    PREVIOUSLY VALID BIT CANNOT BE RELIED UPON TO INDICATE THAT THE            
*    STATIONS, ONCE ENTERED, ARE IN THE SAME SEQUENCE.                          
*                                                                               
CHK0AORD NTR1                                                                   
         LA    R1,RSTAELEM         A(DESCRIPTIVE ELT OF STATION REC)            
         LA    R2,COMBAREA         A(OLD X'0A' ELEMENTS)                        
CHKO0010 EQU   *                                                                
         CLI   0(R1),X'00'         END OF RECORD?                               
         BE    CHKO0040            YES - EXIT OKAY                              
         CLI   0(R1),X'0A'         COMBO STA ELEMENT?                           
         BNE   CHKO0020            NO  - GET NEXT                               
         OC    0(8,R2),0(R2)       ANY OLD X'0A' AT LOC?                        
         BZ    CHKO0040            NO  - FINISHED - EXIT OKAY                   
         CLC   2(5,R1),2(R2)       NEW VS OLD COMBO STATION                     
         BNE   CHKO0030            NOT SAME - EXIT N.G.                         
         ZIC   RF,1(R2)            BUMP TO NEXT OLD X'0A'                       
         AR    R2,RF                                                            
CHKO0020 EQU   *                                                                
         ZIC   RF,1(R1)            BUMP TO NEXT NEW X'0A'                       
         AR    R1,RF                                                            
         B     CHKO0010            GO BACK FOR NEXT                             
CHKO0030 EQU   *                                                                
         LA    RF,1                SET CC = NO GOOD                             
         LTR   RF,RF                                                            
         B     CHKO0050            EXIT                                         
CHKO0040 EQU   *                                                                
         OC    0(7,R2),0(R2)       LAST CHECK: ANY OLD X'0A' LEFT?              
         BNZ   CHKO0030            YES - TOO FEW NEW X'0A'S:  ERROR             
         SR    RF,RF               OKAY - SET CC = ZERO                         
         LTR   RF,RF                                                            
CHKO0050 EQU   *                                                                
         XIT1                                                                   
         SPACE  3                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  DISPCOMS:  DISPLAYS CONTRACT AND STATION COMMENTS.                           
*                                                                               
DISPCOMS NMOD1 0,**DCOM**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,FMSCCMH                                                       
         BAS   RE,CMTFMT           DISPLAY CONTRACT COMMENT IF ANY              
*                                                                               
         LA    R2,FMSSC1H          A(STATION COMMENT FIELD)                     
         MVC   8(L'FMSSC1,R2),SPACES                                            
         FOUT  (R2)                                                             
         LA    R2,FMSSC2H                                                       
         MVC   8(L'FMSSC2,R2),SPACES                                            
         FOUT  (R2)                                                             
         LA    R2,FMSSC1H                                                       
         GOTO1 VGETEL,DMCB,(X'0B',REC),DMCB+8                                   
         CLI   DMCB,X'FF'          ELEMENT FOUND?                               
         BE    STAD0098            NO                                           
         L     R5,DMCB+8           LOAD A(ELEMENT FOUND)                        
         ZIC   RF,1(R5)            GET ELEMENT LENGTH                           
         LA    RE,3                PREPARE FOR MOVE BY LENGTH                   
         SR    RF,RE                                                            
         EX    RF,STAD0095                                                      
         ZIC   RF,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
         CLI   0(R5),X'0B'         ANOTHER STATION COMMENT ELT?                 
         BNE   STAD0098            NO                                           
         LA    R2,FMSSC2H          YES - NEXT COMMENT FIELD ON SCREEN           
         ZIC   RF,1(R5)            GET ELEMENT LENGTH                           
         LA    RE,3                PREPARE FOR MOVE BY LENGTH                   
         SR    RF,RE                                                            
         EX    RF,STAD0095                                                      
         B     STAD0098                                                         
*                                                                               
STAD0095 EQU   *                                                                
         MVC   8(0,R2),2(R5)       MOVE BY LENGTH                               
*                                                                               
STAD0098 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
* SUB-ROUTINE FOR FORMAT OF CONTRACT COMMENT                                    
*                                                                               
CMTFMT   NTR1                                                                   
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         FOUT  (R2)                                                             
         SPACE                                                                  
         GOTO1 VGETEL,DMCB,(X'03',REC),DMCB+8                                   
         CLI   DMCB,X'FF'                                                       
         BE    CF200                NO COMMENT ELEMENT                          
         SPACE                                                                  
         L     R5,DMCB+8                                                        
         USING RSTACEL,R5                                                       
         CLI   RSTACTYP,C'M'                                                    
         BE    CF100                                                            
         MVC   8(1,R2),RSTACTYP                                                 
         MVI   9(R2),C'='                                                       
         MVC   HALF,RSTACNUM                                                    
         LH    R6,HALF                                                          
         EDIT  (R6),(4,10(R2)),ALIGN=LEFT                                       
         B     CF200                                                            
         SPACE                                                                  
CF100    ZIC   R1,RSTACLEN                                                      
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RSTACCMT                                                 
*                                                                               
         CLC   =C'C=',8(R2)        DISPLAY STORED FILE COMMENT                  
         BNE   CF200                                                            
         GOTO1 =A(DISPFC),DMCB,(RC),(RA),10(R2),(46,22(R2)),RR=Y                
*                                                                               
CF200    XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE CTGENRAD                                                       
       ++INCLUDE SEACSFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052RELFM14T  02/11/04'                                      
         END                                                                    
