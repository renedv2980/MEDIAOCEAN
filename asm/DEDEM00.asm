*          DATA SET DEDEM00    AT LEVEL 072 AS OF 11/27/19                      
*PHASE T21B00B,*                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE MEDGET                                                                 
*INCLUDE NSIWEEK                                                                
*INCLUDE NETWEEK                                                                
*INCLUDE NETUNBK                                                                
*INCLUDE TWABLD                                                                 
T21B00   TITLE 'DEDEM00 - $DEM ROOT PHASE'                                      
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* MAY15/003 046 BPOO - SUPPORT WTP MULTIBOOKS AVERAGE                 *         
*                      FIX BOOK ACTION REQUIRING STATION FIELD FOR    *         
*                      $DEM                                           *         
*               BPOO - ALLOW ARB TO FALL THROUGH TAKE OUT NOV93 CHECK *         
*                      ERROR IF NO INPUT CHUNK DUE TO FALINK LENGTH   *         
*                      ERROR.  FIX BBM BOOKTYPE DISPLAY               *         
*                                                                     *         
* AUG14/001 043 BPOO - FIX BUG     WITH VALBK                         *         
* AUG07/001 042 BPOO - STORE SPILLSTA THE ACTUAL MARKET PASSED BACK   *         
* JUN21/001 041 BPOO - FULL WORD ALIGN FALINK SAVE STORAGE AREA       *         
* MAY02/001 041 BPOO - CHANGE THE VALBK ROUTINE TO EGT RID OF FORCING *         
*                      THE USER TO TO TYPE IN BOOKTYPE FOR BBM WEEKLY *         
* MAR28/001 040 BPOO - REATE SUBROUTINE POOL FOR STEREO               *         
*                     FIX BBM BOOK VALIDATION TO SUPPORT WKLY AND MTHLY         
*                     IMS ERROR MESSAGE                               *         
* JAN02/001 37 BPOO - RADAR FILE SUPPORT                              *         
* DEC21/00 036 bpoo - fix the binary userid dnextend link             *         
* NOV07/00 035 bpoo - BBM WEEKLY SUPPORT.  CHANGED D32TBK ROUTINE     *         
* NOV04/00 034 bpoo - Support Userid for authorization                *         
* OCT26/00 033 bpoo - MAKE MOST OF DEM80 TABLES A SEPERATE PHASE      *         
* sep22/00 032 bpoo - new dedemwrk with falink displaced one line     *         
*                      lower                                                    
* Jul24/00 031 GLEE - Couple of bug fixes:                            *         
*                      1) Setting XTRA msg in error-handling routine  *         
*                      2) Use a better literal in VALWKV routine      *         
*                                                                     *         
* Jun27/00 030 GLEE - Support "VAR" and "AVGn" day input              *         
*                                                                     *         
* Jun20/00 029 GLEE - Change versioning scheme to support PC versions *         
*                                                                     *         
* May31/00 028 GLEE - Initialize TSACOM of TSAR block in init routines*         
*                                                                     *         
* May24/00 027 GLEE - Some initial support for Network Cable TP file  *         
*                                                                     *         
* May03/00 026 GLEE - Set status byte on READ/RDHI in DEMFIL          *         
*                                                                     *         
* May01/00 025 GLEE - Re-link to allow P+S1/P+S2 for PAV/STATBKS      *         
*                                                                     *         
* Mar31/00 024 GLEE - Fix bug in FMTFEDAY rtn to handle blank input   *         
*                                                                     *         
* Mar08/00 023 GLEE - Relinked for bigger buy record I/O area         *         
*              GLEE - Modify for changes to DSECTs                    *         
*                                                                     *         
* Jan03/00 022 GLEE - Clear booktype output field before BOOKVAL call *         
*                                                                     *         
* Dec14/99 021 GLEE - Use STAPACK instead of MSPACK                   *         
*                                                                     *         
* Nov12/99 020 GLEE - Remove input-validation logic from FMTFEDT rtn  *         
*                                                                     *         
* Nov03/99 019 GLEE - !!TEMPORARY CHANGE!!                            *         
*                      DEM32 has a bug where it's passing the DMA optn*         
*                       in each request.  It is causing MF $DEM to    *         
*                       kick out incompatible error msgs.  For now,   *         
*                       the compatibility check for DEM optn in DEM32 *         
*                       is suppressed.  This change needs to be rmoved*         
*                       when DEM32 is fixed.                          *         
*                                                                     *         
* Nov03/99 018 GLEE - Allow 2-char station input (e.g. FX/BOS)        *         
*                                                                     *         
* Oct26/99 017 GLEE - Change VALUPBK routine to support VPHs          *         
*                                                                     *         
* Oct21/99 016 GLEE - Reformat any day input with "," in STEREO sessn *         
*                                                                     *         
* Oct11/99 015 GLEE - Reset STMODE after posting error for DEM16      *         
*                                                                     *         
* Sep13/99 014 GLEE - Exit with error when TSAR buffer is full        *         
*              GLEE - Check FORCEEND mode on return from application  *         
*                                                                     *         
* Aug24/99 012 GLEE - Check L(data) before formatting in FMTOFLD rtn  *         
*                                                                     *         
* Aug13/99 011 GLEE - Error msg for PAV/PROGMKT non-DEM32 request     *         
*                                                                     *         
* Aug09/99 010 GLEE - Provide an error-exit in RECEIVE routine        *         
*                                                                     *         
* Jul19/99 009 GLEE - Remove error messages for Canadian BBM data     *         
*                     More enhancements to support Canadian BBM data  *         
*                                                                     *         
* Jul12/99 008 GLEE - New SMWV# routine to replace mth/wk validation  *         
*                      code in VALBK                                  *         
*                                                                     *         
* Jul08/99 007 GLEE - Chg MYSCAN rtn to accept 1st-half length ovrd   *         
*                                                                     *         
* Jun30/99 006 GLEE - Support new Canadian BBM data                   *         
*              GLEE - Fix addressibility problem w/ calling POST rtn  *         
*                                                                     *         
* Jun01/99 005 GLEE - Ignore version data from FALINK                 *         
*                                                                     *         
* May27/99 004 GLEE - Ignore version elements from FALINK             *         
*                                                                     *         
* May11/99 002 GLEE - Initialize start day to zero in D32TBK routine  *         
*                                                                     *         
* May11/99 001 GLEE - Archived source to GLDEDEM00                    *         
*                   - Moved UPDATE LOG to bottom of source code       *         
*                   - RE-leveled back to 001                          *         
***********************************************************************         
         EJECT                                                                  
DEM00    CSECT                                                                  
         NMODL DEMWRKX-DEMWRKD,**$DEM**,RA,R7,R6,RR=RE,CLEAR=YES                
         LR    R9,RC                                                            
         USING DEMWRKD,R9          R9=A(GLOBAL W/S)                             
*                                  SAVE ENTRY TIME VALUES                       
         ST    RE,BRELO                                                         
         ST    RB,ABASE                                                         
         ST    RA,BBASE                                                         
         ST    R7,CBASE                                                         
         ST    R6,DBASE                                                         
         ST    RD,AWORK                                                         
         MVC   ATWA,4(R1)                                                       
         MVC   ATIA,12(R1)                                                      
         MVC   AFAC,16(R1)                                                      
*&&DO                                                                           
* MOVED TO GETCOMS ROUTINE TO CREATE ROOM                                       
         L     R1,=A(EBREC-DEMWRKD)                                             
         LA    R1,DEMWRKD(R1)                                                   
         ST    R1,AEBREC              SET A(EBD RECORD)                         
         AH    R1,=AL2(APWORK-EBREC)  ADD LENGTH OF EBREC                       
         ST    R1,AAPWORK             SET A(APPLICATION WORK AREA)              
         LH    R1,=Y(IOAREA1-DEMWRKD)                                           
         LA    R1,DEMWRKD(R1)                                                   
         ST    R1,AIOAREA1                                                      
         LH    R1,=Y(IOAREA2-DEMWRKD)                                           
         LA    R1,DEMWRKD(R1)                                                   
         ST    R1,AIOAREA2                                                      
         LH    R1,=Y(BINTAB-DEMWRKD)                                            
         LA    R1,DEMWRKD(R1)                                                   
         ST    R1,BINATAB                                                       
         L     R1,=A(TSIOREC-DEMWRKD)                                           
         LA    R1,DEMWRKD(R1)                                                   
         ST    R1,ATSIOREC            SET A(TSAR I/O AREA)                      
*&&                                                                             
*                                                                               
*&&DO                                                                           
         L     R1,AFAC             GET ADDRESSES FROM COMFACS                   
         USING COMFACSD,R1                                                      
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VDEMOMTH,CDEMOMTH                                                
         MVC   VDEMOUT,CDEMOUT                                                  
         MVC   VDEMAND,CDEMAND                                                  
         MVC   VDEMOVAL,CDEMOVAL                                                
         MVC   VGETFACT,CGETFACT                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VGETPROF,CGETPROF                                                
         MVC   VADDAY,CADDAY                                                    
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         DROP  R1                                                               
*&&                                                                             
         GOTO1 =A(GETCOMS),RR=BRELO   GET COMFAC ADDRESSES                      
                                                                                
*                                                                               
         DS    0H                                                               
         XC    DUB,DUB             GET A(TCB)                                   
         MVC   DUB(4),=XL4'00FFFFFF'                                            
         GOTO1 VSWITCH,DUB                                                      
         MVC   ATCB,DUB                                                         
         MVI   ATCB,0               NOT IN XA MODE--CLEAR HOB                   
*                                                                               
         XC    DUB,DUB             GET A(SYSFACS)                               
         MVC   DUB(4),=XL4'FEFFFFFF'                                            
         GOTO1 VSWITCH,DUB                                                      
         MVC   ASYSFAC,DUB                                                      
         MVI   ASYSFAC,0            NOT IN XA MODE--CLEAR HOB                   
                                                                                
*                                                                               
** INITIALIZE FOR STEREO/NON-STEREO SESSION                                     
*                                                                               
         DS    0H                  CHECK IF INITIALIZED ALREADY                 
         L     R8,ATWA                                                          
         USING DEMTWAD,R8                                                       
*                                                                               
*                                                                               
*                                                                               
DEM00_05 TM    DEMFLAG1,DF1NO1ST    WERE WE IN BEFORE?                          
         BO    DEM002                YEP, INIT WAS DONE ALREADY                 
         OI    DEMFLAG1,DF1NO1ST     NO, BUT TURN ON FOR NEXT TIME              
*                                                                               
                                                                                
         DS    0H                  DETERMINE SESSION TYPE                       
         MVI   SVSCREEN,XFF         ASSUME ORIG $DEM SCREEN                     
         GOTO1 VGETFACT,DMCB,0                                                  
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         TM    FATSTAT6,TST6STRO+TST6STFU   FULL STEREO SESSION?                
         BO    DEM00_20             YES, GO INITIALIZE FOR STEREO               
*                                                                               
         DROP  R1                                                               
*                                                                               
         DS    0H                  ORIG $DEM SESSION                            
         LA    R0,DEMTABH           TRANSLATE DATA DICT ITEMS ON SCREEN         
         LA    R2,DEMMSGH                                                       
         LA    R1,DMCB                                                          
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNT       MULTIPLE TRANSLATE                          
         MVI   DDRETN,DDCASEL       LOWER CASE                                  
         MVI   DDSYS,2              DATA DICT STUFF IN SPOT SYSTEM              
         MVI   DDLANG,C' '          DEFAULT LANGUAGE                            
                                                                                
DEM00_10 DS    0H                                                               
         CR    R0,R2               R0 = END CONDITION FOR LOOP                  
         BNH   DEM00_15             EXIT LOOP WHEN END REACHED                  
         SR    RF,RF                                                            
         ICM   RF,1,0(R2)          RF=L(TWA FIELD)                              
         BZ    DEM00_15                                                         
         LR    R3,RF                HOLD ONTO IT                                
         SH    RF,=H'8'                                                         
         TM    1(R2),X02                                                        
         BZ    *+8                                                              
         SH    RF,=H'8'            RF=L(DATA PORTION OF FIELD)                  
         LA    RE,8(R2)            RE-->DATA PORTION OF FIELD                   
         STCM  RE,7,DDIADR         A(INPUT TO DICTATE)                          
         STC   RF,DDILEN           L(INPUT TO DICTATE)                          
         L     RF,AFAC                                                          
         L     RF,CDICTATE-COMFACSD(RF)                                         
         GOTO1 (RF),(R1)                                                        
         AR    R2,R3               BUMP R2 TO NEXT FIELD                        
         B     DEM00_10                                                         
*                                                                               
*                                                                               
DEM00_15 DS    0H                                                               
*&&DO                                                                           
         B     EXIT                EXIT INITIALIZATION FOR NORMAL $DEM          
*&&                                                                             
         B     XITMOD              EXIT INITIALIZATION FOR NORMAL $DEM          
         DROP  R1                                                               
*                                                                               
DEM00_20 DS    0H                  STEREO SESSION CONFIRMED                     
         OI    DEMFLAG1,DF1STERO   REMEMBER IT                                  
*                                                                               
         GOTO1 =A(ISDEM32),RR=BRELO   CHECK FOR DEM32 B4 LOADING SCRN           
*                                                                               
         OI    INPTFLG1,IF1NEW     SET FOR NEW INPUT TO COME                    
         MVI   SVSCREEN,STEOSCRN                                                
         MVI   STMODE,STMINPQ       AND SET STEREO MODE TO INPUT                
         LA    R0,DEMTWAD+64        AND LOAD SCREEN FOR STEREO                  
         XC    DMCB(24),DMCB                                                    
         GOTO1 VCALLOV,DMCB,('STEOSCRN',(R0))                                   
         CLI   DMCB+4,X'FF'        ANY ERRORS?                                  
         BNE   *+6                                                              
         DC    H'0'                 YEP, SOMETHING'S AMISS                      
                                                                                
*                                                                               
         DS    0H                  CHECK FOR DEM32 SESSION                      
         L     R2,ATIA              INPUT IN  DATA  FLD ON $CT SCREEN,          
         AHI   R2,20                 GETS MOVED TO TIA+20                       
         GOTO1 =A(ISDEM3X),RR=BRELO                                             
                                                                                
                                                                                
         DS    0H                  GET DATA DICTIONARY TERMS                    
******   LH    R1,=Y(DCLIST-DEM00)                                              
         L     R1,=A(DCLIST-DEM00)                                              
         A     R1,ABASE                                                         
         MVC   DSLIST(DCLISTX-DCLIST),0(R1)                                     
         XC    DMCB(24),DMCB                                                    
         LA    R1,DMCB                                                          
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNT         MULTIPLE TRANSLATE                        
         MVI   DDRETN,DDCASEU         UPPER CASE                                
         MVI   DDSYS,2                ITEMS IN SPOT SYSTEM DICTIONARY           
         MVI   DDLANG,C' '            DEFAULT LANGUAGE                          
         MVI   DDILEN,DSLISTX-DSLIST  INPUT STRING LENGTH                       
         LA    R0,DSLIST                                                        
         STCM  R0,7,DDIADR            A(INPUT STRING)                           
         L     RF,AFAC                                                          
         L     RF,CDICTATE-COMFACSD(RF)                                         
         GOTO1 (RF),(R1)                                                        
         DROP  R1                                                               
***************************************                                         
                                                                                
         OI    STESRVH+6,X'81'     ALLOW CONSECUTIVE HITS OF ENTER              
         TM    DEMFLAG1,DF1DEM32                                                
         BO    DEM002              DONT EXIT IF DEM32                           
         B     XITMOD              EXIT WHEN STEREO INIT IS DONE                
         DROP  R8                                                               
         EJECT                                                                  
DEM002   DS    0H                                                               
         L     R8,ATWA                                                          
         USING DEMTWAD,R8                                                       
         GOTO1 =A(DEM002M),RR=YES                                               
*                                                                               
         DS    0H                                                               
         TM    DEMFLAG1,DF1CHKFE   HAVE WE CHECKED FE SCRN FOR DEM32?           
         BNZ   DEM002G                                                          
         OI    DEMFLAG1,DF1CHKFE                                                
         CLC   =C'DEM32',STEI1ST    IF FIRST TIME TYPE DEM32                    
*&&DO                                                                           
         BE    *+12                 ELSE DEM32 FLAG MUST BE SET                 
         TM    DEMFLAG1,DF1DEM32    TO BE DEM32                                 
         BNO   DEM7                                                             
*&&                                                                             
         BNE   DEM002G                                                          
         LA    R2,STEI1STH          INPUT IN  DATA  FLD ON $CT SCREEN,          
         GOTO1 =A(ISDEM3X),RR=BRELO                                             
DEM002G  EQU   *                                                                
*&&DO                                                                           
*                                                                               
         LA    R2,STEI1STH          INPUT IN  DATA  FLD ON $CT SCREEN,          
         GOTO1 =A(ISDEM3X),RR=BRELO                                             
*&&                                                                             
*                                                                               
         TM    DEMFLAG1,DF1DEM32   IS THIS A DEM32 SESSION?                     
         BZ    DEM7                 NOPE                                        
                                                                                
         OI    STESRVH+6,X'81'     ALLOW CONSECUTIVE HITS OF ENTER              
*&&DO                                                                           
         MVC   DMCB+4(4),=X'D9000AE7' GET FALINK ADDRESS                        
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VFALINK,DMCB                                                     
*&&                                                                             
         GOTO1 =A(INIFALNK),DMCB,RR=BRELO                                       
         GOTO1 VFALINK,DMCB,FABLK                                               
         B     XITMODSV                                                         
         EJECT                                                                  
         DS    0H                  TASKS FOR STEREO SESSION                     
DEM7     TM    DEMFLAG1,DF1STERO                                                
         BZ    DEM8                                                             
*                                                                               
*                                                                               
         MVI   GOSTEON,STCTL#                                                   
         GOTO1 AGOSTEO                                                          
         OI    STESRVH+6,X'81'     ALLOW CONSECUTIVE HITS OF ENTER              
*                                                                               
         B     XITMODSV            SAVE TIA AREA & EXIT DEM00                   
*                                                                               
DEM8     DS    0H                                                               
         MVI   GOSTEON,STV#        TREAT VALIDATION & GO AS SUBROUTINES         
         GOTO1 AGOSTEO                                                          
         B     XITMODSV            SAVE TIA AREA & EXIT DEM00                   
         EJECT                                                                  
* START OF FIELD VALIDATION ROUTINES                                            
*                                                                               
VALRTNS  DS    0H                                                               
         MVI   GOSTEON,VSCR#                                                    
         GOTO1 AGOSTEO                                                          
         BNE   ERROR0                                                           
         EJECT                                                                  
* VALIDATE SYSTEM                                                               
*                                                                               
         GOTO1 =A(VALSYS),RR=YES                                                
         EJECT                                                                  
* VALIDATE ACTION CODE                                                          
*                                                                               
VALACT   MVI   HELPREQD,HELPACT                                                 
         XC    SCANDLIM,SCANDLIM                                                
         L     R2,AACTTAB                                                       
         USING ACTTABDD,R2          R2=A(ACTION TABLE)                          
VALACT1  L     R1,ADEMACT                                                       
         MVI   FERN,1                                                           
         GOTO1 FVAL,(R1)                                                        
         BNE   VALACT2                                                          
         TM    ACTI,DEFAULT        TEST IF DEFAULT ACTION PRESENT               
         BZ    ERROR                                                            
         L     R1,ADEMACT                                                       
         MVC   8(L'DEMACT,R1),ACTD SET DEFAULT ACTION                           
         OI    6(R1),X80                                                        
         B     VALACT1                                                          
*                                                                               
VALACT2  DS    0H                                                               
         L     RE,ADEMFIL                                                       
         CLC   =C'CTP',8(RE)                                                    
         BNE   VALACT2A                                                         
         CLC   =C'PER',FLD                                                      
         BE    ECUNPER                                                          
VALACT2A CLI   ACTNAME,EOT         SEARCH ACTION TABLE FOR NAME                 
         BE    EIAC                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   ACTNAME(0),FLD                                                   
         BE    *+12                                                             
         LA    R2,ACTTABLL(R2)                                                  
         B     VALACT2                                                          
*                                                                               
*&&DO                                                                           
*        SUPPORT FOR NEW STYLE SID RECORDS                                      
         CLI   ACTACTN,DEMOEST     ESTIMATE ACTION                              
         BNE   VALACT2A                                                         
         L     RE,ASYSNTRY                                                      
         CLC   0(4,RE),=C'SPOT'    EST ACTION FOR SPOT ONLY                     
         BNE   EIAC                                                             
         CLI   PROGPROF,C'N'       AND NEW FORMAT                               
         BNE   *+8                                                              
         LA    R2,ACTTABLL(R2)      USE SECOND TABLE                            
*                                                                               
*&&                                                                             
         ST    R2,AACTNTRY                                                      
         MVC   IACTN,ACTACTN                                                    
         MVC   ACTN,ACTACTN                                                     
         CLC   ACTNAME,FLD                                                      
         BE    *+18                                                             
         L     R1,ADEMACT                                                       
         MVC   8(L'DEMACT,R1),ACTNAME   DISPLAY FULL ACTION NAME                
         OI    6(R1),X80                                                        
         CLI   IACTN,NEXT          TEST FOR NEXT                                
         BNE   VALACT4                                                          
         MVC   NACTN,ACTFUNC       SAVE NEXT SCROLL ACTION                      
         CLI   DEMMODE,NEXT        ONLY VALID IF MODE=NEXT                      
         BNE   EIAS                                                             
         L     R2,AACTTAB          FIND TABLE ENTRY FOR LAST ACTION             
VALACT3  CLI   ACTNAME,EOT         TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ACTACTN,LACTN       MATCH ON LAST TIME ACTION                    
         BE    *+12                                                             
         LA    R2,ACTTABLL(R2)                                                  
         B     VALACT3                                                          
*&&DO                                                                           
*        SUPPORT FOR NEW STYLE SID RECORDS                                      
         CLI   ACTACTN,DEMOEST     ESTIMATE ACTION                              
         BNE   *+8                                                              
         CLI   PROGPROF,C'N'       AND NEW FORMAT                               
         BNE   *+8                                                              
         LA    R2,ACTTABLL(R2)      USE SECOND TABLE                            
*&&                                                                             
*                                                                               
         MVC   ACTN,ACTACTN                                                     
         ST    R2,AACTNTRY                                                      
         CLI   ACTN,HELP           NO MORE VALDATION IF NEXT FOR HELP           
         BE    GO                                                               
*                                                                               
VALACT4  TM    ACTINDS,DDSONLY                                                  
         BZ    *+12                                                             
         CLI   DDS,C'Y'                                                         
         BNE   EIAC                                                             
         TM    ACTINDS,STERONLY                                                 
         BZ    *+12                                                             
         TM    DEMFLAG1,DF1STERO                                                
         BZ    EIAC                                                             
         MVC   HELPSAVE,ACTN       SAVE THIS INPUT ACTION                       
*                                                                               
         TM    FILI,PRESET         SET FILE VALIDATION FIELDS                   
         BZ    VALACT5                                                          
         TM    ACTFILI,PRESET                                                   
         BZ    VALACT5                                                          
         CLC   FILD,ACTFILD                                                     
         BE    VALACT6                                                          
         DC    H'0'                                                             
VALACT5  TM    ACTFILI,DEFAULT+PRESET                                           
         BZ    *+10                                                             
         MVC   FILV,ACTFILI                                                     
*                                                                               
VALACT6  TM    SRCI,PRESET         SET SOURCE VALIDATION FIELDS                 
         BZ    VALACT8                                                          
         TM    ACTSRCI,PRESET                                                   
         BZ    VALACT8                                                          
         CLC   SRCD,ACTSRCD                                                     
         BE    VALACT10                                                         
         DC    H'0'                                                             
VALACT8  TM    ACTSRCI,DEFAULT+PRESET                                           
         BZ    *+10                                                             
         MVC   SRCV,ACTSRCI                                                     
*                                                                               
VALACT10 MVC   FLDN,ACTSTAM        OTHER VALIDATION CONTROLS                    
         CLI   ACTACTN,PROGMKT     IF ACTION=PROGMKT,                           
         BNE   *+16                                                             
         TM    DEMFLAG1,DF1STERO    AND THIS IS A STEREO SESSION,               
         BZ    *+8                                                              
         MVI   DEMN,MAXDEMS         CAN ENTER UP TO MAXDEMS # OF DEMOS          
                                                                                
         MVC   OVERLAY,ACTOVER                                                  
         MVC   DBFNC,ACTFUNC                                                    
         OC    OPTV,ACTOPTR                                                     
VALACTX  DS    0H                                                               
         EJECT                                                                  
* VALIDATE FILE                                                                 
*                                                                               
VALFIL   MVI   HELPREQD,HELPFIL                                                 
         XC    SCANDLIM,SCANDLIM                                                
         L     R2,AFILTAB                                                       
         USING FILTABD,R2          R2=A(FILE TABLE)                             
         MVI   DUB,0                                                            
VALFIL1  L     R1,ADEMFIL                                                       
         MVI   FERN,1                                                           
         GOTO1 FVAL,(R1)                                                        
         BNE   VALFIL2                                                          
         TM    FILI,DEFAULT+PRESET TEST DEFAULT/PRESET FILE                     
         BZ    ERROR                                                            
         L     R1,ADEMFIL                                                       
         MVC   8(L'DEMFIL,R1),FILD SET DEFAULT/PRESET FILE                      
         OI    6(R1),X80                                                        
         B     VALFIL1                                                          
*                                                                               
VALFIL2  CLI   FILNAME,EOT         SEARCH FILE TABLE FOR NAME                   
         BE    EIFN                                                             
*                                                                               
         CLC   FLD(2),=C'T4'       4 WEEK AVE/TP                                
         BNE   *+12                                                             
         MVI   FLD+1,C'P'                                                       
         MVI   DBTPTTS,C'P'                                                     
*                                                                               
         CLI   FILACTN,0           TEST ACTION SPECIFIC FILE                    
         BE    VALFIL3                                                          
         CLC   FILACTN,ACTN        TEST INPUT ACTION=TABLE ACTION               
         BNE   VALFIL4                                                          
         OI    DUB,1                                                            
VALFIL3  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FILNAME(0),FLD                                                   
         BE    *+12                                                             
VALFIL4  LA    R2,FILTABL(R2)                                                   
         B     VALFIL2                                                          
*                                                                               
         TM    DUB,1               TEST ACTION SPECIFIC ENTRY FOUND             
         BZ    *+14                                                             
         CLC   FILACTN,ACTN                                                     
         BNE   VALFIL4                                                          
*                                                                               
         ST    R2,AFILNTRY                                                      
         MVC   FILE,FILNAME                                                     
         CLC   FILNAME,FLD                                                      
         BE    *+18                                                             
         L     R1,ADEMFIL                                                       
         MVC   8(L'DEMFIL,R1),FILNAME    DISPLAY FULL FILE NAME                 
         OI    6(R1),X80                                                        
         CLC   FILNAME,=C'PAV'           IF PAV FILE REQUESTED,                 
         BNE   *+16                                                             
         L     RE,ASYSNTRY                                                      
         CLI   (SYSOVSYS-SYSTABD)(RE),8   SYSTEM MUST BE REP                    
         BNE   EIFN                                                             
         TM    FILINDS,DDSONLY     TEST DDS ONLY                                
         BZ    *+12                                                             
         CLI   DDS,C'Y'                                                         
         BNE   EIFN                                                             
         TM    FILINDS,STERONLY    TEST (FULL) STEREO ONLY                      
         BZ    *+12                                                             
         TM    DEMFLAG1,DF1STERO                                                
         BZ    EIFN                                                             
         TM    FILI,PRESET         TEST FILE HAS PRESET VALUE                   
         BZ    *+14                                                             
         CLC   FILD,FILE           YES - INPUT MUST EQ PRESET VALUE             
         BNE   EIFN                                                             
                                                                                
         DS    0H                                                               
         CLC   FILE,=C'PAV'                IF PAV FILE,                         
         BNE   VFIL010X                                                         
         CLI   ACTN,PROGNAME                AND LIST PROGRAM NAME OR            
         BE    *+8                                                              
         CLI   ACTN,PROGMKT                 LIST PRGM BY MARKET ACTION,         
         BNE   VFIL010X                                                         
         TM    DEMFLAG1,DF1STERO+DF1DEM32   IT MUST BE A DEM32 SESSION          
         BNO   EIFN                                                             
VFIL010X EQU   *                                                                
*                                                                               
         CLI   IACTN,NEXT          TEST IF NEXT IS VALID                        
         BNE   *+14                                                             
         CLC   FILE,LFILE          YES - FILE MUST EQ LFILE                     
         BNE   EKHC                                                             
*                                                                               
         TM    SRCI,PRESET         SET SOURCE VALIDATION FIELDS                 
         BZ    VALFIL6                                                          
         TM    FILSRCI,PRESET                                                   
         BZ    VALFIL6                                                          
         CLC   SRCD,FILSRCD                                                     
         BE    VALFIL7                                                          
         DC    H'0'                                                             
VALFIL6  TM    FILSRCI,PRESET                                                   
         BZ    *+10                                                             
         MVC   SRCV,FILSRCI                                                     
*                                                                               
VALFIL7  MVC   STAV(L'FLDV),FILSTAI   OTHER VALIDATION FIELDS                   
         OC    OPTV,FILOPTR                                                     
VALFILX  DS    0H                                                               
         EJECT                                                                  
* VALIDATE SOURCE                                                               
*                                                                               
VALSRC   MVI   HELPREQD,HELPSRC                                                 
         XC    SCANDLIM,SCANDLIM                                                
* The following instructions in between the two '*^^GYL' delimiters             
*  is to temporarily fix the wrong Mediafax code that STEREO passes             
*  to $DEM.  They along with this comment should be deleted when the            
*  version of STEREO with the right code is released.                           
*^^GYL                                                                          
         TM    DEMFLAG1,DF1STERO   IF FULL STEREO SESSION,                      
         BZ    *+24                                                             
         L     R1,ADEMSRC                                                       
         CLC   8(L'DEMSRC,R1),=C'MDF'  AND SOURCE IS MEDIAFAX,                  
         BNE   *+10                                                             
         MVC   8(L'DEMSRC,R1),=C'MFX'  FUDGE IT TO THE CORRECT SPELLING         
*^^GYL                                                                          
         L     R2,AFMSTAB                                                       
         USING FMSTABD,R2          R2=A(FILE/SOURCE TABLE)                      
         MVI   DUB,0                                                            
VALSRC1  L     R1,ADEMSRC                                                       
***  FUDGE THE FILE FOR NSIO ****                                               
***                                                                             
         L     R1,ADEMSRC                                                       
         CLC   =C'NSO',8(R1)                                                    
         BNE   VALSRC00                                                         
         MVC   8(3,R1),=C'NSI'                                                  
         OI    6(R1),X80                                                        
         L     R1,ADEMFIL                                                       
         MVC   FILE(3),=C'OPA'                                                  
         MVC   8(3,R1),=C'OPA'                                                  
         OI    6(R1),X80                                                        
         L     R1,ADEMSRC                                                       
*                                                                               
VALSRC00 DS    0H                                                               
*                                                                               
         MVI   FERN,29             MISSING SOURCE FIELD                         
         GOTO1 FVAL,(R1)                                                        
         BNE   VALSRC2                                                          
         TM    SRCI,DEFAULT+PRESET TEST OPTIONAL SOURCE                         
         BZ    ERROR                                                            
         BO    VALSRCX                                                          
         L     R1,ADEMSRC                                                       
         MVC   8(L'DEMSRC,R1),SRCD SET DEFAULT/PRESET SOURCE                    
         OI    6(R1),X80                                                        
         B     VALSRC1                                                          
*                                                                               
VALSRC2  CLI   FMSFIL,EOT          SEARCH TABLE FOR FILE/SOURCE/ACTION          
         BNE   VALSRC3                                                          
         TM    DUB,2               TEST SOURCE FOUND                            
         BNZ   ESFC                                                             
         B     EISC                                                             
VALSRC3  CLI   FMSACTN,0           TEST ACTION SPECIFIC                         
         BE    VALSRC4                                                          
         CLC   FMSACTN,ACTN        TEST INPUT ACTION=TABLE ACTION               
         BNE   VALSRC5                                                          
*^^GLEE - Didn't know what the point of remembering ACTION-SPECIFIC is,         
*^^GLEE -  but it was screwing me up, so I'm commenting it out.                 
*&&DO                                                                           
         OI    DUB,1                                                            
*&&                                                                             
VALSRC4  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FMSSRC(0),FLD                                                    
         BE    *+12                                                             
VALSRC5  LA    R2,FMSTABL(R2)                                                   
         B     VALSRC2                                                          
*                                                                               
*^^GLEE - Didn't know what the point of remembering ACTION-SPECIFIC is,         
*^^GLEE -  but it was screwing me up, so I'm commenting it out.                 
*&&DO                                                                           
         TM    DUB,1               TEST ACTION SPECIFIC ENTRY FOUND             
         BZ    *+14                                                             
         CLC   FMSACTN,ACTN        TEST INPUT ACTION=TABLE ACTION               
         BNE   VALSRC5                                                          
*&&                                                                             
         OI    DUB,2               INDICATE SOURCE FOUND                        
         CLC   FMSFIL,FILE                                                      
         BNE   VALSRC5                                                          
*                                                                               
         ST    R2,AFMSNTRY                                                      
         MVC   SRC,FMSSRC                                                       
         CLC   FMSSRC,FLD                                                       
         BE    *+18                                                             
         L     R1,ADEMSRC                                                       
         MVC   8(L'DEMSRC,R1),FMSSRC    DISPLAY FULL SOURCE NAME                
         OI    6(R1),X80                                                        
         TM    FMSINDS,DDSONLY     TEST DDS ONLY                                
         BZ    *+12                                                             
         CLI   DDS,C'Y'                                                         
         BNE   EISC                                                             
         TM    FMSINDS,STERONLY    TEST (FULL) STEREO ONLY                      
         BZ    *+12                                                             
         TM    DEMFLAG1,DF1STERO                                                
         BZ    EISC                                                             
         TM    SRCI,DEFAULT+PRESET                                              
         BO    VALSRC6                                                          
         TM    SRCI,PRESET                                                      
         BZ    *+14                                                             
         CLC   SRCD,SRC                                                         
         BNE   EISC                                                             
VALSRC6  CLI   IACTN,NEXT          TEST IF NEXT IS VALID                        
         BNE   *+14                                                             
         CLC   SRC,LSRC                                                         
         BNE   EKHC                                                             
*                                                                               
         LA    RE,FMSSTAI          MERGE OTHER FIELD VALIDATIONS FLAGS          
         LA    RF,STAV                                                          
         LA    R0,L'FLDV                                                        
VALSRC7  CLI   0(RE),0                                                          
         BE    VALSRC8                                                          
         TM    0(RF),PRESET                                                     
         BNZ   VALSRC8                                                          
         MVC   0(1,RF),0(RE)                                                    
VALSRC8  LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VALSRC7                                                       
         MVC   DBFIL,FMSIFIL                                                    
         MVC   DBSRC,FMSISRC                                                    
         MVC   DBMED,FMSIMED                                                    
*                                                                               
         CLI   DBMED,C'N'          T4 N/A FOR NETWORK                           
         BNE   *+12                                                             
         CLI   DBTPTTS,C'P'                                                     
         BE    VALSRC9                                                          
* THIS IS THE EASY WAY TO DO THIS, INSTEAD OF SETTING A MILLION TABLES          
* IN FOR VALIDATION - SYSCODE ACTION IS ONLY VALID FOR FUSION                   
*                                                                               
         CLI   ACTN,SYSCODE                                                     
         BNE   *+12                                                             
         CLI   DBSRC,C'F'                                                       
         BNE   EIAC                                                             
*                                                                               
*                                                                               
************ fudge for now for county coverage                                  
************ until we figure out the access  stuff                              
************ IF WE DONT FUDGE THEN WE HAVE TO MAKE SURE THIS                    
*********** LOOKS LIKE A TP FILE                                                
**       CLI   DBMED,C'U'                                                       
**       BE    VALSRC9B                                                         
**       CLI   DBSRC,C'F'                                                       
**       BE    VALSRC9B                                                         
*                                                                               
         LA    R4,DBLOCK1          TEST USER HAS ACCESS TO FILE                 
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,DBFIL                                                     
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         L     R0,AIOAREA1                                                      
         ST    R0,DBAREC                                                        
         MVC   DBCOMFCS,AFAC                                                    
         MVI   DBFUNCT,DBTSTACS                                                 
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
*                                                                               
         CLI   DBERROR,0                                                        
         BE    *+16                                                             
VALSRC9  LA    R1,DEMFILH          CURSOR TO FILE WITH ERROR                    
         ST    R1,FADR                                                          
         B     EIFN                                                             
VALSRC9B DS    0H                                                               
         OC    OPTV,FMSOPTR                                                     
*                                                                               
         L     RE,ASYSNTRY                                                      
         TM    SYSINDS-SYSTABD(RE),SYSSPT                                       
         BZ    VALSRC10                                                         
                                                                                
         LA    R0,AGYALPH          SET PARAM 1 FOR MEDGET CALL                  
         ST    R0,DMCB+0            A(ALPHA AGENCY CODE)                        
         MVI   DMCB+0,C'T'          MEDIA CODE, TV OR                           
         CLI   DBMED,C'R'                                                       
         BNE   *+8                                                              
         MVI   DMCB+0,C'R'           RADIO                                      
         GOTO1 VMEDGET,DMCB,,VDATAMGR,WORK                                      
         CLI   8(R1),X'FF'         TEST FOR ERRORS                              
         BE    EIMD                                                             
         MVC   COUNTRY,4(R1)                                                    
         CLI   COUNTRY,CANMEDQ                                                  
         BE    *+8                                                              
         MVI   COUNTRY,USAMEDQ                                                  
         MVC   AGYMED,WORK                                                      
*                                                                               
VALSRC10 DS    0H                                                               
*&&DO                                                                           
         L     R2,AACTNTRY         READ DEFINITION RECORD IF REQUIRED           
         TM    ACTINDS-ACTTABDD(R2),ACTEBOOK                                    
         BZ    VALSRCX                                                          
         LA    R3,KEY                                                           
         USING EBDD,R3             R3=A(ESTIMATE BOOK DEFINITION KEY)           
         XC    EBDKEY,EBDKEY                                                    
         MVI   EBDKTYPE,EBDKTYPQ                                                
         MVI   EBDKSUB,EBDKSUBN    EBD RECORDS SET UP AS NSI                    
         MVC   EBDKAM,AGYMED                                                    
         MVI   IOFLAG,SPT+READ+FIL                                              
         MVC   AIOAREA,AEBREC      READ INTO RESERVED AREA                      
         XC    NDXDA,NDXDA         ENSURE DIRECTORY RECORD IS READ              
         GOTO1 AIO                                                              
         BL    EIIO                TEST FOR I/O ERRORS                          
         BH    EDNF                                                             
         L     RE,AIOAREA1                                                      
         ST    RE,AIOAREA                                                       
*&&                                                                             
*                                                                               
VALSRCX  DS    0H                                                               
         EJECT                                                                  
* VALIDATE STATIONS                                                             
*                                                                               
VALSTA   MVI   HELPREQD,HELPSTA                                                 
         XC    SCANDLIM,SCANDLIM                                                
         MVC   MFLDS,STAN          SET MAX N'STATIONS                           
*                                                                               
*                                                                               
         L     R1,ADEMSTN                                                       
         L     RF,ADEMACT                                                       
*                                                                               
         CLI   ACTN,AFFIDS         AFFIDS BUYLINE CAN BE ON STATION             
         BE    *+10                OR DAYTIMES FIELD                            
         CLC   =C'AFFID',8(RF)     AFFIDS BUYLINE CAN BE ON STATION             
         BNE   *+8                 OR DAYTIMES FIELD                            
         CLI   5(R1),0             ANY INPUT ON STATIONS FIELD?                 
         BNE   *+8                                                              
         L     R1,ADEMDAT          DAYTIMES FIELD.  WE COULD ENTER              
*                                                                               
*                                                                               
         MVI   FERN,1           DEFAULT- MIISING INPUT FIELD                    
         CLC   =C'PROGM',8(RF)                                                  
         BNE   *+8                                                              
         MVI   FERN,31             MISSING PROGRAM NUMBER                       
         CLC   =C'STAT',8(RF)                                                   
         BNE   *+8                                                              
         MVI   FERN,32             MISSING MARKET                               
         CLC   =C'SPILL',8(RF)                                                  
         BNE   *+8                                                              
         MVI   FERN,30             MISSING STATION FIELD                        
         CLC   =C'PROGN',8(RF)                                                  
         BNE   *+8                                                              
         MVI   FERN,30             MISSING STATION FIELD                        
         CLC   =C'BOOKS',8(RF)                                                  
         BNE   *+8                                                              
         MVI   FERN,30             MISSING STATION FIELD                        
*                                                                               
         GOTO1 FVAL,(R1)                                                        
         BNE   VALSTA2                                                          
*                                                                               
         DS    0H                  NO INPUT IN STATIONS FIELD                   
         TM    DEMFLAG1,DF1STERO+DF1DEM32   IF DEM32 SESSION,                   
         BO    *+12                                                             
         CLI   ACTN,STATBKS                 ACTION=BOOKS                        
         BE    ERROR                        REQUIRE STATION FOR $DEM            
                                                                                
         TM    STV2,STV2OPTL        IS INPUT IN STATIONS FLD OPTIONAL?          
         BO    VALSTAX               YES                                        
         CLI   MFLDS,0             TEST ANY STATIONS REQUIRED                   
         BE    VALSTAX                                                          
                                                                                
         TM    DEMFLAG1,DF1STERO+DF1DEM32   IF DEM32 SESSION,                   
         BO    VALSTA1                       CHECK ACTION ALSO                  
         B     ERROR                                                            
                                                                                
VALSTA1  DS    0H                          STTN INPUT IS OPTIONAL IF:           
         CLI   ACTN,PROGNAME                ACTION=PROGNAME                     
         BE    VALSTAX                                                          
         B     ERROR                                                            
*                                                                               
VALSTA2  CLI   MFLDS,0             TEST ANY STATIONS TO VALIDATE                
         BE    EISTA                                                            
         TM    STAV,STAVBUYD                                                    
         BNZ   VALSTA22                                                         
*                                                                               
         ZIC   RF,MFLDS                                                         
         LA    RF,1(RF)                                                         
         GOTO1 VSCANNER,DMCB,(SCANLNTH,FLDH),((RF),AIOAREA1),C',=,/'            
         MVC   SCANDLIM,DMCB+8                                                  
         MVC   NFLDS,4(R1)         SAVE N'SCANNER LINES                         
         CLI   NFLDS,0             TEST FOR SCAN ERRORS                         
         BE    EISTA                                                            
         CLC   NFLDS,MFLDS         TEST N'STATIONS VALID                        
         BH    ETMI                                                             
         L     R2,AIOAREA1                                                      
*                                                                               
VALSTA4  DS    0H                                                               
*  NOW THAT THE FIELDS ARE DIVIDED FOR FUSION TAKE THE SECOND HALF              
* OF SCANNER FIELD AND SCAN AGAIN FOR CHARACTER "-" FOR SYSCODE                 
* USE IOAREA2 FOR THIS SCANNER CALL AND MERGE THE NUMERIC MKT# BACK             
* TO THE SCANNER BLACK OF PREVIOUS SCANBLK                                      
         BAS   RE,BUMPSCAN         VALIDATE A STATION/MARKET                    
         BH    VALSTA20                                                         
         CLI   0(R2),0             TEST L'STATION                               
         BE    EISTA                                                            
*                                                                               
         GOTOR GETSYSCD                                                         
*  FUSION SOURCE                                                                
                                                                                
* AFTER WE COME BACK SCAN THE LINE WITHOUT THE SYSCD                            
                                                                                
VALSTA2F DS    0H                                                               
*                                                                               
*&&DO                                                                           
         TM    STAV,STAVESTS       SPECIAL ESTIMATE BOOK FORMAT                 
         BO    VALSTA10                                                         
*&&                                                                             
         TM    2(R2),X'80'         CAN BE 1 THRU 4 IF NUMERIC                   
         BNZ   VALSTA4A                                                         
                                                                                
         DS    0H                  CHECK AGAINST MIN INPUT LENGTH               
*&&DO                                                                           
         MVI   DUB,3                                                            
         TM    STV2,STV2MRKT                                                    
         BZ    *+8                                                              
*&&                                                                             
         MVI   DUB,2                                                            
         TM    STAV,STAVSTAS        TEST IF PARENT STATION VALID                
         BO    VALSTA4F                                                         
         TM    STV2,STV2MRKT        TEST IF MARKET EXPECTED                     
         BO    VALSTA4F                                                         
         MVI   DUB,3                                                            
VALSTA4F DS    0H                                                               
         CLC   0(1,R2),DUB                                                      
         BL    EISTA                                                            
                                                                                
VALSTA4A DS    0H                  CHECK AGAINST MAX INPUT LENGTH               
         MVI   DUB,8                MAX LENGTH FOR NUMERIC INPUT                
         TM    STV2,STV2NMRC                                                    
         BO    VALSTA4B                                                         
         MVI   DUB,5                EXPECTING STATION INPUT                     
         TM    STV2,STV2MRKT                                                    
         BZ    VALSTA4B                                                         
         MVI   DUB,4                EXPECTING NUMERIC MARKET                    
* FOR FUSION ACTION = SYSCODE WWE CAN TAKE A 5 CHAR NUMERIC                     
* AS THE SYSCODE                                                                
         CLI   ACTN,SYSCODE                                                     
         BNE   *+8                                                              
         MVI   DUB,5                                                            
*&&DO                                                                           
         CLI   DBMED,C'U'           COUNTY COVERAGE # IS                        
         BNE   *+8                  ALLOWED TO BE 5 BYTES                       
         MVI   DUB,5                                                            
*&&                                                                             
         TM    2(R2),X'80'                                                      
         BO    VALSTA4B                                                         
         MVI   DUB,3                EXPECTING ALPHA   MARKET                    
VALSTA4B CLC   0(1,R2),DUB                                                      
         BH    EISTA                                                            
                                                                                
         TM    STAV,STAVSPIL       TEST SPILL VALID                             
         BO    *+12                                                             
         CLI   1(R2),0                                                          
         BNE   EISTA                                                            
         TM    2(R2),X'80'         TEST STATION NUMERIC                         
         BZ    VALSTA6                                                          
*                                                                               
* FOR FUSION ALWAYS ALLOW THE OPTION OF A NUMERIC STATION INPUT                 
* ALSO ALLOWS SPILL MKT FOR NUMERIC STATIONS                                    
*                                                                               
         CLI   DBSRC,C'F'                                                       
         BE    VALSTA4D                                                         
*                                                                               
         TM    STAV,STAVTOTS        YES, TEST IF NUMERIC                        
         BNZ   *+12                                                             
         TM    STV2,STV2NMRC         INPUT IS ALLOWED                           
         BZ    EISTA                                                            
*****    CLI   DBSRC,C'F'         SPILL MKT ALLOWED FOR FUSION                  
*****    BE    *+12               NUMERIC STATIONS                              
*                                                                               
         CLI   1(R2),0             AND SPILL MARKET NOT PRESENT                 
         BNE   EISTA                                                            
VALSTA4D DS    0H                                                               
*                                                                               
         ICM   R0,15,4(R2)         VALIDATE MARKET NUMBER                       
         BZ    EISTA                                                            
         L     RE,=F'9999'                                                      
*                                                                               
* IF ACTION = SYSCODE WE CAN TAKE A 5 CHAR INPUT FOR FUSION                     
         CLI   ACTN,SYSCODE                                                     
         BE    *+8                                                              
         CLI   DBMED,C'U'                                                       
         BNE   *+8                                                              
         L     RE,=F'99999'                                                     
*                                                                               
         TM    STV2,STV2NMRC       IF NUMERIC EXPECTED,                         
         BZ    *+8                                                              
         L     RE,=F'16777215'      MAKE MAX VALUE X'FFFFFF'                    
         CR    R0,RE                                                            
         BH    EFVB                                                             
         STCM  R0,15,WORK                                                       
         TM    STV2,STV2NMRC       KEEP AS NUMERIC?                             
         BO    VALSTA9              YES                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
         CLI   ACTN,SYSCODE                                                     
         BE    *+8                 DONT FORCE A "T" FOR FUSION WHEN             
         MVI   WORK+4,C'T'         ACTION = SYSCODE LISTING                     
         B     VALSTA9                                                          
*&&DO                                                                           
         CLI   DBMED,C'U'           county coverage has                         
         BE    *+8                  5 chars                                     
         B     VALSTA9                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB                                                      
         B     VALSTA9                                                          
*&&                                                                             
*                                                                               
VALSTA6  DS    0H                  INPUT IS NOT NUMERIC                         
         TM    2(R2),X'40'         CHECK IF ITS ALPHABETIC                      
         BZ    VALSTA6A             NOPE                                        
         TM    STV2,STV2MRKT        YES, EXPECTING MARKET?                      
         BZ    VALSTA6A              NOPE                                       
                                                                                
         DS    0H                  AT THIS POINT, ASSUME ALPHA MKT INPT         
         LA    R0,L'ALFMKTS                                                     
         ZIC   R1,NSTAS                                                         
         MR    R0,R0                                                            
         LA    R1,ALFMKTS(R1)                                                   
         MVC   0(L'ALFMKTS,R1),12(R2)   AND SLOT IT INTO LIST                   
         XC    WORK,WORK                                                        
         B     VALSTA9                                                          
                                                                                
VALSTA6A DS    0H                  INPUT IS NOT NUMERIC                         
         TM    STAV,STAVSTAS       TEST PARENT STATION VALID                    
         BZ    EISTA                                                            
         MVC   WORK(5),12(R2)                                                   
         XC    WORK+5(2),WORK+5                                                 
         CLI   DBMED,C'N'                                                       
         BE    VALSTA8                                                          
         CLI   0(R2),5                                                          
         BE    VALSTA7                                                          
         MVI   WORK+4,C'T'                                                      
         CLI   DBMED,C'R'          TEST RADIO                                   
         BNE   *+8                                                              
         MVI   WORK+4,C'A'                                                      
         B     VALSTA9                                                          
VALSTA7  CLI   DBMED,C'R'          TEST RADIO                                   
         BNE   VALSTA8                                                          
         CLI   WORK+4,C'A'                                                      
         BE    VALSTA9                                                          
         CLI   WORK+4,C'F'                                                      
         BE    VALSTA9                                                          
         CLI   WORK+4,C'B'          AM/AM TOTAL LINE                            
         BE    VALSTA9                                                          
         CLI   WORK+4,C'C'          AM/FM TOTAL LINE                            
         BE    VALSTA9                                                          
         CLI   WORK+4,C'D'          FM/FM TOTAL LINE                            
         BE    VALSTA9                                                          
         B     EISTA                                                            
*                                                                               
VALSTA8  DS    0H                                                               
         CLI   DBMED,C'N'                                                       
         BNE   VALSTA8D                                                         
         OC    WORK(5),SPACES      CONVERT -H, -N TO PROPER CODE                
         CLC   WORK+3(2),SPACES    FIGURE OUT STN EXTENSION FROM SRC            
         BE    VALSTA8B                                                         
         CLI   WORK+3,C'-'                                                      
         BNE   *+8                                                              
         MVI   WORK+3,C' '                                                      
         CLI   WORK+4,C'N'                                                      
         BNE   *+10                                                             
         MVC   WORK+3(2),=C'PN'                                                 
         B     VALSTA9             ELSE, LET ANYTHING THRU                      
*                                                                               
VALSTA8B CLI   DBSRC,C'H'          NHT FILE?                                    
         BNE   *+8                                                              
         MVI   WORK+4,C'H'                                                      
         CLI   DBSRC,C'D'          NAD FILE?                                    
         BNE   *+10                                                             
         MVC   WORK+3(2),=C'PN'                                                 
         CLI   DBSRC,C'K'          NTI FILE                                     
         BNE   *+8                                                              
         MVI   WORK+4,C'T'                                                      
         CLI   DBSRC,C'C'          NTI FILE                                     
         BNE   *+8                                                              
         MVI   WORK+4,C'C'                                                      
         B     VALSTA9                                                          
*                                                                               
VALSTA8D CLI   WORK+4,C'T'                                                      
         BE    VALSTA9                                                          
         TM    STAV,STAVPARS       TEST IF P+S1/P+S2 VALID                      
         BZ    EISTA                                                            
         CLI   WORK+4,C'1'                                                      
         BE    VALSTA9                                                          
         CLI   WORK+4,C'2'                                                      
         BE    VALSTA9                                                          
         B     EISTA                                                            
*                                                                               
VALSTA9  DS    0H                  VALIDATE SPILL MARKET NUMBER                 
         XC    WORK+5(2),WORK+5                                                 
         CLI   1(R2),0                                                          
         BE    VALSTA18                                                         
         XC    WORK+5(2),WORK+5                                                 
         TM    3(R2),X'C0'         TEST ALPHA OR NUMERIC INPUT                  
         BZ    EISTA                                                            
*^^NOP   TM    3(R2),X'80'         TEST SPILL MARKET NUMERIC                    
*^^NOP   BZ    EFNN                                                             
* FOR FUSION ALLOW NUMERIC STATION #                                            
         CLI   DBSRC,C'F'                                                       
         BE    *+12                                                             
         TM    2(R2),X'80'         NO SPILL FOR MARKET TOTALS                   
         BO    EISTA                                                            
         TM    3(R2),X'40'         IF ALPHA INPUT                               
         BO    VALSTA9A             GO HANDLE ALPHA MARKET                      
         ICM   R0,15,8(R2)                                                      
         BZ    EISTA                                                            
         C     R0,=F'9999'                                                      
         BH    EFVB                                                             
         STCM  R0,3,WORK+5                                                      
         CLI   DBSRC,C'R'          RADAR ONLY MARKET 1 WORKS                    
         BNE   VALSTA18                                                         
         CHI   R0,1                                                             
         BNE   EISTA                                                            
         B     VALSTA18                                                         
                                                                                
VALSTA9A DS    0H                  ALPHA MARKET INPUTTED                        
*        lets just ignore radar alpha market ...its not useful anyways          
*        since we only have one market anyhow                                   
         CLI   DBSRC,C'R'                                                       
         BNE   VALSTA9B                                                         
         CLC   =C'USA',22(R2)                                                   
         BE    VALSTA18                                                         
VALSTA9B DS    0H                                                               
         LA    R0,L'ALFMKTS                                                     
         ZIC   R1,NSTAS                                                         
         MR    R0,R0                   R1=DSPLCMNT FOR THIS ALPHA MKT           
         LA    R1,ALFMKTS(R1)                                                   
         MVC   0(L'ALFMKTS,R1),22(R2)  SLOT ALPHA MKT INTO LIST                 
         B     VALSTA18                                                         
*                                                                               
*&&DO                                                                           
VALSTA10 CLI   PROGPROF,C'N'       NEW ESTIMATE BOOK FORMAT                     
         BE    VALSTA42                                                         
         CLI   0(R2),3             SPECIAL ESTIMATE BOOK FORMAT                 
         BL    EISTA               STATION MUST BE 3 OR 4 BYTES                 
         CLI   0(R2),4                                                          
         BH    EISTA                                                            
         CLI   1(R2),1                                                          
         BNE   EMIP                                                             
         CLI   22(R2),C'1'         BUYING PERIOD MUST BE 1 THRU 9               
         BL    EMIP                                                             
         CLI   22(R2),C'9'                                                      
         BH    EMIP                                                             
*                                                                               
         MVC   AIOAREA,AIOAREA2    RD STN RCD TO GET MKT INTO IO2               
         LA    R3,KEY                                                           
         USING STAD,R3             R3=A(STATION RECORD KEY)                     
         MVI   STAKEY,C'0'         PRE-FILL KEY WITH ZEROES                     
         MVC   STAKEY+1(L'STAKEY-1),STAKEY                                      
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'T'                                                     
         MVC   STAKCALL(4),12(R2)                                               
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,AGYALPH                                                  
         MVI   IOFLAG,DIR+READ+STA                                              
         GOTO1 AIO                                                              
         BL    EIIO                                                             
         BH    ESNF                                                             
         L     R3,AIOAREA                                                       
         GOTO1 VMSPACK,DMCB,SMKT,STAKCALL,DUB                                   
*                                                                               
         LA    R3,KEY              BUILD KEY OF STATION EST. RECORD             
         USING ESDD,R3             R3=A(STATION ESTIMATE KEY)                   
         XC    ESDKEY,ESDKEY                                                    
         MVI   ESDKTYPE,ESDKTYPQ                                                
         MVI   ESDKSUB,ESDKSUBN    ESD RECORDS SET UP AS NSI                    
         MVC   ESDKAM,AGYMED                                                    
         MVC   ESDKMKT(5),DUB                                                   
         MVC   ESDKNUM,22(R2)                                                   
         L     RE,AAPWORK                                                       
         ST    RE,AIOAREA          READ RECORD INTO APWORK                      
         MVI   IOFLAG,FIL+READ+SPT                                              
         XC    NDXDA,NDXDA         FORCE DIRECTORY READ                         
         GOTO1 AIO                                                              
         BL    EIIO                                                             
         BH    ESNF                                                             
         L     RE,AIOAREA1          RESET A(I/O AREA)                           
         ST    RE,AIOAREA                                                       
*                                                                               
         L     R1,AEBREC           VALIDATE BUYING PERIOD NUMBER                
         LA    R1,DCNELEM-EBDD(R1)                                              
         SR    R0,R0                                                            
         USING EBPELEM,R1          LOCATE BUYING PERIOD ELEMENT                 
VALSTA12 CLI   EBPCODE,0                                                        
         BE    EIBP                                                             
         CLI   EBPCODE,EBPCODEQ                                                 
         BNE   *+14                                                             
         CLC   EBPNUM,22(R2)       MATCH ON INPUT BUYING PERIOD                 
         BE    *+14                                                             
         IC    R0,EBPLEN                                                        
         AR    R1,R0                                                            
         B     VALSTA12                                                         
         DROP  R1                                                               
*                                                                               
         XC    WORK(L'STAS),WORK   BUILD STATION ENTRY                          
         MVC   WORK(5),12(R2)                                                   
         MVC   WORK+5(1),22(R2)                                                 
*                                                                               
*&&                                                                             
VALSTA18 ZIC   R1,NSTAS            MOVE STATION TO OUTPUT AREA                  
         LA    R1,1(R1)                                                         
         STC   R1,NSTAS                                                         
         LA    R0,L'STAS                                                        
         MR    R0,R0                                                            
         LA    R1,STAS-L'STAS(R1)                                               
         MVC   0(L'STAS,R1),WORK                                                
         B     VALSTA4                                                          
*                                                                               
VALSTA20 CLI   IACTN,NEXT          TEST IF NEXT IS VALID                        
         BNE   VALSTAX                                                          
         CLC   NSTAS,LNSTAS                                                     
         BNE   EKHC                                                             
         ZIC   R1,NSTAS                                                         
         LA    R0,L'STAS                                                        
         MR    R0,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   STAS(0),LSTAS                                                    
         BNE   EKHC                                                             
                                                                                
         ZIC   R1,NSTAS                                                         
         LA    R0,L'ALFMKTS                                                     
         MR    R0,R0                                                            
         BCTR  R1,0                                                             
         EXCLC R1,ALFMKTS,LALFMKTS                                              
         BNE   EKHC                                                             
                                                                                
         B     VALSTAX                                                          
*                                                                               
VALSTA22 LA    R3,KEY              VALIDATE (M,)CLT,PRD,EST,STAT,LIN            
         USING CLTD,R3             R3=A(ESTIMATE HEADER KEY)                    
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         L     R2,AIOAREA1                                                      
         GOTO1 VSCANNER,DMCB,(SCANLNTH,FLDH),(7,(R2)),X'7D6B7DFF'               
         MVC   SCANDLIM,DMCB+8                                                  
         MVC   NFLDS,4(R1)         SAVE N'SCANNER LINES                         
         CLI   NFLDS,0             TEST FOR SCAN ERRORS                         
         BE    EISTA                                                            
*                                                                               
         CLI   NFLDS,7             ALLOW FOR 7 FIELDS                           
         BE    VALSTA24            6 ORIGINAL + SYSCODE                         
*                                                                               
*                                                                               
*                                                                               
         CLI   NFLDS,6                                                          
         BH    EISTA                                                            
         BE    VALSTA24                                                         
         CLI   NFLDS,5                                                          
         BNE   EISTA                                                            
         MVC   BUYAGM,AGYMED                                                    
         B     VALSTA26                                                         
*                                                                               
*VALSTA24 BAL   RE,BUMPSCAN         VALIDATE MEDIA CODE                         
VALSTA24 BAS   RE,BUMPSCAN         VALIDATE MEDIA CODE                          
         CLI   1(R2),0                                                          
         BNE   EISTA                                                            
         CLI   0(R2),1             MEDIA IS ONE BYTE LONG                       
         BH    EFTL                                                             
         BL    EISTA                                                            
         GOTO1 VMEDGET,DMCB,(12(R2),AGYALPH),VDATAMGR,WORK                      
         MVC   BUYAGM,WORK                                                      
         CLI   8(R1),X'FF'         TEST MEDIA CODE IS VALID                     
         BE    EIMD                                                             
         CLI   DBMED,C'R'          IF RADIO,                                    
         BNE   VALSTA26                                                         
         TM    BUYAGM,X'01'         LOW-ORDER BIT SHOULD BE OFF                 
         BNZ   EIMD                                                             
*                                                                               
**VALSTA26 BAL   RE,BUMPSCAN         VALIDATE CLIENT                            
VALSTA26 BAS   RE,BUMPSCAN         VALIDATE CLIENT                              
         CLI   1(R2),0                                                          
         BNE   EISTA                                                            
         CLI   0(R2),2             CLIENT CODE IS 2 OR 3 BYTES LONG             
         BL    EFTS                                                             
         CLI   0(R2),3                                                          
         BH    EFTL                                                             
         MVC   BUYCLT,12(R2)                                                    
         GOTO1 VCLPACK,DMCB,BUYCLT,BUYCLTK                                      
         CLI   0(R1),0             TEST CLIENT CODE IS VALID                    
         BNE   EICL                                                             
         MVC   CKEYAM,BUYAGM                                                    
         MVC   CKEYCLT,BUYCLTK                                                  
*                                                                               
         MVC   AIOAREA,AIOAREA2    SET A(IO2)                                   
         MVI   IOFLAG,FIL+READ+SPT                                              
         XC    NDXDA,NDXDA         FORCE DIRECTORY READ                         
         GOTO1 AIO                                                              
         BL    EIIO                                                             
         BH    ECNF                                                             
         L     R3,AIOAREA          R3=A(CLIENT HEADER RECORD)                   
         L     R0,AIOAREA1          RESET A(I/O AREA)                           
         ST    R0,AIOAREA                                                       
         MVC   BUYCLTO,COFFICE     EXTRACT CLIENT OFFICE CODE                   
*                                                                               
         OC    SRC,SRC             TEST SOURCE CODE SET                         
         BNZ   VALSTA28                                                         
         MVC   SRC,=C'NSI'                                                      
         CLI   CPROF+3,C'0'                                                     
         BE    *+10                                                             
         MVC   SRC,=C'ARB'                                                      
         CLI   COUNTRY,CANMEDQ                                                  
         BNE   VALSTA28                                                         
         MVC   SRC,=C'CSI'                                                      
         CLI   CPROF+3,C'0'                                                     
         BE    *+10                                                             
         MVC   SRC,=C'BBM'                                                      
*                                                                               
**VALSTA28 BAL   RE,BUMPSCAN         VALIDATE PRODUCT CODE                      
VALSTA28 BAS   RE,BUMPSCAN         VALIDATE PRODUCT CODE                        
         CLI   1(R2),0                                                          
         BNE   EISTA                                                            
         CLI   0(R2),2             PRODUCT IS 2 OR 3 BYTES LONG                 
         BL    EFTS                                                             
         CLI   0(R2),3                                                          
         BH    EFTL                                                             
         MVC   BUYPRD,12(R2)                                                    
         LA    R1,CLIST                                                         
VALSTA30 CLI   0(R1),C'A'                                                       
         BL    EPNF                                                             
         CLC   BUYPRD,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     VALSTA30                                                         
         MVC   BUYPRDN,3(R1)                                                    
         MVC   BUYPRDK,3(R1)                                                    
*                                                                               
**       BAL   RE,BUMPSCAN         VALIDATE ESTIMATE NUMBER                     
         BAS   RE,BUMPSCAN         VALIDATE ESTIMATE NUMBER                     
         CLI   1(R2),0                                                          
         BNE   EISTA                                                            
         CLI   0(R2),1             ESTIMATE IS 1 THRU 3 BYTES LONG              
         BL    EISTA                                                            
         CLI   0(R2),3                                                          
         BH    EISTA                                                            
         TM    2(R2),X'80'                                                      
         BZ    EFNN                                                             
         OC    4(4,R2),4(R2)       TEST ZERO INPUT                              
         BZ    EFVS                                                             
         OC    4(3,R2),4(R2)       TEST GR 255                                  
         BNZ   EFVB                                                             
         MVC   BUYEST,7(R2)                                                     
*                                                                               
         LA    R3,KEY                                                           
         USING ESTD,R3             R3=A(ESTIMATE HEADER KEY)                    
         XC    EKEY,EKEY                                                        
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BUYAGM                                                    
         MVC   EKEYCLT,BUYCLTK                                                  
         MVC   EKEYPRD,BUYPRD                                                   
         MVC   EKEYEST,BUYEST                                                   
         MVC   AIOAREA,AIOAREA2                                                 
         MVI   IOFLAG,SPT+READ+FIL READ ESTIMATE HEADER RECORD                  
         XC    NDXDA,NDXDA         FORCE DIRECTORY READ                         
         GOTO1 AIO                                                              
         BL    EIIO                TEST FOR I/O ERRORS                          
         BH    ENOF                                                             
         L     R3,AIOAREA          R3=A(ESTIMATE HEADER RECORD)                 
         L     R0,AIOAREA1                                                      
         ST    R0,AIOAREA                                                       
*                                  BUILD DEMO LIST FROM RECORD                  
         ZIC   R0,DEMN             R0=MAX N'DEMOS                               
         LA    R1,EDEMLST          POINT TO ESTHDR DEMO LIST                    
         LA    RE,DEMS             RE=A(OUTPUT DEMO LIST AREA)                  
         SR    RF,RF               RF=N'OUTPUT LIST ENTRIES                     
VALSTA32 OC    0(2,R1),0(R1)       TEST E-O-L                                   
         BZ    VALSTA34                                                         
         CLI   1(R1),21            TEST FOR USER DEMO                           
         BE    VALSTA33            YES - SKIP                                   
         CLI   1(R1),63            TEST FOR WEIGHTED DEMO                       
         BE    VALSTA33            YES - SKIP                                   
         MVC   0(3,RE),0(R1)       MOVE ENTRY TO OUTPUT LIST                    
         LA    RF,1(RF)                                                         
         LA    RE,3(RE)                                                         
VALSTA33 LA    R1,3(R1)                                                         
         BCT   R0,VALSTA32                                                      
*                                                                               
VALSTA34 STC   RF,NDEMS            SET N'DEMOS IN LIST                          
         MVI   0(RE),X'FF'         SET LIST TERMINATOR                          
*                                                                               
         LA    R3,KEY                                                           
         USING ESTD,R3             R3=A(ESTIMATE HEADER KEY)                    
         XC    EKEY,EKEY           BUILD KEY OF POOL ESTIMATE                   
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BUYAGM                                                    
         MVC   EKEYCLT,BUYCLTK                                                  
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,BUYEST                                                   
         MVC   AIOAREA,AIOAREA2                                                 
         MVI   IOFLAG,DIR+READ+SPT                                              
         GOTO1 AIO                 READ POOL EST. DIRECTORY RECORD              
         BNL   *+6                                                              
         DC    H'0'                                                             
         BH    *+8                                                              
         MVI   BUYPRDK,X'FF'       IF FOUND SET POOL PRODUCT NUMBER             
         L     R0,AIOAREA1                                                      
         ST    R0,AIOAREA                                                       
*                                                                               
**       BAL   RE,BUMPSCAN         VALIDATE STATION                             
*                                                                               
         BAS   RE,BUMPSCAN         VALIDATE STATION                             
         XC    DBSTABK,DBSTABK                                                  
         CLI   1(R2),0                                                          
         BNE   EISTA                                                            
         ST    R2,FULL2            HOLD ONTO A(SCANNER TABLE)                   
         MVC   DUB2(L'SCANDLIM),SCANDLIM                                        
         CLI   0(R2),0             ANY STATION INPUT?                           
         BE    EISTA                                                            
         XC    WORK,WORK           BUILD DUMMY TWA HEADER & FIELD               
         ZIC   R1,0(R2)             TO CHECK STATION INPUT VIA SCANNER          
         STC   R1,WORK+5           TWA HDR: DATA LENGTH                         
         BCTR  R1,0                                                             
         EXMVC R1,WORK+8,12(R2)    TWA FLD: DATA                                
         LA    R1,8+1(R1)                                                       
         STC   R1,WORK             TWA HDR: TWA FIELD LENGTH                    
         LA    R2,DEMWRKD          USE APWORK FOR THIS SCANNER                  
         AH    R2,=Y(APWORK-DEMWRKD)                                            
         GOTO1 VSCANNER,DMCB,(0,WORK),(1,(R2)),X'6B7E6B61'                      
         MVC   SCANDLIM,DMCB+8                                                  
         CLI   4(R1),0             CHECK FOR SCANNER ERRORS                     
         BE    EISTA                                                            
         CLI   1(R2),0             SEE IF DIVIDED FIELD                         
         BE    VLSTA36B             IT'S NOT, JUST VALIDATE STATION             
         CLI   DBMED,C'T'           WE HAVE TO SUPPORT SPILL FOR CABLE          
         BE    *+8                                                              
         CLI   DBMED,C'R'           IT IS, VALID FOR RADIO THOUGH               
         BNE   EISTA                                                            
*^^NOP   TM    3(R2),X'80'         TEST VALID NUMERICS                          
*^^NOP   BZ    EIIF                                                             
         TM    3(R2),X'C0'         TEST VALID NUMERICS OR ALPHABETICS           
         BZ    EISTA                                                            
         TM    3(R2),X'40'                                                      
         BO    VLSTA36A                                                         
         MVC   DBSTABK(2),8+2(R2)  USE DBSTABK TO HOLD BINARY MKT#              
         B     VLSTA36B                                                         
VLSTA36A CLI   1(R2),2             ALPHA MARKET IS 2 TO 3 BYTES LONG            
         BL    EISTA                                                            
         CLI   1(R2),3                                                          
         BH    EISTA                                                            
         MVC   ALFMKTS,22(R2)          USE ALFMKTS TO HOLD ALPHA MKT            
         B     VLSTA36B                                                         
                                                                                
VLSTA36B DS    0H                                                               
         CLI   0(R2),3             STATION IS 3 TO 5 BYTES LONG                 
         BL    EISTA                                                            
         BH    *+8                                                              
         MVI   12+3(R2),C' '       pad in a blank if 3                          
         CLI   0(R2),5                                                          
         BH    EISTA                                                            
         LA    R3,KEY              READ STATION RECORD TO GET MARKET            
         USING STAD,R3             R3=A(STATION RECORD KEY)                     
         MVI   STAKEY,C'0'         PRE-FILL KEY WITH ZEROES                     
         MVC   STAKEY+1(L'STAKEY-1),STAKEY                                      
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'R'                                                     
         CLI   DBMED,C'R'                                                       
         BE    *+8                                                              
         MVI   STAKMED,C'T'                                                     
         MVC   STAKCALL(4),12(R2)                                               
         MVC   STAKCALL,12(R2)                                                  
         CLI   DBMED,C'R'                                                       
         BE    *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
*                                                                               
* ADD SUPPORT FOR CABLE BUY LOOKUPS.  IN ORDER TO PROCESS SYSCODES              
* SYSCODE IS ENTER AFTER THE LINE NUMBER- LETS BUMP PASS TO THE                 
* SYSCODE FIELD FROM SCANNER BLOCK TO GRAB THE SYSCODE TO READ                  
* THE SPOT STATION RECORDS WITH THEN RESTORE THE SCANNER BLOCK POINTER          
* BACK THE THE STATIONS FIELD AND PROCEED.                                      
* ONLY ALLOW SYSCODE IF THE MEDIA=T AND SPILL MLK SET WHICH INDICATES           
* THIS IS A CABLE AFFIDS LOOKUP                                                 
*                                                                               
         CLI   NFLDS,7             7 FIELDS - WE HAVE SYSCODE AT END            
         BNE   VLSTA36F                                                         
         ST    R2,FULL             SAVE OFF BUMPSCAN ADDRESS                    
         L     R2,FULL2            RESTORE R2 TO ORIG SCANNER TABLE             
         MVC   SCANDLIM,DUB2       RESTORE ORIG SCANNER OVERRIDES               
         BAS   RE,BUMPSCAN                                                      
         BAS   RE,BUMPSCAN         BUMP TO LAST FIELD                           
         MVC   SYSCODES,6(R2)      STORE HEADEND/SYSCODE                        
         L     R2,FULL                                                          
                                                                                
         ZICM  RF,SYSCODES,(3)   GRAB HEADEND/SYSCODE AND READ                  
         CVD   RF,DUB            STATIONS RECORD.  SPOT STATION                 
         UNPK  STAKCALL(4),DUB   MASTER RECORDS STORES CABLE STATION            
         OI    STAKCALL+3,X'F0'  WITH HEADEND.                                  
         MVI   STAKCALL+4,C'T'                                                  
*                                                                               
VLSTA36F DS    0H                                                               
         MVC   STAKAGY,AGYALPH                                                  
         MVC   STAKCLT,BUYCLT                                                   
         MVC   AIOAREA,AIOAREA2                                                 
         MVI   IOFLAG,DIR+READ+STA                                              
         GOTO1 AIO                                                              
         BL    EIIO                                                             
         BH    ESNF                                                             
*                                                                               
         L     R3,AIOAREA                                                       
*                                                                               
*  RESET STATION IF SYSCODE WAS SET BECAUSED WE MOVED THE SYSCODE INTO          
*  THE STATIONS FIELD TO READ THE SPOT STATION RECORD                           
         OC    SYSCODES,SYSCODES                                                
         BZ    VLSTA37G                                                         
         MVC   STAKCALL,12(R2)                                                  
         MVI   STAKCALL+4,C'T'                                                  
*                                                                               
VLSTA37G DS    0H                  USE STAPACK TO PACK MKT/STTN                 
         LA    RF,WORK                                                          
         USING STAPACKD,RF                                                      
         XC    STAPACKD(32),STAPACKD                                            
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGYALPH                                                  
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPMED,COUNTRY      (DBMED MAY NOT BE SET YET)                  
         MVC   STAPACOM,AFAC                                                    
         MVC   STAPQMKT,SMKT                                                    
         MVC   STAPQSTA,STAKCALL                                                
*                                                                               
         CLI   NFLDS,7             7 FIELDS - WE HAVE SYSCODE AT END            
         BNE   VLSTA37J                                                         
         ZICM   RE,SYSCODES,(3)   GRAB HEADEND/SYSCODE                          
         CVD   RE,DUB                                                           
         UNPK  STAPQSTA(4),DUB                                                  
         OI    STAPQSTA+3,X'F0'                                                 
         MVI   STAPQSTA+4,C'T'                                                  
         MVC   STAPQNET,STAKCALL                                                
*                                                                               
VLSTA37J DS    0H                  USE STAPACK TO PACK MKT/STTN                 
                                                                                
*                                                                               
         DROP  RF                                                               
         GOTO1 VSTAPACK,WORK                                                    
         CLI   WORK+(STAPERR-STAPACKD),0                                        
         BNE   EISTA                                                            
         MVC   BUYSTM,WORK+(STAPMKST-STAPACKD)                                  
         MVC   BUYSTA,STAKCALL                                                  
         L     R0,AIOAREA1          RESET A(I/O AREA)                           
         ST    R0,AIOAREA                                                       
         L     R2,FULL2            RESTORE R2 TO ORIG SCANNER TABLE             
         MVC   SCANDLIM,DUB2       RESTORE ORIG SCANNER OVERRIDES               
*                                                                               
         BAS   RE,BUMPSCAN         VALIDATE LINE NUMBER                         
         CLI   1(R2),0                                                          
         BNE   EISTA                                                            
         CLI   0(R2),1             LINE NUMBER 1 THRU 3 BYTES LONG              
         BL    EISTA                                                            
         CLI   0(R2),3                                                          
         BH    EISTA                                                            
         TM    2(R2),X'80'                                                      
         BZ    EFNN                                                             
         OC    4(4,R2),4(R2)       TEST ZERO INPUT                              
         BZ    EFVS                                                             
***      OC    4(3,R2),4(R2)       TEST GR 255                                  
****     BNZ   EFVB                                                             
*                                                                               
         MVC   BUYLIN,7(R2)                                                     
*                                                                               
         LA    R3,KEY              BUILD BUY KEY                                
         USING BUYD,R3                                                          
         XC    BUYKEY,BUYKEY                                                    
         MVC   BUYKAM,BUYAGM                                                    
         MVC   BUYKCLT,BUYCLTK                                                  
         MVC   BUYKPRD,BUYPRDK                                                  
         MVC   BUYMSTA,BUYSTM                                                   
         MVC   BUYKEST,BUYEST                                                   
***      MVC   BUYKBUY+1(1),BUYLIN                                              
         MVC   BUYKBUY+1(2),6(R2)  2 BYTE BUYLINE                               
*                                                                               
         MVC   AIOAREA,AEBREC      READ RECORD INTO SPECIAL AREA                
         MVI   IOFLAG,DIR+HIGH+SPT                                              
         GOTO1 AIO                                                              
         BL    EIIO                                                             
         BH    EBNF                                                             
         L     R3,AIOAREA                                                       
         CLC   BUYKEY(L'BUYKEY-1),KEYSAVE                                       
         BNE   EBNF                                                             
         MVI   IOFLAG,SPT+READ+FIL                                              
         GOTO1 AIO                                                              
         BNE   EIIO                                                             
         L     RE,AIOAREA1          RESET A(I/O AREA)                           
         ST    RE,AIOAREA                                                       
*                                                                               
         MVC   BUYSTR,BDSTART      EXTRACT BUY START & END DATES                
         MVC   BUYEND,BDEND                                                     
         MVI   NSTAS,1             SET N'STATIONS FOR VALDEM ROUTINE            
*                                                                               
         OC    AFMSNTRY,AFMSNTRY   TEST SOURCE WAS ENTERED                      
         BNZ   VALSTA38                                                         
*                                                                               
         MVC   DBFIL,FILE          IF NO SOURCE, SET DBLOCK VALUES              
         MVC   DBSRC,SRC                                                        
         MVC   DBMED,COUNTRY                                                    
*                                                                               
VALSTA38 DS    0H                                                               
         CLI   DBMED,C'R'          IF RADIO,                                    
         BNE   VALSTA40                                                         
         OC    DBSTABK(2),DBSTABK   SHOULD BE A SPILL MKT# HERE                 
         BNZ   VALSTA40                                                         
         OC    ALFMKTS,ALFMKTS        OR AN ALPHA MKT HERE                      
         BNZ   VALSTA40                                                         
****     BAS   RE,RADSPLMK         IF NONE, GO GET IT (INTO ALFMKTS)            
         GOTOR RADSPLMK            IF NONE, GO GET IT (INTO ALFMKTS)            
*                                                                               
VALSTA40 CLI   IACTN,NEXT          TEST IF NEXT IS VALID                        
         BNE   VALSTAX                                                          
         CLC   BUYVALS(BUYVALSL),LBUYVALS                                       
         BNE   EKHC                                                             
         CLC   LALFMKTS,ALFMKTS                                                 
         BNE   EKHC                                                             
         B     VALSTAX                                                          
         DROP  R3                                                               
*&&DO                                                                           
* EDITS FOR NEW SID ESTIMATE ACTION                                             
VALSTA42 CLI   0(R2),3             SPECIAL ESTIMATE BOOK FORMAT                 
*****    BL    EIIF                STATION MUST BE 3 OR 4 BYTES                 
         BL    EISTA               STATION MUST BE 3 OR 4 BYTES                 
         CLI   0(R2),4                                                          
*****    BH    EIIF                                                             
         BH    EISTA                                                            
         CLI   1(R2),3                                                          
         BH    EMIP                                                             
*                                                                               
         XC    STAS,STAS                                                        
         MVC   NSIDSTA,12(R2)      SAVE SID STATION                             
         MVC   AIOAREA,AIOAREA2    RD STN RECD TO GET MKT IN IO2                
         LA    R3,KEY                                                           
         USING STAD,R3             R3=A(STATION RECORD KEY)                     
         MVI   STAKEY,C'0'         PRE-FILL KEY WITH ZEROES                     
         MVC   STAKEY+1(L'STAKEY-1),STAKEY                                      
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'T'                                                     
         MVC   STAKCALL(4),12(R2)                                               
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,AGYALPH                                                  
         MVI   IOFLAG,DIR+READ+STA                                              
         GOTO1 AIO                                                              
         BL    EIIO                                                             
         BH    ESNF                                                             
         L     R3,AIOAREA                                                       
         GOTO1 VMSPACK,DMCB,SMKT,STAKCALL,DUB                                   
         MVC   NSIDSTAB,DUB+2                                                   
*                                                                               
         MVC   NSIDSCHM,=C'ALL'    SET DEFAULT SCHEME                           
**       BAL   RE,BUMPSCAN                                                      
         BAS   RE,BUMPSCAN                                                      
         CLI   0(R2),3                                                          
         BH    VALSTA44            TRY PERIOD                                   
         CLI   0(R2),2                                                          
         BL    EMIF                                                             
         CLC   12(3,R2),=C'ALL'                                                 
         BE    VALSTA43                                                         
         MVC   NSIDSCHM,12(R2)                                                  
         GOTO1 VCLPACK,DMCB,NSIDSCHM,NSIDSCHB                                   
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),AGYMED                                                  
         MVC   KEY+2(2),NSIDSCHB                                                
         L     RE,AAPWORK                                                       
         ST    RE,AIOAREA          READ RECORD INTO AWORK                       
         MVI   IOFLAG,FIL+READ+SPT                                              
         XC    NDXDA,NDXDA         FORCE DIRECTORY READ                         
         GOTO1 AIO                                                              
         BL    EIIO                                                             
         BH    ESNF                                                             
*VALSTA43 BAL   RE,BUMPSCAN                                                     
VALSTA43 BAS   RE,BUMPSCAN                                                      
VALSTA44 L     RE,AIOAREA1         RESET IO AREA                                
         ST    RE,AIOAREA                                                       
*                                                                               
         CLC   12(4,R2),=C'PER='   VALIDATE PERIOD                              
         BNE   EMIF                                                             
         ZIC   RE,0(R2)            CHECK THE LENGTH                             
         SH    RE,=H'5'                                                         
         BM    EMIP                                                             
         MVC   NSIDPER,16(R2)      SAVE PERIOD                                  
         CLI   1(R2),0             ANY YEAR                                     
         BE    VALSTA48            NO - GET OUT                                 
         CLI   1(R2),2             CHECK THE LENGTH                             
         BNE   EMIP                                                             
         TM    3(R2),X'80'         AND FOR NUMERIC                              
         BNO   EMIP                                                             
         MVC   NSIDYR,11(R2)       SAVE THE BINARY YEAD                         
*                                                                               
VALSTA48 MVI   NSTAS,1             MOVE STATION TO OUTPUT AREA                  
         B     VALSTA4                                                          
*                                                                               
*&&                                                                             
*                                                                               
VALSTAX  DS    0H                                                               
         B     VALBK                                                            
*                                                                               
NSTATBL  DC    C'D',C'PN'                                                       
         DC    C'K',C' T'                                                       
         DC    C'H',C' H'                                                       
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
* MAKE THIS SEPERATE TO CREAT A LIL MORE ROOM    BPOO 10/18                     
         EJECT                                                                  
* VALIDATE BOOK                                                                 
*                                                                               
VALBK    MVI   HELPREQD,HELPBK                                                  
         XC    SCANDLIM,SCANDLIM                                                
         MVC   MFLDS,BKN           SET MAX N'BOOKS                              
         L     R1,ADEMBOK                                                       
         MVI   FERN,27             MISSING BOOKS FIELD                          
         GOTO1 FVAL,(R1)                                                        
         BNE   VALBK2                                                           
                                                                                
                                                                                
         CLI   MFLDS,0             TEST ANY BOOKS REQUIRED                      
         BE    VALBKX                                                           
                                                                                
                                                                                
         TM    DEMFLAG1,DF1STERO+DF1DEM32   IF DEM32 SESSION,                   
         BO    VALBK1                        CHECK ACTION ALSO                  
         B     ERROR                                                            
                                                                                
VALBK1   DS    0H                          BOOK INPUT IS OPTIONAL IF:           
                                                                                
         CLI   ACTN,MARKETS                 ACTION=MARKET                       
         BE    VALBKX                                                           
         CLI   ACTN,STATION                 ACTION=STATION                      
         BE    VALBKX                                                           
         CLI   ACTN,SPILLTO                 ACTION=SPILL                        
         BE    VALBKX                                                           
         CLI   ACTN,DEMOLST                 ACTION=DEMO                         
         BE    VALBKX                                                           
         B     ERROR                       ELSE, BOOK INPUT IS NEEDED           
                                                                                
*                                                                               
VALBK2   CLI   MFLDS,0             TEST ANY BOOKS TO VALIDATE                   
         BE    EIIFBK                                                           
         MVI   SCAN1HL,11          1ST HALF FLD OF INPUT CAN BE ELEVEN          
         ZIC   RF,MFLDS                                                         
         LA    RF,1(RF)                                                         
*&&DO                                                                           
         GOTO1 VSCANNER,DMCB,(SCANLNTH,FLDH),((RF),AIOAREA1),C',=,-'            
*&&                                                                             
         ZIC   R0,FLDH+5                                                        
         STCM  R0,3,FULL+0         L(INPUT STRING) PARAMETER                    
         MVC   FULL+2(1),SCAN1HL   OVRD L(1ST HALF FLD) PARAMETER               
         GOTO1 AMYSCAN,DMCB,(SCANLNTH,FLD),((RF),AIOAREA1),C',=,-',FULL         
         MVC   SCANDLIM,DMCB+8                                                  
         CLI   4(R1),0             CHECK FOR GENERIC INVALID INPUT              
         BE    EIIFBK                                                           
         MVC   NFLDS,4(R1)         SAVE N'SCANNER LINES                         
         CLC   NFLDS,MFLDS         TEST N'BOOKS VALID                           
         BH    ETMI                                                             
         L     R2,AIOAREA1                                                      
*                                                                               
*VALBK4   BAL   RE,BUMPSCAN                                                     
VALBK4   BAS   RE,BUMPSCAN                                                      
         BH    VALBK10                                                          
         CLI   0(R2),0             TEST L'BOOK                                  
         BE    EIIFBK                                                           
*        CLI   0(R2),11            MAX INPUT SIZE IS "MMMDD/YY(B)"              
         CLI   0(R2),12            MAX INPUT SIZE IS "MMMDD/YY(xx)"             
         BH    EIIFBK                                                           
         CLI   1(R2),1             TEST MORE THAN 1 CHAR. AFTER DASH            
         BH    EIIFBK                                                           
         XC    WORK,WORK           CLEAR AREA FOR BOOK EDIT OUTPUT              
*                                                                               
         ZIC   R1,0(R2)            TEST LATEST BOOK REQUIRED                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),=C'LATEST'                                              
         BNE   VALBK4B                                                          
         TM    BKV,BKVLATST        TEST IF LATEST BOOK IS VALID                 
         BZ    ELBI                                                             
         CLI   1(R2),0             WEEK NUMBER NOT ALLOWED                      
         BNE   EIIFBK                                                           
         MVC   WORK(2),LATBOOK     YES - SET SPECIAL BOOK VALUE                 
         B     VALBK8                                                           
                                                                                
VALBK4B  DS    0H                  TEST LATEST N BOOKS                          
         CLI   0(R2),4                                                          
         BNE   VALBK4X                                                          
         CLC   12(3,R2),=C'LATEST'                                              
         BNE   VALBK4X                                                          
         CLI   12+3(R2),C'1'        NUMBER OF BOOKS MUST BE AT LEAST 1          
         BL    ELBI                                                             
         CLI   12+3(R2),C'4'         BUT NO MORE THAN 4                         
         BH    ELBI                                                             
         TM    BKV,BKVLATST         TEST IF LATEST BOOK IS VALID                
         BZ    ELBI                                                             
         CLI   1(R2),0              WEEK NUMBER NOT ALLOWED                     
         BNE   EIIFBK                                                           
         MVI   WORK,XFF                                                         
         MVC   WORK+1(1),12+3(R2)                                               
         CLI   DBMED,C'C'                                                       
         BE    ELAT24                                                           
         B     VALBK8                                                           
                                                                                
VALBK4X  EQU   *                                                                
                                                                                
*                                                                               
         EX    R1,*+8              TEST ESTIMATED BOOK (UPGRADES)               
         B     *+10                                                             
         CLC   12(0,R2),=C'ESTIMATE'                                            
         BNE   VALBK5                                                           
         MVC   DUB+4(4),=AL4(OPTUPGDB)                                          
         MVC   DUB(4),OPTX                                                      
         NC    DUB(4),DUB+4        TEST IF UPGRADES ARE ALLOWED                 
         BNZ   EKWI                                                             
         CLI   1(R2),0             WEEK NUMBER NOT ALLOWED                      
         BNE   EIIFBK                                                           
         OC    OPTR,DUB+4          SET UPGRADE EXPRESSION REQUIRED              
         MVC   WORK(2),ESTBOOK     SET SPECIAL UPGRADE BOOK VALUE               
         B     VALBK8                                                           
                                                                                
*                                                                               
VALBK5   DS    0H                  TEST MULTIPLE BOOKS                          
         CLI   0(R2),4                                                          
         BH    VALBK6                                                           
         ZIC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         EXCLC R1,12(R2),=C'MBKS'                                               
         BNE   VALBK6                                                           
         CLI   1(R2),0             WEEK NUMBER NOT ALLOWED                      
         BNE   EIIFBK                                                           
         MVC   DUB+4(4),=AL4(OPTMBKB)                                           
         MVC   DUB(4),OPTX                                                      
         NC    DUB(4),DUB+4        TEST IF MULTIPLE BOOKS ARE ALLOWED           
         BNZ   EKWI                                                             
         OC    OPTR,DUB+4          SET MULT BOOKS EXPRESSION REQUIRED           
         MVC   WORK(2),MBKBOOK     SET SPECIAL MULT BKS BOOK VALUE              
         B     VALBK8                                                           
*                                                                               
VALBK6   CLI   0(R2),3             CALL BOOKVAL TO VALIDATE BOOK                
         BL    EIIFBK                                                           
                                                                                
         MVI   GOSTEON,SMWV#       SET FOR MONTH OR WEEK VALIDATION             
         GOTO1 AGOSTEO                                                          
         BE    *+10                                                             
         L     RF,EADDR                                                         
         BR    RF                                                               
*                                                                               
VALBK6A  GOTO1 BLDFLD,0            BUILD A TWA FIELD                            
         LA    R0,C'B'             LOOK FOR BOOK TYPE AS DEFAULT                
         CLI   DBFIL,C'N'          TEST IF NETWORK FILE                         
         BNE   *+8                                                              
         IC    R0,DBFIL            YES - SET NETWORK FLAG                       
         CLI   DBSRC,C'K'          NTI PKTPC IS WEEKLY                          
         BNE   *+8                                                              
         LA    R0,C'N'             YES - SET WEEKLY FLAG                        
         CLI   DBSRC,C'C'          NTI CABLE IS WEEKLY TOO                      
         BNE   *+8                                                              
         LA    R0,C'N'             YES - SET WEEKLY FLAG                        
         CLI   DBMED,C'W'          TEST IF WEEKLY MEDIA                         
         BNE   *+8                                                              
         LA    R0,C'N'             YES - SET WEEKLY FLAG                        
         L     RF,ADEMFIL                                                       
         CLC   =C'OTP',8(RF)                                                    
         BNE   *+8                                                              
         LA    R0,C'N'             YES - SET WEEKLY FLAG                        
         CLC   =C'OPA',8(RF)                                                    
         BNE   *+8                                                              
         LA    R0,C'N'             YES - SET WEEKLY FLAG                        
         TM    MISCFLG1,MF1BKVWK   TEST IF VALIDATE FOR WEEKLY ANYWAY           
         BZ    *+8                                                              
         LA    R0,C'N'              YES, SET WEEKLY FLAG                        
         MVI   DUB2,0                                                           
         XC    DMCB,DMCB                                                        
*                                                                               
*                                                                               
*  CHECK FOR COUNTY COVERAGE DATA - HAVE TO ADD CODE TO                         
*  MAKE SURE THE STATE IS VALID BEFORE WE DO THIS                               
         CLI   DBMED,C'U'                                                       
         BNE   VALBK6AB                                                         
         ZIC   RE,FLDH                                                          
         SHI   RE,4                                                             
         STC   RE,FLDH                                                          
         ZIC   RE,FLDH+5                                                        
         SHI   RE,4                                                             
         STC   RE,FLDH+5                                                        
                                                                                
VALBK6AB LA    R1,FLDH                                                          
         ST    R1,DMCB                                                          
         MVC   DMCB(1),DBSRC                                                    
         CLI   DBSELMED,C'N'                                                    
         BNE   *+8                                                              
         MVI   DMCB,C'N'           SET PROPER SOURCE - NIELSEN                  
*                                                                               
         CLI   DBSELSRC,C'R'      RADAR VALIDATE BOOK LIKE ARB                  
         BNE   *+8                                                              
         MVI   DMCB,C'A'                                                        
*                                                                               
         CLI   DBSRC,C'F'      FUSION VALIDATE BOOK LIKE NSI                    
         BNE   *+8                                                              
         MVI   DMCB,C'N'                                                        
*                                                                               
         GOTO1 VBOOKVAL,DMCB,,(1,DUB),((R0),VSCANNER),DUB2,(C'C',AFAC)          
         CLI   4(R1),1             TEST BOOK IS VALID                           
         BNE   EIBK                                                             
         TM    DUB,X'AE'           CAN'T HAVE ANY SPECIAL OPTIONS               
         BNZ   EIBK                                                             
*                                                                               
         CLI   DBSELSRC,C'N'       CSI-WEEKLY SINCE WE CHECKED BOOK             
         BNE   VALBK6B             VALIDATED BY BOOKVAL DO IT AGAIN             
         CLI   DBSELMED,C'O'       OVERNIGHTS ALSO CHECK BOOKTYPES              
         BE    *+8                                                              
         CLI   DBSELMED,C'W'       WEEKLY TIME PERIOD ALLOW BKTYPES             
         BE    *+8                                                              
         CLI   DBSELMED,C'C'       FOR WEEKLY BOOK TYPES, EXCEPT WE             
         BNE   VALBK6B             DONT NEED TO CHECK FOR ERRORS NOW            
*&&DO                                                                           
         TM    MISCFLG1,MF1BKVWK   TEST IF VALIDATE FOR WEEKLY ANYWAY           
         BZ    VALBK6B                                                          
         XC    DMCB,DMCB                                                        
         LA    R1,FLDH                                                          
         ST    R1,DMCB                                                          
         MVC   DMCB(1),DBSRC                                                    
         GOTO1 VBOOKVAL,DMCB,,(1,DUB),(C'B',VSCANNER),DUB2                      
*&&                                                                             
* THIS IS NECESSARY BECAUSE BOOKVAL DOES NOT DO A GOOD JOB                      
* WITH WEEKLY BOOKS AND PARSING THE BOOKTYPE                                    
*                                                                               
         ZIC   RF,0(R2)            IF DUB2 IS BLANK AGAIN THEN FORCE            
         LA    RF,(12-3)(RF,R2)    IN BKTYPE                                    
         CLI   0(RF),C'('          BKTYPES?                                     
         BNE   *+14                1 CHARACTER BOOKTYPE                         
         MVC   DUB2(1),1(RF)                                                    
         B     VALBK6B                                                          
*                                                                               
*CHECK IF TWO CHARACTER BOOKTYPE                                                
         ZIC   RF,0(R2)            IF DUB2 IS BLANK AGAIN THEN FORCE            
         LA    RF,(12-4)(RF,R2)    IN BKTYPE                                    
         CLI   0(RF),C'('          BKTYPES?                                     
         BNE   VALBK6B             1 CHARACTER BOOKTYPE                         
* CALL ROUTINE TO TRANSLATE FROM 2 CHAR INPUT TO 1 BYTE INTERNAL                
         XC    DMCB2,DMCB2                                                      
         MVC   DMCB2+1(2),1(RF)                                                 
         GOTO1 ATRANSBT                                                         
         CLI   DMCB2+4,X'FF'       INVALID 2 CHAR BOOKTYPE?                     
         BE    EIBK                                                             
         MVC   DUB2(1),DMCB2+4     1 BYTEW INTERNAL  BOOKTYPE                   
         B     VALBK6B                                                          
*                                                                               
*                                                                               
*                                                                               
VALBK6B  XC    WORK(L'BKS),WORK                                                 
         MVC   WORK+0(2),DUB+1     SET VALUE IN OUTPUT FIELD                    
         MVC   WORK+3(1),DUB2      SET BOOK TYPE VALUE                          
         ZIC   RF,0(R2)                                                         
         LA    RF,(12-3)(RF,R2)                                                 
                                                                                
         CLC   =C'(W)',0(RF)       LOOK FOR BOOK TYPE "W"                       
         BNE   *+8                                                              
         MVI   WORK+3,C'W'          IF IT IS, FORCE IT IN                       
*                                                                               
*                                                                               
                                                                                
         CLI   DBMED,C'U'           CONVERT STATE ALPHA CODE                    
         BNE   VALBK6BA             TO INTERNAL CODE                            
         MVC   HALF,0(RF)                                                       
         GOTO1 =A(GETCOMS),RR=BRELO   GET COMFAC ADDRESSES                      
         GOTOR VALSTATE                                                         
         BNE   ESTATCDE                                                         
         MVC   WORK+3(1),BYTE                                                   
*                                                                               
VALBK6BA CLI   DBSRC,C'A'           BBM WTP ALWAYS BTYPE W                      
         BNE   VALBK6C                                                          
         L     RF,ADEMFIL                                                       
         CLC   =C'WTP',8(RF)                                                    
         BNE   *+8                                                              
         MVI   WORK+3,C'W'                                                      
VALBK6C  DS    0H                                                               
*  MAKE BOOKTYPE LOWERCASE                                                      
*&&DO                                                                           
         CLI   DBMED,C'T'                                                       
         BNE   *+16                                                             
         CLI   WORK+3,0                                                         
         BE    *+8                                                              
         XI    WORK+3,X'40'                                                     
*&&                                                                             
*&&DO                                                                           
*  NEW CODE FOR CANADAIN CSI                                                    
         CLI   DBMED,C'C'                                                       
         BNE   VALBKPMD                                                         
         CLI   DBSRC,C'A'                                                       
         BE    VALBKPMD                                                         
         CLC   =X'6001',WORK                                                    
         BNH   VALBKPMD                                                         
*                                                                               
         CLI   ACTN,DEMOQHR         IF ACTION=DISPLAY                           
         BNE   VALBKPMD              TELL USER TO USE ACTION=PERIOD             
         MVC   EADDR+2(2),=Y(EACTCSI-DEM00)                                     
         L     RF,EADDR                                                         
         BR    RF                                                               
*&&                                                                             
VALBKPMD DS    0H                                                               
*                                                                               
         DS    0H                                                               
         CLI   ACTN,PROGMKT        IF ACTION=PROGMKT,                           
         BE    VALBKPMG                                                         
         CLI   ACTN,PROGNAME        OR PROGNAME,                                
         BE    VALBKPMG                                                         
         B     VALBKPMX                                                         
VALBKPMG DS    0H                                                               
         CLC   =X'5E03',WORK        THERE IS NO DATA PRIOR TO MAR94             
         BH    ENPMD                                                            
VALBKPMX EQU   *                                                                
*                                                                               
         CLI   1(R2),0             TEST WEEK NUMBER/ALL WEEKS GIVEN             
         BE    VALBK8                                                           
         ZIC   RE,SCAN1HL                                                       
         LA    RE,12(RE,R2)        RE-->DATA FOR 2ND HALF                       
         CLI   0(RE),C'W'          TEST FOR EXPLODE ALL 4 WEEKS                 
         BNE   VALBK7                                                           
         CLI   DBMED,C'W'          EXPLODE WEEKS OPTION NOT VALID               
         BE    EWKBREAK            FOR WEEKLY FILE                              
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    ESTE                 INVALIDATE REQUEST                          
         LA    R0,4                R0=COUNTER OF WEEKS                          
         ZIC   R1,NBKS             GET NUMBER OF BOOKS SO FAR                   
         LR    RF,R1               SAVE IN RF                                   
         AR    R1,R0                                                            
         STC   R1,NBKS                                                          
         CLI   NBKS,MAXBKS         TEST AGAINST BOOK LIMIT                      
         BH    EFVB                                                             
         LA    RE,L'BKS            DEVELOP INDEX INTO BOOK LIST                 
         MR    RE,RE                                                            
         LA    RF,BKS(RF)          RF=POINTER TO NEXT BOOK                      
         LA    RE,1                RE=WEEK NUMBER                               
*                                                                               
VALBK6D  STC   RE,WORK+2                                                        
         MVC   0(L'BKS,RF),WORK                                                 
         LA    RE,1(RE)            INCREMENT WEEK NUMBER                        
         LA    RF,L'BKS(RF)                                                     
         BCT   R0,VALBK6D                                                       
         TM    DEMFLAG1,DF1STERO   DON'T NEED TO WORRY ABOUT                    
         BO    VALBK6E              FORMATTING IF STEREO SESSION                
         CLI   ACTN,DEMOQHR        IF ACTION IS DISPLAY                         
         BE    *+12                                                             
         CLI   ACTN,DEMOPER         OR PERIOD,                                  
         BNE   VALBK6E                                                          
         CLI   NSTAS,1              AND ONLY ONE STATION                        
         BNE   VALBK6E                                                          
         CLI   NFLDS,1              AND ONE BOOK INPUT,                         
         BH    VALBK6E                                                          
         OI    MISCFLG1,MF1WKACR    DISPLAY WEEKS ACROSS THE SCREEN             
VALBK6E  DS    0H                                                               
         B     VALBK4                                                           
*                                                                               
VALBK7   DS    0H                                                               
         ZIC   RE,SCAN1HL                                                       
         LA    RE,12(RE,R2)        RE-->DATA FOR 2ND HALF                       
         CLI   0(RE),C'1'          SPECIFIC WEEK NUMBER EDIT                    
         BL    EIIFBK                                                           
         CLI   0(RE),C'4'                                                       
         BH    EIIFBK                                                           
         MVC   WORK+2(1),0(RE)     SET WEEK NUMBER                              
         NI    WORK+2,X'0F'                                                     
*                                                                               
VALBK8   ZIC   R1,NBKS             MOVE BOOK TO OUTPUT AREA                     
         LA    R1,1(R1)                                                         
         STC   R1,NBKS                                                          
         CLI   NBKS,MAXBKS         TEST AGAINST MAX BOOKS                       
         BH    EFVB                OVERFLOWED LIMIT                             
         LA    R0,L'BKS                                                         
         MR    R0,R0                                                            
         LA    R1,BKS-L'BKS(R1)                                                 
         MVC   0(L'BKS,R1),WORK                                                 
         B     VALBK4                                                           
*                                                                               
VALBK10  CLI   IACTN,NEXT          TEST IF NEXT IS VALID                        
         BNE   VALBK12                                                          
         CLC   NBKS,LNBKS                                                       
         BNE   EKHC                                                             
         ZIC   R1,NBKS                                                          
         LA    R0,L'BKS                                                         
         MR    R0,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   BKS(0),LBKS                                                      
         BNE   EKHC                                                             
*                                                                               
VALBK12  MVI   DUB,0               GET ACTUAL STATION/BOOK VALUES               
         LA    R2,DBSTABK-STABKL   DUB+0=STATION SEQUENCE NUMBER                
         USING STABKD,R2           R2=A(STATION/BOOK BLOCK)                     
         TM    BKV,BKVVALTP        TEST STATION/BOOK VALIDATION REQD            
         BZ    VALBK18                                                          
*                                                                               
VALBK14  ZIC   R1,DUB              GET NEXT STATION                             
         LA    R1,1(R1)                                                         
         CLM   R1,1,NSTAS          TEST ALL STATIONS VALIDATED                  
         BH    VALBKX                                                           
         STC   R1,DUB                                                           
         LA    R2,STABKL(R2)       BUMP TO NEXT DBSTABK ENTRY                   
         LA    R0,L'STAS                                                        
         MR    R0,R0                                                            
         LA    R1,STAS-L'STAS(R1)                                               
         MVC   STASTA,0(R1)        SET STATION & SPILL MARKET                   
         MVC   STASPILL,5(R1)                                                   
         MVI   DUB+1,0                                                          
         MVC   STABKS(STABKSL),BKS                                              
                                                                                
         LA    R0,L'ALFMKTS        GET ALPHA MKT INPUT INTO WORK(3)             
         ZIC   R1,DUB                                                           
         BCTR  R1,0                                                             
         MR    R0,R0                                                            
         LA    R1,ALFMKTS(R1)                                                   
         MVC   WORK(L'ALFMKTS),0(R1)                                            
*                                                                               
* CONVERT ALPHA SPILL MARKETS TO NUMERIC                                        
         OC    0(L'ALFMKTS,R1),0(R1)                                            
         BZ    VALBK16                                                          
         MVC   DUB2,DUB             WE CLOBBERED DUB IN TRMKT -SAVE IT          
         MVC   DUB(L'ALFMKTS),0(R1)                                             
         GOTOR TRAMKT                                                           
         MVC   STASPILL,DUB+3                                                   
         MVC   DUB,DUB2            WE CLOBBERED DUB IN TRMKT -RESTORE           
*                                                                               
VALBK16  ZIC   R1,DUB+1            GET NEXT BOOK                                
         LA    R1,1(R1)                                                         
         CLM   R1,1,NBKS           TEST ALL BOOKS FOR STATION VALIDATED         
         BH    VALBK14                                                          
         STC   R1,DUB+1                                                         
         LA    R0,L'BKS                                                         
         MR    R0,R0                                                            
         LA    R3,STABKS-L'STABKS(R1)                                           
         CLC   0(L'ESTBOOK,R3),ESTBOOK                                          
         BE    VALBK16                                                          
         CLC   0(L'MBKBOOK,R3),MBKBOOK                                          
         BE    VALBK16                                                          
         CLI   0(R3),XFF           DON'T DO IF "LATEST N" REQUESTED             
         BE    VALBK16                                                          
                                                                                
*                                                                               
         LA    R4,DBLOCK1          BUILD DBLOCK TO VALIDATE STA/BOOK            
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
* test                                                                          
         CLI   DBSRC,C'R'                                                       
         BNE   *+14                                                             
         MVC   DBFIL,=C'RTP'                                                    
         B     VALBK16M                                                         
* eot                                                                           
         L     R0,AIOAREA1                                                      
         ST    R0,DBAREC                                                        
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELSTA,STASTA                                                  
         CLI   DBMED,C'T'           IF MEDIA IS TELEVISION,                     
         BNE   *+16                                                             
         TM    DBSELSTA+4,X'F0'      REPLACE ANY PARENT PLUS                    
         BNO   *+8                                                              
         MVI   DBSELSTA+4,C'T'       WITH A 'T'                                 
         CLI   DBMED,C'C'           DONT USE NUMERIC SPILL FOR                  
         BE    *+10                 CANADA                                      
         MVC   DBSELMK,STASPILL                                                 
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELBK,0(R3)                                                    
         MVC   DBBTYPE,3(R3)                                                    
*                                                                               
         TM    MISCFLG1,MF1BKVWK    IF BOOK VALIDATED AS MONTH,                 
         BNZ   *+16                                                             
         CLI   3(R3),C'W'            AND IT'S BOOKTYPE 'W',                     
         BNE   *+8                                                              
         MVI   DBBEST,C'M'            SET "CANADIAN/MONTHLY" FLAG               
*                                                                               
         CLI   DBMED,C'C'                                                       
         BNE   VALBK161                                                         
         L     RF,ADEMFIL                                                       
         CLC   =C'WTP',8(RF)                                                    
         BNE   *+12                                                             
         MVI   DBBEST,C'W'                                                      
         B     VALBK161                                                         
         MVI   DBBEST,C'M'                                                      
         MVI   DBUSEBBM,C'Y'                                                    
*                                                                               
VALBK161 DS    0H                                                               
*                                                                               
                                                                                
         MVI   DBFUNCT,DBGETTLB                                                 
         CLI   DBMED,C'C'          DONT USE NUMERIC SPILL FOR CANADA            
         BE    *+14                                                             
         OC    STASPILL,STASPILL   IF THERE WERE NO SPILL MARKETS,              
         BNZ   VALBK16A                                                         
         OC    WORK(3),WORK         AND THERE WAS AN ALPHA MKT INPUT,           
         BZ    VALBK16A                                                         
         MVC   DBSELALF,WORK        USE IT                                      
                                                                                
VALBK16A DS    0H                                                               
*                                                                               
************ fudge for now for county coverage                                  
************ until we figure out the access  stuff                              
************ IF WE DONT FUDGE THEN WE HAVE TO MAKE SURE THIS                    
*********** LOOKS LIKE A TP FILE                                                
         CLI   DBMED,C'U'                                                       
**       BE    VALBK16M                                                         
         BNE   *+10                                                             
         MVC   DBFILE,=C'CTP '                                                  
* SKIP FUSION FOR NOW                                                           
**       CLI   DBSRC,C'F'                                                       
**       BE    VALBK16M                                                         
*                                                                               
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         CLI   DBERROR,0           TEST FOR DEMAND ERRORS                       
         BE    *+14                                                             
         MVC   0(L'INVBOOK,R3),INVBOOK                                          
         B     VALBK16M                                                         
         CLC   0(L'LATBOOK,R3),LATBOOK   IF LATEST NOT REQUESTED,               
         BNE   VALBK16M                  LEAVE INPUT ALONE                      
*                                                                               
         CLI   DBMED,C'C'                DONT SAVE LATEST BOOK                  
         BNE   VALBK16L                  FOR CANADA                             
         BRAS  RE,CANLATWK        PERFORM SOME CANADIAN LATEST                  
         BE    *+10               WORK                                          
         L     RF,EADDR                                                         
         BR    RF                                                               
         B     VALBK16M                                                         
*                                                                               
VALBK16L EQU   *                                                                
         MVC   0(L'DBACTBK,R3),DBACTBK                                          
                                                                                
VALBK16M EQU   *                                                                
*                                                                               
*****    MVC   STASPILL,DBACTKMK                                                
VALBK16P B     VALBK16                                                          
         DROP  R4                                                               
*                                                                               
VALBK18  DS    0H                                                               
VALBKX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*====================== SPECIAL CIRCUMSTANCE #1 ======================*         
*****                                                                           
*&&DO                                                                           
* Although Arbitron stopped its television service, it still meters             
*  the Detroit and Cleveland markets as of Feb/94                               
                                                                                
         CLI   DBSRC,C'A'          WAS ARB REQUESTED?                           
         BNE   SP1CX                NO, SKIP THIS CODE                          
         CLI   DBMED,C'T'          WAS MED=T REQUESTED?                         
         BNE   SP1CX                NO, SKIP THIS CODE                          
                                                                                
         CLI   ACTN,DEMOQHR        DISPLAY                                      
         BE    SP1C05                                                           
         CLI   ACTN,DEMOPER        PERIOD                                       
         BE    SP1C05                                                           
         CLI   ACTN,DEMOAVL        AVAIL                                        
         BE    SP1C05                                                           
         CLI   ACTN,SPILLTO        SPILL                                        
         BE    SP1C05                                                           
*&&DO                                                                           
         CLI   ACTN,AFFIDS         AFFIDS                                       
         BE    SP1C50                                                           
*&&                                                                             
         BNE   SP1CX                                                            
*                                                                               
** ACTION = DISPLAY, PERIOD, AVAIL, OR SPILL                                    
*                                                                               
SP1C05   MVI   DUB,0               IF BOOK REQUESTED WAS FEB/94 OR              
         LA    R2,DBSTABK-STABKL    AFTER, ITS STATION HAS TO BE                
         USING STABKD,R2            LISTED IN ARBF94 TABLE                      
*                                                                               
SP1C10   ZIC   R1,DUB              GET NEXT STATION                             
         LA    R1,1(R1)                                                         
         CLM   R1,1,NSTAS          TEST ALL STATIONS VALIDATED                  
         BH    SP1CX                YEP, EXIT                                   
         STC   R1,DUB                                                           
         LA    R2,STABKL(R2)       BUMP TO NEXT DBSTABK ENTRY                   
         LA    R0,L'STAS                                                        
         MR    R0,R0                                                            
         LA    R1,STAS-L'STAS(R1)                                               
         MVC   STASTA,0(R1)        SET STATION & SPILL MARKET                   
         MVC   STASPILL,5(R1)                                                   
         MVI   DUB+1,0                                                          
         MVC   STABKS(STABKSL),BKS                                              
*                                                                               
SP1C20   ZIC   R1,DUB+1            GET NEXT BOOK                                
         LA    R1,1(R1)                                                         
         CLM   R1,1,NBKS           TEST ALL BOOKS FOR STATION VALIDATED         
         BH    SP1C10                                                           
         STC   R1,DUB+1                                                         
         LA    R0,L'BKS                                                         
         MR    R0,R0                                                            
         LA    R3,STABKS-L'STABKS(R1)                                           
         CLC   =X'5E02',0(R3)      I'M ONLY INTERESTED IN                       
         BH    SP1C20               BOOKS AFTER FEB/94                          
                                                                                
         LA    RF,ARBF94           LOOK STATION UP IN TABLE                     
SP1C30   CLI   0(RF),EOT                                                        
         BE    ENAB                 STATION NOT IN TABLE--ERROR                 
         CLC   STASTA(4),0(RF)                                                  
         BE    SP1C10              STATION IS GOOD, GET NEXT STATION            
         LA    RF,4(RF)                                                         
         B     SP1C30                                                           
         DROP  R2                                                               
*&&DO                                                                           
*                                                                               
** ACTION = AFFIDS                                                              
*                                                                               
SP1C50   CLC   =X'5E02',BKS        IGNORE THOSE BEFORE FEB/94                   
         BH    SP1CX                                                            
         LA    RF,ARBF94           FIND STATION IN TABLE                        
SP1C50A  CLI   0(RF),EOT                                                        
         BE    ENAB                 EOT MEANS ERROR                             
         CLC   NSIDSTA,0(RF)                                                    
         BE    SP1CX                IF FOUND, THEN CONTINUE                     
         LA    RF,4(RF)                                                         
         B     SP1C50A                                                          
*&&                                                                             
****                                                                            
*&&                                                                             
*                                                                               
SP1CX    DS    0H                                                               
***********************************************************************         
         EJECT                                                                  
* VALIDATE DAY/TIME EXPRESSIONS                                                 
*                                                                               
VALDAY   MVI   HELPREQD,HELPDAY                                                 
         XC    SCANDLIM,SCANDLIM                                                
         XC    SCANDLM2,SCANDLM2                                                
*                                                                               
         L     RF,ADEMACT                                                       
         CLI   ACTN,AFFIDS                                                      
         BE    VALDAYX                                                          
         CLC   =C'AFFID',8(RF)                                                  
         BE    VALDAYX                                                          
*                                                                               
         DS    0H                  CLEAR AREA USED FOR DAY/TIME ROTATNS         
         XC    LDTROTNL,LDTROTNL                                                
         CLI   IACTN,NEXT                                                       
         BE    VALDAY_01G                                                       
         LA    R0,LDTROTN                                                       
         LA    R1,L'LDTROTN                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
VALDAY_01G EQU *                                                                
*                                                                               
         MVC   MFLDS,DAYN          SET MAX N'DAY/TIMES                          
         MVI   MFLDS2,MAXPUCMP     SET MAX N'COMPONENTS IN ROTATION             
         CLC   =AL1(MAXPUCMP),=AL1(MAXDTCMP)                                    
         BNL   *+8                                                              
         MVI   MFLDS2,MAXDTCMP     SET MAX N'COMPONENTS IN ROTATION             
         L     R1,ADEMDAT                                                       
*        LA    R1,DEMDATH                                                       
         MVI   FERN,24                                                          
         GOTO1 FVAL,(R1)                                                        
         BNE   VALDAY2                                                          
         CLI   MFLDS,0             TEST ANY DAY/TIMES REQUIRED                  
         BE    VALDAYX                                                          
         B     ERROR                                                            
*                                                                               
VALDAY2  CLI   MFLDS,0             TEST ANY DAY/TIMES TO VALIDATE               
         BE    EIIFDT                                                           
                                                                                
*                                                                               
         DS    0H                  PARSE OUT ROTATIONS                          
         MVI   SCAN1HL,10          (BUMP-SCANNER-BLOCK RTN USES THIS)           
         MVI   SCANLNTH,L'DUMDAT-10                                             
         ZIC   RF,MFLDS                                                         
         LA    RF,1(RF)                                                         
         L     R2,BINATAB                                                       
         ZIC   R0,FLDH+5                                                        
         STCM  R0,3,FULL+0                                                      
         MVC   FULL+2(1),SCAN1HL                                                
         GOTO1 AMYSCAN,DMCB,(SCANLNTH,FLD),((RF),(R2)),X'6B7E6BFF',FULL         
         MVC   SCANDLIM,DMCB+8                                                  
         MVC   NFLDS,4(R1)         SAVE N'SCANNER LINES                         
         CLI   NFLDS,0             TEST FOR SCAN ERROR                          
         BE    EIIFDT                                                           
         CLC   NFLDS,MFLDS         TEST N'DAY/TIMES VALID                       
         BH    ETMI                                                             
                                                                                
*                                                                               
VALDAY3B DS    0H                  VALIDATE DAYS/TIMES WITHIN ROTATION          
         BAS   RE,BUMPSCAN                                                      
         BH    VALDAY9                                                          
*                                                                               
         XC    WORK2,WORK2          USE  WORK2  FOR DUMMY FIELD                 
         ZICM  R1,0(R2),(1)         R1 = L(DAYS/TIMES INPUT)                    
         BZ    EIIFDT                                                           
         STC   R1,WORK2+5            L(DATA INPUT)                              
         BCTR  R1,0                                                             
         EXMVC R1,WORK2+8,12(R2)     ACTUAL DATA ITSELF                         
         LA    R1,8+1(R1)                                                       
         STC   R1,WORK2+0            L(DUMMY FIELD)                             
*                                                                               
         MVI   FNDX2,0                                                          
         MVI   SCAN1HL2,10          (BUMP-SCANNER-BLOCK RTN USES THIS)          
         MVI   SCNLNTH2,SCANLDAY    SCAN LENGTH FOR DAY/TIME                    
         ZIC   RF,MFLDS2                                                        
         LA    RF,1(RF)                                                         
         L     R3,AIOAREA2                                                      
         GOTO1 VSCANNER,DMCB,(SCNLNTH2,WORK2),((RF),(R3)),C',=+/'               
         MVC   SCANDLM2,DMCB+8                                                  
         MVC   NFLDS2,DMCB+4        SAVE N'SCANNER LINES                        
         CLI   NFLDS2,0             TEST FOR SCAN ERROR                         
         BE    EIIFDT                                                           
         NI    MISCFLG2,XFF-MF2VPUR   ASSUME INPUT IS NOT A PURE#               
         NI    MISCFLG2,XFF-MF2DTROT  ASSUME INPUT IS NOT A ROTATION            
         CLI   NFLDS2,1               IF INPUT IS JUST ONE SUB-FIELD,           
         BE    VALDAY3R                IT'S NOT A ROTATION                      
         CLI   DBMED,C'U'             COUNTY COVERAGE CAN HAVE ROTATION         
         BE    ECUNROT                                                          
         CLC   NFLDS2,MFLDS2          MAKE SURE ROTATION DOESN'T EXCEED         
         BH    ETMC                    MAX DAY/TIME COMPONENTS ALLOWED          
         OI    MISCFLG2,MF2DTROT      WE HAVE OURSELVES A ROTATION!             
         XC    WORK2,WORK2             USE  WORK2  TO STORE DAYS/TIMES          
         LA    R4,WORK2+1              LEAVE 1ST BYTE FOR LENGTH                
         MVI   NDTCOMP,0              KEEP COUNT OF # OF COMPONENTS             
VALDAY3R EQU   *                                                                
                                                                                
*                                                                               
VALDAY4  DS    0H                                                               
         BRAS  RE,BUMPSCN2                                                      
         BH    VALDAY8                                                          
         CLI   0(R3),0             TEST FOR SOME INPUT                          
         BE    EIIFDT                                                           
*                                                                               
         TM    MISCFLG2,MF2VPUR                                                 
         BO    VDYPU                                                            
         LA    RF,NDAYS                                                         
         TM    MISCFLG2,MF2DTROT                                                
         BZ    *+8                                                              
         LA    RF,NDTCOMP                                                       
         CLI   0(RF),0                                                          
         BH    VALDY04G                                                         
         CLI   12(R3),C'P'                                                      
         BNE   VALDY04G                                                         
*                                                                               
         CLI   0(R3),5              MATCH ON ALL 5 CHARACTERS                   
         BNE   *+14                 IF PRIME ENTERED ITS NOT                    
         CLC   12(5,R3),=C'PRIME'   A PURE NUMBER SO SKIP                       
         BE    VALDY04G                                                         
*                                                                               
         ZIC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         STC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,12(R3),13(R3)    REMOVE THE "P" PREFIX                        
         LA    R1,12+1(R1,R3)                                                   
         MVI   0(R1),C' '          BLANK OUT WHERE LAST CHAR USED TO BE         
         OI    MISCFLG2,MF2VPUR                                                 
         B     VDYPU                                                            
VALDY04G EQU   *                                                                
*                                                                               
         TM    MISCFLG2,MF2DTROT   IF INPUT IS A ROTATION                       
         BZ    *+12                                                             
         CLI   NFLDS2,MAXDTCMP      MAKE SURE ROTATION DOESN'T EXCEED           
         BH    ETMC                 MAX DAY/TIME COMPONENTS ALLOWED             
*                                                                               
* CHECK IF COUNTY COVERAGE ASKED FOR SU-SA                                      
* IF SO SET THE INTERNAL DAYCODE TO X'90'                                       
* SO WE CAN TREAT IT UNIQUELY IN THE DQTAB                                      
         CLI   DBMED,C'U'                                                       
         BNE   VALDY04H                                                         
         CLI   0(R3),5              MATCH ON ALL 5 CHARACTERS                   
         BNE   VALDY04H                                                         
         CLC   12(5,R3),=C'PRIME'                                               
         BNE   VALDY04H                                                         
         MVI   WORK,X'7F'             PRIME FORCE TO M-SU PICKS UP              
         MVC   WORK+1(4),=X'07D008FC' ANYTHING IN M-F,SA-SU AND PRIME           
         B     VALDAY8                DAYPARTS 7-11P                            
*                                                                               
VALDY04H CLI   1(R3),0             TEST FOR UNDIVIDED FIELD                     
         BE    VALDAY7             YES-LOOK FOR TIME ONLY                       
*                                                                               
         CLI   0(R3),7             TEST L'DAY EXPRESSION VALID                  
         BH    EIIFDT                                                           
         GOTO1 VDAYPAK,DMCB,(0(R3),12(R3)),WORK,WORK+1                          
* CHECK IF COUNTY COVERAGE ASKED FOR SU-SA                                      
* IF SO SET THE INTERNAL DAYCODE TO X'90'                                       
* SO WE CAN TREAT IT UNIQUELY IN THE DQTAB                                      
         CLI   DBMED,C'U'                                                       
         BNE   VALDY04I                                                         
         CLI   WORK+1,X'76'          SU-SA?                                     
         BNE   *+8                                                              
         MVI   WORK,X'90'                                                       
VALDY04I EQU   *                                                                
*                                                                               
         CLI   WORK,0              TEST DAY EXPRESSION IS VALID                 
         BNE   VALDAY6                                                          
                                                                                
         DS    0H                  VALIDATE FOR "VAR" DAY REQUEST               
         CLC   DBFIL,=C'PAV'        APPLICAPLE TO PAV                           
         BNE   VALDY04M                                                         
         CLI   0(R3),3              MATCH ON ALL THREE CHARACTERS               
         BNE   VALDY04M                                                         
         CLC   12(3,R3),=C'VAR'                                                 
         BNE   VALDY04M                                                         
         TM    DEMFLAG1,DF1STERO+DF1DEM32   IF DEM32 SESSION,                   
         BNO   VALDY04K                                                         
         CLC   D32PCVER,=AL4(XTRCVER4)       MAKE SURE THE VERSION CAN          
         BL    ENOTSUPP                      SUPPORT "VAR" FEATURE              
VALDY04K EQU   *                                                                
         MVI   WORK,X'90'                                                       
         B     VALDAY6                                                          
VALDY04M EQU   *                                                                
                                                                                
         DS    0H                  VALIDATE FOR "AVGn" DAY REQUEST              
         CLC   DBFIL,=C'PAV'        APPLICAPLE TO PAV                           
         BNE   VALDY04P                                                         
         ZIC   RF,0(R3)                                                         
         CHI   RF,4                 INPUT LENGTH S/B <= 4                       
         BH    VALDY04P                                                         
         SHI   RF,2                  AND >= 3                                   
         BNP   VALDY04P                                                         
         EXCLC RF,12(R3),=C'AVG'    LOOK FOR "AV" OR "AVG"                      
         BNE   VALDY04P              NOPE                                       
         TM    DEMFLAG1,DF1STERO+DF1DEM32   IF DEM32 SESSION,                   
         BNO   VALDY04N                                                         
         CLC   D32PCVER,=AL4(XTRCVER4)       MAKE SURE THE VERSION CAN          
         BL    ENOTSUPP                      SUPPORT "VAR" FEATURE              
VALDY04N EQU   *                                                                
         LA    RE,12+1(RF,R3)       RE-->LAST CHAR OF INPUT                     
         CLI   0(RE),C'2'           SHOULD BE BETWEEN 2                         
         BL    VALDY04P                                                         
         CLI   0(RE),C'6'            AND 6 DAYS                                 
         BH    VALDY04P                                                         
         MVC   WORK(1),0(RE)                                                    
         NI    WORK,X'9F'           X'90' BITS ==> VAR OR AVG-N                 
         B     VALDAY6                                                          
VALDY04P EQU   *                                                                
                                                                                
         CLI   NBKS,1              IF MULTI BOOKS DATE INPUT INVALID            
         BH    EIDE                                                             
*                                  VALIDATE DAY/MONTH                           
         GOTO1 VDATVAL,DMCB,(1,12(R3)),WORK                                     
         OC    0(4,R1),0(R1)       TEST DATE IS VALID                           
         BZ    EIDE                                                             
         PACK  DUB,WORK+2(2)                                                    
         CVB   R0,DUB                                                           
         STC   R0,FULL+1                                                        
         PACK  DUB,WORK+4(2)                                                    
         CVB   R0,DUB                                                           
         STC   R0,FULL+2                                                        
         IC    R0,BKS              GET YEAR FROM BOOK INPUT                     
*        CLC   FULL+1(2),TODAYB+1  TEST DATE LESS THAN TODAY                    
*        BNH   *+6                                                              
*        BCTR  R0,0                NO - DECREMENT YEAR                          
         STC   R0,FULL                                                          
*                                                                               
         CVD   R0,DUB              BUILD EBCDIC DATE IN WORK                    
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+0(2),DUB                                                    
         IC    R0,FULL+1                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+2(2),DUB                                                    
         IC    R0,FULL+2                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+4(2),DUB                                                    
         GOTO1 VGETDAY,DMCB,WORK,FULL                                           
         CLC   FULL(3),SPACES                                                   
         BE    EIDE                                                             
         GOTO1 VDAYPAK,DMCB,(3,FULL),WORK,WORK+1                                
         CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALDAY6  GOTO1 VTIMVAL,DMCB,(1(R3),22(R3)),WORK+1                               
         CLI   0(R1),X'FF'         TEST TIME EXPRESSION IS VALID                
         BE    EITE                                                             
         TM    MISCFLG2,MF2DTROT   IF THIS IS A ROTATION,                       
         BO    VALDAY7R             PROCESS IT DIFFERENTLY                      
         B     VALDAY8                                                          
*                                  SCAN FOR TIME/USE LAST DAY INPUT             
VALDAY7  DS    0H                                                               
         LA    RF,NDAYS                                                         
         TM    MISCFLG2,MF2DTROT                                                
         BZ    *+8                                                              
         LA    RF,NDTCOMP                                                       
         CLI   0(RF),0             TEST FOR FIRST FIELD                         
         BE    EIIFDT              YES-ERROR BECAUSE NEED LAST DAY              
         GOTO1 VTIMVAL,DMCB,(0(R3),12(R3)),WORK+1                               
         CLI   0(R1),X'FF'         TEST TIME EXPRESSION IS VALID                
         BE    EITE                                                             
         MVC   WORK(1),DUB2        TAKE DAY FROM LAST ONE INPUT                 
         TM    MISCFLG2,MF2DTROT   IF THIS IS A ROTATION,                       
         BO    VALDAY7R             PROCESS IT DIFFERENTLY                      
         B     VALDAY8                                                          
                                                                                
*                                                                               
VALDAY7R DS    0H                  PROCESSING FOR ROTATIONS                     
         ZIC   R1,NDTCOMP           UPDATE # OF COMPONENTS IN ROTATION          
         LA    R1,1(R1)                                                         
         STC   R1,NDTCOMP                                                       
         MVC   0(L'DAYS,R4),WORK    MOVE COMPONENT TO STORAGE                   
         LA    R4,L'DAYS(R4)                                                    
         MVC   DUB2(1),WORK         SAVE LAST DAY                               
         B     VALDAY4                                                          
*                                                                               
VALDAY8  DS    0H                  MOVE DAY/TIME TO OUTPUT AREA                 
         TM    MISCFLG2,MF2DTROT    ARE WE PROCESSING A ROTATION?               
         BNO   VALDAY8R              NOPE                                       
*                                                                               
         DS    0H                   R4-->SLOT FOR NXT DAY/TIME COMPNT           
         LA    R0,WORK2                                                         
         SR    R4,R0                R4 = L(STORED DATA FOR ROTATION)            
         STC   R4,WORK2                                                         
         ZICM  RE,LDTROTNL,(3)      RE = TOTAL L(DATA FOR ROTATIONS)            
         LA    RE,LDTROTN(RE)       RE-->SLOT TO STORE ROTATION                 
*                                                                               
         DS    0H                  CHECK FOR KEY CHANGE ON "NEXT"               
         CLI   IACTN,NEXT                                                       
         BNE   VALDAY8M                                                         
         CLM   R4,1,0(RE)            CHECK LENGTHS OF ROTATIONS                 
         BNE   EKHC                                                             
         SH    R4,=H'2'                                                         
         EXCLC R4,WORK2+1,1(RE)      CHECK COMPONENTS OF ROTATIONS              
         BNE   EKHC                                                             
         LA    R4,2(R4)              RESTORE LENGTH                             
VALDAY8M EQU   *                                                                
*                                                                               
         DS    0H                  MOVE CURRENT ROTATION TO STORAGE             
         MVC   WORK+0(3),=C'ROT'    "ROT" INDICATES A ROTATION                  
         MVC   WORK+3(2),LDTROTNL   DISPL TO THIS ROTATION                      
         BCTR  R4,0                                                             
         EXMVC R4,0(RE),WORK2+0     STORE COMPONENTS OF ROTATION                
         LA    RE,1(R4,RE)                                                      
         LA    R0,LDTROTN                                                       
         SR    RE,R0                                                            
         STCM  RE,3,LDTROTNL       UPDATE TOTAL LENGTH OF ROTATIONS             
VALDAY8R EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         ZIC   R1,NDAYS            MOVE DAY/TIME TO OUTPUT AREA                 
         LA    R1,1(R1)                                                         
         STC   R1,NDAYS                                                         
         LA    R0,L'DAYS                                                        
         MR    R0,R0                                                            
         LA    R1,DAYS-L'DAYS(R1)                                               
         MVC   0(L'DAYS,R1),WORK                                                
         TM    MISCFLG2,MF2DTROT                                                
         BO    *+10                                                             
         MVC   DUB2(1),WORK        SAVE LAST DAY                                
         B     VALDAY3B                                                         
         EJECT                                                                  
VDYPU    DS    0H                  VALIDATE AS PURE NUMBER                      
         CLC   DBFIL,=C'PAV'        PURE NUMBER ONLY VALID FOR PAV              
         BE    VDYPU03X                                                         
         MVC   XTRA(L'DBFIL),DBFIL                                              
         B     EPUIF                                                            
VDYPU03X EQU   *                                                                
                                                                                
*                                                                               
         TM    MISCFLG2,MF2DTROT    IF INPUT IS A ROTATION                      
         BZ    *+12                                                             
         CLI   NFLDS2,MAXPUCMP       MAKE SURE ROTATION DOESN'T EXCEED          
         BH    ETMC                  MAX PURE# COMPONENTS ALLOWED               
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   GOSTEON,VPUR#                                                    
         GOTO1 AGOSTEO                                                          
         BNE   EIIFPU                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   WORK+0(3),=C'PUR'                                                
         MVC   WORK+3(2),DUB                                                    
         TM    MISCFLG2,MF2DTROT                                                
         BO    VALDAY7R                                                         
         B     VALDAY8                                                          
         EJECT                                                                  
*                                                                               
VALDAY9  CLI   IACTN,NEXT          TEST IF NEXT IS VALID                        
         BNE   VALDAYX                                                          
         CLC   NDAYS,LNDAYS                                                     
         BNE   EKHC                                                             
         ZIC   R1,NDAYS                                                         
         LA    R0,L'DAYS                                                        
         MR    R0,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DAYS(0),LDAYS                                                    
         BNE   EKHC                                                             
VALDAYX  DS    0H                                                               
         EJECT                                                                  
* VALIDATE DEMOS                                                                
*                                                                               
VALDEM   LA    R4,DBLOCK1          BUILD DBLOCK FOR DEMOVAL/DEMOCON             
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,DBFIL                                                     
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         CLI   DBSELMED,C'O'       OVERNIGHTS                                   
         BE    *+8                                                              
         CLI   DBSELMED,C'N'                                                    
         BNE   *+12                                                             
         MVI   DBSELMED,C'T'                                                    
         MVI   DBSELSRC,C'N'                                                    
*                                                                               
         MVC   DBSELAGY,AGYALPH                                                 
         MVI   HELPREQD,HELPDEM                                                 
         XC    SCANDLIM,SCANDLIM                                                
         MVC   MFLDS,DEMN                                                       
         CLI   MFLDS,0             TEST ANY DEMOS TO VALIDATE                   
         BE    VALDEM2                                                          
         ZIC   R1,DEMN             CALCULATE MAX N'DEMOS                        
         ZIC   RE,NSTAS                                                         
         SR    R0,R0                                                            
         DR    R0,RE                                                            
         STC   R1,MFLDS            MAX DEMOS=MAX DEMOS/N'STATIONS               
VALDEM2  L     R1,ADEMDEM                                                       
         MVI   FERN,26             MISSING DEMO FIELD                           
         GOTO1 FVAL,(R1)                                                        
         BNE   VALDEM4                                                          
         TM    DEMV,DEMVESTS       TEST ESTIMATE HEADER DEMOS                   
         BNZ   VALDEM10                                                         
         TM    DEMV,DEMVOPTL       TEST DEMO-INPUT OPTIONAL                     
         BO    VALDEMX                                                          
         CLI   MFLDS,0             TEST ANY DEMOS REQUIRED                      
         BE    VALDEMX                                                          
         B     ERROR                                                            
*                                                                               
VALDEM4  CLI   MFLDS,0             CALL DEMOVAL TO VALIDATE DEMOS               
         BE    EIDEM                                                            
*                                  TEST FOR SPECIAL INPUT KEYWORDS              
         CLC   FLD(2),=C'E='       GET DEMOS FROM ESTIMATE HEADER               
         BE    VALEST                                                           
         CLC   FLD(2),=C'M='       GET DEMOS FROM DEMO LIST                     
         BE    VALDMN                                                           
         CLC   FLD(2),=C'L='       ALL MODIFIERS FOR A DEMO                     
         BE    VALLST                                                           
*                                  VALIDATE INPUT DEMO LIST                     
         MVI   BYTE,C'S'            ASSUME SPOTPAK VALIDATION                   
         CLC   DBFIL,=C'PAV'        (TYPE OF VALIDATION BASED ON FILE)          
         BNE   *+8                                                              
         MVI   BYTE,0                REMOVE SPOTPAK VALIDATION                  
         L     R0,ADEMDEM                                                       
         CLC   DBFIL,=C'CTP'                                                    
         BNE   *+10                                                             
         MVC   DBFILE(3),=C'CUN'                                                
         GOTO1 VDEMOVAL,DMCB,(1,(R0)),(MFLDS,DEMS),(BYTE,DBLOCKD),0             
*        GOTO1 VDEMOVAL,DMCB,(1,DEMDEMH),(MFLDS,DEMS),(BYTE,DBLOCKD),0          
*^^GYL   GOTO1 VDEMOVAL,DMCB,(1,DEMDEMH),(MFLDS,DEMS),(C'S',DBLOCKD),0          
         CLI   0(R1),0             TEST FOR ERRORS                              
         BE    VALDEM6                                                          
         CLC   0(1,R1),MFLDS       TEST RETURN FNDX GR MAXIMUM FIELDS           
         BH    ETMI                                                             
         MVC   FNDX,0(R1)          SET FNDX FROM DEMOVAL FNDX                   
         B     EIDEM                                                            
*                                                                               
VALDEM6  MVC   NDEMS,4(R1)         SAVE N'DEMOS IN OUTPUT LIST                  
* for radar stop cum demos                                                      
*                                                                               
         CLI   DBSRC,C'R'                                                       
         BNE   VALDM6AA                                                         
         LA    RE,DEMS             RADAR DEMOS MUST NOT BE CUM                  
         ZIC   R0,NDEMS                                                         
         LA    R1,1                                                             
         CLI   1(RE),C'C'                                                       
         BNE   VALDM6AA                                                         
         B     EMRAD                                                            
*                                                                               
VALDM6AA L     R2,AACTNTRY                                                      
         TM    ACTINDS-ACTTABDD(R2),ACTAVAIL                                    
         BZ    VALDEM22                                                         
*                                                                               
         LA    RE,DEMS             AVAIL DEMOS MUST BE RTGS/IMPS ONLY           
         ZIC   R0,NDEMS                                                         
         LA    R1,1                                                             
VALDEM6A CLI   1(RE),C'R'                                                       
         BE    VALDEM6B                                                         
         CLI   1(RE),C'I'                                                       
         BE    VALDEM6B                                                         
         CLI   1(RE),C'E'          CANADIAN EXTENDED RATING                     
         BE    VALDEM6B                                                         
         STC   R1,FNDX                                                          
         B     EIDM                ERROR ISSUED IF NOT                          
VALDEM6B LA    RE,3(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,VALDEM6A                                                      
         B     VALDEM22                                                         
*                                                                               
VALDEM10 CLI   NDEMS,0             TEST DEMOS SET (AFFID LOOK-UP)               
         BNE   VALDEM22                                                         
*&&DO                                                                           
         L     R1,AEBREC           BUILD DEMO LIST FROM ESTIMATE DEFN.          
         LA    R1,DCNELEM-EBDD(R1)                                              
         SR    R0,R0                                                            
         USING EDLELEM,R1          FIND DEMO LIST ELEMENT                       
VALDEM12 CLI   EDLCODE,0                                                        
         BE    ERROR                                                            
         CLI   EDLCODE,EDLCODEQ                                                 
         BE    *+14                                                             
         IC    R0,EDLLEN                                                        
         AR    R1,R0                                                            
         B     VALDEM12                                                         
*                                                                               
         ZIC   RF,EDLLEN           BUILD LIST FROM DEMO ELEMENT                 
         SH    RF,=H'2'                                                         
         SR    RE,RE                                                            
         LA    R0,L'EDLDEMOS                                                    
         DR    RE,R0                                                            
         LR    R0,RF               R0=N'ENTRIES IN ELEMENT                      
         LA    R1,EDLDEMOS         R1=A(FIRST DEMO IN ELEMENT)                  
         LA    RE,DEMS             RE=A(FIRST DEMO IN OUTPUT LIST)              
         SR    RF,RF               RF=N'DEMOS IN OUTPUT LIST                    
VALDEM14 CLI   1(R1),C'R'          MUST BE RTGS/IMPS ONLY                       
         BE    *+8                                                              
         CLI   1(R1),C'I'                                                       
         BE    *+8                                                              
         CLI   1(R1),C'E'                                                       
         BNE   VALDEM16                                                         
         MVC   0(3,RE),0(R1)       ADD ENTRY TO OUTPUT LIST                     
         LA    RE,3(RE)                                                         
         LA    RF,1(RF)            BUMP N'DEMOS IN LIST                         
         CLM   RF,1,MFLDS                                                       
         BE    VALDEM18                                                         
VALDEM16 LA    R1,L'EDLDEMOS(R1)   BUMP TO NEXT DEMO IN ELEMENT                 
         BCT   R0,VALDEM14                                                      
         LTR   RF,RF                                                            
         BZ    ERROR                                                            
VALDEM18 MVI   0(RE),X'FF'                                                      
         STC   RF,NDEMS                                                         
*&&                                                                             
* Since the ESTIMATE DEFN record is no longer around, I don't know              
*  where to get demos.  So for now, coming here will just be an error.          
         B     ERROR                                                            
*                                                                               
VALDEM22 CLI   IACTN,NEXT          TEST IF NEXT IS VALID                        
         BNE   VALDEM24                                                         
         CLC   NDEMS,LNDEMS                                                     
         BNE   EKHC                                                             
         ZIC   R1,NDEMS                                                         
         LA    R0,L'DEMS                                                        
         MR    R0,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DEMS(0),LDEMS                                                    
         BNE   EKHC                                                             
*                                                                               
VALDEM24 DS    0H                                                               
         NI    MISCFLG1,XFF-MF1WHHD ASSUME NO WEEKLY HH DEMOS REQUESTED         
         LA    R1,DEMS                                                          
VDM028B  DS    0H                                                               
         CLI   0(R1),XFF            ARE WE AT END OF DEMOLIST?                  
         BE    VDM028X               YEP                                        
         CLI   2(R1),DNWHH1         IS THIS A WEEKLY HH DEMO?                   
         BL    VDM028G                                                          
         CLI   2(R1),DNWHH4                                                     
         BH    VDM028G                                                          
         OI    MISCFLG1,MF1WHHD      YEP                                        
         B     VDM028X                                                          
VDM028G  DS    0H                                                               
         AHI   R1,L'DEMS                                                        
         B     VDM028B                                                          
VDM028X  EQU   *                                                                
*                                                                               
         DS    0H                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32           IF DEM32 SESSION,           
         BNO   VDM032X                                                          
         TM    MISCFLG1,MF1WHHD                      AND WKLY HH DEMOS,         
         BNO   VDM032X                                                          
         CLC   DBSTABK+(STABKS-STABKD)(2),=X'6203'   NO DATA BFOR MAR98         
         BL    ENWHHD                                                           
VDM032X  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         TM    MISCFLG1,MF1WKACR   IF DISPLAYING WEEKS ACROSS SCREEN,           
         BNO   VALDEMA                                                          
         CLI   NDEMS,2              BUT # DEMOS INPUT > 2,                      
         BNH   VALDEMA                                                          
         XI    MISCFLG1,MF1WKACR    TURN OFF WEEKS ACROSS SCREEN                
*                                  CONVERT DEMOS TO PRINTABLE FORMAT            
VALDEMA  DS    0H                                                               
         ZIC   R1,NDEMS            BUILD WORKING DEMOLIST                       
         LA    R0,L'DEMS                                                        
         MR    R0,R0                                                            
         EXMVC R1,WORK,DEMS         BE SURE TO INCLUDE X'FF' FOR END            
         CLI   DBMED,C'R'          IF MEDIA=RADIO,                              
         BNE   VALDEMAB                                                         
         LA    R1,WORK                                                          
VALDEMAA CLI   0(R1),X'FF'          GO THROUGH DEMOLIST AND                     
         BE    VALDEMAB                                                         
         MVI   0(R1),0              FORCE DEFAULT GEOGRAPHICAL AREA             
         LA    R1,L'DEMS(R1)                                                    
         B     VALDEMAA                                                         
                                                                                
VALDEMAB DS    0H                                                               
         CLC   DBFIL,=C'PAV'       IF PAV FILE,                                 
         BNE   VALDEMAD                                                         
         LA    R1,WORK                                                          
VALDEMAC CLI   0(R1),X'FF'          GO THROUGH DEMOLIST AND                     
         BE    VALDEMAD                                                         
         CLI   1(R1),C'T'                                                       
         BNE   *+8                                                              
         MVI   1(R1),C'I'           REPLACE 'T's W/ 'I's BEFORE DEMOCON         
         LA    R1,L'DEMS(R1)                                                    
         B     VALDEMAC                                                         
                                                                                
VALDEMAD DS    0H                                                               
         MVI   BYTE,C'S'            ASSUME SPOTPAK VALIDATION                   
         CLC   DBFIL,=C'PAV'        (TYPE OF VALIDATION BASED ON FILE)          
         BNE   *+8                                                              
         MVI   BYTE,0                REMOVE SPOTPAK VALIDATION                  
**       CLC   DBFIL,=C'CTP'                                                    
**       BNE   *+10                                                             
**       MVC   DBFILE(3),=C'CUN'                                                
         GOTO1 VDEMOCON,DMCB,(NDEMS,WORK),(6,DEMOUT6),(BYTE,DBLOCKD),0          
*^^GYL   GOTO1 VDEMOCON,DMCB,(NDEMS,WORK),(6,DEMOUT6),(C'S',DBLOCKD)            
         GOTO1 (RF),(R1),(NDEMS,WORK),(7,DEMOUT75),(BYTE,DBLOCKD),0             
*^^GYL   GOTO1 (RF),(R1),(NDEMS,WORK),(7,DEMOUT75),(C'S',DBLOCKD)               
         DROP  R4                                                               
VALDEMX  B     VALOPT                                                           
         EJECT                                                                  
* READ ESTIMATE HEADER & EXTRACT DEMO LIST                                      
* INPUT FIELD FORMAT IS - E=M/CLT/PRD/EST                                       
*                                                                               
VALEST   CLI   FLDH+5,11           CHECK MIN & MAX INPUT LENGTHS                
         BL    EFTS                                                             
         CLI   FLDH+5,15                                                        
         BH    EFTL                                                             
         ZIC   R1,FLDH+7                                                        
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),FLD+2        SHIFT OFF LEADING KEYWORD                    
         LA    R1,1(R1)                                                         
         STC   R1,FLDH+5           SET NEW INPUT LENGTH                         
         MVI   SCAN1HL,10          (BUMP-SCANNER-BLOCK RTN USES THIS)           
         MVI   SCANLNTH,10                                                      
         L     R0,AIOAREA2                                                      
         GOTO1 VSCANNER,DMCB,(SCANLNTH,FLDH),(5,(R0)),C',=/='                   
         MVC   SCANDLIM,DMCB+8                                                  
         MVC   NFLDS,4(R1)                                                      
         CLI   NFLDS,0             TEST FOR SCANNER ERRORS                      
         BE    EIIF                                                             
         CLI   NFLDS,4             MUST BE FOUR SUB-FIELDS                      
         BNE   EIIF                                                             
         L     R2,AIOAREA2                                                      
         LA    R3,KEY                                                           
         USING ESTD,R3             R3=A(ESTIMATE HEADER KEY)                    
         XC    EKEY,EKEY                                                        
*                                                                               
*VALEST2  BAL   RE,BUMPSCAN         VALIDATE MEDIA CODE                         
VALEST2  BAS   RE,BUMPSCAN         VALIDATE MEDIA CODE                          
         CLI   1(R2),0                                                          
         BNE   EIIF                                                             
         CLI   0(R2),1             MEDIA IS ONE BYTE LONG                       
         BH    EFTL                                                             
         BL    EIIF                                                             
         GOTO1 VMEDGET,DMCB,(12(R2),AGYALPH),VDATAMGR,WORK                      
         CLI   8(R1),X'FF'         TEST MEDIA CODE IS VALID                     
         BE    EIMD                                                             
         MVC   EKEYAM,WORK         SET AGENCY/MEDIA IN KEY                      
*                                                                               
*        BAL   RE,BUMPSCAN         VALIDATE CLIENT                              
         BAS   RE,BUMPSCAN         VALIDATE CLIENT                              
         CLI   1(R2),0                                                          
         BNE   EIIF                                                             
         CLI   0(R2),2             CLIENT CODE IS 2 OR 3 BYTES LONG             
         BL    EFTS                                                             
         CLI   0(R2),3                                                          
         BH    EFTL                                                             
         GOTO1 VCLPACK,DMCB,12(R2),EKEYCLT                                      
         CLI   0(R1),0             TEST CLIENT CODE IS VALID                    
         BNE   EICL                                                             
*                                                                               
*        BAL   RE,BUMPSCAN         VALIDATE PRODUCT CODE                        
         BAS   RE,BUMPSCAN         VALIDATE PRODUCT CODE                        
         CLI   1(R2),0                                                          
         BNE   EIIF                                                             
         CLI   0(R2),2             PRODUCT IS 2 OR 3 BYTES LONG                 
         BL    EFTS                                                             
         CLI   0(R2),3                                                          
         BH    EFTL                                                             
         MVC   EKEYPRD,12(R2)                                                   
*                                                                               
**       BAL   RE,BUMPSCAN         VALIDATE ESTIMATE NUMBER                     
         BAS   RE,BUMPSCAN         VALIDATE ESTIMATE NUMBER                     
         CLI   1(R2),0                                                          
         BNE   EIIF                                                             
         CLI   0(R2),1             ESTIMATE IS 1 THRU 3 BYTES LONG              
         BL    EIIF                                                             
         CLI   0(R2),3                                                          
         BH    EIIF                                                             
         TM    2(R2),X'80'                                                      
         BZ    EFNN                                                             
         OC    4(4,R2),4(R2)       TEST ZERO INPUT                              
         BZ    EFVS                                                             
         OC    4(3,R2),4(R2)       TEST GR 255                                  
         BNZ   EFVB                                                             
         MVC   EKEYEST,7(R2)                                                    
*                                                                               
         MVI   IOFLAG,SPT+READ+FIL READ ESTIMATE HEADER RECORD                  
         XC    NDXDA,NDXDA         FORCE DIRECTORY READ                         
         GOTO1 AIO                                                              
         BL    EIIO                TEST FOR I/O ERRORS                          
         BH    ENOF                                                             
*                                  BUILD DEMO LIST FROM RECORD                  
         L     R2,AACTNTRY         R2=A(ACTION TABLE ENTRY)                     
         L     R3,AIOAREA1          R3=A(ESTIMATE HEADER RECORD)                
         LA    R1,EDEMLST          POINT TO ESTHDR DEMO LIST                    
         ZIC   R0,MFLDS            R0=MAX N'DEMOS                               
         LA    RE,DEMS             RE=A(OUTPUT DEMO LIST AREA)                  
         SR    RF,RF               RF=N'OUTPUT LIST ENTRIES                     
VALEST4  OC    0(2,R1),0(R1)       TEST E-O-L                                   
         BZ    VALESTA                                                          
         CLI   1(R1),21            TEST FOR USER DEMO                           
         BE    VALEST8             YES - SKIP                                   
         CLI   1(R1),63            TEST FOR WEIGHTED DEMO                       
         BE    VALEST8             YES - SKIP                                   
         TM    ACTINDS-ACTTABDD(R2),ACTAVAIL                                    
         BZ    VALEST6                                                          
         CLI   1(R1),C'R'          YES - ONLY INCLUDE RTG/IMPS                  
         BE    VALEST6                                                          
         CLI   1(R1),C'E'                                                       
         BE    VALEST6                                                          
         CLI   1(R1),C'I'                                                       
         BNE   VALEST8                                                          
VALEST6  MVC   0(3,RE),0(R1)       MOVE ENTRY TO OUTPUT LIST                    
         LA    RE,3(RE)                                                         
         LA    RF,1(RF)                                                         
VALEST8  LA    R1,3(R1)                                                         
         BCT   R0,VALEST4                                                       
*                                  ENSURE OUTPUT LIST IS BUILT                  
VALESTA  LTR   RF,RF               TEST ANY DEMOS IN OUTPUT LIST                
         BZ    EIEH                NO                                           
         STC   RF,NDEMS            SET N'DEMOS IN LIST                          
         MVI   0(RE),X'FF'         SET LIST TERMINATOR                          
VALESTX  B     VALDEM22                                                         
         DROP  R3                                                               
         EJECT                                                                  
* READ DEMO MENU RECORD AND EXTRACT DEMO LIST                                   
* INPUT FIELD FORMAT IS - M=M/LIST                                              
*                                                                               
VALDMN   DS    0H                  DEMO MENU                                    
         L     RE,ASYSNTRY                                                      
         CLI   (SYSOVSYS-SYSTABD)(RE),2   ONLY FOR SPOT SYSTEM                  
         BNE   EIIF                                                             
                                                                                
*                                                                               
         CLI   FLDH+5,5            CHECK MIN & MAX INPUT LENGTHS                
         BL    EFTS                                                             
         CLI   FLDH+5,8                                                         
         BH    EFTL                                                             
         ZIC   R1,FLDH+7                                                        
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),FLD+2        SHIFT OFF LEADING KEYWORD                    
         LA    R1,1(R1)                                                         
         STC   R1,FLDH+5           SET NEW INPUT LENGTH                         
         MVI   SCAN1HL,10          (BUMP-SCANNER-BLOCK RTN USES THIS)           
         MVI   SCANLNTH,10                                                      
         L     R0,AIOAREA2                                                      
         GOTO1 VSCANNER,DMCB,(SCANLNTH,FLDH),(3,(R0)),C',=/='                   
         MVC   SCANDLIM,DMCB+8                                                  
         MVC   NFLDS,4(R1)                                                      
         CLI   NFLDS,0             TEST FOR SCANNER ERRORS                      
         BE    EIIF                                                             
         CLI   NFLDS,2             MUST BE TWO SUB-FIELDS                       
         BH    EIIF                                                             
         L     R2,AIOAREA2                                                      
         LA    R3,KEY                                                           
         USING DMNRECD,R3          R3=A(ESTIMATE HEADER KEY)                    
         XC    DMNKEY,DMNKEY                                                    
         MVC   DMNKTYP,=X'0D26'                                                 
*                                                                               
*VALDMN2  BAL   RE,BUMPSCAN         VALIDATE MEDIA CODE                         
VALDMN2  BAS   RE,BUMPSCAN         VALIDATE MEDIA CODE                          
         CLI   1(R2),0                                                          
         BNE   EIIF                                                             
         CLI   0(R2),1             MEDIA IS ONE BYTE LONG                       
         BH    EFTL                                                             
         BL    EIIF                                                             
         GOTO1 VMEDGET,DMCB,(12(R2),AGYALPH),VDATAMGR,WORK                      
         CLI   8(R1),X'FF'         TEST MEDIA CODE IS VALID                     
         BE    EIMD                                                             
         MVC   DMNKAGMD,WORK                                                    
*                                                                               
***      BAL   RE,BUMPSCAN         VALIDATE MENU CODE                           
         BAS   RE,BUMPSCAN         VALIDATE MENU CODE                           
         CLI   1(R2),0                                                          
         BNE   EIIF                                                             
         CLI   0(R2),1             MENU CODE IS 1 THRU 4 BYTES LONG             
         BL    EIIF                                                             
         CLI   0(R2),4                                                          
         BH    EFTL                                                             
         MVC   DMNKCODE,12(R2)                                                  
*                                                                               
         MVI   IOFLAG,SPT+READ+FIL READ MENU RECORD                             
         XC    NDXDA,NDXDA         FORCE DIRECTORY READ                         
         GOTO1 AIO                                                              
         BL    EIIO                TEST FOR I/O ERRORS                          
         BH    EMNF                                                             
*                                  BUILD DEMO LIST FROM RECORD                  
         L     R2,AACTNTRY         R2=A(ACTION TABLE ENTRY)                     
         L     R3,AIOAREA1                                                      
         AHI   R3,DMNEL05-DMNRECD                                               
         USING DMNEL05,R3          R3=A(FIRST DEMO ELEMENT)                     
         ZIC   R0,MFLDS            R0=MAX N'DEMOS                               
         LA    RE,DEMS             RE=A(OUTPUT DEMO LIST AREA)                  
         SR    RF,RF               RF=N'OUTPUT LIST ENTRIES                     
         SR    R1,R1                                                            
VALDMN4  CLI   DMNEL05,0           TEST E-O-R                                   
         BE    VALDMNA                                                          
         CLI   DMNEL05,05          TEST DEMO ELEMENT                            
         BNE   VALDMN8                                                          
         CLI   DMNRTN+1,X'21'      TEST FOR USER DEMO                           
         BE    VALDMN8             YES - SKIP                                   
         CLI   DMNRTN+1,X'63'      TEST FOR WEIGHTED DEMO                       
         BE    VALDMN8             YES - SKIP                                   
         TM    ACTINDS-ACTTABDD(R2),ACTAVAIL                                    
         BZ    VALDMN6                                                          
         CLI   DMNRTN+1,C'R'       YES - ONLY INCLUDE RTG/IMPS                  
         BE    VALDMN6                                                          
         CLI   DMNRTN+1,C'E'                                                    
         BE    VALDMN6                                                          
         CLI   DMNRTN+1,C'I'                                                    
         BNE   VALDMN8                                                          
VALDMN6  MVC   0(3,RE),DMNRTN      MOVE DEMO TO OUTPUT LIST                     
         LA    RE,3(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VALDMN8          DO FOR MAX N'DEMOS                           
         B     VALDMNA                                                          
VALDMN8  IC    R1,DMN05LEN                                                      
         AR    R3,R1                                                            
         B     VALDMN4                                                          
*                                  ENSURE OUTPUT LIST IS BUILT                  
VALDMNA  LTR   RF,RF               TEST ANY DEMOS IN OUTPUT LIST                
         BZ    EIMR                NO                                           
         STC   RF,NDEMS            SET N'DEMOS IN LIST                          
         MVI   0(RE),X'FF'         SET LIST TERMINATOR                          
VALDMNX  B     VALDEM22                                                         
         DROP  R3                                                               
         EJECT                                                                  
* BUILD LIST OF ALL MODIFIERS FOR A DEMO VALUE                                  
* INPUT FIELD FORMAT IS - L=DEMO                                                
*                                                                               
VALLST   CLI   FLDH+5,3            CHECK MIN & MAX INPUT LENGTHS                
         BL    EFTS                                                             
         CLI   FLDH+5,12                                                        
         BH    EFTL                                                             
         ZIC   R1,FLDH+7                                                        
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),FLD+2        SHIFT OFF LEADING KEYWORD                    
         LA    R1,1(R1)                                                         
         STC   R1,FLDH+5           SET NEW INPUT LENGTH                         
         LA    R1,8(R1)                                                         
         STC   R1,FLDH             SET DUMMY TOTAL LENGTH                       
                                                                                
         MVI   BYTE,C'S'           ASSUME SPOTPAK VALIDATION                    
         CLC   DBFIL,=C'PAV'        (TYPE OF VALIDATION BASED ON FILE)          
         BNE   *+8                                                              
         MVI   BYTE,0               REMOVE SPOTPAK VALIDATION                   
         GOTO1 VDEMOVAL,DMCB,(1,FLDH),(2,WORK),(BYTE,DBLOCK1),0                 
*^^GYL   GOTO1 VDEMOVAL,DMCB,(1,FLDH),(2,WORK),(C'S',DBLOCK1),0                 
         CLI   0(R1),0             TEST FOR ERRORS                              
         BNE   EIIF                                                             
         CLI   4(R1),1             TEST RETURN FNDX GR MAXIMUM FIELDS           
         BH    EIIF                                                             
*                                  BUILD LIST FROM MODTBL                       
         L     R2,AACTNTRY         R2=A(ACTION TABLE ENTRY)                     
         LA    R1,MODTBL           R1=A(MODIFIER LIST)                          
         CLI   DBMED,C'C'          TEST FOR CANADA                              
         BNE   *+8                                                              
         LA    R1,CANTBL           YES-USE CANADA MODIFIER LIST                 
         ZIC   R0,MFLDS            R0=MAX N'DEMOS                               
         LA    RE,DEMS             RE=A(OUTPUT DEMO LIST AREA)                  
         SR    RF,RF               RF=N'OUTPUT LIST ENTRIES                     
VALLST2  CLI   0(R1),EOT           TEST E-O-L                                   
         BE    VALLST8                                                          
         TM    ACTINDS-ACTTABDD(R2),ACTAVAIL                                    
         BZ    VALLST4                                                          
         CLI   0(R1),C'R'          YES - ONLY INCLUDE RTG/IMPS                  
         BE    VALLST4                                                          
         CLI   0(R1),C'E'                                                       
         BE    VALLST4                                                          
         CLI   0(R1),C'I'                                                       
         BNE   VALLST6                                                          
VALLST4  MVI   0(RE),0             BUILD AN OUTPUT ENTRY                        
         MVC   1(1,RE),0(R1)                                                    
         MVC   2(1,RE),WORK+2                                                   
         LA    RF,1(RF)                                                         
         LA    RE,3(RE)                                                         
VALLST6  LA    R1,1(R1)                                                         
         BCT   R0,VALLST2                                                       
*                                                                               
VALLST8  STC   RF,NDEMS            SET N'DEMOS IN LIST                          
         MVI   0(RE),X'FF'         SET LIST TERMINATOR                          
VALLSTX  B     VALDEM22                                                         
         SPACE 1                                                                
MODTBL   DC    C'RSPIXQU',AL1(EOT)                                              
CANTBL   DC    C'REI',AL1(EOT)                                                  
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
*                                                                               
VALOPT   MVI   HELPREQD,HELPOPT                                                 
         XC    SCANDLIM,SCANDLIM                                                
         XC    OPTI,OPTI           CLEAR INPUT OPTIONS BITS                     
         MVI   SCAN1HL,10          (BUMP-SCANNER-BLOCK RTN USES THIS)           
         MVI   SCANLNTH,63                                                      
         CLI   OPTMBKH,0                                                        
         BE    *+22                                                             
         MVC   SCANLNTH(1),OPTMBKH                                              
         BRAS  RE,ADDMBK           ADD STORED AWAY MBK INTO OPTIONS             
         LA    R1,WORK2                                                         
         B     *+8                                                              
         L     R1,ADEMOPT                                                       
VALOPT2  MVI   FERN,1                                                           
         GOTO1 FVAL,(R1)                                                        
         BE    VALOPT22                                                         
         MVI   HELPREQD,0                                                       
*                                  SCAN THE INPUT OPTIONS FIELD                 
         ZIC   R0,FLDH+5                                                        
         STCM  R0,3,FULL+0         L(INPUT STRING) PARAMETER                    
         MVC   FULL+2(1),SCAN1HL   OVRD L(1ST HALF FLD) PARAMETER               
         GOTO1 AMYSCAN,DMCB,(SCANLNTH,FLD),AIOAREA1,C',=,=',FULL                
*        GOTO1 VSCANNER,DMCB,(SCANLNTH,FLDH),AIOAREA1,C',=,='                   
         MVC   SCANDLIM,DMCB+8                                                  
         MVC   NFLDS,4(R1)         SAVE N'SCANNER LINES                         
         L     R2,AIOAREA1                                                      
*                                                                               
*VALOPT4  BAL   RE,BUMPSCAN                                                     
VALOPT4  BAS   RE,BUMPSCAN                                                      
         BH    VALOPT22                                                         
                                                                                
         MVC   SCANDLIM,=C',=,='   RSTORE SCANNER DELMTERS FOR OPT FLD          
                                                                                
         CLI   0(R2),0                                                          
         BE    EIIF                                                             
         CLI   0(R2),L'OPTNAME                                                  
         BH    EIIF                                                             
         ZIC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         L     R3,AOPTTAB                                                       
         USING OPTTABD,R3          R3=A(OPTIONS TABLE)                          
*                                                                               
VALOPT6  CLI   OPTNAME,EOT         TEST E-O-T                                   
         BE    EIKW                                                             
         CLC   OPTSHRT,SPACES      TEST IF OPTION HAS A SHORT NAME              
         BE    VALOPT10                                                         
         LA    RE,1                SHORT NAME CAN BE 2 OR 3 BYTES LONG          
         CLI   OPTSHRT+2,C' '                                                   
         BE    *+8                                                              
         LA    RE,2                                                             
         CR    R1,RE               TEST L'KEYWORD = L'INPUT KEYWORD             
         BNE   VALOPT10                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   OPTSHRT(0),12(R2)   MATCH ON OPTION SHORT NAME                   
         BE    VALOPT14                                                         
VALOPT10 CLC   0(1,R2),OPTMINKL                                                 
         BL    VALOPT12                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   OPTNAME(0),12(R2)   MATCH ON FULL OPTION NAME                    
         BE    VALOPT14                                                         
*                                                                               
VALOPT12 LA    R3,OPTTABL(R3)      BUMP TO NEXT OPTION TABLE ENTRY              
         B     VALOPT6                                                          
*                                                                               
VALOPT14 ST    R3,AOPTNTRY         SAVE A(THIS OPTION TABLE ENTRY)              
         TM    OPTINDS,DDSONLY     TEST IF A DDS ONLY OPTION                    
         BZ    *+12                                                             
         CLI   DDS,C'Y'                                                         
         BNE   EIKW                                                             
* This code was commented out because the STERONLY flag conflicts with          
*  the OPTHELPH flag in the options table.  It would be best if we              
*  could re-instate this code sometime in the future.                           
*&&DO                                                                           
         TM    OPTINDS,STERONLY    TEST IF A (FULL) STEREO ONLY OPTION          
         BZ    *+12                                                             
         TM    DEMFLAG1,DF1STERO                                                
         BZ    EIKW                                                             
*&&                                                                             
*GLEE - The temporary code below is to get around the DEM32 bug which           
*GLEE -  passes the DMA option for each request.                                
*^^TEMP                                                                         
         CLI   OPTOPTN,OPTDMAN             IF  DMA=  OPTION                     
         BNE   VALDMAX                                                          
         TM    DEMFLAG1,DF1STERO+DF1DEM32   AND DEM32 SESSION,                  
         BNO   VALDMAX                                                          
         MVC   DUB(4),OPTX                  USE THIS COMPATIBILITY CHK          
         NC    DUB(4),OPTOPTB                                                   
         BNZ   VALOPT4                      IGNORE IF NOT COMPATIBLE            
VALDMAX  EQU   *                                                                
*^^EOTEMP                                                                       
         CLI   OPTOPTN,OPTIMSN     IF IMS OPTION                                
         BNE   *+8                                                              
         MVI   OPTIMSF,C'A'                                                     
         CLI   OPTOPTN,OPTISSN     IF ISS OPTION                                
         BNE   *+8                                                              
         MVI   OPTIMSF,C'S'                                                     
         MVC   DUB(4),OPTX         TEST IF THIS OPTION IS COMPATIBLE            
         NC    DUB(4),OPTOPTB                                                   
         BZ    VLOPT14C                                                         
         CLI   OPTOPTN,OPTIMSN     IF IMS OPTION                                
         BNE   EKWI                IF NOT IMS OPTION (REG INVALID MSG)          
         CLI   ACTN,DEMOQHR        QTR HOUR SCHEDULE                            
         BE    EIMSINV                                                          
         B     EKIMSI              ELSE PRINT A IMS MSG                         
VLOPT14C MVC   DUB(4),OPTI         TEST IF OPTION PREVIOUSLY INPUT              
         NC    DUB(4),OPTOPTB                                                   
         BNZ   EDIF                                                             
         OC    OPTI,OPTOPTB        NO - SET THIS OPTION INPUT                   
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,OPTIADDR                                                    
         A     R1,ABASE                                                         
         ST    R1,IADDR            SET A(TABLE OR VALIDATION ROUTINE)           
         SR    R1,R1                                                            
         ICM   R1,3,OPTOADDR                                                    
         LA    R1,DEMWRKD(R1)                                                   
         ST    R1,OADDR            SET A(OUTPUT FIELD)                          
                                                                                
         TM    OPTINDS,OPTKONLY    IF OPTION IS KEYWORD ONLY,                   
         BZ    VLOPT15                                                          
         GOTO1 BLDFLD,0            BUILD A TWA FIELD FROM LHS  PORTION          
         B     VLOPT15A                                                         
VLOPT15  GOTO1 BLDFLD,1            BUILD A TWA FIELD FROM DATA PORTION          
*                                                                               
VLOPT15A CLC   FLDH+5(1),OPTMINDL  CHECK DATA LENGTH                            
         BL    EDTS                                                             
         CLC   FLDH+5(1),OPTMAXDL                                               
         BH    EDTL                                                             
*                                                                               
         L     RF,IADDR            RE=A(TABLE OR ROUTINE)                       
         MVI   FERN,OK                                                          
         XC    EADDR,EADDR                                                      
         TM    OPTINDS,OPTARTN                                                  
         BZ    VALOPT16                                                         
*                                                                               
***      BAL   RE,60(RF)           GO TO FIELD VALIDATION ROUTINE               
         BAS   RE,60(RF)           GO TO FIELD VALIDATION ROUTINE               
         ICM   RF,15,EADDR         TEST FOR ERRORS                              
         BNZR  RF                                                               
         CLI   FERN,OK             TEST FOR ERRORS                              
         BE    VALOPT20                                                         
         B     EXIT                YES - ERROR & CURSOR SET SO EXIT             
*                                                                               
VALOPT16 TM    OPTINDS,OPTATAB     PROCESS DATA TABLE                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    OPTINDS,OPTHELPH    TEST TABLE HAS HELP HEADER                   
         BZ    *+8                                                              
         LA    RF,60(RF)           YES - BUMP OVER IT                           
         ZIC   R0,0(RF)            R0=L'LHS OF TABLE                            
         ZIC   R1,1(RF)            R1=L'RHS OF TABLE                            
         LA    RF,2(RF)            POINT TO FIRST TABLE ENTRY                   
         AR    R0,R1               R0=L'DATA TABLE                              
         ZIC   RE,FLDH+7           RF=L'DATA                                    
*                                                                               
VALOPT18 CLI   0(RF),EOT           TEST E-O-T                                   
         BE    EIDV                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(RF)        MATCH INPUT WITH TABLE                       
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     VALOPT18                                                         
         AR    RF,R0                                                            
         SR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RF)                                                    
*                                                                               
VALOPT20 ZIC   R1,OPTOUTDL         MOVE DATA TO OUTPUT AREA                     
         BCTR  R1,0                                                             
         L     RE,OADDR                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),WORK                                                     
         OC    OPTV,OPTOPTR                                                     
         B     VALOPT4                                                          
*                                                                               
VALOPT22 MVC   DUB(4),OPTI         TEST ALL REQUIRED OPTIONS INPUT              
         NC    DUB(4),OPTR                                                      
         CLC   DUB(4),OPTR                                                      
         BE    VALOPT26                                                         
         L     R3,AOPTTAB          NO - LOCATE A MISSING OPTION                 
VALOPT24 CLI   OPTNAME,0           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                THAT WON'T DO                                
         MVC   DUB(4),OPTR                                                      
         NC    DUB(4),OPTOPTB                                                   
         BNZ   *+12                                                             
         LA    R3,OPTTABL(R3)                                                   
         B     VALOPT24                                                         
         MVC   XTRA(L'OPTNAME),OPTNAME                                          
         B     EROM                                                             
VALOPT26 CLI   IACTN,NEXT          TEST IF NEXT IS VALID                        
         BNE   VALOPTX                                                          
         CLC   OPTNS,LOPTNS                                                     
         BNE   EKHC                                                             
*                                                                               
VALOPTX  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IF THIS IS A STEREO SESSION,                 
         BZ    GO                                                               
         B     EXIT                 EXIT "SUB-ROUTINE" WE'RE IN                 
                                                                                
         DROP  R3                                                               
         EJECT                                                                  
* VALIDATE UPGRADE EXPRESSION                                                   
*                                                                               
VALUPGD  DC    CL60'type/fld1(/fld2) - fld=book or value'                       
VALUPGDN NTR1  ,                                                                
         L     R3,AOPTNTRY                                                      
         USING OPTTABD,R3                                                       
         CLC   DBFIL(1),OPTSHRT+2  MAKE SURE UPGRADE FILE CONSISTENT            
         BNE   EKWI                                                             
         GOTO1 VUPVAL,DMCB,FLDH,WORK+20,(C'/',AFAC)                             
         CLI   0(R1),0             TEST FOR ERRORS                              
         BE    EIUE                                                             
         MVC   WORK(L'OPTUPGD),WORK+24                                          
         MVC   OPTUPFIL,OPTSHRT+2  SET UPGRADE FILE (FROM SHORT NAME)           
         MVC   OPTUPBT,WORK+23                                                  
         MVC   OPTLPMMK,WORK+32    SAVE LPM PUT INDEX MKT                       
*&&DO                                                                           
         CLI   DBMED,C'T'                                                       
         BNE   *+16                                                             
         CLI   OPTUPBT,0           MAKE BOOKTYPE LOWERCASE                      
         BE    *+8                                                              
         XI    OPTUPBT,X'40'                                                    
*&&                                                                             
         CLI   WORK,SPUPTSAV       SAVG UPGRADE?                                
         BNE   *+14                CANT EXCEED 3 BOOKS                          
         OC    WORK+26+(3*L'OPTMBK)(2),WORK+26+(3*L'OPTMBK)                     
         BNZ   ESAVGBK             ERROR- PROPOSER PASSED 4 BOOKS               
                                                                                
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
* VALIDATE UPGRADE DAY/TIME EXPRESSION                                          
*                                                                               
VALUPDT  DC    CL60'day1(-day2)/time1(-time2)'                                  
VALUPDTN NTR1  ,                                                                
         L     R0,AIOAREA2                                                      
         GOTO1 VSCANNER,DMCB,(20,FLDH),(2,(R0)),C',=,/'                         
         MVC   SCANDLIM,DMCB+8                                                  
         CLI   4(R1),0                                                          
         BE    EIDV                                                             
         CLI   4(R1),1                                                          
         BH    ETMI                                                             
         L     R2,AIOAREA2                                                      
*                                                                               
         CLI   0(R2),0             TEST L'DAY EXPRESSION VALID                  
         BE    EIDV                                                             
         CLI   0(R2),7                                                          
         BH    EIDV                                                             
         CLI   1(R2),0             TEST TIME INPUT                              
         BE    EIDV                                                             
         GOTO1 VDAYPAK,DMCB,(0(R2),12(R2)),WORK,WORK+1                          
         CLI   WORK,0              TEST DAY EXPRESSION IS VALID                 
         BE    EIDE                                                             
         GOTO1 VTIMVAL,DMCB,(1(R2),22(R2)),WORK+1                               
         CLI   0(R1),X'FF'         TEST TIME EXPRESSION IS VALID                
         BE    EITE                                                             
         B     EXIT                                                             
         SPACE 1                                                                
* VALIDATE UPGRADE BOOK                                                         
*                                                                               
VALUPBK  DC    CL60'month/year'                                                 
VALUPBKN NTR1  ,                                                                
         USING OPTTABD,R3                                                       
         L     R0,AIOAREA2                                                      
         GOTO1 VSCANNER,DMCB,(20,FLDH),(2,(R0)),C',=,-'                         
         MVC   SCANDLIM,DMCB+8                                                  
         CLI   4(R1),0                                                          
         BE    EIBK                                                             
         CLI   4(R1),1                                                          
         BH    EIBK                                                             
         XC    FULL2,FULL2                                                      
         L     R2,AIOAREA2                                                      
         GOTO1 BLDFLD,0                                                         
         LA    RE,FLDH                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB(1),DBSRC                                                    
         CLI   DBMED,C'N'                                                       
         BNE   *+8                                                              
         MVI   DMCB,C'N'                                                        
         CLI   DBSRC,C'F'      FUSION VALIDATE BOOK LIKE NSI                    
         BNE   *+8                                                              
         MVI   DMCB,C'N'                                                        
                                                                                
         MVC   DMCB+08(4),VSCANNER                                              
         MVI   DMCB+08,C'B'        SET TO BOOKTYPE OPTION                       
         CLI   OPTOPTN,OPTVPHN     IF OPTION IS VPH                             
         BNE   *+8                                                              
         MVI   DMCB+08,0            TURN BOOKTYPE OPTION OFF                    
         GOTO1 VBOOKVAL,DMCB,,(1,DUB),,FULL2,(C'C',AFAC)                        
*                                                                               
*-- CHECK BOOK TYPES                                                            
         CLI   FULL2,0                                                          
         BE    *+28                                                             
         CLI   OPTUPBT,0                                                        
         BNE   *+10                                                             
         MVC   OPTUPBT(1),FULL2                                                 
         CLC   OPTUPBT(1),FULL2                                                 
         BNE   EIBK                                                             
*&&DO                                                                           
         CLI   DBMED,C'T'                                                       
         BNE   *+16                                                             
         CLI   OPTUPBT,0           MAKE BOOKTYPE LOWERCASE                      
         BE    *+8                                                              
         XI    OPTUPBT,X'40'                                                    
*&&                                                                             
         CLI   4(R1),1             TEST BOOK IS VALID                           
         BNE   EIBK                                                             
         TM    DUB,X'AE'           NO FUNNY EXPRESSIONS ALLOWED                 
         BNZ   EIBK                                                             
         MVC   WORK(2),DUB+1                                                    
*                                                                               
         CLI   1(R2),1             TEST WEEK NUMBER PRESENT                     
         BL    VUB049                                                           
         BH    EIBK                                                             
         CLI   OPTOPTN,OPTVPHN     WEEK NUMBER NOT VALID FOR VPH OPTN           
         BE    EIBK                                                             
         MVC   WORK+3(1),22(R2)                                                 
         CLI   WORK+3,C'1'         TEST WEEK 1-4                                
         BL    EIBK                                                             
         CLI   WORK+3,C'4'                                                      
         BH    EIBK                                                             
         NI    WORK+3,X'0F'                                                     
         PACK  WORK+4(1),WORK+3(1)                                              
         OC    WORK+1(1),WORK+4    OR WEEK INTO MONTH BYTE                      
VUB049   EQU   *                                                                
*                                                                               
         DS    0H                                                               
         CLI   OPTOPTN,OPTVPHN                                                  
         BNE   VUBVPHX                                                          
                                                                                
         DS    0H                  VPH OPTION ONLY VALID FOR RTG & IMP          
         LA    RE,DEMS                                                          
         ZIC   R0,NDEMS                                                         
         LHI   R1,1                                                             
VUBVPH05 DS    0H                                                               
         CLI   1(RE),C'R'                                                       
         BE    VUBVPH09                                                         
         CLI   1(RE),C'I'                                                       
         BE    VUBVPH09                                                         
         MVC   FADR,ADEMDEM         SET DEMOS FIELD AS PLACE OF ERROR           
         MVC   SCANDLIM,=C',=,='                                                
         STC   R1,FNDX                                                          
         B     EIDM                                                             
VUBVPH09 EQU   *                                                                
         AHI   RE,3                                                             
         AHI   R1,1                                                             
         BCT   R0,VUBVPH05                                                      
VUBVPHX  EQU   *                                                                
         DROP  R3                                                               
VALUPBKX B     EXIT                                                             
         EJECT                                                                  
* SET PRINT QUEUE REPORT-ID                                                     
*                                                                               
VALCLI   DC    CL60'2-3 character client'                                       
VALCLIN  MVC   WORK(3),FLD                                                      
         BR    RE                                                               
         SPACE 1                                                                
* SET PRINT QUEUE REPORT-ID                                                     
*                                                                               
VALPRNT  DC    CL60'3 character report-id'                                      
VALPRNTN MVC   WORK(3),FLD                                                      
         BR    RE                                                               
         SPACE 1                                                                
* VALIDATE NAME                                                                 
*                                                                               
VALSTRT  DC    CL60'up to 8 characters'                                         
VALSTRTN MVC   WORK(1),FLDH+7      SET L'NAME-1 & NAME                          
         MVC   WORK+1(8),FLD                                                    
         BR    RE                                                               
         SPACE 1                                                                
*&&DO                                                                           
* VALIDATE PROGRAM TYPE FILTER                                                  
*                                                                               
VALTYPE  DC    CL60'1 character code or 2 thru 7 character name'                
VALTYPEN ZIC   RF,FLDH+7           RF=L'INPUT-1                                 
         L     R1,AEBREC                                                        
         LA    R1,DCNELEM-EBDD(R1)                                              
         SR    R0,R0                                                            
         USING PCNELEM,R1          LOCATE PROGTYP ELEMENT ON RECORD             
VALTYPE2 CLI   PCNCODE,0                                                        
         BE    EIIF                                                             
         CLI   PCNCODE,PCNCODEQ                                                 
         BE    *+14                                                             
         IC    R0,PCNLEN                                                        
         AR    R1,R0                                                            
         B     VALTYPE2                                                         
         IC    R0,PCNLEN                                                        
         SH    R0,=H'2'                                                         
         SLL   R0,3                R0=NUMBER OF ENTRIES IN LIST                 
         LA    R1,PCNDATA          R1=A(FIRST ENTRY)                            
         USING PCNDATA,R1                                                       
VALTYPE4 LTR   RF,RF                                                            
         BNZ   *+14                                                             
         CLC   PCNDCODE,FLD        MATCH ON CODE (1 BYTE INPUT)                 
         BE    VALTYPE6                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   PCNDNAME(0),FLD     OR NAME (2 THRU 7 BYTES INPUT)               
         BE    VALTYPE6                                                         
         LA    R1,8(R1)            BUMP TO NEXT LIST ENTRY                      
         BCT   R0,VALTYPE4                                                      
         B     EIIF                                                             
VALTYPE6 MVC   WORK(1),PCNDCODE    SET PROGTYPE CODE                            
         BR    RE                                                               
         DROP  R1                                                               
*&&                                                                             
         EJECT                                                                  
*&&DO                                                                           
* VALIDATE DAYPART FILTER                                                       
*                                                                               
VALDPRT  DC    CL60'1 character code or 2 thru 7 character name'                
VALDPRTN ZIC   RF,FLDH+7           RF=L'INPUT-1                                 
         L     R1,AEBREC                                                        
         LA    R1,DCNELEM-EBDD(R1)                                              
         SR    R0,R0                                                            
         USING DCNELEM,R1          LOCATE DAYPART ELEMENT ON RECORD             
VALDPRT2 CLI   DCNCODE,0                                                        
         BE    EIIF                                                             
         CLI   DCNCODE,DCNCODEQ                                                 
         BE    *+14                                                             
         IC    R0,DCNLEN                                                        
         AR    R1,R0                                                            
         B     VALDPRT2                                                         
         IC    R0,DCNLEN                                                        
         SH    R0,=H'2'                                                         
         SLL   R0,3                R0=NUMBER OF ENTRIES IN LIST                 
         LA    R1,DCNDATA          R1=A(FIRST ENTRY)                            
         USING DCNDATA,R1                                                       
VALDPRT4 LTR   RF,RF                                                            
         BNZ   *+14                                                             
         CLC   DCNDCODE,FLD        MATCH ON CODE (1 BYTE INPUT)                 
         BE    VALDPRT6                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   DCNDNAME(0),FLD     OR NAME (2 THRU 7 BYTES INPUT)               
         BE    VALDPRT6                                                         
         LA    R1,8(R1)                                                         
         BCT   R0,VALDPRT4                                                      
         B     EIIF                                                             
VALDPRT6 MVC   WORK(1),DCNDCODE    SET DAYPART CODE                             
         BR    RE                                                               
         DROP  R1                                                               
*&&                                                                             
         EJECT                                                                  
* VALIDATE AFFID START & END DATES                                              
*                                                                               
VALDATE  DC    CL60'"start date"-"end date"'                                    
VALDATEN NTR1  WORK=(RC,DATEWRKX-DATEWRKD)                                      
         USING DATEWRKD,RC                                                      
         GOTO1 VDATCON,DATEPARM,(3,BUYSTR),(2,DATESTRQ)                         
         GOTO1 (RF),(R1),(3,BUYEND),(2,DATEENDQ)                                
         MVI   DATEFLAG,0                                                       
         CLI   FLD,C'-'            TEST START DATE GIVEN                        
         BNE   VALDATE2                                                         
         ZIC   R1,FLDH+5                                                        
         SH    R1,=H'1'                                                         
         BNP   EIIF                                                             
         STC   R1,FLDH+5                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),FLD+1                                                     
         OI    DATEFLAG,X'80'      SET START NOT REQUIRED                       
         L     R0,AIOAREA2                                                      
VALDATE2 GOTO1 VSCANNER,DMCB,FLDH,(3,(R0)),X'6B7E60FF'                          
         MVC   SCANDLIM,DMCB+8                                                  
         MVC   DATENFLD,4(R1)                                                   
         CLI   DATENFLD,0                                                       
         BE    EIIF                                                             
         LA    R0,2                                                             
         TM    DATEFLAG,X'80'                                                   
         BZ    *+8                                                              
         LA    R0,1                                                             
         CLM   R0,1,DATENFLD                                                    
         BL    ETMI                                                             
         L     R2,AIOAREA2                                                      
         TM    DATEFLAG,X'80'      TEST START DATE SET                          
         BNZ   VALDATE4                                                         
         MVI   FERN,OK             VALIDATE START DATE                          
         LA    R1,1                                                             
         BRAS  RE,DATEVAL                                                       
         CLI   FERN,OK                                                          
         BNE   VALDATEX                                                         
         MVC   DATESTRQ,DATEOUT                                                 
         CLI   DATENFLD,1                                                       
         BE    VALDATEX                                                         
         LA    R2,32(R2)                                                        
VALDATE4 MVI   FERN,OK             VALIDATE END DATE                            
         LA    R1,2                                                             
         BRAS  RE,DATEVAL                                                       
         CLI   FERN,OK                                                          
         BNE   VALDATEX                                                         
         MVC   DATEENDQ,DATEOUT                                                 
         MVC   WORK(L'OPTDATS),DATESTRQ                                         
VALDATEX CLI   FERN,OK                                                          
         B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO VALIDATE A START OR END DATE                                       
*                                                                               
*&&DO                                                                           
DATEVAL  NTR1  ,                                                                
         STC   R1,DATEWORK                                                      
         GOTO1 VDATVAL,DATEPARM,(0,12(R2)),DATEWORK+1                           
         L     R0,0(R1)                                                         
         CLM   R0,1,0(R2)                                                       
         BNE   DATEVAL2                                                         
         GOTO1 VDATCON,DATEPARM,(0,DATEWORK+1),(2,DATEOUT)                      
         B     DATEVAL6                                                         
DATEVAL2 GOTO1 VDATVAL,DATEPARM,(1,12(R2)),DATEWORK+1                           
         L     R0,0(R1)                                                         
         CLM   R0,1,0(R2)                                                       
         BNE   EIIF                                                             
         ZIC   R0,BUYSTR           R0=START YEAR                                
         CVD   R0,DATEDUB                                                       
         OI    DATEDUB+7,X'0F'                                                  
         UNPK  DATEWORK+1(2),DATEDUB                                            
         GOTO1 VDATCON,DATEPARM,(0,DATEWORK+1),(2,DATEOUT)                      
         CLC   BUYSTR(1),BUYEND                                                 
         BNE   DATEVAL4                                                         
         B     DATEVAL6                                                         
DATEVAL4 CLC   DATEOUT,DATESTRQ                                                 
         BNL   DATEVAL6                                                         
         ZIC   R0,BUYEND           R0=END YEAR                                  
         CVD   R0,DATEDUB                                                       
         OI    DATEDUB+7,X'0F'                                                  
         UNPK  DATEWORK+1(2),DATEDUB                                            
         GOTO1 VDATCON,DATEPARM,(0,DATEWORK+1),(2,DATEOUT)                      
DATEVAL6 CLI   DATEWORK,0          TEST START DATE                              
         BNE   DATEVAL8                                                         
         CLC   DATEOUT,DATEENDQ                                                 
         BH    EIIF                                                             
         B     DATEVALX                                                         
DATEVAL8 CLC   DATEOUT,DATESTRQ                                                 
         BL    EIIF                                                             
DATEVALX B     EXIT                                                             
         DROP  RC                                                               
         EJECT                                                                  
*&&                                                                             
* VALIDATE SECONDS LENGTH FILTER                                                
*                                                                               
VALSECS  DC    CL60'1 thru 3 characters numeric'                                
VALSECSN MVI   DUB,C'0'                                                         
         MVC   DUB+1(L'DUB-1),DUB                                               
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),FLD                                                       
         CLI   DUB,C'0'                                                         
         BNE   EFNN                                                             
         CLC   DUB+1(L'DUB-1),DUB                                               
         BNE   EFNN                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    EFVS                                                             
         CH    R1,=H'255'                                                       
         BH    EFVB                                                             
         STC   R1,WORK                                                          
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE SPORTS/-SPORTS OPTION                                                
VALSPRT  DC    CL60'**option specified by keyword--no values needed**'          
VALSPRTN CLI   DBSRC,C'N'                                                       
         BNE   ENSP                                                             
         ZIC   R0,FLDH+7                                                        
         MVI   WORK,C'Y'                                                        
         EXCLC R0,FLD,=C'SPORTS'                                                
         BER   RE                                                               
         MVI   WORK,C'N'                                                        
         EXCLC R0,FLD,=C'-SPORTS'                                               
         BER   RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
* INTERFACE TO OVERLAY & HANDLE SCREEN FORMATTING & PRINTING                    
*                                                                               
HELPER   L     RB,ABASE                                                         
         L     RA,BBASE                                                         
         L     R7,CBASE                                                         
         L     R6,DBASE                                                         
******   L     R1,=A(ACTHELP)      INITIALIZE FOR HELP OVERLAY                  
         L     R1,AACTHELP         INITIALIZE FOR HELP OVERLAY                  
*****    A     R1,BRELO                                                         
         CLI   HELPREQD,HELPACT                                                 
         BH    *+12                                                             
         ST    R1,AACTNTRY                                                      
         MVI   HELPSAVE,HELP                                                    
         MVC   OVERLAY,ACTOVER-ACTTABDD(R1)                                     
         MVC   ACTN,ACTACTN-ACTTABDD(R1)                                        
         MVC   IACTN,ACTACTN-ACTTABDD(R1)                                       
         L     RE,FADR                                                          
         LA    RF,DEMTWAD                                                       
         SR    RE,RF                                                            
         STCM  RE,3,HELPDISP       SAVE DISPLACEMENT TO FIELD                   
*                                                                               
GO       XC    BINPARMS(BINPARML),BINPARMS                                      
         TWAXC DEMLN1H,PROT=Y,TRNS=T                                            
         XC    DEMMSG,DEMMSG       CLEAR & TRANSMIT MESSAGE LINE                
         OI    DEMMSGH+6,X'80'                                                  
         L     R1,=A(BINTAB-DEMWRKD)                                            
         LA    R1,DEMWRKD(R1)                                                   
         ST    R1,BINATAB                                                       
         CLI   IACTN,NEXT          TEST ACTION=NEXT                             
         BNE   GO4                                                              
         XC    OPTPRNT,OPTPRNT     ENSURE REPORT NOT PRINTED TWICE              
*                                  REBUILD BINTAB FROM SAVED TWA'S              
         MVC   BINSOFAR(L'BINSAVE),BINSAVE                                      
                                                                                
         LA    R2,1                R2=TWA START PAGE NUMBER                     
         SR    R3,R3                                                            
         ICM   R3,3,TERM           R3=TERMINAL NUMBER                           
*                                                                               
         L     R4,BINATAB          R4=I/O ADDRESS                               
         MVC   DMCB+20(2),=C'L='   SET LENGTH OF READ IN 6TH PARM               
         MVC   DMCB+22(2),=Y(CHKPTTWL)                                          
* MAKE SURE WE DON'T GO PAST THE END OF BINATAB                                 
         LR    R0,R4                                                            
         AH    R0,=Y(CHKPTTWL)                                                  
         LH    RF,=Y(BINTABX-BINTAB)  LENGTH OF BINTAB                          
         A     RF,BINATAB                                                       
         CR    R0,RF               CHECK IF SURPASSED SPACE ALLOTTED            
         BNH   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',((R2),(R3)),(R4)            
*                                                                               
         CLI   NACTN,FRST          TEST REDISPLAY FROM START                    
         BNE   *+10                                                             
         XC    BINNEXT,BINNEXT     YES - RESET RECORD NUMBER                    
         OC    BINLKEY,BINLKEY     TEST IF OVERLAY REQUIRED                     
         BZ    GO10                NO - FORMAT THE OUTPUT                       
*                                  GET OVERLAY INTO CORE                        
GO4      GOTO1 VCALLOV,DMCB,(OVERLAY,0),0,0                                     
         CLI   4(R1),X'FF'         TEST OVERLAY LOADED OK                       
         BNE   *+6                                                              
         DC    H'0'                NO - CAN'T DO MUCH WITHOUT IT                
         MVC   AOVERLAY,0(R1)      SAVE A(OVERLAY PHASE)                        
         CLI   IACTN,NEXT                                                       
         BE    GO10                                                             
*                                  FIRST TIME FOR APPLICATION                   
         XC    BINSAVE,BINSAVE     CLEAR DOWN SAVE AREAS & HEADINGS             
         XC    BINTWAS,BINTWAS                                                  
         XC    BINNEXT,BINNEXT                                                  
         XC    APSAVE,APSAVE                                                    
         L     R0,AAPSAV2                                                       
         LHI   R1,APSAV2L                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         TWAXC DEMHD1H,DEMLN1H,PROT=Y                                           
*                                  HANDLE FORMHEAD CALL TO APPLICATION          
         LA    R1,FORMHEAD                                                      
***      BAL   RE,APCALL                                                        
         BAS   RE,APCALL                                                        
         CLI   APMODE,FORMLINE     TEST OVERLAY USERPED ME                      
         BE    GO5                                                              
         LA    R0,78               DEFAULT DATA LENGTH (PREFORMATTED)           
         OC    BINLKEY,BINLKEY     TEST KEY LENGTH = ZERO                       
         BNZ   *+8                                                              
         ST    R0,BINLREC          SET DEFAULT LENGTH OF RECORD                 
         LH    RF,=Y(LENTWA)       CALCULATE MAXIMUM N'BINSRCH ENTRIES          
         SR    RE,RE                                                            
         D     RE,BINLREC          L'BINTAB/L'RECORD=MAX N'ENTRIES              
         ST    RF,BINMAXN                                                       
         MVI   POSTLINE,X'FF'      ADD DUMMY BINSRCH E-O-F RECORD               
         MVC   POSTLINE+1(L'POSTLINE-1),POSTLINE                                
         OC    BINLKEY,BINLKEY                                                  
         BZ    *+10                                                             
         L     RF,APOST                                                         
         BASR  RE,RF                                                            
                                                                                
*                                                                               
         L     R0,AIOAREA1         HANDLE PROCESS CALL TO APPLICATION           
         ST    R0,AIOAREA                                                       
         XC    NDXDA,NDXDA                                                      
         LA    R1,PROCESS                                                       
***      BAL   RE,APCALL                                                        
         BAS   RE,APCALL                                                        
GO5      MVC   BINAREC,BINATAB     SET A(RECORD) TO FIRST IN TABLE              
         OC    BINSOFAR,BINSOFAR                                                
         BNZ   GO6                                                              
         MVC   DEMMSG,=CL60'NO DATA TO DISPLAY - ENTER NEXT REQUEST'            
         MVI   DEMMODE,FRST                                                     
         LA    R1,DEMACTH                                                       
         B     GOX                                                              
*                                                                               
GO6      DS    0H                                                               
         LA    R2,1                R2=TWA START PAGE NUMBER                     
         SR    R3,R3                                                            
         ICM   R3,3,TERM           R3=TERMINAL NUMBER                           
         L     R4,BINATAB          R4=I/O ADDRESS                               
         MVC   DMCB+20(2),=C'L='   SET LENGTH OF WRITE IN 6TH PARM              
         MVC   DMCB+22(2),=Y(CHKPTTWL)                                          
GO8      GOTO1 VDATAMGR,DMCB,=C'DMWRT ',=C'TEMPSTR',((R2),(R3)),(R4)            
                                                                                
*                                                                               
         DS    0H                                                               
         LA    R1,LASTSL                                                        
         MOVE  (LASTS,(R1)),THESE  SAVE THIS TIME INPUT VALUES                  
         MVC   BINSAVE,BINSOFAR    SAVE BINSRCH PARAMETER VALUES                
         XC    BINNEXT,BINNEXT     START DISPLAY AT FIRST RECORD                
         OC    OPTPRNT,OPTPRNT                                                  
         BZ    GO10                                                             
         GOTO1 APRINT,0            INITIALIZE PRINTED REPORT                    
*                                                                               
GO10     ICM   R1,15,BINNEXT       CALCULATE A(NEXT RECORD)                     
         M     R0,BINLREC                                                       
         A     R1,BINATAB                                                       
         ST    R1,BINAREC          SET A(NEXT RECORD TO BE FORMATTED)           
         MVI   MAXLINE,(DEMLAST-DEMLN1H)/(L'DEMLN1+L'DEMLN1H)                   
         LA    R1,DEMLN1H                                                       
         ST    R1,ALINE                                                         
         MVI   NLINE,0                                                          
         OC    BINLKEY,BINLKEY                                                  
         BNZ   GO16                                                             
*                                                                               
GO12     SR    R0,R0               HANDLE PREFORMATTED LINE DISPLAY             
         ICM   R0,1,MAXLINE        R1=MAXIMUM N'DISPLAY LINES                   
         L     R1,BINSOFAR                                                      
         ICM   RE,15,BINNEXT                                                    
         SR    R1,RE               R0=REMAINING LINES TO PRINT                  
         CR    R1,R0                                                            
         BH    *+6                                                              
         LR    R0,R1                                                            
         OC    OPTPRNT,OPTPRNT     TEST IF PRINTING                             
         BZ    GO13                                                             
         ICM   R0,15,BINSOFAR      YES - SET LOOP TO MAXIMUM RECORDS            
         BNZ   GO13                                                             
         DC    H'0'                                                             
GO13     L     R2,ALINE            R2=A(FIRST OUTPUT TWA LINE)                  
         L     R3,BINAREC          R3=A(FIRST RECORD)                           
         L     R4,BINLREC                                                       
         BCTR  R4,0                R4=RECORD LENGTH-1                           
         MVC   LINE2,SPACES                                                     
         MVC   LINE3,SPACES                                                     
*                                                                               
GO14     OC    OPTPRNT,OPTPRNT     TEST IF PRINTING THE REPORT                  
         BZ    GO14A                                                            
         MVC   LINE1,SPACES                                                     
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   LINE1(0),0(R3)                                                   
         GOTO1 APRINT,1            PRINT THIS LINE                              
GO14A    ZIC   RE,NLINE            BUMP LINE NUMBER                             
         LA    RE,1(RE)                                                         
         STC   RE,NLINE                                                         
         CLC   NLINE,MAXLINE       TEST IF SCREEN IS FULL                       
         BH    GO14B                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R3)       MOVE LINE TO TWA                             
         LA    R2,L'DEMLN1+L'DEMLN1H(R2)                                        
GO14B    LA    R3,1(R4,R3)                                                      
         ICM   RE,15,BINNEXT       BUMP RECORD NUMBER                           
         LA    RE,1(RE)                                                         
         STCM  RE,15,BINNEXT                                                    
         BCT   R0,GO14             DO UNTIL TWA OR TABLE RUN OUT                
         B     GO20                                                             
*                                                                               
GO16     CLC   NLINE,MAXLINE       HANDLE FORMLINE CALLS TO APPLICATION         
         BNL   *+10                                                             
         MVC   BINLAST,BINNEXT     SAVE BINNEXT VALUE                           
         MVC   LINE1,SPACES        CLEAR FORMAT LINES TO SPACES                 
         MVC   LINE2,SPACES                                                     
         MVC   LINE3,SPACES                                                     
         LA    R1,FORMLINE                                                      
**GO18     BAL   RE,APCALL                                                      
GO18     BAS   RE,APCALL                                                        
         CLI   APMODE,SAMEREC      TEST USER WANTS RECORD AGAIN                 
         BE    GO182               YES - DON'T BUMP TO NEXT RECORD              
         L     RE,BINAREC          BUMP TO NEXT RECORD                          
         A     RE,BINLREC                                                       
         ST    RE,BINAREC                                                       
         ICM   RE,15,BINNEXT       BUMP RECORD NUMBER                           
         LA    RE,1(RE)                                                         
         STCM  RE,15,BINNEXT                                                    
         CLI   APMODE,NEXTREC      TEST APPLICATION WANTS ANOTHER               
         BNE   *+12                                                             
         LA    R1,PROCREC          YES - PASS MODE OF PROCREC                   
         B     GO18                                                             
GO182    OC    OPTPRNT,OPTPRNT     TEST IF PRINTING THE REPORT                  
         BZ    GO18A                                                            
         GOTO1 APRINT,1                                                         
GO18A    CLC   NLINE,MAXLINE       TEST SCREEN HAS OVERFLOWED                   
         BNL   GO18B                                                            
         ZIC   RE,NLINE                                                         
         CLC   LINE1,SPACES                                                     
         BE    *+8                                                              
         LA    RE,1(RE)            BUMP NUMBER OF OUTPUT LINES                  
         CLC   LINE2,SPACES                                                     
         BE    *+8                                                              
         LA    RE,1(RE)            TWICE IF TWO OUTPUT LINES                    
         CLC   LINE3,SPACES                                                     
         BE    *+8                                                              
         LA    RE,1(RE)            THRICE IF THREE OUTPUT LINES                 
         STC   RE,NLINE                                                         
GO18B    CLC   NLINE,MAXLINE       TEST SCREEN WILL OVERFLOW                    
         BL    GO18C                                                            
         OC    OPTPRNT,OPTPRNT                                                  
         BZ    *+14                                                             
         CLC   BINNEXT,BINSOFAR                                                 
         BL    GO16                                                             
         MVC   BINNEXT,BINLAST     RESTORE LAST RECORD NUMBER                   
         B     GO20                                                             
GO18C    L     RE,ALINE            MOVE OUTPUT LINES TO TWA                     
         CLC   LINE1,SPACES                                                     
         BE    *+14                                                             
         MVC   L'DEMLN1H(L'DEMLN1,RE),LINE1                                     
         LA    RE,L'DEMLN1H+L'DEMLN1(RE)                                        
         CLC   LINE2,SPACES                                                     
         BE    *+14                                                             
         MVC   L'DEMLN1H(L'DEMLN1,RE),LINE2                                     
         LA    RE,L'DEMLN1H+L'DEMLN1(RE)                                        
         CLC   LINE3,SPACES                                                     
         BE    *+14                                                             
         MVC   L'DEMLN1H(L'DEMLN1,RE),LINE3                                     
         LA    RE,L'DEMLN1H+L'DEMLN1(RE)                                        
         ST    RE,ALINE                                                         
         CLC   BINNEXT,BINSOFAR    TEST ALL RECORDS FORMATTED                   
         BL    GO16                                                             
         B     GO20                                                             
*                                                                               
GO20     OC    OPTPRNT,OPTPRNT     CLOSE THE REPORT IF NECESSARY                
         BZ    GO22                                                             
         GOTO1 APRINT,2                                                         
*                                  DISPLAY PRINT QUEUE REPORT ID                
         MVC   XTRA(L'REPMSG),REPMSG                                            
         MVC   XTRA+10(L'OPTPRNT),OPTPRNT                                       
         SR    R0,R0                                                            
         ICM   R0,3,REPNO                                                       
         EDIT  (R0),(4,XTRA+14),ALIGN=LEFT                                      
*                                                                               
GO22     MVI   DEMMODE,NEXT        OUTPUT MESSAGE & SET CURSOR FOR NEXT         
         MVC   DEMMSG,=CL60'DATA DISPLAYED - PRESS ENTER FOR NEXT'              
         CLI   ACTN,HELP                                                        
         BNE   *+10                                                             
         MVC   DEMMSG(4),=C'HELP'                                               
         XC    DEMACT,DEMACT                                                    
*****    L     R1,=A(ACTNEXT)                                                   
         L     R1,AACTNEXT                                                      
****     A     R1,BRELO                                                         
         MVC   DEMACT,ACTNAME-ACTTABDD(R1)                                      
         OI    DEMACTH+6,X'80'                                                  
         LA    R1,DEMTABH                                                       
         CLC   BINNEXT,BINSOFAR    TEST ALL DATA LINES DISPLAYED                
         BL    GOX                                                              
         MVC   DEMMSG,=CL60'NO MORE TO DISPLAY - ENTER NEXT REQUEST'            
         CLI   ACTN,HELP           TEST HELP JUST COMPLETED                     
         BNE   GO26                                                             
         L     R1,AACTTAB          YES - RESTORE ORIGINAL ACTION                
GO24     CLI   ACTNAME-ACTTABDD(R1),EOT                                         
         BNE   *+6                                                              
         DC    H'0'                THAT WON'T DO                                
         CLC   HELPSAVE,ACTACTN-ACTTABDD(R1)                                    
         BE    *+12                                                             
         LA    R1,ACTTABLL(R1)                                                  
         B     GO24                                                             
         ST    R1,AACTNTRY                                                      
GO26     L     R1,AACTNTRY         RE-DISPLAY ORIGINAL ACTION                   
         MVC   DEMACT,ACTNAME-ACTTABDD(R1)                                      
         XC    BINNEXT,BINNEXT     CAN RE-DISPLAY FROM START NEXT TIME          
         LA    R1,DEMACTH                                                       
         CLI   ACTN,HELP           TEST HELP                                    
         BNE   GOX                                                              
         SR    R1,R1                                                            
         ICM   R1,3,HELPDISP       POINT TO FIELD HEADER OF HELP FIELD          
         LA    R1,DEMTWAD(R1)                                                   
         B     GOX                                                              
*                                                                               
GOX      ST    R1,FADR             SET CURSOR FIELD ADDRESS                     
         LA    R1,DEMLN1H          XMIT ALL NON-ZERO DATA LINES                 
         LA    RF,2304(R1)                                                      
         SR    RE,RE                                                            
GOX2     IC    RE,0(R1)                                                         
         SH    RE,=H'9'            RE=L'DATA-1                                  
         BM    GOXX                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    8(0,R1),8(R1)       TEST FIELD NON-ZERO                          
         BZ    *+8                                                              
         OI    6(R1),X'80'         YES - SET XMIT BIT                           
         AH    RE,=H'9'                                                         
         BXLE  R1,RE,GOX2                                                       
GOXX     B     ERROR4              TACK ON EXTRA MESSAGE IF REQUIRED            
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
*===================== STEREO ROUTINES INTERFACE =====================*         
                                                                                
GOSTEO   NTR1  BASE=ABASE,LABEL=N                                               
         L     RA,BBASE                                                         
         L     R7,CBASE                                                         
         L     R6,DBASE                                                         
                                                                                
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(3,RE),=C'GOS'                                                  
         MVC   3(1,RE),GOSTEON                                                  
         SR    RE,RE               CLEAR RE JUST TO BE SAFE                     
                                                                                
         DS    0H                                                               
         CLI   GOSTEON,STE#                                                     
         BH    GOSTEO2                                                          
         GOTO1 ASTEREO,DMCB,(R9),(GOSTEON,0)                                    
         B     GOSTEOX                                                          
GOSTEO2  DS    0H                                                               
         GOTO1 ASTEREO2,DMCB,(R9),(GOSTEON,0)                                   
         DS    0H                                                               
                                                                                
GOSTEOX  XIT1                                                                   
*&&                                                                             
         EJECT                                                                  
* CALL APPLICATION OVERLAY PASSING A(GLOBAL W/S) IN R1                          
*                                                                               
APCALL   LR    R0,RE                                                            
         STC   R1,APMODE           SET APPLICATION MODE                         
         GOTO1 AOVERLAY,DEMWRKD                                                 
         CLI   APMODE,FORCEEND     SET CONDITION CODE                           
         BE    ERROR                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
* ROUTINE TO GET A TSAR DEMO RECORD CORRESPONDING TO TSARKEY                    
*                                                                               
         PRINT GEN                                                              
         DS    0H                                                               
GETTDR   NTR1  BASE=ABASE,LABEL=N                                               
         PRINT NOGEN                                                            
         L     RA,BBASE                                                         
         L     R7,CBASE                                                         
         L     R6,DBASE                                                         
                                                                                
         MVI   GOSTEON,SGTDR#                                                   
         GOTO1 AGOSTEO                                                          
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
         PRINT OFF                                                              
*&&DO                                                                           
***********************************************************************         
* ROUTINE TO EXECUTE I/O TO DEMO FILES.                               *         
*                                                                     *         
* NTRY - IOFLAG  CONTAINS COMMAND NUMBER, FILE/DIRECTORY NUMBER       *         
*                AND FILE TYPE (DIRECTORY/FILE).                      *         
*        KEY     CONTAINS KEY FOR READ                                *         
*        NDXDA   CONTAINS D/A (MAY BE ZERO FOR FILE I/O)              *         
*        AIOAREA CONTAINS ADDRESS OF OUTPUT RECORD                    *         
*                                                                     *         
* ALL COMMANDS (SEE CMNDTAB) ARE AVAILABLE FOR BOTH FILE AND          *         
* DIRECTORY. FOR FILE COMMANDS A DIRECTORY READ IS EXECUTED IF NDXDA  *         
* IS BINARY ZEROES. AS DMREAD FOR A DANDX FILE IS NOT SUPPORTED THIS  *         
* IS EMULATED. FOR DIRECTORY COMMANDS KEY IS ALWAYS SAVED IN KEYSAVE  *         
* BEFORE I/O IS EXECUTED, KEY CONTAINS ACTUAL RECORD AFTER I/O.       *         
*                                                                     *         
* EXIT - IOFLAG CONTAINS ERROR CONDITION (ZERO MEANS OK)              *         
*        USER'S IO AREA CONTAINS RECORD AFTER I/O                     *         
*        CC=EQUAL IF I/O SUCCESSFUL                                   *         
*        CC=LOW IF DISK ERROR                                         *         
*        CC=HIGH FOR END-OF-FILE & NOT FOUND                          *         
***********************************************************************         
         SPACE 1                                                                
IO       NTR1  BASE=ABASE,WORK=(RC,IOWORKX-IOWORKD)                             
         L     RA,BBASE                                                         
         L     R7,CBASE                                                         
         L     R6,DBASE                                                         
         USING IOWORKD,RC          RC=A(LOCAL W/S)                              
         MVC   IOWORK1,IOFLAG      SAVE COMMAND FLAG                            
         MVC   IOWORK2,IOWORK1                                                  
         PACK  IOWORK3,IOWORK1     INVERT COMMAND FLAG                          
         NI    IOWORK2,X'07'       IOWORK2=FIL/DIR NUMBER                       
         ZIC   RE,IOWORK2                                                       
         LA    RF,L'FDTAB                                                       
         MR    RE,RE                                                            
         LA    RE,FDTAB-L'FDTAB(RF)                                             
IO2      MVC   IOFILE,0(RE)        SET FILE/DIR NAMES                           
         MVC   IODIR,7(RE)                                                      
         MVC   IOINDS,14(RE)       SET FILE/DIRECTORY INDICATORS                
         NI    IOWORK3,X'03'       IOWORK3=COMMAND NUMBER                       
         ZIC   RE,IOWORK3                                                       
         SLL   RE,3                                                             
         LA    RE,CMNDTAB-8(RE)                                                 
         MVC   IOCMND,0(RE)        SET COMMAND                                  
         TM    IOWORK1,DIR                                                      
         BZ    IO6                                                              
*                                  DIRECTORY I/O CALL                           
IO4      MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTO1 VDATAMGR,IOPARM,IOCMND,IODIR,KEY,AIOAREA                         
         L     RF,AIOAREA          EXTRACT D/A FROM RECORD                      
         SR    R0,R0                                                            
         ICM   R0,1,IODADSP        UNLESS DISPLACEMENT IS ZERO                  
         BZ    *+12                                                             
         AR    RF,R0               RF=A(D/A)                                    
         MVC   NDXDA,0(RF)                                                      
         MVC   IOFLAG,8(R1)        RETURN DATAMGR ERROR                         
         B     IOX                                                              
*                                  FILE CALL                                    
IO6      OC    NDXDA,NDXDA         TEST IF D/A PRESENT                          
         BNZ   IO8                                                              
         MVC   IOFLAG,IOWORK2                                                   
         OI    IOFLAG,DIR+READ     NO - DO DIRECTORY READ                       
***      BAL   RE,IO                                                            
         BAS   RE,IO                                                            
         BNE   IOX                                                              
         OC    NDXDA,NDXDA         TEST IF D/A PRESENT                          
         BNZ   IO8                                                              
         MVI   IOFLAG,NOTFOUND     NO - RETURN NOT FOUND                        
         B     IOX                                                              
*                                  NON-DANDX FILE FUDGES                        
IO8      CLI   IOFILTY,0                                                        
         BE    IO10                                                             
         CLI   IOFILTY,2           TEST DIRECTORY ONLY (NO FILE)                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   IOCMND,CMNDREAD     TEST DMREAD/FILE                             
         BNE   *+10                                                             
         MVC   IOCMND,CMNDGETR     YES - SET TO GETREC/FILE                     
         B     IO12                                                             
*                                  DANDX FILE FUDGES                            
IO10     L     RE,AIOAREA          BUILD KEY IN USER I/O AREA                   
         ZIC   RF,IOKEYLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),KEY         SET READ HIGH KEY IN IOAREA                  
         CLC   IOCMND,CMNDREAD     TEST IF READ COMMAND                         
         BNE   *+10                                                             
         MVC   IOCMND,CMNDRDHI     YES - SET COMMAND TO READ HIGH               
*                                  FILE I/O CALL                                
IO12     GOTO1 VDATAMGR,IOPARM,IOCMND,IOFILE,NDXDA,AIOAREA,IOWORK               
         MVC   IOFLAG,8(R1)        RETURN DATAMGR ERROR                         
         CLI   IOFILTY,0           TEST IF A DANDX FILE                         
         BNE   IOX                 NO - EXIT                                    
         CLC   IOCMND,CMNDREAD     TEST IF READ COMMAND                         
         BNE   IOX                 NO - EXIT                                    
         MVI   IOFLAG,NOTFOUND                                                  
         CLI   8(R1),0                                                          
         BNE   IOX                                                              
         L     RE,AIOAREA          TEST RECORD FOUND                            
         ZIC   RF,IOKEYLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),KEY                                                      
         BNE   IOX                 NO - EXIT WITH NOT FOUND                     
         MVI   IOFLAG,0            RESET ERROR                                  
*                                  SET CC & RETURN                              
IOX      MVI   IOWORK1,1                                                        
         CLI   IOFLAG,0                                                         
         BE    IOXX                                                             
         MVI   IOWORK1,2                                                        
         TM    IOFLAG,EOF+NOTFOUND                                              
         BNZ   IOXX                                                             
         MVI   IOWORK1,0                                                        
IOXX     CLI   IOWORK1,1                                                        
         B     EXIT                                                             
         DROP  RC                                                               
         EJECT                                                                  
*&&                                                                             
         PRINT ON                                                               
* INPUT FIELD ERRORS SET HERE                                                   
*                                                                               
EIIO     MVI   FERN,0              I/O ERROR                                    
         B     ERROR                                                            
EMIF     MVI   FERN,1              MISSING INPUT FIELD                          
         B     ERROR0                                                           
EIIF     MVI   FERN,2              INVALID INPUT FIELD                          
         B     ERROR                                                            
EFNN     MVI   FERN,3              FIELD NOT NUMERIC                            
         B     ERROR                                                            
EFTS     MVI   FERN,4              FIELD LENGTH TOO SHORT                       
         B     ERROR                                                            
EFTL     MVI   FERN,5              FIELD LENGTH TOO LONG                        
         B     ERROR                                                            
EFVS     MVI   FERN,6              FIELD VALUE LESS THAN MINIMUM                
         B     ERROR                                                            
EFVB     MVI   FERN,7              FIELD VALUE EXCEEDS MAXIMUM                  
         B     ERROR                                                            
ETMI     MVI   FERN,8              TOO MANY INPUT FIELDS                        
         B     ERROR0                                                           
EROM     MVI   FERN,9              REQUIRED OPTION IS MISSING                   
         B     ERROR0                                                           
EDIF     MVI   FERN,10             DUPLICATED INPUT FIELD                       
         B     ERROR                                                            
EIKW     MVI   FERN,11             INVALID KEYWORD                              
         B     ERROR                                                            
EKWI     MVI   FERN,12             KEYWORD INCOMPATIBLE WITH ANOTHER            
         B     ERROR                                                            
EDTS     MVI   FERN,13             SUBFIELD DATA LENGTH TOO SHORT               
         B     ERROR                                                            
EDTL     MVI   FERN,14             SUBFIELD DATA LENGTH TOO LONG                
         B     ERROR                                                            
EIDV     MVI   FERN,15             INVALID SUBFIELD DATA VALUE                  
         B     ERROR                                                            
EIIFPU   MVI   FERN,16             INVALID PURE NUMBER EXPRESSION               
         B     ERROR                                                            
EIROT    MVI   FERN,17             INVALID ROTATION                             
         B     ERROR                                                            
ETMC     MVI   FERN,18             TOO MANY COMPONENTS IN ROTATION              
         B     ERROR                                                            
ENYRO    MVI   FERN,19             ROTATIONS NOT APPLICABLE YET                 
         B     ERROR                                                            
EIDEM    MVI   FERN,20             INVALID DEMO                                 
         B     ERROR                                                            
EISTA    MVI   FERN,21             INVALID STATION                              
         L     RF,ADEMACT                                                       
         CLC   =C'PROGMKT',8(RF)                                                
         BE    EIPGN                                                            
         CLC   =C'STATION',8(RF)                                                
         BE    EIMKT                                                            
         CLC   =C'SPILL',8(RF)                                                  
         BE    ERROR                                                            
         CLC   =C'PROGNAME',8(RF)                                               
         BE    ERROR                                                            
         B     EIIF                MAINFRAME REGULAR ERROR                      
EIMKT    MVI   FERN,22             INVALID MARKET                               
         B     ERROR                                                            
EIPGN    MVI   FERN,23             INVALID PROGRAM NUMBER                       
         B     ERROR                                                            
EMIFDT   MVI   FERN,24             MISSING DAY/TIME EXPRESSION                  
         B     ERROR                                                            
EIIFDT   MVI   FERN,25             INVALID DAY/TIME EXPRESSION                  
         B     ERROR                                                            
EMIFDM   MVI   FERN,26             MISSING DEMOS FIELD                          
         B     ERROR                                                            
EMIFBK   MVI   FERN,27             MISSING BOOKS FIELD                          
         B     ERROR                                                            
EIIFBK   MVI   FERN,28             INVALID BOOKS FIELD                          
         B     ERROR                                                            
EMIFSRC  MVI   FERN,29             MISSING SOURCE FIELD                         
         B     ERROR                                                            
EMSTA    MVI   FERN,30             MISSING STATION                              
         L     RF,ADEMACT                                                       
         CLC   =C'PROGMKT',8(RF)                                                
         BE    EMPGN                                                            
         CLC   =C'STATION',8(RF)                                                
         BE    EMMKT                                                            
         CLC   =C'SPILL',8(RF)                                                  
         BE    ERROR                                                            
         B     EMIF                MAINFRAME REGULAR ERROR                      
EMPGN    MVI   FERN,31             MISSING PROGRAM NUMBER                       
         B     ERROR                                                            
EMMKT    MVI   FERN,32             MISSING MARKET                               
         B     ERROR                                                            
EPUIF    MVI   FERN,33             PURE NUMBER INVALID FOR FILE                 
         B     ERROR                                                            
ETMBK    MVI   FERN,34             TOO MANY BOOKS FOR MBK                       
         B     ERROR                                                            
ENYUPTRO MVI   FERN,39             UPGD ON TP ROTATION NOT APPLCBL YET          
         B     ERROR                                                            
ENOTSUPP MVI   FERN,40             FEATURE NOT SUPPORTED IN THIS DEM32          
         B     ERROR                                                            
ETCN4    MVI   FERN,41             M/D/Y FMT FOR BBM CAN TP W/ THIS ACT         
         B     ERROR0                                                           
EACTBBM  MVI   FERN,42             USE  PERIOD  FOR THESE BBM MNTHLY BK         
         B     ERROR0                                                           
EIAC     MVI   FERN,70             INVALID ACTION                               
         B     ERROR                                                            
EIAS     MVI   FERN,71             INVALID ACTION SEQUENCE                      
         B     ERROR                                                            
EKHC     MVI   FERN,72             KEY CHANGED ON ACTION=NEXT                   
         B     ERROR0                                                           
*^^GYL   B     ERROR                                                            
EIFN     MVI   FERN,80             INVALID FILE                                 
         B     ERROR                                                            
ESFC     MVI   FERN,81             SOURCE INVALID FOR FILE                      
         B     ERROR                                                            
EISC     MVI   FERN,82             INVALID SOURCE CODE                          
         B     ERROR                                                            
ELBI     MVI   FERN,90             'LATEST' BOOK IS INVALID (FOR FILE)          
         B     ERROR                                                            
EIBK     MVI   FERN,91             INVALID BOOK                                 
         B     ERROR                                                            
EISB     MVI   FERN,92             STATION/BOOK NOT FOUND R2=A(DBLOCK)          
         B     ERROR                                                            
EIDE     MVI   FERN,100            INVALID DAY EXPRESSION                       
         B     ERROR                                                            
EITE     MVI   FERN,101            INVALID TIME EXPRESSION                      
         B     ERROR                                                            
EIUE     MVI   FERN,110            INVALID UPGRADE EXPRESSION                   
         B     ERROR                                                            
EICL     MVI   FERN,121            INVALID CLIENT                               
         B     ERROR                                                            
EIMD     MVI   FERN,122            INVALID MEDIA                                
         B     ERROR                                                            
EIES     MVI   FERN,123            INVALID ESTIMATE                             
         B     ERROR                                                            
ENOF     MVI   FERN,124            ESTIMATE NOT ON FILE                         
         B     ERROR0                                                           
EIEH     MVI   FERN,125            INVALID ESTIMATE HEADER FORMAT               
         B     ERROR0                                                           
EMNF     MVI   FERN,126            MENU RECORD NOT ON FILE                      
         B     ERROR0                                                           
EIMR     MVI   FERN,127            INVALID MENU RECORD FORMAT                   
         B     ERROR0                                                           
EIDM     MVI   FERN,128            INVALID DEMO (S/B RTG/IMP)                   
         B     ERROR                                                            
EDNF     MVI   FERN,129            EST. BOOK DEFN. RECORD NOT FOUND             
         B     ERROR                                                            
ESNF     MVI   FERN,130            STATION NOT ON FILE                          
         B     ERROR                                                            
EIBP     MVI   FERN,131            INVALID BUYING PERIOD NUMBER                 
         B     ERROR                                                            
EMIP     MVI   FERN,132            INVALID OR MISSING BUYING PERIOD             
         B     ERROR                                                            
ECNF     MVI   FERN,133            CLIENT NOT ON FILE                           
         B     ERROR                                                            
EPNF     MVI   FERN,134            PRODUCT NOT ON FILE                          
         B     ERROR                                                            
EBNF     MVI   FERN,135            BUY LINE NOT ON FILE                         
         B     ERROR                                                            
ENAB     MVI   FERN,140            NO ARB DATA AFTER NOV/93                     
         B     ERROR0                                                           
ENSP     MVI   FERN,141            SPRT/-SPRT OPTION FOR NSI ONLY               
         B     ERROR0                                                           
ETCN1    MVI   FERN,142            M/Y FMT FOR NSI CAN TP BFORE OCT/95          
         B     ERROR0                                                           
ETCN2    MVI   FERN,143            M/D/Y FMT FOR NSI CAN TP >= JAN/96           
         B     ERROR0                                                           
EBKMM    MVI   FERN,144            BK (MISMATCH) INCONSISTENT W/ 1ST BK         
         B     ERROR                                                            
EACTCSI  MVI   FERN,145            USE  PERIOD  FOR >= JAN96 CSI MTH BK         
         B     ERROR0                                                           
ESTE     MVI   FERN,150            STEREO ERROR                                 
         B     ERROR0                                                           
ESTL     MVI   FERN,35             SCREEN TOO LONG                              
         B     ERROR0                                                           
ENPMD    MVI   FERN,37             NO PROGRM MARKET DATA PRIOR TO MAR94         
         B     ERROR0                                                           
ENWHHD   MVI   FERN,38             NO WEEKLY HH DATA PRIOR TO MAR98             
         B     ERROR0                                                           
EKIMSI   MVI   FERN,136            IN MARKET SHARES NOT SUPPORTED               
         B     ERROR0                                                           
EMRAD    MVI   FERN,137            invalid modifier for radar                   
         B     ERROR0                                                           
EIMSINV  MVI   FERN,154    IMS NOT AVAILABLE FOR 1/4 HR DETAILS.                
         B     ERROR0                                                           
ESAVGBK  MVI   FERN,45     EXCEEDED MAX NUMBER OF BOOKS FOR UPGRADE             
         B     ERROR0                                                           
ERANGE1  MVI   FERN,149    END MULTIBOOK RANGE MUST HAVE START                  
         B     ERROR0                                                           
ERANGE2  MVI   FERN,156    END RANGE MUST BE GREATER THAN START                 
         B     ERROR0                                                           
EWKBREAK MVI   FERN,157    WEEKLY BREAKOUT NOT VALID FOR WTP                    
         B     ERROR0                                                           
ESTATCDE MVI   FERN,158    INVALIDE STATE CODE                                  
         B     ERROR0                                                           
ECUNROT  MVI   FERN,162    COUNTY COVERAGE CANT PROCESS ROTATIONS               
         B     ERROR0                                                           
ECUNPER  MVI   FERN,163    COUNTY COVERAGE CANT PROCESS ROTATIONS               
         B     ERROR0                                                           
ELAT24   MVI   FERN,165    LAT2-LAT4 NOT AVAILABLE FOR CANADA                   
         B     ERROR0                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT ERROR MESSAGE AND FIELD INDEX INFO                *         
*                                                                     *         
* NTRY - FADR=A(FIELD HEADER OF INPUT FIELD IN ERROR)                 *         
*        FERN=FIELD ERROR NUMBER                                      *         
*        FNDX=MULTIPLE FIELD INDEX                                    *         
*        XTRA=OPTIONAL USER SUPPLIED HELP                             *         
* NTR AT ERROR0 TO SET FIELD INDEX TO ZERO ON ERRORS                  *         
*        ERROR2 TO SET FNDX & CONCATENATE XTRA TO ROOT MESSAGES       *         
*        ERROR4 TO CONTATENATE XTRA TO ROOT MESSAGES                  *         
*        ERROR6 TO SET CURSOR ONLY                                    *         
***********************************************************************         
         SPACE 1                                                                
ERROR0   DS    0H                                                               
         L     RB,ABASE                                                         
         L     RA,BBASE                                                         
         L     R7,CBASE                                                         
         L     R6,DBASE                                                         
         MVI   GOSTEON,SERR0#                                                   
         B     ERRORGO                                                          
                                                                                
ERROR    DS    0H                                                               
         MVI   GOSTEON,SERR1#                                                   
         B     ERRORGO                                                          
                                                                                
ERROR2   DS    0H                                                               
         MVI   GOSTEON,SERR2#                                                   
         B     ERRORGO                                                          
                                                                                
ERROR4   DS    0H                                                               
         MVI   GOSTEON,SERR4#                                                   
         B     ERRORGO                                                          
                                                                                
ERROR6   DS    0H                                                               
         MVI   GOSTEON,SERR6#                                                   
         B     ERRORGO                                                          
*                                                                               
ERRORGO  DS    0H                                                               
         OC    ASTEREO,ASTEREO                                                  
         BNZ   *+16                                                             
         L     R0,=A(STEREO)                                                    
         A     R0,BRELO                                                         
         ST    R0,ASTEREO                                                       
*                                                                               
         DS    0H                                                               
         GOTO1 AERRITFC                                                         
         B     ERRORX                                                           
*                                                                               
ERRORX   DS    0H                                                               
         B     EXIT                                                             
         PRINT OFF                                                              
*&&DO                                                                           
ERROR0   MVI   FNDX,0                                                           
ERROR    OI    DEMMSGH+6,X'80'     TRANSMIT MESSAGE                             
         CLI   FERN,SUPPLIED       TEST USER SUPPLIED MESSAGE                   
         BE    ERROR6                                                           
         GOTO1 VGETMSG,DMCB2,(FERN,DEMMSG),(X'FF',DMCB),(15,0)                  
ERROR2   CLI   FNDX,0              TEST MULTIPLE FIELD INDEX SET                
         BE    ERROR4                                                           
         LA    R1,DEMMSG+L'DEMMSG-1                                             
         LR    RE,R1                                                            
         CLI   0(R1),C' '          YES - CALCULATE L'OUTPUT MESSAGE             
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         SR    RE,R1                                                            
         CH    RE,=H'7'            AND TEST IF FIELD INDEX WILL FIT             
         BL    ERROR4                                                           
         MVC   1(6,R1),=C'-FLD#NN' YES - TELL THE USER WHICH FIELD              
         ZIC   RE,FNDX                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  6(2,R1),DUB                                                      
ERROR4   CLC   XTRA,SPACES         TEST IF EXTRA MESSAGE NEEDED                 
         BE    ERROR6                                                           
         LA    R1,DEMMSG+L'DEMMSG-1                                             
         LR    R0,R1                                                            
         CLI   0(R1),C' '          YES - CALCULATE L'OUTPUT MESSAGE             
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         SR    R0,R1               R0=AVAILABLE ROOM                            
         LA    RF,XTRA+L'XTRA-1                                                 
         LA    RE,XTRA-2                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,RE               RF=L'EXTRA MESSAGE+1                         
         CR    RF,R0               TEST ENOUGH ROOM FOR EXTRA MESSAGE           
         BH    ERROR6                                                           
         MVI   1(R1),C'-'          YES - TACK IT ON                             
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R1),XTRA                                                     
ERROR6   L     R1,FADR             INSERT CURSOR TO FIELD IN ERROR              
         OI    6(R1),X'40'                                                      
         OI    MISCFLG1,MF1ERROR   FLAG FOR ERROR                               
         B     EXIT                                                             
*&&                                                                             
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO EXTRACT & PRE-VALIDATE AN INPUT FIELD                    *         
*                                                                     *         
* NTRY - R1=A(TWA FIELD HEADER)                                       *         
* EXIT - FERN=FIELD ERROR NUMBER                                      *         
*        FADR=A(INPUT FIELD HEADER)                                   *         
*        FLDH=TWA FIELD HEADER                                        *         
*        FLD =EXTRACTED & SPACE FILLED INPUT FIELD                    *         
*        CC  =EQ IF NO INPUT                                          *         
*                                                                     *         
* NOTE - HELP INTERFACE WILL BE ENTERED DIRECTLY FROM FVAL IF 'HELP'  *         
*        OR QUESTION MARK ARE INPUT IN FIELDS THAT HAVE HELP SUPPORT  *         
***********************************************************************         
         SPACE 1                                                                
*VAL     MVI   FERN,1                                                           
FVAL     MVI   FNDX,0              RESET MULTIPLE FIELD INDEX                   
         MVC   BYTE,FERN                                                        
         MVI   FNDX2,0             RESET MULTIPLE SUB-FIELD INDEX               
         ST    R1,FADR             SET A(TWA FIELD HEADER)                      
         MVC   FLDH,0(R1)          EXTRACT HEADER & DATA                        
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD                                               
         ZIC   RF,FLDH                                                          
         LA    R0,L'FLDH+1                                                      
         SR    RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R1)                                                     
         LA    R1,FLD(RF)          CALCULATE L'INPUT DATA                       
         LA    RF,1(RF)                                                         
FVAL2    CLI   0(R1),C' '          FIND LAST INPUT CHARACTER                    
         BNL   FVAL4                                                            
         MVI   0(R1),C' '                                                       
         BCTR  R1,0                                                             
         BCT   RF,FVAL2                                                         
         B     FVALX                                                            
FVAL4    STC   RF,FLDH+5           SET INPUT DATA LENGTH                        
         BCTR  RF,0                                                             
         STC   RF,FLDH+7           SET L'DATA-1 AT FLDH+7                       
         MVI   FERN,OK                                                          
*                                  TEST IF HELP REQUIRED                        
         CLI   FLD,X'6F'           IF FIELD STARTS WITH QUESTION MARK           
         BE    FVAL6                                                            
*&&DO                                                                           
         CLI   FLDH+5,2            OR 'HE(LP)' IS INPUT                         
         BL    FVALX                                                            
*&&                                                                             
         CLI   FLDH+5,4                                                         
*&&DO                                                                           
         BH    FVALX                                                            
*&&                                                                             
         BNE   FVALX               SPELL OUT H-E-L-P TO ACTIVATE HELP           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),=C'HELP'                                                  
         BNE   FVALX                                                            
FVAL6    CLI   HELPREQD,0          AND HELP SUPPORTED FOR THIS FIELD            
         BNE   HELPER              GO AND GIVE SOME HELP                        
*VALX    CLI   FERN,1              SET CONDITION CODE                           
FVALX    CLC   FERN,BYTE           SET CONDITION CODE                           
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUMP TO NEXT SCANNER BLOCK ENTRY                         *         
*                                                                     *         
* NTRY - SCANLNTH=L'RHS OF SCANNER BLOCK ENTRY                        *         
*        R2=A(BLOCK ENTRY), FNDX=CURRENT FIELD INDEX NUMBER           *         
*                                                                     *         
* EXIT - CC=HIGH IF ALL ENTRIES HAVE BEEN PROCESSED                   *         
***********************************************************************         
         SPACE 1                                                                
BUMPSCAN ZIC   R1,FNDX             INCREMENT FIELD INDEX BY ONE                 
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
                                                                                
         CLI   FNDX,1                                                           
         BE    *+18                                                             
         IC    R1,SCAN1HL                                                       
         AR    R2,R1               BUMP FOR 1ST HALF LENGTH                     
         IC    R1,SCANLNTH                                                      
         LA    R2,12(R1,R2)        BUMP FOR 2ND HALF LENGTH & FIXED             
                                                                                
         CLC   FNDX,NFLDS          COMPARE FIELD INDEX TO N'FIELDS              
         BR    RE                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* ROUTINE TO BUMP TO NEXT SCANNER BLOCK ENTRY #2                      *         
*                                                                     *         
* NTRY - SCNLNTH2=L'RHS OF SCANNER BLOCK ENTRY                        *         
*        R3=A(BLOCK ENTRY), FNDX=CURRENT FIELD INDEX NUMBER           *         
*                                                                     *         
* EXIT - CC=HIGH IF ALL ENTRIES HAVE BEEN PROCESSED                   *         
***********************************************************************         
         SPACE 1                                                                
BUMPSCN2 ZIC   R1,FNDX2            INCREMENT FIELD INDEX BY ONE                 
         LA    R1,1(R1)                                                         
         STC   R1,FNDX2                                                         
                                                                                
         CLI   FNDX2,1                                                          
         BE    *+18                                                             
         IC    R1,SCAN1HL2                                                      
         AR    R3,R1               BUMP FOR 1ST HALF LENGTH                     
         IC    R1,SCNLNTH2                                                      
         LA    R3,12(R1,R3)        BUMP FOR 2ND HALF LENGTH & FIXED             
                                                                                
         CLC   FNDX2,NFLDS2        COMPARE FIELD INDEX TO N'FIELDS              
         BR    RE                                                               
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* ROUTINE TO BUILD A TWA FIELD HEADER & A TWA FIELD AT FLDH & FLD     *         
*                                                                     *         
* NTRY - R2=A(SCANNER BLOCK ENTRY)                                    *         
*        R1=ZERO TO BUILD FROM LHS OF BLOCK ELSE RHS                  *         
***********************************************************************         
         SPACE 1                                                                
BLDFLD   LTR   R1,R1                                                            
         BZ    *+8                                                              
         LA    R1,1                                                             
         SR    RF,RF                                                            
         IC    RF,0(R2,R1)         RF=DATA LENGTH                               
         MH    R1,=H'10'                                                        
         LA    R1,12(R2,R1)        R1=A(DATA)                                   
         XC    FLDH,FLDH                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),0(R1)        MOVE IN THE DATA                             
         LA    R0,8(RF)            SET TOTAL FIELD LENGTH                       
         STC   R0,FLDH                                                          
         B     FVAL4              GO SET OTHER VALUES & TEST FOR HELP           
         SPACE 1                                                                
* GENERAL EXIT FROM ROUTINES & MODULE                                           
*                                                                               
EXITH    DS    0H                  EXIT W/ CC HIGH                              
         LA    R0,1                                                             
         J     EXITCR                                                           
                                                                                
EXITL    DS    0H                  EXIT W/ CC LOW                               
         LHI   R0,-1                                                            
         J     EXITCR                                                           
                                                                                
EXITE    DS    0H                  EXIT W/ CC EQUAL                             
         SR    R0,R0                                                            
         J     EXITCR                                                           
                                                                                
EXITCR   DS    0H                                                               
         AR    R0,R9                                                            
         CR    R0,R9               SET CC                                       
         J     EXIT                 AND EXIT                                    
                                                                                
EXIT     XIT1                                                                   
                                                                                
*                                                                               
XITMODSV DS    0H                  SAVE TIA AREA BEFORE EXITING MODULE          
         MVI   GOSTEON,STI#                                                     
         GOTO1 AGOSTEO                                                          
         J     XITMOD                                                           
*                                                                               
XITMOD   DS    0H                  EXITS TO DEM00 CALLER                        
         L     RD,AWORK                                                         
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
REPMSG   DC    C'REPORT ID=XXX,9999'                                            
         SPACE 1                                                                
                                                                                
* Arbitron Feb94 request valid only for the following stations                  
ARBF94   DC    C'WAKC'             CLEVLAND                                     
         DC    C'WBNX'                                                          
         DC    C'WEAO'                                                          
         DC    C'WEWS'                                                          
         DC    C'WJW '                                                          
         DC    C'WKYC'                                                          
         DC    C'WNEO'                                                          
         DC    C'WOAC'                                                          
         DC    C'WOIO'                                                          
         DC    C'WUAB'                                                          
         DC    C'WVIZ'                                                          
         DC    C'WDIV'             DETRIOT                                      
         DC    C'WGPR'                                                          
         DC    C'WJBK'                                                          
         DC    C'WKBD'                                                          
         DC    C'WTVS'                                                          
         DC    C'WXON'                                                          
         DC    C'WXYZ'                                                          
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
***      DROP  R6,R7,R8,R9,RA,RB                                                
         DROP  R6,R7,RA,RB            NEED R8,R9 FOR LATER                      
         EJECT                                                                  
GETCOMS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
                                                                                
         L     R1,=A(EBREC-DEMWRKD)                                             
         LA    R1,DEMWRKD(R1)                                                   
         ST    R1,AEBREC              SET A(EBD RECORD)                         
         AH    R1,=AL2(APWORK-EBREC)  ADD LENGTH OF EBREC                       
         ST    R1,AAPWORK             SET A(APPLICATION WORK AREA)              
         LH    R1,=Y(IOAREA1-DEMWRKD)                                           
         LA    R1,DEMWRKD(R1)                                                   
         ST    R1,AIOAREA1                                                      
         LH    R1,=Y(IOAREA2-DEMWRKD)                                           
         LA    R1,DEMWRKD(R1)                                                   
         ST    R1,AIOAREA2                                                      
         LH    R1,=Y(BINTAB-DEMWRKD)                                            
         LA    R1,DEMWRKD(R1)                                                   
         ST    R1,BINATAB                                                       
         L     R1,=A(TSIOREC-DEMWRKD)                                           
         LA    R1,DEMWRKD(R1)                                                   
         ST    R1,ATSIOREC            SET A(TSAR I/O AREA)                      
*                                                                               
         L     R1,AFAC             GET ADDRESSES FROM COMFACS                   
         USING COMFACSD,R1                                                      
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VDEMOMTH,CDEMOMTH                                                
         MVC   VDEMOUT,CDEMOUT                                                  
         MVC   VDEMAND,CDEMAND                                                  
         MVC   VDEMOVAL,CDEMOVAL                                                
         MVC   VGETFACT,CGETFACT                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VGETPROF,CGETPROF                                                
         MVC   VADDAY,CADDAY                                                    
         MVC   VSWITCH,CSWITCH                                                  
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         XIT1                                                                   
         DROP  R1                                                               
         LTORG                                                                  
***********************************************************************         
* ROUTINE TO BUMP TO NEXT SCANNER BLOCK ENTRY #2                      *         
*                                                                     *         
* NTRY - SCNLNTH2=L'RHS OF SCANNER BLOCK ENTRY                        *         
*        R3=A(BLOCK ENTRY), FNDX=CURRENT FIELD INDEX NUMBER           *         
*                                                                     *         
* EXIT - CC=HIGH IF ALL ENTRIES HAVE BEEN PROCESSED                   *         
***********************************************************************         
         SPACE 1                                                                
BUMPSCN2 NTR1  BASE=*,LABEL=*                                                   
         ZIC   R1,FNDX2            INCREMENT FIELD INDEX BY ONE                 
         LA    R1,1(R1)                                                         
         STC   R1,FNDX2                                                         
                                                                                
         CLI   FNDX2,1                                                          
         BE    *+18                                                             
         IC    R1,SCAN1HL2                                                      
         AR    R3,R1               BUMP FOR 1ST HALF LENGTH                     
         IC    R1,SCNLNTH2                                                      
         LA    R3,12(R1,R3)        BUMP FOR 2ND HALF LENGTH & FIXED             
                                                                                
         CLC   FNDX2,NFLDS2        COMPARE FIELD INDEX TO N'FIELDS              
         XIT1  REGS=(R3)                                                        
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* ROUTINE TO PERFORM SOME TASK FOR CANADIAN LASTEST BOOK                        
* DONT LOCK IN LATEST BOOK - LET THE REPORT MODULES CALL                        
* SPGETDEMF WITH NULL BOOK FOR LATEST                                           
* ONLY LOCK IN YEAR AND CLEAR OUT MONTH/WEEK BYTE                               
* THE PROBLEM IS YOU CAN ASK FOR A MONTHLY BOOK BUT WE                          
* GET BACK WEEKLY ACTBK, SO SAVING IT AND REDOING A LOOKUP IS                   
* ASKING FOR TROUBLE                                                            
CANLATWK NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     RE,ADEMFIL                                                       
         CLC   =C'WTP',8(RE)            SET THE FLAG AS IF WE VALIDATED         
         BNE   *+8                      AS WEEKLY  FOR WTP LATEST               
         OI    MISCFLG1,MF1BKVWK        LOOKUP                                  
*   CANADIAN CSI                                                                
         CLI   DBSRC,C'N'               MUST USE PERIOD FOR LATEST              
         BNE   CANLATY                  CSI TP LOOKUP                           
         CLC   =C'TP',8(RE)                                                     
         BNE   CANLATY                                                          
         MVC   EADDR+2(2),=Y(EACTCSI-DEM00)                                     
         L     RF,ADEMACT                                                       
         CLC   =C'DISPLAY',8(RF)                                                
         BNE   CANLATY                                                          
         CLC   =C'PERIOD',8(RF)                                                 
         BNE   CANLATN                                                          
*                                                                               
** EXITS **                                                                     
*                                                                               
CANLATY  DS    0H                                                               
         XC    EADDR,EADDR                                                      
         J     EXITE                                                            
*                                                                               
CANLATN  DS    0H                                                               
         L     RF,EADDR                                                         
         A     RF,ABASE                                                         
         ST    RF,EADDR                                                         
         J     EXITL                                                            
         EJECT                                                                  
         LTORG                                                                  
*-------------------- EDIT DATA TO BE FORMATTED ----------------------*         
                                                                                
* Routine is called by FMTFLD only.                                             
* At entry,                                                                     
*  R4-->keyword entry in STROIKY table,                                         
*  R1 = length of data to be formatted,                                         
*  IOAREA1 contains the data.                                                   
* At exit,                                                                      
*  R1 = length of editted data to format.                                       
** !!WARNING!! IOAREA2 will be clobbered here                                   
*                                                                               
FMTFEDT  NTR1  BASE=*,LABEL=*                                                   
         USING STROIKYD,R4                                                      
                                                                                
         CLI   STKNUMB,IKNBOO                                                   
         BE    FMTFE50                                                          
         CLI   STKNUMB,IKNDAY                                                   
         BE    FMTFEDAY                                                         
         B     FMTFEX                                                           
*                                                                               
FMTFE50  DS    0H                  FORMATTING BOOK FIELD                        
         CH    R1,=H'3'                                                         
         BNH   FMTFE52                                                          
         L     RE,AIOAREA1                                                      
         CLC   =C'MBK',0(RE)        IF MULTIBOOKS REQUESTED, OR                 
         BE    *+10                                                             
         CLC   =C'EST',0(RE)        IF ESTIMATE REQUESTED,                      
         BNE   FMTFE52                                                          
         LA    R1,3                 FORMAT 3 BYTES ONLY ('EST')                 
                                                                                
FMTFE52  DS    0H                                                               
         B     FMTFEX                                                           
                                                                                
*                                                                               
** DAY/TIME **                                                                  
*                                                                               
FMTFEDAY DS    0H                                                               
         L     R2,AIOAREA2                                                      
         LH    R3,LIOAREA2                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE               CLEAR SCANNER OUTPUT BLOCK AREA              
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   SCAN1HL2,10                                                      
         MVI   SCNLNTH2,SCANLDAY                                                
         LR    R0,R1                                                            
         SLL   R0,16                                                            
         ST    R0,FULL                                                          
         LHI   R0,MAXDTCMP                                                      
         L     R3,AIOAREA2                                                      
         GOTO1 AMYSCAN,DMCB,(SCNLNTH2,AIOAREA1),((R0),(R3)),C',=+/',   +        
               FULL                                                             
         BE    *+12                IF ERROR,                                    
         LH    R1,FULL              RESTORE ORIG LEN FROM  FULL+0(2),           
         B     FMTFEDAYX            AND EXIT ROUTINE                            
*                                                                               
         MVC   NFLDS,DMCB+4                                                     
                                                                                
*                                                                               
FMTFED10 DS    0H                  LOOP THROUGH SCANNER OUTPUT BLOCK            
         CLI   0(R3),0              IF AT END,                                  
         BE    FMTFED49              WE'RE DONE                                 
*                                                                               
         LA    RE,12(R3)            RE-->START OF SEARCH AREA                   
         ZIC   RF,0(R3)                                                         
         AR    RF,RE                RF-->NEXT BYTE AFTER SEARCH AREA            
         LHI   R0,C','              LOOK FOR A COMMA                            
         SRST  RF,RE                                                            
         BC    2,FMTFED29           NOT FOUND, BUMP TO NEXT ENTRY               
*                                                                               
         GOTO1 VDAYPAK,DMCB,(0(R3),12(R3)),WORK,WORK+1                          
         CLI   WORK,0                                                           
         BE    FMTFED29             IF ERROR, SKIP THIS DAY/TIME INPUT          
         GOTO1 VDAYUNPK,DMCB,WORK,(X'07',12(R3))                                
         MVI   0(R3),7                                                          
FMTFED29 EQU   *                                                                
*                                                                               
         DS    0H                  BUMP TO NEXT ENTRY                           
         BAS   RE,FMTFEDBUMP                                                    
         B     FMTFED10                                                         
FMTFED49 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  RE-STRING DAY/TIME INPUT                     
         L     R2,AIOAREA1                                                      
         L     R3,AIOAREA2                                                      
*                                                                               
FMTFED52 DS    0H                  LOOP THROUGH AND STRING 1 AT A TIME          
         CLI   0(R3),0              IF AT END,                                  
         BE    FMTFED59              WE'RE DONE                                 
*                                                                               
         DS    0H                                                               
         XC    WORK,WORK            USE  WORK  AS DUMMY TWA FIELD               
         MVI   WORK+0,L'WORK                                                    
                                                                                
         L     RF,AFAC                                                          
         L     RF,(CUNSCAN-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,(1,12(R3)),(SCNLNTH2,WORK),C',=+/',0                   
         CLI   DMCB+00,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         DS    0H                   MOVE UNSCAN OUTPT TO CALLER'S OUTPT         
         LA    RE,WORK+8                                                        
         LA    RF,WORK+(L'WORK-1)                                               
FMTFED55B DS   0H                                                               
         CLI   0(RF),C' '            SEARCH BACKWARDS FOR LAST CHAR             
         BH    FMTFED55L             IF FOUND, EXIT LOOP                        
         CR    RF,RE                 IF NOT FOUND YET, AND NOT AT START         
         BNH   *+8                                                              
         BCT   RF,FMTFED55B           OF SEARCH STRING, KEEP GOING              
         MVI   0(RE),C' '             OTHERWISE, SURVIVE IT                     
FMTFED55L EQU  *                                                                
         SR    RF,RE                 RF = L(OUTPUT) FROM UNSCAN                 
         BNM   *+6                                                              
         DC    H'0'                                                             
         EXMVC RF,0(R2),0(RE)                                                   
         AR    R2,RF                                                            
         MVI   1(R2),C'+'            ASSUME MORE THAN ONE D/T COMPONENT         
         AHI   R2,1+1                                                           
                                                                                
         DS    0H                   BUMP TO NEXT SCAN BLOCK ENTRY               
         BAS   RE,FMTFEDBUMP                                                    
         B     FMTFED52                                                         
FMTFED59 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  CALCULATE L(OUR OUTPUT TO CALLER)            
         SHI   R2,1                                                             
         MVI   0(R2),C' '                                                       
         L     R0,AIOAREA1                                                      
         SR    R2,R0                                                            
         LR    R1,R2                                                            
*                                                                               
         B     FMTFEDAYX                                                        
                                                                                
*                                                                               
FMTFEDBUMP DS  0H                  BUMP TO NEXT ENTRY                           
         ZIC   R1,SCAN1HL2                                                      
         AR    R3,R1                                                            
         IC    R1,SCNLNTH2                                                      
         LA    R3,12(R1,R3)                                                     
         BR    RE                                                               
*                                                                               
FMTFEDAYX EQU  *                                                                
                                                                                
                                                                                
FMTFEX   DS    0H                                                               
         XIT1  REGS=(R1)                                                        
                                                                                
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
*--------------- FORMAT (EXTRA) DATA INTO OPTIONS FIELD --------------*         
                                                                                
* At entry,                                                                     
*  R5-->chunk containing the input data.                                        
* At exit,                                                                      
*  CC low   if not all of input formatted into field,                           
*  CC equal if all of input formatted into field,                               
*  CC high  if an error occurred.                                               
* Note: BYTE is used by caller, do not clobber it!                              
*                                                                               
FMTOFLD  NTR1  BASE=*,LABEL=*                                                   
         USING STICKEYD,R5                                                      
*                                                                               
*        L     RE,AIOAREA1                                                      
*        LA    RF,2000                                                          
*        XCEF                                                                   
*                                                                               
         L     R4,ASTROIKY                                                      
         USING STROIKYD,R4                                                      
FMTOF12  CLI   0(R4),EOT           SHOULDN'T REACH EOTABLE                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   STKNUMB,IKNOPT      LOCATE OPTION KEYWORD ENTRY                  
         BE    FMTOF14                                                          
         LA    R4,STROIKYQ(R4)                                                  
         B     FMTOF12                                                          
*                                                                               
FMTOF14  DS    0H                  R4-->OPTIONS KEYWORD ENTRY                   
         ZICM  RE,STKFDSPH,(3)                                                  
         LA    RE,DEMWRKD(RE)                                                   
         L     R2,0(RE)            R2-->OPTIONS FIELD                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         LA    R3,8(RF,R2)         R3-->NEXT AVAILABLE SLOT IN OPT FLD          
         BZ    *+8                 CC SET BY  ICM  ABOVE                        
         LA    RF,1(RF)            ADJUST LENGTH FOR COMMA (C',')               
*                                                                               
         DS    0H                  GET L(DATA TO FORMAT)                        
         ZIC   R0,STICLEN                                                       
         SH    R0,=Y(STICFIXL)                                                  
         BZ    FMTOFX                                                           
*                                                                               
         SR    R1,R1               USE R1 TO HOLD THAT LENGTH                   
         CVD   R1,DUB              DUB--COUNTER USED IN PACKED DECIMAL          
         LA    R6,STICDATA                                                      
         L     R7,AIOAREA1          IOAREA1 WILL HAVE DATA TO APPEND            
*                                                                               
FMTOF31  DS    0H                                                               
         CLI   0(R6),STSPSOPI                                                   
         BE    FMTOF32                                                          
         CLI   0(R6),STSPEOPI                                                   
         BE    FMTOF34                                                          
         B     FMTOF36                                                          
*                                                                               
FMTOF32  DS    0H                  STSPSOPI ENCOUNTERED                         
         CP    DUB,=P'1'            IGNORE IF MORE THAN ONE                     
         BE    FMTOF36                                                          
         AP    DUB,=P'1'                                                        
         B     FMTOF38                                                          
*                                                                               
FMTOF34  DS    0H                  STSPEOPI ENCOUNTERED                         
         CP    DUB,=P'0'           WAS THERE A MATCHING STSPSOPI?               
         BE    FMTOF38              NO, LET IT PASS                             
         SP    DUB,=P'1'                                                        
         B     FMTOF38                                                          
*                                                                               
FMTOF36  DS    0H                  PROCESS CHARACTER                            
         CP    DUB,=P'0'           DOES IT BELONG IN OPTIONS FIELD?             
         BE    FMTOF38              NO, IT WAS PUT IN CURRENT FIELD             
         MVC   0(1,R7),0(R6)        YES, IT BELONGS HERE                        
         LA    R7,1(R7)                                                         
         LA    R1,1(R1)             UP LENGTH BY ONE                            
*                                                                               
FMTOF38  DS    0H                                                               
         LA    R6,1(R6)                                                         
         BCT   R0,FMTOF31                                                       
*                                                                               
         L     R6,AIOAREA1                                                      
         CLC   0(3,R6),=C'MBK'                                                  
         BNE   FMTOF40                                                          
         BCTR  R1,0                                                             
         LA    R3,OPTMBKH+1                                                     
         EXMVC R1,0(R3),0(R6)      APPEND INPUT DATA ONTO FIELD                 
         AHI   R1,1                                                             
         STC   R1,OPTMBKH                                                       
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     FMTOFX                                                           
*                                                                               
FMTOF40  DS    0H                  R1=L(DATA TO FORMAT), IOAREA1=DATA           
         LTR   R1,R1               MAKE SURE L(DATA TO FORMAT) > 0              
         BNP   FMTOF80              IF NOT, DON'T FORMAT DATA                   
*                                                                               
         AR    RF,R1               RF=NEW POTENTIAL LENGTH                      
         ZIC   RE,STKFMXL2         GET APPROPRIATE MAX LENGTH OF FIELD          
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    *+8                                                              
         IC    RE,STKFMXLN                                                      
         CR    RF,RE               DOES IT FIT IN DATA FIELD?                   
         BH    FMTOFX              NO, EXIT W/ CC HIGH                          
*                                                                               
         CLI   5(R2),0             IF DATA FIELD NOT EMPTY,                     
         BE    *+12                                                             
         MVI   0(R3),C','           PLACE COMMA TO DIVIDE FIELD                 
         LA    R3,1(R3)                                                         
         BCTR  R1,0                                                             
         L     RE,AIOAREA1                                                      
         EXMVC R1,0(R3),0(RE)      APPEND INPUT DATA ONTO FIELD                 
         STC   RF,5(R2)            UPDATE NEW LEN INTO FLD HEADER               
*                                                                               
FMTOF80  DS    0H                  PREPARE TO EXIT                              
         ZIC   R0,STICLEN                                                       
         SH    R0,=Y(STICFIXL)                                                  
         LA    R1,1(R1)                                                         
         CR    R1,R0               DATA FORMATTED VS. DATA FROM STEREO          
         B     FMTOFX              EXIT W/ CC SET                               
*                                                                               
FMTOFX   DS    0H                                                               
         J     EXIT                                                             
                                                                                
         DROP  R4,R5                                                            
         EJECT                                                                  
         LTORG                                                                  
************************************************************                    
DATEVAL  NTR1  BASE=*,LABEL=*                                                   
         STC   R1,DATEWORK                                                      
         GOTO1 VDATVAL,DATEPARM,(0,12(R2)),DATEWORK+1                           
         L     R0,0(R1)                                                         
         CLM   R0,1,0(R2)                                                       
         BNE   DATEVAL2                                                         
         GOTO1 VDATCON,DATEPARM,(0,DATEWORK+1),(2,DATEOUT)                      
         B     DATEVAL6                                                         
DATEVAL2 GOTO1 VDATVAL,DATEPARM,(1,12(R2)),DATEWORK+1                           
         L     R0,0(R1)                                                         
         CLM   R0,1,0(R2)                                                       
         JNE   EIIF                                                             
         ZIC   R0,BUYSTR           R0=START YEAR                                
         CVD   R0,DATEDUB                                                       
         OI    DATEDUB+7,X'0F'                                                  
         UNPK  DATEWORK+1(2),DATEDUB                                            
         GOTO1 VDATCON,DATEPARM,(0,DATEWORK+1),(2,DATEOUT)                      
         CLC   BUYSTR(1),BUYEND                                                 
         BNE   DATEVAL4                                                         
         B     DATEVAL6                                                         
DATEVAL4 CLC   DATEOUT,DATESTRQ                                                 
         BNL   DATEVAL6                                                         
         ZIC   R0,BUYEND           R0=END YEAR                                  
         CVD   R0,DATEDUB                                                       
         OI    DATEDUB+7,X'0F'                                                  
         UNPK  DATEWORK+1(2),DATEDUB                                            
         GOTO1 VDATCON,DATEPARM,(0,DATEWORK+1),(2,DATEOUT)                      
DATEVAL6 CLI   DATEWORK,0          TEST START DATE                              
         BNE   DATEVAL8                                                         
         CLC   DATEOUT,DATEENDQ                                                 
         JH    EIIF                                                             
         B     DATEVALX                                                         
DATEVAL8 CLC   DATEOUT,DATESTRQ                                                 
         JL    EIIF                                                             
DATEVALX J     EXIT                                                             
         DROP  RC                                                               
         EJECT                                                                  
*                                                                               
*===================== STEREO ROUTINES INTERFACE =====================*         
                                                                                
GOSTEO   NTR1  BASE=*,LABEL=*                                                   
*&&DO                                                                           
         L     RA,BBASE                                                         
         L     R7,CBASE                                                         
         L     R6,DBASE                                                         
                                                                                
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(3,RE),=C'GOS'                                                  
         MVC   3(1,RE),GOSTEON                                                  
         SR    RE,RE               CLEAR RE JUST TO BE SAFE                     
*&&                                                                             
                                                                                
         DS    0H                                                               
         CLI   GOSTEON,STE#                                                     
         BH    GOSTEO2                                                          
         GOTO1 ASTEREO,DMCB,(R9),(GOSTEON,0)                                    
         B     GOSTEOX                                                          
GOSTEO2  DS    0H                                                               
         GOTO1 ASTEREO2,DMCB,(R9),(GOSTEON,0)                                   
         DS    0H                                                               
                                                                                
GOSTEOX  J     EXIT                                                             
***********************************************************************         
* ROUTINE TO VALIDATE STATE CODE FOR COUNTY COVERAGE                            
* ENTRY  HALF=2 BYTE STATE CODE                                                 
* EXIT   BYTE=1 BYTE INTERNAL STATE CODE                                        
* CC =EQ IF VALID STATE CODE                                                    
* CC =NEQ IF INVALID STATE CODE                                                 
***********************************************************************         
VALSTATE NTR1  BASE=*,LABEL=*                                                   
         L     R1,ASTACODE                                                      
VALSTA04 CLC   =X'FFFF',0(R1)                                                   
         BE    VALSTAN                                                          
         CLC   1(2,R1),HALF                                                     
         BE    VALSTA10                                                         
         AHI   R1,STACODEL                                                      
         B     VALSTA04                                                         
VALSTA10 MVC   BYTE,0(R1)                                                       
VALSTAY  J     EXITE                                                            
VALSTAN  J     EXITL                                                            
*                                                                               
*                                                                               
*------------------------ GET RADIO SPILL MKT# -----------------------*         
*                                                                               
RADSPLMK NTR1  BASE=*                                                           
         CLI   DBMED,C'R'          EXIT IF NOT RADIO                            
         BNE   RSMX                                                             
                                                                                
         L     R3,AEBREC           R3-->BUY RECORD                              
         USING BUYD,R3                                                          
         GOTO1 VMSUNPK,DMCB,BUYMSTA,FULL,DUB                                    
*                                                                               
** GET ALPHAMKT CODE FROM MKT RECORD **                                         
*                                                                               
         LA    R1,WORK             BUILD KEY OF MARKET RECORD                   
         USING MKTRECD,R1                                                       
         MVI   MKTKEY,C'0'                                                      
         MVC   MKTKEY+1(L'MKTKEY-1),MKTKEY                                      
         MVI   MKTKTYPE,MKTKTYPQ   RECORD TYPE                                  
         MVI   MKTKMED,C'R'        MEDIA                                        
         MVC   MKTKMKT,FULL        EBCDIC MKT# (FROM MSUNPK ABOVE)              
         MVC   MKTKAGY,BUYALPHA    AGENCY CODE                                  
         DROP  R1,R3                                                            
                                                                                
         L     R0,AIOAREA2         READ MARKET RECORD                           
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'STATION',WORK,(R0)                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LR    RF,R0               PULL OUT ALPHAMKT CODE                       
         USING MKTRECD,RF                                                       
         CLC   WORK(MKTKEYLN),0(RF)                                             
         BNE   RSMX                                                             
         OC    MKTALST(3),MKTALST   ANY ALPHA-MKT CODE?                         
         BZ    RSMX                  NOPE                                       
         MVC   ALFMKTS,MKTALST                                                  
         DROP  RF                                                               
         B     RSMX                                                             
*                                                                               
RSMX     DS    0H                                                               
**       B     EXIT                                                             
         J     EXIT                                                             
         DROP  RB                                                               
         LTORG                                                                  
***********************************************************************         
* ROUTINE TO GRAB SYSCODE FROM STATIONS FIELD                                   
* ON ENTRY R2 SHOULD BE POINTING TO CURRENT SCANNER BLK ENTRY                   
***********************************************************************         
GETSYSCD NTR1  BASE=*,LABEL=*                                                   
         CLI   DBSRC,C'N'          NSI                                          
         BE    *+8                                                              
         CLI   DBMED,C'T'          USTV                                         
         BE    *+8                                                              
         CLI   DBSRC,C'F'          FUSION                                       
         BNE   GETSYSX                                                          
         L     RE,ADEMFIL                                                       
         CLC   =C'T4',8(RE)                                                     
         BE    *+10                                                             
         CLC   =C'TP',8(RE)                                                     
         BNE   GETSYSX                                                          
         CLI   ACTN,STATION                 ACTION=STATION                      
         BE    GETSYSX                                                          
* IF ACTION = SYSCODE THE SCANNER BLOCK SHOULD BE SET AND                       
* ONLY INPUT SHOULD BE NUMERIC OR ALPHA                                         
* IF ITS ALPHA LEAVE IT ALONE AND EXIT                                          
* IF ITS NUMERIC STORE THE NUMERIC SYSCODE IN SYSCODE BUFFER                    
**********                                                                      
*  MOVE SYSCODE TO STORAGE                                                      
         CLI   ACTN,SYSCODE                 ACTION=STATION                      
         BNE   GETSYS20                                                         
         TM    2(R2),X'80'                  NUMERIC SYSCODE?                    
         BZ    GETSYSX                      ALPHA JUST LEAVE                    
         LR    RF,R2                                                            
         USING SCANBLKD,RF                                                      
         LA    R0,L'SYSCODES                                                    
         ZIC   R1,NSTAS                                                         
         MR    R0,R0                                                            
         LA    R1,SYSCODES(R1)                                                  
         MVC   0(L'SYSCODES,R1),SC1STNUM+2 AND SLOT IT INTO LIST                
         DROP  RF                                                               
**********                                                                      
*                                                                               
GETSYS20 DS    0H                                                               
*                                                                               
         ZIC   RF,MFLDS                                                         
         LA    RF,1(RF)                                                         
         XC    WORK2,WORK2     CREATE DUMMY SCREEN FIELD FOR SCANNER            
         LR    RF,R2           CURRENT SCANBLKD ENTRY                           
**       LA    RF,IOAREA1                                                       
         USING SCANBLKD,RF                                                      
         ZIC   RE,SC2NDLEN                                                      
         MVC   WORK+5(1),SC2NDLEN                                               
         ZIC   R0,WORK+5                                                        
         AHI   R0,8                                                             
         STC   R0,WORK                                                          
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),SC2NDFLD                                               
         LA    R3,SC2NDNUM                                                      
         ZIC   RE,SC2NDLEN                                                      
         LA    R4,SC2NDFLD                                                      
         AR    R4,RE               END OF THE SECOND FIELD                      
         DROP  RF                                                               
*                                                                               
         GOTO1 VSCANNER,DMCB,(SCANLNTH,WORK),((RF),AIOAREA2),C',=,-'            
*                                                                               
         L     R1,AIOAREA2                                                      
         USING SCANBLKD,R1                                                      
         MVC   0(4,R3),SC1STNUM                                                 
         ZIC   RE,SC2NDLEN            LENGTH OF SYSCODE                         
         CHI   RE,0                   EXIT OF NO SYSCODE ENTERED                
         BE    GETSYSX                                                          
         TM    SC2NDVAL,X'80'         HAS TO BE NUMERIC                         
         JZ    EISTA                                                            
         CHI   RE,4                   4 BYTE SYSCODE MAX                        
         JH    EISTA                                                            
         CHI   RE,1                   HAS TO BE ATLEAST 1 BYTE                  
         JL    EISTA                                                            
         AHI   RE,1                                                             
         SR    R4,RE                                                            
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                   CLEAR OUT SYSCODE INPUT FROM              
         MVC   0(0,R4),=10X'40'       ORIGINAL SCANNER BLOCK                    
*                                                                               
         LR    RF,R2                  LENGTH OF ORIGINAL                        
         ZIC   RE,1(RF)               2ND FLD WHICH WAS MKT-SYSCODE             
         SHI   RE,3                                                             
         STC   RE,1(RF)                                                         
         MVC   3(1,RF),2(R1)                                                    
         DROP  R1                                                               
                                                                                
*                                                                               
                                                                                
*  MOVE SYSCODE TO STORAGE                                                      
         L     RF,AIOAREA2                                                      
         USING SCANBLKD,RF                                                      
         LA    R0,L'SYSCODES                                                    
         ZIC   R1,NSTAS                                                         
         MR    R0,R0                                                            
         LA    R1,SYSCODES(R1)                                                  
****     MVC   0(L'SYSCODES,R1),SC2NDFLD  AND SLOT IT INTO LIST                 
         MVC   0(L'SYSCODES,R1),SC2NDNUM+2 AND SLOT IT INTO LIST                
         DROP  RF                                                               
                                                                                
*                                                                               
GETSYSX  J     EXITE                                                            
         DROP  RB                                                               
         LTORG                                                                  
*======================= CHECK IF DEM32 SESSION ======================*         
                                                                                
* This routine is called after a STEREO session is confirmed.  This             
*  routine further checks to see if it is a DEM32 session as well.              
* At entry,                                                                     
*   R2-->field to check for DEM32 indicators                                    
* At exit,                                                                      
*   CC equal ==> DEM32 session, and internal DEM32 flags are set                
*   CC nequal==> not DEM32 session                                              
* !! NOTE !! The scheme which uses this routine to help verify DEM32            
*             sessions is flawed.  This routine along with the scheme           
*             will eventually be discarded.  We can not trash it now            
*             because there are DEM32 versions out there which                  
*             still need this routine.  Once we're fairly confident             
*             that the DEM32s out there are up-to-speed, say good-bye           
*             to this routine.  This routine is renamed to "ISDEM3X"            
*             for this purpose.                                                 
                                                                                
ISDEM3X  NMOD1 0,ISDEM3X?,CLEAR=YES                                             
                                                                                
         DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    ID3XXN                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         L     R0,AIOAREA1                                                      
         LR    R3,R0               USE R3 TO POINT TO SCANNER OUTPUT            
         LA    R1,2000                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR OUTPUT AREA FOR SCANNER                
*                                                                               
         DS    0H                                                               
         GOTO1 VSCANNER,DMCB,(R2),AIOAREA1,0                                    
         CLI   DMCB+4,0                                                         
         BE    ID3XXN                                                           
                                                                                
*                                                                               
** DEM32 FLAG **                                                                
*                                                                               
         DS    0H                                                               
         CLI   0(R3),5             L'C"DEM32" = 5                               
         BNE   ID3XXN                                                           
         CLI   1(R3),0             FIELD MUST NOT BE DIVIDED                    
         BNE   ID3XXN                                                           
         CLC   =C'DEM32',12(R3)                                                 
         BNE   ID3XXN                                                           
         OI    DEMFLAG1,DF1DEM32   DEM32 SESSION CONFIRMED!                     
*                                                                               
         AHI   R3,32                                                            
                                                                                
*                                                                               
** VERSION (EXTRACT) DATE **                                                    
*                                                                               
         DS    0H                                                               
         CLI   0(R3),0                                                          
         BNE   *+14                                                             
         MVC   12(8,R3),=C'JAN05/99'  THIS IS 1ST EXTRACT CUT-OFF DATE          
         MVI   0(R3),8                                                          
                                                                                
*                                                                               
         DS    0H                  CHECK FOR "LATEST" BY DDS TERMINAL           
         L     RF,ATWA                                                          
         CLI   (TWAOFFC-TWAD)(RF),C'*'  IGNORE IF NOT DDS TERMINAL              
         BNE   ID3XVERL                                                         
         CLI   0(R3),6                  IGNORE IF L(INPUT) <> 6                 
         BNE   ID3XVERL                                                         
         CLC   =C'LATEST',12(R3)                                                
         BNE   ID3XVERL                                                         
         MVI   D32PCVER+0,XFF                                                   
         MVC   D32PCVER+1(L'D32PCVER-1),D32PCVER   SET TO ALL X'FF'             
         B     ID3XVERX                                                         
ID3XVERL EQU   *                                                                
                                                                                
*                                                                               
         GOTO1 VDATVAL,DMCB,12(R3),DUB                                          
         OC    DMCB+0(4),DMCB+0                                                 
         BZ    ID3XXN                                                           
*                                                                               
*&&DO                                                                           
         GOTO1 VDATCON,DMCB,(0,DUB),(3,D32XTRCT),0                              
*&&                                                                             
         GOTO1 VDATCON,DMCB,(0,DUB),(3,D32PCVER+1),0                            
ID3XVERX EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         B     ID3XXY                                                           
                                                                                
*                                                                               
ID3XXN   DS    0H                                                               
         J     EXITL                                                            
*                                                                               
ID3XXY   DS    0H                                                               
         J     EXITE                                                            
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  RB                                                               
***********************************************************************         
***********************************************************************         
*======================= TRANSLATE ALPHA MARKET ======================*         
                                                                                
* Calls DEMAND to translate an alpha market into a numeric market               
* At entry, DUB(3)   = alpha market padded w/ spaces                            
* At exit,  DUB+3(2) = numeric market                                           
                                                                                
TRAMKT   NMOD1 0,*TRAMKT*,CLEAR=YES                                             
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5          R5=A(DBLOCK)                                 
*                                                                               
*  RADAR STUFF,JUST FUDGE MARKET STUFF FOR NOW....MARKET IS 1                   
         CLI   DBSRC,C'R'                                                       
         BNE   TRAMK10                                                          
         CLC   =C'USA',DUB                                                      
         BNE   TRAMK10                                                          
         MVC   DUB+3(2),=X'0001'                                                
         B     TRAX                                                             
*                                                                               
TRAMK10  DS    0H                                                               
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,DBFIL                                                     
         MVC   DBAREC,AIOAREA2                                                  
*&&DO                                                                           
         MVI   DBFUNCT,DBGETDEM                                                 
*&&                                                                             
         MVI   DBFUNCT,DBCNVA2N                                                 
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBBTYPE,BKS+3                                                    
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELBK,BKS                                                      
         MVC   DBSELMED,DBMED                                                   
         CLI   DBMED,C'O'         OVERNIGHTS READ USTV NSI                      
         BNE   *+8                                                              
         MVI   DBSELMED,C'T'                                                    
         CLI   DBSRC,C'F'         FUSION READ USTV NSI                          
         BNE   *+8                                                              
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELAGY,AGYALPH                                                 
*&&DO                                                                           
         MVI   DBSELDAY,X'40'      ARBITRARILY PICKING MONDAY                   
*&&                                                                             
         MVC   DBSELALF,DUB                                                     
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
                                                                                
*&&DO                                                                           
         MVC   DUB+3(2),DBACTKMK   MARKET # RETURNED IN DBACTKMK                
*&&                                                                             
         MVC   DUB+3(2),DBSELRMK   MARKET # RETURNED IN DBSELRMK                
         CLI   DBERROR,0           USE IT IF NO ERROR                           
         BE    TRAX                                                             
*&&DO                                                                           
         CLI   DBERROR,NOTFOUND     OR IF ERROR=NOTFOUND (NO STATION)           
         BE    TRAX                                                             
*&&                                                                             
         XC    DUB+3(2),DUB+3       ELSE, RETURN NULLS                          
         DROP  R5                                                               
                                                                                
TRAX     DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD POSTLINE TO BINSRCH BUFFER                                     
         PRINT GEN                                                              
POST     NMOD1 (2000+8),**POST**,CLEAR=YES                                      
         PRINT NOGEN                                                            
*                                                                               
         MVC   0(8,RC),=C'*TMPTSIO'   EYECATCHER FOR TEMP TSAR I/O AREA         
         AHI   RC,8                                                             
         ST    RC,ATMPTSIO            A(TEMP TSAR I/O AREA)                     
*                                                                               
                                                                                
         TM    DEMFLAG1,DF1STERO   SEE IF STEREO SESSION                        
         BO    POST10               YES, POST IT THE STEREO WAY                 
                                                                                
         LA    R1,POSTLINE         SET A(BINSRCH RECORD)                        
         ST    R1,BINAREC                                                       
         MVI   BINCMND,1                                                        
         OC    BINLKEY,BINLKEY     TEST IF PREFORMATTED DATA                    
         BNZ   POST2               YES                                          
*                                                                               
         L     R1,BINSOFAR         ADD PREFORMATTED LINE TO TABLE               
         LA    RE,1(R1)                                                         
         ST    RE,BINSOFAR                                                      
         CLC   BINSOFAR,BINMAXN                                                 
         BH    POST4                                                            
***      BH    POSTX           TABLE NOT BIG ENOUGH FUDGE FOR NOW               
         L     R0,BINLREC                                                       
         MR    R0,R0                                                            
         A     R1,BINATAB          R1=A(NEXT TABLE ENTRY)                       
         L     RF,BINAREC          RF=A(RECORD TO BE ADDED)                     
         L     RE,BINLREC                                                       
         BCTR  RE,0                RE=L'RECORD-1                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RF)       MOVE RECORD TO TABLE                         
         B     POSTX                                                            
*                                                                               
POST2    GOTO1 VBINSRCH,BINPARMS   CALL BINSRCH TO ADD ENTRY TO TABLE           
         OC    BINAREC+1(3),BINAREC+1                                           
         BNZ   POSTX                                                            
         B     POSTX               FUDGE IT                                     
*                                                                               
POST4    L     RD,AWORK            TABLE FULL - EXIT WITH ERROR                 
         LM    RE,RC,12(RD)                                                     
         MVI   FERN,120                                                         
         LA    R1,DEMACTH                                                       
         ST    R1,FADR                                                          
         L     RF,AERROR0                                                       
         BR    RF                                                               
*                                                                               
POST10   DS    0H                  POSTING FOR STEREO                           
         MVI   GOSTEON,STPST#                                                   
         GOTO1 AGOSTEO                                                          
         B     POSTX                                                            
*                                                                               
POSTX    DS    0H                                                               
         L     RC,ATMPTSIO            A(TEMP TSAR I/O AREA)                     
         AHI   RC,-8                                                            
         XC    0(8,RC),0(RC)          CLEAR EYECATCHER                          
         XC    ATMPTSIO,ATMPTSIO      DON'T LEAVE ADDR HANGING AROUND           
*                                                                               
         J     EXIT                                                             
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO ADD BINARY USER ID INTO DBEXTEND                                   
         PRINT GEN                                                              
SETUSRID NMOD1 0,**USRID*,CLEAR=YES                                             
         PRINT NOGEN                                                            
*                                                                               
*  DBEXTEND  : SET DBXUUID                                                      
         LA    R4,DBLOCK1          TEST USER HAS ACCESS TO FILE                 
         USING DBLOCKD,R4                                                       
* FIRST CHECK TO SEE IF UID LINK ALREADY ESTABLISHED                            
         L     RF,DBEXTEND                                                      
         CLC   =C'UID ',0(RF)                                                   
         BE    SETUSRX                                                          
*                                                                               
         LA    RF,DBXUID                DEDEMWRK AREA                           
         MVC   4(4,RF),DBEXTEND         SET NEXT LINK TO PREV LINK              
         ST    RF,DBEXTEND                                                      
         USING DBXUIID,RF                                                       
         MVC   DBXUIID,=C'UID '                                                 
         MVC   DBXUUID,USERID                                                   
         CLI   ACTN,PROGNAME                AND LIST PROGRAM NAME OR            
         BNE   SETUSRX                                                          
         MVC   DBSELAGY,=C'SJ'                                                  
         MVC   DBXUUID,=X'0017'                                                 
         DROP  RF                                                               
*                                                                               
*                                                                               
SETUSRX  J     EXIT                                                             
                                                                                
         LTORG                                                                  
*&&DO                                                                           
DBXUID   DS     0X           EXTENDED BLOCK FOR USER ID                         
         DS     CL4                                                             
         DS     A                                                               
         DS     H                                                               
*&&                                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*======================= CHECK IF DEM32 SESSION ======================*         
                                                                                
* This routine is called after a STEREO session is confirmed.  This             
*  routine further checks to see if it is a DEM32 session as well.              
* At entry,                                                                     
*   Mainframe $DEM screen still intact                                          
*      DEMACT(5) = 'DEM32'  if DEM32 session, otherwise nulls                   
*      DEMSTN    = blank or date indicating which DEM32 extract version         
* At exit,                                                                      
*   CC equal ==> DEM32 session, and internal DEM32 flags are set                
*   CC nequal==> not DEM32 session                                              
                                                                                
ISDEM32  NMOD1 0,ISDEM32?,CLEAR=YES                                             
                                                                                
*                                                                               
** DEM32 FLAG **                                                                
*                                                                               
         DS    0H                                                               
*^^TEMP                                                                         
* The following 2 instructions are in place until the clients have              
*  latest version of DEM32, which uses "..." in the DATA field.                 
         CLI   DEMACTH+5,8                                                      
         BE    *+12                                                             
*^^EOTEMP                                                                       
         CLI   DEMACTH+5,5         L'C"DEM32" = 5                               
         BNE   ID32XN                                                           
         CLC   =C'DEM32',DEMACT                                                 
         BNE   ID32XN                                                           
                                                                                
*                                                                               
** VERSION (EXTRACT) DATE **                                                    
*                                                                               
         DS    0H                                                               
         CLI   DEMSTNH+5,0                                                      
         BNE   *+14                                                             
         MVC   DEMSTN(8),=C'JAN05/99'  THIS IS 1ST EXTRACT CUT-OFF DATE         
         MVI   DEMSTNH+5,8                                                      
*^^TEMP                                                                         
* The following 4 instructions are in place until the clients have              
*  latest version of DEM32, which uses "..." in the DATA field.                 
         CLI   DEMACT+5,C','                                                    
         BNE   *+14                                                             
         MVC   DEMSTN(8),=C'JAN29/99'  THIS IS LATST XTRCT CUT-OFF DATE         
         MVI   DEMSTNH+5,8                                                      
*^^EOTEMP                                                                       
                                                                                
         DS    0H                  CHECK FOR "LATEST" BY DDS TERMINAL           
         CLI   (TWAOFFC-TWAD)(R8),C'*'  IGNORE IF NOT DDS TERMINAL              
         BNE   ID32VERL                                                         
         CLI   DEMSTNH+5,6              IGNORE IF L(INPUT) <> 6                 
         BNE   ID32VERL                                                         
         CLC   =C'LATEST',DEMSTN                                                
         BNE   ID32VERL                                                         
         MVI   D32PCVER+0,XFF                                                   
         MVC   D32PCVER+1(L'D32PCVER-1),D32PCVER   SET TO ALL X'FF'             
         B     ID32VERX                                                         
ID32VERL EQU   *                                                                
*                                                                               
         GOTO1 VDATVAL,DMCB,DEMSTN,DUB                                          
         OC    DMCB+0(4),DMCB+0                                                 
         BZ    ID32XN                                                           
*                                                                               
*&&DO                                                                           
         GOTO1 VDATCON,DMCB,(0,DUB),(3,D32XTRCT),0                              
*&&                                                                             
         GOTO1 VDATCON,DMCB,(0,DUB),(3,D32PCVER+1),0                            
ID32VERX EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         OI    DEMFLAG1,DF1DEM32   DEM32 SESSION CONFIRMED!                     
         B     ID32XY                                                           
                                                                                
*                                                                               
ID32XN   DS    0H                                                               
         J     EXITL                                                            
*                                                                               
ID32XY   DS    0H                                                               
         J     EXITE                                                            
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  RB                                                               
***********************************************************************         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO EXECUTE I/O TO DEMO FILES.                               *         
*                                                                     *         
* NTRY - IOFLAG  CONTAINS COMMAND NUMBER, FILE/DIRECTORY NUMBER       *         
*                AND FILE TYPE (DIRECTORY/FILE).                      *         
*        KEY     CONTAINS KEY FOR READ                                *         
*        NDXDA   CONTAINS D/A (MAY BE ZERO FOR FILE I/O)              *         
*        AIOAREA CONTAINS ADDRESS OF OUTPUT RECORD                    *         
*                                                                     *         
* ALL COMMANDS (SEE CMNDTAB) ARE AVAILABLE FOR BOTH FILE AND          *         
* DIRECTORY. FOR FILE COMMANDS A DIRECTORY READ IS EXECUTED IF NDXDA  *         
* IS BINARY ZEROES. AS DMREAD FOR A DANDX FILE IS NOT SUPPORTED THIS  *         
* IS EMULATED. FOR DIRECTORY COMMANDS KEY IS ALWAYS SAVED IN KEYSAVE  *         
* BEFORE I/O IS EXECUTED, KEY CONTAINS ACTUAL RECORD AFTER I/O.       *         
*                                                                     *         
* EXIT - IOFLAG CONTAINS ERROR CONDITION (ZERO MEANS OK)              *         
*        USER'S IO AREA CONTAINS RECORD AFTER I/O                     *         
*        CC=EQUAL IF I/O SUCCESSFUL                                   *         
*        CC=LOW IF DISK ERROR                                         *         
*        CC=HIGH FOR END-OF-FILE & NOT FOUND                          *         
***********************************************************************         
         SPACE 1                                                                
IO       NMOD1 IOWORKX-IOWORKD,**IO**,CLEAR=YES                                 
         USING IOWORKD,RC          RC=A(LOCAL W/S)                              
         MVC   IOWORK1,IOFLAG      SAVE COMMAND FLAG                            
         MVC   IOWORK2,IOWORK1                                                  
         PACK  IOWORK3,IOWORK1     INVERT COMMAND FLAG                          
         NI    IOWORK2,X'07'       IOWORK2=FIL/DIR NUMBER                       
         ZIC   RE,IOWORK2                                                       
         LA    RF,L'FDTAB                                                       
         MR    RE,RE                                                            
         LA    RE,FDTAB-L'FDTAB(RF)                                             
IO2      MVC   IOFILE,0(RE)        SET FILE/DIR NAMES                           
         MVC   IODIR,7(RE)                                                      
         MVC   IOINDS,14(RE)       SET FILE/DIRECTORY INDICATORS                
         NI    IOWORK3,X'03'       IOWORK3=COMMAND NUMBER                       
         ZIC   RE,IOWORK3                                                       
         SLL   RE,3                                                             
         LA    RE,CMNDTAB-8(RE)                                                 
         MVC   IOCMND,0(RE)        SET COMMAND                                  
         TM    IOWORK1,DIR                                                      
         BZ    IO6                                                              
*                                  DIRECTORY I/O CALL                           
IO4      MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTO1 VDATAMGR,IOPARM,IOCMND,IODIR,KEY,AIOAREA                         
         L     RF,AIOAREA          EXTRACT D/A FROM RECORD                      
         SR    R0,R0                                                            
         ICM   R0,1,IODADSP        UNLESS DISPLACEMENT IS ZERO                  
         BZ    *+12                                                             
         AR    RF,R0               RF=A(D/A)                                    
         MVC   NDXDA,0(RF)                                                      
         MVC   IOFLAG,8(R1)        RETURN DATAMGR ERROR                         
         ZICM  RF,IODSDSP,(1)      EXTRACT STATUS BYTE FROM RECORD              
         BZ    *+14                 UNLESS DISPLACEMENT IS ZERO                 
         A     RF,AIOAREA           RF-->STATUS BYTE                            
         MVC   DSTATUS,0(RF)                                                    
         B     IOX                                                              
*                                  FILE CALL                                    
IO6      OC    NDXDA,NDXDA         TEST IF D/A PRESENT                          
         BNZ   IO8                                                              
         MVC   IOFLAG,IOWORK2                                                   
         OI    IOFLAG,DIR+READ     NO - DO DIRECTORY READ                       
***      BAL   RE,IO                                                            
         BAS   RE,IO                                                            
         BNE   IOX                                                              
         OC    NDXDA,NDXDA         TEST IF D/A PRESENT                          
         BNZ   IO8                                                              
         MVI   IOFLAG,NOTFOUND     NO - RETURN NOT FOUND                        
         B     IOX                                                              
*                                  NON-DANDX FILE FUDGES                        
IO8      CLI   IOFILTY,0                                                        
         BE    IO10                                                             
         CLI   IOFILTY,2           TEST DIRECTORY ONLY (NO FILE)                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   IOCMND,CMNDREAD     TEST DMREAD/FILE                             
         BNE   *+10                                                             
         MVC   IOCMND,CMNDGETR     YES - SET TO GETREC/FILE                     
         B     IO12                                                             
*                                  DANDX FILE FUDGES                            
IO10     L     RE,AIOAREA          BUILD KEY IN USER I/O AREA                   
         ZIC   RF,IOKEYLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),KEY         SET READ HIGH KEY IN IOAREA                  
                                                                                
         DS    0H                  SUPPORT FOR DEMDIRN/DEMDIRO MERGE            
***      CLI   IOWORK2,DEM          APPLICABLE FOR DEMfil ONLY                  
****     BNE   IO10M                                                            
         TM    IOWORK1,FIL          APPLICABLE FOR DemFIL ONLY                  
         BZ    IO10M                                                            
         CLC   IOCMND,CMNDRSEQ      NOT APPLICABLE FOR READ SEQUENTIAL          
         BE    IO10M                                                            
         ZICM  RF,IOFSDSP,(1)       GET DSPL OF FIL'S STATUS BYTE               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RF,RE                                                            
         MVC   0(1,RF),DSTATUS       AND MOVE STATUS BYTE IN FOR READ           
IO10M    EQU   *                                                                
                                                                                
         CLC   IOCMND,CMNDREAD     TEST IF READ COMMAND                         
         BNE   *+10                                                             
         MVC   IOCMND,CMNDRDHI     YES - SET COMMAND TO READ HIGH               
*                                  FILE I/O CALL                                
IO12     GOTO1 VDATAMGR,IOPARM,IOCMND,IOFILE,NDXDA,AIOAREA,IOWORK               
         MVC   IOFLAG,8(R1)        RETURN DATAMGR ERROR                         
         ZICM  RF,IOFSDSP,(1)      EXTRACT STATUS BYTE FROM RECORD              
         BZ    *+14                 UNLESS DISPLACEMENT IS ZERO                 
         A     RF,AIOAREA           RF-->STATUS BYTE                            
         MVC   FSTATUS,0(RF)                                                    
                                                                                
         CLI   IOFILTY,0           TEST IF A DANDX FILE                         
         BNE   IOX                 NO - EXIT                                    
         CLC   IOCMND,CMNDREAD     TEST IF READ COMMAND                         
         BNE   IOX                 NO - EXIT                                    
         MVI   IOFLAG,NOTFOUND                                                  
         CLI   8(R1),0                                                          
         BNE   IOX                                                              
         L     RE,AIOAREA          TEST RECORD FOUND                            
         ZIC   RF,IOKEYLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),KEY                                                      
         BNE   IOX                 NO - EXIT WITH NOT FOUND                     
         MVI   IOFLAG,0            RESET ERROR                                  
*                                  SET CC & RETURN                              
IOX      MVI   IOWORK1,1                                                        
         CLI   IOFLAG,0                                                         
         BE    IOXX                                                             
         MVI   IOWORK1,2                                                        
         TM    IOFLAG,EOF+NOTFOUND                                              
         BNZ   IOXX                                                             
         MVI   IOWORK1,0                                                        
IOXX     CLI   IOWORK1,1                                                        
         XIT1                                                                   
         DROP  RC                                                               
         EJECT                                                                  
* TABLE OF FILE/DIRECTORY NAMES FOR I/O PROCESSOR                               
FDTAB    DS    0XL19                                                            
         DC    C'DEMFIL DEMDIR ',AL1(0,20,19,22,18)                             
         DC    C'PAVFIL PAVDIR ',AL1(0,20,19,22,18)                             
         DC    C'SPTFIL SPTDIR ',AL1(1,13,14,15,13)                             
         DC    C'       STATION',AL1(2,00,00,00,17)                             
* TABLE OF VALID I/O COMMANDS FOR I/O PROCESSOR                                 
*                                                                               
CMNDTAB  DS    0CL8                                                             
CMNDRDHI DC    C'DMRDHI  '                                                      
CMNDRSEQ DC    C'DMRSEQ  '                                                      
CMNDREAD DC    C'DMREAD  '                                                      
CMNDGETR DC    C'GETREC  '                                                      
         SPACE 2                                                                
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
*                                                                               
DEM002M  NMOD1 0,*DEM002*,CLEAR=YES                                             
         MVC   SCREEN,SVSCREEN     SET SCREEN # OF CURRENT SCREEN               
         MVC   OVERLAY,SVOVRLAY     AND OVERLAY # OF CURRENT OVERLAY            
*        DROP  R8                                                               
                                                                                
         LA    R2,CORETAB          GET ADDRESSES OF CORERES FACILITIES          
         LA    R3,COREFACS                                                      
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
DEM2     CLI   0(R2),X'FF'                                                      
         BE    DEM3                                                             
         ICM   R0,1,0(R2)                                                       
         GOTO1 (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         B     DEM2                                                             
*                                                                               
DEM3     MVC   VMSPACK,VMSPACK2                                                 
         B     DEM4                                                             
         EJECT                                                                  
DEM4     DS    0H                                                               
*                                                                               
*                                                                               
         SR    RE,RE               SET ADDRESSES OF TABLES & ROUTINES           
         LA    R0,ADDRNUM                                                       
         LTR   R0,R0                                                            
         BZ    DEM4A                                                            
         L     R1,ADDRTAB(RE)                                                   
         A     R1,BRELO                                                         
         ST    R1,ADDRFACS(RE)                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
DEM4A    DS    0H                                                               
*        MVI   GOSTEON,SETP81#     CALL OVERLAY FOR T21B81 TABLES               
*        GOTO1 AGOSTEO                                                          
         GOTO1 VCALLOV,DMCB,(X'81',0),0                                         
         L     RF,DMCB                                                          
         USING PHSE81D,RF                                                       
         L     RE,VUPCASE                                                       
         S     RE,APHASE81                                                      
         AR    RE,RF                                                            
         ST    RE,AUPCSETB         MOVE IN ADDRESS FROM CALL OVERLAY            
         L     RE,VFMSTAB                                                       
         S     RE,APHASE81                                                      
         AR    RE,RF                                                            
         ST    RE,AFMSTAB          MOVE IN ADDRESS FROM CALL OVERLAY            
         L     RE,VSYSTAB                                                       
         S     RE,APHASE81                                                      
         AR    RE,RF                                                            
         ST    RE,ASYSTAB          MOVE IN ADDRESS FROM CALL OVERLAY            
         L     RE,VACTTAB                                                       
         S     RE,APHASE81                                                      
         AR    RE,RF                                                            
         ST    RE,AACTTAB          MOVE IN ADDRESS FROM CALL OVERLAY            
         L     RE,VFILTAB                                                       
         S     RE,APHASE81                                                      
         AR    RE,RF                                                            
         ST    RE,AFILTAB          MOVE IN ADDRESS FROM CALL OVERLAY            
         L     RE,VACTHELP                                                      
         S     RE,APHASE81                                                      
         AR    RE,RF                                                            
         ST    RE,AACTHELP         MOVE IN ADDRESS FROM CALL OVERLAY            
         L     RE,VACTNEXT                                                      
         S     RE,APHASE81                                                      
         AR    RE,RF                                                            
         ST    RE,AACTNEXT         MOVE IN ADDRESS FROM CALL OVERLAY            
         L     RE,VTABLEN                                                       
         S     RE,APHASE81                                                      
         AR    RE,RF                                                            
         ST    RE,ATABLEN          MOVE IN ADDRESS FROM CALL OVERLAY            
         L     RE,VLTABLEN                                                      
         ST    RE,ALTABLEN          MOVE IN ADDRESS FROM CALL OVERLAY           
         L     RE,VTABLENQ                                                      
         ST    RE,ATABLENQ         MOVE IN ADDRESS FROM CALL OVERLAY            
         L     RE,VTABLABL                                                      
         S     RE,APHASE81                                                      
         AR    RE,RF                                                            
         ST    RE,ATABLABL         MOVE IN ADDRESS FROM CALL OVERLAY            
         L     RE,VLTABLAB                                                      
         ST    RE,ALTABLAB         MOVE IN ADDRESS FROM CALL OVERLAY            
         L     RE,VTABLABQ                                                      
         ST    RE,ATABLABQ         MOVE IN ADDRESS FROM CALL OVERLAY            
*                                  TO ADDRESS IN DEDEMWRK, SO ADDRESSES         
*                                  SHOULD BEHAVE NORMALLY                       
         L     RF,=A(STTABADD-DEM00)                                            
         A     RF,ABASE                                                         
         LA    RE,STADDRS                                                       
DEM4AA   CLI   0(RF),EOT           AT END OF DISPLACEMENT TABLE?                
         BE    DEM5                 YES, STEREO ADDRESSES ALL SET               
         L     R1,ABASE            DETERMINE BASE ADDRESS TO                    
         CLI   0(RF),C'C'                                                       
         BE    DEM4AB                                                           
         L     R1,ATWA              USE W/ DISPLACEMENT                         
         CLI   0(RF),C'T'                                                       
         BE    DEM4AB                                                           
         L     R1,ATIA                                                          
         CLI   0(RF),C'I'                                                       
         BE    DEM4AB                                                           
         DC    H'0'                                                             
DEM4AB   SR    R0,R0                                                            
         ICM   R0,3,1(RF)          R0=DISPLACEMENT                              
         AR    R0,R1                                                            
         ST    R0,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,L'STTABADD(RF)                                                
         B     DEM4AA                                                           
*                                                                               
DEM5     DS    0H                  SET LENGTHS OF BIG AREAS                     
*****    L     R1,=A(TABLEN-DEM00)                                              
         L     R1,ATABLEN                                                       
*****    A     R1,ABASE                                                         
*****    LA    R0,TABLENQ                                                       
         L     R0,ATABLENQ                                                      
         SR    RE,RE                                                            
DEM5A_A  ICM   RE,3,0(R1)                                                       
         ZICM  RF,2(R1),(3)                                                     
         LA    RF,DEMWRKD(RF)                                                   
         STH   RE,0(RF)                                                         
*****    LA    R1,L'TABLEN(R1)                                                  
         L     RE,ALTABLEN                                                      
****     LA    R1,ALTABLEN(R1)                                                  
         AR    R1,RE                                                            
         BCT   R0,DEM5A_A                                                       
*                                                                               
         DS    0H                                                               
         MVI   GOSTEON,RTI#        RESTORE STUFF INTO TIA NOW                   
         GOTO1 AGOSTEO                                                          
*                                                                               
DEM5L    DS    0H                  SET LABELS (EYECATCHERS) FOR BUFFERS         
******   L     R1,=A(TABLABEL-DEM00)                                            
         L     R1,ATABLABL                                                      
****     A     R1,ABASE                                                         
*****    LA    R0,TABLABLQ                                                      
         L     R0,ATABLABQ                                                      
DEM5L_B  SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         LA    RE,DEMWRKD(RE)       RE-->A(BUFFER AREA)                         
         ICM   RF,15,0(RE)          RF-->BUFFER AREA                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SH    RF,=H'8'             BACK UP 8 BYTES FOR LABEL                   
         MVC   0(8,RF),2(R1)         AND MOVE LABEL IN                          
****     LA    R1,L'TABLABEL(R1)                                                
         L     RE,ALTABLAB                                                      
         AR    R1,RE                                                            
*****    LA    R1,ALTABLAB(R1)                                                  
         BCT   R0,DEM5L_B                                                       
*                                                                               
DEM6     L     R8,ATWA             EXTRACT VALUES FROM TWA                      
         USING TWAD,R8                                                          
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'                                                     
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAUSRID                                                  
         MVC   AGYALPH,TWAAGY                                                   
         USING DEMTWAD,R8                                                       
*                                                                               
         MVI   SPACES,C' '         INITIALIZE OTHER W/S FIELDS                  
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         SR    R1,R1               SET SPECIAL BOOK VALUES                      
         STCM  R1,3,LATBOOK                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,ESTBOOK                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,MBKBOOK                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,INVBOOK                                                     
         MVC   XTRA,SPACES                                                      
         MVI   SCANLNTH,10                                                      
         MVI   SCAN1HL,10          (BUMP-SCANNER-BLOCK RTN USES THIS)           
         L     R0,AIOAREA1         SET A(I/O AREA) FOR I/O ROUTINE              
         ST    R0,AIOAREA                                                       
*                                                                               
         XC    WORK,WORK           READ THE RESEARCH PROFILE                    
         MVC   WORK(4),=C'S0R0'                                                 
         MVC   WORK+4(2),AGYALPH                                                
         GOTO1 VGETPROF,DMCB,WORK,PROGPROF,VDATAMGR                             
*                                                                               
         LA    R1,DEMACTH                                                       
         ST    R1,ADEMACT                                                       
         LA    R1,DEMFILH                                                       
         ST    R1,ADEMFIL                                                       
         LA    R1,DEMSRCH                                                       
         ST    R1,ADEMSRC                                                       
         LA    R1,DEMSTNH                                                       
         ST    R1,ADEMSTN                                                       
         LA    R1,DEMBOKH                                                       
         ST    R1,ADEMBOK                                                       
         LA    R1,DEMDATH                                                       
         ST    R1,ADEMDAT                                                       
         LA    R1,DEMDEMH                                                       
         ST    R1,ADEMDEM                                                       
         LA    R1,DEMOPTH                                                       
         ST    R1,ADEMOPT                                                       
*                                                                               
         DS    0H                                                               
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         MVC   TSACOM,AFAC                                                      
         DROP  R1                                                               
*                                                                               
DEM002X  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
* TABLE OF CORERES PHASES THAT ROOT SUPPLIES ADDRESSES FOR IN COREFACS          
CORETAB  DC    X'0003220E11121326E014211B1C5D'                                  
         DC    AL1(QFALINK)                                                     
         DC    AL1(QREGTIUN)                                                    
         DC    AL1(QREDEMUP)                                                    
         DC    AL1(QDAYUNPK)                                                    
         DC    AL1(QSTAPACK)                                                    
         DC    AL1(QGETBROD)                                                    
         DC    X'FF'                                                            
         SPACE 1                                                                
*                                                                               
ADDRTAB  DS    0F                                                               
****     DC    A(SYSTAB)                                                        
         DC    A(0)                ZERO OUT TO BE FILLED IN LATER               
****     DC    A(ACTTAB)                                                        
         DC    A(0)                                                             
****     DC    A(FILTAB)                                                        
         DC    A(0)                ZERO OUT TO BE FILLED IN  LATER              
****     DC    A(FMSTAB)                                                        
         DC    A(0)                ZERO OUT TO BE FILLED IN LATER               
         DC    A(OPTTAB)                                                        
                                                                                
         DC    A(DEM32ADD)         INTFC TO FLNK ADDDATA RTN (ADM32ADD)         
         DC    A(DEM32SET)         INTFC TO FLNK SETELEM RTN (ADM32SET)         
         DC    A(D32TBK)           DEM32'S TRANSLATE BOOK ROUTINE               
         DC    A(GOSTEO)           INTFC TO ERR-HNDLING RTNS (AERRITFC)         
         DC    A(ERROR0)                                                        
         DC    A(FAMAP)            MAP TABLE FOR FALINK (AFAMAP)                
         DC    A(0)                                                             
         DC    A(IO)                                                            
         DC    A(IOCOUNT)          I/O COUNT CHECK ROUTINE (AIOCHECK)           
         DC    A(MYSCAN)           MY SCANNER ROUTINE                           
         DC    A(POST)                                                          
         DC    A(PRINT)            PRINT (REPORT) ROUTINE                       
         DC    A(0)                                                             
****     DC    A(UPCASETB)         UPPER CASE TABLE (AUPCSETB)                  
         DC    A(0)                UPPER CASE TABLE ZERO OUT                    
         DC    A(SETUSRID)         SET BINARY USER ID IN DBEXTEND               
         DC    A(0)                A(USERID DBEXTEND LINK)                      
         DC    A(STACODE)          A(STACODE) TABLE                             
         DC    A(TRANSBT)          A(TRANSBT) TRANSALTE BOOKTYPES               
         DC    2A(0)                --SPARE--                                   
***      DC    3A(0)                --SPARE--                                   
                                                                                
         DC    V(BINSRCH)                                                       
         DC    V(MEDGET)                                                        
         DC    V(NSIWEEK)                                                       
         DC    V(TWABLD)                                                        
         DC    V(NETWEEK)                                                       
         DC    V(NETUNBK)                                                       
*                                                                               
         DC    A(0)                    ACTHELP                                  
         DC    A(0)                    ACTNEXT                                  
         DC    A(0)                    TABLEN                                   
         DC    A(0)                    L'TABLEN                                 
         DC    A(0)                    TABLENQ                                  
         DC    A(0)                    TABLABEL                                 
         DC    A(0)                    L'TABLABEL                               
         DC    A(0)                    TABLABLQ                                 
*                                                                               
         DC    2A(0)                --SPARE--                                   
ADDRNUM  EQU   (*-ADDRTAB)/4                                                    
         SPACE 1                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE SYSTEM                                                               
VALSYS   NMOD1 0,*VALSYS*,CLEAR=YES                                             
         MVI   HELPREQD,HELPACT                                                 
         L     R2,ASYSTAB                                                       
         USING SYSTABD,R2          R2=A(SYSTEM TABLE)                           
*                                  CALL GETFACT TO GET SYSTEM                   
         GOTO1 VGETFACT,DMCB,0                                                  
         L     R1,0(R1)            R1=A(FAFACTS)                                
         MVC   TODAYB,FADATEB-FACTSD(R1)                                        
         MVC   DUB(1),FAOVSYS-FACTSD(R1)                                        
VALSYS2  CLI   SYSNAME,EOT         TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SYSOVSYS,DUB        MATCH ON OVERLAY SYSTEM NUMBER               
         BE    *+12                                                             
         LA    R2,SYSTABL(R2)                                                   
         B     VALSYS2                                                          
         ST    R2,ASYSNTRY         SET A(SYSTEM TABLE ENTRY)                    
         MVC   ACTV,SYSACTI                                                     
         MVC   FILV,SYSFILI                                                     
         MVC   SRCV,SYSSRCI                                                     
         MVC   OPTV,SYSOPTR                                                     
VALSYSX  XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* initialize the falink block and set the addr const to point to dummy          
* fields                                                                        
***********************************************************************         
INIFALNK NMOD1 0,*INIFLK*,CLEAR=YES                                             
         LA    R0,FABLK                                                         
         LA    R1,FALINKDL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
                                                                                
         LA    R2,FABLK                                                         
*        ST    R2,AFABLK           FOR OTHER OVERLAYS                           
         USING FALINKD,R2                                                       
         LA    R1,FLNKSCRS         SET A(FIRST SCREEN POSITION)                 
         ST    R1,FALABLD                                                       
         MVC   FALTBLD,VTWABLD         A(TWABLD)                                
         L     R1,AFAC                 A(SWITCH)                                
         L     R1,CSWITCH-COMFACSD(R1)                                          
         ST    R1,FALASWCH                                                      
         LA    R1,*                                                             
         A     R1,=A(RECEIVE-(*-4))    A(MY RECEIVE ROUTINE)                    
         ST    R1,FALARCV                                                       
         LA    R1,*                                                             
         A     R1,=A(SEND-(*-4))       A(MY SEND ROUTINE)                       
         ST    R1,FALASND                                                       
*                                                                               
         LA    R1,*                                                             
         A     R1,=A(BREAK-(*-4))      A(BREAK ROUTINE)                         
         ST    R1,FALASTP                                                       
         LA    R1,*                                                             
         A     R1,=A(RESUME-(*-4))     A(RESUME ROUTINE)                        
         ST    R1,FALARSM                                                       
         LA    R1,*                                                             
         A     R1,=A(FLNKOVFL-(*-4))   A(FALINK BUFFR OVERFLOW ROUTINE)         
         ST    R1,FALAFUL                                                       
*                                                                               
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R1,FAMSGBLK             A(MESSAGE BLOCK)                         
         ST    R1,FALAMSG                                                       
         LA    R1,FACON                A(CONTROL FIELD BUFFER)                  
         ST    R1,FALACON                                                       
         L     R1,ATWA                                                          
         AH    R1,=Y(SVFALINK-DEMTWAD) A(FALINK SAVED STORAGE)                  
         ST    R1,FALASVE                                                       
         LA    R1,*                                                             
*        A     R1,=A(FAMAP-(*-4))      A(MAP TABLE)                             
*        ST    R1,FALAMAP                                                       
         MVC   FALAMAP,AFAMAP                                                   
         MVC   FALAPGS,TWAPGS                                                   
* set addr constants to point to the dummy fields                               
*                                                                               
         LA    R1,DUMACTH                                                       
         MVI   0(R1),16                SET FIELD LENGTH                         
         ST    R1,ADEMACT                                                       
         LA    R1,DUMFILH                                                       
         MVI   0(R1),11                SET FIELD LENGTH                         
         ST    R1,ADEMFIL                                                       
         LA    R1,DUMSRCH                                                       
         MVI   0(R1),11                SET FIELD LENGTH                         
         ST    R1,ADEMSRC                                                       
         LA    R1,DUMSTNH                                                       
         MVI   0(R1),32                SET FIELD LENGTH                         
         ST    R1,ADEMSTN                                                       
         LA    R1,DUMBOKH                                                       
         MVI   0(R1),38                SET FIELD LENGTH                         
         ST    R1,ADEMBOK                                                       
         LA    R1,DUMDATH                                                       
         MVI   0(R1),L'DUMDATH+L'DUMDAT  SET FIELD LENGTH                       
         ST    R1,ADEMDAT                                                       
         LA    R1,DUMDEMH                                                       
         MVI   0(R1),77                SET FIELD LENGTH                         
         ST    R1,ADEMDEM                                                       
         LA    R1,DUMOPTH                                                       
         MVI   0(R1),77                SET FIELD LENGTH                         
         ST    R1,ADEMOPT                                                       
*                                                                               
INIFX    XIT1                                                                   
*                                                                               
TWAPGS   DC    AL4(FALATMS)                                                     
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  R2,RB,R8,R9                                                      
         EJECT                                                                  
***********************************************************************         
*======================= PRINT (REPORT) ROUTINE ======================*         
                                                                                
***********************************************************************         
* ROUTINE TO PRINT UP TO THREE LINES OF DATA                          *         
*                                                                     *         
* NTRY - R1=PRINT CONTROL AS FOLLOWS                                  *         
*                                                                     *         
*        0=INITIALIZE REPORT                                          *         
*        1=PRINT DATA                                                 *         
*        2=CLOSE REPORT                                               *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
PRINT    NMOD1 PRTWRKX-PRTWRKD,**PRINT*,CLEAR=YES                               
                                                                                
         USING PRTWRKD,RC          RC=A(PRINT WORK AREA)                        
         USING DEMTWAD,R8                                                       
         USING DEMWRKD,R9                                                       
                                                                                
         XC    PLINE,PLINE                                                      
         LA    R1,PRINTTAB(R1)     R2=A(PRINT CONTROL)                          
         MVC   PCTRL,0(R1)                                                      
         MVC   PSAVE,PCTRL                                                      
         CLI   PCTRL,X'FF'         TEST LAST TIME CALL                          
         BNE   *+12                                                             
***      BAL   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
         B     PRINTX                                                           
*                                                                               
         CLI   PCTRL,0             TEST INITIALIZE CALL                         
         BNE   PRINT2                                                           
         USING PQPLD,RC                                                         
         ZAP   REPLN,=P'60'        SET LINE GR MAX                              
         MVC   PLDESC(5),=C'$DEM/'                                              
         MVC   PLDESC+5(3),OPTPRNT                                              
         MVC   PLSUBID(3),OPTPRNT                                               
         MVI   PLCLASS,C'Z'        OUTPUT CLASS=Z                               
***      BAL   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
         MVC   REPNO,PLREPNO       SAVE REPORT NUMBER IN W/S                    
         B     PRINTX                                                           
*                                  HANDLE PAGE OVERFLOW                         
         USING PRTWRKD,RC          RC=A(PRINT WORK AREA)                        
PRINT2   LA    R3,LINE1                                                         
         LA    R4,3                                                             
         ZAP   DUB,REPLN                                                        
PRINT4   CLC   0(L'LINE1,R3),SPACES                                             
         BE    *+10                                                             
         AP    DUB,=P'1'                                                        
         LA    R3,L'LINE1(R3)                                                   
         BCT   R4,PRINT4                                                        
*                                                                               
         CP    DUB,=P'60'          TEST LINES WILL FIT ON PAGE                  
         BNH   PRINT10                                                          
         ZAP   REPLN,=P'0'         NO - SKIP TO A NEW PAGE                      
         MVC   PSAVE,PCTRL                                                      
         MVI   PCTRL,X'89'                                                      
         MVC   PLINE,SPACES                                                     
****     BAL   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
*                                  PRINT OPTIONS (IF ANY)                       
         CLI   DEMOPTH+5,0         TEST ANY OPTIONS INPUT                       
         BE    PRINT6                                                           
         MVC   PLINE(8),=C'OPTIONS '                                            
         ZIC   R1,DEMOPTH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLINE+8(0),DEMOPT                                                
         MVI   PCTRL,X'11'         PRINT & SPACE A LINE                         
****     BAL   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
PRINT6   MVI   PCTRL,X'09'         PRINT HEADLINE DATA                          
         LA    R3,DEMHD1                                                        
         LA    R4,3                                                             
PRINT8   MVC   PLINE,SPACES                                                     
         MVC   PLINE(L'DEMHD1),0(R3)                                            
****     BAL   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
         LA    R3,L'DEMHD1H+L'DEMHD1(R3)                                        
         BCT   R4,PRINT8                                                        
         MVC   PLINE,SPACES        PRINT SPACE LINE AFTER HEADINGS              
****     BAL   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
PRINT10  LA    R3,LINE1            PRINT ACTUAL REPORT LINES                    
         LA    R4,3                                                             
         MVC   PCTRL,PSAVE                                                      
PRINT12  MVC   PLINE,0(R3)                                                      
         CLC   PLINE,SPACES        DON'T PRINT BLANK LINES                      
         BE    *+8                                                              
****     BAL   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
         LA    R3,L'LINE1(R3)                                                   
         BCT   R4,PRINT12                                                       
         B     PRINTX                                                           
*                                                                               
PRINTIT  LR    R0,RE               INTERFACE TO DATAMGR                         
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',=C'PRTQUE',0,PCTRL,ATIA                
         CLI   8(R1),0             TEST FOR ERRORS                              
         BE    *+14                                                             
         XC    OPTPRNT,OPTPRNT     YES - STOP BEING CALLED                      
         B     PRINTX                                                           
         AP    REPLN,=P'1'         BUMP LINE COUNT                              
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RC                                                               
         SPACE 1                                                                
PRINTX   DS    0H                                                               
         XIT1                                                                   
                                                                                
PRINTTAB DC    X'0009FFFF'                                                      
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  R8,R9,RB                                                         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=================== MORE OPTIONS' DATA VALIDATION ===================*         
                                                                                
* At entry to these routines,                                                   
*  R3-->OPTTABD,                                                                
*  R8-->DEMTWAD,                                                                
*  R9-->DEMWRKD.                                                                
                                                                                
         USING OPTTABD,R3                                                       
         USING DEMTWAD,R8                                                       
         USING DEMWRKD,R9                                                       
                                                                                
*----------------------- VALIDATE  BEST  OPTION ----------------------*         
         DS    0D                  FORCE DBLE-WRD BNDRY FOR NMOD                
         ORG   *+4                                                              
VALBEST  DC    CL60'**option specified by keyword--no values needed**'          
         NMOD1 0,**OPTV**                                                       
         ZIC   R1,FLDH+7                                                        
         MVI   WORK,C'B'                                                        
         EXCLC R1,FLD,=C'BEST'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
                                                                                
*----------------------- VALIDATE  NOR  OPTION -----------------------*         
         DS    0D                  FORCE DBLE-WRD BNDRY FOR NMOD                
         ORG   *+4                                                              
VALNOR   DC    CL60'**option specified by keyword--no values needed**'          
         NMOD1 0,**OPTV**                                                       
         ZIC   R1,FLDH+7                                                        
         MVI   WORK,C'N'                                                        
         EXCLC R1,FLD,=C'NOR'                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
                                                                                
*----------------------- VALIDATE  WKV  OPTION -----------------------*         
         DS    0D                  FORCE DBLE-WRD BNDRY FOR NMOD                
         ORG   *+4                                                              
VALWKV   DC    CL60'**option specified by keyword--no values needed**'          
         NMOD1 0,**OPTV**                                                       
         ZIC   R1,FLDH+7                                                        
*&&DO                                                                           
         EXCLC R1,FLD,=C'WEEKVERT'                                              
         BE    VALWKV10                                                         
         EXCLC R1,FLD,=C'WKV'                                                   
*&&                                                                             
         EXCLC R1,FLD,=C'WKVERT'                                                
         BE    VALWKV10                                                         
         DC    H'0'                                                             
*                                                                               
VALWKV10 DS    0H                                                               
         MVI   WORK,C'Y'                                                        
         NI    MISCFLG1,XFF-MF1WKACR                                            
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
                                                                                
*----------------------- VALIDATE  MBK  OPTION -----------------------*         
         DS    0D                  FORCE DBLE-WRD BNDRY FOR NMOD                
         ORG   *+4                                                              
VALMBK   DC    CL60'book1(/book2/book3/book4)--no ''/'' within book'            
         NMOD1 0,**OPTV**                                                       
* code to support multibook average for weekly                                  
*                                                                               
*                                                                               
         CLC   =C'OTP',FILE                                                     
         BE    VALMBK60                                                         
         CLC   =C'OPA',FILE                                                     
         BE    VALMBK60                                                         
         CLC   =C'WTP',FILE                                                     
         BE    VALMBK60                                                         
         CLC   =C'LPM',FILE                                                     
         BE    VALMBK60                                                         
*                                                                               
*                                                                               
         L     R0,AIOAREA2                                                      
         GOTO1 VSCANNER,DMCB,(0,FLDH),(R0),C',=/-'                              
         MVC   SCANDLIM,DMCB+8                                                  
         ZICM  R0,DMCB+4,(1)                                                    
         BNZ   *+14                                                             
         MVC   EADDR+2(2),=Y(EIIF-DEM00)                                        
         B     VALMBKXN                                                         
         CLI   DMCB+4,13           magic number is 13 (max)                     
         BNH   VALMBK07X                                                        
         MVC   EADDR+2(2),=Y(ETMBK-DEM00)                                       
         MVC   XTRA(8),=C'max is 13'                                            
         MVI   FNDX,0               DON'T DISPLAY MULTIPLE FLD INFO             
         B     VALMBKXN                                                         
VALMBK07X EQU  *                                                                
         STC   R0,BYTE#2           HOLD ONTO # OF BOOKS                         
*                                                                               
         DS    0H                  BUILD DUMMY FIELD FOR BOOKVAL                
         XC    FLDH(L'FLDH+L'FLD),FLDH                                          
         LA    RE,FLD                                                           
*        LA    RF,FULL                                                          
         LA    RF,WORK2                                                         
         L     R2,AIOAREA2                                                      
         MVC   EADDR+2(2),=Y(EIBK-DEM00)  ASSUME INVALID BOOK ERROR             
         B     VALMBK25                                                         
*                                                                               
VALMBK20 DS    0H                                                               
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
                                                                                
VALMBK25 DS    0H                                                               
         ZICM  R1,0(R2),(1)                                                     
         BZ    VALMBKXN                                                         
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),12(R2)     MOVE BOOK INTO DUMMY FIELD                   
         LA    RE,1(RE,R1)                                                      
                                                                                
         DS    0H                                                               
         MVC   0(1,RF),8+3(R2)     GET WEEK NUMBER IN BINARY                    
         CLI   1(R2),0                                                          
         BE    VALMBK30                                                         
         BC    15,VALMBKXN         INVALIDATE WEEK NUMBER FOR NOW               
         CLI   1(R2),1              WEEK IS ONLY ONE BYTE LONG                  
         BH    VALMBKXN                                                         
         CLI   22(R2),C'1'           AND MUST BE BETWEEN 1                      
         BL    VALMBKXN                                                         
         CLI   22(R2),C'4'           AND 4 INCLUSIVE                            
         BH    VALMBKXN                                                         
                                                                                
VALMBK30 DS    0H                                                               
         LA    RF,1(RF)                                                         
         LA    R2,32(R2)                                                        
         BCT   R0,VALMBK20                                                      
                                                                                
         LA    R0,FLD                                                           
         SR    RE,R0                                                            
         STC   RE,FLDH+5           DATA LENGTH                                  
         LA    RE,8(RE)                                                         
         STC   RE,FLDH             DUMMY TWA FIELD LENGTH                       
                                                                                
*                                                                               
         DS    0H                  VALIDATE BOOKS                               
                                                                                
*        XC    FULL2,FULL2                                                      
         XC    DUB(16),DUB         NEED TO EXPAND THIS FOR 13-MBK               
*                                                                               
         L     R0,AIOAREA2                                                      
* CODE FIX TO FIX NHT SOURCE VALIDATION                                         
         XC    DMCB,DMCB                                                        
         LA    RE,FLDH                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB(1),DBSRC                                                    
         CLI   DBSRC,C'H'                   VALIDATE AS NIELSEN                 
         BNE   *+8                                                              
         MVI   DMCB,C'N'                                                        
                                                                                
         GOTO1 VBOOKVAL,DMCB,,(13,(R0)),(C'B',VSCANNER),DUB,(C'C',AFAC)         
                                                                                
***      GOTO1 VBOOKVAL,DMCB,(DBSRC,FLDH),(4,(R0)),(C'B',VSCANNER),             
***            FULL2                                                            
         CLC   BYTE#2,DMCB+4       MAKE SURE # OF BOOKS ARE THE SAME            
         BE    *+14                                                             
         MVC   EADDR+2(2),=Y(EIIF-DEM00)                                        
         B     VALMBKXN                                                         
*                                                                               
         ZIC   R0,BYTE#2                                                        
         XC    WORK(OPTMBKL),WORK                                               
         L     R2,AIOAREA2                                                      
*        LA    R4,FULL             R4-->WEEK NUMBERS                            
         LA    R4,WORK2            R4-->WEEK NUMBERS                            
         LA    RE,DUB              RE-->BOOK TYPES                              
         LA    RF,WORK                                                          
                                                                                
VALMBK55 DS    0H                  CHECK BOOK TYPES                             
         CLI   0(RE),0                                                          
         BE    *+28                                                             
         CLI   OPTMBKBT,0                                                       
         BNE   *+10                                                             
         MVC   OPTMBKBT,0(RE)                                                   
         CLC   OPTMBKBT,0(RE)                                                   
         BNE   VALMBKXN                                                         
                                                                                
         DS    0H                  CHECK THE BOOK                               
         TM    0(R2),X'AE'          NO FUNNY BOOKS                              
         BNZ   VALMBKXN                                                         
         MVC   0(2,RF),1(R2)                                                    
         ZICM  R1,0(R4),(1)         ANY WEEK NUMBERS?                           
         BZ    *+18                                                             
         SLL   R1,4                  YES, SHIFT TO HIGH NIBBLE,                 
         STC   R1,BYTE                                                          
         OC    1(1,RF),BYTE          AND  OR  IT TO MONTH BYTE                  
                                                                                
         LA    R2,3(R2)                                                         
         LA    R4,1(R4)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,L'OPTMBK(RF)                                                  
         BCT   R0,VALMBK55                                                      
         B     VALMBKXY                                                         
                                                                                
***************************************************                             
*  CODE TO VALIDATE WEEKLY MULTIBKS                                             
*                                                                               
*   FIRST PARSE OUT WITH SEPERATOR '+'                                          
VALMBK60 L     R0,AIOAREA2                                                      
         GOTO1 VSCANNER,DMCB,(0,FLDH),(R0),C',=+'                               
         MVC   SCANDLIM,DMCB+8                                                  
         ZICM  R0,DMCB+4,(1)                                                    
         BNZ   *+14                                                             
VALMBK61 MVC   EADDR+2(2),=Y(EIIF-DEM00)                                        
         B     VALMBKXN                                                         
* ONLY ALLOW 4 "+" COMPONENTS                                                   
         CLI   DMCB+4,4                                                         
         BNH   VLMBK61A                                                         
         MVC   EADDR+2(2),=Y(ETMBK-DEM00)                                       
         MVC   XTRA(8),=C'max is 4'                                             
         MVI   FNDX,0               DON'T DISPLAY MULTIPLE FLD INFO             
         B     VALMBKXN                                                         
*                                                                               
VLMBK61A STC   R0,BYTE#2           HOLD ONTO # OF BOOKS                         
                                                                                
         LA    R4,WORK                                                          
         XC    WORK(OPTMBKL),WORK                                               
*  MOVE EACH COMPNENT TO NEXT SCANNER CALL TO PARSE                             
         L     R3,AIOAREA2                                                      
VALMBK62 XC    WORK2,WORK2                                                      
         USING SCANBLKD,R3                                                      
         XC    WORK2(30),WORK2                                                  
         ZIC   RE,SC1STLEN                                                      
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+8(0),SC1STFLD                                              
         MVC   WORK2+5(1),SC1STLEN                                              
         ZIC   RE,SC1STLEN                                                      
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+100(L'SC1STFLD),SC1STFLD                                   
         DROP  R4                                                               
*   NOW PARSE OUT EACH COMPONENT WITH '-' TO FIND RANGE                         
                                                                                
         L     R0,AIOAREA1                                                      
         GOTO1 VSCANNER,DMCB,(0,WORK2),(R0),C',=-'                              
         L     R5,AIOAREA1                                                      
RANGED   USING SCANBLKD,R5                                                      
         CLI   RANGED.SC1STLEN,12                                               
         BH    VALMBK61                                                         
         MVC   SCANDLIM,DMCB+8                                                  
         MVI   COUNTER2,1                 NO RANGE                              
         ZICM  R0,DMCB+4,(1)                                                    
         BZ    VALMBK76                                                         
         CLI   DMCB+4,2                   CANT HAVE MORE THAN 2 ITEMS           
         BNH   *+14                       IN START-END RANGE                    
         MVC   EADDR+2(2),=Y(ERANGE1-DEM00)                                     
         B     VALMBKXN                                                         
         MVI   RANGEFLG,X'00'             DEFAULT TO NOT RANGE BOOK             
         XC    WORK2+100(30),WORK2+100                                          
         MVC   WORK2+100(L'SC1STFLD),RANGED.SC1STFLD                            
         CHI   R0,1                                                             
         BE    VALMBK76                   NO RANGE                              
         MVI   COUNTER2,2                                                       
VALMBK68 XC    WORK2+100(30),WORK2+100                                          
         MVC   WORK2+100(L'SC1STFLD),RANGED.SC1STFLD                            
         CLI   COUNTER2,2                  START BOOK RANGE                     
         BNE   *+8                                                              
         MVI   RANGEFLG,X'80'                                                   
         CLI   COUNTER2,1                  END BOOK RANGE                       
         BNE   *+8                                                              
         MVI   RANGEFLG,X'40'                                                   
                                                                                
VALMBK76 GOTO1 VDATVAL,DMCB,(0,WORK2+100),DUB2                                  
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+14                                                             
VALMBK78 MVC   EADDR+2(2),=Y(EIIFBK-DEM00)                                      
         B     VALMBKXN                                                         
                                                                                
*                                                                               
         XC    DMCB(6*4),DMCB                                                   
         MVI   BYTE,0                 SET START DAY TO DEFAULT                  
         CLI   DBMED,C'O'             OVERNIGHTS                                
         BE    *+8                                                              
         CLI   DBMED,C'C'                                                       
         BNE   *+8                                                              
         MVI   BYTE,1                 DEFAULT TO MONDAY                         
         GOTO1 VNSIWEEK,DMCB,DUB2,(BYTE,VGETDAY),VADDAY,VDATCON                 
         MVC   1(1,R4),0(R1)          WEEK                                      
         MVC   0(1,R4),4(R1)          YEAR                                      
         CLI   RANGEFLG,X'80'         SAVE THE START BOOK OF RANGE              
         BNE   *+10                                                             
         MVC   HALF2,0(R4)                                                      
         CLI   RANGEFLG,X'40'         SAVE THE START BOOK OF RANGE              
         BNE   VALMBK84                                                         
         CLC   0(2,R4),HALF2         COMPARE END TO  START RANGE                
         BH    VALMBK80              MUST BE HIGHER                             
         MVC   EADDR+2(2),=Y(ERANGE2-DEM00)                                     
         B     VALMBKXN                                                         
VALMBK80 XC    HALF2,HALF2                                                      
VALMBK84 OC    1(1,R4),RANGEFLG      X'80' START X'40 END RANGE                 
VALMBK86 LA    R4,L'OPTMBK(R4)       PUT RANGE STATUS IN HIGH ORDER             
         ZIC   RE,COUNTER2           NIBBLE OF THE WEEKS                        
         SHI   RE,1                                                             
         STC   RE,COUNTER2                                                      
*                                                                               
         LA    RE,WORK+OPTMBKL                                                  
         CR    R4,RE                                                            
         BNL   VALMBK78                                                         
*                                                                               
         CLI   COUNTER2,0                                                       
         BNH   VALMBK90                                                         
         AHI   R5,L'SCLINE                                                      
*                                    COMPARE MAX NUMBER OF BOOKS                
                                                                                
         B     VALMBK68               NEXT SCANNER CHUNK                        
                                                                                
VALMBK90 ZIC   RE,BYTE#2                                                        
         SHI   RE,1                                                             
         STC   RE,BYTE#2                                                        
*                                                                               
         LA    RE,WORK+OPTMBKL                                                  
         CR    R4,RE                                                            
         BNL   VALMBK78                                                         
*                                                                               
         CLI   BYTE#2,0                                                         
         BNH   VALMBKXY                                                         
         AHI   R3,L'SCLINE                                                      
                                                                                
                                                                                
         B     VALMBK62                                                         
***************************************************                             
VALMBKXY DS    0H                  CLEAN EXIT                                   
         XC    EADDR,EADDR                                                      
         B     VALMBKX                                                          
                                                                                
VALMBKXN DS    0H                  EXIT WITH ERROR                              
         L     RF,EADDR                                                         
         A     RF,ABASE                                                         
         ST    RF,EADDR             SET ERROR "ROUTINE"                         
         B     VALMBKX                                                          
                                                                                
VALMBKX  DS    0H                                                               
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
COUNTER2 DS    X                                                                
RANGEFLG DS    X                                                                
*--------------------- VALIDATE IN MKT SHR OPTION --------------------*         
         DS    0D                  FORCE DBLE-WRD BNDRY FOR NMOD                
         ORG   *+4                                                              
VALIMS   DC    CL60'affl1(/affl2/affl3/affl4)'                                  
         NMOD1 0,**OPTV**                                                       
         L     R0,AIOAREA2                                                      
         MVC   EADDR+2(2),=Y(EIIF-DEM00)                                        
         GOTO1 VSCANNER,DMCB,(0,FLDH),(11,(R0)),C',=/-'                         
         MVC   SCANDLIM,DMCB+8                                                  
         SR    R0,R0                                                            
         IC    R0,DMCB+4                                                        
         STC   R0,BYTE#2           HOLD ONTO # OF AFFILIATES                    
         CLI   DMCB+4,0            MUST BE AT LEAST ONE                         
         BE    VALIMSXN                                                         
         CLI   DMCB+4,10           EXCEEDING MAX ALLOWED?                       
         BNH   *+14                                                             
         MVC   EADDR+2(2),=Y(ETMI-DEM00)                                        
         B     VALIMSXN                                                         
         L     R2,AIOAREA2                                                      
         XC    WORK(OPTIMSL),WORK                                               
                                                                                
VALIMS25 DS    0H                                                               
         LA    RE,DUB                                                           
         XC    DUB(5),DUB                                                       
         CLI   0(R2),5             MAX L'AFFIL = 5                              
         BH    VALIMSXN                                                         
         CLI   1(R2),0             SPLIT DATA INVALID                           
         BNE   VALIMSXN                                                         
         ZICM  R1,0(R2),(1)                                                     
         BZ    VALIMSXN                                                         
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),12(R2)     MOVE AFFILIATE                               
         OC    0(5,RE),SPACES                                                   
*                                                                               
         LA    RE,WORK             SLOT AFFL IN TABLE, PROTECT                  
         MVC   EADDR+2(2),=Y(EDIF-DEM00)                                        
VALIMS30 OC    0(5,RE),0(RE)       AGAINST DUPLICATES                           
         BZ    VALIMS35                                                         
         CLC   DUB(5),0(RE)                                                     
         BE    VALIMSXN                                                         
         LA    RE,5(RE)                                                         
         B     VALIMS30                                                         
*                                                                               
VALIMS35 MVC   0(5,RE),DUB         SLOT AFFL AND BUMP TO NEXT SCAN BLK          
         MVC   EADDR+2(2),=Y(EIIF-DEM00)                                        
         LA    R2,32(R2)                                                        
         BCT   R0,VALIMS25                                                      
*                                                                               
VALIMSXY DS    0H                  CLEAN EXIT                                   
         XC    EADDR,EADDR                                                      
         B     VALIMSX                                                          
                                                                                
VALIMSXN DS    0H                  EXIT WITH ERROR                              
         L     RF,EADDR                                                         
         A     RF,ABASE                                                         
         ST    RF,EADDR             SET ERROR "ROUTINE"                         
         B     VALIMSX                                                          
                                                                                
VALIMSX  DS    0H                                                               
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
         DROP  R3,R8,R9                                                         
         EJECT                                                                  
***********************************************************************         
*============================= I/O COUNT =============================*         
                                                                                
* Checks the number of I/Os done for this transaction.  The idea is             
*  that if we are near the I/O count limit, then we should break this           
*  transaction and resume with a new one.                                       
* At exit,                                                                      
*   CC returned HIGH ==> not near I/O count limit                               
*   CC returned NOT HIGH ==> near I/O count limit                               
                                                                                
IOCOUNT  NMOD1 0,*IOCNT**                                                       
         USING DEMWRKD,R9                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         L     RF,ASYSFAC          GET ADDRESS OF SSB                           
         L     RF,YVSSB-YSYSFACD(RF)                                            
         USING SSBD,RF                                                          
         L     RE,ATCB                                                          
         USING TCBD,RE                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SSBMAXIO                                                    
         AHI   R0,-500                                                          
         CLC   =C'C$',AGYALPH       SET TO MAX IOBOUND                          
         BE    *+10                 SECONDARY 10BOUND = 50K                     
         CLC   =C'SJ',AGYALPH       SET TO MAX IOBOUND                          
         BNE   *+8                  SECONDARY 10BOUND = 50K                     
         L     R0,=F'50000'         ONLY FOR CAMPBELL MITHUM                    
         CLM   R0,7,TCBIOCNT                                                    
         DROP  RE,RF                                                            
*                                                                               
         J     EXIT                                                             
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  RB                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================= DEM32 TRANSLATE BOOK ======================*         
                                                                                
* Translates book into a printable format for DEM32                             
* At entry,                                                                     
*   DUB+0(2) = book                                                             
* At exit,                                                                      
*   WORK     = formatted book                                                   
*   HALF     = length of formatted book                                         
                                                                                
D32TBK   NMOD1 0,*D32TBK*                                                       
         USING DEMWRKD,R9                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   WORK,SPACES                                                      
         LA    R4,WORK                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   BYTE,0              USE DEFAULT START DAY IN NSIWEEK             
         CLI   DBMED,C'W'                                                       
         BE    TBK060                                                           
         CLI   DBMED,C'O'          overnight TP also format as weekly           
         BE    TBK060                                                           
*                                                                               
         DS    0H                                                               
*&&DO                                                                           
         MVI   BYTE,0              USE DEFAULT START DAY IN NSIWEEK             
*&&                                                                             
         CLI   DBMED,C'C'           CHECK FOR CANADIEN                          
         BNE   TBK053X                                                          
         CLI   DBSRC,C'N'            NIELSEN                                    
         BNE   TBK053X                                                          
         CLC   DBFIL,=C'TP '         TIME PERIOD                                
         BNE   TBK053X                                                          
*****************************************************************               
* COMMENT ON THIS CHANGE:  CSI USED TO BE PLAN CUTOFF FROM JAN96                
* ON WEEKLY AND MONTHLY. SINCE I HAD TO ADD NEW MONTHLLY BOOKS OF               
* THE WEEKLY'S THE CUTOFF DOESNT APPLY WELL ANYMORE.  I NEEDED TO               
*                                                                               
*                                                                               
*                                                                               
**       CLC   DUB(2),=X'6001'       ON OR AFTER JAN/96                         
**       BL    TBK053X                                                          
***      MVI   BYTE,1               OVERRIDE START DAY TO MONDAY                
***      B     TBK060                                                           
* NEW CODE...WEEKLY NOW ARE ENTERED IN AS BOOKTYPE W FOR CSI                    
         CLI   DBSRC,C'A'         BBM ?                                         
         BE    TBK053X                                                          
         CLI   DUB+2,C'W'                                                       
         BNE   TBK080                                                           
         MVI   BYTE,1               OVERRIDE START DAY TO MONDAY                
         B     TBK060                                                           
TBK053X  EQU   *                                                                
         CLI   DBMED,C'C'         canadian?                                     
         BNE   TBK054                                                           
         CLI   DBSRC,C'A'         BBM ?                                         
         BNE   TBK054                                                           
         CLI   DUB+2,C'W'         BBM WEEKLY??                                  
         BNE   TBK054                                                           
         MVI   BYTE,1             FORCE START DAY TO MONDAY                     
         B     TBK060                                                           
*                                                                               
TBK054   DS    0H                                                               
*                                                                               
         CLI   DBMED,C'N'           CHECK FOR NETWORK                           
         BNE   TBK055X                                                          
         CLI   DBSRC,C'C'            CABLE                                      
         BE    TBK070                                                           
TBK055X  EQU   *                                                                
         B     TBK080              GO FORMAT AS MONTHLY BOOK                    
                                                                                
*                                                                               
TBK060   DS    0H                  FORMAT WEEKLY BOOK                           
         CLI   DBMED,C'O'                                                       
         BNE   *+8                                                              
         MVI   BYTE,1                                                           
         GOTO1 VNSIWEEK,DMCB,(C'D',DUB),(BYTE,VGETDAY),VADDAY,VDATCON           
         ZICM  R1,DMCB+1,(7)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB2(6),0(R1)                                                    
         GOTO1 VDATCON,DMCB,(X'80',DUB2),(3,FULL2)                              
         LHI   R0,FMDBFMT002                                                    
         STC   R0,FULL2+3           SET FORMAT TYPE                             
         B     TBK100                                                           
                                                                                
*                                                                               
TBK070   DS    0H                  FORMAT NETWORK WEEKLY BOOK                   
         GOTO1 VNETUNBK,DMCB,(C'W',DUB),DUB2,VGETDAY,VADDAY,VGETBROD            
         GOTO1 VDATCON,DMCB,(X'80',DUB2),(3,FULL2)                              
         LHI   R0,FMDBFMT002                                                    
         STC   R0,FULL2+3           SET FORMAT TYPE                             
         B     TBK100                                                           
                                                                                
*                                                                               
TBK080   DS    0H                  FORMAT MONTHLY BOOK                          
         MVC   FULL2+0(2),DUB                                                   
         MVI   FULL2+2,1            FORCE A REAL DATE                           
         LHI   R0,FMDBFMT001                                                    
         STC   R0,FULL2+3           SET FORMAT TYPE                             
         B     TBK100                                                           
                                                                                
                                                                                
*                                                                               
TBK100   DS    0H                  FULL2 = A BINARY DATE                        
         ZIC   RF,FULL2+0                                                       
         CVD   RF,DUB2                                                          
         UNPK  0(3,R4),DUB2        YEAR - 3 CHAR REPRESENTATION                 
         OI    2(R4),X'F0'                                                      
         AHI   R4,3                                                             
                                                                                
         DS    0H                  MONTH                                        
         ZIC   RF,FULL2+1                                                       
         BCTR  RF,0                                                             
         LA    RF,MDTRSLT(RF)                                                   
         MVC   WORK+3(1),0(RF)      1 CHAR REPRESENTATION                       
         AHI   R4,1                                                             
                                                                                
         DS    0H                  DAY                                          
         ZIC   RF,FULL2+2                                                       
         BCTR  RF,0                                                             
         LA    RF,MDTRSLT(RF)                                                   
         MVC   WORK+4(1),0(RF)      1 CHAR REPRESENTATION                       
         AHI   R4,1                                                             
                                                                                
         DS    0H                  FORMAT TYPE                                  
         CLI   FULL2+3,1            FORMAT TYPE #1 IS THE DEFAULT               
         BE    TBK139                DON'T NEED TO DOWNLOAD IT                  
         MVC   0(1,R4),FULL2+3                                                  
         OI    0(R4),X'F0'          MAKE IT INTO A CHARACTER                    
         AHI   R4,1                                                             
TBK139   EQU   *                                                                
                                                                                
*                                                                               
TBKX     DS    0H                                                               
         LA    R0,WORK                                                          
         SR    R4,R0                                                            
         STH   R4,HALF             SET LENGTH INTO HALF                         
*                                                                               
         DS    0H                                                               
         J     EXITE                                                            
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
MDTRSLT  DC    C'ABCDEFGHIJKL'      1 - 12  (MONTH & DAYS)                      
         DC    C'MNOPQRSTUVWX'     13 - 24  (DAYS)                              
         DC    C'YZ01234'          25 - 31  (DAYS)                              
                                                                                
                                                                                
         DROP  R9,RB                                                            
**********************************************************************          
* TRANSLATE BOOKTYPE BETWEEN INTERNAL AND 2 CHARACTER AND VICE VERSA            
* ENTRY -DMCB2+0(1) = INDICATOR FROM 1 CHAR TO 2 CHAR OR VICE VERSA             
*                   =12 FOR TRANSLATION FROM 1 TO 2 CHARACTER                   
*        DMCB2+(1)  = 1 CHARACTER INTERNAL BOOKTYPE                             
*     OR DMCB2+1(2) = 2 CHARACTER EXTERNAL BOOKTYPE                             
* EXIT   DMCB2+4(1) = 1 CHARACTER INTERNAL BOOKTYPE                             
*     OR DMCB2+4(2) = 2 CHARACTER EXTERNAL BOOKTYPE                             
**********************************************************************          
TRANSBT  NMOD1 0,*TRNSBT*                                                       
         USING DEMWRKD,R9                                                       
                                                                                
*                                                                               
         L     RF,AFAC                                                          
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB                                               
         ICM   R6,15,0(R1)                                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            LENGTH OF EACH ENTRY                         
         CLI   DMCB2+0,12          TRANSLATE 1 TO 2 CHAR BOOKTYPE?              
         JE    TRNSBT40                                                         
         USING SPBKTYPD,R6                                                      
                                                                                
TRNSBT10 CLI   0(R6),X'FF'         EOT?                                         
         JNE   TRNSBT20                                                         
         MVI   DMCB2+4,X'FF'       IF NOT IN TABLE                              
         J     TRNSBTX             PASS BACK INVALID INDICATOR                  
                                                                                
TRNSBT20 OC    DMCB2+1(2),=X'4040' PAD INPUT WITH SPACES                        
         CLC   SPBKTYPA,DMCB2+1    IS BOOKTYPE IN TABLE?                        
         JE    TRNSBT30                                                         
         AR    R6,R0               NO TRY NEXT                                  
         J     TRNSBT10                                                         
TRNSBT30 MVC   DMCB2+4(L'SPBKTYPN),SPBKTYPN  MOVE INTERNAL DEMO BKTYP           
         J     TRNSBTX                                                          
                                                                                
                                                                                
*************************************************************                   
* TRANSLATE FROM 1 CHARACTER INTERNAL TO 2 CHARCTER FORMAT  *                   
*************************************************************                   
TRNSBT40 CLI   0(R6),X'FF'         EOT?                                         
         JNE   TRNSBT50                                                         
         MVI   DMCB2+4,X'FF'       IF INVALID BOOKTYPE THEN RETURN              
         J     TRNSBTX             INVALID INDICATOR                            
                                                                                
TRNSBT50 CLC   SPBKTYPN,DMCB2+1    IS 1 CHARACTER BOOKTYPE IN TABLE?            
         JE    TRNSBT60                                                         
         AR    R6,R0               NO TRY NEXT                                  
         J     TRNSBT40                                                         
TRNSBT60 MVC   DMCB2+4(L'SPBKTYPA),SPBKTYPA  MOVE 2 CHARCTER BOOKTYPE           
*                                                                               
TRNSBTX  J     EXIT                                                             
                                                                                
*                                                                               
         LTORG                                                                  
                                                                                
         DROP  R9,RB                                                            
*          DATA SET DEDEM00    AT LEVEL 067 AS OF 06/10/08                      
*                                                                               
***********************************************************************         
         TITLE 'DEDEM00 - $DEM ROOT PHASE (FALINK--BREAK)'                      
***********************************************************************         
*======================= FALINK  BREAK  ROUTINE ======================*         
                                                                                
* Routine is called by FALINK to perform necessary tasks before                 
*  breaking the mainframe transaction.                                          
* The application's work area, APWORK, should not be clobbered yet,             
*  otherwise, the attempt to save off the application's variables               
*  so that they can be restored during RESUME would be futile.                  
* At entry,                                                                     
*   R8-->DEMTWAD                                                                
*   R9-->DEMWRKD                                                                
                                                                                
BREAK    NMOD1 0,**BREAK*                                                       
         USING DEMWRKD,R9                                                       
*                                                                               
         L     R8,ATWA                                                          
         USING DEMTWAD,R8                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   GOSTEON,BRKT#        PERFORM BREAK TASKS                         
         GOTO1 AGOSTEO                                                          
                                                                                
*                                                                               
         J     EXIT                                                             
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  R8,R9,RB                                                         
***********************************************************************         
         TITLE 'DEDEM00 - $DEM ROOT PHASE (FALINK--RESUME)'                     
***********************************************************************         
*====================== FALINK  RESUME  ROUTINE ======================*         
                                                                                
* Routine is called by FALINK to perform necessary tasks before we can          
*  resume to the process that was taking place when the mainframe               
*  transaction was broken.                                                      
* At entry,                                                                     
*   R8-->DEMTWAD                                                                
*   R9-->DEMWRKD                                                                
                                                                                
RESUME   NMOD1 0,*RESUME*                                                       
         USING DEMWRKD,R9                                                       
*                                                                               
         L     R8,ATWA                                                          
         USING DEMTWAD,R8                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         OI    RSMFLAG1,RF1APPLC                                                
         NI    BRKFLAG1,XFF-BF1APPLC                                            
*                                                                               
         MVI   GOSTEON,RSMT#        PERFORM RESUME TASKS                        
         GOTO1 AGOSTEO                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         NI    RSMFLAG1,XFF-RF1APPLC                                            
         J     EXIT                                                             
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  R8,R9,RB                                                         
***********************************************************************         
         TITLE 'DEDEM00 - $DEM ROOT PHASE (FALINK--OVERFLOW BREAK/RESUM+        
               E)'                                                              
***********************************************************************         
*=============== FALINK  OVERFLOW BREAK/RESUME  ROUTINE ==============*         
                                                                                
* Routine is called FALINK to perform necessary tasks when its buffer           
*  is filled to capacity and it needs to flush its buffer.                      
* At entry,                                                                     
*   R8-->DEMTWAD                                                                
*   R9-->DEMWRKD                                                                
                                                                                
FLNKOVFL NMOD1 0,*FLKOVF*                                                       
         USING DEMWRKD,R9                                                       
*                                                                               
         L     R8,ATWA                                                          
         USING DEMTWAD,R8                                                       
*                                                                               
         LA    R2,FABLK                                                         
         USING FALINKD,R2                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   MAHMOUNS,X'00'      DOES FALINK WANT TO BREAK?                   
         BE    FLKOVFB              YEP                                         
         CLI   MAHMOUNS,XFF        DOES FALINK WANT TO RESUME?                  
         BE    FLKOVFR              YEP                                         
         DC    H'0'                ???                                          
                                                                                
*                                                                               
FLKOVFB  DS    0H                  FALINK WANTS TO BREAK                        
         OI    BRKFLAG1,BF1FALNK                                                
*                                                                               
         MVI   GOSTEON,BRKT#        PERFORM BREAK TASKS                         
         GOTO1 AGOSTEO                                                          
         B     FLKOVF50                                                         
                                                                                
*                                                                               
FLKOVFR  DS    0H                  FALINK WANTS TO RESUME                       
         OI    RSMFLAG1,RF1FALNK                                                
         NI    BRKFLAG1,XFF-BF1FALNK                                            
*                                                                               
         MVI   GOSTEON,RSMT#        PERFORM RESUME TASKS                        
         GOTO1 AGOSTEO                                                          
*                                                                               
         CLI   STMODE,STMOUTQ       IF STMODE=OUTPUT,                           
         BNE   *+8                                                              
         MVI   APMODE,SAMEREC        WANT TO PROCESS SAME RECORD                
*                                                                               
         NI    RSMFLAG1,XFF-RF1FALNK                                            
         B     FLKOVF50                                                         
                                                                                
*                                                                               
FLKOVF50 DS    0H                                                               
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  R8,R9,RB                                                         
***********************************************************************         
         TITLE 'DEDEM00 - $DEM ROOT PHASE (FALINK--SEND)'                       
***********************************************************************         
*======================= FALINK  SEND  ROUTINE =======================*         
                                                                                
SEND     NMOD1 0,**SEND**                                                       
         USING DEMWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING DEMTWAD,R8                                                       
*                                                                               
         MVC   ASETELEM,0(R1)                                                   
         MVC   AADDDATA,4(R1)                                                   
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   GOSTEON,STCTL#                                                   
         GOTO1 AGOSTEO                                                          
         BL    SENDXL                                                           
         BE    SENDXE                                                           
         B     SENDXH                                                           
                                                                                
*                                                                               
** EXITS FOR SEND HOOK **                                                       
*                                                                               
SENDXL   DS    0H                                                               
         J     EXITL                                                            
                                                                                
SENDXE   DS    0H                                                               
         J     EXITE                                                            
                                                                                
SENDXH   DS    0H                                                               
         J     EXITH                                                            
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  R8,R9,RB                                                         
***********************************************************************         
         EJECT                                                                  
********************************************************************            
RECEIVE  NMOD1 0,*RECEIV*                                                       
         USING DEMWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING DEMTWAD,R8                                                       
         MVC   AGETDATA,0(R1)                                                   
*                                                                               
         MVI   STMODE,STMINPQ          SET STEREO MODE TO INPUT                 
         MVI   GOSTEON,STIIN#          INITIALIZE FOR NEW INPUT TO COME         
         GOTO1 AGOSTEO                                                          
*                                                                               
         LA    R4,STINCTRS                                                      
         LA    R0,STINCTRQ            IF THE SAME WRITE OVER THE OLD            
         SR    R4,R0                  INTERNAL INPUT TABLE ENTRY                
         USING STINCTRD,R4                                                      
*========================================================LOOP RCV100            
RCV100   GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BL    RCVEXIT                 FALINK ERROR                             
         BH    RCVEOD                  END OF DATA                              
                                                                                
*                                                                               
         DS    0H                  CHECK FOR VERSION ELEMENTS                   
         CLI   FPARMS+00,0          ELEMENT DSECT PASSED?                       
         BNE   RCV109                NO, CAN'T BE A VERSION ELEMENT             
*                                                                               
         DS    0H                                                               
         ZICM  RE,FPARMS+01,(7)                                                 
         USING MHELD,RE                                                         
         MVC   SVFALHMC,MHCODE                                                  
*&&DO                                                                           
         CLC   MHCODE,=AL2(FMHVSNA)                                             
         BE    RCV109               (IGNORE FOR NOW)                            
         CLC   MHCODE,=AL2(FMHVSN1)                                             
         BE    RCV109               (IGNORE FOR NOW)                            
*&&                                                                             
         DROP  RE                                                               
RCV109   EQU   *                                                                
*  THIS PART SHOULD MOVE DATA INTO TSAR TABLE JUST AS DEM16 DID BEFORE          
         CLI   FPARMS+00,1                                                      
         BNE   RCV100                                                           
*                                                                               
         DS    0H                      SEE WHICH HEADER DATA BELONGS TO         
         CLC   SVFALHMC,=AL2(FMHRQST)   REQUEST PARAMETERS                      
         BE    RCV115                                                           
         CLC   SVFALHMC,=AL2(FMHVSNA)   VERSION STAMP                           
         BE    RCV200                                                           
         CLC   SVFALHMC,=AL2(FMHVSN1)   VERSION STAMP                           
         BE    RCV200                                                           
         B     RCV100                                                           
*                                                                               
RCV115   DS    0H                                                               
         L     R3,AIOAREA                                                       
         LA    R3,2(R3)                                                         
         USING STICKEYD,R3                                                      
*                                                                               
         L     RE,FPARMS                                                        
         USING MDELD,RE                                                         
         MVC   SVFALDMC,MDCODE        SAVE DATA MAP CODE                        
         ZIC   R2,STICKKNU            COMPARE THE SEQUENCE OF DATA              
         ZICM  RF,MDCODE,2                                                      
         CR    R2,RF                                                            
         BNE   RCNEWCDE                                                         
         ZIC   R2,STICKNTH            OLD SEQ +1                                
         LA    R2,1(R2)                                                         
         CHI   R2,256              CHECK IF SURPASSED LIMIT                     
         BL    *+12                 NO, NOT YET                                 
         MVI   FERN,120             YEP, NEED AN "REQUEST TOO LARGE"            
         B     RCVXN                 ERROR MESSAGE                              
                                                                                
         STCM  R2,1,STICKNTH          MOVE SEQUENCE # INTO TSAR                 
         B     *+8                                                              
RCNEWCDE MVI   STICKNTH,1                                                       
         ZIC   R0,MDCODE+1                                                      
         LA    RF,KEYTAB                                                        
         AR    RF,R0                                                            
         MVC   STICKKNU,0(RF)         MOVE INTERNAL KEY# INTO TSAR              
         CLC   STICKKNU,STINCKNU      COMPARE FOR SAME INTERNAL KEY NUM         
         BE    *+8                                                              
         LA    R4,STINCTRQ(R4)                                                  
         MVC   STINCKNU,STICKKNU      MOVE INTERNAL KEY# INTO STINCTR           
         MVC   STINCDCT,STICKNTH      MOVE SEQUENCE INTO STINCTRS               
         DROP  RE                                                               
*                                                                               
RCV130   ZIC   R2,FPARMS+11            LENGTH OF DATA                           
         LA    R2,STICKEYL+1(R2)       LEN(KEY)+LEN+DATA                        
         STC   R2,STICLEN                                                       
*                                                                               
         LA    RE,2(R2)                LENGTH OF AIOAREA  REC                   
         L     RF,AIOAREA                                                       
         STH   RE,0(RF)                                                         
*                                                                               
         CH    RE,=Y(STMXRECL)         COMPARE AGAINST MAX L(TSAR REC)          
         BNH   RCV150                                                           
         MVI   FERN,120                                                         
         B     RCVEXIT                                                          
*                                                                               
RCV150   LA    RE,STICKEYL+2                                                    
         SR    R2,RE                                                            
         L     RE,FPARMS+12                                                     
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   STICDATA(0),0(RE)                                                
                                                                                
         L     R1,AUPCSETB                                                      
         EX    R2,*+4                                                           
         TR    STICDATA(0),0(R1)       TRANSLATE TO UPPER CASE                  
*                                                                               
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR                                                            
         CLI   TSERRS,0                                                         
         BE     *+6                                                             
         DC    H'0'                                                             
         B     RCV100                                                           
         DROP  R1                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
** PC APPLICATION VERSION STAMPING **                                           
*                                                                               
RCV200   DS    0H                                                               
         L     RE,FPARMS+4                                                      
         MVC   DUB(L'D32PCVER),0(RE)    DUB = VERSION STAMP IN BINARY           
*                                                                               
         DS    0H                                                               
         CLC   SVFALHMC,=AL2(FMHVSNA)   VERSION STAMP W/ ALL TRNSCTNS?          
         BE    RCV250                                                           
         CLC   SVFALHMC,=AL2(FMHVSN1)   VERSION STAMP W/ 1ST TRNSCTN?           
         BE    RCV210                                                           
         DC    H'0'                                                             
                                                                                
*                                                                               
*** VERSION STAMP W/ 1ST TRANSACTION ***                                        
*                                                                               
RCV210   DS    0H                                                               
         MVC   D32PCVER,DUB        REMEMBER PC APPL's VERSION                   
         B     RCV299                                                           
                                                                                
*                                                                               
*** VERSION STAMP W/ ALL TRANSACTIONS ***                                       
*                                                                               
RCV250   DS    0H                                                               
         CLC   D32PCVER,DUB        DID PC APPL's VERSION STAMP CHANGE?          
         BE    *+6                                                              
         DC    H'0'                 YEP, WE CAN'T HANDLE THIS                   
         B     RCV299                                                           
                                                                                
*                                                                               
RCV299   DS    0H                                                               
         B     RCV100                                                           
* =====================================================LOOP RCV100              
RCVEOD   DS    0H                                                               
*                                                                               
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSASAV                                                    
         GOTO1 VTSAR                                                            
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
         MVI   GOSTEON,STINCHK#                                                 
         GOTO1 AGOSTEO                                                          
         B     RCVEXIT                                                          
                                                                                
*                                                                               
RCVXN    DS    0H                  FERN IS SET WITH ERROR NUMBER                
         LA    RE,RCVXN010                                                      
         NTR1                                                                   
         L     RB,ABASE                                                         
         L     RA,BBASE                                                         
         L     R7,CBASE                                                         
         L     R6,DBASE                                                         
         L     RF,AERROR0                                                       
         BR    RF                                                               
RCVXN010 EQU   *                                                                
         OI    MISCFLG1,MF1ERROR                                                
         J     EXITL                                                            
                                                                                
*                                                                               
RCVEXIT  DS    0H                                                               
         TM    MISCFLG1,MF1ERROR                                                
         DROP  R4                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
KEYTAB   DC    X'00'                                                            
KACTION  DC    AL1(IKNACT)                                                      
KFILE    DC    AL1(IKNFIL)                                                      
KSOURCE  DC    AL1(IKNSOU)                                                      
KSTATION DC    AL1(IKNSTA)                                                      
KBOOK    DC    AL1(IKNBOO)                                                      
KDAYTIME DC    AL1(IKNDAY)                                                      
KDEMO    DC    AL1(IKNDEM)                                                      
KOPTION  DC    AL1(IKNOPT)                                                      
KMARKET  DC    AL1(IKNSTA)                                                      
KPROGNUM DC    AL1(IKNSTA)                                                      
KPURENUM DC    AL1(IKNDAY)                                                      
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*====================== GET FALINK ELEMENT ENTRY =====================*         
                                                                                
STMAPECD NMOD1 0,*ELEMCD*                                                       
*        LA    RE,FRHRQST                                                       
         L     RE,AFAMAP                                                        
STMAPE10 CLI   0(RE),0           COMPARE ANY MORE HEADERS                       
         BNE   *+14              THE END?                                       
         XC    AMTENTRY,AMTENTRY  SHIT !                                        
         B     STMAPEX           EXIT                                           
*                                                                               
         CLC   FALEMPC,1(RE)     COMPARE ON ELEMENT MAP CODE                    
         BE    STMAPE30                                                         
         ZICM  RF,3(RE),2        DISP TO NEXT HEADER                            
         AR    RE,RF                                                            
         B     STMAPE10                                                         
STMAPE30 ST    RE,AMTENTRY       FOUND THE ELEMENT MAP CODE                     
STMAPEX  TM    MISCFLG1,MF1ERROR                                                
         XIT1                                                                   
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  RB                                                               
***********************************************************************         
         SPACE 3                                                                
***********************************************************************         
*======================= GET FALINK DATA ENTRY =======================*         
                                                                                
STMAPDCD NMOD1 0,*DATACD*                                                       
* GET ELEMENT HEADER ADDRESS                                                    
         GOTO1 =A(STMAPECD),DMCB,RR=BRELO                                       
         L     R2,AMTENTRY       ADDRESS OF ELEMENT HEADER                      
*                                                                               
         ZIC   RE,0(R2)          LENGTH OF HEADER                               
         AR    RE,R2             JUMP PAST THE ELE HDER TO DATA HDER            
*                                                                               
         USING MDELD,RE                                                         
STMAPD10 CLI   MDLEN,MDLENX      COMPARE END OF DATA TABLE                      
         BNE   *+14              THE END?                                       
         XC    AMTDNTRY,AMTDNTRY SHIT !                                         
         B     STMAPDX                                                          
         CLC   FALDMPC,MDCODE    COMPARE DATA MAP CODE                          
         BE    STMAPD30                                                         
         ZIC   RF,MDLEN          DISP TO NEXT HEADER                            
         AR    RE,RF                                                            
         B     STMAPD10                                                         
STMAPD30 ST    RE,AMTDNTRY       FOUND THE ELEMENT MAP CODE                     
         DROP  RE                                                               
STMAPDX  TM    MISCFLG1,MF1ERROR                                                
         XIT1                                                                   
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  RB                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================= SET FALINK ELEMENT ========================*         
                                                                                
DEM32SET NMOD1 0,*D32SET*                                                       
         GOTO1 =A(STMAPECD),DMCB,RR=BRELO                                       
         MVC   DMCB+4(4),AMTENTRY                                               
         XC    DMCB+8(4),DMCB+8                                                 
         GOTO1 ASETELEM,DMCB,FABLK                                              
         TM    MISCFLG1,MF1ERROR                                                
         XIT1                                                                   
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  RB                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================== FALINK ADD-DATA ==========================*         
                                                                                
DEM32ADD NMOD1 0,*D32ADD*                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   ADMODE,ADMADD                                                    
         BE    D32ADADD                                                         
         CLI   ADMODE,ADMBRK                                                    
         BE    D32ADBRK                                                         
         CLI   ADMODE,ADMDONE                                                   
         BE    D32ADDNE                                                         
         DC    H'0'                                                             
                                                                                
*                                                                               
D32ADADD DS    0H                                                               
         GOTO1 =A(STMAPDCD),DMCB,RR=BRELO                                       
*                                                                               
         MVC   DMCB+04(4),AMTDNTRY                                              
         MVC   DMCB+08(4),ADLDATA                                               
         MVC   DMCB+12(4),LDLDATA                                               
         B     D32ADGO                                                          
                                                                                
*                                                                               
D32ADBRK DS    0H                                                               
         MVC   DMCB+04(4),=AL4(FALABRK)                                         
         MVC   DMCB+08(4),DMCB+04                                               
         B     D32ADGO                                                          
                                                                                
*                                                                               
D32ADDNE DS    0H                                                               
         XC    DMCB+04(8),DMCB+04                                               
         B     D32ADGO                                                          
                                                                                
*                                                                               
D32ADGO  DS    0H                                                               
         GOTO1 AADDDATA,DMCB,FABLK                                              
         TM    MISCFLG1,MF1ERROR                                                
         XIT1                                                                   
                                                                                
                                                                                
         LTORG                                                                  
                                                                                
                                                                                
         DROP  RB                                                               
***********************************************************************         
                                                                                
         DROP  R8,R9                                                            
         TITLE '$DEM VERSION OF SCANNER'                                        
***********************************************************************         
*====================== SCANNER ROUTINE FOR $DEM =====================*         
*                                                                               
* Module will split up variable strings of data into fixed blocks.  The         
*  fields in the string are separated by commas and may be subdivided           
*  with equal signs.                                                            
*                                                                               
* At entry,                                                                     
*   R9-->DEMWRKD                                                                
*                                                                               
*   Parameter 1   byte  0       Non zero = Non standard length of 2nd           
*                                          half of divided fields.  If          
*                                          this is set to (say) 20,             
*                                          divided fields will be 20            
*                                          characters long at line+22           
*                                          and any undivided fields             
*                                          will be 30 at line+12                
*                                                                               
*                 bytes 1-3     A(input string)                                 
*                                                                               
*   Parameter 2   byte  0       (Optional) max number of output lines.          
*                               X'80' = return displacement into                
*                                       string of each field in line.           
*                 bytes 1-3     A(output block)                                 
*                                                                               
*   Parameter 3                 =C',=XY' override C',=' with C'XY'              
*                                                                               
*   Parameter 4                 A(Extra info aligned on fullword bndry)         
*                                +00(2) = length of input string                
*                                +02(1) = ovrd length of 1st half fld           
*                                                                               
* At entry,                                                                     
*   Parameter 2   byte 0        Set to the number of lines on EQ exit           
*                               Set to error code on NEQ exit                   
                                                                                
                                                                                
MYSCAN   NMOD1 MSCNWRKL,$DEMSCAN,CLEAR=YES                                      
         USING MSCNWRKD,RC                                                      
         USING DEMWRKD,R9                                                       
                                                                                
         LR    RA,R1               RA=A(PARAMETER LIST)                         
         LM    R2,R3,0(RA)         R2=A(DATA STRING) R3=A(BLOCK)                
         USING MSCNBLKD,R3                                                      
*                                                                               
         MVC   MSCWMXLI,4(RA)      MAX LINES                                    
*                                                                               
         MVC   MSCWCMMA(2),COMAEQLS SET DEFAULT TO ,=                           
         CLC   8(2,RA),COMAEQLS                                                 
         BNE   MSCAN0                                                           
         MVC   MSCWCMMA(2),10(RA)  OVERRIDE SPECIAL CHARACTERS                  
*                                                                               
MSCAN0   DS    0H                                                               
         SR    R4,R4                                                            
         L     RF,12(RA)           RF-->L(INPUT STRING)                         
         ICM   R4,3,0(RF)          R4 = L(INPUT STRING)                         
         C     R4,=A(L'MSCWBUFF)   CHECK L(INPUT) VS MAX ALLOWED                
         BNH   *+12                                                             
         MVI   MSCWERR,MSCWEITB                                                 
         B     MSCANERR                                                         
                                                                                
         LA    RE,MSCWBUFF                                                      
         LR    RF,R4                                                            
         LR    R0,R2                                                            
         LR    R1,R4                                                            
         MVCL  RE,R0               MOVE INPUT STRING INTO WORK AREA             
         MVC   0(1,RE),MSCWCMMA     AND POP IN A COMMA TO SIMPLIFY              
*                                                                               
         DS    0H                                                               
         XC    MSCWDISP,MSCWDISP                                                
                                                                                
         SR    R0,R0               ESTABLISH L(1ST HALF FIELD)                  
         L     RF,12(RA)                                                        
         LHI   R0,L'MSCB1DAT       ASSUME DEFAULT FOR L(LHS)                    
         CLI   2(RF),L'MSCB1DAT                                                 
         BNH   *+8                                                              
         ICM   R0,1,2(RF)           GET CALLER'S OVERRIDE                       
         STH   R0,MSCWLLFT                                                      
                                                                                
         LA    R0,L'MSCB2DAT       ASSUME DEFAULT FOR L(RHS)                    
         CLI   0(RA),L'MSCB2DAT                                                 
         BNH   *+8                                                              
         IC    R0,0(RA)             ELSE, USE USER'S CHOICE                     
         STH   R0,MSCWLRGT                                                      
                                                                                
         AH    R0,MSCWLLFT                                                      
         STH   R0,MSCWLBTH                                                      
                                                                                
         AHI   R0,MSCBFXL                                                       
         STH   R0,MSCWLROW                                                      
         B     MSCAN4                                                           
                                                                                
*                                                                               
MSCAN4   DS    0H                                                               
         LA    R2,MSCWBUFF                                                      
         LA    R5,0(R2,R4)                                                      
         SR    R6,R6               R6=NUMBER OF LINES USED                      
         EJECT                                                                  
*HANDLE LINES OF DATA                                                           
*                                                                               
MSCAN6   DS    0H                                                               
         XC    0(MSCBFXL,R3),0(R3)      PRESET A LINE                           
         LH    RF,MSCWLBTH                                                      
         BCTR  RF,0                                                             
         EXMVC RF,MSCB1DAT,SPACES                                               
         MVC   MSCB1VBT(2),=X'E0E0'                                             
         BAS   RE,GETL                                                          
                                                                                
         CLI   MSCB1LEN,0                                                       
         BNE   *+8                                                              
         MVI   MSCB1VBT,0                                                       
                                                                                
         CLI   MSCB2LEN,0                                                       
         BNE   *+8                                                              
         MVI   MSCB2VBT,0                                                       
                                                                                
         CLC   MSCB1LEN,MSCWLBTH+1  CHECK L(1st HALF) VS. MAX LENGTH            
         BNH   *+12                                                             
         MVI   MSCWERR,MSCWE1TB                                                 
         B     MSCANERR                                                         
                                                                                
         CLC   MSCB2LEN,MSCWLRGT+1  CHECK L(2nd HALF) VS. MAX LENGTH            
         BNH   *+12                                                             
         MVI   MSCWERR,MSCWE2TB                                                 
         B     MSCANERR                                                         
                                                                                
         CLI   MSCB2LEN,0           IF 2ND HALF HAS INPUT,                      
         BE    MSCAN8                                                           
         CLC   MSCB1LEN,MSCWLLFT+1   CHECK THAT 1ST HALF NOT TOO BIG            
         BNH   *+12                                                             
         MVI   MSCWERR,MSCWE1TB                                                 
         B     MSCANERR                                                         
                                                                                
                                                                                
MSCAN8   DS    0H                                                               
         SR    R7,R7                                                            
         IC    R7,MSCB1LEN                                                      
         LTR   R7,R7                                                            
         BZ    MSCAN18                                                          
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   MSCB1DAT(0),0(R2)                                                
         TM    MSCB1VBT,X'80'                                                   
         BZ    MSCAN10                                                          
         CH    R7,=H'8'                                                         
         BH    MSCAN10                                                          
         EX    R7,VARPAK                                                        
         CVB   R8,MSCWDUB                                                       
         STCM  R8,7,MSCB1VAL+1     STORE AT LEAST 3 BYTES BINARY                
         TM    4(RA),X'80'         THAT'S ALL IF RETURNING DISPS.               
         BO    MSCAN10                                                          
         STCM  R8,8,MSCB1VAL+0                                                  
                                                                                
                                                                                
MSCAN10  DS    0H                                                               
         LA    R2,2(R2,R7)                                                      
         IC    R7,MSCB2LEN                                                      
         LTR   R7,R7                                                            
         BZ    MSCAN20                                                          
         BCTR  R7,0                                                             
         LA    RF,MSCB1DAT                                                      
         AH    RF,MSCWLLFT         MSCB2DAT = MSCB1DAT + MSCWLLFT               
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)                                                    
         TM    MSCB2VBT,X'80'                                                   
         BZ    MSCAN12                                                          
         CH    R7,=H'8'                                                         
         BH    MSCAN12                                                          
         EX    R7,VARPAK                                                        
         CVB   R8,MSCWDUB                                                       
         STCM  R8,7,MSCB2VAL+1     STORE AT LEAST 3 BYTES BINARY                
         TM    4(RA),X'80'         THAT'S ALL IF RETURNING DISPS.               
         BO    MSCAN12                                                          
         STCM  R8,8,MSCB2VAL+0                                                  
                                                                                
                                                                                
MSCAN12  DS    0H                                                               
         LA    R2,2(R2,R7)                                                      
         B     MSCAN20                                                          
                                                                                
                                                                                
VARPAK   PACK  MSCWDUB,0(0,R2)                                                  
                                                                                
                                                                                
MSCAN18  DS    0H                                                               
         LA    R2,1(R2)                                                         
         CLI   1(R3),0                                                          
         BE    *+12                                                             
         MVI   MSCWERR,MSCWE1MS                                                 
         B     MSCANERR                                                         
                                                                                
                                                                                
MSCAN20  DS    0H                                                               
         LA    R6,1(R6)            BUMP N'LINES                                 
         AH    R3,MSCWLROW         BUMP TO NEXT LINE IN BLOCK                   
         CR    R2,R5               ARE WE NOW PAST LAST 'COMMA'                 
         BH    MSCANOK                                                          
         IC    R7,MSCWMXLI                                                      
         LTR   R7,R7                                                            
         BZ    MSCAN6                                                           
         CR    R6,R7               HAVE WE REACHED MAX N'LINES                  
         BNE   MSCAN6                                                           
                                                                                
*                                                                               
** EXITS **                                                                     
*                                                                               
MSCANOK  DS    0H                                                               
         STC   R6,4(RA)            SET NUMBER OF LINES USED                     
         CR    RC,RC                                                            
         J     EXIT                                                             
*                                                                               
MSCANERR DS    0H                                                               
         MVC   4(1,RA),MSCWERR                                                  
         MVC   2(2,R3),=X'FFFF'                                                 
         CR    RB,RC                                                            
         J     EXIT                                                             
                                                                                
                                                                                
         DROP  R3                                                               
         EJECT                                                                  
*VALIDATE AND GET LENGTHS                                                       
*                                                                               
GETL     NTR1                                                                   
         LR    R4,R3                                                            
         USING MSCNBLKD,R4                                                      
*                                                                               
         SR    R5,R5                                                            
         TM    4(RA),X'80'                                                      
         BZ    GETL2                                                            
         MVC   MSCB1DSP,MSCWDISP+1 DISPLACEMENT INTO FIELD                      
                                                                                
*                                                                               
GETL2    DS    0H                                                               
         CLC   0(1,R2),MSCWCMMA    TEST FIELD SEPERATOR                         
         BE    GETL12                                                           
         CLC   0(1,R2),MSCWEQAL                                                 
         BE    GETL14                                                           
         LA    R5,1(R5)                                                         
         CLI   0(R2),C'9'                                                       
         BNH   *+8                                                              
         MVI   MSCB1VBT,0          (ALL INVALID)                                
         CLI   0(R2),C'0'                                                       
         BL    GETL4                                                            
         NI    MSCB1VBT,X'BF'      (INVALID ALPHA)                              
         B     GETL10                                                           
                                                                                
*                                                                               
GETL4    DS    0H                                                               
         NI    MSCB1VBT,X'7F'      (INVALID NUM)                                
         CLI   0(R2),C'Z'                                                       
         BNH   GETL6                                                            
         MVI   MSCB1VBT,0          Z-0 = ALL INVALID                            
         B     GETL10                                                           
                                                                                
*                                                                               
GETL6    DS    0H                                                               
         CLI   0(R2),C'A'          LESS THAN A = ALL INVALID                    
         BNL   GETL8                                                            
         MVI   MSCB1VBT,0                                                       
         B     GETL10                                                           
                                                                                
*                                                                               
GETL8    DS    0H                                                               
         CLI   0(R2),C'F'          OK FOR ALPHA                                 
         BNH   GETL10                                                           
         NI    MSCB1VBT,X'DF'      G-Z = INVALID HEX                            
                                                                                
*                                                                               
GETL10   DS    0H                                                               
         LA    R2,1(R2)                                                         
         B     GETL2                                                            
                                                                                
*                                                                               
GETL12   DS    0H                                                               
         STC   R5,MSCB1LEN         COMMA FOUND                                  
         LA    R5,1(R5)                                                         
         AH    R5,MSCWDISP                                                      
         STH   R5,MSCWDISP                                                      
         J     EXIT                                                             
                                                                                
*                                                                               
GETL14   DS    0H                                                               
         LHI   R0,1                                                             
         AH    R0,MSCWLLFT                                                      
         STC   R0,MSCB1LEN            EQUAL FOUND (PRESET INVALID AND           
         CR    R4,R3                               CHECK MORE THAN 1)           
         JNE   EXIT                                                             
         STC   R5,MSCB1LEN            NOW STORE L1                              
         LA    R5,1(R5)                                                         
         AH    R5,MSCWDISP                                                      
         STH   R5,MSCWDISP                                                      
         TM    4(RA),X'80'                                                      
         BZ    GETL16                                                           
         MVC   MSCB1DSP,MSCWDISP+1    DISPLACEMENT INTO FIELD                   
                                                                                
*                                                                               
GETL16   DS    0H                                                               
         LA    R4,1(R4)            POINT TO FIELD2 DATA                         
         SR    R5,R5               CLEAR L2                                     
         LA    R2,1(R2)            POINT PAST EQUAL SIGN                        
         B     GETL2                                                            
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
                                                                                
                                                                                
COMAEQLS DC    C',='                                                            
                                                                                
                                                                                
         DROP  R9,RB,RC                                                         
***********************************************************************         
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO ROUTINES)'                    
***********************************************************************         
*========================== STEREO ROUTINES ==========================*         
*^^GYL STEREOQ  EQU   (((*-DEM00+4095)/4096)*4096)                              
STEREOQ  EQU   (((*-DEM00+X'07FF')/X'0800')*X'0800')                            
                                                                                
         ORG   DEM00+STEREOQ                                                    
STEREO   NMOD1 0,**DEMS**,RA                                                    
         L     R9,0(R1)                                                         
         USING DEMWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING DEMTWAD,R8                                                       
                                                                                
         L     R1,4(R1)                                                         
         SRL   R1,24               SHIFT TO LOWER BYTE,                         
         BCTR  R1,0                 SUBTRACT ONE,                               
         SLL   R1,2                 AND MULTIPLY BY FOUR                        
         B     STE_00(R1)                                                       
                                                                                
STCTL#   EQU   (STE_01-STE_00)/4+1                                              
STINP#   EQU   (STE_02-STE_00)/4+1                                              
STOUT#   EQU   (STE_03-STE_00)/4+1                                              
STIIN#   EQU   (STE_04-STE_00)/4+1                                              
GCH#     EQU   (STE_05-STE_00)/4+1     GET CHARACTER (FROM SCREEN)              
LSC#     EQU   (STE_06-STE_00)/4+1     LOAD SCREEN                              
CLS#     EQU   (STE_07-STE_00)/4+1     CLEAR SCREEN                             
BRCT#    EQU   (STE_08-STE_00)/4+1     BUILD REQUEST CONTROL TABLE              
FMTS#    EQU   (STE_09-STE_00)/4+1     FORMAT REQUEST ONTO SCREEN               
PCH#     EQU   (STE_10-STE_00)/4+1     PUT CHARACTER (TO SCREEN)                
STPST#   EQU   (STE_11-STE_00)/4+1     POST TO TSAR BUFFER                      
SGTDR#   EQU   (STE_12-STE_00)/4+1     GET TSAR DEMO RECORD                     
URCT#    EQU   (STE_13-STE_00)/4+1     UPDATE REQUEST CONTROL TABLE             
STV#     EQU   (STE_14-STE_00)/4+1     BRANCH TO VALIDATION ROUTINES            
SAPC#    EQU   (STE_15-STE_00)/4+1     CALL APPLICATION OVERLAY                 
LOV#     EQU   (STE_16-STE_00)/4+1     LOAD APPLICATION OVERLAY                 
SSCR#    EQU   (STE_17-STE_00)/4+1     SAVE    (STEREO) SCREEN                  
RSCR#    EQU   (STE_18-STE_00)/4+1     RESTORE (STEREO) SCREEN                  
PVT#     EQU   (STE_19-STE_00)/4+1     PRE-VALIDATION TASKS                     
SERR0#   EQU   (STE_20-STE_00)/4+1     ENTRY POINT INTO ERROR ROUTINE           
SERR1#   EQU   (STE_21-STE_00)/4+1     ENTRY POINT INTO ERROR ROUTINE           
SERR2#   EQU   (STE_22-STE_00)/4+1     ENTRY POINT INTO ERROR ROUTINE           
SERR4#   EQU   (STE_23-STE_00)/4+1     ENTRY POINT INTO ERROR ROUTINE           
SERR6#   EQU   (STE_24-STE_00)/4+1     ENTRY POINT INTO ERROR ROUTINE           
*BTD#     EQU   (STE_25-STE_00)/4+1     CONVERT A BOOK TO A DATE FMT            
STI#     EQU   (STE_26-STE_00)/4+1     SAVE STUFF IN TIA AREA                   
RTI#     EQU   (STE_27-STE_00)/4+1     RESTORE STUFF FROM TIA AREA              
DTSAR#   EQU   (STE_28-STE_00)/4+1     DID TSAR FOR FALINK                      
STINCHK# EQU   (STE_29-STE_00)/4+1     MOVE TO STERO INPUT CHUNK                
SMWV#    EQU   (STE_30-STE_00)/4+1     SET FOR MONTH OR WEEK VALIDATION         
RESUME#  EQU   (STE_31-STE_00)/4+1     AFTER RESUME MODE                        
BREAK#   EQU   (STE_32-STE_00)/4+1     AFTER BREAK MODE                         
VPUR#    EQU   (STE_33-STE_00)/4+1     VALIDATE PURE NUMBER                     
VSCR#    EQU   (STE_34-STE_00)/4+1     VALIDATE SCREEN                          
BRKT#    EQU   (STE_35-STE_00)/4+1     TASKS FOR BREAKING A TRANSACTION         
RSMT#    EQU   (STE_36-STE_00)/4+1     TASKS FOR RESUMING A TRANSACTION         
*ETP81#  EQU   (STE_37-STE_00)/4+1     CALL OVERLAY FOR 81                      
                                                                                
STE_00   DS    0H                                                               
STE_01   B     STCNTLR                                                          
STE_02   B     STINPUT                                                          
STE_03   B     STOUTPUT                                                         
STE_04   B     STININ                                                           
STE_05   B     GETCHAR                                                          
STE_06   B     LOADSCRN                                                         
STE_07   B     CLRSCRN                                                          
STE_08   B     BLDRCT                                                           
STE_09   B     FMTSCRN                                                          
STE_10   B     PUTCHAR                                                          
STE_11   B     STPOST                                                           
STE_12   B     STGETTDR                                                         
STE_13   B     UPDRCT                                                           
STE_14   B     STVAL                                                            
STE_15   B     STAPCALL                                                         
STE_16   B     LOADOVLY                                                         
STE_17   B     SAVESCR                                                          
STE_18   B     RSTRSCR                                                          
STE_19   B     PVLTSK                                                           
STE_20   B     STERR00                                                          
STE_21   B     STERR10                                                          
STE_22   B     STERR20                                                          
STE_23   B     STERR30                                                          
STE_24   B     STERR40                                                          
*STE_25   B     BKTODATE                                                        
STE_26   B     SAVETIA                                                          
STE_27   B     RSTRTIA                                                          
STE_28   B     STCTL120                                                         
STE_29   B     STINP60                                                          
STE_30   B     SETMWVAL                                                         
STE_31   B     STCTL400                                                         
STE_32   B     STCTL440                                                         
STE_33   B     VALPURE                                                          
STE_34   B     VALSCRN                                                          
STE_35   B     BRKTASKS                                                         
STE_36   B     RSMTASKS                                                         
*STE_37   B     SETPHS81                                                        
STE#     EQU   (*-STE_00)/4                                                     
         DC    H'0'                                                             
                                                                                
YES_STE  SR    R9,R9                                                            
NO_STE   LTR   R9,R9                                                            
XIT_STE  XIT1                                                                   
********************************************************************            
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--STCTL#)'                     
*------------------------- STEREO CONTROLLER -------------------------*         
                                                                                
STCNTLR  DS    0H                                                               
         ST    RD,ASTCTLRD         SAVE RD FOR QUICK EXIT OF STCTL#             
                                                                                
*                                                                               
         DS    0H                                                               
*&&DO                                                                           
         LH    R1,=Y(BINTAB-DEMWRKD)                                            
         LA    R1,DEMWRKD(R1)                                                   
         ST    R1,BINATAB                                                       
                                                                                
*&&                                                                             
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    *+12                                                             
         LA    R0,STELAST-1                                                     
         ST    R0,AENDSCN          A(END OF $DEM STEREO SCREEN)                 
                                                                                
         NI    MISCFLG1,XFF-MF1STERR                                            
*                                                                               
STCTL020 DS    0H                                                               
         CLI   STMODE,STMINPQ      INPUT MODE?                                  
         BE    STCTL100                                                         
         CLI   STMODE,STMMIDQ      MIDDLE MODE?                                 
         BE    STCTL200                                                         
         CLI   STMODE,STMOUTQ      OUTPUT MODE?                                 
         BE    STCTL300                                                         
         CLI   STMODE,STMNXTQ      PREPARE-FOR-NEXT-REQUEST MODE?               
         BE    STCTL500                                                         
         B     STCTLX                                                           
         EJECT                                                                  
*                                                                               
** STEREO SESSION INPUT MODE **                                                 
*                                                                               
STCTL100 DS    0H                                                               
         CLI   STMODE,STMINPQ      MAKE SURE MODE IS SET CORRECTLY              
         BE    STCTL110                                                         
         DC    H'0'                                                             
*                                                                               
STCTL110 DS    0H                  READ & PARSE INPUT FROM STEREO               
         MVI   FERN,0               ASSUME NO ERRORS                            
         MVI   FNDX,0                                                           
         XC    FADR,FADR                                                        
*                                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32   IF DEM32 SESSION,                   
         BO    STCTL120                      INPUT RECEIVED ALREADY             
                                                                                
         MVI   GOSTEON,STINP#      GO GET INPUT                                 
         GOTO1 AGOSTEO                                                          
         CLI   FERN,0               ANY ERROR ENCOUNTERED?                      
         BNE   STCTL112                                                         
         TM    INPTFLG1,IF1EOSCN    END OF SCREEN REACHED?                      
         BO    STCTL114                                                         
         B     STCTL120                                                         
                                                                                
STCTL112 DS    0H                  ERROR ENCOUNTERED ON INPUT                   
         MVI   GOSTEON,SERR1#       FORMAT ERROR MESSAGE,                       
         GOTO1 AGOSTEO                                                          
         MVI   STMODE,STMNXTQ        SET NEXT MODE,                             
         B     STCTLX                AND EXIT NOW                               
                                                                                
STCTL114 DS    0H                  END OF SCREEN REACHED                        
         MVI   GOSTEON,CLS#         CLEAR SCREEN,                               
         GOTO1 AGOSTEO                                                          
         OI    STEI1STH+6,X80+X40   POSITION CURSOR,                            
         B     STCTLX                AND EXIT                                   
*                                                                               
STCTL120 DS    0H                                                               
         MVI   GOSTEON,BRCT#       BUILD REQUEST CONTROL TABLE                  
         GOTO1 AGOSTEO                                                          
                                                                                
         MVI   STMODE,STMMIDQ           SET FOR MIDDLE MODE, AND                
         OI    OUPTFLG1,OF1NEW+OF1ITSR   SET FLAGS                              
         B     STCTL200                                                         
         EJECT                                                                  
*                                                                               
** STEREO SESSION MIDDLE MODE **                                                
*                                                                               
STCTL200 DS    0H                                                               
         CLI   STMODE,STMMIDQ      MAKE SURE WE'RE IN THE RIGHT MODE            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
*                                                                               
STCTL220 DS    0H                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32    DEM32?                             
         BO    STCTL222                                                         
*                                                                               
         CLI   SCREEN,ODEMSCRN     DO WE HAVE ORIG $DEM SCREEN LOADED?          
         BE    STCTL222             YEP                                         
         MVI   SCREEN,ODEMSCRN     LOAD IN ORIGINAL $DEM SCREEN                 
         MVI   GOSTEON,LSC#                                                     
         GOTO1 AGOSTEO                                                          
                                                                                
STCTL222 DS    0H                                                               
         MVI   GOSTEON,CLS#        CLEAR UNPROTECTED FIELDS                     
         GOTO1 AGOSTEO                                                          
                                                                                
STCTL223 MVI   GOSTEON,FMTS#       FORMAT REQUEST ONTO SCREEN                   
         GOTO1 (RF)                                                             
         TM    MISCFLG1,MF1ERROR    DID AN ERROR OCCURRED?                      
         BZ    *+12                  NOPE                                       
         MVI   STMODE,STMNXTQ        YEP, SET NEXT MODE                         
         B     STCTLXL                AND EXIT NOW W/ CC LOW                    
                                                                                
STCL225  MVI   GOSTEON,PVT#        DO PRE-VALIDATION TASKS                      
         GOTO1 (RF)                                                             
*                                                                               
         MVI   GOSTEON,STV#        BRANCH TO VALIDATE FIELDS                    
         GOTO1 (RF)                                                             
         TM    MISCFLG1,MF1ERROR    DID AN ERROR OCCURRED?                      
         BZ    *+12                  NOPE                                       
         MVI   STMODE,STMNXTQ        YEP, SET NEXT MODE                         
         B     STCTLXL                AND EXIT NOW W/ CC LOW                    
*                                                                               
                                                                                
         B     STCTL230                                                         
*                                                                               
STCTL230 DS    0H                  LOAD APPL OVERLAY INTO CORE                  
         MVI   GOSTEON,LOV#                                                     
         GOTO1 AGOSTEO                                                          
                                                                                
         TM    OUPTFLG1,OF1ITSR    TEST IF NEED TO INITIALIZE TSAR              
         BZ    STCTL240             NO, DON'T NEED TO                           
                                                                                
         DS    0H                  CALL APPLICATION FOR INITIALIZATION          
         XC    TSARBLCK,TSARBLCK   CLEAR TSAR BLOCK                             
         MVI   APMODE,FORMHEAD                                                  
         MVI   GOSTEON,SAPC#                                                    
         GOTO1 AGOSTEO             L'KEY & MAX L'RECD SET IN APPLCTN            
         MVC   SVMXRLEN,TSRECL                                                  
         MVI   TSACTN,TSAINI        TSAR INITIALIZATION ACTION                  
         MVC   TSAREC,ATSIOREC      ADDRESS OF TSAR RECORD                      
         MVC   TSACOM,AFAC          A(COMFACS)                                  
         MVI   TSPAGL,1             START W/ PAGE 1 OF TEMPSTR                  
         MVI   TSPAGN,3             USE THREE PAGES OF TEMPSTR                  
* IF DEM32 SESSION THEN SET TEMPSTR TO 10 PAGES                                 
* SOMEHOW IS REMEMBER A PROBLEM INCREASING THE NUMBER OF PAGES OF               
* TEMPSTR FOR STEREO SCREEN SESSION BEFORE                                      
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BZ    *+8                                                              
         MVI   TSPAGN,10            USE SIZ PAGES OF TEMPSTR                    
         OI    TSRECI,TSRVAR        VARIABLE LENGTH RECORDS                     
         OI    TSINDS,TSIREUSE+TSIXTTWA                                         
         NI    TSINDS,XFF-TSIINIOK                                              
                                                                                
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0             ANY ERRORS?                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TSINDS,TSIINIOK      INITIALIZATION OK?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    OUPTFLG1,XFF-OF1ITSR DON'T INIT TSAR NEXT TIME AROUND            
                                                                                
         B     STCTL250            READY TO CALL APPL OVLY TO PROCESS           
*                                                                               
STCTL240 DS    0H                  (THIS MUST BE DONE)                          
         MVI   APMODE,FORMHEAD                                                  
         MVI   GOSTEON,SAPC#                                                    
         GOTO1 AGOSTEO                                                          
         B     STCTL250            READY TO CALL APPL OVLY TO PROCESS           
*                                                                               
STCTL250 DS    0H                  HANDLE PROCESS CALL TO APPLICATION           
         L     R0,AIOAREA1                                                      
         ST    R0,AIOAREA                                                       
         XC    NDXDA,NDXDA                                                      
         MVI   APMODE,PROCESS                                                   
         MVI   GOSTEON,SAPC#                                                    
         GOTO1 AGOSTEO                                                          
         BL    STCTLXL                                                          
                                                                                
         TM    BRKFLAG1,BF1APPLC   DID APPLICATION ASK FOR A BREAK?             
         BNZ   STCTLBRK             YEP, DO THAT NOW                            
                                                                                
         DS    0H                  UPDATE REQUEST CONTROL TABLE                 
         MVI   GOSTEON,URCT#                                                    
         GOTO1 AGOSTEO                                                          
         TM    OUPTFLG1,OF1RELSE   RELEASE TSAR DEMO RECORDS YET?               
         BZ    STCTL220             NOPE, NOT YET                               
*                                                                               
         MVI   STMODE,STMOUTQ       YES, MOVE ONTO OUTPUT MODE                  
         B     STCTL300                                                         
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
** STEREO SESSION OUTPUT MODE **                                                
*                                                                               
STCTL300 DS    0H                                                               
         CLI   STMODE,STMOUTQ      MAKE SURE MODE IS SET CORRECTLY              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DS    0H                  LOAD APPL OVERLAY INTO CORE                  
         MVI   GOSTEON,LOV#                                                     
         GOTO1 AGOSTEO                                                          
*                                                                               
         DS    0H                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
                                                                                
*                                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    STCTL400                                                         
*                                                                               
         CLI   SCREEN,STEOSCRN     DO WE HAVE STEREO SCREEN?                    
         BE    STCTL304                                                         
         MVI   SCREEN,STEOSCRN      NOPE, NEED TO LOAD IT                       
         MVI   GOSTEON,LSC#                                                     
         GOTO1 AGOSTEO                                                          
*                                                                               
STCTL304 DS    0H                                                               
         CLI   STEI1STH+5,5        SUPPORT CANCELLING FOR STEREO                
         BNE   STCTL304X            (IN THE SIMPLEST WAY, FOR NOW)              
         CLC   STEI1ST(5),=C'RESET'                                             
         BNE   STCTL304X                                                        
         MVI   STMODE,STMNXTQ                                                   
         B     STCTL500             PREPARE FOR NEXT INPUT NOW                  
STCTL304X EQU  *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         XC    STEMSG,STEMSG       CLEAR & TRANSMIT MESSAGE LINE                
         OI    STEMSGH+6,X80                                                    
         MVI   GOSTEON,CLS#        CLEAR ALL UNPROTECTED FIELDS                 
         GOTO1 AGOSTEO                                                          
         MVC   TSAREC,ATSIOREC     SET ADDRESS OF TSAR RECORD                   
         MVC   TSACOM,AFAC          AND A(COMFACS)                              
                                                                                
         TM    OUPTFLG1,OF1NEW+OF1SCFUL   SET ADCONS FOR SCREEN?                
         BZ    STCTL310                                                         
         LA    R1,STEO1STH                 YES, SET INITIAL VALUES              
         ST    R1,ALINEHDR                                                      
         LA    R1,STEO1ST                                                       
         ST    R1,ALINE                                                         
         ST    R1,ANEXTCHR                                                      
         LA    R1,L'STEO1ST-1(R1)                                               
         ST    R1,AENDLN                                                        
         LA    R1,STELAST-1                                                     
         ST    R1,AENDSCN                                                       
*                                                                               
STCTL310 DS    0H                                                               
         TM    OUPTFLG1,OF1NEW     OUTPUTTING A NEW STEREO REQUEST?             
         BO    STCTL312             YEP                                         
         TM    OUPTFLG1,OF1SCFUL   OUTPUTTED A FULL SCREEN PREVIOUSLY?          
         BO    STCTL314             YEP                                         
         TM    OUPTFLG1,OF1SVSCR   (STEREO) SCREEN SAVED PREVIOUSLY?            
         BO    STCTL316             YEP                                         
         DC    H'0'                                                             
                                                                                
STCTL312 DS    0H                  OUTPUTTING NEW STEREO REQUEST                
         XC    TSRNUM,TSRNUM        SET TO READ 1ST TSAR DEMO RECD              
         NI    OUPTFLG1,XFF-OF1NEW  STREO RQUST NOT NEW NXT TIME AROUND         
         B     STCTL320                                                         
                                                                                
STCTL314 DS    0H                  FULL SCREEN PREVIOUSLY OUTPUTTED             
         MVI   TSACTN,TSARES                                                    
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0            ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                 AFRAID SO!                                  
                                                                                
*&&DO                                                                           
         MVI   GOSTEON,RTI#        RESTORE STUFF INTO TIA                       
         GOTO1 AGOSTEO                                                          
                                                                                
*&&                                                                             
         LA    RE,LASTS                                                         
         LA    RF,LASTSL                                                        
         LA    R0,THESE                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE                  RESTORE PREVIOUS VALUES                   
                                                                                
         LA    RE,SVDBKVAL                                                      
         LA    RF,SVDBKVLQ                                                      
         LA    R0,DBKVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                  RESTORE PREVIOUS DBLOCK KEY VALS          
                                                                                
         MVC   APMODE,SVAPMODE        RESTORE PREVIOUS APMODE SETTING           
         NI    OUPTFLG1,XFF-OF1SCFUL  SCREEN NOT FULL YET                       
         B     STCTL334               PICK UP FROM LAST LEFT OFF                
                                                                                
STCTL316 DS    0H                  (STEREO) SCREEN PREVIOUSLY SAVED             
         MVI   GOSTEON,RSCR#          RESTORE OUTPUT SCREEN                     
         GOTO1 AGOSTEO                                                          
         NI    OUPTFLG1,XFF-OF1SVSCR  SCREEN RESTORED                           
         XC    TSRNUM,TSRNUM       ??? NOT SURE ABOUT THIS                      
         B     STCTL320                                                         
*                                                                               
STCTL320 DS    0H                  READ NEXT RECORD                             
         CLI   APMODE,SAMEREC       IF APPLICATION ASKED FOR SAME RECD,         
         BE    STCTL325              DON'T READ NEXT RECORD                     
         LH    R1,TSRNUM                                                        
         LA    R1,1(R1)                                                         
         STH   R1,TSRNUM                                                        
                                                                                
STCTL325 DS    0H                                                               
         MVI   TSACTN,TSAGET                                                    
         GOTO1 VTSAR,TSARD                                                      
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    STCTL350             YEP, GO DO NEXT INPUT COMBO                 
*                                                                               
STCTL330 DS    0H                  CALL APPLICATION TO EXTRACT OUTPUT           
         CLI   APMODE,NEXTREC       IF APPLICATION ASKED FOR NEXT RECD,         
         BE    STCTL332              DON'T ERASE WHAT APPL PUT IN BUFFR         
         XC    SVIOBDSP,SVIOBDSP    CLEAR SAVED STIOBUFF DISPL                  
         XC    IODATALN,IODATALN                                                
         L     R0,ASTIOBUF                                                      
         LH    R1,LSTIOBUF                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR STEREO I/O BUFFER                      
                                                                                
STCTL332 DS    0H                  APPLICATION CALL                             
         MVI   APMODE,FORMLINE                                                  
         MVI   GOSTEON,SAPC#                                                    
         GOTO1 AGOSTEO                                                          
         CLI   APMODE,NEXTREC      IF APPLICATION ASKED FOR NEXT RECD,          
         BE    STCTL338             DON'T OUTPUT DATA IN BUFFER YET             
         OC    IODATALN,IODATALN   ANYTHING TO OUTPUT?                          
         BZ    STCTL338             NOPE, GO GET NEXT RECORD                    
                                                                                
STCTL334 DS    0H                  XFER DATA FROM BUFFER TO SCREEN              
         LH    R2,SVIOBDSP         PICK UP FROM WHERE WE LAST LEFT OFF          
         CH    R2,IODATALN          UNLESS WE WERE ABLE TO JUST FINISH          
         BNL   STCTL338             LAST TIME                                   
STCTL336 LR    R1,R2                                                            
         A     R1,ASTIOBUF                                                      
         MVC   CHAR,0(R1)                                                       
         MVI   GOSTEON,PCH#                                                     
         GOTO1 AGOSTEO                                                          
         LA    R2,1(R2)                                                         
         TM    OUPTFLG1,OF1SCFUL   IS SCREEN FULL YET?                          
         BO    STCTL340             YEP, EXIT SO STEREO PICKS UP DATA           
         CH    R2,IODATALN                                                      
         BL    STCTL336                                                         
                                                                                
STCTL338 DS    0H                  NO MORE DATA TO PUT TO SCREEN                
         B     STCTL320            GET NEXT TSAR DEMO RECORD                    
*                                                                               
STCTL340 DS    0H                  SCREEN FULL (W/ OUTPUT DATA)                 
         STH   R2,SVIOBDSP         SAVE DISP FOR NEXT TIME AROUND               
         MVC   SVOVRLAY,OVERLAY     AND OVERLAY #                               
         MVC   SVAPMODE,APMODE      AND APMODE SETTING                          
         LA    R0,LASTS             AND THIS TIME'S VALUES                      
         LA    R1,LASTSL                                                        
         LA    RE,THESE                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R0,SVDBKVAL          AND THIS DBLOCK KEY VALUES                  
         LA    R1,SVDBKVLQ                                                      
         LA    RE,DBKVALS                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
STCTL345 DS    0H                  SAVE TSAR BUFFER WHILE EXITING               
         MVI   TSACTN,TSASAV                                                    
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0            ANY ERRORS?                                  
         BE    STCTLX                                                           
         DC    H'0'                 AFRAID SO!                                  
*                                                                               
STCTL350 DS    0H                                                               
         MVI   APMODE,STERTASK     GIVE APPLICATION A CHANCE                    
         MVI   GOSTEON,SAPC#                                                    
         GOTO1 AGOSTEO                                                          
                                                                                
         NI    OUPTFLG1,XFF-OF1RELSE  NO MORE TO RELEASE                        
         TM    OUPTFLG1,OF1RCTX    ARE ALL COMBO OF INPUT EXHAUSTED?            
         BZ    STCTL360                                                         
         OI    OUPTFLG1,OF1DONE     YES, THE OUTPUTTING IS DONE                 
         MVI   CHAR,STSPIEOI         AND PUT END-OF-DATA CHARACTER              
         MVI   GOSTEON,PCH#          ONTO SCREEN                                
         GOTO1 AGOSTEO                                                          
         MVI   STMODE,STMNXTQ      SET STEREO MODE TO NEXT REQUEST              
         B     STCTLX                                                           
*                                                                               
STCTL360 DS    0H                  SET TO PROCESS NEXT INPUT COMBO              
         MVI   GOSTEON,SSCR#        SAVE OUTPUT SCREEN                          
         GOTO1 AGOSTEO                                                          
         OI    OUPTFLG1,OF1SVSCR    REMEMBER THAT SCREEN WAS SAVED              
         MVI   STMODE,STMMIDQ       DO MIDDLE MODE AGAIN                        
         B     STCTL200                                                         
         EJECT                                                                  
STCTL400 DS    0H                                                               
         MVC   TSAREC,ATSIOREC     SET ADDRESS OF TSAR RECORD                   
         MVC   TSACOM,AFAC          AND A(COMFACS)                              
*                                                                               
STCTL410 DS    0H                                                               
         TM    OUPTFLG1,OF1NEW     OUTPUTTING A NEW STEREO REQUEST?             
         BO    STCTL412             YEP                                         
         TM    OUPTFLG1,OF1RESUM   RESUME SEQUENCE                              
         BO    STCTL414             YEP                                         
         TM    OUPTFLG1,OF1SMREQ   OUTPUTTING SAME REQUEST ?                    
         BO    STCTL416             YEP                                         
         DC    H'0'                                                             
*                                                                               
STCTL412 DS    0H                  OUTPUTTING NEW STEREO REQUEST                
         XC    TSRNUM,TSRNUM        SET TO READ 1ST TSAR DEMO RECD              
         NI    OUPTFLG1,XFF-OF1NEW  STREO RQUST NOT NEW NXT TIME AROUND         
         B     STCTL420                                                         
                                                                                
*                                                                               
STCTL414 DS    0H                  RESUME SEQUENCE                              
         NI    OUPTFLG1,XFF-OF1RESUM                                            
         ZICM  R1,WHEREBRK,(1)      PICK UP FROM LAST LEFT OFF                  
         BZ    STCTL420             OTHERWISE, GO TO DEFAULT PLACE              
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     STC41400(R1)                                                     
                                                                                
STC41400 DS    0H                                                               
STC41401 B     STCTL420                    BREAK OCCURRED DURING DOWNLD         
STC41402 B     STCTL450                    REACHED EOF IN TSAR BUFFER           
         DC    H'0'                                                             
                                                                                
WBODWNLD EQU   ((STC41401-STC41400)/4)+1   BREAK OCCURRED DURING DOWNLD         
WBOTSEOF EQU   ((STC41402-STC41400)/4)+1   HAVE REACHED EOF IN TSAR             
                                                                                
*                                                                               
STCTL416 DS    0H                  (STEREO) SCREEN PREVIOUSLY SAVED             
         NI    OUPTFLG1,XFF-OF1SMREQ  TURH OFF SAME REQ FLAG                    
         XC    TSRNUM,TSRNUM          START W/ 1ST RECD IN TSAR                 
                                                                                
*                                                                               
STCTL420 DS    0H                  READ NEXT RECORD                             
         CLI   APMODE,SAMEREC       IF APPLICATION ASKED FOR SAME RECD,         
         BE    STCTL425              DON'T READ NEXT RECORD                     
         LH    R1,TSRNUM                                                        
         LA    R1,1(R1)                                                         
         STH   R1,TSRNUM                                                        
                                                                                
STCTL425 DS    0H                                                               
         MVI   TSACTN,TSAGET                                                    
         GOTO1 VTSAR,TSARD                                                      
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    STCTL450             YEP, GO DO NEXT INPUT COMBO                 
*                                                                               
STCTL430 DS    0H                  CALL APPLICATION TO EXTRACT OUTPUT           
                                                                                
STCTL432 DS    0H                  APPLICATION CALL                             
         MVI   APMODE,FORMLINE                                                  
         MVI   GOSTEON,SAPC#                                                    
         GOTO1 AGOSTEO                                                          
         TM    MISCFLG1,MF1ERROR   ERROR OCCURRED DURING APPLIC CALL?           
         BNZ   STCTLXL              YEP, EXIT W/ CC LOW                         
         TM    BRKFLAG1,BF1NEEDB   DO WE NEED TO BREAK?                         
         BZ    STCTL432X            NOPE                                        
         TM    BRKFLAG1,BF1FALNK    YEP, DID FALINK CALL FOR IT?                
         BZ    *+12                  NOPE                                       
         MVI   WHEREBRK,WBODWNLD     YEP, REMEMBER WHERE WE ARE,                
         B     STCTLXE                AND EXIT W/ CC EQUAL                      
STCTL432X EQU  *                                                                
                                                                                
         NI    OUPTFLG1,XFF-OF1RESUM  IF WE GET HERE, TURN OFF RSM FLAG         
         CLI   APMODE,NEXTREC      IF APPLICATION ASKED FOR NEXT RECD,          
         BE    STCTL438             DON'T OUTPUT DATA IN BUFFER YET             
*                                                                               
STCTL438 DS    0H                  NO MORE DATA TO PUT TO SCREEN                
         B     STCTL420            GET NEXT TSAR DEMO RECORD                    
*                                                                               
STCTL440 DS    0H                  SCREEN FULL (W/ OUTPUT DATA)                 
         MVC   SVOVRLAY,OVERLAY     AND OVERLAY #                               
         MVC   SVAPMODE,APMODE      AND APMODE SETTING                          
         LA    R0,LASTS             AND THIS TIME'S VALUES                      
         LA    R1,LASTSL                                                        
         LA    RE,THESE                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R0,SVDBKVAL          AND THIS DBLOCK KEY VALUES                  
         LA    R1,SVDBKVLQ                                                      
         LA    RE,DBKVALS                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
STCTL445 DS    0H                  SAVE TSAR BUFFER WHILE EXITING               
         MVI   TSACTN,TSASAV                                                    
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0            ANY ERRORS?                                  
         BE    STCTLXE                                                          
         DC    H'0'                 AFRAID SO!                                  
*                                                                               
STCTL450 DS    0H                                                               
         MVI   APMODE,STERTASK     GIVE APPLICATION A CHANCE                    
         MVI   GOSTEON,SAPC#                                                    
         GOTO1 AGOSTEO                                                          
                                                                                
         TM    BRKFLAG1,BF1APPLC   DID APPLICATION ASK FOR A BREAK?             
         BZ    *+12                 NOPE, KEEP PLUGGING ALONG                   
         MVI   WHEREBRK,WBOTSEOF                                                
         B     STCTLBRK             YEP, DO THAT NOW                            
                                                                                
         NI    OUPTFLG1,XFF-OF1RELSE  NO MORE TO RELEASE                        
         TM    OUPTFLG1,OF1RCTX    ARE ALL COMBO OF INPUT EXHAUSTED?            
         BZ    STCTL460                                                         
         OI    OUPTFLG1,OF1DONE     YES, THE OUTPUTTING IS DONE                 
         MVI   ADMODE,ADMDONE        AND SIGNAL DONE W/ DOWNLOADING             
         GOTO1 ADM32ADD                                                         
         MVI   STMODE,STMNXTQ      SET STEREO MODE TO NEXT REQUEST              
         B     STCTL500                                                         
*                                                                               
STCTL460 DS    0H                  SET TO PROCESS NEXT INPUT COMBO              
         OI    OUPTFLG1,OF1SMREQ    TURN SAME REQUEST FLAG                      
         MVI   STMODE,STMMIDQ       DO MIDDLE MODE AGAIN                        
         B     STCTL200                                                         
         DROP  R4                                                               
*                                                                               
** STEREO SESSION PREPARE-FOR-NEXT-REQUEST MODE **                              
*                                                                               
STCTL500 DS    0H                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    STCTL530                                                         
*                                                                               
         CLI   SCREEN,STEOSCRN     SHOULD STILL BE STEREO SCREEN                
         BE    STCTL520                                                         
         MVI   SCREEN,STEOSCRN     IF NOT, LOAD IT IN                           
         MVI   GOSTEON,LSC#                                                     
         GOTO1 AGOSTEO                                                          
*                                                                               
STCTL520 DS    0H                                                               
         XC    STEMSG,STEMSG       CLEAR & TRANSMIT MESSAGE LINE                
         OI    STEMSGH+6,X80                                                    
                                                                                
*                                                                               
STCTL530 DS    0H                                                               
         MVI   GOSTEON,CLS#        CLEAR STEREO SCREEN                          
         GOTO1 AGOSTEO                                                          
                                                                                
         MVI   OUPTFLG1,0          CLEAR OUT ALL OUTPUT FLAGS                   
         MVI   INPTFLG1,IF1NEW     SET INPUT FLAG TO NEW REQUEST                
         MVI   STMODE,STMINPQ      SET STEREO MODE TO INPUT                     
                                                                                
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    *+8                                                              
         OI    STEI1STH+6,X80+X40  POSITION CURSOR AND TRANSMIT                 
         B     STCTLXE                                                          
         EJECT                                                                  
*                                                                               
** REQUEST FOR A FALINK BREAK **                                                
*                                                                               
STCTLBRK DS    0H                                                               
         MVI   ADMODE,ADMBRK                                                    
         GOTO1 ADM32ADD                                                         
         B     STCTLX                                                           
                                                                                
                                                                                
STCTLX   DS    0H                  EXIT STEREO CONTROLLER                       
         B     XIT_STE                                                          
                                                                                
STCTLXL  DS    0H                  EXIT STEREO CONTROLLER W/ CC LOW             
         J     EXITL                                                            
                                                                                
STCTLXE  DS    0H                  EXIT STEREO CONTROLLER W/ CC EQUAL           
         J     EXITE                                                            
                                                                                
STCTLXH  DS    0H                  EXIT STEREO CONTROLLER W/ CC HIGH            
         J     EXITH                                                            
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--STINP#)'                     
*------------------------- INPUT FROM STEREO -------------------------*         
                                                                                
STINPUT  DS    0H                                                               
         TM    INPTFLG1,IF1NEW     IF NEW REQUEST FROM STEREO,                  
         BZ    STINP10                                                          
         MVI   GOSTEON,STIIN#       INITIALIZE FOR NEW INPUT TO COME            
         GOTO1 AGOSTEO                                                          
         NI    INPTFLG1,XFF-IF1NEW                                              
*                                                                               
STINP10  DS    0H                  INITIALIZE SOME ADCONS & VARIABLES           
         LA    R1,STEI1STH                                                      
         ST    R1,ALINEHDR                                                      
         ST    R1,FADR                                                          
         LA    R1,STEI1ST                                                       
         ST    R1,ALINE                                                         
         ST    R1,ANEXTCHR                                                      
         LA    R1,L'STEI1ST-1(R1)                                               
         ST    R1,AENDLN                                                        
                                                                                
         TM    INPTFLG1,IF1EOSCN   END-OF-SCREEN IN PREV TRANSACTION?           
         BZ    STINP20                 NOPE                                     
         NI    INPTFLG1,XFF-IF1EOSCN   YEP, TURN FLAG OFF                       
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         MVC   TSAREC,AIOAREA             AND RESET ADCONS                      
         MVC   TSACOM,AFAC                                                      
         MVI   TSACTN,TSARES              AND RESTORE TSAR BUFFER               
         GOTO1 VTSAR,(R1)                                                       
         DROP  R1                                                               
*&&DO                                                                           
                                                                                
         MVI   GOSTEON,RTI#               RESTORE TIA STORAGE ALSO              
         GOTO1 AGOSTEO                                                          
*&&                                                                             
         B     STINP22                                                          
*                                                                               
STINP20  DS    0H                  READ INPUT FOR NEW KEYWORD & DATA            
         XC    IODATALN,IODATALN                                                
         L     R0,ASTIOBUF                                                      
         LH    R1,LSTIOBUF                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR STEREO I/O BUFFER                      
                                                                                
*                                                                               
STINP22  DS    0H                                                               
         LA    R1,STEI1STH         CHECK IF STEREO HIT ENTER W/ NO DATA         
         SR    R0,R0                                                            
STINP22B ICM   R0,1,0(R1)           IF END OF SCREEN REACHED,                   
         BNZ   STINP22D                                                         
         OI    INPTFLG1,IF1EOSCN     FLAG IT, AND                               
         B     STINP100              SAVE TSAR BUFFER & EXIT                    
STINP22D CLI   5(R1),0              ANY INPUT HERE?                             
         BH    STINP24               YES, GET INPUT FROM STEREO                 
         AR    R1,R0                 NO, LOOK FOR DATA ON NEXT LINE             
         B     STINP22B                                                         
                                                                                
*                                                                               
STINP24  DS    0H                                                               
         TM    INPTFLG1,IF1EOSCN   DID WE REACH END-OF-SCREEN?                  
         BO    STINP100             YES, SAVE TSAR BUFFER & EXIT                
                                                                                
         MVI   GOSTEON,GCH#        GET CHARACTER FROM SCREEN                    
         GOTO1 AGOSTEO                                                          
                                                                                
         L     RF,ASTACTIN          PUT CHAR INTO ACTUAL INPUT AREA             
         ICM   R1,15,0(RF)           R1 = L(ACTUAL INPUT SO FAR)                
         LA    R0,L'CHAR(R1)                                                    
         CH    R0,LSTACTIN           REACHED MAX CAPACITY?                      
         BNH   *+12                                                             
         MVI   FERN,120               YEP, PUT OUT ERROR MESSAGE                
         B     STINPX                                                           
         LA    RE,L'STACTINH(R1,RF)   NOPE                                      
         MVC   0(L'CHAR,RE),CHAR       PUT ACTUAL CHAR INPUTTED IN AREA         
         LA    R1,L'CHAR(R1)                                                    
         STCM  R1,15,0(RF)             UPDATE L(ACTUAL INPUT)                   
                                                                                
         CLI   CHAR,STSPIEOI        END OF INPUT?                               
         BE    STINP60                                                          
         CLI   CHAR,STSPIKEY        KEYWORD SEPARATOR?                          
         BE    STINP30                                                          
                                                                                
         LH    RF,IODATALN                                                      
         CH    RF,LSTIOBUF                                                      
         BL    STINP26                                                          
         MVI   FERN,120           ENQUIRY TOO LARGE TO PROCESS                  
         B     STINPX             EXIT TO HANDLE ERROR                          
                                                                                
STINP26  DS    0H                 L(INPUT) < MAX BUFF SPACE                     
         L     R1,AUPCSETB                                                      
         TR    CHAR,0(R1)          TRANSLATE TO UPPER CASE                      
         LA    RE,1(RF)                                                         
         STH   RE,IODATALN         UPDATE LENGTH OF INPUT                       
         A     RF,ASTIOBUF                                                      
         MVC   0(L'CHAR,RF),CHAR   MOVE CHARACTER INTO INPUT BUFFER             
         B     STINP24                                                          
                                                                                
STINP30  DS    0H                  KEYWORD SEPARATOR WAS READ                   
         L     R3,ASTIOBUF         VALIDATE KEYWORD                             
         MVI   WORK,STSPIASN                                                    
         XC    WORK+1(2),WORK+1                                                 
         LH    R0,IODATALN         R0=L(INPUT)                                  
         ST    R0,FULL2                                                         
         BAS   RE,GETFLDSE         GO "EXTRACT" KEYWORD                         
                                                                                
         LR    R1,R3                                                            
         SR    R1,R2               R1=L("EXTRACTED" KEYWORD)                    
         BP    STINP32                                                          
         BZ    *+6                 IF LENGTH IS ZERO,                           
         DC    H'0'                                                             
         MVI   FERN,SERRSTEQ        EXIT W/ STEREO ERROR                        
         B     STINPX                                                           
                                                                                
STINP32  DS    0H                  LOOK-UP INPUT KEYWORD IN TABLE               
         XC    AKEYNTRY,AKEYNTRY                                                
         LR    R0,R1               SAVE L(INPUT) AROUND                         
         BCTR  R1,0                SET EX LENGTH NOW                            
         L     RF,ASTROIKY         RF-->TABLE VALID INPUT KEYWORDS              
         USING STROIKYD,RF                                                      
STINP32A CLI   0(RF),EOT                                                        
         BE    STINP32X                                                         
         ZIC   RE,STKFMXLN         GET APPROPRIATE MAX LENGTH OF FIELD          
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BNO   *+8                                                              
         IC    RE,STKFMXL2                                                      
         CR    R0,RE               COMPARE INPUT LEN TO MAX FLD LEN             
         BH    STINP32B             CAN'T BE THIS ENTRY IF HIGH                 
         ZICM  RE,STKDKYWD,(3)                                                  
         LA    RE,DEMTWAD(RE)                                                   
         EXCLC R1,0(R2),0(RE)      INPUT MATCHES TABLE?                         
         BNE   STINP32B             NO, TRY NEXT ENTRY                          
         ST    RF,AKEYNTRY          YES, SAVE A(ENTRY)                          
         B     STINP32X              AND EXIT LOOP                              
STINP32B LA    RF,STROIKYQ(RF)     BUMP TABLE POINTER                           
         B     STINP32A                                                         
STINP32X DS    0H                                                               
                                                                                
         OC    AKEYNTRY,AKEYNTRY   IF THIS FIELD IS NOT SET,                    
         BNZ   *+12                                                             
         MVI   FERN,SERRIKWQ        INVALID STEREO KEYWORD                      
         B     STINPX                                                           
                                                                                
         DS    0H                  SET DATA COUNTER ENTRY FOR KEYWORD           
         LA    R4,STINCTRS                                                      
         USING STINCTRD,R4                                                      
STINP34A CLI   0(R4),0             END-OF-TABLE REACHED?                        
         BE    STINP34C                                                         
         CLC   STINCKNU,STKNUMB    SEE IF KEYWORD DUPLICATED                    
         BNE   STINP34B                                                         
         MVI   FERN,SERRDKWQ        YES, SET KYWRD DUPLCTD ERR CODE             
         B     STINPX                AND EXIT                                   
STINP34B LA    R4,STINCTRQ(R4)                                                  
         B     STINP34A                                                         
STINP34C MVC   STINCKNU,STKNUMB    CREATE AN ENTRY FOR THIS KEYWORD             
         DROP  RF                                                               
                                                                                
STINP40  DS    0H                  VALIDATE DATA INPUT FOR KEYWORD              
         ICM   RF,15,FULL2         ANY MORE INPUT IN BUFFER?                    
         BNZ   STINP40C             YES                                         
         CLI   STINCDCT,0           NOPE, SEE IF KEYWORD HAS INPUT              
         BH    STINP20               YES, KEYWORD HAS INPUT                     
         MVI   FERN,SERRSTEQ         NO, KEYWORD HAS NO INPUT==>ERROR           
         B     STINPX                                                           
STINP40C EQU   *                                                                
         BCTR  RF,0                                                             
         ST    RF,FULL2                                                         
                                                                                
         LA    R3,1(R3)            POINT R3 TO START OF INPUT DATA              
         MVI   WORK,STSPIFLD        AND SET DELIMITER CHARACTER                 
         MVI   WORK+1,STSPSOPI      AND START                                   
         MVI   WORK+2,STSPEOPI       & END NEST INDICATORS                      
         BAS   RE,GETFLDSE         EXTRACT FIELD                                
         LR    R1,R3                                                            
STINP40G DS    0H                                                               
         BCTR  R1,0                LOOK FOR TRAILING BLANKS                     
         CR    R2,R1                MAKE SURE NOT PAST BEGINNING                
         BH    STINP40                                                          
         CLI   0(R1),C' '           CHECK FOR BLANKS OR "FUNNY" CHARS           
         BNH   STINP40G                                                         
         SR    R1,R2               R1=L(INPUT DATA)-1                           
                                                                                
         DS    0H                  BUILD TSAR RECORD                            
         L     RE,AIOAREA                                                       
         LA    RE,2(RE)                                                         
         USING STICKEYD,RE                                                      
         L     RF,AKEYNTRY                                                      
         USING STROIKYD,RF                                                      
         MVC   STICKKNU,STKNUMB    INTERNAL KEYWORD #                           
         DROP  RF                                                               
         ZIC   RF,STINCDCT         RF=# OF DATA VALUES FOR KEYWORD              
         LA    RF,1(RF)                                                         
         STC   RF,STINCDCT         RF=NEW # OF DATA VALUES FOR KEYWORD          
         STC   RF,STICKNTH         NTH DATA VALUE FOR KEYWORD                   
         EXMVC R1,STICDATA,0(R2)                                                
         LA    R1,STICFIXL+1(R1)                                                
         STC   R1,STICLEN          L(RECORD)                                    
         LA    R1,2(R1)                                                         
         DROP  RE                                                               
                                                                                
         CH    R1,=Y(STMXRECL)     COMPARE AGAINST MAX L(TSAR RECORD)           
         BNH   STINP45                                                          
         MVI   FERN,120            ENQUIRY IS TOO LARGE TO PROCESS              
         B     STINPX              EXIT TO HANDLE ERROR                         
                                                                                
STINP45  DS    0H                                                               
         L     RE,AIOAREA                                                       
         STH   R1,0(RE)            L(TSAR RECORD)                               
         DROP  R4                                                               
                                                                                
         DS    0H                  ADD RECORD TO BUFFER                         
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR               ADD TSAR RECORD                              
         CLI   TSERRS,0            ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                 SHOULDN'T BE                                
         DROP  R1                                                               
         B     STINP40             GET NEXT DATA VALUE FOR KEYWORD              
*                                                                               
STINP60  DS    0H                  TRANSFER STEREO INPUT TO CHUNK               
         OI    INPTFLG1,IF1DONE    INPUT FROM STEREO IS ALL READ                
         L     R2,ASTINCHK         R2-->CHUNK AREA                              
                                                                                
         L     R3,AIOAREA          R3-->I/O AREA FOR TSAR RECORDS               
         LA    R3,2(R3)                                                         
         USING STICKEYD,R3                                                      
         XC    0(STICKEYL,R3),0(R3)   CLEAR TO READ 1ST RECORD                  
                                                                                
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         MVI   TSACTN,TSARDH                                                    
         XC    TSRNUM,TSRNUM       CLEAR FOR READ HIGH                          
                                                                                
STINP62  DS    0H                                                               
         GOTO1 VTSAR,(R4)                                                       
         TM    TSERRS,TSEEOF       EOF?                                         
         BO    STINP100             YES, GO EXIT                                
         TM    TSERRS,TSERNF       RECORD NOT FOUND?                            
         BO    STINP64              IGNORE SINCE READING HIGH                   
         CLI   TSERRS,0            OTHERWISE, DON'T WANT ANY ERRORS             
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
STINP64  DS    0H                  R3-->TSAR RECORD                             
         ZIC   R1,STICLEN                                                       
                                                                                
         LR    RE,R2               MAKE SURE ENOUGH ROOM IN CHUNK               
         AR    RE,R1                FOR THIS INPUT CHUNK                        
         S     RE,ASTINCHK                                                      
         CH    RE,=Y(STMXCHKL)                                                  
         BNH   STINP66                                                          
         MVI   FERN,120            ENQUIRY TOO LARGE TO PROCESS                 
         B     STINPX              EXIT TO HANDLE ERROR                         
                                                                                
STINP66  DS    0H                  MOVE INPUT CHUNK INTO STORAGE                
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),STICKEYD                                                
         LA    R2,1(R2,R1)         BUMP TO NEXT AVAIL SLOT IN CHUNK             
         MVI   TSACTN,TSANXT                                                    
         B     STINP62                                                          
         DROP  R3,R4                                                            
*                                                                               
STINP100 DS    0H                                                               
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSASAV       SAVE TSAR BUFFER                             
         GOTO1 VTSAR               ADD TSAR RECORD                              
         CLI   TSERRS,0            ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                 SHOULDN'T BE                                
         DROP  R1                                                               
*                                                                               
         DS    0H                  SAVE STEREO INPUT INTO TEMPSTR               
         MVI   GOSTEON,STI#                                                     
         GOTO1 AGOSTEO                                                          
*                                                                               
STINPX   DS    0H                                                               
         B     XIT_STE                                                          
                                                                                
                                                                                
* A little routine to "extract" a field by pointing R2 to the start of          
*  a field and R3 to one byte past last byte of field.                          
* At entry,                                                                     
*  R3       -->start byte of field,                                             
*  WORK(1)   = delimiter character,                                             
*  WORK+1(2) = start- & end-nesting indicators, respectively,                   
*  FULL2     = length of input field.                                           
         DS    0H                                                               
GETFLDSE NTR1                                                                   
         LR    R2,R3                                                            
                                                                                
GFSE10   DS    0H                                                               
         OC    FULL2,FULL2         ANY MORE INPUT?                              
         BZ    GFSEX                NO, EXIT NOW                                
         ZAP   NESTLVL,=P'0'                                                    
*                                                                               
GFSE20   DS    0H                                                               
         CLC   NESTLVL,=P'0'                                                    
         BNL   *+6                                                              
         DC    H'0'                CRAP IF NEGATIVE                             
         BE    GFSE22              CURRENTLY NOT IN A NEST                      
         BH    GFSE24              CURRENTLY IN A NEST                          
                                                                                
GFSE22   DS    0H                  NEST LEVEL = 0                               
         CLC   0(1,R3),WORK+1      MATCH TO START-OF-NESTING INDICATOR          
         BNE   GFSE26               NOPE                                        
         AP    NESTLVL,=P'1'        YES, INCREMENT NEST LEVEL                   
         B     GFSE28                                                           
                                                                                
GFSE24   DS    0H                  NEST LEVEL > 0                               
         CLC   0(1,R3),WORK+2      MATCH TO END-OF-NESTING INDICATOR            
         BNE   GFSE28               NOPE                                        
         SP    NESTLVL,=P'1'        YES, DECREMENT NEST LEVEL                   
         B     GFSE28                                                           
                                                                                
GFSE26   DS    0H                                                               
         CLC   0(1,R3),WORK        LOOK FOR DELIMITER CHARACTER                 
         BE    GFSEX                FOUND IT, WE'RE DONE                        
                                                                                
GFSE28   DS    0H                  BUMP TO NEXT CHARACTER                       
         LA    R3,1(R3)                                                         
         L     R1,FULL2                                                         
         BCTR  R1,0                                                             
         ST    R1,FULL2                                                         
         LTR   R1,R1                                                            
         BZ    GFSEX               EXIT IF NO MORE INPUT LEFT                   
         BP    GFSE20                                                           
         DC    H'0'                                                             
*                                                                               
GFSEX    DS    0H                                                               
         XIT1  REGS=(R2,R3)                                                     
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--STOUT#)'                     
*-------------------------- OUTPUT TO STEREO -------------------------*         
                                                                                
STOUTPUT DS    0H                                                               
         B     XIT_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--STIIN#)'                     
*------------------------ INITIALIZE FOR INPUT -----------------------*         
                                                                                
* Does some initialization for a new request from STEREO.                       
                                                                                
STININ   DS    0H                                                               
         LA    RF,STTBCLRQ         CLEAR SAVED STORAGE AREAS                    
         L     R4,=A(STTABCLR-DEM00)                                            
         A     R4,ABASE                                                         
STIIN15  ZICM  R0,1(R4),(3)                                                     
         L     RE,ATWA                                                          
         CLI   0(R4),C'T'                                                       
         BE    STIIN15G                                                         
         L     RE,ATIA                                                          
         CLI   0(R4),C'I'                                                       
         BE    STIIN15G                                                         
         DC    H'0'                                                             
STIIN15G EQU   *                                                                
         AR    R0,RE               R0-->AREA TO CLEAR                           
         ZICM  R1,3(R4),(3)        R1=LENGTH TO CLEAR                           
         BAS   RE,MVCLR0R2         GO CLEAR AREA                                
         LA    R4,L'STTABCLR(R4)                                                
         BCT   RF,STIIN15                                                       
         B     STIIN20                                                          
*                                                                               
MVCLR0R2 DS    0H                  CLEARS AREA DEFINED BY R0 & R1               
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R0,R2               CLEAR AREA                                   
         BR    RE                                                               
*                                                                               
STIIN20  DS    0H                                                               
         NI    STFLAG1,XFF-SF1MDFRL                                             
                                                                                
         DS    0H                                                               
         XC    TSARBLCK,TSARBLCK                                                
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAINI       INITIALIZATION ACTION                        
         MVC   TSAREC,AIOAREA      ADDRESS OF TSAR RECORD                       
         MVC   TSACOM,AFAC         A(COMFACS)                                   
         MVI   TSPAGL,1            START W/ PAGE 1 OF TEMPSTR                   
         MVI   TSPAGN,1            USE ONLY ONE PAGE OF TEMPSTR                 
         OI    TSRECI,TSRVAR       VARIABLE LENGTH RECORDS                      
         MVI   TSKEYL,STICKEYL     L(TSAR KEY)                                  
         MVC   TSRECL,=Y(STMXRECL) MAX LENGTH OF TSAR RECORD                    
         OI    TSINDS,TSIREUSE+TSIXTTWA                                         
                                                                                
         GOTO1 VTSAR               GO INITIALIZE TSAR                           
         CLI   TSERRS,0            ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                 SHOULDN'T BE                                
         TM    TSINDS,TSIINIOK     INITIALIZATION OK?                           
         BO    *+6                                                              
         DC    H'0'                 HOPE SO                                     
         DROP  R1                                                               
                                                                                
*                                                                               
         DS    0H                  MORE FLAGS TO CLEAR                          
         NI    BRKFLAG1,XFF-(BF1FALNK+BF1APPLC+BF1PRCSS)                        
         NI    RSMFLAG1,XFF-(RF1FALNK+RF1APPLC+RF1PRCSS)                        
         MVI   WHEREBRK,0                                                       
                                                                                
         B     XIT_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--GCH#)'                       
*--------------------------- GET CHARACTER ---------------------------*         
                                                                                
* At entry,                                                                     
*   ALINEHDR = A(field header of current line)                                  
*   ALINE    = A(current line)                                                  
*   AENDLN   = A(end of current line)                                           
*   AENDSCN  = A(end of screen)                                                 
*   ANEXTCHR = A(input character to get)                                        
* At exit,                                                                      
*   ALINEHDR = A(field header of line for next time around)                     
*   ALINE    = A(line for next time around)                                     
*   AENDLN   = A(line end for next time around)                                 
*   ANEXTCHR = A(next input character to get)                                   
                                                                                
GETCHAR  DS    0H                                                               
         ICM   RF,15,ANEXTCHR      RF-->CHAR TO FETCH                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   CHAR,0(RF)                                                       
*                                                                               
         DS    0H                  UPDATE ADDRESSES FOR NEXT TIME               
         C     RF,AENDSCN          ARE WE AT END-OF-SCREEN ALREADY?             
         BL    GCH12                                                            
         OI    INPTFLG1,IF1EOSCN    YES, FLAG IT                                
         SR    RF,RF                 AND ANEXTCHR CAN'T BE DEFINED              
         B     GCH20                                                            
                                                                                
GCH12    DS    0H                                                               
         C     RF,AENDLN           ARE WE AT END-OF-LINE?                       
         BL    GCH14                                                            
         L     RF,ALINEHDR          YES, BUMP TO NEXT LINE                      
         ZIC   R1,0(RF)                                                         
         AR    RF,R1               RF-->NEXT LINE'S FIELD HEADER                
         ST    RF,ALINEHDR                                                      
         ST    RF,FADR                                                          
         LA    RF,8(RF)                                                         
         ST    RF,ALINE            RF-->NEXT LINE                               
         LA    R1,L'STEI1ST-1(RF)                                               
         ST    R1,AENDLN                                                        
         B     GCH20                                                            
                                                                                
GCH14    DS    0H                                                               
         LA    RF,1(RF)                                                         
         B     GCH20                                                            
*                                                                               
GCH20    DS    0H                  BY THIS TIME, RF-->NEXT CHAR TO GET          
         ST    RF,ANEXTCHR                                                      
                                                                                
         B     XIT_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--LSC#)'                       
*----------------------------- LOAD SCREEN ---------------------------*         
                                                                                
* Loads screen into TWA.                                                        
* At entry, SCREEN = screen number to load in.                                  
                                                                                
LOADSCRN DS    0H                                                               
                                                                                
         L     R1,ATWA                                                          
         LA    R2,64(R1)           R2=A(LOCATION TO LOAD SCREEN)                
         LR    R0,R2                                                            
         LA    R1,SCRNLEN                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR AREA WHERE SCREEN WILL BE              
                                                                                
         GOTO1 VCALLOV,DMCB,(SCREEN,(R2)),0                                     
         CLI   DMCB+4,X'FF'        ANY ERRORS?                                  
         BNE   *+6                                                              
         DC    H'0'                 YEP, SOMETHING'S AMISS                      
         MVC   SVSCREEN,SCREEN                                                  
                                                                                
         LR    RE,R2               TRANSMIT ALL FIELDS OF SCREEN                
         SR    RF,RF                                                            
LSC20    CLI   0(RE),0                                                          
         BE    LSC25                                                            
         IC    RF,0(RE)                                                         
         AR    RE,RF                                                            
         B     LSC20                                                            
LSC25    MVC   1(2,RE),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
                                                                                
         B     XIT_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--CLS#)'                       
*---------------------------- CLEAR SCREEN ---------------------------*         
                                                                                
* Clears unprotected fields of a screen, excluding service request fld.         
                                                                                
CLRSCRN  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    CLRDEM32                                                         
         LA    R2,DEMSRVH                                                       
         B     CLS20               SKIP SERVICE REQUEST FIELD                   
*                                                                               
CLS10    DS    0H                                                               
         ZIC   R1,0(R2)            R1=TOTAL LENGTH OF FIELD                     
         SH    R1,=H'8'            LESS HEADER                                  
         TM    1(R2),X02                                                        
         BZ    *+8                                                              
         SH    R1,=H'8'            LESS EXT FLD HDR (IF ANY)                    
         BCTR  R1,0                AND ADJUST FOR EX INSTRUCTION                
*                                                                               
         EX    R1,*+4                                                           
         XC    8(0,R2),8(R2)       CLEAR DATA FIELD                             
         MVI   5(R2),0             SET INPUT LEN                                
         MVI   7(R2),0             AND OUTPUT LEN TO ZERO                       
                                                                                
CLS20    DS    0H                                                               
         BAS   RE,CLSBUMP          DO NEXT FIELD                                
         BNE   CLS10                                                            
                                                                                
         MVC   1(2,R2),=X'0101'    TRANSMIT ENTIRE SCREEN                       
         B     XIT_STE                                                          
                                                                                
                                                                                
CLSBUMP  DS    0H                  BUMP TO NEXT UNPROTECTED FIELD               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BER   RE                  RETURN W/ CC EQUAL                           
         TM    1(R2),X20                                                        
         BO    CLSBUMP             PROT FIELD--KEEP BUMPING                     
         CLI   0(R2),0                                                          
         BR    RE                  RETURN W/ CC NOT-EQUAL                       
*                                                                               
CLRDEM32 DS    0H                                                               
         XC    DUMACT,DUMACT                                                    
         MVI   DUMACTH+5,0                                                      
         XC    DUMFIL,DUMFIL                                                    
         MVI   DUMFILH+5,0                                                      
         XC    DUMSRC,DUMSRC                                                    
         MVI   DUMSRCH+5,0                                                      
         XC    DUMSTN,DUMSTN                                                    
         MVI   DUMSTNH+5,0                                                      
         XC    DUMBOK,DUMBOK                                                    
         MVI   DUMBOKH+5,0                                                      
         XC    DUMDAT,DUMDAT                                                    
         MVI   DUMDATH+5,0                                                      
         XC    DUMDEM,DUMDEM                                                    
         MVI   DUMDEMH+5,0                                                      
         XC    DUMOPT,DUMOPT                                                    
         MVI   DUMOPTH+5,0                                                      
         B     XIT_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--BRCT#)'                      
*-------------------- BUILD REQUEST CONTROL TABLE --------------------*         
                                                                                
* Builds the Request Control Table which controls the formatting of             
*  the inputs onto the screen.                                                  
                                                                                
BLDRCT   DS    0H                                                               
         L     R2,ASTROIKY         R2-->TABLE OF KEYWORDS                       
         USING STROIKYD,R2                                                      
         LA    R3,REQCTLTB         R3-->EMPTY REQ CTL TABLE                     
         USING RCTDSECT,R3                                                      
*                                                                               
BRCT10   DS    0H                                                               
         CLI   0(R2),EOT           AT END OF KEYWORD TABLE?                     
         BE    BRCTX                YEP, EXIT NOW                               
         TM    STKFLAG1,STKF1SCN   KEYWORD ON $DEM SCREEN?                      
         BO    BRCT20               YEP, ADD TO REQ CTL TABLE                   
                                                                                
BRCT15   DS    0H                  BUMP TO NEXT ENTRY IN KEYWORD TABLE          
         LA    R2,STROIKYQ(R2)                                                  
         B     BRCT10                                                           
*                                                                               
BRCT20   DS    0H                  KEYWORD ON $DEM SCREEN                       
         LA    R4,STINCTRS         LOCATE DATA COUNTER NTRY FOR KYWRD           
         USING STINCTRD,R4                                                      
         LA    R0,MAXKYWDS         MAX # OF TIMES TO LOOP                       
                                                                                
BRCT22   DS    0H                                                               
         CLI   0(R4),0             END OF DATA COUNTER TABLE?                   
         BE    BRCT15               YEP, NO DATA FOR THIS KEYWORD               
         CLC   STKNUMB,STINCKNU    MATCH ON KEYWORD'S INTERNAL #                
         BE    BRCT24                                                           
         LA    R4,STINCTRQ(R4)                                                  
         BCT   R0,BRCT22                                                        
         B     BRCT15              NO DATA INPUTTED FOR THIS KYWRD              
                                                                                
BRCT24   DS    0H                  ADD ENTRY INTO REQ CTL TABLE                 
         MVC   RCTIKNUM,STINCKNU                                                
         MVI   RCTFROM,1            INITIALIZE FROM AND                         
         MVI   RCTUPTO,1             TO VALUES                                  
         MVC   RCTDCNT,STINCDCT                                                 
                                                                                
         DS    0H                  SET REQCTLTB DISPLACEMENTS                   
         LA    R0,REQCTLTB                                                      
         LR    R1,R3                                                            
         SR    R1,R0               R1 = DISPL INTO REQCTLTB                     
         ZICM  RF,STKDSRCT,(3)                                                  
         LA    RF,DEMTWAD(RF)      RF = A(TO STORE IT)                          
         STCM  R1,3,0(RF)                                                       
         DROP  R4                                                               
                                                                                
         LA    R3,RCTLENQ(R3)                                                   
         B     BRCT15                                                           
*                                                                               
BRCTX    DS    0H                                                               
         B     XIT_STE                                                          
         DROP  R2,R3                                                            
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--FMTS#)'                      
*--------------------- FORMAT REQUEST ONTO SCREEN --------------------*         
                                                                                
* Formats the input from Stereo onto the original $DEM screen.  This            
*  format process is driven by the Request Control Table (REQCTLTB).            
* At exit,                                                                      
*  CC equal ==> no error occurred in formatting,                                
*  CC not equal ==> error occurred.                                             
                                                                                
FMTSCRN  DS    0H                                                               
         LA    R3,REQCTLTB                                                      
         USING RCTDSECT,R3                                                      
*                                                                               
FMTS10   DS    0H                                                               
         CLI   0(R3),0             AT END OF REQ CTL TABLE?                     
         BE    FMTSXY               YEP.                                        
                                                                                
         L     R4,ASTROIKY         LOCATE KEYWORD ENTRY                         
         USING STROIKYD,R4                                                      
FMTS12   CLC   RCTIKNUM,STKNUMB    MATCH W/ INTERNAL KYWRD #                    
         BE    FMTS20                                                           
         LA    R4,STROIKYQ(R4)                                                  
         B     FMTS12                                                           
*                                                                               
FMTS20   DS    0H                  R4-->KEYWORD ENTRY                           
         ZICM  R2,STKFDSPH,(3)     POINT R2 TO KEYWORD'S FIELD                  
         LA    RE,DEMWRKD                                                       
         AR    RE,R2                                                            
         L     R2,0(RE)                                                         
         ZIC   R1,STKFMXLN                                                      
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BNO   *+8                                                              
         IC    R1,STKFMXL2                                                      
         SR    R0,R0                                                            
         ICM   R0,1,5(R2)          IF SOMETHING ALREADY IN FIELD,               
         BZ    FMTS30                                                           
         SR    R1,R0                                                            
         BCTR  R1,0                 SEE IF IT COULD FIT MORE INPUT              
         LTR   R1,R1                (ADJUST LEN FOR A COMMA ',')                
         BZ    FMTS70              CAN'T FIT ANY MORE DATA                      
         BP    FMTS30                                                           
         DC    H'0'                                                             
*                                                                               
FMTS30   DS    0H                  LOCATE DATA VALUE FOR KEYWORD                
         MVC   BYTE,RCTFROM                                                     
                                                                                
FMTS30A  DS    0H                  LOCATE DATA VALUE FOR KEYWORD                
         L     R5,ASTINCHK                                                      
         USING STICKEYD,R5                                                      
                                                                                
FMTS32   DS    0H                                                               
         C     R5,ASTICHKX         CAN'T BE PAST END OF CHUNK STORAGE           
***      BH    *+12                                                             
         BH    FMTS33                                                           
         CLI   0(R5),0             CAN'T BE PAST END OF CHUNK DATA              
**       BNE   *+6                                                              
**       DC    H'0'                                                             
         BNE   FMTS33                                                           
**                                                                              
         MVI   FERN,120            "REQUEST TOO BIG" ERROR MESSAGE              
         MVI   GOSTEON,SERR0#                                                   
         GOTO1 AGOSTEO                                                          
         B     FMTSXY                EXIT NOW W/ ERROR                          
**                                                                              
FMTS33   DS    0H                                                               
         CLC   STICKKNU,RCTIKNUM   MATCH ON INTERNAL KEYWORD # FIRST            
         BL    FMTS44                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   STICKNTH,BYTE       MATCH ON NTH DATA VALUE (IN BYTE)            
         BL    FMTS44                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         BAS   RE,FMTFLD           FORMAT INPUT INTO TWA FIELD                  
         BL    FMTS34               PARTIAL DATA VALUE FORMATTED                
         BE    FMTS40               ENTIRE DATA VALUE FORMATTED                 
         CLI   RCTIKNUM,IKNOPT      COULDN'T FIT DATA VALUE INTO FIELD          
         BNE   FMTS70               IF AN OPT DATA VALUE,                       
         MVI   FERN,SERRTMOV                                                    
         B     FMTSXN                EXIT NOW W/ ERROR                          
                                                                                
FMTS34   DS    0H                                                               
         BRAS  RE,FMTOFLD          FMT REST OF DATA INTO OPT FIELD              
         BNH   FMTS40               DATA FITTED INTO OPTIONS FIELD              
         MVI   FERN,SERRTMOV        COULD NOT FIT DATA INTO OPT FIELD           
         B     FMTSXN                                                           
*                                                                               
FMTS40   DS    0H                                                               
         CLC   RCTUPTO,BYTE        IF UPTO > BYTE, MORE WAS FORMATTED           
         BH    FMTS42               PREVIOUSLY--KEEP FORMATTING                 
         MVC   RCTUPTO,BYTE                                                     
                                                                                
         TM    STKFLAG1,STKF1SBF   CAN WE HAVE MULTIPLE SUB-FIELDS?             
         BZ    FMTS70               NO, GO DO NEXT KEYWORD                      
         CLC   RCTUPTO,RCTDCNT      YES, ANY MORE TO PUT INTO FLD?              
         BNL   FMTS70                NOPE, DATA VALUES ALL EXHAUSTED            
         BAS   RE,CHKMAX             YES, BUT CHECK IF MAXED OUT YET            
         BE    *+20                   CC EQUAL--MORE CAN FIT                    
         CLI   RCTIKNUM,IKNOPT        CC NOT EQUAL--CHECK IF OPT VALUES         
         BNE   FMTS70                  NO, JUST CONTINUE                        
         MVI   FERN,SERRTMOV           YES, FORMATTING ERROR                    
         B     FMTSXN                   EXIT NOW W/ ERROR                       
         BAS   RE,FMTNXT           SHOULD WE FORMAT NEXT VALUE?                 
         BNE   FMTS70               NOPE                                        
         B     FMTS42              PREPARE TO FORMAT NEXT VALUE                 
                                                                                
FMTS42   DS    0H                  INCREMENT BYTE FOR NEXT DATA VALUE           
         ZIC   R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         B     FMTS32                                                           
                                                                                
FMTS44   DS    0H                  BUMP TO NEXT INPUT CHUNK                     
         ZIC   R0,STICLEN                                                       
         AR    R5,R0                                                            
         B     FMTS32                                                           
*                                                                               
FMTS70   DS    0H                  BUMP TO NEXT NTRY IN REQ CTL TABLE           
         LA    R3,RCTLENQ(R3)                                                   
         B     FMTS10                                                           
                                                                                
                                                                                
FMTSXY   DS    0H                                                               
         B     YES_STE                                                          
                                                                                
FMTSXN   DS    0H                  EXIT W/ ERROR MESSAGE SET                    
         MVI   GOSTEON,SERR1#                                                   
         GOTO1 AGOSTEO                                                          
         B     NO_STE                                                           
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
*--------------------- FORMAT DATA INTO TWA FIELD --------------------*         
                                                                                
* At entry,                                                                     
*  R2-->TWA field where input should go,                                        
*  R4-->keyword entry in STROIKY table,                                         
*  R5-->chunk containing the input data.                                        
* At exit,                                                                      
*  CC low   if not all of input formatted into field,                           
*  CC equal if all of input formatted into field,                               
*  CC high  if an error occurred.                                               
* Note: BYTE is used by caller, do not clobber it!                              
                                                                                
         DS    0H                                                               
FMTFLD   NTR1                                                                   
         USING STROIKYD,R4                                                      
         USING STICKEYD,R5                                                      
                                                                                
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         LA    R3,8(RF,R2)         R3-->NEXT AVAILABLE SLOT                     
         BZ    *+8                 CC SET BY  ICM  ABOVE                        
         LA    RF,1(RF)            ADJUST LENGTH FOR COMMA (C',')               
*                                                                               
         DS    0H                  GET L(DATA TO FORMAT)                        
         ZIC   R0,STICLEN                                                       
         SH    R0,=Y(STICFIXL)                                                  
         BZ    FMTFX                (EXITING W/ CC SET)                         
         SR    R1,R1               USE R1 TO HOLD THAT LENGTH                   
         CVD   R1,DUB              DUB--COUNTER USED IN PACKED DECIMAL          
         LA    R6,STICDATA                                                      
         L     R7,AIOAREA1                                                      
                                                                                
FMTF31   DS    0H                                                               
         CLI   0(R6),STSPSOPI                                                   
         BE    FMTF32                                                           
         CLI   0(R6),STSPEOPI                                                   
         BE    FMTF34                                                           
         B     FMTF36                                                           
                                                                                
FMTF32   DS    0H                  STSPSOPI ENCOUNTERED                         
         CP    DUB,=P'1'            IGNORE IF MORE THAN ONE                     
         BE    FMTF36                                                           
         AP    DUB,=P'1'                                                        
         B     FMTF38                                                           
                                                                                
FMTF34   DS    0H                  STSPEOPI ENCOUNTERED                         
         CP    DUB,=P'0'           WAS THERE A MATCHING STSPSOPI?               
         BE    FMTF36               NO, LET IT PASS                             
         SP    DUB,=P'1'                                                        
         B     FMTF38                                                           
                                                                                
FMTF36   DS    0H                  PROCESS CHARACTER                            
         CP    DUB,=P'0'           DOES IT BELONG IN THIS FIELD?                
         BH    FMTF38               NO, SKIP THIS CHARACTER                     
         MVC   0(1,R7),0(R6)        YES, IT BELONGS HERE                        
         LA    R7,1(R7)                                                         
         LA    R1,1(R1)             UP LENGTH BY ONE                            
                                                                                
FMTF38   DS    0H                                                               
         LA    R6,1(R6)                                                         
         BCT   R0,FMTF31                                                        
*                                                                               
         DS    0H                  R1=L(DATA TO FORMAT), IOAREA1=DATA           
*        BAS   RE,FMTFEDT          EDIT DATA (IF NECESSARY)                     
         BRAS  RE,FMTFEDT          EDIT DATA (IF NECESSARY)                     
                                                                                
         AR    RF,R1               RF=NEW POTENTIAL LENGTH                      
         ZIC   RE,STKFMXL2         GET APPROPRIATE MAX LENGTH OF FIELD          
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    *+8                                                              
         IC    RE,STKFMXLN                                                      
         CR    RF,RE               DOES IT FIT IN DATA FIELD?                   
         BH    FMTFX                NO, EXIT W/ CC HIGH                         
                                                                                
         CLI   5(R2),0             IF DATA FIELD NOT EMPTY,                     
         BE    *+12                                                             
         MVI   0(R3),C','           PLACE COMMA TO DIVIDE FIELD                 
         LA    R3,1(R3)                                                         
         BCTR  R1,0                                                             
         L     RE,AIOAREA1                                                      
         EXMVC R1,0(R3),0(RE)      APPEND INPUT DATA ONTO FIELD                 
         STC   RF,5(R2)            UPDATE NEW LEN INTO FLD HEADER               
                                                                                
         ZIC   R0,STICLEN                                                       
         SH    R0,=Y(STICFIXL)                                                  
         LA    R1,1(R1)                                                         
         CR    R1,R0               DATA FORMATTED VS. DATA FROM STEREO          
         B     FMTFX               EXIT W/ CC SET                               
                                                                                
                                                                                
FMTFX    DS    0H                                                               
         B     XIT_STE                                                          
                                                                                
         DROP  R4,R5                                                            
         EJECT                                                                  
*&&DO                                                                           
*-------------------- EDIT DATA TO BE FORMATTED ----------------------*         
                                                                                
* Routine is called by FMTFLD only.                                             
* At entry,                                                                     
*  R4-->keyword entry in STROIKY table,                                         
*  R1 = length of data to be formatted,                                         
*  IOAREA1 contains the data.                                                   
* At exit,                                                                      
*  R1 = length of editted data to format.                                       
** !!WARNING!! IOAREA2 will be clobbered here                                   
                                                                                
         DS    0H                                                               
FMTFEDT  NTR1                                                                   
         USING STROIKYD,R4                                                      
                                                                                
         CLI   STKNUMB,IKNBOO                                                   
         BE    FMTFE50                                                          
         CLI   STKNUMB,IKNDAY                                                   
         BE    FMTFEDAY                                                         
         B     FMTFEX                                                           
*                                                                               
FMTFE50  DS    0H                  FORMATTING BOOK FIELD                        
         CH    R1,=H'3'                                                         
         BNH   FMTFE52                                                          
         L     RE,AIOAREA1                                                      
         CLC   =C'MBK',0(RE)        IF MULTIBOOKS REQUESTED, OR                 
         BE    *+10                                                             
         CLC   =C'EST',0(RE)        IF ESTIMATE REQUESTED,                      
         BNE   FMTFE52                                                          
         LA    R1,3                 FORMAT 3 BYTES ONLY ('EST')                 
                                                                                
FMTFE52  DS    0H                                                               
         B     FMTFEX                                                           
                                                                                
*                                                                               
** DAY/TIME **                                                                  
*                                                                               
FMTFEDAY DS    0H                                                               
         L     R2,AIOAREA2                                                      
         LH    R3,LIOAREA2                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE               CLEAR SCANNER OUTPUT BLOCK AREA              
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   SCAN1HL2,10                                                      
         MVI   SCNLNTH2,SCANLDAY                                                
         LR    R0,R1                                                            
         SLL   R0,16                                                            
         ST    R0,FULL                                                          
         LHI   R0,MAXDTCMP                                                      
         L     R3,AIOAREA2                                                      
         GOTO1 AMYSCAN,DMCB,(SCNLNTH2,AIOAREA1),((R0),(R3)),C',=+/',   +        
               FULL                                                             
         BE    *+12                IF ERROR,                                    
         LH    R1,FULL              RESTORE ORIG LEN FROM  FULL+0(2),           
         B     FMTFEDAYX            AND EXIT ROUTINE                            
*                                                                               
         MVC   NFLDS,DMCB+4                                                     
                                                                                
*                                                                               
FMTFED10 DS    0H                  LOOP THROUGH SCANNER OUTPUT BLOCK            
         CLI   0(R3),0              IF AT END,                                  
         BE    FMTFED49              WE'RE DONE                                 
*                                                                               
         LA    RE,12(R3)            RE-->START OF SEARCH AREA                   
         ZIC   RF,0(R3)                                                         
         AR    RF,RE                RF-->NEXT BYTE AFTER SEARCH AREA            
         LHI   R0,C','              LOOK FOR A COMMA                            
         SRST  RF,RE                                                            
         BC    2,FMTFED29           NOT FOUND, BUMP TO NEXT ENTRY               
*                                                                               
         GOTO1 VDAYPAK,DMCB,(0(R3),12(R3)),WORK,WORK+1                          
         CLI   WORK,0                                                           
         BE    FMTFED29             IF ERROR, SKIP THIS DAY/TIME INPUT          
         GOTO1 VDAYUNPK,DMCB,WORK,(X'07',12(R3))                                
         MVI   0(R3),7                                                          
FMTFED29 EQU   *                                                                
*                                                                               
         DS    0H                  BUMP TO NEXT ENTRY                           
         BAS   RE,FMTFEDBUMP                                                    
         B     FMTFED10                                                         
FMTFED49 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  RE-STRING DAY/TIME INPUT                     
         L     R2,AIOAREA1                                                      
         L     R3,AIOAREA2                                                      
*                                                                               
FMTFED52 DS    0H                  LOOP THROUGH AND STRING 1 AT A TIME          
         CLI   0(R3),0              IF AT END,                                  
         BE    FMTFED59              WE'RE DONE                                 
*                                                                               
         DS    0H                                                               
         XC    WORK,WORK            USE  WORK  AS DUMMY TWA FIELD               
         MVI   WORK+0,L'WORK                                                    
                                                                                
         L     RF,AFAC                                                          
         L     RF,(CUNSCAN-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,(1,12(R3)),(SCNLNTH2,WORK),C',=+/',0                   
         CLI   DMCB+00,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         DS    0H                   MOVE UNSCAN OUTPT TO CALLER'S OUTPT         
         LA    RE,WORK+8                                                        
         LA    RF,WORK+(L'WORK-1)                                               
FMTFED55B DS   0H                                                               
         CLI   0(RF),C' '            SEARCH BACKWARDS FOR LAST CHAR             
         BH    FMTFED55L             IF FOUND, EXIT LOOP                        
         CR    RF,RE                 IF NOT FOUND YET, AND NOT AT START         
         BNH   *+8                                                              
         BCT   RF,FMTFED55B           OF SEARCH STRING, KEEP GOING              
         MVI   0(RE),C' '             OTHERWISE, SURVIVE IT                     
FMTFED55L EQU  *                                                                
         SR    RF,RE                 RF = L(OUTPUT) FROM UNSCAN                 
         BNM   *+6                                                              
         DC    H'0'                                                             
         EXMVC RF,0(R2),0(RE)                                                   
         AR    R2,RF                                                            
         MVI   1(R2),C'+'            ASSUME MORE THAN ONE D/T COMPONENT         
         AHI   R2,1+1                                                           
                                                                                
         DS    0H                   BUMP TO NEXT SCAN BLOCK ENTRY               
         BAS   RE,FMTFEDBUMP                                                    
         B     FMTFED52                                                         
FMTFED59 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  CALCULATE L(OUR OUTPUT TO CALLER)            
         SHI   R2,1                                                             
         MVI   0(R2),C' '                                                       
         L     R0,AIOAREA1                                                      
         SR    R2,R0                                                            
         LR    R1,R2                                                            
*                                                                               
         B     FMTFEDAYX                                                        
                                                                                
*                                                                               
FMTFEDBUMP DS  0H                  BUMP TO NEXT ENTRY                           
         ZIC   R1,SCAN1HL2                                                      
         AR    R3,R1                                                            
         IC    R1,SCNLNTH2                                                      
         LA    R3,12(R1,R3)                                                     
         BR    RE                                                               
*                                                                               
FMTFEDAYX EQU  *                                                                
                                                                                
                                                                                
FMTFEX   DS    0H                                                               
         XIT1  REGS=(R1)                                                        
                                                                                
         DROP  R4                                                               
         EJECT                                                                  
*&&                                                                             
*&&DO                                                                           
*--------------- FORMAT (EXTRA) DATA INTO OPTIONS FIELD --------------*         
                                                                                
* At entry,                                                                     
*  R5-->chunk containing the input data.                                        
* At exit,                                                                      
*  CC low   if not all of input formatted into field,                           
*  CC equal if all of input formatted into field,                               
*  CC high  if an error occurred.                                               
* Note: BYTE is used by caller, do not clobber it!                              
                                                                                
         DS    0H                                                               
FMTOFLD  NTR1                                                                   
         USING STICKEYD,R5                                                      
*                                                                               
         DS    0H                  SET UP POINTERS                              
         L     R4,ASTROIKY                                                      
         USING STROIKYD,R4                                                      
FMTOF12  CLI   0(R4),EOT           SHOULDN'T REACH EOTABLE                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   STKNUMB,IKNOPT      LOCATE OPTION KEYWORD ENTRY                  
         BE    FMTOF14                                                          
         LA    R4,STROIKYQ(R4)                                                  
         B     FMTOF12                                                          
                                                                                
FMTOF14  DS    0H                  R4-->OPTIONS KEYWORD ENTRY                   
         ZICM  RE,STKFDSPH,(3)                                                  
         LA    RE,DEMWRKD(RE)                                                   
         L     R2,0(RE)            R2-->OPTIONS FIELD                           
                                                                                
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         LA    R3,8(RF,R2)         R3-->NEXT AVAILABLE SLOT IN OPT FLD          
         BZ    *+8                 CC SET BY  ICM  ABOVE                        
         LA    RF,1(RF)            ADJUST LENGTH FOR COMMA (C',')               
*                                                                               
         DS    0H                  GET L(DATA TO FORMAT)                        
         ZIC   R0,STICLEN                                                       
         SH    R0,=Y(STICFIXL)                                                  
         BZ    FMTOFX                                                           
                                                                                
         SR    R1,R1               USE R1 TO HOLD THAT LENGTH                   
         CVD   R1,DUB              DUB--COUNTER USED IN PACKED DECIMAL          
         LA    R6,STICDATA                                                      
         L     R7,AIOAREA1          IOAREA1 WILL HAVE DATA TO APPEND            
                                                                                
FMTOF31  DS    0H                                                               
         CLI   0(R6),STSPSOPI                                                   
         BE    FMTOF32                                                          
         CLI   0(R6),STSPEOPI                                                   
         BE    FMTOF34                                                          
         B     FMTOF36                                                          
                                                                                
FMTOF32  DS    0H                  STSPSOPI ENCOUNTERED                         
         CP    DUB,=P'1'            IGNORE IF MORE THAN ONE                     
         BE    FMTOF36                                                          
         AP    DUB,=P'1'                                                        
         B     FMTOF38                                                          
                                                                                
FMTOF34  DS    0H                  STSPEOPI ENCOUNTERED                         
         CP    DUB,=P'0'           WAS THERE A MATCHING STSPSOPI?               
         BE    FMTOF38              NO, LET IT PASS                             
         SP    DUB,=P'1'                                                        
         B     FMTOF38                                                          
                                                                                
FMTOF36  DS    0H                  PROCESS CHARACTER                            
         CP    DUB,=P'0'           DOES IT BELONG IN OPTIONS FIELD?             
         BE    FMTOF38              NO, IT WAS PUT IN CURRENT FIELD             
         MVC   0(1,R7),0(R6)        YES, IT BELONGS HERE                        
         LA    R7,1(R7)                                                         
         LA    R1,1(R1)             UP LENGTH BY ONE                            
                                                                                
FMTOF38  DS    0H                                                               
         LA    R6,1(R6)                                                         
         BCT   R0,FMTOF31                                                       
*                                                                               
         L     R1,AIOAREA1                                                      
         CLC   0(3,R1),=C'MBK'                                                  
         BNE   FMTOF40                                                          
         ZIC   RE,STICLEN                                                       
         SHI   RE,10               UP TO MBK=                                   
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   OPTMBKH+1(0),0(R1)                                               
         AHI   RE,1                                                             
         STC   RE,OPTMBKH                                                       
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     FMTOFX                                                           
*                                                                               
FMTOF40  DS    0H                  R1=L(DATA TO FORMAT), IOAREA1=DATA           
         LTR   R1,R1               MAKE SURE L(DATA TO FORMAT) > 0              
         BNP   FMTOF80              IF NOT, DON'T FORMAT DATA                   
*                                                                               
         AR    RF,R1               RF=NEW POTENTIAL LENGTH                      
         ZIC   RE,STKFMXL2         GET APPROPRIATE MAX LENGTH OF FIELD          
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    *+8                                                              
         IC    RE,STKFMXLN                                                      
         CR    RF,RE               DOES IT FIT IN DATA FIELD?                   
         BH    FMTOFX              NO, EXIT W/ CC HIGH                          
*                                                                               
         CLI   5(R2),0             IF DATA FIELD NOT EMPTY,                     
         BE    *+12                                                             
         MVI   0(R3),C','           PLACE COMMA TO DIVIDE FIELD                 
         LA    R3,1(R3)                                                         
         BCTR  R1,0                                                             
         L     RE,AIOAREA1                                                      
         EXMVC R1,0(R3),0(RE)      APPEND INPUT DATA ONTO FIELD                 
         STC   RF,5(R2)            UPDATE NEW LEN INTO FLD HEADER               
*                                                                               
FMTOF80  DS    0H                  PREPARE TO EXIT                              
         ZIC   R0,STICLEN                                                       
         SH    R0,=Y(STICFIXL)                                                  
         LA    R1,1(R1)                                                         
         CR    R1,R0               DATA FORMATTED VS. DATA FROM STEREO          
         B     FMTOFX              EXIT W/ CC SET                               
*                                                                               
FMTOFX   DS    0H                                                               
         B     XIT_STE                                                          
                                                                                
         DROP  R4,R5                                                            
         EJECT                                                                  
*&&                                                                             
*------------------- CHECK MAX NUMBER OF SUB FIELDS ------------------*         
                                                                                
* At entry,                                                                     
*  R3-->REQCTLTB,                                                               
*  R4-->keyword entry in STROIKY table.                                         
* Note: BYTE is used by caller, do not clobber it!                              
                                                                                
         DS    0H                                                               
CHKMAX   NTR1                                                                   
         USING RCTDSECT,R3                                                      
         USING STROIKYD,R4                                                      
         CLI   STKNUMB,IKNDAY                                                   
         BE    CMX10                                                            
         CLI   STKNUMB,IKNDEM                                                   
         BE    CMX12                                                            
         CLI   STKNUMB,IKNOPT                                                   
         BE    CMX14                                                            
         DC    H'0'                                                             
                                                                                
CMX10    DS    0H                  DAY/TIME                                     
         LA    RF,MAXDAYS                                                       
         B     CMX20                                                            
                                                                                
CMX12    DS    0H                  DEMOS                                        
         LA    RF,MAXDEMS                                                       
         B     CMX20                                                            
                                                                                
CMX14    DS    0H                  OPTIONS                                      
         ICM   RF,15,=X'00FFFFFF'                                               
         B     CMX20                                                            
*                                                                               
CMX20    DS    0H                                                               
         ZIC   R1,RCTUPTO                                                       
         ZIC   R0,RCTFROM                                                       
         SR    R1,R0                                                            
         LA    R1,1(R1)            R1=# OF SUB-FIELDS SO FAR                    
                                                                                
         CR    R1,RF               HAS IT REACHED THE MAX YET?                  
         BL    YES_STE              NOPE, RETURN CC EQUAL                       
         BE    NO_STE               YEP, RETURN CC EQUAL                        
         DC    H'0'                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
*----------------- TEST WHETHER TO FORMAT NEXT VALUE -----------------*         
                                                                                
* At entry,                                                                     
*  R3-->REQCTLTB,                                                               
*  R5-->current data entry in chunk storage.                                    
* At exit,                                                                      
*  CC equal ==> we can format next field,                                       
*  CC not equal ==> we can't format next field.                                 
* Note: BYTE is used by caller, do not clobber it!                              
                                                                                
         DS    0H                                                               
FMTNXT   NTR1                                                                   
         USING RCTDSECT,R3                                                      
         USING STICKEYD,R5                                                      
                                                                                
         CLI   RCTIKNUM,IKNDEM                                                  
         BE    FNXTDM10                                                         
         B     YES_STE                                                          
*                                                                               
FNXTDM10 DS    0H                  DEMOS FIELD                                  
         LA    R1,STICDATA                                                      
         CLC   0(2,R1),=C'L='                                                   
         BE    NO_STE                                                           
                                                                                
         LR    R0,R5                                                            
         ZIC   R1,STICLEN                                                       
         AR    R5,R1                                                            
         LA    R1,STICDATA                                                      
         CLC   0(2,R1),=C'L='                                                   
         BE    NO_STE                                                           
                                                                                
         B     YES_STE                                                          
*                                                                               
         DROP  R3,R5                                                            
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--PCH#)'                       
*--------------------------- PUT CHARACTER ---------------------------*         
                                                                                
* At entry,                                                                     
*   ALINEHDR = A(field header of current line)                                  
*   ALINE    = A(current line)                                                  
*   AENDLN   = A(end of current line)                                           
*   AENDSCN  = A(end of screen)                                                 
*   ANEXTCHR = A(location to output character)                                  
* At exit,                                                                      
*   ALINEHDR = A(field header of line for next time around)                     
*   ALINE    = A(line for next time around)                                     
*   AENDLN   = A(line end for next time around)                                 
*   ANEXTCHR = A(next location to output character)                             
                                                                                
PUTCHAR  DS    0H                                                               
         ICM   RF,15,ANEXTCHR      RF-->LOCATION TO PUT CHAR                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'CHAR,RF),CHAR                                                
*                                                                               
         DS    0H                  UPDATE ADDRESSES FOR NEXT TIME               
         C     RF,AENDSCN          ARE WE AT END-OF-SCREEN ALREADY?             
         BL    PCH12                                                            
         OI    OUPTFLG1,OF1SCFUL    YES, FLAG IT                                
         SR    RF,RF                 AND ANEXTCHR CAN'T BE DEFINED              
         B     PCH20                                                            
                                                                                
PCH12    DS    0H                                                               
         C     RF,AENDLN           ARE WE AT END-OF-LINE?                       
         BL    PCH14                                                            
         L     RF,ALINEHDR          YES, BUMP TO NEXT LINE                      
         ZIC   R1,0(RF)                                                         
         AR    RF,R1               RF-->NEXT LINE'S FIELD HEADER                
         ST    RF,ALINEHDR                                                      
         LA    RF,8(RF)                                                         
         ST    RF,ALINE            RF-->NEXT LINE                               
         LA    R1,L'STEI1ST-1(RF)                                               
         ST    R1,AENDLN                                                        
         B     PCH20                                                            
                                                                                
PCH14    DS    0H                                                               
         LA    RF,1(RF)                                                         
         B     PCH20                                                            
*                                                                               
PCH20    DS    0H                  BY THIS TIME, RF=A(TO PUT NXT CHAR)          
         ST    RF,ANEXTCHR                                                      
                                                                                
         B     XIT_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--SAPC#)'                      
*---------------------- CALL APPLICATION OVERLAY ---------------------*         
                                                                                
* Call application overlay (for STEREO) passing A(GLOBAL W/S) in R1.            
*  APMODE = mode to send to application overlay.                                
                                                                                
STAPCALL DS    0H                                                               
         NI    MISCFLG1,XFF-MF1STERR                                            
         GOTO1 AOVERLAY,DEMWRKD                                                 
         IPM   R0                  SAVE CONDITION CODE                          
         CLI   APMODE,FORCEEND     SET CONDITION CODE                           
         BNE   SAPC019                                                          
         OI    MISCFLG1,MF1STERR                                                
                                                                                
         LA    RE,SAPC019                                                       
         NTR1                                                                   
         L     RB,ABASE                                                         
         L     RA,BBASE                                                         
         L     R7,CBASE                                                         
         L     R6,DBASE                                                         
         L     RF,AERROR0          ERROR NUMBER SET BY APPLICATION              
         BR    RF                                                               
SAPC019  EQU   *                                                                
*                                                                               
         SPM   R0                  RESTORE CONDITION CODE                       
*                                                                               
         B     XIT_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--STPST#)'                     
*------------------------ POST TO TSAR BUFFER ------------------------*         
                                                                                
* This is the "replacement" routine for POST for a STEREO session.              
*  Records (variable-lengthed) are posted to TSAR instead of BINSRCH.           
*  This routine can either add a record or write an updated record              
*  to the TSAR buffer.                                                          
* At entry,                                                                     
*   ATMPTSIO = A(temporary I/O buffer this routine can use)                     
                                                                                
STPOST   DS    0H                                                               
         MVI   FERN,0              INITIALIZE ERROR CODE                        
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         TM    TSINDS,TSIINIOK     WAS INITIALIZATION OK?                       
         BZ    STPSTXH              NOPE, EXIT NOW                              
                                                                                
         SR    R2,R2                                                            
         ICM   R2,7,TSAREC+1       GET KEY OF TSAR DEMO RECORD                  
         LA    R2,2(R2)             BUMP OVER 2-BYTE LENGTH                     
         L     R3,ATMPTSIO         USE THIS AS TEMP I/O AREA                    
         ST    R3,TSAREC            AND LET TSAR KNOW ABOUT IT                  
         LA    R3,2(R3)            BUMP OVER 2-BYTE LENGTH                      
         SR    R5,R5                                                            
         ICM   R5,1,TSKEYL         R5=L(TSAR KEY)                               
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  R5,0                                                             
         EXMVC R5,0(R3),0(R2)                                                   
                                                                                
         MVI   TSACTN,TSARDH       READ HIGH & SEE IF RECD EXISTS               
         XC    TSRNUM,TSRNUM                                                    
         GOTO1 VTSAR,(R4)                                                       
                                                                                
         MVI   TSACTN,TSAWRT       ASSUME RECORD EXISTS                         
         CLI   TSERRS,0            ANY ERRORS?                                  
         BE    STPOST10             NOPE, WRITE RECORD BACK                     
         MVI   TSACTN,TSAADD       ASSUME RECORD DOESN'T EXIST                  
         TM    TSERRS,TSERNF       IS IT A NOT-FOUND ERROR?                     
         BO    STPOST10             YEP, ADD RECORD TO BUFFER                   
         DC    H'0'                                                             
*                                                                               
STPOST10 DS    0H                                                               
         MVC   TSAREC,ATSIOREC     SET ADDRESS OF TSAR RECORD                   
         XC    TSRNUM,TSRNUM                                                    
         GOTO1 VTSAR,(R4)          GO UPDATE TSAR BUFFER                        
         CLI   TSERRS,0                                                         
         BE    STPOST29                                                         
         MVI   FERN,120            "REQUEST TOO BIG" ERROR MESSAGE              
         MVI   GOSTEON,SERR0#                                                   
         GOTO1 AGOSTEO                                                          
         MVI   STMODE,STMNXTQ      SET TO "NEXT" MODE BEFORE EXITING            
         L     RD,ASTCTLRD         EXIT STEREO CONTROLLER ROUTINE               
         B     STPSTXL              WITH CC SET TO LOW                          
STPOST29 EQU   *                                                                
         DROP  R4                                                               
*                                                                               
STPSTX   DS    0H                                                               
         B     XIT_STE                                                          
                                                                                
STPSTXH  DS    0H                                                               
         J     EXITH                                                            
                                                                                
STPSTXL  DS    0H                                                               
         J     EXITL                                                            
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--SGTDR#)'                     
*------------------------ GET TSAR DEMO RECORD -----------------------*         
                                                                                
* Gets a TSAR demo record from TSAR buffer.                                     
* At entry,                                                                     
*   TSARKEY = key of demo record to get.                                        
* At exit,                                                                      
*   TSAREC  = A(record returned to application overlay)                         
*   CC  set to equal if record already in TSAR buffer                           
*   CC  set to not equal if record was just added to TSAR buffer                
                                                                                
STGETTDR DS    0H                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         ZICM  R2,TSAREC+1,(7)     R2-->TSAR I/O AREA                           
         ZIC   R1,TSKEYL           R1=L(TSAR KEY)                               
         BCTR  R1,0                                                             
         EXMVC R1,2(R2),TSARKEY    MOVE KEY INTO I/O AREA                       
         MVI   TSACTN,TSARDH       READ HIGH FOR RECORD                         
         XC    TSRNUM,TSRNUM                                                    
         GOTO1 VTSAR,(R4)                                                       
         CLI   TSERRS,0               ANY ERRORS?                               
         BE    SGTDR20                 NO, RECORD WAS FOUND                     
         TM    TSERRS,TSEEOF+TSERNF   IS RECORD NOT THERE?                      
         BNZ   SGTDR10                 YEAH, GO CREATE ONE                      
         DC    H'0'                                                             
*                                                                               
SGTDR10  DS    0H                  TSAR DEMO RECORD NOT FOUND                   
         LR    R0,R2                                                            
         LH    R1,SVMXRLEN         R1=MAX TSAR RECORD LENGTH                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR I/O AREA                               
                                                                                
         DS    0H                   AND CREATE RECORD                           
         ZIC   R1,TSKEYL                                                        
         BCTR  R1,0                                                             
         EXMVC R1,2(R2),TSARKEY                                                 
*&&DO                                                                           
         LA    R1,2+1(R1)          R1=L(TSAR DEMO RECORD SO FAR)                
*&&                                                                             
         LA    R1,2+1+1(R1)        R1=L(TSAR DEMO RECORD SO FAR)                
         STH   R1,0(R2)                                                         
         B     SGTDRXN                                                          
*                                                                               
SGTDR20  DS    0H                  TSAR DEMO RECORD FOUND                       
         B     SGTDRXY                                                          
*                                                                               
SGTDRXY  DS    0H                                                               
         B     YES_STE                                                          
SGTDRXN  DS    0H                                                               
         B     NO_STE                                                           
         DROP  R4                                                               
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--URCT#)'                      
*-------------------- UPDATE REQUEST CONTROL TABLE -------------------*         
                                                                                
* Updates the Request Control Table to get next combination of inputs.          
                                                                                
UPDRCT   DS    0H                                                               
         LA    R2,REQCTLTB                                                      
         USING RCTDSECT,R2                                                      
         LR    R0,R2                                                            
                                                                                
URCT10   DS    0H                  GO TO LAST ENTRY OF REQCTLTB                 
         CLI   0(R2),0                                                          
         BE    URCT20                                                           
         LA    R2,RCTLENQ(R2)                                                   
         B     URCT10                                                           
*                                                                               
URCT20   DS    0H                  TRAVERSE BACKWARDS FOR UPDATING              
         SH    R2,=Y(RCTLENQ)                                                   
         CLC   RCTUPTO,RCTDCNT                                                  
         BL    URCT40                                                           
         BE    URCT30                                                           
         DC    H'0'                CAN'T HAVE MORE THAN WHAT'S INPUTTED         
*                                                                               
URCT30   DS    0H                  RCTUPTO = RCTDCNT                            
         CR    R0,R2               ARE WE AT START OF REQCTLTB?                 
         BL    URCT20               NOPE, KEEP TRAVERSING BACKWARDS             
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OI    OUPTFLG1,OF1RCTX    ALL COMBO OF INPUT EXHAUSTED                 
         OI    OUPTFLG1,OF1RELSE   RELEASE TSAR DEMO RECORDS                    
         B     URCT50                                                           
*                                                                               
URCT40   DS    0H                  RCTUPTO < RCTDCNT                            
         ZIC   R1,RCTUPTO                                                       
         LA    R1,1(R1)                                                         
         STC   R1,RCTFROM                                                       
         STC   R1,RCTUPTO                                                       
                                                                                
URCT44   DS    0H                  RE-INIT NTRIES BELOW UPDATED ONE             
         LA    R2,RCTLENQ(R2)                                                   
         CLI   0(R2),0                                                          
         BE    URCT46                                                           
         MVI   RCTFROM,1                                                        
         MVI   RCTUPTO,1                                                        
         B     URCT44                                                           
                                                                                
URCT46   DS    0H                                                               
         B     URCT50                                                           
         DROP  R2                                                               
*                                                                               
URCT50   DS    0H                  CALL APPLICATION FOR STEREO TASKS            
         MVI   APMODE,STERTASK                                                  
         MVI   GOSTEON,SAPC#                                                    
         GOTO1 AGOSTEO                                                          
         B     URCTX                                                            
*                                                                               
URCTX    DS    0H                                                               
         B     XIT_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--STV#)'                       
*------------------- BRANCH TO VALIDATION ROUTINES -------------------*         
                                                                                
* Although this might be a STEREO session, we should use the same               
*  field validation routines that the original $DEM uses.                       
                                                                                
         PRINT GEN                                                              
STVAL    DS    0H                                                               
                                                                                
         NI    MISCFLG1,XFF-MF1ERROR                                            
                                                                                
         LA    RE,STV20                                                         
         L     RF,AVALRTNS                                                      
STV10    NTR1                                                                   
         L     RB,ABASE            RESTORE BASE REGISTERS                       
         L     RA,BBASE                                                         
         L     R7,CBASE             OF THE VALIDATION ROUTINES                  
         L     R6,DBASE                                                         
         BR    RF                                                               
*                                                                               
STV20    DS    0H                                                               
         B     STVX                                                             
*                                                                               
STVX     DS    0H                                                               
         B     XIT_STE                                                          
         PRINT NOGEN                                                            
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--LOV#)'                       
*---------------------- LOAD APPLICATION OVERLAY ---------------------*         
                                                                                
* Loads application overlay into core if not done yet.                          
*  At entry, OVERLAY = overlay number.                                          
                                                                                
LOADOVLY DS    0H                                                               
         OC    AOVERLAY,AOVERLAY    DO WE HAVE OVERLAY YET?                     
         BNZ   LOVX                  YEP, DON'T LOAD IT AGAIN                   
         GOTO1 VCALLOV,DMCB,(OVERLAY,0),0,0                                     
         CLI   4(R1),XFF                                                        
         BNE   *+6                                                              
         DC    H'0'                 CAN'T GO FURTHER W/O OVERLAY                
         MVC   AOVERLAY,0(R1)                                                   
         MVC   SVOVRLAY,OVERLAY                                                 
*                                                                               
LOVX     DS    0H                                                               
         B     XIT_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--SSCR# && RSCR#)'             
*------------------------ SAVE (STEREO) SCREEN -----------------------*         
                                                                                
* Saves only the STEREO screen section, starting at DEMTWAD+64, into            
***  page 3 of TEMPSTR.                                                         
*  page 4 of TEMPSTR.                                                           
                                                                                
SAVESCR  DS    0H                                                               
*                                      3RD PARAMETER                            
         MVI   DMCB+8,3                 PAGE #                                  
****     MVI   DMCB+8,4                 PAGE #                                  
         MVI   DMCB+9,0                 ????                                    
         MVC   DMCB+10(2),TERM          TERMINAL #                              
*                                      4TH PARAMETER                            
         LA    R0,DEMTWAD+64                                                    
         ST    R0,DMCB+12               A(AREA TO SAVE)                         
*                                      6TH PARAMETER                            
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=Y(SCRNLEN)   AMOUNT TO WRITE                         
                                                                                
         DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,,0                          
         B     XIT_STE                                                          
         SPACE 2                                                                
*---------------------- RESTORE (STEREO) SCREEN ----------------------*         
                                                                                
* Restores the STEREO screen section saved in page 3 of TEMPSTR.                
* Restores the STEREO screen section saved in page 4 of TEMPSTR.                
                                                                                
RSTRSCR  DS    0H                                                               
*                                      3RD PARAMETER                            
         MVI   DMCB+8,3                 PAGE #                                  
****     MVI   DMCB+8,4                 PAGE #                                  
         MVI   DMCB+9,0                 ????                                    
         MVC   DMCB+10(2),TERM          TERMINAL #                              
*                                      4TH PARAMETER                            
         LA    R0,DEMTWAD+64                                                    
         ST    R0,DMCB+12               A(AREA TO WRITE TO)                     
*                                      6TH PARAMETER                            
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=Y(SCRNLEN)   AMOUNT TO READ                          
                                                                                
         DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,,0                         
         B     XIT_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--PVT#)'                       
*------------------------ PRE-VALIDATION TASKS -----------------------*         
                                                                                
* Performs those chores required prior to validating the input fields           
*  on the original $DEM screen.                                                 
                                                                                
PVLTSK   DS    0H                                                               
         MVC   WORK2,OPTMBKH                                                    
         LA    R0,THESE            CLEAR  THESE  VALUES                         
         LA    R1,THESEL                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   OPTMBKH,WORK2                                                    
*                                                                               
         B     XIT_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--SERRn#)'                     
*--------------------------- ERROR HANDLER ---------------------------*         
                                                                                
* General error handling routine.  It takes care of those errors                
*  encountered during the normal $DEM field validation routines, and it         
*  handles those errors related to STEREO.                                      
* At entry,                                                                     
*    FERN = error number,                                                       
*    FADR = field where error occurred,                                         
*    FNDX = multiple field index,                                               
*    XTRA = optional user supplied help,                                        
*    SCANDLIM = override delimiters passed to SCANNER                           
* Enter at:                                                                     
*    STERR00 to set field index (FNDX) to zero on errors                        
*    STERR10 to display entire error message                                    
*    STERR20 to set FNDX, concatenate XTRA to root message & set cursor         
*    STERR30 to concatenate XTRA to root message & set cursor only              
*    STERR40 to set cursor only                                                 
                                                                                
STERR00  DS    0H                                                               
         MVI   FNDX,0                                                           
*                                                                               
STERR10  DS    0H                                                               
         OI    MISCFLG1,MF1ERROR                                                
         OI    DEMMSGH+6,X'80'     TRANSMIT MESSAGE                             
         CLI   FERN,SUPPLIED       TEST USER SUPPLIED MESSAGE                   
         BE    STERR40                                                          
         TM    DEMFLAG1,DF1STERO+DF1DEM32  IS THIS A DEM32 SESSION?             
         BO    STERR15G                     YES, SET FALNK MSGBLK INSTD         
         GOTO1 VGETMSG,DMCB2,(FERN,DEMMSG),(X'FF',DMCB),(15,0)                  
         B     STERR15X                                                         
STERR15G DS    0H                                                               
         ZIC   R0,FERN                                                          
         STCM  R0,3,FAMSGBLK+(FAMSGNO-FAMSGD)                                   
         MVI   FAMSGBLK+(FAMSGSYS-FAMSGD),15                                    
STERR15X EQU   *                                                                
*                                                                               
STERR20  DS    0H                                                               
         OI    MISCFLG1,MF1ERROR                                                
         CLI   FNDX,0              TEST MULTIPLE FIELD INDEX SET                
         BE    STERR30                                                          
                                                                                
         DS    0H                  SET REGS POINTING TO DISPLAY AREA            
         LA    R0,DEMMSG+L'DEMMSG-1  R0-->LAST BYTE OF AREA                     
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BNO   *+20                                                             
         LA    R1,FAMSGBLK+((FAMSGXTR-1)-FAMSGD)                                
*^^GYL - 12/15/98 - The following instruction was commented out to              
*^^GYL temporarily fix dump (in FAGETTXT) when msg#18 is used.                  
*&&DO                                                                           
         LA    RF,L'FAMSGXTR                                                    
*&&                                                                             
         LA    RF,L'FAMSGXTR-1                                                  
         LA    R0,0(RF,R1)                                                      
         B     STERR22X                                                         
         LR    R1,R0                                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LR    RF,R0                 R1-->LAST PRINTABLE CHAR IN AREA           
         SR    RF,R1               RF = ROOM LEFT IN DISPLAY AREA               
         BNP   STERR40              NOT POSITIVE==>NO MORE ROOM                 
STERR22X EQU   *                                                                
                                                                                
         DS    0H                  MOVE IN SEPARATOR ': ('                      
         LA    RE,3                                                             
         CR    RE,RF               IF NOT ENOUGH ROOM FOR SEPARATOR,            
         BNH   *+6                                                              
         LR    RE,RF                MOVE AS MUCH AS WILL FIT                    
         BCTR  RE,0                                                             
         EXMVC RE,1(R1),=C': ('                                                 
         LA    R1,1+1(R1,RE)       R1-->NXT OUTPUT LOCATION IN MSG AREA         
         CR    R1,R0               COMPARE TO END OF DISP AREA                  
         BNL   STERR40              NOT BFORE END--CAN'T DSPLY ANY MORE         
                                                                                
         DS    0H                  SET FIELD SEPARATOR CHARACTER                
         MVI   CHAR,C','           ASSUME IT'S THE DEFAULT (COMMA)              
         CLC   SCANDLIM(2),=C',='   WAS AN OVERRIDE SET?                        
         BNE   *+10                  NO, ASSUMPTION IS CORRECT                  
         MVC   CHAR,SCANDLIM+2     GET THE OVERRIDE FLD SEPARATOR               
                                                                                
         DS    0H                  SET FIELD INDEX                              
         ZIC   RE,FNDX                                                          
         BCTR  RE,0                                                             
         STC   RE,BYTE                                                          
                                                                                
         DS    0H                  POINT REGS TO ACTUAL INPUT                   
         L     RF,FADR                                                          
         ZICM  RE,5(RF),(1)                                                     
         BZ    STERR28                                                          
         LA    RE,8-1(RE,RF)       RE-->LAST CHAR OF ACTUAL INPUT               
         LA    RF,8(RF)            RF-->1ST CHAR OF INPUT                       
                                                                                
STERR25  DS    0H                                                               
         CLC   0(1,RF),CHAR        IF CHAR FROM INPT DATA NOT SEPARTOR,         
         BNE   STERR25A             DON'T DECREMENT FIELD INDEX                 
                                                                                
         DS    0H                  SEPARATOR ENCOUNTERED                        
         ZICM  R3,BYTE,(1)          AND IF FIELD INDEX IS ZERO,                 
         BZ    STERR28               DONE EXTRACTING INPUT DATA                 
         BCTR  R3,0                 ELSE, DECREMENT FIELD INDEX                 
         STC   R3,BYTE                                                          
         B     STERR25C              AND KEEP LOOKING                           
                                                                                
STERR25A DS    0H                  CHAR FROM INPT DATA NOT SEPARTOR,            
         CLI   BYTE,0               AND IF FIELD INDEX IS ZERO,                 
         BNE   STERR25C                                                         
         MVC   0(1,R1),0(RF)        MOVE CHAR TO DISPLAY AREA                   
         LA    R1,1(R1)              AND BUMP POINTER TO DISPLAY AREA           
         CR    R1,R0                                                            
         BH    STERR40             DISPLAY AREA CAN'T FIT ANY MORE              
                                                                                
STERR25C DS    0H                 BUMP INPUT DATA POINTER                       
         LA    RF,1(RF)                                                         
         CR    RF,RE                                                            
         BH    STERR28             NO MORE INPUT TO DISPLAY                     
         B     STERR25                                                          
                                                                                
STERR28  DS    0H                                                               
         MVI   0(R1),C')'                                                       
*                                                                               
STERR30  DS    0H                  CONCATENATE XTRA TO ROOT MESSAGE             
         OI    MISCFLG1,MF1ERROR                                                
         CLC   XTRA,SPACES         TEST IF EXTRA MESSAGE NEEDED                 
         BE    STERR40                                                          
         LA    R1,DEMMSG+L'DEMMSG-1                                             
         LA    RF,DEMMSG                                                        
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BNO   *+12                                                             
         LA    R1,(FAMSGBLK+(FAMSGXTR-FAMSGD))+L'FAMSGXTR-1                     
         LA    RF,(FAMSGBLK+(FAMSGXTR-FAMSGD))                                  
         LR    R0,R1                                                            
         CR    RF,R1               (MAKE SURE WE'RE NOT BEFORE START)           
         BH    *+16                                                             
         CLI   0(R1),C' '          YES - CALCULATE L'OUTPUT MESSAGE             
         BH    *+8                                                              
         BCT   R1,*-14                                                          
         SR    R0,R1               R0=AVAILABLE ROOM                            
         BNP   STERR40                                                          
         LA    RF,XTRA+L'XTRA-1                                                 
         LA    RE,XTRA-2                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,RE               RF=L'EXTRA MESSAGE+1                         
         CR    RF,R0               TEST ENOUGH ROOM FOR EXTRA MESSAGE           
         BH    STERR40                                                          
         MVI   1(R1),C'-'          YES - TACK IT ON                             
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R1),XTRA                                                     
*                                                                               
STERR40  DS    0H                                                               
         OI    MISCFLG1,MF1ERROR                                                
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    *+12                                                             
         L     R1,FADR             INSERT CURSOR TO FIELD IN ERROR              
         OI    6(R1),X'40'                                                      
                                                                                
         B     XIT_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--BTD#)'                       
*&&DO                                                                           
*------------------------ CONVERT BOOK TO DATE -----------------------*         
                                                                                
* Converts a book to a date format.  The book is presumed to have a             
*  valid month and year, and optionally a date.  If the book doesn't            
*  conform, then this routine is expected to notify caller of such.             
* At entry,                                                                     
*   WORK+10 = book inputted,                                                    
*   BYTE    = l(book inputted).                                                 
* At exit,                                                                      
*   DUB(3)  = binary date                                                       
*   BYTE    = assumption bits returned by PERVAL if CC equal                    
*   CC  set to equal if book is valid (has at least month AND year)             
*   CC  set to not equal otherwise                                              
                                                                                
BKTODATE DS    0H                                                               
         XC    DUB(3),DUB                                                       
*                                                                               
         DS    0H                  "STRIP OFF" ANY BOOKTYPE OR WEEK             
         ZICM  R0,BYTE,(1)                                                      
         BZ    BTDXN                                                            
         LA    RF,WORK+10                                                       
         SR    R1,R1                                                            
                                                                                
BTD12    DS    0H                                                               
         CLI   0(RF),C'('          INDICATES START OF BOOKTYPE INPUT            
         BE    BTD14                                                            
         CLI   0(RF),C'-'          INDICATES START OF WEEK     INPUT            
         BE    BTD14                                                            
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,BTD12                                                         
                                                                                
BTD14    DS    0H                  R1 = L(PLAIN BOOK INPUT)                     
         LTR   R1,R1                                                            
         BZ    BTDXN                                                            
*                                                                               
         DS    0H                                                               
         LR    R0,R1               HOLD ONTO LENGTH                             
         LA    R2,DBLOCK1          USE AS PERVAL OUTPUT BLOCK                   
         USING PERVALD,R2                                                       
                                                                                
         XC    PVALOUTB,PVALOUTB                                                
         L     RF,AFAC                                                          
         L     RF,CPERVAL-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,((R0),WORK+10),(X'20',PVALOUTB)                        
         CLI   4(R1),0                                                          
         BE    BTD25                                                            
         CLI   4(R1),X'04'                                                      
         BE    BTD25                                                            
         TM    PVALASSM,PVALASM+PVALASY                                         
         BZ    BTD25                                                            
         B     BTDXN                                                            
                                                                                
BTD25    DS    0H                                                               
         MVC   DUB(3),PVALBSTA                                                  
         MVC   BYTE,PVALASSM                                                    
         B     BTDXY                                                            
         DROP  R2                                                               
*                                                                               
BTDXY    DS    0H                                                               
         B     YES_STE                                                          
BTDXN    DS    0H                                                               
         B     NO_STE                                                           
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--STI# && RTI#)'               
*&&                                                                             
*--------------------------- SAVE TIA TABLES -------------------------*         
                                                                                
* saves TIA tables into TEMPSTR                                                 
                                                                                
SAVETIA  DS    0H                                                               
         LA    R0,=C'DMWRT'                                                     
         B     SRTIA                                                            
                                                                                
                                                                                
*-------------------------- RESTORE TIA TABLES -----------------------*         
                                                                                
* restores TIA tables from TEMPSTR                                              
                                                                                
RSTRTIA  DS    0H                                                               
         LA    R0,=C'DMREAD'                                                    
         B     SRTIA                                                            
                                                                                
                                                                                
                                                                                
* Little helper routine to save/restore TIA.                                    
                                                                                
SRTIA    DS    0H                                                               
         MVI   DMCB+8,4            3RD PARAMETER--PAGE #                        
****     MVI   DMCB+8,5            3RD PARAMETER--PAGE #                        
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TERM                                                  
         MVC   DMCB+20(2),=C'L='   6TH PARAMETER                                
         MVC   DMCB+22(2),=Y(TIASVLEN)                                          
         GOTO1 VDATAMGR,DMCB,(R0),=C'TEMPSTR',,ATIA,0                           
         B     XIT_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--SMWV#)'                      
*                                                                               
*-------------------- SET MONTH OR WEEK VALIDATION -------------------*         
                                                                                
* Examines book input to determine whether it needs to be validated as          
*  a month book or a week book.                                                 
* At entry,                                                                     
*   R2-->SCANNER block entry                                                    
* At exit,                                                                      
*   With CC set to equal, book reformatted as ##/YY when necessary              
*   With CC set to not equal,                                                   
*      EADDR = A(error routine to branch to)                                    
                                                                                
SETMWVAL DS    0H                                                               
         XC    EADDR,EADDR                                                      
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   DBMED,C'C'          CHECK MEDIA=CANTV                            
         BE    SMWV050                                                          
         CLI   DBMED,C'W'          CHECK MEDIA=WEEKLY TV                        
         BE    SMWV200                                                          
         CLI   DBSRC,C'K'          CHECK SOURCE=NTI                             
         BE    SMWV300                                                          
         CLI   DBSRC,C'C'          CHECK SOURCE=NTI CABLE                       
         BE    SMWV300                                                          
*&&DO                                                                           
         L     RE,ADEMFIL                                                       
         CLC   =C'OTP',8(RE)         OVERNIGHTS VALIDATE AS WEEKLY              
         BE    SMWV200                                                          
         CLC   =C'OPA',8(RE)         OVERNIGHTS VALIDATE AS WEEKLY              
         BE    SMWV200                                                          
*&&                                                                             
         CLI   DBMED,C'O'            OVERNIGHTS VALIDATE AS WEEKLY              
         BE    SMWV200                                                          
         B     SMWVXY                                                           
                                                                                
*                                                                               
** MEDIA=CAN **                                                                 
*                                                                               
SMWV050  DS    0H                                                               
         CLC   DBFIL,=C'TP '                     "                              
         BE    SMWV060                                                          
         B     SMWVXY                                                           
                                                                                
*                                                                               
*** MEDIA=CAN, FILE=TP ***                                                      
*                                                                               
SMWV060  DS    0H                                                               
         MVC   EADDR+2(2),=Y(EIIFBK-DEM00)  ASSUME INVALID BOOK FORMAT          
         ZICM  R1,0(R2),(1)                                                     
         BZ    SMWVXN                                                           
         STC   R1,BYTE                                                          
         BCTR  R1,0                                                             
         EXMVC R1,WORK+10,12(R2)                                                
         MVI   GOSTEON,BTD#        CONVERT BOOK TO A DATE FORMAT                
         GOTO1 AGOSTEO                                                          
         BNE   SMWVXN                                                           
                                                                                
*                                                                               
         DS    0H                  DUB(3)=BINARY DATE, BYTE=PERVAL BITS         
         CLI   DBSRC,C'N'           CHECK IF SOURCE IS NIELSEN                  
         BE    SMWV070                                                          
         CLI   DBSRC,C'A'           CHECK IF SOURCE IS BBM                      
         BE    SMWV073                                                          
         DC    H'0'                                                             
*                                                                               
SMWV070  DS    0H                  CHECK AGAINST NSI CUT-OFF DATE               
*                                                                               
         L     RE,ADEMFIL                                                       
         CLC   =C'WTP',8(RE)         ITS OK IF NOT WEEKLY TP                    
         BE    SMWV071                                                          
         CLC   =C'LPM',8(RE)         ITS OK IF NOT WEEKLY TP                    
         BE    SMWV071                                                          
         CLC   DUB(2),=X'6001'      IF PRE CUT-OFF DATE,                        
         BNL   SMWV70A              IF HIGHER THAN CUTOFF                       
         OC    DUB(3),DUB  DAMM PERVAL DOESNT TAKE MMYY WHERE YY=00             
         BNZ   SMWVXY      WITHOUT "\" SUCH AS JAN00...FUDGE HERE               
* PERVAL IN FACT CANT GIVE US A BOOK BACK - RESULT IS NULLS                     
SMWV70A  DS    0H          SINCE WE ARE REQUESTING TP THEN                      
**       OC    DUB(3),DUB  DAMM PERVAL DOESNT TAKE MMYY WHERE YY=00             
**       BNZ   SMWVXY      WITHOUT "\" SUCH AS JAN00...FUDGE HERE               
         MVI   BYTE,1      FUDGE TO VALIDATE AS MONTHY REST OF THE WAY          
         B     SMWV079X                                                         
SMWV071  DS    0H                                                               
         CLC   DUB(3),=X'600101'    IF PRE CUT-OFF DATE,                        
         BNL   SMWV079X                                                         
*                                                                               
SMWV072  DS    0H                                                               
         TM    BYTE,PVALASD          AND DATE WAS ASSUMED,                      
         BNZ   SMWVXY                 THEN WE'RE FINE                           
         MVC   EADDR+2(2),=Y(ETCN1-DEM00)                                       
         B     SMWVXN                                                           
*                                                                               
*                                                                               
SMWV073  DS    0H                  CHECK AGAINST BBM CUT-OFF DATE               
         LA    RF,12(R2)            NEED TO LOOK FOR BOOKTYPE "W"               
         ZIC   RE,0(R2)                                                         
         AR    RE,RF                RE-->1 BYTE PASS END OF SRCH STRING         
         LHI   R0,C'('              CHARACTER TO LOOK FOR                       
         SRST  RE,RF                                                            
         BC    11,SMWV073G          NOT FOUND, CAN'T BE BOOKTYPE "W"            
         CLC   =C'(W)',0(RE)        IF BOOKTYPE OF "W",                         
***      BE    SMWV079X              THEN BOOK CAN BE WKLY OR MTHLY             
         BNE   SMWV073F                                                         
*                                                                               
SMWV073E DS    0H                   IF BOOKTYPE W MAKE SURE WTP                 
         L     RF,ADEMFIL                                                       
         CLC   =C'LPM',8(RF)                                                    
         BE    *+10                 IF NOT EXIT WITH ERROR                      
         CLC   =C'WTP',8(RF)                                                    
         BNE   SMWVXN               IF NOT EXIT WITH ERROR                      
         B     SMWV079X                                                         
SMWV073F DS    0H                   IF BOOKTYPE W MAKE SURE WTP                 
SMWV073G DS    0H                                                               
         L     RF,ADEMFIL           MONTHLY BOOKS MUST BE TP                    
         CLC   =C'TP',8(RF)                                                     
*****    BNE   SMWVXN               IF NOT EXIT WITH ERROR                      
         BNE   SMWV079X             IF NOT EXIT WITH ERROR                      
*    NEW CODE SET BYTE TO PVALASD IF IT IS NOT SET ALREADY FOR TP               
*   YEAR 2K AND AFTER DATES DONT DO TOO WELL FROM PERVAL                        
         B     SMWVXY                                                           
*     I GUESS WE DONT NEED CONSISTENCY CHECK FOR                                
*     BBM ANYMORE, IF WE HAVE TO SEPERATE FILE FOR WEEKLY AND MONTHLY           
*                                                                               
         CLI   FNDX,1               FOR BBM NON-"W" BKTP, NEED AN EXTRA         
         BE    SMWVXY                                                           
         MVC   EADDR+2(2),=Y(EBKMM-DEM00)   BOOK FMT CONSISTENCY CHECK          
         LA    R1,X'70'              (MASK FOR BRANCH-NOT-ZERO)                 
         TM    MISCFLG1,MF1BKVWK                                                
         BNZ   *+8                                                              
         LA    R1,X'80'              (MASK FOR BRANCH-ON-ZERO)                  
         TM    BYTE,PVALASD                                                     
         EX    R1,*+4                                                           
         BC    0,SMWVXN                                                         
         B     SMWVXY                                                           
SMWV079X EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  DETERMINE HOW TO VALIDATE BOOK               
         CLI   DBMED,C'C'          CANADIAN WTP DEFAULT                         
         BNE   *+8                                                              
         OI    MISCFLG1,MF1BKVWK    NOPE, NEED TO VALIDATE AS WEEK              
*                                                                               
** WHY WERE WE ONLY DOING THIS FOR FIRST BOOK?? COMMENT OUT                     
**       CLI   FNDX,1               DO THIS FOR FIRST BOOK ONLY                 
**       BH    *+20                                                             
         NI    MISCFLG1,XFF-MF1BKVWK  ASSUME VALIDATE AS MONTH                  
         TM    BYTE,PVALASD                                                     
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1BKVWK      NOPE, NEED TO VALIDATE AS WEEK            
*                                                                               
         DS    0H                  CHECK FOR CONSISTENCY IN BOOK FRMTS          
*                                                                               
         LA    R1,X'80'             (MASK FOR BRANCH-ON-ZERO)                   
         TM    MISCFLG1,MF1BKVWK                                                
         BNZ   *+8                                                              
         LA    R1,X'70'             (MASK FOR BRANCH-NOT-ZERO)                  
         TM    BYTE,PVALASD                                                     
         EX    R1,*+4                                                           
         BC    0,*+14                                                           
         MVC   EADDR+2(2),=Y(EBKMM-DEM00)                                       
         B     SMWVXN                                                           
*                                                                               
SMWV086  DS    0H                  OTHER STUFF TO CHECK FOR MONTHLY BKS         
         TM    MISCFLG1,MF1BKVWK    MAKE SURE VALIDATING AS MONTHLY             
         BNZ   SMWV095X                                                         
*                                   VALIDATING AS MONTHLY                       
         CLI   ACTN,DEMOQHR         IF ACTION=DISPLAY                           
         BNE   SMWV087X              TELL USER TO USE ACTION=PERIOD             
         CLI   DBSRC,C'A'           IF BBM JUST GIVE NORMAL ERROR               
         BE    SMWVXN                                                           
         L     RF,ADEMACT                                                       
         CLC   =C'LILO',8(RF)       ENABLE CSI LILO                             
         BE    SMWV087X                                                         
*                                                                               
         MVC   EADDR+2(2),=Y(EACTCSI-DEM00)                                     
***      CLI   DBSRC,C'N'                                                       
***      BE    *+10                                                             
***      MVC   EADDR+2(2),=Y(EACTBBM-DEM00)                                     
         B     SMWVXN                                                           
SMWV087X EQU   *                                                                
                                                                                
         CLI   ACTN,DEMOPER         IF ACTION<>PERIOD,                          
         BE    SMWV088X              TELL USER TO USE M/D/Y FORMAT              
         L     RE,ADEMFIL                                                       
         CLC   =C'LPM',8(RE)                                                    
         BE    *+10                                                             
         CLC   =C'WTP',8(RE)                                                    
         BNE   SMWV088X                                                         
         MVC   EADDR+2(2),=Y(ETCN2-DEM00)                                       
         CLI   DBSRC,C'N'                                                       
         BE    *+10                                                             
         MVC   EADDR+2(2),=Y(ETCN4-DEM00)                                       
         B     SMWVXN                                                           
SMWV088X EQU   *                                                                
                                                                                
SMWV095X EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  SET NSIWEEK START DAY PARAMETER              
         TM    MISCFLG1,MF1BKVWK    BUT IF NOT VALIDATING AS WEEKLY,            
**       BZ    SMWVXY                EXIT NOW                                   
         BNZ   TEST                                                             
         L     RF,ADEMFIL          MAKE SURE BOOK IS MONTHLY INPUT              
         CLC   =C'TP',8(RF)        FOR FILE TP                                  
         BNE   SMWVXN                                                           
         B     SMWVXY                                                           
TEST     DS    0H                                                               
*                                  VALIDATING AS WEEKLY;                        
         MVI   BYTE,1               SET START DAY AS MONDAY                     
         B     SMWV500                                                          
         EJECT                                                                  
*                                                                               
** MEDIA = WEEKLY TV **                                                         
*                                                                               
SMWV200  DS    0H                                                               
         CLI   0(R2),6                                                          
         BL    SMWVXY                                                           
*                                                                               
         MVI   BYTE,0              SET START DAY TO DEFAULT                     
         B     SMWV500                                                          
                                                                                
                                                                                
*                                                                               
** SOURCE = NTI **                                                              
*                                                                               
SMWV300  DS    0H                                                               
         CLI   0(R2),6                                                          
         BL    SMWVXY                                                           
*                                                                               
         MVI   BYTE,0              SET START DAY TO DEFAULT                     
         B     SMWV500                                                          
         EJECT                                                                  
*                                                                               
** FUDGE INPUT BOOK TO BE WW/YY **                                              
*                                                                               
SMWV500  DS    0H                  BYTE = START DAY PARAMETER                   
         GOTO1 VDATVAL,DMCB,(0,12(R2)),DUB2                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+14                                                             
         MVC   EADDR+2(2),=Y(EIIFBK-DEM00)                                      
         B     SMWVXN                                                           
                                                                                
*                                                                               
         DS    0H                  GET WEEK NUMBER (WW)                         
         XC    DMCB(6*4),DMCB                                                   
         CLI   DBMED,C'N'                                                       
         BE    SMWV515N                                                         
                                                                                
         CLI   DBMED,C'O'                                                       
         BNE   *+8                                                              
         MVI   BYTE,1                                                           
         GOTO1 VNSIWEEK,DMCB,DUB2,(BYTE,VGETDAY),VADDAY,VDATCON                 
         B     SMWV515X                                                         
                                                                                
SMWV515N DS    0H                  MEDIA=NETWORK                                
*&&DO                                                                           
         L     RF,=V(NETWEEK)                                                   
         A     RF,BRELO                                                         
         GOTO1 (RF),DMCB,DUB2,(BYTE,VGETDAY),VADDAY                             
*&&                                                                             
         GOTO1 VNETWEEK,DMCB,DUB2,(BYTE,VGETDAY),VADDAY                         
         MVC   0(1,R1),8(R1)       PICK UP NET WEEK NOT HUT WK                  
SMWV515X EQU   *                                                                
*                                                                               
         DS    0H                                                               
         EDIT  (1,0(R1)),(3,WORK2+0),FILL=0,TRAIL=C'/'                          
         EDIT  (1,4(R1)),(2,WORK2+3),FILL=0                                     
                                                                                
*                                                                               
         DS    0H                  PICK UP ANY BOOKTYPE INPUTTED                
         ZIC   R1,0(R2)                                                         
         LA    RF,12(R2)                                                        
                                                                                
SMWV522  DS    0H                                                               
         CLI   0(RF),C'('          INDICATES START OF BOOKTYPE INPUT            
         BE    SMWV522X                                                         
         AHI   RF,1                                                             
         BCT   R1,SMWV522                                                       
SMWV522X EQU   *                                                                
*                                                                               
         OR    R1,R1                                                            
         BZ    SMWV524G                                                         
         BCTR  R1,0                                                             
         EXMVC R1,WORK2+5,0(RF)    APPEND BOOKTYPE AT END OF "WW/YY"            
         AHI   R1,1                                                             
SMWV524G EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  REFORMAT INPUT INTO SCANNER BLOCK            
         AHI   R1,5                                                             
         STC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,12(R2),WORK2                                                  
                                                                                
*                                                                               
         DS    0H                                                               
         XC    DUB2,DUB2                                                        
         XC    WORK2,WORK2                                                      
         B     SMWVXY                                                           
                                                                                
*                                                                               
** EXITS **                                                                     
*                                                                               
SMWVXY   DS    0H                                                               
         XC    EADDR,EADDR                                                      
         J     EXITE                                                            
*                                                                               
SMWVXN   DS    0H                                                               
         L     RF,EADDR                                                         
         A     RF,ABASE                                                         
         ST    RF,EADDR                                                         
         J     EXITL                                                            
*                                                                               
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--VPUR#)'                      
*------------------------ VALIDATE PURE NUMBER -----------------------*         
                                                                                
* At entry,                                                                     
*   R3-->Scan block containing pure number                                      
* At exit,                                                                      
*   DUB(2)  = pure number in internal format                                    
*   CC  set to equal if input is a valid pure number                            
*   CC  set to not equal otherwise                                              
* WARNING!! Don't clobber  WORK  in this routine                                
                                                                                
VALPURE  DS    0H                                                               
         XC    DUB(2),DUB                                                       
                                                                                
*                                                                               
         DS    0H                  GENERAL VALIDATION                           
         CLI   0(R3),3              LENGTH MUST BE 3                            
         BL    VPURXN                                                           
         CLI   0(R3),4               OR 4                                       
         BH    VPURXN                                                           
         CLI   1(R3),4              CAN'T HAVE SUB-FIELD                        
         BH    VPURXN                                                           
                                                                                
*                                                                               
         DS    0H                  VALIDATE QUARTER HOUR                        
         MVC   DUB2(2),=2X'F0'      FIRST 2 CHARACTERS                          
         MVZ   DUB2(2),12(R3)                                                   
         CLC   DUB2(2),=2X'F0'      MUST BE NUMERIC                             
         BNE   VPURXN                                                           
                                                                                
         PACK  DUB2,12(2,R3)                                                    
         CVB   R1,DUB2                                                          
         STC   R1,DUB              SET 1ST BYTE OF PURE NUMBER                  
                                                                                
*                                                                               
         DS    0H                  VALIDATE DAY                                 
         CLI   12+2(R3),C'D'        TYPICAL                                     
         BE    VPUR032X                                                         
         CLI   12+2(R3),C'E'        WEEKEND                                     
         BE    VPUR032X                                                         
         CLI   12+2(R3),C'0'        OR NUMERIC                                  
         BL    VPURXN                                                           
         CLI   12+2(R3),C'9'                                                    
         BH    VPURXN                                                           
VPUR032X EQU   *                                                                
                                                                                
         DS    0H                                                               
         LA    R1,DAYLST                                                        
         CLI   0(R1),C'9'                                                       
         BE    VPUR034M                                                         
VPUR034G CLC   12+2(1,R3),0(R1)                                                 
         BE    VPUR034M                                                         
         LA    R1,L'DAYLST(R1)                                                  
         B     VPUR034G                                                         
VPUR034M MVC   DUB+1(1),1(R1)                                                   
         B     VPUR039                                                          
*                                                                               
DAYLST   DS    0XL(1+1)                                                         
         DC    C'1',X'10'          MON                                          
         DC    C'2',X'20'          TUE                                          
         DC    C'3',X'30'          WED                                          
         DC    C'4',X'40'          THU                                          
         DC    C'5',X'50'          FRI                                          
         DC    C'6',X'60'          SAT                                          
         DC    C'7',X'70'          SUN                                          
         DC    C'8',X'80'          M-SU                                         
         DC    C'D',X'D0'          TYPICAL                                      
         DC    C'0',X'00'          M-F                                          
         DC    C'E',X'E0'          SA-SU (WEEKEND)                              
         DC    C'9',X'90'          OTHER                                        
VPUR039  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   0(R3),3                                                          
         BE    VPUR049                                                          
                                                                                
         LA    R1,WKLIST           MATCH FOURTH CHARACTER V LIST                
VPUR042  DS    0H                                                               
         CLI   0(R1),XFF                                                        
         BE    VPURXN                                                           
         CLC   0(1,R1),12+3(R3)                                                 
         BE    VPUR044                                                          
         LA    R1,L'WKLIST(R1)                                                  
         B     VPUR042                                                          
                                                                                
VPUR044  OC    DUB+1(1),1(R1)                                                   
         B     VPUR049                                                          
*                                                                               
WKLIST   DS    0XL(1+1)                                                         
         DC    C'1',AL1(02)                                                     
         DC    C'2',AL1(04)                                                     
         DC    C'3',AL1(06)                                                     
         DC    C'4',AL1(08)                                                     
         DC    C'5',AL1(10)                                                     
         DC    C'6',AL1(12)                                                     
         DC    C'7',AL1(14)                                                     
         DC    C'0',AL1(01)                                                     
         DC    C'A',AL1(03)                                                     
         DC    C'B',AL1(05)                                                     
         DC    C'C',AL1(07)                                                     
         DC    C'D',AL1(09)                                                     
         DC    C'E',AL1(11)                                                     
         DC    C'F',AL1(13)                                                     
         DC    C'G',AL1(15)                                                     
VPUR049  EQU   *                                                                
                                                                                
*                                                                               
VPURXY   DS    0H                                                               
         J     YES_STE                                                          
VPURXN   DS    0H                                                               
         J     NO_STE                                                           
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--VSCR#)'                      
*-------------------------- VALIDATE SCREEN --------------------------*         
                                                                                
* At exit,                                                                      
*   FERN = error number                                                         
*   CC  set to equal if screen is okay                                          
*   CC  set to not equal otherwise                                              
                                                                                
VALSCRN  DS    0H                                                               
         MVI   FERN,35                                                          
         SR    R0,R0                                                            
         L     R2,ATWA                                                          
         AHI   R2,64                                                            
                                                                                
VSCR022  DS    0H                  ACTUAL SCREEN                                
         CLI   0(R2),0              AT END OF SCREEN YET?                       
         BE    VSCR022X              YEP                                        
         IC    R0,0(R2)              NOPE,                                      
         AR    R2,R0                  BUMP TO NEXT FIELD                        
         B     VSCR022                                                          
VSCR022X EQU   *                                                                
         AHI   R2,(1+2)             BUMP FOR LAST & INDICATOR BYTES             
                                                                                
         DS    0H                  MAX STORAGE ALLOTTED FOR SCREEN              
         L     R3,ATWA                                                          
         AHI   R3,(64+SCRNLEN)                                                  
                                                                                
         CR    R2,R3               ACTUAL VS. MAX                               
         BH    VSCRXN               ERROR IF ACTUAL > MAX                       
         B     VSCRXY                                                           
                                                                                
*                                                                               
VSCRXY   DS    0H                                                               
         J     YES_STE                                                          
VSCRXN   DS    0H                                                               
         J     NO_STE                                                           
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--BRKT#)'                      
*------------------- TASKS FOR BREAKING TRANSACTION ------------------*         
                                                                                
                                                                                
BRKTASKS DS    0H                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
                                                                                
*                                                                               
         DS    0H                                                               
         TM    TSINDS,TSIINIOK     MAKE SURE TSAR INITIALIZATION WAS OK         
         BO    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         DS    0H                                                               
         OI    BRKFLAG1,BF1PRCSS   BREAK  CURRENTLY IN PROCESS                  
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   SVOVRLAY,OVERLAY     SAVE OVERLAY #                              
         MVC   SVAPMODE,APMODE      SAVE APPLICATION MODE                       
*                                                                               
         LA    R0,LASTS                                                         
         LA    R1,LASTSL                                                        
         LA    RE,THESE                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                SAVE THIS TIME'S VALUES                     
*                                                                               
         LA    R0,SVDBKVAL                                                      
         LA    R1,SVDBKVLQ                                                      
         LA    RE,DBKVALS                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                SAVE THIS DBLOCK KEY VALUES                 
*                                                                               
         MVI   TSACTN,TSASAV        SAVE TSAR BUFFER                            
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0              ANY ERRORS?                                
         BE    *+6                                                              
         DC    H'0'                  AFRAID SO!                                 
*                                                                               
         MVI   APMODE,APMBREAK      GIVE OVERLAY A CHANCE TO SAVE STUFF         
         MVI   GOSTEON,SAPC#                                                    
         GOTO1 AGOSTEO                                                          
         BL    BRKTXL                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         NI    BRKFLAG1,XFF-BF1PRCSS                                            
         DROP  R4                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         B     BRKTX                                                            
                                                                                
*                                                                               
BRKTX    DS    0H                                                               
         J     EXIT                                                             
*                                                                               
BRKTXL   DS    0H                                                               
         J     EXITL                                                            
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--RSMT#)'                      
*------------------- TASKS FOR RESUMING TRANSACTION ------------------*         
                                                                                
                                                                                
RSMTASKS DS    0H                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
                                                                                
*                                                                               
         DS    0H                                                               
         TM    TSINDS,TSIINIOK     MAKE SURE TSAR INITIALIZATION WAS OK         
         BO    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         DS    0H                                                               
         OI    RSMFLAG1,RF1PRCSS   RESUME CURRENTLY IN PROCESS                  
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   TSACTN,TSARES        RESTORE TSAR BUFFER                         
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0              ANY ERRORS?                                
         BE    *+6                                                              
         DC    H'0'                  AFRAID SO!                                 
*                                                                               
         LA    RE,LASTS                                                         
         LA    RF,LASTSL                                                        
         LA    R0,THESE                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE                RESTORE PREVIOUS VALUES                     
*                                                                               
         LA    RE,SVDBKVAL                                                      
         LA    RF,SVDBKVLQ                                                      
         LA    R0,DBKVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                RESTORE PREVIOUS DBLOCK KEY VALS            
*                                                                               
         MVC   OVERLAY,SVOVRLAY                                                 
         MVI   GOSTEON,LOV#         LOAD OVERLAY BACK IN                        
         GOTO1 AGOSTEO                                                          
*                                                                               
         MVI   APMODE,APMRESUM      GIVE OVERLAY A CHANCE TO RSTR STUFF         
         MVI   GOSTEON,SAPC#                                                    
         GOTO1 AGOSTEO                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   STMODE,STMOUTQ                                                   
         BNE   *+8                                                              
         OI    OUPTFLG1,OF1RESUM                                                
                                                                                
*                                                                               
         DS    0H                                                               
         NI    RSMFLAG1,XFF-RF1PRCSS                                            
         DROP  R4                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         B     RSMTX                                                            
                                                                                
*                                                                               
RSMTX    DS    0H                                                               
         J     EXIT                                                             
*                                                                               
*************************************************************                   
DELMBK   NTR1  BASE=*,LABEL=*                                                   
         L     R1,ADEMOPT                                                       
         ZIC   RE,0(R1)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)                                                    
         J     EXIT                                                             
         LTORG                                                                  
*************************************************************                   
ADDMBK   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK2,WORK2                                                      
         L     R1,ADEMOPT        OPTION FIELD HEADER                            
         ZIC   RE,0(R1)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),0(R1)                                                   
         LA    R1,WORK2                                                         
*                                                                               
         LA    R2,OPTMBKH        STORED AWAY MBK WITH LENGTH                    
         ZIC   R3,5(R1)          LENGTH OF FIELD                                
         CHI   R3,0                                                             
         BE    ADDMB20           ALREADY AN OPTION THERE                        
         LA    R4,8(R3,R1)                                                      
         MVI   0(R4),C','                                                       
         AHI   R3,1                                                             
*                                                                               
ADDMB20  ZIC   RE,0(R2)                                                         
         SHI   RE,1                                                             
         LA    R4,8(R1,R3)                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),1(R2)                                                    
         LA    R3,1(RE,R3)                                                      
         STC   R3,5(R1)                                                         
         AHI   R3,8                                                             
         STC   R3,0(R1)                                                         
*                                                                               
         XC    OPTMBKH,OPTMBKH                                                  
ADDMBX   J     EXIT                                                             
         LTORG                                                                  
*************************************************************                   
***********************************************************************         
*SETPHS81 DS    0H                                                              
*        GOTO1 VCALLOV,DMCB,(X'81',0),0                                         
*        L     RF,DMCB                                                          
*        LA    RF,0(RF)                                                         
*        ST    RF,DMCB                                                          
**         J     EXIT                                                           
***********************************************************************         
*                                                                               
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--LTORG && CONSTANTS)'         
*------------------------- LTORG & CONTSTANTS ------------------------*         
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--MISCELLANEOUS)'              
*--------------------------- MISCELLANEOUS ---------------------------*         
                                                                                
         LTORG                                                                  
STEREOL  EQU   *-STEREO                                                         
******   DS    0CL(X'2000'-STEREOL+1)                                           
         DS    0CL(X'2020'-STEREOL+1)  EXTEND IT BY 20 MORE BYTES               
*                                      REALLY WONT CAUSE DAMAGE                 
***********************************************************************         
                                                                                
                                                                                
         DROP  R8,R9,RA,RB                                                      
***********************************************************************         
***********************************************************************         
*========================== STEREO ROUTINES ==========================*         
*^^GYL STEREOQ  EQU   (((*-DEM00+4095)/4096)*4096)                              
STEREO2Q  EQU   (((*-DEM00+X'07FF')/X'0800')*X'0800')                           
                                                                                
         ORG   DEM00+STEREO2Q                                                   
STEREO2  NMOD1 0,*DEMS_2*,RA                                                    
         L     R9,0(R1)                                                         
         USING DEMWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING DEMTWAD,R8                                                       
                                                                                
         L     R1,4(R1)                                                         
         SRL   R1,24               SHIFT TO LOWER BYTE,                         
         BCTR  R1,0                SUBTRACT ONE,                                
         SH    R1,=Y(STE#)                                                      
         SLL   R1,2                 AND MULTIPLY BY FOUR                        
         B     STE2_00(R1)                                                      
                                                                                
BTD#     EQU   (STE_38-STE2_00)/4+1+STE#    SET MONTH OR WEEK VALIDATION        
                                                                                
STE2_00  DS    0H                                                               
STE_38   B     BKTODATE                                                         
*STE_37  B     SETPHS81                                                         
STE2#    EQU   (*-STE2_00)/4                                                    
         DC    H'0'                                                             
                                                                                
YES2_STE  SR    R9,R9                                                           
NO2_STE   LTR   R9,R9                                                           
XIT2_STE  XIT1                                                                  
********************************************************************            
*                                                                               
*------------------------ CONVERT BOOK TO DATE -----------------------*         
                                                                                
* Converts a book to a date format.  The book is presumed to have a             
*  valid month and year, and optionally a date.  If the book doesn't            
*  conform, then this routine is expected to notify caller of such.             
* At entry,                                                                     
*   WORK+10 = book inputted,                                                    
*   BYTE    = l(book inputted).                                                 
* At exit,                                                                      
*   DUB(3)  = binary date                                                       
*   BYTE    = assumption bits returned by PERVAL if CC equal                    
*   CC  set to equal if book is valid (has at least month AND year)             
*   CC  set to not equal otherwise                                              
                                                                                
BKTODATE DS    0H                                                               
         XC    DUB(3),DUB                                                       
*                                                                               
         DS    0H                  "STRIP OFF" ANY BOOKTYPE OR WEEK             
         ZICM  R0,BYTE,(1)                                                      
         BZ    BTDXN                                                            
         LA    RF,WORK+10                                                       
         SR    R1,R1                                                            
                                                                                
BTD12    DS    0H                                                               
         CLI   0(RF),C'('          INDICATES START OF BOOKTYPE INPUT            
         BE    BTD14                                                            
         CLI   0(RF),C'-'          INDICATES START OF WEEK     INPUT            
         BE    BTD14                                                            
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,BTD12                                                         
                                                                                
BTD14    DS    0H                  R1 = L(PLAIN BOOK INPUT)                     
         LTR   R1,R1                                                            
         BZ    BTDXN                                                            
*                                                                               
         DS    0H                                                               
         LR    R0,R1               HOLD ONTO LENGTH                             
         LA    R2,DBLOCK1          USE AS PERVAL OUTPUT BLOCK                   
         USING PERVALD,R2                                                       
                                                                                
         XC    PVALOUTB,PVALOUTB                                                
         L     RF,AFAC                                                          
         L     RF,CPERVAL-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,((R0),WORK+10),(X'60',PVALOUTB)                        
         CLI   4(R1),0                                                          
         BE    BTD25                                                            
         CLI   4(R1),X'04'                                                      
         BE    BTD25                                                            
         TM    PVALASSM,PVALASM+PVALASY                                         
         BZ    BTD25                                                            
         B     BTDXN                                                            
                                                                                
BTD25    DS    0H                                                               
         MVC   DUB(3),PVALBSTA                                                  
         MVC   BYTE,PVALASSM                                                    
         B     BTDXY                                                            
         DROP  R2                                                               
*                                                                               
BTDXY    DS    0H                                                               
         B     YES2_STE                                                         
BTDXN    DS    0H                                                               
         B     NO2_STE                                                          
         TITLE 'DEDEM00 - $DEM ROOT PHASE (STEREO--STI# && RTI#)'               
*                                                                               
*                                                                               
STEREO2L EQU   *-STEREO2                                                        
         DS    0CL(X'2000'-STEREO2L+1)                                          
*                                                                               
***********************************************************************         
                                                                                
                                                                                
         DROP  R8,R9,RA,RB                                                      
***********************************************************************         
         TITLE 'DEDEM00 - $DEM ROOT PHASE (TABLES)'                             
**********************************************************************          
*========================== T21B00's TABLES ==========================*         
                                                                                
       ++INCLUDE DESTACODE                                                      
STACODEL EQU   28                 SHOULD HAVE BEEN DEFINED IN DSECT             
       ++INCLUDE DEDEM80                                                        
         TITLE 'DEDEM00 - $DEM ROOT PHASE (MISCELLANEOUS TABLES)'               
* TABLE OF VALID OPTIONS INPUT & OUTPUT VALUES                                  
*                                                                               
TBLPROG  DC    AL1(3,1)            PROGRAM NAMES OPTION                         
         DC    C'YES',C'Y'                                                      
         DC    C'NO ',C'N'                                                      
         DC    AL1(EOT)                                                         
*                                                                               
TBLTIME  DC    AL1(8,1)            TIME FORMAT OPTION                           
         DC    C'STANDARD',C'S'                                                 
         DC    C'MILITARY',C'M'                                                 
         DC    AL1(EOT)                                                         
*                                                                               
TBLLIST  DC    AL1(7,1)            MARKET LIST FORMAT                           
         DC    C'NUMERIC',C'N'                                                  
         DC    C'ALPHA  ',C'A'                                                  
         DC    AL1(EOT)                                                         
*                                                                               
TBLWTP   DC    AL1(3,1)           MERGE WEEKLY DEMOS OPTION                     
         DC    C'YES',C'Y'                                                      
         DC    AL1(EOT)                                                         
*&&DO                                                                           
TBLSVIS  DC    CL60'1 thru 3 character SVI month (jan-dec)'                     
         DC    AL1(3,1)                                                         
         DC    C'JAN',X'10'                                                     
         DC    C'FEB',X'20'                                                     
         DC    C'MAR',X'30'                                                     
         DC    C'APR',X'40'                                                     
         DC    C'MAY',X'50'                                                     
         DC    C'JUN',X'60'                                                     
         DC    C'JUL',X'70'                                                     
         DC    C'AUG',X'80'                                                     
         DC    C'SEP',X'90'                                                     
         DC    C'OCT',X'A0'                                                     
         DC    C'NOV',X'B0'                                                     
         DC    C'DEC',X'C0'                                                     
         DC    AL1(EOT)                                                         
*&&                                                                             
TBLDATA  DC    AL1(4,1)            DATA TYPE OVERRIDES                          
         DC    C'SVIS',C'S'                                                     
         DC    C'DEMO',C'D'                                                     
         DC    AL1(EOT)                                                         
*                                                                               
TBLPUTS  DC    AL1(5,1)            AVERAGE PUT OPTION                           
         DC    C'1YEAR',X'00'                                                   
         DC    C'2YEAR',C'Y'                                                    
         DC    AL1(EOT)                                                         
*                                                                               
TBLDMA   DC    AL1(3,1)            PRECISION SPECIFICATION                      
         DC    C'IMP',C'I'                                                      
         DC    C'RTG',C'R'                                                      
         DC    AL1(EOT)                                                         
*                                                                               
TBLPMLST DC    AL1(7,1)            MKT/STTN FOR PROGRAM LIST SEQUENCE           
         DC    C'MARKET ',C'M'                                                  
         DC    C'STATION',C'S'                                                  
         DC    AL1(EOT)                                                         
*                                                                               
TBLCAVG  DC    AL1(3,1)            CABLE AVERAGE TYPES                          
         DC    C'PRG',C'P'                                                      
         DC    C'TRK',C'T'                                                      
         DC    C'EPS',C'E'                                                      
         DC    AL1(EOT)                                                         
TBLDEC   DC    AL1(1,1)            PRECISION SPECIFICATION                      
         DC    C'0',C'0'                                                        
         DC    C'1',C'1'                                                        
         DC    C'2',C'2'                                                        
         DC    AL1(EOT)                                                         
*                                                                               
TBLSYSL  DC    AL1(11,1)           SYSCODE LISTING OPTION                       
         DC    C'SYSCODE    ',C'S'  SORT BY SYSCODE                             
         DC    C'MSO NAME   ',C'M'  SORT BY MSONAME                             
         DC    AL1(EOT)                                                         
*                                                                               
DCLIST   DS    0C                  DATA DICTIONARY ESCAPE SEQUENCES             
         DCDDL SP#1BACT,3,L                                                     
         DCDDL SP#1BFIL,3,L                                                     
         DCDDL SP#1BSRC,3,L                                                     
         DCDDL SP#1BSTA,3,L                                                     
         DCDDL SP#1BBOK,3,L                                                     
         DCDDL SP#1BDYT,3,L                                                     
         DCDDL SP#1BDEM,3,L                                                     
         DCDDL SP#1BOPT,3,L                                                     
DCLISTX  EQU   *                                                                
                                                                                
         DS    0CL((DCLISTX-DCLIST)-(DSLISTX-DSLIST)+1)                         
         DS    0CL((DSLISTX-DSLIST)-(DCLISTX-DCLIST)+1)                         
***********************************************************************         
         TITLE 'DEDEM00 - $DEM ROOT PHASE (DEDEMMAPT)'                          
FAMAP    DS    0D                                                               
       ++INCLUDE DEDEMMAPT                                                      
         TITLE 'DEDEM00 - $DEM ROOT PHASE (DEDEMWRK)'                           
*                                                                               
* DEDEMWRK                                                                      
       ++INCLUDE DEDEMWRK                                                       
***      ORG    APSAVE                                                          
*BXUID   DS     0X           EXTENDED BLOCK FOR USER ID                         
*        DS     CL4                                                             
*        DS     A                                                               
*        DS     H                                                               
       ++INCLUDE DEDBEXTRAD                                                     
         TITLE 'DEDEM00 - $DEM ROOT PHASE'                                      
* DSECT TO COVER PRINT ROUTINE WORK AREA                                        
*                                                                               
PRTWRKD  DSECT                                                                  
PCTRL    DS    XL1                 ACTUAL CONTROL BYTE                          
PLINE    DS    CL133               PRINT LINE                                   
PSAVE    DS    XL1                 SAVED CONTROL BYTE                           
PRTWRKX  EQU   *                                                                
         SPACE 1                                                                
* DSECT TO COVER I/O ROUTINE WORK AREA                                          
*                                                                               
IOWORKD  DSECT                                                                  
IOWORK   DS    CL96                FOR GETREC/SPTFIL                            
IOPARM   DS    6F                  DATAMGR PARM LIST                            
IOWORK1  DS    X                   I/O COMMAND FLAG                             
IOWORK2  DS    X                   FILE/DIR NUMBER                              
IOWORK3  DS    X                   COMMAND NUMBER                               
IOFILE   DS    CL7                 I/O FILE NAME                                
IODIR    DS    CL7                 I/O DIRECTORY NAME                           
IOCMND   DS    CL7                 I/O DATAMGR COMMAND                          
IOINDS   DS    0XL5                I/O INDICATORS                               
IOFILTY  DS    XL1                 FILE/DIRECTORY TYPE                          
IOKEYLN  DS    XL1                 L'KEY (DANDX INCLUDES DBMINKEY)              
IODADSP  DS    XL1                 DISPLACEMENT TO D/A IN KEY                   
IOFSDSP  DS    XL1                 DISPLACEMENT TO FIL STATUS BYTE              
IODSDSP  DS    XL1                 DISPLACEMENT TO DIR STATUS BYTE              
IOWORKX  EQU   *                                                                
         SPACE 1                                                                
* DSECT TO COVER VALDAT ROUTINE WORK AREA                                       
*                                                                               
DATEWRKD DSECT                                                                  
DATEDUB  DS    D                                                                
DATEPARM DS    4F                                                               
DATEFLAG DS    X                                                                
DATEWORK DS    XL20                                                             
DATENFLD DS    X                                                                
DATEOUT  DS    XL2                                                              
DATESTRQ DS    XL2                                                              
DATEENDQ DS    XL2                                                              
DATEWRKX EQU   *                                                                
* DSECT TO COVER DEDEM81 TABLE (A LOT OF TABLE THAT USED TO BE IN DEM80         
*                                                                               
PHSE81D  DSECT                                                                  
         ORG   *+88                                                             
APHASE81 DS    A                                                                
VSYSTAB  DS    A                 A(SYSTAB)                                      
VACTTAB  DS    A                 A(ACTTAB)                                      
VACTHELP DS    A                 A(ACTHELP)                                     
VACTNEXT DS    A                 A(ACTNEXT)                                     
VFILTAB  DS    A                 A(FILTAB)                                      
VFMSTAB  DS    A                 A(FMSTAB)                                      
VTABLEN  DS    A                 A(TABLEN)                                      
VLTABLEN DS    A                 A(L'TABLEN)                                    
VTABLENQ DS    A                 A(TABLENQ)                                     
VTABLABL DS    A                 A(TABLABEL)                                    
VLTABLAB DS    A                 A(L'TABLABEL)                                  
VTABLABQ DS    A                 A(TABLABELQ)                                   
VUPCASE  DS    A                 A(UPCASETB)                                    
         EJECT                                                                  
* FATWA                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FAPQPL                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAPQPL                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDICATED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDICTATED                                                     
         PRINT ON                                                               
                                                                                
* SPGENCLT                                                                      
         PRINT OFF                                                              
CLTD     DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENEST                                                                      
         PRINT OFF                                                              
ESTD     DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENBUY                                                                      
         PRINT OFF                                                              
BUYD     DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENSTA                                                                      
         PRINT OFF                                                              
STAD     DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENDMN                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENDMN                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENMKT                                                                      
         PRINT OFF                                                              
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*********************************************************************           
*                                                                   *           
*   DATE   LVL DESCRIPTION                                          *           
* -------  --- ---------------------------------------------------- *           
* 27Apr99  254 Fudge for clients w/o new version of DEM32 (and      *           
*               can't get into FALINK)                        (GLEE)*           
*                                                                   *           
* 26Apr99  253 Remove error msg from change in LVL=252        (GLEE)*           
*                                                                   *           
* 16Apr99  252 Provide error msg for Upds on TP rotations     (GLEE)*           
*                                                                   *           
* 16Apr99  251 Check DEM32 session before loading x'FE' scrn  (GLEE)*           
*                                                                   *           
* 08Mar99  249 Error msg for Weekly HH demo prior to MAR98    (GLEE)*           
*                                                                   *           
* 05Mar99  248 Moved some more tables to DEDEM80              (GLEE)*           
*              Translate DEM32 alpha input to uppercase       (GLEE)*           
*                                                                   *           
* 11Feb99  247 Routine to translate book "DEM32" style        (GLEE)*           
*                                                                   *           
* 26Jan99  246 Implement version (extract) dates for DEM32    (GLEE)*           
*                                                                   *           
* 26Jan99  245 Error msg for progm mkt data prior to MAR94    (GLEE)*           
*                                                                   *           
* 07Jan99  244 Catch STEREO error when no input for keyword   (GLEE)*           
*                                                                   *           
* 23Dec98  243 Exits setting CC to HIGH, LOW, & EQUAL         (GLEE)*           
*                                                                   *           
* 14Dec98  242 I/O count rtn to see if approaching max allowed(GLEE)*           
*                                                                   *           
* 25Nov98  241 Bug fixes, FALINK overflow handling, docmntatn (GLEE)*           
*                                                                   *           
* 17Nov98  240 Pass more descript error msgs under DEM32 sess (GLEE)*           
*                                                                   *           
* 11Nov98  239 Fix bug in validating pure numbers             (GLEE)*           
*                                                                   *           
* 09Nov98  238 Remove trailing blanks from DEM16 inputs       (GLEE)*           
*                                                                   *           
* 06Nov98  237 Allocate temp TSAR I/O area in POST routine    (GLEE)*           
*                                                                   *           
* 17Jun98  156 ADD CODE TO SUPPORT FALINK                     (BPOO)*           
*                                                                   *           
* 16Jun98  154 New entry in STTABCLR table                    (GLEE)*           
*                                                                   *           
* 10Jun98  143 MODULAIZE CODE TO CREATE SPACE                 (BPOO)*           
*                                                                   *           
* 01Jun98  142 Bug fix in day/time validation (trailing comma)(GLEE)*           
*                                                                   *           
* 12May98  141 Support for day/time rotations                 (GLEE)*           
*                                                                   *           
* 11May98  140 Comment out "STEREO ONLY" code in VALOPT       (GLEE)*           
*                                                                   *           
* 05May98  139 Comment out "ACTION-SPECIFIC" code in VALSRC   (GLEE)*           
*              Clear booktype output area in VALUPBK rtn      (GLEE)*           
*                                                                   *           
* 05May98  138 NHT support in upgrade book validation rtn     (MAYA)*           
*                                                                   *           
* 30Apr98  137 More table entries to support PAV file         (GLEE)*           
*                                                                   *           
* 24Apr98  136 Enhance STTABCLR table and clearing routine    (GLEE)*           
*                                                                   *           
* 07Apr98  134 Restore changes made for LVL=098               (GLEE)*           
*                                                                   *           
* 06Apr98  133 Change code to write and read BINTAB to/from         *           
*               TEMPSTR only once                             (GLEE)*           
*                                                                   *           
* 01Apr98  132 Use length for TEMPSTR DMREAD in  GO  routine  (GLEE)*           
*                                                                   *           
* 30Mar98  131 Set labels (eyecatchers) for TIADSECT buffers  (GLEE)*           
*                                                                   *           
* 25Mar98  130 Move STEREO buffers to TIADSECT                (GLEE)*           
*              Encapsulate Validation & GO routines, even for a     *           
*               normal $DEM session                           (GLEE)*           
*              New exits to return to DEM00's caller          (GLEE)*           
*                                                                   *           
* 24Mar98  129 Disable  UPP  option for  TP  file             (GLEE)*           
*              Disable  MBK  option for  RTP  file            (GLEE)*           
*                                                                   *           
* 19Feb98  121 Optimize program a little bit                  (GLEE)*           
*                                                                   *           
* 19Feb98  099 Remove abbreviated way to activate help        (GLEE)*           
*                                                                   *           
* 11Feb98  098 Books for Canadian Meter Market may be monthly       *           
*               or weekly                                     (GLEE)*           
*                                                                   *           
* 16Jan98  097 Increase # of TSAR pages to three              (GLEE)*           
*                                                                   *           
* 05Nov97  096 Store actual request from STEREO in TIA area   (GLEE)*           
*                                                                   *           
* 04Nov97  095 Move STINCHNK to TIA area                      (GLEE)*           
*                                                                   *           
* 30Oct97  094 Enable demo menus for SPOT system only         (GLEE)*           
*                                                                   *           
* 06Aug97  093 Accept 'B', 'C', & 'D' bands for media=radio   (GLEE)*           
*                                                                   *           
* 22Jul97  092 New 4th parameter to NSIWEEK {A(DATCON)}       (MHER)*           
*                                                                   *           
* 01Jul97  091 Trap program when BINSOFAR is nulls to prevent       *           
*               clobbering of system                          (GLEE)*           
*                                                                   *           
* 10Apr97  090 Chg 'T' modifier to 'I' before DEMOCON for PAV (GLEE)*           
*                                                                   *           
* 03Mar97  089 Support cancel ability for STEREO              (GLEE)*           
*                                                                   *           
* 03Mar97  088 Some ESTIMATE Dsects was removed.  Remove support for*           
*               ESTIMATE action                                     *           
*                                                                   *           
* 18Sep96  087 Fix bug in STEREO-input mode--forgot to save & rstore*           
*               TSAR buffer when end-of-screen reached        (GLEE)*           
*                                                                   *           
* 20Aug96  086 Workaround for STEREO-hitting-<Enter> bug      (GLEE)*           
*                                                                   *           
* 02Aug96  085 Fix bug in validating spill mkt number         (GLEE)*           
*          084 Support averaging multiple books               (GLEE)*           
*          083 Support latest n books                         (GLEE)*           
*                                                                   *           
* 09JUL96  082 SET MAX INPUT LEN FOR NUMERIC INPUT IN VALSTA  (GLEE)*           
*                                                                   *           
* 21Jun96  081 Fudge station/book validation for parent plus  (GLEE)*           
*                                                                   *           
* 21Jun96  080 Support new source--MEDIAFAX                   (GLEE)*           
*              Use core-resident version of REGETIUN          (GLEE)*           
*                                                                   *           
* 09Apr96  079 Invalidate 4-week explosion (-W) for STEREO    (GLEE)*           
*                                                                   *           
* 02Apr96  078 Set NSIWEEK start day to Monday for Nielsen          *           
*               Canadien TP                                   (GLEE)*           
*                                                                   *           
* 20Mar96  077 For Nielsen Canadien TP, validate book as m/d/y if   *           
*               book is to be on or after 1st week of 1996    (GLEE)*           
*                                                                   *           
* 22Feb96  076 Support full and lite STEREO sessions          (GLEE)*           
*                                                                   *           
* 15Dec95  075 Fixed bug in which we forgot to turn off MF1WKACR    *           
*               if more than 2 demos requested                (GLEE)*           
*                                                                   *           
* 11Dec95  074 Fixed bug in the test-for-error code after the DATVAL*           
*               call near VALBK6 (was "OC    4(4,R1),0(R1)")  (GLEE)*           
*                                                                   *           
* 29Nov95  073 DEMOVAL SpotPak validation based on file       (GLEE)*           
*                                                                   *           
* 15Nov95  072 Support for STEREO (the real stuff)            (GLEE)*           
*              Moved  PRINT  routine to its own NMOD          (GLEE)*           
*                                                                   *           
* 08Nov95  071 Support for STEREO (preparation stages)        (GLEE)*           
*                                                                   *           
* 31Jul95  070 Flag to display weeks across screen            (GLEE)*           
*              Added week# caption when exploding weeks       (GLEE)*           
*              Display weeks vertically (nullifies displaying       *           
*               weeks across screen                           (GLEE)*           
*                                                                   *           
* 28Jul95  069 Support PAV file (for REP system only)         (GLEE)*           
*                                                                   *           
* 12Jun95  068 Fixed VALSTA bug, max len for mkt vs. station  (GLEE)*           
*                                                                   *           
* 28Mar95  067 Added RTP/SPILLTO entry in FILTAB              (GLEE)*           
*                                                                   *           
* 23Feb95  066 Support alpha mkt input in Stations field      (GLEE)*           
*                                                                   *           
* 21Nov94  065 Require station input for PROGNAME action      (GLEE)*           
*                                                                   *           
* 28Oct94  064 Added RTP/BOOK entry in FILTAB                 (GLEE)*           
*                                                                   *           
* 19Oct94  063 Added second station validation indicator in FILTAB  *           
*               & FMSTAB, and code to support this indicator  (GLEE)*           
*              Restored PRESET flag in FILSTAI field of             *           
*               TP/STATION entry                              (GLEE)*           
*                                                                   *           
* 19Oct94  062 Table entries for PROGNAME & PROGMKT actions   (GLEE)*           
*              Added code in VALSTA for prgm # input, and in VALDEM *           
*               for optional demos input                      (GLEE)*           
*              Removed PRESET flag in FILSTAI field of              *           
*               TP/STATION entry                              (GLEE)*           
*                                                                   *           
* 12OCT94  061 New DMA option for precision specification     (GLEE)*           
*                                                                   *           
* 27JUL94  060 Code to get radio spill mkt#                   (GLEE)*           
*                                                                   *           
* 25JUL94  059 Affids table entry for RTP                     (GLEE)*           
*              Added code in VALSTA for radio affids          (GLEE)*           
*              Use default geographical area when getting           *           
*               printable demos in VALDEM                     (GLEE)*           
*              Moved MEDGET call @ end of VALACT to near            *           
*               the end of VALSRC                             (GLEE)*           
*                                                                   *           
* 02MAY94  058 Table entries for radio time-period (RTP)      (GLEE)*           
*              Checked for Media=T in SPECIAL CIRCUMSTANCE    (GLEE)*           
*               #1 code                                             *           
*                                                                   *           
* 29MAR94  057 Code for ARB F94 sweep of Detroit & Cleveland  (GLEE)*           
*              Suppressed label generation at POST routine    (GLEE)*           
*                                                                   *           
* 22Mar94  056 Added "SPORT" & "-SPORT" options               (GLEE)*           
*                                                                   *           
* 21Mar94  055 Expanded option bits to 4 bytes long           (GLEE)*           
*                                                                   *           
* 18FEB88  SRC (SPANISH) TIME PERIOD FILE.                          *           
*                                                                   *           
* 14JAN88  ADD NSI WEEKLY TIME PERIOD FILE.                         *           
*                                                                   *           
* 27MAR87  INCREASE SIZE OF EBDREC FROM 1000 TO 2000 TO ACCOMODATE  *           
*          ACTUAL SIZE OF BUY RECORD BEING READ. CHANGE ALSO AT     *           
*          INITIALIZATION TO SET AAPWORK ACCORDINGLY.               *           
*                                                                   *           
* 31MAR87  CHANGE AT VALSTA40 TO ALLOW AFFID FEATURE TO WORK FOR    *           
*          CANADA. PROGRAM WILL NOW USE DBSRC FROM VALSRC ROUTINE   *           
*          IF SOURCE WAS ENTERED. THIS PASSES A GOOD VALUE TO       *           
*          BOOKVAL.                                                 *           
*                                                                   *           
* 11JUN87  READ R0 PROFILE                                          *           
*          ADD SECOND ESTIMATE ACTION TABLE ENTRY                   *           
*          CHANGE STATION VALIDATION FOR SID PROCESSING             *           
*                                                                   *           
* 07JAN87  SUPPORT WEEKLY METER MARKET DEMOS                        *           
*********************************************************************           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072DEDEM00   11/27/19'                                      
         END                                                                    
