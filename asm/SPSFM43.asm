*          DATA SET SPSFM43    AT LEVEL 076 AS OF 08/02/02                      
*PHASE T21743A                                                                  
T21743   TITLE 'SPSFM43 - PROFIT WITHIN BILL OVERLAY'                           
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* Mar15/02 073 MCHO - Changed screens from sp* to sc*                 *         
*                                                                     *         
* Jan06/00 071 GLEE - Give ability to split buys across SPOTBUY calls *         
*                                                                     *         
* Nov09/99 070 GLEE - Fix bug that overwrites mkg-level PW record     *         
*                                                                     *         
* Nov02/99 069 GLEE - Check if okay to update file before doing so    *         
*                                                                     *         
* Sep28/99 068 GLEE - Skip #-of-spots check for minus spots           *         
*                                                                     *         
* Sep15/99 067 GLEE - Check for input length of zero in VWD# routine  *         
*                                                                     *         
* Aug30/99 066 GLEE - Don't die if CALLSP isn't pointing to list scrn *         
*                                                                     *         
* Aug24/99 065 MHER - Change wim trade test for diy trade client      *         
*                                                                     *         
* Aug19/99 064 GLEE - Disallow product code to end in "#"--it's       *         
*                      reserved for trade products                    *         
*                                                                     *         
* Jul13/99 063 GLEE - In GBT# rtn, always find 1st effctv b'cast mnth *         
*                                                                     *         
* Jun23/99 062 GLEE - In IAC# routine, always use the week's end date *         
*                      to determine which b'cast month the week is in *         
*                                                                     *         
* May14/99 061 GLEE - Make provisions for "adding" deleted PW-STA recs*         
*                                                                     *         
* May12/99 060 GLEE - Support for WIM doing trade buys                *         
*                                                                     *         
* Oct29/98 059 GLEE - Support for Western Trading                     *         
*                                                                     *         
* Oct29/98 058 GLEE - Don't die in PROCSBIL if can't match on month   *         
*                                                                     *         
* Oct06/98 057 GLEE - If no spots, allow Adj DR/CR input if month has *         
*                      been estimate billed                           *         
*                                                                     *         
* Aug18/98 056 GLEE - LVL=054 change applicable for OWPW estimate only*         
*                                                                     *         
* Aug17/98 055 GLEE - Fix bug introduced in LVL=054                   *         
*                                                                     *         
* Aug10/98 054 GLEE - Use a week's end date to determine b'cast mnth  *         
*              GLEE - Set OOW start day even if OOW PW estimate       *         
*              GLEE - OOW PW paid$ to replace Current$ only           *         
*              GLEE - Prevent changes to CurrPW% for OOW PW clear mths*         
*              GLEE - Remove code that adds override CLCOST to OOW PW *         
*                      paid dollars                                   *         
*                                                                     *         
* Aug06/98 053 GLEE - Set up SUBR04                                   *         
*              GLEE - Move GBT#, BPT#, & IAC# routines to SUBR04      *         
*                                                                     *         
* Aug05/98 052 GLEE - OOW PW est mnth not cleared==>suppress CLT$ Curr*         
*                   - Undo LVL=049, and display "OWPW" flag near Est# *         
*                                                                     *         
* Aug03/98 051 GLEE - Because MOBILE now passes SPOTPROF to GETBROAD, *         
*                      clear SPOTPROF+8 when getting actual b'cst mths*         
*                     Pass A(mkt/est table) to SPOTIO                 *         
*                                                                     *         
* Jul02/98 050 GLEE - Get Adj DR/CR from PWOOWEL for OOW PW BILL ests *         
*                                                                     *         
* Jun30/98 049 GLEE - Display '**OWPW*' flag for months using OWPW$   *         
*                                                                     *         
* Jun30/98 048 GLEE - One more change for OWPW paid $                 *         
*                                                                     *         
* Jun24/98 047 GLEE - Display '**OWPW*' flag for OOW PW BILL estimates*         
*                                                                     *         
* Jun18/98 046 GLEE - Nahm vs Burgess--lvl 045 chng valid for OOW only*         
*                                                                     *         
* Jun12/98 045 GLEE - As of now, only valid input for Adj DR/CR is '0'*         
*                                                                     *         
* Jun11/98 044 GLEE - Update PWOOWEL elem when Adj DR/CR inputted     *         
*                                                                     *         
* May20/98 043 GLEE - Support  PW OOWR  billing                       *         
*                                                                     *         
* Apr16/98 042 GLEE - Bug fix--fill in CLCOST ovrides after rdng buys *         
*                                                                     *         
* Mar31/98 041 GLEE - Bug fix--re-read for mkt-level recd before CEDC#*         
*                                                                     *         
* Mar17/98 040 GLEE - Check if TSAR buffer initialized before sav/rstr*         
*                                                                     *         
* Mar16/98 039 GLEE - Increase size of  SPTTAB                        *         
*                                                                     *         
* Feb25/98 038 GLEE - Bug fix                                         *         
*                                                                     *         
* Feb18/98 037 GLEE - Resolve situation with nested TSAR calls        *         
*                                                                     *         
* Feb03/98 036 GLEE - Move Station Accumulator Table to TSAR          *         
*                                                                     *         
* Jan06/98 035 GLEE - Display error message when year/month of service*         
*                      in Bill Header not resolvable                  *         
*                                                                     *         
* Dec18/97 034 GLEE - Resolve situation when year/month of service in *         
*                      Bill Header records not in ACCUTAB             *         
*                                                                     *         
* Dec10/97 033 GLEE - Clear I/O area before building recd to add      *         
*                                                                     *         
* Nov25/97 032 GLEE - Merge dollar elems w/ same date in FXR# routine *         
*                                                                     *         
* Nov12/97 031 GLEE - Go through "re-final billing" process for       *         
*                      estimate billed months as well                 *         
*                                                                     *         
* Nov07/97 030 GLEE - Always check pay status when displyng DR/CR info*         
*                                                                     *         
* Nov06/97 029 GLEE - Fix upt XMINF & XMWRN routines                  *         
*                                                                     *         
* Oct28/97 028 GLEE - Set up exit-with-messages routine               *         
*                                                                     *         
* Oct01/97 027 GLEE - Fix bug in FXR# routine, which neglected to     *         
*                      remove elems w/ dates outside estimate period  *         
*                                                                     *         
* Aug19/97 026 GLEE - Undo stupid fucking change in LVL=023           *         
*                   - Use area after TWA for SPOT CHUNK area          *         
*                                                                     *         
* Aug18/97 025 GLEE - Increased CHUNK table by 500 bytes              *         
*                                                                     *         
* Aug08/97 024 GLEE - Ignore SPOT CHUNKS with zero spots in them      *         
*                                                                     *         
* Jul31/97 023 GLEE - Workaround for problem introduced in LVL=022    *         
*                                                                     *         
* Jul24/97 022 GLEE - More spots deferral stuff                       *         
*                                                                     *         
* Jul24/97 021 GLEE - Miscellaneous maintenance                       *         
*                                                                     *         
* Jul11/97 020 GLEE - Ignore buy records for spill markets            *         
*                                                                     *         
* Jun24/97 019 GLEE - Lump cable stations with same SYSCODE together  *         
*                      for station-level PW records                   *         
*                                                                     *         
* Jun24/97 018 GLEE - Set up replace text in messages                 *         
*                   - Use error msg when spot date not in estimate    *         
*                                                                     *         
* Jun23/97 017 GLEE - Move UPT# routine to SUBR02                     *         
*                   - Move SUM# routine to SUBR03                     *         
*                                                                     *         
* Jun12/97 016 GLEE - Handle error returned from SPOTBUY              *         
*                                                                     *         
* May28/97 015 GLEE - Remedy situation w/ dates changed in est hdr    *         
*                                                                     *         
* May20/97 014 GLEE - Set up SUBR03                                   *         
*                                                                     *         
* Aug14/96 013 GLEE - Accumulate locked costs instead of straight MVC *         
*                                                                     *         
* Mar25/96 012 GLEE - Check for deferral on unpaid spots              *         
*                                                                     *         
* Jan31/96 011 GLEE - Support client tax dollars                      *         
*              GLEE - Move ESTAB to D-chain                           *         
*                                                                     *         
* Oct12/95 010 GLEE - Add PW station records when necessary           *         
*              GLEE - Various "glich" fixes                           *         
*                                                                     *         
* Sep11/95 009 GLEE - Support re-final billing                        *         
*                                                                     *         
* Aug30/95 008 GLEE - Disallow removal of the Adj DR/CR amounts       *         
*              GLEE - Display  CLT$ Billed  in totals column          *         
*                      when applicable                                *         
*              GLEE - Make PWDOLBLD the date the Adj DR/CR is updated *         
*              GLEE - Disallow simultaneous update of DR/CR & PW%     *         
*                                                                     *         
* Aug25/95 007 GLEE - Fix pay status with minus spots                 *         
*                                                                     *         
* Aug22/95 006 GLEE - Move CHUNK to D-chain & make SPTTAB bigger      *         
*                                                                     *         
* Aug14/95 005 GLEE - Ignore pay status of minus spots                *         
*              GLEE - Move in tax dollars into PWCURTAX               *         
*                                                                     *         
* Jul07/95 004 GLEE - Billing stuff                                   *         
*          Nte GLEE - Put current # of spots in PWDOLSPT.  Although   *         
*                      billing reads PWCUREL for current data, it     *         
*                      needs the # of spots at the station level.     *         
*                      Since there are no PWCURELs in station-level   *         
*                      PW records, we are forced to use PWDOLSPT.     *         
*                                                                     *         
* Jun22/95 003 GLEE - Move SPOTBLOCK and SVACTB to D-chain work area  *         
*                                                                     *         
* May26/95 002 GLEE - Format PW% fields w/ right info.                *         
*                                                                     *         
* May12/95 001 GLEE - New program for Profit Within Bill              *         
*                                                                     *         
* May12/95 Nte GLEE - Instructions commented out w/ *^^SUP are as per *         
*                      Western.  They have asked to suppress the      *         
*                      makegood stuff.  Although the makegoods are    *         
*                      not taken out completely, enough of it have    *         
*                      been so there is virtually no computations     *         
*                      for makegoods.                                 *         
***********************************************************************         
         EJECT                                                                  
T21743   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 NMODWRKL,*T21743*,R7,R5,RR=RE                                    
                                                                                
         LR    RF,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         ST    RF,ANMODWRK                                                      
         ST    RE,RELO                                                          
         ST    RB,BASE1                                                         
         ST    R7,BASE2                                                         
         ST    R5,BASE3                                                         
         BAS   RE,MYINIT           INITIALIZE VALUES FIRST                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
                                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
                                                                                
                                                                                
NEXTMODE DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*========================== INITIALIZE VALUES ========================*         
MYINIT   NTR1                                                                   
                                                                                
         GOTO1 DATCON,DMCB,(X'03',BTODAY),(X'02',CTODAY),0                      
*                                                                               
         DS    0H                  SET UP NMOD WORKING AREA                     
         LA    R0,INIWRKTQ          R0 = LOOP COUNTER                           
         LH    R1,=Y(INIWRKTB-T21743)                                           
         A     R1,BASE1                                                         
MINIT03  ZICM  RE,0(R1),(3)                                                     
         A     RE,ANMODWRK          RE-->WHERE LABEL GOES                       
         MVC   0(8,RE),4(R1)                                                    
         LA    RE,8(RE)                                                         
         ZICM  RF,2(R1),(3)                                                     
         A     RF,ASYSD                                                         
         ST    RE,0(RF)                                                         
         LA    R1,L'INIWRKTB(R1)                                                
         BCT   R0,MINIT03                                                       
*                                                                               
         LA    RE,TIADSPTB         SET UP LABELS IN SPOT STORAGE AREA           
         LA    RF,LBLTAB                                                        
         LA    R0,TIADSPQ                                                       
MINIT05  LH    R1,0(RE)                                                         
         A     R1,ATIA                                                          
         MVC   0(L'LBLTAB,R1),0(RF)                                             
         LA    RE,L'TIADSPTB(RE)                                                
         LA    RF,L'LBLTAB(RF)                                                  
         BCT   R0,MINIT05                                                       
                                                                                
         L     RF,ATIA             SET UP SPOT STORAGE AREA                     
         LH    R1,=Y(ACCUTAB-SPOTAREA)                                          
         AR    R1,RF                                                            
         ST    R1,AACCUTAB                                                      
                                                                                
         LH    R1,=Y(OPTABLES-T21743)                                           
         A     R1,BASE1                                                         
         ST    R1,AOPTABLE         A(OPTION TABLES)                             
         LH    R1,=Y(COREQLST-T21743)                                           
         A     R1,BASE1                                                         
         ST    R1,ACORQLST         A(CORE-RES EQUATES TABLE)                    
         LH    R1,=Y(ELDATTAB-T21743)                                           
         A     R1,BASE1                                                         
         ST    R1,AELDATTB         A(ELDATTAB)                                  
                                                                                
         LA    R1,GOSUB            SUBROUTINE-POOLS INTERFACES                  
         ST    R1,AGOSUB                                                        
                                                                                
         L     RF,RELO                                                          
         L     R1,=A(SUBR01)                                                    
         AR    R1,RF                                                            
         ST    R1,ASUBR01          A(1ST SUBROUTINE POOL)                       
         L     R1,=A(SUBR02)                                                    
         AR    R1,RF                                                            
         ST    R1,ASUBR02          A(2ND SUBROUTINE POOL)                       
         L     R1,=A(SUBR03)                                                    
         AR    R1,RF                                                            
         ST    R1,ASUBR03          A(3RD SUBROUTINE POOL)                       
         L     R1,=A(SUBR04)                                                    
         AR    R1,RF                                                            
         ST    R1,ASUBR04          A(4TH SUBROUTINE POOL)                       
         L     R1,=A(XMSGRTN)                                                   
         AR    R1,RF                                                            
         ST    R1,AXMSGRTN         A(EXIT-WITH-MESSAGE ROUTINE)                 
                                                                                
         LA    R1,LOCDATE                                                       
         ST    R1,ALOCDATE                                                      
                                                                                
         LA    R1,PWBCPWH                                                       
         ST    R1,AM1STREC         A(MY 1ST DATA FIELD)                         
                                                                                
         MVC   AADDAY,ADDAY        SET UP MOBILE ADCON LIST                     
         MVC   ADATCON,DATCON                                                   
         MVC   AGETDAY,GETDAY                                                   
         LA    R2,COREQLSQ         # OF CORE-RES PHASES TO GET                  
         L     R3,ACORQLST         LIST OF CORE-RES PHASES TO GET               
         LA    R4,ACORQADD         R4-->START OF PLACE TO PUT ADDRESSES         
         ICM   R0,14,=X'D9000A'    GET SET FOR CALLOV                           
                                                                                
MINIT10  IC    R0,0(R3)                                                         
         GOTO1 CALLOV,DMCB,0,(R0),0                                             
         CLI   DMCB+4,XFF                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R4),DMCB                                                     
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R2,MINIT10                                                       
                                                                                
         GOTO1 DICTATE,DMCB,C'LL  ',DCLIST,DSLIST                               
*                                                                               
         DS    0H                  CHECK FOR FIRST TIME                         
         NI    MISCFLG3,XFF-MF31STIM                                            
         CLC   PWB1TIM,=C'HI'                                                   
         BE    *+14                                                             
         MVC   PWB1TIM,=C'HI'                                                   
         OI    MISCFLG3,MF31STIM                                                
*                                                                               
         DS    0H                  STUFF TO DO FOR FIRST TIME                   
         TM    MISCFLG3,MF31STIM                                                
         BZ    MINIT12X                                                         
         XC    TSARBLK,TSARBLK      CLEAR TSAR BLOCK                            
MINIT12X EQU   *                                                                
*                                                                               
                                                                                
         MVC   AIO,AIO1                                                         
         MVI   IOOPT,C'Y'          DO MY OWN IO                                 
                                                                                
         MVC   CBLSCMSK,=X'FFFF80' CABLE STATION SYSCODE MASK                   
                                                                                
         DS    0H                  SOME INITIALIZATION FOR TSAR BLOCK           
         LA    R1,TSARBLK                                                       
         USING TSARD,R1                                                         
         MVC   TSACOM,ACOMFACS                                                  
         LA    R0,STACREC           TSAR USED FOR STTN ACCUM TABLE              
         ST    R0,TSAREC                                                        
         DROP  R1                                                               
*                                                                               
         DS    0H                  PF KEYS / SELECT CODE STUFF                  
         OI    CONSERVH+6,X'81'    FOR PFKEY TO WORK                            
         OI    PWBPFLNH+6,X80                                                   
         MVC   PWBPFLN+13(9),SPACES                                             
         MVC   MYCALLSP,CALLSTCK   SAVE CALLSTCK & CALLSP AROUND                
*                                                                               
         CLI   MYCALLSP+1,1                                                     
         BNE   MINIT15                                                          
         CLI   MYCALLSP,LSTSCRNQ   IT HAD BETTER BE FROM LIST SCREEN!           
         BNE   MINIT15              IF NOT, JUST IGNORE & KEEP GOING            
         CLI   CALLPFK,0                                                        
         BNE   MINIT15                                                          
         MVC   CALLPFK,PFKEY                                                    
         MVI   PFKEY,0                                                          
                                                                                
MINIT15  DS    0H                                                               
         LA    R0,PFTABBIL         R0-->PF TABLES FROM BILL ONLY                
         CLI   1(R1),1             DID WE GET HERE FROM SOMEWHERE?              
         BNE   MINIT20              NOPE                                        
         CLI   0(R1),LSTSCRNQ      IT HAD BETTER BE FROM LIST SCREEN!           
*&&DO                                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         BNE   MINIT20              IF NOT, JUST IGNORE & KEEP GOING            
         MVC   PWBPFLN+13(9),PF12DC                                             
         LA    R0,PFTABLST         R0-->PF TABLES FROM LIST SCREEN              
                                                                                
MINIT20  DS    0H                                                               
         LA    R1,PWBMEDH          IN CASE OF ERROR,                            
         ST    R1,ACURFORC          WE KNOW WHERE TO PUT CURSOR                 
         GOTO1 INITPFKY,DMCB,(R0)                                               
         XC    ACURFORC,ACURFORC   CLEAR THIS IF WE GOT BACK OKAY               
                                                                                
         B     XIT                                                              
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL OVERLAY (VALKEY)'                  
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
VK       DS    0H                                                               
                                                                                
         NI    MISCFLG1,XFF-MF1KYCHG                                            
         XC    MYTEXT(MYTEXTL),MYTEXT                                           
*                                                                               
*--------------------------- VALIDATE MEDIA --------------------------*         
*                                                                               
         LA    R2,PWBMEDH          MEDIA                                        
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGED?                  
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
                                                                                
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
         MVC   MYAGYID,AGYID       HOLD ONTO AGENCY ID                          
         DROP  R6                                                               
*                                                                               
*-------------------------- VALIDATE CLIENT --------------------------*         
*                                                                               
         LA    R2,PWBCLTH          CLIENT                                       
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGED?                  
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
                                                                                
         DS    0H                  SET SPOT PROFILE                             
         MVC   WORK(12),=CL12'S000'                                             
         MVC   WORK+4(3),MYAGYID                                                
         MVC   WORK+7(3),QCLT                                                   
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         NI    CLTFLAG,XFF-CFWSTRAD                                             
         TM    COPT2,COP2TRAD                                                   
         BZ    *+8                                                              
         OI    CLTFLAG,CFWSTRAD       TRADE CLIENT                              
                                                                                
         CLI   COFFICE,X'41'                                                    
         BL    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  R6                                                               
         GOTO1 GETPROF,DMCB,WORK,SPOTPROF,DATAMGR                               
*                                                                               
*-------------------------- VALIDATE PRODUCT -------------------------*         
*                                                                               
         LA    R2,PWBPRDH          PRODUCT                                      
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGED?                  
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
                                                                                
         DS    0H                  DISALLOW TRADE PRODUCTS                      
         ZIC   R1,5(R2)                                                         
         LA    R1,(8-1)(R1,R2)      R1-->LAST CHAR OF INPUT                     
         CLI   0(R1),C'#'                                                       
         BNE   VKPRD049                                                         
         MVI   MYERRCD,IPRD2Q       SET ERROR CODE                              
         MVC   MYTEXT+1(6),=C'for PW'  AND EXTRA TEXT                           
         MVI   MYTEXT+0,6                                                       
         B     MYERROR                                                          
VKPRD049 EQU   *                                                                
                                                                                
         GOTO1 VALIPRD                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
*------------------------- VALIDATE ESTIMATE -------------------------*         
*                                                                               
         LA    R2,PWBESTH          ESTIMATE                                     
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGED?                  
         GOTO1 VALIEST                                                          
         OI    4(R2),X'20'                                                      
                                                                                
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         SR    R1,R1                                                            
         ICM   R1,7,EPWPCT         GET PROFIT WITHIN PERCENTAGE                 
         BZ    MPCTE                IF NONE, THEN ERROR                         
         N     R1,=X'007FFFFF'     IN CASE PW% = 0%                             
         ST    R1,OPWPCT           SAVE PW%                                     
         EDIT  (R1),(7,PWBPRCT),2,ZERO=NOBLANK,FLOAT=-                          
         OI    PWBPRCTH+6,X'80'     AND DISPLAY IT                              
                                                                                
         DS    0H                  PULL OTHER INFO FROM EST HDR                 
         NI    ESTFLAG,XFF-EFBILEST              ESTIMATE PERIOD FLG            
         TM    ECONTROL,EBILESTQ                                                
         BZ    *+8                                                              
         OI    ESTFLAG,EFBILEST                                                 
         NI    ESTFLAG,XFF-EFOWPW                                               
         TM    EFLAG1,EF1OOWPW                                                  
         BZ    *+8                                                              
         OI    ESTFLAG,EFOWPW                                                   
                                                                                
         NI    ESTFLAG,XFF-EFWSTRAD              TRADE ESTIMATE FLAG            
                                                                                
         MVC   ESDATE(L'ESTART+L'EEND),ESTART    EST START/END DATES            
         GOTO1 DATCON,DMCB,(X'10',ESDATE),(5,PWBESDT),0                         
         OI    PWBESDTH+6,X'80'                                                 
         PRINT OFF                                                              
*&&DO                                                                           
         TM    ESTFLAG,EFOWPW                    IF OOW PW BILLING,             
         BO    *+18                               DON'T SET OOW START           
*&&                                                                             
         PRINT ON                                                               
         CLI   EOWSDAY,0                         OUT-OF-WEEK START DAY          
         BE    *+10                                                             
         MVC   SPOTPROF+8(1),EOWSDAY                                            
*                                                                               
         DS    0H                  OTHER FLAGS ABOUT ESTIMATE                   
         OI    PWBEFLGH+6,X80                                                   
         MVC   PWBEFLG,=C'*OWPW*'                                               
         TM    ESTFLAG,EFOWPW                                                   
         BO    VKEST059                                                         
         MVC   PWBEFLG,SPACES                                                   
VKEST059 EQU   *                                                                
         DROP  R6                                                               
*                                                                               
*-------------------------- VALIDATE MARKET --------------------------*         
*                                                                               
         XC    BSTA,BSTA           DON'T WANT ANY LEFTOVERS IN HERE             
         XC    QSTA,QSTA            NOR IN HERE                                 
         LA    R2,PWBMKTH          MARKET                                       
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGED?                  
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    VKSTA                                                            
         GOTO1 VALIMKT                                                          
         OI    4(R2),X'20'                                                      
         MVC   PWBMKNM,MKTNM                                                    
         OI    PWBMKNMH+6,X'80'                                                 
         B     VKOPT                                                            
*                                                                               
*---------------- FOR TESTING ONLY, VALIDATE STATION -----------------*         
*                                                                               
VKSTA    DS    0H                                                               
         CLI   TWAOFFC,C'*'        TESTING W/ STATION ONLY FOR                  
         BE    *+12                 DDS TERMINAL                                
         MVI   MYERRCD,EMKTQ                                                    
         B     MYERROR                                                          
                                                                                
         GOTO1 VALISTA                                                          
         OI    4(R2),X'20'                                                      
         MVC   PWBMKNM,MKTNM                                                    
         OI    PWBMKNMH+6,X'80'                                                 
         B     VKOPT                                                            
*                                                                               
*---------------------- VALIDATE OPTIONS FIELD -----------------------*         
*                                                                               
VKOPT    LA    R2,PWBOPTNH                                                      
         BAS   RE,VALOPTS                                                       
         B     VKPW                                                             
*                                                                               
*--------------------- BUILD THE KEY FOR GENCON ----------------------*         
*                                                                               
VKPW     DS    0H                                                               
         TM    MISCFLG1,MF1KYCHG      IF KEY CHANGED,                           
         BZ    VKPW02                                                           
         NI    MISCFLG1,XFF-MF1NKFRS   RESET NECESSARY FLAGS                    
         NI    MISCFLG2,XFF-MF2NKFRS                                            
                                                                                
VKPW02   DS    0H                                                               
         NI    MISCFLG1,XFF-MF1RRDCH                                            
                                                                                
*                                                                               
VKPW05   DS    0H                                                               
         BAS   RE,PWSETUP             SET UP PW TABLES, ETC.                    
*                                                                               
         MVI   GOSUBN,RFB#         CHECK RE-FINAL BILLING                       
         GOTO1 AGOSUB                                                           
         TM    MISCFLG2,MF2RFBQ     IF YES, THEN GO                             
         BO    VKPW02                BACK AND REDO PWSETUP                      
                                                                                
*                                                                               
         DS    0H                  CHECK TO SEE IF EST HDR DATE CHANGED         
         TM    ESTFLAG,EFOWPW       IF OOWR PW BILLING,                         
         BNZ   VKPW25X               THEN SKIP THIS                             
         MVI   GOSUBN,GPR#          GET MKT-LEVEL RECD INTO AIO                 
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,CEDC#                                                     
         GOTO1 AGOSUB               (IF YES, A FLAG WILL BE SET)                
         BE    VKPW25X              NO, DATE DID NOT CHANGE IN EST HDR          
*                                                                               
         MVI   GOSUBN,FXR#         GO FIX RECORD                                
         GOTO1 AGOSUB                                                           
         OI    MISCFLG2,MF1RRDCH                                                
         B     VKPW05              GO BACK AND REDO PW SETUP                    
VKPW25X  EQU   *                                                                
                                                                                
*                                                                               
         TM    MISCFLG1,MF1KYCHG   RESET VALUES IF KEY CHANGED,                 
         BO    VKPW10                                                           
         TM    MISCFLG1,MF1STTCH    IF STATUS CHANGED,                          
         BO    VKPW10                                                           
         CLI   PFKEY,9              OR IF PF9 WAS HIT                           
         BNE   VKPWX                                                            
*                                                                               
VKPW10   DS    0H                                                               
         MVI   GOSUBN,CLS#         CLEAR SCREEN                                 
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,FMS#         FORMAT SCREEN                                
         GOTO1 (RF)                                                             
*                                                                               
VKPW20   DS    0H                                                               
*                                                                               
VKPWX    B     XIT                                                              
         EJECT                                                                  
*------------------------- TEST CHANGE OF KEY ------------------------*         
*                                                                               
KYCHNGED DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BZ    KYCH10                                                           
         TM    4(R2),X'80'                                                      
         BZR   RE                                                               
KYCH10   OI    MISCFLG1,MF1KYCHG                                                
         BR    RE                                                               
         SPACE 2                                                                
*------------------------------ PW SETUP -----------------------------*         
*                                                                               
PWSETUP  NTR1                                                                   
         L     RF,AGOSUB           RF=A(INTERFACE ROUTINE)                      
*                                                                               
** GET PW RECORD AND STATUS FLAGS **                                            
*                                                                               
         MVI   GOSUBN,GPR#         GET PW RECORD (KEY IS BUILT TOO)             
         GOTO1 (RF)                                                             
         BNE   NOREC               PW RECD MUST EXIST TO USE PW/BILL            
                                                                                
         MVC   DUB(1),LOCKFLAG     HOLD ONTO PREVIOUS LOCK STATUSES             
                                                                                
         L     R6,AIO                                                           
         USING PWRECD,R6                                                        
         NI    LOCKFLAG,XFF-LKFFPLKQ-LKFFLCKQ                                   
         TM    PWGNFLG,PWGNBILQ                                                 
         BZ    *+8                                                              
         OI    LOCKFLAG,LKFFLCKQ                                                
         TM    PWGNFLG,PWGNPLKQ                                                 
         BZ    *+8                                                              
         OI    LOCKFLAG,LKFFPLKQ                                                
         DROP  R6                                                               
*                                                                               
** SEE IF BUY/PW STATUS DIFFERENT FROM PREV TRANSACTION **                      
*                                                                               
         NI    MISCFLG1,XFF-MF1STTCH                                            
         TM    MISCFLG1,MF1KYCHG   IF KEY HAS NOT CHANGED,                      
         BO    PWS07                                                            
         CLC   LOCKFLAG,DUB         SEE IF STATUS CHANGED                       
         BE    PWS07                 NOPE                                       
         OI    MISCFLG1,MF1STTCH     YEP, FLAG IT                               
         BAS   RE,CHPRTMOD            AND UNDO MODIFIED FIELDS                  
*                                                                               
** BUILD TABLES **                                                              
*                                                                               
PWS07    DS    0H                                                               
         TM    MISCFLG1,MF1REDO    DO WE NEED TO REDO PW SETUP?                 
         BNZ   PWS10                YES                                         
         TM    MISCFLG1,MF1RRDCH   RE-READING FILE?                             
         BO    PWS10                YES                                         
         CLI   PFKEY,9              NO, AND IF PF9 WASN'T HIT FOR               
         BNE   PWS20                 RE-DISPLAY, JUST RESTORE TIA               
*                                                                               
PWS10    MVI   GOSUBN,GBT#         BUILD BROADCAST MONTH TABLE                  
         GOTO1 (RF)                                                             
                                                                                
         TM    MISCFLG1,MF1KYOPT                                                
         BNZ   PWS10A                                                           
         TM    MISCFLG1,MF1RRDCH                                                
         BO    PWS10A                                                           
         CLI   PFKEY,9                                                          
         BNE   PWS10X                                                           
PWS10A   MVI   GOSUBN,BPT#         BUILD PW TABLE                               
         GOTO1 (RF)                                                             
PWS10X   DS    0H                                                               
                                                                                
         MVI   GOSUBN,IAC#         INITIALIZE ACCUM TABLE                       
         GOTO1 (RF)                                                             
*                                                                               
         MVI   MYERRCD,0                                                        
         MVI   GOSUBN,GBB#         GET BUYS AND BILLS FOR KEY                   
         GOTO1 (RF)                                                             
         CLI   MYERRCD,0                                                        
         BNE   MYERROR                                                          
*                                                                               
         MVI   GOSUBN,FCO#         FILL THE CLCOST OVRRDES INTO ACCUTAB         
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,FLK#         FILL THE LOCKINS INTO ACCUTAB                
         GOTO1 (RF)                                                             
         MVI   GOSUBN,FOWP#        FILL THE OOW PAID DOLLARS                    
         GOTO1 (RF)                                                             
         MVI   GOSUBN,FDC#         FILL THE ADJ DR/CR AMOUNTS                   
         GOTO1 (RF)                                                             
         MVI   GOSUBN,SUM#         SUM UP ACCUTAB                               
         GOTO1 (RF)                                                             
         MVC   SVUSETAX,USETAX     SAVE OPTIONS VALUES                          
         B     PWSX                                                             
*                                                                               
PWS20    MVI   GOSUBN,RTI#         RESTORE TIA                                  
         GOTO1 (RF)                                                             
         MVI   GOSUBN,RFG#         RESET FLAGS IN TABLES                        
         GOTO1 (RF)                                                             
*                                                                               
PWSX     B     XIT                                                              
         EJECT                                                                  
*------------------------- VALIDATE OPTIONS --------------------------*         
VALOPTS  NTR1                                                                   
         LA    R2,PWBOPTNH                                                      
         TM    MISCFLG1,MF1ERRQ    WAS THERE A PREVIOUS ERROR?                  
         BZ    VLOP03               NOPE                                        
         LH    R0,PRVFDSP                                                       
         A     R0,ATWA                                                          
         CR    R0,R2                YEP, WAS IT IN OPTIONS FIELD?               
         BNE   VLOP03                                                           
         BAS   RE,CLRERRS            YES, CLEAR ERROR CODES                     
         NI    MISCFLG1,XFF-MF1ERRQ   AND FLAG                                  
*                                                                               
VLOP03   MVI   USETAX,C'Y'         SET DEFAULT OPTION VALUES                    
         MVI   OPTUSED,0           CLEAR OPTIONS USED FLAG                      
         NI    MISCFLG1,XFF-MF1OPTN                                             
                                                                                
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BZ    VLOP05                                                           
         OI    MISCFLG1,MF1OPTN                                                 
*                                                                               
VLOP05   CLI   5(R2),0             ANY OPTIONS INPUTTED?                        
         BE    VALOPTX              NO                                          
         GOTO1 SCANNER,DMCB,(R2),(X'81',AIO3),0                                 
         CLI   4(R1),0                                                          
         BE    INVLFLD                                                          
         MVC   CNTDOWN,4(R1)       USE CNTDOWN FOR LOOP COUNTER                 
         L     R3,AIO3                                                          
*                                                                               
VLOP10   L     R4,AOPTABLE                                                      
         USING OPTDSECT,R4                                                      
VLOP10A  CLI   0(R4),EOT           END OF TABLE?                                
         BE    INVOPERR             YES, CAN'T MATCH INPUTTED OPTION            
                                                                                
         CLC   OPTNAME(1),0(R3)    MATCH L(KEYWORD)                             
         BNE   VLOPBUMP                                                         
                                                                                
         ZIC   R1,OPTNAME          MATCH KEYWORD                                
         BCTR  R1,0                                                             
         EXCLC R1,OPTNAME+1,12(R3)                                              
         BNE   VLOPBUMP                                                         
                                                                                
         MVC   BYTE,OPTUSED        KEYWORD FOUND                                
         NC    BYTE,OPTBIT          WAS IT SPECIFIED ALREADY?                   
         BNZ   DUPOPERR             YES, ERROR                                  
         LA    RF,OPTNAME+2(R1)    RF-->START OF VALUES                         
         TM    OPTFLAG,OPFOTHAB    FOUND KEYWORD, CHECK VALUES                  
         BZ    VLOP15               VALUES IN ANOTHER TABLE                     
         ZICM  R0,0(RF),(3)                                                     
         A     R0,BASE1                                                         
         LR    RF,R0               RF-->VALUES IN SEPARATE TABLE                
*                                                                               
VLOP15   DS    0H                                                               
         TM    OPTFLAG,OPFKYWRD    OPTION SPECIFIABLE BY KEYWORD ONLY           
         BO    VLOP20                                                           
                                                                                
VLOP15A  CLI   0(RF),EOT           AT END OF VALUES TABLE?                      
         BE    OPDTAERR             YEP, VALUE INPUTTED IS INVALID              
         CLC   1(1,R3),0(RF)       MATCH L(VALUE) INPUTTED                      
         BH    VLOP15B                                                          
         ZIC   R1,1(R3)            R1=L(INPUTTED VALUE)                         
         BCTR  R1,0                                                             
         EXCLC R1,1(RF),22(R3)                                                  
         BE    VLOP20                                                           
VLOP15B  ZIC   R1,0(RF)                                                         
         LA    RF,1(RF,R1)         BUMP TO NEXT OPTION VALUE                    
         B     VLOP15A                                                          
*                                                                               
VLOP20   ZIC   R1,OPTOLEN          USER'S INPUTTED VALUE FOUND                  
         BCTR  R1,0                 HOLD ON TO IT FOR LATER USE                 
         ZICM  RE,OPTOADDR,(3)                                                  
         A     RE,ASYSD            RE-->OUTPUT FIELD                            
         EXMVC R1,0(RE),1(RF)                                                   
         OC    OPTUSED,OPTBIT                                                   
                                                                                
         ZIC   R1,CNTDOWN          ANY MORE SCANNER ENTRIES?                    
         SH    R1,=H'1'                                                         
         BZ    VALOPTX              NOPE, FINISHED W/ OPTIONS                   
         STC   R1,CNTDOWN                                                       
         LA    R3,32(R3)            YES, BUMP TO NEXT SCANNER ENTRY             
         B     VLOP10                                                           
*                                                                               
VLOPBUMP ZIC   R0,OPTLEN                                                        
         AR    R4,R0                                                            
         B     VLOP10A                                                          
*                                                                               
VALOPTX  MVC   SVOPTFLD,PWBOPTN    SAVE OPTIONS INPUTTED                        
         B     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL OVERLAY (VALREC)'                  
***********************************************************************         
*========================== VALIDATE RECORD ==========================*         
VR       DS    0H                                                               
         NI    MISCFLG1,XFF-MF1ERRQ   ASSUME NO ERRORS                          
                                                                                
         L     R6,AIO                                                           
         TM    MISCFLG1,MF1REDO    DID WE NEED TO REDO PW SETUP?                
         BZ    VR03                 NO, DO NORMAL STUFF                         
         MVI   PFKEY,0                                                          
         B     DR                   YES, DISPLAY RECD & IGNORE PFKEY            
*                                                                               
VR03     CLI   PFKEY,0             PFKEY HIT?                                   
         BE    VR05                 NO, CHECK FOR ANYTHING MODIFIED             
         CLI   PFKEY,9             ONLY PF9 IS VALID                            
         BE    DR                                                               
         MVI   PFKEY,0              SO IGNORE ALL OTHERS IF HIT                 
                                                                                
*  BECAUSE IT IS EASIER TO WORK FROM THE ACCUMULATOR TABLE, THE       *         
*   MODIFY BITS ARE TRANSFERRED FROM THE TWA TO ACCUTAB.  FROM        *         
*   THERE, THE RULES OF OVERRIDING VALUES ENTERRED ARE FOLLOWED       *         
                                                                                
VR05     NI    MISCFLG1,XFF-MF1MDFY    ASSUME NOTHING MODIFIED                  
                                                                                
         MVI   BYTE,0                                                           
*                                                                               
** SKED TOTALS COLUMN **                                                        
*                                                                               
         DS    0H                  CHECK SKED CURRENT PW% FIRST                 
         LA    R2,PWBCPWH          R2-->CURRENT PW%                             
         BAS   RE,LOCSTTL          R4-->SKED BILL ADJ LINE                      
         TM    (BSLTTLH+4-BSLDSECT)(R2),X80                                     
         BZ    VR10                                                             
         OI    (ATFLAG-ACCUTABD)(R4),ATFPWQ                                     
         OI    BYTE,X80            FLAG THAT SKED CURR PW% CHANGED              
                                                                                
VR10     DS    0H                  CHECK SKED PROJ PW% SECOND                   
         LA    R2,PWBPJPWH         R2-->PROJECTED PW%                           
         LA    R4,ACCUTABQ(R4)     R4-->SKED TOTALS ENTRY                       
         TM    (BSLTTLH+4-BSLDSECT)(R2),X80                                     
         BZ    VR10A                                                            
         OI    (ATFLAG-ACCUTABD)(R4),ATFPWQ                                     
         OI    BYTE,X40            FLAG THAT SKED PROJ PW% CHANGED              
         B     VR20                 AND SKIP PROJ BILLING CHECK                 
                                                                                
VR10A    DS    0H                  CHECK SKED PROJ BILL TOTAL NEXT              
         LA    R2,PWBPJBLH         (R4 IS STILL @ SKED TOTALS ENTRY)            
         TM    (BSLTTLH+4-BSLDSECT)(R2),X80                                     
         BZ    VR10B                                                            
         OI    (ATFLAG-ACCUTABD)(R4),ATFPJBLQ                                   
         OI    BYTE,X20            FLAG THAT SKED PROJ BILL CHANGED             
                                                                                
VR10B    DS    0H                                                               
         B     VR20                                                             
*                                                                               
** MONTHLY COLUMNS **                                                           
*                                                                               
VR20     DS    0H                  MONTHLY CURR PW%                             
         TM    BYTE,X80            WAS SKED CURR PW% CHANGED?                   
         BO    VR25                 YES, DON'T CHECK MONTH CURR PW%             
                                                                                
         LA    R2,PWBCPWH           R2-->1ST MONTH CURR PW%                     
         MVI   FULL,ATFMTTLQ+ATFBILAJ                                           
         MVI   FULL+1,ATFPWQ                                                    
         XC    FULL+2(2),FULL+2                                                 
         BAS   RE,VRMODFLG                                                      
*                                                                               
VR25     DS    0H                                                               
         TM    BYTE,X40+20         WAS SKED PROJECTED STUFF CHANGE?             
         BNZ   VR40                 YES, DON'T CHECK MONTH PROJ STUFF           
                                                                                
         DS    0H                  CHECK MONTHLY PROJECTED PW%                  
         LA    R2,PWBPJPWH          R2-->1ST MONTH PROJ PW%                     
         MVI   FULL,ATFMTTLQ                                                    
         MVI   FULL+1,ATFPWQ                                                    
         XC    FULL+2(2),FULL+2                                                 
         BAS   RE,VRMODFLG                                                      
                                                                                
         DS    0H                  CHECK MONTHLY PROJECT BILLING TOTAL          
         LA    R2,PWBPJBLH          R2-->1ST MONTH PROJ BILLING TOTAL           
         MVI   FULL,ATFMTTLQ                                                    
         MVI   FULL+1,ATFPJBLQ                                                  
         XC    FULL+2(2),FULL+2                                                 
         BAS   RE,VRMODFLG                                                      
                                                                                
         DS    0H                  CHECK MONTHLY ADJ DR/CR                      
         LA    R2,PWBBILLH          R2-->1ST MONTH ADJ DR/CR                    
         MVI   FULL,ATFMTTLQ+ATFBILAJ                                           
         MVI   FULL+1,ATFDRCRQ                                                  
         XC    FULL+2(2),FULL+2                                                 
         BAS   RE,VRMODFLG                                                      
*                                                                               
VR40     DS    0H                                                               
         BAS   RE,SAVEACTB         SAVE AWAY ACCUTAB IN CASE OF ERROR           
         B     VR100                                                            
         EJECT                                                                  
         USING ACCUTABD,R4                                                      
*                                                                               
** CHECK CURRENT PW% FIRST **                                                   
*                                                                               
VR100    DS    0H                  SKED CURR PW%                                
         LA    R2,PWBCPWH+(BSLTTLH-BSLDSECT)                                    
         BAS   RE,LOCSTTL          GET R4 TO SCHED BILL ADJ ENTRY               
         TM    ATFLAG,ATFPWQ                                                    
         BZ    VR110               CHECK MONTHLYS IF NOTHING MODIFIED           
                                                                                
         OI    MISCFLG1,MF1MDFY    FLAG SOMETHING WAS MODIFIED                  
         BAS   RE,MODSPW           PROCESS SCHED PW% MODIFIED                   
         BE    VR150                NO ERROR OCCURRED                           
         B     BADPWE               ERROR OCCURRED                              
                                                                                
                                                                                
VR110    DS    0H                  MONTHLY CURR PW%                             
         LA    R2,PWBCPWH                                                       
         L     R4,AACCUTAB                                                      
VR120    TM    ATFLAG,ATFSTTLQ     IF SCHED TOTAL LINE REACHED,                 
         BO    VR150                FINISHED CHECKING MONTH LINES               
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   VR135                                                            
         TM    ATFLAG,ATFPWQ       SEE IF ANYTHING MODIFIED                     
         BZ    VR130                                                            
                                                                                
         TM    ATFLAG,ATFDRCRQ     CAN'T BE CHANGING DR/CR AS WELL              
         BO    NMDDCPWE                                                         
                                                                                
         OI    MISCFLG1,MF1MDFY    FLAG SOMETHING WAS MODIFIED                  
         BAS   RE,MODMPW           PROCESS WEEKLY PW% MODIFIED                  
         BNE   BADPWE               ERROR OCCURRED                              
                                                                                
VR130    LA    R2,BSLMQ(R2)        BUMP TO NEXT COLUMN                          
         LA    R4,ACCUTABQ(R4)     BUMP PAST BILL ADJ LINE                      
VR135    LA    R4,ACCUTABQ(R4)                                                  
         B     VR120                                                            
*                                                                               
VR150    DS    0H                                                               
         TM    MISCFLG1,MF1MDFY    IF ANY CURRENT PW% MODIFIED,                 
         BZ    VR200                                                            
         MVI   GOSUBN,SUM#          SUM UP ACCUTAB BEFORE CHECKING              
         GOTO1 AGOSUB               OTHER STUFF                                 
         B     VR200                                                            
*                                                                               
** CHECK PROJECTED PW%, PROJECTED BILL TOTAL, & ADJUSTED DR/CR $ **             
*                                                                               
*** SKED PROJECTED PW% ***                                                      
*                                                                               
VR200    DS    0H                  CHECK SKED PROJ PW%                          
         LA    R2,PWBPJPWH+(BSLTTLH-BSLDSECT)                                   
         BAS   RE,LOCSTTL                                                       
         LA    R4,ACCUTABQ(R4)     R4-->SKED TOTALS ENTRY                       
         TM    ATFLAG,ATFPWQ                                                    
         BZ    VR205                                                            
                                                                                
         OI    MISCFLG1,MF1MDFY                                                 
         BAS   RE,MODSPW                                                        
         BE    VR260                                                            
         B     BADPWE                                                           
*                                                                               
*** SKED PROJECTED BILL TOTAL ***                                               
*                                                                               
VR205    DS    0H                                                               
         LA    R2,PWBPJBLH+(BSLTTLH-BSLDSECT)                                   
         BAS   RE,LOCSTTL                                                       
         LA    R4,ACCUTABQ(R4)     R4-->SKED TOTALS ENTRY                       
         TM    ATFLAG,ATFPJBLQ                                                  
         BZ    VR210                                                            
                                                                                
         OI    MISCFLG1,MF1MDFY                                                 
         BAS   RE,MODPFB                                                        
         BE    VR260                                                            
         B     BADPFBE                                                          
*                                                                               
*** MONTHLY STUFF ***                                                           
*                                                                               
VR210    DS    0H                  LOOP THROUGH EACH MONTH,                     
         SR    R3,R3                                                            
         L     R4,AACCUTAB          STARTING W/ FIRST MONTH                     
                                                                                
VR220    DS    0H                                                               
         LA    R2,PWBPJPWH(R3)     CHECK PROJECTED PW% FIRST                    
         TM    ATFLAG,ATFSTTLQ                                                  
         BO    VR260                                                            
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BO    VR255                                                            
         TM    ATFLAG,ATFMTTLQ                                                  
         BZ    VR255                                                            
                                                                                
         DS    0H                  R4-->A MONTH ENTRY                           
         TM    ATFLAG,ATFPWQ       WAS PROJECTED PW% CHANGED?                   
         BZ    VR230                NO, GO CHECK PROJ BILL TOTAL                
                                                                                
         OI    MISCFLG1,MF1MDFY                                                 
         BAS   RE,MODMPW                                                        
         BNE   BADPWE                                                           
         NI    ATFLAG,XFF-ATFPJBLQ-ATFDRCRQ                                     
         B     VR250               IGNORE PROJ BILL & DR/CR FOR MONTH           
                                                                                
                                                                                
VR230    DS    0H                  CHECK MONTHLY PROJ BILL TOTAL                
         LA    R2,PWBPJBLH(R3)                                                  
         TM    ATFLAG,ATFPJBLQ     WAS PROJECTED BILL TOTAL CHANGED?            
         BZ    VR240                NO, GO CHECK ADJ DR/CR                      
                                                                                
         OI    MISCFLG1,MF1MDFY                                                 
         BAS   RE,MODPFB                                                        
         BNE   BADPFBE                                                          
         NI    ATFLAG,XFF-ATFDRCRQ                                              
         B     VR250               IGNORE DR/CR FOR MONTH                       
                                                                                
                                                                                
VR240    DS    0H                  CHECK MONTHLY DR/CR                          
         LA    R2,PWBBILLH(R3)                                                  
         SH    R4,=Y(ACCUTABQ)                                                  
         TM    ATFLAG,ATFDRCRQ                                                  
         BZ    VR245                                                            
                                                                                
         OI    MISCFLG1,MF1MDFY    TURN ON MODIFY FLAG                          
         XC    FULL,FULL                                                        
         CLI   5(R2),0                                                          
         BE    INBLNKE             CAN'T REMOVE ADJ DR/CR AMOUNT                
                                                                                
         DS    0H                  THERE'S INPUT, CHECK FOR WHOLE $'S           
         MVI   BYTE,C'-'                                                        
         MVI   GOSUBN,VWD#                                                      
         GOTO1 AGOSUB                                                           
         BNE   INVLFLD                                                          
                                                                                
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(R0)                                      
         CLI   DMCB,0                                                           
         BNE   INVLFLD                                                          
         OI    FULL,X80            IN CASE $0 INPUTTED                          
         ICM   R0,15,4(R1)                                                      
         BZ    *+8                                                              
         ST    R0,FULL                                                          
         TM    ESTFLAG,EFOWPW      IS THIS PW OOWR BILLING?                     
         BZ    *+14                 NOPE, SO ANY $ AMOUNT IS FINE               
         CLC   FULL,=X'80000000'    YEP, WAS A "0" ENTERRED?                    
         BNE   BAJZEROE              NOPE, GO DISPLAY ERROR MSG                 
         MVC   ATDRCR,FULL         UPDATE THE ADJ DR/CR IN ACCUTAB              
         MVC   ATBILD,CTODAY        AND THE DATE IT WAS UPDATED                 
                                                                                
         DS    0H                  CALCULATE NEW PROJ FINAL BILLING             
         ICM   R1,15,ATAJBUY        = CLT$ CURRENT                              
         AR    R1,R0                 + NEW ADJ DEBIT/CREDIT.                    
         LA    R4,ACCUTABQ(R4)     R4-->MONTH TOTAL ENTRY IN ACCUTAB            
         STCM  R1,15,ATAJBUY       UPDATE NEW PROJ FINAL BILL                   
                                                                                
         DS    0H                  CALCULATE PROJ PW%                           
         ICM   RF,15,ATACBUY                                                    
         BZ    VR243X                                                           
         ST    RF,TEMPACB          USING ACTUAL WIM COST (<>0),                 
         MVC   TEMPAJB,ATAJBUY      PROJ FINAL BILL,                            
         MVC   TEMPTAX,ATTAX        AND ANY TAX AMOUNT                          
         MVI   GOSUBN,PWPW#                                                     
         GOTO1 AGOSUB                                                           
         MVC   ATPW,TEMPPW                                                      
                                                                                
         ICM   R0,15,ATPW          MAKE SURE PROJ PW% IS W/IN RANGE             
         MVI   MYERRCD,ELOPWQ                                                   
         C     R0,=A(MINPWQ)                                                    
         BL    MYERROR                                                          
         MVI   MYERRCD,EHIPWQ                                                   
         CH    R0,=Y(MAXPWQ)                                                    
         BH    MYERROR                                                          
         MVI   MYERRCD,0                                                        
                                                                                
VR243X   DS    0H                                                               
         B     VR250                                                            
                                                                                
VR245    DS    0H                                                               
         LA    R4,ACCUTABQ(R4)                                                  
         B     VR250                                                            
                                                                                
                                                                                
VR250    LA    R3,BSLMQ(R3)        BUMP DISPLACEMENT TO NEXT COLUMN             
VR255    LA    R4,ACCUTABQ(R4)     BUMP ACCUTAB POINTER                         
         B     VR220                                                            
         DROP  R4                                                               
*                                                                               
VR260    DS    0H                                                               
         MVI   GOSUBN,SUM#         SUM UP ACCUTAB IN CASE SOMETHING             
         GOTO1 AGOSUB               WAS MODIFIED                                
         B     VR300                                                            
*                                                                               
VR300    DS    0H                                                               
         B     VR400                                                            
         EJECT                                                                  
*---------------------------- UPDATE FILE ----------------------------*         
*                                                                               
VR400    DS    0H                                                               
         MVI   GOSUBN,OKUPD#                                                    
         GOTO1 AGOSUB              OKAY TO UPDATE FILE?                         
         BE    VR400OK                                                          
         LA    R0,*                                                             
         ST    R0,ALOCERR           NO, SET ADDR WHERE ERROR IS CAUGHT          
         B     XCUERE                                                           
VR400OK  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         TM    MISCFLG1,MF1MDFY    IF SOMETHING MODIFIED,                       
         BZ    VR402                                                            
                                                                                
         MVI   GOSUBN,UPT#          UPDATE PWTAB FOR NEW PW%s,                  
         GOTO1 AGOSUB                                                           
         OI    MISCFLG1,MF1RRDCH    AND RE-READ FILE BEFORE DISPLAY             
         MVI   PFKEY,0                                                          
         B     VR404                                                            
*                                                                               
VR402    B     VRX                 NO MODIFICATION, NO PFKEY                    
*                                                                               
** MARKET-LEVEL PW RECORD **                                                    
*                                                                               
VR404    MVI   RDUPDATE,C'Y'       READING FOR UPDATE                           
         MVI   GOSUBN,GPR#         GET PW RECORD (KEY IS BUILT TOO)             
         GOTO1 AGOSUB                                                           
         BE    *+6                                                              
         DC    H'0'                RECORD HAD BETTER EXIST                      
         L     R6,AIO                                                           
         USING PWRECD,R6                                                        
                                                                                
VR405    GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWWKCODQ',(R6)),0,0                   
         CLI   12(R1),0                                                         
         BE    VR405A                                                           
         CLI   12(R1),X'06'                                                     
         BE    VR405X                                                           
         DC    H'0'                                                             
VR405A   GOTO1 HELLO,DMCB,(C'D',SYSFIL),('PWWKCODQ',(R6)),0,0                   
         CLI   12(R1),0            KEEP DELETING UNTIL                          
         BE    VR405                ELEMENT NOT FOUND                           
         DC    H'0'                                                             
VR405X   DS    0H                                                               
                                                                                
         LA    R3,ELEMENT          BUILD PW% ELEMENTS                           
         USING PWWKEL,R3                                                        
         LA    R4,PWTAB                                                         
VR410    CLI   0(R4),XFF           FINISHED BUILDING PW ELEMENTS                
         BE    VR430                                                            
         XC    ELEMENT,ELEMENT                                                  
         MVI   PWWKCD,PWWKCODQ     ELEMENT CODE                                 
         MVI   PWWKLEN,PWWKLENQ    ELEMENT LENGTH                               
         MVC   PWWKDATE,0(R4)      START DATE OF BRDCST WEEK                    
         MVC   PWWKPCT,4(R4)       PW%                                          
         DROP  R3                                                               
                                                                                
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R6),(R3),0                             
         CLI   12(R1),0            CHECK FOR ERRORS                             
         BE    VR415                IF NONE, BUILD NEXT ELEMENT                 
         DC    H'0'                 ELSE, DIE                                   
*                                                                               
VR415    LA    R4,8(R4)                                                         
         B     VR410                                                            
*                                                                               
VR430    DS    0H                                                               
         L     R6,AIO                                                           
         TM    ESTFLAG,EFOWPW      IF OOWR PW BILLING,                          
         BNZ   VR435                GO UPDATE OOW PAID$ ELEMENT                 
*                                                                               
         DS    0H                                                               
         MVI   GOSUBN,DDC#         UPDATE PWDOL ELEMENTS                        
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,DCD#         UPDATE PWCUR ELEMENTS                        
         GOTO1 (RF)                                                             
         B     VR439                                                            
*                                                                               
VR435    DS    0H                                                               
         MVI   GOSUBN,DOD#         UPDATE PWOOW ELEMENTS                        
         GOTO1 AGOSUB                                                           
VR439    EQU   *                                                                
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC              PUT MKT-LEVEL RECORD BACK                    
*                                                                               
         DS    0H                  COPY MKT-LEVEL PW RECD INTO IO2              
         L     R0,AIO2                                                          
         LA    R1,L'IO                                                          
         L     RE,AIO                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         EJECT                                                                  
*                                                                               
** UPDATE STATION-LEVEL RECORDS **                                              
*                                                                               
         MVI   GOSUBN,BPK#                                                      
         GOTO1 AGOSUB                                                           
         LA    R6,KEY                                                           
         USING PWRECD,R6                                                        
         MVI   PWKSTA+2,1                                                       
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         B     VR450A                                                           
VR450    GOTO1 SEQ                                                              
VR450A   CLC   KEY(PKYMKTL),KEYSAVE                                             
         BNE   VR480                                                            
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING PWRECD,R6                                                        
         OC    BSTA,BSTA           WAS A SPECIFIC STATION REQUESTED?            
         BZ    VR460                                                            
         MVC   TEMPBSTA,BSTA                                                    
         TM    BSTA,X'F0'                                                       
         BNO   *+10                                                             
         NC    TEMPBSTA,CBLSCMSK                                                
*&&DO                                                                           
         CLC   PWKSTA,BSTA          YES, IS THIS THE PW RECD FOR IT?            
*&&                                                                             
         CLC   PWKSTA,TEMPBSTA      YES, IS THIS THE PW RECD FOR IT?            
         BNE   VR464                 NO, PWKSTA WON'T BE IN STACTAB             
*                                                                               
VR460    DS    0H                  MARK STATIONS GETTING PROCESSED              
         L     R4,TSARBLK+(TSAREC-TSARD)                                        
         LA    R4,0(R4)                                                         
         USING STACRECD,R4                                                      
         XC    STACRECD(STACRECL),STACRECD                                      
         MVC   STACSTA,PWKSTA                                                   
         MVI   GOSUBN,TSR_RDH#                                                  
                                                                                
VR462    DS    0H                                                               
         GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF      EOF OF STTN ACCUM TABLE?                    
         BNZ   VR462X                YEP                                        
         CLC   STACSTA,PWKSTA                                                   
         BNE   VR462G                                                           
         OI    STACFLG,STAFPRCQ                                                 
         MVI   GOSUBN,TSR_PUT#                                                  
         GOTO1 AGOSUB                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
VR462G   EQU   *                                                                
                                                                                
         MVI   GOSUBN,TSR_NXT#                                                  
         B     VR462                                                            
VR462X   EQU   *                                                                
         DROP  R4                                                               
*                                                                               
VR464    DS    0H                                                               
         TM    ESTFLAG,EFOWPW      IF OOWR PW BILLING,                          
         BNZ   VR465                UPDATE OOW PAID$ ELEMENT                    
*                                                                               
         DS    0H                                                               
         MVI   GOSUBN,DDC#         UPDATE STATION-LEVEL PW RECD                 
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,DCD#         "UPDATE" PWCUR ELEMENTS                      
         GOTO1 (RF)                                                             
         B     VR469                                                            
*                                                                               
VR465    DS    0H                                                               
         MVI   GOSUBN,DOD#         UPDATE PWOOW ELEMENTS                        
         GOTO1 AGOSUB                                                           
VR469    EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         GOTO1 PUTREC              PUT STATION-LEVEL RECORD BACK                
         B     VR450                                                            
         DROP  R6                                                               
                                                                                
*                                                                               
VR480    DS    0H                  ADD PW STATION RECDS IF NEEDED               
         L     R4,TSARBLK+(TSAREC-TSARD)                                        
         LA    R4,0(R4)                                                         
         USING STACRECD,R4                                                      
         XC    STACRECD(STACRECL),STACRECD                                      
         MVI   GOSUBN,TSR_RDH#                                                  
                                                                                
VR482    DS    0H                  BUMP THROUGH STATION ACCUM TABLE,            
         GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF      EOF OF STTN ACCUM TABLE?                    
         BNZ   VR490                 YEP                                        
         TM    STACFLG,STAFPRCQ     AND SEE IF ANYTHING NOT PROCESSED           
         BZ    VR484                                                            
         MVI   GOSUBN,TSR_NXT#                                                  
         B     VR482                                                            
                                                                                
VR484    DS    0H                  A STATION NOT PROCESSED                      
         OC    STACSTA,STACSTA     IF THIS IS NULLS,                            
         BNZ   *+6                                                              
         DC    H'0'                 MKT-LEVEL RECD WILL GET CLOBBERED!          
         L     R6,AIO                                                           
                                                                                
         LR    R0,R6                                                            
         LA    R1,L'IO                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR AREA BEFORE BUILDING RECORD            
                                                                                
         USING PWRECD,R6                                                        
         MVI   GOSUBN,BPK#                                                      
         GOTO1 AGOSUB                                                           
         MVC   KEY+(PWKSTA-PWFKEY)(L'PWKSTA),STACSTA                            
                                                                                
         DS    0H                  BUILD RECORD FOR IT                          
         MVC   PWFKEY,KEY           PW KEY                                      
*&&DO                                                                           
         MVC   PWKSTA,STACSTA       KEY FIELD: STATION                          
*&&                                                                             
         MVC   PWAGYA,TWAAGY        AGENCY TAG                                  
         XC    PWEL(PWGNLENQ),PWEL                                              
         MVI   PWGNEL,PWGNELQ       X'01' ELEMENT                               
         MVI   PWGNLEN,PWGNLENQ      ELEM LENGTH                                
         OI    PWGNFLG,PWGNBPLK      LOCK FLAG (CONSISTNT W/ PWMKT REC)         
         TM    LOCKFLAG,LKFFLCKQ                                                
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNBILQ       BUY LOCK                                  
         TM    LOCKFLAG,LKFFPLKQ                                                
         BZ    *+8                                                              
         OI    PWGNFLG,PWGNPLKQ       PW  LOCK                                  
         MVC   PWGNGOAL,SKEDCGTX     SKED'S ACTUAL GOAL TOTAL (W/ TAX)          
         MVC   PWGNGRP,SKEDGRP       SKED'S TOTAL GRP                           
         MVC   PWGNTAX,GLTXRATE      GOAL TAX RATE                              
         MVI   PWEL+PWGNLENQ,0      EORECORD MARKER                             
         LA    R0,(PWEL-PWFKEY+PWGNLENQ+1)                                      
         STCM  R0,3,PWLEN           L(RECORD) SO FAR                            
                                                                                
         TM    ESTFLAG,EFOWPW      IF OOWR PW BILLING,                          
         BNZ   VR485M               UPDATE OOW PAID$ ELEMENT                    
*                                                                               
         DS    0H                                                               
         MVI   GOSUBN,DDC#         PUT IN ADJ DR/CR VALUES                      
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,DCD#         "UPDATE" PWCUR ELEMENTS                      
         GOTO1 (RF)                                                             
         B     VR485X                                                           
*                                                                               
VR485M   DS    0H                                                               
         MVI   GOSUBN,DOD#         UPDATE PWOOW ELEMENTS                        
         GOTO1 AGOSUB                                                           
VR485X   EQU   *                                                                
*                                                                               
         DS    0H                  DETERMINE WHETHER TO ADD OR PUT RECD         
         OI    DMINBTS,X08          READ FOR DELETES                            
         GOTO1 HIGH                                                             
         NI    DMINBTS,XFF-X08                                                  
                                                                                
         L     RF,ADDREC            ASSUME WE'LL NEED TO ADDREC                 
         CLC   KEY(L'PWFKEY),KEYSAVE  RECORD IS NOT ON FILE,                    
         BNE   VR485C                  GO ADD IT NOW                            
                                                                                
         L     R0,AIO               REMEMBER WHAT'S IN AIO                      
         MVC   AIO,AIO3                                                         
         OI    DMINBTS,X08                                                      
         GOTO1 GETREC               GET RECD BEFORE PUTTING IT                  
         NI    DMINBTS,XFF-X08                                                  
         ST    R0,AIO               RESTORE AIO                                 
                                                                                
         NI    KEY+L'PWFKEY,XFF-X80 TURN OFF DELETE IN KEY                      
         GOTO1 WRITE                 AND WRITE IT BACK TO FILE                  
         L     RF,PUTREC                                                        
         B     VR485C                                                           
                                                                                
VR485C   DS    0H                                                               
         GOTO1 (RF)                RF = A(ADDREC) OR A(PUTREC)                  
*&&DO                                                                           
         GOTO1 ADDREC              PUT STATION-LEVEL RECORD BACK                
*&&                                                                             
                                                                                
         DS    0H                  MARK ALL ENTRIES FOR STATION PRCSSED         
VR486A   OI    STACFLG,STAFPRCQ                                                 
         MVI   GOSUBN,TSR_PUT#                                                  
         GOTO1 AGOSUB                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VR486B   DS    0H                                                               
         MVI   GOSUBN,TSR_NXT#                                                  
         GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF                                                  
         BNZ   VR480                                                            
         CLC   STACSTA,PWKSTA                                                   
         BE    VR486A                                                           
         BNE   VR486B                                                           
         DROP  R4,R6                                                            
                                                                                
*                                                                               
VR490    DS    0H                                                               
*                                                                               
** EXIT TASKS **                                                                
*                                                                               
VR500    DS    0H                                                               
         TM    MISCFLG1,MF1RRDCH   NEED TO RE-READ FILE?                        
         BZ    *+12                 NOPE                                        
         BAS   RE,PWSETUP                                                       
         NI    MISCFLG1,XFF-MF1RRDCH                                            
                                                                                
         B     DR                  GO AND RE-DISPLAY VALUES                     
*                                                                               
VRX      MVI   GOSUBN,STI#         SAVE OFF TABLES IN TIA                       
         GOTO1 AGOSUB                                                           
         B     XIT                                                              
         EJECT                                                                  
*--------------------- SET MODIFIED FLAGS ROUTINE --------------------*         
                                                                                
* This routine is called within VALREC only.  It is a generic routine           
*  used to turn on modify flags in ACCUTAB's month entries only.                
* At entry,                                                                     
*   R2-->line on screen we're checking for modification                         
*   FULL(1) = flags telling us which ACCUTAB entry to apply mdfy flgs           
*   FULL+1(1) = which modify flag to turn on                                    
*   FULL+2(1) will be used as a work byte in this routine                       
* At exit,                                                                      
*   FULL+3(1) = C'N' if no flags were turned on, else C'Y'                      
                                                                                
         DS    0H                                                               
VRMODFLG NTR1                                                                   
         MVI   FULL+3,C'N'                                                      
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
                                                                                
VRMF10   DS    0H                                                               
         TM    ATFLAG,ATFSTTLQ     IF SKED ENTRY REACHED,                       
         BO    VRMFX                GO EXIT                                     
         MVI   FULL+2,ATFMTTLQ+ATFBILAJ                                         
         NC    FULL+2(1),ATFLAG                                                 
         CLC   FULL+2(1),FULL                                                   
         BNE   VRMF20                                                           
                                                                                
         DS    0H                  FOUND ACCUTAB ENTRY                          
         TM    4(R2),X80           IF VALUE MODIFIED ON SCREEN,                 
         BZ    VRMF18                                                           
         OC    ATFLAG,FULL+1        TURN MODIFY FLAG ON                         
         MVI   FULL+3,C'Y'                                                      
                                                                                
VRMF18   DS    0H                                                               
         LA    R2,BSLMQ(R2)        BUMP TO NEXT MONTH/COLUMN                    
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   VRMF20                                                           
         LA    R4,ACCUTABQ(R4)                                                  
                                                                                
VRMF20   DS    0H                                                               
         LA    R4,ACCUTABQ(R4)                                                  
         B     VRMF10                                                           
*                                                                               
VRMFX    DS    0H                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*--------------------- MY VALREC ERRORS & WARNINGS -------------------*         
                                                                                
MPCTE    DS    0H                                                               
         MVI   MYERRCD,MPCTQ                                                    
         B     MYERROR                                                          
                                                                                
BADPWE   CLI   MYERRCD,0           IF IT IS NOT MY TYPE OF ERROR,               
         BE    INVLFLD              THEN IT'S AN INVALID FIELD                  
         BNE   MYERROR              ELSE, LET ME PROCESS IT                     
                                                                                
BADPFBE  CLI   MYERRCD,0           IF IT IS NOT MY TYPE OF ERROR,               
         BE    INVLFLD              THEN IT'S AN INVALID FIELD                  
         BNE   MYERROR              ELSE, LET ME PROCESS IT                     
                                                                                
NDRCRE   NI    MISCFLG1,XFF-MF1MDFY    TURN OFF THESE FLAGS                     
         MVI   MYERRCD,NDRCRQ      CAN'T CHANGE EXISTING ADJUSTED               
         B     MYERROR              BILLING                                     
                                                                                
INBLNKE  DS    0H                                                               
         MVI   MYERRCD,INBLNKQ     INPUT CAN'T BE BLANK                         
         B     MYERROR                                                          
                                                                                
NMDDCPWE DS    0H                                                               
         MVI   MYERRCD,NMDDCPWQ    CAN'T CHANGE DR/CR & PW%                     
         B     MYERROR                                                          
                                                                                
BAJZEROE DS    0H                                                               
         MVI   MYERRCD,BAJZEROQ    ONLY VALID INPUT FOR BILL ADJ IS 0           
         B     MYERROR                                                          
                                                                                
XCUERE   DS    0H                                                               
         MVI   MYERRCD,XCUERQ                                                   
         LA    R2,CONSERVH                                                      
         B     MYERROR                                                          
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL OVERLAY (DISPREC)'                 
***********************************************************************         
*=========================== DISPLAY RECORD ==========================*         
DR       DS    0H                                                               
                                                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
DR10A    NI    ATFLAG,XFF-ATFPWQ                                                
         LA    R4,ACCUTABQ(R4)     RESET MODIFIED FLAGS ON DISPLAY              
         CLI   0(R4),XFF                                                        
         BNE   DR10A                                                            
         NI    MISCFLG1,XFF-MF1MDFY                                             
         NI    MISCFLG1,XFF-MF1BALL0                                            
         NI    MISCFLG2,XFF-MF2MRFB-MF2UPSBM                                    
         XC    ACURSINF,ACURSINF                                                
         MVC   MNTHINF,SPACES                                                   
                                                                                
         MVI   GOSUBN,GPR#         GET PW RECORD                                
         GOTO1 AGOSUB                                                           
*                                                                               
** CLT$ **                                                                      
*                                                                               
         LA    R2,PWBLCLTH         LOCKED  CLT$                                 
         LA    R3,PWBCCLTH         CURRENT CLT$                                 
         LA    R6,PWBDCLTH         DIFFERENCE                                   
         MVI   BYTE,C'C'                                                        
         BAS   RE,DBS                                                           
         B     DR30                                                             
*                                                                               
** LOCKED & CURRENT PW% **                                                      
*                                                                               
DR30     LA    R2,PWBLPWH          LOCKED  PW%                                  
         LA    R3,PWBCPWH          CURRENT PW%                                  
         L     R4,AACCUTAB                                                      
DR35     TM    ATFLAG,ATFSTTLQ                                                  
         BO    DR35E                                                            
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   DR35D                                                            
*                                                                               
*** MONTHLY COLUMNS ***                                                         
*                                                                               
         DS    0H                  CURRENT PW% LINE                             
         OI    6(R3),X'80'                                                      
         MVC   8(7,R3),=C'**N/A**' ASSUME NO SPOTS                              
*&&DO                                                                           
         TM    ESTFLAG,EFOWPW       IS IT A OOW PW BILLING ESTIMATE?            
         BZ    *+14                  NO                                         
         MVC   8(7,R3),=C'**OWPW*'   YES, MOVE FLAG IN                          
         B     DR35B                                                            
                                                                                
*&&                                                                             
         TM    ATFLAG,ATFNOSPT      ANY SPOTS HERE?                             
         BNO   *+12                  YEP, THERE'S AT LEAST 1 SPOT               
         TM    ATFLAG2,ATF2OOWP       NOPE, BUT WERE OOWR PAID$ USED?           
         BNO   DR35B                   NOPE                                     
         OC    ATACBUY,ATACBUY      DO THEY ADD UP TO ZERO?                     
         BZ    DR35B                YEP                                         
                                                                                
         XC    8(L'PWBM1,R3),8(R3) AT LEAST 1 SPOT & SUM<>0                     
         MVI   8(R3),C'0'           ASSUME PW%=0                                
         ICM   R1,15,ATPW                                                       
         BZ    DR35B                                                            
         MVC   8(7,R3),=C'*RANGE*'  ASSUME OUT-OF-RANGE                         
         OC    ATAJBUY,ATAJBUY       IF CLCOST IS ZERO                          
         BZ    DR35B                  AUTOMATICALLY OUT-OF-RANGE                
         C     R1,=A(MAXPWQ)                                                    
         BH    DR35B                                                            
         C     R1,=A(MINPWQ)                                                    
         BL    DR35B                                                            
         EDIT  (R1),(8,8(R3)),2,ALIGN=LEFT,FLOAT=-                              
*                                                                               
DR35B    DS    0H                  LOCKED PW% LINE                              
         OI    6(R2),X'80'                                                      
         MVC   8(7,R2),=C'**N/A**' ASSUME LOCKED PW% NON-APPLICABLE             
*&&DO                                                                           
         TM    ESTFLAG,EFOWPW       IS IT A OOW PW BILLING ESTIMATE?            
         BZ    *+14                  NO                                         
         MVC   8(7,R2),=C'**OWPW*'   YES, MOVE FLAG IN                          
         B     DR35C                                                            
                                                                                
*&&                                                                             
*&&DO                                                                           
         L     RF,AIO                                                           
         TM    (PWGNFLG-PWRECD)(RF),PWGNBPLK  SKIP IF NEVER LOCKED BFOR         
         BZ    DR35C                                                            
         TM    ATFLAG,ATFNOSPT                SKIP IF NO SPOTS HERE             
         BO    DR35C                                                            
*&&                                                                             
         OC    ATWLCK,ATWLCK                  SKIP IF ADD UP TO ZERO            
         BZ    DR35C                                                            
                                                                                
         XC    8(L'PWBM1,R2),8(R2)  NEED TO EDIT PW%                            
         MVI   8(R2),C'0'           ASSUME PW%=0                                
         ICM   R1,15,ATLPW                                                      
         BZ    DR35C                                                            
         MVC   8(7,R2),=C'*RANGE*'  ASSUME OUT-OF-RANGE                         
         OC    ATCLCK,ATCLCK         IF LOCKED CLCOST IS ZERO                   
         BZ    DR35C                  THEN IT'S OUT-OF-RANGE                    
         C     R1,=A(MAXPWQ)                                                    
         BH    DR35C                                                            
         C     R1,=A(MINPWQ)                                                    
         BL    DR35C                                                            
         EDIT  (R1),(8,8(R2)),2,ALIGN=LEFT,FLOAT=-                              
*                                                                               
DR35C    LA    R2,BSLMQ(R2)                                                     
         LA    R3,BSLMQ(R3)                                                     
         LA    R4,ACCUTABQ(R4)     BUMP PAST BILL ADJ LINE                      
DR35D    LA    R4,ACCUTABQ(R4)                                                  
         B     DR35                                                             
*                                                                               
*** TOTALS COLUMN ***                                                           
*                                                                               
DR35E    LA    R3,PWBCPWH          TOTAL CURRENT PW%                            
         USING BSLDSECT,R3                                                      
         OI    BSLTTLH+6,X'80'                                                  
         MVC   BSLTTL(7),=C'**N/A**'                                            
*&&DO                                                                           
         TM    ESTFLAG,EFOWPW       IS IT A OOW PW BILLING ESTIMATE?            
         BZ    *+14                  NO                                         
         MVC   BSLTTL(7),=C'**OWPW*' YES, MOVE FLAG IN                          
         B     DR35G                                                            
                                                                                
*&&                                                                             
         TM    ATFLAG,ATFNOSPT     ANY SPOTS IN SKED?                           
         BO    DR35G                                                            
         OC    ATACBUY,ATACBUY     DO THEY ADD UP TO ZERO                       
         BZ    DR35G                                                            
         TM    ATFLAG2,ATF2BILL    IF BILLED,                                   
         BZ    *+8                                                              
         OI    BSLTTLH+1,X20        PROTECT FIELD                               
                                                                                
         XC    BSLTTL,BSLTTL        NEED TO EDIT PW%                            
         MVI   BSLTTL,C'0'            ASSUME PW%=0                              
         ICM   R1,15,ATPW                                                       
         BZ    DR35G                                                            
         MVC   BSLTTL(7),=C'*RANGE*'  ASSUME OUT-OF-RANGE                       
         OC    ATAJBUY,ATAJBUY        IF CLCOST IS ZERO                         
         BZ    DR35G                   AUTOMATICALLY OUT-OF-RANGE               
         C     R1,=A(MAXPWQ)                                                    
         BH    DR35G                                                            
         C     R1,=A(MINPWQ)                                                    
         BL    DR35G                                                            
         EDIT  (R1),(8,BSLTTL),2,ALIGN=LEFT,FLOAT=-                             
         DROP  R3                                                               
                                                                                
                                                                                
DR35G    LA    R2,PWBLPWH          TOTAL LOCKED PW%                             
         USING BSLDSECT,R2                                                      
         OI    BSLTTLH+6,X'80'                                                  
         MVC   BSLTTL(7),=C'**N/A**'                                            
*&&DO                                                                           
         TM    ESTFLAG,EFOWPW       IS IT A OOW PW BILLING ESTIMATE?            
         BZ    *+14                  NO                                         
         MVC   BSLTTL(7),=C'**OWPW*' YES, MOVE FLAG IN                          
         B     DR35H                                                            
                                                                                
*&&                                                                             
         OC    ATWLCK,ATWLCK                                                    
         BZ    DR35H                                                            
                                                                                
         XC    BSLTTL,BSLTTL        NEED TO EDIT PW%                            
         MVI   BSLTTL,C'0'            ASSUME PW%=0                              
         ICM   R1,15,ATLPW                                                      
         BZ    DR35H                                                            
         MVC   BSLTTL(7),=C'*RANGE*'  ASSUME OUT-OF-RANGE                       
         OC    ATCLCK,ATCLCK         IF LOCKED CLCOST IS ZERO                   
         BZ    DR35H                  THEN IT'S OUT-OF-RANGE                    
         C     R1,=A(MAXPWQ)                                                    
         BH    DR35H                                                            
         C     R1,=A(MINPWQ)                                                    
         BL    DR35H                                                            
         EDIT  (R1),(8,BSLTTL),2,ALIGN=LEFT,FLOAT=-                             
         DROP  R2                                                               
DR35H    B     DR38                                                             
*                                                                               
** WIM$ **                                                                      
*                                                                               
DR38     LA    R2,PWBLWIMH         LOCKED  WIM$                                 
         LA    R3,PWBCWIMH         CURRENT WIM$                                 
         LA    R6,PWBDWIMH         DIFFERENCE                                   
         MVI   BYTE,C'W'                                                        
         BAS   RE,DBS                                                           
*                                                                               
** MAKEGOODS **                                                                 
*                                                                               
*^^SUP   LA    R2,PWBMGINH         MAKEGOODS IN                                 
*^^SUP   LA    R3,PWBMGOTH         MAKEGOODS OUT                                
*^^SUP   LA    R6,PWBADJMH         COST ADJUSTMENTS                             
*^^SUP   MVI   BYTE,C'M'                                                        
*^^SUP   BAS   RE,DBS                                                           
*^^SUP   B     DR50                                                             
*                                                                               
** ADJUSTED DR/CR, PROJECTED BILLING & PROJECT PW% FIELDS **                    
*                                                                               
DR50     XC    CURRSUM,CURRSUM     USE CURRSUM TO KEEP DR/CR TOTAL              
         XC    LOCKSUM,LOCKSUM     USE LOCKSUM TO KEEP PJ BILL TOTAL            
         XC    ADRCRRFB,ADRCRRFB   NOTHING NEEDS RE-FNL BILL YET                
         MVI   BYTE,0              USE AS DISPLAY FLAG (TOTALS COLM)            
         LA    R2,PWBBILLH                                                      
         LA    R6,PWBPJBLH                                                      
         LA    R3,PWBPJPWH                                                      
         L     R4,AACCUTAB         DRIVING OFF ACCUTAB                          
         USING ACCUTABD,R4                                                      
*                                                                               
*** DR/CR LINE ***                                                              
*                                                                               
DR50A    CLI   0(R4),XFF                                                        
         BE    DR60                                                             
         TM    ATFLAG,ATFSTTLQ                                                  
         BO    DR50B                                                            
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   DR58                BUMP TO A TOTALS LINE                        
         B     DR51                                                             
*                                                                               
DR50B    DS    0H                                                               
         LA    R2,PWBBILLH+(BSLTTLH-BSLDSECT)                                   
         XC    8(L'PWBM1,R2),8(R2)                                              
         OI    6(R2),X'80'                                                      
         CLI   BYTE,C'*'           IF FLAGGED TO DISPLAY IN TOTALS              
         BE    DR52                 COLUMN, GO DISPLAY IT                       
*                                                                               
DR51     DS    0H                                                               
         XC    8(L'PWBM1,R2),8(R2) GET READY TO EDIT DR/CR AMOUNT               
         OI    6(R2),X'80'                                                      
         TM    ATFLAG,ATFNOSPT     ANY SPOTS IN MONTH/SKED?                     
         BNO   DR51AG                  YEP                                      
         TM    ATFLAG2,ATF2ESTB        NO, IS MONTH ESTIM BILLED?               
         BNZ   DR51AG                   YES                                     
         MVC   8(L'PWBM1,R2),NSPTMRK    NO                                      
         B     DR55                                                             
DR51AG   EQU   *                                                                
*                                                                               
*&&DO                                                                           
DR51A    OC    ATDRCR,ATDRCR       IGNORE PAY STATUS IF DR/CR HAD BEEN          
         BNZ   DR52                 INPUTTED PREVIOUSLY (DR/CR <> NULL)         
                                                                                
*&&                                                                             
         TM    ATFLAG2,ATF2OOWP    USED OOWR PAID DOLLARS?                      
         BO    DR51C                    YEP                                     
DR51B    TM    ATFLAG,ATFUNPDQ     ANY SPOTS UNPAID IN MONTH/SKED?              
         BZ    DR51C                    NO                                      
         MVC   8(L'PWBM1,R2),UNPDMRK    YEP                                     
         TM    ATFLAG2,ATF2BILL    IF MONTH IS BILLED,                          
         BZ    DR54X                DISPLAY INFO MESSAGE                        
         TM    MISCFLG2,MF2UPSBM   DON'T CLOBBER WHAT WAS SET BEFORE            
         BNZ   DR54X                                                            
         OI    MISCFLG2,MF2UPSBM   FLAG--SHOW THIS INFO MSG                     
         ST    R2,ACURSINF         SET A(CURSOR) FOR INFO MSG                   
         MVC   MNTHINF,=C'PER'     SET MONTH PERTAINING TO INFO MSG             
         TM    ESTFLAG,EFBILEST                                                 
         BO    DR51BG                                                           
         GOTO1 DATCON,DMCB,(2,ATMSTART),(0,MYDATE6),0                           
         GOTO1 AGETBROD,DMCB,(1,MYDATE6),STARTEND,AGETDAY,AADDAY                
         GOTO1 DATCON,DMCB,(0,STARTEND+6),(4,WORK),0                            
         MVC   MNTHINF,WORK                                                     
DR51BG   EQU   *                                                                
         LA    R0,MYTEXT+1         FORMAT SUBSTITUTE TEXT                       
         LR    R1,R0                                                            
         MVC   0(L'MNTHINF,R1),MNTHINF                                          
         LA    R1,L'MNTHINF+1(R1)                                               
         MVC   0(2,R1),=C'is'                                                   
         LA    R1,2+1(R1)                                                       
         MVC   0(5,R1),=C'final'                                                
         LA    RF,5                                                             
         TM    ATFLAG2,ATF2FNLB                                                 
         BNZ   *+14                                                             
         MVC   0(8,R1),=C'estimate'                                             
         LA    RF,8                                                             
         AR    R1,RF                                                            
         SR    R1,R0                R1 = L(SUBSTITUTE TEXT)                     
         STC   R1,MYTEXT            PUT L(SUBSTITUTE TEXT) HERE                 
         B     DR54X                                                            
*                                                                               
DR51C    DS    0H                  THIS CODE IS IF WE CAME FROM BLIST           
         CLI   MYCALLSP+1,1        DID WE COME FROM LIST SCREEN?                
         BNE   DR51D                                                            
         CLI   CALLPFK,4           IS SEL CODE EQUIV TO PF4?                    
         BNE   DR51D                                                            
         OC    ATDRCR,ATDRCR       ANY DR/CR AMOUNT?                            
         BNZ   DR51D                                                            
         OC    BINYRMT,BINYRMT     ANY YEAR/MONTH?                              
         BZ    DR51D                                                            
         GOTO1 DATCON,DMCB,(2,ATMEND),(3,MYDATE6),0                             
         CLC   BINYRMT,MYDATE6     DO YEAR/MONTH MATCH?                         
         BNE   DR51D                                                            
         MVI   8(R2),C'0'          FORCE A ZERO IN CLT$ ADJ DR/CR               
         OI    6(R2),X01            AND FORCE FOR MODFIED FOR NXT INPUT         
         OI    MISCFLG1,MF1BALL0    AND FLAG FOR INFO MESSAGE                   
         XC    BINYRMT,BINYRMT     CLEAR FOR NEXT TIME AROUND                   
         ST    R2,ABALL0            AND PLACE CURSOR IN THIS FIELD              
         B     DR55                                                             
*                                                                               
DR51D    DS    0H                                                               
         B     DR52                                                             
*                                                                               
DR52     TM    ATFLAG,ATFSTTLQ     WAS THIS A SKED LINE?                        
         BZ    DR52A                NOPE, IT'S A MONTH LINE                     
         CLI   BYTE,C'*'           CAN WE DISPLAY OR LEAVE BLANK?               
         BNE   DR55                 LEAVE BLANK                                 
         MVI   8(R2),C'0'                                                       
         ICM   R0,15,CURRSUM                                                    
         BZ    DR55                                                             
         MVI   GOSUBN,RUP#         ROUND OFF BEFORE DISPLAYING                  
         GOTO1 AGOSUB                                                           
         L     R1,FULL                                                          
         B     DR54                 GO EDIT NUMBER                              
*                                                                               
DR52A    DS    0H                                                               
         OC    ATDRCR,ATDRCR                                                    
         BNZ   DR52D                                                            
         TM    ATFLAG2,ATF2FNLB    CHECK IF SET UP FOR RE-FNL BILL              
         BZ    DR54X                NOPE                                        
         TM    MISCFLG1,MF1IMSG     YES, WAS SOMETHING SET UP PREV?             
         BNZ   DR54X                 YEP, THESE MSGS GET PRECEDENCE             
         TM    MISCFLG2,MF2IMSG                                                 
         BNZ   DR54X                 YEP, DISPLAY WHAT'S SET UP ALREADY         
         OC    ADRCRRFB,ADRCRRFB                                                
         BNZ   DR54X                 YEP, DON'T CLOBBER ADDR                    
         ST    R2,ADRCRRFB           NOPE, CURS POINTS HERE W/ INFO MSG         
         OI    MISCFLG2,MF2MRFB                                                 
         B     DR54X                                                            
                                                                                
DR52D    DS    0H                                                               
         MVI   BYTE,C'*'           MAKE ZERO=NOBLANK FOR TOTALS COLM            
         MVI   8(R2),C'0'                                                       
         ICM   R0,15,ATDRCR                                                     
         LR    R1,R0                                                            
         SLL   R1,1                CHECK IF H.O. BIT WAS ON                     
         OR    R1,R1                AND THE REST ZEROES                         
         BZ    DR55                IF SO, DISPLAY THE '0'                       
         LR    R1,R0                                                            
         A     R1,CURRSUM                                                       
         ST    R1,CURRSUM          UPDATE RUNNING TOTAL                         
         MVI   GOSUBN,RUP#                                                      
         GOTO1 AGOSUB                                                           
         L     R1,FULL             R1=ROUNDED DR/CR AMOUNT FOR DISPLAY          
*                                                                               
DR54     EDIT  (R1),(8,8(R2)),ALIGN=LEFT,FLOAT=-                                
DR54X    EQU   *                   DONE W/ DR/CR FOR THIS MONTH                 
*                                                                               
*** PROJECTED BILLING LINE ***                                                  
*                                                                               
DR55     TM    ATFLAG,ATFSTTLQ     IF WE'RE AT THE SKED LINE                    
         BZ    DR55A                                                            
         L     R0,LOCKSUM           DISPLAY THE TOTAL AMOUNT                    
         LA    R6,PWBPJBLH+(BSLTTLH-BSLDSECT)  IN TOTALS COLUMN                 
         B     DR55B                                                            
         AR    R0,R1                IS THE PROJECTED BILLING AMOUNT             
DR55A    ICM   R0,15,ATAJBUY+ACCUTABQ   GET THE PROJ BILLING AMOUNT             
         LR    R1,R0                                                            
         A     R1,LOCKSUM          ADD IT TO RUNNING TOTAL                      
         ST    R1,LOCKSUM                                                       
DR55B    MVI   GOSUBN,RUP#         ROUND FIGURE FOR DISPLAY                     
         GOTO1 AGOSUB                                                           
         XC    8(L'PWBM1,R6),8(R6)                                              
         OI    6(R6),X80                                                        
         MVI   8(R6),C'0'                                                       
         ICM   R1,15,FULL                                                       
         BZ    DR55X                                                            
         EDIT  (R1),(8,8(R6)),ALIGN=LEFT,FLOAT=-                                
DR55X    DS    0H                                                               
*                                                                               
*** PROJECTED PW% LINE ***                                                      
*                                                                               
DR57     DS    0H                                                               
         MVC   FULL,ATAJBUY+ACCUTABQ   FULL=PROJ BILLING AMOUNT                 
         TM    ATFLAG,ATFSTTLQ     IF WE'RE AT THE SKED LINE                    
         BZ    DR57A                                                            
         MVC   FULL,LOCKSUM            FULL=TOTAL PROJ BILLING AMOUNT           
         LA    R3,PWBPJPWH+(BSLTTLH-BSLDSECT)  POINT TO TOTALS COLUMN           
                                                                                
DR57A    DS    0H                                                               
         OI    6(R3),X80                                                        
         MVC   8(7,R3),=C'**N/A**'  ASSUME NO SPOTS                             
*&&DO                                                                           
         TM    ESTFLAG,EFOWPW       IS IT A OOW PW BILLING ESTIMATE?            
         BZ    *+22                  NO                                         
         TM    ATFLAG2,ATF2OOWP      YES, IS MONTH USING OWPW PAID$ ?           
         BO    *+14                   YES                                       
         MVC   8(7,R3),=C'**OWPW*'    NO, MOVE FLAG IN                          
         B     DR57X                                                            
                                                                                
*&&                                                                             
         TM    ATFLAG,ATFNOSPT                                                  
         BNO   *+12                  THERE'S AT LEAST 1 SPOT                    
         TM    ATFLAG2,ATF2OOWP      0 SPOTS, BUT WERE OOWR PAID$ USED?         
         BNO   DR57X                  NOPE                                      
         OC    ATACBUY,ATACBUY                                                  
         BZ    DR57X                                                            
                                                                                
         XC    8(L'PWBM1,R3),8(R3)                                              
         MVI   8(R3),C'0'                                                       
         ICM   R1,15,ATPW+ACCUTABQ  GET THE PROJ PW%                            
         BZ    DR57X                                                            
         MVC   8(7,R3),=C'*RANGE*'  ASSUME OUT-OF-RANGE                         
         OC    FULL,FULL            IS PROJ BILL TOTL ZERO?                     
         BZ    DR57X                 YES, IT'S OUT-OF-RANGE                     
         C     R1,=A(MAXPWQ)                                                    
         BH    DR57X                                                            
         C     R1,=A(MINPWQ)                                                    
         BL    DR57X                                                            
         EDIT  (R1),(8,8(R3)),2,ALIGN=LEFT,FLOAT=-                              
DR57X    DS    0H                                                               
*                                                                               
*** POINTER BUMPING ***                                                         
*                                                                               
         LA    R2,BSLMQ(R2)        BUMP TO NEXT COLUMN (DR/CR LINE)             
         LA    R6,BSLMQ(R6)        BUMP TO NEXT COLUMN (PB BILL LINE)           
         LA    R3,BSLMQ(R3)        BUMP TO NEXT COLUMN (PROJ PW% LINE)          
         LA    R4,ACCUTABQ(R4)     BUMP PAST BILL ADJ LINE                      
DR58     LA    R4,ACCUTABQ(R4)     BUMP ACCUTAB                                 
         B     DR50A                                                            
*                                                                               
         B     DR60                                                             
DRCRDIE  DC    H'0'                                                             
*                                                                               
** PW UPDATE STATUS **                                                          
*                                                                               
DR60     OI    PWBPWSTH+6,X'80'                                                 
         MVC   PWBPWST,=CL8'LOCKED'                                             
         TM    LOCKFLAG,LKFFPLKQ                                                
         BO    *+10                                                             
         MVC   PWBPWST,=CL8'UNLOCKED'                                           
                                                                                
         OI    PWBBYSTH+6,X'80'                                                 
         MVC   PWBBYST,=CL8'LOCKED'                                             
         TM    LOCKFLAG,LKFFLCKQ                                                
         BO    DR70                                                             
         MVC   PWBBYST,=CL8'UNLOCKED'                                           
*                                                                               
** CLT$ BILLED LINE **                                                          
*                                                                               
DR70     DS    0H                                                               
         LA    R2,PWBCBILH                                                      
         L     R4,AACCUTAB                                                      
         MVI   GOSUBN,RUP#                                                      
         MVI   BYTE,C'N'           FLAG - DON'T FORMAT TOTALS COLUMN            
*                                                                               
DR72     DS    0H                                                               
         CLI   0(R4),XFF           GO EXIT IF AT END OF ACCUTAB                 
         BE    DR79                                                             
         TM    ATFLAG,ATFSTTLQ+ATFBILAJ        IF SKED BIL ADJ ENTRY,           
         BNO   *+12                                                             
         LA    R2,PWBCBILH+(BSLTTLH-BSLDSECT)   POINT TO TOTALS FIELD           
         B     DR74                                                             
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   DR76A                                                            
                                                                                
DR74     DS    0H                  R4-->BILL ADJ ENTRY                          
         LA    R4,ACCUTABQ(R4)      BUMP PAST IT TO MONTH ENTRY                 
         XC    8(L'BSLM1,R2),8(R2) CLEAR FIELD                                  
         OI    6(R2),X80            AND TRANSMIT                                
         TM    ATFLAG2,ATF2ESTB    AT LEAST ESTIMATE BILLED YET?                
         BO    DR75                 YEP, SHOW BILLED AMOUNT                     
         TM    ATFLAG,ATFSTTLQ      NOPE, THEN IS IT SKED TOTALS NTRY?          
         BZ    DR76                  NOPE, DON'T SHOW BILLED AMOUNT             
         CLI   BYTE,C'Y'             YES, ANY BILLED AMOUNT SHOWN?              
         BNE   DR76                   NO, DON'T FORMAT FOR TOTAL COLUMN         
                                                                                
DR75     DS    0H                                                               
         MVI   BYTE,C'Y'           FLAG - FORMAT TOTALS COLUMN                  
         MVI   8(R2),C'0'          ASSUME BILL AMOUNT IS ZERO                   
         ICM   R0,15,ATBILL                                                     
         BZ    DR76                                                             
         GOTO1 AGOSUB              ROUND UP BILL AMOUNT                         
         L     R1,FULL                                                          
         EDIT  (R1),(8,8(R2)),ALIGN=LEFT,FLOAT=-                                
                                                                                
DR76     DS    0H                                                               
         LA    R2,BSLMQ(R2)        BUMP TWA POINTER AND                         
DR76A    LA    R4,ACCUTABQ(R4)      ACCUTAB POINTER TO NEXT MNTH                
         B     DR72                                                             
*                                                                               
DR79     DS    0H                                                               
*                                                                               
** EXIT TASKS **                                                                
*                                                                               
DRX      MVI   GOSUBN,STI#         SAVE OFF TABLES IN TIA                       
         GOTO1 AGOSUB                                                           
                                                                                
         TM    MISCFLG1,MF1BALL0   IF 0 FORCED INTO A DR/CR FIELD,              
         BZ    *+12                                                             
         MVI   MYINFCD,BALL0Q       GIVE USER AN INFO MSG                       
         B     MYINFO                                                           
                                                                                
         TM    MISCFLG1,MF1STTCH   IF STATUS WAS CHANGED SINCE PREV             
         BZ    *+12                                                             
         MVI   MYINFCD,RSTCHQ       TRANSACTION, GIVE USER AN INFO MSG          
         B     MYINFO                                                           
                                                                                
         TM    MISCFLG2,MF2UPSBM   IF UNPAID SPOT IN BILLED MONTH   ,           
         BZ    *+12                                                             
         MVI   MYINFCD,BMUPSQ       GIVE USER AN INFO MSG                       
         B     MYINFO                                                           
                                                                                
         TM    MISCFLG2,MF2MRFB    IF A MONTH SET UP FOR RE-FNL BILL,           
         BZ    *+12                                                             
         MVI   MYINFCD,NBILGENQ     GIVE USER AN INFO MSG                       
         B     MYINFO                                                           
                                                                                
         B     DRX10                                                            
*                                                                               
DRX10    DS    0H                                                               
         MVI   GOSUBN,S1D#         SET AM1STREC                                 
         GOTO1 AGOSUB                                                           
                                                                                
         MVC   ACURFORC,AFRSTKEY   PLACE CURSOR @ 1ST KEY FIELD                 
         TM    LOCKFLAG,LKFFPLKQ                                                
         BO    DRXX                 IF PW STATUS ON LOCK                        
         MVC   ACURFORC,AM1STREC    ELSE, PUT IT ON 1ST DATA FIELD              
         B     DRXX                                                             
*                                                                               
DRXX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*------------------ DISPLAY SECTION OF BILL SCREEN -------------------*         
                                                                                
* AT ENTRY,                                                                     
*   R2-->LOCKED  $ LINE   OR   MAKEGOODS IN                                     
*   R3-->CURRENT $ LINE   OR   MAKEGOODS OUT                                    
*   R6-->DIFFERENCE LINE  OR   COST ADJUSTMENTS                                 
*   BYTE = 'C' FOR CLT$, 'W' FOR WIM$, 'M' FOR MAKEGOODS                        
                                                                                
* At exit,                                                                      
*   LOCKSUM = sum of locked $s or sum of MGin                                   
*   CURRSUM = sum of current $s or sum of MGout                                 
                                                                                
* The formulae are                                                              
*   CLT$ Diff = CLT$ Current - CLT$ Locked                                      
*   WIM$ Diff = WIM$ Current - WIM$ Locked                                      
*   Cost Adjm = WIM$ Diff - (MGin - MGout)                                      
                                                                                
DBS      NTR1                                                                   
         XC    LOCKSUM,LOCKSUM                                                  
         XC    CURRSUM,CURRSUM                                                  
         XC    TEMPSUM,TEMPSUM                                                  
         ST    R2,ALOCKLN          HOLD ONTO A(START OF LINE)                   
         ST    R3,ACURRLN                                                       
         ST    R6,ADIFFLN                                                       
*                                                                               
         MVI   SHOWTOTC,C'N'       ASSUME WE'RE NOT DISPLAYING CURRENT$         
                                                                                
*                                                                               
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
DBS10    TM    ATFLAG,ATFSTTLQ                                                  
         BO    DBS20                                                            
         TM    ATFLAG,ATFMTTLQ                                                  
         BZ    DBS15                                                            
                                                                                
         ICM   RF,15,ATCLCK        ASSUME CLT$                                  
         ICM   RE,15,ATAJBUY                                                    
*                                                                               
         MVI   SHOWCURR,C'Y'        ASSUME WE'RE DISPLAYING CURRENT$            
         TM    ESTFLAG,EFOWPW        IF OOW PW ESTIMATE,                        
         BZ    DBS10AG                                                          
         TM    ATFLAG2,ATF2OOWP       AND MONTH ISN'T CLEARED                   
         BNZ   DBS10AG                                                          
         MVI   SHOWCURR,C'N'          DON'T DISPLAY CURRENT$                    
         SR    RE,RE                  ZERO IT OUT                               
DBS10AG  EQU   *                                                                
         CLI   BYTE,C'C'                                                        
         BE    DBS10A                                                           
*                                                                               
         ICM   RF,15,ATWLCK        IT'S   WIM$                                  
         ICM   RE,15,ATACBUY                                                    
         MVI   SHOWCURR,C'Y'        DISPLAY CURRENT$ WHEN IT'S WIM$             
         CLI   BYTE,C'W'                                                        
         BE    DBS10A                                                           
*                                                                               
*^^SUP   ICM   RF,15,ATMGIN        IT'S   MAKEGOODS                             
*^^SUP   ICM   RE,15,ATMGOUT                                                    
*^^SUP   CLI   BYTE,C'M'                                                        
*^^SUP   BE    DBS10A                                                           
         DC    H'0'                                                             
*                                                                               
DBS10A   L     R0,LOCKSUM          UPDATE 1ST LINE AMOUNT                       
         AR    R0,RF                                                            
         ST    R0,LOCKSUM                                                       
         L     R0,CURRSUM          UPDATE 2ND LINE AMOUNT                       
         AR    R0,RE                                                            
         ST    R0,CURRSUM                                                       
                                                                                
         STM   RE,RF,HOLDRE        SAVE RE & RF IN HOLDRE & HOLDRF              
         MVI   GOSUBN,RUP#                                                      
         L     R0,HOLDRF                                                        
         GOTO1 AGOSUB                                                           
         L     R1,FULL                                                          
         EDIT  (R1),(8,8(R2)),ZERO=NOBLANK,ALIGN=LEFT                           
*                                                                               
         CLI   SHOWCURR,C'Y'       DISPLAY CURRENT$ ?                           
         BNE   DBS10AW              NO, SKIP THIS                               
         L     R0,HOLDRE                                                        
         GOTO1 AGOSUB                                                           
         L     R1,FULL                                                          
         EDIT  (R1),(8,8(R3)),ZERO=NOBLANK,ALIGN=LEFT                           
         MVI   SHOWTOTC,C'Y'       DISPLAY TOTAL CURRENT$                       
DBS10AW  EQU   *                                                                
         LM    RE,RF,HOLDRE                                                     
                                                                                
         SR    RE,RF               RE=DIFFERENCE TO BE DISPLAYED                
         CLI   BYTE,C'M'           IF MAKEGOOD,                                 
         BNE   DBS10B                                                           
         LCR   RF,RE                THEN RF = MGin - MGout                      
         ICM   RE,15,ATACBUY                                                    
         ICM   R0,15,ATWLCK                                                     
         SR    RE,R0               GET WIM$ DIFFERENCE                          
         SR    RE,RF                AND SUBTRACT MG DIFFERENCE FROM IT          
         L     R0,TEMPSUM                                                       
         AR    R0,RE                                                            
         ST    R0,TEMPSUM          TEMPSUM = COST ADJMNT TOTAL                  
*                                                                               
* Because there is a bug with specifying ZERO=NOBLANK & FLOAT=-                 
*  together in the EDIT macro, I am moving in the zero myself                   
*  to get around this problem.                                                  
*                                                                               
DBS10B   XC    8(L'PWBM1H,R6),8(R6)                                             
         MVI   8(R6),C'0'          ASSUME DIFFERENCE IS ZERO                    
                                                                                
         LR    R0,RE                                                            
         MVI   GOSUBN,RUP#                                                      
         GOTO1 AGOSUB                                                           
         ICM   R1,15,FULL          IF ROUNDED TO ZERO,                          
         BZ    DBS10C               SKIP EDIT INSTRUCTION                       
         LNR   R1,R1               LEAVE ROOM FOR '+' SIGN, IF NEEDED           
         EDIT  (R1),(8,8(R6)),ALIGN=LEFT,FLOAT=-                                
         L     R1,FULL             SEE IF WE NEED A '+' SIGN                    
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         MVI   8(R6),C'+'                                                       
*                                                                               
DBS10C   OI    6(R2),X'80'                                                      
         OI    6(R3),X'80'                                                      
         OI    6(R6),X'80'                                                      
                                                                                
         LA    R2,BSLMQ(R2)                                                     
         LA    R3,BSLMQ(R3)                                                     
         LA    R6,BSLMQ(R6)                                                     
         LA    R4,ACCUTABQ(R4)     BUMP PAST BILL ADJ LINE                      
DBS15    LA    R4,ACCUTABQ(R4)                                                  
         B     DBS10                                                            
*                                                                               
** DO TOTALS COLUMN **                                                          
*                                                                               
DBS20    LA    R0,BSLTTLH-BSLDSECT                                              
         L     R2,ALOCKLN                                                       
         AR    R2,R0               R2-->TOTALS COLUMN OF 1ST LINE               
         L     R3,ACURRLN                                                       
         AR    R3,R0               R3-->TOTALS COLUMN OF 2ND LINE               
         L     R6,ADIFFLN                                                       
         AR    R6,R0               R6-->TOTALS COLUMN OF DIFFERENCE             
                                                                                
         MVI   GOSUBN,RUP#         ROUND OFF VALUES BEFORE DISPLAYING           
         L     R0,LOCKSUM                                                       
         GOTO1 AGOSUB                                                           
         L     R1,FULL                                                          
         EDIT  (R1),(8,8(R2)),ZERO=NOBLANK,ALIGN=LEFT                           
*                                                                               
         CLI   SHOWTOTC,C'Y'       DISPLAY TOTAL CURRENT$                       
         BNE   DBS029               NO, SKIP THIS                               
         L     R0,CURRSUM                                                       
         GOTO1 AGOSUB                                                           
         L     R1,FULL                                                          
         EDIT  (R1),(8,8(R3)),ZERO=NOBLANK,ALIGN=LEFT                           
DBS029   EQU   *                                                                
                                                                                
         L     RF,LOCKSUM                                                       
         L     RE,CURRSUM                                                       
         SR    RE,RF               RE=DIFFERENCE TO BE DISPLAYED                
         CLI   BYTE,C'M'           IF MAKEGOOD,                                 
         BNE   DBS20A                                                           
         L     RE,TEMPSUM           TOTAL IS STORED IN TEMPSUM                  
*                                                                               
* Because there is a bug with specifying ZERO=NOBLANK & FLOAT=-                 
*  together in the EDIT macro, I am moving in the zero myself                   
*  to get around this problem.                                                  
*                                                                               
DBS20A   XC    8(L'PWBM1H,R6),8(R6)                                             
         MVI   8(R6),C'0'          ASSUME DIFFERENCE IS ZERO                    
                                                                                
         LR    R0,RE               ROUND OFF THE DIFFERENCE                     
         MVI   GOSUBN,RUP#                                                      
         GOTO1 AGOSUB                                                           
         ICM   R1,15,FULL          IF ROUNDED TO ZERO,                          
         BZ    DBS20B               SKIP EDIT INSTRUCTION                       
         LNR   R1,R1               LEAVE ROOM FOR '+' SIGN, IF NEEDED           
         EDIT  (R1),(8,8(R6)),ALIGN=LEFT,FLOAT=-                                
         L     R1,FULL             SEE IF WE NEED A '+' SIGN                    
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         MVI   8(R6),C'+'                                                       
*                                                                               
DBS20B   OI    6(R2),X'80'                                                      
         OI    6(R3),X'80'                                                      
         OI    6(R6),X'80'                                                      
                                                                                
         B     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL OVERLAY'                           
***********************************************************************         
*================= CHECK FOR PROTECTED FIELD MODIFIED ================*         
                                                                                
* Routine is executed if buy or PW status was changed.  This is to pre-         
*  vent any mishaps when a user signs onto the PW/Bill screen w/ PW             
*  status on unlock and while he is working, someone goes into                  
*  PW/Maint and changes the PW status to lock, which effectively pro-           
*  tects the data fields on both screens.                                       
                                                                                
CHPRTMOD NTR1                                                                   
         LA    R2,PWBCPWH              START W/ CURRENT PW% LINE                
         LA    RF,(BSLTTLH-BSLDSECT)(R2)                                        
         LA    R0,PWBBILLH             DO ADJ DR/CR$ LINE NEXT                  
*                                                                               
CPM10    CR    R2,RF               AT THE END OF LINE YET?                      
         BNH   CPM15                                                            
         CR    R2,R0                YES, DID WE DO ADJ DR/CR$ YET?              
         BNL   CPMX                  YEP, SO EXIT                               
                                                                                
         LR    R2,R0                 NO, DO ADJ DR/CR$ LINE NOW                 
         LA    RF,(BSLM4H-BSLDSECT)(R2)                                         
         B     CPM10                                                            
*                                                                               
CPM15    TM    4(R2),X'80'         IF FIELD WAS MODIFIED,                       
         BZ    CPM20                                                            
         NI    4(R2),XFF-X'80'      TURN OFF MODIFIED BIT                       
         OI    6(R2),X'80'          AND TRANSMIT IT                             
*                                                                               
CPM20    LA    R2,BSLMQ(R2)        BUMP TO NEXT COLUMN                          
         B     CPM10                                                            
*                                                                               
CPMX     B     XIT                                                              
***********************************************************************         
         EJECT                                                                  
         PRINT OFF                                                              
*&&DO                                                                           
***********************************************************************         
*============================ KEEP MODIFIED ==========================*         
                                                                                
* Routine is executed when an error occurs.  This is to remember which          
*  fields were changed so that if an error occurs, the user would only          
*  need to correct the error and not need to re-input the rest of the           
*  fields.                                                                      
                                                                                
KEEPMOD  NTR1                                                                   
         LA    R2,PWBCPWH              START W/ CURRENT PW% LINE                
         LA    RF,(BSLTTLH-BSLDSECT)(R2)                                        
         LA    R0,PWBBILLH             DO ADJ DR/CR$ LINE NEXT                  
*                                                                               
KPM10    CR    R2,RF               AT THE END OF LINE YET?                      
         BNH   KPM15                                                            
         CR    R2,R0                YES, DID WE DO ADJ DR/CR$ YET?              
         BNL   KPMX                  YEP, SO EXIT                               
                                                                                
         LR    R2,R0                 NO, DO ADJ DR/CR$ LINE NOW                 
         LA    RF,(BSLM4H-BSLDSECT)(R2)                                         
         B     KPM10                                                            
*                                                                               
KPM15    TM    4(R2),X'80'         IF FIELD WAS MODIFIED,                       
         BZ    KPM20                                                            
         OI    6(R2),X'81'          MAKE MODIFIED FOR NEXT TIME                 
*                                                                               
KPM20    LA    R2,BSLMQ(R2)        BUMP TO NEXT COLUMN                          
         B     KPM10                                                            
*                                                                               
KPMX     B     XIT                                                              
***********************************************************************         
         EJECT                                                                  
*&&                                                                             
         PRINT ON                                                               
***********************************************************************         
*======================== GET NUMBER OF WEEKS ========================*         
                                                                                
* Gets number of broadcast weeks in month                                       
*  At entry, R3-->start date of broadcast month                                 
*  At exit,  RF = # of weeks                                                    
                                                                                
         DS    0H                                                               
GTNWEEKS NTR1                                                                   
         SR    RF,RF                                                            
         L     RE,AACCUTAB                                                      
         USING ACCUTABD,RE                                                      
GNW10    CLC   ATMSTART,0(R3)                                                   
         BE    GNW20                                                            
         LA    RE,ACCUTABQ(RE)                                                  
         B     GNW10                                                            
*                                                                               
GNW20    DS    0H                  RE-->1ST ENTRY OF MONTH                      
         LA    RF,1(RF)                                                         
         LA    RE,ACCUTABQ(RE)     KEEP COUNTING THE NUMBER OF ENTRIES          
         CLI   0(RE),XFF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    ATFLAG,ATFMTTLQ                                                  
         BZ    GNW20                                                            
         DROP  RE                                                               
                                                                                
         XIT1  REGS=(RF)                                                        
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*========================= LOCATE SCHED LINE =========================*         
                                                                                
*        POINTS R4 TO SCHEDULE TOTALS LINE WITHIN ACCUTAB                       
                                                                                
LOCSTTL  DS    0H                                                               
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
LST10    CLI   0(R4),XFF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    ATFLAG,ATFSTTLQ                                                  
         BO    LSTX                                                             
         LA    R4,ACCUTABQ(R4)                                                  
         B     LST10                                                            
*                                                                               
LSTX     BR    RE                                                               
         DROP  R4                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================ LOCATE DATE ============================*         
                                                                                
*        Points R4 to week entry corresponding to DATE2                         
                                                                                
LOCDATE  NTR1  BASE=BASE1,LABEL=NO                                              
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
                                                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
LDT10    CLI   0(R4),XFF                                                        
         BE    LDTXN                                                            
         TM    ATFLAG,ATFSTTLQ                                                  
         BO    LDTXN                                                            
         TM    ATFLAG,ATFMTTLQ                                                  
         BO    LDT20                                                            
                                                                                
         CLC   ATWSTART,DATE2                                                   
         BH    LDTXN                                                            
         CLC   ATWEND,DATE2                                                     
         BNL   LDTX                                                             
*                                                                               
LDT20    LA    R4,ACCUTABQ(R4)                                                  
         B     LDT10                                                            
*                                                                               
LDTXN    DS    0H                  CAN'T LOCATE ENTRY                           
         SR    R4,R4                PASS BACK NULLS IN R4                       
         B     LDTX                                                             
*                                                                               
LDTX     XIT1  REGS=(R4)                                                        
         DROP  R4                                                               
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*==================== SUB-ROUTINE POOL INTERFACE =====================*         
                                                                                
GOSUB    NTR1  BASE=BASE1,LABEL=NO                                              
         L     R7,BASE2                                                         
         L     R5,BASE3                                                         
                                                                                
         L     RF,ASUBR01                                                       
         CLI   GOSUBN,R01#                                                      
         BNH   GOSUBGO                                                          
         L     RF,ASUBR02                                                       
         CLI   GOSUBN,R02#                                                      
         BNH   GOSUBGO                                                          
         L     RF,ASUBR03                                                       
         CLI   GOSUBN,R03#                                                      
         BNH   GOSUBGO                                                          
         L     RF,ASUBR04                                                       
         CLI   GOSUBN,R04#                                                      
         BNH   GOSUBGO                                                          
         DC    H'0'                                                             
GOSUBGO  GOTO1 (RF),DMCB,(GOSUBN,(RC)),(RA),(R9),(R8)                           
         XIT1                                                                   
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*======================== CLEAR ERROR FIELDS =========================*         
CLRERRS  DS    0H                                                               
         MVI   MYERRCD,0                                                        
         MVI   SVERROR,0                                                        
         MVI   ERROR,0                                                          
         BR    RE                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= SCHEDULE CALCULATIONS =======================*         
                                                                                
*  ROUTINES TO PICK OUT VALUE(S) ENTERRED, AND UPDATE                           
*   CORRESPONDING VALUE(S)                                                      
*  AT ENTRY TO THESE ROUTINES,                                                  
*   R2-->RESPECTIVE MONTH COLUMN                                                
*   R4-->RESPECTIVE ENTRY IN ACCUTAB                                            
                                                                                
         USING ACCUTABD,R4                                                      
                                                                                
*------------------------ SCHEDL PW% MODIFIED ------------------------*         
*                                                                               
MODSPW   NTR1                                                                   
         BAS   RE,GETPW            GET THE PW% FROM SCHED LINE                  
         BNE   NO                   (TEMPPW HAS NEW PW%)                        
         L     RF,TEMPPW                                                        
         STCM  RF,15,ATPW           AND UPDATE SKED PW% IN ACCUTAB              
                                                                                
         TM    ATFLAG,ATFSTTLQ+ATFBILAJ                                         
         BNO   MODSPW30            DO PROJ PW%                                  
*                                                                               
** CURRENT PW% **                                                               
*                                                                               
         L     R4,AACCUTAB         MODIFY ALL WEEKLY PW%                        
MODSPW10 TM    ATFLAG,ATFSTTLQ                                                  
         BO    MODSPW20                                                         
         TM    ATFLAG,ATFMTTLQ                                                  
         BO    MODSPW15                                                         
                                                                                
         TM    ATFLAG,ATFNOSPT     DON'T MODIFY WEEK W/O SPOTS                  
         BO    MODSPW15                                                         
         OC    ATACBUY,ATACBUY      AND THOSE W/ ACTUAL BUY = 0                 
         BZ    MODSPW15                                                         
         TM    ATFLAG2,ATF2CCOD     AND THOSE W/ CLCOST OVERRIDED               
         BO    MODSPW15                                                         
         TM    ATFLAG2,ATF2OOWP     AND THOSE WHOSE MTH HAS OOW PW PD$          
         BO    MODSPW15                                                         
         STCM  RF,15,ATPW          PUT NEW PW% INTO ACCUTAB                     
                                                                                
MODSPW15 LA    R4,ACCUTABQ(R4)                                                  
         B     MODSPW10                                                         
*                                                                               
MODSPW20 B     YES                                                              
*                                                                               
** PROJECTED PW% **                                                             
*                                                                               
MODSPW30 DS    0H                  R4-->SKED TOTAL ENTRY                        
         L     R4,AACCUTAB                                                      
                                                                                
MODSPW40 TM    ATFLAG,ATFSTTLQ                                                  
         BO    MODSPW60                                                         
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   MODSPW50                                                         
                                                                                
         LR    R3,R4               R3-->MONTH BILL ADJ ENTRY                    
         LA    R4,ACCUTABQ(R4)     R4-->MONTH TOTAL ENTRY                       
                                                                                
         TM    ATFLAG,ATFNOSPT     ANY SPOTS IN MONTH                           
         BO    MODSPW50             NOPE                                        
         OC    ATACBUY,ATACBUY     ANY REAL WIM$ CURRENT?                       
         BZ    MODSPW50             NOPE                                        
         OC    (ATDRCR-ACCUTABD)(,R3),(ATDRCR-ACCUTABD)(R3)                     
         BNZ   MODSPW42                                                         
         TM    ATFLAG,ATFUNPDQ                                                  
         BO    MODSPW50                                                         
                                                                                
MODSPW42 DS    0H                                                               
         MVC   ATPW,TEMPPW         UPDATE MONTH PROJ PW%                        
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWBUY#                                                    
         GOTO1 AGOSUB                                                           
         MVC   ATAJBUY,TEMPAJB     PROJ FINAL BILLING                           
                                                                                
         ICM   RF,15,ATAJBUY                                                    
         ICM   RE,15,(ATAJBUY-ACCUTABD)(R3)                                     
         SR    RF,RE                                                            
         BNZ   *+8                                                              
         L     RF,=X'80000000'               SPECIAL FORMAT IF DR/CR=$0         
         CLM   RF,15,(ATDRCR-ACCUTABD)(R3)   SEE IF CHANGE TO SKED PW%          
         BE    *+14                                                             
         OI    (ATFLAG2-ACCUTABD)(R3),ATF2CHDC  CHANGED DR/CR VALUE             
         MVC   (ATBILD-ACCUTABD)(,R3),CTODAY SET DATE OF CHANGE                 
         STCM  RF,15,(ATDRCR-ACCUTABD)(R3)                                      
                                                                                
MODSPW50 LA    R4,ACCUTABQ(R4)                                                  
         B     MODSPW40                                                         
*                                                                               
MODSPW60 DS    0H                                                               
         B     YES                                                              
         SPACE 2                                                                
*------------------------ MNTHLY PW% MODIFIED ------------------------*         
*                                                                               
MODMPW   NTR1                                                                   
         BAS   RE,GETPW            GET THE PW% FROM MONTH LINE                  
         BNE   NO                   (TEMPPW HAS NEW PW%)                        
         L     RF,TEMPPW                                                        
         STCM  RF,15,ATPW           AND UPDATE MNTH PW% IN ACCUTAB              
                                                                                
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   MODMPW30            DO PROJ PW%                                  
*                                                                               
** CURRENT PW% **                                                               
*                                                                               
         LA    R3,ATMSTART         GET # OF WEEKS IN MONTH                      
         BAS   RE,GTNWEEKS                                                      
         ST    RF,FULL             FULL=# OF WEEKS IN BRDCST MONTH              
                                                                                
         L     R0,FULL                                                          
MODMPW10 SH    R4,=Y(ACCUTABQ)     BUMP BACKWARDS TO START OF MNTH              
         BCT   R0,MODMPW10                                                      
                                                                                
         L     R0,FULL                                                          
MODMPW20 TM    ATFLAG,ATFNOSPT     OVERRIDE PW% FOR EACH WK IN MNTH             
         BO    MODMPW22             EXCEPT FOR THOSE W/O SPOTS                  
         OC    ATACBUY,ATACBUY      AND THOSE W/ ACTUAL BUY = 0                 
         BZ    MODMPW22                                                         
         TM    ATFLAG2,ATF2CCOD     AND THOSE W/ CLCOST OVERRIDED               
         BO    MODMPW22                                                         
         MVC   ATPW,TEMPPW                                                      
                                                                                
MODMPW22 DS    0H                                                               
         LA    R4,ACCUTABQ(R4)                                                  
         BCT   R0,MODMPW20                                                      
         B     YES                                                              
*                                                                               
** PROJECTED PW% **                                                             
*                                                                               
MODMPW30 DS    0H                  R4-->MNTH TTL, WHERE PROJ PW% IS             
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVC   TEMPPW,ATPW         ATPW=PROJ PW%                                
         MVI   GOSUBN,PWBUY#                                                    
         GOTO1 AGOSUB                                                           
         MVC   ATAJBUY,TEMPAJB     ATAJBUY=PROJ FINAL BILL FOR MONTH            
                                                                                
         DS    0H                  CALCULATE DR/CR VALUE                        
         LR    R3,R4                                                            
         SH    R3,=Y(ACCUTABQ)     R3-->BILL ADJ, WHERE DR/CR IS                
         ICM   RF,15,ATAJBUY                                                    
         ICM   RE,15,(ATAJBUY-ACCUTABD)(R3)                                     
         SR    RF,RE                                                            
         BNZ   *+8                                                              
         L     RF,=X'80000000'               SPECIAL FORMAT IF DR/CR=$0         
         CLM   RF,15,(ATDRCR-ACCUTABD)(R3)   SEE IF CHANGE TO MNTH PW%          
         BE    *+14                                                             
         OI    (ATFLAG2-ACCUTABD)(R3),ATF2CHDC  CHANGED DR/CR VALUE             
         MVC   (ATBILD-ACCUTABD)(,R3),CTODAY SET DATE OF CHANGE                 
         STCM  RF,15,(ATDRCR-ACCUTABD)(R3)                                      
         B     YES                                                              
         EJECT                                                                  
*------------------------- PROJ BILL MODIFIED ------------------------*         
*                                                                               
         DS    0H                                                               
MODPFB   NTR1                                                                   
         MVI   MYERRCD,0           ASSUME NONE OF MY ERRORS                     
                                                                                
         MVI   BYTE,0              CHECK FOR NON-NEGATIVE WHOLE $'S             
         MVI   GOSUBN,VWD#                                                      
         GOTO1 AGOSUB                                                           
         BNE   NO                                                               
                                                                                
         XC    TEMPAJB,TEMPAJB     GET PROJ BILL VALUE INPUTTED                 
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(R0)                                      
         CLI   DMCB,0                                                           
         BNE   NO                                                               
                                                                                
         L     R0,4(R1)            R0=VALUE OF INPUT                            
         LTR   R0,R0                                                            
         BM    NO                                                               
         ST    R0,TEMPAJB                                                       
                                                                                
         DS    0H                  CALCULATE PROJECTED PW%                      
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWPW#                                                     
         GOTO1 AGOSUB                                                           
         MVC   ATPW,TEMPPW          AND UPDATE ACCUTAB                          
                                                                                
         ICM   R1,15,ATPW          MAKE SURE PROJ PW% IS W/IN RANGE             
         BZ    MODPFB20                                                         
         MVI   MYERRCD,ELOPWQ                                                   
         C     R1,=A(MINPWQ)                                                    
         BL    NO                                                               
         MVI   MYERRCD,EHIPWQ                                                   
         CH    R1,=Y(MAXPWQ)                                                    
         BH    NO                                                               
         MVI   MYERRCD,0                                                        
*                                                                               
MODPFB20 DS    0H                  FUDGE PROJ PW% ONTO TWA                      
         LA    R0,PWBPJBLH                                                      
         SR    R2,R0                                                            
         LA    R2,PWBPJPWH(R2)                                                  
         XC    8(L'BSLM1,R2),8(R2)                                              
         MVI   8(R2),C'0'                                                       
         LA    R0,1                                                             
         OR    R1,R1                                                            
         BZ    MODPFB25                                                         
         EDIT  (R1),(8,8(R2)),2,ALIGN=LEFT,FLOAT=-                              
MODPFB25 STC   R0,5(R2)                                                         
                                                                                
         LA    RF,MODSPW                                                        
         TM    ATFLAG,ATFSTTLQ                                                  
         BO    *+8                                                              
         LA    RF,MODMPW                                                        
         BASR  RE,RF                                                            
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
*-------------------------- GET PW% ENTERRED -------------------------*         
*                                                                               
GETPW    NTR1                                                                   
         MVI   MYERRCD,0           ASSUME NONE OF MY ERRORS                     
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(R0)                                      
         CLI   DMCB,0                                                           
         BNE   NO                                                               
         MVI   MYERRCD,OUTRNGEQ    ASSUME BAD PW% ENTERRED                      
         L     R0,4(R1)            MAKE SURE PW% INPUTTED IS                    
         C     R0,=A(MINPWQ)                                                    
         BL    NO                   NOT LESS THAN MIN PW%                       
         CH    R0,=Y(MAXPWQ)                                                    
         BH    NO                   AND NOT BIGGER THAN MAX PW%                 
                                                                                
         ST    R0,TEMPPW           PW% IS GOOD                                  
         B     YES                                                              
         SPACE 2                                                                
         DROP  R4                                                               
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL (MISC STUFF)'                      
***********************************************************************         
*======================== MISCELLANEOUS STUFF ========================*         
                                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
NOREC    DS    0H                  RECORD NOT FOUND                             
         MVI   MYERRCD,RNFQ                                                     
         L     R2,AFRSTKEY                                                      
         B     MYERROR                                                          
*                                                                               
MISSFLD  DS    0H                                                               
         MVI   MYERRCD,MFLDQ                                                    
         B     MYERROR                                                          
*                                                                               
INVLFLD  DS    0H                                                               
         MVI   MYERRCD,IFLDQ                                                    
         B     MYERROR                                                          
*                                                                               
INVOPERR DS    0H                  INVALID OPTION KEYWORD                       
         MVI   MYERRCD,INVOQ                                                    
         B     MYERROR                                                          
*                                                                               
DUPOPERR DS    0H                  DUPLICATE OPTION SPECIFIED                   
         MVI   MYERRCD,DUPOQ                                                    
         B     MYERROR                                                          
*                                                                               
OPDTAERR DS    0H                  INVALID OPTION DATA VALUE                    
         MVI   MYERRCD,IOPDQ                                                    
         B     MYERROR                                                          
                                                                                
                                                                                
         PRINT OFF                                                              
*&&DO                                                                           
ERRGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO1,ERROR       ERROR #                                     
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMERR        ERROR TYPE MSG                              
         MVI   GTMSYS,GTGENSYS      SYSTEM                                      
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT         A(OUTPUT AREA)                               
         DROP  R1                                                               
ERREXIT  MVI   MYERRCD,0           INDICATE ERR MSG HANDLED BY GENCON           
         OI    MISCFLG1,MF1ERRQ                                                 
         BAS   RE,KEEPMOD                                                       
         BAS   RE,RSTRACTB                                                      
         MVI   GOSUBN,STI#         SAVE OFF TIA                                 
         GOTO1 AGOSUB                                                           
         GOTO1 ERREX                                                            
*&&                                                                             
         PRINT ON                                                               
         SPACE 2                                                                
*---------------------------- SAVE ACCUTAB ---------------------------*         
SAVEACTB DS    0H                                                               
         ST    RE,HOLDRE                                                        
         L     R0,ASVACTB               DESTINATION                             
         LA    R1,L'SVACTB              L'DESTINATION                           
         L     RE,AACCUTAB              SOURCE                                  
         LR    RF,R1                    L'SOURCE                                
         MVCL  R0,RE                                                            
         L     RE,HOLDRE                                                        
         BR    RE                                                               
         SPACE 2                                                                
*--------------------------- RESTORE ACCUTAB -------------------------*         
RSTRACTB DS    0H                                                               
         ST    RE,HOLDRE                                                        
         L     R0,AACCUTAB              DESTINATION                             
         LA    R1,L'ACCUTAB             L'DESTINATION                           
         L     RE,ASVACTB               SOURCE                                  
         LR    RF,R1                    L'SOURCE                                
         MVCL  R0,RE                                                            
         L     RE,HOLDRE                                                        
         BR    RE                                                               
         EJECT                                                                  
*----------------- EXIT AND DISPLAY MESSAGE ROUTINES -----------------*         
                                                                                
MYERROR  DS    0H                                                               
         CLI   MYERRCD,PWNAVQ      IF IT'S THIS ERROR, DON'T DO                 
         BE    MYERROR2             ANYTHING BUT DISPLAY ERROR MESSAGE          
         BAS   RE,RSTRACTB                                                      
         MVI   GOSUBN,MNI#         KEEP INPUTTED BUT NOT YET VALIDATED          
         GOTO1 AGOSUB               FIELDS MODIFIED                             
MYERROR2 DS    0H                                                               
         MVI   BYTE,C'E'                                                        
         B     XMSGGO                                                           
                                                                                
MYWARN   DS    0H                                                               
         MVI   BYTE,C'W'                                                        
         B     XMSGGO                                                           
                                                                                
MYINFO   DS    0H                                                               
         MVI   BYTE,C'I'                                                        
         B     XMSGGO                                                           
                                                                                
XMSGGO   DS    0H                                                               
         GOTO1 AXMSGRTN,DMCB,(BYTE,(RC))                                        
         B     XIT                                                              
         PRINT OFF                                                              
*&&DO                                                                           
*----------------------- MY ROUTINES FOR ERRORS ----------------------*         
                                                                                
MYERROR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'**ERROR**'                                         
         LA    R1,CONHEAD+10                                                    
         ZIC   RF,MYERRCD                                                       
         LTR   RF,RF                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     MYERR0(RF)                                                       
                                                                                
*                                                                               
MSGERR   MVI   ERROR,0                                                          
         OI    MISCFLG1,MF1ERRQ                                                 
         BAS   RE,KEEPMOD                                                       
         LR    R0,R2                                                            
         S     R0,ATWA                                                          
         STH   R0,PRVFDSP                                                       
         BAS   RE,RSTRACTB         RESTORE ACCUTAB                              
         MVI   GOSUBN,STI#         SAVE OFF TIA                                 
         GOTO1 AGOSUB                                                           
         GOTO1 ERREX2                                                           
         EJECT                                                                  
*--------------- MY ROUTINES FOR INFORMATIONAL MESSAGES --------------*         
                                                                                
MYINFO   XC    CONHEAD,CONHEAD                                                  
         LA    R1,CONHEAD                                                       
         ZIC   RF,MYINFCD                                                       
         LTR   RF,RF                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     MYINF0(RF)                                                       
                                                                                
RSTCHQ   EQU   ((MYINF1-MYINF0)/4)+1                                            
BALL0Q   EQU   ((MYINF2-MYINF0)/4)+1                                            
NBILGENQ EQU   ((MYINF3-MYINF0)/4)+1                                            
BMUPSQ   EQU   ((MYINF4-MYINF0)/4)+1                                            
                                                                                
MYINF0   DS    0H                                                               
MYINF1   B     RSTCH                                                            
MYINF2   B     BALL0                                                            
MYINF3   B     NBILGEN                                                          
MYINF4   B     BMUPS                                                            
         DC    H'0'                                                             
*                                                                               
RSTCH    MVC   0(41,R1),=C'Record redisplayed because status changed'           
         L     R2,AFRSTKEY                                                      
         B     MSGINF                                                           
*                                                                               
BALL0    MVC   0(39,R1),=C'Automatic $0 entered in Adj DR/CR field'             
         MVC   ACURFORC,ABALL0                                                  
         B     MSGINF                                                           
*                                                                               
NBILGEN  DS    0H                                                               
         MVC   0(50,R1),=C'Billing will not generate w/o CLT$ Adj DR/CR+        
                value'                                                          
         MVC   ACURFORC,ADRCRRFB                                                
         B     MSGINF                                                           
*                                                                               
BMUPS    DS    0H                                                               
         MVC   0(L'MNTHINF,R1),MNTHINF                                          
         MVC   L'MNTHINF+1(38,R1),=C'is final billed and has unpaid spo+        
               t(s)'                                                            
         MVC   ACURFORC,ACURSINF                                                
         B     MSGINF                                                           
*                                                                               
MSGINF   DS    0H                                                               
         B     XIT                                                              
*&&                                                                             
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL (LTORG && CONST)'                  
***********************************************************************         
*======================== LTORG AND CONSTANTS ========================*         
         LTORG                                                                  
         SPACE 2                                                                
DCLIST   DS    0C                                                               
         DCDDL SP#TOTAL,6,L                                                     
         DCDDL SP#SCHDL,4,L                                                     
DCLISTX  DC    X'00'                                                            
                                                                                
UNPDMRK  DC    (L'PWBM1)C'*'                                                    
NSPTMRK  DC    (L'PWBM1)C'.'                                                    
PF12DC   DC    CL9'12=Return'                                                   
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN MAINTENANCE OVERLAY (TABLES)'           
***********************************************************************         
*=============================== TABLES ==============================*         
                                                                                
TIADSPTB DS    0H                  DISP OF LABELS IN SPOT STORAGE               
         DC    Y(ACTBLABL-SPOTAREA)                                             
*&&DO                                                                           
         DC    Y(ESTBLABL-SPOTAREA)                                             
*&&                                                                             
         DC    Y(SPTBLABL-SPOTAREA)                                             
TIADSPQ  EQU   (*-TIADSPTB)/(L'TIADSPTB)                                        
         SPACE 2                                                                
LBLTAB   DS    0CL8                LABEL NAMES FOR SPOT STORAGE                 
         DC    CL8'*ACTB43*'                                                    
*&&DO                                                                           
         DC    CL8'**ESTAB*'                                                    
*&&                                                                             
         DC    CL8'*SPTTAB*'                                                    
LBLTABQ  EQU   (*-LBLTAB)/(L'LBLTAB)                                            
                                                                                
         DS    0CL(TIADSPQ-LBLTABQ+1)                                           
         DS    0CL(LBLTABQ-TIADSPQ+1)                                           
         SPACE 2                                                                
COREQLST DS    0AL1                LIST OF CORE-RESIDENT PHASE TO GET           
         DC    AL1(QTSAR)                                                       
         DC    AL1(QPWCALC)                                                     
         DC    AL1(QSPOTIO)         THE ORDER OF THIS LIST SHOULD               
         DC    AL1(QSPOTBUY)        BE CHANGED WITH CARE                        
         DC    AL1(QSPOTGL)                                                     
         DC    AL1(QMOBILE)                                                     
         DC    AL1(QGETBROD)                                                    
COREQLSQ EQU   *-COREQLST                                                       
         SPACE 2                                                                
YESNO    DS    0C                                                               
         DC    AL1(3),C'YES'                                                    
         DC    AL1(2),C'NO'                                                     
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*=========================== OPTIONS TABLES ==========================*         
                                                                                
OPTABLES DS    0C                  *********** SEE OPTDSECT **********          
*                                                                               
OPT1     DC    AL1(1),AL1(OPT2-OPT1)                                            
         DC    AL1(OPUTAX,OPFOTHAB)                                             
         DC    AL1(1),AL2(USETAX-SYSD)                                          
OPT1NAME DC    AL1(OPT1V1-OPT1NAME-1),C'TAX'                                    
OPT1V1   DC    AL2(YESNO-T21743)                                                
*                                                                               
OPT2     DS    0C                                                               
*                                                                               
OPTX     DC    AL1(EOT)                                                         
         EJECT                                                                  
*============================ PFKEY TABLES ===========================*         
                                                                                
PFTABLES DS    0C                  ***** REFER TO PFTABD & KEYD ******          
*                                                                               
** PROCESS THE FOLLOWING ONLY IF FROM LIST SCREEN **                            
*                                                                               
PFTABLST DS    0C                                                               
*                                                                               
PFT12    DS    0C                  Return to LIST screen                        
         DC    AL1(PFT12X-PFT12,12)                                             
         DC    AL1(PFTRPROG)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    3C' ',8C' ',8C' '                                                
PFT12X   EQU   *                                                                
*                                                                               
** THE FOLLOWING ALWAYS GET PROCESSED **                                        
*                                                                               
PFTABBIL DS    0C                                                               
*                                                                               
PFT09    DS    0C                  Redisplay                                    
         DC    AL1(PFT09X-PFT09,09)                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PFTRETRN)                                                    
         DC    3C' ',8C' ',8C' '                                                
PFT09X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
***********************************************************************         
         SPACE 2                                                                
         DROP  R5,R7,R8,R9,RA,RB,RC                                             
         TITLE 'SPSFM43 - PW BILL (SUBR01)'                                     
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
SUBR01Q  EQU   ((((*-T21743)/4096)+1)*4096)                                     
                                                                                
         ORG   T21743+SUBR01Q                                                   
SUBR01   NMOD1 0,**4301**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         L     R1,0(R1)                                                         
         SRL   R1,24               SHIFT TO LOW-ORDER BYTE                      
         BCTR  R1,0                 SUBTRACT ONE,                               
         SLL   R1,2                 AND MULTIPLY BY FOUR                        
         B     R01_00(R1)                                                       
                                                                                
BPK#     EQU   (R01_01-*)/4+1                                                   
GPR#     EQU   (R01_02-*)/4+1                                                   
*&&DO                                                                           
GBT#     EQU   (R01_03-*)/4+1                                                   
BPT#     EQU   (R01_04-*)/4+1                                                   
IAC#     EQU   (R01_05-*)/4+1                                                   
*&&                                                                             
GBB#     EQU   (R01_06-*)/4+1                                                   
FLK#     EQU   (R01_07-*)/4+1                                                   
FDC#     EQU   (R01_08-*)/4+1                                                   
RUP#     EQU   (R01_09-*)/4+1                                                   
*&&DO                                                                           
SUM#     EQU   (R01_10-*)/4+1                                                   
UPT#     EQU   (R01_11-*)/4+1                                                   
*&&                                                                             
                                                                                
R01_00   DS    0H                                                               
R01_01   B     BPWKEY                                                           
R01_02   B     GETPWREC                                                         
*&&DO                                                                           
R01_03   B     GETBMTAB                                                         
R01_04   B     BPWTAB                                                           
R01_05   B     INITACTB                                                         
*&&                                                                             
R01_06   B     GTBUYBIL                                                         
R01_07   B     FILLCKN                                                          
R01_08   B     FILLDRCR                                                         
R01_09   B     ROUNDUP                                                          
*&&DO                                                                           
R01_10   B     SUMUP                                                            
R01_11   B     UPWTAB                                                           
*&&                                                                             
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
                                                                                
YES_01   SR    RC,RC                                                            
NO_01    LTR   RC,RC                                                            
XIT_01   XIT1                                                                   
         TITLE 'SPSFM43 - PW BILL (SUBR01--BPK#)'                               
*---------------------------- BUILD PW KEY ---------------------------*         
*                                                                               
BPWKEY   DS    0H                                                               
         XC    KEY,KEY                                                          
PWK      USING PWRECD,KEY                                                       
         MVC   PWK.PWKTYP,=X'0D7A'                                              
         MVC   PWK.PWKAGMD,BAGYMD                                               
         MVC   PWK.PWKCLT,BCLT                                                  
         MVC   PWK.PWKPRD,BPRD                                                  
         MVC   PWK.PWKEST,BEST                                                  
         MVC   PWK.PWKMKT,BMKT                                                  
         DROP  PWK                                                              
         B     XIT_01                                                           
         TITLE 'SPSFM43 - PW BILL (SUBR01--GPR#)'                               
*---------------------------- GET PW RECORD --------------------------*         
                                                                                
GETPWREC DS    0H                                                               
         MVI   GOSUBN,BPK#         SET UP PROFIT WITHIN KEY                     
         GOTO1 AGOSUB                                                           
                                                                                
         L     R6,AIO                                                           
         USING PWRECD,R6                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'PWFKEY),KEYSAVE                                            
         BNE   NO_01                                                            
                                                                                
         GOTO1 GETREC                                                           
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWWKCODQ',(R6)),0,0                   
         CLI   12(R1),X'06'                                                     
         BE    NO_01               NO ELEM ==> NO PW RECD                       
         CLI   12(R1),0             (FROM USER'S POINT-OF-VIEW)                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DS    0H                  DIG OUT SOME VALUES                          
         MVC   SKEDCGTX,PWGNGOAL                                                
         MVC   SKEDGRP,PWGNGRP                                                  
         MVC   GLTXRATE,PWGNTAX                                                 
*                                                                               
         B     YES_01                                                           
         DROP  R6                                                               
         TITLE 'SPSFM43 - PW BILL (SUBR01--GBB#)'                               
*------------------------- GET BUYS AND BILLS ------------------------*         
                                                                                
*  Get actual buys and bills into ACCUTAB                                       
                                                                                
GTBUYBIL DS    0H                                                               
         L     R2,ASBLOCK                                                       
         USING SBLOCK,R2                                                        
                                                                                
         LA    RE,SBLOCK                                                        
         LA    RF,SPBKLEN                                                       
         XCEF                                                                   
                                                                                
         MVC   SBCOMFAC,ACOMFACS                                                
         MVC   SBAIO1(12),AIO1                                                  
                                                                                
         LA    R1,IOHOOK               SET ADDRESS OF IOHOOK,                   
         ST    R1,SBIOHOOK                                                      
         L     R0,ATIA                                                          
         PRINT OFF                                                              
*&&DO                                                                           
         LH    R1,=Y(ESTAB-SPOTAREA)                                            
         AR    R1,R0                                                            
         ST    R1,SBASVETB              ESTIMATE TABLE,                         
*&&                                                                             
         PRINT ON                                                               
         MVC   SBASVETB,AESTAB                                                  
*&&DO                                                                           
         MVC   SBACHUNK,ASPCHUNK        CHUNKS TABLE,                           
*&&                                                                             
         LH    R1,=Y(SVSPARE-T217FFD)                                           
         LA    R1,T217FFD(R1)                                                   
         ST    R1,SBACHUNK              CHUNKS TABLE,                           
         LH    R1,=Y(SPTTAB-SPOTAREA)                                           
         AR    R1,R0                                                            
         ST    R1,SBASPTTB              AND SPOT TABLE                          
         LH    R1,=Y(SPTTABX-SPTTAB)                                            
         ST    R1,SBLSPTTB             L(SPOT TABLE)                            
                                                                                
         OI    SBEFLAG,SBEWIPW                                                  
         TM    ESTFLAG,EFBILEST    IF THIS IS AN E ESTIMATE,                    
         BZ    *+8                                                              
         OI    SBEFLAG,SBEEEST      TURN ON THIS FLAG FOR SPOTBUY               
         MVI   SBEPAID,C'Y'        EXTRACT PAID DATA                            
         MVI   SBEBYDT,C'Y'        GET AFFID DATE                               
                                                                                
         MVI   SBQSKIP,SBQSKMED+SBQSKMKT+SBQSKGL                                
         MVI   SBQREAD,SBQRDBH                                                  
         MVC   SBQAGY,AGENCY                                                    
         MVC   SBQMED,QMED                                                      
         MVC   SBQCLT,QCLT                                                      
                                                                                
         MVC   SBQPRD,QPRD                                                      
         MVC   SBQBPRD,BPRD                                                     
         MVC   SBEPRD,BPRD                                                      
         TM    CLTFLAG,CFWSTRAD    IS IT A TRADE CLIENT?                        
         BO    GBGPRDG              YEP, TREAT PRODUCT DIFFERENTLY              
         TM    ESTFLAG,EFWSTRAD    IS IT A TRADE ESTIMATE?                      
         BO    GBGPRDG              YEP, TREAT PRODUCT DIFFERENTLY              
         B     GBGPRDX             ELSE, LEAVE PRODUCT ALONE                    
GBGPRDG  EQU   *                                                                
         MVC   SBQPRD,=C'POL'      SET TO PRODUCT TO POL                        
         MVI   SBQBPRD,XFF          TO READ ALL BRANDS                          
         MVI   SBEPRD,XFF                                                       
GBGPRDX  EQU   *                                                                
                                                                                
         MVC   SBQMKT,QMKT                                                      
         MVC   SBQSTA,QSTA                                                      
         MVC   SBBAGYMD,BAGYMD                                                  
         ZIC   R0,BEST                                                          
         STC   R0,SBQEST                                                        
         STC   R0,SBQESTND                                                      
         L     R1,SBASVETB                                                      
         XC    0(256,R1),0(R1)     CLEAR ESTIM TABLE                            
         AR    R1,R0                                                            
         MVI   0(R1),1                                                          
                                                                                
         MVC   SBAMKEST,AMKESTAB   A(MKT/EST TABLE)                             
                                                                                
         L     RE,SBACHUNK         CLEAR CHUNK TABLE                            
         LH    RF,=Y(CHUNKX-CHUNK)                                              
         XCEF                                                                   
         L     RE,SBASPTTB         CLEAR SPOTTAB TABLE                          
         L     RF,SBLSPTTB                                                      
         XCEF                                                                   
                                                                                
         LA    R1,BRDWKTB2                                                      
         ST    R1,SBADATE          SET A(DATES),                                
         ZIC   R1,NUMWEEKS                                                      
         ST    R1,SBNDATES          AND THE NUMBER OF THEM                      
                                                                                
         XC    ELEM,ELEM           CLEAR ELEM FIELD,                            
         LA    R1,ELEM              AND USE IT AS AN 86-BYTE WORK AREA          
         STCM  R1,7,SBAWIPW         FOR SPOTIO TO DO ITS PW STUFF               
                                                                                
         MVI   GOSUBN,STW#         SAVE TWA DATA BEFORE SPOTIO CALL             
         GOTO1 AGOSUB                                                           
*                                                                               
         GOTO1 ASPOTIO,DMCB,SBLOCK                                              
*                                                                               
         MVI   GOSUBN,RTW#         RESTORE TWA DATA AFTER SPOTIO CALL           
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
** POST-SPOTIO TASKS **                                                         
*                                                                               
GBB20    DS    0H                  CHECK FOR ANY SPOTS IN SKED                  
         MVI   BYTE,XFF                                                         
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
GBB20A   TM    ATFLAG,ATFSTTLQ                                                  
         BO    GBB20X                                                           
         TM    ATFLAG,ATFMTTLQ                                                  
         BO    GBB20C                                                           
                                                                                
         TM    ATFLAG,ATFNOSPT     ANY SPOTS FOR THIS WEEK?                     
         BO    GBB20D                                                           
*                                                                               
GBB20B   LA    R4,ACCUTABQ(R4)                                                  
         TM    ATFLAG,ATFMTTLQ      YES, FAST FOWARD TO MONTH LINE              
         BZ    GBB20B                                                           
         NI    ATFLAG,XFF-ATFNOSPT   AND INDICATE SPOT FOR MNTH BL ADJ          
         NI    ATFLAG+ACCUTABQ,XFF-ATFNOSPT            AND MNTH TOTAL           
         MVI   BYTE,XFF-ATFNOSPT   SET FLAG FOR SKED LINE                       
*                                                                               
GBB20C   LA    R4,ACCUTABQ(R4)                                                  
GBB20D   LA    R4,ACCUTABQ(R4)                                                  
         B     GBB20A                                                           
*                                                                               
GBB20X   NC    ATFLAG,BYTE         SET FLAG ON SKED BILL ADJ                    
         NC    ATFLAG+ACCUTABQ,BYTE   AND SKED TOTAL LINES                      
         DROP  R4                                                               
         SPACE 2                                                                
         DS    0H                  CHECK FOR ENTIRE SKED PAID                   
         MVI   BYTE,0              ASSUME ENTIRE SKED IS PAID                   
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
GBB30A   TM    ATFLAG,ATFSTTLQ                                                  
         BO    GBB30X                                                           
         TM    ATFLAG,ATFMTTLQ     IF MONTH LINE IS REACHED HERE,               
         BO    GBB30C               BUMP TO NEXT MONTH                          
                                                                                
         TM    ATFLAG,ATFUNPDQ     WAS ANYTHING UNPAID IN THIS WEEK?            
         BZ    GBB30D               NO, BUMP TO NEXT WEEK                       
                                                                                
         MVI   BYTE,ATFUNPDQ        YES, CHANGE SKED'S FLAG, AND                
GBB30B   LA    R4,ACCUTABQ(R4)       FAST-FORWARD TO MONTH LINE                 
         TM    ATFLAG,ATFMTTLQ                                                  
         BZ    GBB30B                                                           
         OI    ATFLAG,ATFUNPDQ       AND SET FLAG ON BILL ADJ                   
         OI    ATFLAG+ACCUTABQ,ATFUNPDQ   AND MONTH TOTAL LINES                 
*                                                                               
GBB30C   LA    R4,ACCUTABQ(R4)                                                  
GBB30D   LA    R4,ACCUTABQ(R4)                                                  
         B     GBB30A                                                           
*                                                                               
GBB30X   OC    ATFLAG,BYTE                                                      
         OC    ATFLAG+ACCUTABQ,BYTE                                             
         DROP  R4                                                               
         SPACE 2                                                                
         DS    0H                  CHECK FOR ENTIRE SKED BILLED                 
         MVI   BYTE,ATF2BILL       ASSUME IT IS                                 
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
GBB40A   TM    ATFLAG,ATFSTTLQ                                                  
         BO    GBB40X                                                           
         TM    ATFLAG,ATFMTTLQ     BUMP TO MONTH LINE                           
         BZ    GBB40C                                                           
                                                                                
         LA    R4,ACCUTABQ(R4)     BUMP PAST BILLING ADJ LINE                   
         TM    ATFLAG2,ATF2ESTB    IF NOT ESTIMATE BILLED                       
         BO    GBB40B                                                           
         NI    BYTE,XFF-ATF2BILL    TURN OFF BILLED FLAGS                       
         B     GBB40C                                                           
GBB40B   TM    ATFLAG2,ATF2FNLB    IF NOT FINAL BILLED                          
         BO    GBB40C                                                           
         NI    BYTE,XFF-ATF2FNLB    TURN OFF FINAL BILLED FLAG                  
                                                                                
GBB40C   LA    R4,ACCUTABQ(R4)                                                  
         B     GBB40A                                                           
                                                                                
GBB40X   OC    ATFLAG2,BYTE         SET FLAG IN SKED LINE                       
         OC    ATFLAG2+ACCUTABQ,BYTE                                            
         DROP  R4                                                               
*                                                                               
** EXIT READING FROM FILE **                                                    
*                                                                               
GBBX     B     XIT_01                                                           
         EJECT                                                                  
*                                                                               
** SPOTIO HOOK **                                                               
*                                                                               
IOHOOK   NTR1                                                                   
         L     R6,SBAIO1           CHECK IF RECORD IS DELETED                   
         TM    (BUYRCNTL-BUYRECD)(R6),X80                                       
         BNZ   IOHKXIT              YES IT IS                                   
                                                                                
         CLI   SBMODE,SBPROCSP                                                  
         BE    PROCBUY                                                          
         CLI   SBMODE,SBPROCBL                                                  
         BE    PROCSBIL                                                         
         CLI   SBMODE,SBPROCBH                                                  
         BE    PROCBLHD                                                         
         B     IOHKXIT                                                          
         EJECT                                                                  
PROCBUY  DS    0H                  R6-->BUY RECORD                              
         USING BUYRECD,R6                                                       
         CLC   BUYMSTA(2),BMKT     SEE IF SPILL MARKET                          
         BNE   IOHKXIT              YEP, IGNORE THIS BUY RECORD                 
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   BUYMODE,0           SET NORMAL PROCESSING MODE                   
*                                                                               
         DS    0H                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(3),BUYMSTA+2   GET STAPACKED STATION                        
         TM    BUYMSTA+2,X'F0'     IF CABLE STATION,                            
         BNO   *+10                                                             
         NC    FULL(3),CBLSCMSK     KEEP SYSCODE & IGNORE NETWORK               
         CLI   USETAX,C'N'         DOES USER WANT TAX?                          
         BNE   *+10                 YES                                         
         XC    BDNTAX,BDNTAX        NOPE, SO CLEAR TAX FIELD                    
         TM    BDSTAT2,X'30'       TRADE BUY?                                   
         BZ    *+8                                                              
         MVI   BDPURP,X'FE'         YEP, MAKE $$ GO UNDER CASH PRODUCT          
                                                                                
         NI    MISCFLG3,XFF-MF3SSCHK  DON'T SKIP SPOT CHECK                     
         TM    BDCIND,X01             TEST FOR MINUS SPOT IN BUYLINE            
         BZ    *+8                     IF IT IS, SKIP #-OF-SPOTS TEST           
         OI    MISCFLG3,MF3SSCHK      SKIP SPOT CHECK                           
         PRINT OFF                                                              
*&&DO                                                                           
         MVI   GOSUBN,PDS#         SET SPOT PAID FLAGS                          
         GOTO1 AGOSUB                                                           
         CLI   MYERRCD,0           CHECK IF ANYTHING WENT WRONG                 
         BNE   IOHKXIT                                                          
*&&                                                                             
         PRINT ON                                                               
*^^gyl   BAS   RE,PBUYAFD          FUDGE AFFID DATES                            
*                                                                               
PBUY05   DS    0H                                                               
         OI    SBEFLAG2,SBENOMIN   IGNORE MINUS SPOTS (SAVES TBL SPACE)         
         OI    SBEFLAG2,SBESPLBY   SPLIT BUY (IF CHUNKS NOT BIG ENOUGH)         
         MVI   SBESPOTS,SBESPAID   GET PAID SPOTS FIRST                         
*                                                                               
PBUY08G  DS    0H                                                               
         GOTO1 ASPOTBUY,DMCB,SBLOCK                                             
         TM    SBEFLAG2,SBENOMIN   IF THIS FLAG NOT TURNED OFF,                 
         BNZ   PBUY08X                                                          
         MVI   MYERRCD,TBOFQ        WE ENCOUNTERED A TABLE OVERFLOW             
         B     IOHKXIT                                                          
PBUY08X  EQU   *                                                                
                                                                                
*                                                                               
         L     R3,SBACHUNK                                                      
         USING SCHUNKD,R3                                                       
*                                                                               
PBUY10   OC    SCNEXT,SCNEXT                                                    
         BZ    PBUY090                                                          
*                                                                               
         DS    0H                                                               
         TM    MISCFLG3,MF3SSCHK   SKIP SPOT CHECK?                             
         BO    PBUYSPTS             YEP!                                        
                                                                                
         ICM   R0,15,SCSPOTS       TEST IF THERE ARE ANY SPOTS                  
         BZ    PBUY40               IGNORE CHUNK IF THERE ARE NONE              
PBUYSPTS EQU   *                                                                
                                                                                
*                                                                               
         PRINT OFF                                                              
*&&DO                                                                           
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
PBUY10A  TM    ATFLAG,ATFSTTLQ+ATFMTTLQ                                         
         BNZ   PBUY10B                                                          
         CLC   ATWSTART,SCDATE     FIND WITHIN RANGE                            
         BH    PBUY40                                                           
         CLC   ATWEND,SCDATE                                                    
         BNL   PBUY20                                                           
PBUY10B  LA    R4,ACCUTABQ(R4)                                                  
         B     PBUY10A                                                          
*&&                                                                             
         PRINT ON                                                               
         MVC   DATE2,SCDATE                                                     
         L     RF,ALOCDATE                                                      
         BASR  RE,RF               FIND ENTRY WITHIN ACCUTAB                    
         OR    R4,R4               A(ENTRY) RETURNED IN  R4                     
         BZ    PBUY40               UNLESS IT COULD NOT FIND AN ENTRY           
         USING ACCUTABD,R4                                                      
*                                                                               
PBUY20   CLI   BUYMODE,0           TEST NORMAL PROCEESING                       
         BNE   PBUY30               NO, MUST BE MAKEGOODS-OUT                   
                                                                                
*                                                                               
         DS    0H                  SEE IF SPOTS CAN BE DEFERRED                 
         CLI   SBESPOTS,SBESPAID    CAN'T DEFER PAID SPOTS                      
         BE    PBUY22X                                                          
                                                                                
         LA    R1,ACCUTABQ(R4)               BUMP TO NEXT ACCUTAB ENTRY         
         TM    ATFLAG-ACCUTABD(R1),ATFMTTLQ  WAS R4-->LAST WEEK IN MTH?         
         BZ    PBUY22X                        NO, CAN'T DEFER SPOT              
         MVC   SDATE2,ATMSTART                SET START AND                     
         MVC   EDATE2,ATMEND                   END DATES OF B'CST MNTH          
         MVI   GOSUBN,DFR#                    DEFER SPOT?                       
         GOTO1 AGOSUB                                                           
         BNE   PBUY22X                         NO, CAN'T DEFER SPOT             
                                                                                
         LR    R1,R4                                                            
PBUY22G  LA    R1,ACCUTABQ(R1)                                                  
         TM    ATFLAG-ACCUTABD(R1),ATFSTTLQ                                     
         BNZ   PBUY22X                                                          
         TM    ATFLAG-ACCUTABD(R1),ATFMTTLQ                                     
         BNZ   PBUY22G                                                          
         LR    R4,R1               DEFER SPOT TO THE NEXT MONT                  
PBUY22X  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   SBESPOTS,SBESUNPD   IF UNPAID SPOTS ARE BEING PASSED,            
         BNE   *+8                                                              
         OI    ATFLAG,ATFUNPDQ      MARK SPOT UNPAID IN WEEK                    
                                                                                
*                                                                               
         DS    0H                                                               
         NI    ATFLAG,XFF-ATFNOSPT AT LEAST ONE SPOT IN WEEK                    
                                                                                
         ICM   R1,15,ATACBUY       TAKE WHAT'S IN ACCUTAB,                      
         A     R1,SCGROSS           ADD IT TO WHAT'S IN CHUNK,                  
         STCM  R1,15,ATACBUY        AND PUT IT BACK INTO ACCUTAB                
         ICM   R1,15,ATTAX         DO THE SAME FOR TAX DOLLARS                  
         A     R1,SCTAX                                                         
         STCM  R1,15,ATTAX                                                      
         ICM   R1,15,ATCLTAX       UPDATE CLIENT TAX DOLLARS                    
         A     R1,SCPWCLTX                                                      
         STCM  R1,15,ATCLTAX                                                    
         ICM   R1,15,ATAJBUY       UPDATE CLCOST$ (ADJUSTED BUY)                
         A     R1,SCPWGRS                                                       
         STCM  R1,15,ATAJBUY        IN ACCUTAB                                  
         ICM   R1,15,ATNSPT        UPDATE # OF SPOTS                            
         A     R1,SCSPOTS                                                       
         STCM  R1,15,ATNSPT                                                     
         ICM   R1,15,ATWNET        UPDATE WIM NET $                             
         A     R1,SCNET                                                         
         STCM  R1,15,ATWNET                                                     
         ICM   R1,15,ATCNET        UPDATE CLT NET $                             
         A     R1,SCPWNET                                                       
         STCM  R1,15,ATCNET                                                     
                                                                                
         CLI   BUYMODE,0                                                        
         BNE   PBUY25                                                           
         MVI   GOSUBN,SST#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
PBUY25   DS    0H                                                               
*^^SUP   CLI   BDMGDATE,X'C0'      TEST NEW MAKEGOOD                            
*^^SUP   BNH   PBUY40               NOTE -- IGNORE OLD MAKEGOODS !              
*^^SUP   MVC   DATE2,SCDATE         YES, ADD TO MAKEGOOD-IN ACCUMS              
*^^SUP   GOTO1 ALOCDATE            GET ACCUTAB SLOT IN R4, AND                  
*^^SUP   ICM   R1,15,ATMGIN         ADD TO MAKEGOOD-IN ACCUM                    
*^^SUP   A     R1,SCGROSS                                                       
*^^SUP   STCM  R1,15,ATMGIN                                                     
         B     PBUY40                                                           
                                                                                
* THIS PROCESSING SPECIAL FOR MAKEGOODS OUT                                     
                                                                                
PBUY30   MVC   DATE2,SCDATE                                                     
         GOTO1 ALOCDATE            GET ACCUTAB SLOT IN R4                       
                                                                                
         OR    R4,R4               A(ENTRY) PASSED BACK?                        
         BNZ   PBUY32               YES                                         
         GOTO1 DATCON,DMCB,(X'82',DATE2),(5,MYTEXT+1),0                         
         MVC   MYTEXT(1),DMCB+4                                                 
         MVI   MYERRCD,SPDATQ      SET ERROR MSG CODE                           
         B     IOHKXIT                                                          
                                                                                
*                                                                               
PBUY32   DS    0H                                                               
*^^SUP   ICM   R1,15,ATMGOUT       ADD MG-OUT INTO ACCUTAB                      
*^^SUP   A     R1,SCGROSS                                                       
*^^SUP   STCM  R1,15,ATMGOUT                                                    
*                                                                               
PBUY40   L     R3,SCNEXT                                                        
         B     PBUY10                                                           
                                                                                
*                                                                               
** FINISH PROCESSING DATA IN CHUNKS AREA **                                     
*                                                                               
PBUY090  DS    0H                                                               
         OC    SBACONT,SBACONT     ANY BUY CONTINUATION?                        
         BZ    PBUY100              NO, DONE WITH BUY RECORD                    
         B     PBUY08G             GO GET THE REST OF THE BUY                   
                                                                                
*                                                                               
** FINISHED PROCESSING BUY RECORD **                                            
*                                                                               
PBUY100  DS    0H                                                               
         CLI   SBESPOTS,SBESPAID   IF WE WERE GETTING PAID SPOTS                
         BNE   *+12                                                             
         MVI   SBESPOTS,SBESUNPD    GO BACK AND GET UNPAID SPOTS                
         B     PBUY08G                                                          
*                                                                               
         CLI   SBESPOTS,SBESUNPD   IF WE WERE GETTING UNPAID SPOTS              
         BNE   *+12                                                             
         MVI   SBESPOTS,0           WE'VE FINISHED PROCESSING BUY REC           
         B     PBUY100X                                                         
*                                                                               
         DC    H'0'                                                             
PBUY100X EQU   *                                                                
                                                                                
*                                                                               
PBUYX    CLI   BUYMODE,0           TEST NORMAL PROCESSING MODE                  
         BNE   PBUYX2               NO - DONE                                   
*^^SUP   BAS   RE,TSTMGOUT         SEE IF ANY MAKEGOODS-OUT THIS LINE           
         CLI   BUYMODE,C'M'        WERE THERE ANY?                              
         BE    PBUY05               YUP                                         
*                                                                               
PBUYX2   B     IOHKXIT                                                          
         EJECT                                                                  
* See if any spots on this buyline were madegood.                               
*  If they were, set all elements not madegood to be ignored,                   
*  and process buy again with only madegood spots in place.                     
                                                                                
TSTMGOUT DS    0H                                                               
         LA    R1,BDELEM                                                        
*                                                                               
TMGOUT2  ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BE    TMGOUTX                                                          
         CLI   0(R1),X'0B'                                                      
         BL    TMGOUT2                                                          
         CLI   0(R1),X'0C'                                                      
         BH    TMGOUT2                                                          
         CLI   1(R1),10            TEST ALLOCATED                               
         BNH   TMGOUT2                                                          
*                                                                               
         TM    RSTATUS-REGELEM(R1),X'02' TEST MG ON NEW LINE                    
         BO    TMGOUT4                                                          
         OI    0(R1),X'80'         GET RID OF NON-MG ELEMENTS                   
         B     TMGOUT2                                                          
*                                                                               
TMGOUT4  NI    RSTATUS-REGELEM(R1),X'FF'-X'42' RESET STATUS                     
         MVI   BUYMODE,C'M'        SET FLAG WE FOUND ONE                        
         B     TMGOUT2                                                          
*                                                                               
TMGOUTX  BR    RE                                                               
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
*&&DO                                                                           
* This routine was a desperation fix.  When SPOTBUY was asked to break          
*  out affid dates, my allocated CHUNK space was not enough to store            
*  all the CHUNK entries broken out for each different affid date.              
*  Since all that is really needed in the PROCBUY routine was whether           
*  a spot was matched or not, the affid date is going to be exploited           
*  as a flag.  The affid date, if present, will be set to a dummy               
*  compressed date (x'0001').  This will hopefully separate the                 
*  matched spots from the unmatched spots without having to break out           
*  all the different CHUNK entries.                                             
                                                                                
PBUYAFD  NTR1                                                                   
         AH    R6,=Y(BDELEM-BUYREC)                                             
         SR    R0,R0                                                            
                                                                                
PBUYAFD2 DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    XIT_01                                                           
         CLI   0(R6),X'10'                                                      
         BNE   *+10                                                             
         MVC   ADATE-AFFELEM(,R6),=X'0001'                                      
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PBUYAFD2                                                         
*&&                                                                             
         EJECT                                                                  
PROCSBIL DS    0H                  R6-->STATION BILL RECORD                     
                                                                                
         USING STABUCKD,R6                                                      
         CLC   STABKMKT,BMKT       MATCH ON MKT NUMBER                          
         BNE   IOHKXIT                                                          
         OC    BSTA,BSTA            AND STATION IF DDS TESTING                  
         BZ    *+14                                                             
         CLC   STABKSTA,BSTA                                                    
         BNE   IOHKXIT                                                          
         CLI   STABKCUR,X01        THESE RECDS HAVE THE ADJUSTMENTS             
         BNE   IOHKXIT              FOR PW (PER GRANT)                          
         DROP  R6                                                               
*                                                                               
         DS    0H                  GET BILL AMOUNTS                             
         MVI   ELCODE,X'0E'                                                     
         MVC   DATADISP,=Y(STABELEM-STABUCK)                                    
         BAS   RE,GETEL                                                         
         B     PSBL05A                                                          
PSBL05   BAS   RE,NEXTEL                                                        
PSBL05A  BNE   IOHKXIT                                                          
         USING STABELEM,R6                                                      
                                                                                
         DS    0H                  FIND MONTH NTRY W/IN ACCUTAB                 
         MVC   STARTEND(12),ESDATE JUST USE THE ESTIMATE'S START/END            
         TM    ESTFLAG,EFBILEST     IF THIS IS AN E ESTIMATE,                   
         BO    PSBL07               AS PER GRANT                                
                                                                                
         MVC   WORK(2),STABPER     SET UP BINARY YEAR, MONTH                    
         MVI   WORK+2,1             DAY                                         
         GOTO1 DATCON,DMCB,(3,WORK),(0,MYDATE6),0                               
PSBL06   GOTO1 AGETBROD,DMCB,(1,MYDATE6),STARTEND,AGETDAY,AADDAY                
*****                                                                           
         CLI   DMCB,X'FF'           ERROR FROM GETBROAD                         
         BNE   PSBL06A                                                          
         CLC   =C'13',MYDATE6+2                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MYDATE6+2(2),=C'12'                                              
         B     PSBL06                                                           
*****                                                                           
PSBL06A  CLC   STARTEND(6),ESDATE     FORCE DATE TO START ON OR                 
         BNL   *+10                    AFTER ESTIMATE'S START DATE              
         MVC   STARTEND(6),ESDATE                                               
         CLC   STARTEND+6(6),EEDATE   FORCE DATE TO END ON OR                   
         BNH   *+10                    BEFORE ESTIMATE'S END DATE               
         MVC   STARTEND+6(6),EEDATE                                             
                                                                                
PSBL07   DS    0H                                                               
         GOTO1 DATCON,DMCB,(X'10',STARTEND),(2,WORK),0                          
         MVC   FULL(2),WORK                                                     
         MVC   FULL+2(2),WORK+3    REMOVE HYPHEN FROM DATCON OUTPUT             
*                                                                               
         DS    0H                  FULL=STRT/END OF TARGET BCAST MNTH           
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
PSBL10   CLI   0(R4),XFF                                                        
         BE    PSBL05              CAN'T MATCH MONTH--TRY NEXT ELEMENT          
         TM    ATFLAG,ATFMTTLQ                                                  
         BZ    PSBL20                                                           
         CLC   ATMONTH,FULL                                                     
         BE    PSBL30                                                           
PSBL20   LA    R4,ACCUTABQ(R4)                                                  
         B     PSBL10                                                           
                                                                                
PSBL30   DS    0H                                                               
         OI    ATFLAG2,ATF2ESTB    AUTOMATICALLY ESTIM BILLED                   
         OI    ATFLAG2+ACCUTABQ,ATF2ESTB                                        
                                                                                
         DS    0H                  ADD UP BILL AMOUNTS                          
         ICM   R2,15,STABGRS        GET GROSS BILLED AMOUNT                     
         CLI   USETAX,C'Y'          USE TAX IN CALCULATIONS?                    
         BE    PSBL35A               YES                                        
                                                                                
         SR    R1,R1                 NO, GET (CLT GROSS) TAX AMOUNT             
         CLI   STABELEM+1,21          (DEPENDING ON WHETHER ELEM                
         BNE   *+8                                                              
         ICM   R1,14,STABTAX                                                    
         CLI   STABELEM+1,22           LENGTH IS 21 OR 22 BYTES)                
         BNE   *+8                                                              
         ICM   R1,14,STABTAX+1                                                  
         SRA   R1,8                   PROPAGATE TO LOWER-ORDER BYTES            
         SR    R2,R1               R2 = CLT$ BILLED W/O TAX                     
                                                                                
PSBL35A  DS    0H                  ADD TO ACCUMULATOR                           
         A     R2,ATBILL                                                        
         ST    R2,ATBILL                                                        
         ST    R2,ATBILL+ACCUTABQ                                               
*                                                                               
         B     PSBL05                                                           
         DROP  R4,R6                                                            
         EJECT                                                                  
PROCBLHD DS    0H                  R6-->BILL HEADER RECORD                      
                                                                                
         USING BILLRECD,R6                                                      
         CLC   BLMKT,SPACES                                                     
         BE    *+14                                                             
         CLC   BLMKT,QMKT          IS IT THE RIGHT MARKET?                      
         BNE   PBHDX                NO, GO EXIT                                 
                                                                                
         DS    0H                  FIND MONTH ENTRY W/IN ACCUTAB                
         MVC   STARTEND,ESDATE     JUST USE THE ESTIMATE'S START/END            
         TM    ESTFLAG,EFBILEST     IF THIS IS AN E ESTIMATE,                   
         BO    PBHD07                                                           
                                                                                
         MVC   WORK(2),BKEYYSRV    SET UP BINARY YEAR, MONTH                    
         MVI   WORK+2,1             DAY                                         
         GOTO1 DATCON,DMCB,(3,WORK),(0,MYDATE6),0                               
PBHD06   GOTO1 AGETBROD,DMCB,(1,MYDATE6),STARTEND,AGETDAY,AADDAY                
*****                                                                           
         CLI   DMCB,X'FF'           ERROR FROM GETBROAD                         
         BNE   PBHD06A                                                          
         CLC   =C'13',MYDATE6+2                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MYDATE6+2(2),=C'12'                                              
         B     PBHD06                                                           
*****                                                                           
PBHD06A  CLC   STARTEND(6),ESDATE     FORCE DATE TO START ON OR                 
         BNL   *+10                    AFTER ESTIMATE'S START DATE              
         MVC   STARTEND(6),ESDATE                                               
         CLC   STARTEND+6(6),EEDATE   FORCE DATE TO END ON OR                   
         BNH   *+10                    BEFORE ESTIMATE'S END DATE               
         MVC   STARTEND+6(6),EEDATE                                             
                                                                                
PBHD07   DS    0H                                                               
         GOTO1 DATCON,DMCB,(X'10',STARTEND),(2,WORK),0                          
         MVC   FULL(2),WORK                                                     
         MVC   FULL+2(2),WORK+3    REMOVE HYPHEN FROM DATCON OUTPUT             
*                                                                               
         DS    0H                  ADJST TO USE CORRECT MTH IN CASE EST         
         DS    0H                   SPANS INTO NXT MTH NOT IN ACCUTAB           
         LA    R3,BRDMTHTB                                                      
         USING BCSTTABD,R3                                                      
PBHD08B  CLI   0(R3),XFF           END OF BROADCAST MONTH TABLE?                
         BNE   PBHD08E              NO, GO CHECK THIS B'CST MONTH               
         MVC   WORK+0(2),BKEYYSRV   YEP, GET YEAR/MONTH FOR ERROR MSG           
         MVI   WORK+2,0                                                         
         GOTO1 DATCON,DMCB,(X'83',WORK),(6,MYTEXT+1),0                          
         MVC   MYTEXT(1),DMCB+4      LENGTH OF REPLACE TEXT                     
         MVI   MYERRCD,BHDATQ                                                   
         B     PBHDX                                                            
PBHD08E  EQU   *                                                                
                                                                                
         CLC   BCSSTART(4),FULL    MATCH ON B'CST MTH START/END                 
         BE    *+12                                                             
         LA    R3,BCSTTABQ(R3)                                                  
         B     PBHD08B                                                          
                                                                                
         TM    BCSFLAG,BCSFMXQ     IF B'CST MTH IS EFFECTIVE EOT,               
         BZ    *+8                                                              
         SH    R3,=Y(BCSTTABQ)      THEN USE PREVIOUS B'CST MONTH               
         MVC   FULL,BCSSTART       SET B'CST MTH START/END IN FULL              
         DROP  R3                                                               
*                                                                               
                                                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
PBHD10   CLI   0(R4),XFF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    ATFLAG,ATFMTTLQ                                                  
         BZ    PBHD20                                                           
         CLC   ATMONTH,FULL                                                     
         BE    PBHD30                                                           
PBHD20   LA    R4,ACCUTABQ(R4)                                                  
         B     PBHD10                                                           
*                                                                               
PBHD30   DS    0H                                                               
         OI    ATFLAG2,ATF2ESTB     AUTOMATICALLY ESTIM BILLED                  
         OI    ATFLAG2+ACCUTABQ,ATF2ESTB                                        
         TM    BILSTAT2,BSTCLRDQ                                                
         BZ    PBHDX                                                            
         OI    ATFLAG2,ATF2FNLB     IT'S ALSO FINAL BILLED                      
         OI    ATFLAG2+ACCUTABQ,ATF2FNLB                                        
*                                                                               
PBHDX    DS    0H                                                               
         B     IOHKXIT                                                          
         DROP  R4,R6                                                            
                                                                                
                                                                                
IOHKXIT  DS    0H                                                               
         XIT1                                                                   
         DROP  R2                                                               
         TITLE 'SPSFM43 - PW BILL (SUBR01--FLK#)'                               
*--------------------------- FILL IN LOCKINS -------------------------*         
                                                                                
* Puts locked dollars (from mkt-level recd) into ACCUTAB                        
                                                                                
FILLCKN  DS    0H                                                               
         MVI   GOSUBN,GPR#         GET PW RECORD                                
         GOTO1 AGOSUB                                                           
         L     R6,AIO                                                           
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
         MVI   ELCODE,PWDOLCDQ                                                  
                                                                                
         BAS   RE,GETEL                                                         
FLK10    BNE   FLK30                                                            
                                                                                
         USING PWDOLEL,R6                                                       
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
FLK20    TM    ATFLAG,ATFSTTLQ     IF SKED LINE, GET NEXT ELEM                  
         BO    FLK20C                                                           
         TM    ATFLAG,ATFMTTLQ     IF MNTH LINE, GET NEXT WEEK                  
         BO    FLK20B                                                           
                                                                                
         CLC   PWDOLWK,ATWSTART    IF DATE IS NOT WITHIN                        
         BL    FLK20B                                                           
         CLC   PWDOLWK,ATWEND       RANGE, TRY NEXT WEEK'S DATE                 
         BH    FLK20B                                                           
                                                                                
         ICM   R1,15,PWDOLWG                                                    
         A     R1,ATWLCK                                                        
         ST    R1,ATWLCK           WIM LOCK                                     
         ICM   R1,15,PWDOLCG                                                    
         A     R1,ATCLCK                                                        
         ST    R1,ATCLCK           CLT LOCK                                     
         ICM   R1,15,PWDOLTAX                                                   
         A     R1,ATLKTAX                                                       
         ST    R1,ATLKTAX          TAX LOCK                                     
         ICM   R1,15,PWDOLCTX                                                   
         A     R1,ATLKCTX                                                       
         ST    R1,ATLKCTX          CLT TAX DOLLARS LOCK                         
                                                                                
         CLI   USETAX,C'Y'                                                      
         BE    FLK20C                                                           
         L     R0,ATWLCK           GET WIM LOCK                                 
         ICM   R1,15,PWDOLTAX                                                   
         SR    R0,R1               LESS TAX ON WIM NET                          
         ST    R0,ATWLCK           GIVES WIM LOCK W/O TAX                       
         L     R0,ATCLCK           GET CLT LOCK                                 
         ICM   R1,15,PWDOLCTX                                                   
         SR    R0,R1                LESS CLT TAX DOLLARS                        
         ST    R0,ATCLCK            GIVES CLT LOCK W/O TAX                      
         B     FLK20C                                                           
*                                                                               
FLK20B   LA    R4,ACCUTABQ(R4)                                                  
         B     FLK20                                                            
FLK20C   BAS   RE,NEXTEL                                                        
         B     FLK10                                                            
         DROP  R4,R6                                                            
*                                                                               
** OVERRIDING LOCKED DOLLAR AMOUNTS **                                          
*                                                                               
FLK30    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PWCLLCDQ                                                  
                                                                                
         BAS   RE,GETEL                                                         
         B     FLK30B                                                           
FLK30A   BAS   RE,NEXTEL                                                        
FLK30B   BNE   FLK40                                                            
                                                                                
         USING PWCLLEL,R6                                                       
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
FLK30C   TM    ATFLAG,ATFSTTLQ     IF SKED LINE, GET NEXT ELEM                  
         BO    FLK30A                                                           
         TM    ATFLAG,ATFMTTLQ     IF MNTH LINE, GET NEXT WEEK                  
         BO    FLK30D                                                           
                                                                                
         CLC   PWCLLWK,ATWSTART    IF DATE IS NOT WITHIN                        
         BL    FLK30D                                                           
         CLC   PWCLLWK,ATWEND       RANGE, TRY NEXT WEEK'S DATE                 
         BH    FLK30D                                                           
         ICM   R1,15,PWCLLAMT                                                   
         A     R1,ATCLCK                                                        
         ST    R1,ATCLCK           CLT LOCKIN OVERRIDE                          
         OI    ATFLAG2,ATF2CLOD     AND TURN FLAG ON IN ACCUTAB                 
         B     FLK30A                                                           
*                                                                               
FLK30D   LA    R4,ACCUTABQ(R4)                                                  
         B     FLK30C                                                           
         DROP  R4,R6                                                            
*                                                                               
FLK40    DS    0H                                                               
*                                                                               
FLKX     B     XIT_01                                                           
         TITLE 'SPSFM43 - PW BILL (SUBR01--FDC#)'                               
*---------------------------- FILL IN DR/CR --------------------------*         
                                                                                
* Puts Adj DR/CR amounts (from mkt-level PW recd) into ACCUTAB                  
                                                                                
FILLDRCR DS    0H                                                               
                                                                                
         DS    0H                  GET PW RECORD                                
         MVI   GOSUBN,BPK#                                                      
         GOTO1 AGOSUB                                                           
         LA    R6,KEY                                                           
         USING PWRECD,R6                                                        
         XC    PWKSTA,PWKSTA       READ MKT-LEVEL PW RECORD,                    
         OC    BSTA,BSTA                                                        
         BZ    FDC007X                                                          
         MVC   PWKSTA,BSTA          UNLESS DDS TESTING                          
         TM    BSTA,X'F0'                                                       
         BNO   *+10                                                             
         NC    PWKSTA,CBLSCMSK                                                  
FDC007X  EQU   *                                                                
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'PWFKEY),KEYSAVE                                            
         BNE   FDCX                                                             
         GOTO1 GETREC                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         L     R6,AIO                                                           
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
         TM    ESTFLAG,EFOWPW      IF OOWR PW BILLING,                          
         BNZ   FDC050               USE OOW PAID$ ELEMENTS                      
                                                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,PWDOLCDQ                                                  
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
         BAS   RE,GETEL                                                         
         BNE   FDCX                EXIT IF NO DOLLAR ELEMENT                    
         USING PWDOLEL,R6                                                       
*                                                                               
FDC10    DS    0H                  R4-->1ST WEEK OF A MONTH (ACCUTAB)           
         CLC   ATWSTART,PWDOLWK    LOOK FOR FIRST WEEK IN RECD                  
         BNE   FDC14                                                            
                                                                                
         DS    0H                   FOUND IT!                                   
FDC12    LA    R4,ACCUTABQ(R4)      FAST FORWARD TO BILL ADJ LINE               
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   FDC12                                                            
         MVC   ATDRCR,PWDOLBIL       AND MOVE ADJ BILLING INFO                  
         MVC   ATBILD,PWDOLBLD       INTO ACCUTAB                               
         LA    R4,ACCUTABQ(R4)      BUMP TO MONTH TOTAL LINE                    
         B     FDC20                                                            
                                                                                
FDC14    DS    0H                   NO 1ST WEEK FOR MONTH IN RECD               
         LA    R4,ACCUTABQ(R4)      FAST FORWARD TO MONTH TOTAL LINE            
         TM    ATFLAG,ATFMTTLQ                                                  
         BZ    FDC14                                                            
         TM    ATFLAG,ATFBILAJ                                                  
         BO    FDC14                                                            
*                                                                               
FDC20    BAS   RE,NEXTEL           GET 1ST DOLLAR ELEMENT OF NEXT MONTH         
         BNE   FDCX                                                             
         CLC   ATWEND,PWDOLWK      ATWEND = END DATE OF CURRENT MONTH           
         BNL   FDC20                                                            
                                                                                
         LA    R4,ACCUTABQ(R4)     R4-->1ST WEEK OF NEXT MNTH (ACCUTAB)         
         TM    ATFLAG,ATFSTTLQ     STOP IF SKED LINE REACHED                    
         BZ    FDC10                                                            
         DROP  R4,R6                                                            
                                                                                
*                                                                               
FDC050   DS    0H                  USE OOWR PAID$ ELEMENTS                      
         MVI   ELCODE,PWOOWCDQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   FDC079              EXIT IF NO PAID$ ELEMENT                     
         USING PWOOWEL,R6                                                       
*                                                                               
FDC055   DS    0H                  LOOK FOR PAID$ ELEM'S B'CAST MONTH           
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
         DS    0H                                                               
FDC057   LA    R4,ACCUTABQ(R4)      FAST FORWARD TO A BILL ADJ LINE             
         TM    ATFLAG,ATFSTTLQ      IF SKED LINE REACHED,                       
         BO    FDC070                GET NEXT ELEM                              
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   FDC057                                                           
         GOTO1 DATCON,DMCB,(X'02',ATMEND),(X'03',FULL),0                        
         CLC   PWOOWYM,FULL                                                     
         BNE   FDC057                                                           
                                                                                
         MVC   ATDRCR,PWOOWBIL      FOUND CORRECT BILL ADJ LINE                 
         XC    ATBILD,ATBILD        MOVE ADJ BILLING INFO INTO ACCUTAB          
         B     FDC070                                                           
*                                                                               
FDC070   DS    0H                                                               
         BRAS  RE,NEXTEL           GET NEXT OOW PAID$ ELEM                      
         BE    FDC055                                                           
FDC079   EQU   *                                                                
         DROP  R4                                                               
                                                                                
*                                                                               
FDCX     DS    0H                                                               
         B     XIT_01                                                           
         DROP  R6                                                               
         TITLE 'SPSFM43 - PW BILL (SUBR01--RUP#)'                               
*------------------------------ ROUNDING -----------------------------*         
                                                                                
*  Rounds off to the nearest 100s                                               
*   At entry, R0 contains the number to be rounded                              
*   At exit,  FULL = the result                                                 
                                                                                
ROUNDUP  DS    0H                                                               
         AR    R0,R0               2N (N = THE NUMBER)                          
         SRDA  R0,32                                                            
         D     R0,=F'100'          N/50                                         
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'            (N/50)+1                                     
         SRA   R1,1                (N/100)+0.5                                  
         ST    R1,FULL                                                          
         B     XIT_01                                                           
         PRINT OFF                                                              
*&&DO                                                                           
         TITLE 'SPSFM43 - PW BILL (SUBR01--SUM#)'                               
*------------------------- TOTAL UP SCHEDULE -------------------------*         
                                                                                
*  By this time, actual & adjusted goals and buys, PW%, and lockins             
*   should be set in ACCUTAB already                                            
                                                                                
SUMUP    DS    0H                                                               
         XC    MTOTALS(MTOTALQ),MTOTALS                                         
         XC    STOTALS(STOTALQ),STOTALS                                         
         XC    SKEDBILL,SKEDBILL                                                
         XC    SKEDDRCR,SKEDDRCR                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
SUM10    TM    ATFLAG,ATFSTTLQ                                                  
         BO    SUM30                                                            
         TM    ATFLAG,ATFMTTLQ                                                  
         BO    SUM20                                                            
                                                                                
         LA    R0,ATACVALN         R0 = # OF VALUES TO SUM UP                   
         SR    R1,R1                                                            
SUM12    LA    R2,ATACVALS(R1)     R2-->ACCUMULATOR VALUE                       
         ICM   RE,15,0(R2)         RE = ACCUMULATOR VALUE                       
         L     RF,MTOTALS(R1)                                                   
         AR    RF,RE                                                            
         ST    RF,MTOTALS(R1)                                                   
         L     RF,STOTALS(R1)                                                   
         AR    RF,RE                                                            
         ST    RF,STOTALS(R1)                                                   
         LA    R1,L'ATACVALS(R1)                                                
         BCT   R0,SUM12                                                         
         B     SUM50                                                            
*                                                                               
SUM20    DS    0H                  MONTH LINE                                   
         TM    ATFLAG,ATFBILAJ      DETERMINE WHETHER IT'S BILL ADJ             
         BZ    SUM23                OR MONTH TOTALS LINE                        
                                                                                
         DS    0H                  MONTHLY BILL ADJUSTMENT                      
         MVC   ATACVALS(MTOTALQ),MTOTALS   TOTALS W/O DR/CR AMOUNT              
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPAJB,ATAJBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWPW#        CALC CURR PW%                                
         GOTO1 AGOSUB                                                           
         MVC   ATPW,TEMPPW          AND UPDATE ACCUTAB                          
                                                                                
         MVC   TEMPACB,ATWLCK      CALC PW% OF LOCKED VALUES                    
         MVC   TEMPAJB,ATCLCK                                                   
         MVC   TEMPTAX,ATLKTAX                                                  
         MVI   GOSUBN,PWPW#                                                     
         GOTO1 (RF)                                                             
         MVC   ATLPW,TEMPPW         AND UPDATE ACCUTAB                          
         MVC   ATLPW+ACCUTABQ,TEMPPW  PUT ON MNTH TOTALINE TOO                  
                                                                                
         ICM   R0,15,ATDRCR         ADD DR/CR TO CURRENT CLT$                   
         BZ    SUM29                                                            
         CLM   R0,15,=X'80000000'                                               
         BNE   *+6                                                              
         SR    R0,R0                                                            
         LR    R1,R0                                                            
         LR    RF,R0                                                            
         A     RF,SKEDDRCR                                                      
         ST    RF,SKEDDRCR                                                      
         A     R1,MTTLAJB                                                       
         ST    R1,MTTLAJB                                                       
         A     R0,SKEDAJB                                                       
         ST    R0,SKEDAJB                                                       
         B     SUM29                                                            
                                                                                
SUM23    DS    0H                  MONTHLY TOTALS LINE                          
         MVC   ATACVALS(MTOTALQ),MTOTALS   GET SUMS TO MONTH LINE               
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPAJB,ATAJBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWPW#                CALC PROJ PW%                        
         GOTO1 AGOSUB                                                           
         MVC   ATPW,TEMPPW                  AND UPDATE ACCUTAB                  
                                                                                
         L     R0,SKEDBILL                 CALCULATE THE ACTUAL BILL            
         A     R0,ATBILL                                                        
         ST    R0,SKEDBILL                  FOR ENTIRE SCHEDULE                 
                                                                                
         XC    MTOTALS(MTOTALQ),MTOTALS    RESET FOR NEXT MONTH                 
                                                                                
SUM29    B     SUM50                                                            
*                                                                               
SUM30    DS    0H                  SKED BILL ADJ LINE                           
         MVC   ATDRCR,SKEDDRCR     MOVE IN SKED TOTAL DR/CR                     
         MVC   ATBILL,SKEDBILL      AND SKED TOTAL BILL                         
         MVC   ATACVALS(STOTALQ),STOTALS   GET SUMS TO SKED LINE                
         ICM   R0,15,ATAJBUY                                                    
         S     R0,SKEDDRCR                                                      
         STCM  R0,15,ATAJBUY                W/O DR/CR TOTAL                     
                                                                                
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPAJB,ATAJBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWPW#           CALC CURR PW%                             
         GOTO1 AGOSUB                                                           
         MVC   ATPW,TEMPPW             AND UPDATE ACCUTAB                       
                                                                                
         MVC   TEMPACB,ATWLCK         CALC PW% OF LOCKED VALUES                 
         MVC   TEMPAJB,ATCLCK                                                   
         MVC   TEMPTAX,ATLKTAX                                                  
         MVI   GOSUBN,PWPW#                                                     
         GOTO1 (RF)                                                             
         MVC   ATLPW,TEMPPW            AND UPDATE ACCUTAB                       
         MVC   ATLPW+ACCUTABQ,TEMPPW   PUT IT IN SKED TOTALS LINE TOO           
                                                                                
         LA    R4,ACCUTABQ(R4)     BUMP TO SKED TOTALS LINE                     
                                                                                
         DS    0H                  SKED TOTALS LINE                             
         MVC   ATACVALS(STOTALQ),STOTALS   GET SUMS TO SKED LINE                
         MVC   ATBILL,SKEDBILL                                                  
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPAJB,ATAJBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWPW#           CALC PROJ PW%                             
         GOTO1 AGOSUB                                                           
         MVC   ATPW,TEMPPW             AND UPDATE ACCUTAB                       
         B     SUMUPX                                                           
*                                                                               
SUM50    LA    R4,ACCUTABQ(R4)                                                  
         B     SUM10                                                            
*                                                                               
SUMUPX   B     XIT_01                                                           
         DROP  R4                                                               
         TITLE 'SPSFM43 - PW BILL (SUBR01--UPT#)'                               
*--------------------------- UPDATE PW% TABLE ------------------------*         
                                                                                
* transfers PW%s from ACCUTAB at end of every "good" modification *             
                                                                                
UPWTAB   DS    0H                                                               
                                                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
         LA    R6,PWTAB                                                         
                                                                                
UPT10    TM    ATFLAG,ATFSTTLQ     IF SCHED TOTALS ENTRY                        
         BO    UPTX                 THEN WE'RE FINISHED                         
         TM    ATFLAG,ATFMTTLQ     IF MONTHLY TOTALS ENTRY,                     
         BO    UPT20                THEN SKIP                                   
                                                                                
         CLC   0(4,R6),ATWEEK      MAKE SURE WE'RE AT THE RIGHT WEEK            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   4(4,R6),ATPW        UPDATE PW TABLE                              
         LA    R6,8(R6)            BUMP PWTAB POINTER                           
                                                                                
UPT20    LA    R4,ACCUTABQ(R4)     BUMP ACCUTAB POINTER                         
         B     UPT10                                                            
*                                                                               
UPTX     CLI   0(R6),XFF           BETTER BE AT EOTABLE OF PWTAB                
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT_01                                                           
         DROP  R4                                                               
*&&                                                                             
         PRINT ON                                                               
         TITLE 'SPSFM43 - PW BILL (SUBR01--LTORG && CONSTANTS)'                 
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
         SPACE 2                                                                
         TITLE 'SPSFM43 - PW BILL (SUBR01--MISC STUFF)'                         
*--------------------- SUBR01 MISCELLANEOUS STUFF --------------------*         
                                                                                
         GETEL R6,DATADISP,ELCODE                                               
                                                                                
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(4096-SUBR01L+1)                                              
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM43 - PW BILL (SUBR02)'                                     
***********************************************************************         
*======================== SUBROUTINE POOL TWO ========================*         
SUBR02Q  EQU   ((((*-T21743)/4096)+1)*4096)                                     
                                                                                
         ORG   T21743+SUBR02Q                                                   
SUBR02   NMOD1 0,**4302**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         L     R1,0(R1)                                                         
         SRL   R1,24               SHIFT TO LOW-ORDER BYTE                      
         SH    R1,=Y(R01#)          ADJUST FOR PREVIOUS ROUTINES,               
         BCTR  R1,0                 SUBTRACT ONE,                               
         SLL   R1,2                 AND MULTIPLY BY FOUR                        
         B     R02_00(R1)                                                       
                                                                                
CLS#     EQU   (R02_01-*)/4+R01#+1                                              
FMS#     EQU   (R02_02-*)/4+R01#+1                                              
S1D#     EQU   (R02_03-*)/4+R01#+1                                              
RFG#     EQU   (R02_04-*)/4+R01#+1                                              
DDC#     EQU   (R02_05-*)/4+R01#+1                                              
PDS#     EQU   (R02_06-*)/4+R01#+1                                              
PWBUY#   EQU   (R02_07-*)/4+R01#+1                                              
PWPW#    EQU   (R02_08-*)/4+R01#+1                                              
STI#     EQU   (R02_09-*)/4+R01#+1                                              
RTI#     EQU   (R02_10-*)/4+R01#+1                                              
DCD#     EQU   (R02_11-*)/4+R01#+1                                              
VWD#     EQU   (R02_12-*)/4+R01#+1                                              
SST#     EQU   (R02_13-*)/4+R01#+1                                              
RFB#     EQU   (R02_14-*)/4+R01#+1                                              
DLM#     EQU   (R02_15-*)/4+R01#+1                                              
UPT#     EQU   (R02_16-*)/4+R01#+1                                              
                                                                                
R02_00   DS    0H                                                               
R02_01   B     CLRSCR                                                           
R02_02   B     FMTSCR                                                           
R02_03   B     SET1DATA                                                         
R02_04   B     RESETFLG                                                         
R02_05   B     DODRCR                                                           
R02_06   B     PAIDSP                                                           
R02_07   B     PWCBUY                                                           
R02_08   B     PWCPW                                                            
R02_09   B     SAVETIA                                                          
R02_10   B     RSTRTIA                                                          
R02_11   B     DOCURD                                                           
R02_12   B     VWHOLDOL                                                         
R02_13   B     SETSTATB                                                         
R02_14   B     RFNLBIL                                                          
R02_15   B     DELEM                                                            
R02_16   B     UPWTAB                                                           
R02#     EQU   (*-R02_00)/4+R01#                                                
         DC    H'0'                                                             
                                                                                
YES_02   SR    RC,RC                                                            
NO_02    LTR   RC,RC                                                            
XIT_02   XIT1                                                                   
         TITLE 'SPSFM43 - PW BILL (SUBR02--CLS#)'                               
*---------------------------- CLEAR SCREEN ---------------------------*         
                                                                                
* Clears the months and total columns of screen                                 
                                                                                
CLRSCR   DS    0H                                                               
         LA    R2,PWBESDTH         R2-->"BEGINNING" OF SCREEN                   
         LA    R3,PWBPFLNH         R3-->"END" OF SCREEN                         
*                                                                               
CLS10    CR    R2,R3               AT END OF SCREEN?                            
         BNL   CLRSCRX              YES, FINISHED CLEARING SCREEN               
         ZIC   R0,0(R2)            BUMP PAST TITLE COLUMN                       
         AR    R2,R0               R2-->1ST MONTH COLUMN                        
         LA    RF,BSLLENQ(R2)      RF-->END OF ROW                              
*                                                                               
CLS20    CR    R2,RF               IF AT END OF ROW,                            
         BNL   CLS10                PROCESS NEXT LINE                           
         XC    8(L'BSLM1,R2),8(R2) ELSE, CLEAR DATA FIELD                       
         OI    6(R2),X'80'          AND TRANSMIT IT                             
         LA    R2,BSLMQ(R2)        BUMP TO NEXT MONTH COLUMN                    
         B     CLS20                                                            
*                                                                               
CLRSCRX  B     XIT_02                                                           
         TITLE 'SPSFM43 - PW BILL (SUBR02--FMS#)'                               
*---------------------------- FORMAT SCREEN --------------------------*         
                                                                                
* Formats the PW/Bill screen if key changed, PF9 hit for redisplay, or          
*  buy/PW status changed.                                                       
* Fields on PW% and Adj DR/CR$ lines are protected if PW status is on           
*  LOCK, there are no buys for that month, or the period is not long            
*  enough to fill the screen, otherwise, they are unprotected.                  
* Furthermore, if there is an unpaid spot in a month, its Adj DR/CR$            
*  is protected, unless the Adj DR/CR$ feature was previously                   
*  activated (DR/CR <> NULLS).                                                  
                                                                                
FMTSCR   DS    0H                                                               
         USING BSLDSECT,R2                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
** CURRENT PW% LINE **                                                          
*                                                                               
         LA    R2,PWBCPWH                                                       
         LA    RF,BSLTTLH          RF=A(TOTALS COLUMN)                          
         L     R4,AACCUTAB                                                      
         BAS   RE,FMSBUMP          R4-->FIRST MONTH ENTRY                       
*                                                                               
FMT10    TM    LOCKFLAG,LKFFPLKQ   IF PW LOCKED,                                
         BO    FMT12B               JUST PROTECT FIELD                          
         TM    ATFLAG,ATFSTTLQ     DID WE REACH SCHED ENTRY?                    
         BZ    FMT12A               NOPE                                        
         CR    R2,RF                YES, ARE WE AT TOTALS COLUMN?               
         BH    FMT20                 PASSED IT, DONE W/ PW% LINE                
         BE    FMT12A                AT IT, DO REGULAR CHECK                    
         BL    FMT12B                NOT YET, PROTECT FIELD                     
*                                                                               
FMT12A   TM    ATFLAG,ATFNOSPT     ANY BUYS FOR MONTH/SKED?                     
         BO    FMT12B               YES ==> PROTECT                             
         OC    ATACBUY,ATACBUY     ACTUAL WIM$ ZERO?                            
         BZ    FMT12B               YES ==> PROTECT                             
         TM    ATFLAG2,ATF2OOWP    USING OOW PW PAID$ ?                         
         BO    FMT12B               YES ==> PROTECT                             
                                                                                
         NI    1(R2),XFF-X20        UNPROTECT FIELD                             
         B     FMT12C                                                           
FMT12B   OI    1(R2),X20            PROTECT FIELD                               
FMT12C   OI    6(R2),X80                                                        
                                                                                
         LA    R2,BSLMQ(R2)                                                     
         CR    R2,RF                                                            
         BH    FMT20                                                            
         TM    ATFLAG,ATFSTTLQ     CAN WE STILL BUMP ACCUTAB POINTER?           
         BO    *+8                                                              
         BAS   RE,FMSBUMP           YEP!                                        
         B     FMT10                                                            
*                                                                               
** ADJUSTED DR/CR LINE **                                                       
*                                                                               
FMT20    LA    R2,PWBBILLH                                                      
         LA    RF,BSLTTLH          RF=END-OF-LOOP CONDITION                     
         L     R4,AACCUTAB                                                      
FMT22    BAS   RE,FMSBUMP          BUMP POINTER TO MONTH ENTRY                  
         SH    R4,=Y(ACCUTABQ)      MAKE IT POINT TO BILL ADJ ENTRY             
         TM    ATFLAG,ATFSTTLQ     AT END OF SCHED?                             
         BO    FMT26                                                            
         TM    ATFLAG,ATFNOSPT     ANY BUYS FOR THE MONTH?                      
         BNO   *+16                 YEP                                         
         TM    ATFLAG2,ATF2ESTB     NOPE, BUT IS MONTH ESTIM BILLED?            
         BNZ   FMT24                 YEP ==> UNPROTECT                          
         B     FMT24A                OTHERWISE, PROTECT                         
*&&DO                                                                           
         OC    ATDRCR,ATDRCR       DR/CR INPUTTED PREVIOUSLY?                   
         BNZ   FMT24                YES ==> UNPROTECT                           
*&&                                                                             
         TM    ATFLAG2,ATF2OOWP    USED OOWR PAID DOLLARS?                      
         BO    FMT24                YES ==> UNPROTECT                           
         TM    ATFLAG,ATFUNPDQ     ANY SPOTS UNPAID?                            
         BO    FMT24A               YES ==> PROTECT                             
*                                                                               
FMT24    DS    0H                                                               
         NI    1(R2),XFF-X20        UNPROTECT FIELD                             
         B     FMT24B                                                           
FMT24A   OI    1(R2),X20            PROTECT FIELD                               
FMT24B   OI    6(R2),X80                                                        
         LA    R4,ACCUTABQ(R4)     BUMP PAST BILL ADJ ENTRY                     
                                                                                
         LA    R2,BSLMQ(R2)                                                     
         CR    R2,RF                                                            
         BL    FMT22                                                            
         B     FMT30                                                            
*                                                                               
FMT26    OI    1(R2),X20           PROTECT THOSE COLS NOT USED BY SKED          
         OI    6(R2),X80                                                        
         LA    R2,BSLMQ(R2)                                                     
         CR    R2,RF               ARE WE DONE?                                 
         BL    FMT26                NO, NOT YET                                 
         B     FMT30                                                            
*                                                                               
** SET UP HEADINGS FOR COLUMNS **                                               
*                                                                               
FMT30    L     R4,AACCUTAB         SET UP COLUMN HEADINGS                       
         LA    R2,PWBM1H                                                        
FMT32    BAS   RE,FMSBUMP          BUMP POINTER TO A MONTH ENTRY                
         TM    ATFLAG,ATFSTTLQ                                                  
         BO    FMT36                                                            
                                                                                
         GOTO1 DATCON,DMCB,(2,ATMSTART),(0,MYDATE6),0                           
         GOTO1 AGETBROD,DMCB,(1,MYDATE6),STARTEND,AGETDAY,AADDAY                
         GOTO1 DATCON,DMCB,(0,STARTEND+6),(4,WORK),0                            
         OI    6(R2),X'80'                                                      
         MVC   8(3,R2),WORK                                                     
         TM    ESTFLAG,EFBILEST                                                 
         BZ    *+10                                                             
         MVC   8(3,R2),PERIOD                                                   
                                                                                
         TM    ATFLAG2,ATF2ESTB    TEST FOR ESTIM BILL FIRST                    
         BZ    FMT34                                                            
         MVI   8+3(R2),C'*'                                                     
         TM    ATFLAG2,ATF2FNLB    TEST FOR FINAL BILL                          
         BZ    FMT34                                                            
         MVI   8+3+1(R2),C'*'                                                   
*                                                                               
FMT34    LA    R2,BSLMQ(R2)                                                     
         B     FMT32                                                            
*                                                                               
FMT36    LA    R2,PWBM1H           TOTALS (LAST) COLUMN                         
         MVC   BSLTTL(L'SP@TOTAL),SP@TOTAL                                      
         OC    BSLTTL,SPACES                                                    
                                                                                
         TM    ATFLAG2,ATF2ESTB    TEST FOR ESTIM BILL FIRST                    
         BZ    *+8                                                              
         MVI   BSLTTL+L'SP@TOTAL,C'*'                                           
         TM    ATFLAG2,ATF2FNLB    TEST FOR FINAL BILL                          
         BZ    *+8                                                              
         MVI   BSLTTL+L'SP@TOTAL+1,C'*'                                         
                                                                                
         OI    BSLTTLH+6,X'80'                                                  
*                                                                               
** PROJECTED PW% & PROJECTED BILLING LINE **                                    
*                                                                               
         DS    0H                                                               
         LA    R2,PWBPJPWH         R2-->PROJECTED PW% LINE                      
         LA    RF,BSLTTLH          RF=END-OF-LOOP CONDITION                     
         LA    R3,PWBPJBLH         R3-->PROJECTED BILLING TOTAL LINE            
         L     R4,AACCUTAB                                                      
FMT42    BAS   RE,FMSBUMP          BUMP POINTER TO MONTH ENTRY                  
         SH    R4,=Y(ACCUTABQ)      BUMP BACK TO BILL ADJ ENTRY                 
         TM    ATFLAG,ATFSTTLQ     AT END OF SCHED?                             
         BO    FMT46                                                            
                                                                                
FMT42A   TM    ATFLAG,ATFNOSPT     ANY BUYS FOR THE MONTH?                      
         BO    FMT44A               NO ==> PROTECT                              
         OC    ATDRCR,ATDRCR       DR/CR INPUTTED PREVIOUSLY?                   
         BNZ   *+12                 YES ==> IGNORE PAY STATUS                   
         TM    ATFLAG,ATFUNPDQ     ANY SPOTS UNPAID?                            
         BO    FMT44A               YES ==> PROTECT                             
         OC    ATACBUY,ATACBUY     IS WIM$ CURRENT ZERO?                        
         BZ    FMT44A               YES ==> PROTECT                             
*                                                                               
         NI    1(R2),XFF-X20        UNPROTECT FIELDS                            
         NI    1(R3),XFF-X20                                                    
         B     FMT44B                                                           
FMT44A   OI    1(R2),X20            PROTECT FIELDS                              
         OI    1(R3),X20                                                        
FMT44B   OI    6(R2),X80           TRANSMIT FIELDS                              
         OI    6(R3),X80                                                        
         LA    R4,ACCUTABQ(R4)     BUMP PAST BILL ADJ ENTRY                     
                                                                                
         LA    R2,BSLMQ(R2)        BUMP TO NEXT COLUMN                          
         LA    R3,BSLMQ(R3)        BUMP TO NEXT COLUMN                          
         CR    R2,RF               RF-->TOTALS COL, CHECK LOOP CNDITION         
         BH    FMT49                                                            
         B     FMT42                                                            
*                                                                               
FMT46    DS    0H                  R4-->SKED TOTALS                             
         CR    R2,RF               WHAT ABOUT R2?                               
         BE    FMT42A               R2-->TOTALS COL, GO & FORMAT IT             
         BL    *+6                  R2 IS NOT @ TOTALS COL YET                  
         DC    H'0'                 R2 BETTER NOT PASS TOTALS COL               
         OI    1(R2),X20           PROTECT THOSE COLS NOT USED BY SKED          
         OI    6(R2),X80                                                        
         LA    R2,BSLMQ(R2)                                                     
         OI    1(R3),X20                                                        
         OI    6(R3),X80                                                        
         LA    R3,BSLMQ(R3)                                                     
         B     FMT46                                                            
*                                                                               
FMT49    DS    0H                                                               
         B     FMTX                                                             
*                                                                               
FMTX     B     XIT_02                                                           
         SPACE 2                                                                
FMSBUMP  DS    0H                  BUMP POINTER TO ACCUTAB                      
         LA    R4,ACCUTABQ(R4)                                                  
         TM    ATFLAG,ATFMTTLQ+ATFSTTLQ                                         
         BZ    FMSBUMP                                                          
         LA    R4,ACCUTABQ(R4)     BUMP PAST BILL ADJ LINE                      
         BR    RE                                                               
         SPACE 2                                                                
         DROP  R2,R4                                                            
         TITLE 'SPSFM43 - PW BILL (SUBR02--S1D#)'                               
*-------------------------- SET 1ST DATA FIELD -----------------------*         
                                                                                
* Looks through the data fields of the PW/Bill screen for the first             
*  unprotected data field and saves the address in AM1STREC.  If all            
*  data fields are protected, then AM1STREC will be set to AFRSTKEY.            
                                                                                
SET1DATA DS    0H                                                               
         USING BSLDSECT,R2                                                      
*                                                                               
** CURRENT PW% LINE **                                                          
*                                                                               
         LA    R2,PWBCPWH                                                       
         LA    R4,BSLMQ                                                         
         LA    R5,BSLLENQ-1(R2)    R3=END OF LOOP CONDITION                     
*                                                                               
S1D10    DS    0H                                                               
         TM    1(R2),X20           TEST PROTECTION                              
         BZ    S1D30                FOUND IT!                                   
         BXLE  R2,R4,S1D10                                                      
*                                                                               
** CLT$ ADJUSTED DR/CR LINE **                                                  
*                                                                               
         LA    R2,PWBBILLH                                                      
         LA    R4,BSLMQ                                                         
         LA    R5,BSLLENQ-1(R2)    R3=END OF LOOP CONDITION                     
*                                                                               
S1D20    DS    0H                                                               
         TM    1(R2),X20           TEST PROTECTION                              
         BZ    S1D30                FOUND IT!                                   
         BXLE  R2,R4,S1D20                                                      
                                                                                
         L     R2,AFRSTKEY         ALL DATA FIELDS ARE PROTECTED                
S1D30    DS    0H                                                               
         ST    R2,AM1STREC         SET AM1STREC                                 
                                                                                
         B     XIT_02                                                           
         DROP  R2                                                               
         TITLE 'SPSFM43 - PW BILL (SUBR02--RFG#)'                               
*------------------------- RESET FLAGS IN TABLE ----------------------*         
RESETFLG DS    0H                                                               
         L     R4,AACCUTAB         RESET FLAGS IN ACCUTAB                       
         USING ACCUTABD,R4                                                      
RFG10    CLI   0(R4),XFF                                                        
         BE    RFGX                                                             
         NI    ATFLAG,ATFOK1Q      LEAVE THESE BITS ALONE                       
         NI    ATFLAG2,ATF2OK1Q                                                 
         LA    R4,ACCUTABQ(R4)                                                  
         B     RFG10                                                            
         DROP  R4                                                               
*                                                                               
RFGX     B     XIT_02                                                           
         SPACE 2                                                                
ATFOK1Q  EQU   ATFMTTLQ+ATFSTTLQ+ATFBILAJ+ATFUNPDQ+ATFNOSPT                     
ATF2OK1Q EQU   ATF2BILL+ATF2COVD                                                
         TITLE 'SPSFM43 - PW BILL (SUBR02--DDC#)'                               
*------------------------------- DR/CR $ -----------------------------*         
DODRCR   DS    0H                                                               
                                                                                
* Updates weekly dollar elements (PWDOLEL) for DR/CR$.  We only                 
*  want the bill amount and date to be in 1st existing week for                 
*  each month (if such PWDOLEL element exists)                                  
* At entry,                                                                     
*   R6-->PW record (mkt- or station-level)                                      
                                                                                
         ST    R6,FULL             HOLD ONTO A(RECORD) IN FULL                  
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
         MVI   ELCDLO,PWDOLCDQ                                                  
         MVI   ELCDHI,PWDOLCDQ                                                  
         MVC   DATADISP,=Y(PWEL-PWRECD)                                         
                                                                                
DDC10    DS    0H                                                               
         L     R6,FULL             POINT R6 TO START OF RECORD                  
         BAS   RE,GTEL             FIND ELEM W/ 1ST WEEK OF MONTH               
         B     DDC10B                                                           
DDC10A   BAS   RE,NXTEL                                                         
DDC10B   BNE   DDC30                CAN'T FIND IT, SO ADD IT                    
                                                                                
         USING PWDOLEL,R6                                                       
DDC20    CLC   PWDOLWK,ATWSTART    MATCH TO WEEK IN ACCUTAB                     
         BNE   DDC10A               NOPE, TRY NEXT ELEMENT                      
DDC22    LA    R4,ACCUTABQ(R4)      YES, FAST FOWARD TO BILL ADJ LINE           
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   DDC22                                                            
         MVC   PWDOLBIL,ATDRCR     MOVE IN $ AMOUNT                             
         MVC   PWDOLBLD,ATBILD      AND BILL DATE                               
         B     DDC40               GET NEXT ACCUTAB ENTRY W/ DR/CR              
*                                                                               
DDC30    DS    0H                  ADD NEW ELEMENT TO RECORD                    
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   PWDOLCD,PWDOLCDQ     ELEM CODE                                   
         MVI   PWDOLLEN,PWDOLLNQ    ELEM LENGTH                                 
         MVC   PWDOLWK,ATWSTART     DATE OF 1ST WEEK OF MONTH                   
                                                                                
DDC32    LA    R4,ACCUTABQ(R4)     FAST FOWARD TO BILL ADJ ENTRY                
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   DDC32                                                            
                                                                                
         MVC   PWDOLBIL,ATDRCR      $ AMOUNT                                    
         MVC   PWDOLBLD,ATBILD      BILL DATE                                   
         L     R0,FULL                                                          
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R0),(R6),0                             
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DDC40    DS    0H                                                               
         LA    R4,2*ACCUTABQ(R4)   BUMP 1ST WEEK OF NEXT MONTH                  
         TM    ATFLAG,ATFSTTLQ      R4-->1ST WEEK OF NEXT MONTH                 
         BZ    DDC10                 UNLESS IT'S A SKED ENTRY                   
                                                                                
         B     XIT_02                                                           
         DROP  R4,R6                                                            
         TITLE 'SPSFM43 - PW BILL (SUBR02--PDS#)'                               
*------------------------------ PAID SPOTS ---------------------------*         
                                                                                
* Turns on unpaid (ATFUNPDQ) flags in ACCUTAB                                   
* At entry, R6-->buy record                                                     
                                                                                
PAIDSP   DS    0H                                                               
                                                                                
         LR    R3,R6                                                            
         USING BUYREC,R3                                                        
*                                                                               
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0B'                                                     
         LA    R6,(BDELEM-BUYREC)(R6)                                           
*                                                                               
PDS20    BAS   RE,NXTEL                                                         
         BNE   PDSX                                                             
*                                                                               
         USING REGELEM,R6                                                       
         MVC   DATE2,RDATE                                                      
         L     RF,ALOCDATE                                                      
         BASR  RE,RF                                                            
                                                                                
         OR    R4,R4               A(ENTRY) PASSED BACK?                        
         BNZ   PDS22                YES                                         
         GOTO1 DATCON,DMCB,(X'82',DATE2),(5,MYTEXT+1),0                         
         MVC   MYTEXT(1),DMCB+4                                                 
         MVI   MYERRCD,SPDATQ      SET ERROR MSG CODE                           
         B     PDSX                                                             
                                                                                
*                                                                               
PDS22    DS    0H                                                               
         USING ACCUTABD,R4         R4-->WEEK CONTAINING DATE2                   
                                                                                
         TM    RSTATUS,X40         TEST IF SPOT HAS BEEN MINUSED                
         BO    PDS30                                                            
                                                                                
*                                                                               
         DS    0H                  SPOT HAS NOT BEEN MINUSED                    
         OC    RPAY,RPAY           CHECK FOR SPOTS PAID                         
         BNZ   PDS50                                                            
                                                                                
         DS    0H                  NO PAID DATE--CHECK FOR DEFERMENT            
         CLI   SPOTPROF+8,0         CHECK OUT-OF-WEEK ROTATOR                   
         BE    PDS24                 NONE==>NO DEFERMENTS WAS ALLOWED           
         ZIC   RF,RLEN                                                          
         AR    RF,R6                                                            
         CLI   0(RF),X'10'          TEST SPOT MATCHED                           
         BE    PDS24                 MATCHED==>NO DEFERMENTS DONE               
                                                                                
         GOTO1 DATCON,DMCB,(2,ATMEND),(0,WORK)                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 (RF),(R1),(0,WORK),(2,SDATE2)  START OF CALNDR MONTH             
         GOTO1 ADDAY,(R1),WORK,WORK+6,31                                        
         MVC   WORK+6+4(2),=C'01'                                               
         GOTO1 (RF),(R1),WORK+6,WORK,F'-1'                                      
         GOTO1 DATCON,(R1),(0,WORK),(2,EDATE2)  END OF CALNDR MONTH             
                                                                                
         CLC   RDATE,EDATE2         TEST SPOT AFTER MONTH                       
         BH    PDS24X                YES, IGNORE (AS IN PAY PROGRAM)            
         GOTO1 DATCON,DMCB,(2,RDATE),WORK                                       
         SR    R0,R0                                                            
         IC    R0,BDSEDAY                                                       
         SRDL  R0,4                 R0 = START DAY OF ROTATION                  
         SRL   R1,28                R1 =  END   "  "     "                      
         CR    R0,R1                SHOULD INDICATE OUT-OF-WEEK ROT             
         BNH   PDS24                                                            
         LA    R1,7(R1)                                                         
         SR    R1,R0                                                            
         LR    R0,R1                R0 = # OF DAYS DIFFERENCE                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,HALF)                                      
                                                                                
         DS    0H                   HALF = LATEST DATE SPOT COULD RUN           
         CLC   HALF,SDATE2          TEST BEFORE START                           
         BL    PDS24X                                                           
         CLC   HALF,EDATE2           OR AFTER END                               
         BH    PDS24X                                                           
         B     PDS24                                                            
                                                                                
PDS24    DS    0H                                                               
         OI    ATFLAG,ATFUNPDQ      NO PAID DATE==>SPOT IS UNPAID               
PDS24X   EQU   *                                                                
                                                                                
         B     PDS50                                                            
                                                                                
*                                                                               
PDS30    DS    0H                  SPOT HAS BEEN MINUSED                        
         OC    RPAY,RPAY           IS MINUSED SPOT PAID?                        
         BZ    PDS50                NOPE, EVERYTHING OKAY                       
                                                                                
         DS    0H                  MINUSED SPOT WAS PAID                        
         LR    R5,R6               SAVE A(CURRENT POOL ORIG BUY ELEM)           
         ZIC   R0,RLEN                                                          
         AR    R6,R0                                                            
         CLI   RCODE,X'0C'         LOOK AT POOL OTO ELEM                        
         BNE   PDS35                                                            
         TM    RSTATUS,X80          IT SHOULD BE THE MINUS SPOT                 
         BZ    PDS35                                                            
         OC    RPAY,RPAY           SEE IF PAID YET                              
         BNZ   PDS50                YES, EVERYTHING OKAY                        
         OI    ATFLAG,ATFUNPDQ      NO, SPOT IS UNPAID                          
                                                                                
PDS35    DS    0H                                                               
         LR    R6,R5               RESTORE A(POOL ORIG) INTO R6                 
         B     PDS50                                                            
*                                                                               
PDS50    DS    0H                                                               
         B     PDS20                                                            
         DROP  R4,R6                                                            
*                                                                               
PDSX     B     XIT_02                                                           
         DROP  R3                                                               
         TITLE 'SPSFM43 - PW BILL (SUBR02--PWBUY# && PWPW#)'                    
*        At entry, TEMPAJB, TEMPACB, & TEMPPW                                   
*         has corresponding values into respective routines                     
                                                                                
*---------------------------- ADJUSTED BUYS --------------------------*         
*                                                                               
PWCBUY   DS    0H                                                               
         L     R2,AIO3             USE 3RD I/O FOR PWBLOCK                      
         XC    0(PWBLKL,R2),0(R2)                                               
         USING PWBLKD,R2                                                        
         MVI   PWACT,PWGETBUY                                                   
         MVC   PWACTBUY,TEMPACB                                                 
         MVC   PWPCT,TEMPPW                                                     
         CLI   USETAX,C'Y'                                                      
         BNE   *+10                                                             
         MVC   PWTAX,TEMPTAX                                                    
                                                                                
         GOTO1 APWCALC,DMCB,(R2)                                                
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TEMPAJB,PWVAL                                                    
         B     XIT_02                                                           
                                                                                
*--------------------------- PW PERCENTAGE ---------------------------*         
*                                                                               
PWCPW    DS    0H                                                               
         MVC   TEMPPW,OPWPCT       SET DEFAULT VALUE                            
         OC    TEMPAJB,TEMPAJB     IF NO ADJUSTED GOALS,                        
         BZ    PWCPWX               THEN EXIT                                   
                                                                                
         L     R2,AIO3             USE 3RD I/O FOR PWBLOCK                      
         XC    0(PWBLKL,R2),0(R2)                                               
         MVI   PWACT,PWGETPW                                                    
*                                                                               
         MVC   PWACTBUY,TEMPACB                                                 
         MVC   PWADJBUY,TEMPAJB                                                 
         MVC   PWTAX,TEMPTAX                                                    
         CLI   USETAX,C'Y'                                                      
         BE    *+10                                                             
         XC    PWTAX,PWTAX                                                      
                                                                                
         GOTO1 APWCALC,DMCB,(R2)                                                
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TEMPPW,PWVAL                                                     
*                                                                               
PWCPWX   B     XIT_02                                                           
         DROP  R2                                                               
         TITLE 'SPSFM43 - PW BILL (SUBR02--STI# && RTI#)'                       
*--------------------------- SAVE TIA TABLES -------------------------*         
                                                                                
* save TIA tables into TEMPSTR                                                  
                                                                                
SAVETIA  DS    0H                                                               
         MVI   DMCB+8,PAGEQ                                                     
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ATIA,0,0                     
*                                                                               
         DS    0H                  SAVE AWAY STTN ACCUM TABLE                   
         TM    TSARBLK+(TSINDS-TSARD),TSIINIOK                                  
         BZ    STI015X                                                          
                                                                                
         MVI   GOSUBN,TSR_SAV#                                                  
         GOTO1 AGOSUB                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
STI015X  EQU   *                                                                
*                                                                               
         B     XIT_02                                                           
         SPACE 2                                                                
*-------------------------- RESTORE TIA TABLES -----------------------*         
                                                                                
* restore TIA tables from TEMPSTR                                               
                                                                                
RSTRTIA  DS    0H                                                               
         MVI   DMCB+8,PAGEQ        3RD PARAMETER                                
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='   6TH PARAMETER                                
         MVC   DMCB+22(2),=H'18432'                                             
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,ATIA,0                      
*                                                                               
         DS    0H                  RESTORE STTN ACCUM TABLE                     
         TM    TSARBLK+(TSINDS-TSARD),TSIINIOK                                  
         BZ    RTI015X                                                          
                                                                                
         MVI   GOSUBN,TSR_RES#                                                  
         GOTO1 AGOSUB                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
RTI015X  EQU   *                                                                
*                                                                               
         B     XIT_02                                                           
         TITLE 'SPSFM43 - PW BILL (SUBR02--DCD#)'                               
*------------------------- DO CURRENT DOLLARS ------------------------*         
                                                                                
* Adds/updates PWCUREL elements for those months whose Adj DR/CR $              
*  values were modified.  These are the elements billing reads to               
*  report current dollar amounts.  PWDOLSPT for each week in those              
*  months gets updated to the current number of spots, for both mkt-            
*  and station-level PW records (per Grant).                                    
* At entry,                                                                     
*   R6-->PW record                                                              
                                                                                
DOCURD   DS    0H                                                               
         LR    R3,R6               HOLD ONTO A(PW RECORD) IN R3                 
         USING PWRECD,R3                                                        
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
         MVC   DATADISP,=Y(PWEL-PWRECD)                                         
                                                                                
DCD10    DS    0H                                                               
         TM    ATFLAG,ATFSTTLQ     IF SKED ENTRY REACHED,                       
         BO    DCDX                 GO EXIT                                     
                                                                                
         LR    R5,R4               HOLD ONTO A(1ST WEEK OF MONTH)               
DCD12    LA    R4,ACCUTABQ(R4)                                                  
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   DCD12                                                            
         TM    ATFLAG,ATFDRCRQ     WAS ADJ DR/CR MODIFIED?                      
         BO    DCD20                YES, UPDATE CURRENT $'s                     
         TM    ATFLAG2,ATF2CHDC    WAS ADJ DR/CR VALUE CHANGED?                 
         BO    DCD20                YES, UPDATE CURRENT $'s                     
         B     DCDBUMP             ELSE, DON'T UPDATE CURRENT $'s               
*                                                                               
DCD20    DS    0H                                                               
                                                                                
         DS    0H                  ADD PWCUREL'S FOR THIS MONTH                 
         XR    R4,R5               SWAP R4 AND R5                               
         XR    R5,R4                                                            
         XR    R4,R5               R4-->1ST WEEK, R5-->BILL ADJ ENTRY           
         MVI   BYTE,0              FLAG (0=1ST WEEK, 1=NOT 1ST WEEK)            
                                                                                
DCD32    DS    0H                                                               
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ   DID ALL WEEKS IN MONTH YET?           
         BO    DCD40                       YEP, NO MORE PWCUREL'S               
                                                                                
         OC    PWKSTA,PWKSTA              IF STATION-LEVEL RECD,                
         BNZ   DCD34                       DON'T PROCESS PWCUREL                
                                                                                
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWCURCDQ',(R3)),             +        
               (L'ATWSTART,ATWSTART),0                                          
         L     R6,12(R1)                                                        
         CLI   12(R1),0                                                         
         BE    DCD33                                                            
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         CLI   12(R1),6                                                         
         BE    DCD33                                                            
         DC    H'0'                                                             
                                                                                
         USING PWCUREL,R6                                                       
DCD33    DS    0H                                                               
         MVI   PWCURCD,PWCURCDQ                                                 
         MVI   PWCURLEN,PWCURLNQ                                                
         MVC   PWCURWK,ATWSTART                                                 
         MVC   PWCURSPT,ATNSPT                                                  
         MVC   PWCURWG,ATACBUY                                                  
         MVC   PWCURWN,ATWNET                                                   
         MVC   PWCURCG,ATAJBUY                                                  
         MVC   PWCURCN,ATCNET                                                   
         MVC   PWCURTAX,ATTAX                                                   
         MVC   PWCURCTX,ATCLTAX                                                 
         CLI   BYTE,0                                                           
         BNE   DCD33A                                                           
         MVC   PWCURBIL,(ATDRCR-ACCUTABD)(R5)                                   
         MVC   PWCURBLD,(ATBILD-ACCUTABD)(R5)                                   
         MVI   BYTE,1              CHANGE FLAG TO NOT 1ST WK FOR NEXT           
         DROP  R6                                                               
                                                                                
DCD33A   DS    0H                                                               
         CLI   12(R1),0            DO WE NEED TO ADD ELEM TO RECD               
         BE    DCD34                NOPE                                        
                                                                                
         DS    0H                   YEP                                         
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R3),(R6),0                             
         CLI   12(R1),0                                                         
         BE    DCD34                                                            
         DC    H'0'                                                             
                                                                                
DCD34    DS    0H                  UPDATE PWDOLSPT (AS PER GRANT)               
         MVC   DUB(4),ATNSPT       THIS IS IF RECD IS MKT-LEVEL                 
         OC    PWKSTA,PWKSTA       IF MKT-LEVEL PW RECORD,                      
         BZ    DCD36                UPDATE PWDOLSPT NOW                         
                                                                                
         DS    0H                  LOCATE STATION ENTRY FOR WEEK                
         XC    DUB(4),DUB          GET # OF SPOTS FOR WEEK                      
                                                                                
         XC    SVTSRNUM,SVTSRNUM   SAVE TSAR RECD#                              
         ZICM  RF,TSARBLK+(TSRNUM-TSARD),(3)                                    
         BZ    *+16                                                             
         CLM   RF,3,TSARBLK+(TSPRECN-TSARD)                                     
         BH    *+8                                                              
         STCM  RF,3,SVTSRNUM                                                    
                                                                                
         L     RF,TSARBLK+(TSAREC-TSARD)                                        
         LA    RF,0(RF)                                                         
         USING STACRECD,RF                                                      
         XC    STACRECD(STACRECL),STACRECD                                      
         DROP  RF                                                               
         MVI   GOSUBN,TSR_RDH#                                                  
                                                                                
DCD34A   DS    0H                                                               
         GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF     IF END OF STTN ACCUM TABLE,                  
         BNZ   DCD34G               NO SPOTS FOR THIS WEEK                      
                                                                                
         L     RF,TSARBLK+(TSAREC-TSARD)                                        
         LA    RF,0(RF)                                                         
         USING STACRECD,RF                                                      
         CLC   STACSTA,PWKSTA       MATCH ON STATION                            
         BNE   DCD34B                                                           
         CLC   STACSTRT,ATWSTART    MATCH ON START DATE OF WEEK                 
         BNE   DCD34B                                                           
         MVC   DUB(4),STACSPT      GET # OF SPOTS @ STATION LEVEL               
         B     DCD34G                                                           
                                                                                
DCD34B   DS    0H                                                               
         MVI   GOSUBN,TSR_NXT#                                                  
         B     DCD34A                                                           
         DROP  RF                                                               
DCD34G   EQU   *                                                                
                                                                                
         OC    SVTSRNUM,SVTSRNUM   NEED TO RESTORE ORIG TSAR RECD?              
         BZ    DCD34M               NOPE                                        
         MVC   TSARBLK+(TSRNUM-TSARD)(L'TSRNUM),SVTSRNUM                        
         MVI   GOSUBN,TSR_GET#                                                  
         GOTO1 AGOSUB                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DCD34M   EQU   *                                                                
         B     DCD36                                                            
                                                                                
DCD36    DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWDOLCDQ',(R3)),             +        
               (L'ATWSTART,ATWSTART),0                                          
         L     R6,12(R1)                                                        
         CLI   12(R1),0                                                         
         BE    DCD36A                                                           
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         CLI   12(R1),6                                                         
         BE    DCD36A                                                           
         DC    H'0'                                                             
                                                                                
DCD36A   DS    0H                                                               
         USING PWDOLEL,R6                                                       
         MVI   PWDOLCD,PWDOLCDQ                                                 
         MVI   PWDOLLEN,PWDOLLNQ                                                
         MVC   PWDOLWK,ATWSTART                                                 
         MVC   PWDOLSPT,DUB        UPDATE # OF SPOTS                            
         DROP  R6                                                               
                                                                                
         CLI   12(R1),0            DO WE NEED TO ADD ELEM TO RECD               
         BE    DCD38                NOPE                                        
                                                                                
         DS    0H                   YEP                                         
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R3),(R6),0                             
         CLI   12(R1),0                                                         
         BE    DCD38                                                            
         DC    H'0'                                                             
                                                                                
DCD38    DS    0H                                                               
         LA    R4,ACCUTABQ(R4)                                                  
         B     DCD32                                                            
*                                                                               
DCD40    DS    0H                  PROCESS NEXT MONTH'S STUFF                   
         B     DCDBUMP                                                          
                                                                                
                                                                                
DCDBUMP  DS    0H                  BUMP TO NEXT MONTH                           
         LA    R4,2*ACCUTABQ(R4)   R4 MUST BE @ BILL ADJ ENTRY                  
         B     DCD10                                                            
                                                                                
                                                                                
DCDX     DS    0H                                                               
         B     XIT_02                                                           
         DROP  R3,R4                                                            
         TITLE 'SPSFM43 - PW BILL (SUBR02--VWD#)'                               
*----------------------- VALIDATE WHOLE DOLLARS ----------------------*         
                                                                                
* Routine validates inputs which need to be whole dollars, i.e. no              
*  decimal points.                                                              
* At entry,                                                                     
*   R2-->field hdr of field need to be validated,                               
*   BYTE=C'-'  if negative input is allowed,                                    
*       =X'00' if negative input not allowed.                                   
* At exit,                                                                      
*   CC set to equal if input is valid,                                          
*   CC set to not-equal if input is invalid, and                                
                                                                                
VWHOLDOL DS    0H                                                               
         ZICM  R1,5(R2),(1)        R1=LENGTH OF INPUT                           
         BZ    VWDXN                                                            
         BCTR  R1,0                 LESS 1 FOR EX INSTRUCTIONS                  
         LA    RE,8(R2)            RE-->INPUT TO VALIDATE                       
                                                                                
         CLI   BYTE,C'-'           ALLOW FOR NEGATIVE INPUT?                    
         BNE   VWD10                NO                                          
         CLI   8(R2),C'-'           YES, AND IT MUST BE IN THE 1ST CHAR         
         BNE   VWD10                                                            
         LA    RE,1(RE)            RE-->INPUT                                   
         BCTR  R1,0                R1 = EX LENGTH                               
*                                                                               
VWD10    DS    0H                  VALIDATE INPUT                               
         EXMVC R1,WORK,0(RE)                                                    
                                                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         NC    WORK(0),ZEROES      REMOVE LOWER NIBBLES                         
                                                                                
         EXCLC R1,WORK,ZEROES      ARE THEY STILL EQUAL?                        
         BNE   VWDXN                                                            
                                                                                
                                                                                
VWDXY    DS    0H                                                               
         B     YES_02                                                           
*                                                                               
VWDXN    DS    0H                                                               
         B     NO_02                                                            
         TITLE 'SPSFM43 - PW MAINTENANCE (SUBR02--SST#)'                        
*----------------- STATION ACCUMULATOR TABLE ROUTINES ----------------*         
                                                                                
* AT ENTRY,                                                                     
*   FULL(3) =  station (MSPACKED)                                               
*   R3    -->  SCHUNK                                                           
*   R4    -->  corresponding week entry in ACCUTAB                              
                                                                                
SETSTATB DS    0H                                                               
                                                                                
         USING SCHUNKD,R3                                                       
                                                                                
         LA    R5,BRDWKTAB         FIRST MAKE SURE DATE IS W/IN SCHED           
         TM    ESTFLAG,EFBILEST    IF ONLY ONE PW PERIOD,                       
         BZ    SST05                                                            
         LA    R5,BRDMTHTB          USE MONTH TABLE                             
*                                                                               
SST05    CLI   0(R5),XFF                                                        
         BE    SST05DIE                                                         
         CLC   0(2,R5),SCDATE       STRT DATE CAN'T BE HIGHER THAN IT           
         BH    SST05DIE                                                         
         CLC   2(2,R5),SCDATE                                                   
         BNL   SST05X                                                           
         LA    R5,BCSTTABQ(R5)                                                  
         B     SST05                                                            
SST05DIE DC    H'0'                 NOT W/IN SCHED, DIE                         
SST05X   MVC   DUB(4),0(R5)        DUB(4)=START/END OF WEEK                     
                                                                                
         DS    0H                  LOOK FOR STTN/WEEK IN STTN ACCUM TBL         
         L     R5,TSARBLK+(TSAREC-TSARD)                                        
         LA    R5,0(R5)             POINT TO STTN ACCUM RECD AREA               
         USING STACRECD,R5                                                      
         XC    STACRECD(STACRECL),STACRECD                                      
         MVC   STACSTA,FULL                                                     
         MVI   GOSUBN,TSR_RDH#                                                  
                                                                                
SST10    DS    0H                                                               
         GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF     EOF IN STTN ACCUM TABLE?                     
         BNZ   SST20                NO, NOT YET                                 
         CLC   STACSTA,FULL                                                     
         BNE   SST10A                                                           
         CLC   STACSTRT(4),DUB     FIND WEEK                                    
         BE    SST30                                                            
SST10A   MVI   GOSUBN,TSR_NXT#                                                  
         B     SST10                                                            
*                                                                               
SST20    DS    0H                  ADD ENTRY TO TABLE                           
         XC    STACRECD(STACRECL),STACRECD                                      
         MVC   STACSTA,FULL         MOVE IN STATION                             
         MVC   STACSTRT(4),DUB      MOVE IN START/END DATES OF WEEK             
*                                                                               
SST30    DS    0H                  UPDATE TABLE                                 
         L     R1,STACSPT                                                       
         A     R1,SCSPOTS           # OF SPOTS                                  
         ST    R1,STACSPT                                                       
                                                                                
         L     R1,STACGRS                                                       
         A     R1,SCGROSS           GROSS $                                     
         ST    R1,STACGRS                                                       
                                                                                
         L     R1,STACNET                                                       
         A     R1,SCNET             NET $                                       
         ST    R1,STACNET                                                       
                                                                                
         L     R1,STACTAX                                                       
         A     R1,SCTAX             TAX $                                       
         ST    R1,STACTAX                                                       
                                                                                
         L     R1,STACCTX                                                       
         A     R1,SCPWCLTX          CLIENT TAX $                                
         ST    R1,STACCTX                                                       
                                                                                
         L     R1,STACCGRS         UPDATE GROSS CLCOST$ (ADJ BUY)               
         A     R1,SCPWGRS                                                       
         ST    R1,STACCGRS          AND UPDATE STACTB                           
                                                                                
         L     R1,STACCNET         UPDATE NET   CLCOST$ (ADJ BUY)               
         A     R1,SCPWNET                                                       
         ST    R1,STACCNET          AND UPDATE STACTB                           
*                                                                               
         DS    0H                                                               
         MVI   GOSUBN,TSR_PUT#     PUT STTN ACCUM RECD BACK TO TABLE            
         TM    MYTSERRS,TSEEOF                                                  
         BZ    *+8                                                              
         MVI   GOSUBN,TSR_ADD#      OR ADD IT IF NOT IN TABLE YET               
         GOTO1 AGOSUB                                                           
         BE    SST039                                                           
         MVI   MYWRNCD,WECSTQ      NO MORE ROOM IN STTN ACCUM TABLE?            
         TM    MYTSERRS,TSEEOF                                                  
         BNZ   SST039                                                           
         DC    H'0'                                                             
SST039   EQU   *                                                                
                                                                                
         B     XIT_02                                                           
         DROP  R3,R5                                                            
         TITLE 'SPSFM43 - PW BILL (SUBR02--RFB#)'                               
*-------------------------- RE-FINAL BILLING -------------------------*         
                                                                                
* Routine modifies PW records in case final billing needs to be                 
*  re-generated.                                                                
* Routine is modified to execute the "re-final billing" process for             
*  estimate billed months as well.                                              
                                                                                
RFNLBIL  DS    0H                                                               
         NI    MISCFLG2,XFF-MF2RFBQ  ASSUME NO RE-FINAL BILLING NEEDED          
         MVC   AIO,AIO2              USE 2ND I/O AREA IN THIS ROUTINE           
         MVI   GOSUBN,BPK#           BUILD KEY OF MKT PW RECORD                 
         GOTO1 AGOSUB                                                           
                                                                                
         GOTO1 HIGH                                                             
*                                                                               
** Go through ACCUTAB for each PW recd **                                       
*                                                                               
RFB10    DS    0H                                                               
         CLC   KEY(PKYMKTL),KEYSAVE   SAME MED/CLT/PRD/EST/MKT?                 
         BNE   RFBX                    NOPE, EXIT NOW                           
                                                                                
         GOTO1 GETREC                  YEP, GET THE PW RECORD                   
         L     R6,AIO                                                           
         USING PWRECD,R6                                                        
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
*** See if month will need re-final billing ***                                 
*                                                                               
RFB20    DS    0H                  R4-->1ST WEEK ENTRY                          
         LR    R5,R4                AND HAVE R5 HOLD ON TO IT                   
         TM    ATFLAG,ATFSTTLQ     IF SKED ENTRY                                
         BO    RFB80                THEN TIME TO PUT RECD BACK                  
RFB20A   TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BO    RFB22                                                            
         LA    R4,ACCUTABQ(R4)                                                  
         B     RFB20A              GET R4 TO BILL ADJ ENTRY                     
                                                                                
RFB22    DS    0H                                                               
         OC    PWKSTA,PWKSTA       IF STATION-LEVEL PW RECD,                    
         BZ    *+16                                                             
         TM    ATFLAG2,ATF2RFB      WE KNOW IF MNTH IS TO BE RE-FNL BIL         
         BO    RFB50                 MONTH NEEDS RE-FINAL BILLING               
         B     RFB70                 ELSE, BUMP TO NEXT MONTH                   
*                                                                               
         DS    0H                  PW RECD IS MKT-LEVEL                         
         TM    ATFLAG2,ATF2BILL    IS MONTH BILLED AT ALL?                      
         BZ    RFB70                NOPE, DON'T WORRY ABOUT THIS MONTH          
         TM    ATFLAG2,ATF2OOWP    USED OOWR PAID DOLLARS?                      
         BO    *+12                 YEP, DON'T CHECK PAY STATUS                 
         TM    ATFLAG,ATFUNPDQ     AN UNPAID SPOT IN MONTH?                     
         BO    RFB50                YES, CHANGE RECD FOR RE-FINAL BILL          
*                                                                               
         DS    0H                  GET WIMCOST AND # SPOTS FROM PWCUREL         
         XC    TEMPACB,TEMPACB                                                  
         XC    TEMPNSPT,TEMPNSPT                                                
         LR    R4,R5               POINT R4 TO 1ST WEEK OF MONTH AGAIN          
RFB32    DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWCURCDQ',(R6)),             +        
               (L'ATWSTART,ATWSTART),0                                          
         CLI   DMCB+12,0                                                        
         BNE   RFB34                                                            
                                                                                
         L     RF,DMCB+12                                                       
         USING PWCUREL,RF                                                       
         ICM   R1,15,PWCURWG                                                    
         A     R1,TEMPACB                                                       
         ST    R1,TEMPACB          SUM UP WIMCOST FROM FILE                     
         ICM   R1,15,PWCURSPT                                                   
         A     R1,TEMPNSPT                                                      
         ST    R1,TEMPNSPT         SUM UP # SPOTS FROM FILE                     
         DROP  RF                                                               
                                                                                
RFB34    DS    0H                                                               
         LA    R4,ACCUTABQ(R4)                                                  
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   RFB32                                                            
                                                                                
         CLC   ATNSPT,TEMPNSPT     DID # OF SPOTS FOR MONTH CHANGE?             
         BNE   RFB50                YEP, CHANGE RECD FOR RE-FNL BILL            
         CLC   ATACBUY,TEMPACB     DID WIMCOST FOR MONTH CHANGE?                
         BNE   RFB50                YEP, CHANGE RECD FOR RE-FNL BILL            
         B     RFB70               ELSE, BUMP TO NEXT MONTH                     
*                                                                               
*** Change PW recd for re-final billing for month ***                           
*                                                                               
RFB50    DS    0H                                                               
         XR    R4,R5               SWAP R4 & R5                                 
         XR    R5,R4                                                            
         XR    R4,R5               R4-->1ST WEEK, R5-->BILL ADJ ENTRY           
         MVI   HALF,0              FLAG: 0=1ST WEEK, 1=NOT 1ST WEEK             
         MVI   HALF+1,0            FLAG: 0=NO CHANGE, 1=RECD CHANGED            
                                                                                
RFB52    DS    0H                                                               
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BO    RFB59                                                            
         CLI   HALF,0              1ST WEEK?                                    
         BNE   RFB55                NO, JUST DELETE PWCUREL ELEM                
                                                                                
         DS    0H                   YES, ZERO OUT PWDOLBIL & PWDOLSPT           
         MVI   HALF,1                AND CHANGE FLAG FOR NOT 1ST WEEK           
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWDOLCDQ',(R6)),             +        
               (L'ATWSTART,ATWSTART),0                                          
         CLI   DMCB+12,0                                                        
         BNE   RFB58               CAN'T NULL OUT PWDOLBIL & PWDOLSPT           
                                                                                
         L     RF,DMCB+12                                                       
         USING PWDOLEL,RF                                                       
         OC    PWDOLBIL,PWDOLBIL   IF ADJ DR/CR AMT ALREADY NULLS,              
         BZ    RFB58                THEN LEAVE ELEMENT ALONE                    
         XC    PWDOLSPT,PWDOLSPT   NULL OUT # OF SPOTS                          
         XC    PWDOLBIL,PWDOLBIL   NULL OUT ADJ DR/CR AMOUNT                    
         MVC   PWDOLBLD,CTODAY      AND REMEMBER WHEN IT WAS DONE               
         OI    MISCFLG2,MF2RFBQ    RE-FINAL BILLING NEEDED                      
         OI    ATFLAG2-ACCUTABD(R5),ATF2RFB                                     
         MVI   HALF+1,1             MADE CHANGES TO MONTH IN RECD               
         DROP  RF                                                               
                                                                                
RFB55    DS    0H                  DELETE CORRESPONDING PWCUREL ELEM            
         OC    PWKSTA,PWKSTA        (FOR MKT-LVL PW RECD ONLY)                  
         BNZ   RFB57                                                            
         CLI   HALF+1,1            RECORD CHANGED?                              
         BNE   RFB57                NO, THEN DON'T DELETE ELEMS                 
         MVI   ELCODE,PWCURCDQ                                                  
         MVI   ELEMENT,L'ATWSTART                                               
         MVC   ELEMENT+1(L'ATWSTART),ATWSTART                                   
         MVI   GOSUBN,DLM#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
RFB57    DS    0H                  BUMP TO NEXT WEEK                            
         LA    R4,ACCUTABQ(R4)                                                  
         B     RFB52                                                            
                                                                                
RFB58    DS    0H                  DID NOT NULL OUT PWDOLBIL & PWDOLSPT         
         LR    R4,R5               POINT R4 BACK TO BILL ADJ ENTRY              
         B     RFB70                AND BUMP TO NEXT MONTH                      
                                                                                
RFB59    DS    0H                  MONTH CHANGED,                               
         B     RFB70                BUMP TO NEXT MONTH                          
                                                                                
*                                                                               
RFB70    DS    0H                  BUMP TO NEXT MONTH (R4-->BILL ADJ)           
         LA    R4,2*ACCUTABQ(R4)                                                
         B     RFB20                                                            
                                                                                
*                                                                               
RFB80    DS    0H                  WENT THRU ENTIRE ESTIMATE PERIOD             
         TM    MISCFLG2,MF2RFBQ    WILL RE-FINAL BILLING BE NEEDED?             
         BZ    RFBX                 NOPE, EXIT NOW                              
                                                                                
         GOTO1 PUTREC               YES, PUT UPDATED RECORD BACK                
                                                                                
         GOTO1 SEQ                   AND GET THE NEXT ONE                       
         B     RFB10                                                            
         DROP  R4,R6                                                            
*                                                                               
RFBX     DS    0H                                                               
         MVC   AIO,AIO1            RESTORE I/O AREA#1 AS DEFAULT                
         B     XIT_02                                                           
         TITLE 'SPSFM43 - PW BILL (SUBR02--DLM#)'                               
*-------------------------- DELETE ELEMENTS --------------------------*         
                                                                                
* Deletes all occurences of an element with a given element code & an           
*  optional search argument from a record.                                      
* At entry,                                                                     
*   SYSFIL = name of file,                                                      
*   R6    -->record,                                                            
*   ELCODE = code of elements to be removed.                                    
*   ELEMENT(1) = l(optional search argument)--zero means none,                  
*   ELEMENT+1  = optional search argument                                       
                                                                                
DELEM    DS    0H                                                               
         CLI   ELCODE,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         DS    0H                  SET UP OPTNL SRCH ARG PARAMS                 
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         ICM   R2,1,ELEMENT                                                     
         BZ    *+8                                                              
         LA    R3,ELEMENT+1                                                     
*                                                                               
DLM10    DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(ELCODE,(R6)),((R2),(R3)),0             
         CLI   12(R1),0                                                         
         BE    DLM20                                                            
         CLI   12(R1),X'06'                                                     
         BE    DLMX                                                             
         DC    H'0'                                                             
                                                                                
DLM20    DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(ELCODE,(R6)),((R2),(R3)),0             
         CLI   12(R1),0            KEEP DELETING UNTIL                          
         BE    DLM10                ELEMENT NOT FOUND                           
         DC    H'0'                                                             
*                                                                               
DLMX     DS    0H                                                               
         B     XIT_02                                                           
         TITLE 'SPSFM43 - PW BILL (SUBR02--UPT#)'                               
*--------------------------- UPDATE PW% TABLE ------------------------*         
                                                                                
* transfers PW%s from ACCUTAB at end of every "good" modification *             
                                                                                
UPWTAB   DS    0H                                                               
                                                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
         LA    R6,PWTAB                                                         
                                                                                
UPT10    TM    ATFLAG,ATFSTTLQ     IF SCHED TOTALS ENTRY                        
         BO    UPTX                 THEN WE'RE FINISHED                         
         TM    ATFLAG,ATFMTTLQ     IF MONTHLY TOTALS ENTRY,                     
         BO    UPT20                THEN SKIP                                   
                                                                                
         CLC   0(4,R6),ATWEEK      MAKE SURE WE'RE AT THE RIGHT WEEK            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   4(4,R6),ATPW        UPDATE PW TABLE                              
         LA    R6,8(R6)            BUMP PWTAB POINTER                           
                                                                                
UPT20    LA    R4,ACCUTABQ(R4)     BUMP ACCUTAB POINTER                         
         B     UPT10                                                            
*                                                                               
UPTX     CLI   0(R6),XFF           BETTER BE AT EOTABLE OF PWTAB                
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT_02                                                           
         DROP  R4                                                               
         TITLE 'SPSFM43 - PW BILL (SUBR02--LTORG && CONSTANTS)'                 
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
         SPACE 2                                                                
PERIOD   DC    C'PER'                                                           
ZEROES   DC    (L'PWBM1)CL1'0'                                                  
         SPACE 2                                                                
         TITLE 'SPSFM43 - PW BILL (SUBR02--MISC STUFF)'                         
*--------------------- SUBR02 MISCELLANEOUS STUFF --------------------*         
                                                                                
GTEL     AH    R6,DATADISP                                                      
FRSTEL   CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                  RETURN CC NOT EQUAL                          
         CLI   ELCDLO,0                                                         
         BER   RE                  RETURN CC EQUAL                              
         CLI   ELCDHI,0                                                         
         BER   RE                  RETURN CC EQUAL                              
         CLC   ELCDLO,ELCDHI                                                    
         BHR   RE                  RETURN CC NOT EQUAL                          
         B     NXTEL2                                                           
NXTEL    CLI   0(R6),0                                                          
         BE    NXTELX                                                           
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NXTEL2   CLI   0(R6),0                                                          
         BE    NXTELX                                                           
         CLC   ELCDLO,0(R6)                                                     
         BH    NXTEL                                                            
         CLC   ELCDHI,0(R6)                                                     
         BL    NXTEL                                                            
         CR    RB,RB                                                            
         B     *+6                                                              
NXTELX   LTR   RB,RB                                                            
         BR    RE                                                               
                                                                                
                                                                                
SUBR02L  EQU   *-SUBR02                                                         
         DS    0CL(4096-SUBR02L+1)                                              
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM43 - PW BILL (SUBR03)'                                     
***********************************************************************         
*======================= SUBROUTINE POOL THREE =======================*         
SUBR03Q  EQU   (((*-T21743+X'0FFF')/X'1000')*X'1000')                           
                                                                                
         ORG   T21743+SUBR03Q                                                   
SUBR03   NMOD1 0,**4303**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         L     R1,0(R1)                                                         
         SRL   R1,24               SHIFT TO LOW-ORDER BYTE                      
         SH    R1,=Y(R02#)          ADJUST FOR PREVIOUS ROUTINES,               
         BCTR  R1,0                 SUBTRACT ONE,                               
         SLL   R1,2                 AND MULTIPLY BY FOUR                        
         B     R03_00(R1)                                                       
                                                                                
CEDC#    EQU   (R03_01-*)/4+R02#+1                                              
FXR#     EQU   (R03_02-*)/4+R02#+1                                              
SUM#     EQU   (R03_03-*)/4+R02#+1                                              
DFR#     EQU   (R03_04-*)/4+R02#+1                                              
STW#     EQU   (R03_05-*)/4+R02#+1                                              
RTW#     EQU   (R03_06-*)/4+R02#+1                                              
MNI#     EQU   (R03_07-*)/4+R02#+1                                              
TSR_INI# EQU   (R03_08-R03_00)/4+R02#+1                                         
TSR_ADD# EQU   (R03_09-R03_00)/4+R02#+1                                         
TSR_RDH# EQU   (R03_10-R03_00)/4+R02#+1                                         
TSR_WRT# EQU   (R03_11-R03_00)/4+R02#+1                                         
TSR_GET# EQU   (R03_12-R03_00)/4+R02#+1                                         
TSR_PUT# EQU   (R03_13-R03_00)/4+R02#+1                                         
TSR_NXT# EQU   (R03_14-R03_00)/4+R02#+1                                         
TSR_SAV# EQU   (R03_15-R03_00)/4+R02#+1                                         
TSR_RES# EQU   (R03_16-R03_00)/4+R02#+1                                         
FCO#     EQU   (R03_17-R03_00)/4+R02#+1                                         
FOWP#    EQU   (R03_18-R03_00)/4+R02#+1                                         
DOD#     EQU   (R03_19-R03_00)/4+R02#+1                                         
                                                                                
R03_00   DS    0H                                                               
R03_01   B     CHKESTDT                                                         
R03_02   B     FXRECD                                                           
R03_03   B     SUMUP                                                            
R03_04   B     DEFERSPT                                                         
R03_05   B     SAVETWA             SAVE TWA INTO TEMPSTR                        
R03_06   B     RSTRTWA             RESTORE TWA FROM TEMPSTR                     
R03_07   B     MODNXTIN            MODIFY FOR NEXT INPUT                        
R03_08   B     TSR_INI             INITIALIZE     TSAR BUFFER                   
R03_09   B     TSR_ADD             ADD RECORD TO  TSAR BUFFER                   
R03_10   B     TSR_RDH             READ HIGH FROM TSAR BUFFER                   
R03_11   B     TSR_WRT             WRITE RECD TO  TSAR BUFFER                   
R03_12   B     TSR_GET             GET RECD FROM  TSAR BUFFER                   
R03_13   B     TSR_PUT             PUT RECD INTO  TSAR BUFFER                   
R03_14   B     TSR_NXT             NEXT RECD FROM TSAR BUFFER                   
R03_15   B     TSR_SAV             SAVE           TSAR BUFFER                   
R03_16   B     TSR_RES             RESTORE        TSAR BUFFER                   
R03_17   B     FILLCLC             FILL CLCOST OVERRIDES INTO ACCUTAB           
R03_18   B     FILLOOWP            FILL OOW PAID DOLLARS INTO ACCUTAB           
R03_19   B     DOOWRD              DO OOWR PAID DOLLARS ELEMENT                 
R03#     EQU   (*-R03_00)/4+R02#                                                
         DC    H'0'                                                             
                                                                                
YES_03   SR    RC,RC                                                            
NO_03    LTR   RC,RC                                                            
XIT_03   XIT1                                                                   
         TITLE 'SPSFM43 - PW BILL (SUBR03--CEDC#)'                              
*---------------- CHECK ESTIMATE HEADER'S DATE CHANGED ---------------*         
                                                                                
* Routine checks the PWWKEL elements against the broadcast periods to           
*  see if the dates in the estimate header had changed since the PWWKEL         
*  elements were last written to the file.                                      
* At entry,                                                                     
*   AIO = A(mkt-level PW record)                                                
                                                                                
CHKESTDT DS    0H                                                               
                                                                                
*                                                                               
** CHECK FIRST WEEK OF SCHEDULE **                                              
*                                                                               
         DS    0H                                                               
         LA    R3,BRDWKTAB         R3-->WEEK DATES OF SCHEDULE                  
         TM    ESTFLAG,EFBILEST                                                 
         BZ    *+8                                                              
         LA    R3,BRDMTHTB         USE PERIOD DATES IF ONE PERIOD               
         USING BCSTTABD,R3                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PWWKCODQ                                                  
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
         BAS   RE,GETEL3                                                        
         BNE   CEDCXN                                                           
         USING PWWKEL,R6                                                        
                                                                                
*                                                                               
         CLC   PWWKDATE,BCSSTART                                                
         BNE   CEDCXN                                                           
                                                                                
*                                                                               
** CHECK FINAL WEEK OF SCHEDULE **                                              
*                                                                               
         DS    0H                                                               
         CLI   BCSTTABQ(R3),XFF                                                 
         BE    *+12                                                             
         LA    R3,BCSTTABQ(R3)                                                  
         B     *-12                                                             
                                                                                
         DS    0H                  R3-->FINAL WEEK OF SCHEDULE                  
         ST    R6,FULL             HOLD ONTO (ADDR OF PWWKEL)                   
         BAS   RE,NEXTEL3                                                       
         BE    *-8                                                              
         L     R6,FULL             R6-->A(LAST PWWKEL IN RECORD)                
*                                                                               
         CLC   PWWKDATE,BCSSTART                                                
         BNE   CEDCXN                                                           
                                                                                
*                                                                               
         B     CEDCXY                                                           
         DROP  R3,R6                                                            
                                                                                
*                                                                               
CEDCXN   DS    0H                                                               
         B     NO_03                                                            
                                                                                
CEDCXY   DS    0H                                                               
         B     YES_03                                                           
         TITLE 'SPSFM43 - PW BILL (SUBR03--FXR#)'                               
*--------------------------- FIX PW RECORDS --------------------------*         
                                                                                
* Routine to fix broadcast dates in PW records.  The only way this rtn          
*  can be called is if the dates in the PW records are out of sync w/           
*  the dates in the estimate header (such as dates in est hdr changing)         
*  (or if an estimate is changed to an E-estimate)                              
                                                                                
FXRECD   DS    0H                                                               
         MVI   GOSUBN,BPK#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
** READ AND GET RECORD **                                                       
*                                                                               
         DS    0H                                                               
         GOTO1 HIGH                                                             
         B     FXR024                                                           
*                                                                               
FXR022   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
FXR024   DS    0H                                                               
         CLC   KEY(PKYMKTL),KEYSAVE                                             
         BNE   FXRX                                                             
*                                                                               
         GOTO1 GETREC                                                           
                                                                                
*                                                                               
** BODY OF THE RECORD FIX **                                                    
*                                                                               
         DS    0H                                                               
         L     R6,AIO                                                           
         LA    R6,(PWEL-PWRECD)(R6)                                             
                                                                                
*                                                                               
FXR052   DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    FXR099                                                           
                                                                                
*                                                                               
         L     RE,AELDATTB                                                      
FXR062   DS    0H                                                               
         CLI   0(RE),EOT           AT END OF TABLE?                             
         BE    FXR090               YES, BUMP TO NEXT ELEMENT                   
         CLC   0(1,R6),0(RE)       SHOULD WE PROCESS THIS ELEMENT?              
         BE    *+12                 YES                                         
         LA    RE,L'ELDATTAB(RE)                                                
         B     FXR062                                                           
*                                                                               
         ZIC   RF,1(RE)                                                         
         AR    RF,R6               RF-->(COMPRESSED) DATE FIELD IN ELEM         
*                                                                               
         LA    RE,BRDWKTAB                                                      
         TM    ESTFLAG,EFBILEST                                                 
         BZ    *+8                                                              
         LA    RE,BRDMTHTB                                                      
         USING BCSTTABD,RE                                                      
                                                                                
FXR074   DS    0H                  FIND MATCHING WEEK FOR DATE IN ELEM          
         CLI   0(RE),XFF                                                        
         BE    FXR074B                                                          
         CLC   BCSSTART,0(RF)                                                   
         BH    FXR074D                                                          
         CLC   BCSEND,0(RF)                                                     
         BL    FXR074D                                                          
         MVC   0(2,RF),BCSSTART                                                 
         B     FXR074X                                                          
                                                                                
FXR074B  DS    0H                  ELEM DATE OUTSIDE OF ESTIMATE PERIOD         
         GOTO1 RECUP,DMCB,(0,AIO),(R6)                                          
         B     FXR052               R6 POINTS TO NEXT ELEMENT                   
                                                                                
FXR074D  DS    0H                                                               
         LA    RE,BCSTTABQ(RE)                                                  
         B     FXR074                                                           
FXR074X  EQU   *                                                                
         DROP  RE                                                               
                                                                                
*                                                                               
FXR090   DS    0H                  BUMP TO NEXT ELEMENT                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     FXR052                                                           
FXR099   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                 MAKE SURE WE HAVE PWWKEL FOR ALL WKS          
         L     RF,AIO                                                           
         USING PWRECD,RF                                                        
         OC    PWKSTA,PWKSTA       APPLICABLE TO MKT-LEVEL RECDS ONLY           
         BNZ   FXR119                                                           
         DROP  RF                                                               
*                                                                               
         DS    0H                  BUILD SKELETON PWWKEL ELEMENT                
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PWWKEL,R6                                                        
         MVI   PWWKCD,PWWKCODQ                                                  
         MVI   PWWKLEN,PWWKLENQ                                                 
*                                                                               
         DS    0H                  DRIVE OFF OF BROADCAST TABLE                 
         LA    R3,BRDWKTAB                                                      
         TM    ESTFLAG,EFBILEST                                                 
         BZ    *+8                                                              
         LA    R3,BRDMTHTB                                                      
         USING BCSTTABD,R3                                                      
*                                                                               
FXR113   DS    0H                                                               
         CLI   0(R3),XFF                                                        
         BE    FXR117                                                           
                                                                                
         MVC   PWWKDATE,BCSSTART                                                
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWWKCODQ',AIO),              +        
               (L'PWWKDATE,PWWKDATE),0                                          
         CLI   DMCB+12,0                                                        
         BE    FXR115                                                           
                                                                                
         MVC   PWWKPCT,OPWPCT                                                   
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,PWWKEL,0                            
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FXR115   DS    0H                  BUMP TO NEXT WEEK IN SCHEDULE                
         LA    R3,BCSTTABQ(R3)                                                  
         B     FXR113                                                           
FXR117   EQU   *                                                                
         DROP  R3,R6                                                            
*                                                                               
FXR119   EQU   *                                                                
                                                                                
*                                                                               
** MERGE DOLLAR ELEMENTS WITH SAME DATES **                                     
*                                                                               
         L     R6,AIO                                                           
         LA    R6,(PWEL-PWRECD)(R6)                                             
                                                                                
FXR132   DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    FXR159                                                           
*                                                                               
         L     RE,AELDATTB                                                      
FXR134   DS    0H                  MATCH ELEMENT CODE TO TABLE                  
         CLI   0(RE),EOT                                                        
         BE    FXR155                                                           
         CLC   0(1,R6),0(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'ELDATTAB(RE)                                                
         B     FXR134                                                           
                                                                                
         MVC   CNTDOWN,2(RE)       CNTDOWN = # OF BUCKETS                       
         ZIC   R0,1(RE)                                                         
         STH   R0,FULL             FULL(2) = DISPL TO DATE                      
         IC    R0,3(RE)                                                         
         STH   R0,FULL+2           FULL+2(2) = DISPL TO 1ST BUCKET              
                                                                                
*                                                                               
         DS    0H                  LOOK FOR SAME ELCODE W/ SAME DATE            
         ZIC   R3,1(R6)                                                         
         AR    R3,R6               START W/ ELEM AFTER R6                       
*                                                                               
FXR138   DS    0H                                                               
         CLI   0(R3),0             IF END OF RECORD,                            
         BE    FXR155               BUMP R6 TO NEXT ELEMENT                     
         CLC   0(1,R3),0(R6)       SAME ELEMENT CODE?                           
         BNE   FXR138D                                                          
         LH    RE,FULL                                                          
         AR    RE,R6               RE-->(COMPRESSED) DATE FIELD                 
         LH    RF,FULL                                                          
         AR    RF,R3               RF-->(COMPRESSED) DATE FIELD                 
         CLC   0(2,RE),0(RF)       DATES MATCH?                                 
         BNE   FXR138D                                                          
         B     FXR138X                                                          
*                                                                               
FXR138D  DS    0H                                                               
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FXR138                                                           
FXR138X  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  CONSOLIDATE ELEMENTS                         
         MVC   HALF,FULL+2          HALF = DISPL TO 1ST BUCKET                  
         ZICM  R2,CNTDOWN,(1)       R2 = NUMBER OF BUCKETS                      
         BZ    FXR149                NO BUCKETS==>NOTHING TO MERGE              
*                                                                               
FXR142   DS    0H                                                               
         LH    RE,HALF                                                          
         AR    RE,R6               RE-->1ST BUCKET OF DEST. ELEM                
         ICM   R0,15,0(RE)                                                      
         LH    RF,HALF                                                          
         AR    RF,R3               RF-->1ST BUCKET OF SOURCE ELEM               
         ICM   R1,15,0(RF)                                                      
                                                                                
         AR    R0,R1                                                            
         STCM  R0,15,0(RE)                                                      
                                                                                
         LA    R1,4                                                             
         AH    R1,HALF                                                          
         STH   R1,HALF             HALF = DISPL TO NEXT BUCKET                  
         BCT   R2,FXR142                                                        
                                                                                
*                                                                               
         DS    0H                  MERGE NON-BUCKET ITEMS                       
         CLI   0(R6),PWDOLCDQ                                                   
         BE    FXR146                                                           
         CLI   0(R6),PWBAKDOL                                                   
         BE    FXR146                                                           
         CLI   0(R6),PWCURCDQ                                                   
         BE    FXR148                                                           
         B     FXR149                                                           
*                                                                               
FXR146   DS    0H                  LOCKED DOLLAR ELEMENT                        
         USING PWDOLEL,R6                                                       
         MVI   BYTE,C'N'                                                        
         ICM   R0,15,PWDOLBIL                                                   
         C     R0,=X'80000000'                                                  
         BNE   *+10                                                             
         MVI   BYTE,C'Y'                                                        
         SR    R0,R0                                                            
         ICM   R1,15,(PWDOLBIL-PWDOLEL)(R3)                                     
         C     R1,=X'80000000'                                                  
         BNE   *+10                                                             
         MVI   BYTE,C'Y'                                                        
         SR    R1,R1                                                            
         AR    R0,R1                                                            
         LTR   R0,R0                                                            
         BNZ   *+16                                                             
         CLI   BYTE,C'Y'                                                        
         BNE   *+8                                                              
         ICM   R0,15,=X'80000000'                                               
         STCM  R0,15,PWDOLBIL       BILL OVERRIDE DOLLARS                       
                                                                                
         CLC   PWDOLBLD,(PWDOLBLD-PWDOLEL)(R3)                                  
         BH    *+10                                                             
         MVC   PWDOLBLD,(PWDOLBLD-PWDOLEL)(R3)                                  
                                                                                
         B     FXR149                                                           
         DROP  R6                                                               
*                                                                               
FXR148   DS    0H                  CURRENT DOLLAR ELEMENT                       
         USING PWCUREL,R6                                                       
         MVI   BYTE,C'N'                                                        
         ICM   R0,15,PWCURBIL                                                   
         C     R0,=X'80000000'                                                  
         BNE   *+10                                                             
         MVI   BYTE,C'Y'                                                        
         SR    R0,R0                                                            
         ICM   R1,15,(PWCURBIL-PWCUREL)(R3)                                     
         C     R1,=X'80000000'                                                  
         BNE   *+10                                                             
         MVI   BYTE,C'Y'                                                        
         SR    R1,R1                                                            
         AR    R0,R1                                                            
         LTR   R0,R0                                                            
         BNZ   *+16                                                             
         CLI   BYTE,C'Y'                                                        
         BNE   *+8                                                              
         ICM   R0,15,=X'80000000'                                               
         STCM  R0,15,PWCURBIL       BILL OVERRIDE DOLLARS                       
                                                                                
         CLC   PWCURBLD,(PWCURBLD-PWCUREL)(R3)                                  
         BH    *+10                                                             
         MVC   PWCURBLD,(PWCURBLD-PWCUREL)(R3)                                  
                                                                                
         B     FXR149                                                           
         DROP  R6                                                               
FXR149   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  DELETE SOURCE ELEMENT                        
         GOTO1 RECUP,DMCB,(0,AIO),(R3)   R3 NOW POINTS TO NEXT ELEMENT          
         B     FXR138                                                           
                                                                                
*                                                                               
FXR155   DS    0H                  BUMP R6 TO NEXT ELEMENT                      
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     FXR132                                                           
FXR159   EQU   *                                                                
         EJECT                                                                  
*                                                                               
** WRITE RECORD BACK TO FILE **                                                 
*                                                                               
         DS    0H                                                               
         GOTO1 PUTREC                                                           
         B     FXR022                                                           
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
FXRX     DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM43 - PW BILL (SUBR03--SUM#)'                               
*------------------------- TOTAL UP SCHEDULE -------------------------*         
                                                                                
*  By this time, actual & adjusted goals and buys, PW%, and lockins             
*   should be set in ACCUTAB already                                            
                                                                                
SUMUP    DS    0H                                                               
         XC    MTOTALS(MTOTALQ),MTOTALS                                         
         XC    STOTALS(STOTALQ),STOTALS                                         
         XC    SKEDBILL,SKEDBILL                                                
         XC    SKEDDRCR,SKEDDRCR                                                
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
SUM10    TM    ATFLAG,ATFSTTLQ                                                  
         BO    SUM30                                                            
         TM    ATFLAG,ATFMTTLQ                                                  
         BO    SUM20                                                            
                                                                                
         LA    R0,ATACVALN         R0 = # OF VALUES TO SUM UP                   
         SR    R1,R1                                                            
SUM12    LA    R2,ATACVALS(R1)     R2-->ACCUMULATOR VALUE                       
         ICM   RE,15,0(R2)         RE = ACCUMULATOR VALUE                       
         L     RF,MTOTALS(R1)                                                   
         AR    RF,RE                                                            
         ST    RF,MTOTALS(R1)                                                   
         L     RF,STOTALS(R1)                                                   
         AR    RF,RE                                                            
         ST    RF,STOTALS(R1)                                                   
         LA    R1,L'ATACVALS(R1)                                                
         BCT   R0,SUM12                                                         
         B     SUM50                                                            
*                                                                               
SUM20    DS    0H                  MONTH LINE                                   
         TM    ATFLAG,ATFBILAJ      DETERMINE WHETHER IT'S BILL ADJ             
         BZ    SUM23                OR MONTH TOTALS LINE                        
                                                                                
         DS    0H                  MONTHLY BILL ADJUSTMENT                      
         MVC   ATACVALS(MTOTALQ),MTOTALS   TOTALS W/O DR/CR AMOUNT              
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPAJB,ATAJBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWPW#        CALC CURR PW%                                
         GOTO1 AGOSUB                                                           
         MVC   ATPW,TEMPPW          AND UPDATE ACCUTAB                          
                                                                                
         MVC   TEMPACB,ATWLCK      CALC PW% OF LOCKED VALUES                    
         MVC   TEMPAJB,ATCLCK                                                   
         MVC   TEMPTAX,ATLKTAX                                                  
         MVI   GOSUBN,PWPW#                                                     
         GOTO1 (RF)                                                             
         MVC   ATLPW,TEMPPW         AND UPDATE ACCUTAB                          
         MVC   ATLPW+ACCUTABQ,TEMPPW  PUT ON MNTH TOTALINE TOO                  
                                                                                
         ICM   R0,15,ATDRCR         ADD DR/CR TO CURRENT CLT$                   
         BZ    SUM29                                                            
         CLM   R0,15,=X'80000000'                                               
         BNE   *+6                                                              
         SR    R0,R0                                                            
         LR    R1,R0                                                            
         LR    RF,R0                                                            
         A     RF,SKEDDRCR                                                      
         ST    RF,SKEDDRCR                                                      
         A     R1,MTTLAJB                                                       
         ST    R1,MTTLAJB                                                       
         A     R0,SKEDAJB                                                       
         ST    R0,SKEDAJB                                                       
         B     SUM29                                                            
                                                                                
SUM23    DS    0H                  MONTHLY TOTALS LINE                          
         MVC   ATACVALS(MTOTALQ),MTOTALS   GET SUMS TO MONTH LINE               
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPAJB,ATAJBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWPW#                CALC PROJ PW%                        
         GOTO1 AGOSUB                                                           
         MVC   ATPW,TEMPPW                  AND UPDATE ACCUTAB                  
                                                                                
         L     R0,SKEDBILL                 CALCULATE THE ACTUAL BILL            
         A     R0,ATBILL                                                        
         ST    R0,SKEDBILL                  FOR ENTIRE SCHEDULE                 
                                                                                
         XC    MTOTALS(MTOTALQ),MTOTALS    RESET FOR NEXT MONTH                 
                                                                                
SUM29    B     SUM50                                                            
*                                                                               
SUM30    DS    0H                  SKED BILL ADJ LINE                           
         MVC   ATDRCR,SKEDDRCR     MOVE IN SKED TOTAL DR/CR                     
         MVC   ATBILL,SKEDBILL      AND SKED TOTAL BILL                         
         MVC   ATACVALS(STOTALQ),STOTALS   GET SUMS TO SKED LINE                
         ICM   R0,15,ATAJBUY                                                    
         S     R0,SKEDDRCR                                                      
         STCM  R0,15,ATAJBUY                W/O DR/CR TOTAL                     
                                                                                
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPAJB,ATAJBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWPW#           CALC CURR PW%                             
         GOTO1 AGOSUB                                                           
         MVC   ATPW,TEMPPW             AND UPDATE ACCUTAB                       
                                                                                
         MVC   TEMPACB,ATWLCK         CALC PW% OF LOCKED VALUES                 
         MVC   TEMPAJB,ATCLCK                                                   
         MVC   TEMPTAX,ATLKTAX                                                  
         MVI   GOSUBN,PWPW#                                                     
         GOTO1 (RF)                                                             
         MVC   ATLPW,TEMPPW            AND UPDATE ACCUTAB                       
         MVC   ATLPW+ACCUTABQ,TEMPPW   PUT IT IN SKED TOTALS LINE TOO           
                                                                                
         LA    R4,ACCUTABQ(R4)     BUMP TO SKED TOTALS LINE                     
                                                                                
         DS    0H                  SKED TOTALS LINE                             
         MVC   ATACVALS(STOTALQ),STOTALS   GET SUMS TO SKED LINE                
         MVC   ATBILL,SKEDBILL                                                  
         MVC   TEMPACB,ATACBUY                                                  
         MVC   TEMPAJB,ATAJBUY                                                  
         MVC   TEMPTAX,ATTAX                                                    
         MVI   GOSUBN,PWPW#           CALC PROJ PW%                             
         GOTO1 AGOSUB                                                           
         MVC   ATPW,TEMPPW             AND UPDATE ACCUTAB                       
         B     SUMUPX                                                           
*                                                                               
SUM50    LA    R4,ACCUTABQ(R4)                                                  
         B     SUM10                                                            
*                                                                               
SUMUPX   B     XIT_03                                                           
         DROP  R4                                                               
         TITLE 'SPSFM43 - PW BILL (SUBR03--DFR#)'                               
*------------------ CHECK IF SPOT SHOULD BE DEFERRED -----------------*         
                                                                                
* AT ENTRY,                                                                     
*   R3    -->  SCHUNK                                                           
*   R6    -->  buy record                                                       
*   SDATE2  =  start date of broadcast month                                    
*   EDATE2  =  end   date of broadcast month                                    
*                                                                               
* At exit,                                                                      
*   CC set to equal if spot should be deferred,                                 
*   CC set to not equal otherwise                                               
                                                                                
DEFERSPT DS    0H                                                               
                                                                                
         USING SCHUNKD,R3                                                       
         USING BUYRECD,R6                                                       
                                                                                
*                                                                               
** SET RUN DATE OF SPOTS IN CHUNK **                                            
*                                                                               
         DS    0H                                                               
         MVC   RUNDATE,SCDATE      SET DATE                                     
*                                                                               
         DS    0H                                                               
         OC    SCADATE,SCADATE     IF SPOT IS MATCHED,                          
         BZ    *+14                                                             
         MVC   RUNDATE,SCADATE      USE AFFID DATE                              
         B     DFR019X                                                          
*                                                                               
         DS    0H                  ADVANCE TO LAST DAY OF ROTATION              
         CLC   RUNDATE,EDATE2       IF ALREADY PAST END OF B'CST MNTH,          
         BH    DFRXN                 CAN NOT BE DEFERRED                        
                                                                                
         SR    R0,R0                R0 = DAYS TO END OF ROTATION                
         ZIC   RE,BDSEDAY                                                       
         SRDL  RE,4                                                             
         SRL   RF,28                                                            
         CR    RE,RF                                                            
         BNH   *+12                                                             
         LA    RF,7(RF)                                                         
         SR    RF,RE                                                            
         LR    R0,RF                SET DAYS TO END OF ROTATION                 
                                                                                
         GOTO1 DATCON,DMCB,(2,SCDATE),(0,STARTEND)                              
         GOTO1 ADDAY,DMCB,STARTEND,STARTEND+6,(R0)                              
         GOTO1 DATCON,DMCB,(0,STARTEND+6),(2,RUNDATE)                           
*                                                                               
DFR019X  EQU   *                                                                
                                                                                
*                                                                               
** TEST RUN DATE WITHIN BROADCAST MONTH **                                      
*                                                                               
         CLC   RUNDATE,SDATE2      IF BEFORE START,                             
         BL    DFRXN                IT CAN'T BE DEFERRED                        
         CLC   RUNDATE,EDATE2      IF AFTER END,                                
         BH    DFRXY                IT CAN BE DEFERRED                          
                                                                                
*                                                                               
         DS    0H                                                               
         B     DFRXN                                                            
                                                                                
*                                                                               
** EXITS **                                                                     
*                                                                               
DFRXN    DS    0H                                                               
         B     NO_03                                                            
*                                                                               
DFRXY    DS    0H                                                               
         B     YES_03                                                           
         DROP  R3,R6                                                            
         TITLE 'SPSFM43 - PW BILL (SUBR03--STW# && RTW#)'                       
*------------------------------ SAVE TWA -----------------------------*         
                                                                                
* save TWA into TEMPSTR                                                         
                                                                                
SAVETWA  DS    0H                                                               
         MVI   DMCB+8,PAGE00                                                    
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ATWA,0,0                     
         B     XIT_03                                                           
         SPACE 2                                                                
*----------------------------- RESTORE TWA ---------------------------*         
                                                                                
* restore TWA from TEMPSTR                                                      
                                                                                
RSTRTWA  DS    0H                                                               
         MVI   DMCB+8,PAGE00       3RD PARAMETER                                
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='   6TH PARAMETER                                
         MVC   DMCB+22(2),=H'18432'                                             
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,ATWA,0                      
         B     XIT_03                                                           
         TITLE 'SPSFM43 - PW BILL (SUBR03--MNI#)'                               
*----------------------- MODIFY FOR NEXT INPUT -----------------------*         
                                                                                
* Routine is executed when an error occurs.  This is to remember which          
*  fields were changed so that if an error occurs, the user would only          
*  need to correct the error and not need to re-input the rest of the           
*  fields.                                                                      
                                                                                
MODNXTIN DS    0H                                                               
         LA    R2,PWBCPWH              START W/ CURRENT PW% LINE                
         LA    RF,(BSLTTLH-BSLDSECT)(R2)                                        
         LA    R0,PWBBILLH             DO ADJ DR/CR$ LINE NEXT                  
*                                                                               
MNI10    CR    R2,RF               AT THE END OF LINE YET?                      
         BNH   MNI15                                                            
         CR    R2,R0                YES, DID WE DO ADJ DR/CR$ YET?              
         BNL   MNIX                  YEP, SO EXIT                               
                                                                                
         LR    R2,R0                 NO, DO ADJ DR/CR$ LINE NOW                 
         LA    RF,(BSLM4H-BSLDSECT)(R2)                                         
         B     MNI10                                                            
*                                                                               
MNI15    TM    4(R2),X'80'         IF FIELD WAS MODIFIED,                       
         BZ    MNI20                                                            
         OI    6(R2),X'81'          MAKE MODIFIED FOR NEXT TIME                 
*                                                                               
MNI20    LA    R2,BSLMQ(R2)        BUMP TO NEXT COLUMN                          
         B     MNI10                                                            
*                                                                               
MNIX     DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM43 - PW BILL (SUBR03--TSR_#''s)'                           
*--------------------------- TSAR INTERFACE --------------------------*         
                                                                                
         USING TSARD,R3                                                         
TSR_INI  DS    0H                  INITIALIZE     TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         XC    TSARD(TSARDL),TSARD                                              
         MVI   TSACTN,TSAINI                                                    
         LA    R0,STACREC           TSAR USED FOR STTN ACCUM TABLE              
         ST    R0,TSAREC                                                        
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSPAGL,PAGETSRQ      LOW TEMPSTR PAGE TO USE                     
         MVI   TSPAGN,1             ONE PAGE SHOULD BE ENOUGH                   
         MVI   TSKEYL,STACKEYL      KEY LENGTH                                  
         LA    R0,STACRECL                                                      
         STH   R0,TSRECL            (FIXED) RECORD LENGTH                       
         OI    TSINDS,TSIREUSE+TSIXTTWA                                         
         XC    TSRNUM,TSRNUM                                                    
         B     TSR_GO                                                           
                                                                                
                                                                                
TSR_ADD  DS    0H                  ADD RECORD TO  TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSAADD                                                    
         XC    TSRNUM,TSRNUM                                                    
         B     TSR_GO                                                           
                                                                                
                                                                                
TSR_RDH  DS    0H                  READ HIGH FROM TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSARDH                                                    
         XC    TSRNUM,TSRNUM                                                    
         B     TSR_GO                                                           
                                                                                
                                                                                
TSR_WRT  DS    0H                  WRITE RECD TO  TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSAWRT                                                    
         XC    TSRNUM,TSRNUM                                                    
         B     TSR_GO                                                           
                                                                                
                                                                                
TSR_GET  DS    0H                  GET RECD FROM  TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSAGET                                                    
         B     TSR_GO                                                           
                                                                                
                                                                                
TSR_PUT  DS    0H                  PUT RECD INTO  TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSAPUT                                                    
         B     TSR_GO                                                           
                                                                                
                                                                                
TSR_NXT  DS    0H                  NEXT RECD FROM TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSANXT                                                    
         B     TSR_GO                                                           
                                                                                
                                                                                
TSR_SAV  DS    0H                  SAVE           TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSASAV                                                    
         B     TSR_GO                                                           
                                                                                
                                                                                
TSR_RES  DS    0H                  RESTORE        TSAR BUFFER                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSARES                                                    
         B     TSR_GO                                                           
                                                                                
                                                                                
         EJECT                                                                  
TSR_GO   DS    0H                                                               
         GOTO1 ATSAR,TSARD                                                      
         MVC   MYTSERRS,TSERRS     PASS BACK ERRORS                             
         CLI   TSERRS,0                                                         
         BE    YES_03                                                           
         B     NO_03                                                            
         DROP  R3                                                               
         TITLE 'SPSFM43 - PW BILL (SUBR03--FCO#)'                               
*---------------------- FILL IN CLCOST OVERRIDES ---------------------*         
                                                                                
FILLCLC  DS    0H                                                               
         MVI   GOSUBN,GPR#         GET PW RECORD                                
         GOTO1 AGOSUB                                                           
*                                                                               
         MVI   ELCODE,PWCLCCDQ                                                  
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
                                                                                
*                                                                               
         DS    0H                                                               
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
FCO022   DS    0H                                                               
         TM    ATFLAG,ATFSTTLQ                                                  
         BO    FCO029                                                           
         TM    ATFLAG,ATFMTTLQ                                                  
         BO    FCO026                                                           
                                                                                
         CLC   ATPW,=X'80000000'   IF PW% HAS THIS VALUE,                       
         BNE   FCO026                                                           
                                                                                
         OI    ATFLAG2,ATF2CCOD     TURN ON OVERRIDE FLAG AND                   
         L     R6,AIO               GET CORRESPONDING CLCOST OVERRIDE           
         BAS   RE,GETEL3                                                        
         B     FCO024B                                                          
FCO024A  BAS   RE,NEXTEL3                                                       
FCO024B  BNE   FCO026                                                           
                                                                                
         USING PWCLCEL,R6                                                       
         CLC   PWCLCWK,ATWSTART                                                 
         BNE   FCO024A                                                          
         MVC   ATAJBUY,PWCLCAMT                                                 
         DROP  R6                                                               
*                                                                               
FCO026   DS    0H                                                               
         LA    R4,ACCUTABQ(R4)                                                  
         B     FCO022                                                           
         DROP  R4                                                               
*                                                                               
FCO029   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM43 - PW BILL (SUBR03--FOWP#)'                              
*------------------ FILL IN OUT-OF-WEEK PAID DOLLARS -----------------*         
                                                                                
* If OOW estimate paid dollars element exist, the dollar amounts                
*  overrides the lock-in dollars filled in by the FLK# routine.                 
*  However, before the OOW estimate paid dollars for a particular month         
*  can be displayed, every station in the market must be paid off for           
*  that month.                                                                  
* At entry,                                                                     
*   AIO = A(PW record)                                                          
                                                                                
FILLOOWP DS    0H                                                               
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
                                                                                
*                                                                               
         DS    0H                  TURN ON "USE OOWR PAID$" FLAG                
         L     R6,AIO                                                           
         MVI   ELCODE,PWOOWCDQ                                                  
         BRAS  RE,GETEL3                                                        
FOWP010A DS    0H                                                               
         BNE   FOWP010X                                                         
*                                                                               
         USING PWOOWEL,R6                                                       
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
FOWP010B DS    0H                                                               
         TM    ATFLAG,ATFSTTLQ      SKED ENTRY OF ACCUTAB?                      
         BO    FOWP010M              YEP, EXIT LOOP                             
         TM    ATFLAG,ATFMTTLQ      LOOK FOR MONTH ENTRY                        
         BNO   FOWP010G                                                         
         GOTO1 DATCON,DMCB,(X'02',ATMEND),(X'03',FULL),0                        
         CLC   PWOOWYM,FULL         FOUND B'CAST MONTH?                         
         BNE   *+8                                                              
         OI    ATFLAG2,ATF2OOWP      YES, TURN PAID$ FLAG ON,                   
         LA    R4,ACCUTABQ(R4)        AND BUMP PAST MONTH ENTRY                 
FOWP010G EQU   *                                                                
         LA    R4,ACCUTABQ(R4)      BUMP TO NEXT ENTRY                          
         B     FOWP010B                                                         
FOWP010M EQU   *                                                                
         DROP  R4                                                               
*                                                                               
         DS    0H                                                               
         BRAS  RE,NEXTEL3          GET NEXT OOWR PAID$ ELEMENT                  
         B     FOWP010A                                                         
FOWP010X EQU   *                                                                
         DROP  R6                                                               
                                                                                
*                                                                               
         DS    0H                  TURN OFF PAID$ FLAG IF N/A                   
         MVI   GOSUBN,GPR#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         MVI   MYBYTE,0                                                         
FOWP012  DS    0H                                                               
         CLI   MYBYTE,C'N'          ANY ATF2OOWP FLAG TURNED ON?                
         BE    FOWP017               NO, SO DON'T DO ANY MORE READS             
         MVI   MYBYTE,C'N'                                                      
         GOTO1 SEQ                  READ FOR STTN-LEVEL PW RECORDS              
         CLC   KEY(PKYMKTL),KEYSAVE                                             
         BNE   FOWP017                                                          
         GOTO1 GETREC                                                           
*                                                                               
         DS    0H                                                               
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
FOWP013B DS    0H                                                               
         TM    ATFLAG,ATFSTTLQ      IF SKED ENTRY REACHED,                      
         BO    FOWP012               READ NEXT STTN-LVL RECORD                  
         TM    ATFLAG,ATFMTTLQ                                                  
         BNO   FOWP013S                                                         
         TM    ATFLAG2,ATF2OOWP                                                 
         BZ    FOWP013R                                                         
                                                                                
         MVI   MYBYTE,C'Y'          AT LEAST 1 ATF2OOWP FLAG STILL ON           
         GOTO1 DATCON,DMCB,(X'02',ATMEND),(X'03',FULL),0                        
*                                                                               
         DS    0H                   SEE IF ANY PAID$ ELEM FOR MONTH             
         L     R6,AIO                                                           
         MVI   ELCODE,PWOOWCDQ                                                  
         BRAS  RE,GETEL3                                                        
FOWP013C DS    0H                                                               
         BNE   FOWP013G                                                         
         USING PWOOWEL,R6                                                       
         CLC   PWOOWYM,FULL          IF PAID$ ELEM FOUND FOR MONTH              
         BE    FOWP013R               BUMP TO NEXT MONTH                        
         BRAS  RE,NEXTEL3            OTHERWISE, KEEP LOOKING                    
         B     FOWP013C                                                         
FOWP013G EQU   *                                                                
         DROP  R6                                                               
*                                                                               
         DS    0H                   NO PAID$ ELEM FOR MONTH                     
         L     R6,AIO                                                           
         MVI   ELCODE,PWDOLCDQ       LOOK FOR LOCKED$ ELEM                      
         BRAS  RE,GETEL3                                                        
FOWP013J DS    0H                                                               
         BNE   FOWP013N              NO LOCKED$ ELEM FOR MONTH NEITHER          
         USING PWDOLEL,R6                                                       
         CLC   ATMSTART,PWDOLWK      IF LOCKED$ ELEM                            
         BH    FOWP013L                                                         
         CLC   ATMEND,PWDOLWK         IS WITHIN B'CAST MONTH,                   
         BL    FOWP013L                                                         
         OC    PWDOLSPT,PWDOLSPT      AND THERE ARE SPOTS,                      
         BZ    FOWP013L                                                         
         NI    ATFLAG2,XFF-ATF2OOWP   DON'T USE PAID$ FOR MONTH                 
         B     FOWP013R                                                         
FOWP013L DS    0H                                                               
         BRAS  RE,NEXTEL3                                                       
         B     FOWP013J                                                         
FOWP013N EQU   *                                                                
         DROP  R6                                                               
*                                                                               
FOWP013R DS    0H                                                               
         LA    R4,ACCUTABQ(R4)      BUMP PAST MONTH ENTRY                       
FOWP013S DS    0H                                                               
         LA    R4,ACCUTABQ(R4)      BUMP TO NEXT ENTRY                          
         B     FOWP013B                                                         
         DROP  R4                                                               
FOWP017  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   GOSUBN,GPR#         RESTORE ORIGINAL RECD IN AIO                 
         GOTO1 AGOSUB                                                           
                                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         L     R6,AIO                                                           
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
         MVI   ELCODE,PWOOWCDQ                                                  
                                                                                
*                                                                               
         DS    0H                                                               
         BAS   RE,GETEL3           DO WE HAVE OOW EST PAID $ ELEM?              
         BNE   FOWP039              NOPE, EXIT NOW                              
         USING PWOOWEL,R6                                                       
                                                                                
*                                                                               
FOWP020  DS    0H                  R6-->PWOOWEL ELEMENT                         
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
*                                                                               
FOWP022  DS    0H                  R4-->1ST WEEK OF B'CAST MONTH                
         TM    ATFLAG,ATFSTTLQ     IF SKED LINE, GET NEXT ELEM                  
         BO    FOWP033                                                          
*                                                                               
         DS    0H                  CONVERT DATE TO BINARY FORM                  
         GOTO1 DATCON,DMCB,(X'02',ATMSTART),(0,MYDATE6),0                       
         GOTO1 AGETBROD,DMCB,(1,MYDATE6),STARTEND,AGETDAY,AADDAY                
         GOTO1 DATCON,DMCB,(0,STARTEND+6),(X'03',FULL),0                        
*                                                                               
         CLC   PWOOWYM,FULL        DO THE MONTHS MATCH?                         
         BNE   FOWP030              NO, FAST FORWARD TO NEXT B'CST MNTH         
*                                                                               
         LA    RF,ACCUTABD                                                      
FOWP022G LA    RF,ACCUTABQ(RF)                FAST FORWARD TO MNTH NTRY         
         TM    ATFLAG-ACCUTABD(RF),ATFMTTLQ                                     
         BZ    FOWP022G                                                         
         TM    ATFLAG2-ACCUTABD(RF),ATF2OOWP  CAN WE USE OOWP PAID$ ?           
         BZ    FOWP033                         NO, GO TO NXT PAID$ ELEM         
                                                                                
         DS    0H                  FILL IN OOW PAID DOLLARS                     
*&&DO                                                                           
         MVC   ATCLCK,PWOOWCG       NEW "LOCK" CLT GROSS                        
         MVC   ATWLCK,PWOOWWG       NEW "LOCK" WIM GROSS                        
         MVC   ATLKTAX,PWOOWTAX     WIM TAX                                     
         MVC   ATLKCTX,PWOOWCTX     CLT TAX                                     
                                                                                
         CLI   USETAX,C'Y'                                                      
         BE    FOWP024G                                                         
         L     R0,ATWLCK                                                        
         ICM   R1,15,PWOOWTAX                                                   
         SR    R0,R1                                                            
         ST    R0,ATWLCK                                                        
         L     R0,ATCLCK                                                        
         ICM   R1,15,PWOOWCTX                                                   
         SR    R0,R1                                                            
         ST    R0,ATCLCK                                                        
FOWP024G EQU   *                                                                
                                                                                
*&&                                                                             
         MVC   ATAJBUY,PWOOWCG     NEW "CURRENT" CLT GROSS                      
         MVC   ATACBUY,PWOOWWG     NEW "CURRENT" WIM GROSS                      
*                                                                               
FOWP026  DS    0H                  CLEAR LOCK $ IN REMAINING WEEKS              
         LA    R4,ACCUTABQ(R4)                                                  
         TM    ATFLAG,ATFMTTLQ            WHEN MONTH ENTRY REACHED,             
         BZ    *+16                                                             
         OI    ATFLAG2,ATF2OOWP            TURN ON FLAGS INDICATING             
         OI    ATFLAG2+ACCUTABQ,ATF2OOWP   THAT OOWR PAID$ WERE USED            
         B     FOWP026X                                                         
                                                                                
*&&DO                                                                           
         XC    ATCLCK,ATCLCK        CLT GROSS                                   
         XC    ATWLCK,ATWLCK        WIM GROSS                                   
*&&                                                                             
         XC    ATAJBUY,ATAJBUY      CLT GROSS                                   
         XC    ATACBUY,ATACBUY      WIM GROSS                                   
*&&DO                                                                           
         XC    ATLKTAX,ATLKTAX      WIM TAX                                     
         XC    ATLKCTX,ATLKCTX      CLT TAX                                     
*&&                                                                             
         B     FOWP026                                                          
FOWP026X EQU   *                                                                
         B     FOWP033                                                          
*                                                                               
FOWP030  DS    0H                                                               
         LA    R4,ACCUTABQ(R4)     FAST FORWARD TO NEXT B'CAST MONTH            
         TM    ATFLAG,ATFMTTLQ      MONTH LINE YET?                             
         BZ    *-8                   NOPE, KEEP BUMPING                         
         LA    R4,2*ACCUTABQ(R4)    BUMP PAST MONTHLY TOTALS LINE               
         B     FOWP022                                                          
*                                                                               
FOWP033  DS    0H                                                               
         BAS   RE,NEXTEL3          GET NEXT OOW EST PAID$ ELEMENT               
         BE    FOWP020                                                          
         DROP  R4,R6                                                            
FOWP039  EQU   *                                                                
                                                                                
*                                                                               
FOWPX    DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM43 - PW BILL (SUBR03--DOD#)'                               
*------------------- DO OUT-OF-WEEK ROTATOR DOLLARS ------------------*         
                                                                                
* Updates PWOOWEL elements for those months whose Adj DR/CR $ values            
*  were modified.                                                               
* At entry,                                                                     
*   R6-->PW record                                                              
                                                                                
DOOWRD   DS    0H                                                               
         LR    R3,R6               HOLD ONTO A(PW RECORD) IN R3                 
         USING PWRECD,R3                                                        
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
         MVC   DATADISP,=Y(PWEL-PWRECD)                                         
                                                                                
DOD010   DS    0H                                                               
         TM    ATFLAG,ATFSTTLQ     IF SKED ENTRY REACHED,                       
         BO    DODX                 GO EXIT                                     
                                                                                
DOD012   LA    R4,ACCUTABQ(R4)                                                  
         TM    ATFLAG,ATFMTTLQ+ATFBILAJ                                         
         BNO   DOD012                                                           
         TM    ATFLAG,ATFDRCRQ     WAS ADJ DR/CR MODIFIED?                      
         BO    DOD020               YES, UPDATE CURRENT $'s                     
         TM    ATFLAG2,ATF2CHDC    WAS ADJ DR/CR VALUE CHANGED?                 
         BO    DOD020               YES, UPDATE CURRENT $'s                     
         B     DODBUMP             ELSE, DON'T UPDATE CURRENT $'s               
                                                                                
*                                                                               
DOD020   DS    0H                                                               
         DS    0H                  CONVERT DATE TO BINARY FORM                  
         GOTO1 DATCON,DMCB,(X'02',ATMSTART),(0,MYDATE6),0                       
         GOTO1 AGETBROD,DMCB,(1,MYDATE6),STARTEND,AGETDAY,AADDAY                
         GOTO1 DATCON,DMCB,(0,STARTEND+6),(X'03',FULL),0                        
*                                                                               
         DS    0H                  GET PWOOWEL FOR MONTH                        
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWOOWCDQ',(R3)),             +        
               (L'PWOOWYM,FULL),0                                               
         L     R6,DMCB+12                                                       
         CLI   DMCB+12,0            PWOOWEL FOR MONTH EXISTS                    
         BE    DOD042                                                           
         CLI   DMCB+12,6            NO PWOOWEL FOR MONTH                        
         BE    DOD032                                                           
         DC    H'0'                                                             
                                                                                
                                                                                
*                                                                               
** PWOOWEL FOR Y/M DOES NOT EXIST **                                            
*                                                                               
DOD032   DS    0H                  BUILD NEW PWOOWEL ELEMENT                    
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING PWOOWEL,R6                                                       
         MVI   PWOOWCD,PWOOWCDQ     ELEM CODE                                   
         MVI   PWOOWLEN,PWOOWLNQ    ELEM LENGTH                                 
         MVC   PWOOWYM,FULL         YEAR/MONTH                                  
         DROP  R6                                                               
                                                                                
*                                                                               
         DS    0H                  FIND INSERTION POINT IN RECORD               
         LR    R6,R3                                                            
         AH    R6,DATADISP                                                      
         MVI   ELCODE,PWOOWCDQ                                                  
                                                                                
DOD036B  DS    0H                                                               
         BAS   RE,NEXTEL3           INSERT WITHIN PWOOWEL ELEMENTS              
         BNE   DOD038                                                           
         USING PWOOWEL,R6                                                       
         CLC   PWOOWYM,FULL          IN CHRONOLOGICAL ORDER                     
         BNH   DOD036B                                                          
         DROP  R6                                                               
         B     DOD050               GO INSERT PWOOWEL ELEM INTO RECORD          
*                                                                               
DOD038   DS    0H                                                               
         LR    R6,R3                INSERT BEFORE X'F1' ACTIVITY ELEM           
         MVI   ELCODE,X'F1'                                                     
         BAS   RE,GETEL3                                                        
         B     DOD050               GO INSERT PWOOWEL ELEM INTO RECORD          
                                                                                
                                                                                
*                                                                               
** PWOOWEL FOR Y/M EXISTS **                                                    
*                                                                               
DOD042   DS    0H                  R6-->PWOOWEL ELEMENT IN RECORD               
         USING PWOOWEL,R6                                                       
         CLI   PWOOWLEN,PWOOWLNQ    IS THE ELEMENT TOO SHORT?                   
         BNL   DOD062                NOPE                                       
*                                                                               
         DS    0H                   PWOOWEL ELEM TOO SHORT--EXPAND IT           
         XC    ELEMENT,ELEMENT                                                  
         ZIC   R1,PWOOWLEN                                                      
         BCTR  R1,0                                                             
         EXMVC R1,ELEMENT,PWOOWEL                                               
         MVI   ELEMENT+1,PWOOWLNQ                                               
         GOTO1 RECUP,DMCB,(R3),(R6)   DELETE OLD PWOOWEL                        
         B     DOD050                 GO ADD NEW PWOOWEL                        
         DROP  R6                                                               
                                                                                
                                                                                
*                                                                               
DOD050   DS    0H                  ADD PWOOWEL TO RECORD                        
         DS    0H                   R6-->INSERTION POINT                        
         DS    0H                   ELEMENT = PWOOWEL                           
         GOTO1 RECUP,DMCB,(R3),ELEMENT,(C'R',(R6))                              
         CLI   DMCB+8,C'R'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     DOD062               GO UPDATE ADJ DR/CR VALUE                   
                                                                                
                                                                                
*                                                                               
DOD062   DS    0H                                                               
         DS    0H                  R6-->PWOOWEL ELEMENT IN RECORD               
         USING PWOOWEL,R6                                                       
         MVC   PWOOWBIL,ATDRCR      UPDATE ADJ DR/CR VALUE                      
         DROP  R6                                                               
                                                                                
*                                                                               
         DS    0H                  PROCESS NEXT MONTH'S STUFF                   
         B     DODBUMP                                                          
                                                                                
                                                                                
DODBUMP  DS    0H                  BUMP TO NEXT MONTH                           
         LA    R4,2*ACCUTABQ(R4)    R4 MUST BE @ BILL ADJ ENTRY TO STRT         
         B     DOD010               NOW R4-->NEXT MONTH                         
                                                                                
                                                                                
DODX     DS    0H                                                               
         B     XIT_03                                                           
         DROP  R3,R4                                                            
         TITLE 'SPSFM43 - PW BILL (SUBR03--MISC STUFF)'                         
*--------------------- SUBR03 MISCELLANEOUS STUFF --------------------*         
                                                                                
GETEL3   DS    0H                  "GETEL3  R6,DATADISP,ELCODE"                 
         PRINT OFF                                                              
         AH    R6,DATADISP                                                      
         PRINT ON                                                               
FIRSTEL3 DS    0H                                                               
         PRINT OFF                                                              
         CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
         PRINT ON                                                               
NEXTEL3  DS    0H                                                               
         PRINT OFF                                                              
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL3                                                         
         PRINT ON                                                               
                                                                                
                                                                                
         TITLE 'SPSFM43 - PW BILL (SUBR03--LTORG && CONSTANTS)'                 
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
SUBR03L  EQU   *-SUBR03                                                         
         DS    0CL(X'1000'-SUBR03L+1)                                           
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM43 - PW BILL (SUBR04)'                                     
***********************************************************************         
*======================== SUBROUTINE POOL FOUR =======================*         
SUBR04Q  EQU   (((*-T21743+X'0FFF')/X'1000')*X'1000')                           
                                                                                
         ORG   T21743+SUBR04Q                                                   
SUBR04   NMOD1 0,**4304**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         L     R1,0(R1)                                                         
         SRL   R1,24               SHIFT TO LOW-ORDER BYTE                      
         SH    R1,=Y(R03#)          ADJUST FOR PREVIOUS ROUTINES,               
         BCTR  R1,0                 SUBTRACT ONE,                               
         SLL   R1,2                 AND MULTIPLY BY FOUR                        
         B     R04_00(R1)                                                       
                                                                                
GBT#     EQU   (R04_01-*)/4+R03#+1                                              
BPT#     EQU   (R04_02-*)/4+R03#+1                                              
IAC#     EQU   (R04_03-*)/4+R03#+1                                              
OKUPD#   EQU   (R04_04-*)/4+R03#+1                                              
                                                                                
R04_00   DS    0H                                                               
R04_01   B     GETBMTAB                                                         
R04_02   B     BPWTAB                                                           
R04_03   B     INITACTB                                                         
R04_04   B     OKUPDFIL                                                         
R04#     EQU   (*-R04_00)/4+R03#                                                
         DC    H'0'                                                             
         TITLE 'SPSFM43 - PW BILL (SUBR04--GBT#)'                               
*------------------------ GET BROADCAST MONTHS -----------------------*         
                                                                                
GETBMTAB DS    0H                                                               
*                                                                               
** BROADCAST WEEKS **                                                           
*                                                                               
         XC    BRDWKTAB,BRDWKTAB     GENERATE WEEKS OF SCHEDULE                 
         XC    BRDWKTB2,BRDWKTB2                                                
         MVI   BYTE,C'W'                                                        
         GOTO1 AMOBILE,DMCB,(14,ESDATE),(4,BRDWKTB2),MOBINPAD,SPOTPROF          
         MVC   NUMWEEKS,DMCB       SAVE # OF DATE-PAIRS GENERATED               
         LA    R2,BRDWKTB2                                                      
         LA    R3,BRDWKTAB                                                      
         BAS   RE,BLDBTAB          BUILD B'CAST WEEK TABLE FOR MY USE           
*                                                                               
** BROADCAST MONTHS **                                                          
*                                                                               
         DS    0H                  GENERATE MONTHS OF SCHEDULE                  
         L     R2,AIO3             USE IO3 FOR MOBILE OUTPUT                    
         XC    BRDMTHTB,BRDMTHTB     GENERATE MONTHS OF SCHEDULE                
         LA    R3,BRDMTHTB                                                      
         MVI   BYTE,C'M'                                                        
         MVC   WORK(6),ESDATE      WORK = START DATE OF EST (EBCDIC)            
                                                                                
         ZIC   RE,NUMWEEKS         RE=# OF DATE-PAIRS GENERATED                 
         BCTR  RE,0                                                             
         MH    RE,=Y(BCSTTABQ)                                                  
         LA    RE,BRDWKTAB(RE)     RE-->LAST DATE PAIR                          
         USING BCSTTABD,RE                                                      
         CLI   BCSTTABQ(RE),XFF     SHOULD BE FOLLOWED BY X'FF'                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   HALF,BCSEND         HALF = BRDCST WEEK END-DATE (CMPRSS)         
         DROP  RE                                                               
                                                                                
         TM    ESTFLAG,EFBILEST    IF THIS IS ONLY ONE PERIOD, CONSIDER         
         BZ    GBT10                THE ENTIRE PERIOD AS ONE MONTH              
         GOTO1 DATCON,DMCB,(0,WORK),(2,(R2)),0     START OF "MONTH"             
         MVC   2(2,R2),HALF                        END   OF "MONTH"             
         MVI   4(R2),XFF                           EOTABLE MARKER               
         BAS   RE,BLDBTAB          BUILD BRDMTHTB FOR MYSELF                    
         B     GBTX                                                             
*                                                                               
GBT10    GOTO1 DATCON,DMCB,(2,HALF),(0,WORK+6),0                                
                                                                                
         MVC   ESTOOWSD,SPOTPROF+8     SAVE PROFILE SETTING                     
         MVI   SPOTPROF+8,0            CLEAR IT TO GET B'CST MONTHS             
         GOTO1 AMOBILE,DMCB,(5,WORK),(0,(R2)),MOBINPAD,SPOTPROF                 
         MVC   SPOTPROF+8(1),ESTOOWSD  RESTORE PROFILE SETTINGS                 
                                                                                
GBT014   DS    0H                                                               
*&&DO                                                                           
         TM    ESTFLAG,EFOWPW      OUT-OF-WEEK PW BILLING ESTIMATE:             
         BZ    GBT014X                                                          
                                                                                
*&&                                                                             
         CLC   2(2,R2),BRDWKTAB+2  B'CST MNTH END VS. 1ST WK'S END DATE         
         BNL   GBT014X                                                          
         LA    R2,4(R2)            IF LOWER, BUMP TO NEXT MONTH                 
         CLI   0(R2),XFF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     GBT014              GO BACK AND CHECK AGAIN                      
GBT014X  EQU   *                                                                
                                                                                
         BAS   RE,BLDBTAB          BUILD BRDMTHTB FOR MYSELF                    
         B     GBTX                                                             
                                                                                
GBTX     DS    0H                                                               
         J     XIT                                                              
                                                                                
                                                                                
         DS    0H                  BLD B'CAST TABLES FOR MY USE                 
BLDBTAB  NTR1                                                                   
         USING BCSTTABD,R3                                                      
BLDBTABA CLI   0(R2),XFF           R2-->MOBILE OUTPUT, R3-->MY TABLE            
         BE    BLDBTABX                                                         
                                                                                
         MVC   BCSSTART(4),0(R2)    MOVE IN START & END DATES                   
         CLI   BYTE,C'M'                                                        
         BNE   *+8                                                              
         OI    BCSFLAG,BCSFMXQ     TURN FLAG ON FOR MONTH TABLE                 
                                                                                
         LA    R2,4(R2)                                                         
         LA    R3,BCSTTABQ(R3)                                                  
         B     BLDBTABA                                                         
         DROP  R3                                                               
*                                                                               
BLDBTABX MVI   0(R3),XFF           EOTABLE MARKER                               
         J     XIT                 EXIT BLDBTAB ROUTINE                         
                                                                                
         TITLE 'SPSFM43 - PW BILL (SUBR04--BPT#)'                               
*--------------------------- BUILD PW% TABLE -------------------------*         
                                                                                
BPWTAB   DS    0H                                                               
         MVI   ELCODE,PWWKCODQ                                                  
         MVC   DATADISP,=Y(PWEL-PWFKEY)                                         
                                                                                
         LA    R3,BRDWKTAB         R3-->WEEK DATES OF SCHEDULE                  
         TM    ESTFLAG,EFBILEST                                                 
         BZ    *+8                                                              
         LA    R3,BRDMTHTB         USE PERIOD DATES IF ONE PERIO                
         LA    R4,PWTAB                                                         
         XC    PWTAB,PWTAB                                                      
                                                                                
         USING BCSTTABD,R3                                                      
BPT10    MVC   TEMPPW,OPWPCT       SET DEFAULT                                  
         L     R6,AIO               IN CASE NOT FOUND IN RECORD                 
         BRAS  RE,GETEL                                                         
         B     BPT20B                                                           
BPT20A   BRAS  RE,NEXTEL                                                        
BPT20B   BNE   BPT30                                                            
                                                                                
         USING PWWKEL,R6                                                        
         CLC   PWWKDATE,BCSSTART   GET ELEM FOR CORRESPONDING WEEK              
*&&DO                                                                           
         BNE   BPT20A                                                           
*&&                                                                             
         BL    BPT20A                                                           
         MVC   TEMPPW,PWWKPCT       AND GET PW% FROM IT                         
         DROP  R6                                                               
                                                                                
BPT30    MVC   0(4,R4),BCSSTART    DATE FOR PW TABLE                            
         MVC   4(4,R4),TEMPPW      PW%  FOR PW TABLE                            
         LA    R4,8(R4)            BUMP POINTER IN PWTAB                        
         LA    R3,BCSTTABQ(R3)     BUMP POINTER IN BRDWKTAB/BRDMTHTB            
         CLI   0(R3),XFF                                                        
         BNE   BPT10                                                            
         DROP  R3                                                               
                                                                                
         MVI   0(R4),XFF                                                        
         J     XIT                                                              
         TITLE 'SPSFM43 - PW BILL (SUBR04--IAC#)'                               
*----------------------- INITIALIZE ACCUM TABLE ----------------------*         
                                                                                
*  FIRST WE GET DATES OF ALL WEEKS IN PERIOD                                    
*   THEN WE FILL IN THE PROFIT WITHIN %                                         
                                                                                
INITACTB DS    0H                                                               
*                                                                               
** INITIALIZE TABLES **                                                         
*                                                                               
         L     RE,AACCUTAB                                                      
         LA    RF,L'ACCUTAB                                                     
         XCEF                                                                   
*                                                                               
         MVI   GOSUBN,TSR_INI#            INITIALIZE STATION                    
         GOTO1 AGOSUB                      ACCUMULATOR TABLE                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
** SET WEEK & MONTH DATES INTO ACCUTAB **                                       
*                                                                               
         DS    0H                  INITIALIZE TABLE POINTERS                    
         LA    R3,BRDMTHTB                                                      
         USING BCSTTABD,R3                                                      
         L     R4,AACCUTAB                                                      
         USING ACCUTABD,R4                                                      
         LA    R6,BRDWKTAB                                                      
         TM    ESTFLAG,EFBILEST    IF ONE PW PERIOD,                            
         BZ    *+8                                                              
         LA    R6,BRDMTHTB          USE MONTH IN PLACE OF WEEKS                 
         MVC   SDATE2,0(R6)        SAVE START DATE OF 1ST BDCST WEEK            
*                                                                               
IAC10    DS    0H                                                               
         CLC   BCSEND,2(R6)        B'CST MNTH END VS. WEEK'S END DATE           
         BNL   IAC013               NOT LOW: FILL IN WEEKS                      
                                                                                
IAC10A   DS    0H                   YEP, PUT BILL ADJ & TOTALS ENTRY            
         MVI   BYTE,ATFMTTLQ+ATFBILAJ                                           
IAC10B   DS    0H                                                               
         MVC   ATMONTH,BCSSTART                                                 
         MVC   ATWSTART,SDATE2                                                  
         MVC   ATWEND,EDATE2                                                    
         OC    ATFLAG,BYTE                                                      
         LA    R4,ACCUTABQ(R4)                                                  
         CLI   BYTE,ATFMTTLQ                                                    
         BE    IAC10C                                                           
         NI    BYTE,XFF-ATFBILAJ                                                
         B     IAC10B                                                           
                                                                                
IAC10C   DS    0H                                                               
         XC    SDATE2,SDATE2         GET NEXT MONTH'S 1ST BDCST                 
         CLI   0(R6),XFF              WEEK START DATE                           
         BE    *+10                                                             
         MVC   SDATE2,0(R6)                                                     
         NI    BCSFLAG,XFF-BCSFMXQ   MONTH IS NOT EFFECTIVE EOTABLE             
                                                                                
         LA    R3,BCSTTABQ(R3)     BUMP MONTH POINTER                           
         CLI   0(R6),XFF           WAS THIS THE LAST NTRY?                      
         BE    IAC16                YEP, DO NEXT STEP                           
         B     IAC10                                                            
                                                                                
*                                                                               
IAC013   DS    0H                                                               
         MVC   ATMONTH,BCSSTART    START-END DATES OF BRDCST MNTH               
         MVC   ATWEEK,0(R6)        START-END DATES OF BRDCST WEEK               
         LA    R4,ACCUTABQ(R4)     BUMP ACCUTAB POINTER                         
         MVC   EDATE2,2(R6)        REMEMBER END DATE OF WEEK                    
         LA    R6,BCSTTABQ(R6)     BUMP WEEK POINTER                            
         CLI   0(R6),XFF           WAS THIS THE LAST NTRY?                      
         BE    IAC10A               YEP, DO NEXT STEP                           
         B     IAC10                                                            
                                                                                
IAC16    DS    0H                  DO SKED ENTRIES                              
         MVC   DATE4(2),BRDMTHTB   GET START AND                                
         MVC   DATE4+2(2),EDATE2    END DATES OF PERIOD (WEEKS)                 
         SH    R3,=Y(BCSTTABQ)                                                  
         MVC   HALF,BCSEND          END DATES OF PERIOD (MNTHS)                 
                                                                                
         MVI   BYTE,ATFSTTLQ+ATFBILAJ                                           
IAC16A   MVC   ATMSTART,DATE4      BDCST MONTH START                            
         MVC   ATMEND,HALF          AND END DATES                               
         MVC   ATWEEK,DATE4        BDCST WEEK STRT/END DATES                    
         OC    ATFLAG,BYTE                                                      
         LA    R4,ACCUTABQ(R4)                                                  
         CLI   BYTE,ATFSTTLQ                                                    
         BE    IAC16B                                                           
         NI    BYTE,XFF-ATFBILAJ                                                
         B     IAC16A                                                           
                                                                                
IAC16B   DS    0H                                                               
         MVI   0(R4),XFF           EOTABLE MARKER                               
         DROP  R3                                                               
*                                                                               
** FILL IN THE PROFIT WITHIN %'S **                                             
*                                                                               
         L     R4,AACCUTAB                                                      
         LA    R6,PWTAB                                                         
IAC40    TM    ATFLAG,ATFSTTLQ     IF SCHED TOTALS ENTRY,                       
         BO    IAC40X               THEN WE'RE FINISHED                         
         TM    ATFLAG,ATFMTTLQ     IF MONTHLY TOTALS ENTRY,                     
         BO    IAC40D               THEN DON'T FILL IN PW% YET                  
                                                                                
         MVC   ATPW,4(R6)          MOVE IN  PW%, AND                            
         LA    R6,8(R6)             BUMP TO NEXT ENTRY IN PWTAB                 
                                                                                
IAC40D   LA    R4,ACCUTABQ(R4)     DO NEXT ENTRY                                
         B     IAC40                                                            
*                                                                               
IAC40X   CLI   0(R6),XFF           BETTER BE AT EOTABLE OF PWTAB                
         BE    IAC50                                                            
         DC    H'0'                                                             
*                                                                               
** SET OTHER FLAGS **                                                           
*                                                                               
IAC50    L     R4,AACCUTAB                                                      
IAC50A   OI    ATFLAG,ATFNOSPT     ASSUME NO SPOTS                              
         TM    ATFLAG,ATFSTTLQ                                                  
         BO    IAC50X                                                           
         LA    R4,ACCUTABQ(R4)                                                  
         B     IAC50A                                                           
IAC50X   DS    0H                                                               
         B     IAC60                                                            
                                                                                
*                                                                               
IAC60    DS    0H                                                               
         B     IACX                                                             
*                                                                               
IACX     DS    0H                                                               
         J     XIT                                                              
         DROP  R4                                                               
         TITLE 'SPSFM43 - PW BILL (SUBR04--OKUPD#)'                             
*------------------------ OKAY TO UPDATE FILE? -----------------------*         
                                                                                
*  Routine attempts to catch any potential errors during execution.             
*   Motivation behind this is to catch them BEFORE the program updates          
*   the file instead of during.                                                 
                                                                                
OKUPDFIL DS    0H                                                               
*                                                                               
** CHECK STATION ACCUMULATOR TABLE **                                           
*                                                                               
         DS    0H                                                               
         ZICM  R4,TSARBLK+((TSAREC+1)-TSARD),(7)                                
         BZ    OKUPDXN                                                          
                                                                                
*                                                                               
         USING STACRECD,R4                                                      
         XC    STACRECD(STACRECL),STACRECD                                      
         MVI   GOSUBN,TSR_RDH#                                                  
*                                                                               
OKUPST10 DS    0H                  LOOP THROUGH STTN ACCUM TABLE                
         GOTO1 AGOSUB                                                           
         TM    MYTSERRS,TSEEOF      IF WE REACHED THIS,                         
         BZ    OKUPSTX               STTN ACCUM TABLE SEEMS OKAY                
         CLI   MYTSERRS,0           IF WE GET ANY OTHER ERRORS,                 
         BE    OKUPDXN               SOMETHING'S WRONG                          
         OC    STACSTA,STACSTA      IF STATION CODE IS NULLS,                   
         BZ    OKUPDXN               SOMETHING'S WRONG                          
         MVI   GOSUBN,TSR_NXT#                                                  
         B     OKUPST10                                                         
*                                                                               
OKUPSTX  EQU   *                                                                
                                                                                
*                                                                               
** EXITS **                                                                     
*                                                                               
OKUPDXY  DS    0H                                                               
         J     YES                                                              
*                                                                               
OKUPDXN  DS    0H                                                               
         J     NO                                                               
         TITLE 'SPSFM43 - PW BILL (SUBR04--MISC STUFF)'                         
*--------------------- SUBR04 MISCELLANEOUS STUFF --------------------*         
                                                                                
                                                                                
         TITLE 'SPSFM43 - PW BILL (SUBR04--LTORG && CONSTANTS)'                 
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
SUBR04L  EQU   *-SUBR04                                                         
         DS    0CL(X'1000'-SUBR04L+1)                                           
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM43 - PW BILL (SUBRXM)'                                     
***********************************************************************         
*==================== EXIT WITH MESSAGE ROUTINES =====================*         
                                                                                
* Exits which leave T21743 entirely and displays a message go through           
*  this routine.                                                                
                                                                                
*&&DO                                                                           
SUBRXMQ  EQU   ((((*-T21743)/4096)+1)*4096)                                     
*&&                                                                             
SUBRXMQ  EQU   ((((*-T21743)/X'100')+1)*X'100')                                 
                                                                                
         ORG   T21743+SUBRXMQ                                                   
XMSGRTN  NMOD1 0,**43XM**                                                       
         SR    RC,RC                                                            
         ICM   RC,7,1(R1)                                                       
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         XC    CONHEAD,CONHEAD     CLEAR THE WAY FOR THE MESSAGE                
                                                                                
         CLI   0(R1),C'E'          EXIT W/ AN ERROR MSG                         
         BE    XMERR                                                            
         CLI   0(R1),C'W'          EXIT W/ A WARNING MSG                        
         BE    XMWRN                                                            
         CLI   0(R1),C'I'          EXIT W/ AN INFO  MSG                         
         BE    XMINF                                                            
         DC    H'0'                                                             
                                                                                
                                                                                
XIT_XM   XIT1                                                                   
         TITLE 'SPSFM43 - PW BILL (SUBRXM-ERR MSGS)'                            
*--------------------------- ERROR MESSAGES --------------------------*         
                                                                                
* At entry, R2-->field in error.                                                
                                                                                
XMERR    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         OI    MISCFLG1,MF1ERRQ                                                 
         OI    6(R2),X'81'         FIELD IN ERROR IS MODIFIED FOR NEXT          
         CLI   MYERRCD,PWNAVQ                                                   
         BE    XMERR2                                                           
         MVI   GOSUBN,STI#         SAVE OFF TIA                                 
         GOTO1 AGOSUB                                                           
                                                                                
XMERR2   DS    0H                                                               
         MVC   AERREX,ERREX        SET ADDRESS OF ERREX ROUTINE                 
         CLI   MYERRCD,ERRX#                                                    
         BL    XMERRGO                                                          
         MVC   AERREX,ERREX2        TO GO OFF TO                                
         MVC   CONHEAD(9),=C'**ERROR**'                                         
         LA    R1,CONHEAD+10                                                    
         CLI   MYERRCD,ERRX2#                                                   
         BL    XMERRGO                                                          
         DC    H'0'                                                             
                                                                                
XMERRGO  DS    0H                                                               
         CLI   MYERRCD,0                                                        
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   MYERRCD,XMERRQ                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   RF,MYERRCD          BRANCH OFF TO SET ERROR MESSAGE              
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMERR00(RF)                                                      
                                                                                
MFLDQ    EQU   ((XMERR01-XMERR00)/4)+1                                          
IFLDQ    EQU   ((XMERR02-XMERR00)/4)+1                                          
RNFQ     EQU   ((XMERR03-XMERR00)/4)+1                                          
INVOQ    EQU   ((XMERR04-XMERR00)/4)+1                                          
DUPOQ    EQU   ((XMERR05-XMERR00)/4)+1                                          
IOPDQ    EQU   ((XMERR06-XMERR00)/4)+1                                          
MPCTQ    EQU   ((XMERR07-XMERR00)/4)+1                                          
NLKSTAQ  EQU   ((XMERR08-XMERR00)/4)+1                                          
EAJB0Q   EQU   ((XMERR09-XMERR00)/4)+1                                          
BPNLQ    EQU   ((XMERR10-XMERR00)/4)+1                                          
BAJZEROQ EQU   ((XMERR11-XMERR00)/4)+1                                          
ICCOVRDQ EQU   ((XMERR12-XMERR00)/4)+1                                          
EMKTQ    EQU   ((XMERR13-XMERR00)/4)+1                                          
EAJBPWQ  EQU   ((XMERR14-XMERR00)/4)+1                                          
NOACBQ   EQU   ((XMERR15-XMERR00)/4)+1                                          
NMODLCKQ EQU   ((XMERR16-XMERR00)/4)+1                                          
NMODPLKQ EQU   ((XMERR17-XMERR00)/4)+1                                          
NLKNTXQ  EQU   ((XMERR18-XMERR00)/4)+1                                          
NPOLQ    EQU   ((XMERR19-XMERR00)/4)+1                                          
PWNAVQ   EQU   ((XMERR20-XMERR00)/4)+1                                          
TBOFQ    EQU   ((XMERR21-XMERR00)/4)+1                                          
SPDATQ   EQU   ((XMERR22-XMERR00)/4)+1                                          
BHDATQ   EQU   ((XMERR30-XMERR00)/4)+1                                          
IPRD2Q   EQU   ((XMERR31-XMERR00)/4)+1                                          
XCUERQ   EQU   ((XMERR32-XMERR00)/4)+1                                          
                                                                                
OUTRNGEQ EQU   ((XMERR23-XMERR00)/4)+1                                          
OUTRNG2Q EQU   ((XMERR24-XMERR00)/4)+1                                          
EHIPWQ   EQU   ((XMERR25-XMERR00)/4)+1                                          
ELOPWQ   EQU   ((XMERR26-XMERR00)/4)+1                                          
NMDDCPWQ EQU   ((XMERR27-XMERR00)/4)+1                                          
NDRCRQ   EQU   ((XMERR28-XMERR00)/4)+1                                          
INBLNKQ  EQU   ((XMERR29-XMERR00)/4)+1                                          
*                                                                               
XMERR00  DS    0H                                                               
XMERR01  B     MFLD                MISSING INPUT FIELD                          
XMERR02  B     IFLD                INVALID INPUT FIELD                          
XMERR03  B     RNF                 RECORD NOT FOUND                             
XMERR04  B     INVO                INVALID OPTION KEYWORD                       
XMERR05  B     DUPO                DUPLICATE OPTION SPECIFIED                   
XMERR06  B     IOPD                INVALID OPTION DATA VALUE                    
XMERR07  B     MPCT                MISSING PW%                                  
XMERR08  B     NLKSTA              CAN'T LOCK ON STATION LEVEL                  
XMERR09  B     EAJB0               CLCOST CAN'T BE 0 WHEN WIMCOST<>0            
XMERR10  B     BPNL                BUY/PW NOT LOCKED YET                        
XMERR11  B     BAJZERO             ONLY VALID INPUT FOR BLL ADJ IS 0            
XMERR12  B     ICCOVRD             INVALID OVVERID INPUT                        
XMERR13  B     EMKT                INVALID MARKET CODE                          
XMERR14  B     EAJBPW              CAN'T ENTER CLCOST & PW% AT SAME             
XMERR15  B     NOACB               CAN'T HAVE CLCOST W/O WIMCOST                
XMERR16  B     NMODLCK             CAN'T LOCK/UNLOCK BUY AND MODIFY             
XMERR17  B     NMODPLK             CAN'T LOCK/UNLOCK PW  AND MODIFY             
XMERR18  B     NLKNTX              CAN'T XFR COSTS WHEN TAX=NO                  
XMERR19  B     NPOL                PRODUCT CAN'T BE POL                         
XMERR20  B     PWNAV               PW FUNCTIONS NOT AVAIL TO BUY SVCE           
XMERR21  B     TBOF                TABLE OVERFLOW.  PLEASE CONTACT DDS          
XMERR22  B     SPDAT               SPOT DATE (&T) NOT IN ESTIMATE PERIO         
XMERR30  B     BHDAT               YR/MTH OF SERVICE (&T) NOT IN ESTIMA         
XMERR31  B     IPRD2               INVALID PRODUCT CODE &T                      
XMERR32  B     XCUER               EXECUTION ERROR AT X&T.  PLS ... DDS         
ERRX#    EQU   ((*-XMERR00)/4)+1                                                
                                                                                
XMERR23  B     OUTRNGE                                                          
XMERR24  B     OUTRNG2                                                          
XMERR25  B     EHIPW                                                            
XMERR26  B     ELOPW                                                            
XMERR27  B     NMDDCPW                                                          
XMERR28  B     NDRCR                                                            
XMERR29  B     INBLNK                                                           
ERRX2#   EQU   ((*-XMERR00)/4)+1                                                
                                                                                
XMERRQ   EQU   ((*-XMERR00)/4)+1                                                
         EJECT                                                                  
                                                                                
MFLD     MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
IFLD     MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
RNF      DS    0H                  RECORD NOT FOUND                             
         MVI   ERROR,NOTFOUND                                                   
         B     ERREXIT                                                          
*                                                                               
INVO     MVC   MSGNUM2,=H'206'     INVALID OPTION KEYWORD                       
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
DUPO     MVC   MSGNUM2,=H'208'     DUPLICATE OPTION SPECIFIED                   
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
IOPD     MVC   MSGNUM2,=H'209'     INVALID OPTION DATA VALUE                    
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
MPCT     MVC   MSGNUM2,=AL2(SE#MSSPW)   MISSING PW%                             
         B     ERRGTXT                                                          
*                                                                               
NLKSTA   MVC   MSGNUM2,=AL2(SE#NLSTL)   CAN'T LOCK ON STATION LEVEL             
         B     ERRGTXT                                                          
*                                                                               
EAJB0    MVC   MSGNUM2,=AL2(SE#CLCN0)   CLCOST NO 0 WHEN WIMCOST<>0             
         B     ERRGTXT                                                          
*                                                                               
BPNL     MVC   MSGNUM2,=AL2(SE#BPNLK)   BUY/PW NOT LOCKED YET                   
         B     ERRGTXT                                                          
*                                                                               
BAJZERO  DS    0H                                                               
         MVC   MSGNUM2,=AL2(SE#VIBA0)   ONLY VALID INPUT FOR BLL ADJ 0          
         B     ERRGTXT                                                          
*                                                                               
ICCOVRD  DS    0H                                                               
         MVC   MSGNUM2,=AL2(SE#IOVRD)   INVALID OVERRIDE INPUT                  
         B     ERRGTXT                                                          
*                                                                               
EMKT     MVC   MSGNUM2,=AL2(SE#INVMK)   INVALID MARKET CODE                     
         B     ERRGTXT                                                          
*                                                                               
EAJBPW   MVC   MSGNUM2,=AL2(SE#NCPSM)   CAN'T ENTER CLCOST & PW%                
         B     ERRGTXT                                                          
*                                                                               
NOACB    MVC   MSGNUM2,=AL2(SE#NCWOW)   CAN'T HAVE CLCOST W/O WIMCOST           
         B     ERRGTXT                                                          
*                                                                               
NMODLCK  MVC   MSGNUM2,=AL2(SE#NMBLK)   CAN'T LOCK/UNLOCK BUY & MODFY           
         B     ERRGTXT                                                          
*                                                                               
NMODPLK  MVC   MSGNUM2,=AL2(SE#NMPLK)   CAN'T LOCK/UNLOCK PW  & MODFY           
         B     ERRGTXT                                                          
*                                                                               
NLKNTX   MVC   MSGNUM2,=AL2(SE#NXTXN)   CAN'T XFR COSTS WHEN TAX=NO             
         B     ERRGTXT                                                          
*                                                                               
NPOL     MVC   MSGNUM2,=AL2(SE#NOPOL)   PRODUCT CAN'T BE POL                    
         B     ERRGTXT                                                          
*                                                                               
PWNAV    MVC   MSGNUM2,=AL2(SE#PWNAV)   PW FUNCTIONS NOT AVAILABLE              
         B     ERRGTXT                                                          
*                                                                               
TBOF     DS    0H                  TABLE OVERFLOW.  PLEASE CONTACT DDS          
         MVC   MSGNUM2,=H'65'                                                   
         MVI   MSGSYS,GTGENSYS                                                  
         LA    R0,PWBMKTH                                                       
         ST    R0,ACURFORC                                                      
         B     ERRGTXT                                                          
*                                                                               
SPDAT    DS    0H                  &T SPOT NOT IN ESTIMATE PERIOD               
         MVC   MSGNUM2,=AL2(SE#SPDT2)                                           
         LA    R0,PWBMKTH                                                       
         ST    R0,ACURFORC                                                      
         B     ERRGTXT                                                          
*                                                                               
BHDAT    DS    0H                  YR/MTH OF SERVICE (&T) NOT IN ESTIMA         
         MVC   MSGNUM2,=AL2(SE#YMNEP)                                           
         LA    R0,PWBMKTH                                                       
         ST    R0,ACURFORC                                                      
         B     ERRGTXT                                                          
*                                                                               
IPRD2    DS    0H                  INVALID PRODUCT CODE &T                      
         MVC   MSGNUM2,=AL2(SE#INVP2)                                           
         B     ERRGTXT                                                          
*                                                                               
XCUER    DS    0H                  INVALID PRODUCT CODE &T                      
         MVC   MSGNUM2,=AL2(GE$XCUER)                                           
         MVI   MSGSYS,GTGENSYS                                                  
         L     R0,ALOCERR                                                       
         S     R0,BASE1                                                         
         BP    *+6                                                              
         DC    H'0'                                                             
         ST    R0,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL+1,MYTEXT+1,L'FULL-1,=C'TOG'                     
         ICM   R0,15,DMCB+16                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R0,MYTEXT+0                                                      
         B     ERRGTXT                                                          
*                                                                               
OUTRNGE  LR    R3,R1                                                            
         MVC   0(18,R3),=C'PW% not in range ('                                  
         L     RE,=A(MINPWQ)                                                    
         EDIT  (RE),(7,18(R3)),2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                
         AR    R3,R0                                                            
         MVC   18(4,R3),=C'%...'                                                
         LA    R3,18+4(R3)                                                      
         LH    RE,=Y(MAXPWQ)                                                    
         EDIT  (RE),(6,0(R3)),2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=+                 
         AR    R3,R0                                                            
         MVC   0(2,R3),=C'%)'                                                   
         MVI   MYERRCD,0           DON'T FORCE USER TO HIT PF9                  
         B     ERREXIT                                                          
*                                                                               
OUTRNG2  LR    R3,R1                                                            
         MVC   0(31,R3),=C'CLCOST makes PW% out of range ('                     
         L     RE,=A(MINPWQ)                                                    
         EDIT  (RE),(7,31(R3)),2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                
         AR    R3,R0                                                            
         MVC   31(4,R3),=C'%...'                                                
         LA    R3,31+4(R3)                                                      
         LH    RE,=Y(MAXPWQ)                                                    
         EDIT  (RE),(6,0(R3)),2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=+                 
         AR    R3,R0                                                            
         MVC   0(2,R3),=C'%)'                                                   
         MVI   MYERRCD,0           DON'T FORCE USER TO HIT PF9                  
         B     ERREXIT                                                          
*                                                                               
EHIPW    DS    0H                                                               
         LR    R3,R1                                                            
         MVC   0(34,R3),=C'Value makes PW% greater than max ('                  
         L     RE,=A(MAXPWQ)                                                    
         EDIT  (RE),(7,34(R3)),2,ALIGN=LEFT                                     
         AR    R3,R0                                                            
         MVC   34(2,R3),=C'%)'                                                  
         MVI   MYERRCD,0           DON'T FORCE USER TO HIT PF9                  
         B     ERREXIT                                                          
*                                                                               
ELOPW    DS    0H                                                               
         LR    R3,R1                                                            
         MVC   0(34,R3),=C'Value makes PW% smaller than min ('                  
         L     RE,=A(MINPWQ)                                                    
         EDIT  (RE),(7,34(R3)),2,ALIGN=LEFT,FLOAT=-                             
         AR    R3,R0                                                            
         MVC   34(2,R3),=C'%)'                                                  
         MVI   MYERRCD,0           DON'T FORCE USER TO HIT PF9                  
         B     ERREXIT                                                          
*                                                                               
NMDDCPW  DS    0H                                                               
         MVC   0(43,R1),=C'Can''t modify the DR/CR && PW% simultaneousl+        
               y'                                                               
         MVI   MYERRCD,0           DON'T FORCE USER TO HIT PF9                  
         B     ERREXIT                                                          
*                                                                               
NDRCR    DS    0H                                                               
         MVC   0(29,R1),=C'Can''t change existing billing'                      
         MVI   MYERRCD,0           DON'T FORCE USER TO HIT PF9                  
         B     ERREXIT                                                          
*                                                                               
INBLNK   DS    0H                                                               
         MVC   0(22,R1),=C'Input can not be blank'                              
         MVI   MYERRCD,0           DON'T FORCE USER TO HIT PF9                  
         B     ERREXIT                                                          
         EJECT                                                                  
ERRGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMERR        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MYTEXT,0            ANY REPLACE TEXT?                            
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT        YES, PUT LENGTH IN                          
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT           AS WELL AS THE ADDR OF THE TEXT            
         DROP  R1                                                               
         MVI   ERROR,0                                                          
         B     ERREXIT                                                          
                                                                                
                                                                                
ERREXIT  DS    0H                                                               
         LR    R0,R2               SAVE DISPL OF FIELD                          
         S     R0,ATWA                                                          
         STH   R0,PRVFDSP                                                       
         OC    ACURFORC,ACURFORC   WANT TO FORCE CURSOR ELSEWHERE?              
         BZ    *+8                                                              
         L     R2,ACURFORC          YEP                                         
                                                                                
         GOTO1 AERREX                                                           
         B     XIT_XM                                                           
         TITLE 'SPSFM43 - PW BILL (SUBRXM-WRN MSGS)'                            
*-------------------------- WARNING MESSAGES -------------------------*         
                                                                                
* At entry, R2-->PWMMEDH.                                                       
                                                                                
XMWRN    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         MVI   GOSUBN,STI#         SAVE OFF TIA                                 
         GOTO1 AGOSUB                                                           
                                                                                
         MVC   AERREX,ERREX        SET ADDRESS OF ERREX ROUTINE                 
         CLI   MYWRNCD,WRNX#                                                    
         BL    XMWRNGO                                                          
         LA    R1,CONHEAD                                                       
         MVC   AERREX,ERREX2        TO GO OFF TO                                
         CLI   MYWRNCD,WRNX2#                                                   
         BL    XMWRNGO                                                          
         DC    H'0'                                                             
                                                                                
XMWRNGO  DS    0H                                                               
         CLI   MYWRNCD,0                                                        
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   MYWRNCD,XMWRNQ                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   RF,MYWRNCD          BRANCH OFF TO SET WARNING MESSAGE            
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMWRN00(RF)                                                      
                                                                                
RSLMKTQ  EQU   ((XMWRN04-XMWRN00)/4)+1                                          
WECSTQ   EQU   ((XMWRN01-XMWRN00)/4)+1                                          
                                                                                
XMWRN00  DS    0H                                                               
XMWRN04  B     RSLMKT              BUY HAS BEEN CHANGED, RE-STTN LK MKT         
WRNX#    EQU   ((*-XMWRN00)/4)+1                                                
                                                                                
XMWRN01  B     WECST               EXCEEDED CAPACITY OF STATION TABLE           
WRNX2#   EQU   ((*-XMWRN00)/4)+1                                                
                                                                                
XMWRNQ   EQU   ((*-XMWRN00)/4)+1                                                
         EJECT                                                                  
RSLMKT   DS    0H                  BUY HAS BEEN CHANGED, RE-STTN LK MKT         
         MVC   MSGNUM2,=AL2(SW#RSLMK)                                           
         LA    R0,PWMRQIDH                                                      
         ST    R0,ACURFORC                                                      
         B     WRNGTXT                                                          
*                                                                               
WECST    DS    0H                                                               
         MVC   CONHEAD(34),=C'Exceeded capacity of STATION table'               
         MVC   ACURFORC,AFRSTKEY                                                
         OI    PWBMEDH+6,X80+X01   FORCE KEY CHANGE FOR NEXT TIME               
         B     WRNEXIT                                                          
         EJECT                                                                  
WRNGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMWRN        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MYTEXT,0             ANY REPLACE TEXT?                           
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT         YES, PUT LENGTH IN                         
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT            AS WELL AS THE ADDR OF THE TEXT           
         DROP  R1                                                               
         B     WRNEXIT                                                          
                                                                                
                                                                                
WRNEXIT  DS    0H                                                               
         GOTO1 AERREX                                                           
         B     XIT_XM                                                           
         TITLE 'SPSFM43 - PW BILL (SUBRXM-INF MSGS)'                            
*---------------------------- INFO MESSAGES --------------------------*         
                                                                                
* At entry, R2-->appropriate field to put cursor on.                            
                                                                                
XMINF    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         XC    ACURFORC,ACURFORC                                                
                                                                                
         CLI   MYINFCD,0                                                        
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   MYINFCD,XMINFQ                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         DS    0H                                                               
         MVC   AERREX,ERREX        SET ADDRESS OF ERREX ROUTINE                 
         CLI   MYINFCD,INFX#                                                    
         BL    XMINFGO                                                          
         LA    R1,CONHEAD                                                       
         MVC   AERREX,ERREX2                                                    
         CLI   MYINFCD,INFX2#                                                   
         BL    XMINFGO                                                          
         DC    H'0'                                                             
                                                                                
XMINFGO  DS    0H                                                               
         ZIC   RF,MYINFCD          BRANCH OFF TO SET WARNING MESSAGE            
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMINF00(RF)                                                      
                                                                                
RSTCHQ   EQU   ((XMINF01-XMINF00)/4)+1                                          
BALL0Q   EQU   ((XMINF02-XMINF00)/4)+1                                          
NBILGENQ EQU   ((XMINF03-XMINF00)/4)+1                                          
BMUPSQ   EQU   ((XMINF04-XMINF00)/4)+1                                          
                                                                                
XMINF00  DS    0H                                                               
XMINF04  B     BMUPS                                                            
INFX#    EQU   ((*-XMINF00)/4)+1                                                
                                                                                
XMINF01  B     RSTCH                                                            
XMINF02  B     BALL0                                                            
XMINF03  B     NBILGEN                                                          
INFX2#   EQU   ((*-XMINF00)/4)+1                                                
                                                                                
XMINFQ   EQU   ((*-XMINF00)/4)+1                                                
         EJECT                                                                  
RSTCH    MVC   0(41,R1),=C'Record redisplayed because status changed'           
         B     INFEXIT                                                          
*                                                                               
BALL0    MVC   0(39,R1),=C'Automatic $0 entered in Adj DR/CR field'             
         MVC   ACURFORC,ABALL0                                                  
         B     INFEXIT                                                          
*                                                                               
NBILGEN  DS    0H                                                               
         MVC   0(50,R1),=C'Billing will not generate w/o CLT$ Adj DR/CR+        
                value'                                                          
         MVC   ACURFORC,ADRCRRFB                                                
         B     INFEXIT                                                          
*                                                                               
BMUPS    DS    0H                                                               
         MVC   MSGNUM2,=AL2(SI#BMUPS)                                           
         MVC   ACURFORC,ACURSINF                                                
         B     INFGTXT                                                          
         EJECT                                                                  
INFGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMINF        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MYTEXT,0             ANY REPLACE TEXT?                           
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT         YES, PUT LENGTH IN                         
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT            AS WELL AS THE ADDR OF THE TEXT           
         DROP  R1                                                               
         B     INFEXIT                                                          
                                                                                
                                                                                
INFEXIT  DS    0H                                                               
         OC    ACURFORC,ACURFORC   NEED TO SET CURSOR?                          
         BNZ   INFEXITX             NOPE                                        
         MVC   ACURFORC,AFRSTKEY   PLACE CURSOR ON 1ST KEY FIELD,               
                                                                                
INFEXITX DS    0H                                                               
         GOTO1 AERREX                                                           
         B     XIT_XM                                                           
         TITLE 'SPSFM43 - PW BILL (SUBRXM--LTORG && CONSTANTS)'                 
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
         TITLE 'SPSFM43 - PW BILL (SUBRXM--MISC STUFF)'                         
*--------------------- SUBR03 MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBRXML  EQU   *-XMSGRTN                                                        
         DS    0CL(4096-SUBRXML+1)                                              
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM43 - PROFIT WITHIN BILL (MORE TABLES)'                     
***********************************************************************         
*======================== MORE T21743's TABLE ========================*         
                                                                                
INIWRKTB DS    0XL(2+2+8)          TABLE FOR INITIALIZING WORK AREA             
         DC    AL2(SVACLABL-NMODWORK,ASVACTB-SYSD),CL8'*SVACTB*'                
         DC    AL2(SPBKLABL-NMODWORK,ASBLOCK-SYSD),CL8'*SPTBLK*'                
         DC    AL2(CHNKLABL-NMODWORK,ASPCHUNK-SYSD),CL8'**CHUNK*'               
         DC    AL2(ESTBLABL-NMODWORK,AESTAB-SYSD),CL8'**ESTAB*'                 
         DC    AL2(MKESLABL-NMODWORK,AMKESTAB-SYSD),CL8'*MKESTB*'               
INIWRKTQ EQU   (*-INIWRKTB)/(L'INIWRKTB)                                        
                                                                                
                                                                                
ELDATTAB DS    0XL(1+1+2)          ELEMENTS W/ BROADCAST DATES IN THEM          
         DS     0XL1                ELCODE                                      
         DS     0XL1                DATE DISPL INTO ELEM                        
         DS     0XL2                # BUCKETS, 1ST BUCKET DISPL                 
         DC     AL1(PWWKCODQ),AL1(PWWKDATE-PWWKEL),AL1(0,0)                     
         DC     AL1(PWDOLCDQ),AL1(PWDOLWK-PWDOLEL),AL1(7,4)                     
         DC     AL1(PWCURCDQ),AL1(PWCURWK-PWCUREL),AL1(7,4)                     
         DC     AL1(PWBAKDOL),AL1(PWDOLWK-PWDOLEL),AL1(7,4)                     
         DC     AL1(PWCLCCDQ),AL1(PWCLCWK-PWCLCEL),AL1(1,4)                     
         DC     AL1(PWCLLCDQ),AL1(PWCLLWK-PWCLLEL),AL1(1,4)                     
         DC     AL1(PWBAKCLL),AL1(PWCLLWK-PWCLLEL),AL1(1,4)                     
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL'                                   
***********************************************************************         
*========================== T21743's EQUATES =========================*         
EOT      EQU   X'00'                                                            
X80      EQU   X'80'                                                            
X40      EQU   X'40'                                                            
X20      EQU   X'20'                                                            
X10      EQU   X'10'                                                            
X08      EQU   X'08'                                                            
X04      EQU   X'04'                                                            
X02      EQU   X'02'                                                            
X01      EQU   X'01'                                                            
XFF      EQU   X'FF'                                                            
LSTSCRNQ EQU   X'99'               PW/LIST SCREEN PHASE NUMBER                  
PAGE00   EQU   0                   TWA PAGE # FOR TEMPSTR                       
PAGEQ    EQU   1                   TWA PAGE # FOR TEMPSTR                       
PAGETSRQ EQU   4                   TEMPSTR PAGE FOR TSAR                        
MINPWQ   EQU   -10000              MIN PW% = -100.00%                           
MAXPWQ   EQU   8499                MAX PW% = +84.99%                            
PWBLKL   EQU   PWBLKX-PWBLKD       L(PWBLOCK)                                   
PKYMKTL  EQU   PWKSTA-PWFKEY       L(PW KEY UNTIL & INCLUDING MKT)              
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL (SPSFMWORKD)'                      
***********************************************************************         
*============================= SPSFMWORKD ============================*         
       ++INCLUDE SPSFMWORKD                                                     
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL (TWA DSECTS)'                      
***********************************************************************         
*================================ TWA ================================*         
                                                                                
*---------------------------- BASE SCREEN ----------------------------*         
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
*--------------------------- PW BILL SCREEN --------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM98D                                                       
         EJECT                                                                  
*--------------------------- PW MAINT SCREEN -------------------------*         
         ORG   CONTAGH                                                          
* SCSFM9AD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SCSFM9AD                                                       
         PRINT ON                                                               
                                                                                
         DS    0CL(L'PWBOPTN-L'PWMOPTN+1)                                       
         DS    0CL(L'PWMOPTN-L'PWBOPTN+1)                                       
         EJECT                                                                  
*---------------------------- STORAGE AREA ---------------------------*         
                                                                                
         ORG   PWBWORK                                                          
BASE1    DS    A                   A(1ST BASE REG OF MAIN NMOD)                 
BASE2    DS    A                   A(2ND BASE REG OF MAIN NMOD)                 
BASE3    DS    A                   A(3RD BASE REG OF MAIN NMOD)                 
AGOSUB   DS    A                   A(SUBRTNS INTERFACE)                         
ASUBR01  DS    A                   A(1ST SUBROUTINE POOL)                       
ASUBR02  DS    A                   A(2ND SUBROUTINE POOL)                       
ASUBR03  DS    A                   A(3RD SUBROUTINE POOL)                       
ASUBR04  DS    A                   A(4TH SUBROUTINE POOL)                       
AXMSGRTN DS    A                   A(EXIT W/ MSG ROUTINE)                       
AM1STREC DS    A                   A(MY 1ST DATA FIELD)                         
PRVFDSP  DS    H                   FIELD DISP FROM PREV TRANSACTION             
         DS    H                   (SPARE TO KEEP FULL ALIGNMENT)               
                                                                                
AOPTABLE DS    A                   A(OPTIONS TABLE)                             
ALOCDATE DS    A                   A(LOCDATE)                                   
AACCUTAB DS    A                   A(ACCUMULATOR TABLE)                         
ACORQLST DS    A                   A(CORE-RES EQUATE LIST)                      
AELDATTB DS    A                   A(ELDATTAB)                                  
ACORQADD DS    0A                  **** CORE-RES ADDRESSES GO HERE ****         
ATSAR    DS    A                    A(TSAR)                                     
APWCALC  DS    A                    A(PWCALC)                                   
ASPOTIO  DS    A                    A(SPOTIO)                                   
ASPOTBUY DS    A                    A(SPOTBUY)                                  
ASPOTGL  DS    A                    A(SPOTGOAL)                                 
AMOBILE  DS    A                    A(MOBILE)                                   
MOBINPAD DS    0F                   ******** MOBILE ADCON LIST *******          
AGETBROD DS    A                     A(GETBROAD)                                
AADDAY   DS    A                     A(ADDAY)                                   
AGETDAY  DS    A                     A(GETDAY)                                  
ADATCON  DS    A                     A(DATCON)                                  
                                                                                
DSLIST   DS    0C                 ******** DATA DICTIONARY TERMS ******         
         DSDDL PRINT=YES                                                        
                                                                                
         DS    0F                                                               
TSARBLK  DS    XL(TSARDL)          TSAR PARAMETER BLOCK                         
                                                                                
                                                                                
MYTWAL   EQU   *-CONHEADH                                                       
         DS    0CL(3520-MYTWAL)    CHECK AGAINST GENCON'S TWA LIMIT             
         EJECT                                                                  
*------------------------- OTHER TWA STORAGES ------------------------*         
                                                                                
* DDGENTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
                                                                                
                                                                                
* SPSFMSAVED                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPSFMSAVED                                                     
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL OVERLAY'                           
***********************************************************************         
*============================ OTHER DSECTS ===========================*         
                                                                                
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAGETTXTD                                                                     
* DDCOMFACS                                                                     
* DDPERVALD                                                                     
* DDCOREQUS                                                                     
* DDTSARD                                                                       
* GEMSGEQUS                                                                     
* SPDDEQUS                                                                      
* SPMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE GEMSGEQUS                                                      
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE SPMSGEQUS                                                      
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL (SPGEN DSECTS)'                    
***********************************************************************         
*============================ SPGEN DSECTS ===========================*         
                                                                                
*----------------------------- SPGENWIPW -----------------------------*         
       ++INCLUDE SPGENWIPW         WESTERN INTL. PROFIT WITHIN REC              
         EJECT                                                                  
*------------------------------ SPGENAGY -----------------------------*         
AGYHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*------------------------------ SPGENCLT -----------------------------*         
CLTHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*------------------------------ SPGENEST -----------------------------*         
ESTHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*------------------------------ SPGENBUY -----------------------------*         
BUYRECD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*------------------------------ SPGENBILL ----------------------------*         
BILLRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENBILL                                                      
         PRINT ON                                                               
         SPACE 2                                                                
*------------------------------ SPGENSTAB ----------------------------*         
* STABUCKD DSECT                                                                
         PRINT OFF                                                              
       ++INCLUDE SPGENSTAB                                                      
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL OVERLAY'                           
***********************************************************************         
*============================== PW BLOCK =============================*         
                                                                                
       ++INCLUDE SPPWBLOCK                                                      
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL OVERLAY (SYSD)'                    
***********************************************************************         
*========================== SAVED WORK AREA ==========================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
* The following area is shared by the PW phases under SPSFM.  The               
*  purpose is to enable communication among themselves when needed.             
                                                                                
PVACTEQU DS    XL(L'ACTEQU)        ACTEQU OF LAST TRANSACTION                   
CTODAY   DS    XL2                 TODAY'S DATE COMPRESSED                      
CALLPFK  DS    XL(L'PFKEY)                                                      
BUYRID   DS    CL3                 BUYER'S INITIALS                             
BINYRMT  DS    XL2                 A BINARY YEAR/MONTH FIELD                    
SVOPTFLD DS    CL(L'PWMOPTN)       SAVE TEXT IN OPTIONS FIELD                   
                                                                                
* The following fields are owned by this program only                           
                                                                                
*                                 **************** MISC ***************         
ALOCKLN  DS    A                   A(LOCK $ LINE)                               
ACURRLN  DS    A                   A(CURRENT $ LINE)                            
ADIFFLN  DS    A                   A(DIFFERENCE LINE)                           
ABALL0   DS    A                   A(DR/CR FIELD W/ FORCED $0)                  
ADRCRRFB DS    A                   A(DR/CR FLD SET UP FOR RE-FNL BILL)          
ACURSINF DS    A                   A(PUT CURSOR FOR INFO MSG)                   
ANMODWRK DS    A                   A(NMOD WORKING AREA)                         
ASVACTB  DS    A                   A(SAVED ACCUTAB W/IN TRANSACTION)            
ASBLOCK  DS    A                   A(SPOTBLOCK)                                 
ASPCHUNK DS    A                   A(SPOT CHUNK AREA)                           
AESTAB   DS    A                   A(ESTIMATE TABLE)                            
AMKESTAB DS    A                   A(MKT/EST TABLE FOR SPOTIO)                  
AERREX   DS    A                   A(ERREX ROUTINE) TO USE                      
ALOCERR  DS    A                   A(LOCATION WHERE ERROR OCCURRED)             
HOLDRE   DS    F                                                                
HOLDRF   DS    F                                                                
RELO     DS    F                                                                
TEMPPW   DS    F                   TEMP STORAGE FOR PROFIT WITHIN               
TEMPACB  DS    F                    "      "     "  ACTUAL   BUYS               
TEMPAJB  DS    F                    "      "     "  ADJUSTED BUYS               
TEMPTAX  DS    F                    "      "     "  TAX DOLLARS                 
TEMPCTX  DS    F                    "      "     "  CLT TAX DOLLARS             
TEMPNSPT DS    F                    "      "     "  # OF SPOTS                  
*                                                                               
MTOTALS  DS    0F                                                               
MTTLCLK  DS    F                   MONTH TOTAL FOR CLIENT   LOCKS               
MTTLWLK  DS    F                     "     "    "  WIM      LOCKS               
MTTLAJB  DS    F                     "     "    "  ADJUSTED BUYS                
MTTLACB  DS    F                     "     "    "  ACTUAL   BUYS                
MTTLMGI  DS    F                     "     "    "  MAKEGOODS IN                 
MTTLMGO  DS    F                     "     "    "  MAKEGOODS OUT                
MTTLLTX  DS    F                     "     "    "  LOCKED TAX DOLLARS           
MTTLLCTX DS    F                     "     "    "  LOCKED CLT TAX $$            
MTTLTAX  DS    F                     "     "    "  TAX DOLLARS                  
MTTLCTX  DS    F                     "     "    "  CLT TAX DOLLARS              
MTTLNSPT DS    XL4                   "     "    "  # OF SPOTS                   
MTTLWNET DS    XL4                   "     "    "  WIM NET DOLLARS              
MTTLCNET DS    XL4                   "     "    "  CLT NET DOLLARS              
MTOTALQ  EQU   *-MTOTALS                                                        
*                                                                               
STOTALS  DS    0F                                                               
SKEDCLK  DS    F                   SCHED TOTAL FOR CLIENT   LOCKS               
SKEDWLK  DS    F                     "     "    "  WIM      LOCKS               
SKEDAJB  DS    F                     "     "    "  ADJUSTED BUYS                
SKEDACB  DS    F                     "     "    "  ACTUAL   BUYS                
SKEDMGI  DS    F                     "     "    "  MAKEGOODS IN                 
SKEDMGO  DS    F                     "     "    "  MAKEGOODS OUT                
SKEDLTX  DS    F                     "     "    "  LOCKED TAX DOLLARS           
SKEDLCTX DS    F                     "     "    "  LOCKED CLT TAX $$            
SKEDTAX  DS    F                     "     "    "  TAX DOLLARS                  
SKEDCTX  DS    F                     "     "    "  CLT TAX DOLLARS              
SKEDNSPT DS    XL4                   "     "    "  # OF SPOTS                   
SKEDWNET DS    XL4                   "     "    "  WIM NET DOLLARS              
SKEDCNET DS    XL4                   "     "    "  CLT NET DOLLARS              
STOTALQ  EQU   *-STOTALS                                                        
         DS    0CL(STOTALQ-MTOTALQ+1)                                           
         DS    0CL(MTOTALQ-STOTALQ+1)                                           
SKEDBILL DS    F                   SCHED TOTAL FOR ACTUAL BILL                  
SKEDDRCR DS    F                     "     "    "  DR/CR AMOUNTS                
                                                                                
SKEDCGTX DS    F                   SKED ACTUAL GOAL TOTAL (W/ TAX)              
SKEDGRP  DS    F                   SKED'S GRP                                   
GLTXRATE DS    F                   TAX RATE FOR GOALS                           
                                                                                
LOCKSUM  DS    F                                                                
CURRSUM  DS    F                                                                
TEMPSUM  DS    F                                                                
OPWPCT   DS    F                   ORIG SKED PW% (ESTIMATE'S PW%)               
ESDATE   DS    CL(L'ESTART)        EBCDIC START DATE OF ESTIMATE                
EEDATE   DS    CL(L'EEND)            "    END    "   "     "                    
MYDATE6  DS    CL6                 TEMP STORAGE FOR AN EBCDIC DATE              
STARTEND DS    CL12                TEMP STRGE FOR STRT-END DATE                 
DATE2    DS    XL2                 COMPRESSED 2-BYTE DATE                       
         ORG   DATE2                                                            
DATE4    DS    0XL4                COMPRESSED STRT/END DATE                     
SDATE2   DS    XL2                                                              
EDATE2   DS    XL2                                                              
RUNDATE  DS    XL2                                                              
MYAGYID  DS    CL(L'AGYID)         AGENCY ID FROM AGYHDR                        
ESTOOWSD DS    XL(L'EOWSDAY)       OUT-OF-WEEK ROTATOR START DAY                
TEMPBSTA DS    XL(L'BSTA)          TEMP STORAGE FOR BINARY STATION              
CBLSCMSK DS    XL(L'BSTA)          CABLE STATION SYSCODE MASK                   
MYCALLSP DS    XL2                                                              
NUMWEEKS DS    XL1                 NUMBER OF WEEKS IN PERIOD                    
MSGSYS   DS    XL1                                                              
MSGNUM2  DS    XL2                                                              
MYERRCD  DS    XL1                 MY ERROR   CODE                              
MYINFCD  DS    XL1                 MY INFRMTN CODE                              
MYWRNCD  DS    XL1                 MY WARNING CODE                              
MYBYTE   DS    XL1                 MY ONE BYTE (TEMP) STORAGE                   
SVERROR  DS    XL(L'ERROR)         SAVED ERROR ON ERREXIT                       
GOSUBN   DS    XL1                 ROUTINE # FOR SUB-RTN INTERFACES             
USETAX   DS    CL1                 USE TAX (Y)ES OR (N)O                        
SVUSETAX DS    CL(L'USETAX)                                                     
CNTDOWN  DS    XL1                 COUNTER VARIABLE                             
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
BUYMODE  DS    XL1                                                              
MNTHINF  DS    CL3                 MONTH OF INFO MESSAGE                        
SVTSRNUM DS    XL(L'TSRNUM)        SAVE RECD# FOR NESTED TSAR CALLS             
                                                                                
*                                 *************** FLAGS ***************         
MISCFLG1 DS    XL1                 MISC FLAG #1                                 
MF1KYCHG EQU   X80                  KEY FIELDS HAVE BEEN CHANGED                
MF1MDFY  EQU   X40                  SOMETHING WAS MODIFIED                      
MF1RRDCH EQU   X20                  RE-READ FILE ON CHANGE                      
MF1STTCH EQU   X10                  (BUY AND/OR PW) STATUS CHANGED              
MF1BALL0 EQU   X08                  0 FORCED INTO CLT$ ADJ DR/CR FLD            
MF1ERRQ  EQU   X04                  PREV TRANSACTION HAD ERROR                  
MF1OPTN  EQU   X02                  OPTION INPUTTED THIS TIM                    
MF1REDO  EQU   MF1KYCHG+MF1OPTN+MF1STTCH          REDO PW SETUP                 
MF1NKFRS EQU   MF1MDFY+MF1RRDCH+MF1STTCH+MF1ERRQ  RESET ON NEW KEY              
MF1KYOPT EQU   MF1KYCHG+MF1OPTN                   KEY OR OPTN CHANGED           
MF1IMSG  EQU   MF1STTCH+MF1BALL0                                                
                                                                                
MISCFLG2 DS    XL1                 MISC FLAG #2                                 
MF2RFBQ  EQU   X80                  PROCESS(ED) FOR RE-FINAL BILLING            
MF2MRFB  EQU   X40                  A MONTH SET UP FOR RE-FNL BILLING           
MF2UPSBM EQU   X20                  UNPAID SPOT IN FINAL BILLED MONTH           
MF2IMSG  EQU   MF2MRFB+MF2UPSBM                                                 
MF2NKFRS EQU   MF2RFBQ+MF2MRFB+MF2UPSBM           RESET ON NEW KEY              
                                                                                
MISCFLG3 DS    XL1                 MISC FLAG #3                                 
MF31STIM EQU    X80                 FIRST TIME (THAT SCREEN WAS LOADED)         
MF3SSCHK EQU    X40                 SKIP #-OF-SPOTS CHECK                       
MF3NKFRS EQU   0                                                                
                                                                                
LOCKFLAG DS    XL1                 LOCK FLAG                                    
LKFFLCKQ EQU   X'80'                FILE HAS BUY LOCKED                         
LKFFPLKQ EQU   X'08'                FILE HAS PW% LOCKED                         
                                                                                
OPTUSED  DS    XL1                 OPTIONS USED FLAG                            
OPUTAX   EQU   X80                  TAX= OPTION SPECIFIED                       
OPUNBA   EQU   X40                  NBA  OPTION SPECIFIED                       
                                                                                
CLTFLAG  DS    XL1                 CLIENT   HEADER FLAG                         
CFWSTRAD EQU    X80                 WESTERN TRADING CLIENT                      
                                                                                
ESTFLAG  DS    XL1                 ESTIMATE HEADER FLAG                         
EFBILEST EQU   X'80'                ECONTROL HAS EBILESTQ ON                    
EFOWPW   EQU    X40                 OUT-OF-WEEK PW BILLING                      
EFWSTRAD EQU    X20                 WESTERN TRADING ESTIMATE                    
                                                                                
SHOWCURR DS    CL1                 DISPLAY CURRENT$ (C'Y'/C'N')                 
SHOWTOTC DS    CL1                 DISPLAY TOTAL CURRENT$ (C'Y'/C'N')           
                                                                                
MYTSERRS DS    XL(L'TSERRS)        REMEMBER ERROR RETURNED FROM TSAR            
                                                                                
*                                 *************** TABLES **************         
BRDMTHQ  EQU   5                       MAX OF 5 BROADCAST MONTHS                
BRDMTHTB DS    XL(BRDMTHQ*BCSTTABQ+1)  TABLE OF BROADCAST MONTHS                
                                                                                
BRDWKQ   EQU   15                      MAX OF 5 BROADCAST WEEKS                 
BRDWKTAB DS    XL(BRDWKQ*BCSTTABQ+1)   TABLE OF BROADCAST WEEKS (MINE)          
BRDWKTB2 DS    XL(BRDWKQ*4+1)            "   "      "       "                   
                                                                                
PWTAB    DS    XL(15*(4+4)+1)      PW TABLE (DATES & PW%) BY WEEK               
                                                                                
*                                 ************** BUFFERS **************         
MYTEXT   DS    0X                  MISCELLANEOUS TEXT FIELD                     
         DS    XL1                  L'TEXT                                      
         DS    CL20                 THE TEXT ITSELF                             
MYTEXTX  EQU   *                                                                
MYTEXTL  EQU   MYTEXTX-MYTEXT                                                   
                                                                                
STACREC  DS    XL(STACRECL)        I/O AREA TO HOLD STACTAB ENTRY               
                                                                                
MYSSPREL EQU   *-SYSSPARE                                                       
         DS    0CL(1024-MYSSPREL)  CHECK AGAINST AVAIL SYSSPARE AMT             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= ACCUMULATOR TABLE DSECT =====================*         
ACCUTABD DSECT                                                                  
ATMONTH  DS    0XL4                BROADCAST MONTH                              
ATMSTART DS    XL2                  START DATE                                  
ATMEND   DS    XL2                  END   DATE                                  
ATWEEK   DS    0XL4                BROADCAST WEEK                               
ATWSTART DS    XL2                  START DATE                                  
ATWEND   DS    XL2                  END   DATE                                  
                                                                                
ATACVALS DS    0XL4                ACCUMULATOR VALUES                           
ATCLCK   DS    XL4                  LOCK IN (PREVIOUS ADJUSTED BUY)             
ATWLCK   DS    XL4                  LOCK IN (PREVIOUS ACTUAL   BUY)             
ATAJBUY  DS    XL4                  ADJUSTED SPOT BUYS                          
ATACBUY  DS    XL4                  ACTUAL BUY                                  
ATMGIN   DS    XL4                  MAKEGOODS IN                                
ATMGOUT  DS    XL4                  MAKEGOODS OUT                               
ATLKTAX  DS    XL4                  LOCKED TAX DOLLARS                          
ATLKCTX  DS    XL4                  LOCKED CLT TAX DOLLARS                      
ATTAX    DS    XL4                  TAX DOLLARS                                 
ATCLTAX  DS    XL4                  CLT TAX DOLLARS                             
ATNSPT   DS    XL4                  # OF SPOTS                                  
ATWNET   DS    XL4                  WIM NET DOLLARS                             
ATCNET   DS    XL4                  CLT NET DOLLARS                             
ATACVALQ EQU   *-ATACVALS                                                       
ATACVALN EQU   (ATACVALQ)/(L'ATACVALS)                                          
ATBILL   DS    XL4                  BILL AMOUNT (BACTUAL IN BILL HDR)           
ATLPW    DS    XL4                  LOCKED  PROFIT WITHIN %                     
ATPW     DS    XL4                  CURRENT PROFIT WITHIN %                     
ATDRCR   DS    XL4                 CLT$ ADJ DR/CR AMOUNT                        
ATBILD   DS    XL2                 ADJUSTMENT BILLING DATE                      
                                                                                
ATFLAG   DS    XL1                 FLAGS                                        
ATFMTTLQ EQU   X80                  ENTRY IS MONTHLY  TOTAL                     
ATFSTTLQ EQU   X40                  ENTRY IS SCHEDULE TOTAL                     
ATFBILAJ EQU   X20                  ENTRY IS BILL ADJ LIN                       
ATFPWQ   EQU   X10                  PW WAS MODIFIED                             
ATFDRCRQ EQU   X08                  DR/CR WAS MODIFIED (BILL ADJ ONLY)          
ATFPJBLQ EQU   X04                  PRJ BILL WAS MODFD (TTL NTRY ONLY)          
ATFUNPDQ EQU   X02                  A SPOT UNPAID                               
ATFNOSPT EQU   X01                  NO SPOTS FOR THIS WEEK                      
                                                                                
ATFLAG2  DS    XL1                 MORE FLAGS                                   
ATF2RFB  EQU   X80                  MONTH CHANGED FOR RE-FINAL BILLING          
ATF2CHDC EQU   X40                  ADJ DR/CR VALUE CHANGED                     
ATF2OOWP EQU   X20                  USING OOW EST PAID$, NOT LOCK$              
ATF2ESTB EQU   X08                  ESTIM BILL (FOR ATFMTTLQ ONLY)              
ATF2FNLB EQU   X04                  FINAL BILL (FOR ATFMTTLQ ONLY)              
ATF2CCOD EQU   X02                  CLCOST OVERRIDED (eg. =5000)                
ATF2CLOD EQU   X01                  CLLOCK OVERRIDED (eg. =5000)                
ATF2BILL EQU   ATF2ESTB+ATF2FNLB                                                
ATF2COVD EQU   ATF2CCOD+ATF2CLOD                                                
                                                                                
ACCUTABQ EQU   ((*-ACCUTABD+3)/4)*4                                             
         DS    0CL(ATACVALQ-STOTALQ+1)                                          
         DS    0CL(STOTALQ-ATACVALQ+1)                                          
         EJECT                                                                  
*=================== STATION ACCUMULATOR TABLE DSECT =================*         
*                                                                               
STACRECD DSECT                                                                  
STACKEYD DS    0X                  KEY                                          
STACSTA  DS     XL3                 MSPACKED STATION                            
STACSTRT DS     XL2                 START DATE OF WEEK                          
STACEND  DS     XL2                 END   DATE OF WEEK                          
STACKEYL EQU   *-STACKEYD                                                       
                                                                                
STACFLG  DS    XL1                 FLAG                                         
STAFMKTQ EQU   X'80'                ENTRY ACCOUNTED FOR @ MKT LEVEL             
STAFPRCQ EQU   X'40'                ENTRY ACCOUNTED FOR @ STAT LEVEL            
STAFATBQ EQU   X'20'                TRANSFERRED TO ACCUTAB ALREADY              
STACSPT  DS    XL4                 # OF SPOTS                                   
STACGRS  DS    XL4                 GROSS DOLLARS                                
STACNET  DS    XL4                 NET   DOLLARS                                
STACTAX  DS    XL4                 TAX   DOLLARS                                
STACCTX  DS    XL4                 CLT TAX DOLLARS                              
STACCGRS DS    XL4                 CLIENT GROSS DOLLARS                         
STACCNET DS    XL4                 CLIENT NET   DOLLARS                         
STACRECL EQU   *-STACRECD                                                       
         EJECT                                                                  
*========================= BROADCAST TABLE DSECT =====================*         
BCSTTABD DSECT                                                                  
BCSSTART DS    XL2                 START DATE OF A BROADCAST PERIOD             
BCSEND   DS    XL2                 END    "   "  "    "        "                
BCSFLAG  DS    XL1                 FLAG                                         
BCSFMXQ  EQU   X80                  ENTRY IS EFFECTIVELY THE EOTABLE            
*                                    (FOR MONTH TABLE ONLY)                     
BCSTTABQ EQU   *-BCSTTABD                                                       
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*===================== LINE (ON BILL SCREEN) DSECT ===================*         
                                                                                
BSLDSECT DSECT                                                                  
BSLM1H   DS    CL8                                                              
BSLM1    DS    CL8      8   26    7  A  P N             T        Y              
BSLM2H   DS    CL8                                                              
BSLM2    DS    CL8      8   36    7  A  P N             T        Y              
BSLM3H   DS    CL8                                                              
BSLM3    DS    CL8      8   46    7  A  P N             T        Y              
BSLM4H   DS    CL8                                                              
BSLM4    DS    CL8      8   56    7  A  P N             T        Y              
BSLTTLH  DS    CL8                                                              
BSLTTL   DS    CL8      8   67    7  A  P N             T        Y              
BSLLENQ  EQU   *-BSLDSECT                                                       
         DS    0CL(BSLLENQ-(PWBTTL+L'PWBTTL-PWBM1H)+1)                          
         DS    0CL((PWBTTL+L'PWBTTL-PWBM1H)-BSLLENQ+1)                          
BSLMQ    EQU   BSLLENQ/5                                                        
***********************************************************************         
                                                                                
***********************************************************************         
*======================== OPTIONS TABLE DSECT  =======================*         
                                                                                
OPTDSECT DSECT                                                                  
OPTNUMB  DS    XL1                 INTERNAL OPTION NUMBER                       
OPTLEN   DS    XL1                 LENGTH OF OPTION TABLE                       
OPTBIT   DS    XL1                 OPTION BIT                                   
OPTFLAG  DS    XL1                 FLAGS ABOUT OPTION ENTRY                     
OPFOTHAB EQU   X80                  VALUES FOUND IN ANOTHER TABLE               
OPFKYWRD EQU   X40                  OPTION SPECIFIED BY KEYWORD ONLY            
OPTOLEN  DS    XL1                 OUTPUT LENGTH                                
OPTOADDR DS    XL2                 DISPL FROM SYSD OF OUTPUT FIELD              
OPTNAME  DS    0C                  AL1(L'NAME),C'NAME'                          
OPTVALS  DS    0C                  AL1(L'VALUE),C'VALUE' OR A(TABLE)            
***********************************************************************         
                                                                                
***********************************************************************         
*============================= SPOTBLOCK =============================*         
                                                                                
SBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPOTBLOCK                                                      
         PRINT ON                                                               
SPBKLEN  EQU   SBLOCKX-SBLOCK      L(SPOTBLOCK)                                 
                                                                                
                                                                                
         EJECT                                                                  
*============================== SPOTTABD =============================*         
         PRINT OFF                                                              
       ++INCLUDE SPOTTABD                                                       
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL (NMOD WORK AREA)'                  
***********************************************************************         
*=========================== NMOD WORK AREA ==========================*         
NMODWORK DSECT                                                                  
*                                                                               
SVACLABL DS    D                   *SVACTB*                                     
SVACTB   DS    XL(L'ACCUTAB)                                                    
SVACTBX  EQU   *                                                                
*                                                                               
SPBKLABL DS    D                   *SPTBLK*                                     
SPBLK    DS    XL(SPBKLEN)                                                      
SPBLKX   EQU   *                                                                
*                                                                               
CHNKLABL DS    D                   **CHUNK*                                     
CHUNK    DS    XL2500                                                           
CHUNKX   EQU   *                                                                
*                                                                               
ESTBLABL DS    D                   **ESTAB*                                     
ESTAB    DS    XL256                                                            
ESTABX   EQU   *                                                                
*                                                                               
MKESLABL DS    D                   *MKESTB*                                     
MKESTTAB DS    XL256                                                            
MKESTBX  EQU   *                                                                
*                                                                               
NMODWRKL EQU   *-NMODWORK          LENGTH FOR NMOD WORK AREA                    
***********************************************************************         
         TITLE 'SPSFM43 - PROFIT WITHIN BILL (SPOT STRG AREA)'                  
***********************************************************************         
*========================== SPOT STORAGE AREA ========================*         
                                                                                
*        THESE ARE STORAGE AREAS WHICH SPOTIO, SPOTBUY, AND SPOTGOAL            
*         USE.  THESE AREAS WILL BE PUT IN TIA, SINCE THEY ARE HUGE             
                                                                                
SPOTAREA DSECT                                                                  
ACTBLABL DS    D                   *ACCUTB*                                     
ACCUTAB  DS    XL(25*ACCUTABQ+1)                                                
ACCUTABX EQU   *                                                                
                                                                                
         PRINT OFF                                                              
*&&DO                                                                           
ESTBLABL DS    D                   **ESTAB*                                     
ESTAB    DS    XL256                                                            
ESTABX   EQU   *                                                                
                                                                                
*&&                                                                             
         PRINT ON                                                               
SPTBLABL DS    D                   *SPTTAB*                                     
*&&DO                                                                           
SPTTAB   DS    XL7000                                                           
*&&                                                                             
SPTTAB   DS    200XL(SPTTABL)                                                   
SPTTABX  EQU   *                                                                
                                                                                
MYTIALEN EQU   *-SPOTAREA                                                       
         DS    0CL((X'4800'-MYTIALEN)+1)                                        
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076SPSFM43   08/02/02'                                      
         END                                                                    
