*          DATA SET SPWRI00    AT LEVEL 055 AS OF 10/30/14                      
*PHASE T20400A                                                                  
*INCLUDE EQVRD                                                                  
*INCLUDE COVAIL                                                                 
*INCLUDE DPTRD                                                                  
*INCLUDE SPBVAL                                                                 
*INCLUDE DDUCOM                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T20400 - SPOTPAK WRITER CONTROLLER'                             
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI00 (T20400A) - SPOT WRITER CONTROLLER               *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 30OCT14 55 AKT -- INIT COMMENT SEQUENCE NUMBER                    *           
* 17OCT11 54 AKT -- NEW SJ JOHNSON ESTIMATE INTERFACE DPG           *           
* 14APR10 53 AKT -- INIT SECOND TSAR BUFFER FOR CML ENTRIES         *           
* 31MAR08 52 AKT -- RESOLVE GETACOM FOR SPWRI0B                     *           
* 13JUL07 51 AKT -- RESOLVE GETSTABF FOR SPWRI0B                    *           
* 29JUN07 50 AKT -- RESOLVE GETSLP AND PUTSLP FOR SPWRI0B           *           
* 30NOV06 49 AKT -- SET SBTSARBF FOR SPOTIO                         *           
* 16NOV06 48 AKT -- RELINK FOR NEW 6K I/O AREAS                     *           
* 12OCT06 47 AKT -- BRS NOW DEFUCT                                  *           
* 20SEP06 46 EFJ -- INCRESE SOON WEEK LIMIT TO 105 & REMOVE         *           
*                   OBSOLETE SECURITY CODE                          *           
* 15NOV05 45 AKT -- INCREASE MAX TSAR REC LEN TO 4,000 FOR INV AND  *           
*                -- RESOLVE PUTNINV AND GETNINV FOR SPWRI0B         *           
* 27OCT05 44 EFJ -- L43 FIX UNCOVERED BUG - ADDED RESTRICTION!      *           
* 25OCT05 43 EFJ -- REMOVE MORE ARCHAIC SOON RESTRICTIONS           *           
* 19SEP05 42 EFJ -- HIGHER DEFAULT SOON WEEK LIMIT                  *           
* 08DEC04 40 AKT -- INCREASE SBABKLST FOR LPM POSTING, FTP DEFUNCT  *           
* 08JUL04 39 AKT -- RESOLVE TWO ADDRESSES IN SPWRI0B (OM ROUTINES)  *           
* 22SEP03 38 EFJ -- CHAIN RPTS IN SAME PQ ETRY                      *           
* 18SEP03 37 AKT -- FIX SELECT RECACT TABLE ENTRIES                 *           
* 18SEP03 36 AKT -- 02 ENTRIES IN RECACT - ACTEQU = ACTNUM          *           
* 14AUG03 35 AKT -- ACCESS AWARE - CALL SECRET AND SAVE IN ASECBLK  *           
* 23JAN03 34 EFJ -- SAVE DRSTBUF ADDR FOR FREEMAIN                  *           
* 10JAN03 33 EFJ -- FREE TASR BUFFER                                *           
* 17OCT02 32 AKT -- INIT TSAR (LEVEL 30 & 31 ADDED DURING THIS CHG) *           
* 22NOV02 31 EFJ -- TELL SPOTIO NOT TO PASS DELETES FOR WRITERS     *           
* 04NOV02 30 BOB -- NEW RECACT ENTRIES FOR KRTAPE REPORT            *           
* 21AUG02 29 EFJ -- SPWRIOFF & SPWRIGEN NOW SPWRI0B AND SPWRI0C     *           
* 15AUG02 28 EFJ -- NEW RECACT ENTRIES FOR ORDBIL REPORT            *           
* 06JUN02 26 EFJ -- SPECIAL MVMFI SECURITY                          *           
* 17MAY02 25 EFJ -- RCPACK                                          *           
* 03APR02 23 EFJ -- RE-ENABLE SC JOHNSON TAPES                      *           
*                -- ALLOW RER OV                                    *           
* 05FEB02 22 EFJ -- NEW RECACT ENTRIES FOR SAATCHI RER              *           
*                -- DISABLE AGYSUM REPORT (PW - INITIATIVE)         *           
* 15MAY01 21 EFJ -- ALLOW SLOCKIN AS UPDATIVE SOON                  *           
* 17APR01 20 EFJ -- NEW RECACT ENTRIES FOR OLDWRI (FOR DIFF JCL)    *           
* 26FEB01 19 EFJ -- RE-ENABLE PG FOR STEVE KAYE (SEE L14)           *           
* 03OCT00 18 EFJ -- SUPPORT FOR USER COMMENTS                       *           
* 29AUG00 16 EFJ -- SET USERID IN SBUSERID                          *           
* 12JUL00 15 EFJ -- SET CONFDEL IN GENSTAT4                         *           
* 03MAR00 14 EFJ -- DISABLE PG DATA COLLECTION REPORT               *           
* 02FEB00 13 BOB -- ADD THDBILL RECORD TYPE                         *           
* 11OCT99 12 BOB -- MAKE OFFLINE DPG BUFFER HUGE                    *           
* 22DEC98 10 EFJ -- INCREASE SBABKLST                               *           
* 09NOV98 09 EFJ -- NEW REPORT TO LIST WRITERS (SHARES B5 SCREEN)   *           
* 05NOV98 08 EFJ -- DISABLE TEST RECORD TYPE                        *           
* 02SEP98 07 EFJ -- NEW PW REPORT (SHARES B5 SCREEN WITH AGYSUM)    *           
* 26JUN98 06 EFJ -- DISABLE UNUSED PHASES                           *           
* 02JUN98 05 EFJ -- RENAME 49 TO AGYSUM                             *           
* 27MAY98 04 EFJ -- ALLOW 49 SOON                                   *           
* 08MAY98 03 EFJ -- SUPPORT NEW 49 REPORT                           *           
* 08MAY98 02 EFJ -- REMOVE ACTION EBI                               *           
* 08MAY98 01 EFJ -- LEVEL RESET                                     *           
*-------------------------------------------------------------------*           
* 09DEC97 41 EFJ -- REMOVE ACTION HELP                              *           
* 19NOV97 40 EFJ -- ALMOST HAD L39...                               *           
* 18NOV97 39 EFJ -- ADD CFAFTER ACTION (CASHFLOW W/CF AS REPORT NM) *           
* 18JUL97 38 EFJ -- USE BINSRCH2 FROM COMFACS                       *           
* 17SEP96 37 EFJ -- RENAME BYCH TO BAS                              *           
* 16SEP96 36 EFJ -- FIX BYCH REC ACT ENTRY                          *           
* 28AUG96 35 EFJ -- REC ACT ENTRIES FOR BYCH REPORT                 *           
* 05JUN96 34 EFJ -- NO DELETE FROM LIST IF SECURITY                 *           
* 09MAY96 33 EFJ -- REC ACT ENTRIES FOR IAS REPORT                  *           
* 05APR96 32 EFJ -- PASS SBGETBRD TO SPOTBUY                        *           
* 26MAR96 31 EFJ -- SET INTENSITY FOR REQ FLT FIELD FOR DDS         *           
* 20FEB96 30 EFJ -- UNDO LEVEL 28 CHANGE - DOES NOT RE-DISP PQ ID   *           
* 09JAN96 29 EFJ -- CHANGE JCL FOR BGL REPORT                       *           
* 09JAN96 28 EFJ -- RE-DISPLAY SAME LIST PAGE AFTER SELECT          *           
* 28DEC95 27 EFJ -- SUPPORT NEW WL PROFILE                          *           
* 10NOV95 26 EFJ -- ADD TH TO AGYTAB                                *           
* 02NOV95 25 EFJ -- ONE MORE CLIENT TO PREVIOUS FIX                 *           
* 30OCT95 24 EFJ -- ALLOW 21 WEEK REQUESTS FOR DSMO                 *           
* 19OCT95 23 EFJ -- EASE SOON RESTRICTIONS FOR TBS                  *           
* 12OCT95 22 EFJ -- SUPPORT LIMIT ACCESS IN SPOTIO                  *           
* 29SEP95 21 EFJ -- DISALLOW 1 YEAR SOON FOR O&M                    *           
* 19SEP95 20 EFJ -- ALLOW 1 YEAR SOON FOR O&M                       *           
* 12JUL95 19 EFJ -- ADD WI & WT TO LIST OF SECURITY AGYS            *           
* 11JUL95 18 EFJ -- CHANGE SYSTEM TO S (NOT F)                      *           
* 04MAY95 17 EFJ -- DON'T ALLOW WI TO DELETE FORMATS FROM LIST      *           
*                   UNLESS AUTH=X'400F'                             *           
* 31MAR95 16 EFJ -- SET GENSTAT5 TO SET PREVAL BIT FOR FIELDS       *           
* 14NOV94 15 EFJ -- ALLOW SOON FOR AGY DSMO CLT CTT                 *           
* 31OCT94 14 EFJ -- SAVE A(TIOB) IN ATIOB INSTEAD OF WHO KNOWS WHAT *           
* ??OCT94 13 GP  -- NEW RECACT TABLE ENTRIES FOR BUYING RULES PGM   *           
* 13SEP94 12 EFJ -- RE-ROUTE MSPACK/UNPK CALLS THROUGH NEW STAPACK  *           
* 16AUG94 11 EFJ -- ALLOW SOON FOR AGY DSMO CLT CTC                 *           
* 21JUL94 10 EFJ -- FIX CALLOV CALL FOR SPPWCALC                    *           
* 19JUL94 09 EFJ -- GET A(SPPWCALC)                                 *           
* 06JUN94 07 TCS -- REWORK SYSD                                     *           
* 19MAY94 06 EFJ -- CLEAR FUCKING LNAMPOOL AFTER SYS6 (IT'S IN THE  *           
*                   MIDDLE OF WRIGEN'S ADCONS)                      *           
* 09MAR94 05 TCS -- CHANGE EOD CODE FOR DC REPORT TO RC             *           
* 08MAR94 04 TCS -- DISSALLOW SOON PROCESSING FOR MEDIA=*           *           
* 05JAN94 00 EFJ -- HISTORY LOST.  LEVEL RESET.  ADD 'A' TO PHASE   *           
* 18JAN94 00 PXZ -- ALLOW CLIENT=ALL FOR SOON FOR AGY WR            *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
T20400   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,T20400,R7,R6,RR=R2,CLEAR=YES                             
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    R5,RC                                                            
         USING SPOOLD,R5                                                        
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
*                                                                               
WRI10    ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=Y(LENIOAS)      GRABBING 3 2000 BYTE I/O AREAS               
         ST    R9,ASYSD                                                         
         LA    R8,4095(R9)                                                      
         LA    R8,1(R8)                                                         
         USING SYSD,R9,R8                                                       
         L     R1,SYSPARMS                                                      
         L     RA,4(R1)                                                         
         ST    RA,ATWA                                                          
         USING T204FFD,RA                                                       
         ST    R7,BASER7           SAVE SECOND AND THIRD REGISTERS              
         ST    R6,BASER6                                                        
*                                                                               
         L     RF,0(R1)            GET PF KEY FROM TIOB                         
         LLC   RE,TIOBAID-TIOBD(RF)                                             
         LA    RF,12                                                            
         CR    RE,RF               PF 13-24 BECOME 1-12                         
         BNH   *+6                                                              
         SR    RE,RF                                                            
         STC   RE,PFKEY                                                         
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         BNE   XIT                                                              
         NI    CONKEYH+1,X'FF'-X'20'  UNPROTECT ACTION FOR GENCON               
         OI    CONSERVH+1,X'01'     SERVICE FIELD IS ALWAYS MODIFIED            
         OI    CONSERVH+6,X'80'                                                 
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         BAS   RE,CHKTOP           CHECK TOP OF SCREEN                          
*                                                                               
         OI    GENSTAT1,RDUPAPPL                                                
         GOTO1 GENCON,DMCB,(R5)    OFF TO GENCON                                
*                                                                               
*        IF OFF-LINE NEED TO FREE DPG BUFFER                                    
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   NO PRINTER MEANS ON-LINE                     
         BZ    XIT                                                              
*                                                                               
         LA    R4,DRSTBSV          BUFFER ADDRESS SAVEAREA                      
         L     R3,DRSTBSV                                                       
         ICM   R3,15,0(R3)         BUFFER LENGTH                                
         FREEMAIN EC,LV=(R3),A=(R4)  FREE CORE                                  
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         L     R1,ATSARBF          A(BUFFER)                                    
         ICM   R0,15,=A(TSARBUFL)  LENGTH OF BUFFER                             
         FREEMAIN RC,A=(1),LV=(0)  FREE STORAGE                                 
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         ICM   R1,15,ATSARBF2      HAVE A(TSAR BUFFER2)?                        
         BZ    WRI20               NO - DON'T FREE                              
         ICM   R0,15,=A(TSA2BUFL)  1 MEG BUFFER                                 
         FREEMAIN RC,A=(1),LV=(0)  FREE STORAGE                                 
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
WRI20    CLI   TWACOMMN,1          PROCESS HEADERS REQ?                         
         BE    XIT                  YES                                         
         OC    RPT2ID,RPT2ID       IF THERE IS A SECOND REPORT                  
         BZ    XIT                                                              
         TM    OPTIND5,OPT5CONT    AND IT'S NOT A CONTINUATION                  
         BNZ   XIT                                                              
         L     R9,SYSPARMS         START AGAIN FROM THE TOP                     
         LR    R0,R5               CLEAR W/S                                    
         ICM   R1,15,=AL4(LENWORK)                                              
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     WRI10                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* INITIALIZE SYSTEM ADDRESSES                                                   
*                                                                               
SYSINIT  NTR1                                                                   
*                                  GET TERMINAL VALUES                          
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAORIG                                                   
         MVC   SBUSERID,TWAORIG                                                 
         MVC   AGENCY,TWAAGY                                                    
         MVC   SBTWAACS,TWAACCS                                                 
*                                                                               
*                                  SET UP CERTAIN ADDRESSES                     
         L     R1,SYSPARMS                                                      
         LM    R2,R4,8(R1)         A(TIOB) A(TIA) A(COMFACS)                    
         L     R2,0(R1)            NO IDEA WHAT WAS AT 8(R1) BUT...             
         ST    R2,ATIOB            ...TIOB IS AT 0(R1)  EJOR                    
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         MVC   GETPROF,CGETPROF                                                 
***                                                                             
* SET UP SECRET BLOCK SO OTHER OVERLAYS DONT NEED TO CALL IT                    
***                                                                             
         OC    T204FFD+4(2),T204FFD+4    ON NEW SECURITY?                       
         BNZ   *+14                      YES                                    
         OC    T204FFD+6(2),T204FFD+6    LIMIT ACCESS?                          
         BZ    SYS0                      NO, DONT NEED TO CALL SECRET           
*                                                                               
         LH    R6,=Y(SECBLK-T204FFD)                                            
         AR    R6,RA                                                            
         ST    R6,ASECBLK                                                       
*                                                                               
         GOTO1 CSECRET,DMCB,('SECPINIT',ASECBLK),0                              
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
SYS0     OC    TWAVPRNT,TWAVPRNT   TEST OFFLINE                                 
         BNZ   SYS1                YES                                          
         GOTO1 CSWITCH,DMCB,X'00FFFFFF'  NO-GET PROGRAM NAME                    
         L     R1,0(R1)                                                         
         USING TCBD,R1                                                          
         MVC   PROGNAME,TCBLNPRG                                                
         DROP  R1                                                               
         CLI   PROGNAME,C'P'       TEST PRORAM = PBE                            
         BNE   *+12                                                             
         CLI   CONREC,C'P'         YES-TEST RECORD = PBE                        
         BNE   BADREC              NO-ERROR                                     
*                                                                               
SYS1     LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
*                                                                               
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
*                                                                               
         L     RF,=V(DDUCOM)                                                    
         A     RF,RELO                                                          
         ST    RF,VDDUCOM                                                       
*                                                                               
* BINSRCH NOW IN COMFACS                                                        
         L     RF,ACOMFACS                                                      
         MVC   BINSRCH,CBINSRCH-COMFACSD(RF)                                    
         MVC   SBABINSR,BINSRCH    SET A(BINSRCH)                               
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
SYS4     CLI   0(R2),0             MODULE NO-OP'D?                              
         BE    SYS4A                YES                                         
         MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
*                                                                               
SYS4A    LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,SYS4                                                          
*                                                                               
* FUCKING X'FF' LEFT OVER IN 4(R1) FROM SOMETHING ABOVE...                      
         MVC   DMCB+4(4),=X'D9000A79'                                           
*         MVI   DMCB+7,X'79'                                                    
         GOTO1 (RF),(R1),0                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PWCALC,DMCB         SAVE MODULE ADDRESS                          
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   TEST OFFLINE                                 
         BZ    SYS5                                                             
         B     SYS5                ** REMOVE IF WANT CORERES IN DUMP **         
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         L     RE,SPOTMKRK         GET SPOTMKRK IN DUMPS                        
         ST    RE,MCUSRDMP                                                      
         SR    RF,RF                                                            
         ICM   RF,3,=X'7800'                                                    
         LA    RE,0(RE,RF)                                                      
         ST    RE,MCUSRDMP+4                                                    
         DROP  R1                                                               
*                                                                               
SYS5     MVC   GETBROAD,GETBROD2   MODULES NOW CORERES                          
         MVC   SBGETBRD,GETBROAD   PASS GETBROAD TO SPOTBUY                     
         MVC   DAYUNPK,DAYUNPK2                                                 
*                                                                               
         GOTO1 CALLOV,DMCB,(X'0C',0),0,0  LOAD SPWRI0C (T2040C)                 
         L     R2,DMCB                                                          
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    RF,NSYSCOMM                                                      
*                                                                               
SYS6     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   RF,SYS6                                                          
         ST    R2,CURSERR          AND SET A(CURSERR)                           
         STC   R3,CURSERR                                                       
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   TEST OFFLINE                                 
         BZ    SYS7                                                             
***                                                                             
* INIT TSAR 10/17/02 AKAT                                                       
***                                                                             
         ICM   R0,15,=A(TSARBUFL)  LENGTH OF BUFFER                             
         GETMAIN  RU,LV=(0),LOC=(ANY,ANY)    31-BIT STORAGE                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ATSARBF          A(BUFFER)                                    
*                                                                               
         OC    ATSARBF,ATSARBF     BUFFER PRESENT?                              
         BNZ   *+6                 YES                                          
         DC    H'0'                WHERE ARE WE SUPPOSED TO KEEP RECS?          
*                                                                               
         XC    DMCB(24),DMCB       GET A(TSAROFF)                               
         MVC   DMCB+4(4),=X'D9000A7D'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'         ERROR GETTING A(TSAROFF)?                    
         BNE   *+6                 NO                                           
         DC    H'0'                CANNOT PROCEED W/O A(TSAROFF)                
         MVC   ATSAROF,DMCB                                                     
*                                                                               
         XC    TSAREA,TSAREA                                                    
         LA    R2,TSAREA                                                        
         ST    R2,SBTSARBF         A(TSAREA) FOR SPOTIO                         
         USING TSARD,R2                                                         
*                                                                               
         MVI   TSOFFACT,TSAINI     ACTION = INIT                                
         MVC   TSABUF,ATSARBF      SET A(BUFFER)                                
         MVC   TSAREC,=A(TSARBUFL) SET BUFFLEN HERE ON INIT CALL                
         MVI   TSKEYL,TSARKEYL     KEY LEN                                      
         OI    TSRECI,TSRVAR       SET VARIABLE LENGTH RECS                     
         MVC   TSRECL,=Y(4000)     MAX REC LEN                                  
         OI    TSIND2,TSI2MANY     N'RECS IS FULLWORD                           
         GOTO1 ATSAROF,(R2)                                                     
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
         XC    COMSEQ,COMSEQ       INIT BUY COMMENT SEQUENCE NUMBER             
*                                                                               
         GOTO1 CALLOV,DMCB,(X'0B',0),0,0  YES-LOAD SPWRI0B (T2040B)             
         L     R2,DMCB                                                          
         SR    R3,R3                                                            
         LA    R4,SYSCOMM2                                                      
         LA    RF,NSYSCOM2                                                      
SYS6A    ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   RF,SYS6A                                                         
***                                                                             
* IF SPWRIWORKD EVER GETS REWORKED, MOVE THESE INTO SYSCOMM2 AND                
* DON'T FORGET TO PUT IN SPARE ROUTINES FOR NEXT TIME!                          
***                                                                             
         LA    R4,PUTNINV                                                       
         ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
*                                                                               
         LA    R3,1(R3)                                                         
         LA    R4,GETNINV                                                       
         ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
*                                                                               
         LA    R3,1(R3)                                                         
         LA    R4,GETSLP                                                        
         ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
*                                                                               
         LA    R3,1(R3)                                                         
         LA    R4,PUTSLP                                                        
         ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
*                                                                               
         LA    R3,1(R3)                                                         
         LA    R4,GETSTABF                                                      
         ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
*                                                                               
         LA    R3,1(R3)                                                         
         LA    R4,GETACOM                                                       
         ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
*                                                                               
SYS7     DS    0H                                                               
*        XC    LNAMPOOL,LNAMPOOL                                                
         EJECT                                                                  
* OTHER INITIALIZATION                                                          
*                                                                               
*                                  SEED SYSD WITH DUMP COMMENTS                 
         MVC   DUMPSYSD,=C'**SYSD**'                                            
         MVC   DUMPSPIO,=C'*SPOTIO*'                                            
         LH    R1,=Y(BUFF-8-SYSD)                                               
         LA    R1,SYSD(R1)                                                      
         MVC   0(8,R1),=C'**WEEKS*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,AWEEKS                                                        
         LA    R1,NWEEKS*4(R1)                                                  
         MVC   0(4,R1),=C'MONS'                                                 
         LA    R1,4(R1)                                                         
         ST    R1,AMONTHS                                                       
         LA    R1,NMONTHS*4(R1)                                                 
         MVC   0(4,R1),=C'QTRS'                                                 
         LA    R1,4(R1)                                                         
         ST    R1,AQTRS                                                         
         LA    R1,NQTRS*4(R1)                                                   
         MVC   0(4,R1),=C'DAYS'                                                 
         LA    R1,4(R1)                                                         
         ST    R1,ADAYS                                                         
         LA    R1,NDAYS*4(R1)                                                   
         MVC   0(4,R1),=C'YEAR'                                                 
         LA    R1,4(R1)                                                         
         ST    R1,AYEARS                                                        
         LA    R1,NYEARS*4(R1)                                                  
         MVC   0(4,R1),=C'BMON'                                                 
         LA    R1,4(R1)                                                         
         ST    R1,ABILMNTH                                                      
         LA    R1,NBILMNTH*6(R1)                                                
         MVC   0(4,R1),=C'HYRS'                                                 
         LA    R1,4(R1)                                                         
         ST    R1,AHYEARS                                                       
*                                                                               
         L     R1,AWEEKS                                                        
         LA    R1,LDATES(R1)                                                    
         MVC   0(8,R1),=C'**BKLST*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,SBABKLST                                                      
         MVI   SBABKLST,0                                                       
*         LA    R1,360(R1)                                                      
*         LA    R1,450(R1)                                                      
         LA    R1,954(R1)              (53 * 2) * 9 BYTE ENTRIES                
         MVC   0(8,R1),=C'**DPG***'                                             
         LA    R1,8(R1)                                                         
         ST    R1,ADPGPROG                                                      
         ST    R1,DRSTBUF                                                       
         AHI   R1,4000                                                          
         ST    R1,DRENDBUF                                                      
         MVC   0(8,R1),=C'**DPGIO*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,DRSPTIO                                                       
*                                                                               
* READ WL PROFILE                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0WL'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),WRIMED                                                 
         MVC   WORK+7(3),WRICLT                                                 
         GOTO1 GETPROF,DMCB,(X'C0',WORK),WLPROF,DATAMGR                         
         CLI   WLPROF+1,105        TEST LESS THAN 105 WEEKS                     
         BH    *+8                  NO                                          
         MVI   WLPROF+1,105        ALLOW ANY 2 YEAR REQUEST                     
*                                                                               
         CLI   DDS,C'Y'            SKIP CHECK FOR DDS                           
         BE    SYS7C                                                            
         CLI   WLPROF,C'Y'         AGY ON SECURITY?                             
         BNE   SYS7C                                                            
*                                                                               
SYS7B    TM    AUTH,X'40'          UNLESS ACCESS                                
         BNZ   SYS7C                                                            
         OI    GENSTAT4,NODELLST                                                
         OI    GENSTAT5,NOCHGLST                                                
*                                                                               
SYS7C    DS    0H                                                               
*                                                                               
*        IF OFF-LINE NEED TO REASSIGN DPG BUFFER TO GETMAIN                     
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   NO PRINTER MEANS ON-LINE                     
         BZ    SYS8                                                             
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(20*1024)             20K BUFFER                              
*                                                                               
         LA    R4,DRSTBUF          BUFFER ADDRESS SAVEAREA                      
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         ICM   R1,15,DRSTBUF       BUFFER START ADDRESS                         
         ST    R1,DRSTBSV          SAVE BUFFER ADDR FOR FREEMAIN                
         STCM  R3,15,0(R1)         SAVE BUFFER SIZE BEFORE BUFFER START         
         LA    R1,4(R1)            FIND BUFFER START                            
         STCM  R1,15,DRSTBUF       RESET NEW BUFFER START                       
         STCM  R1,15,ADPGPROG                                                   
         LA    R1,0(R3,R1)         BUFFER END ADDRESS                           
         AHI   R1,-4               WE GRABBED 4 BYTES AT BEGINNING              
         STCM  R1,15,DRENDBUF                                                   
*                                                                               
*                                  SET SYSTEM DEPENDENT VALUES                  
SYS8     MVC   SYSDUMMY,DUMMY      END OF SYSTEM BASE                           
         MVI   SYSTEM,C'S'         SPOT                                         
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 1000 BYTES                       
         MVC   GETUSER,VALUSER     ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
         MVI   GETMSYS,24          USES GETMSG FOR SYSTEM 24                    
         MVC   LWORK,=AL4(LENWORK)    WE TOOK XXXXX BYTES IN NMOD               
         MVC   RCPROG(2),=C'SP'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9020400'    PRESET FOR SYSTEM CALLOVS               
         MVI   FILTIDNO,8          FILTER ID NUM                                
**NOP    OI    GENSTAT2,DISTHSPG                                                
         OI    GENSTAT4,CONFDEL    CONFIRM DELETES                              
         OI    GENSTAT5,GENPRVAL                                                
*                                                                               
         MVI   NTWA,1                                                           
         LH    R1,=Y(STARTSV-SYSD)                                              
         LA    R1,SYSD(R1)                                                      
         ST    R1,ASTARTSV                                                      
*                                                                               
         CLI   DDS,C'Y'                                                         
         BNE   *+8                                                              
         NI    WRIRFLH+1,X'FF'-X'0C'  SET NORMAL INTENSITY                      
*                                                                               
         LA    R1,RECACTS2         ALLOW SOON JOBS                              
         ST    R1,ARECACT                                                       
*                                                                               
         MVC   SBPRINT,VPRINT      A(PRINT)                                     
*                                                                               
         GOTO1 INITDRON            INITIALIZE DRONE EARLY                       
*                                                                               
         MVI   ALTPHS,0                                                         
         SR    RE,RE                                                            
         ICM   RE,1,CONRECH+5                                                   
         BZ    SYSINTX                                                          
         BCTR  RE,0                                                             
*                                                                               
         CLC   =X'28BB',USERID     ONLY PBE ALLOWED FOR MVMFI                   
         BE    SYS15                                                            
*                                                                               
         EX    RE,WRITER           TEST USER RECORD                             
         BNE   *+12                 NO                                          
         OI    SBIOFLAG,SBSKDEL    SET NO DELETED RECORDS                       
         B     SYSINTX                                                          
*                                                                               
         EX    RE,OLDWRI                                                        
         BE    SYSINTX                                                          
*                                                                               
         EX    RE,SL                                                            
         BNE   SYS14A                                                           
         LA    R1,RECACTS          DISALLOW SOON SL JOBS                        
         CLC   AGENCY,=C'SJ'                                                    
         BE    *+14                                                             
         CLC   AGENCY,=C'H9'                                                    
         BNE   *+8                                                              
         LA    R1,RECACTS2         ALLOW SOON SL FOR SJ & H9                    
         ST    R1,ARECACT                                                       
*                                                                               
SYS14A   EX    RE,W3                                                            
         BE    SYSINTX                                                          
         EX    RE,CLOSEOUT                                                      
         BE    SYSINTX                                                          
****                                                                            
**       EX    RE,FTP              DEFUNCT!                                     
**       BE    SYSINTX                                                          
****                                                                            
         LTR   RE,RE                                                            
         BZ    *+12                                                             
         EX    RE,SUMMARY                                                       
         BE    SYSINTX                                                          
         CLI   CONRECH+5,3                                                      
         BNE   *+14                                                             
         CLC   CONREC(3),=C'PRE'                                                
         BE    SYSINTX                                                          
         LTR   RE,RE               TEST RECORD=TRANSMIT                         
         BZ    SYS15               (AT LEAST 2 CHARACTERS)                      
         EX    RE,TRANSMIT                                                      
         BNE   SYS15                                                            
         OI    REQIND,REQITRAN     YES                                          
*                                  USER RECORD -                                
SYS15    L     R1,ARECACT          GET FULL NAME FROM RECACT TABLE              
*                                                                               
SYS16    CLI   0(R1),4                                                          
         BNE   SYS18                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   1(0,R1),CONREC                                                   
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     SYS16                                                            
         MVC   CONREC(8),1(R1)                                                  
*                                                                               
         CLC   =X'28BB',USERID     ONLY PBE ALLOWED FOR MVMFI                   
         BNE   *+14                                                             
         CLC   CONREC,=CL8'PBE'                                                 
         BNE   BADREC                                                           
*                                                                               
         TM    REQIND,REQITRAN     TEST RECORD=TRANSMIT                         
         BZ    SYS17                                                            
         LLC   RE,9(R1)            YES-USE RECORD NUMBER FOR ALTERNATE          
         CVD   RE,DUB                  PHASE NUMBER FOR KEYING PROGRAM          
         UNPK  HALF,DUB                RECORDS                                  
         IC    RE,HALF                                                          
         ICM   RF,8,HALF+1                                                      
         SLL   RF,4                                                             
         SLDL  RE,4                                                             
         STC   RE,ALTPHS                                                        
         B     SYSINTX                                                          
*                                                                               
SYS17    LA    R1,CONREC                                                        
         LA    RE,8                                                             
         CLI   0(R1),C' '                                                       
         BNH   *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,*-12                                                          
         LNR   RE,RE                                                            
         LA    RE,8(RE)                                                         
         STC   RE,CONRECH+5                                                     
*                                                                               
SYS18    LA    R1,CONRECH                                                       
         ST    R1,DRSPTFLD                                                      
         OI    DRFLAGS,DREXPDIC                                                 
         MVI   DRACTION,DRROW                                                   
         GOTO1 DRONE,DMCB,DRGEN    VALIDATE THE RECORD                          
         CLI   DRERROR,0                                                        
         BNE   BADREC                                                           
         CLI   DRATTRIB,C'P'       CHECK ATTRIBUTE FOR REPORT INDICATOR         
         BNE   BADREC                                                           
         MVC   RPTOVLY,DRATTRIB+1  YES - SAVE THE REPORT OVERLAY NUMBER         
         CLI   RPTOVLY,0           CHECK IT'S NOT ZERO                          
         BE    BADREC                                                           
         MVC   DPGFILE,DRATTRIB+2  SAVE DPG FILE NUMBER, IF ANY                 
         MVC   RPTSCRN,DRATTRIB+3  SAVE SCREEN NUMBER, IF ANY                   
*                                                                               
SYSINTX  CR    RB,RB               NORMAL EXIT                                  
         B     XIT                                                              
         SPACE 2                                                                
BADREC   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(14),=C'INVALID RECORD'                                   
         OI    CONHEADH+6,X'80'                                                 
         OI    CONRECH+6,X'40'     INSERT CURSOR                                
         LTR   RB,RB               ERROR XIT                                    
         B     XIT                                                              
         SPACE 2                                                                
WRITER   CLC   CONREC(0),=C'WRITER'  EXECUTED INSTRUCTIONS                      
OLDWRI   CLC   CONREC(0),=C'OLDWRI'                                             
SL       CLC   CONREC(0),=C'SL      '                                           
W3       CLC   CONREC(0),=C'W3      '                                           
TRANSMIT CLC   CONREC(0),=C'TRANSMIT'                                           
CLOSEOUT CLC   CONREC(0),=C'CLOSEOUT'                                           
****   FTP      CLC   CONREC(0),=C'FTP'     DEFUNCT!                            
SUMMARY  CLC   CONREC(0),=C'SUMMARY'                                            
*                                                                               
         EJECT                                                                  
*&&DO                                                                           
* IF REQUEST IS LIMITED, THEN ALLOW SOON REQUESTS                               
* OUTPUT : CC EQ - SOON ALLOWED                                                 
*          CC NE - SOON NOT ALLOWED                                             
*                                                                               
SOONCHEK LR    R0,RE                                                            
         CLI   WRIMED,C'*'         NOT MEDIA=*                                  
         BE    SOONNO                                                           
         CLI   WRICLTH+5,0         SINGLE CLIENT                                
         BE    SOONNO                                                           
         CLC   WRICLT(3),=C'ALL'                                                
         BE    SOONNO                                                           
         CLI   WRICLT,C'*'                                                      
         BE    SOONNO                                                           
         CLI   WRICLT,C'$'                                                      
         BE    SOONNO                                                           
         CLI   WRICLT,C'='                                                      
         BE    SOONNO                                                           
         LA    R1,WRICOLSH         TEST ANY DEMOS IN COLUMNS                    
         LA    RF,14                                                            
*                                                                               
SOON2    CLI   5(R1),0                                                          
         BE    SOON6                                                            
         LLC   RE,5(R1)                                                         
         SH    RE,=H'2'                                                         
         BNP   SOON6                                                            
         LA    R3,8(R1)                                                         
*                                                                               
SOON4    CLC   0(3,R3),=C'DEM'                                                  
         BE    SOON7                                                            
         CLC   0(3,R3),=C'CPP'                                                  
         BE    SOON7                                                            
         LA    R3,1(R3)                                                         
         BCT   RE,SOON4                                                         
*                                                                               
SOON6    LLC   RE,0(R1)                                                         
         AR    R1,RE                                                            
         IC    RE,0(R1)                                                         
         AR    R1,RE                                                            
         BCT   RF,SOON2                                                         
*                                  NO-CLIENT ACCOUNTING NEEDS ONLY              
         B     SOONYES             SINGLE CLIENT (PERIOD CHECKED LATER)         
*                                                                               
SOON7    CLI   WRIESTH+5,0         SINGLE ESTIMATE                              
         BE    SOONNO                                                           
         TM    WRIESTH+4,X'08'                                                  
         BZ    SOONNO                                                           
         CLI   WRIPRDH+5,0         SINGLE PRD / ALL MKTS                        
         BE    SOONNO              OR MULTIPLE PRDS / SINGLE MKT                
         CLC   WRIPRD(3),=C'ALL'                                                
         BE    SOON8                                                            
         CLC   WRIPRD(3),=C'POL'                                                
         BE    SOON8                                                            
         CLC   WRIPRD(4),=C'PGR='                                               
         BNE   SOONYES                                                          
*                                                                               
SOON8    CLI   WRIMKTH+5,0                                                      
         BE    SOONNO                                                           
         CLC   AGENCY,=C'WD'       IF AGENCY=WD,                                
         BNE   *+14                                                             
         CLC   WRIMKT(4),=C'MGR='  MARKET GROUPS ARE OK                         
         BE    SOONYES                                                          
         TM    WRIMKTH+4,X'08'                                                  
         BZ    SOONNO                                                           
*                                                                               
SOONYES  LR    RE,R0                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
SOONNO   LTR   RE,R0                                                            
         BR    RE                                                               
*&&                                                                             
         EJECT                                                                  
* ROUTINE EXECUTED BEFORE GOING TO GENCON                                       
*                                                                               
CHKTOP   LR    R0,RE                                                            
         LLC   RE,CONACTH+5                                                     
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BM    CTX                                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=CL8'REPORT' IF ACTION IS NOT REPORT                   
         BE    CTX                                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=C'LIST'  OR LIST                                      
         BE    CTX                                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=C'SELECT'   OR REPORT SELECT                          
         BE    CTX                                                              
         XC    CONWHEN,CONWHEN     THEN ERASE PRINT OPTION FIELD                
         OI    CONWHENH+6,X'80'                                                 
         MVI   CONWHENH+5,0                                                     
CTX      LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
CORETAB  DS    0C                  TABLE OF CORE-RESIDENT MODULES               
         DC    AL1(QDEMOVAL)                                                    
         DC    AL1(QSPOTMKR)                                                    
         DC    AL1(QSTAPACK)                                                    
         DC    AL1(QRCPACK)                                                     
         DC    AL1(QDAYUNPK)                                                    
         DC    AL1(QGENCON)                                                     
         DC    AL1(QDRONE)                                                      
         DC    AL1(QSPOTIO)                                                     
         DC    AL1(QSPOTBUY)                                                    
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QMOBILE)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QSPOTGL)                                                     
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(0)              WAS WRIGEN                                   
         DC    AL1(QUPVAL)                                                      
         DC    AL1(QGETBROD)                                                    
         DC    AL1(QSPOTSLK)                                                    
         DC    AL1(QGENPRG)                                                     
         DC    AL1(QSTAVAL)                                                     
CORES    EQU   (*-CORETAB)                                                      
*                                                                               
TABLES   DS    0A                                                               
         DC    A(RECACTS)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                                                               
SYSVCON  DS    0A                                                               
         DC    A(0)                                                             
         DC    V(EQVRD)                                                         
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    V(COVAIL)                                                        
         DC    V(DPTRD)                                                         
         DC    V(SPBVAL)                                                        
         DC    V(DUMMY)                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* TABLES OF RECORDS ACTIONS AND COMBINATIONS                                    
*                                                                               
RECACTS  DS    0D                                                               
*                                                                               
* X'01' ENTRIES ARE AVAILABLE REPORTS                                           
*                                                                               
* CL8 EXPANDED REPORT NAME                                                      
* CL1 REPORT NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
*                                                                               
         DC    X'04',C'WRITER  ',AL1(01),X'0000'    WRITER                      
         DC    X'04',C'PBE     ',AL1(02),X'0000'    POST BUY EXCEPTION          
         DC    X'04',C'SCH     ',AL1(03),X'0000'    SCHEDULE REPORT             
         DC    X'04',C'RRS     ',AL1(04),X'0000'    RADIO ROTATIONAL            
*        DC    X'04',C'CSRSUM  ',AL1(05),X'0000'    COKE STATUS SUMMARY         
*        DC    X'04',C'CSRDTL  ',AL1(06),X'0000'    COKE STATUS DETAIL          
         DC    X'04',C'ALAN    ',AL1(07),X'0000'    ALLOCATION ANALYSIS         
         DC    X'04',C'PG      ',AL1(08),X'0000'    P&G DATA COLLECTION         
*        DC    X'04',C'ATT     ',AL1(09),X'0000'    AT&T TAPES                  
         DC    X'04',C'GFTAPE  ',AL1(10),X'0000'    GF TAPE                     
         DC    X'04',C'SBS     ',AL1(11),X'0000'    SPOT BUYING SCHED           
         DC    X'04',C'PRE     ',AL1(12),X'0000'    WRITER 'PRE' REPORT         
         DC    X'04',C'W3      ',AL1(13),X'0000'    WRITER W3 REPORT            
         DC    X'04',C'SL      ',AL1(14),X'0000'    STATION LOCKIN              
*        DC    X'04',C'DO      ',AL1(15),X'0000'    DRAFT ORDER                 
*        DC    X'04',C'BRS     ',AL1(16),X'0000'    BRAND TIME SCHEDULE         
*        DC    X'04',C'PC      ',AL1(18),X'0000'    PC DATA INTERFACE           
         DC    X'04',C'TBA     ',AL1(19),X'0000'    BUY ACTIVITY REPORT         
         DC    X'04',C'CLOSEOUT',AL1(20),X'0000'    ESTIMATE CLOSEOUT           
*        DC    X'04',C'SUMMARY ',AL1(22),X'0000'    SUMMARY RECORD              
*        DC    X'04',C'FTP     ',AL1(24),X'0000'    FILE TRANSFER PROGM         
         DC    X'04',C'CASHFLOW',AL1(25),X'0000'    CASH FLOW                   
         DC    X'04',C'TRANSMIT',AL1(26),X'0000'    DOWNLOAD TRANSMIT           
         DC    X'04',C'HDBILL  ',AL1(27),X'0000'    HOME DEPOT BILLING          
         DC    X'04',C'MSCJ    ',AL1(28),X'0000'    SC JOHNSON MED TAPE         
         DC    X'04',C'ISCJ    ',AL1(29),X'0000'    SC JOHNSON INV TAPE         
         DC    X'04',C'DC      ',AL1(30),X'0000'    MEDIA CALENDAR              
         DC    X'04',C'BGL     ',AL1(31),X'0000'    BUYING GUIDELINES           
         DC    X'04',C'IAS     ',AL1(32),X'0000'    INV APPROVAL STATUS         
         DC    X'04',C'BAS     ',AL1(33),X'0000'    BUY CHECKING                
         DC    X'04',C'CFAFTER ',AL1(34),X'0000'    CASH FLOW                   
*        DC    X'04',C'AGYSUM  ',AL1(35),X'0000'    AGENCY SUMMARY (PW)         
         DC    X'04',C'PWREC   ',AL1(36),X'0000'    PW RECORD LIST              
         DC    X'04',C'FLIST   ',AL1(37),X'0000'    FORMAT LIST                 
*        DC    X'04',C'THDBILL ',AL1(38),X'0000'    HOME DEPOT BILLING          
         DC    X'04',C'OLDWRI  ',AL1(39),X'0000'    OLD WRITER                  
         DC    X'04',C'RER     ',AL1(40),X'0000'    RETAIL EXPEND. REPT         
         DC    X'04',C'ORDBIL  ',AL1(41),X'0000'    ORD VS BILL REPORT          
         DC    X'04',C'KRTAPE  ',AL1(42),X'0000'    KRAFT TAPE                  
         DC    X'04',C'SCJE    ',AL1(43),X'0000'    SC JOHNSON EST              
         SPACE 3                                                                
* X'02' ENTRIES ARE AVAILABLE ACTIONS                                           
*                                                                               
* CL8 EXPANDED ACTION NAME                                                      
* CL1 ACTION NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 SPARE                                                                     
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,02,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,03,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,04,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,05,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,06,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
**NOP         DC    X'02',C'HELP    ',AL1(00,00,00)                             
         EJECT                                                                  
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
         SPACE 3                                                                
* X'03' ENTRIES ARE OK REC/ACT COMBOS                                           
* CL1 RECORD NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 PHASE NUMBER FOR SCREEN                                                   
* CL1 PHASE NUMBER FOR EDIT                                                     
* CL1 PHASE NUMBER FOR SPECS                                                    
* CL1 PHASE NUMBER FOR REPORT                                                   
* CL1 WHEN OK BITS 80=SCREEN 40=NOW 20=SOON 10=OV 08=DDS                        
* CL2 CODE FOR REPORTS                                                          
* CL2 CODE FOR EOD HANDLING                                                     
         SPACE 1                                                                
*                                 SC  SP  OK                                    
*                                   OV  RP                                      
         DC    X'03',AL1(01,01),X'F1010000C0',C'    '  WRITER   ADD             
         DC    X'03',AL1(01,02),X'F1010000C0',C'    '  WRITER   CHANGE          
         DC    X'03',AL1(01,03),X'F1010000C0',C'    '  WRITER   DISPLAY         
         DC    X'03',AL1(01,04),X'F1010000C0',C'    '  WRITER   DELETE          
         DC    X'03',AL1(01,05),X'F101000018',C'RWRW'  WRITER   SELECT          
         DC    X'03',AL1(01,06),X'F1010000C0',C'    '  WRITER   RESTORE         
         DC    X'03',AL1(01,10),X'F1010000C0',C'    '  WRITER   LIST            
         DC    X'03',AL1(01,12),X'F101000118',C'RWRW'  WRITER   REPORT          
*                                                                               
         DC    X'03',AL1(02,01),X'E1010000C0',C'    '  PBE      ADD             
         DC    X'03',AL1(02,02),X'E1010000C0',C'    '  PBE      CHANGE          
         DC    X'03',AL1(02,03),X'E1010000C0',C'    '  PBE      DISPLAY         
         DC    X'03',AL1(02,04),X'E1010000C0',C'    '  PBE      DELETE          
         DC    X'03',AL1(02,05),X'E101000018',C'PBRW'  PBE      SELECT          
         DC    X'03',AL1(02,06),X'E1010000C0',C'    '  PBE      RESTORE         
         DC    X'03',AL1(02,10),X'E1010000C0',C'    '  PBE      LIST            
         DC    X'03',AL1(02,12),X'E101000118',C'PBRW'  PBE      REPORT          
*                                                                               
         DC    X'03',AL1(03,01),X'E2010000C0',C'    '  SCH      ADD             
         DC    X'03',AL1(03,02),X'E2010000C0',C'    '  SCH      CHANGE          
         DC    X'03',AL1(03,03),X'E2010000C0',C'    '  SCH      DISPLAY         
         DC    X'03',AL1(03,04),X'E2010000C0',C'    '  SCH      DELETE          
         DC    X'03',AL1(03,05),X'E201000018',C'SCRW'  SCH      SELECT          
         DC    X'03',AL1(03,06),X'E2010000C0',C'    '  SCH      RESTORE         
         DC    X'03',AL1(03,10),X'E2010000C0',C'    '  SCH      LIST            
         DC    X'03',AL1(03,12),X'E201000118',C'SCRW'  SCH      REPORT          
*                                                                               
         DC    X'03',AL1(04,01),X'E3010000C0',C'    '  RRS      ADD             
         DC    X'03',AL1(04,02),X'E3010000C0',C'    '  RRS      CHANGE          
         DC    X'03',AL1(04,03),X'E3010000C0',C'    '  RRS      DISPLAY         
         DC    X'03',AL1(04,04),X'E3010000C0',C'    '  RRS      DELETE          
         DC    X'03',AL1(04,05),X'E301000018',C'RRRW'  RRS      SELECT          
         DC    X'03',AL1(04,06),X'E3010000C0',C'    '  RRS      RESTORE         
         DC    X'03',AL1(04,10),X'E3010000C0',C'    '  RRS      LIST            
         DC    X'03',AL1(04,12),X'E301000118',C'RRRW'  RRS      REPORT          
*                                                                               
*        DC    X'03',AL1(05,12),X'E401000118',C'CSRW'  CSRSUM   REPORT          
*        DC    X'03',AL1(06,12),X'E501000118',C'CDRW'  CSRDTL   REPORT          
*                                                                               
         DC    X'03',AL1(07,01),X'E6010000C0',C'    '  ALAN     ADD             
         DC    X'03',AL1(07,02),X'E6010000C0',C'    '  ALAN     CHANGE          
         DC    X'03',AL1(07,03),X'E6010000C0',C'    '  ALAN     DISPLAY         
         DC    X'03',AL1(07,04),X'E6010000C0',C'    '  ALAN     DELETE          
         DC    X'03',AL1(07,05),X'E601000018',C'ALRW'  ALAN     SELECT          
         DC    X'03',AL1(07,06),X'E6010000C0',C'    '  ALAN     RESTORE         
         DC    X'03',AL1(07,10),X'E6010000C0',C'    '  ALAN     LIST            
         DC    X'03',AL1(07,12),X'E601000118',C'ALRW'  ALAN     REPORT          
*                                                                               
         DC    X'03',AL1(08,01),X'E7010000C0',C'    '  PG       ADD             
         DC    X'03',AL1(08,02),X'E7010000C0',C'    '  PG       CHANGE          
         DC    X'03',AL1(08,03),X'E7010000C0',C'    '  PG       DISPLAY         
         DC    X'03',AL1(08,04),X'E7010000C0',C'    '  PG       DELETE          
         DC    X'03',AL1(08,05),X'E701000018',C'PGRW'  PG       SELECT          
         DC    X'03',AL1(08,06),X'E7010000C0',C'    '  PG       RESTORE         
         DC    X'03',AL1(08,10),X'E7010000C0',C'    '  PG       LIST            
         DC    X'03',AL1(08,12),X'E701000118',C'PGRW'  PG       REPORT          
*                                                                               
*        DC    X'03',AL1(09,12),X'E801000118',C'ATAT'  ATT      REPORT          
*                                                                               
         DC    X'03',AL1(10,01),X'E9010000C0',C'    '  GFTAPE   ADD             
         DC    X'03',AL1(10,02),X'E9010000C0',C'    '  GFTAPE   CHANGE          
         DC    X'03',AL1(10,03),X'E9010000C0',C'    '  GFTAPE   DISPLAY         
         DC    X'03',AL1(10,04),X'E9010000C0',C'    '  GFTAPE   DELETE          
         DC    X'03',AL1(10,05),X'E901000018',C'GFGF'  GFTAPE   SELECT          
         DC    X'03',AL1(10,06),X'E9010000C0',C'    '  GFTAPE   RESTORE         
         DC    X'03',AL1(10,10),X'E9010000C0',C'    '  GFTAPE   LIST            
         DC    X'03',AL1(10,12),X'E901000118',C'GFGF'  GFTAPE   REPORT          
*                                                                               
         DC    X'03',AL1(11,01),X'EA010000C0',C'    '  SBS      ADD             
         DC    X'03',AL1(11,02),X'EA010000C0',C'    '  SBS      CHANGE          
         DC    X'03',AL1(11,03),X'EA010000C0',C'    '  SBS      DISPLAY         
         DC    X'03',AL1(11,04),X'EA010000C0',C'    '  SBS      DELETE          
         DC    X'03',AL1(11,05),X'EA01000018',C'BSRW'  SBS      SELECT          
         DC    X'03',AL1(11,06),X'EA010000C0',C'    '  SBS      RESTORE         
         DC    X'03',AL1(11,10),X'EA010000C0',C'    '  SBS      LIST            
         DC    X'03',AL1(11,12),X'EA01000138',C'BSRW'  SBS      REPORT          
*                                                                               
         DC    X'03',AL1(12,01),X'F1010000C0',C'    '  PRE      ADD             
         DC    X'03',AL1(12,02),X'F1010000C0',C'    '  PRE      CHANGE          
         DC    X'03',AL1(12,03),X'F1010000C0',C'    '  PRE      DISPLAY         
         DC    X'03',AL1(12,04),X'F1010000C0',C'    '  PRE      DELETE          
         DC    X'03',AL1(12,05),X'F101000018',C'RWR1'  PRE      SELECT          
         DC    X'03',AL1(12,06),X'F1010000C0',C'    '  PRE      RESTORE         
         DC    X'03',AL1(12,10),X'F1010000C0',C'    '  PRE      LIST            
         DC    X'03',AL1(12,12),X'F101000118',C'RWR1'  PRE      REPORT          
*                                                                               
         DC    X'03',AL1(13,01),X'F1010000C0',C'    '  W3       ADD             
         DC    X'03',AL1(13,02),X'F1010000C0',C'    '  W3       CHANGE          
         DC    X'03',AL1(13,03),X'F1010000C0',C'    '  W3       DISPLAY         
         DC    X'03',AL1(13,04),X'F1010000C0',C'    '  W3       DELETE          
         DC    X'03',AL1(13,05),X'F101000018',C'W3W3'  W3       SELECT          
         DC    X'03',AL1(13,06),X'F1010000C0',C'    '  W3       RESTORE         
         DC    X'03',AL1(13,10),X'F1010000C0',C'    '  W3       LIST            
         DC    X'03',AL1(13,12),X'F101000118',C'W3W3'  W3       REPORT          
*                                                                               
         DC    X'03',AL1(14,01),X'EB010000C0',C'    '  SL       ADD             
         DC    X'03',AL1(14,02),X'EB010000C0',C'    '  SL       CHANGE          
         DC    X'03',AL1(14,03),X'EB010000C0',C'    '  SL       DISPLAY         
         DC    X'03',AL1(14,04),X'EB010000C0',C'    '  SL       DELETE          
         DC    X'03',AL1(14,05),X'EB01000018',C'SLLK'  SL       SELECT          
         DC    X'03',AL1(14,06),X'EB010000C0',C'    '  SL       RESTORE         
         DC    X'03',AL1(14,10),X'EB010000C0',C'    '  SL       LIST            
         DC    X'03',AL1(14,12),X'EB01000118',C'SLLK'  SL       REPORT          
*                                                                               
*        DC    X'03',AL1(15,12),X'D101000118',C'DORW'  DO       REPORT          
*                                                                               
***      DC    X'03',AL1(16,01),X'EC010000C0',C'    '  BRS      ADD             
***      DC    X'03',AL1(16,02),X'EC010000C0',C'    '  BRS      CHANGE          
***      DC    X'03',AL1(16,03),X'EC010000C0',C'    '  BRS      DISPLAY         
***      DC    X'03',AL1(16,04),X'EC010000C0',C'    '  BRS      DELETE          
***      DC    X'03',AL1(16,05),X'EC01000018',C'BRRW'  BRS      SELECT          
***      DC    X'03',AL1(16,06),X'EC010000C0',C'    '  BRS      RESTORE         
***      DC    X'03',AL1(16,10),X'EC010000C0',C'    '  BRS      LIST            
***      DC    X'03',AL1(16,12),X'EC01000118',C'BRRW'  BRS      REPORT          
*                                                                               
*        DC    X'03',AL1(18,12),X'ED01000118',C'PCRW'  PC       REPORT          
*                                                                               
         DC    X'03',AL1(19,01),X'EE010000C0',C'    '  TBA      ADD             
         DC    X'03',AL1(19,02),X'EE010000C0',C'    '  TBA      CHANGE          
         DC    X'03',AL1(19,03),X'EE010000C0',C'    '  TBA      DISPLAY         
         DC    X'03',AL1(19,04),X'EE010000C0',C'    '  TBA      DELETE          
         DC    X'03',AL1(19,05),X'EE01000018',C'BARW'  TBA      SELECT          
         DC    X'03',AL1(19,06),X'EE010000C0',C'    '  TBA      RESTORE         
         DC    X'03',AL1(19,10),X'EE010000C0',C'    '  TBA      LIST            
         DC    X'03',AL1(19,12),X'EE01000118',C'BARW'  TBA      REPORT          
*                                                                               
         DC    X'03',AL1(20,01),X'F2200000C0',C'    '  CLOSEOUT ADD             
         DC    X'03',AL1(20,02),X'F2200000C0',C'    '  CLOSEOUT CHANGE          
         DC    X'03',AL1(20,03),X'F2200000C0',C'    '  CLOSEOUT DISPLAY         
         DC    X'03',AL1(20,04),X'F2200000C0',C'    '  CLOSEOUT DELETE          
         DC    X'03',AL1(20,05),X'F220000018',C'COCO'  CLOSEOUT SELECT          
         DC    X'03',AL1(20,06),X'F2200000C0',C'    '  CLOSEOUT RESTORE         
         DC    X'03',AL1(20,10),X'F2200000C0',C'    '  CLOSEOUT LIST            
         DC    X'03',AL1(20,12),X'F220002018',C'COCO'  CLOSEOUT REPORT          
*                                                                               
*        DC    X'03',AL1(22,12),X'F322000000',C'    '  SUMMARY  RECORDS         
*                                                                               
*        DC    X'03',AL1(24,01),X'F4010000C0',C'    '  FTP      ADD             
*        DC    X'03',AL1(24,02),X'F4010000C0',C'    '  FTP      CHANGE          
*        DC    X'03',AL1(24,03),X'F4010000C0',C'    '  FTP      DISPLAY         
*        DC    X'03',AL1(24,04),X'F4010000C0',C'    '  FTP      DELETE          
*        DC    X'03',AL1(24,05),X'F401000018',C'FTFT'  FTP      SELECT          
*        DC    X'03',AL1(24,06),X'F4010000C0',C'    '  FTP      RESTORE         
*        DC    X'03',AL1(24,10),X'F4010000C0',C'    '  FTP      LIST            
*        DC    X'03',AL1(24,12),X'F424002438',C'FTFT'  FTP      REPORT          
*                                                                               
         DC    X'03',AL1(25,01),X'D3010000C0',C'    '  CASHFLOW ADD             
         DC    X'03',AL1(25,02),X'D3010000C0',C'    '  CASHFLOW CHANGE          
         DC    X'03',AL1(25,03),X'D3010000C0',C'    '  CASHFLOW DISPLAY         
         DC    X'03',AL1(25,04),X'D3010000C0',C'    '  CASHFLOW DELETE          
         DC    X'03',AL1(25,05),X'D301000018',C'CARW'  CASHFLOW SELECT          
         DC    X'03',AL1(25,06),X'D3010000C0',C'    '  CASHFLOW RESTORE         
         DC    X'03',AL1(25,10),X'D3010000C0',C'    '  CASHFLOW LIST            
         DC    X'03',AL1(25,12),X'D301000118',C'CARW'  CASHFLOW REPORT          
*                                                                               
         DC    X'03',AL1(26,01),X'F1010000C0',C'    '  TRANSMIT ADD             
         DC    X'03',AL1(26,02),X'F1010000C0',C'    '  TRANSMIT CHANGE          
         DC    X'03',AL1(26,03),X'F1010000C0',C'    '  TRANSMIT DISPLAY         
         DC    X'03',AL1(26,04),X'F1010000C0',C'    '  TRANSMIT DELETE          
         DC    X'03',AL1(26,05),X'F101000018',C'RWRT'  TRANSMIT SELECT          
         DC    X'03',AL1(26,06),X'F1010000C0',C'    '  TRANSMIT RESTORE         
         DC    X'03',AL1(26,10),X'F1010000C0',C'    '  TRANSMIT LIST            
         DC    X'03',AL1(26,12),X'F101000118',C'RWRT'  TRANSMIT REPORT          
*                                                                               
         DC    X'03',AL1(27,01),X'D9010000C0',C'    '  HDBILL   ADD             
         DC    X'03',AL1(27,02),X'D9010000C0',C'    '  HDBILL   CHANGE          
         DC    X'03',AL1(27,03),X'D9010000C0',C'    '  HDBILL   DISPLAY         
         DC    X'03',AL1(27,04),X'D9010000C0',C'    '  HDBILL   DELETE          
         DC    X'03',AL1(27,05),X'D901000018',C'HDRW'  HDBILL   SELECT          
         DC    X'03',AL1(27,06),X'D9010000C0',C'    '  HDBILL   RESTORE         
         DC    X'03',AL1(27,10),X'D9010000C0',C'    '  HDBILL   LIST            
         DC    X'03',AL1(27,12),X'D901000118',C'HDRW'  HDBILL   REPORT          
*                                                                               
         DC    X'03',AL1(28,01),X'D8010000C0',C'    '  MSCJ     ADD             
         DC    X'03',AL1(28,02),X'D8010000C0',C'    '  MSCJ     CHANGE          
         DC    X'03',AL1(28,03),X'D8010000C0',C'    '  MSCJ     DISPLAY         
         DC    X'03',AL1(28,04),X'D8010000C0',C'    '  MSCJ     DELETE          
         DC    X'03',AL1(28,05),X'D801000018',C'SJSJ'  MSCJ     SELECT          
         DC    X'03',AL1(28,06),X'D8010000C0',C'    '  MSCJ     RESTORE         
         DC    X'03',AL1(28,10),X'D8010000C0',C'    '  MSCJ     LIST            
         DC    X'03',AL1(28,12),X'D801000118',C'SJSJ'  MSCJ     REPORT          
*                                                                               
         DC    X'03',AL1(29,01),X'DA010000C0',C'    '  ISCJ     ADD             
         DC    X'03',AL1(29,02),X'DA010000C0',C'    '  ISCJ     CHANGE          
         DC    X'03',AL1(29,03),X'DA010000C0',C'    '  ISCJ     DISPLAY         
         DC    X'03',AL1(29,04),X'DA010000C0',C'    '  ISCJ     DELETE          
         DC    X'03',AL1(29,05),X'DA01000018',C'SISI'  ISCJ     SELECT          
         DC    X'03',AL1(29,06),X'DA010000C0',C'    '  ISCJ     RESTORE         
         DC    X'03',AL1(29,10),X'DA010000C0',C'    '  ISCJ     LIST            
         DC    X'03',AL1(29,12),X'DA01000118',C'SISI'  ISCJ     REPORT          
*                                                                               
         DC    X'03',AL1(30,01),X'D6010000C0',C'    '  DC       ADD             
         DC    X'03',AL1(30,02),X'D6010000C0',C'    '  DC       CHANGE          
         DC    X'03',AL1(30,03),X'D6010000C0',C'    '  DC       DISPLAY         
         DC    X'03',AL1(30,04),X'D6010000C0',C'    '  DC       DELETE          
         DC    X'03',AL1(30,05),X'D601000018',C'DCRC'  DC       SELECT          
         DC    X'03',AL1(30,06),X'D6010000C0',C'    '  DC       RESTORE         
         DC    X'03',AL1(30,10),X'D6010000C0',C'    '  DC       LIST            
         DC    X'03',AL1(30,12),X'D601000118',C'DCRC'  DC       REPORT          
*                                                                               
         DC    X'03',AL1(31,01),X'D4010000C0',C'    '  BGL      ADD             
         DC    X'03',AL1(31,02),X'D4010000C0',C'    '  BGL      CHANGE          
         DC    X'03',AL1(31,03),X'D4010000C0',C'    '  BGL      DISPLAY         
         DC    X'03',AL1(31,04),X'D4010000C0',C'    '  BGL      DELETE          
         DC    X'03',AL1(31,05),X'D401000018',C'BGBG'  BGL      SELECT          
         DC    X'03',AL1(31,06),X'D4010000C0',C'    '  BGL      RESTORE         
         DC    X'03',AL1(31,10),X'D4010000C0',C'    '  BGL      LIST            
         DC    X'03',AL1(31,12),X'D401000118',C'BGBG'  BGL      REPORT          
*                                                                               
         DC    X'03',AL1(32,01),X'D2010000C0',C'    '  IAS      ADD             
         DC    X'03',AL1(32,02),X'D2010000C0',C'    '  IAS      CHANGE          
         DC    X'03',AL1(32,03),X'D2010000C0',C'    '  IAS      DISPLAY         
         DC    X'03',AL1(32,04),X'D2010000C0',C'    '  IAS      DELETE          
         DC    X'03',AL1(32,05),X'D201000018',C'IARW'  IAS      SELECT          
         DC    X'03',AL1(32,06),X'D2010000C0',C'    '  IAS      RESTORE         
         DC    X'03',AL1(32,10),X'D2010000C0',C'    '  IAS      LIST            
         DC    X'03',AL1(32,12),X'D201000118',C'IARW'  IAS      REPORT          
*                                                                               
         DC    X'03',AL1(33,01),X'B3010000C0',C'    '  BAS      ADD             
         DC    X'03',AL1(33,02),X'B3010000C0',C'    '  BAS      CHANGE          
         DC    X'03',AL1(33,03),X'B3010000C0',C'    '  BAS      DISPLAY         
         DC    X'03',AL1(33,04),X'B3010000C0',C'    '  BAS      DELETE          
         DC    X'03',AL1(33,05),X'B301000018',C'BCRW'  BAS      SELECT          
         DC    X'03',AL1(33,06),X'B3010000C0',C'    '  BAS      RESTORE         
         DC    X'03',AL1(33,10),X'B3010000C0',C'    '  BAS      LIST            
         DC    X'03',AL1(33,12),X'B301000118',C'BCRW'  BAS      REPORT          
*                                                                               
         DC    X'03',AL1(34,01),X'D3010000C0',C'    '  CASHFLOW ADD             
         DC    X'03',AL1(34,02),X'D3010000C0',C'    '  CASHFLOW CHANGE          
         DC    X'03',AL1(34,03),X'D3010000C0',C'    '  CASHFLOW DISPLAY         
         DC    X'03',AL1(34,04),X'D3010000C0',C'    '  CASHFLOW DELETE          
         DC    X'03',AL1(34,05),X'D301000018',C'CACF'  CASHFLOW SELECT          
         DC    X'03',AL1(34,06),X'D3010000C0',C'    '  CASHFLOW RESTORE         
         DC    X'03',AL1(34,10),X'D3010000C0',C'    '  CASHFLOW LIST            
         DC    X'03',AL1(34,12),X'D301000118',C'CACF'  CASHFLOW REPORT          
*                                                                               
*        DC    X'03',AL1(35,12),X'B501000118',C'PWRW'  AGYSUM   REPORT          
*                                                                               
         DC    X'03',AL1(36,01),X'B5010000C0',C'    '  PW REC   ADD             
         DC    X'03',AL1(36,02),X'B5010000C0',C'    '  PW REC   CHANGE          
         DC    X'03',AL1(36,03),X'B5010000C0',C'    '  PW REC   DISPLAY         
         DC    X'03',AL1(36,04),X'B5010000C0',C'    '  PW REC   DELETE          
         DC    X'03',AL1(36,05),X'B501000018',C'PWRW'  PW REC   SELECT          
         DC    X'03',AL1(36,06),X'B5010000C0',C'    '  PW REC   RESTORE         
         DC    X'03',AL1(36,10),X'B5010000C0',C'    '  PW REC   LIST            
         DC    X'03',AL1(36,12),X'B501000118',C'PWRW'  PW REC   REPORT          
*                                                                               
         DC    X'03',AL1(37,01),X'B5010000C0',C'    '  FORMAT   ADD             
         DC    X'03',AL1(37,02),X'B5010000C0',C'    '  FORMAT   CHANGE          
         DC    X'03',AL1(37,03),X'B5010000C0',C'    '  FORMAT   DISPLAY         
         DC    X'03',AL1(37,04),X'B5010000C0',C'    '  FORMAT   DELETE          
         DC    X'03',AL1(37,05),X'B501000018',C'FMRW'  FORMAT   SELECT          
         DC    X'03',AL1(37,06),X'B5010000C0',C'    '  FORMAT   RESTORE         
         DC    X'03',AL1(37,10),X'B5010000C0',C'    '  FORMAT   LIST            
         DC    X'03',AL1(37,12),X'B501000118',C'FMRW'  FORMAT   REPORT          
*                                                                               
*        DC    X'03',AL1(38,12),X'D901000118',C'HTHT'  THDBILL  REPORT          
*                                                                               
         DC    X'03',AL1(39,01),X'F1010000C0',C'    '  OLDWRI   ADD             
         DC    X'03',AL1(39,02),X'F1010000C0',C'    '  OLDWRI   CHANGE          
         DC    X'03',AL1(39,03),X'F1010000C0',C'    '  OLDWRI   DISPLAY         
         DC    X'03',AL1(39,04),X'F1010000C0',C'    '  OLDWRI   DELETE          
         DC    X'03',AL1(39,05),X'F101000018',C'RORO'  OLDWRI   SELECT          
         DC    X'03',AL1(39,06),X'F1010000C0',C'    '  OLDWRI   RESTORE         
         DC    X'03',AL1(39,10),X'F1010000C0',C'    '  OLDWRI   LIST            
         DC    X'03',AL1(39,12),X'F101000118',C'RORO'  OLDWRI   REPORT          
*                                                                               
         DC    X'03',AL1(40,01),X'B6010000C0',C'    '  RER      ADD             
         DC    X'03',AL1(40,02),X'B6010000C0',C'    '  RER      CHANGE          
         DC    X'03',AL1(40,03),X'B6010000C0',C'    '  RER      DISPLAY         
         DC    X'03',AL1(40,04),X'B6010000C0',C'    '  RER      DELETE          
         DC    X'03',AL1(40,05),X'B601000018',C'RWRW'  RER      SELECT          
         DC    X'03',AL1(40,06),X'B6010000C0',C'    '  RER      RESTORE         
         DC    X'03',AL1(40,10),X'B6010000C0',C'    '  RER      LIST            
         DC    X'03',AL1(40,12),X'B601000118',C'RWRW'  RER      REPORT          
*                                                                               
         DC    X'03',AL1(41,01),X'B5010000C0',C'    '  ORDVBILL ADD             
         DC    X'03',AL1(41,02),X'B5010000C0',C'    '  ORDVBILL CHANGE          
         DC    X'03',AL1(41,03),X'B5010000C0',C'    '  ORDVBILL DISPLAY         
         DC    X'03',AL1(41,04),X'B5010000C0',C'    '  ORDVBILL DELETE          
         DC    X'03',AL1(41,05),X'B501000018',C'RWRW'  ORDVBILL SELECT          
         DC    X'03',AL1(41,06),X'B5010000C0',C'    '  ORDVBILL RESTORE         
         DC    X'03',AL1(41,10),X'B5010000C0',C'    '  ORDVBILL LIST            
         DC    X'03',AL1(41,12),X'B501000118',C'RWRW'  ORDVBILL REPORT          
*                                                                               
         DC    X'03',AL1(42,01),X'E9010000C0',C'    '  KRTAPE   ADD             
         DC    X'03',AL1(42,02),X'E9010000C0',C'    '  KRTAPE   CHANGE          
         DC    X'03',AL1(42,03),X'E9010000C0',C'    '  KRTAPE   DISPLAY         
         DC    X'03',AL1(42,04),X'E9010000C0',C'    '  KRTAPE   DELETE          
         DC    X'03',AL1(42,05),X'E901000018',C'KRKR'  KRTAPE   SELECT          
         DC    X'03',AL1(42,06),X'E9010000C0',C'    '  KRTAPE   RESTORE         
         DC    X'03',AL1(42,10),X'E9010000C0',C'    '  KRTAPE   LIST            
         DC    X'03',AL1(42,12),X'E901000118',C'KRKR'  KRTAPE   REPORT          
*                                                                               
         DC    X'03',AL1(43,01),X'F5010000C0',C'    '  SCJE     ADD             
         DC    X'03',AL1(43,02),X'F5010000C0',C'    '  SCJE     CHANGE          
         DC    X'03',AL1(43,03),X'F5010000C0',C'    '  SCJE     DISPLAY         
         DC    X'03',AL1(43,04),X'F5010000C0',C'    '  SCJE     DELETE          
         DC    X'03',AL1(43,05),X'F501000018',C'RWRW'  SCJE     SELECT          
         DC    X'03',AL1(43,06),X'F5010000C0',C'    '  SCJE     RESTORE         
         DC    X'03',AL1(43,10),X'F5010000C0',C'    '  SCJE     LIST            
         DC    X'03',AL1(43,12),X'F501000118',C'RWRW'  SCJE     REPORT          
*                                                                               
         DC    X'FF'                                                            
         SPACE 3                                                                
*              PHASE USED UP BY SYSTEM SO FAR                                   
         SPACE 1                                                                
*              X0 X1 X2 X3 X4 X5 X6 X7 X8 X9 XA XB XC XD XE XF                  
*                                                                               
*        0X     Y  Y     Y                                                      
*        FX        Y                                         Y                  
         EJECT                                                                  
* TABLES OF RECORDS ACTIONS AND COMBINATIONS #2                                 
*                                                                               
* >>>>>> SAME AS #1 TABLES, BUT WITH SOON REPORTING ALLOWED <<<<<<              
*                                                                               
RECACTS2 DS    0D                                                               
         DC    X'04',C'WRITER  ',AL1(01),X'0000'    WRITER                      
         DC    X'04',C'PBE     ',AL1(02),X'0000'    POST BUY EXCEPTION          
         DC    X'04',C'SCH     ',AL1(03),X'0000'    SCHEDULE REPORT             
         DC    X'04',C'RRS     ',AL1(04),X'0000'    RADIO ROTATIONAL            
*        DC    X'04',C'CSRSUM  ',AL1(05),X'0000'    COKE STATUS SUMMARY         
*        DC    X'04',C'CSRDTL  ',AL1(06),X'0000'    COKE STATUS DETAIL          
         DC    X'04',C'ALAN    ',AL1(07),X'0000'    ALLOCATION ANALYSIS         
         DC    X'04',C'PG      ',AL1(08),X'0000'    P&G DATA COLLACTION         
*        DC    X'04',C'ATT     ',AL1(09),X'0000'    AT&T TAPES                  
         DC    X'04',C'GFTAPE  ',AL1(10),X'0000'    GF TAPE                     
         DC    X'04',C'SBS     ',AL1(11),X'0000'    SPOT BUYING SCHED           
         DC    X'04',C'PRE     ',AL1(12),X'0000'    WRITER 'PRE' REPORT         
         DC    X'04',C'W3      ',AL1(13),X'0000'    WRITER W3 REPORT            
         DC    X'04',C'SL      ',AL1(14),X'0000'    STATION LOCKIN              
*        DC    X'04',C'DO      ',AL1(15),X'0000'    DRAFT ORDER                 
*        DC    X'04',C'BRS     ',AL1(16),X'0000'    BRAND TIME SCHEDULE         
*        DC    X'04',C'PC      ',AL1(18),X'0000'    PC DATA INTERFACE           
         DC    X'04',C'TBA     ',AL1(19),X'0000'    BUY ACTIVITY REPORT         
         DC    X'04',C'CLOSEOUT',AL1(20),X'0000'    ESTIMATE CLOSEOUT           
*        DC    X'04',C'SUMMARY ',AL1(22),X'0000'    SUMMARY RECORDS             
*        DC    X'04',C'FTP     ',AL1(24),X'0000'    FILE TRANSFER PROGM         
         DC    X'04',C'CASHFLOW',AL1(25),X'0000'    CASH FLOW                   
         DC    X'04',C'TRANSMIT',AL1(26),X'0000'    DOWNLOAD TRANSMIT           
         DC    X'04',C'HDBILL  ',AL1(27),X'0000'    HOME DEPOT BILLING          
         DC    X'04',C'MSCJ    ',AL1(28),X'0000'    SC JOHNSON MED TAPE         
         DC    X'04',C'ISCJ    ',AL1(29),X'0000'    SC JOHNSON INV TAPE         
         DC    X'04',C'DC      ',AL1(30),X'0000'    MEDIA CALENDAR              
         DC    X'04',C'BGL     ',AL1(31),X'0000'    BUYING GUIDELINES           
         DC    X'04',C'IAS     ',AL1(32),X'0000'    INV APPROVAL STATUS         
         DC    X'04',C'BAS     ',AL1(33),X'0000'    BUY CHECKING                
         DC    X'04',C'CFAFTER ',AL1(34),X'0000'    CASHFLOW                    
*        DC    X'04',C'AGYSUM  ',AL1(35),X'0000'    AGENCY SUMMARY (PW)         
         DC    X'04',C'PWREC   ',AL1(36),X'0000'    PW RECORD LIST              
         DC    X'04',C'FLIST   ',AL1(37),X'0000'    FORMAT LIST                 
*        DC    X'04',C'THDBILL ',AL1(38),X'0000'    HOME DEPOT BILLING          
         DC    X'04',C'OLDWRI  ',AL1(39),X'0000'    OLDWRI                      
         DC    X'04',C'RER     ',AL1(40),X'0000'    RETAIL EXPEND. REPT         
         DC    X'04',C'ORDBIL  ',AL1(41),X'0000'    ORD VS BILL REPORT          
         DC    X'04',C'KRTAPE  ',AL1(42),X'0000'    KRAFT TAPE                  
         DC    X'04',C'SCJE    ',AL1(43),X'0000'    SC JOHNSON EST              
         DC    X'04',C'TEST    ',AL1(99),X'0000'    FOR DDS TESTING             
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,02,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,03,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,04,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,05,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,06,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
**NOP         DC    X'02',C'HELP    ',AL1(00,00,00)                             
*                                                                               
*                                 SC  SP  OK                                    
*                                   OV  RP                                      
         DC    X'03',AL1(01,01),X'F1010000C0',C'    '  WRITER   ADD             
         DC    X'03',AL1(01,02),X'F1010000C0',C'    '  WRITER   CHANGE          
         DC    X'03',AL1(01,03),X'F1010000C0',C'    '  WRITER   DISPLAY         
         DC    X'03',AL1(01,04),X'F1010000C0',C'    '  WRITER   DELETE          
         DC    X'03',AL1(01,05),X'F101000038',C'RWRW'  WRITER   SELECT          
         DC    X'03',AL1(01,06),X'F1010000C0',C'    '  WRITER   RESTORE         
         DC    X'03',AL1(01,10),X'F1010000C0',C'    '  WRITER   LIST            
         DC    X'03',AL1(01,12),X'F101000138',C'RWRW'  WRITER   REPORT          
*                                                                               
         DC    X'03',AL1(02,01),X'E1010000C0',C'    '  PBE      ADD             
         DC    X'03',AL1(02,02),X'E1010000C0',C'    '  PBE      CHANGE          
         DC    X'03',AL1(02,03),X'E1010000C0',C'    '  PBE      DISPLAY         
         DC    X'03',AL1(02,04),X'E1010000C0',C'    '  PBE      DELETE          
         DC    X'03',AL1(02,05),X'E101000038',C'PBRW'  PBE      SELECT          
         DC    X'03',AL1(02,06),X'E1010000C0',C'    '  PBE      RESTORE         
         DC    X'03',AL1(02,10),X'E1010000C0',C'    '  PBE      LIST            
         DC    X'03',AL1(02,12),X'E101000138',C'PBRW'  PBE      REPORT          
*                                                                               
         DC    X'03',AL1(03,01),X'E2010000C0',C'    '  SCH      ADD             
         DC    X'03',AL1(03,02),X'E2010000C0',C'    '  SCH      CHANGE          
         DC    X'03',AL1(03,03),X'E2010000C0',C'    '  SCH      DISPLAY         
         DC    X'03',AL1(03,04),X'E2010000C0',C'    '  SCH      DELETE          
         DC    X'03',AL1(03,05),X'E201000038',C'SCRW'  SCH      SELECT          
         DC    X'03',AL1(03,06),X'E2010000C0',C'    '  SCH      RESTORE         
         DC    X'03',AL1(03,10),X'E2010000C0',C'    '  SCH      LIST            
         DC    X'03',AL1(03,12),X'E201000138',C'SCRW'  SCH      REPORT          
*                                                                               
         DC    X'03',AL1(04,01),X'E3010000C0',C'    '  RRS      ADD             
         DC    X'03',AL1(04,02),X'E3010000C0',C'    '  RRS      CHANGE          
         DC    X'03',AL1(04,03),X'E3010000C0',C'    '  RRS      DISPLAY         
         DC    X'03',AL1(04,04),X'E3010000C0',C'    '  RRS      DELETE          
         DC    X'03',AL1(04,05),X'E301000038',C'RRRW'  RRS      SELECT          
         DC    X'03',AL1(04,06),X'E3010000C0',C'    '  RRS      RESTORE         
         DC    X'03',AL1(04,10),X'E3010000C0',C'    '  RRS      LIST            
         DC    X'03',AL1(04,12),X'E301000138',C'RRRW'  RRS      REPORT          
*                                                                               
*        DC    X'03',AL1(05,12),X'E401000138',C'CSRW'  CSRSUM   REPORT          
*        DC    X'03',AL1(06,12),X'E501000138',C'CDRW'  CSRDTL   REPORT          
*                                                                               
         DC    X'03',AL1(07,01),X'E6010000C0',C'    '  ALAN     ADD             
         DC    X'03',AL1(07,02),X'E6010000C0',C'    '  ALAN     CHANGE          
         DC    X'03',AL1(07,03),X'E6010000C0',C'    '  ALAN     DISPLAY         
         DC    X'03',AL1(07,04),X'E6010000C0',C'    '  ALAN     DELETE          
         DC    X'03',AL1(07,05),X'E601000038',C'ALRW'  ALAN     SELECT          
         DC    X'03',AL1(07,06),X'E6010000C0',C'    '  ALAN     RESTORE         
         DC    X'03',AL1(07,10),X'E6010000C0',C'    '  ALAN     LIST            
         DC    X'03',AL1(07,12),X'E601000138',C'ALRW'  ALAN     REPORT          
*                                                                               
         DC    X'03',AL1(08,01),X'E7010000C0',C'    '  PG       ADD             
         DC    X'03',AL1(08,02),X'E7010000C0',C'    '  PG       CHANGE          
         DC    X'03',AL1(08,03),X'E7010000C0',C'    '  PG       DISPLAY         
         DC    X'03',AL1(08,04),X'E7010000C0',C'    '  PG       DELETE          
         DC    X'03',AL1(08,05),X'E701000038',C'PGRW'  PG       SELECT          
         DC    X'03',AL1(08,06),X'E7010000C0',C'    '  PG       RESTORE         
         DC    X'03',AL1(08,10),X'E7010000C0',C'    '  PG       LIST            
         DC    X'03',AL1(08,12),X'E701000138',C'PGRW'  PG       REPORT          
*                                                                               
*        DC    X'03',AL1(09,12),X'E801000138',C'ATAT'  ATT      REPORT          
*                                                                               
         DC    X'03',AL1(10,01),X'E9010000C0',C'    '  GFTAPE   ADD             
         DC    X'03',AL1(10,02),X'E9010000C0',C'    '  GFTAPE   CHANGE          
         DC    X'03',AL1(10,03),X'E9010000C0',C'    '  GFTAPE   DISPLAY         
         DC    X'03',AL1(10,04),X'E9010000C0',C'    '  GFTAPE   DELETE          
         DC    X'03',AL1(10,05),X'E901000038',C'GFGF'  GFTAPE   SELECT          
         DC    X'03',AL1(10,06),X'E9010000C0',C'    '  GFTAPE   RESTORE         
         DC    X'03',AL1(10,10),X'E9010000C0',C'    '  GFTAPE   LIST            
         DC    X'03',AL1(10,12),X'E901000138',C'GFGF'  GFTAPE   REPORT          
*                                                                               
         DC    X'03',AL1(11,01),X'EA010000C0',C'    '  SBS      ADD             
         DC    X'03',AL1(11,02),X'EA010000C0',C'    '  SBS      CHANGE          
         DC    X'03',AL1(11,03),X'EA010000C0',C'    '  SBS      DISPLAY         
         DC    X'03',AL1(11,04),X'EA010000C0',C'    '  SBS      DELETE          
         DC    X'03',AL1(11,05),X'EA01000038',C'BSRW'  SBS      SELECT          
         DC    X'03',AL1(11,06),X'EA010000C0',C'    '  SBS      RESTORE         
         DC    X'03',AL1(11,10),X'EA010000C0',C'    '  SBS      LIST            
         DC    X'03',AL1(11,12),X'EA01000138',C'BSRW'  SBS      REPORT          
*                                                                               
         DC    X'03',AL1(12,01),X'F1010000C0',C'    '  PRE      ADD             
         DC    X'03',AL1(12,02),X'F1010000C0',C'    '  PRE      CHANGE          
         DC    X'03',AL1(12,03),X'F1010000C0',C'    '  PRE      DISPLAY         
         DC    X'03',AL1(12,04),X'F1010000C0',C'    '  PRE      DELETE          
         DC    X'03',AL1(12,05),X'F101000038',C'RWR1'  PRE      SELECT          
         DC    X'03',AL1(12,06),X'F1010000C0',C'    '  PRE      RESTORE         
         DC    X'03',AL1(12,10),X'F1010000C0',C'    '  PRE      LIST            
         DC    X'03',AL1(12,12),X'F101000138',C'RWR1'  PRE      REPORT          
*                                                                               
         DC    X'03',AL1(13,01),X'F1010000C0',C'    '  W3       ADD             
         DC    X'03',AL1(13,02),X'F1010000C0',C'    '  W3       CHANGE          
         DC    X'03',AL1(13,03),X'F1010000C0',C'    '  W3       DISPLAY         
         DC    X'03',AL1(13,04),X'F1010000C0',C'    '  W3       DELETE          
         DC    X'03',AL1(13,05),X'F101000038',C'W3W3'  W3       SELECT          
         DC    X'03',AL1(13,06),X'F1010000C0',C'    '  W3       RESTORE         
         DC    X'03',AL1(13,10),X'F1010000C0',C'    '  W3       LIST            
         DC    X'03',AL1(13,12),X'F101000118',C'W3W3'  W3       REPORT          
*                                                                               
         DC    X'03',AL1(14,01),X'EB010000C0',C'    '  SL       ADD             
         DC    X'03',AL1(14,02),X'EB010000C0',C'    '  SL       CHANGE          
         DC    X'03',AL1(14,03),X'EB010000C0',C'    '  SL       DISPLAY         
         DC    X'03',AL1(14,04),X'EB010000C0',C'    '  SL       DELETE          
         DC    X'03',AL1(14,05),X'EB01000038',C'SLLK'  SL       SELECT          
         DC    X'03',AL1(14,06),X'EB010000C0',C'    '  SL       RESTORE         
         DC    X'03',AL1(14,10),X'EB010000C0',C'    '  SL       LIST            
         DC    X'03',AL1(14,12),X'EB01000138',C'SLLK'  SL       REPORT          
*                                                                               
*        DC    X'03',AL1(15,12),X'D101000118',C'DORW'  DO       REPORT          
*                                                                               
***      DC    X'03',AL1(16,01),X'EC010000C0',C'    '  BRS      ADD             
***      DC    X'03',AL1(16,02),X'EC010000C0',C'    '  BRS      CHANGE          
***      DC    X'03',AL1(16,03),X'EC010000C0',C'    '  BRS      DISPLAY         
***      DC    X'03',AL1(16,04),X'EC010000C0',C'    '  BRS      DELETE          
***      DC    X'03',AL1(16,05),X'EC01000038',C'BRRW'  BRS      SELECT          
***      DC    X'03',AL1(16,06),X'EC010000C0',C'    '  BRS      RESTORE         
***      DC    X'03',AL1(16,10),X'EC010000C0',C'    '  BRS      LIST            
***      DC    X'03',AL1(16,12),X'EC01000138',C'BRRW'  BRS      REPORT          
*                                                                               
*        DC    X'03',AL1(18,12),X'ED01000138',C'PCRW'  PC       REPORT          
*                                                                               
         DC    X'03',AL1(19,01),X'EE010000C0',C'    '  TBA      ADD             
         DC    X'03',AL1(19,02),X'EE010000C0',C'    '  TBA      CHANGE          
         DC    X'03',AL1(19,03),X'EE010000C0',C'    '  TBA      DISPLAY         
         DC    X'03',AL1(19,04),X'EE010000C0',C'    '  TBA      DELETE          
         DC    X'03',AL1(19,05),X'EE01000038',C'BARW'  TBA      SELECT          
         DC    X'03',AL1(19,06),X'EE010000C0',C'    '  TBA      RESTORE         
         DC    X'03',AL1(19,10),X'EE010000C0',C'    '  TBA      LIST            
         DC    X'03',AL1(19,12),X'EE01000138',C'BARW'  TBA      REPORT          
*                                                                               
         DC    X'03',AL1(20,01),X'F2200000C0',C'    '  CLOSEOUT ADD             
         DC    X'03',AL1(20,02),X'F2200000C0',C'    '  CLOSEOUT CHANGE          
         DC    X'03',AL1(20,03),X'F2200000C0',C'    '  CLOSEOUT DISPLAY         
         DC    X'03',AL1(20,04),X'F2200000C0',C'    '  CLOSEOUT DELETE          
         DC    X'03',AL1(20,05),X'F220000038',C'COCO'  CLOSEOUT SELECT          
         DC    X'03',AL1(20,06),X'F2200000C0',C'    '  CLOSEOUT RESTORE         
         DC    X'03',AL1(20,10),X'F2200000C0',C'    '  CLOSEOUT LIST            
         DC    X'03',AL1(20,12),X'F220002018',C'COCO'  CLOSEOUT REPORT          
*                                                                               
*        DC    X'03',AL1(22,12),X'F322000000',C'    '  SUMMARY  RECORDS         
*                                                                               
*        DC    X'03',AL1(24,01),X'F4010000C0',C'    '  FTP      ADD             
*        DC    X'03',AL1(24,02),X'F4010000C0',C'    '  FTP      CHANGE          
*        DC    X'03',AL1(24,03),X'F4010000C0',C'    '  FTP      DISPLAY         
*        DC    X'03',AL1(24,04),X'F4010000C0',C'    '  FTP      DELETE          
*        DC    X'03',AL1(24,05),X'F401000038',C'FTFT'  FTP      SELECT          
*        DC    X'03',AL1(24,06),X'F4010000C0',C'    '  FTP      RESTORE         
*        DC    X'03',AL1(24,10),X'F4010000C0',C'    '  FTP      LIST            
*        DC    X'03',AL1(24,12),X'F424002438',C'FTFT'  FTP      REPORT          
*                                                                               
         DC    X'03',AL1(25,01),X'D3010000C0',C'    '  CASHFLOW ADD             
         DC    X'03',AL1(25,02),X'D3010000C0',C'    '  CASHFLOW CHANGE          
         DC    X'03',AL1(25,03),X'D3010000C0',C'    '  CASHFLOW DISPLAY         
         DC    X'03',AL1(25,04),X'D3010000C0',C'    '  CASHFLOW DELETE          
         DC    X'03',AL1(25,05),X'D301000038',C'CARW'  CASHFLOW SELECT          
         DC    X'03',AL1(25,06),X'D3010000C0',C'    '  CASHFLOW RESTORE         
         DC    X'03',AL1(25,10),X'D3010000C0',C'    '  CASHFLOW LIST            
         DC    X'03',AL1(25,12),X'D301000138',C'CARW'  CASHFLOW REPORT          
*                                                                               
         DC    X'03',AL1(26,01),X'F1010000C0',C'    '  TRANSMIT ADD             
         DC    X'03',AL1(26,02),X'F1010000C0',C'    '  TRANSMIT CHANGE          
         DC    X'03',AL1(26,03),X'F1010000C0',C'    '  TRANSMIT DISPLAY         
         DC    X'03',AL1(26,04),X'F1010000C0',C'    '  TRANSMIT DELETE          
         DC    X'03',AL1(26,05),X'F101000038',C'RWRT'  TRANSMIT SELECT          
         DC    X'03',AL1(26,06),X'F1010000C0',C'    '  TRANSMIT RESTORE         
         DC    X'03',AL1(26,10),X'F1010000C0',C'    '  TRANSMIT LIST            
         DC    X'03',AL1(26,12),X'F101000138',C'RWRT'  TRANSMIT REPORT          
*                                                                               
         DC    X'03',AL1(27,01),X'D9010000C0',C'    '  HDBILL   ADD             
         DC    X'03',AL1(27,02),X'D9010000C0',C'    '  HDBILL   CHANGE          
         DC    X'03',AL1(27,03),X'D9010000C0',C'    '  HDBILL   DISPLAY         
         DC    X'03',AL1(27,04),X'D9010000C0',C'    '  HDBILL   DELETE          
         DC    X'03',AL1(27,05),X'D901000038',C'HDRW'  HDBILL   SELECT          
         DC    X'03',AL1(27,06),X'D9010000C0',C'    '  HDBILL   RESTORE         
         DC    X'03',AL1(27,10),X'D9010000C0',C'    '  HDBILL   LIST            
         DC    X'03',AL1(27,12),X'D901000138',C'HDRW'  HDBILL   REPORT          
*                                                                               
         DC    X'03',AL1(28,01),X'D8010000C0',C'    '  MSCJ     ADD             
         DC    X'03',AL1(28,02),X'D8010000C0',C'    '  MSCJ     CHANGE          
         DC    X'03',AL1(28,03),X'D8010000C0',C'    '  MSCJ     DISPLAY         
         DC    X'03',AL1(28,04),X'D8010000C0',C'    '  MSCJ     DELETE          
         DC    X'03',AL1(28,05),X'D801000038',C'SJSJ'  MSCJ     SELECT          
         DC    X'03',AL1(28,06),X'D8010000C0',C'    '  MSCJ     RESTORE         
         DC    X'03',AL1(28,10),X'D8010000C0',C'    '  MSCJ     LIST            
         DC    X'03',AL1(28,12),X'D801000118',C'SJSJ'  MSCJ     REPORT          
*                                                                               
         DC    X'03',AL1(29,01),X'DA010000C0',C'    '  ISCJ     ADD             
         DC    X'03',AL1(29,02),X'DA010000C0',C'    '  ISCJ     CHANGE          
         DC    X'03',AL1(29,03),X'DA010000C0',C'    '  ISCJ     DISPLAY         
         DC    X'03',AL1(29,04),X'DA010000C0',C'    '  ISCJ     DELETE          
         DC    X'03',AL1(29,05),X'DA01000038',C'SISI'  ISCJ     SELECT          
         DC    X'03',AL1(29,06),X'DA010000C0',C'    '  ISCJ     RESTORE         
         DC    X'03',AL1(29,10),X'DA010000C0',C'    '  ISCJ     LIST            
         DC    X'03',AL1(29,12),X'DA01000118',C'SISI'  ISCJ     REPORT          
*                                                                               
         DC    X'03',AL1(30,01),X'D6010000C0',C'    '  DC       ADD             
         DC    X'03',AL1(30,02),X'D6010000C0',C'    '  DC       CHANGE          
         DC    X'03',AL1(30,03),X'D6010000C0',C'    '  DC       DISPLAY         
         DC    X'03',AL1(30,04),X'D6010000C0',C'    '  DC       DELETE          
         DC    X'03',AL1(30,05),X'D601000038',C'DCRC'  DC       SELECT          
         DC    X'03',AL1(30,06),X'D6010000C0',C'    '  DC       RESTORE         
         DC    X'03',AL1(30,10),X'D6010000C0',C'    '  DC       LIST            
         DC    X'03',AL1(30,12),X'D601000138',C'DCRC'  DC       REPORT          
*                                                                               
         DC    X'03',AL1(31,01),X'D4010000C0',C'    '  BGL      ADD             
         DC    X'03',AL1(31,02),X'D4010000C0',C'    '  BGL      CHANGE          
         DC    X'03',AL1(31,03),X'D4010000C0',C'    '  BGL      DISPLAY         
         DC    X'03',AL1(31,04),X'D4010000C0',C'    '  BGL      DELETE          
         DC    X'03',AL1(31,05),X'D401000038',C'BGBG'  BGL      SELECT          
         DC    X'03',AL1(31,06),X'D4010000C0',C'    '  BGL      RESTORE         
         DC    X'03',AL1(31,10),X'D4010000C0',C'    '  BGL      LIST            
         DC    X'03',AL1(31,12),X'D401000138',C'BGBG'  BGL      REPORT          
*                                                                               
         DC    X'03',AL1(32,01),X'D2010000C0',C'    '  IAS      ADD             
         DC    X'03',AL1(32,02),X'D2010000C0',C'    '  IAS      CHANGE          
         DC    X'03',AL1(32,03),X'D2010000C0',C'    '  IAS      DISPLAY         
         DC    X'03',AL1(32,04),X'D2010000C0',C'    '  IAS      DELETE          
         DC    X'03',AL1(32,05),X'D201000038',C'IARW'  IAS      SELECT          
         DC    X'03',AL1(32,06),X'D2010000C0',C'    '  IAS      RESTORE         
         DC    X'03',AL1(32,10),X'D2010000C0',C'    '  IAS      LIST            
         DC    X'03',AL1(32,12),X'D201000138',C'IARW'  IAS      REPORT          
*                                                                               
         DC    X'03',AL1(33,01),X'B3010000C0',C'    '  BAS      ADD             
         DC    X'03',AL1(33,02),X'B3010000C0',C'    '  BAS      CHANGE          
         DC    X'03',AL1(33,03),X'B3010000C0',C'    '  BAS      DISPLAY         
         DC    X'03',AL1(33,04),X'B3010000C0',C'    '  BAS      DELETE          
         DC    X'03',AL1(33,05),X'B301000038',C'BCRW'  BAS      SELECT          
         DC    X'03',AL1(33,06),X'B3010000C0',C'    '  BAS      RESTORE         
         DC    X'03',AL1(33,10),X'B3010000C0',C'    '  BAS      LIST            
         DC    X'03',AL1(33,12),X'B301000138',C'BCRW'  BAS      REPORT          
*                                                                               
         DC    X'03',AL1(34,01),X'D3010000C0',C'    '  CASHFLOW ADD             
         DC    X'03',AL1(34,02),X'D3010000C0',C'    '  CASHFLOW CHANGE          
         DC    X'03',AL1(34,03),X'D3010000C0',C'    '  CASHFLOW DISPLAY         
         DC    X'03',AL1(34,04),X'D3010000C0',C'    '  CASHFLOW DELETE          
         DC    X'03',AL1(34,05),X'D301000038',C'CACF'  CASHFLOW SELECT          
         DC    X'03',AL1(34,06),X'D3010000C0',C'    '  CASHFLOW RESTORE         
         DC    X'03',AL1(34,10),X'D3010000C0',C'    '  CASHFLOW LIST            
         DC    X'03',AL1(34,12),X'D301000138',C'CACF'  CASHFLOW REPORT          
*                                                                               
*        DC    X'03',AL1(35,12),X'B501000138',C'PWRW'  AGYSUM   REPORT          
*                                                                               
         DC    X'03',AL1(36,01),X'B5010000C0',C'    '  PW REC   ADD             
         DC    X'03',AL1(36,02),X'B5010000C0',C'    '  PW REC   CHANGE          
         DC    X'03',AL1(36,03),X'B5010000C0',C'    '  PW REC   DISPLAY         
         DC    X'03',AL1(36,04),X'B5010000C0',C'    '  PW REC   DELETE          
         DC    X'03',AL1(36,05),X'B501000038',C'PWRW'  PW REC   SELECT          
         DC    X'03',AL1(36,06),X'B5010000C0',C'    '  PW REC   RESTORE         
         DC    X'03',AL1(36,10),X'B5010000C0',C'    '  PW REC   LIST            
         DC    X'03',AL1(36,12),X'B501000138',C'PWRW'  PW REC   REPORT          
*                                                                               
         DC    X'03',AL1(37,01),X'B5010000C0',C'    '  FORMAT   ADD             
         DC    X'03',AL1(37,02),X'B5010000C0',C'    '  FORMAT   CHANGE          
         DC    X'03',AL1(37,03),X'B5010000C0',C'    '  FORMAT   DISPLAY         
         DC    X'03',AL1(37,04),X'B5010000C0',C'    '  FORMAT   DELETE          
         DC    X'03',AL1(37,05),X'B501000038',C'FMRW'  FORMAT   SELECT          
         DC    X'03',AL1(37,06),X'B5010000C0',C'    '  FORMAT   RESTORE         
         DC    X'03',AL1(37,10),X'B5010000C0',C'    '  FORMAT   LIST            
         DC    X'03',AL1(37,12),X'B501000138',C'FMRW'  FORMAT   REPORT          
*                                                                               
*        DC    X'03',AL1(38,12),X'D901000138',C'HTHT'  THDBILL  REPORT          
*                                                                               
         DC    X'03',AL1(39,01),X'F1010000C0',C'    '  OLDWRI   ADD             
         DC    X'03',AL1(39,02),X'F1010000C0',C'    '  OLDWRI   CHANGE          
         DC    X'03',AL1(39,03),X'F1010000C0',C'    '  OLDWRI   DISPLAY         
         DC    X'03',AL1(39,04),X'F1010000C0',C'    '  OLDWRI   DELETE          
         DC    X'03',AL1(39,05),X'F101000038',C'RORO'  OLDWRI   SELECT          
         DC    X'03',AL1(39,06),X'F1010000C0',C'    '  OLDWRI   RESTORE         
         DC    X'03',AL1(39,10),X'F1010000C0',C'    '  OLDWRI   LIST            
         DC    X'03',AL1(39,12),X'F101000118',C'RORO'  OLDWRI   REPORT          
*                                                                               
         DC    X'03',AL1(40,01),X'B6010000C0',C'    '  RER      ADD             
         DC    X'03',AL1(40,02),X'B6010000C0',C'    '  RER      CHANGE          
         DC    X'03',AL1(40,03),X'B6010000C0',C'    '  RER      DISPLAY         
         DC    X'03',AL1(40,04),X'B6010000C0',C'    '  RER      DELETE          
         DC    X'03',AL1(40,05),X'B601000038',C'RWRW'  RER      SELECT          
         DC    X'03',AL1(40,06),X'B6010000C0',C'    '  RER      RESTORE         
         DC    X'03',AL1(40,10),X'B6010000C0',C'    '  RER      LIST            
         DC    X'03',AL1(40,12),X'B601000138',C'RWRW'  RER      REPORT          
*                                                                               
         DC    X'03',AL1(41,01),X'B5010000C0',C'    '  ORDVBILL ADD             
         DC    X'03',AL1(41,02),X'B5010000C0',C'    '  ORDVBILL CHANGE          
         DC    X'03',AL1(41,03),X'B5010000C0',C'    '  ORDVBILL DISPLAY         
         DC    X'03',AL1(41,04),X'B5010000C0',C'    '  ORDVBILL DELETE          
         DC    X'03',AL1(41,05),X'B501000038',C'RWRW'  ORDVBILL SELECT          
         DC    X'03',AL1(41,06),X'B5010000C0',C'    '  ORDVBILL RESTORE         
         DC    X'03',AL1(41,10),X'B5010000C0',C'    '  ORDVBILL LIST            
         DC    X'03',AL1(41,12),X'B501000138',C'RWRW'  ORDVBILL REPORT          
*                                                                               
         DC    X'03',AL1(42,01),X'E9010000C0',C'    '  KRTAPE   ADD             
         DC    X'03',AL1(42,02),X'E9010000C0',C'    '  KRTAPE   CHANGE          
         DC    X'03',AL1(42,03),X'E9010000C0',C'    '  KRTAPE   DISPLAY         
         DC    X'03',AL1(42,04),X'E9010000C0',C'    '  KRTAPE   DELETE          
         DC    X'03',AL1(42,05),X'E901000038',C'KRKR'  KRTAPE   SELECT          
         DC    X'03',AL1(42,06),X'E9010000C0',C'    '  KRTAPE   RESTORE         
         DC    X'03',AL1(42,10),X'E9010000C0',C'    '  KRTAPE   LIST            
         DC    X'03',AL1(42,12),X'E901000138',C'KRKR'  KRTAPE   REPORT          
*                                                                               
         DC    X'03',AL1(43,01),X'F5010000C0',C'    '  SCJE     ADD             
         DC    X'03',AL1(43,02),X'F5010000C0',C'    '  SCJE     CHANGE          
         DC    X'03',AL1(43,03),X'F5010000C0',C'    '  SCJE     DISPLAY         
         DC    X'03',AL1(43,04),X'F5010000C0',C'    '  SCJE     DELETE          
         DC    X'03',AL1(43,05),X'F501000038',C'RWRW'  SCJE     SELECT          
         DC    X'03',AL1(43,06),X'F5010000C0',C'    '  SCJE     RESTORE         
         DC    X'03',AL1(43,10),X'F5010000C0',C'    '  SCJE     LIST            
         DC    X'03',AL1(43,12),X'F501000138',C'RWRW'  SCJE     REPORT          
*                                                                               
         DC    X'03',AL1(99,01),X'D3010000C0',C'    '  TEST     ADD             
         DC    X'03',AL1(99,02),X'D3010000C0',C'    '  TEST     CHANGE          
         DC    X'03',AL1(99,03),X'D3010000C0',C'    '  TEST     DISPLAY         
         DC    X'03',AL1(99,04),X'D3010000C0',C'    '  TEST     DELETE          
         DC    X'03',AL1(99,05),X'D301000038',C'W9W9'  TEST     SELECT          
         DC    X'03',AL1(99,06),X'D3010000C0',C'    '  TEST     RESTORE         
         DC    X'03',AL1(99,10),X'D3010000C0',C'    '  TEST     LIST            
         DC    X'03',AL1(99,12),X'D301000138',C'W9W9'  TEST     REPORT          
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE SPWRIWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*        INCLUDE DDSPOOLD                                                       
*        INCLUDE DDSPLWORKD                                                     
*        INCLUDE SPWRIFFD                                                       
*        INCLUDE SPWRIF1D                                                       
*        INCLUDE DDGENTWA                                                       
*        INCLUDE SPGENAGY                                                       
*        INCLUDE SPGENCLT                                                       
*        INCLUDE SPGENPRD                                                       
*        INCLUDE SPGENPRG                                                       
*        INCLUDE SPGENEST                                                       
*        INCLUDE SPGENMKG                                                       
*        INCLUDE SPGENMKT                                                       
*        INCLUDE SPGENSTA                                                       
*        INCLUDE CTGENFILE                                                      
*        INCLUDE FAFACTS                                                        
*        INCLUDE FATIOB                                                         
*        INCLUDE DDCOMFACS                                                      
*        INCLUDE DRGLOBAL                                                       
*        INCLUDE DDBIGBOX                                                       
*        INCLUDE DDWIDED                                                        
*        INCLUDE DDOFFICED                                                      
*        INCLUDE DRONEBLKHD                                                     
*        INCLUDE DDMASTD                                                        
*        INCLUDE DDTSARD                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPWRIFFD                                                       
         ORG     CONTAGH                                                        
       ++INCLUDE SPWRIF1D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPWRISAVED                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENPRG                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENMKG                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FATCB                                                          
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DRONEBLKHD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055SPWRI00   10/30/14'                                      
         END                                                                    
