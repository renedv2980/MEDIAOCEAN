*          DATA SET ACREPEM02  AT LEVEL 068 AS OF 11/09/20                      
*PHASE ACEM02A                                                                  
                                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE QSORT                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE COVAIL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE GETCAP                                                                 
*INCLUDE EMLCHK                                                                 
*INCLUDE URIENC                                                                 
                                                                                
         TITLE 'WEEKLY APPROVAL NOTICES BY EMAIL'                               
                                                                                
* SMAN 001 04AUG06 BUG FIX: CHECK IF SORTER IS EMPTY                            
* SMAN 002 07AUG06 PRINT PID AND EMAIL ADDRESS OF APPROVER IN PQ                
* SMAN 003 09AUG06 BUG FIX: IGNORE DUPLICATE EMAIL LINES                        
* SMAN 004 11AUG06 IMPROVEMENT TO FILTERING DUPLICATE EMAIL LINES               
* SMAN 005 10NOV06 BUG FIX: ANOTHER CHECK IF SORTER IS EMPTY                    
* SMAN 006 11JAN07 <BR10666L> BUG FIX ON 1R ACCOUNT CODE                        
*                  <UKCR00010794> MAKE URL FILE DEPENDENT                       
* SMAN 007 15JAN07 OVERNIGHT REPORT: SMALL BUG FIX                              
* SMAN 008 16JAN07 OVERNIGHT REPORT: ERROR MSG WHEN SA0STAT IS LOCKED           
* SMAN 009 09FEB07 OVERNIGHT REPORT: ENHANCEMENTS TO 200+ LINES PER             
*                  EMAIL, DISCLAIMER AND PARM CARD TO SKIP SMTP CALLS           
*                  FOR EASIER DEBUGGING                                         
* SMAN 010 20FEB07 OVERNIGHT REPORT: FURTHER CHANGES RE LVL 009                 
* NSHE 001 31AUG07 REWRITTEN TO COPE WITH GREATER NUMBER OF OUTSTANDING         
*                  TIMESHEETS AND PRODUCE A MORE INFORMATIVE REPORT             
* NSHE 002 03SEP07 FIX WHEN EMAIL TABLE IS EMPTY                                
* NSHE 003 12SEP07 CHANGE TO APPROVER RECORD STRUCTURE                          
* NSHE 004 31JAN08 MAKE LANGUAGE SOFT                                           
* NSHE 005 07AUG08 CHANGE TO JOBPASD STRUCTURE                                  
* MPEN 006 19AUG08 EMAIL EXPENSE/ORDERS/JOB/ESTIMATE CLAIM APPROVERS            
*                  READ IN EMAIL PROFILE, PRINT DETAILED REPORT                 
*                  INITIAL INVOICES WORK                                        
* NSHE 007 19JAN09 ESTIMATES BUG FIX                                            
* TKLU 008 30JAN09 <BR22890L> BUG FIX IN GETAPP FOR EXPENSES                    
* MPEN 009 09FEB09 <BR23039L> LVL007 PROPERLY FIX ESTIMATE NAME DISPLAY         
* JFOS 010 26FEB09 <BR23478L> EXCLUDE LOGICALLY-DELETED ORDERS                  
* JFOS 011 27FEB09 <BR23519L> INCREASE EMAILMAX AND DEAL WITH LARGE NOS         
*                             OF EMAILS                                         
* MPEN     09MAR09 CHANGES TO APPPASD AND TAPPASD FOR NEW BRANDOCEAN            
* MPEN 012 18MAR09 <OT53128L> MINI BUG FIX IN PROETAB RUNITMNY NOT REST         
* TFRY 013 07APR09 <BR24288L> READ EXCRSTA INSTEAD OF EXCKSTAT                  
* NSHE 014 11MAY09 CHANGE TO APPROVER RECORD AND EXPENSE RECORD                 
* MPEN 015 21JAN09 <LO01-8529> PRODUCE SUMMARY EMAILS                           
* MPEN 015 20MAR09 SUPPORT INVOICE MODULE                                       
* MPEN 016 09JUN09 <OT54772L> FIX FOR DDS ONLY USERS                            
* MPEN 017 28JUL09 <LO01-9024> READ NEW EXPENSE PASSIVE, READ CLIENT            
*                  JOB APPROVER AND BOX THE REPORTS                             
* JFOS 019 09SEP09 <OT56212L> USE IOAREA2 FOR GSECALP/GETLANG                   
* MPEN 018 22SEP09 <LO01-9024>MINOR FORMATTING BUG FIX FOR GERMANY              
* MPEN 019 04NOV09 <UKCR00025575> FIX NASTY LOOP BUG                            
* SMAN 020 09NOV09 <UKCR00025670> SMALL FIX TO ESTIMATE NOTIFICATIONS           
* MPEN 021 17NOV09 <BR28864L>,<BR28862L> MINI BUG FIXES                         
* MPEN 022 11JAN09 <DU01-9598> NEW DOWN REPORT WITH ALPHA ID AND OFF            
*                              MERGE US CHANGES                                 
* MPEN 023 26APR10 <PR000221> REMOVE PID AND AGENCY INFO IF PROF SET            
* MPEN 024 15JUN10 <BR16129D> CHECK WHETHER CLIENT APPROVAL NEEDED              
* YNGX 025 16JUL10 <BR34639L> REFRESH 1R A/C NAMES ON DOWNLOAD REPORT           
* SMAN 026 20JUL10 <OT61909L> BUG FIX TO LVL 25                                 
* SMAN 027 26JUL10 <BR16346D> FIX TYPO                                          
* NSHE 029 20JAN11 <BR17170D> CHANGE INVOICES TO DEAL WITH NEW REC              
* JFOS 030 27JAN11 <BR17294D> INVOICES EMAILS POSTED/PART-APPRVD ONLY           
* JFOS 031 11FEB11 <BR17397D> FIX SUP NAME TRUNCATED, ordnum in d/load          
* MPEN 032 11MAY10 <PR000244> READ GETOPT TO DETECT IF SEQ APP IN USE           
* MPEN 033 27MAY11 <OT66686L> ENSURE DON'T DIE IF NO 1R/SJ                      
* MPEN 034 25AUG11            CHANGES FOR BUILD 32                              
* MPEN 035 24FEB12 <BR19221D> READ ORDER PO1 PROFILE TO CHECK IF SEQ AP         
* TKLU 036 19MAR12 <PR001499> ORDER AND INVOICE ADDITIONS TO REPORT,            
*                             ALSO NEW TSO CONTROL SETTINGS (PARMD),            
*                             BUG FIXES (LAYOUT, INV: SORTNMBR USE)             
* TKLU 037 20MAR12 <BR19381D> ESTIAMTE COLUMNS SHIFT FIX                        
* MPEN 038 26MAR12 <PR002113> RELINK FOR LARGER COBLOCK                         
* YNGX 039 06NOV12 <OT74166L> INCREASE EMAILMAX FROM 40000 TO 60000             
* MPEN 040 03OCT12 <LO01-9742> COPY US CHANGES                                  
* NRAK 041 29SEP12 <BR51947L> BETTER DEBUGGING CODE AND EMLCHK                  
* MPEN 051 15APR14 <DSRD-2523> PRODUCE HTML EMAILS                              
*                  <DSRD-2767> UPDATE AURA HTML EMAILS FOR JOBS                 
*                  <DSRD-4854> UPDATE AURA EMAILS FOR ESTIMATES                 
*                  <DSRD-5859> UPDATE AURA EMAILS FOR ORDERS                    
*                  <DSRD-5373> GERMAN TRANSLATIONS                              
*                  <DSRD-5346> GERMAN TRANSLATION FOR FIND US AT                
*                  <DSRD-5464> ADD GERMAN ADDRESS                               
*                  <PCA001793> EXTRACT ESTIMATE AMOUNT                          
*                  <PCA001738> CONTROL WHICH MODULES EMAILS SENT FOR            
*                  <DSRD-7402> MAKE WEB LINK COUNTRY SPECIFIC                   
*                  <ITMF-3321> SKIP ALPHA IDS THAT DON'T EXIST                  
*                  <RD-11140>  CHANGES FOR DYNAMIC URLS                         
* MPEN     14OCT16 <RD-13630>  UPDATE IMAGE SOURCE URLS                         
* MPEN 052 08DEC16 <RD-14351>  CHANGES FOR EXPENSE MODULE                       
* MPEN 053 03JAN17 <ITIDS-12183> FIX FOR FEDERATED EMAILS                       
* MPEN 054 10JAN17 <DSRD-14632> FIX FOR CANADIAN ADDRESS                        
* MPEN     26JUL17 <DSRD-16461> FIX FOR ESTIMATE NUMBER                         
* MPEN 055 17AUG17 <ITMF-18728> ENSURE NUMRECS DECREMENTED PROPERLY             
* MPEN 056 10AUG17 <DSRD-15131> IMPROVEMENTS TO EMAIL FORMATTING                
* MPEN 057 29AUG17 <ITMF-18938> INCREASE EMAILBUF                               
*nrak 058 05sep17 <ITMF-18981> preserve index in email table                    
* MPEN 060 31Oct17 <DSRD-17323> Skip terminated pids                            
* MPEN     06Nov17 <DSPCA-2796> Disable BO emails                               
* MPEN 061 16Nov17 <DSRD-17478> Copy US fixes                                   
* MPEN 062 02Jul18 <DSRD-19564> Remove BCC from emails                          
* MPEN 063 29Oct18 <DSRD-15233> URI encode urls                                 
* MPEN 064 23Oct18 <DSRD-20447> Relink for DPAPASD changes                      
* MPEN 065 21Jan19 <DSRD-21358> Update the German office address                
* TKLU 066 22Feb19 <ITMF-33926> PUTETAB initialisation fix (1CO/2CO)            
* YNGX 067 13Dec19 <ITMF-42018> DETACH FROM JESMAIL IF BUFFET FULL              
*                               INCREASE EMAILMAX FROM 70000 TO 90000           
* SGAV 068 02JUL20 <DSPCA-3045> Cntrol frequency of email notifications         
* NSHE 069 05Aug20 <DSRD-27173> Clean up URL logic                              
* JSHA 068 30Oct20 <DSRD-27879> Fix for test files in URL code                  
*                                                                               
***********************************************************************         
* **NOTE** THIS BOOK USES 31 BIT ADDRESSING FOR THE EMAIL TABLE       *         
***********************************************************************         
ACEM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*ACEM02*,R7                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          GLOBAL STORAGE                               
         LA    RC,SPACEND                                                       
         USING WORKD,RC            LOCAL STORAGE(SAVED STORAGE)                 
         LARL  R8,GLOBALS                                                       
         USING GLOBALS,R8          R8=A(GLOBAL LITERALS)                        
         LR    R9,R8                                                            
         AHI   R9,4096                                                          
         USING GLOBALS+4096,R9     R9=A(GLOBAL LITERALS)+4096                   
         USING SORTRECD,SORTREC                                                 
         LH    RF,=Y(IOAREA-WORKD)                                              
         LA    R2,WORKD                                                         
         AR    R2,RF                                                            
         ST    R2,AIOAREA                                                       
         LH    RF,=Y(EMPROF-WORKD)                                              
         LA    R2,WORKD                                                         
         AR    R2,RF                                                            
         ST    R2,AEMPROF                                                       
         LH    RF,=Y(EMPROF2-WORKD)                                             
         LA    R2,WORKD                                                         
         AR    R2,RF                                                            
         ST    R2,AEMPROF2                                                      
         LH    RF,=Y(ORDPROF-WORKD)                                             
         LA    R2,WORKD                                                         
         AR    R2,RF                                                            
         ST    R2,AORDPROF                                                      
         LH    RF,=Y(IOAREA2-WORKD)                                             
         LA    R2,WORKD                                                         
         AR    R2,RF                                                            
         ST    R2,AIOAREA2                                                      
         LH    RF,=Y(JOBAPP-WORKD)                                              
         LA    R2,WORKD                                                         
         AR    R2,RF                                                            
         ST    R2,AJOBAPP                                                       
         LAY   RF,HOOK                                                          
         ST    RF,HEADHOOK         SET A(HEADLINE HOOK)                         
*                                                                               
         USING EMPROFD,EMPROF                                                   
         USING EMPRO2D,EMPROF2                                                  
*                                                                               
         MVC   AFILES(FILESX),FILES SET UP FILE NAMES                           
         GOTO1 DATCON,DMCB,(5,0),(13,TODAY)                                     
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY6)                                     
         GOTO1 DATCON,DMCB,(4,RCDATE),(20,TODAYL)                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAYP)                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(3,TODAYB)                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAYC)                                
         USING PARMD,RCFFPARM      PARM EUROPE                                  
         CLI   MODE,REQFRST        NOTE REQFRST! ALLOWS PATCHING.               
         BE    INIT                                                             
         CLI   MODE,RUNLAST        RUNLAST PROCESSING?                          
         BNE   EXIT                                                             
         DS    0H                  THIS IS A USEFUL PLACE TO PATCH TO           
         B     EXIT                CHECK TOTNUM ETC AND CHECK TOTAL             
*                                                                               
EXITH    LHI   RE,2                SET CC HIGH                                  
         J     EXITCC                                                           
*                                                                               
EXITL    XR    RE,RE               SET CC LOW                                   
         J     EXITCC                                                           
*                                                                               
EXITE    LHI   RE,1                SET CC EQUAL                                 
*                                                                               
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
*                                                                               
EXITH23  LHI   RE,2                SET CC HIGH                                  
         J     EXITCC2                                                          
*                                                                               
EXITL23  XR    RE,RE               SET CC LOW                                   
         J     EXITCC2                                                          
*                                                                               
EXITE23  LHI   RE,1                SET CC EQUAL                                 
         J     EXITCC2                                                          
*                                                                               
EXITCC2  CHI   RE,1                                                             
EXITR23  XIT1  REGS=(R2,R3)        EXIT WITH REGISTER R2 KEPT                   
EXITR2   XIT1  REGS=(R2)                                                        
EXITR5   XIT1  REGS=(R5)                                                        
         EJECT                                                                  
***********************************************************************         
* INITIALISING                                                        *         
***********************************************************************         
         SPACE 1                                                                
INIT     MVI   LLANG,X'FF'                                                      
         L     R5,ADMASTC                                                       
         USING MASTD,R5                                                         
         MVC   CTRY,MCCTRY                                                      
         MVC   RCRUN,MCTSTRUN      Save test run indicators                     
         L     RF,MCSSB                                                         
         USING SSOOFFD,RF                                                       
         MVC   RCDSPAC,SSODSPAC                                                 
         L     RF,MCBXAREA                                                      
         ST    RF,ADBOX                                                         
         XR    R4,R4                                                            
         LA    R5,EMRECL           GETMAIN AREA FOR SORTING ELEMENT             
         L     RE,=A(EMAILMAX)     GROUPS WITHIN A PERSON CODE                  
         MR    R4,RE                                                            
         ST    R5,EMLTBLN                                                       
         L     R0,EMLTBLN                                                       
         GETMAIN RU,LV=(0),LOC=(31,31)                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,AEMLTAB          START OF AREA                                
*                                                                               
         L     R5,ADMASTC                                                       
         MVC   DUB,=CL8'GETURL'    ACGETURL                                     
         MVC   DUB+6(1),MCTEST3    LOAD EITHER LIVE OR TEST                     
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   INIT06                                                           
         MVC   DUB,=CL8'GETURL'                                                 
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
INIT06   MVC   VGETURL,4(R1)       A(ACGETURL)                                  
         MVC   MCAPHAS3,4(R1)      SET UP PATCHABILITY                          
*                                                                               
         ICM   R0,15,0(R1)         LENGTH OF PHASE                              
         L     R1,FULL                                                          
         AR    R0,R1               ADD LENGTH OF PREVIOUS PHASE                 
         ST    R0,FULL                                                          
         ICM   RF,15,WORD          A(GETFORM)                                   
         STCM  RF,15,MCUSRDMP                                                   
         AR    RF,R0                                                            
         LA    RF,4095(RF)                                                      
         STCM  RF,15,MCUSRDMP+4                                                 
         DROP  R5                                                               
*                                                                               
         GOTO1 ADDICTAT,DMCB,C'LL  ',DATI,DATO RESOLVE DATA DICT ITEMS          
*                                                                               
         L     RF,AELE800E         Put year in copyright                        
         MVC   0(L'ELEM800E,RF),TODAYL                                          
*                                                                               
         L     RF,AEMS620E         Put year in copyright                        
         MVC   0(L'EMLS620E,RF),TODAYL                                          
*                                                                               
         MVI   IOCOMP,0                                                         
         CLC   PARMDAG,SPACES      UNLESS SPECIFIC REQUEST                      
         BNH   INIT08                                                           
         GOTO1 VHEXIN,DMCB,PARMDAG,IOCOMP,2                                     
*                                                                               
INIT08   DS    0H                  TESTING FILTERS                              
         LAY   R5,TARPID                 INIT                                   
         MVC   0(L'TARPID,R5),SPACES                                            
         LAY   R5,TARPIN                                                        
         XC    0(L'TARPIN,R5),0(R5)                                             
         LAY   R5,TESTMAIL                                                      
         MVC   0(L'TESTMAIL,R5),SPACES                                          
*                                                                               
         CLI   QCNTINUE,QCNTQ                                                   
         JNE   INIT30                NONE SUPPLIED                              
         LAY   R5,TESTMAIL                                                      
         CLC   QCACCT,SPACES                                                    
         JNH   INIT15                                                           
         MVC   0(L'TESTMAIL,R5),QCACCT                                          
         OC    0(L'TESTMAIL,R5),SPACES                                          
         LA    RF,L'TESTMAIL-17      L'SUFFIX+1 FOR FIRST INCREMENT             
INIT10   DS    0H                                                               
         LA    R5,1(R5)                                                         
         CLI   0(R5),C' '                                                       
         JNH   *+8                                                              
         JCT   RF,INIT10                                                        
         MVC   0(16,R5),=C'@MEDIAOCEAN.COM:'                                    
*                                                                               
INIT15   LAY   R5,TARPID                                                        
         MVC   0(L'TARPID,R5),QDRAFT                                            
         OC    0(L'TARPID,R5),SPACES                                            
*                                                                               
         CLC   0(L'TARPID,R5),SPACES                                            
         JNH   INIT30                                                           
         MVC   IOKEY1,SPACES                                                    
         LA    R2,IOKEY1                                                        
         USING PERRECD,R2                                                       
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,IOCOMP                                                   
         MVC   PERKCODE,0(R5)                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,PERKEY,PERKEY,0                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,PERKDA,AIOAREA,DMWORK                  
         L     R2,AIOAREA                                                       
         LA    RF,PERRFST                                                       
         USING PIDEL,RF                                                         
         SR    RE,RE                                                            
*                                                                               
INIT20   CLI   PIDEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PIDEL,PIDELQ                                                     
         JE    INIT25                                                           
         IC    RE,PIDLN                                                         
         AR    RF,RE                                                            
         J     INIT20                                                           
*                                                                               
INIT25   LAY   R5,TARPIN                                                        
         MVC   0(L'TARPIN,R5),PIDNO                                             
         DROP  R2,RF                                                            
*                                                                               
INIT30   DS    0H                                                               
         L     RF,ADCOMFAC                                                      
         MVC   VGETRET,CGETRET-COMFACSD(RF)                                     
*                                                                               
         XC    IOKEY1,IOKEY1                                                    
         LA    R2,IOKEY1                                                        
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
         MVI   CPYKCPY,X'41'       COMPANY CODES FROM X'41' TO X'FD'            
*                                                                               
         CLC   PARMDAG,SPACES      UNLESS SPECIFIC REQUEST                      
         BNH   RUNF02                                                           
         MVC   CPYKCPY,IOCOMP                                                   
*                                                                               
         B     RUNF02                                                           
RUNF01   MVC   IOKEY1,CSVKEY1                                                   
         LA    R2,IOKEY1                                                        
         IC    RF,CPYKCPY                                                       
         LA    RF,1(,RF)                                                        
         STC   RF,CPYKCPY                                                       
RUNF02   MVC   CSVKEY1,IOKEY1                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,CPYKEY,CPYKEY,0                       
         BE    RUNF03                                                           
         DC    H'0'                                                             
RUNF03   DS    0H                                                               
*&&UK                                                                           
         CLI   IOCOMP,0            ANY SPECIFIC COMPANY?                        
         BE    RUNF03A                                                          
         CLC   CPYKCPY,IOCOMP                                                   
         BNE   RUNF03B                                                          
*&&                                                                             
RUNF03A  CLI   CPYKCPY,X'FE'       REACHED END OF COMPANY RECORDS?              
         BNE   RUNF04                                                           
RUNF03B  CLI   SORTSW,0            IS SORTER EMPTY?                             
         BE    EXIT                                                             
         GOTOR GETSORT                                                          
         SAM31                                                                  
         L     R1,AEMLTAB                                                       
         L     R0,EMLTBLN          FREE UP THE SPACE WE ASSIGNED                
         FREEMAIN RU,A=(1),LV=(0)                                               
         SAM24                                                                  
         B     EXIT                                                             
RUNF04   MVC   CSVKEY1,IOKEY1                                                   
RUNF05   CLC   CPYKEY+CPYKEND(L'CPYKEY-1),SPACES REST OF KEY NULL?              
         BE    RUNF06              YES, WE ARE AT A COMPANY RECORD              
         B     RUNF01              ELSE, NEXT RECORD                            
RUNF06   MVC   COCODE,CPYKCPY                                                   
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,CPYKDA,AIOAREA,DMWORK                  
         BE    RUNF07                                                           
         DC    H'0'                                                             
***********************************************************************         
* CHECK IF TIMESHEET APPROVAL BATCHING HAS BEEN SPECIFIED             *         
***********************************************************************         
         SPACE 1                                                                
RUNF07   GOTO1 =V(HELLO),DMCB,(C'G',ACCMST),('NAMELQ',AIOAREA),0,0              
         CLI   12(R1),0            WAS 'GET' SUCCESSFUL?                        
         BNE   RUNF07A             NO NAMEL FOUND!                              
         MVC   CONAME,SPACES                                                    
         L     R3,12(,R1)                                                       
         USING NAMELD,R3                                                        
         XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         MVC   CONAME(0),NAMEREC                                                
         EX    RF,*-6                                                           
*                                                                               
RUNF07A  DS    0H                                                               
*&&UK                                                                           
         MVC   HEAD2+01(L'AC@AGY),AC@AGY                                        
         MVC   HEAD2+12(L'CONAME),CONAME                                        
*&&                                                                             
         GOTO1 =V(HELLO),DMCB,(C'G',ACCMST),('CPYELQ',AIOAREA),0,0              
         CLI   12(R1),0            WAS 'GET' SUCCESSFUL?                        
         BNE   RUNF01              NO CPYEL FOUND; GO TO NEXT REC'D             
*                                                                               
         XC    CPYINDS,CPYINDS                                                  
         L     R3,12(,R1)          R3 CONTAINS ADDRESS OF CPYELD                
         USING CPYELD,R3                                                        
         MVC   COUSID,CPYLOGO      SAVE USER ID                                 
         CLI   CPYLN,CPYLN3Q       IS ELEMENT TOO SHORT?                        
         BNH   RUNF01              YES: GO TO NEXT RECORD                       
         TM    CPYSTATB,CPYSANBE   APPROVAL NOTICE BY EMAIL WANTED?             
         BZ    RUNF01              NO (DEFAULT): GO TO NEXT RECORD              
         TM    CPYSTAT4,CPYSOFF2   2 CHAR OFFICES?                              
         BZ    *+8                                                              
         OI    CPYINDS,CPY2CHA     SET 2 CHAR OFFICES                           
         CLI   CPYLN,CPYLN4Q                                                    
         JL    *+16                                                             
         MVC   REQPRE,CPYREQPF     SAVE OFF REQUISITION PREFIX/SUFFIX           
         MVC   REQSUF,CPYREQSF                                                  
         MVC   ALPCODE,CPYALPHA    EXTRACT AGENCY ALPHA CODE                    
         MVC   ORIGINUM,CPYUID     COMPANY PRINCIPAL ID NUMBER                  
         GOTOR GSECALP             GET SECURITY ALPHA ID                        
         JL    RUNF01                                                           
         GOTOR GETLANG             GET LANGUAGE                                 
*                                                                               
RUNF07C  CLC   COMPLANG,LLANG      TEST LANGUAGE CHANGE                         
         BE    RUNF08              NO                                           
         GOTO1 ADDICTAT,DMCB,C'LL  ',DATI,DATO RESOLVE DATA DICT ITEMS          
         ORG   *-2                                                              
         MVC   3(L'COMPLANG,R1),COMPLANG SET LANGUAGE FOR DICTATE               
         BASR  RE,RF                                                            
         MVC   LLANG,COMPLANG      SAVE LANGUAGE CODE                           
         MVC   RCLANG,COMPLANG                                                  
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         MVC   MCLANG,COMPLANG                                                  
         B     RUNF08                                                           
         DROP  R3,RF                                                            
*                                  READ 'ALL' PROFILE SO IF AGENCY NOT          
         USING CPXELD,R3                                                        
RUNF08   GOTO1 =V(HELLO),DMCB,(C'G',ACCMST),('CPXELQ',AIOAREA),0,0              
         CLI   12(R1),0            WAS 'GET' SUCCESSFUL?                        
         BNE   RUNF01              YES                                          
*                                                                               
         L     R3,12(,R1)                                                       
         MVC   CPXST4,CPXSTAT4                                                  
         MVC   CPXSTA,CPXSTATA                                                  
         MVC   FEDURL,SPACES                                                    
*                                                                               
         L     R2,AIOAREA          EXTRACT FEDERATED URL                        
         LA    R3,CPYRFST                                                       
*                                                                               
         LA    R0,FEDURL                                                        
         LHI   R1,L'FEDURL                                                      
         SR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING FFTELD,R3                                                        
RUNF09A  CLI   FFTEL,0                                                          
         BE    RUNF10                                                           
         CLI   FFTEL,FFTELQ                                                     
         BE    RUNF09B                                                          
RUNF09AA LLC   R0,FFTLN                                                         
         AR    R3,R0                                                            
         B     RUNF09A                                                          
*                                                                               
RUNF09B  CLI   FFTTYPE,FFTTFURL    CHECK FEDERATED URL ELEMENT                  
         BNE   RUNF09AA                                                         
         LLC   RF,FFTLN                                                         
         SHI   RF,1+FFTDATA-FFTELD                                              
         LA    RE,FEDURL                                                        
         MVC   0(0,RE),FFTDATA     EXTRACT FEDERATED URL                        
         EX    RF,*-6                                                           
         B     RUNF09AA                                                         
*                                                                               
RUNF10   XC    WORK,WORK           READ EM AGENCY PROFILE IF EXISTS             
         MVI   WORK,C'A'           C'AOEM'                                      
         MVC   WORK+1(2),=C'EM'                                                 
         MVC   WORK+12(2),ORIGINUM AGENCY ID                                    
         L     RF,AEMPROF                                                       
         GOTO1 GETPROF,DMCB,(X'90',WORK),0(RF),DATAMGR                          
         L     RF,AEMPROF                                                       
         OC    0(L'ORDPROF,RF),0(RF)                                            
         JNZ   RUNF12                                                           
*                                                                               
         XC    WORK,WORK           GET PROGRAM PROFILES                         
         MVI   WORK,C'A'           C'AOEM'                                      
         MVC   WORK+2(2),=C'EM'                                                 
         MVC   WORK+12(2),ALPCODE  AGENCY ALPHA                                 
         L     RF,AEMPROF                                                       
         GOTO1 GETPROF,DMCB,(X'D0',WORK),0(RF),DATAMGR                          
*                                                                               
RUNF12   GOTOR GETSEQ                                                           
*                                                                               
         XC    WORK,WORK           READ EM AGENCY PROFILE IF EXISTS             
         MVI   WORK,C'A'-X'40'     C'aEMA'                                      
         MVC   WORK+1(3),=C'EMA'                                                
         MVC   WORK+12(2),ORIGINUM AGENCY ID                                    
         L     RF,AEMPROF2                                                      
         GOTO1 GETPROF,DMCB,(X'90',WORK),0(RF),DATAMGR                          
         L     RF,AEMPROF2                                                      
         OC    0(L'ORDPROF,RF),0(RF)                                            
         JNZ   RUNF12A                                                          
*                                                                               
         XC    WORK,WORK           READ SECOND PROFILE PAGE                     
         MVI   WORK,C'A'-X'40'     C'aEMA'                                      
         MVC   WORK+1(3),=C'EMA'                                                
         MVC   WORK+12(2),ALPCODE  AGENCY ALPHA                                 
         L     RF,AEMPROF2                                                      
         GOTO1 GETPROF,DMCB,(X'D0',WORK),0(RF),DATAMGR                          
         L     RF,AEMPROF2                                                      
*                                                                               
RUNF12A  CLI   EM2EMFE,X'00'       PROFILE VALUE MISSING?                       
         BE    RUNF13              YES: PROCEED TO SEND EMAIL NOTIFICNS         
         CLI   EM2EMFE,C'D'        SEND EMAIL NOTIFICATIONS DAILY?              
         BE    RUNF13                                                           
         GOTO1 GETDAY,DMCB,TODAY6,DUB  GET DAY-NUMBER OF WEEK                   
         CLC   DUB(3),SPACES       SUCCESSFUL?                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    0(R1),X'F0'         GET INTO NUMERIC FORMAT                      
         CLC   EM2EMFE,0(R1)       PROFILE SETTING MATCHING WITH DAYNO?         
         BNE   RUNF01              NO: GO TO NEXT AGENCY RECORD                 
*                                                                               
RUNF13   XC    WORK,WORK           READ PO1 AGENCY PROFILE IF EXISTS            
         MVI   WORK,C'A'-X'40'     C'APO1'                                      
         MVC   WORK+1(3),=C'PO1'                                                
         MVC   WORK+12(2),ORIGINUM AGENCY ID                                    
         L     RF,AORDPROF                                                      
         GOTO1 GETPROF,DMCB,(X'90',WORK),0(RF),DATAMGR                          
         L     RF,AORDPROF                                                      
         OC    0(L'ORDPROF,RF),0(RF)                                            
         JNZ   RUNF14                                                           
*                                                                               
         XC    WORK,WORK           READ PO1 AGENCY PROFILE IF EXISTS            
         MVI   WORK,C'A'-X'40'     C'APO1'                                      
         MVC   WORK+1(3),=C'PO1'                                                
         MVC   WORK+12(2),ALPCODE  AGENCY ALPHA                                 
         L     RF,AORDPROF                                                      
         GOTO1 GETPROF,DMCB,(X'C0',WORK),0(RF),DATAMGR                          
RUNF14   CLI   PARMDDL,YESQ        ENFORCE D/LOAD?                              
         BNE   RUNF16                                                           
         MVI   DOWNREP,YESQ                                                     
*                                                                               
RUNF16   CLI   PARMDDL,NOQ         ENFORCE TEXT?                                
         BNE   RUNF18                                                           
         MVI   DOWNREP,NOQ                                                      
*                                                                               
RUNF18   GOTOR GLDGSJ              GET SJ LEDGER LEVELS                         
         GOTOR GLDG1R              GET 1R LEDGER LEVELS                         
*                                                                               
         LHI   R1,L'GOBLOCKB+L'GOXBLCKA+L'GOBBLCKA                              
         L     R0,AGOBLOCK         CLEAR OUT GOBLOCK AREAS                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,APRGBUFF         CLEAR OUT GOBLOCK BUFFER AREA                
         LHI   R1,PRGBUFFX-PRGBUFF                                              
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*&&UK*&& NI    AC@MUKHT,X'FF'-X'40'                                             
         NI    AC@EMNL2,X'FF'-X'40'  Undo capitilisation                        
         NI    AC@EMNL5,X'FF'-X'40'                                             
         NI    AC@EMNL7,X'FF'-X'40'                                             
         CLI   COMPLANG,CTRYGER      EMNL8/EMNL9 not start of line              
         BE    RUNF20                                                           
         NI    AC@EMNL8,X'FF'-X'40'                                             
         NI    AC@EMNL9,X'FF'-X'40'                                             
*                                                                               
RUNF20   NI    AC@PEROP,X'FF'-X'40'                                             
         NI    AC@PERPE,X'FF'-X'40'                                             
         NI    AC@JBCAD,X'FF'-X'40'                                             
         NI    AC@ESTNA,X'FF'-X'40'                                             
*                                                                               
         GOTOR BLDDSC                Setup disclaimer lines                     
***********************************************************************         
* READ TAPPASD TO GET A TIMESHEET NEEDING APPROVING                   *         
***********************************************************************         
         SPACE 1                                                                
TIMREAD  CLI   SHOWTIME,NOQ        ARE WE SHOWING TIME?                         
         BE    EXPREAD             SKIP TO EXPENSES                             
*                                                                               
         CLI   PARMTYP,C' '                                                     
         BNH   TIMR005                                                          
         CLI   PARMTYP,SORTTIME                                                 
         BNE   EXPREAD                                                          
*                                                                               
TIMR005  TM    RUNIND3,RUN3NTI     ANY MORE TIMESHEETS?                         
         BNZ   JOBREAD             NO SKIP TO EXPENSES                          
         XC    IOKEY2,IOKEY2                                                    
         LA    R4,IOKEY2                                                        
         USING TAPPASD,R4          TIME PASSIVE DSECT                           
         XC    TAPPAS,TAPPAS                                                    
         MVI   TAPPTYP,TAPPTYPQ    X'3E'                                        
         MVI   TAPPSUB,TAPPSUBQ    X'16'                                        
         MVC   TAPPCPY,COCODE      COMPANY CODE                                 
         OI    TAPPKYST,TAPSAWPQ   AWAITING APPROVAL                            
         MVC   CSVKEY2,TAPPAS                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,TAPPAS,TAPPAS,0                       
         BE    TIMR030                                                          
         DC    H'0'                                                             
*                                                                               
TIMR020  LA    R4,IOKEY2                       RESTORE READ SEQUENCE            
         MVC   IOKEY2,CSVKEY2                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,TAPPAS,TAPPAS,0                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,TAPPAS,TAPPAS,0                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TIMR030  CLC   CSVKEY2(TAPPCAT-TAPPASD),TAPPAS SAME CO. AWAITING APPVL?         
         BE    TIMSORT                                                          
         CLC   CSVKEY2(TAPPCPY-TAPPASD),TAPPAS IF NOT, ARE WE STILL             
         BE    EXPREAD                         READING TAPPAS                   
         OI    RUNIND3,RUN3NTI                 SET NO MORE TIMESHEETS           
         B     EXPREAD                                                          
         EJECT                                                                  
***********************************************************************         
* BUILD SORT RECORD                                                   *         
***********************************************************************         
         SPACE 1                                                                
TIMSORT  MVC   CSVKEY2,TAPPAS                                                   
*                                                                               
         LA    R0,SORTREC                                                       
         LHI   R1,SORTRECL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ZAP   SORTAMNT,PZERO                                                   
         MVC   SORTDATE,TAPPPEDT   PERIOD END DATE                              
         ICM   RF,7,SORTDATE                                                    
         LCR   RF,RF                                                            
         STCM  RF,7,SORTDATE       CONVERT THE DATE                             
*                                                                               
         CLI   TAPPCAT,TAPPMAN     APPROVER IS MANAGER?                         
         BNE   TIMS020                                                          
*                                                                               
         LAY   R5,TARPID                                                        
         CLC   0(L'TARPID,R5),SPACES                                            
         BNH   TIMS010                                                          
         LLC   RF,ONERL3L                                                       
         LLC   RE,ONERL4L                                                       
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         LLC   RF,ONERL3L                                                       
         LA    RF,TAPMODSP(RF)                                                  
         CLC   0(0,RF),0(R5)                                                    
         EX    RE,*-6                                                           
         BNE   TIMR020                                                          
*                                                                               
TIMS010  MVC   SORT1R,TAPMODSP                                                  
         MVC   SORTUL,ONERUL                                                    
         MVC   SORTOFF,TAPMCOFF                                                 
         CLC   SORTOFF,SPACES                                                   
         BH    TIMS040                                                          
         XR    RF,RF                                                            
         TM    CPYINDS,CPY2CHA                                                  
         BZ    *+8                                                              
         AHI   RF,1                                                             
         MVC   SORTOFF(0),TAPMODSP PUT IN OFFICE                                
         EX    RF,*-6                                                           
         B     TIMS040                                                          
*                                                                               
TIMS020  CLI   TAPPCAT,TAPP1NA     1N ACCOUNT APPROVAL?                         
         BNE   TIMS030                                                          
*                                                                               
         LAY   R5,TARPID                                                        
         CLC   0(L'TARPID,R5),SPACES                                            
         BNH   TIMS025                                                          
         LLC   RF,ONERL3L                                                       
         LLC   RE,ONERL4L                                                       
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         CLC   TAPCPODS(0),0(R5)                                                
         EX    RE,*-6                                                           
         BNE   TIMR020                                                          
*                                                                               
TIMS025  MVC   SORTUL,ONENUL                                                    
         MVC   SORT1N,TAPC1NAC     1N ACCOUNT                                   
         XR    RF,RF                                                            
         TM    CPYINDS,CPY2CHA                                                  
         BZ    *+8                                                              
         AHI   RF,1                                                             
         MVC   SORTOFF(0),TAPC1NAC PUT IN OFFICE                                
         EX    RF,*-6                                                           
         B     TIMS035                                                          
*                                                                               
TIMS030  CLI   TAPPCAT,TAPPCLI     APPROVER IS CLIENT?                          
         BNE   TIMR020                                                          
*                                                                               
         LAY   R5,TARPID                                                        
         CLC   0(L'TARPID,R5),SPACES                                            
         BNH   TIMS032                                                          
         LLC   RF,ONERL3L                                                       
         LLC   RE,ONERL4L                                                       
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         CLC   TAPCPODS(0),0(R5)                                                
         EX    RF,*-6                                                           
         BNE   TIMR020                                                          
*                                                                               
TIMS032  MVC   SORTCPJ,TAPCACT     SAVE CPJ                                     
         MVC   SORTOFF,TAPCOFF                                                  
         MVC   SORTMED,TAPCMED                                                  
         MVC   SORTUL,SJUL                                                      
         B     TIMS035                                                          
*                                                                               
TIMS035  LA    RF,TAPCPODS                                                      
         LLC   R3,ONERL3L          LENGTH SUB DEPT                              
         LLC   R6,ONERL4L          LENGTH PERSON                                
         SR    R6,R3                                                            
         AR    RF,R6                                                            
         SHI   R3,1                                                             
         MVC   SORT1R(0),0(RF)     SAVE OFFICE/DEPT/SUB-DEPT CODE               
         EX    R3,*-6                                                           
         LA    R5,SORT1R+1(R3)                                                  
         SHI   R6,1                                                             
         MVC   0(0,R5),TAPCPODS    SAVE PERSON CODE                             
         EX    R6,*-6                                                           
         CLC   SORTOFF,SPACES                                                   
         BH    TIMS040                                                          
         XR    RE,RE                                                            
         TM    CPYINDS,CPY2CHA                                                  
         BZ    *+8                                                              
         AHI   RE,1                                                             
         MVC   SORTOFF(0),0(RF)    SAVE OFFICE CODE                             
         EX    RE,*-6                                                           
         B     TIMS040                                                          
*                                                                               
TIMS040  MVI   SORTMODL,SORTTIME   SET TIMESHEETS                               
         MVC   SORTSEQ,SEQTIME                                                  
*                                                                               
         GOTOR GETAPP              GET APPROVER                                 
         OC    APPRPID,APPRPID     Any Approver?                                
         BZ    TIMR020                                                          
*                                                                               
         MVC   SVDA,TAPPDA         read record to get timetime element          
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,SVDA,AIOAREA,DMWORK                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TIMRECD,R4                                                       
         L     R4,AIOAREA                                                       
         LA    R3,TIMRFST                                                       
*                                                                               
         USING TIMELD,R3                                                        
TIMS042  CLI   TIMEL,0             Lookup timetime type to get actual           
         BE    TIMR020             period end                                   
         CLI   TIMEL,TIMELQ                                                     
         BNE   *+12                                                             
         CLI   TIMETYP,TIMETIME                                                 
         BE    TIMS044                                                          
         LLC   R0,TIMLN                                                         
         AR    R3,R0                                                            
         B     TIMS042                                                          
*                                                                               
TIMS044  MVC   SORTLEND,TIMETPDT   get actual period end                        
         GOTOR GETAP1R             GET APPROVER's 1R account                    
         BNE   TIMS090                                                          
         GOTOR CSTPRF                                                           
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
*                                                                               
         XC    NTFY#(NTFYLNQ),NTFY#                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LA    R1,24               Hours in a day                               
         CLC   CONCA,SPACES        SETTING NOT ON?                              
         BE    TIMS090                                                          
*&&US*&& OC    CONCA,CONCA         ANY NOTIFICATION NEEDED?                     
*&&US*&& BZ    TIMS090                                                          
         ZAP   DUB,CONCA                                                        
         BZ    TIMS090                                                          
         CVB   RF,DUB                                                           
         STCM  RF,3,NTFY#                                                       
         MR    RE,R1                                                            
         STCM  RF,3,NTFYHRS                                                     
*                                                                               
         CLC   CONLA,SPACES        SETTING NOT ON?                              
         BE    TIMS090                                                          
*&&US*&& OC    CONLA,CONLA         ANY NOTIFICATION NEEDED?                     
*&&US*&& BE    TIMS090                                                          
         ZAP   DOUBLE,CONLA                                                     
         BZ    TIMS090                                                          
         CVB   RF,DOUBLE                                                        
         STCM  RF,3,LOCK#                                                       
         MR    RE,R1                                                            
         STCM  RF,3,LOCKHRS                                                     
*                                                                               
         LA    R3,TIMRFST                                                       
         USING GDAELD,R3                                                        
TIMS055  CLI   GDAEL,0             End of Record                                
         BE    TIMS090                                                          
         CLI   GDAEL,GDAELQ        X'E5' - General Date Element                 
         BNE   TIMS060                                                          
         CLI   GDATYPE,GDALMSUB    Line manager approvals                       
         BE    TIMS065                                                          
TIMS060  SR    R1,R1                                                            
         IC    R1,GDALN                                                         
         AR    R3,R1                                                            
         B     TIMS055                                                          
*                                                                               
TIMS065  GOTO1 DATCON,DMCB,(1,GDADATE),(3,NTFYDTE)                              
*                                                                               
         LA    R5,24                                                            
         SR    R0,R0                                                            
         ICM   R0,3,NTFY#                                                       
TIMS070  LA    R1,DMCB             Normal call for PQ retain                    
         USING GETRETD,R1                                                       
         XC    GRDCB,GRDCB         Clear control block                          
         STCM  R5,3,GRDHRS         Set # of hours before notifying              
         MVC   GRDIDYMD,NTFYDTE    Set YMD binary input date                    
         GOTO1 VGETRET,(R1)                                                     
         BE    *+6                 Return code GRDRETC non-zero                 
         DC    H'0'                                                             
         MVC   NTFYDTE,GRDODYMD    Update binary input date                     
         BCT   R0,TIMS070                                                       
         CLC   GRDODYMD,TODAYB     Should we Notify them?                       
         BH    TIMR020             Read Next Tappasd record                     
         MVC   LOCKDTE,GRDODYMD                                                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LOCK#                                                       
TIMS080  LA    R1,DMCB             Normal call for PQ retain                    
         USING GETRETD,R1                                                       
         XC    GRDCB,GRDCB         Clear control block                          
         STCM  R5,3,GRDHRS         Set # of hours before notifying              
         MVC   GRDIDYMD,LOCKDTE    Set YMD binary input date                    
         GOTO1 VGETRET,(R1)                                                     
         BE    *+6                 Return code GRDRETC non-zero                 
         DC    H'0'                                                             
         MVC   LOCKDTE,GRDODYMD                                                 
         BCT   R0,TIMS080                                                       
         CLC   GRDODYMD,TODAYB     Should we LOCK them?                         
         BH    TIMS060                                                          
         OI    SORTSTAT,SORTLCK    LOCK APPROVER                                
         B     TIMS060                                                          
         DROP  R1,R3,R4                                                         
*                                                                               
TIMS090  MVC   SORTALP,ALPCODE                                                  
         GOTOR PUTSRT                                                           
         B     TIMR020             NEXT TAPPASD RECORD                          
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* READ EXSPASD FOR EXPENSE CLAIMS WHICH NEED APPROVING                *         
***********************************************************************         
EXPREAD  CLI   SHOWEXP,NOQ         ARE WE SHOWING EXPENSES                      
         BE    ORDREAD             NO SKIP TO ORDERS                            
         CLI   PARMTYP,C' '                                                     
         BNH   EXPR010                                                          
         CLI   PARMTYP,SORTEXP                                                  
         BNE   ORDREAD                                                          
*                                                                               
EXPR010  TM    RUNIND3,RUN3NEX     ANY MORE EXPENSE CLAIMS FIXED?               
         BNZ   EXPR105             NO GO TO EXPENSE CLAIMS CHOOSE               
*                                  READ THESE PASSIVES FOR EXPENSES             
         LA    R4,IOKEY2           THAT HAVE FIXED APPROVERS                    
         USING EXSPASD,R4          EXPENSE CLAIM PASSIVE DSECT                  
         XC    EXSPAS,EXSPAS                                                    
         MVI   EXSPTYP,EXSPTYPQ    X'37'                                        
         MVI   EXSPSUB,EXSPSUBQ    X'19'                                        
         MVC   EXSPCPY,COCODE      COMPANY CODE                                 
         MVC   CSVKEY2,EXSPAS                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,EXSPAS,EXSPAS,0                       
         BE    EXPR035                                                          
         DC    H'0'                                                             
*                                                                               
EXPR020  CLI   CSVKEY2,PIDKTYPQ    ARE WE READING PID PASSIVES?                 
         BE    EXPR120                                                          
         LA    R4,IOKEY2                                                        
         MVC   IOKEY2,CSVKEY2                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,EXSPAS,EXSPAS,0                       
         BE    *+6                                                              
         DC    H'0'                                                             
EXPR030  GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,EXSPAS,EXSPAS,0                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EXPR035  CLC   CSVKEY2(EXSPCAT-EXSPASD),EXSPAS SAME CO. AWAITING APVL?          
         BNE   EXPR105                                                          
         TM    EXSPKYST,EXCSSUBM   SUBMITTED/AWAITING APPROVAL?                 
         BNZ   *+12                                                             
         TM    EXSPKYST,EXCSFNTA   FINANCE APPROVAL?                            
         BZ    EXPR030                                                          
*                                                                               
         MVC   CSVKEY2,EXSPAS                                                   
         MVC   SVDA,EXSPDA                                                      
*                                                                               
         LA    R0,SORTREC                                                       
         LHI   R1,SORTRECL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ZAP   SORTAMNT,PZERO                                                   
*                                  READ CIDELDS TO GET                          
         MVI   SORTMODL,SORTEXP    SET EXPENSES                                 
         MVC   SORTSEQ,SEQEXP                                                   
         MVC   SORTNMBR(L'EXSPREF),EXSPREF    SAVE OFF REFERENCE                
         TM    EXSPKYST,EXSSFNTA   FINANCE APPROVAL?                            
         BNZ   EXPR100                                                          
         CLI   EXSPCAT,EXSPLFN1    IF NOT MARKED AS AWAITING FINANCE            
         BE    EXPR020             THEN DON'T SEND EMAIL                        
*                                                                               
EXPR045  CLI   EXSPCAT,EXSPMED1                                                 
         BE    EXPR020                                                          
         CLI   EXSPCAT,EXSPMED2                                                 
         BE    EXPR020                                                          
         CLI   EXSPCAT,EXSPMNB1                                                 
         BE    EXPR020                                                          
         CLI   EXSPCAT,EXSPMNB2                                                 
         BE    EXPR020                                                          
         CLI   EXSPCAT,EXSPMBL1                                                 
         BE    EXPR020                                                          
         CLI   EXSPCAT,EXSPMBL2                                                 
         BE    EXPR020                                                          
         MVI   SORTCLIL,C'1'                                                    
         CLI   EXSPCAT,EXSPCLI1                                                 
         BE    *+12                                                             
         CLI   EXSPCAT,EXSPCLI2                                                 
         BNE   EXPR050                                                          
         MVI   SORTCLIL,C'2'                                                    
         MVC   SORTOFF,EXSPCOFF            OFFICE CLIENT MEDIA                  
         MVC   SORTCPJ,EXSPSJAC                                                 
         MVC   SORTMED,EXSPMED                                                  
         MVC   SORTUL,SJUL                                                      
         B     EXPSORT                                                          
*                                                                               
EXPR050  CLI   EXSPCAT,EXSPLMN1            LINE MANAGER APPROVALS               
         BL    EXPR060                                                          
         CLI   EXSPCAT,EXSPLMN5                                                 
         BH    EXPR060                                                          
         MVI   SORTLIL,C'1'                                                     
         CLI   EXSPCAT,EXSPLMN2            STORE LINE MANAGER LEVEL             
         BNE   *+8                                                              
         MVI   SORTLIL,C'2'                                                     
         CLI   EXSPCAT,EXSPLMN3                                                 
         BNE   *+8                                                              
         MVI   SORTLIL,C'3'                                                     
         CLI   EXSPCAT,EXSPLMN4                                                 
         BNE   *+8                                                              
         MVI   SORTLIL,C'4'                                                     
         CLI   EXSPCAT,EXSPLMN5                                                 
         BNE   *+8                                                              
         MVI   SORTLIL,C'5'                                                     
         MVC   SORTUL,ONERUL                                                    
         MVC   SORT1R,EXSP1RAC                                                  
         B     EXPSORT                                                          
*                                          WPP BEHAVIOUR                        
EXPR060  CLI   EXSPCAT,EXSPCNB1            OFFICE LEVEL 1                       
         BE    *+12                                                             
         CLI   EXSPCAT,EXSPCBL1                                                 
         BNE   EXPR065                                                          
         MVI   SORTCLIL,C'1'                                                    
         MVC   SORTOFF,EXSPCOFF                                                 
         MVC   SORTCPJ,EXSPSJAC                                                 
         MVC   SORTMED,EXSPMED                                                  
         MVC   SORTUL,SJUL                                                      
         B     EXPR080                                                          
*                                                                               
EXPR065  CLI   EXSPCAT,EXSPCNB2            OFFICE LEVEL 2                       
         BE    *+12                                                             
         CLI   EXSPCAT,EXSPCBL2                                                 
         BNE   EXPR070                                                          
         MVI   SORTCLIL,C'2'                                                    
         MVC   SORTOFF,EXSPCOFF                                                 
         MVC   SORTCPJ,EXSPSJAC                                                 
         MVC   SORTMED,EXSPMED                                                  
         MVC   SORTUL,SJUL                                                      
         B     EXPR080                                                          
*                                                                               
EXPR070  CLI   EXSPCAT,EXSPRNB1            1R CLIENT LEVEL 1                    
         BE    *+12                                                             
         CLI   EXSPCAT,EXSPRBL1                                                 
         BNE   EXPR075                                                          
         MVI   SORT1RL,C'1'                                                     
         MVC   SORT1R,EXSP1RAC                                                  
         MVC   SORTUL,ONERUL                                                    
         B     EXPR080                                                          
*                                                                               
EXPR075  CLI   EXSPCAT,EXSPRNB2            1R CLIENT LEVEL 2                    
         BE    *+12                                                             
         CLI   EXSPCAT,EXSPRBL2                                                 
         BNE   EXPR080                                                          
         MVI   SORT1RL,C'2'                                                     
         MVC   SORT1R,EXSP1RAC                                                  
         MVC   SORTUL,ONERUL                                                    
         B     EXPR080                                                          
*                                                                               
EXPR080  CLI   EXSPCAT,EXSPCNB1            BILLABLE OR NON-BILLABLE?            
         BE    EXPR090                                                          
         CLI   EXSPCAT,EXSPCNB2                                                 
         BE    EXPR090                                                          
         CLI   EXSPCAT,EXSPRNB1                                                 
         BE    EXPR090                                                          
         CLI   EXSPCAT,EXSPRNB2                                                 
         BE    EXPR090                                                          
         CLI   EXSPCAT,EXSPCBL1                                                 
         BE    EXPR095                                                          
         CLI   EXSPCAT,EXSPCBL2                                                 
         BE    EXPR095                                                          
         CLI   EXSPCAT,EXSPRBL1                                                 
         BE    EXPR095                                                          
         CLI   EXSPCAT,EXSPRBL2                                                 
         BE    EXPR095                                                          
         B     EXPSORT                                                          
*                                                                               
EXPR090  MVI   SORTBNB,NOQ         NON-BILLABLE                                 
         B     EXPSORT                                                          
*                                                                               
EXPR095  MVI   SORTBNB,C'B'        BILLABLE                                     
         B     EXPSORT                                                          
*                                                                               
EXPR100  CLI   EXSPCAT,EXSPLFN1    IGNORE IF NOT FINANCE MANAGER VIEW           
         BNE   EXPR020                                                          
         MVC   SORT1R,EXSP1RAC                                                  
         MVI   SORTMODL,SORTFIN    FINANCE APPROVAL FOR EXPENSES                
         MVC   SORTUL,ONERUL                                                    
         B     EXPR050                                                          
*                                  READ PID PASSIVES FOR EXPENSES FOR           
         USING PIDRECD,R4          WHICH THE APPROVERS ARE CHOSEN               
EXPR105  CLC   CSVKEY2(EXSPCPY-EXSPASD),IOKEY2 IF NOT, ARE WE STILL             
         BE    *+8                             READING EXSPASD                  
         OI    RUNIND3,RUN3NEX                                                  
*                                                                               
EXPR110  TM    RUNIND3,RUN3NPI     ANY MORE EXPENSE CLAIMS CHOOSE APPR?         
         BNZ   ORDREAD             NO THEN GO TO ORDERS                         
*                                                                               
         LA    R4,IOKEY2                                                        
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,COCODE                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,PIDKEY,PIDKEY,0                       
         BE    EXPR130                                                          
         DC    H'0'                                                             
*                                                                               
EXPR120  LA    R4,IOKEY2                                                        
         MVC   IOKEY2,CSVKEY2                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,PIDKEY,PIDKEY,0                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EXPR125  GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,PIDKEY,PIDKEY,0                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EXPR130  MVC   CSVKEY2,PIDKEY                                                   
         CLI   PIDKTYP,PIDKTYPQ                ARE WE STILL READING             
         BNE   EXPR140                         PIDRECS?                         
         CLI   PIDKSUB,PIDKSUBQ                                                 
         BNE   EXPR140                                                          
         CLC   PIDKCPY,COCODE                                                   
         BNE   EXPR140                                                          
         CLI   PIDKSTYP,PIDKXAPQ                                                
         BNE   EXPR125                                                          
         TM    PIDKXSTA,PIDSSUBM                                                
         BZ    EXPR125                                                          
*                                                                               
         MVC   SVDA,PIDKDA                                                      
*                                                                               
         LA    R0,SORTREC                                                       
         LHI   R1,SORTRECL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ZAP   SORTAMNT,PZERO                                                   
         MVI   SORTMODL,SORTEXP                                                 
         MVC   SORTSEQ,SEQEXP                  SET EXPENSES                     
         MVC   SORTNMBR,PIDKXCLM                                                
         MVC   SORTOFF,PIDKXOFF                                                 
         MVC   SORTUL,ONERUL                                                    
         MVC   SORTPID9,PIDKPID                                                 
         MVI   SORTSTAT,SORTCAP                                                 
         B     EXPSORT                                                          
*                                                                               
EXPR140  CLC   CSVKEY2(PIDKCPY-PIDRECD),PIDKEY IF NOT, ARE WE STILL             
         BE    ORDREAD                         READING EXSPASD                  
         OI    RUNIND3,RUN3NPI                 SET NO MORE EXPENSES             
         B     ORDREAD                                                          
         SPACE 1                                                                
***********************************************************************         
* GET EXPENSE CLAIM TOTAL AND PUT TO SORTER                           *         
***********************************************************************         
         SPACE 1                                                                
         USING EXCRECD,R5                                                       
EXPSORT  GOTO1 DATAMGR,DMCB,DMGET,ACCMST,SVDA,AIOAREA,DMWORK                    
         L     R5,AIOAREA          GET THE EXPENSE CLAIM                        
*                                                                               
         LAY   RF,TARPIN                                                        
         OC    0(L'TARPIN,RF),0(RF)                                             
         BZ    EXPS005                                                          
         CLC   EXCKPIDB,0(RF)                                                   
         BNE   EXPR020                                                          
*                                                                               
EXPS005  OC    EXCKSEQ,EXCKSEQ     MAKE SURE IT'S A HEADER RECORD               
         BNZ   EXPS045                                                          
         CLC   SORTOFF,SPACES                                                   
         BH    EXPS010                                                          
         XR    RF,RF                                                            
         TM    CPYINDS,CPY2CHA                                                  
         BZ    *+8                                                              
         AHI   RF,1                                                             
         MVC   SORTOFF(0),EXCK1RAC-EXCRECD(R5)                                  
         EX    RF,*-6                                                           
*                                                                               
EXPS010  XR    RF,RF                                                            
         ICM   RF,3,EXCKDATE-EXCRECD(R5)  REVERSE 2'S COMPLEMENT                
         LNR   RF,RF                                                            
         STCM  RF,3,WORK                                                        
         GOTO1 DATCON,DMCB,(2,WORK),(1,SORTDATE) CONVERT OUR DATE               
*                                                                               
         CLC   SORT1R,SPACES                                                    
         BH    *+10                                                             
         MVC   SORT1R,EXCK1RAC-EXCRECD(R5) SAVE 1R ACCOUNT                      
         LA    R5,EXCRFST-EXCRECD(R5)                                           
         XR    R0,R0                                                            
*                                                                               
         USING CLDELD,R5                                                        
EXPS030  CLI   CLDEL,0             CLDELD LIVE ON SEQUENTAIL RECORD             
         BE    EXPS055                                                          
         CLI   CLDEL,CLDELQ                                                     
         BE    EXPS040                                                          
EXPS035  IC    R0,CLDLN                                                         
         AR    R5,R0                                                            
         B     EXPS030                                                          
*                                                                               
EXPS040  CLI   CLDTYPE,CLDTHDRQ                                                 
         BNE   EXPS035                                                          
         AP    SORTAMNT,CLDTAMT    GET TOTAL EXPENSE AMOUNT                     
         B     EXPS055                                                          
*                                                                               
         USING EXCRECD,R5                                                       
EXPS045  L     R5,AIOAREA                                                       
         MVC   IOKEY3(L'EXCKEY),0(R5)                                           
         XC    EXCKSEQ,EXCKSEQ     CLEAR OUT SEQUENCE TO READ HEADER            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,EXCKEY,EXCKEY,0                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,EXCKDA,AIOAREA,DMWORK                  
         L     R5,AIOAREA          GET THE EXPENSE CLAIM                        
         B     EXPS010                                                          
*                                                                               
EXPS055  MVC   SORTALP,ALPCODE                                                  
         GOTOR PUTSRT                                                           
         B     EXPR020                                                          
         DROP  R4,R5                                                            
***********************************************************************         
* GET ORDRECD FOR ORDERS WHICH NEED APPROVING                         *         
***********************************************************************         
         SPACE 1                                                                
ORDREAD  CLI   SHOWORD,NOQ         ARE WE SHOWING ORDERS?                       
         BE    JOBREAD             NO GO TO JOBS                                
*                                                                               
         CLI   PARMTYP,C' '                                                     
         BNH   ORDR010                                                          
         CLI   PARMTYP,SORTORD                                                  
         BNE   JOBREAD                                                          
*                                                                               
ORDR010  TM    RUNIND3,RUN3NOR     ANY MORE ORDERS?                             
         BNZ   JOBREAD             NO GO TO JOBS                                
         XC    IOKEY2,IOKEY2                                                    
         LA    R4,IOKEY2                                                        
         USING ORDRECD,R4          ORDER RECORD DSECT                           
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ    X'1A'                                        
         MVC   ORDKCPY,COCODE      COMPANY CODE                                 
         MVC   CSVKEY2,ORDKEY                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,ORDKEY,ORDKEY,0                       
         BE    ORDR030                                                          
         DC    H'0'                                                             
*                                                                               
ORDR020  LA    R4,IOKEY2                                                        
         MVC   IOKEY2,CSVKEY2                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,ORDKEY,ORDKEY,0                       
         BE    ORDR025                                                          
         DC    H'0'                                                             
*                                                                               
ORDR025  LA    R4,IOKEY2                                                        
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,ORDKEY,ORDKEY,0                       
         BE    ORDR030                                                          
         DC    H'0'                                                             
*                                                                               
ORDR030  CLC   CSVKEY2(ORDKSP1-ORDRECD),ORDKEY SAME CO. AWAITING APPVL?         
         BE    ORDR040                                                          
         CLC   CSVKEY2(ORDKCPY-ORDRECD),ORDKEY IF NOT, ARE WE STILL             
         BE    JOBREAD                                                          
         OI    RUNIND3,RUN3NOR                                                  
         B     JOBREAD                                                          
*                                                                               
ORDR040  TM    ORDKSTA2,ORDSEXEX   BRANDOCEAN ORDER                             
         BZ    ORDR025             NO THEN WE DON'T WANT IT                     
         TM    ORDKSTAT,ORDSLDEL   TEST LOGICALLY DELETED                       
         BNZ   ORDR025             YES - IGNORE                                 
         TM    ORDKSTA2,ORDSPAPP   PARTLY APPROVED?                             
         BNZ   ORDSORT             YES THEN WE WANT IT                          
         TM    ORDKSTA2,ORDSSUBM   SUBMITTED?                                   
         BZ    ORDR025             NO,THEREFORE IT IS NO GOOD                   
***********************************************************************         
* BUILD SORT RECORD FOR ORDERS                                        *         
***********************************************************************         
         SPACE 1                                                                
ORDSORT  MVC   CSVKEY2,IOKEY2                                                   
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,ORDKDA,AIOAREA,DMWORK                  
         L     R5,AIOAREA          GET THE ORDER RECORD                         
R        USING ORDRECD,R5                                                       
         LA    R6,R.ORDRFST        SET R6,R3 TO POINT TO FIRST ELEMENT          
         LR    R3,R6                                                            
*                                                                               
         LA    R0,SORTREC                                                       
         LHI   R1,SORTRECL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ZAP   SORTAMNT,PZERO                                                   
         MVC   SORTNMBR(L'R.ORDKORD),R.ORDKORD      ORDER NUMBER                
         MVC   SORTORDN,SPACES                                                  
*                                  CHECK FOR ORDER REQUISITION #                
         USING FFTELD,R6                                                        
ORDS010  CLI   FFTEL,0                                                          
         BNE   ORDS020                                                          
         LR    R6,R3                                                            
         B     ORDS040                                                          
*                                                                               
ORDS020  CLI   FFTEL,FFTELQ        X'DB'                                        
         BNE   ORDS025                                                          
         CLI   FFTTYPE,FFTTORNO    ORDER REQUISITION #                          
         BE    ORDS030                                                          
ORDS025  LLC   R0,FFTLN                                                         
         AR    R6,R0                                                            
         B     ORDS010                                                          
*                                                                               
ORDS030  LA    RF,SORTNMBR                                                      
         CLI   REQPRE,C' '                                                      
         BNH   *+14                                                             
         MVC   0(L'REQPRE,RF),REQPRE                                            
         AHI   RF,1                                                             
         MVC   0(L'ORDKORD,RF),FFTDATA    SAVE OFF REQUISTION NUMBER            
         AHI   RF,L'ORDKORD                                                     
         CLI   REQSUF,C' '                                                      
         BNH   *+14                                                             
         MVC   0(L'REQSUF,RF),REQSUF                                            
         AHI   RF,1                                                             
         LR    R6,R3                                                            
*                                  READ ORDELD TO GET                           
         USING ORDELD,R6           DATE AND CPJ                                 
ORDS040  CLI   ORDEL,0             NO MORE ORDELDS                              
         BE    ORDS065             MOVE TO NEXT ORDER                           
         CLI   ORDEL,ORDELQ        X'67'                                        
         BE    ORDS050                                                          
         CLI   ORDEL,OAMELQ        X'68'                                        
         BE    ORDS060                                                          
         CLI   ORDEL,ENMELQ        ORDER NAME ELEMENT                           
         BE    ORDS062                                                          
ORDS045  LLC   R0,ORDLN                                                         
         AR    R6,R0                                                            
         B     ORDS040                                                          
*                                                                               
ORDS050  DS    0H                                                               
         LAY   RF,TARPIN                                                        
         OC    0(L'TARPIN,RF),0(RF)                                             
         BZ    ORDS055                                                          
         OC    ORDCPID,ORDCPID                                                  
         BZ    ORDS055                                                          
         CLC   ORDCPID,0(RF)                                                    
         BNE   ORDR025                                                          
*                                                                               
ORDS055  MVC   SORTDATE,ORDDATE    DATE                                         
         MVC   SORTOFF,R.ORDROFF   OFFICE CODE FROM RECORD                      
         MVI   SORTMODL,SORTORD    SET ORDER MODULE                             
         MVC   SORTSEQ,SEQORD                                                   
         TM    ORDSTAT2,ORDSEXEX                                                
         BZ    *+10                                                             
         MVC   SORTRBDT,ORDRQBD    REQUIRED BY DATE                             
         MVC   SORTSUP,ORDSUP+L'ORDSUPC SUPPLIER ACCOUNT NUMBER                 
         CLC   ORDJOB+L'ACTKCPY(L'ACTKUNT+L'ACTKLDG),SJUL                       
         BNE   ORDS045                                                          
         MVC   SORTCPJ,ORDJOB+L'ACTKUNT+L'ACTKLDG+L'ACTKCPY SAVE OFF            
         LLC   RF,PPROLEN                                   CPJ                 
         LA    RF,SORTCPJ(RF)                                                   
         MVC   SORTMED,0(RF)       SAVE OFF MEDIA TOO                           
         B     ORDS045                                                          
*                                                                               
         USING OAMELD,R6                                                        
ORDS060  AP    SORTAMNT,OAMAMNT    AMOUNT INVOICED TO DATE                      
         B     ORDS045                                                          
*                                                                               
         USING ENMELD,R6                                                        
ORDS062  LLC   RF,ENMLN                                                         
         SHI   RF,ENMLNQ+1                                                      
         CHI   RF,L'SORTORDN       LENGTH IS TOO LONG                           
         JNH   *+8                                                              
         LHI   RF,L'SORTORDN-1                                                  
         MVC   SORTORDN(0),ENMNAME ORDER NAME                                   
         EX    RF,*-6                                                           
         B     ORDS045                                                          
*                                                                               
         USING PIDD,R5                                                          
ORDS065  GOTOR RDGOPT              READ GETOPT TO CHECK SEQUENTIAL              
         LR    R6,R3                                                            
         L     R5,APIDBLOC                                                      
         XC    0(L'PIDBLOCK,R5),0(R5)                                           
*                                  READ ORDELD TO GET                           
         USING PIDELD,R6           DATE AND CPJ                                 
ORDS070  CLI   PIDEL,0             NO MORE ORDELDS                              
         BE    ORDS100             MOVE TO NEXT ORDER                           
         CLI   PIDEL,PIDELQ        X'D8'                                        
         BE    ORDS080                                                          
ORDS075  LLC   R0,PIDLN                                                         
         AR    R6,R0                                                            
         B     ORDS070                                                          
*                                                                               
ORDS080  LR    R3,R6                                                            
         LLC   R0,PIDNTR#                                                       
         MVC   BYTE,PIDTYPE        SAVE OFF LEVEL                               
*                                                                               
ORDS085  TM    PIDNTRS,PIDAPPQ     HAS THIS PERSON APPROVED?                    
         BNZ   ORDS095             YES                                          
         MVC   PIDLVL,BYTE                                                      
         MVC   PIDBIN,PIDNTRS+1                                                 
         AHI   R5,PIDDL                                                         
         L     RF,APIDBLOC                                                      
         AHI   RF,L'PIDBLOCK                                                    
         CR    R5,RF                                                            
         BL    *+6                                                              
         DC    H'0'                MAKE SURE WE DON'T OVERFLOW ON PIDS          
         L     RF,AORDPROF         IF PO1 PROFILE SET THEN                      
         CLI   3(RF),YESQ          FIND FIRST THAT IS NOT APPROVED              
         BE    ORDS100                                                          
*                                                                               
ORDS095  AHI   R6,3                BUMP TO LAY DSECT OVER SECOND APPR           
         BCT   R0,ORDS085                                                       
         LR    R6,R3                                                            
         B     ORDS075                                                          
*                                                                               
ORDS100  L     R5,APIDBLOC         SORT OUR PIDS IN LEVEL ORDER                 
         OC    0(PIDDL,R5),0(R5)   ANY PIDS?                                    
         BZ    ORDR020                                                          
         LA    RF,L'PIDBLOCK(R5)                                                
         XC    BYTE,BYTE                                                        
*                                                                               
ORDS105  CR    R5,RF                                                            
         BNL   ORDS120                                                          
         OC    0(PIDDL,R5),0(R5)                                                
         BZ    ORDS120                                                          
         OC    PIDDL(PIDDL,R5),PIDDL(R5)  NEXT ENTRY IS BLANK?                  
         BZ    ORDS115                                                          
         CLC   PIDDL(L'PIDLVL,R5),PIDLVL                                        
         BNL   ORDS115                                                          
*                                                                               
         XC    0(PIDDL,R5),PIDDL(R5)                                            
         XC    PIDDL(PIDDL,R5),0(R5)                                            
         XC    0(PIDDL,R5),PIDDL(R5)                                            
         MVI   BYTE,1                                                           
*                                                                               
ORDS115  AHI   R5,PIDDL                                                         
         B     ORDS105                                                          
*                                                                               
ORDS120  CLI   BYTE,1              FINISHED SORTING?                            
         BE    ORDS100                                                          
*                                                                               
         L     RE,AGOXBLCK         CHECK IF SEQUENTIAL APPROVAL?                
         CLI   GOSAP-GOXBLOCK(RE),YESQ  IF YES SKIP                             
         BNE   ORDS130                                                          
         L     R5,APIDBLOC                                                      
         LA    RF,L'PIDBLOCK(R5)                                                
         LA    RE,SORTPIDS                                                      
         MVC   BYTE,PIDLVL                                                      
*                                                                               
ORDS125  MVC   0(L'PIDBIN,RE),PIDBIN                                            
         AHI   RE,L'PIDBIN         ONLY MOVE PIDS IN WHICH HAVE THE             
         AHI   R5,PIDDL            SAME LEVEL LOWEST LEVEL FIRST                
         CR    R5,RF                                                            
         BNL   ORDS145                                                          
         OC    0(PIDDL,R5),0(R5)                                                
         BZ    ORDS145                                                          
         CLC   BYTE,PIDLVL                                                      
         BNE   ORDS145                                                          
         B     ORDS125                                                          
*                                                                               
ORDS130  L     R5,APIDBLOC                                                      
         LA    RF,L'PIDBLOCK(RF)                                                
         LA    RE,SORTPIDS                                                      
         LHI   R0,SORTIL                                                        
*                                                                               
ORDS135  CR    R5,RF                                                            
         BNL   ORDS145                                                          
         OC    0(PIDDL,R5),0(R5)    ANY ENTRY?                                  
         BZ    ORDS145                                                          
         MVC   0(L'PIDBIN,RE),PIDBIN                                            
         AHI   R5,PIDDL                                                         
         AHI   RE,L'PIDBIN                                                      
         BCT   R0,ORDS135                                                       
         XR    R0,R0                                                            
*                                                                               
ORDS145  MVC   SORTALP,ALPCODE                                                  
         GOTOR PUTSRT                                                           
         B     ORDR020                                                          
         DROP  R,R4,R5,R6                                                       
***********************************************************************         
* GET DRAPASD FOR JOBS WHICH NEED APPROVING                           *         
***********************************************************************         
         SPACE 2                                                                
JOBREAD  CLI   SHOWJOB,NOQ         ARE WE SHOWING JOBS?                         
         BE    ESTREAD             NO                                           
         CLI   PARMTYP,C' '                                                     
         BNH   JOBR010                                                          
         CLI   PARMTYP,SORTJOB                                                  
         BNE   ESTREAD                                                          
*                                                                               
JOBR010  TM    RUNIND3,RUN3NJO     ANY MORE JOBS?                               
         BNZ   ESTREAD                                                          
*                                                                               
         XC    IOKEY2,IOKEY2                                                    
         LA    R4,IOKEY2                                                        
         USING DRAPASD,R4          DRAFT JOB PASSIVE DSECT                      
         XC    DRAPAS,DRAPAS                                                    
         MVI   DRAPTYP,DRAPTYPQ    X'37'                                        
         MVI   DRAPSUB,DRAPSUBQ    X'09'                                        
         MVC   DRAPCPY,COCODE      COMPANY CODE                                 
         MVI   DRAPAPS,DRAPDRA     DRAFT APPROVAL STATUS                        
         MVC   CSVKEY2,DRAPAS                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DRAPAS,DRAPAS,0                       
         BE    JOBR030                                                          
         DC    H'0'                                                             
*                                                                               
JOBR020  LA    R4,IOKEY2                                                        
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DRAPAS,DRAPAS,0                       
         BE    JOBR030                                                          
         DC    H'0'                                                             
*                                                                               
JOBR030  CLC   CSVKEY2(DRAPREM-DRAPASD),DRAPAS SAME CO. AWAITING APPVL?         
         BNE   JOBR040                                                          
         LAY   RF,TARPIN                                                        
         OC    0(L'TARPIN,RF),0(RF)                                             
         BZ    JOBSORT                                                          
         OC    DRAPPID,DRAPPID                                                  
         BZ    JOBSORT                                                          
         CLC   DRAPPID,0(RF)                                                    
         BE    JOBSORT                                                          
         B     JOBR020                                                          
*                                                                               
JOBR040  CLC   CSVKEY2(DRAPCPY-DRAPASD),DRAPAS IF NOT, ARE WE STILL             
         BE    ESTREAD                         READING DRAFT ACCOUNT            
         OI    RUNIND3,RUN3NJO                 SET NO MORE JOBS                 
         B     ESTREAD                                                          
***********************************************************************         
* BUILD SORT RECORD FOR JOBS                                          *         
***********************************************************************         
         SPACE 1                                                                
JOBSORT  LA    R0,SORTREC                                                       
         LHI   R1,SORTRECL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   SORTOFF,DRAPSOFF                                                 
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,DRAPDA,AIOAREA,DMWORK                  
         L     R5,AIOAREA          GET THE JOB                                  
         USING ACTRECD,R5                                                       
         LA    R6,ACTRFST          SET R6 TO POINT TO FIRST ELEMENT             
         XR    R0,R0                                                            
*                                  READ JOBELDS TO GET                          
         USING NAMELD,R6                                                        
JOBS010  CLI   NAMEL,0             NO MORE JOBS                                 
         BE    JOBR020             MOVE TO NEXT JOB                             
         CLI   NAMEL,NAMELQ        X'20'                                        
         BE    JOBS030                                                          
         CLI   NAMEL,JOBELQ        X'26'                                        
         BE    JOBS040                                                          
JOBS020  IC    R0,NAMLN                                                         
         AR    R6,R0                                                            
         B     JOBS010                                                          
*                                                                               
JOBS030  MVC   SORTNAME,SPACES                                                  
         XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q          ENSURE RIGHT MOVE LENGTH                     
         BCTR  RF,0                                                             
         MVC   SORTNAME(0),NAMEREC GET JOB NAME                                 
         EX    RF,*-6                                                           
         B     JOBS020                                                          
*                                                                               
         USING JOBELD,R6           DATE AND CPJ                                 
JOBS040  TM    JOBSTA2,JOBSREJ     JOB REJECTED?                                
         BNZ   JOBR020                                                          
         TM    JOBSRCES,JOBSFALQ+JOBSDDLQ IS IT BRANDOCEAN JOB?                 
         BNZ   JOBS050                                                          
         TM    JOBSRCES,JOBSAJNQ+JOBSJOBQ                                       
         BNZ   JOBS050                                                          
         B     JOBR020                                                          
*                                                                               
JOBS050  MVC   SORTDATE,JOBADATE   ADDED DATE                                   
         MVC   SORTCPJ,ACTKACT                                                  
         LA    RF,ACTKACT                                                       
         XR    R0,R0                                                            
         IC    R0,PPROLEN                                                       
         AR    RF,R0                                                            
         MVC   SORTMED,0(RF)                                                    
         MVC   SORTUL,SJUL                                                      
         MVI   SORTMODL,SORTJOB    SET JOB MODULE                               
         MVC   SORTSEQ,SEQJOB                                                   
         MVC   SORTALP,ALPCODE                                                  
         GOTOR PUTSRT                                                           
         B     JOBR020                                                          
         DROP  R4,R5,R6                                                         
***********************************************************************         
* GET EGNPASD FOR ESTIMATES WHICH NEED APPROVING                      *         
***********************************************************************         
         SPACE 2                                                                
ESTREAD  CLI   SHOWEST,NOQ         ARE WE SHOWING ESTIMATES?                    
         BE    INVREAD             NO THEN MOVE ONTO INVOICES                   
         CLI   PARMTYP,C' '                                                     
         BNH   ESTR010                                                          
         CLI   PARMTYP,SORTEST                                                  
         BNE   INVREAD                                                          
ESTR010  TM    RUNIND3,RUN3NES     ARE WE STILL READING ESTIMATES               
         BNZ   INVREAD             NO THEN MOVE ONTO INVOICES                   
*                                                                               
         XC    IOKEY2,IOKEY2                                                    
         LA    R4,IOKEY2                                                        
         USING EGNPASD,R4          ESTIMATE PASSIVE DSECT                       
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ    X'27'                                        
         MVI   EGNPSUB,EGNPSUBQ    X'02'                                        
         MVC   EGNPCPY,COCODE      COMPANY CODE                                 
         MVC   CSVKEY2,EGNPAS                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,EGNPAS,EGNPAS,0                       
         BE    ESTR030                                                          
         DC    H'0'                                                             
*                                                                               
ESTR020  LA    R4,IOKEY2                                                        
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,EGNPAS,EGNPAS,0                       
         BE    ESTR030                                                          
         DC    H'0'                                                             
*                                                                               
ESTR030  CLC   CSVKEY2(EGNPREM-EGNPASD),EGNPAS SAME CO. AWAITING APPVL?         
         BE    ESTSORT                                                          
         CLC   CSVKEY2(EGNPCPY-EGNPASD),EGNPAS IF NOT, ARE WE STILL             
         BE    INVREAD                         READING ESTIMATES                
         OI    RUNIND3,RUN3NES                 SET NO MORE ESTIMATES            
         B     INVREAD                                                          
***********************************************************************         
* BUILD SORT RECORD FOR ESTIMATES                                     *         
***********************************************************************         
         SPACE 1                                                                
ESTSORT  TM    EGNPSTA1,EGNPSUBM   IS THIS A SUBMITTED CLAIM?                   
         BZ    ESTR020             NOPE THEN WE DON'T WANT IT                   
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,EGNPDA,AIOAREA,DMWORK                  
         L     R5,AIOAREA          GET THE JOB                                  
         USING ESTRECD,R5                                                       
         LA    R6,ESTRFST          SET R6 TO POINT TO FIRST ELEMENT             
*                                                                               
         LA    RE,SORTREC                                                       
         LHI   RF,SORTRECL                                                      
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  R0,RE                                                            
*                                  READ EMDELDS TO GET                          
         USING EMDELD,R6           DATE                                         
ESTS010  CLI   EMDEL,0                                                          
         BE    ESTS090             MOVE TO NEXT ESTIMATE                        
         CLI   EMDEL,EMDELQ        X'A9'                                        
         BE    ESTS025                                                          
         CLI   EMDEL,ENMELQ        X'AA'                                        
         BE    ESTS085                                                          
ESTS020  LLC   R0,EMDLN                                                         
         AR    R6,R0                                                            
         B     ESTS010                                                          
*                                                                               
ESTS025  DS    0H                                                               
         LAY   RF,TARPIN                                                        
         OC    0(L'TARPIN,RF),0(RF)                                             
         BZ    ESTS030                                                          
         OC    EMDAPI,EMDAPI                                                    
         BZ    ESTS030                                                          
         CLC   EMDAPI,0(RF)                                                     
         BNE   ESTR020                                                          
ESTS030  MVC   SORTESTN,SPACES     CLEAR OUT ESTIMATE NAME                      
         MVC   SORTDATE,EMDADT     ESTIMATE ADDED DATE                          
         MVC   SORTNMBR(L'EMDGNO),EMDGNO     ESTIMATE NO.                       
         ZAP   SORTAMNT,EMDAMT                                                  
         TM    ESTRSTA2,ESTKSINA   SUBMITTED TO INTERNAL APPROVER?              
         BZ    ESTS035             NO                                           
         MVC   SORTPID9,EMDSIA     SUPPOSED INTERNAL APPROVER                   
         B     *+10                                                             
*                                                                               
ESTS035  MVC   SORTPID9,EMDSCA     SUPPOSED CLIENT APPROVER                     
         MVC   SORTUL(2),SJUL      JOBS                                         
         MVC   SORTCPJ,SPACES                                                   
         XR    RF,RF                                                            
         XR    RE,RE                                                            
         LHI   RE,L'ESTKCLI        DEAL WITH CPJ(JOB CODE)                      
         LA    R3,SORTCPJ          REMOVE ANY SPACES IN CPJ                     
         LA    RF,ESTKCLI+L'ESTKCLI-1                                           
*                                                                               
ESTS045  CLI   0(RF),C' '                                                       
         BNE   ESTS055                                                          
         SHI   RF,1                                                             
         BCT   RE,ESTS045                                                       
*                                                                               
ESTS055  BCTR  RE,0                                                             
         MVC   0(0,R3),ESTKCLI                                                  
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         AR    R3,RE                                                            
         LHI   RE,L'ESTKPRO                                                     
         LA    RF,ESTKPRO+L'ESTKPRO-1                                           
ESTS060  CLI   0(RF),C' '                                                       
         BNE   ESTS065                                                          
         SHI   RF,1                                                             
         BCT   RE,ESTS060                                                       
ESTS065  BCTR  RE,0                                                             
         MVC   0(0,R3),ESTKPRO                                                  
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         AR    R3,RE                                                            
         LHI   RE,L'ESTKJOB                                                     
         LA    RF,ESTKJOB+L'ESTKJOB-1                                           
ESTS070  CLI   0(RF),C' '                                                       
         BNE   ESTS075                                                          
         SHI   RF,1                                                             
         BCT   RE,ESTS070                                                       
ESTS075  BCTR  RE,0                                                             
         MVC   0(0,R3),ESTKJOB                                                  
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         AR    R3,RE                                                            
         B     ESTS020                                                          
*                                                                               
         USING ENMELD,R6           EXTRACT ESTIMATE NAME                        
ESTS085  XR    RF,RF                                                            
         IC    RF,ENMLN                                                         
         SHI   RF,(ENMLNQ+1)                                                    
         MVC   SORTESTN(0),ENMNAME ESTIMATE NAME                                
         EX    RF,*-6                                                           
ESTS090  MVI   SORTMODL,SORTEST    SET ESTIMATE MODULE                          
         MVC   SORTSEQ,SEQEST                                                   
         MVC   SORTOFF,ESTRSOFF    ESTIMATE OFFICE                              
         MVC   SORTALP,ALPCODE                                                  
         GOTOR PUTSRT                                                           
         B     ESTR020                                                          
         DROP  R4,R5,R6                                                         
***********************************************************************         
* READ REBPASD FOR INVOICES WHICH NEED APPROVING                      *         
***********************************************************************         
INVREAD  CLI   SHOWINV,NOQ         ARE WE SHOWING INVOICES?                     
         BE    INVR040             NO THEN CHECK ANYTHING TO SORT               
         CLI   PARMTYP,C' '                                                     
         BNH   INVR010                                                          
         CLI   PARMTYP,SORTINV                                                  
         BNE   INVR040                                                          
INVR010  TM    RUNIND3,RUN3NIV     ARE WE STILL READING INVOICES                
         BNZ   INVR040             NO THEN CHECK ANYTHING TO SORT               
*                                                                               
         MVC   SVINVN,SPACES                                                    
         XC    IOKEY2,IOKEY2                                                    
         LA    R4,IOKEY2                                                        
         USING REBPASD,R4          INVOICE PASSIVE DSECT                        
         XC    REBPAS,REBPAS                                                    
         MVI   REBPTYP,REBPTYPQ    X'3F'                                        
         MVI   REBPSUB,REBPSUBQ    X'09'                                        
         MVC   REBPCPY,COCODE      COMPANY CODE                                 
         MVI   REBPMOD,REBPMREF    REFERENCE NUMBER                             
         MVC   CSVKEY2,REBPAS                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,REBPAS,REBPAS,0                       
         BE    INVR030                                                          
         DC    H'0'                                                             
*                                                                               
INVR020  LA    R4,IOKEY2                                                        
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,REBPAS,REBPAS,0                       
         BE    INVR030                                                          
         DC    H'0'                                                             
*                                                                               
INVR030  CLC   CSVKEY2(REBPREM-REBPASD),REBPAS SAME CO. AWAITING APPVL?         
         BE    INVSORT                                                          
         CLC   CSVKEY2(REBPCPY-REBPASD),REBPAS IF NOT, ARE WE STILL             
         BE    INVR040             READING INVOICES                             
         OI    RUNIND3,RUN3NIV     SET NO MORE INVOICES                         
*                                                                               
INVR040  CLI   SORTSW,0                                                         
         BE    RUNF01   NOTHING TO SORT, NOW GO TO NEXT COMPANY RECORD          
         GOTOR GETSORT                                                          
         B     RUNF01   READ NEXT COMPANY RECORD                                
***********************************************************************         
* BUILD SORT RECORD FOR INVOICES                                      *         
***********************************************************************         
INVSORT  TM    REBPSTA1,REBSBRAN   ADDED BY BRANDOCEAN?                         
         BZ    INVR020                                                          
         TM    REBPSTA2,REBSBPAP+REBSBPOS                                       
         BZ    INVR020             CHECK POSTED OR PART APPROVED                
         CLC   REBPNUM,SPACES      FILTER OUT CONTROL RECORDS                   
         BNH   INVR020                                                          
         CLC   SVINVN,SPACES       FIRST TIME THROUGH?                          
         BNH   INVS010                                                          
         CLC   REBPNUM,SVINVN      CHECK IF WE HAVE SEQUENTIAL RECORD           
         BNE   INVS010                                                          
         TM    RUNIND4,RUNIAPP     HAVE WE GOT APPROVALS FOR THIS INV           
         BNZ   INVR020             IF YES THEN WE CAN SKIP SEQUENTIALS          
         OI    RUNIND4,RUNIGAPP    IF NO THEN READ THEM                         
         B     INVS015                                                          
*                                                                               
INVS010  NI    RUNIND4,X'FF'-(RUNIAPP+RUNIGAPP)                                 
         MVC   SVINVN,REBPNUM                                                   
*                                                                               
INVS015  LA    R0,SORTREC          NO DON'T WANT IT THEN                        
         LHI   R1,SORTRECL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ZAP   SORTAMNT,PZERO                                                   
         MVC   SORTINTR,REBPREF    REFERENCE NUMBER                             
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,REBPDA,AIOAREA,DMWORK                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIOAREA                                                       
         MVC   SORTNMBR(L'REBKNUM),REBKNUM-REBRECD(R5)  INVOICE LOG NO          
         LA    R5,REBRFST-REBRECD(R5)                                           
         XR    R0,R0                                                            
         XR    RF,RF                                                            
*                                                                               
         USING RBDELD,R5                                                        
APP      USING RBDELD,R6                                                        
INVS025  CLI   RBDEL,0                                                          
         BE    INVS100                                                          
         CLI   RBDEL,RBDELQ        INVOICE ELEMENT                              
         BE    INVS035                                                          
INVS030  IC    R0,RBDLN                                                         
         AR    R5,R0                                                            
         B     INVS025                                                          
*                                                                               
INVS035  TM    RUNIND4,RUNIGAPP    GETTING APPROVAL ELS ONLY?                   
         BNZ   INVS060                                                          
         CLI   RBDTYP,RBDTHDR      HEADER ELEMENT?                              
         BNE   INVS045                                                          
         ZAP   SORTAMNT,RBDTIAMT                                                
         MVC   SORTDATE,RBDDATE                                                 
         MVC   SORTSUP,RBDSUPP                                                  
         LAY   RF,TARPIN                                                        
         OC    0(L'TARPIN,RF),0(RF)                                             
         BZ    INVS030                                                          
         OC    RBDPIDAD,RBDPIDAD                                                
         BZ    INVS030                                                          
         CLC   RBDPIDAD,0(RF)                                                   
         BNE   INVR020                                                          
         B     INVS030                                                          
*                                                                               
INVS045  CLI   RBDTYP,RBDTBAT      BATCH DETAILS?                               
         BNE   INVS050                                                          
         MVC   SORTDUPD,RBDBADD    BATCH = INVOICE UPDATED DATE                 
         LAY   RF,TARPIN                                                        
         OC    0(L'TARPIN,RF),0(RF)                                             
         BZ    INVS030                                                          
         OC    RBDBABY,RBDBABY                                                  
         BZ    INVS030                                                          
         CLC   RBDBABY,0(RF)                                                    
         BNE   INVR020                                                          
         B     INVS030                                                          
*                                                                               
INVS050  CLI   RBDTYP,RBDTITM      ITEM POSTINGS                                
         BNE   INVS060                                                          
         CLC   RBDSEQ,=XL2'0001'   ONLY TAKE ORDER FROM 1ST ITEM                
         BNE   INVS030                                                          
         MVC   SORTOFF,RBDROFF                                                  
         MVC   SORTION,RBDRORD                                                  
         B     INVS030                                                          
*                                                                               
INVS060  CLI   RBDTYP,RBDTAPP      APPROVALS                                    
         BNE   INVS030                                                          
         OI    RUNIND4,RUNIAPP     SET GOT APPROVALS                            
         LLC   R0,RBDAPNUM         NO. OF APPROVALS                             
         LR    R6,R5                                                            
         LA    RE,SORTPIDS         FIRST PID IN SORTREC                         
         LA    RF,SORTPIDS+SORTIPID LAST PID IN SORTREC                         
*                                                                               
INVS070  CLI   APP.RBDAPSTA,0      ANY APPROVALS?                               
         BE    INVS080                                                          
         TM    APP.RBDAPSTA,RBDAORSR IF APPROVAL NOT REQ NOT WANTED             
         BZ    INVS090                                                          
         TM    APP.RBDAPSTA,RBDAREJ+RBDAAPP IGNORE IF REJ AND APPROVED          
         BNZ   INVS090                                                          
*                                                                               
INVS080  MVC   0(L'RBDAPPID,RE),APP.RBDAPPID NEEDS TO APPROVE                   
         AHI   RE,L'RBDAPPID                                                    
INVS090  AHI   R6,RBDAPLNQ                                                      
         CR    RE,RF               CHECK WHETHER HIT LAST PID IN SORTR          
         BH    INVS030                                                          
         BCT   R0,INVS070                                                       
         B     INVS030                                                          
*                                                                               
INVS100  OC    SORTPIDS(SORTIPID),SORTPIDS ANY APPROVER'S FOUND?                
         BZ    INVR020                                                          
*                                                                               
         CLI   SHOWDET,NOQ         skip if no details required                  
         BE    INVS110                                                          
         GOTOR GETIID,SORTION      read invoices for item details               
         LAY   R1,TEMP100                                                       
         MVC   SORTITYP,0(R1)                                                   
         MVC   SORTIORD,1(R1)                                                   
         MVC   SORTRBDT,THREE                                                   
*                                                                               
INVS110  MVC   SORTSEQ,SEQINV                                                   
         MVI   SORTMODL,SORTINV                                                 
         MVC   SORTALP,ALPCODE                                                  
         GOTOR PUTSRT                                                           
         B     INVR020                                                          
         DROP  R5,R4,APP                                                        
***********************************************************************         
* GET INVOICE ITEM DETAILS                                            *         
***********************************************************************         
         SPACE 1                                                                
         SPACE 1                                                                
         USING RBDELD,R2                                                        
GETIID   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**GETI**'                                                      
         MVC   DUB(L'RBDRORD),0(R1)      (SORTION order number)                 
         LAY   R3,TEMP100                                                       
         XC    0(L'TEMP100,R3),0(R3)                                            
         MVC   IOKEYSV,IOKEY2                                                   
GETIID02 L     R2,AIOAREA          start with present record                    
         AHI   R2,REBRFST-REBRECD                                               
GETIID04 CLI   RBDEL,0                                                          
         JE    GETIID20                                                         
         CLI   RBDEL,RBDELQ                                                     
         JNE   GETIID16                                                         
         CLI   RBDTYP,RBDTITM                                                   
         JNE   GETIID16                                                         
         CLI   0(R3),C'M'          set bill/nonbill/mixed                       
         JE    GETIID08                                                         
         CLC   0(1,R3),RBDRTYP                                                  
         JE    GETIID08                                                         
         CLI   0(R3),0                                                          
         JNE   GETIID06                                                         
         MVC   0(1,R3),RBDRTYP                                                  
         J     GETIID08                                                         
GETIID06 MVI   0(R3),C'M'                                                       
GETIID08 CLC   RBDRORD,SPACES      save order numbers                           
         JNH   GETIID16                                                         
         CLC   DUB(L'RBDRORD),SPACES                                            
         JNH   GETIID10                                                         
         CLC   RBDRORD,DUB         skip this one                                
         JE    GETIID16                                                         
GETIID10 LR    R1,R3                                                            
         AHI   R1,2                                                             
         LHI   R0,MAXORDQ                                                       
GETIID12 OC    0(L'RBDRORD,R1),0(R1)                                            
         JZ    GETIID14                                                         
         CLC   0(L'RBDRORD,R1),RBDRORD                                          
         JE    GETIID16                                                         
         AHI   R1,L'RBDRORD+L'ORDRQBD                                           
         JCT   R0,GETIID12                                                      
         MVI   1(R3),C'+'          if > max num set flag to say so              
         J     GETIID16                                                         
GETIID14 MVC   0(L'RBDRORD,R1),RBDRORD                                          
GETIID16 LLC   R0,RBDLN                                                         
         AR    R2,R0                                                            
         J     GETIID04                                                         
         USING REBRECD,R3                                                       
DIR      USING REBRECD,R4                                                       
GETIID20 CLI   REBKSEQ,0           sequence 0?                                  
         JNE   GETIID22                                                         
         LA    R4,KEY              then build IO seq for REBKEY reads           
         MVC   DIR.REBKEY,REBKEY                                                
         MVI   DIR.REBKSEQ,1                                                    
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DIR.REBKEY,DIR.REBKEY,0               
         JNE   GETIID30                                                         
         J     GETIID24                                                         
GETIID22 DS    0H                  else get sequential                          
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DIR.REBKEY,DIR.REBKEY,0               
         JNE   GETIID30            and check same REB                           
         CLC   DIR.REBKEY(REBKSEQ-REBRECD),REBKEY                               
         JNE   GETIID30                                                         
GETIID24 GOTO1 DATAMGR,DMCB,DMGET,ACCMST,REBKDA,AIOAREA,DMWORK                  
         JE    GETIID02                                                         
         DC    H'0'                                                             
GETIID30 LAY   R4,TEMP100          read orders for required by dates            
         AHI   R4,2                                                             
         LHI   R0,MAXORDQ                                                       
         LA    R5,DUB                                                           
         LA    R6,THREE                                                         
         BRAS  RE,GETIIDO                                                       
GETIID32 LA    R5,0(R4)                                                         
         LA    R6,L'RBDRORD(R4)                                                 
         BRAS  RE,GETIIDO                                                       
         AHI   R4,L'RBDRORD+L'ORDRQBD                                           
         JCT   R0,GETIID32                                                      
GETIID50 XC    IOKEY2,IOKEY2                                                    
         MVC   IOKEY2(L'REBKEY),IOKEYSV                                         
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,IOKEY2,IOKEY2,0                       
         J     EXIT                                                             
         DROP  R2,R3,DIR                                                        
                                                                                
         USING ORDRECD,R2                                                       
         USING ORDELD,R3                                                        
GETIIDO  ST    RE,FULL             get order details                            
         XC    0(L'ORDRQBD,R6),0(R6)                                            
         CLC   0(L'ORDKORD,R5),SPACES                                           
         JNH   GETIIDOX                                                         
         LA    R2,IOKEY2                                                        
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,COCODE                                                   
         MVC   ORDKORD,0(R5)                                                    
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,ORDKEY,ORDKEY,0                       
         JNE   GETIIDOX                                                         
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,ORDKDA,AIOAREA,DMWORK                  
         JE    GETIIDO2                                                         
         DC    H'0'                                                             
GETIIDO2 L     R3,AIOAREA                                                       
         AHI   R3,ORDRFST-ORDRECD                                               
GETIIDO4 CLI   ORDEL,0                                                          
         JE    GETIIDOX                                                         
         CLI   ORDEL,ORDELQ                                                     
         JE    GETIIDO6                                                         
         LLC   R1,ORDLN                                                         
         AR    R3,R1                                                            
         J     GETIIDO4                                                         
GETIIDO6 TM    ORDSTAT2,ORDSEXEX                                                
         JZ    GETIIDOX                                                         
         MVC   0(L'ORDRQBD,R6),ORDRQBD                                          
GETIIDOX L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT RECORDS TO SORTER                                    *         
***********************************************************************         
         SPACE 1                                                                
PUTSRT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**PUTS**'                                                      
         CLI   SORTSW,0            TEST SORT INITIALISED                        
         JNE   PSRT02                                                           
         LAY   R5,SORTCARD                                                      
         LAY   R6,SORTTYPE                                                      
         MVC   0(L'SORTCARD,R5),SPACES                                          
         MVC   0(L'SORTTYPE,R6),SPACES                                          
         MVC   0(13,R5),=C'SORT FIELDS=('                                       
         LA    RF,13(R5)           A(FIRST SORTCARD ENTRY)                      
         MVI   0(RF),C'1'          DISPLACEMENT OF 1                            
         AHI   RF,1                                                             
         MVI   0(RF),C','                                                       
         AHI   RF,1                                                             
         LHI   RE,SORTKEYL                                                      
         CVD   RE,DUB                                                           
         UNPK  0(2,RF),DUB                                                      
         LTR   RE,RE                                                            
         JZ    *+12                                                             
         OI    1(RF),X'F0'                                                      
         AHI   RF,2                                                             
         MVC   0(2,RF),=C',A'                                                   
         AHI   RF,2                                                             
         MVC   0(18,RF),=C'),FORMAT=BI,WORK=1'                                  
*                                                                               
         MVC   0(30,R6),=C'RECORD TYPE=F,LENGTH=(000,,,,)'                      
         LHI   R1,SORTRECL                                                      
         CVD   R1,DUB              CONVERT RECORD LENGTH TO CHARACTER           
         OI    DUB+7,X'0F'                                                      
         UNPK  22(3,R6),DUB+6(2)                                                
         MVI   SORTSW,1            SET SORT INITIALISED                         
         GOTO1 ADSORTER,DMCB,(R5),(R6)                                          
*                                                                               
PSRT02   GOTO1 ADSORTER,DMCB,SORTPUT,SORTREC                                    
         L     RF,TOTNUMP                                                       
         AHI   RF,1                                                             
         ST    RF,TOTNUMP                                                       
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* GET RECORDS BACK FROM PRELIMINARY SORTING                           *         
***********************************************************************         
         SPACE 1                                                                
GETSORT  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**GETS**'                                                      
         SAM31                                                                  
         L     R0,AEMLTAB          CLEAR EMAIL TABLE                            
         ST    R0,ANXTETB                                                       
         L     R1,EMLTBLN                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         SAM24                                                                  
         XC    NUMEMLS,NUMEMLS                                                  
         XC    NUMRECS,NUMRECS                                                  
         XC    NUMREC2,NUMREC2                                                  
         XC    RUNIND1,RUNIND1                                                  
         XC    RUNIND2,RUNIND2                                                  
         XC    RUNIND4,RUNIND4                                                  
         XC    ONERCODE,ONERCODE                                                
*                                                                               
         LA    RF,AAURURL                                                       
         GOTO1 VGETURL,DMCB,(RF)                                                
         L     RE,ADMASTC          GET THE UTL ADDRESS                          
         L     RE,MCUTL-MASTD(RE)                                               
         MVC   SVURL,TEXTSPCS                                                   
*                                                                               
         L     R1,AAURURL          PULL URL FROM TABLE                          
         USING AGYURLD,R1                                                       
GET06    CLI   AGUSE,0             TEST OR LIVE                                 
         JE    GET08               LIVE                                         
         CLC   AGUSE,4(RE)         FIND MATCH ON SE NUMBER FOR TEST             
         JNE   GET10                                                            
         CLI   RCDSPAC,C'A'        ARE WE RUNNING FOR PRODUCTION?               
         JNE   GET12               IF NOT - GO AHEAD                            
         J     GET10               IF WE ARE - CONTINUE LOOPING                 
*                                                                               
GET08    CLI   AGUAA,0             END OF TABLE, USE DEFAULT                    
         JE    GET12                                                            
         CLC   AGUAA,ALPCODE       MATCH ON AGENCY ALPHA                        
         JE    GET12                                                            
GET10    AHI   R1,AGYURLQ          NO MATCH, NEXT ENTRY                         
         J     GET06                                                            
*                                                                               
GET12    MVC   SVURL,AGUURL2                                                    
         MVC   SVHTTP,AGUHTT                                                    
         MVC   SVENV,AGUENV                                                     
         MVC   SVFED,AGUFED                                                     
         MVC   SVFEDP,AGUFEDP                                                   
         MVC   SVFEDS,AGUFEDS                                                   
         MVC   SVTST,AGUTST                                                     
*                                  PUT REGION SPECIFIC ADDRESS                  
*&&UK                                UK/German web address                      
GET14    L     RF,AELE140E                                                      
         MVC   0(L'ELEM140E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MUKHT,RF),AC@MUKHT                                        
         L     RF,AELE840E                                                      
         MVC   0(L'ELEM840E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MUKHT,RF),AC@MUKHT                                        
         L     RF,AEMS120E                                                      
         MVC   0(L'EMLS120E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MUKHT,RF),AC@MUKHT                                        
         L     RF,AEMS660E                                                      
         MVC   0(L'EMLS660E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MUKHT,RF),AC@MUKHT                                        
*&&                                                                             
         L     RE,AEMS610E                                                      
         L     RF,AELE790E                                                      
*&&UK                                                                           
         MVC   0(L'ELEM790E,RE),TEXTSPCS                                        
         MVC   0(L'AC@MOUK,RE),AC@MOUK                                          
         MVC   0(L'EMLS610E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MOUK,RF),AC@MOUK                                          
*                                                                               
         CLI   COMPLANG,CTRYGBR      UK address                                 
         JE    GET16                                                            
         CLI   COMPLANG,CTRYDFL                                                 
         JE    GET16                                                            
         MVC   0(L'ELEM790E,RE),TEXTSPCS                                        
         MVC   0(L'AC@MODE,RE),AC@MODE                                          
         MVC   0(L'EMLS610E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MODE,RF),AC@MODE                                          
         CLI   COMPLANG,CTRYGER      German address                             
         JE    GET16                                                            
*&&                                                                             
*&&US                                                                           
         MVC   0(L'ELEM790E,RE),TEXTSPCS                                        
         MVC   0(L'AC@MOCA,RE),AC@MOCA                                          
         MVC   L'AC@MOCA(L'AC@MOC2,RE),AC@MOC2                                  
         MVC   0(L'EMLS610E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MOCA,RF),AC@MOCA                                          
         MVC   L'AC@MOCA(L'AC@MOC2,RF),AC@MOC2                                  
         CLI   CTRY,CTRYCAN          Canadian address                           
         JE    GET04                                                            
*&&                                                                             
         MVC   0(L'ELEM790E,RE),TEXTSPCS                                        
         MVC   0(L'AUADDR,RE),AUADDR                                            
         MVC   0(L'EMLS610E,RF),TEXTSPCS                                        
         MVC   0(L'AUADDR,RF),AUADDR Default is US address                      
*                                                                               
GET04    L     RF,AEMS610E                                                      
         GOTO1 SQUASHER,DMCB,0(RF),L'EMLS610E                                   
         L     RF,AELE790E                                                      
         GOTO1 SQUASHER,DMCB,0(RF),L'ELEM790E                                   
         L     RF,AELE140E                                                      
         MVC   0(L'ELEM140E,RF),TEXTSPCS                                        
         LA    RE,AUHTML             Assume its US                              
*&&US                                                                           
         CLI   CTRY,CTRYCAN          Canadian address                           
         JNE   *+8                                                              
         LA    RE,AC@MCAHT                                                      
         MVC   0(L'AUHTML,RF),0(RE)                                             
*&&                                                                             
         L     RF,AELE840E                                                      
         MVC   0(L'ELEM840E,RF),TEXTSPCS                                        
         MVC   0(L'AUHTML,RF),0(RE)                                             
         L     RF,AEMS120E                                                      
         MVC   0(L'EMLS120E,RF),TEXTSPCS                                        
         MVC   0(L'AUHTML,RF),0(RE)                                             
         L     RF,AEMS660E                                                      
         MVC   0(L'EMLS660E,RF),TEXTSPCS                                        
         MVC   0(L'AUHTML,RF),0(RE)                                             
*                                  STORE URL FOR USE LATER (IN SVURL)           
GET16    CLI   PARMDPQ,YESQ        SUPPRESS FROM GOING TO PRINT QUEUE?          
         JE    GET18               . YES                                        
*                                                                               
         L     RF,REMOTEC          REPORTING ON CLIENT PQ                       
         USING REMOTED,RF                                                       
         XC    REMOTKEY,REMOTKEY   START A NEW REPORT                           
         MVI   REMOTSYS,C'A'       A=ACCOUNTING                                 
         MVC   REMOTPRG,=C'EM'     THE 'EM' FROM PHASE NAME                     
         MVC   REMOTJID,REMOTSYS                                                
         MVC   REMOTDST,ORIGINUM   USE PRINCIPAL ID NUMBER FROM CO RECS         
         MVC   REMOTFRM,=C'DEF'                                                 
         MVC   REMOTSUB+1(3),=C'DEF'                                            
         DROP  RF                                                               
GET18    CLI   RCWRITE,NOQ                                                      
         JE    GET20                                                            
         GOTO1 VSMTP,DMCB,('SMTPAINI',0) INITIALISE JESMAIL                     
         GOTO1 VSMTP,DMCB,('SMTPASLL',0)        SET LONG LINES                  
         GOTO1 VSMTP,DMCB,('SMTPHTMH',0) SET HTML HEADER                        
         GOTO1 VSMTP,DMCB,('SMTPAFR2',0),(L'AUFROM,AUFROM) Set from add         
         MVI   RCSUBPRG,0          SET FIRST PAGE                               
*                                                                               
GET20    GOTO1 ADSORTER,DMCB,SORTGET                                            
         ICM   RF,15,4(R1)         4(R1) CONTAINS ADDRESS OF RECORD             
         JNZ   GET22               IF ZERO, THEN AT EOF                         
         GOTOR PROETAB             PROCESS EMAIL TABLE                          
         GOTO1 ADSORTER,DMCB,SORTEND                                            
*                                                                               
         GOTOR PRTDET              PRINT REPORT DETAILS                         
         GOTOR DETREP              PRINT DETAILED REPORT                        
         GOTOR LCKREP              PRINT LOCK REPORT                            
         SAM24                                                                  
         MVI   SORTSW,0                                                         
         CLI   RCWRITE,NOQ                                                      
         JE    EXITE                                                            
         GOTO1 VSMTP,DMCB,('SMTPAEND',0) DETACH FROM JESMAIL                    
         J     EXITE                                                            
*                                                                               
GET22    MVC   SORTREC(200),0(RF)                                               
         MVC   SORTREC+200(SORTRECL-200),200(RF)                                
         LA    R1,SORTKEYL         Default length for compare                   
         CLI   EM2SOFF,YESQ        Sort By Office Profile set?                  
         JE    *+8                                                              
         LA    R1,SORTKEYL-L'SORTOFF   length of key w/o office                 
         AHI   R1,-1                                                            
         LAY   RE,SAVEREC                                                       
         BASR  RF,0                                                             
         CLC   SORTREC(0),0(RE)    SAME RECORD AS BEFORE?                       
         EX    R1,0(RF)                                                         
         JE    GET20                                                            
         MVC   0(200,RE),SORTREC                                                
         MVC   200(SORTRECL-200,RE),SORTREC+200                                 
         NI    RUNIND1,X'FF'-(RUNINOAP+RUNINOCP+RUNITERM)                       
         NI    RUNIND2,X'FF'-(RUN2DEFT)                                         
         CLI   SORTMODL,SORTTIME   ONLY FOR TIME AND EXPENSES                   
         JNE   GET24                                                            
         TM    RUNIND2,RUN2TIM     FIRST TIME?                                  
         JNZ   *+8                 NO                                           
         OI    RUNIND2,RUN2TIM     SET GOT TIMESHEETS                           
         J     GET28                                                            
*                                                                               
GET24    CLI   SORTMODL,SORTEXP    GOT EXPENSES                                 
         JNE   GET26                                                            
         TM    RUNIND2,RUN2EXP     FIRST TIME?                                  
         JNZ   *+8                 NO                                           
         OI    RUNIND2,RUN2EXP     SET GOT EXPENSES                             
         J     GET28                                                            
*                                                                               
GET26    CLI   SORTMODL,SORTFIN    GOT FINANCE APPROVER EXPENSES?               
         JNE   GET56               NO GO TO ORDERS                              
         GOTOR GETFIN              IF WE'RE DOING FINANCE GO TO GETFIN          
         JE    GET44                                                            
         OI    RUNIND1,RUNINOAP    SET NO FINANCE APPROVER                      
         J     GET32                                                            
*                                                                               
GET28    CLC   SORTREC(SORTNMBR-SORTKEY),WORK  Same REC as before?              
         JE    GET20                           only report one                  
         MVC   WORK(SORTKEYL),SORTREC                                           
         TM    SORTSTAT,SORTCAP    APPROVER CHOSEN?                             
         JZ    GET30                                                            
         MVC   APPRPID,SORTPID9                                                 
         J     GET44                                                            
*                                                                               
GET30    GOTOR GETAPP              GET APPROVER                                 
         JE    GET44                                                            
         JH    GET20                                                            
         OI    RUNIND1,RUNINOAP    SET NO APPROVER                              
         CLI   SORTMODL,SORTTIME                                                
         JNE   GET32                                                            
         GOTOR SETTHED                                                          
         J     GET34                                                            
*                                                                               
GET32    CLI   SORTMODL,SORTFIN                                                 
         JE    *+14                                                             
         CLI   SORTMODL,SORTEXP                                                 
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR SETEHED                                                          
         J     GET34                                                            
*                                                                               
GET34    LA    RF,P+1                                                           
         MVC   0(L'AC@ANFFA,RF),AC@ANFFA                                        
         LA    RF,L'AC@ANFFA+1(RF)                                              
         CLC   SORTUL,SJUL                                                      
         JNE   GET36                                                            
         MVC   0(L'ACTKLDG+L'ACTKUNT,RF),SJUL                                   
         MVC   L'SJUL(L'SORTCPJ,RF),SORTCPJ                                     
         J     GET40                                                            
*                                                                               
GET36    CLC   SORTUL,ONENUL                                                    
         JNE   GET38                                                            
         MVC   0(L'ACTKLDG+L'ACTKUNT,RF),ONENUL                                 
         MVC   L'ONENUL(L'ACTKACT,RF),SORT1N                                    
         J     GET40                                                            
*                                                                               
GET38    MVC   0(L'ACTKLDG+L'ACTKUNT,RF),ONERUL                                 
         MVC   L'ONERUL(L'ACTKACT,RF),SORT1R                                    
*                                                                               
GET40    LA    RF,L'ACTKACT+L'SJUL+1(RF)                                        
         CLI   SORTMODL,SORTFIN                                                 
         JE    *+12                                                             
         CLI   SORTMODL,SORTEXP                                                 
         JNE   GET42                                                            
         MVI   0(RF),C'('                                                       
         MVC   1(L'AC@EXPN,RF),AC@EXPN                                          
         LA    RF,L'AC@EXPN+2(RF)                                               
         MVC   0(L'SORTNMBR,RF),SORTNMBR                                        
         LA    RF,L'SORTNMBR+1(RF)                                              
         MVI   0(RF),C')'                                                       
*                                                                               
GET42    GOTO1 SQUASHER,DMCB,P,80                                               
         GOTO1 ACREPORT                                                         
         J     GET54                                                            
*                                                                               
GET44    GOTOR GETEML,0            VALIDATE APPROVER                            
         JE    GET54                                                            
         CLI   SORTMODL,SORTTIME                                                
         JNE   GET46                                                            
         GOTOR SETTHED                                                          
         J     GET48                                                            
*                                                                               
GET46    CLI   SORTMODL,SORTFIN                                                 
         JE    *+14                                                             
         CLI   SORTMODL,SORTEXP                                                 
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR SETEHED                                                          
         J     GET48                                                            
*                                                                               
GET48    MVC   P+1(L'AC@ANFFA),AC@ANFFA  'APPROVER NOT FOUND FOR A/C'           
         MVC   P+32(L'SORTCPJ),SORTCPJ                                          
         CLC   SORTCPJ,SPACES                                                   
         JH    GET52                                                            
         CLC   SORTUL,ONENUL                                                    
         JNE   GET50                                                            
         MVC   P+32(L'ACTKLDG+L'ACTKUNT),ONENUL                                 
         MVC   P+34(L'ACTKACT),SORT1N                                           
         J     GET52                                                            
*                                                                               
GET50    MVC   P+32(L'ACTKLDG+L'ACTKUNT),ONERUL                                 
         MVC   P+34(L'ACTKACT),SORT1R                                           
*                                                                               
GET52    MVI   P+47,C'('                                                        
         GOTO1 VHEXOUT,DMCB,APPRPID,P+48,2                                      
         MVI   P+52,C')'                                                        
         GOTO1 SQUASHER,DMCB,P,80                                               
         GOTO1 ACREPORT                                                         
         OI    RUNIND1,RUNINOCP    NO VALID CHARACTER PID                       
*                                                                               
GET54    TM    RUNIND1,RUNINOCP+RUNINOAP+RUNITERM DON'T REPEAT ERRORS           
         JNZ   GET20                                                            
         GOTOR PUTETAB             ELSE PUT DETAILS INTO EMAIL TABLE            
         JE    GET20               GO TO NEXT SORTED RECORD                     
         J     GETERR                                                           
*                                                                               
GET56    CLI   SORTMODL,SORTORD    IF DOING ORDERS                              
         JNE   GET66                                                            
         TM    RUNIND2,RUN2ORD     FIRST TIME?                                  
         JNZ   *+8                 NO                                           
         OI    RUNIND2,RUN2ORD     SET GOT ORDERS                               
         LA    R5,SORTPIDS         ADDRESS OF FIRST PID                         
         LHI   RF,SORTIL           NUMBER OF PIDS FOR ORDERS                    
         ST    RF,SAVERF                                                        
*                                                                               
GET58    NI    RUNIND1,X'FF'-(RUNINOCP+RUNINOAP+RUNITERM)                       
         MVC   APPRPID,0(R5)                                                    
         OC    APPRPID,APPRPID     ANY APPROVER?                                
         JNZ   GET60                                                            
         OI    RUNIND1,RUNINOAP    SET NO APPROVER                              
         J     GET62                                                            
*                                                                               
GET60    GOTOR GETEML,0            VALIDATE APPROVER                            
         JE    GET62                                                            
         OI    RUNIND1,RUNINOCP    NO VALID CHARACTER PID                       
         GOTOR SETOHED                                                          
         MVC   P+1(L'AC@ANFO),AC@ANFO                                           
         MVC   P+38(L'SORTNMBR),SORTNMBR ORDER NUMBER                           
         MVI   P+44,C'('                                                        
         GOTO1 VHEXOUT,DMCB,APPRPID,P+45,2                                      
         MVI   P+49,C')'                                                        
         GOTO1 SQUASHER,DMCB,P,80                                               
         GOTO1 ACREPORT                                                         
*                                                                               
GET62    AHI   R5,L'PIDBIN                                                      
         L     RF,SAVERF                                                        
         JCT   RF,GET64                                                         
         TM    RUNIND1,RUNINOCP+RUNINOAP+RUNITERM  DON'T REPEAT ERRORS          
         JNZ   GET20                                                            
         GOTOR PUTOTAB             PUT DETAILS INTO EMAIL TABLE                 
         JE    GET20                                                            
         J     GETERR                                                           
*                                                                               
GET64    ST    RF,SAVERF                                                        
         TM    RUNIND1,RUNINOCP+RUNINOAP+RUNITERM  DON'T REPEAT ERRORS          
         JNZ   GET58                                                            
         GOTOR PUTOTAB             PUT DETAILS INTO EMAIL TABLE                 
         JE    GET58                                                            
         J     GETERR                                                           
*                                                                               
GET66    CLI   SORTMODL,SORTJOB    IF DOING JOBS                                
         JNE   GET76                                                            
         TM    RUNIND2,RUN2JOB     FIRST TIME?                                  
         JNZ   *+8                                                              
         OI    RUNIND2,RUN2JOB     SET GOT JOBS                                 
         GOTOR GETAPP              GET JOB APPROVER                             
         JE    GET68                                                            
         OI    RUNIND1,RUNINOAP    SET NO APPROVER                              
         GOTOR SETJHED                                                          
*                                                                               
         MVC   P+1(L'AC@ANFFA),AC@ANFFA                                         
         MVC   P+32(L'SORTCPJ),SORTCPJ  JOB CODE                                
         GOTO1 SQUASHER,DMCB,P,80                                               
         GOTO1 ACREPORT                                                         
         J     GET74                                                            
*                                                                               
GET68    L     R3,AJOBAPP          LIST OF JOB APPROVERS                        
         LA    R4,L'JOBAPP(R3)                                                  
*                                                                               
GET70    OC    0(L'APPRPID,R3),0(R3)                                            
         JZ    GET20                                                            
         CR    R3,R4                                                            
         JNL   GET20                                                            
         NI    RUNIND1,X'FF'-(RUNINOCP+RUNINOAP+RUNITERM)                       
         MVC   APPRPID,0(R3)                                                    
         GOTOR GETEML,0            VALIDATE APPROVER                            
         JE    GET72                                                            
         GOTOR SETJHED                                                          
         MVC   P+1(L'AC@ANFFA),AC@ANFFA                                         
         MVC   P+32(L'SORTCPJ),SORTCPJ                                          
         MVI   P+47,C'('                                                        
         GOTO1 VHEXOUT,DMCB,APPRPID,P+48,2                                      
         MVI   P+52,C')'                                                        
         GOTO1 SQUASHER,DMCB,P,80                                               
         GOTO1 ACREPORT                                                         
         OI    RUNIND1,RUNINOCP    NO VALID CHARACTER PID                       
*                                                                               
GET72    GOTOR PUTJTAB             PUT DETAILS INTO EMAIL TABLE                 
         LA    R3,L'APPRPID(R3)                                                 
         JE    GET70                                                            
         J     GETERR                                                           
*                                                                               
GET74    TM    RUNIND1,RUNINOCP+RUNINOAP+RUNITERM  DON'T REPEAT ERRORS          
         JNZ   GET20                                                            
         GOTOR PUTJTAB             PUT DETAILS INTO EMAIL TABLE                 
         JE    GET20                                                            
         J     GETERR                                                           
*                                                                               
GET76    CLI   SORTMODL,SORTEST    ESTIMATES?                                   
         JNE   GET82                                                            
         TM    RUNIND2,RUN2EST     FIRST TIME?                                  
         JNZ   *+8                                                              
         OI    RUNIND2,RUN2EST     SET GOT ESTIMATES                            
         MVC   APPRPID,SORTPID9                                                 
         OC    APPRPID,APPRPID     ANY APPROVER?                                
         JNZ   GET78                                                            
         OI    RUNIND1,RUNINOAP    SET NO APPROVER                              
         GOTOR SETSHED                                                          
         J     GET80                                                            
*                                                                               
GET78    GOTOR GETEML,0            VALIDATE APPROVER                            
         JE    GET80                                                            
         GOTOR SETSHED                                                          
         MVC   P+1(L'AC@ANFFA),AC@ANFE                                          
         MVC   P+39(L'SORTNMBR),SORTNMBR   ESTIMATE NUMBER                      
         MVI   P+45,C'('                                                        
         GOTO1 VHEXOUT,DMCB,SORTPID9,P+46,2                                     
         MVI   P+50,C')'                                                        
         GOTO1 SQUASHER,DMCB,P,80                                               
         OI    RUNIND1,RUNINOCP    NO VALID CHARACTER PID                       
*                                                                               
GET80    TM    RUNIND1,RUNINOCP+RUNINOAP+RUNITERM  DON'T REPEAT ERRORS          
         JNZ   GET20                                                            
         GOTOR PUTSTAB             PUT DETAILS INTO EMAIL TABLE                 
         JE    GET20                                                            
         J     GETERR                                                           
*                                                                               
GET82    CLI   SORTMODL,SORTINV    INVOICES?                                    
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    RUNIND2,RUN2INV     FIRST TIME?                                  
         JNZ   *+8                                                              
         OI    RUNIND2,RUN2INV                                                  
         LA    R5,SORTPIDS         FIRST PID                                    
         XR    RF,RF                                                            
         AHI   RF,SORTIL           MAX 8 PIDS FOR INVOICES                      
         ST    RF,SAVERF                                                        
*                                                                               
GET84    NI    RUNIND1,X'FF'-(RUNINOCP+RUNINOAP+RUNITERM)                       
         MVC   APPRPID,0(R5)       GET INVOICE APPROVER                         
         OC    APPRPID,APPRPID                                                  
         JNZ   GET86                                                            
         OI    RUNIND1,RUNINOAP    SET NO APPROVER                              
         J     GET88               HIT END OF APPROVERS GO TO NEXT              
*                                                                               
GET86    GOTOR GETEML,0            VALIDATE APPROVER                            
         JE    GET88                                                            
         GOTOR SETIHED                                                          
         MVC   P+1(L'AC@NFAI),AC@NFAI    'APPROVER NOT FOUND FOR A/C'           
         MVC   P+39(L'SORTINTR),SORTINTR                                        
         GOTO1 SQUASHER,DMCB,P,80                                               
         GOTO1 ACREPORT                                                         
         OI    RUNIND1,RUNINOCP    NO VALID CHARACTER PID                       
*                                                                               
GET88    AHI   R5,L'PIDBIN                                                      
         L     RF,SAVERF                                                        
         JCT   RF,GET90                                                         
         TM    RUNIND1,RUNINOCP+RUNINOAP+RUNITERM  DON'T REPEAT ERRORS          
         JNZ   GET20                                                            
         GOTOR PUTITAB             ELSE PUT DETAILS INTO EMAIL TABLE            
         JE    GET20               GO TO NEXT SORTED RECORD                     
         J     GETERR                                                           
*                                                                               
GET90    ST    RF,SAVERF                                                        
         TM    RUNIND1,RUNINOCP+RUNINOAP+RUNITERM  DON'T REPEAT ERRORS          
         JNZ   GET20                                                            
         GOTOR PUTITAB             ELSE PUT DETAILS INTO EMAIL TABLE            
         JE    GET84               GO TO NEXT SORTED RECORD                     
         J     GETERR                                                           
*                                                                               
GETERR   DS    0H                                                               
         MVI   SORTSW,0                                                         
         CLI   RCWRITE,NOQ                                                      
         JE    GETERR2                                                          
         GOTO1 VSMTP,DMCB,('SMTPAEND',0) DETACH FROM JESMAIL                    
*                                                                               
GETERR2  MVI   P,C' '              ERROR IF EMAIL BUFFER IS FULL                
         MVC   P+1(L'P-1),P                                                     
         MVI   PSECOND,C' '                                                     
         MVC   PSECOND+1(L'P-1),PSECOND                                         
         MVC   P+1(L'BUFFERR1),BUFFERR1                                         
         MVC   PSECOND+1(L'BUFFERR2),BUFFERR2                                   
         GOTO1 VHEXOUT,DMCB,COCODE,P+2+L'BUFFERR2,1                             
         GOTO1 ACREPORT                                                         
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATION MODULE FOR APPROVER                                      *         
* EXIT CC EQUAL IF APPROVER PID FOUND                                           
* EXIT CC LOW IF APPROVER PID NOT FOUND                                         
***********************************************************************         
         SPACE 1                                                                
GETAPP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETAPP*'                                                      
         XC    APPRPID,APPRPID                                                  
         NI    RUNIND1,X'FF'-(RUNIDEFT+RUNILVL+RUNIGLVL)                        
         NI    RUNIND2,X'FF'-RUN2DEFT                                           
         L     R3,AJOBAPP                NON-DEFAULT JOB APPROVERS              
         XC    0(L'JOBAPP,R3),0(R3)                                             
*                                                                               
GETAPP00 XC    RUNIND5,RUNIND5                                                  
         CLC   SORTUL,SJUL                                                      
         JNE   GETAPP32                                                         
         CLC   SORTCPJ,SPACES      NO SJ ACCOUNT SO NO APPROVER                 
         JNH   GETAPOK                                                          
         CLI   SORTMODL,SORTTIME   ONLY FOR TIME                                
         JNE   GETAPP02                                                         
         CLC   SORT1R,SPACES                                                    
         JNH   GETAPP02                                                         
         MVC   ONERCODE,SORT1R                                                  
         GOTOR CSTPRF              IF 1R ACCOUNT READ COST PROFILES             
*                                                                               
         L     RF,ACOBLOCK                                                      
         CLI   COCLA-COBLOCK(RF),C'N' CHECK CLIENT LEVEL APPROVAL               
         JE    GETAPOK             IF NOT SKIP                                  
*                                                                               
GETAPP02 XC    IOKEY3,IOKEY3                                                    
         LA    R2,APRTAB                                                        
         USING APRTABD,R2                                                       
SJ       USING JOBPASD,IOKEY3                                                   
GETAPP04 XC    SJ.JOBPAS,SJ.JOBPAS       BUILD KEY FOR JOB CODE                 
         MVI   SJ.JOBPTYP,JOBPTYPQ                                              
         MVI   SJ.JOBPSUB,JOBPSUBQ                                              
         MVC   SJ.JOBPCPY,COCODE                                                
         MVI   SJ.JOBPVIEW,JOBPVOFF                                             
         MVC   SJ.JOBPCOFF,SPACES                                               
         MVC   SJ.JOBPCMED,SPACES                                               
         MVC   SJ.JOBPCPJ,SPACES                                                
         OC    APRSTAT,APRSTAT                                                  
         JNZ   GETAPP06                                                         
         MVI   SJ.JOBPCODE,X'FF'                                                
         MVC   SJ.JOBPCODE+1(L'JOBPCODE-1),SJ.JOBPCODE                          
         CLI   SORTMODL,SORTTIME                                                
         JNE   *+12                                                             
         MVI   SJ.JOBPAPPL,JOBPATIM   TIME?                                     
         J     GETAPP16                                                         
                                                                                
         CLI   SORTMODL,SORTEXP       EXPENSES?                                 
         JNE   *+12                                                             
         MVI   SJ.JOBPAPPL,JOBPAEXP                                             
         J     GETAPP16                                                         
                                                                                
         CLI   SORTMODL,SORTJOB    JOBS?                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   SJ.JOBPAPPL,JOBPAJOB                                             
         J     GETAPP16                                                         
*                                                                               
GETAPP06 CLI   SORTMODL,SORTTIME                                                
         JNE   *+12                                                             
         MVI   SJ.JOBPAPPL,JOBPATIM   TIME?                                     
         J     GETAPP08                                                         
*                                                                               
         CLI   SORTMODL,SORTEXP       EXPENSES?                                 
         JNE   *+12                                                             
         MVI   SJ.JOBPAPPL,JOBPAEXP                                             
         J     GETAPP08                                                         
*                                                                               
         CLI   SORTMODL,SORTJOB    JOBS?                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   SJ.JOBPAPPL,JOBPAJOB                                             
         J     GETAPP08                                                         
*                                                                               
GETAPP08 TM    APRSTAT,APRJOB      JOB LEVEL                                    
         JZ    GETAPP09                                                         
         LLC   RF,PPROLEN                                                       
         LA    RF,SORTCPJ(RF)                                                   
         CLI   0(RF),X'40'         Have we got a job                            
         JNH   GETAPP22            No                                           
         LLC   RF,JOBLEN                                                        
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),SORTCPJ                                            
         EX    RF,0(RE)                                                         
         J     GETAPP12                                                         
*                                                                               
GETAPP09 TM    APRSTAT,APRPRO      PRODUCT LEVEL                                
         JZ    GETAPP10                                                         
         LLC   RF,PCLILEN                                                       
         LA    RF,SORTCPJ(RF)                                                   
         CLI   0(RF),X'40'         Have we got a product                        
         JNH   GETAPP22            No                                           
         LLC   RF,PPROLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),SORTCPJ                                            
         EX    RF,0(RE)                                                         
         J     GETAPP12                                                         
*                                                                               
GETAPP10 TM    APRSTAT,APRCLI      CLIENT LEVEL                                 
         JZ    GETAPP12                                                         
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   SJ.JOBPCPJ(0),SORTCPJ                                            
         EX    RF,0(RE)                                                         
                                                                                
GETAPP12 TM    APRSTAT,APRMED      MEDIA LEVEL                                  
         JZ    GETAPP14                                                         
         CLC   SORTMED,SPACES                                                   
         JNH   GETAPP22                                                         
         MVC   SJ.JOBPCMED,SORTMED                                              
         GOTOR SETLVL,DMCB,SORTMEDL,SJ.JOBPAPPL                                 
                                                                                
GETAPP14 TM    APRSTAT,APROFF      OFFICE LEVEL                                 
         JZ    GETAPP16                                                         
         CLC   SORTOFF,SPACES                                                   
         JNH   GETAPP22                                                         
         MVC   SJ.JOBPCOFF,SORTOFF                                              
         GOTOR SETLVL,DMCB,SORTCLIL,SJ.JOBPAPPL                                 
                                                                                
GETAPP16 MVC   CSVKEY3,IOKEY3                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,SJ.JOBPAS,SJ.JOBPAS,0                 
         J     GETAPP20                                                         
                                                                                
GETAPP18 GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,SJ.JOBPAS,SJ.JOBPAS,0                 
GETAPP20 CLC   SJ.JOBPAS(JOBPPIDB-JOBPASD),CSVKEY3                              
         JE    GETAPP24                                                         
GETAPP2A CLI   SENDNDF,NOQ           SENDING TO NON-DEFAULT?                    
         JNE   GETAPP21                                                         
GETAPP2B TM    RUNIND1,RUNIGLVL      HAVE WE GOT SOME APPRVS. ALREADY?          
         JNZ   GETAPP22              GREAT OK TO CONTINUE                       
         TM    RUNIND1,RUNILVL       HAVE WE FOUND SOME APPRVS. AT LVL?         
         JNZ   GETAPPN               YES THEN WE'VE GOT A PROBLEM               
         J     GETAPP22                                                         
*                                                                               
GETAPP21 TM    RUNIND1,RUNIDEFT      SENDING TO NON-DEFAULT SO DOESN'T          
         JZ    GETAPP22              MATTER IF WE DON'T FIND ANYTHING           
         J     GETAPP2B              FOR DEFAULT BUT CHECK FOR NON-DEF          
*                                                                               
GETAPP22 LA    R2,APRTABL(R2)                                                   
         CLI   0(R2),X'FF'                                                      
         JNE   GETAPP04                                                         
         CLI   SORTMODL,SORTJOB      JOBS?                                      
         JNE   GETAPPN                                                          
         TM    RUNIND1,RUNIDEFT      BEEN THROUGH TWICE?                        
         JNZ   GETAPPY                                                          
         OI    RUNIND1,RUNIDEFT      BEEN THROUGH ONCE                          
         CLI   SENDNDF,NOQ           DO WE SEND TO NON-DEFAULT AS WELL?         
         JE    GETAPPY                                                          
         LA    R2,APRTAB             NOW LOOK FOR NON-DEFAULT                   
         J     GETAPP02                                                         
                                                                                
GETAPP24 OI    RUNIND1,RUNILVL       FOUND APPROVERS AT THIS LEVEL              
         CLI   SORTMODL,SORTJOB      JOBS?                                      
         JNE   GETAPP30                                                         
         TM    RUNIND1,RUNIDEFT      LOOKING FOR DEFAULT ONLY                   
         JZ    GETAPP26                                                         
         TM    SJ.JOBPSTAT,JOBPDFLT  NON-DEFAULT                                
         JNZ   GETAPP18                                                         
         J     GETAPP28                                                         
*                                                                               
GETAPP26 TM    SJ.JOBPSTAT,JOBPDFLT  DEFAULT                                    
         JZ    GETAPP18                                                         
         OI    RUNIND2,RUN2DEFT                                                 
*                                                                               
GETAPP28 TM    RUNIND1,RUNIGLVL                                                 
         JZ    GETAPP29                                                         
         CLC   APRLVL,RUNIND5        SAME LEVEL APPROVER?                       
         JNE   GETAPP18              IF NOT THEN DON'T WANT                     
*                                                                               
GETAPP29 MVC   0(L'APPRPID,R3),SJ.JOBPPIDB      SAVE OF PID                     
         MVC   RUNIND5,APRLVL                                                   
         OI    RUNIND1,RUNIGLVL       GOT AN APPROVER AT THIS LEVEL             
         LA    R3,L'APPRPID(R3)                                                 
         J     GETAPP18                                                         
*                                                                               
GETAPP30 MVC   APPRPID,SJ.JOBPPIDB    ELSE SAVE BINARY PID                      
         J     GETAPPY                                                          
*                                                                               
GETAPP32 CLC   SORTUL,ONENUL          1N ACCOUNT APPROVAL                       
         JNE   GETAPP38                                                         
         CLC   SORT1N,SPACES          1N ACCOUNT                                
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETAPP34 LA    R3,IOKEY3                                                        
         XC    IOKEY3,IOKEY3                                                    
         USING NCTPASD,R3                                                       
         XC    NCTPAS,NCTPAS                                                    
         MVI   NCTPTYP,NCTPTYPQ                                                 
         MVI   NCTPSUB,NCTPSUBQ                                                 
         MVC   NCTPCPY,COCODE                                                   
         MVI   NCTPAPPL,NCTPATIM                                                
         MVC   NCTPNCC,SORT1N                                                   
         MVC   CSVKEY3,NCTPAS                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,NCTPAS,NCTPAS,0                       
         JE    GETAPP36                                                         
         DC    H'0'                                                             
*                                                                               
GETAPP36 CLC   NCTPAS(NCTPPIDB-NCTPASD),CSVKEY3                                 
         JNE   GETAPPN             NO VALID APPROVER FOUND                      
         MVC   APPRPID,NCTPPIDB                                                 
         J     GETAPPY                                                          
         DROP  R3                                                               
*                                                                               
GETAPP38 CLC   SORTUL,ONERUL                                                    
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   SORT1R,SPACES                                                    
         JH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETAPP40 XC    IOKEY3,IOKEY3                                                    
         LA    R2,IOKEY3                                                        
         USING DPAPASD,R2                                                       
         XC    DPAPAS,DPAPAS       BUILD DPAPASD KEY                            
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,COCODE                                                   
*                                                                               
         CLI   SORTMODL,SORTTIME                                                
         JNE   *+12                                                             
         MVI   DPAPAPPL,DPAPATIM                                                
         J     GETAPP42                                                         
*                                                                               
         CLI   SORTMODL,SORTEXP                                                 
         JNE   GETAPPN             INVALID TYPE                                 
         MVI   DPAPAPPL,DPAPAEXP                                                
         J     GETAPP42                                                         
*                                                                               
GETAPP42 MVC   DPAP1RAC,SORT1R                                                  
         ZAP   DPAPXVAL,PZERO                                                   
         MVC   CSVKEY3,DPAPAS                                                   
         CLI   SORTMODL,SORTEXP    ONLY FOR EXPENSES                            
         JNE   GETAPP44                                                         
         CLI   SORT1RL,C'1'        GET LEVEL CORRECT                            
         JNE   *+8                                                              
         MVI   DPAPAPPL,DPAPAEX1                                                
         CLI   SORT1RL,C'2'                                                     
         JNE   *+8                                                              
         MVI   DPAPAPPL,DPAPAEX2                                                
*                                                                               
GETAPP44 GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS,0                       
         JE    GETAPP46                                                         
         DC    H'0'                                                             
*                                                                               
GETAPP46 CLC   DPAPAS(DPAPPIDB-DPAPASD),CSVKEY3                                 
         JNE   GETAPP48                                                         
         MVC   APPRPID,DPAPPIDB                                                 
         J     GETAPPY                                                          
*                                                                               
GETAPP48 XC    DPAPAS,DPAPAS                                                    
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,COCODE                                                   
         MVC   DPAPAPPL,CSVKEY3+DPAPAPPL-DPAPAS                                 
         LLC   RE,ONERL3L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   DPAP1RAC(0),SORT1R                                               
         EX    RE,0(R1)                                                         
         OC    DPAP1RAC,SPACES                                                  
         ZAP   DPAPXVAL,PZERO                                                   
         MVC   CSVKEY3,DPAPAS                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS,0                       
         JE    GETAPP50                                                         
         DC    H'0'                                                             
*                                                                               
GETAPP50 CLC   DPAPAS(DPAPPIDB-DPAPASD),CSVKEY3                                 
         JNE   GETAPP52                                                         
         MVC   APPRPID,DPAPPIDB                                                 
         J     GETAPPY                                                          
*                                                                               
GETAPP52 XC    DPAPAS,DPAPAS                                                    
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,COCODE                                                   
         MVC   DPAPAPPL,CSVKEY3+DPAPAPPL-DPAPAS                                 
         LLC   RE,ONERL2L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   DPAP1RAC(0),SORT1R                                               
         EX    RE,0(R1)                                                         
         OC    DPAP1RAC,SPACES                                                  
         ZAP   DPAPXVAL,PZERO                                                   
         MVC   CSVKEY3,DPAPAS                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS,0                       
         JE    GETAPP54                                                         
         DC    H'0'                                                             
*                                                                               
GETAPP54 CLC   DPAPAS(DPAPPIDB-DPAPASD),CSVKEY3                                 
         JNE   GETAPP56                                                         
         MVC   APPRPID,DPAPPIDB                                                 
         J     GETAPPY                                                          
*                                                                               
GETAPP56 XC    DPAPAS,DPAPAS                                                    
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,COCODE                                                   
         MVC   DPAPAPPL,CSVKEY3+DPAPAPPL-DPAPAS                                 
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   DPAP1RAC(0),SORT1R                                               
         EX    RE,0(R1)                                                         
         OC    DPAP1RAC,SPACES                                                  
         ZAP   DPAPXVAL,PZERO                                                   
         MVC   CSVKEY3,DPAPAS                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS,0                       
         JE    GETAPP58                                                         
         DC    H'0'                                                             
*                                                                               
GETAPP58 CLC   DPAPAS(DPAPPIDB-DPAPASD),CSVKEY3                                 
         JNE   GETAPPN                                                          
         MVC   APPRPID,DPAPPIDB                                                 
*                                                                               
GETAPPY  J     EXITE                                                            
*                                                                               
GETAPPN  J     EXITL                 APPROVER PID NOT FOUND                     
*                                                                               
GETAPOK  J     EXITH                 EXIT HIGH FOR APPROVER NOT NEEDED          
*                                                                               
         EJECT                                                                  
         DROP  R2,SJ                                                            
***********************************************************************         
* Get Approvers 1R code                                               *         
***********************************************************************         
         SPACE 1                                                                
GETAP1R  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETA1R*'                                                      
         MVC   ONERCODE,SPACES                                                  
*                                                                               
         USING PIDRECD,R4                                                       
         LA    R4,IOKEY3                                                        
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,COCODE                                                   
         MVC   PIDKPID,APPRPID                                                  
         MVI   PIDKSTYP,PIDKPERQ                                                
         MVC   CSVKEY3,PIDKEY                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,PIDKEY,PIDKEY,0                       
         JNE   GETA1RN                                                          
         CLC   CSVKEY3(PIDKPER-PIDKEY),IOKEY3                                   
         JNE   GETA1RN                                                          
         MVC   SVDA,PIDKDA         Save off Disk Address for PUTREC             
*                                                                               
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,SVDA,AIOAREA2,DMWORK                   
         JE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
*                                                                               
         USING PERRECD,R4                                                       
         L     R4,AIOAREA2                                                      
         LA    R5,PERRFST                                                       
         USING LOCELD,R5                                                        
GETA1R10 CLI   LOCEL,0                                                          
         JE    GETA1RN                                                          
         CLI   LOCEL,LOCELQ                                                     
         JE    GETA1R30                                                         
GETA1R20 XR    R1,R1                                                            
         IC    R1,LOCLN                                                         
         AR    R5,R1                                                            
         J     GETA1R10                                                         
*                                                                               
GETA1R30 CLC   LOCSTART,TODAYP     Find current location                        
         JH    GETA1R20                                                         
         OC    LOCEND,LOCEND                                                    
         JZ    *+14                                                             
         CLC   LOCEND,TODAYP                                                    
         JL    GETA1R20                                                         
         LA    RF,ONERCODE         Fill out APProvers 1R account                
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),LOCOFF      Move in Office                               
         AHI   RE,1                                                             
         AR    RF,RE                                                            
         LLC   R1,ONERL1L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RE,R1                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),LOCDEPT     Move in Department                           
         AHI   RE,1                                                             
         AR    RF,RE                                                            
         LLC   R1,ONERL2L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RE,R1                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),LOCSUB      Move in Sub-Department                       
         AHI   RE,1                                                             
         AR    RF,RE                                                            
         LLC   R1,ONERL3L                                                       
         LLC   RE,ONERL4L                                                       
         SR    RE,R1                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),PERKCODE    Move in person                               
         DROP  R4,R5                                                            
*                                                                               
GETA1RY  J     EXITE               Account found                                
*                                                                               
GETA1RN  J     EXITL               Account not found                            
         EJECT                                                                  
***********************************************************************         
* GET FINANCE APPROVER                                                *         
***********************************************************************         
         SPACE 1                                                                
GETFIN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETFIN*'                                                      
         LA    R4,IOKEY2                                                        
         NI    RUNIND1,X'FF'-RUNIDEFT                                           
*                                                                               
         USING DPAPASD,IOKEY2      READ AT PERSON LEVEL                         
         XC    DPAPAS,DPAPAS                                                    
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,COCODE                                                   
         MVI   DPAPAPPL,DPAPAEXF                                                
         ZAP   DPAPXVAL,PZERO                                                   
         MVC   DPAP1RAC,SORT1R     1R ACCOUNT CODE                              
         OC    DPAP1RAC,SPACES                                                  
         MVC   CSVKEY2,DPAPAS                                                   
*                                                                               
GETFIN2  GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS                         
         JE    GETFIN4                                                          
         DC    H'0'                                                             
*                                                                               
GETFIN4  CLC   DPAPAS(DPAPPIDB-DPAPASD),CSVKEY2                                 
         JNE   GETFIN8             LOOK AT NEXT LEVEL                           
GETFIN6  MVC   APPRPID,DPAPPIDB    SAVE PID                                     
         J     EXITE               FOUND IT AT DEPARTMENT                       
*                                                                               
GETFIN8  XC    DPAPAS,DPAPAS       LOOK AT SUB DEPARTMENT                       
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,COCODE                                                   
         MVC   DPAPAPPL,CSVKEY2+DPAPAPPL-DPAPAS                                 
         LLC   RE,ONERL3L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   DPAP1RAC(0),SORT1R                                               
         EX    RE,0(R1)                                                         
         OC    DPAP1RAC,SPACES                                                  
         ZAP   DPAPXVAL,PZERO                                                   
         MVC   CSVKEY2,DPAPAS                                                   
*                                                                               
GETFIN10 GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS                         
         JE    GETFIN12                                                         
         DC    H'0'                                                             
*                                                                               
GETFIN12 CLC   DPAPAS(DPAPPIDB-DPAPASD),CSVKEY2                                 
         JNE   GETFIN14                                                         
         J     GETFIN6                                                          
*                                                                               
GETFIN14 XC    DPAPAS,DPAPAS       NOW LOOK AT DEPARTMENT                       
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,COCODE                                                   
         MVC   DPAPAPPL,CSVKEY2+DPAPAPPL-DPAPAS                                 
         LLC   RE,ONERL2L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   DPAP1RAC(0),SORT1R                                               
         EX    RE,0(R1)                                                         
         OC    DPAP1RAC,SPACES                                                  
         ZAP   DPAPXVAL,PZERO                                                   
         MVC   CSVKEY2,DPAPAS                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS,0                       
         JE    GETFIN16                                                         
         DC    H'0'                                                             
*                                                                               
GETFIN16 CLC   DPAPAS(DPAPPIDB-DPAPASD),CSVKEY2                                 
         JNE   GETFIN18                                                         
         J     GETFIN6                                                          
*                                                                               
GETFIN18 XC    DPAPAS,DPAPAS       LOOK AT OFFICE LEVEL                         
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,COCODE                                                   
         MVC   DPAPAPPL,CSVKEY2+DPAPAPPL-DPAPAS                                 
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   DPAP1RAC(0),SORT1R                                               
         EX    RE,0(R1)                                                         
         OC    DPAP1RAC,SPACES                                                  
         ZAP   DPAPXVAL,PZERO                                                   
         MVC   CSVKEY2,DPAPAS                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS,0                       
         JE    GETFIN20                                                         
         DC    H'0'                                                             
*                                                                               
GETFIN20 CLC   DPAPAS(DPAPPIDB-DPAPASD),CSVKEY2                                 
         JNE   GETFIN22                                                         
         J     GETFIN6                                                          
*                                                                               
GETFIN22 XC    DPAPAS,DPAPAS       LOOK FOR HIGHEST LEVEL AGENCY                
         MVI   DPAPTYP,DPAPTYPQ    APPROVER                                     
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,COCODE                                                   
         MVC   DPAPAPPL,CSVKEY2+DPAPAPPL-DPAPAS                                 
         MVI   DPAP1RAC,X'FF'                                                   
         MVC   DPAP1RAC+1(L'DPAP1RAC-1),DPAP1RAC                                
         ZAP   DPAPXVAL,PZERO                                                   
         MVC   CSVKEY2,DPAPAS                                                   
*                                                                               
GETFIN24 GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS,0                       
         JE    GETFIN26                                                         
         DC    H'0'                                                             
*                                                                               
GETFIN26 CLC   DPAPAS(DPAPPIDB-DPAPASD),CSVKEY2                                 
         JNE   GETFINN                                                          
         J     GETFIN6                                                          
*                                                                               
GETFINN  J     EXITL                                                            
*                                                                               
GETFINY  J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
*  GET SECURITY ALPHA CODE                                            *         
***********************************************************************         
         SPACE 1                                                                
GSECALP  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**GSEC**'                                                      
         MVC   SECCODE,ALPCODE                                                  
         XC    IOKEY2,IOKEY2                                                    
         LA    R5,IOKEY2                                                        
         USING CT5REC,R5           SYSTEM ACCESS RECORD                         
         MVI   CT5KTYP,CT5KTYPQ    C'5'                                         
         MVC   CT5KALPH,SECCODE                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY2,AIOAREA2                       
         L     R5,AIOAREA2                                                      
         CLC   IOKEY2(L'CT5KEY),0(R5)                                           
         JNE   GSECALPN                                                         
*                                                                               
         LA    R5,CT5DATA                                                       
         USING CTSEAD,R5                                                        
         XR    R0,R0                                                            
GSALP02  CLI   CTSEAEL,0           TEST EOR                                     
         JE    GSECALPX                                                         
         CLI   CTSEAEL,CTSEAELQ    X'B8'                                        
         JE    GSALP08                                                          
         CLI   CTSEAEL,CTAGDELQ                                                 
         JE    GSALP10                                                          
GSALP06  IC    R0,CTSEALEN                                                      
         AR    R5,R0                                                            
         J     GSALP02                                                          
                                                                                
GSALP08  MVC   SECCODE,CTSEAAID    AGENCY ALPHA ID                              
         J     GSALP06                                                          
*                                                                               
         USING CTAGDD,R5                                                        
GSALP10  MVC   COMPCTRY,CTRY       DEFAULT IS DDS COUNTRY CODE                  
         CLI   CTAGDLEN,CTAGDCTY-CTAGDD                                         
         JL    GSALP06                                                          
         MVC   COMPCTRY,CTAGDCTY                                                
         L     R1,ADMASTC                                                       
         USING MASTD,R1                                                         
         MVC   MCCTRY,COMPCTRY                                                  
         MVC   RCCTRY,COMPCTRY                                                  
         MVC   CTRY,COMPCTRY                                                    
         J     GSALP06                                                          
         DROP  R1                                                               
                                                                                
GSECALPX J     EXITE                                                            
GSECALPN J     EXITL                                                            
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET LEDGER SJ LENGTHS                                               *         
***********************************************************************         
         SPACE 2                                                                
GLDGSJ   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GLDGSJ*'                                                      
         XC    IOKEY3,IOKEY3                                                    
         LA    R4,IOKEY3                                                        
         USING LDGRECD,R4          READ SJ LEDGER TO GET CLI,PRO,JOB            
         MVC   LDGKEY,SPACES       LENGTHS                                      
         MVC   LDGKCPY,COCODE                                                   
         MVC   LDGKUNT(L'SORTUL),SJUL                                           
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,LDGKEY,LDGKEY,0                       
         JNE   GLDGSJX                                                          
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,LDGKDA,AIOAREA2,DMWORK                 
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(HELLO),DMCB,(C'G',ACCMST),('ACLELQ',AIOAREA2),0,0             
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R5,12(R1)                                                        
         USING ACLELD,R5                                                        
         MVC   PCLILEN(L'PCLILEN),ACLELLVA                                      
         MVC   PPROLEN(L'PPROLEN),ACLELLVB                                      
         MVC   JOBLEN(L'JOBLEN),ACLELLVC                                        
         DROP  R4,R5                                                            
*                                                                               
GLDGSJX  J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* GET 1R LEDGER LENGTHS                                               *         
***********************************************************************         
         SPACE 1                                                                
GLDG1R   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GLDG1R*'                                                      
         XC    IOKEY3,IOKEY3                                                    
         LA    R4,IOKEY3                                                        
         USING LDGRECD,R4          READ 1N LEDGER TO GET LENGTHS                
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,COCODE                                                   
         MVC   LDGKUNT(L'SORTUL),ONERUL                                         
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,LDGKEY,LDGKEY,0                       
         JNE   GLDG1RX                                                          
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,LDGKDA,AIOAREA2,DMWORK                 
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(HELLO),DMCB,(C'G',ACCMST),('ACLELQ',AIOAREA2),0,0             
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R5,12(R1)                                                        
         USING ACLELD,R5                                                        
         MVC   ONERL1L(L'ONERL1L),ACLELLVA                                      
         MVC   ONERL2L(L'ONERL2L),ACLELLVB                                      
         MVC   ONERL3L(L'ONERL3L),ACLELLVC                                      
         MVC   ONERL4L(L'ONERL4L),ACLELLVD                                      
         DROP  R4,R5                                                            
GLDG1RX  J     EXITE                                                            
         EJECT                                                                  
**********************************************************************          
* GET PERSON DETAILS OF TIMESHEET/EXPENSE CLAIM                      *          
* ON NTRY P1=A(EMRECD)                                               *          
**********************************************************************          
         SPACE 1                                                                
         USING EMRECD,R2                                                        
GETPERS  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETPER*'                                                      
         L     R2,0(R1)                                                         
         LA    R3,IOKEY2           GET & PRINT NAME OF APPROVEE                 
         USING PERRECD,R3                                                       
         MVC   PERFSTNM,SPACES                                                  
         MVC   PERMIDNM,SPACES                                                  
         MVC   PERLSTNM,SPACES                                                  
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F'                                        
         MVC   PERKCPY,COCODE      COMPANY CODE                                 
         MVC   PERKCODE(L'EMPERC),EMPERC PERSON CODE                            
         MVC   SVPCODE(L'EMPERC),EMPERC                                         
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,PERKEY,PERKEY,0                       
         JNE   *+2                 CANNOT FIND PERSON RECORD                    
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,PERKDA,AIOAREA,DMWORK                  
         JNE   *+2                                                              
         L     R3,AIOAREA                                                       
*                                                                               
         LA    R5,PERRFST          DISPLACEMENT TO FIRST ELEMENT                
         USING GPNELD,R5                                                        
         SR    R0,R0                                                            
GPERS02  CLI   GPNEL,0             TEST EOR                                     
         JE    GETPERSX                                                         
         CLI   GPNEL,GPNELQ        X'5A'                                        
         JE    GPERS06                                                          
GPERS04  IC    R0,GPNLN                                                         
         AR    R5,R0                                                            
         J     GPERS02                                                          
*                                                                               
GPERS06  XR    R4,R4                                                            
         IC    R4,GPNLN                                                         
         SHI   R4,GPNLNQ+1                                                      
*                                                                               
         CLI   GPNTYP,GPNTLST      ARE WE AT LAST NAME ELEMENT?                 
         JNE   GPERS08             NO                                           
         BASR  RE,0                                                             
         MVC   PERLSTNM(0),GPNNME                                               
         EX    R4,0(RE)                                                         
         J     GPERS04                                                          
*                                                                               
GPERS08  CLI   GPNTYP,GPNTFST      ARE WE AT FIRST NAME ELEMENT?                
         JNE   GPERS04             NO                                           
         BASR  RE,0                                                             
         MVC   PERFSTNM(0),GPNNME  SAVE FIRST NAME                              
         EX    R4,0(RE)                                                         
         J     GPERS04                                                          
*                                                                               
GETPERSX J     EXITE                                                            
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
***********************************************************************         
* EXTRACT OFFICE AND OFFICE DEPT SUB-DEPT                             *         
***********************************************************************         
         SPACE 1                                                                
EXTOFC   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**EXTO**'                                                      
         L     R3,0(R1)                                                         
         MVC   CURDPT,SPACES                                                    
         MVC   CURODS,SPACES                                                    
         MVC   CURSDP,SPACES                                                    
         LLC   RF,ONERL3L                                                       
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         MVC   CURODS(0),0(R3)     EXTRACT CURRENT OFF/DEPT/SUB-DEPT            
         EX    RF,0(R1)                                                         
         LLC   RF,ONERL1L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RE,RF                                                            
         AR    RF,R3                                                            
         BCTR  RE,0                                                             
         BASR  R1,0                                                             
         MVC   CURDPT(0),0(RF)     EXTRACT CURRENT DEPT                         
         EX    RE,0(R1)                                                         
         LLC   RF,ONERL2L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RE,RF                                                            
         AR    RF,R3                                                            
         BCTR  RE,0                                                             
         BASR  R1,0                                                             
         MVC   CURSDP(0),0(RF)     EXTRACT CURRENT SUB DEPT                     
         EX    RE,0(R1)                                                         
         LLC   RF,ONERL1L                                                       
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         CLC   CUROFFC(0),0(R3)    COMPARE TO PREVIOUS OFFICE                   
         EX    RF,0(R1)                                                         
         JE    EXITH               SAME AS BEFORE                               
         BASR  R1,0                                                             
         MVC   CUROFFC(0),0(R3)    DIFFERENT - EXTRACT IT                       
         EX    RF,0(R1)                                                         
         OC    CUROFFC,SPACES                                                   
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE/RETURN EMAIL ADDRESS OF APPROVER                           *         
* NTRY R1=0 VALIDATE APPROVER/RETURN EMAIL ADDRESS                    *         
*      R1=1 RETURN EMAIL ADDRESS + NAMES                              *         
* EXIT CC EQUAL IF EMAIL ADDRESS FOUND                                *         
* EXIT CC LOW IF EMAIL ADDRESS NOT FOUND                              *         
***********************************************************************         
         SPACE 1                                                                
GETEML   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETEML*'                                                      
         STC   R1,BYTE                                                          
         XC    APPEMAIL,APPEMAIL                                                
         MVC   APPFSTNM,SPACES                                                  
         MVC   APPMIDNM,SPACES                                                  
         MVC   APPLSTNM,SPACES                                                  
         XC    IOKEY2,IOKEY2                                                    
         LA    R3,IOKEY2                                                        
         USING SA0REC,R3                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SECCODE     AGENCY ALPHA CODE                            
         MVC   SA0KNUM,APPRPID     APPROVER BINARY PID                          
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY2,AIOAREA                        
         L     R3,AIOAREA                                                       
         CLC   IOKEY2(L'SA0KEY),0(R3)                                           
         JNE   EXITL                                                            
GETEM11  TM    SA0STAT,X'20'       LOCKED                                       
         JO    EXITL                                                            
*                                                                               
GETEM12  LA    R3,SA0DATA                                                       
         USING SAPALD,R3                                                        
         XR    R0,R0                                                            
GETEM13  CLI   SAPALEL,0           TEST EOR                                     
         JNE   GETEM14                                                          
         DC    H'0'                                                             
GETEM14  CLI   SAPALEL,SAPALELQ                                                 
         JE    *+14                                                             
         IC    R0,SAPALLN                                                       
         AR    R3,R0                                                            
         J     GETEM13                                                          
         MVC   PIDCHAR,SAPALPID                                                 
*                                                                               
GETEM16  XC    IOKEY2,IOKEY2                                                    
         USING SAPEREC,R3                                                       
         LA    R3,IOKEY2                                                        
         MVI   SAPETYP,SAPETYPQ    C'F' - SECURITY PERSON REC                   
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SECCODE     SECURITY ALPHA ID                            
         MVC   SAPEPID,PIDCHAR                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY2,AIOAREA                        
         L     R3,AIOAREA                                                       
         CLC   IOKEY2(SAPEDEF-SAPEKEY),0(R3)                                    
         JNE   GETEMN                                                           
*                                                                               
         LA    R5,SAPEDATA                                                      
         USING SAPEED,R5                                                        
         XR    R0,R0                                                            
GETEM18  CLI   SAPEEEL,0           TEST EOR                                     
         JE    GETEMY              IF EMAIL ADDRESS NF DON'T WORRY              
         CLI   SAPEEEL,SAPEEELQ    X'E5' - PERSON EMAIL ELEM                    
         JE    GETEM22                                                          
         CLI   SAPEEEL,SANAMELQ    X'C5'                                        
         JE    GETEM32                                                          
         CLI   SAPEEEL,SAPERELQ    X'C6' - PERSONNEL DETAILS                    
         JE    GETEM38                                                          
GETEM20  IC    R0,SAPEELN                                                       
         AR    R5,R0                                                            
         J     GETEM18                                                          
*                                                                               
GETEM22  IC    RF,SAPEELN                                                       
         SHI   RF,SAPEELNQ+1                                                    
         BASR  RE,0                                                             
         MVC   APPEMAIL(0),SAPEEID SAVE EMAIL ADDRESS                           
         EX    RF,0(RE)                                                         
         GOTOR =V(EMLCHK),DMCB,(L'APPEMAIL,APPEMAIL)  validate                  
         JNE   GETEM25             NOTHING LEFT                                 
         XC    APPEMAIL,APPEMAIL   USE extracted email(s)                       
         LLC   RF,DMCB                                                          
         SHI   RF,1                                                             
         L     R1,DMCB                                                          
         BASR  RE,0                                                             
         MVC   APPEMAIL(0),0(R1)                                                
         EX    RF,0(RE)                                                         
         J     GETEM20                                                          
         DROP  R5                                                               
*                                                                               
GETEM25  XC    APPEMAIL,APPEMAIL   FOUND SOMETHING, BUT IT'S NOT EMAIL          
         J     GETEM20                                                          
*                                                                               
         USING SANAMD,R5                                                        
GETEM32  CLI   BYTE,0              TEST EMAIL WANTED ONLY                       
         JE    GETEM20             IGNORE NAMES                                 
         LA    R1,SANAMELN         LENGTH OF NAME                               
         USING SANAMELN,R1                                                      
         TM    SANAMIND,SANAMIFN   TEST FIRST NAME PRESENT                      
         JZ    GETEM34                                                          
         XR    RF,RF                                                            
         IC    RF,SANAMELN                                                      
         CHI   RF,15               TEST > MAX LENGTH                            
         JNH   *+8                                                              
         LA    RF,15               SET IT IF GREATER                            
         AHI   RF,-1                                                            
         BASR  RE,0                                                             
         MVC   APPFSTNM(0),SANAME                                               
         EX    RF,0(RE)                                                         
         LA    R1,2(RF,R1)         MOVE ONTO MIDDLE NAME                        
                                                                                
GETEM34  TM    SANAMIND,SANAMIMN   TEST MIDDLE NAME PRESENT                     
         JZ    GETEM36                                                          
         IC    RF,SANAMELN                                                      
         CHI   RF,15               TEST > MAX LENGTH                            
         JNH   *+8                                                              
         LA    RF,15               SET IT IF GREATER                            
         AHI   RF,-1                                                            
         BASR  RE,0                                                             
         MVC   APPMIDNM(0),SANAME                                               
         EX    RF,0(RE)                                                         
         LA    R1,2(RF,R1)         MOVE ONTO MIDDLE NAME                        
                                                                                
GETEM36  TM    SANAMIND,SANAMILN   TEST LAST NAME PRESENT                       
         JZ    GETEM20                                                          
         IC    RF,SANAMELN                                                      
         CHI   RF,58               TEST > MAX LENGTH                            
         JNH   *+8                                                              
         LA    RF,58               SET IT IF GREATER                            
         AHI   RF,-1                                                            
         BASR  RE,0                                                             
         MVC   APPLSTNM(0),SANAME                                               
         EX    RF,0(RE)                                                         
         J     GETEM20                                                          
*                                                                               
         USING SAPERD,R5                                                        
GETEM38  OC    SAPERDTE,SAPERDTE   NO TERMINATION DATE                          
         JZ    GETEM20                                                          
         CLC   TODAYC,SAPERDTE     CHECK WHETHER PID IS TERMINATED              
         JNH   GETEM20                                                          
         OI    RUNIND1,RUNITERM    SET NO APPROVER (TERM)                       
         J     GETEM20                                                          
*                                                                               
GETEMY   TM    RUNIND1,RUNITERM    TERMINATED USER?                             
         JZ    EXITE                                                            
         XC    APPEMAIL,APPEMAIL   CLEAR OUT EMAIL ADDRESS                      
         J     EXITH                                                            
*                                                                               
GETEMN   OI    RUNIND1,RUNINOCP    NO VALID CHARACTER PID                       
         J     EXITL                                                            
         DROP  R1,R3                                                            
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* PUT DETAILS INTO EMAIL TABLE BUFFER (TIMESHEETS AND EXPENSES)       *         
***********************************************************************         
         SPACE 1                                                                
PUTETAB  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PUTETA*'                                                      
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         USING EMRECD,R2                                                        
         CLI   SORTMODL,0                                                       
         JE    PUTETAXY                                                         
         CLI   SORTMODL,SORTFIN    IS IT FINANCE APPROVER FOR ESTIMATES         
         JNE   PUTE0A                                                           
         MVI   EMAMODL,SORTEXP     IF IT IS THEN SET AS EXPENSES TO             
         J     *+10                MAKE THINGS EASIER LATER ON!                 
PUTE0A   MVC   EMAMODL,SORTMODL                                                 
         MVC   EMASTAT,SORTSTAT    SAVE SORT REC STATUS                         
         MVC   EMAPIN,APPRPID      APPROVER BINARY PID                          
         MVC   EMASEQ,SORTSEQ                                                   
         MVC   EMASEQ2,SORTSEQ                                                  
         MVC   EMASEQ3,SORTSEQ                                                  
         MVC   EMADDR,APPEMAIL                                                  
         MVC   EMORDN,SORTNMBR     EXPENSE NO.                                  
         OC    EMORDN,SPACES                                                    
         MVC   EMAOFF,SORTOFF                                                   
         OC    EMAOFF,SPACES                                                    
         MVC   EMAALP,SORTALP                                                   
         CLC   SORTUL,SJUL                                                      
         JNE   PUTE0B                                                           
         MVC   EMULC,SJUL                                                       
         MVC   EMACC,SORTCPJ                                                    
         J     PUTE02                                                           
PUTE0B   CLC   SORTUL,ONERUL       NO CPJ THEN USE 1R                           
         JNE   PUTE0C                                                           
         MVC   EMULC,ONERUL                                                     
         MVC   EMACC,SORT1R                                                     
         J     PUTE02                                                           
PUTE0C   CLC   SORTUL,ONENUL                                                    
         JE    *+6                                                              
         DC    H'0'                WRONG LEDGER SOMETHING WRONG                 
         MVC   EMULC,ONENUL                                                     
         MVC   EMACC,SORT1N                                                     
*                                                                               
PUTE02   XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RF,ONERL3L          LENGTH SUB DEPT                              
         IC    RE,ONERL4L          LENGTH PERSON                                
         SR    RE,RF                                                            
         LA    R5,SORT1R(RF)                                                    
         MVC   EMPERC,SPACES       Initialise as for 2CO we won't set           
         SHI   RE,1                last byte of EMPERC and 0(R2) isn't          
         BASR  R1,0                cleared down                                 
         MVC   EMPERC(0),0(R5)     PERSON CODE                                  
         EX    RE,0(R1)                                                         
         OC    EMPERC,SPACES                                                    
         L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         MVC   EMDATE,SORTDATE     PERIOD END DATE                              
         MVC   EMDATE2,SORTDATE     PERIOD END DATE                             
         MVC   EMRLDAT,SORTLEND    LOCATION END DATE                            
         MVC   EMAPID,PIDCHAR      APPROVER PID NAME                            
         MVC   EMAPID2,PIDCHAR     APPROVER PID NAME                            
         ZAP   EMAMNT,SORTAMNT                                                  
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
*                                                                               
         L     R5,NUMRECS                                                       
         LA    R5,1(R5)            INCREMENT # OF RECORDS                       
         ST    R5,NUMRECS          STORED IN EMAILTAB                           
         ST    R5,NUMREC2                                                       
         L     RE,=A(EMAILMAX)     GROUPS WITHIN A PERSON CODE                  
         CR    R5,RE                                                            
         JL    PUTETAXY                                                         
         SAM24                                                                  
                                                                                
         GOTOR PROETAB             HIT MAX EMAILS                               
         GOTOR PRTDET                                                           
         GOTOR DETREP                                                           
         GOTOR LCKREP                                                           
*                                                                               
         SAM31                                                                  
         L     R0,AEMLTAB          CLEAR EMAIL TABLE                            
         ST    R0,ANXTETB                                                       
         L     R1,EMLTBLN                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    NUMRECS,NUMRECS                                                  
         XC    NUMREC2,NUMREC2                                                  
*                                                                               
PUTETAXN J     EXITL                                                            
*                                                                               
PUTETAXY SAM24                                                                  
         J     EXITE                                                            
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* PUT DETAILS INTO EMAIL TABLE BUFFER (ORDERS)                        *         
***********************************************************************         
         SPACE 1                                                                
PUTOTAB  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PUTOTA*'                                                      
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         USING EMRECD,R2                                                        
         CLI   SORTMODL,0                                                       
         JE    PUTOTABY                                                         
         MVC   EMAMODL,SORTMODL                                                 
         MVC   EMASEQ,SORTSEQ                                                   
         MVC   EMASEQ2,SORTSEQ                                                  
         MVC   EMASEQ3,SORTSEQ                                                  
         MVC   EMAPIN,APPRPID      APPROVER BINARY PID                          
         MVC   EMADDR,APPEMAIL                                                  
         MVC   EMDATE,SORTDATE     ORDER DATE                                   
         MVC   EMDATE2,SORTDATE     ORDER DATE                                  
         MVC   EMORDNM,SORTESTN    ESTIMATE NAME                                
         CLC   SORTCPJ,SPACES                                                   
         JNH   PUTO02                                                           
         MVC   EMOCPJ(L'SJUL),SJUL                                              
         MVC   EMOCPJ+L'SJUL(L'SORTCPJ),SORTCPJ                                 
PUTO02   MVC   EMAPID,PIDCHAR      APPROVER PID NAME                            
         MVC   EMAPID2,PIDCHAR     APPROVER PID NAME                            
         MVC   EMORDN,SORTNMBR     ORDER NO.                                    
         OC    EMORDN,SPACES                                                    
         MVC   EMSUP,SORTSUP       SUPPLIER ACCOUNT NO.                         
         OC    EMSUP,SPACES                                                     
         ZAP   EMAMNT,SORTAMNT     ORDER AMOUNT                                 
         MVC   EMRBDAT,SORTRBDT                                                 
         MVC   EMAOFF,SORTOFF                                                   
         OC    EMAOFF,SPACES                                                    
         MVC   EMAALP,SORTALP                                                   
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
                                                                                
         L     R5,NUMRECS                                                       
         LA    R5,1(R5)            INCREMENT # OF RECORDS                       
         ST    R5,NUMRECS          STORED IN EMAILTAB                           
         ST    R5,NUMREC2                                                       
         L     RE,=A(EMAILMAX)     GROUPS WITHIN A PERSON CODE                  
         CR    R5,RE                                                            
         JL    PUTOTABY                                                         
         SAM24                                                                  
*                                                                               
         GOTOR PROETAB                                                          
         GOTOR PRTDET                                                           
         GOTOR DETREP                                                           
         GOTOR LCKREP                                                           
         SAM31                                                                  
         L     R0,AEMLTAB          CLEAR EMAIL TABLE                            
         ST    R0,ANXTETB                                                       
         L     R1,EMLTBLN                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    NUMRECS,NUMRECS                                                  
         XC    NUMREC2,NUMREC2                                                  
*                                                                               
PUTOTABN SAM24                                                                  
         J     EXITL                                                            
*                                                                               
PUTOTABY SAM24                                                                  
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* PUT DETAILS INTO EMAIL TABLE BUFFER (JOBS)                          *         
***********************************************************************         
         SPACE 1                                                                
PUTJTAB  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PUTJTA*'                                                      
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         USING EMRECD,R2                                                        
         CLI   SORTMODL,0                                                       
         JE    PUTJTABY                                                         
         MVC   EMAMODL,SORTMODL                                                 
         MVC   EMASEQ,SORTSEQ                                                   
         MVC   EMASEQ2,SORTSEQ                                                  
         MVC   EMASEQ3,SORTSEQ                                                  
         MVC   EMAPIN,APPRPID      APPROVER BINARY PID                          
         MVC   EMADDR,APPEMAIL                                                  
         MVC   EMAPID,PIDCHAR      APPROVER PID NAME                            
         MVC   EMAPID2,PIDCHAR     APPROVER PID NAME                            
         MVC   EMAOFF,SORTOFF                                                   
         OC    EMAOFF,SPACES                                                    
         MVC   EMAALP,SORTALP                                                   
         MVC   EMULC,SJUL                                                       
         MVC   EMACC,SORTCPJ       JOB CODE                                     
         MVC   EMJOBN,SORTNAME     JOB NAME                                     
         MVC   EMDATE,SORTDATE     JOB ADDED DATE                               
         MVC   EMDATE2,SORTDATE     JOB ADDED DATE                              
         TM    RUNIND2,RUN2DEFT    DEFAULT APPROVER?                            
         JZ    *+8                                                              
         OI    EMDIND,DEFAPP       SET DEFAULT APPROVER                         
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
                                                                                
         L     R5,NUMRECS                                                       
         LA    R5,1(R5)            INCREMENT # OF RECORDS                       
         ST    R5,NUMRECS          STORED IN EMAILTAB                           
         ST    R5,NUMREC2                                                       
         L     RE,=A(EMAILMAX)     GROUPS WITHIN A PERSON CODE                  
         CR    R5,RE                                                            
         JL    PUTJTABY                                                         
*                                                                               
         SAM24                                                                  
         GOTOR PROETAB                                                          
         GOTOR PRTDET                                                           
         GOTOR DETREP                                                           
         GOTOR LCKREP                                                           
         SAM31                                                                  
         L     R0,AEMLTAB          CLEAR EMAIL TABLE                            
         ST    R0,ANXTETB                                                       
         L     R1,EMLTBLN                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    NUMRECS,NUMRECS                                                  
         XC    NUMREC2,NUMREC2                                                  
*                                                                               
PUTJTABN SAM24                                                                  
         J     EXITL                                                            
*                                                                               
PUTJTABY SAM24                                                                  
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* PUT DETAILS INTO EMAIL TABLE BUFFER (ESTIMATES)                     *         
***********************************************************************         
         SPACE 1                                                                
PUTSTAB  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PUTSTA*'                                                      
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         USING EMRECD,R2                                                        
         CLI   SORTMODL,0                                                       
         JE    PUTSTABY                                                         
         MVC   EMAMODL,SORTMODL                                                 
         MVC   EMASEQ,SORTSEQ                                                   
         MVC   EMASEQ2,SORTSEQ                                                  
         MVC   EMASEQ3,SORTSEQ                                                  
         MVC   EMAPIN,APPRPID      APPROVER BINARY PID                          
         MVC   EMADDR,APPEMAIL                                                  
         MVC   EMAPID,PIDCHAR      APPROVER PID NAME                            
         MVC   EMAPID2,PIDCHAR     APPROVER PID NAME                            
         MVC   EMULC,SJUL                                                       
         MVC   EMACC,SORTCPJ       JOB CODE                                     
         MVC   EMESTN,SORTNMBR     ESTIMATE NUMBER                              
         MVC   EMESTNM,SORTESTN    ESTIMATE NAME                                
         MVC   EMDATE,SORTDATE     ESTIMATE ADDED DATE                          
         MVC   EMDATE2,SORTDATE     ESTIMATE ADDED DATE                         
         ZAP   EMAMNT,SORTAMNT     ESTIMATE AMOUNT                              
         MVC   EMAOFF,SORTOFF                                                   
         OC    EMAOFF,SPACES                                                    
         MVC   EMAALP,SORTALP                                                   
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
                                                                                
         L     R5,NUMRECS                                                       
         LA    R5,1(R5)            INCREMENT # OF RECORDS                       
         ST    R5,NUMRECS          STORED IN EMAILTAB                           
         ST    R5,NUMREC2          STORED IN EMAILTAB                           
         L     RE,=A(EMAILMAX)     GROUPS WITHIN A PERSON CODE                  
         CR    R5,RE                                                            
         JL    PUTSTABY                                                         
*                                                                               
         SAM24                                                                  
         GOTOR PROETAB                                                          
         GOTOR PRTDET                                                           
         GOTOR DETREP                                                           
         GOTOR LCKREP                                                           
         SAM31                                                                  
         L     R0,AEMLTAB          CLEAR EMAIL TABLE                            
         ST    R0,ANXTETB                                                       
         L     R1,EMLTBLN                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    NUMRECS,NUMRECS                                                  
         XC    NUMREC2,NUMREC2                                                  
*                                                                               
PUTSTABN SAM24                                                                  
         J     EXITL                                                            
*                                                                               
PUTSTABY SAM24                                                                  
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* PUT DETAILS INTO EMAIL TABLE BUFFER (INVOICES)                      *         
***********************************************************************         
         SPACE 1                                                                
PUTITAB  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PUTITA*'                                                      
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         USING EMRECD,R2                                                        
         CLI   SORTMODL,0                                                       
         JE    PUTITABY                                                         
         MVC   EMAMODL,SORTMODL                                                 
         MVC   EMASEQ,SORTSEQ                                                   
         MVC   EMASEQ2,SORTSEQ                                                  
         MVC   EMASEQ3,SORTSEQ                                                  
         MVC   EMAPIN,APPRPID      APPROVER BINARY PID                          
         MVC   EMADDR,APPEMAIL                                                  
         MVC   EMAPID,PIDCHAR      APPROVER PID NAME                            
         MVC   EMAPID2,PIDCHAR     APPROVER PID NAME                            
         MVC   EMAINTR,SORTINTR                                                 
         MVC   EMAITYP,SORTITYP                                                 
         MVC   EMAIORD,SORTIORD                                                 
         MVC   EMSUP,SORTSUP       INVOICE SUPPLIER CODE                        
         OC    EMSUP,SPACES                                                     
         MVC   EMILN,SORTNMBR      INVOICE LOG NUMBER                           
         MVC   EMRBDAT,SORTRBDT    INVOICE ORDER REQ BY DATE                    
         MVC   EMDTUPD,SORTDUPD    INVOICE UPDATED DATE                         
         MVC   EMIORDN,SORTION     INVOICE ORDER NUMBER                         
         OC    EMIORDN,SPACES                                                   
         MVC   EMDATE,SORTDATE     INVOICE ADDED DATE                           
         MVC   EMDATE2,SORTDATE     INVOICE ADDED DATE                          
         ZAP   EMAMNT,SORTAMNT     INVOICE AMOUNT                               
         MVC   EMAOFF,SORTOFF                                                   
         OC    EMAOFF,SPACES                                                    
         MVC   EMAALP,SORTALP                                                   
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
                                                                                
         L     R5,NUMRECS                                                       
         LA    R5,1(R5)            INCREMENT # OF RECORDS                       
         ST    R5,NUMRECS          STORED IN EMAILTAB                           
         ST    R5,NUMREC2          STORED IN EMAILTAB                           
         L     RE,=A(EMAILMAX)     GROUPS WITHIN A PERSON CODE                  
         CR    R5,RE                                                            
         JL    PUTITABY                                                         
*                                                                               
         SAM24                                                                  
         GOTOR PROETAB                                                          
         GOTOR PRTDET                                                           
         GOTOR DETREP                                                           
         GOTOR LCKREP                                                           
         SAM31                                                                  
         L     R0,AEMLTAB          CLEAR EMAIL TABLE                            
         ST    R0,ANXTETB                                                       
         L     R1,EMLTBLN                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    NUMRECS,NUMRECS                                                  
         XC    NUMREC2,NUMREC2                                                  
*                                                                               
PUTITABN SAM24                                                                  
         J     EXITL                                                            
*                                                                               
PUTITABY SAM24                                                                  
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* SORTING AND EMAIL BATCHING                                          *         
* USES 31 BIT ADDRESSING IN PLACES                                    *         
***********************************************************************         
         SPACE 1                                                                
PROETAB  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PROETA*'                                                      
         CLI   RCWRITE,NOQ         No emails to be sent?                        
         JE    PETABX              Yes then exit                                
         OC    NUMRECS,NUMRECS                                                  
         JZ    PETABX                                                           
*                                                                               
         L     RF,TOTNUMR          Total up number of records                   
         A     RF,NUMRECS                                                       
         ST    RF,TOTNUMR                                                       
*                                                                               
         XC    RUNIND2,RUNIND2                                                  
         SAM31                                                                  
         L     R2,AEMLTAB                                                       
         USING EMRECD,R2                                                        
R        USING EMRECD,R5                                                        
         L     R3,NUMRECS                                                       
         CLI   EM2SOFF,YESQ        Sort by office profile set?                  
         JNE   PETAB05                                                          
         CLI   SHOWSEM,YESQ        If summary emails don't bother               
         JE    PETAB05                                                          
         XR    R0,R0                                                            
         L     R2,AEMLTAB                                                       
         L     RF,VQSORT                                                        
         O     RF,=X'80000000'                                                  
         GOTO1 (RF),DMCB,(R2),(R3),EMRECL,EMLENG4,EMAPID2-EMRECD                
         J     PETAB10                                                          
*                                                                               
PETAB05  XR    R0,R0                                                            
         L     R2,AEMLTAB                                                       
         L     RF,VQSORT                                                        
         O     RF,=X'80000000'                                                  
         GOTO1 (RF),DMCB,(R2),(R3),EMRECL,EMLENG,0                              
*                                                                               
PETAB10  ST    R2,ANXTETB                                                       
*                                                                               
PETAB15  XC    NUMPUT,NUMPUT                                                    
         XC    NUMTOT,NUMTOT                                                    
         CLC   APPRPID,EMAPIN      Test same as previous                        
         MVC   APPRPID,EMAPIN                                                   
         JE    PETAB20             Yes - use existing details                   
         SAM24                                                                  
         GOTOR GETEML,1            Else get approver names                      
PETAB20  SAM31                                                                  
         OC    EMADDR,EMADDR       Test email address set                       
         JNZ   PETAB30                                                          
         JCT   R3,PETAB25          Decrement count                              
         J     PETAB370            Haven't got email address so exit            
*                                                                               
PETAB25  LA    R2,EMRECL(R2)       No email address get next record             
         J     PETAB15                                                          
*                                                                               
PETAB30  ST    R2,ANXTETB                                                       
         L     RF,ASVEMREC         Move EMRECD to 24 bit storage!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         MVC   APPEMAIL,EMADDR     Store email address for later                
         GOTOR DOEMADD,0(R2)       Output email address and subject             
         JE    PETAB35                                                          
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         JCT   R3,PETAB25                                                       
         J     PETAB370                                                         
*                                                                               
PETAB35  CLI   EMAMODL,0                                                        
         JNE   *+6                                                              
         DC    H'0'                Error no module set                          
*                                                                               
         CLI   SHOWSEM,YESQ        Show summary emails default=Y                
         JNE   PETAB95                                                          
         GOTOR DOSHEAD             Do summary email headings                    
*                                                                               
         L     R6,AEMLSUMT         Do top section of email                      
*                                                                               
PETAB40  CLI   0(R6),X'FF'                                                      
         JE    PETAB50                                                          
*                                                                               
         LLC   RF,0(R6)                                                         
         SHI   RF,2                                                             
         CHI   RF,MAXELEN                                                       
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TEXT1,C' '                                                       
         MVC   TEXT1,TEXTSPCS                                                   
         BASR  RE,0                                                             
         MVC   TEXT1(0),1(R6)                                                   
         EX    RF,0(RE)                                                         
         CLC   TEXT1,TEXTSPCS      Line all spaces?                             
         JNH   PETAB45                                                          
*                                                                               
         GOTO1 SQUASHER,DMCB,TEXT1,L'TEXT1                                      
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB45                                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                  Print email line                             
PETAB45  LLC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         J     PETAB40                                                          
*                                                                               
PETAB50  JCT   R3,PETAB55          Decrement number of records                  
         SAM24                                                                  
         L     RF,NUMPUT                                                        
         AHI   RF,1                                                             
         ST    RF,NUMPUT                                                        
         J     PETAB60                                                          
*                                                                               
PETAB55  SAM31                                                                  
         L     RF,NUMPUT           Count number of timesheets                   
         AHI   RF,1                                                             
         ST    RF,NUMPUT                                                        
         L     R2,ANXTETB                                                       
         LR    R5,R2               R2=A(NEXT RECORD)                            
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
         CLC   R.EMAPIN(L'EMAPIN+L'EMASEQ),EMAPIN                               
         JE    PETAB50             Same PID and module? Add up total            
         SAM24                                                                  
*                                                                               
PETAB60  L     RF,AEMS255E                                                      
         CURED (4,NUMPUT),(4,12(RF)),0,ALIGN=LEFT                               
         L     R6,AEMLSECT                                                      
*                                  Start a new one                              
PETAB65  CLI   0(R6),X'FF'                                                      
         JE    PETAB75             Move to next module                          
         LLC   RF,0(R6)                                                         
         SHI   RF,2                                                             
         CHI   RF,MAXELEN                                                       
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TEXT1,C' '                                                       
         MVC   TEXT1,TEXTSPCS                                                   
         BASR  RE,0                                                             
         MVC   TEXT1(0),1(R6)                                                   
         EX    RF,0(RE)                                                         
         CLC   TEXT1,TEXTSPCS      Line all spaces?                             
         JNH   PETAB70                                                          
*                                                                               
         GOTO1 SQUASHER,DMCB,TEXT1,L'TEXT1                                      
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB70             Print email line                             
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB70  LLC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         J     PETAB65                                                          
*                                                                               
PETAB75  SAM31                                                                  
         L     R2,ANXTETB                                                       
         L     RF,ASVEMREC         Move EMRECD to 24 bit storage!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         CLC   R.EMAPIN(L'EMAPIN),EMAPIN                                        
         JNE   PETAB80             Same PID?  Then finish section and           
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         GOTOR DOSHEAD             Start headings for next module               
         J     PETAB50                                                          
*                                                                               
PETAB80  L     R2,ASVEMREC                                                      
         L     R6,AEMLSEND                                                      
*                               ** End section **                               
PETAB85  CLI   0(R6),X'FF'                                                      
         JE    PETAB365                                                         
         LLC   RF,0(R6)                                                         
         SHI   RF,2                                                             
         CHI   RF,MAXELEN                                                       
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TEXT1,C' '                                                       
         MVC   TEXT1,TEXTSPCS                                                   
         BASR  RE,0                                                             
         MVC   TEXT1(0),1(R6)                                                   
         EX    RF,0(RE)                                                         
         CLC   TEXT1,TEXTSPCS      Line all spaces?                             
         JNH   PETAB90                                                          
*                                                                               
         GOTO1 SQUASHER,DMCB,TEXT1,L'TEXT1                                      
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB90                                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)    PRINT EMAIL LINE                
*                                                                               
PETAB90  LLC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         J     PETAB85                                                          
*                               ** Master loop over whole email **              
PETAB95  L     R6,AELEMT                                                        
*                                                                               
PETAB100 CLM   R6,15,AELEMSEC                                                   
         JNL   PETAB110                                                         
         LLC   RF,0(R6)                                                         
         SHI   RF,2                                                             
         CHI   RF,MAXELEN                                                       
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TEXT1,C' '                                                       
         MVC   TEXT1,TEXTSPCS                                                   
         BASR  RE,0                                                             
         MVC   TEXT1(0),1(R6)                                                   
         EX    RF,0(RE)                                                         
*                                                                               
         GOTO1 SQUASHER,DMCB,TEXT1,L'TEXT1                                      
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB105                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                  Print email line                             
PETAB105 LLC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         J     PETAB100                                                         
*                               ** Starting a section of the email **           
PETAB110 L     R6,AELEMSEC                                                      
         GOTOR DOAHEAD             Do headings                                  
*                                                                               
PETAB115 CLM   R6,15,AELEMTML      List section?                                
         JNL   PETAB130                                                         
         LLC   RF,0(R6)                                                         
         SHI   RF,2                                                             
         CHI   RF,MAXELEN                                                       
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
PETAB120 MVI   TEXT1,C' '                                                       
         MVC   TEXT1,TEXTSPCS                                                   
         BASR  RE,0                                                             
         MVC   TEXT1(0),1(R6)                                                   
         EX    RF,0(RE)                                                         
*                                                                               
         GOTO1 SQUASHER,DMCB,TEXT1,L'TEXT1                                      
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB125                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                  Print email line                             
PETAB125 LLC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         J     PETAB115                                                         
*                                                                               
         USING APPTABD,R4                                                       
PETAB130 LA    R4,TEXT1                                                         
*                                                                               
         CLI   EMAMODL,SORTTIME    Time module?                                 
         JNE   PETAB165                                                         
         MVC   TEXT1,TEXTSPCS      Do person now                                
         GOTOR GETPERS,DMCB,EMRECD                                              
         MVC   APTIMA,TBROPE                                                    
         MVC   APTIMB,TBDOPE                                                    
         MVC   APPFST(L'PERFSTNM),PERFSTNM                                      
         MVC   APTIMC,TBDCLO                                                    
         MVC   APTIMD,TBDOPE                                                    
         MVC   APPLST(L'PERLSTNM),PERLSTNM                                      
         MVC   APTIME,TBDCLO                                                    
         MVC   APTIMF,TBDOPE                                                    
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB135                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB135 MVC   TEXT1,TEXTSPCS                                                   
         MVC   APTIMG(L'LINKOP),LINKOP                                          
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB140                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB140 MVC   TEXT1,TEXTSPCS                                                   
*                                                                               
         GOTOR SETURL,DMCB,APTIMH  Build URL (R5) set on return                 
         MVC   0(DISPURTL,R5),DISPURLT                                          
         AHI   R5,DISPURTL                                                      
         CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         JCT   R5,*-8                                                           
         AHI   R5,1                                                             
         MVC   0(L'PERTXT,R5),PERTXT                                            
         AHI   R5,L'PERTXT                                                      
         MVC   0(L'EMPERC,R5),EMPERC                                            
         AHI   R5,L'EMPERC         Move PID into URL                            
         CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         JCT   R5,*-8                                                           
         AHI   R5,1                                                             
         MVI   0(R5),X'50'                                                      
         MVC   1(L'ENDTXT,R5),ENDTXT                                            
         AHI   R5,L'ENDTXT+1                                                    
         GOTO1 DATCON,DMCB,(1,EMDATE),(20,0(R5))                                
         OC    EMRLDAT,EMRLDAT     Location end date                            
         JZ    PETAB145                                                         
         AHI   R5,8                                                             
         MVI   0(R5),X'50'                                                      
         MVC   1(L'LOCTXT,R5),LOCTXT                                            
         AHI   R5,L'LOCTXT+1                                                    
         GOTO1 DATCON,DMCB,(1,EMRLDAT),(20,0(R5))                               
*                                                                               
PETAB145 MVC   8(L'LINKCB,R5),LINKCB                                            
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB150                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB150 MVC   TEXT1,TEXTSPCS                                                   
         GOTO1 DATCON,DMCB,(1,EMDATE),(23,WORK)                                 
         MVC   APTDAY,WORK+8                                                    
*                                                                               
         USING MONTABD,RF                                                       
         L     RF,AMONTAB          Lookup calendar dictionary equate            
*                                  from table                                   
PETAB155 CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MONNUM,WORK+5       match on month number                        
         JE    PETAB160                                                         
         LLC   R0,MONLEN                                                        
         AR    RF,R0                                                            
         J     PETAB155                                                         
*                                                                               
PETAB160 GOTO1 ADDICTAT,DMCB,C'LL  ',MONDD,APTMTH                               
*                                                                               
         MVC   APTYER,WORK                                                      
         MVC   APTIMI,LINKCL                                                    
         MVC   APTIMJ,TBDCLO                                                    
         MVC   APTIMK,TBRCLO                                                    
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB290                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
         J     PETAB290                                                         
*                                                                               
PETAB165 CLI   EMAMODL,SORTJOB     Jobs module?                                 
         JNE   PETAB195                                                         
         MVC   TEXT1,TEXTSPCS      Do person now                                
         MVC   APJOBA,TBROPE                                                    
         MVC   APJOBB,TBDOPE                                                    
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB170                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB170 MVC   TEXT1,TEXTSPCS                                                   
         MVC   APJOBC(L'LINKOP),LINKOP                                          
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB175                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB175 MVC   TEXT1,TEXTSPCS                                                   
         GOTOR SETURL,DMCB,APJOBD  Build URL (R5) set on return                 
         MVC   0(DISPURJL,R5),DISPURLJ                                          
         AHI   R5,DISPURJL         Build URL                                    
         CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         JCT   R5,*-8                                                           
         AHI   R5,1                                                             
         MVC   0(L'CLITXT,R5),CLITXT                                            
         AHI   R5,L'CLITXT                                                      
*                                                                               
         LLC   RF,PCLILEN          Convert client code                          
         GOTOR URIENC,DMCB,((RF),EMACC),0(R5)                                   
         LLC   RF,8(R1)                                                         
         AR    R5,RF                                                            
         MVI   0(R5),X'50'                                                      
         AHI   R5,1                                                             
         MVC   0(L'PROTXT,R5),PROTXT                                            
         AHI   R5,L'PROTXT                                                      
*                                                                               
         LLC   RF,PCLILEN                                                       
         LA    RF,EMACC(RF)                                                     
         LLC   R0,PPROLEN                                                       
         LLC   RE,PCLILEN                                                       
         SR    R0,RE                                                            
         GOTOR URIENC,DMCB,((R0),0(RF)),0(R5)                                   
         LLC   RF,8(R1)                                                         
         AR    R5,RF                                                            
         MVI   0(R5),X'50'                                                      
         AHI   R5,1                                                             
         MVC   0(L'JOBTXT,R5),JOBTXT                                            
         AHI   R5,L'JOBTXT                                                      
*                                                                               
         LLC   RF,PPROLEN                                                       
         LA    RF,EMACC(RF)                                                     
         LLC   R0,JOBLEN                                                        
         LLC   RE,PPROLEN                                                       
         SR    R0,RE                                                            
         GOTOR URIENC,DMCB,((R0),0(RF)),(X'80',0(R5))                           
         LLC   RF,8(R1)                                                         
         AR    R5,RF                                                            
         MVC   0(L'LINKCB,R5),LINKCB                                            
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB180                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB180 MVC   TEXT1,TEXTSPCS                                                   
         MVC   APJCOD,EMACC                                                     
         MVC   APJOBE,LINKCL                                                    
         MVC   APJOBF,TBDCLO                                                    
         MVC   APJOBG,TBDOPE                                                    
         MVC   APJNAM,EMJOBN                                                    
         MVC   APJOBH,TBDCLO                                                    
         MVC   APJOBI,TBDOPE                                                    
         GOTO1 DATCON,DMCB,(1,EMDATE),(23,WORK)                                 
         MVC   APJDAY,WORK+8                                                    
*                                                                               
         USING MONTABD,RF                                                       
         L     RF,AMONTAB          Lookup calendar dictionary equate            
*                                  from table                                   
PETAB185 CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MONNUM,WORK+5       match on month number                        
         JE    PETAB190                                                         
         LLC   R0,MONLEN                                                        
         AR    RF,R0                                                            
         J     PETAB185                                                         
*                                                                               
PETAB190 GOTO1 ADDICTAT,DMCB,C'LL  ',MONDD,APJMTH                               
*                                                                               
         MVC   APJYER,WORK                                                      
         MVC   APJOBJ,TBDCLO                                                    
         MVC   APJOBK,TBRCLO                                                    
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB290                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
         J     PETAB290                                                         
*                                                                               
PETAB195 CLI   EMAMODL,EMAEST      Estimates module?                            
         JNE   PETAB225                                                         
         MVC   TEXT1,TEXTSPCS      Do person now                                
         MVC   APESTA,TBROPE                                                    
         MVC   APESTB,TBDOPE                                                    
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB200                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB200 MVC   TEXT1,TEXTSPCS                                                   
         MVC   APESTC(L'LINKOP),LINKOP                                          
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB205                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB205 MVC   TEXT1,TEXTSPCS                                                   
         GOTOR SETURL,DMCB,APESTD  Build URL (R5) set on return                 
         MVC   0(DISPUREL,R5),DISPURLE                                          
         AHI   R5,DISPUREL         Build URL                                    
         CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         JCT   R5,*-8                                                           
         AHI   R5,1                                                             
         MVC   0(L'ESTTXT,R5),ESTTXT                                            
         AHI   R5,L'ESTTXT                                                      
         MVC   0(L'EMESTN,R5),EMESTN                                            
         AHI   R5,L'EMESTN                                                      
         MVC   0(L'LINKCB,R5),LINKCB                                            
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB210                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB210 MVC   TEXT1,TEXTSPCS                                                   
         MVC   APESNO,EMESTN                                                    
         MVC   APESTE,LINKCL                                                    
         MVC   APESTF,TBDCLO                                                    
         MVC   APESTG,TBDOPE                                                    
         MVC   APENAM,EMESTNM                                                   
         MVC   APESTH,TBDCLO                                                    
         MVC   APESTI,TBDOPE                                                    
         GOTO1 DATCON,DMCB,(1,EMDATE),(23,WORK)                                 
         MVC   APEDAY,WORK+8                                                    
*                                                                               
         USING MONTABD,RF                                                       
         L     RF,AMONTAB          Lookup calendar dictionary equate            
*                                  from table                                   
PETAB215 CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MONNUM,WORK+5       match on month number                        
         JE    PETAB220                                                         
         LLC   R0,MONLEN                                                        
         AR    RF,R0                                                            
         J     PETAB215                                                         
*                                                                               
PETAB220 GOTO1 ADDICTAT,DMCB,C'LL  ',MONDD,APEMTH                               
*                                                                               
         MVC   APEYER,WORK                                                      
         MVC   APESTJ,TBDCLO                                                    
         MVC   APESTK,TBRCLO                                                    
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB290                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
         J     PETAB290                                                         
*                                                                               
PETAB225 CLI   EMAMODL,EMAORD      Order module?                                
         JNE   PETAB250                                                         
         GOTOR GETNAME,DMCB,EMSUP                                               
         MVC   TEXT1,TEXTSPCS      Do person now                                
         MVC   APORDA,TBROPE                                                    
         MVC   APORDB,TBDOPE                                                    
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB230                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB230 MVC   TEXT1,TEXTSPCS                                                   
         MVC   APORDC(L'LINKOP),LINKOP                                          
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB235                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB235 MVC   TEXT1,TEXTSPCS                                                   
         GOTOR SETURL,DMCB,APORDD  Build URL (R5) set on return                 
         MVC   0(DISPUROL,R5),DISPURLO                                          
         AHI   R5,DISPUROL         Build URL                                    
         CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         JCT   R5,*-8                                                           
         AHI   R5,1                                                             
         MVC   0(L'ORDTXT,R5),ORDTXT                                            
         AHI   R5,L'ORDTXT                                                      
         MVC   0(L'EMORDN,R5),EMORDN                                            
         AHI   R5,L'EMORDN                                                      
         MVC   0(L'LINKCB,R5),LINKCB                                            
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB240                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB240 MVC   TEXT1,TEXTSPCS                                                   
         MVC   APORNO,EMORDN                                                    
         MVC   APORDE,LINKCL                                                    
         MVC   APORDF,TBDCLO                                                    
         MVC   APORDG,TBDOPE                                                    
         MVC   APONAM,EMORDNM                                                   
         MVC   APORDH,TBDCLO                                                    
         MVC   APORDI,TBDOPE                                                    
         MVC   APOJNM,SPACES                                                    
         CLC   EMOCPJ,SPACES       C/P/J populated                              
         JNH   PETAB245                                                         
         GOTOR GETNAME,DMCB,EMOCPJ                                              
         LLC   RF,LACCNAM                                                       
         BASR  R1,0                                                             
         MVC   APOJNM(0),ACCNAM                                                 
         EX    RF,0(R1)                                                         
PETAB245 MVC   APORDJ,TBDCLO                                                    
         MVC   APORDK,TBRCLO                                                    
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB290                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
         J     PETAB290                                                         
*                                                                               
PETAB250 CLI   EMAMODL,EMAINV      Invoice module?                              
         JNE   PETAB270                                                         
         MVC   TEXT1,TEXTSPCS      Do person now                                
         MVC   APINVA,TBROPE                                                    
         MVC   APINVB,TBDOPE                                                    
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB255                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB255 MVC   TEXT1,TEXTSPCS                                                   
         MVC   APINVC(L'LINKOP),LINKOP                                          
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB260                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB260 MVC   TEXT1,TEXTSPCS                                                   
         GOTOR SETURL,DMCB,APINVD  Build URL (R5) set on return                 
         MVC   0(DISPURIL,R5),DISPURLI                                          
         AHI   R5,DISPURIL         Build URL                                    
         CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         JCT   R5,*-8                                                           
         AHI   R5,1                                                             
         MVC   0(L'INVTXT,R5),INVTXT                                            
         AHI   R5,L'INVTXT                                                      
         MVC   0(L'EMILN,R5),EMILN                                              
         AHI   R5,L'EMILN                                                       
         MVC   0(L'LINKCB,R5),LINKCB                                            
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB265                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB265 MVC   TEXT1,TEXTSPCS                                                   
         MVC   APINVN,EMILN                                                     
         MVC   APINVE,LINKCL                                                    
         MVC   APINVF,TBDCLO                                                    
         MVC   APINVG,TBDOPE                                                    
         MVC   APINVR,EMAINTR                                                   
         MVC   APINVH,TBDCLO                                                    
         MVC   APINVI,TBDOPE                                                    
         MVC   APINVS,EMSUP                                                     
         MVC   APINVJ,TBDCLO                                                    
         MVC   APINVK,TBDOPE                                                    
         GOTOR GETNAME,DMCB,EMSUP                                               
         LLC   RF,LACCNAM                                                       
         BASR  RE,0                                                             
         MVC   APINVSN(0),ACCNAM  SUPPLIER ACCOUNT NAME                         
         EX    RF,0(RE)                                                         
         MVC   APINVL,TBDCLO                                                    
         MVC   APINVM,TBRCLO                                                    
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB290                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
         J     PETAB290                                                         
*                                                                               
PETAB270 CLI   EMAMODL,EMAEXP      Expenses module?                             
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   TEXT1,TEXTSPCS      Do person now                                
         MVC   APEXPA,TBROPE                                                    
         MVC   APEXPB,TBDOPE                                                    
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB275                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB275 MVC   TEXT1,TEXTSPCS                                                   
         MVC   APEXPC(L'LINKOP),LINKOP                                          
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB280                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB280 MVC   TEXT1,TEXTSPCS                                                   
         GOTOR SETURL,DMCB,APEXPD  Build URL (R5) set on return                 
         MVC   0(DISPURXL,R5),DISPURLX                                          
         AHI   R5,DISPURXL         Build URL                                    
         CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         JCT   R5,*-8                                                           
         AHI   R5,1                                                             
         MVC   0(L'EXPTXT,R5),EXPTXT                                            
         AHI   R5,L'EXPTXT                                                      
         MVC   0(L'EMORDN,R5),EMORDN                                            
         AHI   R5,L'EMILN                                                       
         MVC   0(L'LINKCB,R5),LINKCB                                            
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB285                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PETAB285 MVC   TEXT1,TEXTSPCS                                                   
         MVC   APEXPN,EMORDN                                                    
         MVC   APEXPE,LINKCL                                                    
         MVC   APEXPF,TBDCLO                                                    
         MVC   APEXPG,TBDOPE                                                    
         MVC   APEXPR,EMACC                                                     
         MVC   APEXPH,TBDCLO                                                    
         MVC   APEXPI,TBRCLO                                                    
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB290                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
         J     PETAB290                                                         
*                                                                               
PETAB290 SAM31                                                                  
         L     R2,ANXTETB                                                       
         JCT   R3,PETAB295         Any more record to process                   
         J     PETAB350            no                                           
*                                                                               
PETAB295 LR    R5,R2                                                            
         LA    R2,EMRECL(R2)       R2=A(NEXT RECORD)                            
         ST    R2,ANXTETB                                                       
         CLI   EM2SOFF,YESQ        Sorting by office?                           
         JNE   PETAB325                                                         
         CLC   R.EMAPID2,EMAPID2   Same approver                                
         JNE   PETAB350                                                         
         CLC   R.EMASEQ,EMASEQ     same module?                                 
         JNE   PETAB335            no then terminate section                    
         CLC   R.EMAOFF,EMAOFF     Same office                                  
         JE    PETAB325                                                         
*                                                                               
         L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD Move EMRECD to 24 bit storage area           
         SAM24                                                                  
         L     R6,AELEMTML                                                      
*                                                                               
PETAB300 CLM   R6,15,AELEMOEN      Terminate office section                     
         JE    PETAB310                                                         
         LLC   RF,0(R6)                                                         
         SHI   RF,2                                                             
         CHI   RF,MAXELEN                                                       
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TEXT1,C' '                                                       
         MVC   TEXT1,TEXTSPCS                                                   
         BASR  RE,0                                                             
         MVC   TEXT1(0),1(R6)                                                   
         EX    RF,0(RE)                                                         
         CLC   TEXT1,TEXTSPCS      Line all spaces?                             
         JNH   PETAB305                                                         
*                                                                               
         GOTO1 SQUASHER,DMCB,TEXT1,L'TEXT1                                      
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB305                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)    PRINT EMAIL LINE                
*                                                                               
PETAB305 LLC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         J     PETAB300                                                         
*                                                                               
PETAB310 L     R2,ASVEMREC         Redo office headings                         
         GOTOR DOAHEAD                                                          
         L     R6,AELEMOFF                                                      
*                                                                               
PETAB315 CLM   R6,15,AELEMTML      Start a new office section                   
         JE    PETAB130                                                         
         LLC   RF,0(R6)                                                         
         SHI   RF,2                                                             
         CHI   RF,MAXELEN                                                       
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TEXT1,C' '                                                       
         MVC   TEXT1,TEXTSPCS                                                   
         BASR  RE,0                                                             
         MVC   TEXT1(0),1(R6)                                                   
         EX    RF,0(RE)                                                         
         CLC   TEXT1,TEXTSPCS      Line all spaces?                             
         JNH   PETAB320                                                         
*                                                                               
         GOTO1 SQUASHER,DMCB,TEXT1,L'TEXT1                                      
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB320                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)    PRINT EMAIL LINE                
*                                                                               
PETAB320 LLC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         J     PETAB315                                                         
*                                                                               
PETAB325 CLC   R.EMAPIN(L'EMAPIN+L'EMASEQ),EMAPIN SAME PID AND MODUL?           
         JNE   PETAB330                                                         
         L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     PETAB130            DON'T PRINT HEADINGS AGAIN                   
*                                                                               
PETAB330 CLC   R.EMAPIN,EMAPIN     SAME APPROVER?                               
         JNE   PETAB350                                                         
*                                                                               
PETAB335 L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         GOTOR DOAHEAD                                                          
         L     R6,AELEMTML                                                      
*                               ** End the section **                           
PETAB340 CLM   R6,15,AELEMSEN                                                   
         JE    PETAB110                                                         
         LLC   RF,0(R6)                                                         
         SHI   RF,2                                                             
         CHI   RF,MAXELEN                                                       
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TEXT1,C' '                                                       
         MVC   TEXT1,TEXTSPCS                                                   
         BASR  RE,0                                                             
         MVC   TEXT1(0),1(R6)                                                   
         EX    RF,0(RE)                                                         
         CLC   TEXT1,TEXTSPCS      Line all spaces?                             
         JNH   PETAB345                                                         
*                                                                               
         GOTO1 SQUASHER,DMCB,TEXT1,L'TEXT1                                      
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB345                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)    PRINT EMAIL LINE                
*                                                                               
PETAB345 LLC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         J     PETAB340                                                         
*                               ** End of the email **                          
PETAB350 L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         L     R6,AELEMTML                                                      
*                                                                               
PETAB355 CLI   0(R6),X'FF'                                                      
         JE    PETAB365                                                         
         LLC   RF,0(R6)                                                         
         SHI   RF,2                                                             
         CHI   RF,MAXELEN                                                       
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TEXT1,C' '                                                       
         MVC   TEXT1,TEXTSPCS                                                   
         BASR  RE,0                                                             
         MVC   TEXT1(0),1(R6)                                                   
         EX    RF,0(RE)                                                         
*                                                                               
         GOTO1 SQUASHER,DMCB,TEXT1,L'TEXT1                                      
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    PETAB360                                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                  Print email line                             
PETAB360 LLC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         J     PETAB355                                                         
*                                                                               
PETAB365 GOTO1 VSMTP,DMCB,('SMTPASND',0)        SEND EMAIL                      
*                                                                               
PETAB370 CHI   R3,1                Any more records?                            
         JNH   PETAB375            no exit                                      
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         J     PETAB15                                                          
*                                                                               
PETAB375 L     RF,TOTNUME          Total up number of emails for run            
         A     RF,NUMEMLS                                                       
         ST    RF,TOTNUME                                                       
*                                                                               
PETABX   SAM24                                                                  
         J     EXITE                                                            
         DROP  R2,R                                                             
         EJECT                                                                  
***********************************************************************         
* Do headings for summary emails                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING EMRECD,R2                                                        
         USING SUMHTABD,R4                                                      
DOSHEAD  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**DOSH**'                                                      
*                                                                               
         XC    NUMPUT,NUMPUT       Clear count total                            
         LA    R4,SUMHTAB                                                       
         J     DOSHEA04                                                         
*                                                                               
DOSHEA02 LHI   R0,SUMHTABL                                                      
         AR    R4,R0                                                            
*                                                                               
DOSHEA04 CLI   SUMHATY,0                                                        
         JNE   *+6                                                              
         DC    H'0'                No match found                               
         CLC   SUMHATY,EMAMODL                                                  
         JNE   DOSHEA02                                                         
*                                                                               
         L     R5,AEMS285E         Clear out previous heading                   
         MVC   0(L'EMLS285E,R5),TEXTSPCS                                        
         L     RF,SUMHPCH                                                       
         LLC   R1,SUMHPCH                                                       
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R5),0(RF)       Please click here to access                  
         EX    R1,0(RE)                                                         
*                                                                               
         L     R5,AEMS280E                                                      
         MVC   0(L'EMLS280E,R5),TEXTSPCS                                        
         GOTOR SETSURL                                                          
         LA    R2,BLDFURL                                                       
         LLH   RF,BLDLEN                                                        
*                                                                               
         CHI   RF,L'EMLS280E       Move url to html                             
         JNH   DOSHEA06                                                         
         MVC   0(L'EMLS280E,R5),0(R2)                                           
         SHI   RF,L'EMLS280E                                                    
         LA    R2,L'EMLS280E(R2)                                                
         L     R5,AEMS282E                                                      
*                                  Move in specific url                         
DOSHEA06 BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R5),0(R2)                                                    
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         AR    R5,RF                                                            
*                                                                               
         L     R5,AEMS255E                                                      
         MVC   0(L'EMLS255E,R5),TEXTSPCS                                        
         MVC   1(L'AC@YHAV,R5),AC@YHAV     You have                             
         L     RF,SUMHAPP                                                       
         LLC   R1,SUMHAPP                                                       
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   16(0,R5),0(RF)              Timesheets to approve                
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         AR    R5,R1                                                            
         MVC   24(L'AC@TOAPP,R5),AC@TOAPP                                       
         NI    24(R5),X'FF'-X'40'                                               
         CLI   COMPLANG,CTRYGER      EMNL8/EMNL9 not start of line              
         JE    DOSHEA08                                                         
         TM    SUMHIND,SUMHULC       UK lower case?                             
         JZ    DOSHEA08                                                         
         NI    16(R5),X'FF'-X'40'                                               
         J     DOSHEA10                                                         
*                                                                               
DOSHEA08 TM    SUMHIND,SUMHGLC       Germany lower case?                        
         JZ    *+8                                                              
         NI    16(R5),X'FF'-X'40'                                               
*                                                                               
DOSHEA10 L     R5,AEMS625E                 Find us here...                      
         MVC   0(L'EMLS625E,R5),TEXTSPCS                                        
         MVC   0(L'AC@FUSAT,R5),AC@FUSAT                                        
*                                                                               
DOSHEAX  J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* Do headings for each section of the email                           *         
***********************************************************************         
         SPACE 1                                                                
         USING EMRECD,R2                                                        
         USING AURHTABD,R4                                                      
DOAHEAD  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*DOAHEA*'                                                      
*                                                                               
         LA    R4,AURHTAB                                                       
         J     DOAHEA04                                                         
*                                                                               
DOAHEA02 LHI   R0,AURHTABL                                                      
         AR    R4,R0                                                            
*                                                                               
DOAHEA04 CLI   AURHATY,0                                                        
         JNE   *+6                                                              
         DC    H'0'                No match found                               
         CLC   EMAMODL,AURHATY                                                  
         JNE   DOAHEA02            Not timesheets go onto expenses              
*                                                                               
         L     R5,AELE805E          Find us here....                            
         MVC   0(L'ELEM805E,R5),TEXTSPCS                                        
         MVC   0(L'AC@FUSAT,R5),AC@FUSAT                                        
*                                                                               
         L     R5,AELE265E                                                      
         MVC   0(L'ELEM265E,R5),TEXTSPCS                                        
         L     RF,AURHDES                                                       
         LLC   R1,AURHDES                                                       
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R5),0(RF)       Here are the details                         
         EX    R1,0(RE)                                                         
*                                                                               
         L     R5,AELE280E                                                      
         MVC   0(L'ELEM280E,R5),TEXTSPCS                                        
         L     RF,AURHPCH                                                       
         LLC   R1,AURHPCH                                                       
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R5),0(RF)       Please click here to see...                  
         EX    R1,0(RE)                                                         
*                                                                               
         L     R5,AELE276E                                                      
         MVC   0(L'ELEM276E,R5),TEXTSPCS                                        
         L     RF,AELE278E                                                      
         MVC   0(L'ELEM278E,RF),TEXTSPCS                                        
         CLC   SVURL,SPACES        Complete URL exists?                         
         JH    *+14                                                             
         MVC   0(L'LINKCB,R5),LINKCB                                            
         J     DOAHEA08                                                         
*                                                                               
         GOTOR SETDURL                                                          
         LA    R2,BLDFURL                                                       
         LLH   RF,BLDLEN                                                        
*                                                                               
         CHI   RF,L'ELEM276E       Check if federated url is too long           
         JNH   DOAHEA06            if so use second line for url link           
         MVC   0(L'ELEM276E,R5),0(R2)                                           
         SHI   RF,L'ELEM276E                                                    
         LA    R2,L'ELEM276E(R2)                                                
         L     R5,AELE278E                                                      
*                                                                               
DOAHEA06 BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R5),0(R2)                                                    
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         AR    R5,RF                                                            
*                                                                               
DOAHEA08 L     R5,AELE370E         Fill out for the period ending...            
         MVC   0(L'ELEM370E,R5),TEXTSPCS                                        
         CLI   EM2SOFF,YESQ        Are we sorting by office?                    
         JNE   DOAHEA10                                                         
         MVC   0(L'AC@FOR,R5),AC@FOR                                            
         MVC   1+L'AC@FOR(L'AC@OFF,R5),AC@OFF                                   
         LA    R5,2+L'AC@FOR+L'AC@OFF(R5)                                       
         MVI   0(R5),C'"'                                                       
         MVC   1(L'EMAOFF,R5),EMAOFF                                            
         AHI   R5,L'EMAOFF                                                      
         CLI   EMAOFF+1,C' '                                                    
         JNH   *+8                                                              
         AHI   R5,1                1 char office?                               
         MVI   0(R5),C'"'                                                       
         AHI   R5,2                                                             
         L     RF,AURHMHE                                                       
         LLC   R1,AURHMHE                                                       
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R5),0(RF)                                                    
         EX    R1,0(RE)                                                         
         AHI   R1,2                                                             
         AR    R5,R1                                                            
         MVI   0(R5),C':'                                                       
         J     DOAHEAX                                                          
*                                                                               
DOAHEA10 L     RF,AURHMHE                                                       
         LLC   R1,AURHMHE                                                       
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R5),0(RF)                                                    
         EX    R1,0(RE)                                                         
         J     DOAHEAX                                                          
*                                                                               
DOAHEAX  J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* Print details of outstanding approvals                              *         
***********************************************************************         
         SPACE 1                                                                
         USING EMRECD,R2                                                        
R        USING EMRECD,R5                                                        
         USING BOXD,R4                                                          
PRTDET   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PRTDET*'                                                      
         SAM31                                                                  
         L     R2,AEMLTAB                                                       
         ST    R2,ANXTETB                                                       
         OC    NUMREC2,NUMREC2                                                  
         JZ    PRTDETX                                                          
         MVC   NUMRECS,NUMREC2     ESTABLISH NUMRECS FOR DETAILED               
         XC    RUNIND2,RUNIND2     REPORT                                       
*                                                                               
         L     R3,NUMREC2                                                       
         L     R2,AEMLTAB                                                       
         L     RF,VQSORT                                                        
         O     RF,=X'80000000'                                                  
         GOTO1 (RF),DMCB,(R2),(R3),EMRECL,EMLENG3,EMASEQ2-EMRECD                
*                                                                               
PDET02   L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
*                                  SORT ACCORDING TO MODULES                    
         XC    NUMAPPR,NUMAPPR     RESET FOR NEW MODULE                         
         XC    APPRPID,APPRPID                                                  
         TM    RUNIND2,RUN2TIM     TEST IF WE'RE DOING TIMESHEETS NOW           
         JNZ   PDET04                                                           
         CLI   EMAMODL,EMATIME     TEST IF WE HAVE MOVED TO TIMESHEETS          
         JNE   PDET04                                                           
         MVI   RCSUBPRG,1          YES WE HAVE SO START NEW PAGE                
         MVI   FORCEHED,YESQ       NEW HEADINGS AND SET INDICATOR               
         OI    RUNIND2,RUN2TIM                                                  
         J     PDET12                                                           
*                                                                               
PDET04   TM    RUNIND2,RUN2EXP     TEST IF WE'RE DOING EXPENSES NOW             
         JNZ   PDET06                                                           
         CLI   EMAMODL,EMAEXP      TEST IF WE HAVE MOVED TO EXPENSES            
         JNE   PDET06                                                           
         MVI   RCSUBPRG,2          YES WE HAVE SO START NEW PAGE                
         MVI   FORCEHED,YESQ       NEW HEADINGS AND SET INDICATOR               
         OI    RUNIND2,RUN2EXP                                                  
         J     PDET12                                                           
*                                                                               
PDET06   TM    RUNIND2,RUN2ORD     TEST IF WE'RE DOING ORDERS NOW               
         JNZ   PDET08                                                           
         CLI   EMAMODL,EMAORD      TEST IF WE HAVE MOVED TO ORDERS              
         JNE   PDET08                                                           
         MVI   RCSUBPRG,3          YES WE HAVE SO START A NEW PAGE              
         MVI   FORCEHED,YESQ       NEW HEADINGS AND SET INDICATOR               
         OI    RUNIND2,RUN2ORD                                                  
         J     PDET12                                                           
*                                                                               
PDET08   TM    RUNIND2,RUN2JOB     TEST IF WE'RE DOING JOBS NOW                 
         JNZ   PDET10                                                           
         CLI   EMAMODL,EMAJOB      TEST IF WE HAVE MOVED TO JOBS NOW            
         JNE   PDET10                                                           
         MVI   RCSUBPRG,4          YES WE HAVE SO START A NEW PAGE              
         MVI   FORCEHED,YESQ       NEW HEADINGS AND SET INDICATOR               
         OI    RUNIND2,RUN2JOB                                                  
         J     PDET12                                                           
*                                                                               
PDET10   TM    RUNIND2,RUN2EST     TEST IF WE'RE DOING ESTIMATES NOW            
         JNZ   PDET11                                                           
         CLI   EMAMODL,EMAEST      TEST IF WE HAVE MOVED TO ESTIMATES           
         JNE   PDET11              NOW                                          
         MVI   RCSUBPRG,5          YES WE HAVE SO START A NEW PAGE              
         MVI   FORCEHED,YESQ       NEW HEADINGS AND SET INDICATOR               
         OI    RUNIND2,RUN2EST                                                  
         J     PDET12                                                           
*                                                                               
PDET11   TM    RUNIND2,RUN2INV     TEST IF WE'RE DOING INVOICES NOW             
         JNZ   PDET12                                                           
         CLI   EMAMODL,EMAINV      TEST IF WE HAVE MOVED TO INVOICES            
         JNE   PDET12              NOW                                          
         MVI   RCSUBPRG,6          YES WE HAVE SO START A NEW PAGE              
         MVI   FORCEHED,YESQ       NEW HEADINGS AND SET INDICATOR               
         OI    RUNIND2,RUN2INV                                                  
         J     PDET12                                                           
*                                                                               
PDET12   OC    EMAPID2,EMAPID2                                                  
         JNZ   PDET13                                                           
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         LA    R2,EMRECL(R2)       IF NO EMAPID GO TO NEXT ONE                  
         ST    R2,ANXTETB                                                       
         JCT   R3,PDET02                                                        
         L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
*                                                                               
PDET13   XR    R4,R4                                                            
         L     R4,NUMAPPR                                                       
         MVC   LOWDTE,=X'FFFFFF'   FOR APPLSTNM, LENGTH IS AS IT                
         MVC   P,SPACES            INIT PRINTLINE                               
         MVC   P+1(L'EMAPID2),EMAPID2     IS SO THAT LONG NAMES DON'T           
         CLC   APPRPID,EMAPIN      TEST SAME AS PREVIOUS                        
         MVC   APPRPID,EMAPIN                                                   
         JE    PDET14              YES - USE EXISTING DETAILS                   
         NI    RUNIND1,X'FF'-(RUNINOCP+RUNITERM)                                
         GOTOR GETEML,1                                                         
PDET14   MVC   P+10(L'APPFSTNM),APPFSTNM  SPILL OVER OTHER COLUMNS              
         MVC   P+26(L'APPLSTNM-2*L'APPFSTNM),APPLSTNM                           
         MVC   P+86(L'AC@EHBST),AC@EHBST  EMAIL HAS BEEN SENT TO                
         OC    EMADDR,EMADDR       TEST EMAIL ADDRESS SET                       
         JNZ   PDET16                                                           
         LA    RF,AC@EANFF                                                      
         LHI   RE,L'AC@EANFF-1                                                  
         TM    RUNIND1,RUNINOCP    TEST EMAIL ADDRESS SET                       
         JZ    *+12                                                             
         LA    RF,AC@PIDTE                                                      
         LHI   RE,L'AC@PIDTE-1                                                  
         BASR  R1,0                                                             
         MVC   P+86(0),0(RF)       EMAIL ADDRESS NOT FOUND/TERMINATED           
         EX    RE,0(R1)                                                         
         J     PDET22                                                           
*                                                                               
PDET16   LA    RE,L'EMADDR                                                      
         LA    RF,EMADDR+L'EMADDR-1                                             
PDET18   CLI   0(RF),C' '                                                       
         JH    PDET20                                                           
         AHI   RF,-1                                                            
         JCT   RE,PDET18                                                        
         DC    H'0'                                                             
PDET20   BCTR  RE,0                                                             
         BASR  R1,0                                                             
         MVC   PSECOND+86(0),EMADDR                                             
         EX    RE,0(R1)                                                         
*                                                                               
PDET22   SAM31                                                                  
PDET23   L     R2,ANXTETB                                                       
         OC    EMDATE,EMDATE       DON'T WANT BLANK DATES                       
         JZ    PDET24                                                           
         CLC   LOWDTE,EMDATE       FIND EARLIEST DATE                           
         JNH   *+10                                                             
         MVC   LOWDTE,EMDATE                                                    
PDET24   LA    R4,1(R4)                                                         
         ST    R4,NUMAPPR                                                       
         LR    R5,R2                                                            
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
         CLC   R.EMAPIN(L'EMAPIN+L'EMASEQ),EMAPIN                               
         JNE   PDET26              SAME MODULE AND APPROVER?                    
         JCT   R3,PDET23           YES THEN LOOP                                
*                                                                               
PDET26   SAM24                                                                  
         L     R2,ASVEMREC                                                      
         EDIT  (B4,NUMAPPR),(10,P+62)                                           
         GOTO1 DATCON,DMCB,(1,LOWDTE),(13,P+73)                                 
*&&UK                                                                           
         MVC   HEAD2+01(L'AC@AGY),AC@AGY                                        
         MVC   HEAD2+12(L'CONAME),CONAME                                        
*&&                                                                             
         GOTO1 ACREPORT                                                         
         MVC   PSECOND,SPACES                                                   
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         JCT   R3,PDET02           KEEP GOING UNTIL NO MORE RECS                
         SAM24                                                                  
         L     R4,ADBOX                                                         
         MVI   BOXREQ,C'C'         CLOSE BOXES                                  
         MVI   FORCEHED,NOQ                                                     
         GOTO1 ACREPORT                                                         
         MVI   BOXYORN,NOQ         CLOSE BOXES                                  
*                                                                               
PRTDETX  SAM24                                                                  
         J     EXITE                                                            
         DROP  R2,R,R4                                                          
         EJECT                                                                  
***********************************************************************         
* DETAILED REPORT OF OUTSTANDING APPROVALS                            *         
***********************************************************************         
         SPACE 1                                                                
DETREP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*DETREP*'                                                      
         SAM24                                                                  
         USING PRTTIM,R4                                                        
         LA    R4,P                                                             
         CLI   SHOWDET,NOQ         DON'T SHOW REPORT                            
         JE    DETX2                                                            
         OC    NUMRECS,NUMRECS                                                  
         JZ    DETX2                                                            
         MVC   SVPCODE,SPACES                                                   
*                                                                               
         CLI   PARMDPQ,YESQ                                                     
         JE    DET00                                                            
*                                                                               
         L     RF,REMOTEC          REPORTING ON CLIENT PQ                       
         USING REMOTED,RF                                                       
         XC    REMOTKEY,REMOTKEY   START A NEW REPORT                           
         MVI   REMOTSYS,C'A'       A=ACCOUNTING                                 
         MVC   REMOTPRG,=C'EM'     THE 'EM' FROM PHASE NAME                     
         MVC   REMOTJID,REMOTSYS                                                
         MVC   REMOTDST,ORIGINUM   USE PRINCIPAL ID NUMBER FROM CO RECS         
         MVC   REMOTFRM,=C'DET'                                                 
         MVC   REMOTSUB+1(3),=C'DET'                                            
         MVI   FORCEHED,YESQ                                                    
DET00    MVI   RCSUBPRG,7                                                       
         CLI   DOWNREP,YESQ        DOWNLOAD REPORT?                             
         JNE   DET01                                                            
         OI    REMOTTYP,REMOTDLQ   SET REPORT TYPE TO DOWNLOAD.                 
         GOTOR SETDHED             SET DOWNLOAD REPORT HEADINGS                 
*                                                                               
         USING EMRECD,R2                                                        
R        USING EMRECD,R5                                                        
DET01    SAM31                                                                  
         L     R2,AEMLTAB                                                       
         ST    R2,ANXTETB                                                       
         L     R3,NUMRECS                                                       
         XR    R0,R0                                                            
         L     RF,VQSORT                                                        
         O     RF,=X'80000000'                                                  
         GOTO1 (RF),DMCB,(R2),(R3),EMRECL,EMLENG2,EMAPID-EMRECD                 
*                                  SORT ACCORDING TO PID THEN MODULES           
DET02    L     R2,ANXTETB                                                       
         OC    EMAPID,EMAPID       CHECK WHETHER EMAIL PID IS EMPTY             
         JNZ   DET03                                                            
         LA    R2,EMRECL(R2)                                                    
         JCT   R3,DET02                                                         
         J     DETX                                                             
*                                                                               
DET03    ST    R2,ANXTETB                                                       
         L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         CLC   APPRPID,EMAPIN                                                   
         MVC   APPRPID,EMAPIN                                                   
         JE    DET03A                                                           
         GOTOR GETEML,1                                                         
*                                                                               
DET03A   CLI   DOWNREP,YESQ                                                     
         JNE   DET03D                                                           
         CLI   EMAMODL,EMATIME             ONLY FOR TIMESHEETS                  
         JE    DET03B                      OR EXPENSES                          
         CLI   EMAMODL,EMAEXP                                                   
         JNE   DET03C                                                           
DET03B   CLC   SVPCODE,EMPERC              SAME PERSON AS BEFORE?               
         JE    DET03C                                                           
         GOTOR GETPERS,DMCB,EMRECD         STORAGE AREA                         
DET03C   GOTOR PRTDREP,DMCB,EMRECD         ADD DATA TO REPORT                   
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         LA    R2,EMRECL(R2)               BUMP TO NEXT ENTRY                   
         ST    R2,ANXTETB                                                       
         JCT   R3,DET02                                                         
         J     DETX                                                             
*                                                                               
DET03D   MVI   FORCEHED,YESQ               NEW PAGE FOR EACH APPROVER           
         MVC   PRTTHDAP(L'EMAPID),EMAPID       APPROVER PID                     
         MVC   PRTTHDFN(L'APPFSTNM),APPFSTNM   FIRST NAME                       
         MVC   PRTTHDLN(L'APPLSTNM),APPLSTNM   LAST NAME                        
         GOTOR MYREPORT                                                         
*                                                                               
DET04    MVC   P,SPACES                                                         
         GOTOR MYREPORT                                                         
DET05    CLI   EMAMODL,EMATIME             TIMESHEETS?                          
         JNE   DET14                                                            
         GOTOR CHKMAX,DMCB,10                                                   
         JNE   DET05                                                            
         LA    RF,AC@ETUC                                                       
         LA    R0,L'AC@ETUC                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@ETUC+L'AC@ETUC                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   PRTTCPJ(0),0(RF)                                                 
         EX    RE,0(R1)                                                         
         GOTOR MYREPORT                                                         
         GOTOR SETBOX,DMCB,EMAMODL         SET BOXES                            
         MVC   P,SPACES                                                         
         MVC   PRTTCPJ(L'AC@CLPJO),AC@CLPJO           CLI/PRO/JOB               
         MVI   PRTTCPJ+L'AC@CLPJO,C'/'                                          
         MVC   PRTTCPJ+1+L'AC@CLPJO(L'ONENUL),ONENUL  1N                        
         MVI   PRTTCPJ+1+L'AC@CLPJO+L'ONENUL,C'/'                               
         MVC   PRTTFNM(L'AC@CFNAM),AC@CFNAM           FIRST NAME                
         MVC   PRTTLNM(L'AC@CLNAM),AC@CLNAM           LAST NAME                 
         MVC   PRTTDAT(L'AC@DATE),AC@DATE             DATE                      
         GOTOR MYREPORT                                                         
         MVC   P,SPACES                                                         
         MVC   PRTTCPJ(L'AC@1RACC),AC@1RACC           1R ACCOUNT                
         GOTOR MYREPORT                                                         
         GOTOR PRTLINE                     PRINT HORIZONTAL LINE                
         J     DET07                                                            
*                                                                               
DET06    GOTOR CHKMAX,DMCB,1                                                    
         JNE   DET05                                                            
DET07    MVC   P,SPACES                                                         
         MVC   PRTTCPJ(L'EMULA),EMULA                                           
         CLC   SVPCODE,EMPERC              SAME PERSON AS BEFORE?               
         JE    DET08                                                            
         GOTOR GETPERS,DMCB,EMRECD                                              
DET08    MVC   PRTTFNM(L'PERFSTNM),PERFSTNM                                     
         MVC   PRTTLNM(L'PERLSTNM),PERLSTNM                                     
         MVC   WORK(L'EMDATE),EMDATE                                            
         GOTO1 DATCON,DMCB,(1,WORK),(13,PRTTDAT)                                
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR MYREPORT                                                         
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         LR    R5,R2                                                            
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
         CLC   R.EMAPIN(L'EMAPIN+L'EMASEQ),EMAPIN                               
         JE    DET10                       SAME MODULE AND PID?                 
         CLC   R.EMAPIN,EMAPIN             SAME PID?                            
         JE    DET12                                                            
         SAM24                                                                  
         MVC   P,SPACES                                                         
         GOTOR CLOBOX              CLOSE BOXES                                  
         GOTOR MYREPORT                                                         
         SAM31                                                                  
         JCT   R3,DET02            DIFFERENT PID                                
         J     DETX                                                             
*                                                                               
DET10    L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         JCT   R3,DET06            SAME MODULE                                  
         J     DETX                EOR                                          
*                                                                               
DET12    L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         MVC   P,SPACES                                                         
         GOTOR CLOBOX              CLOSE BOXES                                  
         GOTOR MYREPORT                                                         
         JCT   R3,DET04            SAME PID GO TO NEXT MODULE                   
         J     DETX                EOR                                          
*                                                                               
DET14    CLI   EMAMODL,EMAEXP             EXPENSES?                             
         JNE   DET26                                                            
         GOTOR CHKMAX,DMCB,10                                                   
         JNE   DET14                                                            
         LA    RF,AC@EXUC                                                       
         LA    R0,L'AC@EXUC                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@EXUC+L'AC@EXUC                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   PRTTCPJ(0),0(RF)                                                 
         EX    RE,0(R1)                                                         
         GOTOR MYREPORT                                                         
         GOTOR SETBOX,DMCB,EMAMODL        SET BOXES                             
         MVC   P,SPACES                                                         
*&&UK                                                                           
         MVC   PRTTCPJ(L'AC@CLPJO),AC@CLPJO   CLIENT/PRODUCT/JOB                
         MVI   PRTTCPJ+L'AC@CLPJO,C'/'                                          
*&&                                                                             
*&&US                                                                           
         MVC   PRTTCPJ(L'AC@EXPN),AC@EXPN     Expense Claim #                   
*&&                                                                             
         MVC   PRTTFNM(L'AC@CFNAM),AC@CFNAM   FIRST NAME                        
         MVC   PRTTLN2(L'AC@CLNAM),AC@CLNAM   LAST NAME                         
         MVC   PRTTCAM(L'AC@AMT),AC@AMT       AMOUNT                            
         MVC   PRTTDA2(L'AC@DATE),AC@DATE     DATE                              
         GOTOR MYREPORT                                                         
*&&UK                                                                           
         MVC   P,SPACES                                                         
         MVC   PRTTCPJ(L'AC@1RACC),AC@1RACC   1R ACCOUNT                        
         GOTOR MYREPORT                                                         
*&&                                                                             
         GOTOR PRTLINE                     PRINT HORIZONTAL LINE                
         J     DET17                                                            
*                                                                               
DET16    GOTOR CHKMAX,DMCB,1                                                    
         JNE   DET14                                                            
DET17    MVC   P,SPACES                                                         
*&&UK*&& MVC   PRTTCPJ(L'EMULA),EMULA                                           
*&&US*&& MVC   PRTTCPJ(L'EMORDN),EMORDN                                         
         CLC   SVPCODE,EMPERC      SAME PERSON AS BEFORE?                       
         JE    DET18                                                            
         GOTOR GETPERS,DMCB,EMRECD                                              
DET18    MVC   PRTTFNM(L'APPFSTNM),PERFSTNM                                     
         MVC   PRTTLN2(L'APPLSTNM),PERLSTNM                                     
         MVC   WORK(L'EMAMNT),EMAMNT                                            
         MVC   WORK+10(L'EMDATE),EMDATE                                         
         OC    EMAMNT,EMAMNT                                                    
         JZ    DET20                                                            
         CURED (P6,WORK),(L'PRTTCAM,PRTTCAM),2,COMMAS=YES,ZERO=BLANK,  *        
               ALIGN=RIGHT         DATE                                         
DET20    GOTO1 DATCON,DMCB,(1,WORK+10),(13,PRTTDA2)                             
         JE    *+6                                                              
         DC    H'0'                NO DATE FOUND                                
         GOTOR MYREPORT                                                         
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         LR    R5,R2                                                            
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
         CLC   R.EMAPIN(L'EMAPIN+L'EMASEQ),EMAPIN                               
         JE    DET22                        SAME MODULE AND PID?                
         CLC   R.EMAPIN,EMAPIN              SAME PID?                           
         JE    DET24                                                            
         SAM24                                                                  
         MVC   P,SPACES                                                         
         GOTOR CLOBOX              CLOSE BOXES                                  
         GOTOR MYREPORT                                                         
         SAM31                                                                  
         JCT   R3,DET02                                                         
         J     DETX                                                             
*                                                                               
DET22    L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         JCT   R3,DET16            SAME MODULE                                  
         J     DETX                                                             
*                                                                               
DET24    L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         MVC   P,SPACES                                                         
         GOTOR CLOBOX              CLOSE BOXES                                  
         GOTOR MYREPORT                                                         
         JCT   R3,DET04            SAME PID, MOVE TO NEXT MODULE                
         J     DETX                                                             
*                                                                               
DET26    CLI   EMAMODL,EMAORD                                                   
         JNE   DET42                                                            
         GOTOR CHKMAX,DMCB,10                                                   
         JNE   DET26                                                            
         LA    RF,AC@ORUC                                                       
         LA    R0,L'AC@ORUC                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@ORUC+L'AC@ORUC                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTOR MYREPORT                                                         
         GOTOR SETBOX,DMCB,EMAMODL        SET BOXES                             
         MVC   P,SPACES                                                         
         MVC   PRTTORD(L'AC@ORDC),AC@ORDC ORDER NUMBER                          
         MVC   PRTTDA3(L'AC@DATE),AC@DATE ORDER DATE                            
         MVC   PRTTSUP(L'AC@LSUPC),AC@LSUPC SUPPLIER CODE                       
         MVC   PRTTSUN(L'AC@LSUPN),AC@LSUPN SUPPLIER NAME                       
         MVC   PRTTOMT(L'AC@AMT),AC@AMT   AMOUNT                                
         MVC   PRTTRBD(L'AC@RQRDB),AC@RQRDB   REQUIRED BY                       
         GOTOR MYREPORT                                                         
         GOTOR PRTLINE                    PRINT HORIZONTAL LINE                 
         J     DET29                                                            
*                                                                               
DET28    GOTOR CHKMAX,DMCB,1                                                    
         JNE   DET26                                                            
DET29    MVC   P,SPACES                                                         
         MVC   PRTTORD(L'EMORDN),EMORDN   ORDER NUMBER/REQUISITION NO.          
         MVC   PRTTSUP(L'EMSUP),EMSUP     SUPPLIER ACCOUNT CODE                 
         GOTOR GETNAME,DMCB,EMSUP                                               
         LLC   RF,LACCNAM                                                       
         BASR  R1,0                                                             
         MVC   PRTTSUN(0),ACCNAM          SUPPLIER ACCOUNT NAME                 
         EX    RF,0(R1)                                                         
*                                                                               
DET34    OC    EMAMNT,EMAMNT                                                    
         JZ    DET36                                                            
         MVC   WORK(L'EMAMNT),EMAMNT                                            
         CURED (P6,WORK),(L'PRTTOMT,PRTTOMT),2,COMMAS=YES,ZERO=BLANK,  +        
               ALIGN=RIGHT                                                      
*                                                                               
DET36    MVC   WORK(L'EMDATE),EMDATE      ORDER DATE                            
         GOTO1 DATCON,DMCB,(1,WORK),(13,PRTTDA3)                                
         JE    *+6                                                              
         DC    H'0'                       NO DATE FOUND                         
*                                                                               
         OC    EMRBDAT,EMRBDAT     REQUIRED BY DATE (IF PRESENT)                
         JZ    DET36A                                                           
         GOTO1 DATCON,DMCB,(1,EMRBDAT),(13,PRTTRBD)                             
*                                                                               
DET36A   GOTOR MYREPORT                                                         
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         LR    R5,R2                                                            
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
         CLC   R.EMAPIN(L'EMAPIN+L'EMASEQ),EMAPIN                               
         JE    DET38               SAME MODULE AND PID?                         
         CLC   R.EMAPIN,EMAPIN     SAME PID?                                    
         JE    DET40                                                            
         SAM24                                                                  
         MVC   P,SPACES                                                         
         GOTOR CLOBOX              CLOSE BOXES                                  
         GOTOR MYREPORT                                                         
         SAM31                                                                  
         JCT   R3,DET02                                                         
         J     DETX                                                             
*                                                                               
DET38    L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         JCT   R3,DET28            SAME MODULE                                  
         J     DETX                                                             
*                                                                               
DET40    L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         MVC   P,SPACES                                                         
         GOTOR CLOBOX              CLOSE BOXES                                  
         GOTOR MYREPORT                                                         
         JCT   R3,DET04            SAME PID, MOVE TO NEXT MODULE                
         J     DETX                                                             
*                                                                               
DET42    CLI   EMAMODL,EMAJOB             JOBS                                  
         JNE   DET50                                                            
         GOTOR CHKMAX,DMCB,10                                                   
         JNE   DET42                                                            
         LA    RF,AC@JOUC                                                       
         LA    R0,L'AC@JOUC                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@JOUC+L'AC@JOUC                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTOR MYREPORT                                                         
         GOTOR SETBOX,DMCB,EMAMODL        SET BOXES                             
         MVC   P,SPACES                                                         
         MVC   PRTTJBC(L'AC@JOBC),AC@JOBC JOB CODE                              
         MVC   PRTTJBN(L'AC@JOBN),AC@JOBN JOB NAME                              
         MVC   PRTTDA4(L'AC@DATE),AC@DATE DATE                                  
         GOTOR MYREPORT                                                         
         GOTOR PRTLINE                    PRINT HORIZONTAL LINE                 
         J     DET45                                                            
*                                                                               
DET44    GOTOR CHKMAX,DMCB,1                                                    
         JNE   DET42                                                            
DET45    MVC   P,SPACES                                                         
         OC    EMULA,SPACES                                                     
         MVC   PRTTJBC(L'EMULA),EMULA     JOB CODE                              
         MVC   PRTTJBN(L'EMJOBN),EMJOBN   JOB NAME                              
         MVC   WORK(L'EMDATE),EMDATE      JOB ADDED DATE                        
         GOTO1 DATCON,DMCB,(1,WORK),(13,PRTTDA4)                                
         JE    *+6                                                              
         DC    H'0'                NO DATE FOUND                                
         GOTOR MYREPORT                                                         
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         LR    R5,R2                                                            
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
         CLC   R.EMAPIN(L'EMAPIN+L'EMASEQ),EMAPIN                               
         JE    DET46               SAME MODULE AND PID?                         
         CLC   R.EMAPIN,EMAPIN     SAME PID?                                    
         JE    DET48                                                            
         SAM24                                                                  
         MVC   P,SPACES                                                         
         GOTOR CLOBOX              CLOSE BOXES                                  
         GOTOR MYREPORT                                                         
         SAM31                                                                  
         JCT   R3,DET02                                                         
         J     DETX                                                             
*                                                                               
DET46    L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         JCT   R3,DET44            SAME MODULE                                  
         J     DETX                                                             
*                                                                               
DET48    L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         MVC   P,SPACES                                                         
         GOTOR CLOBOX              CLOSE BOXES                                  
         GOTOR MYREPORT                                                         
         JCT   R3,DET04            SAME PID, MOVE TO NEXT MODULE                
         J     DETX                                                             
*                                                                               
DET50    CLI   EMAMODL,EMAEST      ESTIMATES                                    
         JNE   DET64                                                            
         GOTOR CHKMAX,DMCB,10                                                   
         JNE   DET50                                                            
         LA    RF,AC@ESUC                                                       
         LA    R0,L'AC@ESUC                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@ESUC+L'AC@ESUC                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTOR MYREPORT                                                         
         GOTOR SETBOX,DMCB,EMAMODL SET BOXES                                    
         MVC   P,SPACES                                                         
         MVC   PRTTESN(L'AC@ESTNO),AC@ESTNO ESTIMATE NUMBER                     
         MVC   PRTTESM(L'AC@ESTNM),AC@ESTNM ESTIMATE NAME                       
         MVC   PRTTJBC2(L'AC@JOBC),AC@JOBC  JOB CODE                            
         MVC   PRTTJBN2(L'AC@JOBN),AC@JOBN  JOB NAME                            
         MVC   PRTTDA5(L'AC@DATE),AC@DATE   ESTIMATE ADDED DATE                 
         GOTOR MYREPORT                                                         
         GOTOR PRTLINE                      PRINT HORIZONTAL LINE               
         J     DET53                                                            
*                                                                               
DET52    GOTOR CHKMAX,DMCB,1                                                    
         JNE   DET50                                                            
DET53    MVC   P,SPACES                                                         
         MVC   PRTTESN(L'EMESTN),EMESTN     ESTIMATE NUMBER                     
         MVC   PRTTESM,EMESTNM              ESTIMATE NAME                       
         MVI   P+35,C' '                                                        
         MVC   PRTTJBC2(L'EMULA),EMULA      JOB CODE                            
         GOTOR GETNAME,DMCB,EMULA                                               
         LLC   RF,LACCNAM                                                       
         BASR  R1,0                                                             
         MVC   PRTTJBN2(0),ACCNAM           SUPPLIER ACCOUNT NAME               
         EX    RF,0(R1)                                                         
*                                                                               
DET58    MVC   WORK(L'EMDATE),EMDATE        ESTIMATE ADDED DATE                 
         GOTO1 DATCON,DMCB,(1,WORK),(13,PRTTDA5)                                
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR MYREPORT                                                         
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         LR    R5,R2                                                            
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
         CLC   R.EMAPIN(L'EMAPIN+L'EMASEQ),EMAPIN                               
         JE    DET60                        SAME MODULE AND PID?                
         CLC   R.EMAPIN(L'EMAPIN),EMAPIN    SAME PID?                           
         JE    DET62                                                            
         MVC   P,SPACES                                                         
         GOTOR CLOBOX              CLOSE BOXES                                  
         SAM24                                                                  
         GOTOR MYREPORT                                                         
         SAM31                                                                  
         JCT   R3,DET02            MOVE TO NEXT PID                             
         J     DETX                                                             
*                                                                               
DET60    L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         JCT   R3,DET52            SAME MODULE                                  
         J     DETX                                                             
         EJECT                                                                  
*                                                                               
DET62    L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         MVC   P,SPACES                                                         
         GOTOR CLOBOX              CLOSE BOXES                                  
         GOTOR MYREPORT                                                         
         JCT   R3,DET04            SAME PID, MOVE TO NEXT MODULE                
         J     DETX                                                             
*                                                                               
DET64    CLI   EMAMODL,EMAINV      INVOICES                                     
         JE    *+6                                                              
         DC    H'0'                ERROR UNKNOWN MODULE                         
         GOTOR CHKMAX,DMCB,10                                                   
         JNE   DET64                                                            
         LA    RF,AC@INUC                                                       
         LA    R0,L'AC@INUC                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@INUC+L'AC@INUC                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTOR MYREPORT                                                         
         GOTOR SETBOX,DMCB,EMAMODL     SET BOXES                                
         MVC   P,SPACES                                                         
         MVC   PRTTIRF(L'AC@ILGNO),AC@ILGNO LOG NO.                             
         MVC   PRTTINV(L'AC@INVN),AC@INVN   INVOICE NUMBER                      
         MVC   PRTTITY(L'AC@TYPE),AC@TYPE   (INVOICE) TYPE                      
         MVC   PRTTAM2(L'AC@AMT),AC@AMT     AMOUNT                              
         MVC   PRTTORN(L'AC@ORDER),AC@ORDER ORDER                               
         MVC   PRTTORR(L'ACSRQRDB),ACSRQRDB REQUIRED BY DATE                    
         MVC   PRTTDA6(L'AC@DATE),AC@DATE   DATE                                
         MVC   PRTTSUC(L'AC@LSUPC),AC@LSUPC SUPPLIER CODE                       
         MVC   PRTTSUN2(L'AC@LSUPN),AC@LSUPN SUPPLIER NAME                      
         GOTOR MYREPORT                                                         
         GOTOR PRTLINE                      PRINT HORIZONTAL LINE               
         J     DET67                                                            
*                                                                               
DET66    GOTOR CHKMAX,DMCB,1                                                    
         JNE   DET64                                                            
DET67    MVC   P,SPACES                                                         
         MVC   PRTTIRF(L'EMILN),EMILN       LOG NO.                             
         MVC   PRTTINV(L'EMAINTR),EMAINTR   INVOICE REFERENCE                   
         MVC   PRTTITY,AC@MIX                                                   
         CLI   EMAITYP,C'M'                                                     
         JE    DET67A                                                           
         MVC   PRTTITY,AC@EXPA                                                  
         CLI   EMAITYP,C'N'                                                     
         JE    DET67A                                                           
         MVC   PRTTITY,AC@PROC                                                  
*                                                                               
DET67A   OC    EMAMNT,EMAMNT                INVOICE AMOUNT                      
         JZ    DET68                                                            
         MVC   WORK(L'EMAMNT),EMAMNT                                            
         CURED (P6,WORK),(L'PRTTAM2,PRTTAM2),2,COMMAS=YES,ZERO=BLANK,  *        
               ALIGN=RIGHT                                                      
DET68    MVC   PRTTORN(L'EMIORDN),EMIORDN   ORDER NUMBER                        
         OC    EMRBDAT,EMRBDAT                                                  
         JZ    DET70                                                            
         GOTO1 DATCON,DMCB,(1,EMRBDAT),(13,PRTTORR)                             
DET70    MVC   PRTTSUC(L'EMSUP),EMSUP       SUPPLIER CODE                       
         GOTOR GETNAME,DMCB,EMSUP                                               
         LLC   RF,LACCNAM                                                       
         BASR  R1,0                                                             
         MVC   PRTTSUN2(0),ACCNAM           SUPPLIER ACCOUNT NAME               
         EX    RF,0(R1)                                                         
*                                                                               
DET74    MVC   WORK(L'EMDATE),EMDATE        DATE                                
         GOTO1 DATCON,DMCB,(1,WORK),(13,PRTTDA6)                                
         JE    *+6                                                              
         DC    H'0'                         NO DATE FOUND                       
*                                                                               
         LA    R5,EMAIORD+1        look at invoice order list                   
         LHI   R0,MAXORDQ                                                       
         MVI   BYTE,0                                                           
         CLC   EMIORDN(L'RBDRORD),SPACES                                        
         JH    DET74B                                                           
         CLC   EMAIORD+1(L'RBDRORD),SPACES                                      
         JNH   DET74B                                                           
         MVC   PRTTORN(L'RBDRORD),EMAIORD+1                                     
         CLC   EMAIORD+1+L'RBDRORD(L'ORDRQBD),SPACES                            
         JNH   DET74A                                                           
         GOTO1 DATCON,DMCB,(1,EMAIORD+1+L'RBDRORD),(13,PRTTORR)                 
DET74A   AHI   R5,L'RBDRORD+L'ORDRQBD                                           
         SHI   R0,1                                                             
*                                                                               
DET74B   GOTOR MYREPORT                                                         
*                                  process invoice order list                   
DET74C   CLC   0(L'RBDRORD,R5),SPACES                                           
         JNH   DET74F                                                           
         CLI   BYTE,0                                                           
         JNE   DET74D                                                           
         MVI   BYTE,1                                                           
         OC    EMDTUPD,EMDTUPD     any added date?                              
         JZ    DET74D                                                           
         GOTO1 DATCON,DMCB,(2,EMDTUPD),(13,PRTTDA6)                             
DET74D   MVC   PRTTORN(L'RBDRORD),0(R5)                                         
         CLC   L'RBDRORD(L'ORDRQBD,R5),SPACES                                   
         JNH   DET74E                                                           
         GOTO1 DATCON,DMCB,(1,L'RBDRORD(R5)),(13,PRTTORR)                       
DET74E   GOTOR MYREPORT                                                         
         AHI   R5,L'RBDRORD+L'ORDRQBD                                           
         JCT   R0,DET74C                                                        
         CLI   EMAIORD,C'+'        are there more on the invoice?               
         JNE   DET74G                                                           
         MVI   PRTTORN,C'+'                                                     
         GOTOR MYREPORT            then say so                                  
         J     DET74G                                                           
DET74F   CLI   BYTE,1              already processed?                           
         JE    DET74G                                                           
         OC    EMDTUPD,EMDTUPD     any added date?                              
         JZ    DET74G                                                           
         GOTO1 DATCON,DMCB,(2,EMDTUPD),(13,PRTTDA6)                             
         GOTOR MYREPORT                                                         
*                                                                               
DET74G   DS    0H                                                               
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         LR    R5,R2                                                            
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
         L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         CLC   R.EMAPIN(L'EMAPIN+L'EMASEQ),EMAPIN                               
         JE    DET76               SAME MODULE AND PID?                         
         CLC   R.EMAPIN,EMAPIN     SAME PID?                                    
         JE    DET78                                                            
         SAM24                                                                  
         MVC   P,SPACES                                                         
         GOTOR CLOBOX              CLOSE BOXES                                  
         GOTOR MYREPORT                                                         
         SAM31                                                                  
         JCT   R3,DET02                                                         
         J     DETX                                                             
*                                                                               
DET76    L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         JCT   R3,DET66            SAME MODULE                                  
         J     DETX                                                             
*                                                                               
DET78    L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         MVC   P,SPACES                                                         
         GOTOR CLOBOX              CLOSE BOXES                                  
         GOTOR MYREPORT                                                         
         JCT   R3,DET04            SAME PID, MOVE TO NEXT MODULE                
         J     DETX                                                             
*                                                                               
DETX     CLI   DOWNREP,YESQ        DOWNLOAD REPORT?                             
         JNE   DETX2                                                            
         SAM24                                                                  
         GOTOR DOWN,DMCB,DOWNCLOS CLOSE REPORT                                  
*                                                                               
DETX2    SAM24                                                                  
         J     EXITE                                                            
         DROP  R2,R4,R                                                          
         EJECT                                                                  
***********************************************************************         
* SET HEADINGS AND CALL ACREPORT                                      *         
***********************************************************************         
         SPACE 1                                                                
MYREPORT ST    RE,FULL                                                          
*&&UK                                                                           
         MVC   HEAD2+01(L'AC@AGY),AC@AGY                                        
         MVC   HEAD2+12(L'CONAME),CONAME                                        
*&&                                                                             
         GOTO1 ACREPORT                                                         
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET BOXES FOR DETAILED REPORT                                       *         
* ON NTRY P1=MODULE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING BOXD,R4                                                          
SETBOX   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETBOX*'                                                      
         L     R5,0(R1)                                                         
         L     R4,ADBOX                                                         
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXCOLS+(PRTTCL1-PRTTIM),C'L'                                    
         MVI   BOXROWS,C'T'                                                     
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXREQ,C'O'          OPEN BOXES AGAIN                            
         CLI   0(R5),EMATIME        TIME MODULE                                 
         JE    SETBOX2                                                          
         CLI   0(R5),EMAORD         ORDER MODULE                                
         JE    SETBOX4                                                          
         CLI   0(R5),EMAEXP         EXPENSES MODULE                             
         JE    SETBOX6                                                          
         CLI   0(R5),EMAEST         ESTIMATES MODULE                            
         JE    SETBOX8                                                          
         CLI   0(R5),EMAJOB         JOBS MODULE                                 
         JE    SETBOX10                                                         
         CLI   0(R5),EMAINV         INVOICES MODULE                             
         JE    SETBOX12                                                         
         DC    H'0'                                                             
*                                                                               
SETBOX2  MVI   BOXCOLS+(PRTTCL2-PRTTIM),C'C'  TIMESHEETS                        
         MVI   BOXCOLS+(PRTTCL3-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCL4-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCL5-PRTTIM),C'R'                                    
         J     SETBOXX                                                          
*                                                                               
SETBOX4  MVI   BOXCOLS+(PRTTCL9-PRTTIM),C'C'  ORDERS                            
         MVI   BOXCOLS+(PRTTCLA-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCLB-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCLC-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCLD-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCLD1-PRTTIM),C'R'                                   
         J     SETBOXX                                                          
*                                                                               
SETBOX6  MVI   BOXCOLS+(PRTTCL2-PRTTIM),C'C'  EXPENSES                          
         MVI   BOXCOLS+(PRTTCL3-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCL6-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCL7-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCL8-PRTTIM),C'R'                                    
         J     SETBOXX                                                          
*                                                                               
SETBOX8  MVI   BOXCOLS+(PRTTCLH-PRTTIM),C'C'  ESTIMATES                         
         MVI   BOXCOLS+(PRTTCLI-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCLJ-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCLK-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCLL-PRTTIM),C'R'                                    
         J     SETBOXX                                                          
*                                                                               
SETBOX10 MVI   BOXCOLS+(PRTTCLE-PRTTIM),C'C'  JOBS                              
         MVI   BOXCOLS+(PRTTCLF-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCLG-PRTTIM),C'R'                                    
         J     SETBOXX                                                          
*                                                                               
SETBOX12 MVI   BOXCOLS+(PRTTCLM-PRTTIM),C'C'  INVOICES                          
         MVI   BOXCOLS+(PRTTCLN-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCLN1-PRTTIM),C'C'                                   
         MVI   BOXCOLS+(PRTTCLO-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCLP-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCLP1-PRTTIM),C'C'                                   
         MVI   BOXCOLS+(PRTTCLQ-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCLR-PRTTIM),C'C'                                    
         MVI   BOXCOLS+(PRTTCLS-PRTTIM),C'R'                                    
         J     SETBOXX                                                          
*                                                                               
SETBOXX  MVI   BOXYORN,YESQ        ESTABLISH BOXES                              
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         GOTO1 ACREPORT                                                         
         J     EXITE                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* CLOSE BOXES                                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING BOXD,R4                                                          
CLOBOX   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CLOBOX*'                                                      
         L     R4,ADBOX                                                         
         MVI   BOXREQ,C'C'                                                      
         MVI   FORCEHED,NOQ                                                     
         J     EXITE                                                            
         EJECT                                                                  
         DROP  R4                                                               
***********************************************************************         
* PRINT A HORIZONTAL LINE                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING BOXD,R4                                                          
PRTLINE  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PRTLIN*'                                                      
         L     R4,ADBOX                                                         
         MVI   BOXREQ,C'B'                                                      
         J     EXITE                                                            
         EJECT                                                                  
         DROP  R4                                                               
***********************************************************************         
* WILL WE HIT MAX LINES?                                              *         
* ON NTRY P1=NUMBER OF LINES TO CHECK                                 *         
***********************************************************************         
         SPACE 1                                                                
CHKMAX   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKMAX*'                                                      
         XR    RF,RF                                                            
         L     R0,0(R1)                                                         
         IC    RF,LINE                                                          
         AR    RF,R0                                                            
         CLM   RF,1,MAXLINES              WILL WE HIT MAX LINES?                
         JL    CHKMAXOK                   NO WE'RE FINE                         
         GOTOR CLOBOX                                                           
         MVI   FORCEHED,YESQ                                                    
         GOTOR MYREPORT                   START NEW HEADINGS                    
         J     CHKMAXNO                   AND REPRINT PERSON ID                 
*                                                                               
CHKMAXOK J     EXITE                                                            
*                                                                               
CHKMAXNO J     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* SET TIMESHEETS HEADING FOR ERROR REPORT                             *         
***********************************************************************         
         SPACE 1                                                                
SETTHED  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETTHE*'                                                      
         TM    RUNIND4,RUNTHED            ALREADY PRINTED HEADING?              
         JNZ   EXITE                                                            
         MVC   P,SPACES                                                         
         LA    RF,AC@ETUC                                                       
         LA    R0,L'AC@ETUC                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@ETUC+L'AC@ETUC                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTO1 ACREPORT                                                         
         MVC   P,SPACES                                                         
         LA    RF,AC@ETUL                                                       
         LA    R0,L'AC@ETUL                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@ETUL+L'AC@ETUL                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTO1 ACREPORT                                                         
         OI    RUNIND4,RUNTHED                                                  
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* SET EXPENSES HEADING FOR ERROR REPORT                               *         
***********************************************************************         
         SPACE 1                                                                
SETEHED  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETEHE*'                                                      
         TM    RUNIND4,RUNEHED            ALREADY PRINTED HEADING?              
         JNZ   EXITE                                                            
         MVC   P,SPACES                                                         
         LA    RF,AC@EXUC                                                       
         LA    R0,L'AC@EXUC                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@EXUC+L'AC@EXUC                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTO1 ACREPORT                                                         
         MVC   P,SPACES                                                         
         LA    RF,AC@EXUL                                                       
         LA    R0,L'AC@EXUL                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@EXUL+L'AC@EXUL                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         OI    RUNIND4,RUNEHED                                                  
         GOTO1 ACREPORT                                                         
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* SET ORDER HEADINGS FOR ERROR REPORT                                 *         
***********************************************************************         
         SPACE 1                                                                
SETOHED  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETOHE*'                                                      
         TM    RUNIND4,RUNOHED            ALREADY PRINTED HEADING?              
         JNZ   EXITE                                                            
         MVC   P,SPACES                                                         
         LA    RF,AC@ORUC                                                       
         LA    R0,L'AC@ORUC                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@ORUC+L'AC@ORUC                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTO1 ACREPORT                                                         
         MVC   P,SPACES                                                         
         LA    RF,AC@ORUL                                                       
         LA    R0,L'AC@ORUL                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@ORUL+L'AC@ORUL                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         OI    RUNIND4,RUNOHED                                                  
         GOTO1 ACREPORT                                                         
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* SET JOB HEADINGS FOR ERROR PAGE                                     *         
***********************************************************************         
         SPACE 1                                                                
SETJHED  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETJHE*'                                                      
         TM    RUNIND4,RUNJHED            ALREADY PRINTED JOB HEADING           
         JNZ   EXITE                                                            
         MVC   P,SPACES                                                         
         LA    RF,AC@JOUC                                                       
         LA    R0,L'AC@JOUC                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@JOUC+L'AC@JOUC                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTO1 ACREPORT                                                         
         MVC   P,SPACES                                                         
         LA    RF,AC@JOUL                                                       
         LA    R0,L'AC@JOUL                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@JOUL+L'AC@JOUL                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTO1 ACREPORT                                                         
         OI    RUNIND4,RUNJHED                                                  
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* SET ESTIMATE HEADINGS FOR ERROR PAGE                                *         
***********************************************************************         
         SPACE 1                                                                
SETSHED  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETSHE*'                                                      
         TM    RUNIND4,RUNSHED            ALREADY PRINTED HEADING?              
         JNZ   EXITE                                                            
         MVC   P,SPACES                                                         
         LA    RF,AC@ESUC                                                       
         LA    R0,L'AC@ESUC                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@ESUC+L'AC@ESUC                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTO1 ACREPORT                                                         
         MVC   P,SPACES                                                         
         LA    RF,AC@ESUL                                                       
         LA    R0,L'AC@ESUL                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@ESUL+L'AC@ESUL                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTO1 ACREPORT                                                         
         OI    RUNIND4,RUNSHED                                                  
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* SET INVOICE HEADERS FOR ERROR REPORT                                *         
***********************************************************************         
         SPACE 1                                                                
SETIHED  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETIHE*'                                                      
         TM    RUNIND4,RUNIHED            ALREADY PRINTED HEADING?              
         JNZ   EXITE                                                            
         MVC   P,SPACES                                                         
         LA    RF,AC@INUC                                                       
         LA    R0,L'AC@INUC                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@INUC+L'AC@INUC                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTO1 ACREPORT                                                         
         MVC   P,SPACES                                                         
         LA    RF,AC@INUL                                                       
         LA    R0,L'AC@INUL                                                     
         GOTOR SNIPPER,0(RF)                                                    
         LA    RE,AC@INUL+L'AC@INUL                                             
         SR    RE,RF                                                            
         AHI   RE,-1                                                            
         BASR  R1,0                                                             
         MVC   P+2(0),0(RF)                                                     
         EX    RE,0(R1)                                                         
         GOTO1 ACREPORT                                                         
         OI    RUNIND4,RUNIHED                                                  
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* REMOVE LEADING SPACES                                               *         
***********************************************************************         
         SPACE 1                                                                
SNIPPER  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SNIPPE*'                                                      
         LR    RF,R1                                                            
         CLI   0(RF),C' '                                                       
         JH    *+12                                                             
         AHI   RF,1                                                             
         JCT   R0,*-12                                                          
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* HEADER HOOKLINE                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BOXD,R4                                                          
HOOK     NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**HOOK**'                                                      
         SAM24                                                                  
         L     R4,ADBOX                                                         
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         CLI   RCSUBPRG,0          SKIP IF DETAILED REPORT OR ERROR             
         JE    HOOKX               PAGE                                         
         CLI   RCSUBPRG,7                                                       
         JNL   HOOKX                                                            
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXCOLS+(PRTCL1-PRTREP),C'L'                                     
         MVI   BOXCOLS+(PRTCL2-PRTREP),C'C'                                     
         MVI   BOXCOLS+(PRTCL3-PRTREP),C'C'                                     
         MVI   BOXCOLS+(PRTCL4-PRTREP),C'C'                                     
         MVI   BOXCOLS+(PRTCL5-PRTREP),C'C'                                     
         MVI   BOXCOLS+(PRTCL6-PRTREP),C'C'                                     
         MVI   BOXCOLS+(PRTCL7-PRTREP),C'R'                                     
*                                                                               
HOOK2    MVI   BOXYORN,YESQ        ESTABLISH BOXES                              
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,NOQ                                                     
*                                                                               
HOOKX    J     EXITE                                                            
         EJECT                                                                  
         DROP  R4                                                               
***********************************************************************         
* READ FOR COST PROFILE                                               *         
***********************************************************************         
         SPACE 1                                                                
CSTPRF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CSTPRF*'                                                      
         L     R0,ACOBLOCK                                                      
         LA    R1,COBLOCKX-COBLOCK                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         MVC   COADM,DATAMGR       PASS A(DATA MANAGER)                         
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,COCODE                                                    
         MVC   COKMTHD,SPACES                                                   
         LA    RF,ONERCODE                                                      
         LLC   R1,ONERL1L                                                       
         AHI   R1,-1                                                            
         BASR  RE,0                                                             
         MVC   COKOFC(0),0(RF)                                                  
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         AR    RF,R1               Bump to next spot in ONERCODE                
         LLC   RE,ONERL2L                                                       
         LLC   R1,ONERL1L                                                       
         SR    RE,R1                                                            
         LR    R1,RE                                                            
         AHI   R1,-1                                                            
         BASR  RE,0                                                             
         MVC   COKDPT(0),0(RF)                                                  
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         AR    RF,R1               Bump to next spot in ONERCODE                
         LLC   RE,ONERL3L                                                       
         LLC   R1,ONERL2L                                                       
         SR    RE,R1                                                            
         LR    R1,RE                                                            
         AHI   R1,-1                                                            
         BASR  RE,0                                                             
         MVC   COKSDT(0),0(RF)                                                  
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         AR    RF,R1               Bump to next spot in ONERCODE                
         LLC   RE,ONERL4L                                                       
         LLC   R1,ONERL3L                                                       
         SR    RE,R1                                                            
         LR    R1,RE                                                            
         AHI   R1,-1                                                            
         BASR  RE,0                                                             
         MVC   COKPER(0),0(RF)                                                  
         EX    R1,0(RE)                                                         
         GOTO1 VGETCAP,DMCB,ACOBLOCK                                            
         CLI   COSTATUS,0                                                       
         JE    EXITE                                                            
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DETAILED REPORT OF LOCKED APPROVALS                                 *         
***********************************************************************         
         SPACE 1                                                                
LCKREP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*LCKREP*'                                                      
*        CLI   SHOWLCK,NOQ         DON'T SHOW REPORT                            
*        JE    LCKREPX                                                          
         OC    NUMRECS,NUMRECS                                                  
         JZ    LCKREPX                                                          
         XC    APPRPID,APPRPID     Clear PID field for later compare            
*                                                                               
         CLI   PARMDPQ,YESQ                                                     
         JE    LCKR010                                                          
*                                                                               
         L     RF,REMOTEC          REPORTING ON CLIENT PQ                       
         USING REMOTED,RF                                                       
         XC    REMOTKEY,REMOTKEY   START A NEW REPORT                           
         MVI   REMOTSYS,C'A'       A=ACCOUNTING                                 
         MVC   REMOTPRG,=C'EM'     THE 'EM' FROM PHASE NAME                     
         MVC   REMOTJID,REMOTSYS                                                
         MVC   REMOTDST,ORIGINUM   USE PRINCIPAL ID NUMBER FROM CO RECS         
         MVC   REMOTFRM,=C'LCK'                                                 
         MVC   REMOTSUB+1(3),=C'LCK'                                            
LCKR010  MVI   RCSUBPRG,8                                                       
*                                                                               
         USING EMRECD,R2                                                        
         SAM31                                                                  
         L     R2,AEMLTAB                                                       
         L     R3,NUMRECS                                                       
         ST    R2,ANXTETB                                                       
         L     RF,VQSORT                                                        
         O     RF,=X'80000000'                                                  
         GOTO1 (RF),DMCB,(R2),(R3),EMRECL,EMLENG2,EMAPID-EMRECD                 
         L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
*                                  SORT ACCORDING TO PID THEN MODULES           
         MVI   FORCEHED,YESQ                                                    
LCKR020  OC    EMAPID,EMAPID       CHECK WHETHER EMAIL PID IS EMPTY             
         JZ    LCKR110                                                          
         CLI   EMAMODL,EMATIME     TIMESHEETS                                   
         JNE   LCKR110                                                          
         TM    EMASTAT,EMALCK      LOCK?                                        
         JNO   LCKR110                                                          
*                                                                               
         CLC   APPRPID,EMAPIN      ALREADY PROCESSED?                           
         JE    LCKR110                                                          
         MVC   APPRPID,EMAPIN                                                   
         GOTOR GETEML,1                                                         
         MVC   P+1(L'EMAPID),EMAPID        APPROVER PID                         
         MVC   P+10(L'APPFSTNM),APPFSTNM   FIRST NAME                           
         MVC   P+26(L'APPLSTNM),APPLSTNM   LAST NAME                            
         MVC   P+84(6),=CL6'LOCKED'        USER HAS BEEN LOCKED                 
*                                                                               
         MVC   ONERCODE,SPACES                                                  
*                                                                               
         USING PIDRECD,R4                                                       
         LA    R4,IOKEY3                                                        
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,COCODE                                                   
         MVC   PIDKPID,APPRPID                                                  
         MVI   PIDKSTYP,PIDKPERQ                                                
         MVC   CSVKEY3,PIDKEY                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,PIDKEY,PIDKEY,0                       
         MVC   SVDA,PIDKDA         Save off Disk Address for PUTREC             
*                                                                               
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,SVDA,AIOAREA,DMWORK                    
         JE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
*                                                                               
         USING PERRECD,R4                                                       
         L     R4,AIOAREA                                                       
         LA    R5,PERRFST                                                       
         USING LOCELD,R5                                                        
LCKR030  CLI   LOCEL,0                                                          
         JE    LCKR060                                                          
         CLI   LOCEL,LOCELQ                                                     
         JE    LCKR050                                                          
LCKR040  XR    R1,R1                                                            
         IC    R1,LOCLN                                                         
         AR    R5,R1                                                            
         J     LCKR030                                                          
*                                                                               
LCKR050  CLC   LOCSTART,TODAYP     Find current location                        
         JH    LCKR040                                                          
         OC    LOCEND,LOCEND                                                    
         JZ    *+14                                                             
         CLC   LOCEND,TODAYP                                                    
         JL    LCKR040                                                          
         LA    RF,ONERCODE         Fill out APProvers 1R account                
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),LOCOFF      Move in Office                               
         AHI   RE,1                                                             
         AR    RF,RE                                                            
         LLC   R1,ONERL1L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RE,R1                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),LOCDEPT     Move in Department                           
         AHI   RE,1                                                             
         AR    RF,RE                                                            
         LLC   R1,ONERL2L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RE,R1                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),LOCSUB      Move in Sub-Department                       
         AHI   RE,1                                                             
         AR    RF,RE                                                            
         LLC   R1,ONERL3L                                                       
         LLC   RE,ONERL4L                                                       
         SR    RE,R1                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),PERKCODE    Move in person                               
         DROP  R4,R5                                                            
*                                                                               
LCKR060  CLC   ONERCODE,SPACES     ANY CODE?                                    
         JE    LCKR110                                                          
         CLI   PARMD1R,YESQ        OVERRIDE TO PUT OUT 1R ACCOUNT               
         JNE   *+10                                                             
         MVC   PSECOND+1(L'ONERCODE),ONERCODE                                   
         GOTOR MYREPORT                                                         
         MVC   PSECOND,SPACES                                                   
*                                                                               
         USING ACTRECD,R4                                                       
         LA    R4,IOKEY3                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COCODE                                                   
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   ACTKACT,ONERCODE                                                 
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),ACCDIR,ACTKEY,ACTKEY,0               
         MVC   SVDA,ACTKDA         Save off Disk Address for PUTREC             
*                                                                               
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,SVDA,AIOAREA,DMWORK                    
         JE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
*                                                                               
         L     R4,AIOAREA                                                       
         LA    R5,ACTRFST                                                       
         USING RSTELD,R5                                                        
         XR    R1,R1                                                            
LCKR070  CLI   RSTEL,0                                                          
         JE    LCKR100                                                          
         CLI   RSTEL,RSTELQ                                                     
         JE    LCKR090                                                          
LCKR080  IC    R1,RSTLN                                                         
         AR    R5,R1                                                            
         J     LCKR070                                                          
*                                                                               
LCKR090  CLI   RSTLN,RSTLN3Q       Make sure that element is big enough         
         JL    LCKR080                                                          
         OI    RSTSTAT7,RSTLCKAP   Turn on LOCKED bit                           
         J     LCKR080                                                          
*                                                                               
LCKR100  CLI   RCWRITE,NOQ                                                      
         JE    LCKR110                                                          
         CLI   RCRUN,RUNTST        IS this a TEST RUN?                          
         JE    LCKR110                                                          
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',ACCMST,SVDA,AIOAREA,DMWORK             
*                                                                               
LCKR110  SAM31                                                                  
         L     R2,ANXTETB                                                       
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
         L     RF,ASVEMREC         MOVE EMRECD TO 24 BIT STORAGE!               
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD                                              
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         JCT   R3,LCKR020                                                       
*                                                                               
LCKREPX  SAM24                                                                  
         J     EXITE                                                            
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* READ ACCOUNT RECORD AND GET NAME                                    *         
* ON NTRY P1=ACCOUNT CODE                                             *         
* ON EXIT ACCNAM  CONTAINS ACCOUNT NAME AND LACCNAM CONTAINS LENGTH   *         
***********************************************************************         
         SPACE 1                                                                
GETNAME  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETNAM*'                                                      
         L     RF,0(R1)                                                         
         MVC   ACCNAM,SPACES                                                    
         XC    LACCNAM,LACCNAM                                                  
         LA    R4,IOKEY2                                                        
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COCODE                                                   
         MVC   ACTKULA,0(RF)                                                    
         OC    ACTKULA,SPACES                                                   
         MVC   CSVKEY2,IOKEY2                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,ACTKEY,ACTKEY,0                       
         CLC   CSVKEY2(ACTKEND),IOKEY2                                          
         JNE   GETNAMN                                                          
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,ACTKDA,AIOAREA,DMWORK                  
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIOAREA                                                       
         LA    R4,ACTRFST                                                       
         XR    RE,RE                                                            
*                                                                               
         USING NAMELD,R4                                                        
GETNAM2  CLI   NAMEL,0                                                          
         JE    GETNAMN                                                          
         CLI   NAMEL,NAMELQ                                                     
         JE    GETNAM4                                                          
         IC    RE,NAMLN                                                         
         AR    R4,RE                                                            
         J     GETNAM2                                                          
*                                                                               
GETNAM4  XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q                                                       
         BCTR  RF,0                                                             
         OC    NAMEREC,SPACES                                                   
         STC   RF,LACCNAM                                                       
         BASR  RE,0                                                             
         MVC   ACCNAM(0),NAMEREC SUPPLIER ACCOUNT NAME                          
         EX    RF,0(RE)                                                         
         J     EXITE                                                            
*                                                                               
GETNAMN  J     EXITL                                                            
         EJECT                                                                  
         DROP  R4                                                               
***********************************************************************         
* SET DOWNLOAD REPORT HEADINGS                                        *         
***********************************************************************         
         SPACE 1                                                                
SETDHED  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETDHE*'                                                      
         GOTOR DOWN,DMCB,DOWNINI                                                
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@RSPID,AC@RSPID),(R2) PID                 
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@CFNAM,AC@CFNAM),(R2) FIRST NAME          
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@CLNAM,AC@CLNAM),(R2) LAST NAME           
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@OFFC,AC@OFFC),(R2)    OFFICE             
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@AGALF,AC@AGALF),(R2) ALPHA ID            
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@RGSID,AC@RGSID),(R2) REGISTER            
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@MOD,AC@MOD),(R2)      MODULE             
         MVC   TEXT2,SPACES                                                     
         MVC   TEXT2,AC@CLPJO                                                   
         LA    RE,TEXT2+L'AC@CLPJO                                              
         LA    RF,TEXT2                                                         
         CLI   0(RE),C' '                                                       
         JH    *+10                                                             
         JCT   RE,*-8                                                           
         DC    H'0'                                                             
         MVI   0(RE),C'/'                                                       
         AHI   RE,1                                                             
         MVC   0(L'ONENUL2,RE),ONENUL2                                          
         AHI   RE,L'ONENUL2                                                     
         MVI   0(RE),C'/'                                                       
         AHI   RE,1                                                             
         MVC   0(L'ONERUL2,RE),ONERUL2                                          
         AHI   RE,L'ONERUL2                                                     
         SR    RE,RF                                                            
         STC   RE,BYTE                                                          
         GOTOR DOWN,DMCB,DOWNADD,(BYTE,TEXT2),(R2)    CLI/PRO/JOB/1N/1R         
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@TSFN,AC@TSFN),(R2)   FIRST NAME          
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@TSLN,AC@TSLN),(R2)   LAST NAME           
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@JOBN,AC@JOBN),(R2)   JOB NAME            
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@ILGNO,AC@ILGNO),(R2) INV LOG NO          
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@INVN,AC@INVN),(R2)   INV. NO.            
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@TYPE,AC@TYPE),(R2)   INV. TYPE           
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@INVUP,AC@INVUP),(R2) INV. UPD.           
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@ESTNO,AC@ESTNO),(R2) EST NO.             
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@ORDC,AC@ORDC),(R2)   ORDER NO.           
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@RQRDB,AC@RQRDB),(R2) REQ BY DATE         
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@ESTNM,AC@ESTNM),(R2) EST NAME            
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@LSUPC,AC@LSUPC),(R2) SUPP CODE           
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@LSUPN,AC@LSUPN),(R2) SUPP NAME           
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@DATE,AC@DATE),(R2)   DATE                
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@AMT,AC@AMT),(R2)     AMOUNT              
         GOTOR DOWN,DMCB,DOWNEOL,,0(R2)                                         
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ENCLOSE A PIECE OF TEXT IN QUOTES AND SEE IF IT WILL FIT *         
* ON THE PRINT LINE FOR A DOWNLOAD REPORT                             *         
* NTR1   P1=REQUEST TYPE                                              *         
*        P2=LENGTH OF FIELD TO BE ADDED, A(FIELD TO BE ADDED)         *         
*        P3=ADDRESS OF NEXT FREE SPACE ON PRINT LINE                  *         
*        R2=ADDRESS OF NEXT FREE SPACE ON PRINT LINE                  *         
***********************************************************************         
         SPACE 1                                                                
DOWN     NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**DOWN**'                                                      
         L     RF,0(R1)                                                         
         STCM  RF,1,BYTE                                                        
         XR    R4,R4                                                            
         IC    R4,4(R1)            R4=L'FIELD                                   
         L     R5,4(R1)            R5=A(FIELD DATA)                             
         L     R2,8(R1)            R2=ADDR OF NEXT FREE SPACE ON LINE           
*                                                                               
         CLI   BYTE,DOWNEWL        NEW LINE                                     
         JE    DOWN01                                                           
         CLI   BYTE,DOWNEOL        START A NEW LINE                             
         JE    DOWN00                                                           
         CLI   BYTE,DOWNINI        INITIALISE                                   
         JNE   DOWN02                                                           
         MVI   CLEARHED,NOQ                                                     
         MVI   RCSUBPRG,X'FF'                                                   
         MVI   SKIPSPEC,NOQ                                                     
         MVI   NEWPAGE,YESQ                                                     
         MVC   P,SPACES                                                         
         GOTO1 ACREPORT                                                         
         MVC   P+1(L'AC@AGY),AC@AGY AGENCY                                      
         MVI   P+1+L'AC@AGY,C':'                                                
         MVC   P+3+L'AC@AGY(L'CONAME),CONAME                                    
         GOTO1 ACREPORT             DATE                                        
         MVC   P+1(L'AC@DATE),AC@DATE                                           
         MVI   P+1+L'AC@DATE,C':'                                               
         MVC   P+3+L'AC@DATE(L'TODAY),TODAY                                     
         MVC   PSECOND+51(L'AC@ANBE),AC@ANBE REPORT HEADING                     
         GOTO1 ACREPORT                                                         
         MVC   P+51(L'AC@ANBU),AC@ANBU REPORT HEADING                           
         OI    RUNIND2,RUN2DHED    SET DOING HEADER                             
         GOTO1 ACREPORT                                                         
         MVI   FORCEHED,YESQ                                                    
         J     DOWN01                                                           
*                                                                               
DOWN00   TM    RUNIND2,RUN2DHED                                                 
         JNZ   DOWN0A                                                           
         MVI   CLEARHED,YESQ                                                    
         MVI   RCSUBPRG,X'FF'                                                   
         MVI   SKIPSPEC,YESQ                                                    
         MVI   NEWPAGE,NOQ                                                      
*                                                                               
DOWN0A   SHI   R2,1                                                             
         MVI   0(R2),C';'          END OLD LINE                                 
         NI    RUNIND2,X'FF'-RUN2DHED TURN OFF HEADER                           
         NI    RUNIND5,X'FF'-RUN5LI2  TURN OFF PSECOND INDICATOR                
         NI    RUNIND5,X'FF'-RUN5LI3  TURN OFF PTHIRD  INDICATOR                
         GOTO1 ACREPORT                                                         
         J     DOWNX                                                            
*                                                                               
DOWN01   LA    R2,P                AND START A NEW ONE                          
         MVC   P,SPACES                                                         
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         J     DOWNX                                                            
*                                                                               
DOWN02   CLI   BYTE,DOWNADD        ADDING A FIELD, CHECK IT FITS FIRST          
         JNE   DOWN08                                                           
         LA    RE,P+L'P                                                         
         TM    RUNIND5,RUN5LI2     ON HEADER WE USE PSECOND                     
         JZ    *+8                                                              
         LA    RE,PSECOND+L'PSECOND                                             
         TM    RUNIND5,RUN5LI3     ON HEADER WE ALSO USE PTHIRD                 
         JZ    *+8                                                              
         LA    RE,PTHIRD+L'PTHIRD                                               
         LR    RF,R2                                                            
         AR    RF,R4                                                            
         CR    RF,RE                                                            
         JL    DOWN04                                                           
         SHI   R2,1                                                             
         MVI   0(R2),C' '                                                       
         TM    RUNIND2,RUN2DHED                                                 
         JNZ   DOWN03                                                           
         MVI   CLEARHED,YESQ                                                    
         MVI   RCSUBPRG,X'FF'                                                   
         MVI   SKIPSPEC,YESQ                                                    
         MVI   NEWPAGE,NOQ                                                      
         GOTO1 ACREPORT                                                         
         LA    R2,P                ON HEADER WE USE PSECOND                     
         MVC   P,SPACES                                                         
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         J     DOWN04                                                           
*                                                                               
DOWN03   TM    RUNIND5,RUN5LI2     ALREADY USED PSECOND?                        
         JNZ   DOWN03A                                                          
         LA    R2,PSECOND          ON HEADER WE USE PSECOND                     
         MVC   PSECOND,SPACES                                                   
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         OI    RUNIND5,RUN5LI2                                                  
         J     DOWN04                                                           
*                                                                               
DOWN03A  TM    RUNIND5,RUN5LI3                                                  
         JZ    *+6                                                              
         DC    H'0'                WE'VE USED 3 LINES!                          
         LA    R2,PTHIRD                                                        
         MVC   PTHIRD,SPACES                                                    
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         OI    RUNIND5,RUN5LI3                                                  
         J     DOWN04                                                           
*                                                                               
DOWN04   SHI   R4,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R2),0(R5)       PUT FIELD IN                                 
         EX    R4,0(RE)                                                         
         LR    RF,R2                                                            
         AR    RF,R4                                                            
         AHI   R4,1                                                             
         LR    RE,R4                                                            
DOWN04A  CLI   0(RF),C'"'          GET RID OF " CHARS THEY ARE BAD              
         JNE   DOWN04B             BY SHUNTING DATA ALONG                       
         LR    R3,RF                                                            
         AHI   R3,1                                                             
         SHI   RE,2                                                             
         BASR  R1,0                                                             
         MVC   0(0,RF),0(R3)                                                    
         EX    RE,0(R1)                                                         
         AHI   RE,2                                                             
*                                                                               
DOWN04B  SHI   RF,1                                                             
         JCT   RE,DOWN04A                                                       
         AR    R2,R4                                                            
DOWN05   CLI   0(R2),C' '                                                       
         JH    DOWN06                                                           
         SHI   R2,1                                                             
         JCT   R5,DOWN05                                                        
*                                                                               
DOWN06   AHI   R2,1                                                             
         MVC   0(L'SEPQ,R2),SEPQ                                                
         AHI   R2,L'SEPQ                                                        
         J     DOWNX                                                            
*                                                                               
DOWN08   CLI   BYTE,DOWNBLK        PUT BLANK FIELD                              
         JNE   DOWN12                                                           
         LHI   R4,L'SEPQ+1                                                      
         LA    RE,P+L'P-1                                                       
         LR    RF,R2                                                            
         AR    RF,R4                                                            
         CR    RF,RE                                                            
         JL    DOWN10                                                           
         SHI   R2,1                                                             
         MVI   0(R2),C' '                                                       
         MVI   CLEARHED,YESQ                                                    
         MVI   RCSUBPRG,X'FF'                                                   
         MVI   SKIPSPEC,YESQ                                                    
         MVI   NEWPAGE,NOQ                                                      
         GOTO1 ACREPORT                                                         
         LA    R2,P                AND START A NEW ONE                          
         MVC   P,SPACES                                                         
         MVI   0(R2),C'"'                                                       
         AHI   R2,2                                                             
         MVC   0(L'SEPQ,R2),SEPQ                                                
         AHI   R2,L'SEPQ                                                        
         J     DOWNX                                                            
*                                                                               
DOWN10   MVI   0(R2),C' '                                                       
         AHI   R2,1                                                             
         MVC   0(L'SEPQ,R2),SEPQ                                                
         AHI   R2,L'SEPQ                                                        
         J     DOWNX                                                            
*                                                                               
DOWN12   CLI   BYTE,DOWNNUM        PUT IN NUMBER                                
         JNE   DOWN14                                                           
         SHI   R2,1                                                             
DOWN13   CLI   0(R5),C' '          FIND START OF TEXT                           
         JH    DOWN13A                                                          
         AHI   R5,1                                                             
         JCT   R4,DOWN13                                                        
*                                                                               
DOWN13A  LA    RE,P+L'P                                                         
         LR    RF,R2                                                            
         AR    RF,R4                                                            
         CR    RF,RE               CHECK IT WILL FIT                            
         JL    DOWN13B                                                          
         MVI   0(R2),C' '          IF NOT START A NEW LINE                      
         MVI   CLEARHED,YESQ                                                    
         MVI   RCSUBPRG,X'FF'                                                   
         MVI   SKIPSPEC,YESQ                                                    
         MVI   NEWPAGE,NOQ                                                      
         GOTO1 ACREPORT                                                         
         LA    R2,P                                                             
         MVC   P,SPACES                                                         
*                                                                               
DOWN13B  SHI   R4,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,R2),0(R5)                                                    
         EX    R4,0(R1)                                                         
         AHI   R4,2                                                             
         AR    R2,R4                                                            
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         J     DOWNX                                                            
*                                                                               
DOWN14   CLI   BYTE,DOWNCLOS       END REPORT                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   P,SPACES            CLEAR PRINT LINE                             
         MVI   P,C':'              ADD END OF REPORT CHAR                       
         MVI   CLEARHED,YESQ       DO NOT FORCE HEADINGS                        
         MVI   RCSUBPRG,X'EE'                                                   
         MVI   SKIPSPEC,YESQ       DOWNLOAD MEANS NO HEADINGS'RE WANTED         
         MVI   NEWPAGE,NOQ         AND NO PAGE THROWS                           
         GOTO1 ACREPORT            LAST TIME ONLY                               
*                                                                               
DOWNX    J     EXITR2                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT THE DOWNLOAD REPORT                                           *         
* ON NTRY P1=A(EMRECD)                                                *         
***********************************************************************         
         USING EMRECD,R3                                                        
PRTDREP  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PRTDRE*'                                                      
         L     R3,0(R1)                                                         
         GOTOR DOWN,DMCB,DOWNEWL                                                
         GOTOR DOWN,DMCB,DOWNADD,(L'EMAPID,EMAPID),0(R2) PID                    
         GOTOR DOWN,DMCB,DOWNADD,(L'APPFSTNM,APPFSTNM),0(R2) 1ST NAME           
         GOTOR DOWN,DMCB,DOWNADD,(L'APPLSTNM,APPLSTNM),0(R2) LAST NAME          
         GOTOR DOWN,DMCB,DOWNADD,(L'EMAOFF,EMAOFF),0(R2) OFFICE                 
         GOTOR DOWN,DMCB,DOWNADD,(L'EMAALP,EMAALP),0(R2) AGENCY ALPHA           
         GOTOR DOWN,DMCB,DOWNADD,(L'CONAME,CONAME),0(R2) AGENCY NAME            
*                                                                               
PRTDR2   CLI   EMAMODL,EMATIME    TIMESHEETS?                                   
         JNE   PRTDR4                                                           
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@ETME,AC@ETME),0(R2) TIMESHEETS           
         GOTOR DOWN,DMCB,DOWNADD,(L'EMULA,EMULA),0(R2) CL/PR/JB/1N/1R           
         GOTOR DOWN,DMCB,DOWNADD,(L'PERFSTNM,PERFSTNM),0(R2) 1ST NAME           
         GOTOR DOWN,DMCB,DOWNADD,(L'PERLSTNM,PERLSTNM),0(R2) LAST NAME          
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               JOB NAME                  
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV LOG NO.               
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV NO.                   
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV. TYPE                 
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV. UPDATED              
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               EST NO.                   
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               ORDER NO.                 
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               ORDER REQ BY DATE         
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               ESTIMATE NAME             
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               SUPPLIER CODE             
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               SUPPLIER NAME             
         GOTO1 DATCON,DMCB,(1,EMDATE),(13,TEXT2)      DATE                      
         GOTOR DOWN,DMCB,DOWNADD,(8,TEXT2),0(R2)                                
         GOTOR DOWN,DMCB,DOWNNUM,(L'ZERAMT,ZERAMT),0(R2)    AMOUNT              
         J     PRTDRX                                                           
*                                                                               
PRTDR4   CLI   EMAMODL,EMAEXP     EXPENSES?                                     
         JNE   PRTDR8                                                           
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@EXPCL,AC@EXPCL),0(R2) EXPENSES           
         GOTOR DOWN,DMCB,DOWNADD,(L'EMULA,EMULA),0(R2) CL/PR/JB/1N/1R           
         GOTOR DOWN,DMCB,DOWNADD,(L'PERFSTNM,PERFSTNM),0(R2) 1ST NAME           
         GOTOR DOWN,DMCB,DOWNADD,(L'PERLSTNM,PERLSTNM),0(R2) LAST NAME          
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               JOB NAME                  
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV LOG NO.               
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV NO.                   
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV. TYPE                 
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV. UPDATED              
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               EST NO.                   
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               ORDER NO.                 
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               ORDER REQ BY DATE         
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               ESTIMATE NAME             
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               SUPPLIER CODE             
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               SUPPLIER NAME             
         GOTO1 DATCON,DMCB,(1,EMDATE),(13,TEXT2)      DATE                      
         GOTOR DOWN,DMCB,DOWNADD,(8,TEXT2),0(R2)      AMOUNT                    
         MVC   WORK,SPACES                                                      
         CP    EMAMNT,PZERO                                                     
         JH    PRTDR6                                                           
         GOTOR DOWN,DMCB,DOWNNUM,(L'ZERAMT,ZERAMT),0(R2)    AMOUNT              
         J     PRTDRX                                                           
*                                                                               
PRTDR6   CURED (P6,EMAMNT),(14,WORK),2,ZERO=BLANK,ALIGN=RIGHT,COMMAS=NO         
         GOTOR DOWN,DMCB,DOWNNUM,(14,WORK),0(R2)                                
         J     PRTDRX                                                           
*                                                                               
PRTDR8   CLI   EMAMODL,EMAORD     ORDERS?                                       
         JNE   PRTDR16                                                          
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@ORDS,AC@ORDS),0(R2) ORDERS               
                                                                                
         CLC   EMOCPJ(L'SJUL),SJUL                                              
         JNE   PRTDR8A                                CL/PR/JB OR NULLS         
         CLI   COMPLANG,CTRYGER                                                 
         JNE   PRTDR8A                                                          
         GOTOR DOWN,DMCB,DOWNADD,(L'EMOCPJ,EMOCPJ),0(R2)                        
         J     PRTDR8B                                                          
PRTDR8A  GOTOR DOWN,DMCB,DOWNBLK,,0(R2)                                         
PRTDR8B  GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               FIRST NAME                
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               LAST NAME                 
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               JOB NAME                  
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV LOG NO.               
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV NO.                   
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV. TYPE                 
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV. UPDATED              
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               EST NO.                   
         GOTOR DOWN,DMCB,DOWNADD,(L'EMORDN,EMORDN),0(R2) ORDER NO.              
         OC    EMRBDAT,EMRBDAT                        ORDER REQ BY DATE         
         JZ    PRTDR8C                                                          
         GOTO1 DATCON,DMCB,(1,EMRBDAT),(13,TEXT2)                               
         GOTOR DOWN,DMCB,DOWNADD,(8,TEXT2),0(R2)                                
         J     PRTDR8D                                                          
PRTDR8C  GOTOR DOWN,DMCB,DOWNBLK,,0(R2)                                         
PRTDR8D  GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               EST NAME                  
         GOTOR DOWN,DMCB,DOWNADD,(L'EMSUP,EMSUP),0(R2) SUPPLIER CODE            
         GOTOR GETNAME,DMCB,EMSUP                                               
         LLC   RF,LACCNAM                                                       
         LTR   RF,RF                                                            
         JZ    PRTDR10                                                          
         GOTOR DOWN,DMCB,DOWNADD,(LACCNAM,ACCNAM),0(R2) SUPPLIER NAME           
         J     PRTDR12                                                          
PRTDR10  GOTOR DOWN,DMCB,DOWNBLK,,0(R2)                                         
*                                                                               
PRTDR12  GOTO1 DATCON,DMCB,(1,EMDATE),(13,TEXT2)      DATE                      
         GOTOR DOWN,DMCB,DOWNADD,(8,TEXT2),0(R2)                                
         MVC   WORK,SPACES                                                      
         CP    EMAMNT,PZERO                                                     
         JH    PRTDR14                                                          
         GOTOR DOWN,DMCB,DOWNNUM,(L'ZERAMT,ZERAMT),0(R2)    AMOUNT              
         J     PRTDRX                                                           
*                                                                               
PRTDR14  CURED (P6,EMAMNT),(14,WORK),2,ZERO=BLANK,ALIGN=RIGHT,COMMAS=NO         
         GOTOR DOWN,DMCB,DOWNNUM,(14,WORK),0(R2)     AMOUNT                     
         J     PRTDRX                                                           
*                                                                               
PRTDR16  CLI   EMAMODL,EMAJOB                         JOBS?                     
         JNE   PRTDR18                                                          
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@JOBS,AC@JOBS),0(R2) JOBS                 
         GOTOR DOWN,DMCB,DOWNADD,(L'EMULA,EMULA),0(R2) CL/PR/JB/1N/1R           
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               FIRST NAME                
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               LAST NAME                 
         GOTOR DOWN,DMCB,DOWNADD,(L'EMJOBN,EMJOBN),0(R2) JOB NAME               
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV LOG NO.               
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV NO.                   
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV. TYPE                 
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV. UPDATED              
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               EST NO.                   
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               ORDER NO.                 
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               ORDER RQ BY DATE          
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               ESTIMATE NAME             
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               SUPPLIER CODE             
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               SUPPLIER NAME             
         GOTO1 DATCON,DMCB,(1,EMDATE),(13,TEXT2)      DATE                      
         GOTOR DOWN,DMCB,DOWNADD,(8,TEXT2),0(R2)                                
         GOTOR DOWN,DMCB,DOWNNUM,(L'ZERAMT,ZERAMT),0(R2)    AMOUNT              
         J     PRTDRX                                                           
*                                                                               
PRTDR18  CLI   EMAMODL,EMAEST                         ESTIMATES?                
         JNE   PRTDR24                                                          
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@ESTS,AC@ESTS),0(R2) MODULE               
         GOTOR DOWN,DMCB,DOWNADD,(L'EMULA,EMULA),0(R2) CLI/PRO/JOB              
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               FIRST NAME                
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               LAST NAME                 
         GOTOR GETNAME,DMCB,EMULA                                               
         LLC   RF,LACCNAM                                                       
         LTR   RF,RF                                                            
         JZ    PRTDR20                                                          
         GOTOR DOWN,DMCB,DOWNADD,(LACCNAM,ACCNAM),0(R2) JOB NAME                
         J     PRTDR22                                                          
PRTDR20  GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               JOB NAME                  
*                                                                               
PRTDR22  GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV LOG NO.               
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV NO.                   
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV. TYPE                 
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               INV. UPD.                 
         GOTOR DOWN,DMCB,DOWNADD,(L'EMESTN,EMESTN),0(R2) ESTIMATE NO            
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               ORDER NO.                 
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               ORDER REQ BY DATE         
         GOTOR DOWN,DMCB,DOWNADD,(L'EMESTNM,EMESTNM),0(R2) EST NAME             
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               SUPPLIER CODE             
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               SUPPLIER NAME             
         GOTO1 DATCON,DMCB,(1,EMDATE),(13,TEXT2)      DATE                      
         GOTOR DOWN,DMCB,DOWNADD,(8,TEXT2),0(R2)                                
         CP    EMAMNT,PZERO                                                     
         JH    PRTDR23                                                          
         GOTOR DOWN,DMCB,DOWNNUM,(L'ZERAMT,ZERAMT),0(R2)    AMOUNT              
         J     PRTDRX                                                           
*                                                                               
PRTDR23  CURED (P6,EMAMNT),(14,WORK),2,ZERO=BLANK,COMMAS=NO,ALIGN=RIGHT         
         GOTOR DOWN,DMCB,DOWNNUM,(14,WORK),0(R2)            AMOUNT              
         J     PRTDRX                                                           
*                                                                               
PRTDR24  CLI   EMAMODL,EMAINV                         INVOICES?                 
         JE    *+6                                                              
         DC    H'0'       SOMETHING WRONG WITH EMRECD!                          
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@INVS,AC@INVS),0(R2)  INVOICES            
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)                     CL/PR/JB            
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)                     1ST NAME            
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)                     LAST NAME           
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)                     JOB NAME            
         GOTOR DOWN,DMCB,DOWNADD,(L'EMILN,EMILN),0(R2)      INV LOG NO.         
         GOTOR DOWN,DMCB,DOWNADD,(L'EMAINTR,EMAINTR),0(R2)  INV NO.             
         MVC   FULL,AC@MIX                                                      
         CLI   EMAITYP,C'M'                                                     
         JE    PRTDR24A                                                         
         MVC   FULL,AC@EXPA                                                     
         CLI   EMAITYP,C'N'                                                     
         JE    PRTDR24A                                                         
         MVC   FULL,AC@PROC                                                     
PRTDR24A GOTOR DOWN,DMCB,DOWNADD,(4,FULL),0(R2)             INV. TYPE           
         OC    EMDTUPD,EMDTUPD     any added date?                              
         JZ    PRTDR24B                                                         
         GOTO1 DATCON,DMCB,(2,EMDTUPD),(13,TEXT2)                               
         GOTOR DOWN,DMCB,DOWNADD,(8,TEXT2),0(R2)                                
         J     PRTDR24C                                                         
PRTDR24B GOTOR DOWN,DMCB,DOWNBLK,,0(R2)                     INV. UPD.           
PRTDR24C GOTOR DOWN,DMCB,DOWNBLK,,0(R2)                     EST NO.             
         MVC   TEXT2,SPACES        single or list of orders                     
         MVC   TEXT3,SPACES                                                     
         CLC   EMIORDN,SPACES                                                   
         JH    PRTDR24D            if no order two blank entries                
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)                                         
         GOTOR DOWN,DMCB,DOWNBLK,,0(R2)                                         
         J     PRTDR24X                                                         
PRTDR24D MVC   TEXT2(L'EMIORDN),EMIORDN                                         
         MVC   TEXT3(L'DATENDQ),DATENDQ                                         
         LA    R4,TEXT2+L'EMIORDN+1                                             
         LA    R5,TEXT3+L'DATENDQ+1                                             
         OC    EMRBDAT,EMRBDAT                                                  
         JZ    PRTDR24E                                                         
         GOTO1 DATCON,DMCB,(1,EMRBDAT),(13,TEXT3)                               
         AHI   R5,8-L'DATENDQ                                                   
PRTDR24E LA    R6,EMAIORD+1                                                     
         LHI   R0,MAXORDPQ                                                      
PRTDR24F CLC   0(L'EMIORDN,R6),SPACES                                           
         JNH   PRTDR24H                                                         
         MVC   0(L'EMIORDN,R4),0(R6)                                            
         MVC   0(L'DATENDQ,R5),DATENDQ                                          
         OC    L'EMIORDN(L'ORDRQBD,R6),L'EMIORDN(R6)                            
         JZ    PRTDR24G                                                         
         GOTO1 DATCON,DMCB,(1,L'EMIORDN(R6)),(13,0(R5))                         
         AHI   R5,8-L'DATENDQ                                                   
PRTDR24G AHI   R5,L'DATENDQ+1                                                   
         AHI   R4,L'EMIORDN+1                                                   
         AHI   R6,L'RBDRORD+L'ORDRQBD                                           
         J     PRTDR24F                                                         
PRTDR24H GOTOR DOWN,DMCB,DOWNADD,(L'TEXT2,TEXT2),0(R2) ORDER NUMBER(S)          
         GOTOR DOWN,DMCB,DOWNADD,(L'TEXT3,TEXT3),0(R2) + REQ BY DATE(S)         
PRTDR24X GOTOR DOWN,DMCB,DOWNBLK,,0(R2)                EST NAME                 
         GOTOR DOWN,DMCB,DOWNADD,(L'EMSUP,EMSUP),0(R2) SUPP CODE                
         GOTOR GETNAME,DMCB,EMSUP                                               
         LLC   RF,LACCNAM                                                       
         LTR   RF,RF                                                            
         JZ    PRTDR26                                                          
         AHI   RF,1                                                             
         GOTOR DOWN,DMCB,DOWNADD,((RF),ACCNAM),0(R2)                            
         J     PRTDR28                                                          
PRTDR26  GOTOR DOWN,DMCB,DOWNBLK,,0(R2)               SUPP NAME                 
*                                                                               
PRTDR28  GOTO1 DATCON,DMCB,(1,EMDATE),(13,TEXT2)      DATE                      
         GOTOR DOWN,DMCB,DOWNADD,(8,TEXT2),0(R2)                                
         MVC   WORK,SPACES                                                      
         CP    EMAMNT,PZERO                                                     
         JH    PRTDR30                                                          
         GOTOR DOWN,DMCB,DOWNNUM,(L'ZERAMT,ZERAMT),0(R2)    AMOUNT              
         J     PRTDRX                                                           
*                                                                               
PRTDR30  CURED (P6,EMAMNT),(14,WORK),2,ZERO=BLANK,COMMAS=NO,ALIGN=RIGHT         
         GOTOR DOWN,DMCB,DOWNNUM,(14,WORK),0(R2)            AMOUNT              
         J     PRTDRX                                                           
*                                                                               
PRTDRX   GOTOR DOWN,DMCB,DOWNEOL,,0(R2)                                         
         J     EXITE                                                            
         EJECT                                                                  
         DROP  R3                                                               
***********************************************************************         
* SET EXPENSES APPROVAL LEVEL                                         *         
* ON NTRY P1=LEVEL                                                    *         
*         P2=JOBPAPPL                                                 *         
***********************************************************************         
         SPACE 1                                                                
SETLVL   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETLVL*'                                                      
         CLC   SORTBNB,SPACES      IF BILL/NON-BILL NOT SET QUIT                
         JNH   SETLVLX                                                          
         L     R4,0(R1)                                                         
         L     R5,4(R1)                                                         
         CLI   SORTBNB,C'B'        BILLABLE                                     
         JNE   SETLVL4                                                          
         CLI   0(R4),C'1'                                                       
         JNE   SETLVL2                                                          
         MVI   0(R5),JOBPAX1B                                                   
         J     SETLVLX                                                          
*                                                                               
SETLVL2  CLI   0(R4),C'2'                                                       
         JNE   SETLVLX                                                          
         MVI   0(R5),JOBPAX2B                                                   
         J     SETLVLX                                                          
*                                                                               
SETLVL4  CLI   SORTBNB,NOQ         NON-BILLABLE                                 
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),C'1'                                                       
         JNE   SETLVL6                                                          
         MVI   0(R5),JOBPAX1N                                                   
         J     SETLVLX                                                          
*                                                                               
SETLVL6  CLI   0(R4),C'2'                                                       
         JNE   SETLVLX                                                          
         MVI   0(R5),JOBPAX2N                                                   
         J     SETLVLX                                                          
*                                                                               
SETLVLX  J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* SORT SEQUENCE - DETERMINES SORT SEQUENCE FOR MODULES ON EMAILS      *         
***********************************************************************         
         SPACE 1                                                                
GETSEQ   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETSEQ*'                                                      
         LA    R1,SORTORDX                                                      
         LA    R2,SORTORDX+SORTORDL                                             
         XC    SEQTAB(SEQTABL),SEQTAB                                           
         XR    R0,R0                                                            
         IC    R0,=C'1'                                                         
*                                                                               
GETSQ2   CR    R1,R2                HIT END OF TABLE?                           
         JNL   EXITE                                                            
         CLI   0(R1),SORTTIME       TIMESHEETS                                  
         JE    GETSQ4                                                           
         CLI   0(R1),SORTEXP        EXPENSES                                    
         JE    GETSQ6                                                           
         CLI   0(R1),SORTORD        ORDERS                                      
         JE    GETSQ8                                                           
         CLI   0(R1),SORTJOB        JOBS                                        
         JE    GETSQ10                                                          
         CLI   0(R1),SORTEST        ESTIMATES                                   
         JE    GETSQ12                                                          
         CLI   0(R1),SORTINV        INVOICES                                    
         JE    GETSQ14                                                          
         J     GETSQERR                                                         
*                                                                               
GETSQ4   LA    RE,SEQTIME           TIMESHEETS                                  
         J     GETSQ20                                                          
*                                                                               
GETSQ6   LA    RE,SEQEXP            EXPENSES                                    
         J     GETSQ20                                                          
*                                                                               
GETSQ8   LA    RE,SEQORD            ORDERS                                      
         J     GETSQ20                                                          
*                                                                               
GETSQ10  LA    RE,SEQJOB            JOBS                                        
         J     GETSQ20                                                          
*                                                                               
GETSQ12  LA    RE,SEQEST            ESTIMATES                                   
         J     GETSQ20                                                          
*                                                                               
GETSQ14  LA    RE,SEQINV            INVOICES                                    
         J     GETSQ20                                                          
*                                                                               
GETSQ20  STC   R0,0(RE)             STORE SORT SEQUENCE NUMBER                  
         AHI   R0,1                                                             
         AHI   R1,1                                                             
         J     GETSQ2                                                           
*                                                                               
GETSQERR LA    RE,SEQTAB            ERROR PROFILE MISSING OR BAD                
         LA    RF,SEQTAB+SEQTABL    FIX SORT ORDER                              
GETSQE2  CR    RE,RF                                                            
         JNL   EXITE                                                            
         STC   R0,0(RE)                                                         
         AHI   RE,1                                                             
         AHI   R0,1                                                             
         J     GETSQE2                                                          
         EJECT                                                                  
***********************************************************************         
*  GET LANGUAGE CODE                                                  *         
***********************************************************************         
         SPACE 1                                                                
GETLANG  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETLAN*'                                                      
         LA    R5,IOKEY2                                                        
         USING CTIREC,R5           SYSTEM ACCESS RECORD                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,ORIGINUM                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY2,AIOAREA2                       
         L     R5,AIOAREA2                                                      
         CLC   IOKEY2(L'CTIKEY),0(R5)                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,CTIDATA                                                       
         USING CTAGYD,R5                                                        
GLANG02  CLI   CTAGYEL,0           TEST EOR                                     
         JE    GETLANGX                                                         
         CLI   CTAGYEL,CTAGYELQ                                                 
         JE    GLANG06                                                          
GLANG04  LLC   R0,CTAGYLEN                                                      
         AR    R5,R0                                                            
         J     GLANG02                                                          
*                                                                               
GLANG06  MVC   COMPLANG,CTAGYLNG                                                
*                                                                               
GETLANGX J     EXITE                                                            
         DROP  R5                                                               
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* READ FOR OFFICE NAME                                                *         
* ON NTRY P1=OFFICE CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING OFFRECD,IOKEY2                                                   
GETOFFN  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETOFN*'                                                      
*&&UK*&& XC    IOKEY2,IOKEY2                                                    
*&&US*&& MVC   IOKEY2,SPACES                                                    
         MVC   OFFNAM,SPACES                                                    
         L     RF,0(R1)                                                         
         MVC   SVOFF,0(RF)                                                      
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COCODE                                                   
         MVC   OFFKOFF,SVOFF                                                    
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,OFFKEY,OFFKEY,0                       
         JE    GETOFF2                                                          
         CLI   1(RF),C' '                                                       
         JNH   GETOFF4                                                          
         DC    H'0'                                                             
*                                                                               
GETOFF2  GOTO1 DATAMGR,DMCB,DMGET,ACCMST,OFFKDA,AIOAREA,DMWORK                  
         JE    GETOFF6                                                          
         DC    H'0'                                                             
*                                                                               
GETOFF4  MVC   WORK,SPACES                                                      
         MVC   WORK(2),=C'2D'                                                   
         MVC   WORK+2(1),SVOFF                                                  
         GOTOR GETNAME,DMCB,WORK                                                
         LLC   RF,LACCNAM                                                       
         BASR  R1,0                                                             
         MVC   OFFNAM(0),ACCNAM                                                 
         EX    RF,0(R1)                                                         
         J     GETOFFX                                                          
*                                                                               
GETOFF6  GOTO1 =V(HELLO),DMCB,(C'G',ACCMST),('NAMELQ',AIOAREA),0,0              
         CLI   12(R1),0            WAS 'GET' SUCCESSFUL?                        
         JNE   GETOFFX             NO OFFICE NAME                               
         L     R3,12(,R1)                                                       
         USING NAMELD,R3                                                        
         XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         CHI   RF,L'OFFNAM-1       Dont save too much                           
         BNH   *+8                                                              
         LA    RF,L'OFFNAM-1                                                    
         BASR  R1,0                                                             
         MVC   OFFNAM(0),NAMEREC                                                
         EX    RF,0(R1)                                                         
*                                                                               
GETOFFX  J     EXITE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT OVERFLOW MESSAGE ON EMAIL                                     *         
***********************************************************************         
         SPACE 1                                                                
EOVFL    NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*EOVFL**'                                                      
         MVC   TEXT1(L'AC@EOVFL),AC@EOVFL YOU MAY HAVE MORE THAN LISTED         
         LA    RF,TEXT1+L'AC@EOVFL                                              
*                                                                               
         TM    RUNIND2,RUN2TIM     CHECK WHETHER ANY MORE                       
         JZ    *+14                TIMESHEETS                                   
         MVC   0(L'AC@ETME,RF),AC@ETME                                          
         AHI   RF,L'AC@ETME+1                                                   
*                                                                               
         TM    RUNIND2,RUN2EXP     CHECK WHETHER ANY MORE                       
         JZ    *+14                EXPENSE CLAIMS                               
         MVC   0(L'AC@EXPCL,RF),AC@EXPCL                                        
         AHI   RF,L'AC@EXPCL+1                                                  
*                                                                               
         TM    RUNIND2,RUN2ORD     CHECK WHETHER ANY MORE ORDERS                
         JZ    *+14                                                             
         MVC   0(L'AC@ORDS,RF),AC@ORDS                                          
         AHI   RF,L'AC@ORDS+1                                                   
*                                                                               
         TM    RUNIND2,RUN2JOB     CHECK WHETHER ANY MORE JOBS                  
         JZ    *+14                                                             
         MVC   0(L'AC@JOBS,RF),AC@JOBS                                          
         AHI   RF,L'AC@JOBS+1                                                   
*                                                                               
         TM    RUNIND2,RUN2EST     CHECK WHETHER ANY MORE                       
         JZ    *+14                ESTIMATES                                    
         MVC   0(L'AC@ESTS,RF),AC@ESTS                                          
         AHI   RF,L'AC@ESTS+1                                                   
*                                                                               
         TM    RUNIND2,RUN2INV     CHECK WHETHER ANY MORE                       
         JZ    *+14                INVOICES                                     
         MVC   0(L'AC@INVS,RF),AC@INVS                                          
         AHI   RF,L'AC@INVS+1                                                   
*                                                                               
         MVC   0(L'AC@TOAPP,RF),AC@TOAPP                                        
         OI    0(RF),X'40'                                                      
         J     EXITE                                                            
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* SET THE EMAIL ADDRESS AND SUBJECT                                   *         
* ON NTRY P1=EMRECD                                                   *         
***********************************************************************         
         USING EMRECD,R2                                                        
DOEMADD  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*DOEMAD*'                                                      
         LR    R2,R1                                                            
         LA    RE,L'EMADDR                                                      
         LA    RF,EMADDR+L'EMADDR-1                                             
DOEMA02  CLI   0(RF),C' '                                                       
         JH    DOEMA04                                                          
         AHI   RF,-1                                                            
         JCT   RE,DOEMA02                                                       
         DC    H'0'                                                             
*                                                                               
DOEMA04  MVI   1(RF),C':' EMAIL ADDRESS HAS TO BE TERMINATED WITH ':'           
         XC    SVEMAD,SVEMAD                                                    
         CLI   PARMDOE,YESQ        OVERRIDE TO TEST                             
         JNE   DOEMA06                                                          
         MVC   SVEMAD,EMADDR                                                    
         XC    EMADDR,EMADDR                                                    
*        MVC   EMADDR(17),=C'JIM.SHEA@DDS.NET:'                                 
         LAY   R5,TESTMAIL         OVERRIDE EMAIL PATCHED IN?                   
         CLC   0(L'TESTMAIL,R5),SPACES                                          
         JH    *+6                                                              
         DC    H'0'                WHERE IS IT?                                 
         MVC   EMADDR,0(R5)                                                     
         MVC   TEXT1,TEXTSPCS                                                   
         MVC   TEXT1(L'SVEMAD),SVEMAD  PUT IN EMAIL ADDRESS                     
         MVC   TEXT1+L'SVEMAD+1(L'AC@AURAA),AC@AURAA                            
         LA    RE,TEXT1+L'TEXT1-1  Find last significant character              
         LA    RF,L'TEXT1                                                       
         CLI   0(RE),C' '                                                       
         JH    *+16                                                             
         AHI   RE,-1                                                            
         AHI   RF,-1                                                            
         J     *-16                                                             
         LA    R1,L'TEXT1                                                       
         SR    R1,RF                                                            
         AHI   R1,-1                                                            
         BASR  RF,0                                                             
         MVC   1(0,RE),TEXTSPCS                                                 
         EX    R1,0(RF)                                                         
         AHI   R1,-1               One for EX and one for the disp              
         BASR  RF,0                                                             
         MVC   2(0,RE),AC@AURAA                                                 
         EX    R1,0(RF)                                                         
         J     DOEMA08                                                          
*                                                                               
DOEMA06  MVC   TEXT1,TEXTSPCS                                                   
         MVC   TEXT1(L'AC@AURAA),AC@AURAA PUT IN TEXT BRANDOCEAN APPR           
*                                                                               
DOEMA08  MVC   WORK,SPACES                                                      
         MVC   WORK,EMADDR         store in 24 bit address                      
         XC    DMCB,DMCB                                                        
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAPRS',WORK),(L'TEXT1,TEXT1)                     
*                                                                               
         L     R1,NUMTOT                                                        
         AHI   R1,1                                                             
         ST    R1,NUMTOT                                                        
         MVC   TEXT1,TEXTSPCS                                                   
         MVC   TEXT1(L'SVEMAD),SVEMAD                                           
*                                                                               
         L     RF,AELE271E         Clear out previous information               
         MVC   0(L'ELEM271E,RF),TEXTSPCS                                        
         L     RF,AELE272E                                                      
         MVC   0(L'ELEM272E,RF),TEXTSPCS                                        
         L     RF,AELE273E                                                      
         MVC   0(L'ELEM273E,RF),TEXTSPCS                                        
         L     RF,AEMS260E                                                      
         MVC   0(L'EMLS260E,RF),TEXTSPCS                                        
         L     RF,AEMS261E                                                      
         MVC   0(L'EMLS261E,RF),TEXTSPCS                                        
         L     RF,AEMS262E                                                      
         MVC   0(L'EMLS262E,RF),TEXTSPCS                                        
*                                                                               
         CLI   EM2AGYE,YESQ        show agency info?                            
         JE    *+12                                                             
         CLI   EM2PIDE,YESQ        show pid info?                               
         JNE   DOEMAX                                                           
*                                  Do your details... line                      
         L     RF,AELE271E                                                      
         MVC   0(L'PARAOP,RF),PARAOP                                            
         AHI   RF,L'PARAOP                                                      
         MVC   0(L'AC@YRDT,RF),AC@YRDT                                          
         AHI   RF,L'AC@YRDT                                                     
         MVI   0(RF),C':'                                                       
         AHI   RF,1                                                             
         MVC   0(L'PARACL,RF),PARACL                                            
*                                                                               
         L     RF,AEMS260E                                                      
         MVC   0(L'PARAOP,RF),PARAOP                                            
         AHI   RF,L'PARAOP                                                      
         MVC   0(L'AC@YRDT,RF),AC@YRDT                                          
         AHI   RF,L'AC@YRDT                                                     
         MVI   0(RF),C':'                                                       
         AHI   RF,1                                                             
         MVC   0(L'PARACL,RF),PARACL                                            
*                                                                               
         CLI   EM2AGYE,YESQ        show agency info?                            
         JNE   DOEMA10                                                          
*                                                                               
         L     RF,AELE272E                                                      
         MVC   0(L'PARAOP,RF),PARAOP                                            
         AHI   RF,L'PARAOP                                                      
         MVC   0(L'COUSID,RF),COUSID                                            
         AHI   RF,L'COUSID+1                                                    
         MVI   0(RF),C'"'                                                       
         MVC   1(L'EMAALP,RF),EMAALP                                            
         AHI   RF,L'EMAALP+1                                                    
         MVI   0(RF),C'"'                                                       
         AHI   RF,2                                                             
         MVC   0(L'CONAME,RF),CONAME                                            
         AHI   RF,L'CONAME                                                      
         MVC   0(L'PARACL,RF),PARACL                                            
*                                                                               
         L     RF,AEMS261E                                                      
         MVC   0(L'PARAOP,RF),PARAOP                                            
         AHI   RF,L'PARAOP                                                      
         MVC   0(L'COUSID,RF),COUSID                                            
         AHI   RF,L'COUSID+1                                                    
         MVI   0(RF),C'"'                                                       
         MVC   1(L'EMAALP,RF),EMAALP                                            
         AHI   RF,L'EMAALP+1                                                    
         MVI   0(RF),C'"'                                                       
         AHI   RF,2                                                             
         MVC   0(L'CONAME,RF),CONAME                                            
         AHI   RF,L'CONAME                                                      
         MVC   0(L'PARACL,RF),PARACL                                            
*                                                                               
DOEMA10  CLI   EM2PIDE,YESQ        show pid info?                               
         JNE   DOEMA12                                                          
*                                                                               
         L     RF,AELE273E                                                      
         MVC   0(L'PARAOP,RF),PARAOP                                            
         AHI   RF,L'PARAOP                                                      
         MVC   0(L'EMAPID,RF),EMAPID                                            
         AHI   RF,L'EMAPID                                                      
         MVC   0(L'PARACL,RF),PARACL                                            
*                                                                               
         L     RF,AEMS262E                                                      
         MVC   0(L'PARAOP,RF),PARAOP                                            
         AHI   RF,L'PARAOP                                                      
         MVC   0(L'EMAPID,RF),EMAPID                                            
         AHI   RF,L'EMAPID                                                      
         MVC   0(L'PARACL,RF),PARACL                                            
*                                                                               
DOEMA12  L     RF,AELE271E                                                      
         GOTO1 SQUASHER,DMCB,0(RF),MAXELEN                                      
         L     RF,AELE272E                                                      
         GOTO1 SQUASHER,DMCB,0(RF),80                                           
         L     RF,AELE273E                                                      
         GOTO1 SQUASHER,DMCB,0(RF),80                                           
         L     RF,AEMS260E                                                      
         GOTO1 SQUASHER,DMCB,0(RF),80                                           
         L     RF,AEMS261E                                                      
         GOTO1 SQUASHER,DMCB,0(RF),80                                           
         L     RF,AEMS262E                                                      
         GOTO1 SQUASHER,DMCB,0(RF),80                                           
*                                                                               
DOEMAX   J     EXITE                                                            
         DROP  R2                                                               
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* PRINT DETAILS FOR EXPENSE SECTION OF EMAIL                          *         
* ON NTRY P1=EMRECD                                                   *         
* ON EXIT R2 PRESERVED                                                *         
***********************************************************************         
         USING EMRECD,R2                                                        
R        USING EMRECD,R5                                                        
PRTEEXP  NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R1                                                            
         OI    RUNIND2,RUN2EXP                                                  
         CLI   SHOWSEM,YESQ        SHOW SUMMARY EMAILS DEFAULT=Y                
         JNE   PRTEE06                                                          
         MVC   TEXT1+1(L'AC@YHAV),AC@YHAV     YOU HAVE                          
         MVC   TEXT1+16(L'AC@EXPCL),AC@EXPCL   EXPENSES                         
         MVC   TEXT1+24+L'AC@EXPCL(L'AC@TOAPP),AC@TOAPP TO APPROVE              
         NI    TEXT1+24+L'AC@EXPCL,X'FF'-X'40'                                  
         XC    NUMPUT,NUMPUT                                                    
*                                                                               
PRTEE02  JCT   R3,PRTEE04          DECREMENT NUMBER OF RECORDS                  
         L     R4,NUMPUT                                                        
         AHI   R4,1                                                             
         ST    R4,NUMPUT                                                        
         EDIT  (4,NUMPUT),(4,TEXT1+12),ALIGN=LEFT                               
         GOTO1 SQUASHER,DMCB,TEXT1,80                                           
         GOTOR SNDSMTP,TEXT1       OUTPUT DETAIL                                
         J     PRTEEE                                                           
*                                                                               
PRTEE04  L     R4,NUMPUT           COUNT NUMBER OF EXPENSES                     
         AHI   R4,1                                                             
         ST    R4,NUMPUT                                                        
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         LR    R5,R2               R2=A(NEXT RECORD)                            
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
         CLC   R.EMAPIN(L'EMAPIN+L'EMASEQ),EMAPIN SAME PID AND MODUL?           
         JNE   PRTEE05                                                          
         L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     PRTEE02             ADD UP TOTAL                                 
*                                                                               
PRTEE05  L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         SAM24                                                                  
         EDIT  (4,NUMPUT),(4,TEXT1+12),ALIGN=LEFT                               
         GOTO1 SQUASHER,DMCB,TEXT1,80                                           
         GOTOR SNDSMTP,TEXT1       OUTPUT DETAIL                                
         JNE   PRTEEE                                                           
         SAM31                                                                  
         CLC   R.EMAPIN,EMAPIN               SAME APPROVER?                     
         JNE   PRTEEE                                                           
         J     PRTEEL                       YES GO ONTO ORDERS                  
*                                                                               
PRTEE06  MVC   TEXT1,AC@DERYA                                                   
         MVI   TEXT1+75,C':'                                                    
         GOTO1 SQUASHER,DMCB,TEXT1,80                                           
PRTEE08  MVC   TEXT2,SPACES                                                     
*&&UK                                                                           
         MVC   TEXT2+2(L'AC@CLPJO),AC@CLPJO   CLIENT/PRODUCT/JOB                
         MVI   TEXT2+2+L'AC@CLPJO,C'/'                                          
         MVC   TEXT2+3+L'AC@CLPJO(L'ONERUL),ONERUL                              
*&&                                                                             
*&&US*&& MVC   TEXT2+2(L'AC@EXPN),AC@EXPN     Expense Claim #                   
         MVC   TEXT2+18(L'AC@CFNAM),AC@CFNAM  FIRST NAME                        
         MVC   TEXT2+35(L'AC@CLNAM),AC@CLNAM  LAST NAME                         
         MVC   TEXT2+64(L'AC@AMT),AC@AMT      AMOUNT                            
         MVC   TEXT2+72(L'AC@DATE),AC@DATE    DATE                              
         J     PRTEEH                                                           
*                                                                               
PRTEEH   SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     EXITH23             GO TO PRINT LINES FOR DETAILED EMAIL         
*                                                                               
PRTEEE   SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     EXITE23             GO  TO END EMAIL                             
*                                                                               
PRTEEL   SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     EXITL23             GO TO NEXT MODULE                            
         EJECT                                                                  
         DROP  R2,R                                                             
***********************************************************************         
* PRINT ORDER SECTION ON EMAIL                                        *         
* ON NTRY P1=A(EMRECD)                                                *         
* ON EXIT R2 PRESERVED                                                *         
***********************************************************************         
         USING EMRECD,R2                                                        
R        USING EMRECD,R5                                                        
PRTEORD  NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R1                                                            
         OI    RUNIND2,RUN2ORD                                                  
         CLI   SHOWSEM,YESQ        SHOW SUMMARY EMAILS DEFAULT=Y                
         JNE   PRTEO06                                                          
         MVC   TEXT1+1(L'AC@YHAV),AC@YHAV     YOU HAVE                          
         MVC   TEXT1+16(L'AC@ORDS),AC@ORDS    ORDERS                            
         MVC   TEXT1+24+L'AC@ORDS(L'AC@TOAPP),AC@TOAPP TO APPROVE               
         NI    TEXT1+24+L'AC@ORDS,X'FF'-X'40'                                   
         XC    NUMPUT,NUMPUT                                                    
PRTEO02  JCT   R3,PRTEO04                                                       
         L     R4,NUMPUT                                                        
         AHI   R4,1                                                             
         ST    R4,NUMPUT                                                        
         EDIT  (4,NUMPUT),(4,TEXT1+12),ALIGN=LEFT                               
         GOTO1 SQUASHER,DMCB,TEXT1,80                                           
         GOTOR SNDSMTP,TEXT1       OUTPUT DETAIL                                
         J     PRTEOE                                                           
*                                                                               
PRTEO04  L     R4,NUMPUT           COUNT NUMBER OF ORDERS                       
         AHI   R4,1                                                             
         ST    R4,NUMPUT                                                        
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         LR    R5,R2               R2=A(NEXT RECORD)                            
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
         CLC   R.EMAPIN(L'EMAPIN+L'EMASEQ),EMAPIN SAME PID AND MODUL?           
         JNE   PRTEO05                                                          
         L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     PRTEO02             ADD UP TOTAL                                 
*                                                                               
PRTEO05  L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         SAM24                                                                  
         EDIT  (4,NUMPUT),(4,TEXT1+12),ALIGN=LEFT                               
         GOTO1 SQUASHER,DMCB,TEXT1,80                                           
         GOTOR SNDSMTP,TEXT1       OUTPUT DETAIL                                
         JNE   PRTEOE                                                           
         SAM31                                                                  
         CLC   R.EMAPIN,EMAPIN               SAME APPROVER?                     
         JNE   PRTEOE                                                           
         J     PRTEOL                       YES GO ONTO NEXT MODULE             
*                                                                               
PRTEO06  MVC   TEXT1,AC@DORYA      ORDERS                                       
         MVI   TEXT1+75,C':'                                                    
         GOTO1 SQUASHER,DMCB,TEXT1,80                                           
PRTEO08  MVC   TEXT2,SPACES                                                     
         MVC   TEXT2+2(L'AC@ORDC),AC@ORDC       ORDER NUMBER                    
         MVC   TEXT2+16(L'AC@SUPC),AC@SUPC    SUPPLIER CODE                     
         MVC   TEXT2+32(L'AC@SUPN),AC@SUPN    SUPPLIER NAME                     
         MVC   TEXT2+64(L'AC@AMT),AC@AMT      AMOUNT                            
         MVC   TEXT2+72(L'AC@DATE),AC@DATE    ORDER DATE                        
         J     PRTEOH                                                           
*                                                                               
PRTEOH   SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     EXITH23             GO TO PRINT LINES FOR DETAILED EMAIL         
*                                                                               
PRTEOE   SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     EXITE23             GO  TO END EMAIL                             
*                                                                               
PRTEOL   SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     EXITL23             GO TO NEXT MODULE                            
         EJECT                                                                  
         DROP  R2,R                                                             
***********************************************************************         
* PRINT ESTIMATE DETAILS ON EMAIL                                     *         
* ON NTRY P1=A(EMRECD)                                                *         
* ON EXIT R2 PRESERVED                                                *         
***********************************************************************         
         USING EMRECD,R2                                                        
R        USING EMRECD,R5                                                        
PRTEEST  NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'**PRTE**'                                                      
         LR    R2,R1                                                            
         OI    RUNIND2,RUN2EST                                                  
         CLI   SHOWSEM,YESQ        SHOW SUMMARY EMAILS DEFAULT=Y                
         JNE   PRTES06                                                          
         MVC   TEXT1+1(L'AC@YHAV),AC@YHAV     YOU HAVE                          
         MVC   TEXT1+16(L'AC@6E),AC@6E        ESTIMATES                         
         MVC   TEXT1+24+L'AC@6E(L'AC@TOAPP),AC@TOAPP TO APPROVE                 
         NI    TEXT1+24+L'AC@6E,X'FF'-X'40'                                     
         XC    NUMPUT,NUMPUT                                                    
*                                                                               
PRTES02  JCT   R3,PRTES04          DECREMENT NUMBER OF RECORDS                  
         L     R4,NUMPUT                                                        
         AHI   R4,1                                                             
         ST    R4,NUMPUT                                                        
         EDIT  (4,NUMPUT),(4,TEXT1+12),ALIGN=LEFT                               
         GOTO1 SQUASHER,DMCB,TEXT1,80                                           
         GOTOR SNDSMTP,TEXT1       OUTPUT DETAIL                                
         J     PRTESE                                                           
*                                                                               
PRTES04  L     R4,NUMPUT           COUNT NUMBER OF ESTIMATES                    
         AHI   R4,1                                                             
         ST    R4,NUMPUT                                                        
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         LR    R5,R2               R2=A(NEXT RECORD)                            
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
         CLC   R.EMAPIN(L'EMAPIN+L'EMASEQ),EMAPIN SAME PID AND MODUL?           
         JNE   PRTES05                                                          
         L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     PRTES02             ADD UP TOTAL                                 
*                                                                               
PRTES05  L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         SAM24                                                                  
         EDIT  (4,NUMPUT),(4,TEXT1+12),ALIGN=LEFT                               
         GOTO1 SQUASHER,DMCB,TEXT1,80                                           
         GOTOR SNDSMTP,TEXT1       OUTPUT DETAIL                                
         JNE   PRTESE              END EMAIL                                    
         SAM31                                                                  
         CLC   R.EMAPIN,EMAPIN     SAME APPROVER?                               
         JNE   PRTESE              END EMAIL                                    
         J     PRTESL                                                           
*                                                                               
PRTES06  MVC   TEXT1,AC@DESYA      ESTIMATES                                    
         MVI   TEXT1+75,C':'                                                    
         GOTO1 SQUASHER,DMCB,TEXT1,80                                           
PRTES08  MVC   TEXT2,SPACES                                                     
         MVC   TEXT2+2(L'AC@ESTNO),AC@ESTNO   ESTIMATE NUMBER                   
         MVC   TEXT2+10(L'AC@ESTNM),AC@ESTNM  ESTIMATE NAME                     
         MVC   TEXT2+50(L'AC@JOBC),AC@JOBC    JOB CODE                          
         MVC   TEXT2+72(L'AC@DATE),AC@DATE    ESTIMATE ADDED DATE               
         J     PRTESH                                                           
*                                                                               
PRTESH   SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     EXITH23             GO TO PRINT LINES FOR DETAILED EMAIL         
*                                                                               
PRTESE   SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     EXITE23             GO  TO END EMAIL                             
*                                                                               
PRTESL   SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     EXITL23             GO TO NEXT MODULE                            
         EJECT                                                                  
         DROP  R2,R                                                             
***********************************************************************         
* PRINT INVOICE DETAILS ON EMAIL                                      *         
* ON NTRY P1=A(EMRECD)                                                *         
* ON EXIT R2 PRESERVED                                                *         
***********************************************************************         
         USING EMRECD,R2                                                        
R        USING EMRECD,R5                                                        
PRTEINV  NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'**PRTE**'                                                      
         LR    R2,R1                                                            
         OI    RUNIND2,RUN2INV                                                  
         CLI   SHOWSEM,YESQ        SHOW SUMMARY EMAILS DEFAULT=Y                
         JNE   PRTEI06                                                          
         MVC   TEXT1+1(L'AC@YHAV),AC@YHAV     YOU HAVE                          
         MVC   TEXT1+16(L'AC@INVS),AC@INVS    INVOICES                          
         MVC   TEXT1+24+L'AC@INVS(L'AC@TOAPP),AC@TOAPP TO APPROVE               
         NI    TEXT1+24+L'AC@INVS,X'FF'-X'40'                                   
         XC    NUMPUT,NUMPUT                                                    
*                                                                               
PRTEI02  JCT   R3,PRTEI04          DECREMENT NUMBER OF RECORDS                  
         L     R4,NUMPUT                                                        
         AHI   R4,1                                                             
         ST    R4,NUMPUT                                                        
         EDIT  (4,NUMPUT),(4,TEXT1+12),ALIGN=LEFT                               
         GOTO1 SQUASHER,DMCB,TEXT1,80                                           
         GOTOR SNDSMTP,TEXT1       OUTPUT DETAIL                                
         J     PRTEIE                                                           
*                                                                               
PRTEI04  L     R4,NUMPUT           COUNT NUMBER OF INVOICES                     
         AHI   R4,1                                                             
         ST    R4,NUMPUT                                                        
         SAM31                                                                  
         L     R2,ANXTETB                                                       
         LR    R5,R2               R2=A(NEXT RECORD)                            
         LA    R2,EMRECL(R2)                                                    
         ST    R2,ANXTETB                                                       
         CLC   R.EMAPIN(L'EMAPIN+L'EMASEQ),EMAPIN SAME PID AND MODUL?           
         JNE   PRTEI05                                                          
         L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     PRTEI02             ADD UP TOTAL                                 
*                                                                               
PRTEI05  L     RF,ASVEMREC                                                      
         XC    0(EMRECL,RF),0(RF)                                               
         MVC   0(EMRECL,RF),EMRECD MOVE EMRECD TO 24 BIT STORAGE AREA           
         SAM24                                                                  
         EDIT  (4,NUMPUT),(4,TEXT1+12),ALIGN=LEFT                               
         GOTO1 SQUASHER,DMCB,TEXT1,80                                           
         GOTOR SNDSMTP,TEXT1       OUTPUT DETAIL                                
         JNE   PRTEIE              END EMAIL                                    
         SAM31                                                                  
         CLC   R.EMAPIN,EMAPIN     SAME APPROVER?                               
         JNE   PRTEIE              END EMAIL                                    
         J     PRTEIL                                                           
*                                                                               
PRTEI06  MVC   TEXT1,AC@DISYA      INVOICES                                     
         MVI   TEXT1+75,C':'                                                    
         GOTO1 SQUASHER,DMCB,TEXT1,80                                           
PRTEI08  MVC   TEXT2,SPACES                                                     
         MVC   TEXT2+2(L'AC@ILGNO),AC@ILGNO  LOG NUMBER                         
         MVC   TEXT2+10(L'AC@INVN),AC@INVN  INVOICE NUMBER                      
         MVC   TEXT2+31(L'AC@DATE),AC@DATE DATE                                 
         MVC   TEXT2+48(L'AC@AMT),AC@AMT   AMOUNT                               
         MVC   TEXT2+56(L'AC@SUPC),AC@SUPC SUPPLIER CODE                        
         MVC   TEXT2+71(L'AC@SUPN),AC@SUPN SUPPLIER NAME                        
         J     PRTEIH                                                           
*                                                                               
PRTEIH   SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     EXITH23             GO TO PRINT LINES FOR DETAILED EMAIL         
*                                                                               
PRTEIE   SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     EXITE23             GO  TO END EMAIL                             
*                                                                               
PRTEIL   SAM24                                                                  
         L     R2,ASVEMREC                                                      
         J     EXITL23             GO TO NEXT MODULE                            
         EJECT                                                                  
         DROP  R2,R                                                             
*&&                                                                             
***********************************************************************         
* READ GETOPT AND SAVE TO LOCAL STORAGE                               *         
***********************************************************************         
         USING GOBLOCKD,R3                                                      
RDGOPT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**RDGO**'                                                      
         L     R3,AGOBLOCK                                                      
         MVC   GOADM,DATAMGR                                                    
         MVC   GOAEXT,AGOXBLCK     US USES 1ST EXTENSION BLOCK                  
         MVC   GOABEXT,AGOBBLCK    UK USES 2ND EXTENSION BLOCK                  
         MVC   GOABUFF,APRGBUFF                                                 
         LHI   RE,PRGBUFFX-PRGBUFF                                              
         ST    RE,GOLBUFF                                                       
         MVC   GOSELCUL(1),COCODE                                               
         MVC   GOSELCUL+1(2),SJUL                                               
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELPRO,SPACES                                                  
         MVI   GOWHICH,0                                                        
         MVC   GOCTRY,COMPLANG                                                  
         L     RF,ADCOMFAC                                                      
         MVC   GOACOVL,COVAIL                                                   
         MVC   GOABINSR,BINSRCH                                                 
         MVC   GOSELOFC,SORTOFF                                                 
*                                                                               
         LA    R2,SORTCPJ                                                       
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   GOSELCLI(0),0(R2)                                                
         EX    RE,0(RF)                                                         
         AHI   RE,1                                                             
         AR    R2,RE                                                            
         LLC   RF,PPROLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   GOSELPRO(0),0(R2)                                                
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         AR    R2,RF                                                            
         LLC   RE,PPROLEN                                                       
         LLC   RF,JOBLEN                                                        
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   GOSELJOB(0),0(R2)                                                
         EX    RF,0(RE)                                                         
*                                                                               
         LLC   RF,PPROLEN                                                       
         LA    RF,SORTCPJ(RF)                                                   
         MVC   GOSELMED,0(RF)                                                   
*                                                                               
RDGOPT02 GOTO1 GETOPT,DMCB,GOBLOCKD                                             
         JE    EXITE                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* READ GETOPT AND SAVE TO LOCAL STORAGE                               *         
***********************************************************************         
         SPACE 1                                                                
BLDDSC   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**BLDD**'                                                      
         L     RF,AEMAIL1                                                       
         MVC   0(L'AC@EMNL1,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL1,RF),AC@EMNL1                                        
*                                                                               
         L     RF,AEMAIL2                                                       
         MVC   0(L'AC@EMNL2,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL2,RF),AC@EMNL2                                        
*                                                                               
         L     RF,AEMAIL3                                                       
         MVC   0(L'AC@EMNL3,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL3,RF),AC@EMNL3                                        
*                                                                               
         L     RF,AEMAIL4                                                       
         MVC   0(L'AC@EMNL4,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL4,RF),AC@EMNL4                                        
*                                                                               
         L     RF,AEMAIL5                                                       
         MVC   0(L'AC@EMNL5,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL5,RF),AC@EMNL5                                        
*                                                                               
         L     RF,AEMAIL6                                                       
         MVC   0(L'AC@EMNL6,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL6,RF),AC@EMNL6                                        
*                                                                               
         L     RF,AEMAIL7                                                       
         MVC   0(L'AC@EMNL7,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL7,RF),AC@EMNL7                                        
*                                                                               
         L     RF,AEMAIL8                                                       
         MVC   0(L'AC@EMNL8,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL8,RF),AC@EMNL8                                        
*                                                                               
         L     RF,AEMAIL9                                                       
         MVC   0(L'AC@EMNL9,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL9,RF),AC@EMNL9                                        
*                                                                               
         L     RF,AEMAILA                                                       
         MVC   0(L'AC@EMNLA,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNLA,RF),AC@EMNLA                                        
*                                                                               
         L     RF,AEMALS1                                                       
         MVC   0(L'AC@EMNL1,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL1,RF),AC@EMNL1                                        
*                                                                               
         L     RF,AEMALS2                                                       
         MVC   0(L'AC@EMNL2,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL2,RF),AC@EMNL2                                        
*                                                                               
         L     RF,AEMALS3                                                       
         MVC   0(L'AC@EMNL3,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL3,RF),AC@EMNL3                                        
*                                                                               
         L     RF,AEMALS4                                                       
         MVC   0(L'AC@EMNL4,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL4,RF),AC@EMNL4                                        
*                                                                               
         L     RF,AEMALS5                                                       
         MVC   0(L'AC@EMNL5,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL5,RF),AC@EMNL5                                        
*                                                                               
         L     RF,AEMALS6                                                       
         MVC   0(L'AC@EMNL6,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL6,RF),AC@EMNL6                                        
*                                                                               
         L     RF,AEMALS7                                                       
         MVC   0(L'AC@EMNL7,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL7,RF),AC@EMNL7                                        
*                                                                               
         L     RF,AEMALS8                                                       
         MVC   0(L'AC@EMNL8,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL8,RF),AC@EMNL8                                        
*                                                                               
         L     RF,AEMALS9                                                       
         MVC   0(L'AC@EMNL9,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL9,RF),AC@EMNL9                                        
*                                                                               
         L     RF,AEMALSA                                                       
         MVC   0(L'AC@EMNLA,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNLA,RF),AC@EMNLA                                        
*                                                                               
         L     RF,AEMAIL1          Squash up the disclaimer lines to            
         GOTO1 SQUASHER,DMCB,0(RF),MAXELEN make it neat and tidy                
         L     RF,AEMAIL3                                                       
         GOTO1 SQUASHER,DMCB,0(RF),MAXELEN                                      
         L     RF,AEMAIL6                                                       
         GOTO1 SQUASHER,DMCB,0(RF),MAXELEN                                      
         L     RF,AEMAIL8                                                       
         GOTO1 SQUASHER,DMCB,0(RF),MAXELEN                                      
         L     RF,AEMAILA                                                       
         GOTO1 SQUASHER,DMCB,0(RF),80                                           
*                                                                               
         L     RF,AEMALS1          Squash up the disclaimer lines to            
         GOTO1 SQUASHER,DMCB,0(RF),MAXELEN make it neat and tidy                
         L     RF,AEMALS3                                                       
         GOTO1 SQUASHER,DMCB,0(RF),MAXELEN                                      
         L     RF,AEMALS6                                                       
         GOTO1 SQUASHER,DMCB,0(RF),MAXELEN                                      
         L     RF,AEMALS8                                                       
         GOTO1 SQUASHER,DMCB,0(RF),MAXELEN                                      
         L     RF,AEMALSA                                                       
         GOTO1 SQUASHER,DMCB,0(RF),80                                           
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* Set the url for the application                                     *         
* On ntry P1=A(area for building url)                                 *         
***********************************************************************         
         SPACE 1                                                                
SETURL   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETURL*'                                                      
         L     R5,0(R1)                                                         
         GOTOR BLDURL                                                           
*                                                                               
         LA    R2,BLDFURL          Move url into                                
         LLH   R0,BLDLEN                                                        
*                                                                               
SETURL02 MVC   0(L'APTIMH,R5),TEXTSPCS                                          
         CHI   R0,L'APTIMH                                                      
         JNH   SETURL04                                                         
         MVC   0(L'APTIMH,R5),0(R2)                                             
         GOTO1 VSMTP,DMCB,('SMTPAPTL',0(R5))                                    
         SHI   R0,L'APTIMH                                                      
         AHI   R2,L'APTIMH                                                      
         J     SETURL02                                                         
*                                                                               
SETURL04 LR    RF,R0                                                            
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         MVC   0(0,R5),0(R2)                                                    
         EX    RF,0(R1)                                                         
         AHI   RF,1                                                             
         AR    R5,RF                                                            
         J     EXITR5                                                           
         EJECT                                                                  
***********************************************************************         
* Build the url for the application                                   *         
***********************************************************************         
         SPACE 1                                                                
BLDURL   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDURL*'                                                      
*                                                                               
         LA    R0,BLDFURL                                                       
         LHI   R1,L'BLDFURL                                                     
         SR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         XC    BLDLEN,BLDLEN                                                    
*                                                                               
         LA    R2,BLDFURL                                                       
         LA    RF,SVHTTP                                                        
         LHI   RE,L'SVHTTP-1                                                    
         CLI   SVFED,YESQ          Is this a system with a url prefix?          
         JNE   BLDURL02                                                         
         TM    CPXSTA,CPXFEDAT     Is federated auth in use?                    
         JZ    BLDURL02                                                         
         LA    RF,HTTP             Always http for federated urls               
         LHI   RE,L'HTTP-1                                                      
*                                                                               
BLDURL02 BASR  R1,0                                                             
         MVC   0(0,R2),0(RF)                                                    
         EX    RE,0(R1)                                                         
         AHI   R2,L'SVHTTP                                                      
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         TM    CPXSTA,CPXFEDAT     Is federated auth in use?                    
         JNZ   BLDURL04                                                         
         CLC   SVENV,SPACES        Any environment?                             
         JNH   BLDURL04                                                         
         MVC   0(L'AGUENV,R2),SVENV                                             
         AHI   R2,L'SVENV                                                       
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
*                                                                               
BLDURL04 CLI   SVFED,YESQ          Is this a system with a url prefix?          
         JNE   BLDURL06                                                         
         TM    CPXSTA,CPXFEDAT     Is federated auth in use?                    
         JZ    BLDURL06                                                         
         MVC   0(L'AGUFEDP,R2),SVFEDP                                           
         AHI   R2,L'SVFEDP                                                      
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         MVC   0(L'FEDURL,R2),FEDURL                                            
         AHI   R2,L'FEDURL                                                      
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         MVC   0(L'AGUFEDS,R2),SVFEDS                                           
         AHI   R2,L'SVFEDS                                                      
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
*                                  Squash and save new url length               
BLDURL06 MVC   0(L'SVURL,R2),SVURL                                              
         AHI   R2,L'SVURL                                                       
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         LA    RE,BLDFURL                                                       
         SR    R2,RE                                                            
         STH   R2,BLDLEN                                                        
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* Build the url for the summary emails                                *         
* On Ntry R4=SUMHTAB entry                                                      
***********************************************************************         
         SPACE 1                                                                
         USING SUMHTABD,R4                                                      
SETSURL  NTR1                                                                   
         J     *+12                                                             
         DC    C'*SETSUR*'                                                      
         GOTOR BLDURL                                                           
*                                                                               
         LA    R2,BLDFURL                                                       
         LLH   R0,BLDLEN                                                        
         AR    R2,R0                                                            
*                                  Move url to html                             
         L     RF,SUMHURL                                                       
         LLC   R1,SUMHURL                                                       
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R2),0(RF)       URL link                                     
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         AR    R2,R1                                                            
         MVC   0(L'LINKCB,R2),LINKCB                                            
         AHI   R2,L'LINKCB                                                      
         LA    RF,BLDFURL                                                       
         SR    R2,RF                                                            
         CHI   R2,L'BLDFURL        Ensure length is good                        
         JH    *+2                                                              
         STH   R2,BLDLEN                                                        
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* Build the url for the detailed emails                               *         
* On Ntry R4=AURHTAB entry                                                      
***********************************************************************         
         SPACE 1                                                                
         USING AURHTABD,R4                                                      
SETDURL  NTR1                                                                   
         J     *+12                                                             
         DC    C'*SETDUR*'                                                      
         GOTOR BLDURL                                                           
*                                                                               
         LA    R2,BLDFURL                                                       
         LLH   R0,BLDLEN                                                        
         AR    R2,R0                                                            
*                                                                               
         L     RF,AURHURL          Move in specific URL                         
         LLC   R1,AURHURL                                                       
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R2),0(RF)                                                    
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         AR    R2,R1                                                            
         MVC   0(L'LINKCB,R2),LINKCB                                            
         AHI   R2,L'LINKCB                                                      
         LA    RF,BLDFURL                                                       
         SR    R2,RF                                                            
         CHI   R2,L'BLDFURL        Ensure length is good                        
         JH    *+2                                                              
         STH   R2,BLDLEN                                                        
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* LTORG EQUATES AND CONSTANTS FOR WHOLE PROGRAM                       *         
***********************************************************************         
         SPACE 1                                                                
GLOBALS  DS    0D                                                               
URIENC   DC    V(URIENC)           ADDRESS OF URIENC                            
         LTORG                                                                  
MAXELEN  EQU   160                 MAXIMUM LENGTH OF EMAIL LINE                 
MAXORDQ  EQU   9                                                                
MAXORDPQ EQU   5                                                                
MAXORDL  EQU   ((L'RBDRORD+L'ORDRQBD)*MAXORDQ)+1                                
         SPACE 1                                                                
FILES    DC    0X                  LIST OF FILES MOVED INTO WORKING             
         DC    C'ACCFIL '          STORAGE SO WE HAVE ADDRESSABILITY            
         DC    C'ACCDIR '          EVERYWHERE                                   
         DC    C'ACCMST '                                                       
         DC    C'CTFILE '                                                       
FILESX   EQU   *-FILES                                                          
*                                                                               
AUFROM   DC    C'AuraNotifications<AuraNotifications@mediaocean.com>'           
AUADDR   DC    C'MEDIAOCEAN | 45 W. 18TH ST. | NEW YORK, NY 10011'              
AUHTML   DC    C'www.mediaocean.com'                                            
HTTP     DC    C'http://'                                                       
*                                  Approval email url (time)                    
SVFED    DS    CL1                 Saved fed system flag                        
SVTST    DS    CL1                 Saved test system flag                       
SVENV    DS    CL(L'AGUENV)        URL prefix                                   
SVHTTP   DS    CL(L'AGUHTT)        http/https                                   
SVURL    DS    CL(L'AGUURL2)       remaining URL                                
SVFEDP   DS    CL(L'AGUFEDP)       Federated prefix URL                         
SVFEDS   DS    CL(L'AGUFEDS)       Federate suffix URL                          
FEDURL   DS    CL250                                                            
BLDFURL  DS    CL320               Full url                                     
BLDLEN   DS    H                   length of url                                
*                               ** Aura standard email headings tab **          
AURHTAB  DS    0X                                                               
*                                                                               
         DC    AL1(SORTTIME)    Timesheets                                      
         DC    AL3(0)                                                           
         DC    AL1(L'AC@DTRYA)                                                  
         DC    AL3(AC@DTRYA)                                                    
         DC    AL1(L'AC@PCHTL)                                                  
         DC    AL3(AC@PCHTL)                                                    
         DC    AL1(L'AC@PEROP)                                                  
         DC    AL3(AC@PEROP)                                                    
         DC    AL1(APPRURTL)                                                    
         DC    AL3(APPRURT)                                                     
*                                                                               
         DC    AL1(SORTEXP)     Expenses                                        
         DC    AL3(0)                                                           
         DC    AL1(L'AC@DERYA)                                                  
         DC    AL3(AC@DERYA)                                                    
         DC    AL1(L'AC@PCHTE)                                                  
         DC    AL3(AC@PCHTE)                                                    
         DC    AL1(L'AC@FEXP1)                                                  
         DC    AL3(AC@FEXP1)                                                    
         DC    AL1(APPRURXL)                                                    
         DC    AL3(APPRURX)                                                     
*                                                                               
         DC    AL1(SORTORD)     Orders                                          
         DC    AL3(0)                                                           
         DC    AL1(L'AC@DORYA)                                                  
         DC    AL3(AC@DORYA)                                                    
         DC    AL1(L'AC@PCHOR)                                                  
         DC    AL3(AC@PCHOR)                                                    
         DC    AL1(L'AC@ORDNN)                                                  
         DC    AL3(AC@ORDNN)                                                    
         DC    AL1(APPRUROL)                                                    
         DC    AL3(APPRURO)                                                     
*                                                                               
         DC    AL1(SORTJOB)     Jobs                                            
         DC    AL3(0)                                                           
         DC    AL1(L'AC@DJRYA)                                                  
         DC    AL3(AC@DJRYA)                                                    
         DC    AL1(L'AC@PCHJO)                                                  
         DC    AL3(AC@PCHJO)                                                    
         DC    AL1(L'AC@JBCAD)                                                  
         DC    AL3(AC@JBCAD)                                                    
         DC    AL1(APPRURJL)                                                    
         DC    AL3(APPRURJ)                                                     
*                                                                               
         DC    AL1(SORTEST)     Estimates                                       
         DC    AL3(0)                                                           
         DC    AL1(L'AC@DESYA)                                                  
         DC    AL3(AC@DESYA)                                                    
         DC    AL1(L'AC@PCHES)                                                  
         DC    AL3(AC@PCHES)                                                    
         DC    AL1(L'AC@ESTNA)                                                  
         DC    AL3(AC@ESTNA)                                                    
         DC    AL1(APPRUREL)                                                    
         DC    AL3(APPRURE)                                                     
*                                                                               
         DC    AL1(SORTINV)     Invoices                                        
         DC    AL3(0)                                                           
         DC    AL1(L'AC@DISYA)                                                  
         DC    AL3(AC@DISYA)                                                    
         DC    AL1(L'AC@PCHIN)                                                  
         DC    AL3(AC@PCHIN)                                                    
         DC    AL1(L'AC@PEROP)                                                  
         DC    AL3(AC@PEROP)                                                    
         DC    AL1(APPRURIL)                                                    
         DC    AL3(APPRURI)                                                     
*                                                                               
AURHTABX DC    X'FF'                                                            
*                                                                               
SUMHTAB  DS    0X                                                               
*                                                                               
         DC    AL1(SORTTIME)    Timesheets                                      
         DC    AL3(0)                                                           
         DC    AL1(L'AC@PCHTL)                                                  
         DC    AL3(AC@PCHTL)                                                    
         DC    AL1(L'AC@ETME)                                                   
         DC    AL3(AC@ETME)                                                     
         DC    AL1(APPRURTL)                                                    
         DC    AL3(APPRURT)                                                     
         DC    AL1(SUMHULC+SUMHGLC)                                             
*                                                                               
         DC    AL1(SORTEXP)     Expenses                                        
         DC    AL3(0)                                                           
         DC    AL1(L'AC@PCHTE)                                                  
         DC    AL3(AC@PCHTE)                                                    
         DC    AL1(L'AC@EXPCL)                                                  
         DC    AL3(AC@EXPCL)                                                    
         DC    AL1(APPRURXL)                                                    
         DC    AL3(APPRURX)                                                     
         DC    AL1(SUMHULC+SUMHGLC)                                             
*                                                                               
         DC    AL1(SORTORD)     Orders                                          
         DC    AL3(0)                                                           
         DC    AL1(L'AC@PCHOR)                                                  
         DC    AL3(AC@PCHOR)                                                    
         DC    AL1(L'AC@ORD2)                                                   
         DC    AL3(AC@ORD2)                                                     
         DC    AL1(APPRUROL)                                                    
         DC    AL3(APPRURO)                                                     
         DC    AL1(SUMHULC+SUMHGLC)                                             
*                                                                               
         DC    AL1(SORTJOB)     Jobs                                            
         DC    AL3(0)                                                           
         DC    AL1(L'AC@PCHJO)                                                  
         DC    AL3(AC@PCHJO)                                                    
         DC    AL1(L'AC@JOBS)                                                   
         DC    AL3(AC@JOBS)                                                     
         DC    AL1(APPRURJL)                                                    
         DC    AL3(APPRURJ)                                                     
         DC    AL1(SUMHULC+SUMHGLC)                                             
*                                                                               
         DC    AL1(SORTEST)     Estimates                                       
         DC    AL3(0)                                                           
         DC    AL1(L'AC@PCHES)                                                  
         DC    AL3(AC@PCHES)                                                    
         DC    AL1(L'AC@ESTS)                                                   
         DC    AL3(AC@ESTS)                                                     
         DC    AL1(APPRUREL)                                                    
         DC    AL3(APPRURE)                                                     
         DC    AL1(SUMHULC)                                                     
*                                                                               
         DC    AL1(SORTINV)     Invoices                                        
         DC    AL3(0)                                                           
         DC    AL1(L'AC@PCHIN)                                                  
         DC    AL3(AC@PCHIN)                                                    
         DC    AL1(L'AC@INVS)                                                   
         DC    AL3(AC@INVS)                                                     
         DC    AL1(APPRURIL)                                                    
         DC    AL3(APPRURI)                                                     
         DC    AL1(SUMHULC+SUMHGLC)                                             
*                                                                               
SUMHTABX DC    X'FF'                                                            
*                                                                               
APPRURT  DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-time'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-time'                                              
         DC    X'50'                                                            
         DC    C'route=time/display/myTimesheetApprovals/AwaitingMe'            
APPRURTL EQU   *-APPRURT                                                        
*                                  Approval email url (jobs)                    
APPRURJ  DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-jobs'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-jobs'                                              
         DC    X'50'                                                            
         DC    C'route=jobs/display/myApprovals/AwaitingMe'                     
APPRURJL EQU   *-APPRURJ                                                        
*                                  Approval email url (orders)                  
APPRURO  DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-ords'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-ords'                                              
         DC    X'50'                                                            
         DC    C'route=orders/myApprovals/AwaitingMe'                           
APPRUROL EQU   *-APPRURO                                                        
*                                  Approval email url (estimates)               
APPRURE  DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-ests'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-ests'                                              
         DC    X'50'                                                            
         DC    C'route=estimates/display/myApprovals/AwaitingMe'                
APPRUREL EQU   *-APPRURE                                                        
*                                  Approval email url (invoices)                
APPRURI  DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-invs'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-invs'                                              
         DC    X'50'                                                            
         DC    C'route=invoices/myApprovals/AwaitingMe'                         
APPRURIL EQU   *-APPRURI                                                        
*                                  Approval email url (expenses)                
APPRURX  DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-exps'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-exps'                                              
         DC    X'50'                                                            
         DC    C'route=expenses/myApprovals/AwaitingMe'                         
APPRURXL EQU   *-APPRURX                                                        
*                                  Individual t/s display URL                   
DISPURLT DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-time'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-time'                                              
         DC    X'50'                                                            
         DC    C'route=time/display/timesheet'                                  
         DC    X'50'                                                            
DISPURTL EQU   *-DISPURLT                                                       
*                                                                               
DISPURLJ DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-jobs'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-jobs'                                              
         DC    X'50'                                                            
         DC    C'route=jobs/display/jobDisplay'                                 
         DC    X'50'                                                            
DISPURJL EQU   *-DISPURLJ                                                       
*                                                                               
DISPURLE DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-ests'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-ests'                                              
         DC    X'50'                                                            
         DC    C'route=estimates/display/estimateDisplay'                       
         DC    X'50'                                                            
DISPUREL EQU   *-DISPURLE                                                       
*                                                                               
DISPURLO DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-ords'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-ords'                                              
         DC    X'50'                                                            
         DC    C'route=orders/display/orderDisplay'                             
         DC    X'50'                                                            
DISPUROL EQU   *-DISPURLO                                                       
*                                                                               
DISPURLI DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-invs'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-invs'                                              
         DC    X'50'                                                            
         DC    C'route=invoices/invoiceDisplay'                                 
         DC    X'50'                                                            
DISPURIL EQU   *-DISPURLI                                                       
*                                                                               
DISPURLX DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-exps'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-exps'                                              
         DC    X'50'                                                            
         DC    C'route=expenses/display/expenseDisplay'                         
         DC    X'50'                                                            
DISPURXL EQU   *-DISPURLX                                                       
*                                                                               
VGETCAP  DC    V(GETCAP)                                                        
VGETRET  DC    A(0)                Now getting from comfacs                     
SQUASHER DC    V(SQUASHER)                                                      
COVAIL   DC    V(COVAIL)                                                        
VQSORT   DC    V(QSORT)                                                         
VHEXIN   DC    V(HEXIN)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VPRINTER DC    V(PRINTER)                                                       
*                                                                               
AELEMT   DC    A(ELEMT)                                                         
AELEMSEC DC    A(ELEMSECT)                                                      
AELEMTML DC    A(ELEMTML)                                                       
AELEMSEN DC    A(ELEMSEND)                                                      
AELE140E DC    A(ELEM140E)                                                      
AELE265E DC    A(ELEM265E)                                                      
AELE276E DC    A(ELEM276E)                                                      
AELE278E DC    A(ELEM278E)                                                      
AELE271E DC    A(ELEM271E)                                                      
AELE272E DC    A(ELEM272E)                                                      
AELE273E DC    A(ELEM273E)                                                      
AELE280E DC    A(ELEM280E)                                                      
AELE370E DC    A(ELEM370E)                                                      
AELE790E DC    A(ELEM790E)                                                      
AELE800E DC    A(ELEM800E)                                                      
AELE805E DC    A(ELEM805E)                                                      
AELE840E DC    A(ELEM840E)                                                      
AELEMOFF DC    A(ELEMOFF)                                                       
AELEMOEN DC    A(ELEMOEN)                                                       
AEMAIL1  DC    A(EMAILL1)                                                       
AEMAIL2  DC    A(EMAILL2)                                                       
AEMAIL3  DC    A(EMAILL3)                                                       
AEMAIL4  DC    A(EMAILL4)                                                       
AEMAIL5  DC    A(EMAILL5)                                                       
AEMAIL6  DC    A(EMAILL6)                                                       
AEMAIL7  DC    A(EMAILL7)                                                       
AEMAIL8  DC    A(EMAILL8)                                                       
AEMAIL9  DC    A(EMAILL9)                                                       
AEMAILA  DC    A(EMAILLA)                                                       
*                                                                               
AEMLSUMT DC    A(EMLSUMT)                                                       
AEMLSECT DC    A(EMLSSECT)                                                      
AEMLSEND DC    A(EMLSEND)                                                       
AEMS120E DC    A(EMLS120E)                                                      
AEMS255E DC    A(EMLS255E)                                                      
AEMS260E DC    A(EMLS260E)                                                      
AEMS261E DC    A(EMLS261E)                                                      
AEMS262E DC    A(EMLS262E)                                                      
AEMS285E DC    A(EMLS285E)                                                      
AEMS280E DC    A(EMLS280E)                                                      
AEMS282E DC    A(EMLS282E)                                                      
AEMS610E DC    A(EMLS610E)                                                      
AEMS620E DC    A(EMLS620E)                                                      
AEMS625E DC    A(EMLS625E)                                                      
AEMS660E DC    A(EMLS660E)                                                      
AEMALS1  DC    A(EMAILS1)                                                       
AEMALS2  DC    A(EMAILS2)                                                       
AEMALS3  DC    A(EMAILS3)                                                       
AEMALS4  DC    A(EMAILS4)                                                       
AEMALS5  DC    A(EMAILS5)                                                       
AEMALS6  DC    A(EMAILS6)                                                       
AEMALS7  DC    A(EMAILS7)                                                       
AEMALS8  DC    A(EMAILS8)                                                       
AEMALS9  DC    A(EMAILS9)                                                       
AEMALSA  DC    A(EMAILSA)                                                       
*                                                                               
SORTGET  DC    C'GET'                                                           
SORTPUT  DC    C'PUT'                                                           
SORTEND  DC    C'END'                                                           
PZERO    DC    P'0'                                                             
DATENDQ  DC    CL2'ND'                                                          
SJUL     DC    C'SJ'                                                            
ONERUL   DC    C'1R'                                                            
ONENUL   DC    C'1N'                                                            
SEPQ     DC    CL3'" "'            DOWNLOAD FILE SEPARATOR                      
ONERUL2  DC    C'1R'                                                            
ONENUL2  DC    C'1N'                                                            
ZERAMT   DC    CL4'0.00'                                                        
BUFFERR1 DC    C'**EMAIL BUFFER FULL**'                                         
BUFFERR2 DC    C'FOR COMPANY CODE='                                             
*                                  DOWNLOAD REPORT INDICATORS                   
DOWNINI  EQU   0                   INITIALISE                                   
DOWNADD  EQU   1                   ADD TEXT TO LINE                             
DOWNEWL  EQU   2                   NEW LINE                                     
DOWNEOL  EQU   3                   END LINE                                     
DOWNBLK  EQU   4                   BLANK FIELD                                  
DOWNNUM  EQU   5                   NUMBER                                       
DOWNCLOS EQU   6                   CLOSE REPORT                                 
*                                                                               
EMAILMAX EQU   90000               MAX NUMBER OF EMAILS                         
LINEMAX  EQU   170                 MAX NUMBER OF LINES PER EMAIL                
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
*                                                                               
TEXTSPCS DC    CL(MAXELEN)' '                                                   
*                                                                               
TBROPE   DC    C'<tr>'                                                          
TBRCLO   DC    C'</tr>'                                                         
TBDOPE   DC    C'<td>'                                                          
TBDCLO   DC    C'</td>'                                                         
TBDCOL   DC    C'<td colspan="5">'                                              
PARAOP   DC    C'<p style="color:#4E4D4C; font-size:13px; text-decorati+        
                on:none; line-height:16px;">'                                   
PARACL   DC    C'</p>'                                                          
LINKOP   DC    C'<a style="text-decoration:none;" href="'                       
LINKCB   DC    C'">'                                                            
LINKCL   DC    C'</a>'                                                          
PERTXT   DC    C'person='                                                       
ENDTXT   DC    C'end='                                                          
LOCTXT   DC    C'loc='                                                          
CLITXT   DC    C'client='                                                       
PROTXT   DC    C'product='                                                      
JOBTXT   DC    C'job='                                                          
ESTTXT   DC    C'estimate='                                                     
ORDTXT   DC    C'order='                                                        
INVTXT   DC    C'invoice='                                                      
EXPTXT   DC    C'expense='                                                      
*                                                                               
APRTAB   DS    0XL1                                                             
         DC    B'11111000'         Office/client/product/job/media              
         DC    C'1'                                                             
         DC    B'11011000'         Office/client/product/media                  
         DC    C'2'                                                             
         DC    B'10011000'         Office/client/media                          
         DC    C'3'                                                             
         DC    B'00011000'         Office/media                                 
         DC    C'4'                                                             
         DC    B'00010000'         Media                                        
         DC    C'M'                                                             
         DC    B'11101000'         Office/client/product/job                    
         DC    C'J'                                                             
         DC    B'11001000'         Office/client/product                        
         DC    C'P'                                                             
         DC    B'10001000'         Office/client                                
         DC    C'C'                                                             
         DC    B'00001000'         Office                                       
         DC    C'O'                                                             
         DC    B'00000000'         Agency                                       
         DC    C'A'                                                             
         DC    X'FF'                                                            
         SPACE 1                                                                
DATI     DS    0X                                                               
         DCDDL AC#EANFF,L'AC@EANFF EMAIL ADDRESS NOT FOUND FOR...               
         DCDDL AC#ANFFA,L'AC@ANFFA APPROVER NOT FOUND FOR ACCOUNT...            
         DCDDL AC#AURAA,L'AC@AURAA BRANDOCEAN APPROVALS NEEDED                  
         DCDDL AC#DTRYA,L'AC@DTRYA HERE ARE THE DETAILS OF THE TIME...          
         DCDDL AC#DERYA,L'AC@DERYA HERE ARE THE DETAILS OF THE EXPEN...         
         DCDDL AC#DORYA,L'AC@DORYA HERE ARE THE DETAILS OF THE ORDER...         
         DCDDL AC#DJRYA,L'AC@DJRYA HERE ARE THE DETAILS OF THE JOB...           
         DCDDL AC#DESYA,L'AC@DESYA HERE ARE THE DETAILS OF THE ESTIM...         
         DCDDL AC#DISYA,L'AC@DISYA HERE ARE THE DETAILS OF THE INVOIC..         
         DCDDL AC#PCHTL,L'AC@PCHTL PLEASE CLICK HERE TO VIEW ALL TIME..         
         DCDDL AC#PCHTE,L'AC@PCHTE PLEASE CLICK HERE TO VIEW ALL EXP..          
         DCDDL AC#PCHES,L'AC@PCHES PLEASE CLICK HERE TO VIEW ALL EST..          
         DCDDL AC#PCHJO,L'AC@PCHJO PLEASE CLICK HERE TO VIEW ALL JOB..          
         DCDDL AC#PCHOR,L'AC@PCHOR PLEASE CLICK HERE TO VIEW ALL ORD..          
         DCDDL AC#PCHIN,L'AC@PCHIN PLEASE CLICK HERE TO VIEW ALL INV..          
         DCDDL AC#PEROP,L'AC@PEROP FOR OFFICE PERSON NAME AND PERIOD            
         DCDDL AC#PERPE,L'AC@PERPE FOR PERSON NAME AND PERIOD END DATE          
         DCDDL AC#FEXP1,L'AC@FEXP1 FOR EXPENSE NUMBER AND 1R PERSON CO          
         DCDDL AC#JBCAD,L'AC@JBCAD FOR JOB CODE, NAME AND ADDED DATE            
         DCDDL AC#ESTNA,L'AC@ESTNA FOR ESTIMATE NUMBER, NAME AND ADDE           
         DCDDL AC#ORDNN,L'AC@ORDNN FOR ORDER NUMBER, NAME AND ADDED DAT         
         DCDDL AC#RSPID,L'AC@RSPID PID                                          
         DCDDL AC#EHBST,L'AC@EHBST EMAIL HAS BEEN SENT TO                       
         DCDDL AC#ETME,L'AC@ETME   TIMESHEETS                                   
         DCDDL AC#EXPCL,L'AC@EXPCL EXPENSE CLAIMS                               
         DCDDL AC#ORD2,L'AC@ORD2   ORDERS (HTML)                                
         DCDDL AC#ORDS,L'AC@ORDS   ORDERS                                       
         DCDDL AC#JOBS,L'AC@JOBS   JOBS                                         
         DCDDL AC#ESTS,L'AC@ESTS   ESTIMATES                                    
         DCDDL AC#INVS,L'AC@INVS   INVOICES                                     
         DCDDL AC#MOD,L'AC@MOD     MODULE                                       
         DCDDL AC#ETME,L'AC@ETUC,C TIMESHEETS                                   
         DCDDL AC#ETME,L'AC@ETUL,CU                                             
         DCDDL AC#EXPCL,L'AC@EXUC,C EXPENSE CLAIMS                              
         DCDDL AC#EXPCL,L'AC@EXUL,CU                                            
         DCDDL AC#ORDS,L'AC@ORUC,C ORDERS                                       
         DCDDL AC#ORDS,L'AC@ORUL,CU                                             
         DCDDL AC#JOBS,L'AC@JOUC,C JOBS                                         
         DCDDL AC#JOBS,L'AC@JOUL,CU                                             
         DCDDL AC#EST,L'AC@ESUC,C  ESTIMATES                                    
         DCDDL AC#EST,L'AC@ESUL,CU                                              
         DCDDL AC#INVS,L'AC@INUC,C INVOICES                                     
         DCDDL AC#INVS,L'AC@INUL,CU                                             
         DCDDL AC#CLPJO,L'AC@CLPJO CL/PR/JOB                                    
         DCDDL AC#CFNAM,L'AC@CFNAM FIRST NAME                                   
         DCDDL AC#CLNAM,L'AC@CLNAM LAST NAME                                    
         DCDDL AC#AMT,L'AC@AMT     AMOUNT                                       
         DCDDL AC#DATE,L'AC@DATE   DATE                                         
         DCDDL AC#ORDC,L'AC@ORDC   ORDER NO                                     
         DCDDL AC#SUPC,L'AC@LSUPC                                               
         DCDDL AC#SUPC,L'AC@SUPC   SUPPLIER CODE                                
         DCDDL AC#SUPN,L'AC@LSUPN                                               
         DCDDL AC#SUPN,L'AC@SUPN   SUPPLIER NAME                                
         DCDDL AC#JOBC,L'AC@JOBC   JOB CODE                                     
         DCDDL AC#JOBN,L'AC@JOBN   JOB NAME                                     
         DCDDL AC#ESTNO,L'AC@ESTNO EST. NO                                      
         DCDDL AC#ESTNM,L'AC@ESTNM ESTIMATE NAME                                
         DCDDL AC#RQRDB,L'AC@RQRDB REQUIRED BY                                  
         DCDDL AC#RQRDB,L'ACSRQRDB                                              
         DCDDL AC#TYPE,L'AC@TYPE   TYPE                                         
         DCDDL AC#INVUP,L'AC@INVUP INV posted                                   
         DCDDL AC#MIX,L'AC@MIX     MIX                                          
         DCDDL AC#EXPA,L'AC@EXPA   EXPENSES                                     
         DCDDL AC#PROC,L'AC@PROC   PRODUCT(ION)                                 
         DCDDL AC#YHAV,L'AC@YHAV   YOU HAVE                                     
         DCDDL AC#6E,L'AC@6E       BRANDOCEAN ESTIMATES                         
         DCDDL AC#EMNL1,L'AC@EMNL1 STANDARD EMAIL LINE 1                        
         DCDDL AC#EMNL2,L'AC@EMNL2 STANDARD EMAIL LINE 2                        
         DCDDL AC#EMNL3,L'AC@EMNL3 STANDARD EMAIL LINE 3                        
         DCDDL AC#EMNL4,L'AC@EMNL4 STANDARD EMAIL LINE 4                        
         DCDDL AC#EMNL5,L'AC@EMNL5 STANDARD EMAIL LINE 5                        
         DCDDL AC#EMNL6,L'AC@EMNL6 STANDARD EMAIL LINE 6                        
         DCDDL AC#EMNL7,L'AC@EMNL7 STANDARD EMAIL LINE 7                        
         DCDDL AC#EMNL8,L'AC@EMNL8 STANDARD EMAIL LINE 8                        
         DCDDL AC#EMNL9,L'AC@EMNL9 STANDARD EMAIL LINE 9                        
         DCDDL AC#EMNLA,L'AC@EMNLA STANDARD EMAIL LINE 9                        
         DCDDL AC#ANFE,L'AC@ANFE   APPROVER NOT FOUND FOR ESTIMATE NO.          
         DCDDL AC#NFAI,L'AC@NFAI   APPROVER NOT FOUND FOR INVOICE NO.           
         DCDDL AC#ANFO,L'AC@ANFO   APPROVER NOT FOUND FOR ORDER NO.             
         DCDDL AC#1RACC,L'AC@1RACC 1R ACCOUNT                                   
         DCDDL AC#MEDC,L'AC@MEDC   MEDIA                                        
         DCDDL AC#INVN,L'AC@INVN   INVOICE NUMBER                               
         DCDDL AC#ILGNO,L'AC@ILGNO LOG NO.                                      
         DCDDL AC#ORDER,L'AC@ORDER ORDER                                        
         DCDDL AC#TOAPP,L'AC@TOAPP TO APPROVE                                   
         DCDDL AC#OFFC,L'AC@OFFC   OFFICE                                       
         DCDDL AC#AGALF,L'AC@AGALF AGENCY ALPHA ID                              
         DCDDL AC#AGY,L'AC@AGY     AGENCY                                       
         DCDDL AC#RGSID,L'AC@RGSID LOGON REGISTERED UNDER                       
         DCDDL AC#TSFN,L'AC@TSFN   TIMESHEETS-FIRST NAME                        
         DCDDL AC#TSLN,L'AC@TSLN   TIMESHEETS-LAST NAME                         
         DCDDL AC#ANBE,L'AC@ANBE   BRANDOCEAN APPROVALS BY APPROVER             
         DCDDL AC#ANBE,L'AC@ANBU,CU                                             
         DCDDL AC#EXPNO,L'AC@EXPN  EXPENSE CLAIM #                              
         DCDDL AC#YRDT,L'AC@YRDT   YOUR DETAILS                                 
         DCDDL AC#FOR,L'AC@FOR     FOR                                          
         DCDDL AC#OFF,L'AC@OFF     OFFICE                                       
         DCDDL AC#MODE,L'AC@MODE   MEDIAOCEAN DE                                
*&&UK*&& DCDDL AC#MOUK,L'AC@MOUK   MEDIAOCEAN UK                                
*&&US*&& DCDDL AC#MOCA,L'AC@MOCA   MEDIAOCEAN CA                                
*&&US*&& DCDDL AC#MOC2,L'AC@MOC2   MEDIAOCEAN CA 2                              
         DCDDL AC#FUSAT,L'AC@FUSAT FIND US AT                                   
*&&UK*&& DCDDL AC#MUKHT,L'AC@MUKHT MEDIAOCEAN UK/GERMAN WEBSITE ADDRESS         
*&&US*&& DCDDL AC#MCAHT,L'AC@MCAHT MEDIAOCEAN CANADA    WEBSITE ADDRESS         
         DCDDL AC#PIDTE,L'AC@PIDTE PID IS TERMINATED                            
DATIX    DC    X'00'                                                            
*                                                                               
DATO     DS    0C                                                               
AC@EANFF DS    CL30                                                             
AC@ANFFA DS    CL35                                                             
AC@AURAA DS    CL80                                                             
AC@DTRYA DS    CL80                                                             
AC@DERYA DS    CL80                                                             
AC@DORYA DS    CL80                                                             
AC@DJRYA DS    CL80                                                             
AC@DESYA DS    CL80                                                             
AC@DISYA DS    CL80                                                             
AC@PCHTL DS    CL80                                                             
AC@PCHTE DS    CL80                                                             
AC@PCHES DS    CL80                                                             
AC@PCHJO DS    CL80                                                             
AC@PCHOR DS    CL80                                                             
AC@PCHIN DS    CL80                                                             
AC@PEROP DS    CL80                                                             
AC@PERPE DS    CL80                                                             
AC@FEXP1 DS    CL36                                                             
AC@JBCAD DS    CL33                                                             
AC@ESTNA DS    CL40                                                             
AC@ORDNN DS    CL62                                                             
AC@RSPID DS    CL3                                                              
AC@EHBST DS    CL22                                                             
AC@ETME  DS    CL10                                                             
AC@EXPCL DS    CL14                                                             
AC@ORD2  DS    CL12                                                             
AC@ORDS  DS    CL8                                                              
AC@JOBS  DS    CL4                                                              
AC@ESTS  DS    CL9                                                              
AC@INVS  DS    CL18                                                             
AC@MOD   DS    CL6                                                              
AC@ETUC  DS    CL10                                                             
AC@ETUL  DS    CL10                                                             
AC@EXUC  DS    CL14                                                             
AC@EXUL  DS    CL14                                                             
AC@ORUC  DS    CL8                                                              
AC@ORUL  DS    CL8                                                              
AC@JOUC  DS    CL4                                                              
AC@JOUL  DS    CL4                                                              
AC@ESUC  DS    CL8                                                              
AC@ESUL  DS    CL8                                                              
AC@INUC  DS    CL20                                                             
AC@INUL  DS    CL20                                                             
AC@CLPJO DS    CL9                                                              
AC@CFNAM DS    CL10                                                             
AC@CLNAM DS    CL9                                                              
AC@AMT   DS    CL6                                                              
AC@DATE  DS    CL5                                                              
AC@ORDC  DS    CL8                                                              
AC@LSUPC DS    CL15                                                             
AC@SUPC  DS    CL13                                                             
AC@LSUPN DS    CL17                                                             
AC@SUPN  DS    CL13                                                             
AC@JOBC  DS    CL8                                                              
AC@JOBN  DS    CL8                                                              
AC@ESTNO DS    CL7                                                              
AC@ESTNM DS    CL13                                                             
AC@RQRDB DS    CL12                                                             
ACSRQRDB DS    CL8                                                              
AC@TYPE  DS    CL4                                                              
AC@INVUP DS    CL11                                                             
AC@MIX   DS    CL4                                                              
AC@EXPA  DS    CL4                                                              
AC@PROC  DS    CL4                                                              
AC@YHAV  DS    CL9                                                              
AC@6E    DS    CL20                                                             
AC@EMNL1 DS    CL80                                                             
AC@EMNL2 DS    CL80                                                             
AC@EMNL3 DS    CL80                                                             
AC@EMNL4 DS    CL80                                                             
AC@EMNL5 DS    CL80                                                             
AC@EMNL6 DS    CL80                                                             
AC@EMNL7 DS    CL80                                                             
AC@EMNL8 DS    CL80                                                             
AC@EMNL9 DS    CL80                                                             
AC@EMNLA DS    CL80                                                             
AC@ANFE  DS    CL37                                                             
AC@NFAI  DS    CL37                                                             
AC@ANFO  DS    CL35                                                             
AC@1RACC DS    CL10                                                             
AC@MEDC  DS    CL5                                                              
AC@INVN  DS    CL14                                                             
AC@ILGNO DS    CL7                                                              
AC@ORDER DS    CL5                                                              
AC@TOAPP DS    CL13                                                             
AC@OFFC  DS    CL6                                                              
AC@AGALF DS    CL15                                                             
AC@AGY   DS    CL7                                                              
AC@RGSID DS    CL22                                                             
AC@TSFN  DS    CL21                                                             
AC@TSLN  DS    CL20                                                             
AC@ANBE  DS    CL30                                                             
AC@ANBU  DS    CL30                                                             
AC@EXPN  DS    CL14                                                             
AC@YRDT  DS    CL19                                                             
AC@FOR   DS    CL3                                                              
AC@OFF   DS    CL6                                                              
AC@MODE  DS    CL65                                                             
*&&UK                                                                           
AC@MOUK  DS    CL72                                                             
*&&                                                                             
*&&US                                                                           
AC@MOCA  DS    CL80                                                             
AC@MOC2  DS    CL9                                                              
*&&                                                                             
AC@FUSAT DS    CL18                                                             
AC@MUKHT DS    CL20                                                             
*&&US                                                                           
AC@MCAHT DS    CL20                                                             
*&&                                                                             
AC@PIDTE DS    CL22                PID TERMINATED                               
DATOX    DS    0C                                                               
*                                                                               
AMONTAB  DC    A(MONTAB)           MONTH TABLE LOOKUP                           
ASVEMREC DC    A(SVEMREC)          ADDRESS OF SAVED EMAIL RECORD                
ACOBLOCK DC    A(CCOBLOCK)         ADDRESS OF COBLOCK                           
AGOXBLCK DC    A(GOXBLCKA)         ADDRESS OF GOXBLOCK                          
AGOBBLCK DC    A(GOBBLCKA)         ADDRESS OF GOBBLOCK                          
AGOBLOCK DC    A(GOBLOCKB)         ADDRESS OF GOBLOCK                           
APRGBUFF DC    A(PRGBUFF)          ADDRESS OF PROGRAM BUFFER AREA               
APIDBLOC DC    A(PIDBLOCK)         ADDRESS OF PIDBLOCK                          
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
* **NOTE** WE'RE RUNNING OUT OF STORAGE!!!                            *         
***********************************************************************         
WORKD    DSECT                                                                  
AIOAREA  DS    A                   ADDRESS OF IO AREA                           
AIOAREA2 DS    A                   ADDRESS OF IO AREA                           
EMLTBLN  DS    F                   LENGTH OF EMAIL TAB                          
AEMLTAB  DS    A                   ADDRESS OF EMAIL TAB                         
ACURETB  DS    A                   ADDRESS OF CURRENT RECORD IN EMAIL           
ANXTETB  DS    A                   ADDRESS OF NEXT ENTRY IN EMAIL TAB           
AEMPROF  DS    A                   ADDRESS OF EMAIL PROFILE                     
AEMPROF2 DS    A                   ADDRESS OF EMAIL PROFILE 2 RECORD            
AORDPROF DS    A                   ADDRESS OF ORDER PROFILE                     
AJOBAPP  DS    A                   ADDRESS OF JOB APPROVER TABLE                
AAURURL  DS    A                   Address of AURTAB                            
VGETURL  DS    A                   Address of GETURL module                     
*                                                                               
NTFY#    DS    XL2                 NUMBER OF DAYS  BEFORE NOTIFYING             
NTFYHRS  DS    XL2                 NUMBER OF HOURS BEFORE NOTIFYING             
NTFYDTE  DS    PL3                 NOTIFY DATE                                  
LOCK#    DS    XL2                 NUMBER OF DAYS  BEFORE LOCKING               
LOCKHRS  DS    XL2                 NUMBER OF HOURS BEFORE LOCKING               
LOCKDTE  DS    PL3                 LOCK DATE                                    
NTFYLNQ  EQU   *-NTFY#                                                          
TODAYP   DS    PL3                 TODAYS DATE (Packed)                         
TODAYB   DS    XL3                 TODAYS DATE (Binary)                         
TODAYC   DS    XL2                 TODAYS DATE (Compressed)                     
TODAY    DS    CL8                 TODAYS DATE(PRINTABLE)                       
TODAYL   DS    CL8                 TODAY'S DATE                                 
TODAY6   DS    CL6                 TODAY'S DATE IN C'YYMMDD' 6-BYTE FMT         
*                                                                               
AFILES   DS    0X                                                               
ACCFIL   DS    CL7                                                              
ACCDIR   DS    CL7                                                              
ACCMST   DS    CL7                                                              
CTFILE   DS    CL7                                                              
*                                                                               
COCODE   DS    XL1                 COMPANY CODE                                 
COMPLANG DS    XL1                 COMPANY LANGUAGE                             
LLANG    DS    XL1                 LAST LANGUAGE CODE                           
CTRY     DS    XL1                 COUNTRY                                      
COMPCTRY DS    XL1                 COMPANY COUNTRY                              
REQPRE   DS    CL1                 REQUISITION NO. PREFIX                       
REQSUF   DS    CL1                 REQUISITION NO. SUFFIX                       
ALPCODE  DS    CL2                 AGENCY ALPHA CODE FROM COMPANY ELEM          
SECCODE  DS    CL2                 AGENCY ALPHA CODE FROM CTFILE                
SVOFF    DS    CL2                 SAVED OFFICE CODE                            
SVPCODE  DS    CL7                 SAVED PERSON CODE                            
SVDATE   DS    PL3                 SAVED T/S DATE                               
LOWDTE   DS    PL3                 EARLIEST DATED TIMESHEET                     
CONAME   DS    CL36                COMPANY NAME                                 
*OFFNAM   DS    CL36                OFFICE NAME                                 
SVINVN   DS    CL(L'REBKNUM)       SAVED INVOICE NUMBER                         
COUSID   DS    CL7                 SAVED USER ID                                
*                                                                               
CPXST4   DS    XL1                 SAVED COMPANY EXTRA STATUS 4                 
CPXSTA   DS    XL1                 SAVED COMPANY EXTRA STATUS A                 
*                                                                               
RCRUN    DS    XL1                 SYSTEM RUN INDICATOR (MCTSTRUN)              
RUNTST   EQU   X'FF'                                                            
RCDSPAC  DS    CL1                 DSPACE A=ADV,C=CSC,Q=FQA,R=REP,T=TST         
*                                                                               
CPYINDS  DS    XL1                 COMPANY INDICATOR                            
CPY2CHA  EQU   X'80'               2 CHAR OFFICE                                
*                                                                               
RUNIND1  DS    XL1                 RUN INDICATOR                                
RUNITERM EQU   X'80'               TERMINATED PID                               
RUNINOAP EQU   X'40'               NO APPROVER FOR ACCOUNT                      
RUNINOCP EQU   X'20'               NO CHARACTER PID EXISTS                      
RUNIDEFT EQU   X'08'               DONE DEFAULT APPROVERS                       
RUNINDEF EQU   X'04'               DONE NON-DEFAULT APPROVERS                   
RUNILVL  EQU   X'02'               FOUND APPROVERS FOR LEVEL                    
RUNIGLVL EQU   X'01'               GOT APPROVERS FOR LEVEL                      
*                                                                               
RUNIND2  DS    XL1                 SECOND RUN INDICATOR                         
RUN2TIM  EQU   X'80'               DOING TIMESHEETS                             
RUN2EXP  EQU   X'40'               DOING EXPENSES                               
RUN2ORD  EQU   X'20'               DOING ORDERS                                 
RUN2JOB  EQU   X'10'               DOING JOBS                                   
RUN2EST  EQU   X'08'               DOING ESTIMATES                              
RUN2INV  EQU   X'04'               DOING INVOICES                               
RUN2DEFT EQU   X'02'               IS DEFAULT APPROVER                          
RUN2DHED EQU   X'01'               DOWNLOADING HEADER                           
*                                                                               
RUNIND3  DS    XL1                 THIRD RUN INDICATOR                          
RUN3NTI  EQU   X'80'               NO MORE TIMESHEETS                           
RUN3NEX  EQU   X'40'               NO MORE EXPENSE CLAIMS (FIXED APPR)          
RUN3NOR  EQU   X'20'               NO MORE ORDERS                               
RUN3NJO  EQU   X'10'               NO MORE JOBS                                 
RUN3NES  EQU   X'08'               NO MORE ESTIMATES                            
RUN3NIV  EQU   X'04'               NO MORE INVOICES                             
RUN3NPI  EQU   X'02'               NO MORE EXPENSE CLAIMS (CHOOSE APPR)         
*                                                                               
RUNIND4  DS    XL1                 ERROR HEADINGS INDICATOR                     
RUNTHED  EQU   X'80'               TIMESHEETS                                   
RUNEHED  EQU   X'40'               EXPENSES                                     
RUNOHED  EQU   X'20'               ORDERS                                       
RUNJHED  EQU   X'10'               JOBS                                         
RUNSHED  EQU   X'08'               ESTIMATES                                    
RUNIHED  EQU   X'04'               INVOICES                                     
RUNIAPP  EQU   X'02'               INVOICES MODULE GOT APPROVERS                
RUNIGAPP EQU   X'01'               LOOK FOR APPROVALS ONLY ON INV REC           
*                                  GETAPP APPROVER INDICATOR                    
RUNIND5  DS    XL1                                                              
RUN5LI2  EQU   X'80'               USING PSECOND ON DOWNLOAD REPORT             
RUN5LI3  EQU   X'40'               USING PTHIRD ON DOWNLOAD REPORT              
*                                                                               
PCLILEN  DS    XL1                 CLIENT LENGTH                                
PPROLEN  DS    XL1                 PRODUCT LENGTH                               
JOBLEN   DS    XL1                 JOB LENGTH                                   
*                                                                               
ONERCODE DS    CL12                1R ACCOUNT CODE                              
ONERL1L  DS    XL1                 OFFICE LENGTH                                
ONERL2L  DS    XL1                 DEPARTMENT LENGTH                            
ONERL3L  DS    XL1                 SUB-DEPT LENGTH                              
ONERL4L  DS    XL1                 PERSON LENGTH                                
*                                                                               
CUROFFC  DS    CL2                                                              
CURDPT   DS    CL6                                                              
CURSDP   DS    CL6                                                              
CURODS   DS    CL8                                                              
*                                                                               
SORTSW   DS    XL1                 SORTER INDICATOR BYTE                        
NUMRECS  DS    F                   NUMBER OF RECORDS IN EMAILBUF                
NUMREC2  DS    F                   NUMBER OF RECORDS IN EMAILBUF                
NUMAPPR  DS    F                   NUMBER OF RECORD TO APPROVE                  
NUMEMLS  DS    F                   NUMBER OF EMAILS                             
NUMPUT   DS    F                   NUMBER OF ITEMS ON EMAIL                     
NUMTOT   DS    F                   NUMBER OF LINES ON EMAIL                     
SAVERF   DS    F                   SAVED REGISTER RF                            
SVDA     DS    XL4                                                              
*                                                                               
TOTNUMP  DS    F                   TOTAL NUMBER OF RECORDS PUT TO SORT          
TOTNUMR  DS    F                   TOTAL NUMBER OF RECORDS FOR RUN              
TOTNUME  DS    F                   TOTAL NUMBER OF EMAILS FOR RUN               
*                                                                               
TEXT1    DS    CL(MAXELEN)                                                      
TEXT2    DS    CL80                                                             
TEXT3    DS    CL80                                                             
*                                                                               
APPFSTNM DS    CL15                APPROVER FIRST NAME                          
APPMIDNM DS    CL15                APPROVER MIDDLE NAME                         
APPLSTNM DS    CL58                APPROVER LAST NAME                           
*                                                                               
PERFSTNM DS    CL15                PERSON FIRST NAME                            
PERMIDNM DS    CL15                PERSON MIDDLE NAME                           
PERLSTNM DS    CL58                PERSON LAST NAME                             
*                                                                               
APPRPID  DS    XL2                 APPROVER PID                                 
PIDCHAR  DS    CL8                 PERSONAL-ID CHAR                             
APPEMAIL DS    CL50                APPROVER EMAIL ADDRESS                       
*                                                                               
ACCNAM   DS    CL(L'NAMEREC)       SUPPLIER NAME                                
LACCNAM  DS    XL1                                                              
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
SEQTAB   DS    0X                  SORT SEQUENCE TABLE                          
SEQTIME  DS    CL1                 (USES SORTEQUS)                              
SEQEXP   DS    CL1                                                              
SEQORD   DS    CL1                                                              
SEQJOB   DS    CL1                                                              
SEQEST   DS    CL1                                                              
SEQINV   DS    CL1                                                              
SEQTABL  EQU   *-SEQTAB                                                         
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
SVEMAD   DS    CL50                SAVED EMAIL ADDRESS                          
*                                                                               
EMPROF   DS    CL16                EMAIL PROFILE RECORDS                        
EMPROF2  DS    CL16                                                             
ORDPROF  DS    CL16                ORDER PROFILES                               
*                                                                               
***********************************************************************         
* I/O KEYS                                                            *         
***********************************************************************         
IOCOMP   DS    XL1                 SPECIFIC COMPANY CODE                        
IOKEYSV  DS    XL(L'ACTKEY)        SAVED IO KEY                                 
IOKEY1   DS    CL64                I/O KEY                                      
IOKEY2   DS    CL64                I/O KEY                                      
IOKEY3   DS    CL64                I/O KEY                                      
CSVKEY1  DS    CL64                SAVED I/O KEY                                
CSVKEY2  DS    CL64                SAVED I/O KEY                                
CSVKEY3  DS    CL64                SAVED I/O KEY                                
*                                                                               
***********************************************************************         
* SORT RECORD                                                         *         
***********************************************************************         
SORTREC  DS    XL(SORTRECL)        SORT RECORD                                  
*                                                                               
***********************************************************************         
* FROM HERE ON OUTSIDE RC WORKD USE (RELATIVE ADDRESSING)             *         
***********************************************************************         
SORTCARD DS    CL80                SORT RECORD FIELDS                           
SORTTYPE DS    CL80                                                             
*                                                                               
TEMP100  DS    XL100               (GETIID)                                     
JOBAPP   DS    XL32                SAVED JOB APPROVERS                          
* storage below is for debug/test runs.                                         
TARPID   DS    CL8             APPROVER FILTER                                  
TARPIN   DS    XL2             PIN DERIVED FROM TARPID, NEEDS PARMDAG           
TESTMAIL DS    CL50     override email address to use (QCNT=C)                  
*ESTSTDT DC    XL3'00'             PACKED START DATE FILTER                     
*ESTENDT DS    XL3'00'             PACKED END DATE FILTER                       
         DS    0H                                                               
*                                                                               
SAVEREC  DS    XL(SORTRECL)        SAVE RECORD                                  
*                                                                               
***********************************************************************         
* I/O AREAS                                                           *         
***********************************************************************         
IOAREA   DS    XL2000                  I/O AREA                                 
IOAREA2  DS    XL2000                  I/O AREA                                 
*                                                                               
WORKX    EQU   *-WORKD                                                          
***********************************************************************         
* EMAIL DSECT FOR EMAIL BUFFER                                        *         
***********************************************************************         
EMRECD   DSECT                                                                  
EMADDR   DS    CL50                APPROVER EMAIL ADDRESS                       
EMAPID   DS    CL8                 APPROVER CHARACTER PID                       
EMAPIN   DS    XL2                 APPROVER BINARY PID                          
EMASEQ   DS    CL1                 SORT SEQUENCE                                
*                                                                               
EMORDN   DS    CL8                 ORDER NO/REQUISTION NUMBER                   
         ORG   EMORDN                                                           
EMILN    DS    CL7                 INVOICE LOG NUMBER                           
         DS    CL1                                                              
EMDATE   DS    PL3                 PERIOD END DATE                              
EMLENG   EQU   *-EMADDR            RECORD LENGTH(PROETAB)                       
EMLENG2  EQU   *-EMAPID            SECOND RECORD LENGTH (DETREP)                
*                                **DUPLICATE FIELDS TO GET SORT ORDER           
*                                **CORRECT                                      
EMASEQ2  DS    CL1                 SECOND BRANDOCEAN MODULE(FOR PRTDET)         
EMAPID2  DS    CL8                 APPROVER CHARACTER PID(FOR PRTDET)           
EMLENG3  EQU   *-EMASEQ2           THIRD RECORD LENGTH(PRTDET)                  
EMASEQ3  DS    CL1                 THIRD BRANDOCEAN MODULE(FOR EM2SOFF)         
EMAOFF   DS    CL2                 OFFICE CODE OF ORIGIN LOGON                  
EMDATE2  DS    PL3                 SECOND DATE FOR SORTING PURPORSES            
EMLENG4  EQU   *-EMAPID2           FOURTH RECORD LENGTH (EM2SOFF)               
EMAMODL  DS    CL1                 BRANDOCEAN MODULE                            
EMATIME  EQU   SORTTIME            TIMESHEETS                                   
EMAEXP   EQU   SORTEXP             EXPENSES                                     
EMAORD   EQU   SORTORD             ORDERS                                       
EMAJOB   EQU   SORTJOB             JOBS                                         
EMAEST   EQU   SORTEST             ESTIMATES                                    
EMAINV   EQU   SORTINV             INVOICES                                     
EMESTN   DS    CL6                 ESTIMATE NO.                                 
*                                  /ESTIMATE NUMBER                             
EMAALP   DS    CL2                 AGENCY ALPHA ID                              
EMRBDAT  DS    PL3                 ORDER REQUIRED BY DATE                       
         ORG   EMRBDAT                                                          
EMRLDAT  DS    PL3                 LOCATION END DATE                            
EMULA    DS    0CL14               U/L/ACC                                      
EMULC    DS    CL2                 UNIT AND LEDGER                              
EMACC    DS    CL12                ACCOUNT CODE                                 
EMPERC   DS    CL7                 PERSON CODE                                  
EMAMNT   DS    PL6                 ORDER/EXPENSE AMOUNT                         
EMASTAT  DS    XL1                 RECORD STATUS BYTE                           
EMACPY   DS    XL1                 COMPANY SUBMITTED UNDER                      
EMALCK   EQU   SORTLCK             X'80'-APPROVER HAS OVERDUE T/S               
         DS    XL113               FREE SPACE                                   
EMRECL   EQU   *-EMRECD                                                         
         ORG   EMULA                                                            
EMSUP    DS    XL14                SUPPLIER ACCOUNT CODE                        
         ORG   EMAMNT+L'EMAMNT     **JOB MODULE                                 
EMJOBN   DS    CL36                JOB NAME                                     
EMDIND   DS    XL1                 JOB EMAIL INDICATOR                          
DEFAPP   EQU   X'80'               YES DEFAULT APPROVER                         
         ORG   EMAMNT+L'EMAMNT     **ESTIMATE MODULE**                          
EMESTNM  DS    CL50                ESTIMATE NAME                                
         ORG   EMAMNT+L'EMAMNT     **ORDER MODULE**                             
EMOCPJ   DS    CL14                SJ CLI/PRO/JOB                               
EMORDNM  DS    CL50                ORDER NAME                                   
         ORG   EMAMNT+L'EMAMNT     **INVOICE MODULE**                           
EMAINTR  DS    CL20                REFERENCE NUMBER                             
EMIORDN  DS    CL6                 INVOICE ORDER NUMBER                         
EMDTUPD  DS    XL2                 INVOICE UPDATED DATE                         
EMAITYP  DS    CL1                 INVOICE TYPE                                 
EMAIORD  DS    CL(MAXORDL)         INVOICE ORDERS                               
         ORG                                                                    
***********************************************************************         
* SORT RECORD DSECT                                                   *         
***********************************************************************         
SORTRECD DSECT                                                                  
*                                                                               
SORTKEY  DS    0XL51               SORT KEY                                     
SORTSEQ  DS    CL1                 SORT SEQUENCE                                
SORTCPJ  DS    CL12                ACCOUNT CODE                                 
SORTUL   DS    0CL2                UNIT LEDGER                                  
SORTUNT  DS    CL1                 UNIT                                         
SORTLDG  DS    CL1                 LEDGER                                       
SORT1R   DS    CL12                1R ACCOUNT CODE                              
SORT1N   DS    CL12                1N ACCOUNT CODE                              
SORTMED  DS    CL1                 MEDIA CODE                                   
SORTDATE DS    PL3                 PERIOD END DATE IN 2'S COMPLEMENT            
SORTLEND DS    PL3                 LOCATION END DATE IN 2'S COMPLEMENT          
SORTNMBR DS    CL8                 ORDER NO./ESTIMATE NO./INV LOG NO.           
*                                  /ORDER REQUISITION NO./EXPENSE NO.           
SORTOFF  DS    CL2                 OFFICE CODE                                  
SORTPID9 DS    XL2                 ESTIMATE CLIENT/INTERNAL APPROVER            
*                                  OR EXPENSE APPROVER IF CHOSEN                
SORTKEYL EQU   *-SORTRECD          KEY LENGTH                                   
*                                                                               
SORTDATA DS    0XL190              (adjust if MAXORDQ changes)                  
SORTMODL DS    CL1                 BRANDOCEAN MODULE(SEE SORTEQUS ABOV)         
SORTALP  DS    CL2                 AGENCY ALPHA ID                              
SORTAMNT DS    PL6                 ORDER AMOUNT                                 
SORTSUP  DS    XL14                SUPPLIER ACCOUNT NO.                         
SORTINTR DS    CL20                INVOICE REFERENCE                            
SORTION  DS    CL6                 INVOICE ORDER NUMBER                         
SORTSTAT DS    XL1                 SORT STATUS BYTE                             
SORTLCK  EQU   X'80'               APPROVER LOCK                                
SORTCAP  EQU   X'40'               EXPENSES APPR CHOSEN (SEE SORTPID9)          
SORTNAME DS    CL36                JOB NAME                                     
SORTESTN DS    0CL50               ESTIMATE NAME                                
SORTORDN DS    CL100               ORDER NAME                                   
SORTIORD DS    CL(MAXORDL)         ORDERS ON INVOICES                           
*                                  **APPROVAL LEVELS**                          
SORTCLIL DS    CL1                 OFFICE CLIENT LEVEL                          
SORTMEDL DS    CL1                 MEDIA CLIENT LEVEL                           
SORTLIL  DS    CL1                 LINE MANAGER LEVEL                           
SORT1RL  DS    CL1                 1R LEVEL FOR WPP                             
SORTBNB  DS    CL1                 BILLABLE/NON BILLABLE                        
*                                  OTHER DATA                                   
SORTRBDT DS    PL3                 ORDER REQUIRED BY DATE                       
SORTDUPD DS    XL2                 INVOICE UPDATED DATE                         
SORTITYP DS    CL1                 INVOICE TYPE                                 
*                                                                               
* PIDS AND APPROVERS FOR OTHER MODULES                                          
*                                                                               
*                                  **ORDER MODULE/INVOICE MODULE**              
SORTPIDS DS    0X                    AND EXPENSE IF SORTCAP IS SET              
         DS    XL2                                                              
SORTPIDL EQU   *-SORTPIDS                                                       
         DS    20XL(L'PIDBIN)      LIST OF APPROVERS                            
SORTIPID EQU   *-SORTPIDS                                                       
SORTRECL EQU   *-SORTRECD                                                       
SORTIL   EQU   SORTIPID/SORTPIDL   NUMBER OF PIDS                               
         ORG                                                                    
***********************************************************************         
* BOX DSECT FOR SUMMARY REPORT                                        *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   DSECT                                                                  
PRTCL1   DS    XL1                 COLUMN #1            +1                      
PRTPID   DS    CL8                 PID                  +9                      
PRTCL2   DS    XL1                 COLUMN #2            +10                     
PRTFSTN  DS    CL15                APPROVER FIRST NAME  +25                     
PRTCL3   DS    XL1                 COLUMN #3            +26                     
PRTLSTN  DS    CL30                APPROVER LAST NAME   +56                     
PRTCL4   DS    XL1                 COLUMN #4            +57                     
PRTNMA   DS    CL15                NUMBER TO APPROVE    +72                     
PRTCL5   DS    XL1                 COLUMN#5             +73                     
PRTETS   DS    CL12                EARLIEST TIMESHEET   +85                     
PRTCL6   DS    XL1                 COLUMN#6             +86                     
PRTCOM   DS    CL38                COMMENTS             +124                    
PRTCL7   DS    XL1                 COLUMN #7            +125                    
         SPACE 1                                                                
***********************************************************************         
* BOX DSECT FOR DETAILED REPORT                                       *         
***********************************************************************         
PRTTIM   DSECT                                                                  
*                                                                               
PRTTIMH  DS    0X                  *** Approver header ***                      
         DS    XL2                                                              
PRTTHDAP DS    CL8                 Approver PID                                 
         DS    XL8                                                              
PRTTHDFN DS    CL15                Approver First Name                          
         DS    XL2                                                              
PRTTHDLN DS    CL15                Approver Last Name                           
*                                                                               
*                                  **TIMESHEET MODULE**                         
         ORG   PRTTIMH                                                          
         DS    XL1                 N/D                  +1                      
PRTTCL1  DS    XL1                 COLUMN #1            +2                      
PRTTCPJ  DS    CL15                CLIENT PRODUCT JOB   +17                     
PRTTCL2  DS    XL1                 COLUMN #2            +18                     
PRTTFNM  DS    CL16                FIRST NAME           +34                     
PRTTCL3  DS    XL1                 COLUMN #3            +35                     
PRTTLNM  DS    CL54                LAST NAME            +89                     
PRTTCL4  DS    XL1                 COLUMN #4            +90                     
PRTTDAT  DS    CL8                 DATE                 +98                     
PRTTCL5  DS    XL1                 COLUMN #5            +99                     
*                                                                               
*                                  **EXPENSES MODULE**                          
         ORG   PRTTLNM                                                          
PRTTLN2  DS    CL36                LAST NAME            +71                     
PRTTCL6  DS    XL1                 COLUMN #5            +72                     
PRTTCAM  DS    CL14                AMOUNT               +85                     
PRTTCL7  DS    XL1                 COLUMN #6            +86                     
PRTTDA2  DS    CL8                 DATE                 +95                     
PRTTCL8  DS    XL1                 COLUMN #7            +96                     
*                                                                               
*                                  **ORDER MODULE**                             
         ORG   PRTTCPJ                                                          
PRTTORD  DS    CL15                ORDER NUMBER         +17                     
PRTTCL9  DS    XL1                 COLUMN #8            +18                     
PRTTDA3  DS    CL8                 ORDER DATE           +26                     
PRTTCLA  DS    XL1                 COLUMN #9            +27                     
PRTTSUP  DS    CL16                SUPPLIER CODE        +43                     
PRTTCLB  DS    XL1                 COLUMN #10           +44                     
PRTTSUN  DS    CL36                SUPPLIER NAME        +80                     
PRTTCLC  DS    XL1                 COLUMN #11           +81                     
PRTTOMT  DS    CL14                ORDER AMOUNT         +95                     
PRTTCLD  DS    XL1                 COLUMN #12           +96                     
PRTTRBD  DS    CL12                REQUIRED BY DATE     +108                    
PRTTCLD1 DS    XL1                 COLUMN #12/1         +109                    
*                                                                               
*                                  **JOBS MODULE**                              
         ORG   PRTTCPJ                                                          
PRTTJBC  DS    CL14                JOB CODE             +16                     
PRTTCLE  DS    XL1                 COLUMN #13           +17                     
PRTTJBN  DS    CL72                JOB NAME             +88                     
PRTTCLF  DS    XL1                 COLUMN #14           +89                     
PRTTDA4  DS    CL8                 JOB DATE             +98                     
PRTTCLG  DS    XL1                 COLUMN #15           +99                     
*                                                                               
*                                  **ESTIMATE MODULE**                          
         ORG   PRTTCPJ                                                          
PRTTESN  DS    CL15                ESTIMATE NUMBER      +17                     
PRTTCLH  DS    XL1                 COLUMN #16           +18                     
PRTTESM  DS    CL17                ESTIMATE NAME        +35                     
PRTTCLI  DS    XL1                 COLUMN #17           +36                     
PRTTJBC2 DS    CL14                JOB CODE             +50                     
PRTTCLJ  DS    XL1                 COLUMN #18           +51                     
PRTTJBN2 DS    CL36                JOB NAME             +87                     
PRTTCLK  DS    XL1                 COLUMN #19           +88                     
PRTTDA5  DS    CL8                 ESTIMATE ADDED DATE  +96                     
PRTTCLL  DS    XL1                 COLUMN #20           +97                     
*                                                                               
*                                  **INVOICE MODULE**                           
         ORG   PRTTCPJ                                                          
PRTTIRF  DS    CL7                 LOG NO.              +9                      
PRTTCLM  DS    XL1                 COLUMN #21           +10                     
PRTTINV  DS    CL20                INVOICE NUMBER       +30                     
PRTTCLN  DS    XL1                 COLUMN #22           +31                     
PRTTITY  DS    CL4                 INVOICE TYPE         +35                     
PRTTCLN1 DS    XL1                 COLUMN #22/1         +36                     
PRTTAM2  DS    CL14                INVOICE AMOUNT       +50                     
PRTTCLO  DS    XL1                 COLUMN #23           +51                     
PRTTORN  DS    CL6                 ORDER NUMBER         +57                     
PRTTCLP  DS    XL1                 COLUMN #24           +58                     
PRTTORR  DS    CL8                 ORDER REQ BY DATE    +66                     
PRTTCLP1 DS    XL1                 COLUMN #24/1         +67                     
PRTTDA6  DS    CL8                 INVOICE DATE         +75                     
PRTTCLQ  DS    XL1                 COLUMN #25           +76                     
PRTTSUC  DS    CL14                SUPPLIER CODE        +90                     
PRTTCLR  DS    XL1                 COLUMN #26           +91                     
PRTTSUN2 DS    CL36                SUPPLIER NAME        +127                    
PRTTCLS  DS    XL1                 COLUMN #27           +128                    
*                                                                               
***********************************************************************         
* Approval hierarchy search table DSECT                               *         
***********************************************************************         
APRTABD  DSECT                                                                  
APRSTAT  DS    XL1                 Levels to include                            
APRCLI   EQU   X'80'               Client                                       
APRPRO   EQU   X'40'               Product                                      
APRJOB   EQU   X'20'               Job                                          
APRMED   EQU   X'10'               Media                                        
APROFF   EQU   X'08'               Office                                       
APRLVL   DS    CL1                 Level of approver                            
APRTABL  EQU   *-APRTABD                                                        
         SPACE 1                                                                
***********************************************************************         
* PARM OPTION DSECT EUROPE                                                      
***********************************************************************         
PARMD    DSECT                                                                  
PARMDPQ  DS    CL1   RCFFPARM+0    SUPPRESS REPORT FROM PQ                      
PARMDOE  DS    CL1   RCFFPARM+1    OVERRIDE EMAILS                              
PARMD1R  DS    CL1   RCFFPARM+2    1R OUTPUT OVERRIDE                           
PARMDDL  DS    CL1   RCFFPARM+3    ENFORCE DOWNLOAD                             
PARMDAG  DS    CL2   RCFFPARM+4/5  THIS AGENCY ONLY                             
PARMTYP  DS    CL1   RCFFPARM+6    FILTER BY RECORD TYPE                        
* use sortequs equates                                                          
                                                                                
***********************************************************************         
* EMAIL PROFILE RECORD DSECTS                                         *         
***********************************************************************         
EMPROFD  DSECT                                                                  
         DS    0XL16               MODULES                                      
SHOWTIME DS    CL1                 TIMESHEETS                                   
SHOWEXP  DS    CL1                 EXPENSES                                     
SHOWORD  DS    CL1                 ORDERS                                       
SHOWEST  DS    CL1                 ESTIMATES                                    
SHOWJOB  DS    CL1                 JOBS                                         
SHOWINV  DS    CL1                 INVOICES                                     
SHOWDET  DS    CL1                 DETAILED REPORT                              
SHOWSEM  DS    CL1                 SHOW SUMMARY EMAILS                          
*                                  MODULE DISPLAY ORDER                         
SORTORDX DS    0X                                                               
SORTORD1 DS    CL1                 FIRST ITEM ON EMAILS                         
SORTORD2 DS    CL1                 SECOND ITEM ON EMAILS                        
SORTORD3 DS    CL1                 THIRD ITEM ON EMAILS                         
SORTORD4 DS    CL1                 FOURTH ITEM ON EMAILS                        
SORTORD5 DS    CL1                 FIFTH ITEM ON EMAILS                         
SORTORD6 DS    CL1                 SIXTH ITEM ON EMAILS                         
SORTORDL EQU   *-SORTORDX                                                       
*                                                                               
SENDNDF  DS    CL1                 SEND EMAILS TO NON-DEFAULT APPROVERS         
DOWNREP  DS    CL1                 SET DOWNLOAD REPORT                          
*                                  MODULE DISPLAY ORDER EQUATES                 
EMPRO2D  DSECT                                                                  
         DS    0XL16               2ND PROFILE                                  
EM2SOFF  DS    CL1                 SORT BY OFFICE                               
EM2PIDE  DS    CL1                 SHOW PID ON EMAILS                           
EM2AGYE  DS    CL1                 SHOW AGENCY INFO ON EMAILS                   
EM2EMFE  DS    CL1                 CONTROL EMAIL FREQUENCY DAILY/WEEKLY         
         DS    CL12                N/D                                          
SORTEQUS DS    0X                                                               
SORTTIME EQU   C'T'                TIMESHEETS                                   
SORTEXP  EQU   C'E'                EXPENSES                                     
SORTFIN  EQU   C'F'                EXPENSES(FINANCE APPROVER)                   
SORTORD  EQU   C'O'                ORDERS                                       
SORTJOB  EQU   C'J'                JOBS                                         
SORTEST  EQU   C'S'                ESTIMATES                                    
SORTINV  EQU   C'I'                INVOICES                                     
***********************************************************************         
* PID DSECT FOR ORDERS/INVOICES                                       *         
***********************************************************************         
PIDD    DSECT                                                                   
PIDLVL  DS     XL1                                                              
PIDBIN  DS     XL2                                                              
PIDDL   EQU    *-PIDD                                                           
***********************************************************************         
* LIST OF TIMESHEETS TABLE ROW DATA (APPROVER EMAIL)                  *         
***********************************************************************         
         SPACE 1                                                                
APPTABD  DSECT                                                                  
APTIMA   DS    CL4                 <tr>                                         
APTIMB   DS    CL4                 <td>                                         
APPFST   DS    CL15                first name                                   
APTIMC   DS    CL5                 </td>                                        
APTIMD   DS    CL4                 <td>                                         
APPLST   DS    CL15                last name                                    
APTIME   DS    CL5                 </td>                                        
APTIMF   DS    CL4                 <td>                                         
         DS    XL(MAXELEN-(*-APTIMA))                                           
         ORG   APTIMA                                                           
APTIMG   DS    CL(L'LINKOP)                                                     
         DS    XL(MAXELEN-(*-APTIMA))                                           
         ORG   APTIMA                                                           
APTIMH   DS    CL(MAXELEN)         url TLINK                                    
         ORG   APTIMA                                                           
*                                                                               
*&&US                                                                           
APTMTH   DS    CL9                 MONTH                                        
         DS    CL1                                                              
*&&                                                                             
*&&UK                                                                           
APTDAY   DS    CL2                 DAY                                          
         DS    CL1                                                              
*&&                                                                             
*&&UK                                                                           
APTMTH   DS    CL9                 MONTH                                        
         DS    CL1                                                              
*&&                                                                             
*&&US                                                                           
APTDAY   DS    CL2                 DAY                                          
         DS    CL1                                                              
*&&                                                                             
APTYER   DS    CL4                 YEAR                                         
APTIMI   DS    CL4                 </a>                                         
APTIMJ   DS    CL5                 </td>                                        
APTIMK   DS    CL5                 </tr>                                        
         DS    XL(MAXELEN-(*-APTIMA))                                           
APPENDX  EQU   *-APPTABD                                                        
         ORG   APTIMA                                                           
APJOBA   DS    CL4                 <tr>                                         
APJOBB   DS    CL4                 <td>                                         
         DS    XL(MAXELEN-(*-APTIMA))                                           
         ORG   APTIMA                                                           
APJOBC   DS    CL(L'LINKOP)                                                     
         DS    XL(MAXELEN-(*-APTIMA))                                           
         ORG   APTIMA                                                           
APJOBD   DS    CL(MAXELEN)         url link                                     
         ORG   APTIMA                                                           
APJCOD   DS    CL12                Job code                                     
APJOBE   DS    CL4                 </a>                                         
APJOBF   DS    CL5                 </td>                                        
APJOBG   DS    CL4                 <td>                                         
APJNAM   DS    CL36                Job name                                     
APJOBH   DS    CL5                 </td>                                        
APJOBI   DS    CL4                 <td>                                         
*&&US                                                                           
APJMTH   DS    CL9                 MONTH                                        
         DS    CL1                                                              
*&&                                                                             
*&&UK                                                                           
APJDAY   DS    CL2                 DAY                                          
         DS    CL1                                                              
*&&                                                                             
*&&UK                                                                           
APJMTH   DS    CL9                 MONTH                                        
         DS    CL1                                                              
*&&                                                                             
*&&US                                                                           
APJDAY   DS    CL2                 DAY                                          
         DS    CL1                                                              
*&&                                                                             
APJYER   DS    CL4                 YEAR                                         
APJOBJ   DS    CL5                 </td>                                        
APJOBK   DS    CL5                 </tr>                                        
         DS    XL(MAXELEN-(*-APTIMA))                                           
         ORG   APTIMA                                                           
APESTA   DS    CL4                 <tr>                                         
APESTB   DS    CL4                 <td>                                         
         DS    XL(MAXELEN-(*-APTIMA))                                           
         ORG   APTIMA                                                           
APESTC   DS    CL(L'LINKOP)                                                     
         DS    XL(MAXELEN-(*-APTIMA))                                           
         ORG   APTIMA                                                           
APESTD   DS    CL(MAXELEN)         url TLINK                                    
         ORG   APTIMA                                                           
APESTD2  DS    CL60                url link 2                                   
         DS    XL(MAXELEN-(*-APTIMA))                                           
         ORG   APTIMA                                                           
APESNO   DS    CL6                 Estimate number                              
APESTE   DS    CL4                 </a>                                         
APESTF   DS    CL5                 </td>                                        
APESTG   DS    CL4                 <td>                                         
APENAM   DS    CL50                Estimate name                                
APESTH   DS    CL5                 </td>                                        
APESTI   DS    CL4                 <td>                                         
*&&US                                                                           
APEMTH   DS    CL9                 MONTH                                        
         DS    CL1                                                              
*&&                                                                             
*&&UK                                                                           
APEDAY   DS    CL2                 DAY                                          
         DS    CL1                                                              
*&&                                                                             
*&&UK                                                                           
APEMTH   DS    CL9                 MONTH                                        
         DS    CL1                                                              
*&&                                                                             
*&&US                                                                           
APEDAY   DS    CL2                 DAY                                          
         DS    CL1                                                              
*&&                                                                             
APEYER   DS    CL4                 YEAR                                         
APESTJ   DS    CL5                 </td>                                        
APESTK   DS    CL5                 </tr>                                        
         DS    XL(MAXELEN-(*-APTIMA))                                           
         ORG   APTIMA                                                           
APORDA   DS    CL4                 <tr>                                         
APORDB   DS    CL4                 <td>                                         
         DS    XL(MAXELEN-(*-APTIMA))                                           
         ORG   APTIMA                                                           
APORDC   DS    CL(L'LINKOP)                                                     
         ORG   APTIMA                                                           
APORDD   DS    CL250               url TLINK                                    
         ORG   APTIMA                                                           
APORNO   DS    CL8                 Order number                                 
APORDE   DS    CL4                 </a>                                         
APORDF   DS    CL5                 </td>                                        
APORDG   DS    CL4                 <td>                                         
APONAM   DS    CL50                Order name                                   
APORDH   DS    CL5                 </td>                                        
APORDI   DS    CL4                 <td>                                         
APOJNM   DS    CL50                Job name                                     
APORDJ   DS    CL5                 </td>                                        
APORDK   DS    CL5                 </tr>                                        
         ORG   APTIMA                                                           
APINVA   DS    CL4                 <tr>                                         
APINVB   DS    CL4                 <td>                                         
         ORG   APTIMA                                                           
APINVC   DS    CL(L'LINKOP)                                                     
         ORG   APTIMA                                                           
APINVD   DS    CL250               url TLINK                                    
         ORG   APTIMA                                                           
APINVN   DS    CL(L'EMILN)         Invoice log number                           
APINVE   DS    CL4                 </a>                                         
APINVF   DS    CL5                 </td>                                        
APINVG   DS    CL4                 <td>                                         
APINVR   DS    CL20                Invoice reference                            
APINVH   DS    CL5                 </td>                                        
APINVI   DS    CL4                 <td>                                         
APINVS   DS    CL(L'EMSUP)         Supplier code                                
APINVJ   DS    CL5                 </td>                                        
APINVK   DS    CL4                 <td>                                         
APINVSN  DS    CL(L'NAMEREC)       Supplier name                                
APINVL   DS    CL5                 </td>                                        
APINVM   DS    CL5                 </tr>                                        
         ORG   APTIMA                                                           
APEXPA   DS    CL4                 <tr>                                         
APEXPB   DS    CL4                 <td>                                         
         ORG   APTIMA                                                           
APEXPC   DS    CL(L'LINKOP)                                                     
         ORG   APTIMA                                                           
APEXPD   DS    CL250               url TLINK                                    
         ORG   APTIMA                                                           
APEXPN   DS    CL(L'EMILN)         Expense number                               
APEXPE   DS    CL4                 </a>                                         
APEXPF   DS    CL5                 </td>                                        
APEXPG   DS    CL4                 <td>                                         
APEXPR   DS    CL12                1R Account code                              
APEXPH   DS    CL5                 </td>                                        
APEXPI   DS    CL5                 </tr>                                        
         EJECT                                                                  
***********************************************************************         
* Calendar table dsect                                                *         
***********************************************************************         
         SPACE 1                                                                
MONTABD  DSECT                                                                  
MONNUM   DS    CL2                                                              
MONLEN   DS    XL1                                                              
MONDD    DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* Aura standard email headings table dsect                            *         
***********************************************************************         
         SPACE 1                                                                
AURHTABD DSECT                                                                  
AURHATY  DS    CL1                 Application type                             
         DS    XL3                 n/d                                          
AURHDES  DS    AL4                 Address of description line                  
AURHPCH  DS    AL4                 Address of "please click here" line          
AURHMHE  DS    AL4                 Address of module heading                    
AURHURL  DS    AL4                 Address of URL link                          
AURHTABL EQU   *-AURHTABD                                                       
         EJECT                                                                  
***********************************************************************         
* Aura summary email headings table dsect                             *         
***********************************************************************         
         SPACE 1                                                                
SUMHTABD DSECT                                                                  
SUMHATY  DS    CL1                 Application type                             
         DS    XL3                 n/d                                          
SUMHPCH  DS    AL4                 Address of "please click here" line          
SUMHAPP  DS    AL4                 Address of module heading                    
SUMHURL  DS    AL4                 URL link                                     
SUMHIND  DS    XL1                 Indicator byte                               
SUMHULC  EQU   X'80'               Lower case module heading UK                 
SUMHGLC  EQU   X'40'               Lower case module heading DE                 
SUMHTABL EQU   *-SUMHTABD                                                       
         EJECT                                                                  
***********************************************************************         
* Includes                                                            *         
***********************************************************************         
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         SPACE 1                                                                
SSOOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
COBLOCKD DSECT                                                                  
       ++INCLUDE ACCAPBLOCK                                                     
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
          PRINT ON                                                              
* COMFACSD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACLDGTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACLDGTABD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGETURLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGETURLD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDICTATED                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDDICTATED                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDCTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDGETRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGETRETD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
* BIGBOX                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* GETOPT DSECTS                                                                 
         PRINT OFF                                                              
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
GOXBLKD  DSECT                                                                  
       ++INCLUDE ACGOXBLOCK                                                     
GOBBLKD  DSECT                                                                  
       ++INCLUDE ACGOBBLOCK                                                     
         PRINT ON                                                               
         SPACE 2                                                                
***********************************************************************         
* LITERALS AND TABLES                                                 *         
***********************************************************************         
ACEM02   CSECT                                                                  
         DS    0D                      ALIGNMENT                                
SVEMREC  DS    XL(EMRECL)              SAVED EMAIL RECORD                       
CCOBLOCK DS    CL(COBLOCKX-COBLOCK)    COBLOCK FOR GETCAP                       
PIDBLOCK DS    XL(PIDDL*SORTIPID)      LIST OF PIDS FOR SORTING                 
*                                                                               
* ACMONTAB                             Table of months                          
*                                                                               
       ++INCLUDE ACMONTAB                                                       
*                                                                               
* ACURLTAB                                                                      
*                                  ACURLTAB                                     
       ++INCLUDE ACURLTAB                                                       
GOBLOCKB DS    XL(GOBLOCKX-GOBLOCK)         (400)                               
GOXBLCKA DS    XL(GOXBLKX-GOXBLOCK)         (400)                               
GOBBLCKA DS    XL(GOBBLKXX-GOBBLOCK)        (600)                               
*                                                                               
PRGBUFF  DS    XL(24*ONEK)         CHUNKY BUFFER AREA                           
PRGBUFFX DS    0H                                                               
ONEK     EQU   1024                                                             
*                                                                               
***********************************************************************         
* HTML TEMPLATE FOR AURA                                              *         
***********************************************************************         
         SPACE 1                                                                
EMLHTML  DS    0H                                                               
*                               ** Approver email top section **                
ELEMT    DS    0X                                                               
*                                                                               
ELEM5    DC    AL1(ELEM5L)                                                      
         DC    C'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 '                 
         DC    C'Transitional//EN" '                                            
         DC    C'"http://www.w3.org/TR/xhtml11/DTD/xhtml1-transitional'         
         DC    C'.dtd">'                                                        
ELEM5L   EQU   *-ELEM5                                                          
*                                                                               
ELEM10   DC    AL1(ELEM10L)                                                     
         DC    C'<html xmlns="http://www.w3.org/1999/xhtml">'                   
ELEM10L  EQU   *-ELEM10                                                         
*                                                                               
ELEM15   DC    AL1(ELEM15L)                                                     
         DC    C'<head>'                                                        
ELEM15L  EQU   *-ELEM15                                                         
*                                                                               
ELEM20   DC    AL1(ELEM20L)                                                     
         DC    C'<title>'                                                       
ELEM20L  EQU   *-ELEM20                                                         
*                                                                               
ELEM25   DC    AL1(ELEM25L)                                                     
         DC    C'</title>'                                                      
ELEM25L  EQU   *-ELEM25                                                         
*                                                                               
ELEM30   DC    AL1(ELEM30L)                                                     
         DC    C'<meta http-equiv="Content-Type" content="text/html;'           
         DC    C' charset=UTF-8" />'                                            
ELEM30L  EQU   *-ELEM30                                                         
*                                                                               
ELEM35   DC    AL1(ELEM35L)                                                     
         DC    C'<meta name="viewport" content="width=device-width; '           
         DC    C'initial-scale=1; maximum-scale=1.0"/>'                         
ELEM35L  EQU   *-ELEM35                                                         
*                                                                               
ELEM40   DC    AL1(ELEM40L)                                                     
         DC    C'</head>'                                                       
ELEM40L  EQU   *-ELEM40                                                         
*                                                                               
ELEM45   DC    AL1(ELEM45L)                                                     
         DC    C'<body bgcolor="#ffffff">'                                      
ELEM45L  EQU   *-ELEM45                                                         
*                                                                               
ELEM50   DC    AL1(ELEM50L)                                                     
         DC    C'<!-- Outer Wrap -->'                                           
ELEM50L  EQU   *-ELEM50                                                         
*                                                                               
ELEM55   DC    AL1(ELEM55L)                                                     
         DC    C'<table width="100%" border="0" cellspacing="0" '               
         DC    C'cellpadding="0" bgcolor="#ffffff" style="table-layout'         
         DC    C':fixed;">'                                                     
ELEM55L  EQU   *-ELEM55                                                         
*                                                                               
ELEM60   DC    AL1(ELEM60L)                                                     
         DC    C'<tr>'                                                          
ELEM60L  EQU   *-ELEM60                                                         
*                                                                               
ELEM65   DC    AL1(ELEM65L)                                                     
         DC    C'<td>'                                                          
ELEM65L  EQU   *-ELEM65                                                         
*                                                                               
ELEM70   DC    AL1(ELEM70L)                                                     
         DC    C'<!-- Inner Wrap -->'                                           
ELEM70L  EQU   *-ELEM70                                                         
*                                                                               
ELEM75   DC    AL1(ELEM75L)                                                     
         DC    C'<table style="width:800px;" border="0" '                       
         DC    C'cellspacing="0" cellpadding="0" align="center">'               
ELEM75L  EQU   *-ELEM75                                                         
*                                                                               
ELEM80   DC    AL1(ELEM80L)                                                     
         DC    C'<tr>'                                                          
ELEM80L  EQU   *-ELEM80                                                         
*                                                                               
ELEM85   DC    AL1(ELEM85L)                                                     
         DC    C'<td>'                                                          
ELEM85L  EQU   *-ELEM85                                                         
*                                                                               
ELEM90   DC    AL1(ELEM90L)                                                     
         DC    C'<!-- top shadow -->'                                           
ELEM90L  EQU   *-ELEM90                                                         
*                                                                               
ELEM95   DC    AL1(ELEM95L)                                                     
         DC    C'<table width="100%" border="0" cellspacing="0"'                
         DC    C'cellpadding="0">'                                              
ELEM95L  EQU   *-ELEM95                                                         
*                                                                               
ELEM100  DC    AL1(ELEM100L)                                                    
         DC    C'<tr>'                                                          
ELEM100L EQU   *-ELEM100                                                        
*                                                                               
ELEM105  DC    AL1(ELEM105L)                                                    
         DC    C'<td height="10"></td>'                                         
ELEM105L EQU   *-ELEM105                                                        
*                                                                               
ELEM110  DC    AL1(ELEM110L)                                                    
         DC    C'</tr>'                                                         
ELEM110L EQU   *-ELEM110                                                        
*                                                                               
ELEM115  DC    AL1(ELEM115L)                                                    
         DC    C'</table>'                                                      
ELEM115L EQU   *-ELEM115                                                        
*                                                                               
ELEM120  DC    AL1(ELEM120L)                                                    
         DC    C'<!-- header-->'                                                
ELEM120L EQU   *-ELEM120                                                        
*                                                                               
ELEM125  DC    AL1(ELEM125L)                                                    
         DC    C'<table width="100%" bgcolor="#ffffff" border="0" '             
         DC    C'cellspacing="0" cellpadding="0">'                              
ELEM125L EQU   *-ELEM125                                                        
*                                                                               
ELEM130  DC    AL1(ELEM130L)                                                    
         DC    C'<tr>'                                                          
ELEM130L EQU   *-ELEM130                                                        
*                                                                               
ELEM135  DC    AL1(ELEM135L)                                                    
         DC    C'<td>'                                                          
ELEM135L EQU   *-ELEM135                                                        
*                                                                               
ELEM140  DC    AL1(ELEM140L)                                                    
         DC    C'<a href="'                                                     
ELEM140E DS    CL20                                                             
         DC    C'">'                                                            
         DC    C'<img alt="Aura" src="http://info.mediaocean.com/rs/'           
         DC    C'331-XPM-231/images/Aura-header.png"/>'                         
ELEM140L EQU   *-ELEM140                                                        
*                                                                               
ELEM145  DC    AL1(ELEM145L)                                                    
         DC    C'</a>'                                                          
ELEM145L EQU   *-ELEM145                                                        
*                                                                               
ELEM150  DC    AL1(ELEM150L)                                                    
         DC    C'</td>'                                                         
ELEM150L EQU   *-ELEM150                                                        
*                                                                               
ELEM155  DC    AL1(ELEM155L)                                                    
         DC    C'</tr>'                                                         
ELEM155L EQU   *-ELEM155                                                        
*                                                                               
ELEM160  DC    AL1(ELEM160L)                                                    
         DC    C'</table>'                                                      
ELEM160L EQU   *-ELEM160                                                        
*                                                                               
ELEM165  DC    AL1(ELEM165L)                                                    
         DC    C'<!-- end: header -->'                                          
ELEM165L EQU   *-ELEM165                                                        
*                                                                               
ELEM170  DC    AL1(ELEM170L)                                                    
         DC    C'<!-- body -->'                                                 
ELEM170L EQU   *-ELEM170                                                        
*                                                                               
ELEM175  DC    AL1(ELEM175L)                                                    
         DC    C'<table width="100%" border="0" cellspacing="0" '               
         DC    C'cellpadding="0">'                                              
ELEM175L EQU   *-ELEM175                                                        
*                                                                               
ELEM180  DC    AL1(ELEM180L)                                                    
         DC    C'<tr>'                                                          
ELEM180L EQU   *-ELEM180                                                        
*                                                                               
ELEM185  DC    AL1(ELEM185L)                                                    
         DC    C'<td height="10"></td>'                                         
ELEM185L EQU   *-ELEM185                                                        
*                                                                               
ELEM190  DC    AL1(ELEM190L)                                                    
         DC    C'</tr>'                                                         
ELEM190L EQU   *-ELEM190                                                        
*                                                                               
ELEM195  DC    AL1(ELEM195L)                                                    
         DC    C'</table>'                                                      
ELEM195L EQU   *-ELEM195                                                        
*                                                                               
ELEM200  DC    AL1(ELEM200L)                                                    
         DC    C'<!-- tape check module -->'                                    
ELEM200L EQU   *-ELEM200                                                        
*                                                                               
ELEMTL   EQU   *-ELEMT                                                          
*                                     ** Email section **                       
ELEMSECT DS    0X                                                               
*                                                                               
ELEM205  DC    AL1(ELEM205L)                                                    
         DC    C'<table width="100%" bgcolor="#ffffff" border="0" '             
         DC    C'cellspacing="0" cellpadding="0" style="font-family:'           
ELEM205L EQU   *-ELEM205                                                        
*                                                                               
ELEM210  DC    AL1(ELEM210L)                                                    
         DC    C'Calibri, sans-serif; font-size:13px; color:#4E4D4C; '          
         DC    C'background-color:#D9D9CD; -webkit-border-radius: 3px;'         
         DC    C' -moz-border-radius: 3px; border-radius: 3px;">'               
ELEM210L EQU   *-ELEM210                                                        
*                                                                               
ELEM215  DC    AL1(ELEM215L)                                                    
         DC    C'<tr>'                                                          
ELEM215L EQU   *-ELEM215                                                        
*                                                                               
ELEM220  DC    AL1(ELEM220L)                                                    
         DC    C'<td height="20" colspan="5" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM220L EQU   *-ELEM220                                                        
*                                                                               
ELEM225  DC    AL1(ELEM225L)                                                    
         DC    C'<img height="20" alt="spacer" src="http://info.'               
         DC    C'mediaocean.com/rs/331-XPM-231/images/spacer_icon.png"'         
         DC    C'/>'                                                            
ELEM225L EQU   *-ELEM225                                                        
*                                                                               
ELEM230  DC    AL1(ELEM230L)                                                    
         DC    C'</td>'                                                         
ELEM230L EQU   *-ELEM230                                                        
*                                                                               
ELEM235  DC    AL1(ELEM235L)                                                    
         DC    C'</tr>'                                                         
ELEM235L EQU   *-ELEM235                                                        
*                                                                               
ELEM240  DC    AL1(ELEM240L)                                                    
         DC    C'<tr>'                                                          
ELEM240L EQU   *-ELEM240                                                        
*                                                                               
ELEM245  DC    AL1(ELEM245L)                                                    
         DC    C'<td width="20">'                                               
ELEM245L EQU   *-ELEM245                                                        
*                                                                               
ELEM250  DC    AL1(ELEM250L)                                                    
         DC    C'<img height="1" width="20" alt="spacer" '                      
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231'                
         DC    C'/images/spacer_icon.png"/>'                                    
ELEM250L EQU   *-ELEM250                                                        
*                                                                               
ELEM255  DC    AL1(ELEM255L)                                                    
         DC    C'</td>'                                                         
ELEM255L EQU   *-ELEM255                                                        
*                                                                               
ELEM260  DC    AL1(ELEM260L)                                                    
         DC    C'<td valign="top" width="360" '                                 
         DC    C'style="font-family: Calibri, sans-serif; '                     
         DC    C'font-size:30px; color:#4E4D4C; line-height:14pt;">'            
ELEM260L EQU   *-ELEM260                                                        
*                                                                               
ELEM265  DC    AL1(ELEM265L)                                                    
         DC    C'<div>'                                                         
ELEM265E DS    CL80                Here are the details.....                    
         DC    C'</div>'                                                        
ELEM265L EQU   *-ELEM265                                                        
*                                                                               
ELEM270  DC    AL1(ELEM270L)                                                    
         DC    C'<p>'                                                           
ELEM270L EQU   *-ELEM270                                                        
*                                                                               
ELEM271  DC    AL1(ELEM271L)                                                    
ELEM271E DS    CL(MAXELEN)                                                      
ELEM271L EQU   *-ELEM271                                                        
*                                                                               
ELEM272  DC    AL1(ELEM272L)                                                    
ELEM272E DS    CL(MAXELEN)                                                      
ELEM272L EQU   *-ELEM272                                                        
*                                                                               
ELEM273  DC    AL1(ELEM273L)                                                    
ELEM273E DS    CL(MAXELEN)                                                      
ELEM273L EQU   *-ELEM273                                                        
*                                                                               
ELEM275  DC    AL1(ELEM275L)                                                    
         DC    C'<a style="color:#4E4D4C; font-size:13px; '                     
         DC    C'text-decoration:none; font-weight:bold;" href="'               
ELEM275L EQU   *-ELEM275                                                        
*                                                                               
ELEM276  DC    AL1(ELEM276L)                                                    
ELEM276E DS    CL160               Url e.g. http://aura.mediaocean.com          
ELEM276L EQU   *-ELEM276                                                        
*                                                                               
ELEM278  DC    AL1(ELEM276L)                                                    
ELEM278E DS    CL160               Url e.g. http://aura.mediaocean.com          
ELEM278L EQU   *-ELEM278                                                        
*                                                                               
ELEM280  DC    AL1(ELEM280L)                                                    
ELEM280E DS    CL80               Please click here to access....               
ELEM280L EQU   *-ELEM280                                                        
*                                                                               
ELEM285  DC    AL1(ELEM285L)                                                    
         DC    C'</a>'                                                          
ELEM285L EQU   *-ELEM285                                                        
*                                                                               
ELEM290  DC    AL1(ELEM290L)                                                    
         DC    C'</p>'                                                          
ELEM290L EQU   *-ELEM290                                                        
*                                                                               
ELEM295  DC    AL1(ELEM295L)                                                    
         DC    C'</td>'                                                         
ELEM295L EQU   *-ELEM295                                                        
*                                                                               
ELEM300  DC    AL1(ELEM300L)                                                    
         DC    C'<td width="20">'                                               
ELEM300L EQU   *-ELEM300                                                        
*                                                                               
ELEM305  DC    AL1(ELEM305L)                                                    
         DC    C'<img height="1" width="20" alt="spacer" src="'                 
         DC    C'http://info.mediaocean.com/rs/331-XPM-231/images/'             
         DC    C'spacer_icon.png"/>'                                            
ELEM305L EQU   *-ELEM305                                                        
*                                                                               
ELEM310  DC    AL1(ELEM310L)                                                    
         DC    C'</td>'                                                         
ELEM310L EQU   *-ELEM310                                                        
*                                                                               
ELEM315  DC    AL1(ELEM315L)                                                    
         DC    C'<td style="vertical-align:top; font-family: Calibri, '         
         DC    C'sans-serif; font-size:13px; color:#4E4D4C; '                   
         DC    C'line-height:14px;">'                                           
ELEM315L EQU   *-ELEM315                                                        
*                                                                               
ELEM320  DC    AL1(ELEM320L)                                                    
         DC    C'<table border="0" style="background-color:#ffffff;">'          
ELEM320L EQU   *-ELEM320                                                        
*                                                                               
ELEM325  DC    AL1(ELEM325L)                                                    
         DC    C'<tr>'                                                          
ELEM325L EQU   *-ELEM325                                                        
*                                                                               
ELEM330  DC    AL1(ELEM330L)                                                    
         DC    C'<td width="360" colspan="2">'                                  
ELEM330L EQU   *-ELEM330                                                        
*                                                                               
ELEM335  DC    AL1(ELEM335L)                                                    
         DC    C'<img height="20" width="360" border="0" alt="spacer" '         
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/images'         
         DC    C'/spacer_icon.png"/>'                                           
ELEM335L EQU   *-ELEM335                                                        
*                                                                               
ELEM340  DC    AL1(ELEM340L)                                                    
         DC    C'</td>'                                                         
ELEM340L EQU   *-ELEM340                                                        
*                                                                               
ELEM345  DC    AL1(ELEM345L)                                                    
         DC    C'</tr>'                                                         
ELEM345L EQU   *-ELEM345                                                        
*                                                                               
ELEMOFF  DS    0X                                                               
*                                                                               
ELEM346  DC    AL1(ELEM346L)                                                    
         DC    C'<tr>'                                                          
ELEM346L EQU   *-ELEM346                                                        
*                                                                               
ELEM350  DC    AL1(ELEM350L)                                                    
         DC    C'<td width="20">'                                               
ELEM350L EQU   *-ELEM350                                                        
*                                                                               
ELEM355  DC    AL1(ELEM355L)                                                    
         DC    C'<img height="1" width="20" border="0" alt="spacer"'            
         DC    C' src="http://info.mediaocean.com/rs/331-XPM-231/'              
         DC    C'images/spacer_icon.png"/>'                                     
ELEM355L EQU   *-ELEM355                                                        
*                                                                               
ELEM360  DC    AL1(ELEM360L)                                                    
         DC    C'</td>'                                                         
ELEM360L EQU   *-ELEM360                                                        
*                                                                               
ELEM365  DC    AL1(ELEM365L)                                                    
         DC    C'<td width="340">'                                              
ELEM365L EQU   *-ELEM365                                                        
*                                                                               
ELEM366  DC    AL1(ELEM366L)                                                    
         DC    C'<div style="width:360px; background-color:#ffffff; '           
         DC    C'font-family: Calibri, sans-serif; font-size:13px;">'           
ELEM366L EQU   *-ELEM366                                                        
*                                                                               
ELEM370  DC    AL1(ELEM370L)                                                    
ELEM370E DS    CL120                for the period ending                       
         DC    C'</div>'                                                        
         DC    C'</td>'                                                         
ELEM370L EQU   *-ELEM370                                                        
*                                                                               
ELEM375  DC    AL1(ELEM375L)                                                    
         DC    C'</tr>'                                                         
ELEM375L EQU   *-ELEM375                                                        
*                                                                               
ELEM380  DC    AL1(ELEM380L)                                                    
         DC    C'<tr>'                                                          
ELEM380L EQU   *-ELEM380                                                        
*                                                                               
ELEM385  DC    AL1(ELEM385L)                                                    
         DC    C'<td width="20">'                                               
ELEM385L EQU   *-ELEM385                                                        
*                                                                               
ELEM390  DC    AL1(ELEM390L)                                                    
         DC    C'<img height="1" width="20" border="0" alt="spacer" '           
         DC    C' src="http://info.mediaocean.com/rs/331-XPM-231/'              
         DC    C'images/spacer_icon.png"/>'                                     
ELEM390L EQU   *-ELEM390                                                        
*                                                                               
ELEM395  DC    AL1(ELEM395L)                                                    
         DC    C'</td>'                                                         
ELEM395L EQU   *-ELEM395                                                        
*                                                                               
ELEM400  DC    AL1(ELEM400L)                                                    
         DC    C'<td>'                                                          
ELEM400L EQU   *-ELEM400                                                        
*                                                                               
ELEM405  DC    AL1(ELEM405L)                                                    
         DC    C'<div style="width:340px; height:380px; '                       
         DC    C'overflow-y:scroll; background-color:#ffffff;">'                
ELEM405L EQU   *-ELEM405                                                        
*                                                                               
ELEM410  DC    AL1(ELEM410L)                                                    
         DC    C'<table cellpadding="5" style="font-family: Calibri, '          
         DC    C'sans-serif; font-size:13px;">'                                 
ELEM410L EQU   *-ELEM410                                                        
*                                                                               
** list of timesheets goes here see TABROD DSECT **                             
                                                                                
ELEMTML  DS    0X                                                               
*                                                                               
ELEM415  DC    AL1(ELEM415L)                                                    
         DC    C'</table>'                                                      
ELEM415L EQU   *-ELEM415                                                        
*                                                                               
ELEM420  DC    AL1(ELEM420L)                                                    
         DC    C'</div>'                                                        
ELEM420L EQU   *-ELEM420                                                        
*                                                                               
ELEM425  DC    AL1(ELEM425L)                                                    
         DC    C'</td>'                                                         
ELEM425L EQU   *-ELEM425                                                        
*                                                                               
ELEM430  DC    AL1(ELEM430L)                                                    
         DC    C'</tr>'                                                         
ELEM430L EQU   *-ELEM430                                                        
*                                                                               
ELEM440  DC    AL1(ELEM440L)                                                    
         DC    C'<tr>'                                                          
ELEM440L EQU   *-ELEM440                                                        
*                                                                               
ELEM445  DC    AL1(ELEM445L)                                                    
         DC    C'<td width="360" colspan="2">'                                  
ELEM445L EQU   *-ELEM445                                                        
*                                                                               
ELEM450  DC    AL1(ELEM450L)                                                    
         DC    C'<img height="20" width="360" border="0" alt="spacer" '         
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM450L EQU   *-ELEM450                                                        
*                                                                               
ELEM455  DC    AL1(ELEM455L)                                                    
         DC    C'</td>'                                                         
ELEM455L EQU   *-ELEM455                                                        
*                                                                               
ELEM460  DC    AL1(ELEM460L)                                                    
         DC    C'</tr>'                                                         
ELEM460L EQU   *-ELEM460                                                        
*                                                                               
ELEMOEN  DS    0X                                                               
*                                                                               
ELEM465  DC    AL1(ELEM465L)                                                    
         DC    C'</table>'                                                      
ELEM465L EQU   *-ELEM465                                                        
*                                                                               
ELEM470  DC    AL1(ELEM470L)                                                    
         DC    C'</td>'                                                         
ELEM470L EQU   *-ELEM470                                                        
*                                                                               
ELEM475  DC    AL1(ELEM475L)                                                    
         DC    C'<td width="20">'                                               
ELEM475L EQU   *-ELEM475                                                        
*                                                                               
ELEM480  DC    AL1(ELEM480L)                                                    
         DC    C'<img height="1" width="20" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM480L EQU   *-ELEM480                                                        
*                                                                               
ELEM485  DC    AL1(ELEM485L)                                                    
         DC    C'</td>'                                                         
ELEM485L EQU   *-ELEM485                                                        
*                                                                               
ELEM490  DC    AL1(ELEM490L)                                                    
         DC    C'</tr>'                                                         
ELEM490L EQU   *-ELEM490                                                        
*                                                                               
ELEM495  DC    AL1(ELEM495L)                                                    
         DC    C'<tr>'                                                          
ELEM495L EQU   *-ELEM495                                                        
*                                                                               
ELEM500  DC    AL1(ELEM500L)                                                    
         DC    C'<td height="30" colspan="3" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM500L EQU   *-ELEM500                                                        
*                                                                               
ELEM505  DC    AL1(ELEM505L)                                                    
         DC    C'<img height="30" width="20" border="0" alt="spacer" '          
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM505L EQU   *-ELEM505                                                        
*                                                                               
ELEM510  DC    AL1(ELEM510L)                                                    
         DC    C'</td>'                                                         
ELEM510L EQU   *-ELEM510                                                        
*                                                                               
ELEM515  DC    AL1(ELEM515L)                                                    
         DC    C'</tr>'                                                         
ELEM515L EQU   *-ELEM515                                                        
*                                                                               
ELEM520  DC    AL1(ELEM520L)                                                    
         DC    C'</table>'                                                      
ELEM520L EQU   *-ELEM520                                                        
*                                      ** end of email section **               
ELEMSEND DS    0X                      ** end of email **                       
*                                                                               
ELEM525  DC    AL1(ELEM525L)                                                    
         DC    C'<!-- no background module -->'                                 
ELEM525L EQU   *-ELEM525                                                        
*                                                                               
ELEM530  DC    AL1(ELEM530L)                                                    
         DC    C'<table width="100%" bgcolor="#ffffff" border="0" '             
         DC    C'cellspacing="0" cellspadding="0" style="font-family: '         
         DC    C'Calibri, sans-serif; font-size:10px; color:#4E4D4C; '          
ELEM530L EQU   *-ELEM530                                                        
*                                                                               
ELEM535  DC    AL1(ELEM535L)                                                    
         DC    C'background-color:#ffffff; -webkit-border-radius: 3px;'         
         DC    C' -moz-border-radius: 3px; border-radius: 3px;">'               
ELEM535L EQU   *-ELEM535                                                        
*                                                                               
ELEM540  DC    AL1(ELEM540L)                                                    
         DC    C'<tr>'                                                          
ELEM540L EQU   *-ELEM540                                                        
*                                                                               
ELEM545  DC    AL1(ELEM545L)                                                    
         DC    C'<td rowspan="3" width="30"><img height="1" width="30"'         
         DC    C' border="0" alt="spacer" src="http://info.mediaocean'          
         DC    C'.com/rs/331-XPM-231/images/spacer_icon.png"/>'                 
         DC    C'</td>'                                                         
ELEM545L EQU   *-ELEM545                                                        
*                                                                               
ELEM550  DC    AL1(ELEM550L)                                                    
         DC    C'<td>'                                                          
         DC    X'50'                                                            
         DC    C'nbsp;</td>'                                                    
ELEM550L EQU   *-ELEM550                                                        
*                                                                               
ELEM555  DC    AL1(ELEM555L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM555L EQU   *-ELEM555                                                        
*                                                                               
ELEM560  DC    AL1(ELEM560L)                                                    
         DC    C'</td>'                                                         
ELEM560L EQU   *-ELEM560                                                        
*                                                                               
ELEM565  DC    AL1(ELEM565L)                                                    
         DC    C'</tr>'                                                         
ELEM565L EQU   *-ELEM565                                                        
*                                                                               
ELEM570  DC    AL1(ELEM570L)                                                    
         DC    C'<tr>'                                                          
ELEM570L EQU   *-ELEM570                                                        
*                                                                               
ELEM575  DC    AL1(ELEM575L)                                                    
         DC    C'<td style="vertical-align:top;">'                              
ELEM575L EQU   *-ELEM575                                                        
*                                                                               
ELEM580  DC    AL1(ELEM580L)                                                    
         DC    C'<p>'                                                           
ELEM580L EQU   *-ELEM580                                                        
*                                                                               
ELEM585  DC    AL1(ELEM585L)                                                    
EMAILL1  DS    CL80                                                             
EMAILL2  DS    CL80                                                             
ELEM585L EQU   *-ELEM585                                                        
*                                                                               
ELEM600  DC    AL1(ELEM600L)                                                    
EMAILL3  DS    CL80                                                             
EMAILL4  DS    CL80                                                             
ELEM600L EQU   *-ELEM600                                                        
*                                                                               
ELEM605  DC    AL1(ELEM605L)                                                    
EMAILL5  DS    CL80                                                             
         DC    C'</p>'                                                          
ELEM605L EQU   *-ELEM605                                                        
*                                                                               
ELEM610  DC    AL1(ELEM610L)                                                    
         DC    C'<p>'                                                           
ELEM610L EQU   *-ELEM610                                                        
*                                                                               
ELEM615  DC    AL1(ELEM615L)                                                    
EMAILL6  DS    CL80                                                             
EMAILL7  DS    CL80                                                             
ELEM615L EQU   *-ELEM615                                                        
*                                                                               
ELEM630  DC    AL1(ELEM630L)                                                    
EMAILL8  DS    CL80                                                             
EMAILL9  DS    CL80                                                             
ELEM630L EQU   *-ELEM630                                                        
*                                                                               
ELEM632  DC    AL1(ELEM632L)                                                    
EMAILLA  DS    CL80                                                             
ELEM632L EQU   *-ELEM632                                                        
*                                                                               
ELEM635  DC    AL1(ELEM635L)                                                    
         DC    C'</p>'                                                          
ELEM635L EQU   *-ELEM635                                                        
*                                                                               
ELEM655  DC    AL1(ELEM655L)                                                    
         DC    C'</td>'                                                         
ELEM655L EQU   *-ELEM655                                                        
*                                                                               
ELEM660  DC    AL1(ELEM660L)                                                    
         DC    C'</tr>'                                                         
ELEM660L EQU   *-ELEM660                                                        
*                                                                               
ELEM665  DC    AL1(ELEM665L)                                                    
         DC    C'<tr>'                                                          
ELEM665L EQU   *-ELEM665                                                        
*                                                                               
ELEM670  DC    AL1(ELEM670L)                                                    
         DC    C'<td height="20" style="font-size:1px; '                        
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM670L EQU   *-ELEM670                                                        
*                                                                               
ELEM675  DC    AL1(ELEM675L)                                                    
         DC    C'<img height="20" border="0" alt="spacer" '                     
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM675L EQU   *-ELEM675                                                        
*                                                                               
ELEM680  DC    AL1(ELEM680L)                                                    
         DC    C'</td>'                                                         
ELEM680L EQU   *-ELEM680                                                        
*                                                                               
ELEM685  DC    AL1(ELEM685L)                                                    
         DC    C'</tr>'                                                         
ELEM685L EQU   *-ELEM685                                                        
*                                                                               
ELEM690  DC    AL1(ELEM690L)                                                    
         DC    C'</table>'                                                      
ELEM690L EQU   *-ELEM690                                                        
*                                                                               
ELEM695  DC    AL1(ELEM695L)                                                    
         DC    C'<!-- end: body -->'                                            
ELEM695L EQU   *-ELEM695                                                        
*                                                                               
ELEM700  DC    AL1(ELEM700L)                                                    
         DC    C'<!-- Footer -->'                                               
ELEM700L EQU   *-ELEM700                                                        
*                                                                               
ELEM705  DC    AL1(ELEM705L)                                                    
         DC    C'<table width="100%" cellspacing="0" cellpadding="0" '          
         DC    C'style="height:78px; font-family: Calibri, sans-serif;'         
ELEM705L EQU   *-ELEM705                                                        
*                                                                               
ELEM710  DC    AL1(ELEM710L)                                                    
         DC    C' font-size:10px; color:#ffffff; '                              
         DC    C'background-color:#4E4D4C; text-align:center; '                 
         DC    C'border-bottom:1px solid #4E4D4C; '                             
ELEM710L EQU   *-ELEM710                                                        
*                                                                               
ELEM715  DC    AL1(ELEM715L)                                                    
         DC    C'-webkit-border-radius: 3px; -moz-border-radius: 3px; '         
         DC    C'border-radius: 3px;">'                                         
ELEM715L EQU   *-ELEM715                                                        
*                                                                               
ELEM720  DC    AL1(ELEM720L)                                                    
         DC    C'<tr>'                                                          
ELEM720L EQU   *-ELEM720                                                        
*                                                                               
ELEM725  DC    AL1(ELEM725L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
ELEM725L EQU   *-ELEM725                                                        
*                                                                               
ELEM730  DC    AL1(ELEM730L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer"'            
         DC    C' src="http://info.mediaocean.com/rs/331-XPM-231/'              
         DC    C'images/spacer_icon.png"/>'                                     
ELEM730L EQU   *-ELEM730                                                        
*                                                                               
ELEM735  DC    AL1(ELEM735L)                                                    
         DC    C'</td>'                                                         
ELEM735L EQU   *-ELEM735                                                        
*                                                                               
ELEM740  DC    AL1(ELEM740L)                                                    
         DC    C'<td colspan="2" height="15" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM740L EQU   *-ELEM740                                                        
*                                                                               
ELEM745  DC    AL1(ELEM745L)                                                    
         DC    C'<img height="15" border="0" alt="spacer" '                     
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM745L EQU   *-ELEM745                                                        
*                                                                               
ELEM750  DC    AL1(ELEM750L)                                                    
         DC    C'</td>'                                                         
ELEM750L EQU   *-ELEM750                                                        
*                                                                               
ELEM755  DC    AL1(ELEM755L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
ELEM755L EQU   *-ELEM755                                                        
*                                                                               
ELEM760  DC    AL1(ELEM760L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer"'            
         DC    C' src="http://info.mediaocean.com/rs/331-XPM-231/'              
         DC    C'images/spacer_icon.png"/>'                                     
ELEM760L EQU   *-ELEM760                                                        
*                                                                               
ELEM765  DC    AL1(ELEM765L)                                                    
         DC    C'</td>'                                                         
ELEM765L EQU   *-ELEM765                                                        
*                                                                               
ELEM770  DC    AL1(ELEM770L)                                                    
         DC    C'</tr>'                                                         
ELEM770L EQU   *-ELEM770                                                        
*                                                                               
ELEM775  DC    AL1(ELEM775L)                                                    
         DC    C'<tr style="vertical-align:bottom;">'                           
ELEM775L EQU   *-ELEM775                                                        
*                                                                               
ELEM780  DC    AL1(ELEM780L)                                                    
         DC    C'<td style="text-align:left;">'                                 
ELEM780L EQU   *-ELEM780                                                        
*                                                                               
ELEM785  DC    AL1(ELEM785L)                                                    
         DC    C'<div>'                                                         
ELEM785L EQU   *-ELEM785                                                        
*                                                                               
ELEM790  DC    AL1(ELEM790L)                                                    
ELEM790E DS    CL90                                                             
ELEM790L EQU   *-ELEM790                                                        
*                                                                               
ELEM795  DC    AL1(ELEM795L)                                                    
         DC    C'<br />'                                                        
ELEM795L EQU   *-ELEM795                                                        
*                                                                               
ELEM800  DC    AL1(ELEM800L)                                                    
         DC    X'50'                                                            
         DC    C'copy '                                                         
ELEM800E DS    CL4                                                              
         DC    C' MEDIAOCEAN'                                                   
ELEM800L EQU   *-ELEM800                                                        
*                                                                               
ELEM805  DC    AL1(ELEM805L)                                                    
         DC    C'| '                                                            
ELEM805E DS    CL18                                                             
         DC    C':'                                                             
ELEM805L EQU   *-ELEM805                                                        
*                                                                               
ELEM810  DC    AL1(ELEM810L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="https://www.facebook.com/team.mediaocean">'              
         DC    C'FACEBOOK'                                                      
         DC    C'</a>'                                                          
         DC    C' |'                                                            
ELEM810L EQU   *-ELEM810                                                        
*                                                                               
ELEM815  DC    AL1(ELEM815L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="https://twitter.com/teammediaocean">'                    
         DC    C'TWITTER'                                                       
         DC    C'</a>'                                                          
         DC    C' |'                                                            
ELEM815L EQU   *-ELEM815                                                        
*                                                                               
ELEM820  DC    AL1(ELEM820L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="http://www.linkedin.com/company/mediaocean">'            
         DC    C'LINKEDIN'                                                      
         DC    C'</a>'                                                          
         DC    C' |'                                                            
ELEM820L EQU   *-ELEM820                                                        
*                                                                               
ELEM822  DC    AL1(ELEM822L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="http://instagram.com/teammediaocean">'                   
         DC    C'INSTAGRAM'                                                     
         DC    C'</a>'                                                          
ELEM822L EQU   *-ELEM822                                                        
*                                                                               
ELEM825  DC    AL1(ELEM825L)                                                    
         DC    C'</div>'                                                        
ELEM825L EQU   *-ELEM825                                                        
*                                                                               
ELEM830  DC    AL1(ELEM830L)                                                    
         DC    C'</td>'                                                         
ELEM830L EQU   *-ELEM830                                                        
*                                                                               
ELEM835  DC    AL1(ELEM835L)                                                    
         DC    C'<td style="text-align:right;">'                                
ELEM835L EQU   *-ELEM835                                                        
*                                                                               
ELEM840  DC    AL1(ELEM840L)                                                    
         DC    C'<a href="'                                                     
ELEM840E DS    CL20                                                             
         DC    C'">'                                                            
ELEM840L EQU   *-ELEM840                                                        
*                                                                               
ELEM845  DC    AL1(ELEM845L)                                                    
         DC    C'<img src="http://info.mediaocean.com/rs/331-XPM-231/'          
         DC    C'images/MO-Aura-footer-logo.png" alt="MediaOcean" />'           
ELEM845L EQU   *-ELEM845                                                        
*                                                                               
ELEM850  DC    AL1(ELEM850L)                                                    
         DC    C'</a>'                                                          
ELEM850L EQU   *-ELEM850                                                        
*                                                                               
ELEM855  DC    AL1(ELEM855L)                                                    
         DC    C'</td>'                                                         
ELEM855L EQU   *-ELEM855                                                        
*                                                                               
ELEM860  DC    AL1(ELEM860L)                                                    
         DC    C'</tr>'                                                         
ELEM860L EQU   *-ELEM860                                                        
*                                                                               
ELEM865  DC    AL1(ELEM865L)                                                    
         DC    C'<tr>'                                                          
ELEM865L EQU   *-ELEM865                                                        
*                                                                               
ELEM870  DC    AL1(ELEM870L)                                                    
         DC    C'<td colspan="2" height="15" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM870L EQU   *-ELEM870                                                        
*                                                                               
ELEM875  DC    AL1(ELEM875L)                                                    
         DC    C'<img height="15" border="0" alt="spacer" '                     
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM875L EQU   *-ELEM875                                                        
*                                                                               
ELEM880  DC    AL1(ELEM880L)                                                    
         DC    C'</td>'                                                         
ELEM880L EQU   *-ELEM880                                                        
*                                                                               
ELEM885  DC    AL1(ELEM885L)                                                    
         DC    C'</tr>'                                                         
ELEM885L EQU   *-ELEM885                                                        
*                                                                               
ELEM890  DC    AL1(ELEM890L)                                                    
         DC    C'</table>'                                                      
ELEM890L EQU   *-ELEM890                                                        
*                                                                               
ELEM895  DC    AL1(ELEM895L)                                                    
         DC    C'<!-- end: Footer -->'                                          
ELEM895L EQU   *-ELEM895                                                        
*                                                                               
ELEM900  DC    AL1(ELEM900L)                                                    
         DC    C'</td>'                                                         
ELEM900L EQU   *-ELEM900                                                        
*                                                                               
ELEM905  DC    AL1(ELEM905L)                                                    
         DC    C'</tr>'                                                         
ELEM905L EQU   *-ELEM905                                                        
*                                                                               
ELEM910  DC    AL1(ELEM910L)                                                    
         DC    C'</table>'                                                      
ELEM910L EQU   *-ELEM910                                                        
*                                                                               
ELEM915  DC    AL1(ELEM915L)                                                    
         DC    C'<!-- End Inner Wrap -->'                                       
ELEM915L EQU   *-ELEM915                                                        
*                                                                               
ELEM920  DC    AL1(ELEM920L)                                                    
         DC    C'</td>'                                                         
ELEM920L EQU   *-ELEM920                                                        
*                                                                               
ELEM925  DC    AL1(ELEM925L)                                                    
         DC    C'</tr>'                                                         
ELEM925L EQU   *-ELEM925                                                        
*                                                                               
ELEM930  DC    AL1(ELEM930L)                                                    
         DC    C'</table>'                                                      
ELEM930L EQU   *-ELEM930                                                        
*                                                                               
ELEM935  DC    AL1(ELEM935L)                                                    
         DC    C'<!-- End Outer Wrap -->'                                       
ELEM935L EQU   *-ELEM935                                                        
*                                                                               
ELEM940  DC    AL1(ELEM940L)                                                    
         DC    C'</body>'                                                       
ELEM940L EQU   *-ELEM940                                                        
*                                                                               
ELEM945  DC    AL1(ELEM945L)                                                    
         DC    C'</html>'                                                       
ELEM945L EQU   *-ELEM945                                                        
*                                                                               
EMLSENDL EQU   *-EMLSEND                                                        
EMLHTMLL EQU   *-EMLHTML                                                        
ELEMSENX DC    X'FF'                                                            
*                                ** Summary Email **                            
EMLSUM   DS    0X                                                               
*                                                                               
EMLSUMT  DS    0H                   Top Section                                 
*                                                                               
EMLS05   DC    AL1(EMLS05L)                                                     
         DC    C'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 '                 
         DC    C'Transitional//EN" '                                            
         DC    C'"http://www.w3.org/TR/xhtml11/DTD/xhtml1-transitional'         
         DC    C'.dtd">'                                                        
EMLS05L  EQU   *-EMLS05                                                         
*                                                                               
EMLS10   DC    AL1(EMLS10L)                                                     
         DC    C'<html xmlns="http://www.w3.org/1999/xhtml">'                   
EMLS10L  EQU   *-EMLS10                                                         
*                                                                               
EMLS15   DC    AL1(EMLS15L)                                                     
         DC    C'<head>'                                                        
EMLS15L  EQU   *-EMLS15                                                         
*                                                                               
EMLS20   DC    AL1(EMLS20L)                                                     
         DC    C'<title>'                                                       
EMLS20L  EQU   *-EMLS20                                                         
*                                                                               
EMLS25   DC    AL1(EMLS25L)                                                     
         DC    C'</title>'                                                      
EMLS25L  EQU   *-EMLS25                                                         
*                                                                               
EMLS30   DC    AL1(EMLS30L)                                                     
         DC    C'<meta http-equiv="Content-Type" content="text/html;'           
         DC    C' charset=UTF-8" />'                                            
EMLS30L  EQU   *-EMLS30                                                         
*                                                                               
EMLS35   DC    AL1(EMLS35L)                                                     
         DC    C'<meta name="viewport" content="width=device-width; '           
         DC    C'initial-scale=1; maximum-scale=1.0"/>'                         
EMLS35L  EQU   *-EMLS35                                                         
*                                                                               
EMLS40   DC    AL1(EMLS40L)                                                     
         DC    C'</head>'                                                       
EMLS40L  EQU   *-EMLS40                                                         
*                                                                               
EMLS45   DC    AL1(EMLS45L)                                                     
         DC    C'<body bgcolor="#ffffff">'                                      
EMLS45L  EQU   *-EMLS45                                                         
*                                                                               
EMLS50   DC    AL1(EMLS50L)                                                     
         DC    C'<!-- Outer Wrap -->'                                           
EMLS50L  EQU   *-EMLS50                                                         
*                                                                               
EMLS55   DC    AL1(EMLS55L)                                                     
         DC    C'<table width="800" border="0" cellspacing="0" '                
         DC    C'cellpadding="0" bgcolor="#ffffff" '                            
         DC    C'style="table-layout:fixed;">'                                  
EMLS55L  EQU   *-EMLS55                                                         
*                                                                               
EMLS60   DC    AL1(EMLS60L)                                                     
         DC    C'<tr>'                                                          
EMLS60L  EQU   *-EMLS60                                                         
*                                                                               
EMLS65   DC    AL1(EMLS65L)                                                     
         DC    C'<td>'                                                          
EMLS65L  EQU   *-EMLS65                                                         
*                                                                               
EMLS70   DC    AL1(EMLS70L)                                                     
         DC    C'<!-- top shadow -->'                                           
EMLS70L  EQU   *-EMLS70                                                         
*                                                                               
EMLS75   DC    AL1(EMLS75L)                                                     
         DC    C'<table width="800" border="0" cellspacing="0" '                
         DC    C'cellpadding="0">'                                              
EMLS75L  EQU   *-EMLS75                                                         
*                                                                               
EMLS80   DC    AL1(EMLS80L)                                                     
         DC    C'<tr>'                                                          
EMLS80L  EQU   *-EMLS80                                                         
*                                                                               
EMLS85   DC    AL1(EMLS85L)                                                     
         DC    C'<td height="10">'                                              
EMLS85L  EQU   *-EMLS85                                                         
*                                                                               
EMLS90   DC    AL1(EMLS90L)                                                     
         DC    C'</tr>'                                                         
EMLS90L  EQU   *-EMLS90                                                         
*                                                                               
EMLS95   DC    AL1(EMLS95L)                                                     
         DC    C'</table>'                                                      
EMLS95L  EQU   *-EMLS95                                                         
*                                                                               
EMLS100  DC    AL1(EMLS100L)                                                    
         DC    C'<!-- header -->'                                               
EMLS100L EQU   *-EMLS100                                                        
*                                                                               
EMLS105  DC    AL1(EMLS105L)                                                    
         DC    C'<table width="800" bgcolor="#ffffff" border="0" '              
         DC    C'cellspacing="0" cellpadding="0">'                              
EMLS105L EQU   *-EMLS105                                                        
*                                                                               
EMLS110  DC    AL1(EMLS110L)                                                    
         DC    C'<tr>'                                                          
EMLS110L EQU   *-EMLS110                                                        
*                                                                               
EMLS115  DC    AL1(EMLS115L)                                                    
         DC    C'<td>'                                                          
EMLS115L EQU   *-EMLS115                                                        
*                                                                               
EMLS120  DC    AL1(EMLS120L)                                                    
         DC    C'<a href="'                                                     
EMLS120E DS    CL20                                                             
         DC    C'">'                                                            
EMLS120L EQU   *-EMLS120                                                        
*                                                                               
EMLS125  DC    AL1(EMLS125L)                                                    
         DC    C'<img alt="Aura" src="http://info.mediaocean.com/rs/'           
         DC    C'331-XPM-231/images/Aura-header.png"/>'                         
EMLS125L EQU   *-EMLS125                                                        
*                                                                               
EMLS130  DC    AL1(EMLS130L)                                                    
         DC    C'</a>'                                                          
EMLS130L EQU   *-EMLS130                                                        
*                                                                               
EMLS135  DC    AL1(EMLS135L)                                                    
         DC    C'</td>'                                                         
EMLS135L EQU   *-EMLS135                                                        
*                                                                               
EMLS140  DC    AL1(EMLS140L)                                                    
         DC    C'</tr>'                                                         
EMLS140L EQU   *-EMLS140                                                        
*                                                                               
EMLS145  DC    AL1(EMLS145L)                                                    
         DC    C'</table>'                                                      
EMLS145L EQU   *-EMLS145                                                        
*                                                                               
EMLS150  DC    AL1(EMLS150L)                                                    
         DC    C'<!-- end: header -->'                                          
EMLS150L EQU   *-EMLS150                                                        
*                                                                               
EMLS155  DC    AL1(EMLS155L)                                                    
         DC    C'<!-- body -->'                                                 
EMLS155L EQU   *-EMLS155                                                        
*                                                                               
EMLS160  DC    AL1(EMLS160L)                                                    
         DC    C'<table width="800" border="0" cellspacing="0" '                
         DC    C'cellpadding="0">'                                              
EMLS160L EQU   *-EMLS160                                                        
*                                                                               
EMLS165  DC    AL1(EMLS165L)                                                    
         DC    C'<tr>'                                                          
EMLS165L EQU   *-EMLS165                                                        
*                                                                               
EMLS170  DC    AL1(EMLS170L)                                                    
         DC    C'<td height="10">'                                              
EMLS170L EQU   *-EMLS170                                                        
*                                                                               
EMLS175  DC    AL1(EMLS175L)                                                    
         DC    C'</td>'                                                         
EMLS175L EQU   *-EMLS175                                                        
*                                                                               
EMLS180  DC    AL1(EMLS180L)                                                    
         DC    C'</tr>'                                                         
EMLS180L EQU   *-EMLS180                                                        
*                                                                               
EMLS185  DC    AL1(EMLS185L)                                                    
         DC    C'</table>'                                                      
EMLS185L EQU   *-EMLS185                                                        
*                                                                               
EMLS190  DC    AL1(EMLS190L)                                                    
         DC    C'<!-- tape check module -->'                                    
EMLS190L EQU   *-EMLS190                                                        
*                                                                               
EMLSUMTL EQU   *-EMLSUMT                                                        
EMLSUMTX DC    X'FF'                                                            
*                                ** Start of email section **                   
EMLSSECT DS    0X                                                               
*                                                                               
EMLS195  DC    AL1(EMLS195L)                                                    
         DC    C'<table width="800" bgcolor="#ffffff" border="0" '              
         DC    C'cellspacing="0" cellpadding="0" '                              
         DC    C'style="font-family: Calibri, sans-serif; '                     
         DC    C'font-size:13px; color:#4E4D4C; '                               
EMLS195L EQU   *-EMLS195                                                        
*                                                                               
EMLS200  DC    AL1(EMLS200L)                                                    
         DC    C'background-color:#D9D9CD; '                                    
         DC    C'-webkit-border-radius: 3px; '                                  
         DC    C'-moz-border-radius: 3px; '                                     
         DC    C'border-radius: 3px;">'                                         
EMLS200L EQU   *-EMLS200                                                        
*                                                                               
EMLS205  DC    AL1(EMLS205L)                                                    
         DC    C'<tr>'                                                          
EMLS205L EQU   *-EMLS205                                                        
*                                                                               
EMLS210  DC    AL1(EMLS210L)                                                    
         DC    C'<td height="20" colspan="3" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
EMLS210L EQU   *-EMLS210                                                        
*                                                                               
EMLS215  DC    AL1(EMLS215L)                                                    
         DC    C'<img height="20" alt="spacer" '                                
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS215L EQU   *-EMLS215                                                        
*                                                                               
EMLS220  DC    AL1(EMLS220L)                                                    
         DC    C'</td>'                                                         
EMLS220L EQU   *-EMLS220                                                        
*                                                                               
EMLS225  DC    AL1(EMLS225L)                                                    
         DC    C'</tr>'                                                         
EMLS225L EQU   *-EMLS225                                                        
*                                                                               
EMLS230  DC    AL1(EMLS230L)                                                    
         DC    C'<tr>'                                                          
EMLS230L EQU   *-EMLS230                                                        
*                                                                               
EMLS235  DC    AL1(EMLS235L)                                                    
         DC    C'<td width="20">'                                               
EMLS235L EQU   *-EMLS235                                                        
*                                                                               
EMLS240  DC    AL1(EMLS240L)                                                    
         DC    C'<img height="1" width="20" alt="spacer" '                      
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS240L EQU   *-EMLS240                                                        
*                                                                               
EMLS245  DC    AL1(EMLS245L)                                                    
         DC    C'</td>'                                                         
EMLS245L EQU   *-EMLS245                                                        
*                                                                               
EMLS250  DC    AL1(EMLS250L)                                                    
         DC    C'<td valign="top" width="360" '                                 
         DC    C'style="font-family: Calibri, sans-serif; '                     
         DC    C'font-size:30px; color:#4E4D4C; line-height:36px;">'            
EMLS250L EQU   *-EMLS250                                                        
*                                                                               
EMLS255  DC    AL1(EMLS255L)                                                    
         DC    C'<div>'                                                         
EMLS255E DS    CL80                                                             
         DC    C'</div>'                                                        
EMLS255L EQU   *-EMLS255                                                        
*                                                                               
EMLS260  DC    AL1(EMLS260L)                                                    
EMLS260E DS    CL(MAXELEN)                                                      
EMLS260L EQU   *-EMLS260                                                        
*                                                                               
EMLS261  DC    AL1(EMLS261L)                                                    
EMLS261E DS    CL(MAXELEN)                                                      
EMLS261L EQU   *-EMLS261                                                        
*                                                                               
EMLS262  DC    AL1(EMLS262L)                                                    
EMLS262E DS    CL(MAXELEN)                                                      
EMLS262L EQU   *-EMLS262                                                        
*                                                                               
EMLS270  DC    AL1(EMLS270L)                                                    
         DC    C'<p style="color:#4E4D4C; font-size:13px; '                     
         DC    C'text-decoration:none; line-height:16px;">'                     
EMLS270L EQU   *-EMLS270                                                        
*                                                                               
EMLS275  DC    AL1(EMLS275L)                                                    
         DC    C'<a style="color:#4E4D4C; font-size:13px; '                     
         DC    C'text-decoration:none; font-weight:bold;" href="'               
EMLS275L EQU   *-EMLS275                                                        
*                                                                               
EMLS280  DC    AL1(EMLS280L)                                                    
EMLS280E DS    CL160                                                            
EMLS280L EQU   *-EMLS280                                                        
*                                                                               
EMLS282  DC    AL1(EMLS282L)                                                    
EMLS282E DS    CL160                                                            
EMLS282L EQU   *-EMLS282                                                        
*                                                                               
EMLS285  DC    AL1(EMLS285L)                                                    
EMLS285E DS    CL80                                                             
EMLS285L EQU   *-EMLS285                                                        
*                                                                               
EMLS290  DC    AL1(EMLS290L)                                                    
         DC    C'</a>'                                                          
EMLS290L EQU   *-EMLS290                                                        
*                                                                               
EMLS295  DC    AL1(EMLS295L)                                                    
         DC    C'</p>'                                                          
EMLS295L EQU   *-EMLS295                                                        
*                                                                               
EMLS300  DC    AL1(EMLS300L)                                                    
         DC    C'</td>'                                                         
EMLS300L EQU   *-EMLS300                                                        
*                                                                               
EMLS305  DC    AL1(EMLS305L)                                                    
         DC    C'<td width="20">'                                               
EMLS305L EQU   *-EMLS305                                                        
*                                                                               
EMLS310  DC    AL1(EMLS310L)                                                    
         DC    C'<img height="1" width="20" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS310L EQU   *-EMLS310                                                        
*                                                                               
EMLS315  DC    AL1(EMLS315L)                                                    
         DC    C'</td>'                                                         
EMLS315L EQU   *-EMLS315                                                        
*                                                                               
EMLS320  DC    AL1(EMLS320L)                                                    
         DC    C'</tr>'                                                         
EMLS320L EQU   *-EMLS320                                                        
*                                                                               
EMLS325  DC    AL1(EMLS325L)                                                    
         DC    C'<tr>'                                                          
EMLS325L EQU   *-EMLS325                                                        
*                                                                               
EMLS330  DC    AL1(EMLS330L)                                                    
         DC    C'<td height="30" colspan="3" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
EMLS330L EQU   *-EMLS330                                                        
*                                                                               
EMLS335  DC    AL1(EMLS335L)                                                    
         DC    C'<img height="30" width="20" border="0" alt="spacer" '          
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS335L EQU   *-EMLS335                                                        
*                                                                               
EMLS340  DC    AL1(EMLS340L)                                                    
         DC    C'</td>'                                                         
EMLS340L EQU   *-EMLS340                                                        
*                                                                               
EMLS345  DC    AL1(EMLS345L)                                                    
         DC    C'</tr>'                                                         
EMLS345L EQU   *-EMLS345                                                        
*                                                                               
EMLS350  DC    AL1(EMLS350L)                                                    
         DC    C'</table>'                                                      
EMLS350L EQU   *-EMLS350                                                        
*                                                                               
EMLSSECX DC    X'FF'                ** End of summary email section **          
EMLSSENL EQU   *-EMLSSECT                                                       
*                                                                               
EMLSEND  DS    0X                                                               
*                                                                               
EMLS355  DC    AL1(EMLS355L)                                                    
         DC    C'<!-- no background module -->'                                 
EMLS355L EQU   *-EMLS355                                                        
*                                                                               
EMLS360  DC    AL1(EMLS360L)                                                    
         DC    C'<table width="800" bgcolor="#ffffff" border="0" '              
         DC    C'cellspacing="0" cellpadding="0" style="font-family: '          
EMLS360L EQU   *-EMLS360                                                        
*                                                                               
EMLS365  DC    AL1(EMLS365L)                                                    
         DC    C'Calibri, sans-serif; font-size:10px; color:#4E4D4C; '          
         DC    C'background-color:#ffffff; -webkit-border-radius: 3px;'         
         DC    C' -moz-border-radius: 3px; border-radius: 3px;">'               
EMLS365L EQU   *-EMLS365                                                        
*                                                                               
EMLS370  DC    AL1(EMLS370L)                                                    
         DC    C'<tr>'                                                          
EMLS370L EQU   *-EMLS370                                                        
*                                                                               
EMLS375  DC    AL1(EMLS375L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
EMLS375L EQU   *-EMLS375                                                        
*                                                                               
EMLS380  DC    AL1(EMLS380L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS380L EQU   *-EMLS380                                                        
*                                                                               
EMLS385  DC    AL1(EMLS385L)                                                    
         DC    C'</td>'                                                         
EMLS385L EQU   *-EMLS385                                                        
*                                                                               
EMLS390  DC    AL1(EMLS390L)                                                    
         DC    C'<td>'                                                          
EMLS390L EQU   *-EMLS390                                                        
*                                                                               
EMLS395  DC    AL1(EMLS395L)                                                    
         DC    X'50'                                                            
         DC    C'nbsp;'                                                         
EMLS395L EQU   *-EMLS395                                                        
*                                                                               
EMLS400  DC    AL1(EMLS400L)                                                    
         DC    C'</td>'                                                         
EMLS400L EQU   *-EMLS400                                                        
*                                                                               
EMLS405  DC    AL1(EMLS405L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
EMLS405L EQU   *-EMLS405                                                        
*                                                                               
EMLS410  DC    AL1(EMLS410L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS410L EQU   *-EMLS410                                                        
*                                                                               
EMLS415  DC    AL1(EMLS415L)                                                    
         DC    C'</td>'                                                         
EMLS415L EQU   *-EMLS415                                                        
*                                                                               
EMLS420  DC    AL1(EMLS420L)                                                    
         DC    C'</tr>'                                                         
EMLS420L EQU   *-EMLS420                                                        
*                                                                               
EMLS425  DC    AL1(EMLS425L)                                                    
         DC    C'<tr>'                                                          
EMLS425L EQU   *-EMLS425                                                        
*                                                                               
EMLS430  DC    AL1(EMLS430L)                                                    
         DC    C'<td style="vertical-align:top;">'                              
EMLS430L EQU   *-EMLS430                                                        
*                                                                               
EMLS435  DC    AL1(EMLS435L)                                                    
         DC    C'<p>'                                                           
EMLS435L EQU   *-EMLS435                                                        
*                                                                               
EMLS440  DC    AL1(EMLS440L)                                                    
EMAILS1  DS    CL80                                                             
EMAILS2  DS    CL80                                                             
EMLS440L EQU   *-EMLS440                                                        
*                                                                               
EMLS445  DC    AL1(EMLS445L)                                                    
EMAILS3  DS    CL80                                                             
EMAILS4  DS    CL80                                                             
EMLS445L EQU   *-EMLS445                                                        
*                                                                               
EMLS450  DC    AL1(EMLS450L)                                                    
EMAILS5  DS    CL80                                                             
         DC    C'</p>'                                                          
EMLS450L EQU   *-EMLS450                                                        
*                                                                               
EMLS455  DC    AL1(EMLS455L)                                                    
         DC    C'<p>'                                                           
EMLS455L EQU   *-EMLS455                                                        
*                                                                               
EMLS460  DC    AL1(EMLS460L)                                                    
EMAILS6  DS    CL80                                                             
EMAILS7  DS    CL80                                                             
EMLS460L EQU   *-EMLS460                                                        
*                                                                               
EMLS465  DC    AL1(EMLS465L)                                                    
EMAILS8  DS    CL80                                                             
EMAILS9  DS    CL80                                                             
EMLS465L EQU   *-EMLS465                                                        
*                                                                               
EMLS468  DC    AL1(EMLS468L)                                                    
EMAILSA  DS    CL80                                                             
EMLS468L EQU   *-EMLS468                                                        
*                                                                               
EMLS470  DC    AL1(EMLS470L)                                                    
         DC    C'</p>'                                                          
EMLS470L EQU   *-EMLS470                                                        
*                                                                               
EMLS475  DC    AL1(EMLS475L)                                                    
         DC    C'</td>'                                                         
EMLS475L EQU   *-EMLS475                                                        
*                                                                               
EMLS480  DC    AL1(EMLS480L)                                                    
         DC    C'</tr>'                                                         
EMLS480L EQU   *-EMLS480                                                        
*                                                                               
EMLS485  DC    AL1(EMLS485L)                                                    
         DC    C'<tr>'                                                          
EMLS485L EQU   *-EMLS485                                                        
*                                                                               
EMLS490  DC    AL1(EMLS490L)                                                    
         DC    C'<td height="20" style="font-size:1px; '                        
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
EMLS490L EQU   *-EMLS490                                                        
*                                                                               
EMLS495  DC    AL1(EMLS495L)                                                    
         DC    C'<img height="20" border="0" alt=spacer" '                      
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS495L EQU   *-EMLS495                                                        
*                                                                               
EMLS500  DC    AL1(EMLS500L)                                                    
         DC    C'</td>'                                                         
EMLS500L EQU   *-EMLS500                                                        
*                                                                               
EMLS505  DC    AL1(EMLS505L)                                                    
         DC    C'</tr>'                                                         
EMLS505L EQU   *-EMLS505                                                        
*                                                                               
EMLS510  DC    AL1(EMLS510L)                                                    
         DC    C'</table>'                                                      
EMLS510L EQU   *-EMLS510                                                        
*                                                                               
EMLS515  DC    AL1(EMLS515L)                                                    
         DC    C'<!-- end: body -->'                                            
EMLS515L EQU   *-EMLS515                                                        
*                                                                               
EMLS520  DC    AL1(EMLS520L)                                                    
         DC    C'<!-- Footer -->'                                               
EMLS520L EQU   *-EMLS520                                                        
*                                                                               
EMLS525  DC    AL1(EMLS525L)                                                    
         DC    C'<table width="800" cellspacing="0" cellpadding="0" '           
         DC    C'style="height:78px; font-family: Calibri, '                    
EMLS525L EQU   *-EMLS525                                                        
*                                                                               
EMLS530  DC    AL1(EMLS530L)                                                    
         DC    C'sans-serif; font-size:10px; color:#ffffff; '                   
         DC    C'background-color:#4E4D4C; text-align:center; '                 
EMLS530L EQU   *-EMLS530                                                        
*                                                                               
EMLS535  DC    AL1(EMLS535L)                                                    
         DC    C'border-bottom:1px solid #4E4D4C; '                             
         DC    C'-webkit-border-radius: 3px; -moz-border-radius: 3px; '         
         DC    C'border-radius: 3px;">'                                         
EMLS535L EQU   *-EMLS535                                                        
*                                                                               
EMLS540  DC    AL1(EMLS540L)                                                    
         DC    C'<tr>'                                                          
EMLS540L EQU   *-EMLS540                                                        
*                                                                               
EMLS545  DC    AL1(EMLS545L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
EMLS545L EQU   *-EMLS545                                                        
*                                                                               
EMLS550  DC    AL1(EMLS550L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS550L EQU   *-EMLS550                                                        
*                                                                               
EMLS555  DC    AL1(EMLS555L)                                                    
         DC    C'</td>'                                                         
EMLS555L EQU   *-EMLS555                                                        
*                                                                               
EMLS560  DC    AL1(EMLS560L)                                                    
         DC    C'<td colspan="2" height="15" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
EMLS560L EQU   *-EMLS560                                                        
*                                                                               
EMLS565  DC    AL1(EMLS565L)                                                    
         DC    C'<img height="15" border="0" alt="spacer" '                     
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS565L EQU   *-EMLS565                                                        
*                                                                               
EMLS570  DC    AL1(EMLS570L)                                                    
         DC    C'</td>'                                                         
EMLS570L EQU   *-EMLS570                                                        
*                                                                               
EMLS575  DC    AL1(EMLS575L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
EMLS575L EQU   *-EMLS575                                                        
*                                                                               
EMLS580  DC    AL1(EMLS580L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS580L EQU   *-EMLS580                                                        
*                                                                               
EMLS585  DC    AL1(EMLS585L)                                                    
         DC    C'</td>'                                                         
EMLS585L EQU   *-EMLS585                                                        
*                                                                               
EMLS590  DC    AL1(EMLS590L)                                                    
         DC    C'</tr>'                                                         
EMLS590L EQU   *-EMLS590                                                        
*                                                                               
EMLS595  DC    AL1(EMLS595L)                                                    
         DC    C'<tr style="vertical-align:bottom;">'                           
EMLS595L EQU   *-EMLS595                                                        
*                                                                               
EMLS600  DC    AL1(EMLS600L)                                                    
         DC    C'<td style="text-align:left;">'                                 
EMLS600L EQU   *-EMLS600                                                        
*                                                                               
EMLS605  DC    AL1(EMLS605L)                                                    
         DC    C'<div>'                                                         
EMLS605L EQU   *-EMLS605                                                        
*                                                                               
EMLS610  DC    AL1(EMLS610L)                                                    
EMLS610E DS    CL90                                                             
EMLS610L EQU   *-EMLS610                                                        
*                                                                               
EMLS615  DC    AL1(EMLS615L)                                                    
         DC    C'<br />'                                                        
EMLS615L EQU   *-EMLS615                                                        
*                                                                               
EMLS620  DC    AL1(EMLS620L)                                                    
         DC    X'50'                                                            
         DC    C'copy'                                                          
EMLS620E DS    CL4                                                              
         DC    C' MEDIAOCEAN'                                                   
EMLS620L EQU   *-EMLS620                                                        
*                                                                               
EMLS625  DC    AL1(EMLS625L)                                                    
         DC    C'| '                                                            
EMLS625E DS    CL18                                                             
         DC    C':'                                                             
EMLS625L EQU   *-EMLS625                                                        
*                                                                               
EMLS630  DC    AL1(EMLS630L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="https://www.facebook.com/team.mediaocean">'              
         DC    C'FACEBOOK'                                                      
         DC    C'</a>'                                                          
         DC    C' |'                                                            
EMLS630L EQU   *-EMLS630                                                        
*                                                                               
EMLS635  DC    AL1(EMLS635L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="https://twitter.com/teammediaocean">'                    
         DC    C'TWITTER'                                                       
         DC    C'</a>'                                                          
         DC    C' |'                                                            
EMLS635L EQU   *-EMLS635                                                        
*                                                                               
EMLS640  DC    AL1(EMLS640L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="http://www.linkedin.com/company/mediaocean">'            
         DC    C'LINKEDIN'                                                      
         DC    C'</a>'                                                          
         DC    C' |'                                                            
EMLS640L EQU   *-EMLS640                                                        
*                                                                               
EMLS642  DC    AL1(EMLS642L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="http://instagram.com/teammediaocean">'                   
         DC    C'INSTAGRAM'                                                     
         DC    C'</a>'                                                          
EMLS642L EQU   *-EMLS642                                                        
*                                                                               
EMLS645  DC    AL1(EMLS645L)                                                    
         DC    C'</div>'                                                        
EMLS645L EQU   *-EMLS645                                                        
*                                                                               
EMLS650  DC    AL1(EMLS650L)                                                    
         DC    C'</td>'                                                         
EMLS650L EQU   *-EMLS650                                                        
*                                                                               
EMLS655  DC    AL1(EMLS655L)                                                    
         DC    C'<td style="text-align:right;">'                                
EMLS655L EQU   *-EMLS655                                                        
*                                                                               
EMLS660  DC    AL1(EMLS660L)                                                    
         DC    C'<a href="'                                                     
EMLS660E DS    CL20                                                             
         DC    C'">'                                                            
EMLS660L EQU   *-EMLS660                                                        
*                                                                               
EMLS665  DC    AL1(EMLS665L)                                                    
         DC    C'<img src="http://info.mediaocean.com/rs/331-XPM-231/'          
         DC    C'images/MO-Aura-footer-logo.png" alt="MediaOcean" />'           
EMLS665L EQU   *-EMLS665                                                        
*                                                                               
EMLS670  DC    AL1(EMLS670L)                                                    
         DC    C'</a>'                                                          
EMLS670L EQU   *-EMLS670                                                        
*                                                                               
EMLS675  DC    AL1(EMLS675L)                                                    
         DC    C'</td>'                                                         
EMLS675L EQU   *-EMLS675                                                        
*                                                                               
EMLS680  DC    AL1(EMLS680L)                                                    
         DC    C'</tr>'                                                         
EMLS680L EQU   *-EMLS680                                                        
*                                                                               
EMLS685  DC    AL1(EMLS685L)                                                    
         DC    C'</table>'                                                      
EMLS685L EQU   *-EMLS685                                                        
*                                                                               
EMLS690  DC    AL1(EMLS690L)                                                    
         DC    C'<!-- end: Footer -->'                                          
EMLS690L EQU   *-EMLS690                                                        
*                                                                               
EMLS695  DC    AL1(EMLS695L)                                                    
         DC    C'</td>'                                                         
EMLS695L EQU   *-EMLS695                                                        
*                                                                               
EMLS700  DC    AL1(EMLS700L)                                                    
         DC    C'</tr>'                                                         
EMLS700L EQU   *-EMLS700                                                        
*                                                                               
EMLS705  DC    AL1(EMLS705L)                                                    
         DC    C'</table>'                                                      
EMLS705L EQU   *-EMLS705                                                        
*                                                                               
EMLS710  DC    AL1(EMLS710L)                                                    
         DC    C'<!-- End Inner Wrap -->'                                       
EMLS710L EQU   *-EMLS710                                                        
*                                                                               
EMLS715  DC    AL1(EMLS715L)                                                    
         DC    C'</td>'                                                         
EMLS715L EQU   *-EMLS715                                                        
*                                                                               
EMLS720  DC    AL1(EMLS720L)                                                    
         DC    C'</tr>'                                                         
EMLS720L EQU   *-EMLS720                                                        
*                                                                               
EMLS725  DC    AL1(EMLS725L)                                                    
         DC    C'</table>'                                                      
EMLS725L EQU   *-EMLS725                                                        
*                                                                               
EMLS730  DC    AL1(EMLS730L)                                                    
         DC    C'<!-- End Outer Wrap -->'                                       
EMLS730L EQU   *-EMLS730                                                        
*                                                                               
EMLS735  DC    AL1(EMLS735L)                                                    
         DC    C'</body>'                                                       
EMLS735L EQU   *-EMLS735                                                        
*                                                                               
EMLS740  DC    AL1(EMLS740L)                                                    
         DC    C'</html>'                                                       
EMLS740L EQU   *-EMLS740                                                        
*                                                                               
EMLSENDX EQU   *-EMLSEND                                                        
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068ACREPEM02 11/09/20'                                      
         END                                                                    
