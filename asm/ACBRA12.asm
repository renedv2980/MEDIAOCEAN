*          DATA SET ACBRA12    AT LEVEL 140 AS OF 11/02/20                      
*PHASE T62412A                                                                  
                                                                                
ACBRA12  TITLE '- BRA eTime download Server'                                    
                                                                                
***********************************************************************         
* Level change comments                                                         
* ---------------------                                                         
* NSHE 001 15FEB06 eTime Time DownLoads - first version                         
* NSHE 003 08MAR07 BR11315L  fix to back up approver search                     
* NSHE 005 19MAR07 BR11509L  fix initial call                                   
* NSHE 006 03APR07 UKCR00012056 fix to split locations                          
* NSHE     24APR07 BR11894L Show old data row numbers correctly                 
* NSHE     27APR07 lO01-6306 add template time                                  
* NSHE 007 10MAY07 UKCR00012576 Fix search for my timesheets                    
* NSHE 008 11MAY07 BR10010Y Fix summary so it obeys office id security          
* NSHE 009 16MAY07 BR12167L Fix search where person isn't on limit list         
* NSHE 010 25MAY07 BR12272L Ensure fields cleared for status io call            
* NSHE 011 05JUN07 merge US and UK versions + DU01-6546 1N security             
* SMAN 012 19JUN07 BR10859D Hire date vs first MCS date                         
* SMAN     19JUN07 BR10021Y Fix error msg for overflow in t/s search            
* NSHE     20JUN07 BR12551L Fix error msg when start date ahead                 
* NSHE 013 25JUN07 BR12619L Fix when searching for back up approver             
* NSHE 014 02JUL07 BR12710L Hire date bug                                       
* NSHE 015 06JUL07 BR12680L Search bug when looking at clients                  
* TKLU 016 09JUL07 BR12820L PROMTSR return CC setting bug fix                   
* NSHE 017 09JUL07 BR12827L Deal with edit hours when calendar year in          
*                           future year                                         
* TKLU 018 18JUL07 LO01-6487 Add edit hours, period # and 1R off/dep/           
*                            sub/per to T/S display, plus person name           
* NSHE 019 27JUL07 BR13100L Improve error message when searching                
* NSHE 020 02AUG07 BR13164L Handle LOA status change in middle of               
*                  period                                                       
* TKLU 021 15AUG07 remove person names from level 18                            
* NSHE 021         <LO01-6490> Profile control client approvals                 
* NSHE 021         <BR11165D> Ensure correct status for self approval           
* NSHE 021         <UKCR00013890> Pass person search overide                    
* NSHE 022 12SEP07 <BR10043Y> Fix initial column bug                            
* NSHE 023 26SEP07 <UKCR00014399> Fix TS Lock feature                           
* NSHE 024 05OCT07 LO01-6904 Show back up approver in display                   
* NSHE 025 17OCT07 LO01-6924 Speed up search results                            
* NSHE     31OCT07 BR11555D Remove profiles not used                            
* NSHE 026 21NOV07 UKCR00015125 Add email address of user to output             
* NSHE     22NOV07 BR10065Y Fix client summaries                                
* NSHE 027 14DEC07 UKCR00015315 Client summary problem                          
* NSHE 028 19DEC07 BR10069Y Client summary in wrong order                       
* NSHE     20DEC07 BR11770D Showing deleted records                             
* NSHE 029 04JAN08 Merge US changes                                             
* NSHE 031 03FEB08 Run search and summary offline - fix to initial call         
* TKLU 032 08FEB08 <BR11986D> COTSD t/s end date bug fix                        
* NSHE 033 11FEB08 <BR15981L> Fix timesheet display for adjustments             
* TKLU 034 12FEB08 <BR11989D> Handle LOA vs. BRO=Yes in TSD=O scenario          
* SMAN 035 06MAR08 <BR15946L> Show older overdue timesheets                     
* NSHE             further fix to time summary                                  
* NSHE 036 14MAR08 further fix to time summary                                  
* SMAN 037 17MAR08 Clear TSWKEY for timesheet listing                           
* NSHE 038 27MAR08 further fix to time summary                                  
* NSHE 042 02APR08 Remover initial and display as now in ACBRA27                
* YNGX 042 15APR08 <BR17334L> Bug fix showing person in overdue list            
* NSHE 043 10JUN08 <UKCR00017868> Fix to summary + order name in search         
* NSHE 043         return narrative in search                                   
* NSHE 044 07JUL08 LO01-7842 Changes to audit                                   
* NSHE             LO01-7862 Target Utilisation                                 
* NSHE 047 22SEP08 LO01-8148 Target Utilisation                                 
* NSHE 048 03NOV08 LO01-8229 Allow search to bring back 1N time only            
* NSHE/SMAN 049 11NOV08 OT50464L Bug fixes for split location                   
* JFOS 050 27FEB09 BR23426L Search: match TACPAS/TIMOFF on SJ time              
* NSHE 051 11MAY09 Change to audit record                                       
* SMAN 052 18JUN09 Change GAPLST calls                                          
* NSHE 053 19SEP09 Allow approvals by office and media                          
* NSHE/MPEN 054 06OCT09 <BR14757D> FIX BUG IN CLIENT SUMMARY                    
* MPEN 055 07OCT09 <BR14760D> MINI BUG FIX TO THE ABOVE                         
* SMAN 056 15OCT09 <BR14716D> Avoid duplicates in Backup searches               
* SMAN 057 30OCT09 <BR14821D> Fix to Approver Overdue folder                    
* NSHE 058 30OCT09 Merge US changes                                             
* NSHE             Fix for looking up timesheets as an approver                 
* NSHE 059 11NOV09 Don't clear TSAR buffer with each request                    
* NRAK 059 17NOV09 <BR28677L> FIX CLIENT SUMMARY DETAIL CALL/VAL CPJ            
* NSHE 060 03DEC09 Allow US to use daily edit hours                             
* MPEN 061 09FEB10 <BR154300D> Correct client summary search                    
* SMAN 062 02MAR10 <BR31389L> Fix to TIMSRCH for split locations                
* NSHE 063 12MAR10 add new files in file list                                   
* MPEN 064 08JUL10 <BR16280D> Stop hours duplication                            
* MPEN     08JUL10 <BR34406L> Allow searching on non-active users               
* MPEN 065 09JUL10 Fix for above to ensure a location is passed                 
* NRAK 066 01MAR10 <lo01-9305> apply 1r limit/approver rules                    
* NRAK 066         allow persons to move between calendars                      
* NRAK 066         run summaries off fixed periods (SUMTAB)                     
* NRAK 066         Retire TACPAS references, use TSJPAS instead                 
* NRAK 066         Fix search by person+office(/dep/sdp), null TIMHRS           
* NRAK 067 01aug10 <br35022l> performance: skip real record reading             
*                     if by status/not started. BLDATSR TO REBUILD              
*                     PERTAB AT OFFICE LEVEL, NOT PERSON.                       
* NRAK 069 09aug10 <br16418l> fix office filtering                              
* NRAK 071 17AUG10 <UKCR00028786> Optimise calendar record reading              
* NRAK 071         <br16496d> bugfix staff summary date handling                
* NRAK 073 10SEP10 <UKCR00028786> 3-byte TD#SEQ for large reqs                  
* NSHE 074 11AUG10 Change how GAPLST is called                                  
* NRAK 075 01DEC10 If cli summ but no limlist, read product offices             
*                     into GAPLST  rea for TLGETAD etc                          
* NRAK 076 18jan11 <br17167D> fix dept/subdpt filtering                         
* NSHE 077 27JAN11 Optimise calendar buffering                                  
* SMAN 078 21FEB11 <BR39640L> Bug fix to time srch for split location           
* NSHE 079 04FEB11 <BR39506L> Fix awaiting approval search                      
* SMAN     14MAR11 <BR17564D> Bug fix to HRS search                             
* SMAN     17MAR11 <UKCR00031256> Collect Client Hrs for TIMLIS                 
* NSHE 081 16FEB11 <PR001254> Show job locking                                  
* JFOS     16FEB11 <BR39740L> Person last name now 36 chs in List/Srch          
* NRAK     04MAY11 <BR10042X> Date optimisation causes loops                    
* NSHE 082 02JUN11 Remove TACPASD references                                    
* NRAK 083 06jul11 <BR18234D> return 1N dummies with proper ledger              
* YNGX 084 26JUL11 <BR18204D> Fix awaiting approval search bug                  
* NRAK 085 13SEP11 <UKCR33196> AVOID OVERLAPS BETWEEN X#SRCRET LOOPS            
* NRAK 086 29SEP11 <br18619d> fix to previous level                             
* YNGX 087 14OCT11 <BR18712D> FIX TOTAL HOUR BUG IN TIME SEARCH                 
* NSHE 088 11NOV11 Change GAPLIST calls                                         
* YNGX 088 06JAN12 <BR19031D> ADD PARTIALLY APPROVED TO SETATAB                 
* MPEN 089 16MAR12 <BR19344D> FIX CHECK FOR DUPLICATE ENTRIES IN TSAR           
* NRAK 091 09JAN12 <BR53386L> target util-handle more than 7 days               
* NSHE     05OCT12 Client approver issues with office calendars                 
* JFOS     22OCT12 <BR52440L> Allow max. possible GAPAREA - see comment         
* NSHE 092 10JUL13 Fix client approval look up                                  
* NSHE 093 28AUG13 Add middle names to all return calls + stats for             
* NSHE             list calls.  Audit for mobile approvals                      
*NRAK 094 19SEP13 <DSBO-311> bad map code on search/middle name                 
*NRAK 095 20SEP13 <DSBO-315> random names in List by status                     
*JFOS 096 30SEP13 <DSBO-326> Use later of loc start/hire date for t/s           
*                            period start. Further fix for lvl95 issues         
*NSHE     09OCT13 <DSRD-150> Log search requests with min/max hours             
*NRAK 097 15OCT13 <DSRD-381> Var length clitabd entries (capacity)              
*NSHE     22OCT13 <DSRD-176> Add logging for summary and search details         
*NSHE 098 06MAY14 <DSRD-2195> Improve error message                             
*MPEN 100 13AUG14 <DSBO-1100> Ignore lock date on list if approver              
*NSHE 101 06MAY14 <DSRD-3738> Fix back up approver search for sub               
*NSHE     19AUG14 <DSRD-3944> Awaiting me flag incorrect for Aura               
*NSHE 102 30JAN15 <DSRD-5398> Cannot copy timesheet Aura fix                    
*NSHE 103 25FEB15 <DSRD-6027> Allow future time to appear in awaiting           
*                 approver folder                                               
*MPEN 104 23APR15 <DSBO-1449> Fix for overdue timesheet lookup                  
*NSHE 105 24APR15 <DSBO-1406> Remove SETFAC routine                             
*NSHE 106 19JUN15 <DSRD-6906> Show awaiting approval in submitted srch          
*NSHE     22JUN15 <DSRD-6360> Show correct status for timesheet                 
*NSHE     25JUN15 <DSRD-7729> Ensure search sends correct job length            
*NSHE 107 03Jul15 <DSRD-7813> Open media system for dashboard                   
*NSHE 108 17Feb16 Add further alpha ids for QA system                           
*NSHE 109 04May16 DSRD-11053 Deal with empty timesheets                         
*NSHE 110 26Aug16 DSRD-12032 Show closed job indicator                          
*MPEN 111 06Sep16 DSRD-13161 My timesheet approvers list                        
*NSHE 112 16May17 DSRD-15819 fix search issue when estimate number              
*MPEN 113 24May17 DSRD-15869 Fix awaiting me list hours issue                   
*MPEN 114 17Jul17 DSRD-15589 Extending timeline narrative to 200 chars          
*NSHE     17Jul17 ITMF- 5589 Set TSAR buffer from saved storage                 
*MPEN 115 03Oct17 DSRD-16995 Fix for bad narrative                              
*MPEN 116 14Dec17 DSRD-17646 Return subdepartment in timesheet list             
*NSHE     11Jan18 DSRD-17088 Use back up approver rights when obeying           
*NSHE             search over 1R=N                                              
*NSHE 117 05Mar18 DSRD-18379 Deal with fiscal start month correctly             
*MPEN 118 19Mar18 DSRD-18367 Apply limlist and access security to srch          
*MPEN 119 30Apr18 DSRD-18930 Fix for time search limlist filtering              
*MPEN 120 26Jun18 DSRD-18930 Revert fix above                                   
*MPEN 121 08Oct18 DSRD-20424 Fix for limlist filtering                          
*MPEN 122 23Oct18 DSRD-20447 Relink for new DPAPASD                             
*MPEN     23Nov18 DSRD-20852 Fix for limlist security check                     
*MPEN 123 10Dec18 DSRD-20995 Fix for above                                      
*NSHE 124 15Nov18 DSRD-20736 Add PID to search results                          
*NSHE 125 18Feb19 DSRD-21215 Amend date range for approver list view            
*MPEN     05Mar19 DSRD-21474 Handle product office override on search           
*MPEN 126 27Mar19 DSRD-22100 Fix for cal error on awaiting me                   
*MPEN 127 24Jun19 DSRD-22535 Working with zero hour t/s from list               
*MPEN     19Jul19 DSRD-22961 Don't check client code if no limlist              
*MPEN     14Aug19 DSRD-23224 Fix for timesheet search overdue/in prog           
*NSHE     19Jul19 DSRD-23252 Add email to the search response for API           
*MPEN 128 03Sep19 DSRD-23747 Fix for zero hour t/s                              
*NRAK 129 12Sep19 DSRD-23917 fix to optimisation (calendars not found)          
*NSHE     04Oct19 DSRD-23640 Get staff of staff timesheets                      
*NSHE     18Oct19 DSRD-24254 Add line manager email to search results           
*MPEN     29Oct19 DSRD-24181 Return timeoff/widget/api application              
*VGUP 130 19DEC19 DSRD-24773 Fixed the list timesheet issue                     
*NSHE 131 06MAR19 DSRD-25715 fix dictionary issue when offline                  
*NSHE 132 20MAR19 DSRD-25851 New approver time search for home                  
*NRAK 133 20MAY20 DSRD-26420 ignore bad location dates                          
*MPEN 134 16JUN20 DSRD-26127 Fix for wrong error message returned               
*MPEN 135 03SEP20 DSRD-26699 Line manager as search parm                        
*NSHE     17SEP20 DSRD-27547 split location bug                                 
*YNGX 136 23Sep20 DSRD-27594 EU Restrict access t/s based on status             
*MPEN     13Oct20 DSRD-27946 Enable line manager and person search              
*                                                                               
* US Levels                                                                     
* ---------                                                                     
* RGUP 001 28Sep07 All UK Levels up to 18                                       
* JSHA 002 28Nov07 US/UK Merge All UK Levels from 19 to 26                      
* JSHA 004 18Mar08 US/UK Merge All UK Levels from 30 to 34                      
* RGUP 005 31Mar08 Validate office in TLIOMSD/TLGETAD routines                  
* JSHA 006 21Jul08 Fix TDTPPERS length in BLDATSR Routine                       
* JSHA 007 12Aug08 Fix TAPMODSP length in TLGETAS Routine                       
* JSHA 008 25Sep08 Change Dept/Subdept to handle 3 byte dept                    
* JSHA     25Sep08 US/UK Merge up to level UK level 44                          
* JSHA 013 12Dec08 US/UK Merge                                                  
* JSHA 014 16Dec08 Fix bug with profile lookup for lowest level                 
* JSHA 015 27Jan09 Limit Hour search by limlst and approver rec                 
* JSHA 018 04Feb09 US/UK Merge for level 49                                     
* JSHA 019 29Jun09 US/UK Merge for level 50 - 51                                
*                                                                               
***********************************************************************         
* Comments                                                                      
* --------                                                                      
* This contains a timesheet list download and two timeline downloads            
* where one is for a particular timesheet, the other one for time-              
* lines filtered across several timesheets.                                     
*                                                                               
* Timesheet TSAR records are colected in TSRWSSVR, TSRMINB1 used for            
* I/O minimisation (calendar records)                                           
*                                                                               
* CSVKEY1 IS RESERVED FOR TLGETAD/TLGETAS/TLIOMSD TIMESHEET READ SEQ            
*         DO NOT USE IN ANY MODULE USED IN LIST/SEARCH/SUMMARY!                 
***********************************************************************         
SVRDEF   CSECT                                                                  
*&&UK                                                                           
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,           x        
               FILES2=FILES2,                                          x        
               SLOWLIST=SLOWS,FACS=FACS,WORKERKEY=ACBO,ABENDLIST=FAILS,x        
               SYSPHASE=X'0624',SYSTEM=ACCSYSQ,                        x        
               SYSTEM2=MEDSYSQ,                                        x        
               APPEND=Y,                                               x        
               SERVERTYPE=TSTACBO,SEGMENT=Y,LOADFACSOFF=Y,             x        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#TWA,TWAD)                  
*&&                                                                             
*                                                                               
*&&US                                                                           
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,           x        
               SLOWLIST=SLOWS,FACS=FACS,WORKERKEY=ACBO,ABENDLIST=FAILS,x        
               SYSPHASE=X'0624',SYSTEM=ACCSYSQ,                        x        
               APPEND=Y,                                               x        
               SERVERTYPE=TSTACBO,SEGMENT=Y,LOADFACSOFF=Y,             x        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#TWA,TWAD)                  
*&&                                                                             
                                                                                
SLOWS    DC    C':'                                                             
FAILS    DC    C':'                                                             
                                                                                
         EJECT                                                                  
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO12**,R7,R6,CLEAR=YES,RR=RE                                 
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R4,LP_ARUNP         R4=A(RUNPARMS)                               
         USING RUNPARMD,R4                                                      
         XR    R3,R3                                                            
         ICM   R3,7,RUNPARUN                                                    
         USING RUNFACSD,R3         R3=A(RUNFACS)                                
                                                                                
         LHI   R0,LVALUESL                                                      
         CHI   R0,OVALUESL                                                      
         BE    *+6                                                              
         DC    H'0'                LITERALS OUT OF STEP                         
                                                                                
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING SAVED,R8            R8=A(SAVE W/S)                               
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BNZ   INIT02                                                           
         L     R9,LP_ABLK1         ONLINE - ROOT PROVIDES WORKD/SAVED           
         ICM   R8,15,RSVRSAVE                                                   
         B     INIT03                                                           
                                                                                
INIT02   L     R9,RSVRSAVE                                                      
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)                                                     
INIT03   ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         LR    RC,R8                                                            
         AHI   RC,4096                                                          
         USING SAVED+4096,RC       RC=A(2ND 4K OF W/S)                          
         MVC   ATWA,LP_ATWA                                                     
                                                                                
INIT04   MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK/RUNNER CALLING MODE           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         MVC   ARCPRINT,RCPRINT                                                 
         MVC   ABUFFRIN,RBUFFRIN                                                
         MVC   APRINTER,RPRINTER                                                
         L     RA,ATWA             RA=A(ON/OFFLINE TWA)                         
         USING TWAD,RA                                                          
         MVI   TWAMODE,0                                                        
         ST    R5,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         EJECT                                                                  
         L     R4,LP_ARUNP         R1=A(RUNPARMS)                               
         USING RUNPARMD,R4                                                      
         CLI   RUNPMODE,RRUNSTRQ   FIRST FOR RUN                                
         BE    RUNSTR                                                           
         CLI   RUNPMODE,RPRCWRKQ   PROCESS WORK                                 
         BE    PRCWRK                                                           
         CLI   RUNPMODE,RRUNREQQ   RUN REQUEST                                  
         BE    RUNREQ                                                           
         J     EXITY                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
* INITIALISE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNSTR04            NO                                           
                                                                                
         L     RF,RCOMFACS         YES - LOAD FACILITIES OVERLAYS               
         ST    RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,(1,0),0,0                                              
         MVC   AROUT1,0(R1)                                                     
         GOTOR (RF),DMCB,(2,0),0,0                                              
         MVC   AROUT2,0(R1)                                                     
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
                                                                                
RUNSTR04 LA    R0,SAVED                                                         
         ST    R0,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)              BLOCK #2         
         OI    LP_AIND1,LP_AICOM   COMMA OK FOR LIST DELIMITERS                 
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   LA    R0,SAVEVAR                                                       
         LA    R1,SAVELN1Q                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   OVALUES(OVALUESL),LVALUES                                        
         L     RF,=A(BRATAB)                                                    
         A     RF,SRVRRELO                                                      
         ST    RF,ABRATAB                                                       
                                                                                
         LA    R0,RQ_DATA          CLEAR REQUEST VALUES                         
         LA    R1,RQMAXQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    PRCWRK02                                                         
                                                                                
         LAY   R0,IOAREA1          Clear I/O areas                              
         LHI   R1,IOAREASL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
PRCWRK02 MVC   AGAPAREA,ACOLIST  NB. now using COLIST+COLTAB+GOBLOCKA+          
*                                           JOBLOCKA+FREESTOR+GENAEXTN          
         L     RE,AGAPAREA                                                      
         MVI   0(RE),GAPTEOT                                                    
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   MVI   GIND2,GI2ETIM                                                    
                                                                                
         MVC   MAP,LP_QMAPN        SAVE MAP EQUATE IN STORAGE                   
                                                                                
         ICM   RF,15,AMASTC        SET TRACE OPTION IF OFFLINE                  
         BZ    *+10                                                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNRQ10                                                          
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,CTFILE,(4,0),0                               
                                                                                
RUNRQ10  GOTOR (#CPYINI,ACPYINI)   (RE)INITIALISE COMPANY VALUES                
         LA    RE,DDSTRNG                                                       
         ICM   RF,B'1111',0(RE)                                                 
         ICM   RF,B'0001',CULANG                                                
         GOTO1 VDICTATE,DMCB,(RF),DCDICTL,DSDICTL                               
         GOTOR INIBUF                                                           
*&&UK                                                                           
         CLI   CUTSYS,X'73'                                                     
         BNE   RUNRQ12                                                          
         MVC   TEMP(3),=X'B40101'  Set this date for                            
         CLC   CUAALF,=C'BA'       QAROD*, QFROD*, QMROD*                       
         JE    RUNRQ12                                                          
         CLC   CUAALF,=C'BB'                                                    
         JE    RUNRQ12                                                          
         CLC   CUAALF,=C'BC'                                                    
         JE    RUNRQ12                                                          
         CLC   CUAALF,=C'AD'                                                    
         JE    RUNRQ12                                                          
         CLC   CUAALF,=C'AA'                                                    
         JE    RUNRQ12                                                          
         CLC   CUAALF,=C'AB'                                                    
         JE    RUNRQ12                                                          
         CLC   CUAALF,=C'AC'                                                    
         JE    RUNRQ12                                                          
         CLC   CUAALF,=C'CA'                                                    
         JE    RUNRQ12                                                          
         CLC   CUAALF,=C'CB'                                                    
         JE    RUNRQ12                                                          
         CLC   CUAALF,=C'CC'                                                    
         JE    RUNRQ12                                                          
         CLC   CUAALF,=C'A1'                                                    
         JE    RUNRQ12                                                          
         CLC   CUAALF,=C'Y9'                                                    
         JE    RUNRQ12                                                          
         MVC   TEMP(3),=X'B60101'  Set this date for                            
         CLC   CUAALF,=C'16'       German Aura logon                            
         JE    RUNRQ12                                                          
         MVC   TEMP(3),=X'A60701'                                               
RUNRQ11  GOTOR VDATCON,DMCB,(1,TEMP),(3,TODAYB)                                 
         B     RUNRQ14                                                          
*&&                                                                             
RUNRQ12  GOTOR VDATCON,DMCB,(5,0),(3,TODAYB)                                    
RUNRQ14  GOTOR (RF),(R1),,(2,TODAYC)                                            
         GOTOR (RF),(R1),,(1,TODAYP)                                            
         GOTOR (RF),(R1),,(0,TODAYF)                                            
                                                                                
         OC    ONERL1L,ONERL1L     1R levels found?                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         XR    RF,RF                                                            
         ICM   RF,3,LP_QMAPN                                                    
         CHI   RF,A#TSTL                 * TIME SEARCH                          
         BE    TIMSRCH                                                          
         CHI   RF,A#TIML                 * TIMESHEET LIST                       
         BE    TIMLIS                                                           
         CHI   RF,A#TSUM                 * TIME SUMMARY VIEW                    
         BE    TIMSUM                                                           
         CHI   RF,A#TSAT                 * TIMESHEET AUDIT TRAIL                
         BE    TIMAUD                                                           
         DC    H'0'                UNKNOWN REQUEST                              
         EJECT                                                                  
*                                                                               
DDSTRNG  DC    CL2'LL',XL2'0600'                                                
*                                                                               
***********************************************************************         
* Time Search: Search TimeSheets/lines D/Load                         *         
***********************************************************************         
                                                                                
* uses AIO3 for GETACN, GETWCD                                                  
* uses AIO2 for TSWRECD reading                                                 
                                                                                
         USING LW_D,RF                                                          
TIMSRCH  DS    0H                  ** DOWNLOAD TIMELINES ETIME **               
         GOTOR TIMSRC00                                                         
         BE    TIMSRCHX                                                         
         MVC   LP_ERROR,ROUERRV                                                 
         J     XERROR                                                           
*                                                                               
TIMSRCHX J     EXITY                                                            
*                                                                               
***********************************************************************         
* Time Summary View Download(s)                                       *         
***********************************************************************         
                                                                                
* uses AIO3 for GETACN, GETWCD, GETCAL                                          
* uses AIO5 for period # and period start/end date table                        
                                                                                
TIMSUM   DS    0H                  ** DOWNLOAD TIME SUMMARY **                  
         GOTOR TIMSUM00                                                         
         BE    TIMSUMX                                                          
         J     XERROR                                                           
*                                                                               
TIMSUMX  J     EXITY                                                            
*                                                                               
***********************************************************************         
* TimeSheet Audit Trail DownLoad                                      *         
***********************************************************************         
TIMAUD   DS    0H                  ** D/LOAD TIMESHEET AUDIT TRAIL **           
         GOTOR TIMAUD00                                                         
         J     EXITY               NO                                           
*                                                                               
***********************************************************************         
* TimeSheet List Download                                             *         
***********************************************************************         
                                                                                
* uses AIO3 for GETACN, GETWCD, GETCAL                                          
* uses AIO2 for TSWRECD reading                                                 
                                                                                
TIMLIS   DS    0H                  ** DOWNLOAD LIST OF TIMESHEETS **            
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   TIML000                                                          
         TM    SCPYEL+(CPYSTATB-CPYELD),CPYSBOMT                                
         BNZ   TIML000                                                          
         MVC   LP_ERROR,=AL2(AE$MBDEN)  Access to mobile app denied             
         J     XERROR                                                           
                                                                                
TIML000  GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         XC    X#SRCIND,X#SRCIND                                                
         MVC   X#GAOV,RQ_TLGC                                                   
         OC    CCTPID,CCTPID       is current users PID known?                  
         BNZ   TIML001                                                          
                                                                                
         MVC   LP_ERROR,=AL2(AE$NCPID)                                          
         J     XERROR                                                           
                                                                                
TIML001  DS    0H                                                               
         CLC   RQ_TLED,SPACES      end date must be passed                      
         BH    TIML003                                                          
         CLI   RQ_TLTY,RQ_TLSQ     error if not status view                     
         BE    TIML002                                                          
                                                                                
         MVC   LP_ERROR,=AL2(AE$EQYTD)                                          
         J     XERROR                                                           
                                                                                
TIML002  MVC   TS#DATE,TODAYP                                                   
TIML003  MVC   TEMP2(2),CCTPID                                                  
         GOTOR (#ACCPID,AACCPID)    io1                                         
         BE    TIML004                                                          
         MVC   LP_ERROR,=AL2(AE$INPID)                                          
         J     XERROR                                                           
*                                                                               
TIML004  OC    TEMP2+10(L'TS#PRSAC),SPACES                                      
         MVC   TS#PRSAC,TEMP2+10      save accounting person code               
         XC    TS#PBLK(TS#PBLQ),TS#PBLK                                         
         MVC   TS#DATE,TODAYP                                                   
         MVC   TS#PDAT,TS#DATE     end date of period                           
         MVC   TS#PEND,TS#DATE                                                  
         MVC   TS#PEST,=X'FFFFFF'                                               
         MVC   TS#PCOD,TS#PRSAC                                                 
         GOTOR PERDTL   (data returned in TS#PBLK)    io1                       
         BE    TIML005                                                          
         MVC   LP_ERROR,FULL2                                                   
         J     XERROR                                                           
*                                                                               
TIML005  L     R0,AIO4             copy person record to IO4                    
         LA    R1,IOLENQ                                                        
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R1,TS#PCLE          retrieve office                              
         MVC   TS#1ROFF,LOCOFF-LOCELD(R1)                                       
         MVC   TS#1RDPT,LOCDEPT-LOCELD(R1)                                      
         MVC   TS#1RSDP,LOCSUB-LOCELD(R1)                                       
         L     R1,TS#PEMP                                                       
         USING EMPELD,R1                                                        
         MVC   X#HIRED,EMPHIR      save hire and termination date               
         MVC   X#TERMD,EMPTRM                                                   
         DROP  R1                                                               
                                                                                
         LA    RE,1                if 2 char office person code shorter         
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         BNZ   *+8                                                              
         LA    RE,0                                                             
         MVC   TS#1RACT(0),TS#1ROFF                                             
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         LA    RF,TS#1RACT                                                      
         AR    RF,RE                                                            
         LR    R0,RE                                                            
         XR    R1,R1                                                            
         IC    R1,ONERL2L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),TS#1RDPT                                                 
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),TS#1RSDP                                                 
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL3L                                                       
         IC    R1,ONERL4L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),TS#PRSAC                                                 
         EX    R1,*-6                                                           
         GOTOR (#CSTPRF,ACSTPRF),DMCB,TS#1RACT  get approvers profiles          
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         MVC   X#TU,COTTU                                                       
         ZAP   DUB1,CONDO                                                       
         CVB   R2,DUB1                                                          
         LNR   R2,R2                                                            
*                                                                               
         MVC   WORK(L'TODAYF),TODAYF                                            
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,(R2)                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,X#CUTDT)                              
*                                                                               
TIML006  CLI   RQ_TLTY,RQ_TLDQ     is it the date request                       
         BE    TIML028                                                          
         CLI   RQ_TLSU,RQ_TLMQ     is it the user or approver                   
         BE    TIML012             user                                         
         MVI   TS#PTYPE,TS#PTLAS                                                
         XC    FULL1,FULL1                                                      
         ZAP   DUB1,COTBV                                                       
         CVB   R2,DUB1                                                          
         LNR   R2,R2                                                            
         DROP  R3                                                               
*                                                                               
         MVC   WORK(6),TODAYF                                                   
         OI    RQ_TLST,X'40'       validate it                                  
         CLI   RQ_TLST,TS#AWAIT                                                 
         BE    TIML008                                                          
         CLI   RQ_TLST,TS#OVERD                                                 
         BE    TIML008                                                          
         CLI   RQ_TLST,TS#PROGR                                                 
         BE    TIML008                                                          
         CLI   RQ_TLST,TS#SUBMT                                                 
         BE    TIML008                                                          
         CLI   RQ_TLST,TS#REJEC                                                 
         BE    TIML008                                                          
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,(R2)                              
         B     TIML010                                                          
TIML008  GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
TIML010  GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CSDT)                              
*                                                                               
         MVC   WORK(L'TODAYF),TODAYF                                            
*        LA    R2,14                                                            
         LA    R2,9                Show 9 months ahead                          
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,(R2)                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CEDT)                              
         B     TIML030                                                          
*                                                                               
TIML012  MVI   TS#PTYPE,TS#PTLMS                                                
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         JNE   TIML018                                                          
         MVC   WORK(6),TODAYF                                                   
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CSDT)                              
*                                                                               
         MVC   WORK(L'TODAYF),TODAYF                                            
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'1'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CEDT)                              
         B     TIML030                                                          
*                                                                               
TIML018  XR    RE,RE                                                            
         ICM   RE,1,SCPYEL+CPYSFST-CPYELD                                       
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         BH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
*                                                                               
         MVC   TS#CSDT,TS#DATE                                                  
         MVC   TS#CSDT+1(1),BYTE1                                               
         MVI   TS#CSDT+2,X'01'                                                  
*                                                                               
         CLC   TS#DATE+1(1),BYTE1                                               
         BNL   TIML020                                                          
         XR    R1,R1                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,(1,TS#CSDT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CSDT)                              
*                                                                               
TIML020  GOTO1 VDATCON,DMCB,(1,TS#CSDT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'M',WORK),(X'80',WORK+6),F'11'                     
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CEDT)                              
         CLC   TS#CSDT(2),TS#DATE                                               
         BE    TIML026                                                          
         CLC   TS#CEDT(2),TS#DATE                                               
         BE    TIML024                                                          
*                                                                               
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'1'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,WORK+12)                              
         CLC   WORK+12(2),TS#DATE                                               
         BNE   TIML022                                                          
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'-2'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CSDT)                              
         B     TIML030                                                          
TIML022  GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'2'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,WORK+12)                              
         CLC   WORK+12(2),TS#DATE                                               
         BNE   TIML030                                                          
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CSDT)                              
         B     TIML030                                                          
TIML024  GOTO1 VDATCON,DMCB,(1,TS#CEDT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'1'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CEDT)                              
         B     TIML030                                                          
*                                                                               
TIML026  GOTO1 VDATCON,DMCB,(1,TS#CSDT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'-3'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CSDT)                              
         B     TIML030                                                          
*                                                                               
TIML028  MVI   TS#PTYPE,TS#PTLAD                                                
         CLI   RQ_TLSU,RQ_TLMQ     is it the user or approver                   
         BNE   *+8                                                              
         MVI   TS#PTYPE,TS#PTLMD                                                
         GOTOR VDATCON,DMCB,(0,RQ_TLED+2),(1,TS#CEDT)                           
         GOTOR VDATCON,DMCB,(0,RQ_TLSD+2),(1,TS#CSDT)                           
                                                                                
TIML030  XR    R1,R1               for dates: 2's complement required           
         GOTOR CALPRDS,0           calendar periods retrieval                   
         BE    TIML032                                                          
         MVC   LP_ERROR,FULL2                                                   
         J     XERROR                                                           
                                                                                
TIML032  GOTOR (#MTSAR,AMTSAR),DMCB,TSAINI,TSARTBUF,TSARTDL                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R4,R4                                                            
         IC    R4,X#DITMS          HAVE WE ANY PERIODS                          
         LTR   R4,R4                                                            
         BNZ   TIML034                                                          
         CLI   TS#PTYPE,TS#PTLMD   are we doing my timesheet list               
         BE    TIML830                                                          
         CLI   TS#PTYPE,TS#PTLMS                                                
         BE    TIML830             yes - don't exit with error                  
         JZ    XERROR              NO                                           
TIML034  GOTO1 VXSORT,DMCB,(X'00',AIO5),(R4),PERLENQ,PERKEYQ,0                  
         ICM   R1,7,TS#CEDT                                                     
         LNR   R1,R1                                                            
         STCM  R1,7,TS#CRED                                                     
         XR    R1,R1                                                            
         ICM   R1,7,TS#CSDT                                                     
         LNR   R1,R1                                                            
         STCM  R1,7,TS#CRSD                                                     
         CLI   RQ_TLTY,RQ_TLSQ     status validated for each module             
         BNE   TIML036             later on                                     
         CLI   RQ_TLST,C' '                                                     
         BH    TIML036                                                          
         MVC   LP_ERROR,=AL2(AE$MISTA)                                          
         J     XERROR                                                           
*                                                                               
TIML036  CLI   RQ_TLSU,RQ_TLAQ     approver view?                               
         BNE   TIML040             no - user                                    
         MVI   BYTE2,GAPLTDTE      Set get approvers by date                    
         CLI   RQ_TLTY,RQ_TLSQ     Are we listing by status                     
         BNE   TIML038             No                                           
         MVI   BYTE2,GAPLTSTA      Yes - set get approvers by status            
*                                                                               
TIML038  GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT22Q',TSARABUF),           +        
               ('GAPLAPPR',SPACES),(BYTE2,GAPLPARM),('QTIME',0)                 
         BE    TIML040                                                          
         CLI   RQ_TLGC,YESQ                                                     
         JE    EXITY                                                            
         MVC   LP_ERROR,FULL2                                                   
         J     XERROR                                                           
*                                                                               
TIML040  CLI   RQ_TLSU,RQ_TLMQ                                                  
         BE    TIML050             mine                                         
         CLI   RQ_TLST,TS#NSTRT    not started                                  
         BE    TIML800                                                          
*                                                                               
         USING X_DPRINT,RF                                                      
TIML050  L     RF,ARCPRINT                                                      
         CLI   RQ_TLSU,RQ_TLMQ                                                  
         BE    TIML200             date or status mine                          
         CLI   RQ_TLTY,RQ_TLDQ     branch for various types                     
         BE    TIML060             date view                                    
         TM    LP_FLAG,LP_FOFFL    Test offline                                 
         BZ    TIML055                                                          
         SR    R1,R1                                                            
         ICM   R1,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   R1,XPMOBILQ          app is connected                            
         JE    TIML055             Don't want stats for mobile                  
         MVC   PDESC(L'TLAPSTAT),TLAPSTAT    only print when offline            
         GOTOR APRINTER                                                         
TIML055  LA    R1,APSTATAB                                                      
         OI    X#SRCIND,X#SRCAPP                                                
         L     RE,=A(TLGETAS)                                                   
         B     TIML500                                                          
*                                                                               
TIML060  TM    LP_FLAG,LP_FOFFL    Test offline                                 
         BZ    TIML065                                                          
         SR    R1,R1                                                            
         ICM   R1,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   R1,XPMOBILQ          app is connected                            
         JE    TIML065             Don't want stats for mobile                  
         MVC   PDESC(L'TLAPPER),TLAPPER                                         
         GOTOR APRINTER                                                         
TIML065  LA    R1,APSTATAB                                                      
         OI    X#SRCIND,X#SRCAPP                                                
         L     RE,=A(TLGETAD)      date/approver                                
         B     TIML500                                                          
*                                                                               
******** Combined for 'MY TimeSheets' *********************************         
                                                                                
TIML200  TM    LP_FLAG,LP_FOFFL    Test offline                                 
         BZ    TIML220             Don't print if online                        
         SR    R1,R1                                                            
         ICM   R1,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   R1,XPMOBILQ          app is connected                            
         JE    TIML220             Don't want stats for mobile                  
         CLI   RQ_TLTY,RQ_TLDQ     branch for various types                     
         BE    TIML210             date view                                    
         MVC   PDESC(L'TLMYSTAT),TLMYSTAT                                       
         GOTOR APRINTER                                                         
         B     TIML220                                                          
                                                                                
TIML210  MVC   PDESC(L'TLMYPER),TLMYPER                                         
         GOTOR APRINTER                                                         
         DROP  RF                                                               
                                                                                
TIML220  L     RE,=A(TLIOMSD)      set IO module address                        
         LA    R1,MYSTATAB                                                      
         B     TIML500                                                          
                                                                                
MYSTATAB DC    AL1(TIMSFAPP),C'6'      fully approved                           
         DC    AL1(TIMSSUBM),C'5'      submitted                                
         DC    AL1(TIMSPAPP),C'8'      part approved                            
         DC    AL1(TIMSREJE),C'7'      rejected                                 
         DC    AL1(TAPSSAVQ),C'4'      In progress                              
         DC    AL1(TAPSSAVQ),C'3'      Overdue                                  
         DC    AL1(0),C'2'             Not started                              
         DC    AL1(FF),C' '            all                                      
         DC    X'FFFF'                                                          
         DS    0H                                                               
*                                                                               
APSTATAB DC    AL1(TAPSAPPQ),C'6'      approved                                 
         DC    AL1(TAPSSUBQ),C'5'      submitted                                
         DC    AL1(TAPSAWPQ),C'1'      await approval                           
         DC    AL1(TAPSREJQ),C'7'      rejected                                 
         DC    AL1(TAPSSAVQ),C'4'      saved                                    
         DC    AL1(TAPSSAVQ),C'3'      overdue                                  
         DC    AL1(FF),C' '            all                                      
         DC    X'FFFF'                                                          
         DS    0H                                                               
                                                                                
* Request description literals                                                  
TLMYPER  DC    C'My period list'                                                
TLMYSTAT DC    C'My status list'                                                
TLAPPER  DC    C'Approver period list'                                          
TLAPSTAT DC    C'Approver status list'                                          
***********************************************************************         
                                                                                
TIML500  A     RE,SRVRRELO         save 'get routine' address                   
         ST    RE,X#AGADR                                                       
         ST    R1,X#ATSTT          and status filter table address              
         OI    RQ_TLST,X'40'       validate it                                  
         XC    X#ITEMS,X#ITEMS                                                  
*                                                                               
TIML510  CLC   0(2,R1),=X'FFFF'    end of table?                                
         BE    TIML520                                                          
         CLC   RQ_TLST,1(R1)                                                    
         BE    TIML530                                                          
         AHI   R1,2                                                             
         B     TIML510                                                          
                                                                                
TIML520  MVC   LP_ERROR,=AL2(AE$INFLT)                                          
         J     XERROR                                                           
                                                                                
TIML530  MVC   X#FLTST,0(R1)       save hex value of status filter              
                                                                                
         XC    IOKEY,IOKEY         set to 'first time'                          
         XC    CSVKEY1,CSVKEY1     set to 'first time'                          
         XC    X#SVSEQ,X#SVSEQ                                                  
*                                                                               
TIML600  L     RF,X#AGADR          routine address                              
         BASR  RE,RF                                                            
         BNE   TIML700             end of IO reached                            
*                                                                               
         CLI   X#TU,C'Y'                                                        
         BE    TIML610                                                          
         CLI   TS#PTYPE,TS#PTLMD   don't need darec for approvers               
         BE    TIML610                                                          
         CLI   TS#PTYPE,TS#PTLMS                                                
         BNE   TIML620                                                          
TIML610  MVC   IODA,TS#RECDA       read DA record                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR PROMTSR             process T/S record into table                
         B     TIML600                                                          
                                                                                
TIML620  GOTOR PROPTSR             process T/S passive into table               
         B     TIML600                                                          
                                                                                
TIML700  CLI   RQ_TLTY,RQ_TLDQ     are we date type request                     
         BE    TIML810                                                          
TIML800  CLI   RQ_TLTY,RQ_TLSQ                                                  
         BE    TIML802                                                          
         MVC   LP_ERROR,=AL2(AE$NAWTT)                                          
         J     XERROR                                                           
*                                                                               
TIML802  CLI   RQ_TLST,TS#OVERD    overdue and not started?                     
         BE    TIML810                                                          
         CLI   RQ_TLST,TS#NSTRT                                                 
         BNE   TIML830                                                          
*                                                                               
TIML810  CLI   RQ_TLSU,RQ_TLMQ     Mine?                                        
         BE    TIML820                                                          
         CLI   RQ_TLSU,RQ_TLAQ     Approver?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR BLDATSR             BUILD TSAR RECS OF MISSING TIME RECS         
         BE    TIML830                                                          
         MVC   LP_ERROR,FULL2                                                   
         J     XERROR                                                           
*                                                                               
TIML820  GOTOR CHKSTAB             READ PERIOD TABLE AND BUILD TSAR REC         
         BE    TIML830                                                          
         MVC   LP_ERROR,FULL2                                                   
         J     XERROR                                                           
                                                                                
TIML830  GOTOR OFDPNAME            get office dept sub-dept names               
         GOTOR OUTPTSR             put out TimeSheet records                    
         J     EXITY                                                            
                                                                                
         EJECT                                                                  
**********************************************************************          
* GENERAL EXIT AND DECLARATIONS                                      *          
**********************************************************************          
                                                                                
EXITH    LHI   RE,2                                                             
         J     EXITCC                                                           
EXITL    DS    0H                                                               
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* General ERROR Exit                                                  *         
***********************************************************************         
                                                                                
XERROR   DS    0H                                                               
         MVI   LP_RMODE,LP_RERRR                                                
         MVI   LP_EMSYS,6                                                       
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
*                                                                               
LVALUES  DS    0D                                                               
         DC    CL8'DMKEY'                                                       
         DC    CL8'ACCDIR'                                                      
         DC    CL8'ACCMST'                                                      
         DC    CL8'ACCARC'                                                      
         DC    CL8'CTFILE'                                                      
         DC    X'41E0'                                                          
         DC    S(0)                                                             
         DC    PL1'0'                                                           
         DC    16C'*'                                                           
         DC    16X'FF'                                                          
         DC    PL5'1000000'                                                     
         DC    PL5'-1000000'                                                    
         DC    CL2'1N'                                                          
         DC    CL2'1R'                                                          
         DC    X'06FA'                                                          
LVALUESL EQU   *-LVALUES                                                        
*                                                                               
WORKLEN  DC    AL2(WORKL)                                                       
*                                                                               
DCDICTL  DS    0X                                                               
         DCDDL AC#ACC,L'MCS@ACC                                                 
         DCDDL AC#RSTYT,L'MCS@RSTY                                              
         DCDDL AC#HOURS,L'MCS@HRS                                               
         DCDDL AC#WC,L'MCS@WC                                                   
         DCDDL AC#INTRF,L'MCS@INTR                                              
         DCDDL AC#ORDER,L'MCS@ORDR                                              
         DCDDL AC#ITMM,L'MCS@ITMM                                               
         DCDDL AC#ITMT,L'MCS@ITMT                                               
         DCDDL AC#ITMP,L'MCS@ITMP                                               
         DCDDL AC#TEXT,L'MCS@TEXT                                               
         DCDDL AC#ITMC,L'MCS@ITMC                                               
         DCDDL AC#NRTV,L'MCS@NRTV                                               
         DCDDL AC#EST,L'MCS@EST                                                 
         DCDDL AC#MOBL,L'MCS@MOBL                                               
DCDICTLX DC    X'FF'                                                            
*                                                                               
BRATAB   DS    0X                                                               
         DC    20XL3'00'                                                        
BRATABX  DC    X'FF'                                                            
*                                                                               
RECTIM   EQU   01                                                               
ACTMAN   EQU   06                  Manager search                               
GAPSMAX  EQU   5000                Max number of entries                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOCAL ROUTINES                                                      *         
***********************************************************************         
***********************************************************************         
* PROCESS TIME AUDIT REQUEST                                          *         
* USE AIO4 FOR TIME AUDIT RECORD                                      *         
***********************************************************************         
         SPACE 1                                                                
TIMAUD00 NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*TIMAUD*'                                                      
         GOTOR VDATCON,DMCB,(0,RQ_ATDAT),(1,TS#PEDT)                            
         XC    TS#DATE,TS#DATE                                                  
         CLC   RQ_ALDAT,SPACES                                                  
         BNH   TIMA002                                                          
         CLC   RQ_ATDAT,RQ_ALDAT                                                
         BNH   TIMA002          location date after period date                 
*                                   makes no sense.                             
         GOTOR VDATCON,DMCB,(0,RQ_ALDAT),(1,TS#PEDT)                            
                                                                                
TIMA002  MVC   TEMP2(8),RQ_ATPER    (acc person code)                           
         GOTOR (#ACCPFP,AACCPFP)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TS#PSPID,TEMP2+8                                                 
         SR    R1,R1                                                            
         ICM   R1,7,TS#PEDT                                                     
         LNR   R1,R1                                                            
         STCM  R1,7,TS#DATE                                                     
                                                                                
         USING AUDRECD,R4                                                       
         LA    R4,IOKEY                                                         
         XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVI   AUDKAUDT,AUDKTIME                                                
         MVC   AUDKCPY,CUXCPY                                                   
         MVC   AUDKPEDT,TS#DATE                                                 
         MVC   AUDKPIDB,TS#PSPID                                                
         MVC   CSVKEY1,AUDKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         B     TIMA006                                                          
TIMA004  MVC   IOKEY,CSVKEY2                                                    
         LA    R4,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
TIMA006  CLC   CSVKEY1(AUDKSEQ-AUDKEY),AUDKEY DO ANY RECORDS EXIST              
         JNE   TIMAUDY             NO                                           
*                                                                               
TIMA008  MVC   CSVKEY2,AUDKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO4                                                          
         USING AUDRECD,R4                                                       
TIMA010  LA    R2,AUDRFST                                                       
         USING STCELD,R2                                                        
TIMA012  CLI   STCEL,0                                                          
         BE    TIMA004                                                          
         CLI   STCEL,STCELQ                                                     
         BE    TIMA016                                                          
         CLI   STCEL,TIMELQ                                                     
         BE    TIMA078                                                          
TIMA014  SR    RE,RE                                                            
         IC    RE,STCLN                                                         
         AR    R2,RE                                                            
         B     TIMA012                                                          
*                                                                               
TIMA016  CLI   STCIND,STCITIME                                                  
         BNE   TIMA014                                                          
         GOTOR CLRATVL                                                          
         MVC   AT_TYPE,STCTTYP                                                  
         OI    AT_TYPE,X'F0'                                                    
         CLI   STCTTYP,X'0A'                                                    
         BNE   *+8                                                              
         MVI   AT_TYPE,C'A'                                                     
         CLI   STCTTYP,X'0B'                                                    
         BNE   *+8                                                              
         MVI   AT_TYPE,C'B'                                                     
         CLI   STCTTYP,X'0C'                                                    
         BNE   *+8                                                              
         MVI   AT_TYPE,C'C'                                                     
         CLI   STCTTYP,X'0D'                                                    
         BNE   *+8                                                              
         MVI   AT_TYPE,C'D'                                                     
         CLI   STCTTYP,X'0E'                                                    
         BNE   *+8                                                              
         MVI   AT_TYPE,C'E'                                                     
                                                                                
         MVC   AT_DATE,STCTDTE                                                  
         UNPK  DUB2,STCTTIM                                                     
         OI    DUB2+7,C'0'                                                      
         MVC   AT_TIME,DUB2+2                                                   
         CURED (B2,STCTROW),(4,AT_ROW),0,ZERO=YES,ALIGN=LEFT                    
         MVC   TEMP2(L'STCTPID),STCTPID                                         
         GOTOR (#GETPID,AGETPID)    USES IO1                                    
         BE    TIMA018                                                          
         MVI   AT_PID,C'<'        pass <user> if no name found                  
         MVI   AT_PID+7,C'>'                                                    
         XOUT  STCTPID,AT_PID+1,2                                               
         B     *+10                                                             
TIMA018  MVC   AT_PID,TEMP2                                                     
         GOTOR (#GETPIN,AGETPIN)    USES IO1                                    
         BNE   *+16                                                             
         MVC   AT_PRFN,TEMP2                                                    
         MVC   AT_PRMN,TEMP2+32                                                 
         MVC   AT_PRLN,WORK2                                                    
         CLI   STCLN,STCLN3Q                                                    
         BL    TIMA026                                                          
         MVC   TEMP2(2),STCTUSR                                                 
         MVC   AT_USER,SPACES                                                   
         GOTOR (#GETUSR,AGETUSR)    USES IO1                                    
         BE    TIMA020                                                          
         MVI   AT_USER,C'<'        pass <user> if no name found                 
         MVI   AT_USER+5,C'>'                                                   
         XOUT  STCTUSR,AT_USER+1,2                                              
         B     TIMA026                                                          
*                                                                               
TIMA020  MVC   AT_USER(10),TEMP2                                                
         NI    STCTSTA3,X'FF'-STCTSMOA Remove MOA change flag                   
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Aura is connected              
         CHI   RF,XPRODIKQ                                                      
         JE    TIMA026                                                          
         CLI   STCTSTA3,STCTMOBL   Was the time entered via mobile site         
         JNE   TIMA026                                                          
         LA    RE,AT_USER+9        Yes - append mobile to user id               
         LHI   RF,9                                                             
TIMA022  CLI   0(RE),C' '                                                       
         JH    TIMA024                                                          
         SHI   RE,1                                                             
         JCT   RF,TIMA022                                                       
TIMA024  AHI   RE,2                                                             
         MVC   0(L'MCS@MOBL,RE),MCS@MOBL                                        
                                                                                
TIMA026  CLI   STCTSTA3,STCTMOBL   Was the time entered via mobile site         
         JNE   *+8                                                              
         MVI   AT_APPL,AT_AMOBQ                                                 
         CLI   STCTSTA3,STCTAURA   Was the time entered via Aura                
         JNE   *+8                                                              
         MVI   AT_APPL,AT_AAURQ                                                 
         CLI   STCTSTA3,STCTSPEC   Was the time entered via Spectra             
         JNE   *+8                                                              
         MVI   AT_APPL,AT_ASPEQ                                                 
         CLI   STCTSTA3,STCTTIMO   Timeoff?                                     
         JNE   *+8                                                              
         MVI   AT_APPL,AT_ATIMO                                                 
         CLI   STCTSTA3,STCTWIDG   Widget?                                      
         JNE   *+8                                                              
         MVI   AT_APPL,AT_AWIDG                                                 
         CLI   STCTSTA3,STCTAAPI   API?                                         
         JNE   *+8                                                              
         MVI   AT_APPL,AT_AAPI                                                  
         CLI   STCTTYP,STCTTSAD                                                 
         BE    TIMA042                                                          
         MVC   TS#STAT,STCDTFR                                                  
         GOTOR SETSSTA,2                                                        
         MVC   AT_STFR,X#STAT                                                   
         MVC   TS#STAT,STCDTTO                                                  
         GOTOR SETSSTA,2                                                        
         MVC   AT_STTO,X#STAT                                                   
         LA    R1,COMNTTB                                                       
         MVC   AT_CMNTS,SPACES                                                  
         LA    R3,AT_CMNTS                                                      
         USING COMNTTBD,R1                                                      
TIMA028  CLI   0(R1),X'FF'                                                      
         BE    TIMA042                                                          
         OC    COMNTSTA,COMNTSTA                                                
         BZ    TIMA030                                                          
         MVC   BYTE1,COMNTSTA                                                   
         NC    BYTE1,STCTSTAT                                                   
         BZ    TIMA040                                                          
         B     TIMA034                                                          
*                                                                               
TIMA030  CLI   STCTTYP,STCTMRAM                                                 
         BE    TIMA032                                                          
         CLI   STCTTYP,STCTMRDL                                                 
         BE    TIMA032                                                          
         CLI   STCTTYP,STCTMRAD                                                 
         BNE   TIMA040                                                          
TIMA032  MVC   BYTE1,COMNTST2                                                   
         NC    BYTE1,STCTSTA2                                                   
         BZ    TIMA040                                                          
TIMA034  MVC   LAREADDR,COMNTDIC                                                
         EX    0,LARE                                                           
         MVC   0(L'MCS@ACC,R3),0(RE)                                            
         LA    R3,L'MCS@ACC(R3)                                                 
TIMA036  CLI   0(R3),C' '                                                       
         BH    TIMA038                                                          
         SHI   R3,1                                                             
         B     TIMA036                                                          
TIMA038  MVI   1(R3),C','                                                       
         LA    R3,3(R3)                                                         
TIMA040  LA    R1,COMNTTBL(R1)                                                  
         B     TIMA028                                                          
*                                                                               
TIMA042  LA    R3,AT_CMNTS                                                      
         LA    R3,L'AT_CMNTS-1(R3)                                              
         LHI   RF,L'AT_CMNTS-1                                                  
TIMA044  CLI   0(R3),C' '                                                       
         BH    TIMA046                                                          
         SHI   R3,1                                                             
         BCT   RF,TIMA044                                                       
         B     TIMA048                                                          
*                                                                               
TIMA046  CLI   0(R3),C','                                                       
         BNE   TIMA048                                                          
         MVI   0(R3),C' '                                                       
                                                                                
TIMA048  CLI   STCTTYP,STCTMRAM                                                 
         BE    TIMA062                                                          
         CLI   STCTTYP,STCTMRDL                                                 
         BE    TIMA062                                                          
         CLI   STCTTYP,STCTMRAD                                                 
         BNE   TIMA068             Time based changes                           
TIMA062  MVC   AT_MROW,STCTMROW                                                 
         CLI   STCLN,STCLN4Q                                                    
         BNH   TIMA076             Old short element - nothing to do            
         MVC   AT_TPRC,STCMCPRC                                                 
         MVC   AT_TTOT,STCMCTOT                                                 
         MVC   AT_TMUL,STCMCMUL                                                 
         MVC   AT_TCDE,STCMCCOD                                                 
         CLI   STCLN,STCLNMQ                                                    
         BNH   TIMA076                                                          
         LLC   RF,STCLN                                                         
         SHI   RF,STCLNMQ+1                                                     
         MVC   AT_TNAR(0),STCMCTXT                                              
         EX    RF,*-6                                                           
         B     TIMA076                                                          
                                                                                
TIMA068  CLI   STCTTYP,STCTRWAM                                                 
         BE    TIMA070                                                          
         CLI   STCTTYP,STCTRWDL                                                 
         BE    TIMA070                                                          
         CLI   STCTTYP,STCTRWAD                                                 
         BNE   TIMA076                                                          
TIMA070  CLI   STCLN,STCLN3Q                                                    
         BNH   TIMA076             Old short element - nothing to do            
         MVC   AT_TACC,STCTCULA                                                 
         MVI   AT_TTY,C'N'                                                      
         CLI   STCTCTTY,TIMTCB                                                  
         BNE   *+8                                                              
         MVI   AT_TTY,C'B'                                                      
         CLI   STCTCTTY,TIMTCR                                                  
         BNE   *+8                                                              
         MVI   AT_TTY,C'R'                                                      
         MVC   AT_TWC,STCTCTSK                                                  
         MVC   AT_THRS,STCTCHRS                                                 
         MVC   AT_TORD,STCTCORD                                                 
         MVC   AT_TINT,STCTCINT                                                 
         MVC   AT_TEST,STCTEST#                                                 
         CLI   STCLN,STCLNTQ                                                    
         BNH   TIMA076                                                          
         LLC   RF,STCLN                                                         
         SHI   RF,STCLNTQ+1                                                     
         MVC   AT_TNAR(0),STCTCNAR                                              
         EX    RF,*-6                                                           
                                                                                
TIMA076  GOTOR LP_APUTO,LP_D       send and clear data                          
         B     TIMA014                                                          
                                                                                
         USING TIMELD,R2                                                        
TIMA078  CLI   TIMETYP,TIMERJAP                                                 
         BNE   TIMA098                                                          
         GOTOR CLRATVL                                                          
         TM    TIMRRSTA,TIMRRAPP                                                
         BZ    TIMA080                                                          
         MVI   AT_TYPE,AT_TAPPR                                                 
TIMA080  TM    TIMRRSTA,TIMRRREJ                                                
         BZ    TIMA082                                                          
         MVI   AT_TYPE,AT_TREJT                                                 
TIMA082  TM    TIMRRSTA,TIMRRADJ                                                
         BZ    TIMA084                                                          
         MVI   AT_TYPE,AT_TRWAD                                                 
TIMA084  MVC   AT_DATE,TIMRRDTE                                                 
         UNPK  DUB2,TIMRRTME                                                    
         OI    DUB2+7,C'0'                                                      
         MVC   AT_TIME,DUB2+2                                                   
         CURED (B2,TIMRIDNO),(4,AT_ROW),0,ZERO=YES,ALIGN=LEFT                   
         MVC   TEMP2(L'TIMRRPID),TIMRRPID                                       
         GOTOR (#GETPID,AGETPID)    USES IO1                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AT_PID,TEMP2                                                     
         GOTOR (#GETPIN,AGETPIN)    USES IO1                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AT_PRFN,TEMP2                                                    
         MVC   AT_PRMN,TEMP2+32                                                 
         MVC   AT_PRLN,WORK2                                                    
         MVC   AT_USER,SPACES                                                   
         MVC   TEMP2(2),TIMRUSER                                                
         GOTOR (#GETUSR,AGETUSR)    USES IO1                                    
         BE    TIMA088                                                          
         MVI   AT_USER,C'<'        pass <user> if no name found                 
         MVI   AT_USER+5,C'>'                                                   
         XOUT  TIMRUSER,AT_USER+1,2                                             
         B     TIMA094                                                          
*                                                                               
TIMA088  MVC   AT_USER(10),TEMP2                                                
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Aura is connected              
         CHI   RF,XPRODIKQ                                                      
         JE    TIMA094                                                          
         TM    TIMRRSTA,TIMRMOBL   Was the time entered via mobile site         
         JZ    TIMA094                                                          
         LA    RE,AT_USER+9        Yes - append mobile to user id               
         LHI   RF,9                                                             
TIMA090  CLI   0(RE),C' '                                                       
         JH    TIMA092                                                          
         SHI   RE,1                                                             
         JCT   RF,TIMA090                                                       
TIMA092  AHI   RE,2                                                             
         MVC   0(L'MCS@MOBL,RE),MCS@MOBL                                        
                                                                                
TIMA094  TM    TIMRRSTA,TIMRMOBL   Was the time entered via mobile site         
         JZ    *+8                                                              
         MVI   AT_APPL,AT_AMOBQ                                                 
         TM    TIMRRSTA,TIMRAURA   Was the time entered via Aura                
         JZ    *+8                                                              
         MVI   AT_APPL,AT_AAURQ                                                 
         TM    TIMRRSTA,TIMRSPEC   Was the time entered via Spectra             
         JZ    *+8                                                              
         MVI   AT_APPL,AT_ASPEQ                                                 
         MVC   AT_CMNTS,SPACES                                                  
         SR    RE,RE                                                            
         IC    RE,TIMLN                                                         
         CHI   RE,TIMRLN1Q                                                      
         BNH   TIMA096                                                          
         SHI   RE,TIMRLN1Q+1                                                    
         MVC   AT_CMNTS(0),TIMREJAP                                             
         EX    RE,*-6                                                           
*                                                                               
TIMA096  GOTOR LP_APUTO,LP_D       send and clear data                          
         B     TIMA014                                                          
*                                                                               
TIMA098  CLI   TIMETYP,TIMEARIN                                                 
         BNE   TIMA014                                                          
         GOTOR CLRATVL                                                          
         TM    TIMASTAT,TIMASAPR                                                
         BZ    TIMA100                                                          
         MVI   AT_TYPE,AT_TAPPR                                                 
         B     TIMA102                                                          
TIMA100  TM    TIMASTAT,TIMASREJ                                                
         B     TIMA014                                                          
         MVI   AT_TYPE,AT_TREJT                                                 
TIMA102  MVC   AT_DATE,TIMADATE                                                 
         UNPK  DUB2,TIMATIME                                                    
         OI    DUB2+7,C'0'                                                      
         MVC   AT_TIME,DUB2+2                                                   
         CURED (B2,TIMAIDNO),(4,AT_ROW),0,ZERO=YES,ALIGN=LEFT                   
         MVC   TEMP2(L'TIMAPDAC),TIMAPDAC                                       
         GOTOR (#GETPID,AGETPID)    USES IO1                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AT_PID,TEMP2                                                     
         GOTOR (#GETPIN,AGETPIN)    USES IO1                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AT_PRFN,TEMP2                                                    
         MVC   AT_PRMN,TEMP2+32                                                 
         MVC   AT_PRLN,WORK2                                                    
         MVC   AT_CMNTS,SPACES                                                  
         MVC   AT_USER,SPACES                                                   
         MVC   TEMP2(2),TIMAUSER                                                
         GOTOR (#GETUSR,AGETUSR)    USES IO1                                    
         BE    TIMA104                                                          
         MVI   AT_USER,C'<'        pass <user> if no name found                 
         MVI   AT_USER+5,C'>'                                                   
         XOUT  TIMAUSER,AT_USER+1,2                                             
         B     TIMA110                                                          
*                                                                               
TIMA104  MVC   AT_USER(10),TEMP2                                                
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Aura is connected              
         CHI   RF,XPRODIKQ                                                      
         JE    TIMA110                                                          
         TM    TIMASTAT,TIMAMOBL   Was the time entered via mobile site         
         JZ    TIMA110                                                          
         LA    RE,AT_USER+9        Yes - append mobile to user id               
         LHI   RF,9                                                             
TIMA106  CLI   0(RE),C' '                                                       
         JH    TIMA108                                                          
         SHI   RE,1                                                             
         JCT   RF,TIMA106                                                       
TIMA108  AHI   RE,2                                                             
         MVC   0(L'MCS@MOBL,RE),MCS@MOBL                                        
                                                                                
*                                                                               
TIMA110  TM    TIMASTAT,TIMAMOBL   Was the time entered via mobile site         
         JZ    *+8                                                              
         MVI   AT_APPL,AT_AMOBQ                                                 
         TM    TIMASTAT,TIMAAURA   Was the time entered via Aura                
         JZ    *+8                                                              
         MVI   AT_APPL,AT_AAURQ                                                 
         TM    TIMASTAT,TIMASTMP   Was the time entered via Spectra             
         JZ    *+8                                                              
         MVI   AT_APPL,AT_ASPEQ                                                 
         GOTOR LP_APUTO,LP_D       send and clear data                          
         B     TIMA014                                                          
*                                                                               
TIMAUDY  CR    RB,RB                                                            
         J     *+6                                                              
TIMAUDN  LTR   RB,RB                                                            
         XIT1                                                                   
         DROP  R2,R4                                                            
*                                                                               
COMNTTB  DS    0C                                                               
         DC    S(MCS@ACC),AL1(STCTACC,0)                                        
         DC    S(MCS@RSTY),AL1(STCTTTYP,0)                                      
         DC    S(MCS@HRS),AL1(STCTHRS,0)                                        
         DC    S(MCS@WC),AL1(STCTTSK,0)                                         
         DC    S(MCS@ORDR),AL1(STCTTORD,0)                                      
         DC    S(MCS@INTR),AL1(STCTTINT,0)                                      
         DC    S(MCS@ITMM),AL1(0,STCMMULT)                                      
         DC    S(MCS@ITMT),AL1(0,STCMTOT)                                       
         DC    S(MCS@ITMP),AL1(0,STCMPRCE)                                      
         DC    S(MCS@TEXT),AL1(0,STCMTEXT)                                      
         DC    S(MCS@ITMC),AL1(0,STCMITMS)                                      
         DC    S(MCS@NRTV),AL1(STCTTEXT,0)                                      
         DC    S(MCS@EST),AL1(STCTTEST,0)                                       
COMNTTBX DC    X'FF'                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* CLEAR AT_VALS ARE AND INITIALISE PACKED FIELDS                      *         
***********************************************************************         
         SPACE 1                                                                
CLRATVL  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    CL8'*CLRATVL'                                                    
         LA    RE,AT_VALS                                                       
         LA    RF,AT_LNQ                                                        
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
         ZAP   AT_THRS,PZERO                                                    
         ZAP   AT_TTOT,PZERO                                                    
         ZAP   AT_TPRC,PZERO                                                    
         ZAP   AT_TMUL,PZERO                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* TIME SEARCH ROUTINE                                                 *         
***********************************************************************         
         SPACE 1                                                                
TIMSRC00 NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*TIMSRC*'                                                    
                                                                                
         MVI   X#GAOV,NOQ                                                       
         XC    X#IND,X#IND                                                      
         XC    X#SRCIND,X#SRCIND                                                
         XC    X#ITEMS,X#ITEMS                                                  
         MVC   ANYACCNT,SPACES                                                  
         XC    GAPLCALT,GAPLCALT                                                
         XC    TS#CSDT,TS#CSDT     set start and end dates                      
         XC    TS#PEDT,TS#PEDT                                                  
         NI    X#ACCSTA,X'FF'-X#CLISR    Init CLI Search                        
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         CLC   RQ_TTSTD,SPACES                                                  
         BNH   TIMR002                                                          
         GOTOR VDATCON,DMCB,(0,RQ_TTSTD+2),(1,TS#CSDT)                          
*                                                                               
TIMR002  MVC   TS#CEDT,FFS                                                      
         CLC   RQ_TTEND,SPACES                                                  
         BNH   TIMR004                                                          
         GOTOR VDATCON,DMCB,(0,RQ_TTEND+2),(1,TS#CEDT)                          
*                                                                               
TIMR004  XR    R1,R1                                                            
         ICM   R1,7,TS#CEDT                                                     
         LNR   R1,R1                                                            
         STCM  R1,7,TS#CRED                                                     
         XR    R1,R1                                                            
         ICM   R1,7,TS#CSDT                                                     
         LNR   R1,R1                                                            
         STCM  R1,7,TS#CRSD                                                     
*                                                                               
         OC    CCTPID,CCTPID                                                    
         BNZ   TIMR006                                                          
         MVC   ROUERRV,=AL2(AE$NCPID)                                           
         B     TIMSRCN                                                          
*                                                                               
TIMR006  MVC   TEMP2(2),CCTPID                                                  
         GOTOR (#ACCPID,AACCPID)                                                
         BE    TIMR008                                                          
         MVC   ROUERRV,=AL2(AE$INPID)                                           
         B     TIMSRCN                                                          
*                                                                               
TIMR008  OC    TEMP2+10(L'TS#PRSAC),SPACES                                      
         MVC   TS#PRSAC,TEMP2+10      save accounting person code               
         XC    TS#PBLK(TS#PBLQ),TS#PBLK                                         
         MVC   TS#PDAT,TODAYP      end date of period                           
         MVC   TS#PEND,TODAYP                                                   
         MVC   TS#PEST,=X'FFFFFF'                                               
         MVC   TS#PCOD,TS#PRSAC                                                 
         GOTOR PERDTL   (data returned in TS#PBLK)                              
         BE    TIMR010                                                          
         MVC   ROUERRV,FULL2                                                    
         B     TIMSRCN                                                          
*                                                                               
TIMR010  L     R0,AIO4             copy person record to IO4                    
         LA    R1,IOLENQ                                                        
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R1,TS#PCLE          retrieve office                              
         MVC   TS#1ROFF,LOCOFF-LOCELD(R1)                                       
         MVC   TS#APOFF,LOCOFF-LOCELD(R1)                                       
         MVC   TS#1RDPT,LOCDEPT-LOCELD(R1)                                      
         MVC   TS#1RSDP,LOCSUB-LOCELD(R1)                                       
         LA    RE,1                if 2 char office person code shorter         
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         BNZ   *+8                                                              
         LA    RE,0                                                             
         MVC   TS#1RACT(0),TS#1ROFF                                             
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         LA    RF,TS#1RACT                                                      
         AR    RF,RE                                                            
         LR    R0,RE                                                            
         XR    R1,R1                                                            
         IC    R1,ONERL2L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),TS#1RDPT                                                 
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),TS#1RSDP                                                 
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL3L                                                       
         IC    R1,ONERL4L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),TS#PRSAC                                                 
         EX    R1,*-6                                                           
         GOTOR (#CSTPRF,ACSTPRF),DMCB,TS#1RACT  get approvers profiles          
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         MVC   X#TU,COTTU                                                       
         ZAP   DUB1,CONDO                                                       
         CVB   R2,DUB1                                                          
         LNR   R2,R2                                                            
*                                                                               
         CLI   RQ_TSSRC,RQ_TSTMY   my timesheets?                               
         BE    TIMR012             yes - got person code from PID               
         CLC   RQ_TTPRS,SPACES     have we got a person                         
         BNH   TIMR012             no                                           
         CLC   TS#PRSAC,RQ_TTPRS   Connected user same search person            
         BNE   TIMR011                                                          
         CLC   RQ_TTOFF,SPACES     validate Off/Dep/Sub filters                 
         BH    TIMR011                                                          
         CLC   RQ_TTDEP,SPACES                                                  
         BH    TIMR011                                                          
         CLC   RQ_TTSUB,SPACES                                                  
         BH    TIMR011                                                          
         MVI   RQ_TSSRC,RQ_TSTMY   Set as my timesheet search                   
         B     TIMR012                                                          
                                                                                
TIMR011  MVC   TS#PRSAC,SPACES                                                  
         MVC   TEMP2(8),RQ_TTPRS                                                
         GOTOR (#ACCPFP,AACCPFP)                                                
         BE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$IVPER)                                           
         B     TIMSRCN                                                          
         L     R0,AIO4             copy person record to IO4                    
         LA    R1,IOLENQ                                                        
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   TS#PSPID,TEMP2+8                                                 
         MVC   TS#PRSAC,TEMP2                                                   
*                                                                               
TIMR012  CLC   RQ_LMPID,SPACES     Any line manager pid passed?                 
         JNH   TIMR013                                                          
         MVC   TEMP2(8),RQ_LMPID                                                
         GOTOR (#GETPIN,AGETPIN)   Lookup line manager pin                      
         BE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$IVPER)                                           
         B     TIMSRCN                                                          
         MVC   TS#LMPIN,TEMP2+50                                                
*                                                                               
TIMR013  MVC   WORK(L'TODAYF),TODAYF                                            
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,(R2)                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,X#CUTDT)                              
*                                                                               
         CLI   RQ_TSSRC,RQ_TSTAP   If just my approvals can skip date           
         JE    TIMR018               validation as this is worked out           
         OC    TS#CSDT,TS#CSDT                                                  
         BNZ   TIMR014                                                          
         MVC   ROUERRV,=AL2(AE$PRDNT)   MUST ENTER START AND END DATE           
         B     TIMSRCN                                                          
TIMR014  CLC   TS#CEDT,FFS                                                      
         BNE   TIMR016                                                          
         MVC   ROUERRV,=AL2(AE$PRDNT)                                           
         B     TIMSRCN                                                          
*                                                                               
TIMR016  GOTO1 VDATCON,DMCB,(1,TS#CSDT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'2'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#DATE)                              
         CLC   TS#DATE,TS#CEDT                                                  
         BNL   TIMR018                                                          
         MVC   ROUERRV,=AL2(AE$IVPRD)  INVALID PERIOD > 2YEARS                  
         B     TIMSRCN                                                          
*                                                                               
TIMR018  GOTOR (#MTSAR,AMTSAR),DMCB,TSAINI,TSARTBUF,                   +        
               TSARTDSR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TIMR020  ZAP   X#MINHR,PHRSMIN     set minimum and maximum hours                
         CLC   RQ_TTHMI,SPACES                                                  
         BNH   TIMR022                                                          
         MVC   TEMP2(16),RQ_TTHMI                                               
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#MINHR,TEMP2+16(8)                                              
                                                                                
TIMR022  ZAP   X#MAXHR,PHRSMAX                                                  
         CLC   RQ_TTHMA,SPACES                                                  
         BNH   TIMR024                                                          
         MVC   TEMP2(16),RQ_TTHMA                                               
         GOTOR (#CONAMT,ACONAMT)                                                
         ZAP   X#MAXHR,TEMP2+16(8)                                              
                                                                                
TIMR024  TM    LP_FLAG,LP_FOFFL    Test offline                                 
         BZ    TIMR040                                                          
         SR    R1,R1                                                            
         ICM   R1,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   R1,XPMOBILQ          app is connected                            
         JE    TIMR040             Don't want stats for mobile                  
         USING X_DPRINT,RF                                                      
         L     RF,ARCPRINT                                                      
         CLI   RQ_TTYPE,RQ_THOUR   Is it hours or timesheet                     
         JE    TIMR026                                                          
         CLC   RQ_TTHMI,SPACES                                                  
         JH    *+14                                                             
         CLC   RQ_TTHMA,SPACES                                                  
         BNH   TIMR036                                                          
                                                                                
         MVC   PDESC(L'TSTSMIN),TSTSMIN                                         
         MVC   PDESC+L'TSTSMIN+1(L'RQ_TSSRC),RQ_TSSRC                           
         MVC   PDESC+L'TSTSMIN+3(L'RQ_TTHMA),RQ_TTHMA                           
         MVC   PDESC+L'TSTSMIN+4+L'RQ_TTHMA(L'RQ_TTHMI),RQ_TTHMI                
         J     TIMR038                                                          
                                                                                
TIMR026  MVC   PDESC(L'TSHRSMN),TSHRSMN                                         
         MVC   PDESC+L'TSHRSMN+1(2),ONENUL                                      
         CLC   RQ_TT1NA,SPACES                                                  
         JH    TIMR028                                                          
         MVC   PDESC+L'TSHRSMN+1(2),=C'SJ'                                      
         CLC   RQ_TTCLI,SPACES                                                  
         JH    TIMR028                                                          
         MVC   PDESC+L'TSHRSMN+1(2),=C'  '                                      
TIMR028  MVC   PDESC+L'TSHRSMN+4(6),=C'Person'                                  
         CLC   RQ_TTPRS,SPACES                                                  
         JH    TIMR030                                                          
         MVC   PDESC+L'TSHRSMN+4(6),=C'      '                                  
TIMR030  MVC   PDESC+L'TSHRSMN+11(8),=C'location'                               
         CLC   RQ_TTOFF,SPACES                                                  
         JH    TIMR032                                                          
         MVC   PDESC+L'TSHRSMN+11(8),=C'        '                               
TIMR032  MVC   PDESC+L'TSHRSMN+20(3),RQ_TSBIL                                   
         J     TIMR038                                                          
                                                                                
TIMR036  MVC   PDESC(L'TSTSNMN),TSTSNMN                                         
         MVC   PDESC+L'TSTSNMN+1(L'RQ_TSSRC),RQ_TSSRC                           
TIMR038  GOTOR APRINTER                                                         
*                                                                               
TIMR040  XC    TEMP2,TEMP2                                                      
         MVC   TEMP2(L'ACTKACT),RQ_TT1NA                                        
         GOTOR VTS1NC                                                           
         BNE   TIMSRCN                                                          
*                                                                               
TIMR042  MVC   TEMP2(L'ACTKULA),SPACES                                          
         MVC   TEMP2(L'RQ_TTCLI+L'RQ_TTPRO+L'RQ_TTJOB),RQ_TTCLI                 
         GOTOR VTSCPJ                                                           
         BNE   TIMSRCN                                                          
                                                                                
TIMR044  MVC   TEMP2(2),RQ_TTWCD                                                
         GOTOR VTSWCD                                                           
         BNE   TIMSRCN                                                          
*                                                                               
TIMR046  CLI   RQ_TSSRC,RQ_TSTMY   my timesheets?                               
         BE    TIMR048             yes - got person code from PID               
         CLC   RQ_TTPRS,SPACES     Do we have a person code passed              
         BNH   TIMR050             no                                           
TIMR048  OI    TS#ICPJ,TS#IPERR    yes - set we have person                     
         GOTOR PERSOFF             Reset office code for person                 
*                                                                               
         USING ACTRECD,R2                                                       
TIMR050  MVC   TS#1RACT,SPACES                                                  
         MVC   X#1RACT,SPACES                                                   
         MVC   X#1RPER,SPACES                                                   
         MVC   TS#1ROFF,SPACES                                                  
         MVC   TS#1RDPT,SPACES                                                  
         MVC   TS#1RSDP,SPACES                                                  
         CLC   RQ_TTOFF,SPACES     validate Off/Dep/Sub filters                 
         BNH   TIMR058                                                          
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(L'ACTKUNT+L'ACTKLDG),ONERUL                              
         XR    RE,RE                                                            
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         BZ    *+8                                                              
         LA    RE,1                                                             
         MVC   ACTKACT(0),RQ_TTOFF                                              
         EX    RE,*-6                                                           
         OC    ACTKACT,SPACES                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    TIMR052                                                          
         MVC   ROUERRV,=AL2(AE$IVOFF)                                           
         B     TIMSRCN                                                          
*                                                                               
TIMR052  MVC   X#1RACT,ACTKACT                                                  
         OI    TS#ICPJ,TS#IOFFR                                                 
*                                                                               
         CLC   RQ_TTDEP,SPACES                                                  
         BNH   TIMR058                                                          
         LA    R2,IOKEY                                                         
         LA    R1,1                                                             
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         BZ    *+8                                                              
         LA    R1,2                                                             
         LA    R1,ACTKACT(R1)                                                   
         LR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,ONERL2L                                                       
         AHI   R1,-1                                                            
         MVC   0(0,RE),RQ_TTDEP                                                 
         EX    R1,*-6                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    TIMR054                                                          
         MVC   ROUERRV,=AL2(AE$IDEPT)                                           
         B     TIMSRCN                                                          
*                                                                               
TIMR054  MVC   X#1RACT,ACTKACT                                                  
         OI    TS#ICPJ,TS#IDPTR                                                 
*                                                                               
         CLC   RQ_TTSUB,SPACES                                                  
         BNH   TIMR058                                                          
         LA    R2,IOKEY                                                         
         SR    R1,R1                                                            
         IC    R1,ONERL2L                                                       
         LA    RE,ACTKACT(R1)                                                   
         LA    R0,R1                                                            
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         MVC   0(0,RE),RQ_TTSUB                                                 
         EX    R1,*-6                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    TIMR056                                                          
         MVC   ROUERRV,=AL2(AE$ISDPT)                                           
         B     TIMSRCN                                                          
*                                                                               
TIMR056  MVC   X#1RACT,ACTKACT                                                  
         OI    TS#ICPJ,TS#ISDPR                                                 
         DROP  R2                                                               
*                                                                               
TIMR058  CLI   RQ_TSSRC,RQ_TSTAP   If just my approvals can skip                
         JE    TIMR062               security validation                        
         GOTOR CHKLSEC             Check security for saved searches            
         BNE   TIMSRCN                                                          
         CLI   RQ_TSSRC,RQ_TSTBA   Back up approver                             
         BNE   TIMR062                                                          
         CLI   RQ_AWAIT,YESQ       Are we looking for timesheets to             
         BNE   *+8                                       approve                
         OI    X#SRCIND,X#SRCAPP   Set approver is searching                    
*                                                                               
TIMR060  MVI   TS#PTYPE,TS#PTRB    back up approval TS search                   
         J     TIMR100                                                          
*                                  Reinitialize TSAR for search                 
TIMR062  GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         MVI   GAPLPARM,0          Clear flag to append to TSAR                 
         TM    TS#ICPJ,TS#IPERR    Do we have a person code                     
         BZ    TIMR068             No                                           
         MVC   X#1RPER,TS#PRSAC    Save person code for filtering               
         MVI   TS#PTYPE,TS#PTRHP   hour person search                           
         CLI   RQ_TTYPE,RQ_THOUR   hours or timesheets                          
         BE    TIMR064                                                          
         MVI   TS#PTYPE,TS#PTRTP   person TS search                             
*                                                                               
*        CLI   RQ_TPRSR,C'Y'       are we able to overide person search         
*        BE    TIMR064             yes - don't check limlist/approver           
         GOTOR AWTAPP              Awaiting approval routine                    
         BNE   TIMR064                                                          
         OI    X#SRCIND,X#SRCAPP+X#SRCRET  Set approver search                  
         J     TIMR100                    and we need to return                 
                                                                                
TIMR064  MVC   TS#PRSAC,X#1RPER                                                 
         OC    TS#LMPIN,TS#LMPIN   Line manager PIN passed?                     
         JNZ   *+12                                                             
         CLI   RQ_TSSRC,RQ_TSTMY   my timesheets?                               
         BE    TIMR066             yes - ignore limit list for yourself         
         CLI   RQ_TSSRC,RQ_TSTST   Staff search?                                
         JNE   TIMR065                                                          
         OC    TS#LMPIN,TS#LMPIN   Line manager PIN passed?                     
         JZ    TIMR065                                                          
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLAPPR',SPACES),('GAPLTDTE',GAPLPARM),              +        
               ('QTIME',TS#LMPIN)                                               
         OI    X#SRCIND,X#SRCAPP                                                
         J     TIMR100             and we need to return                        
*                                                                               
TIMR065  CLI   RQ_TPRSR,C'Y'       are we able to overide person search         
         BE    TIMR066             yes - ignore limlst and approver             
                                                                                
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLLIML',SPACES),('GAPLTDTE',GAPLPARM),('QTIME',0)            
         BE    TIMR066                                                          
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLAPPR',SPACES),('GAPLTDTE',GAPLPARM),('QTIME',0)            
         BE    TIMR066                                                          
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLBKAP',SPACES),('GAPLTDTE',GAPLPARM),('QTIME',0)            
*                                                                               
TIMR066  L     RE,=A(TLIOMSD)                                                   
         B     TIMR110                                                          
*                                                                               
TIMR068  CLC   ANYACCNT,SPACES                                                  
         BE    TIMR080                                                          
         MVI   TS#PTYPE,TS#PTRHC   hours client/non-client search               
*                                                                               
         XC    BYTE2,BYTE2                                                      
         CLI   RQ_APP1R,C'Y'       Does user have limited hr search?            
         BNE   TIMR072                                                          
         XC    GAPAREA,GAPAREA     if so, get the 1R limits 1st                 
         MVC   GAPLRUNL,=C'1R'                                                  
         MVC   GAPLRACT,X#1RACT    FILTER-LIMITED (EG OFFICE)                   
         OI    X#ACCSTA,X#CLISR    Show that we are doing a CLI Search          
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLLIML',GAPLRUNL),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         JNE   TIMR068A                                                         
         CLI   RQ_MYRR,YESQ   Do we want to include my reports reports          
         JNE   TIMR070           No                                             
         OI    X#SRCIND,X#SRCLMR Yes - set we want limit list,                  
         J     TIMR070             approvals and there reports                  
*                                                                               
TIMR068A GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLAPPR',GAPLRUNL),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         JNE   TIMR068B                                                         
         CLI   RQ_MYRR,YESQ   Do we want to include my reports reports          
         JNE   TIMR070           No                                             
         OI    X#SRCIND,X#SRCMRR+X#SRCIRR Yes - set we want reports             
         GOTOR GETMYRR             get my reports reports PIN into WMP          
         NI    X#SRCIND,X'FF'-(X#SRCLMR) and return                             
         J     TIMR070                                                          
                                                                                
TIMR068B DS    0H                                                               
*&&US                                                                           
         CLC   CUAALF,=C'H7'       GroupM US only behaviour                     
         JNE   TIMR069                                                          
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLBKAP',GAPLRUNL),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         JE    TIMR070             if we have something                         
         J     TIMR072                                                          
*&&                                                                             
                                                                                
TIMR069  MVI   GAPLPARM,0                                                       
         MVI   RQ_APP1R,C'N'       No 1R restrictions to check                  
         J     TIMR072             if we have something                         
TIMR070  MVI   GAPLPARM,GAPLPADD      gaplst needs to know buffer not           
*                                                          empty                
TIMR072  MVC   GAPLFILT,ANYACCNT                                                
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT20Q',TSARABUF),           +        
               ('GAPLLIML',GAPLRUNL),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         BE    TIMR100                                                          
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT20Q',TSARABUF),           +        
               ('GAPLAPPR',GAPLRUNL),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         BNE   TIMR074                                                          
         OI    X#SRCIND,X#SRCAPP   set approver is searching                    
         J     TIMR100                                                          
TIMR074  XC    GAPAREA,GAPAREA     check for any SJ/1N entries added            
         MVI   GAPTDAT1-GAPTABD+GAPAREA,GAPTT2Q                                 
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   TIMR098             None found                                   
         MVC   ROUERRV,FULL2       display error from GAPLST call               
         B     TIMSRCN              either no limit list or approver            
*                                                                               
TIMR080  MVI   TS#PTYPE,TS#PTRHO   hours office/dept/sub-dept search            
         CLI   RQ_TTYPE,RQ_THOUR   Or are we the hours search                   
         BE    TIMR082             Hours search                                 
         MVI   TS#PTYPE,TS#PTRTA   TS my approval search                        
         CLI   RQ_TSSRC,RQ_TSTAP   If just my approvals                         
         JE    TIMR082                 can skip awaiting                        
         MVI   TS#PTYPE,TS#PTRTO   TS office/dept/sub-dept search               
         GOTOR AWTAPP              Awaiting approval routine                    
         BNE   TIMR082                                                          
         OI    X#SRCIND,X#SRCAPP+X#SRCRET  Set approver search                  
         J     TIMR100                      and we need to return               
*                                                                               
TIMR082  MVC   GAPLRUNL,=C'1R'                                                  
         MVC   GAPLRACT,X#1RACT                                                 
         XC    GAPLPARM,GAPLPARM                                                
         CLI   RQ_TSSRC,RQ_TSTAP   If just my approvals                         
         JE    TIMR084                 go to aproval rights                     
*                                                                               
         CLI   RQ_TSSRC,RQ_TSTST   Staff search?                                
         JNE   TIMR083                                                          
         OC    TS#LMPIN,TS#LMPIN   Line manager PIN passed?                     
         JZ    TIMR083                                                          
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLAPPR',SPACES),('GAPLTDTE',GAPLPARM),              +        
               ('QTIME',TS#LMPIN)                                               
         JNE   TIMR090                                                          
         J     TIMR100                                                          
                                                                                
TIMR083  GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLLIML',GAPLFILT),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         JNE   TIMR084                                                          
         CLI   RQ_MYRR,YESQ   Do we want to include my reports reports          
         JNE   TIMR100           No                                             
         OI    X#SRCIND,X#SRCLMR Yes - set we want limit list,                  
         J     TIMR100             approvals and there reports                  
TIMR084  GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLAPPR',GAPLFILT),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         JNE   TIMR090                                                          
         OI    X#SRCIND,X#SRCAPP    set approver is searching                   
         CLI   RQ_MYRR,YESQ   Do we want to include my reports reports          
         JNE   TIMR100           No                                             
         OI    X#SRCIND,X#SRCMRR+X#SRCIRR Yes - set we want reports             
         GOTOR GETMYRR             get my reports reports PIN into WMP          
         NI    X#SRCIND,X'FF'-(X#SRCLMR) and return                             
         J     TIMR100                                                          
*                                                                               
TIMR090  CLI   RQ_TSSRC,RQ_TSTAP   If just my approvals and no approval         
         JE    TIMR500               rights - go to exit                        
         TM    X#SRCIND,X#SRCLMR  Was this a repeat call from limit             
         JNZ   TIMR500              list and no approvals found                 
*&&US                                then put out data found                    
         CLI   RQ_TTYPE,RQ_THOUR   Are we the hours search                      
         JNE   TIMR092             No - timesheet search                        
         CLC   CUAALF,=C'H7'       GroupM US only behaviour                     
         JNE   TIMR092                                                          
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLBKAP',GAPLFILT),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         JNE   TIMR092             if we have something                         
         OI    X#SRCIND,X#SRCAPP   set approver is searching                    
         J     TIMR100                                                          
*&&                                                                             
TIMR092  XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   TIMR094             Buffer is empty                              
         TM    X#SRCIND,X#SRCFND   Did we find something in phase 1             
         JNZ   TIMR400             Yes - output this                            
         MVC   ROUERRV,FULL2       Display error from GAPLST call               
         J     TIMSRCN                                                          
                                                                                
TIMR094  TM    TS#ICPJ,TS#IOFFR+TS#IDPTR+TS#ISDPR                               
         JNZ   TIMR096                                                          
         TM    X#SRCIND,X#SRCFND   Did we find something in phase 1             
         JNZ   TIMR400             Yes - output this                            
         MVC   ROUERRV,=AL2(AE$IOFCQ) Input to office required                  
         CLI   RQ_TTYPE,RQ_THOUR    Timesheet or hours search?                  
         JNE   TIMSRCN              Timesheet                                   
         MVC   ROUERRV,=AL2(AE$OFRAC) Input to off or account required          
         J     TIMSRCN                                                          
TIMR096  DS    0H                                                               
*&&US                                                                           
         CLC   CUAALF,=C'H7'       GroupM US only behaviour                     
         JNE   TIMR098                                                          
         CLI   RQ_TPRSR,C'Y'       are we able to overide person search         
         JNE   TIMR100             No - so don't build override in tsar         
*&&                                                                             
TIMR098  GOTOR BLDOVR              build override                               
                                                                                
TIMR100  L     RE,=A(TLGETAD)                                                   
*                                                                               
TIMR110  A     RE,SRVRRELO         save 'get routine' address                   
         ST    RE,X#AGADR                                                       
         GOTOR CALPRDS,0           calendar periods retrieval                   
         BE    TIMR112                                                          
         MVC   ROUERRV,FULL2                                                    
         B     TIMSRCN                                                          
*                                                                               
TIMR112  SR    R4,R4                                                            
         IC    R4,X#DITMS          HAVE WE ANY PERIODS                          
         LTR   R4,R4                                                            
         BNZ   TIMR114             YES                                          
         CLI   TS#PTYPE,TS#PTRTP   IF PERSON SEARCH THIS IS OK                  
         BE    TIMSRCY                                                          
         CLI   TS#PTYPE,TS#PTRHP   IF PERSON HOUR SEARCH OK                     
         BE    TIMSRCY                                                          
         BZ    TIMSRCN             IF NOT ERROR                                 
TIMR114  GOTO1 VXSORT,DMCB,(X'00',AIO5),(R4),PERLENQ,PERKEYQ,0                  
*                                                                               
         XC    X#FLTST,X#FLTST     CLEAR DOWN STATUS FILTER                     
         XC    X#REQST,X#REQST                                                  
*                                                                               
*        CLI   TS#PTYPE,TS#PTRTP   person TS search                             
*        BE    TIMR120             we want status passed from web               
*                                                                               
         CLI   RQ_TTYPE,RQ_THOUR   Timesheet or hours search?                   
         JNE   TIMR118                                                          
         MVI   X#FLTST,TIMSFAPP    get only approved time                       
         MVC   X#REQST(2),=X'F6FF' Set fully approved only                      
         CLI   RQ_TSUNA,C'Y'       do we want unapproved time                   
         BNE   TIMR150             no                                           
         MVI   X#FLTST,X'FF'       yes - get all statuses                       
         MVC   X#REQST(7),=C'1345678' approval statues for real TS              
         MVI   X#REQST+7,X'FF'     Set end marker                               
         B     TIMR150                                                          
*                                                                               
TIMR118  TM    X#SRCIND,X#SRCRET  Searching for awaiting approval               
         BZ    TIMR120            No                                            
         MVI   X#FLTST,TIMSAWAP   Yes - set appropriate status                  
         MVI   X#FLTST+1,X'FF'                                                  
         MVI   X#REQST,C'1'                                                     
         MVI   X#REQST+1,X'FF'                                                  
         B     TIMR150                                                          
*                                                                               
TIMR120  LA    R1,RQ_NSTRT                                                      
         LA    RF,STATUSTB                                                      
         LA    R2,X#FLTST          HEX STATUS                                   
         LA    R3,X#REQST          WEB FRONT END EQUIVALENTS                    
TIMR122  CLI   0(RF),X'FF'                                                      
         BE    TIMR132                                                          
         CLI   0(R1),C'Y'                                                       
         BNE   TIMR130                                                          
         MVC   0(1,R2),0(RF)                                                    
         MVC   0(1,R3),1(RF)                                                    
         AHI   R3,1                                                             
         AHI   R2,1                                                             
TIMR130  LA    RF,2(RF)                                                         
         AHI   R1,1                                                             
         B     TIMR122                                                          
*                                                                               
TIMR132  MVI   0(R2),X'FF'         set end of hex status                        
         MVI   0(R3),X'FF'         set end of web status                        
         CLI   X#FLTST,X'FF'       are there any values set                     
         BNE   TIMR150             yes                                          
         TM    X#SRCIND,X#SRCFND   Did we find something in phase 1             
         JNZ   TIMR400             Yes - output this                            
         MVC   ROUERRV,=AL2(AE$INFLT)  no - error                               
         B     TIMSRCN                                                          
*                                                                               
TIMR150  XC    IOKEY,IOKEY         Set to 'first time'                          
         XC    CSVKEY1,CSVKEY1                                                  
         XC    X#SVSEQ,X#SVSEQ                                                  
         CLI   RQ_APP1R,C'Y'       Apply 1R rules to SJ look up                 
         BNE   TIMR200             No                                           
         CLI   TS#PTYPE,TS#PTRHC   Only applicable to SJ hours search           
         BE    TIMR200                                                          
TIMR158  MVI   RQ_APP1R,C'N'                                                    
*                                                                               
TIMR200  L     RF,X#AGADR          routine address                              
         BASR  RE,RF                                                            
         BNE   TIMR300             end of IO reached                            
                                                                                
         OI    X#SRCIND,X#SRCFND   Yes - set we found records                   
         CLI   RQ_TTYPE,RQ_THOUR   Timesheet or hours search?                   
         JE    TIMR210                                                          
         CLI   TS#PTYPE,TS#PTRTP   person timesheet search                      
         BNE   TIMR220                                                          
TIMR210  MVC   IODA,TS#RECDA       read DA record                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR PROMTSR             process T/S record into table                
         BE    TIMR200                                                          
         B     TIMSRCN                                                          
                                                                                
TIMR220  GOTOR PROPTSR             process T/S passive into table               
         BE    TIMR200                                                          
         B     TIMSRCN                                                          
*                                                                               
TIMR300  DS    0H                                                               
*        CLI   TS#PTYPE,TS#PTRTP   person timesheet search                      
*        BE    TIMR305                                                          
*        CLI   TS#PTYPE,TS#PTRB    back up approver search                      
*        BE    TIMR305                                                          
*        CLI   TS#PTYPE,TS#PTRTO   office/dept/sub timesheet search             
*        BNE   TIMR400                                                          
         CLI   RQ_TTYPE,RQ_THOUR   Timesheet or hours search?                   
         JE    TIMR400                                                          
         TM    X#SRCIND,X#SRCRET   Do we need to return do complete             
         JZ    TIMR305                                       search             
         NI    X#SRCIND,X'FF'-(X#SRCAPP+X#SRCRET)  Reset TSAR buffer            
         XC    TS#1ROFF,TS#1ROFF                   Reset calendar               
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         TM    TS#ICPJ,TS#IPERR    Do we have a person code                     
         JNZ   TIMR064             Yes                                          
         J     TIMR082             No                                           
*                                                                               
TIMR305  LA    R1,X#REQST                                                       
TIMR310  CLI   0(R1),X'FF'                                                      
         BE    TIMR400                                                          
         CLI   0(R1),TS#OVERD    overdue and not started?                       
         BE    TIMR320                                                          
         CLI   0(R1),TS#NSTRT                                                   
         BE    TIMR320                                                          
         LA    R1,1(R1)                                                         
         B     TIMR310                                                          
*                                                                               
TIMR320  L     RE,=A(TLIOMSD)                                                   
         A     RE,SRVRRELO                                                      
         CLM   RE,15,X#AGADR       If use TLIOMSD go to CHKSTAB                 
         BE    TIMR330                                                          
         GOTOR BLDATSR             BUILD TSAR RECS OF MISSING TIME RECS         
         BE    TIMR400                                                          
         MVC   ROUERRV,FULL2                                                    
         B     TIMSRCN                                                          
*                                                                               
TIMR330  GOTOR CHKSTAB             READ PERIOD TABLE AND BUILD TSAR REC         
         BE    TIMR400                                                          
         MVC   ROUERRV,FULL2                                                    
         B     TIMSRCN                                                          
*                                                                               
TIMR400  TM    X#SRCIND,X#SRCLMR   Did we come from limit list                  
         JZ    TIMR412             Go back and get approvals                    
         CLC   ANYACCNT,SPACES                                                  
         JH    TIMR068A                                                         
         J     TIMR084                                                          
*                                                                               
TIMR412  TM    X#SRCIND,X#SRCMRR   Do we need to get my reports reports         
         JZ    TIMR500             No                                           
         XC    TS#1ROFF,TS#1ROFF                                                
                                                                                
         OC    DAPID2,DAPID2        Test we created a list of PINs              
         JZ    TIMR440              No - exit as nothing to do                  
                                                                                
         L     R4,SVADRPN           R4=A(In list of PINs)                       
         TM    X#SRCIND,X#SRCIRR                                                
         JZ    TIMR410                                                          
         NI    X#SRCIND,X'FF'-(X#SRCIRR)                                        
                                                                                
         USING LW_D,R2                                                          
         XR    R2,R2               POINT TO LIST IN WMP                         
         ICM   R2,7,DAPID2                                                      
         XR    R3,R3                                                            
         ICM   R3,3,LW_NUMN        number of entries                            
         STH   R3,COUNTHW          store count                                  
         LA    R4,LW_DATA2         start of list                                
         ST    R4,SVADRPN          save address of list of PINs                 
*                                                                               
TIMR410  LLH   RF,COUNTHW         Any more PINs to process                      
         CHI   RF,0                                                             
         JE    TIMR440            No - go to show time or hour records          
                                                                                
* clear TSAR buffer for approver rights                                         
                                                                                
TIMR420  GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
                                                                                
         MVC   HALF1,0(R4)         Save PIN                                     
         L     R4,SVADRPN          Bump R4 to next PIN                          
         LA    R4,L'PIDNO(R4)       and subtract the count of PINs              
         ST    R4,SVADRPN                                                       
         LH    R3,COUNTHW                                                       
         SHI   R3,1                                                             
         STH   R3,COUNTHW                                                       
                                                                                
* call GAPLST to get approver rights for the PIN in the WMP                     
                                                                                
         MVC   GAPLRUNL,=C'1R'                                                  
         MVC   GAPLRACT,X#1RACT                                                 
         XC    GAPLPARM,GAPLPARM                                                
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLAPPR',GAPLFILT),('GAPLTDTE',GAPLPARM),            +        
               ('QTIME',HALF1)                                                  
         JNE   TIMR420             No approver rights - get next PIN            
         CLC   ANYACCNT,SPACES                                                  
         JH    TIMR070                                                          
         J     TIMR100             Process time for this approver               
                                                                                
TIMR440  NI    X#SRCIND,X'FF'-(X#SRCAPP+X#SRCMRR+X#SRCIRR)                      
                                                                                
TIMR500  GOTOR GTNMOUT             get names and output data                    
         BNE   TIMSRCN                                                          
*                                                                               
TIMSRCY  CR    RB,RB                                                            
         J     TIMSRCX                                                          
TIMSRCN  LTR   RB,RB                                                            
TIMSRCX  XIT1                                                                   
*                                                                               
STATUSTB DC    AL1(0),C'2'             Not started                              
         DC    AL1(TAPSSAVQ),C'3'      Overdue                                  
         DC    AL1(TAPSSAVQ),C'4'      In progress                              
         DC    AL1(TIMSSUBM),C'5'      submitted                                
         DC    AL1(TIMSFAPP),C'6'      fully approved                           
         DC    AL1(TIMSREJE),C'7'      rejected                                 
         DC    AL1(TIMSAWAP),C'1'      awaiting approval                        
         DC    AL1(TIMSPAPP),C'8'      part approved                            
         DC    X'FFFF'                                                          
         DS    0H                                                               
*                                                                               
TSHRSMN  DC    C'Hours search'                                                  
TSTSNMN  DC    C'Timesheet search no min/max'                                   
TSTSMIN  DC    C'Timesheet search min hours '                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SUMMARY ROUTINE FOR TIMESHEETS                                      *         
***********************************************************************         
         SPACE 1                                                                
TIMSUM00 NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*TIMSUM*'                                                    
                                                                                
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         XC    ANYACCNT,ANYACCNT                                                
         XC    GAPLCALT,GAPLCALT                                                
         OC    CCTPID,CCTPID       check connected PID code                     
         BNZ   TIMS002                                                          
         MVC   LP_ERROR,=AL2(AE$NCPID)                                          
         B     TIMSUMN                                                          
                                                                                
TIMS002  MVC   TS#DATE,TODAYP                                                   
         MVC   TEMP2(2),CCTPID                                                  
         GOTOR (#ACCPID,AACCPID)                                                
         BE    TIMS004                                                          
         MVC   LP_ERROR,=AL2(AE$INPID)                                          
         B     TIMSUMN                                                          
*                                                                               
TIMS004  OC    TEMP2+10(L'TS#PRSAC),SPACES                                      
         MVC   TS#PRSAC,TEMP2+10      save accounting person code               
         XC    TS#PBLK(TS#PBLQ),TS#PBLK                                         
         MVC   TS#PDAT,TS#DATE                                                  
         MVC   TS#PEND,TS#DATE                                                  
         MVC   TS#PEST,=X'FFFFFF'                                               
         MVC   TS#PCOD,TS#PRSAC                                                 
         GOTOR PERDTL   (data returned in TS#PBLK)                              
         BE    TIMS005                                                          
         MVC   LP_ERROR,FULL2                                                   
         B     TIMSUMN                                                          
*                                                                               
TIMS005  L     R1,TS#PCLE          retrieve office                              
         MVC   TS#1ROFF,LOCOFF-LOCELD(R1)                                       
         MVC   TS#1RDPT,LOCDEPT-LOCELD(R1)                                      
         MVC   TS#1RSDP,LOCSUB-LOCELD(R1)                                       
                                                                                
         LA    RE,1                if 2 char office person code shorter         
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         BNZ   *+8                                                              
         LA    RE,0                                                             
         MVC   TS#1RACT(0),TS#1ROFF                                             
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         LA    RF,TS#1RACT                                                      
         AR    RF,RE                                                            
         LR    R0,RE                                                            
         XR    R1,R1                                                            
         IC    R1,ONERL2L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),TS#1RDPT                                                 
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),TS#1RSDP                                                 
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL3L                                                       
         IC    R1,ONERL4L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   0(0,RF),TS#PRSAC                                                 
         EX    R1,*-6                                                           
                                                                                
         GOTOR (#CSTPRF,ACSTPRF),DMCB,TS#1RACT  get approvers profiles          
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         ZAP   DUB1,CONDO                                                       
         CVB   R2,DUB1                                                          
         LNR   R2,R2                                                            
*                                                                               
         MVC   WORK(L'TODAYF),TODAYF                                            
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,(R2)                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,X#CUTDT)                              
*                                                                               
         OC    RQ_SUPER(L'RQ_SUPER+L'RQ_SUCLI+L'RQ_SU1NA),SPACES                
         MVC   GAPLFILT,SPACES                                                  
                                                                                
         CLI   RQ_SUTYP,RQ_SUT2Q  for staff details need person                 
         BNE   TIMS015                                                          
         CLC   RQ_SUPER,SPACES                                                  
         BH    TIMS010                                                          
         MVC   LP_ERROR,=AL2(AE$MIPER)                                          
         B     TIMSUMN                                                          
*                                                                               
TIMS010  MVC   TS#PRSAC,RQ_SUPER                                                
         OC    TS#PRSAC,SPACES                                                  
         MVC   TEMP2(8),TS#PRSAC                                                
         GOTOR (#ACCPFP,AACCPFP)                                                
         BE    TIMS012                                                          
         MVC   LP_ERROR,=AL2(AE$IVPER)                                          
         B     TIMSUMN                                                          
TIMS012  L     R0,AIO4             copy person record to IO4                    
         LA    R1,IOLENQ                                                        
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVI   BYTE1,GAPTT1Q                                                    
         MVI   TS#PTYPE,TS#PTSMP                                                
         CLI   RQ_SUVIE,RQ_SUVMQ   is this month view                           
         BE    TIMS050             yes                                          
         MVI   TS#PTYPE,TS#PTSWP                                                
         B     TIMS050                                                          
*                                                                               
TIMS015  CLI   RQ_SUTYP,RQ_SUT4Q  for client details need client                
         BNE   TIMS035                                                          
         CLC   RQ_SUCLI,SPACES                                                  
         BH    TIMS020                                                          
         CLC   RQ_SU1NA,SPACES                                                  
         BH    TIMS025                                                          
         MVC   LP_ERROR,=AL2(AE$MSCLI)                                          
         B     TIMSUMN                                                          
*                                                                               
TIMS020  MVC   TEMP2(L'ACTKULA),SPACES                                          
         MVC   TEMP2(L'RQ_SUCLI),RQ_SUCLI                                       
         GOTOR VTSCPJ              (SETS ANYACCNT)                              
         BE    TIMS022                                                          
         MVC   LP_ERROR,=AL2(AE$INCLI)                                          
         B     TIMSUMN                                                          
                                                                                
TIMS022  MVC   GAPLFILT,ANYACCNT                                                
         MVI   BYTE1,GAPTT2Q    (reads SJ)                                      
         B     TIMS050                                                          
*                                                                               
TIMS025  LA    R1,IOKEY            validate 1N account and save it              
         USING ACTRECD,R1                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),ONENUL                                                
         MVC   ACTKACT,RQ_SU1NA                                                 
         MVC   ANYACCNT,ACTKULA                                                 
         MVC   GAPLFILT,ACTKULA                                                 
         MVI   BYTE1,GAPTT3Q    (reads 1N)                                      
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    TIMS050                                                          
         MVC   LP_ERROR,=AL2(AE$INACC)                                          
         B     TIMSUMN                                                          
         DROP  R1                                                               
                                                                                
TIMS035  CLI   RQ_SUTYP,RQ_SUT1Q   for staff summary get approvals              
         BNE   TIMS040                                                          
         MVI   BYTE1,GAPTT1Q                                                    
         B     TIMS050                                                          
                                                                                
TIMS040  CLI   RQ_SUTYP,RQ_SUT3Q   for client summary get approvals             
         BNE   TIMS045                                                          
         MVI   BYTE1,GAPTT20Q      (reads SJ and 1N)                            
         CLI   RQ_SUX1N,YESQ                                                    
         BNE   TIMS050                                                          
         MVI   BYTE1,GAPTT2Q       (or SJ only if security to excl 1N)          
         B     TIMS050                                                          
                                                                                
TIMS045  MVC   LP_ERROR,=AL2(AE$IVTYP)   => invalid type                        
         B     TIMSUMN                                                          
                                                                                
TIMS050  GOTOR (#GAPLST,AGAPLST),DMCB,(BYTE1,TSARABUF),                +        
               ('GAPLLIML',GAPLFILT),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         BE    TIMS060                                                          
         GOTOR (#GAPLST,AGAPLST),DMCB,(BYTE1,TSARABUF),                +        
               ('GAPLAPPR',GAPLFILT),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         BE    TIMS060                                                          
         GOTOR (#GAPLST,AGAPLST),DMCB,(BYTE1,TSARABUF),                +        
               ('GAPLBKAP',GAPLFILT),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         BE    TIMS060                                                          
         BH    TIMS052                                                          
         MVC   LP_ERROR,FULL2      empty or full                                
         B     TIMSUMN                                                          
                                                                                
TIMS052  CLI   RQ_SUTYP,RQ_SUT3Q   for client summary                           
         BE    TIMS054                                                          
         CLI   RQ_SUTYP,RQ_SUT4Q   for client detail                            
         BE    TIMS054                                                          
         MVC   LP_ERROR,FULL2      empty                                        
         B     TIMSUMN                                                          
TIMS054  MVI   GAPLCALT,GAPLALLC   set get all clients                          
         B     TIMS061                                                          
*                                                                               
TIMS060  OI    X#SRCIND,X#SRCAPP   set approval view                            
*                                                                               
* BUILD X#SUMTAB FOR DAY,MONTH, WEEK VIEW                                       
TIMS061  GOTOR VDATCON,DMCB,(0,RQ_SUDTE+2),(1,TS#CEDT) validate date            
         LA    R4,X#SUMTAB                                                      
                                                                                
*                                                                               
         CLI   RQ_SUVIE,RQ_SUVMQ   is this month view                           
         BNE   TIMS065                                                          
         GOTO1 VDATCON,DMCB,(1,TS#CEDT),(0,WORK)  YES, WANT LAST 5              
         MVC   WORK+6(6),WORK                                                   
         GOTO1 VGETDAY,DMCB,WORK+6,WORK+6         MON-SUN PERIODS               
         LLC   RF,0(R1)                                                         
         LA    RE,7                                                             
         SR    RE,RF                                                            
         ST    RE,DMCB+8                                                        
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK,,0   FIND NEXT SUN                  
         GOTO1 VDATCON,DMCB,(0,WORK),(1,TS#CEDT)                                
         LA    R4,X#SUMTAB                                                      
         USING SUMTABD,R4                                                       
         LA    R3,5                                5 WEEKS                      
TIMS063  GOTO1 VDATCON,DMCB,(0,WORK),(1,STEND)                                  
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK,F'-6'  PREV MON                     
         GOTO1 VDATCON,DMCB,(0,WORK),(1,STSTART)                                
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK,F'-1'  PREV SUN                     
         LA    R4,SUMTABL(R4)                                                   
         BCT   R3,TIMS063                                                       
*                                                                               
         MVI   0(R4),0                                                          
         SHI   R4,SUMTABL              LAST ENTRY IS EARLIEST                   
         MVC   TS#CSDT,STSTART         SET START DATE FOR CALPRDS               
         B     TIMS075                                                          
*                                                                               
TIMS065  CLI   RQ_SUVIE,RQ_SUVWQ                                                
         BNE   TIMS070                                                          
         LA    R3,7                              SUMTAB IS LAST 7 DAYS          
         GOTO1 VDATCON,DMCB,(0,RQ_SUDTE+2),(0,WORK)                             
TIMS066  GOTO1 VDATCON,DMCB,(0,WORK),(1,STEND)                                  
         GOTO1 VDATCON,DMCB,(0,WORK),(1,STSTART)                                
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK,F'-1'                               
         LA    R4,SUMTABL(R4)                                                   
         BCT   R3,TIMS066                                                       
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK,F'1'                                
         GOTO1 VDATCON,DMCB,(0,WORK),(1,TS#CSDT)      gives us start            
         B     TIMS075                                                          
*                                                                               
TIMS070  DS    0H                                                               
         DROP  R4                                                               
*                                                                               
TIMS075  SR    R1,R1                                                            
         CLI   RQ_SUTYP,RQ_SUT2Q   DETAIL CALLS AT PERSON LVL                   
         BE    TIMS085                                                          
         CLI   RQ_SUTYP,RQ_SUT4Q                                                
         BE    TIMS085                                                          
         LA    R1,RUNNOPER                                                      
TIMS085  GOTOR CALPRDS             and read periods into AIO5                   
         BE    TIMS090                                                          
         MVC   LP_ERROR,FULL2                                                   
         B     TIMSUMN                                                          
*                                                                               
TIMS090  GOTOR (#MTSAR,AMTSAR),DMCB,TSAINI,TSARTBUF,TSARTDL                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R4,R4                                                            
         IC    R4,X#DITMS          have we any periods                          
         LTR   R4,R4                                                            
         BNZ   TIMS095             yes                                          
         CLI   TS#PTYPE,TS#PTSMP   no - ok if person level                      
         BE    TIMSUMY                                                          
         CLI   TS#PTYPE,TS#PTSWP                                                
         BE    TIMSUMY                                                          
         BZ    TIMSUMN             error if not                                 
TIMS095  GOTO1 VXSORT,DMCB,(X'00',AIO5),(R4),PERLENQ,PERKEYQ,0                  
*                                                                               
TIMS110  ICM   R1,7,TS#CEDT                                                     
         LNR   R1,R1                                                            
         STCM  R1,7,TS#CRED                                                     
         XR    R1,R1                                                            
         ICM   R1,7,TS#CSDT                                                     
         LNR   R1,R1                                                            
         STCM  R1,7,TS#CRSD                                                     
         CLI   RQ_SUTYP,RQ_SUT1Q   staff summary - all staff                    
         BNE   TIMS112                                                          
         L     RE,=A(TLGETAD)      staff IO routine                             
         MVI   TS#PTYPE,TS#PTSMS                                                
         CLI   RQ_SUVIE,RQ_SUVMQ   is this month view                           
         BE    TIMS190             yes                                          
         MVI   TS#PTYPE,TS#PTSWS                                                
         B     TIMS190                                                          
                                                                                
TIMS112  CLI   RQ_SUTYP,RQ_SUT2Q   staff summary - details of one               
         BNE   TIMS120                                                          
         L     RE,=A(TLIOMSD)      staff details IO routine                     
         B     TIMS190                                                          
*                                                                               
TIMS120  CLI   RQ_SUTYP,RQ_SUT3Q   client summary - totals                      
         BNE   TIMS130                                                          
         L     RE,=A(TLGETAD)      common client IO routine                     
         MVI   TS#PTYPE,TS#PTSMA                                                
         CLI   RQ_SUVIE,RQ_SUVMQ   is this month view                           
         BE    TIMS190             yes                                          
         MVI   TS#PTYPE,TS#PTSWA                                                
         B     TIMS190                                                          
*                                                                               
TIMS130  CLI   RQ_SUTYP,RQ_SUT4Q   client summary - details                     
         BNE   TIMS140                                                          
         MVI   TS#PTYPE,TS#PTSMC                                                
         CLI   RQ_SUVIE,RQ_SUVMQ   is this month view                           
         BE    TIMS132             yes                                          
         MVI   TS#PTYPE,TS#PTSWC                                                
TIMS132  L     RF,AGAPAREA                                                      
         MVC   0(L'TSJPACT,RF),SPACES RE-INIT FOR PERSON LIST                   
         MVI   L'TSJPACT(RF),GAPTEOT                                            
         CLI   GAPLCALT,GAPLALLC   no limit list or approver                    
         BNE   TIMS134                                                          
         GOTOR BLDOVR              yes build override entry in table            
TIMS134  L     RE,=A(TLGETAD)      common client IO routine                     
         B     TIMS190                                                          
*                                                                               
TIMS140  DC    H'0'                unknown type                                 
*                                                                               
TIMS190  A     RE,SRVRRELO         save it                                      
         ST    RE,X#AGADR                                                       
                                                                                
         XC    IOKEY,IOKEY         do pre settings                              
         XC    CSVKEY1,CSVKEY1     do pre settings                              
         XC    X#ITEMS,X#ITEMS                                                  
         MVI   X#FLTST,X'FF'       set to get all status values                 
         MVC   X#REQST(8),=C'12345678' approval statues for real TS             
         MVI   X#REQST+8,X'FF'     Set end marker                               
         XC    IOKEY,IOKEY         set to 'first time'                          
                                                                                
TIMS200  L     RF,X#AGADR          routine address                              
         BASR  RE,RF                                                            
         BNE   TIMS300             end of IO reached                            
                                                                                
         CLI   TS#PTYPE,TS#PTSMS                                                
         BE    TIMS220                                                          
         CLI   TS#PTYPE,TS#PTSMA                                                
         BE    TIMS220                                                          
         CLI   TS#PTYPE,TS#PTSMC                                                
         BE    TIMS220                                                          
TIMS210  MVC   IODA,TS#RECDA       read DA record                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR PROMTSR             process T/S record into table                
         B     TIMS200                                                          
                                                                                
TIMS220  GOTOR PROPTSR             process T/S passive into table               
         B     TIMS200                                                          
                                                                                
                                                                                
TIMS300  CLI   RQ_SUTYP,RQ_SUT1Q   staff summary - all staff                    
         BNE   TIMS310                                                          
         GOTOR BLDATSR             Build Tsar recs for missing time             
         BE    TIMS400                                                          
         MVC   LP_ERROR,FULL2                                                   
         B     TIMSUMN                                                          
*                                                                               
TIMS310  CLI   RQ_SUTYP,RQ_SUT2Q   staff summary - details of one               
         BNE   TIMS320                                                          
         GOTOR CHKSTAB             Build Tsar recs for missing time             
         BE    TIMS400                                                          
         MVC   LP_ERROR,FULL2                                                   
         B     TIMSUMN                                                          
*                                                                               
TIMS320  CLI   RQ_SUTYP,RQ_SUT3Q   client summary - all clients                 
         BE    TIMS330                                                          
         CLI   RQ_SUTYP,RQ_SUT4Q   client detail - one client                   
         BE    *+6                                                              
         DC    H'0'                                                             
TIMS330  GOTOR BLDCTSR             BUILD TSAR RECS FOR MISSING TIME             
         BE    TIMS400                  (last GAPLST entry)                     
         MVC   LP_ERROR,FULL2                                                   
         B     TIMSUMN                                                          
*                                                                               
TIMS400  GOTOR OUTSUM                                                           
TIMS410  TM    LP_FLAG,LP_FOFFL    Test offline                                 
         BZ    TIMSUMY                                                          
         SR    R1,R1                                                            
         ICM   R1,3,CUXPNUM        Check whether Brandocean or mobile           
         CHI   R1,XPMOBILQ          app is connected                            
         JE    TIMSUMY             Don't want stats for mobile                  
         USING X_DPRINT,RF                                                      
         L     RF,ARCPRINT                                                      
         LA    R1,SUTTAB                                                        
TIMS412  CLI   0(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(L'RQ_SUTYP,R1),RQ_SUTYP                                        
         JE    TIMS414                                                          
         LA    R1,SUTTABL(R1)                                                   
         J     TIMS412                                                          
                                                                                
TIMS414  MVC   PDESC(SUTTABL-1),1(R1)                                           
         LA    R1,SUDTAB                                                        
TIMS416  CLI   0(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(L'RQ_SUVIE,R1),RQ_SUVIE                                        
         JE    TIMS418                                                          
         LA    R1,SUDTABL(R1)                                                   
         J     TIMS416                                                          
                                                                                
TIMS418  MVC   PDESC+SUTTABL(SUDTABL-1),1(R1)                                   
                                                                                
TIMS440  GOTOR APRINTER                                                         
*                                                                               
TIMSUMY  CR    RB,RB                                                            
         J     *+6                                                              
TIMSUMN  LTR   RB,RB                                                            
         XIT1                                                                   
*                                                                               
SUTTAB   DS    0X                                                               
         DC    AL1(RQ_SUT1Q),CL14'Staff summary '                               
SUTTABL  EQU   *-SUTTAB                                                         
         DC    AL1(RQ_SUT2Q),CL14'Staff detail  '                               
         DC    AL1(RQ_SUT3Q),CL14'Client summary'                               
         DC    AL1(RQ_SUT4Q),CL14'Client detail '                               
SUTTABX  DC    X'FF'                                                            
                                                                                
SUDTAB   DS    0X                                                               
         DC    AL1(RQ_SUVMQ),CL8'monthly '                                      
SUDTABL  EQU   *-SUDTAB                                                         
         DC    AL1(RQ_SUVWQ),CL8'weekly '                                       
SUBTABX  DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* AWAITING APPROVAL ROUTINE                                           *         
***********************************************************************         
         SPACE 1                                                                
AWTAPP   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*AWTAPP*'                                                    
                                                                                
         CLI   RQ_TSSRC,RQ_TSTMY   my timesheets?                               
         JE    EXITH               Yes - ignore awaiting approval               
         CLI   RQ_AWAIT,YESQ       Are we looking for my approvals              
         JNE   EXITH               No                                           
*                                  Yes - look up my approval rights             
         XC    MYBYTE1,MYBYTE1                                                  
         MVC   GAPLRUNL,=C'1R'                                                  
         MVC   GAPLRACT,X#1RACT                                                 
         XC    GAPLPARM,GAPLPARM                                                
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT1Q',TSARABUF),            +        
               ('GAPLAPPR',GAPLRUNL),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         JNE   AWTAPP02              Didn't find anything                       
         MVI   GAPLPARM,GAPLPADD     gaplst needs to know buffer not            
*                                                          empty                
AWTAPP02 GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT20Q',TSARABUF),           +        
               ('GAPLAPPR',0),('GAPLTDTE',GAPLPARM),('QTIME',0)                 
         JE    EXITY               Found something                              
         CLI   GAPLPARM,GAPLPADD   Did we find something before                 
         JNE   EXITH               No - then skip approver look up              
         J     EXITY                                                            
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET NAMES FOR ACCOUNTS AND PUT RECORD OUT TO DDLINK                 *         
***********************************************************************         
         SPACE 1                                                                
GTNMOUT  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*GTNMOU*'                                                    
         GOTOR CLINAME             get client product job names                 
         GOTOR WCNAME              get workcode description                     
         GOTOR ORDNAME             get order name                               
         GOTOR ESTNAME             get estimate name                            
         GOTOR OFDPNAME            get office dept sub-dept names               
         GOTOR OUTPTSR             put out TimeSheet records                    
                                                                                
GTNMOUY  CR    RB,RB                                                            
         J     *+6                                                              
GTNMOUN  LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD TSAR RECS OF MISSING TIME RECORDS FOR APPROVERS               *         
*                                                                               
* NOTE USES CSVKEY1, CSVKEY2 AND CSVKEY3 - TLGETAD ETC SHOULD NOT NEED          
*      CSVKEY1 ANYMORE.                                                         
***********************************************************************         
         SPACE 1                                                                
BLDATSR  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*BLDATSR'                                                      
*                                                                               
         MVC   TS#1ROFF,SPACES                                                  
         XC    TS#PEDT,TS#PEDT                                                  
GAP      USING GAPTABD,GAPAREA                                                  
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
BLDAT02  TM    ATSRERRS,TSEEOF                                                  
         BNZ   BLDATY              end of buffer                                
*                                                                               
         CLI   GAP.GAPTDAT1,GAPTT1Q    only interested in 1R entries            
         BNE   BLDAT06             so ignore any that aren't                    
         TM    GAP.GAPTSTA,GAPTSMQ     look for main entry                      
         BZ    BLDAT04                                                          
*                                                                               
         CLC   GAP.GAPTLEN,ONERL1L                                              
         JL    BLDAT03             No office or lower level                     
         CLI   TS#PTYPE,TS#PTLAS   All list calls can avoid this                
         JNH   BLDAT03             search filtering                             
         CLI   TS#PTYPE,TS#PTSMS   All summary calls can avoid this             
         JNL   BLDAT03             search filtering                             
         CLC   RQ_TTOFF,SPACES     Office filter?                               
         BNH   BLDAT03                                                          
         LLC   RF,ONERL1L                                                       
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         JNE   BLDAT04             Don't want this                              
         CLC   GAP.GAPTACT(0),RQ_TTOFF                                          
*                                                                               
         CLC   GAP.GAPTLEN,ONERL2L                                              
         JL    BLDAT03             Do we have dept level in GAP                 
         CLC   RQ_TTDEP,SPACES     Dept filter?                                 
         BNH   BLDAT03                                                          
         LA    RE,GAP.GAPTACT                                                   
         LLC   R0,ONERL1L                                                       
         AR    RE,R0                                                            
         LLC   RF,ONERL2L                                                       
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         JNE   BLDAT04             Don't want this                              
         CLC   0(0,RE),RQ_TTDEP                                                 
*                                                                               
         CLC   GAP.GAPTLEN,ONERL3L                                              
         JL    BLDAT03             NOT ENOUGH TO WORK WITH                      
         CLC   RQ_TTSUB,SPACES     OFFICE FILTER?                               
         BNH   BLDAT03                                                          
         LA    RE,GAP.GAPTACT       A(SUBDEP IN GAPTACT                         
         LLC   R0,ONERL2L                                                       
         AR    RE,R0                A(SUBDEP IN GAPTACT                         
         LLC   RF,ONERL3L                                                       
         SR    RF,R0               RF=L'SUBDEP                                  
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         JNE   BLDAT04             DON'T WANT THIS                              
         CLC   0(0,RE),RQ_TTSUB                                                 
*                                                                               
         USING OFFALD,R1                                                        
BLDAT03  L     R1,AOFFAREA                                                      
         LA    RE,1                IF 2 CHAR OFFICE PERSON CODE SHORTER         
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         BNZ   *+8                                                              
         LA    RE,0                                                             
         MVC   OFFAOFFC(0),GAP.GAPTACT                                          
         EX    RE,*-6                                                           
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         BE    BLDAT08                                                          
         DROP  R1                                                               
                                                                                
BLDAT04  GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
BLDAT06  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         B     BLDAT02                                                          
*                                                                               
BLDAT08  DS    0H                                                               
         USING ACTRECD,R2                                                       
BLDAT12  LA    R2,IOKEY            build 1R account record key                  
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(L'ACTKUNT+L'ACTKLDG),ONERUL                              
         MVC   ACTKACT,GAP.GAPTACT                                              
         MVC   CSVKEY1,ACTKEY      save key    (NOTE CSVKEY1 NOW FREE)          
                                                                                
BLDAT14  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    BLDAT18                                                          
         DC    H'0'                no errors should occur                       
                                                                                
BLDAT16  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         BE    BLDAT18                                                          
         DC    H'0'                no errors should occur                       
                                                                                
BLDAT18  CLC   CSVKEY1(ACTKACT-ACTRECD),ACTKEY                                  
         BNE   BLDAT04                                                          
                                                                                
BLDAT22  MVC   GAPAREA2,GAPAREA                                                 
         LLC   RE,GAP.GAPTLEN          for main entry save office               
         SHI   RE,1                                                             
         EX    RE,BLDAT24                                                       
         BNE   BLDAT04                                                          
         B     BLDAT26                                                          
                                                                                
BLDAT24  CLC   GAP.GAPTACT(0),ACTKACT                                           
                                                                                
BLDAT26  CLC   ACTKEY+ACTKEND(L'ACTKEY-ACTKEND),SPACES                          
         BE    BLDAT30                                                          
                                                                                
BLDAT28  MVI   ACTKEY+ACTKEND,FF   if no account read for next                  
         B     BLDAT14                                                          
                                                                                
BLDAT30  TM    ACTKSTAT,ACTSABLP   test low level account                       
         BZ    BLDAT16                                                          
*                                                                               
* POST-FILTER FOR OFFICE/DPT/SUBDEP (GAPLST ENTRY MAY BE HIGH LEVEL)            
*                                                                               
         CLI   TS#PTYPE,TS#PTLAS   All list calls can avoid this                
         JNH   BLDAT032            search filtering                             
         CLI   TS#PTYPE,TS#PTSMS   All summary calls can avoid this             
         JNL   BLDAT032            search filtering                             
         CLC   RQ_TTOFF,SPACES     Office filter?                               
         BNH   BLDAT03A                                                         
         LA    R1,ACTKACT                                                       
         LLC   RF,ONERL1L          L'OFFICE CODE                                
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         JNE   BLDAT16             DON'T WANT THIS                              
         CLC   0(0,R1),RQ_TTOFF                                                 
*                                                                               
         CLC   RQ_TTDEP,SPACES     DEPT FILTER?                                 
         BNH   BLDAT032                                                         
         AHI   RF,1                                                             
         AR    R1,RF               A(DEPT)                                      
         LLC   RF,ONERL2L                                                       
         LLC   RE,ONERL1L                                                       
         SR    RF,RE               L'DEPT                                       
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         JNE   BLDAT16             DON'T WANT THIS                              
         CLC   0(0,R1),RQ_TTDEP                                                 
*                                                                               
         CLC   RQ_TTSUB,SPACES     SUBDPT FILTER?                               
         BNH   BLDAT03A                                                         
         AHI   RF,1                                                             
         AR    R1,RF               A(DEPT)                                      
         LLC   RF,ONERL3L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RF,RE               L'DEPT                                       
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         JNE   BLDAT16             MATCH                                        
         CLC   0(0,R1),RQ_TTSUB                                                 
*                                                                               
BLDAT03A CLC   RQ_TTPRS,SPACES     PERSON CODE FILTER?                          
         BNH   BLDAT032                                                         
         LA    R1,ACTKACT                                                       
         LLC   RE,ONERL3L                                                       
         LLC   RF,ONERL4L                                                       
         AR    R1,RE               A(DEPT)                                      
         SR    RF,RE               L'DEPT                                       
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         JNE   BLDAT16             MATCH                                        
         CLC   0(0,R1),RQ_TTPRS                                                 
*                                                                               
BLDAT032 DS    0H                                                               
         CLI   RQ_TSSRC,RQ_TSTBA   BACK UP APPROVER                             
         BNE   BLDAT033                                                         
         LLC   RE,ONERL3L                                                       
         LLC   RF,ONERL4L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         LA    R1,ACTKACT                                                       
         AR    R1,RE                                                            
         CLC   0(0,R1),TS#PCOD     BACKUP MUST IGNORE ITSELF                    
         EX    RF,*-6                                                           
         BE    BLDAT16             NEXT PERSON                                  
*                                                                               
                                                                                
BLDAT033 MVC   TS#1RACT,ACTKACT                                                 
         GOTOR TT1RVGT             test against table                           
         BNE   BLDAT28                                                          
                                                                                
         MVC   TS#PRSAC,SPACES       retrieve current person                    
         XR    RE,RE                                                            
         IC    RE,ONERL3L                                                       
         LA    RE,ACTKACT(RE)                                                   
         MVC   TS#PRSAC(7),0(RE)                                                
*                                                                               
         MVC   CSVKEY3,ACTKEY      save current ACT key                         
*                                                                               
BLDAT035 GOTOR CHKMCS                                                           
         L     R4,AIO5                                                          
         USING PERTABD,R4                                                       
BLDAT037 NI    PERSTAT,X'FF'-(PERSREAD+PERSMCSU)                                
         LA    R4,PERLENQ(R4)                                                   
         CLI   0(R4),0                                                          
         BNE   BLDAT037                                                         
         DROP  R4                                                               
*                                                                               
         USING PERRECD,R2                                                       
BLDAT040 LA    R2,IOKEY            and read for person record                   
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,TS#PRSAC                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         BE    BLDAT045                                                         
         LA    R2,IOKEY                                                         
         MVC   0(L'ACTKEY,R2),CSVKEY3                                           
         B     BLDAT28                                                          
                                                                                
BLDAT045 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         BE    *+6                                                              
         DC    H'0'                Fatal error                                  
*                                                                               
         LLC   RE,ONERL1L          use current office for calendar              
         BCTR  RE,0                                                             
         MVC   TS#1ROFF,SPACES                                                  
         MVC   TS#1ROFF(0),TS#1RACT                                             
         EX    RE,*-6                                                           
         CLC   TS#COFF,TS#1ROFF                                                 
         JE    BLDAT050                                                         
*        MVC   TS#COFF,TS#1ROFF                                                 
         GOTOR CALPRDS,RUNNOPER    REBUILD CALENDAR FOR THIS OFFICE             
         JE    BLDAT049                                                         
         MVC   LP_ERROR,FULL2                                                   
         J     XERROR                                                           
*  SORT ADDED TO TRY TO AVOID CHANGING TSAR SORT                                
BLDAT049 MVC   TS#1RACT,ACTKACT-ACTKEY+CSVKEY3    CALPPER DESTROYS              
         LLC   R4,X#DITMS                           ON TRANSFER                 
         GOTO1 VXSORT,DMCB,(X'00',AIO5),(R4),PERLENQ,PERKEYQ,0                  
*                                                                               
BLDAT050 L     R2,AIO4                                                          
         USING LOCELD,RE                                                        
         LA    RE,PERRFST                                                       
*                                                                               
         MVC   X#LCOMP,SPACES      build locel style ODS                        
         USING LOCOFF,RF                                                        
         LA    RF,X#LCOMP                                                       
         XR    R1,R1               for main entry save office                   
         IC    R1,ONERL1L                                                       
         SHI   R1,1                                                             
         MVC   LOCOFF(0),TS#1RACT                                               
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         LR    R4,R1                                                            
         LA    R4,TS#1RACT(R4)                                                  
         LR    R0,R1                                                            
         IC    R1,ONERL2L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   LOCDEPT(0),0(R4)                                                 
         EX    R1,*-6                                                           
         LA    R4,1(R1,R4)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   LOCSUB(0),0(R4)                                                  
         EX    R1,*-6                                                           
         DROP  RF                                                               
*                                                                               
BLDAT054 SR    R0,R0               check the periods for which we are           
         IC    R0,LOCLN                            BrO user                     
         AR    RE,R0                                                            
*                                                                               
         CLI   LOCEL,0                                                          
         BE    BLDAT100                                                         
         CLI   LOCEL,LOCELQ                                                     
         BNE   BLDAT054                                                         
*                                                                               
         CLC   X#LCOMP,LOCOFF      is it for this location?                     
         BNE   BLDAT054                                                         
*                                                                               
         OC    LOCLOCK,LOCLOCK     HAVE WE TIMESHEET LOCK OUT DATE              
         BZ    BLDAT060            NO                                           
         OC    LOCEND,LOCEND                                                    
         BZ    BLDAT058                                                         
         CLC   LOCLOCK,LOCEND      YES - SKIP IF HIGHER THAN END DATE           
         BNL   BLDAT060                                                         
BLDAT058 CLC   LOCLOCK,TS#CSDT     DID LOCATION LOCK BEFORE START DATE          
         BL    BLDAT054            YES                                          
*                                                                               
BLDAT060 OC    LOCEND,LOCEND       CHECK WITHIN RANGE - ACTIVE?                 
         BZ    BLDAT065                                                         
*                                                                               
         CLC   LOCEND,TS#CSDT      DID LOCATION END BEFORE START DATE           
         BL    BLDAT054            YES                                          
*                                                                               
BLDAT065 CLC   LOCSTART,TS#CEDT    DID LOCATION START AFTER END DATE            
         BH    BLDAT054            YES                                          
*                                                                               
BLDAT068 L     R4,AIO5             GET PERIODS FOR THIS DATE RANGE              
         USING PERTABD,R4                                                       
BLDAT070 CLI   TS#PTYPE,TS#PTSMS   SUMMARIES ALWAYS PUT OUT ZERO LINES          
         BNL   BLDAT094                 was BLDAT100                            
         XR    RF,RF                                                            
         ICM   RF,7,PERENDT                                                     
         LNR   RF,RF                                                            
         STCM  RF,7,X#PEREND                                                    
         CLC   X#PEREND,LOCSTART                                                
         BL    BLDAT097                                                         
         OC    LOCLOCK,LOCLOCK                                                  
         BZ    BLDAT075                                                         
         OC    LOCEND,LOCEND                                                    
         BZ    BLDAT074                                                         
         CLC   LOCLOCK,LOCEND                                                   
         BNL   BLDAT075                                                         
BLDAT074 CLC   PERSTDT,LOCLOCK                                                  
         BH    BLDAT097                                                         
BLDAT075 OC    LOCEND,LOCEND                                                    
         BZ    *+14                                                             
         CLC   PERSTDT,LOCEND                                                   
         BH    BLDAT097                                                         
*                                                                               
BLDAT078 L     R1,ABRATAB                                                       
         OC    0(L'GDADATE,R1),0(R1)                                            
         BZ    BLDAT097                                                         
BLDAT080 OC    0(L'GDADATE,R1),0(R1)                                            
         BZ    BLDAT090                                                         
         CLC   X#PEREND,0(R1)                                                   
         BL    BLDAT087                                                         
         OC    L'GDADATE(L'GDADATE,R1),L'GDADATE(R1)                            
         BZ    BLDAT083                                                         
         CLC   X#PEREND,L'GDADATE(R1)                                           
         BNL   BLDAT085                                                         
BLDAT083 OI    PERSTAT,PERSMCSU                                                 
         B     BLDAT087                                                         
*                                                                               
BLDAT085 NI    PERSTAT,X'FF'-PERSMCSU                                           
BLDAT087 LA    R1,L'GDADATE+L'GDADATE2(R1)                                      
         B     BLDAT080                                                         
*                                                                               
BLDAT090 TM    PERSTAT,PERSMCSU    ARE THEY AN MCS USER                         
         BZ    BLDAT097            NO - THEN DON'T READ FOR THIS                
BLDAT094 OI    PERSTAT,PERSREAD    SET TO READ FOR THIS PERIOD                  
                                                                                
BLDAT097 LA    R4,PERLENQ(R4)                                                   
         CLI   0(R4),0                                                          
         BE    BLDAT054                                                         
         B     BLDAT070                                                         
         DROP  RE                                                               
*                                                                               
BLDAT100 L     R4,AIO5                                                          
BLDAT103 TM    PERSTAT,PERSREAD    HAVE WE ANY PERIODS TO READ                  
         BNZ   BLDAT110            YES - ONE IS ENOUGH                          
*        CLI   TS#PTYPE,TS#PTSMS   SUMMARIES ALWAYS PUT OUT ZERO LINES          
*        BNL   BLDAT110                                                         
BLDAT105 LA    R4,PERLENQ(R4)      NO - CHECK REST OF TABLE                     
         CLI   0(R4),0             HAVE WE REACHED THE END OF TABLE             
         BNE   BLDAT103            NO                                           
*                                                                               
BLDAT108 LA    R2,IOKEY                                                         
         MVC   0(L'ACTKEY,R2),CSVKEY3                                           
         B     BLDAT28             read high for next account                   
*                                                                               
         USING TDTPASD,R3                                                       
BLDAT110 LA    R3,IOKEY            read for entry                               
         XC    TDTPAS,TDTPAS                                                    
         MVI   TDTPTYP,TDTPTYPQ                                                 
         MVI   TDTPSUB,TDTPSUBQ                                                 
         MVC   TDTPCPY,CUXCPY                                                   
         MVC   TDTPPEDT,PERENDT                                                 
         MVC   TDTPODSP,TS#1RACT                                                
         OC    TDTPODSP,SPACES                                                  
                                                                                
         MVC   CSVKEY2,IOKEY       save key                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   CSVKEY2(TDTPCULA-TDTPASD),TDTPASD                                
         BE    BLDAT105             REAL RECORD, DON'T NEED DUMMY.              
         USING TD#TABD,R3                                                       
         L     R3,AIO1                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO1                                        
         MVC   TD#STAD,PERSTDT                                                  
         XR    RE,RE                                                            
         ICM   RE,7,PERENDT                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,X#PEREND                                                    
         MVC   TD#ENDD,X#PEREND                                                 
         XC    TD#TTYE,TD#TTYE                                                  
         CLI   TS#PTYPE,TS#PTSWS   staff summary weekly                         
         BNE   BLDAT115                                                         
         MVC   TD#ENDD,TD#STAD                                                  
         MVC   X#WRKDT,TD#STAD                                                  
BLDAT115 MVC   TD#PNUM,PERNUM                                                   
         MVC   TD#PERC,TS#PRSAC                                                 
         MVC   TD#OFFC(L'TD#OFFC+L'TD#DEPT+L'TD#SBDP),SPACES                    
         XR    R1,R1               for main entry save office                   
         IC    R1,ONERL1L                                                       
         SHI   R1,1                                                             
         MVC   TD#OFFC(0),TS#1RACT                                              
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         LA    RE,TS#1RACT                                                      
         AR    RE,R1                                                            
         LR    R0,R1                                                            
         IC    R1,ONERL2L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   TD#DEPT(0),0(RE)                                                 
         EX    R1,*-6                                                           
         LA    RE,1(R1,RE)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   TD#SBDP(0),0(RE)                                                 
         EX    R1,*-6                                                           
         MVI   TD#TYP,TD#1RQ                                                    
         ZAP   TD#HOUR,PZERO                                                    
         ZAP   TD#CLIHR,PZERO                                                   
         ZAP   TD#MATRS,PZERO                                                   
         MVI   TD#STTS,TS#NSTRT                                                 
         MVI   TD#STTV,TS#NSTRT                                                 
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CLC   TD#ENDD,X#CUTDT     is it overdue                                
         BNL   BLDAT117            no                                           
         MVI   TD#STTV,TS#OVERD    yes                                          
         CHI   RF,XPRODIKQ          app is connected                            
         JE    BLDAT117                                                         
         MVI   TD#STTS,TS#OVERD                                                 
                                                                                
BLDAT117 CLI   TS#PTYPE,TS#PTRB    back up approver search                      
         BNL   BLDAT120                                                         
         CLI   TS#PTYPE,TS#PTRTP   skip for person timsheet search              
         BE    BLDAT120                                                         
         CLC   RQ_TLST,SPACES                                                   
         BNH   BLDAT123                                                         
         CLC   RQ_TLST,TD#STTV     Does it match requested status               
         BE    BLDAT123            Yes                                          
         CHI   RF,XPRODIKQ         Aura or BrandOcean                           
         BNE   BLDAT105            BrandOcean                                   
         CLC   RQ_TLST,TD#STTS     For Aura we check both statuses              
         BE    BLDAT123                                                         
         B     BLDAT105            No match                                     
*                                                                               
BLDAT120 LA    R1,X#REQST                                                       
BLDAT121 CLI   0(R1),X'FF'                                                      
         BE    BLDAT105                                                         
         CLC   0(1,R1),TD#STTV                                                  
         BE    BLDAT123                                                         
         CHI   RF,XPRODIKQ         Aura or BrandOcean                           
         BNE   BLDAT122            BrandOcean                                   
         CLC   0(1,R1),TD#STTS                                                  
         BE    BLDAT123                                                         
BLDAT122 LA    R1,1(R1)                                                         
         B     BLDAT121                                                         
*                                                                               
BLDAT123 CLI   TS#PTYPE,TS#PTSMS   MONTHLY SUMMARIES use sumtab for             
         BE    BLDAT135                                     dates               
         CLI   TS#PTYPE,TS#PTSMP                                                
         BE    BLDAT135                                                         
         CLI   TS#PTYPE,TS#PTSMA                                                
         BE    BLDAT135                                                         
         CLI   TS#PTYPE,TS#PTSMC                                                
         BE    BLDAT135                                                         
*                                                                               
         CLI   TS#PTYPE,TS#PTSWS   staff summary weekly                         
         BNE   BLDAT125                                                         
         CLC   X#WRKDT,TS#CSDT     don't want days earlier in period            
         BL    BLDAT130                                                         
BLDAT125 SR    R1,R1                                                            
         ICM   R1,7,X#ITEMS                                                     
         AHI   R1,1                                                             
         STCM  R1,7,X#ITEMS                                                     
         MVC   TD#SEQ,X#ITEMS                                                   
*                                                                               
         GOTOR (#MTSAR,AMTSAR),DMCB,TSAADD,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    BLDAT130                                                         
         TM    TSARERRS,X'80'                                                   
         BO    *+6                 TOO MANY ITEMS                               
         DC    H'0'                BCTSERRS=X'20'=DUPLICATE KEY                 
         MVC   FULL2(2),=AL2(AE$NASRC)                                          
         B     BLDATN                                                           
*                                                                               
BLDAT130 CLI   TS#PTYPE,TS#PTSWS   staff summary weekly                         
         BNE   BLDAT105                                                         
         CLC   X#WRKDT,X#PEREND    needs one per day                            
         BNL   BLDAT105                                                         
         GOTO1 VDATCON,DMCB,(1,X#WRKDT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,F'1'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,X#WRKDT)                              
         MVC   TD#ENDD,X#WRKDT                                                  
         MVC   TD#STAD,X#WRKDT                                                  
         CLC   X#WRKDT,TS#CEDT     don't want days later in period              
         BH    BLDAT105                                                         
         B     BLDAT123                                                         
*                                                                               
BLDAT135 DS    0H                                                               
         LA    R2,X#SUMTAB                                                      
         USING SUMTABD,R2                                                       
         MVC   TEMP2(L'TD#STAD),TD#STAD                                         
         MVC   TEMP2+L'TD#STAD(L'TD#ENDD),TD#ENDD                               
         CLC   STSTART,TD#STAD    CHECK IF LATEST PERTAB PERIOD FOR             
         BNH   BLDAT140           SUMTAB RANGE.EXCEPTION TO LOGIC BELOW         
         CLC   STEND,TD#ENDD                                                    
         BL    BLDAT150                                                         
*                                                                               
BLDAT140 CLI   STSTART,0          ADD A RECORD FOR EVERY SUMTAB PERIOD          
         BE    BLDAT105             STARTING IN THIS T/S PERIOD                 
         CLC   STSTART,TEMP2                                                    
         BL    BLDAT145                                                         
         CLC   STSTART,TEMP2+L'TD#STAD                                          
         BNH   BLDAT150                                                         
*                                                                               
BLDAT145 LA    R2,SUMTABL(R2)                                                   
         B     BLDAT140                                                         
*                                                                               
BLDAT150 MVC   TD#STAD,STSTART                                                  
         MVC   TD#ENDD,STEND                                                    
         SR    R1,R1                                                            
         ICM   R1,7,X#ITEMS                                                     
         AHI   R1,1                                                             
         STCM  R1,7,X#ITEMS                                                     
         MVC   TD#SEQ,X#ITEMS                                                   
                                                                                
         GOTOR (#MTSAR,AMTSAR),DMCB,TSAADD,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    BLDAT145            NEXT SUMTAB                                  
         TM    TSARERRS,X'80'                                                   
         BO    *+6                 TOO MANY ITEMS                               
         DC    H'0'                BCTSERRS=X'20'=DUPLICATE KEY                 
         MVC   FULL2(2),=AL2(AE$NASRC)                                          
         B     BLDATN                                                           
         DROP  R2                                                               
*                                                                               
BLDATY   DS    0H                                                               
         LA    R2,IOKEY            RESTORE CONNECTED PERSON REC                 
         USING PERRECD,R2                                                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,TS#PCOD                                                 
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         BE    *+6                                                              
         DC    H'0'                Fatal error                                  
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         BE    *+6                                                              
         DC    H'0'                Fatal error                                  
         CR    RB,RB                                                            
         J     *+6                                                              
*                                                                               
BLDATN   LTR   RB,RB                                                            
         XIT1                                                                   
         DROP  R2,R3,R4,GAP                                                     
         LTORG                                                                  
***********************************************************************         
* BUILD TSAR RECS OF MISSING TIME RECORDS FOR CLIENTS                 *         
*                                                                               
*   FOR SUMMARY DETAIL FILLS IN THE GAPS IN X#SUMTAB/PERTAB                     
*         FOR GAPLST ENTRY                                                      
*   FOR ALL-CLIENT SUMMARY, CALLED ONCE TO BUILD BLANK ENTRIES FOR ALL          
*        CLIENT CODES (AGAPAREA TABLE) AND X#SUMTAB PERIODS                     
***********************************************************************         
         SPACE 1                                                                
BLDCTSR  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*BLDCTSR'                                                      
                                                                                
         USING SUMTABD,R2                                                       
         LA    R2,X#SUMTAB         R2=A(LIST OF PERIODS                         
         L     R4,AGAPAREA         LIST OF CLIENTS WE'VE FOUND                  
         USING CLITABD,R4                                                       
         CLI   TS#PTYPE,TS#PTSMA                                                
         JE    BLDCT02                                                          
         CLI   TS#PTYPE,TS#PTSWA                                                
         JE    BLDCT02                                                          
         LA    R4,L'PERHVIEW+L'PERHACC(R4)  DETAIL HAS HEADER                   
*                                                                               
BLDCT02  L     R3,AIO1                 R3 =A(TSAR                               
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO1                                        
         CLI   GAPLCALT,GAPLALLC                                                
         BE    BLDCT07                                                          
         CLI   TS#PTYPE,TS#PTSMA                                                
         JE    BLDCT25                                                          
         CLI   TS#PTYPE,TS#PTSWA                                                
         JE    BLDCT25                                                          
*                                                                               
*   HERE FOR DETAIL CALLS, OR SUMMARY IF NO LIMLIST (USES CLITABD)              
         USING TD#TABD,R3                                                       
BLDCT07  MVC   TD#STAD,STSTART                                                  
         MVC   TD#ENDD,STEND                                                    
         MVI   TD#TYP,TD#SJ1NQ                                                  
*                                                                               
         MVC   TD#ACC,SPACES                                                    
         LLC   RE,PCLILEN                                                       
         CLI   CLIVIEW,TSJP1NAQ                                                 
         JNE   *+8                                                              
         LA    RE,L'TSJPACT                                                     
         SHI   RE,1                                                             
         MVC   TD#ACC(0),CLICDE                                                 
         EX    RE,*-6                                                           
         MVC   TD#UNT(L'PRODUL),PRODUL                                          
         CLI   CLIVIEW,TSJP1NAQ                                                 
         JNE   BLDCT10                                                          
         MVC   TD#UNT(2),ONENUL                                                 
*                                                                               
BLDCT10  DS    0H                                                               
         CLI   TS#PTYPE,TS#PTSMC                                                
         JL    BLDCT40                                                          
* Client detail uses different table structure                                  
         LR    RF,R4                                                            
         L     R4,AGAPAREA                                                      
         MVC   TD#UNT(L'PRODUL),PRODUL                                          
         CLI   PERHVIEW,TSJP1NAQ                                                
         JNE   *+10                                                             
         MVC   TD#UNT(2),ONENUL                                                 
         MVC   TD#ACC,PERHACC                                                   
         LR    R4,RF                                                            
*                                                                               
         MVC   TD#PERC,PERCDE         detail call only here                     
         CLI   TS#PTYPE,TS#PTSMA                                                
         JNL   BLDCT40                                                          
         MVC   TD#OFFC,PERCOFC        only staff needs location                 
         MVC   TD#DEPT,PERCDPT                does that even get here?          
         MVC   TD#SBDP,PERCSDP                                                  
         B     BLDCT40                                                          
*                                                                               
*                                                                               
*    SET ACCOUNT FROM GAPAREA (CLIENT TOTALS IF LIMLIST/APPROVER REC)           
         USING GAPTABD,R4                                                       
BLDCT25  MVI   TD#TYP,TD#SJ1NQ                                                  
         LA    R4,GAPAREA                                                       
         TM    STSTAT,STSTFUND     DATA FOR PERIOD?                             
         BNZ   BLDCT75             YES, SKIP                                    
*                                                                               
         MVC   TD#STAD,STSTART                                                  
         MVC   TD#ENDD,STEND                                                    
         CLI   GAPTDAT1,GAPTT3Q    COPY GAPLST CLI AND DERIVE U/L               
         BNE   BLDCT30                                                          
         MVC   TD#ACC,GAPTACT                                                   
         MVC   TD#UNT(2),ONENUL                                                 
         B     BLDCT40                                                          
BLDCT30  CLI   GAPTDAT1,GAPTT2Q                                                 
         BNE   BLDCT35                                                          
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   TD#ACC(0),GAPTACC                                                
         EX    RE,0(RF)                                                         
         OC    TD#ACC,SPACES                                                    
         MVC   TD#UNT(2),PRODUL                                                 
         B     BLDCT40                                                          
*                                                                               
BLDCT35  CLI   GAPTDAT1,GAPTT6Q                                                 
         BNE   BLDCT40                                                          
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   TD#ACC(0),GAPTCPJ                                                
         EX    RE,0(RF)                                                         
         OC    TD#ACC,SPACES                                                    
         MVC   TD#UNT(2),PRODUL                                                 
*                                                                               
*  ALL CALLS COME HERE TO ADD DUMMIES                                           
BLDCT40  ZAP   TD#HOUR,PZERO                                                    
         ZAP   TD#CLIHR,PZERO                                                   
         ZAP   TD#MATRS,PZERO                                                   
         XC    TD#TTYE,TD#TTYE                                                  
         MVC   TD#ORD#,FFS                                                      
         MVI   TD#STTS,TS#NSTRT                                                 
         MVI   TD#STTV,TS#NSTRT                                                 
         CLC   TD#ENDD,X#CUTDT     is it overdue                                
         BNL   BLDCT50             no                                           
         MVI   TD#STTV,TS#OVERD    yes                                          
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         JE    BLDCT50                                                          
         MVI   TD#STTS,TS#OVERD                                                 
*                                                                               
BLDCT50  DS    0H                                                               
BLDCT55  SR    R1,R1                                                            
         ICM   R1,7,X#ITEMS                                                     
         AHI   R1,1                                                             
         STCM  R1,7,X#ITEMS                                                     
         MVC   TD#SEQ,X#ITEMS                                                   
         GOTOR (#MTSAR,AMTSAR),DMCB,TSAADD,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    BLDCT75                                                          
         TM    TSARERRS,X'80'                                                   
         BO    *+6                 TOO MANY ITEMS                               
         DC    H'0'                BCTSERRS=X'20'=DUPLICATE KEY                 
         MVC   FULL2(2),=AL2(AE$NASRC)                                          
         B     BLDCTN                                                           
*                                                                               
         USING PERTABD,R2                                                       
BLDCT75  DS    0H                  TOTALS USING GAPLST ENTRY?                   
         CLI   GAPLCALT,GAPLALLC                                                
         BE    BLDCT80                                                          
         CLI   TS#PTYPE,TS#PTSMA                                                
         JE    BLDCT76                                                          
         CLI   TS#PTYPE,TS#PTSWA                                                
         JNE   BLDCT80                                                          
BLDCT76  LA    R2,SUMTABL(R2)                                                   
         CLI   0(R2),0                                                          
         JNE   BLDCT02             NEXT PERIOD FOR CLIENT CODE                  
         J     BLDCTY                                                           
*                                                                               
         USING CLITABD,R4                                                       
BLDCT80  CLI   TS#PTYPE,TS#PTSMA   TOTALS RUNNING OFF CLICDE?                   
         JE    BLDCT81                       (NO LIMLIST/APPROVER REC)          
         CLI   TS#PTYPE,TS#PTSWA                                                
         JNE   BLDCT85                                                          
BLDCT81  LA    R2,SUMTABL(R2)                                                   
         CLI   0(R2),0                                                          
         BNE   BLDCT02             NEXT PERIOD FOR THIS PERSON                  
*                                                                               
         LLC   RE,PCLILEN                                                       
         LA    RE,L'CLIVIEW(RE)      short length (view+cli)                    
         CLI   CLIVIEW,TSJP1NAQ                                                 
         JNE   *+8                                                              
         LA    RE,CLICLEN          1n length                                    
         AR    R4,RE               NEXT CLIENT, 1ST PERIOD                      
*                                                                               
         CLI   0(R4),GAPTEOT                                                    
         JE    BLDCTY              NO MORE CLIENTS                              
         LA    R2,X#SUMTAB                                                      
         B     BLDCT02                                                          
                                                                                
BLDCT85  LA    R2,SUMTABL(R2)      DETAIL CALLS                                 
         CLI   0(R2),0                                                          
         BNE   BLDCT02             NEXT PERIOD FOR THIS PERSON                  
         LA    R4,PERCPODS(R4)     NEXT PERSON, 1ST PERIOD                      
         CLI   0(R4),GAPTEOT                                                    
         JE    BLDCTY              NO MORE PEOPLE                               
         LA    R2,X#SUMTAB                                                      
         B     BLDCT02                                                          
*                                                                               
BLDCTY   CR    RB,RB                                                            
         J     *+6                                                              
BLDCTN   LTR   RB,RB                                                            
         XIT1                                                                   
         DROP  R2,R3,R4                                                         
         LTORG                                                                  
***********************************************************************         
* FOR MY TIMESHEETS OR FOR ONE PERSON - FIND NOT STARTED/OVERDUE      *         
* READ PERIODS BACK IN FROM AIO5 AND ADD RECORDS TO TSAR BUFFER       *         
***********************************************************************         
         SPACE 1                                                                
CHKSTAB  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*CHKSTAB'                                                      
                                                                                
         USING TD#TABD,R4                                                       
         L     R4,AIO1                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO1                                        
         L     R3,AIO5                                                          
         LA    R2,X#SUMTAB                                                      
                                                                                
CHKST02  DS    0H                                                               
         CLI   TS#PTYPE,TS#PTSMS   WHAT KIND OF CALL?                           
         BL    CHKST20                                                          
*                  SUMMARY DATE HANDLING                                        
         USING SUMTABD,R2                                                       
         TM    STSTAT,STSTFUND                                                  
         JNZ   CHKST60             HAVE REAL TIME                               
         MVC   TD#STAD,STSTART                                                  
         MVC   TD#ENDD,STEND                                                    
         J     CHKST30                                                          
         DROP  R2                                                               
*                  LIST/SEARCH DATE HANDLING                                    
         USING PERTABD,R3                                                       
CHKST20  XC    BYTE1,BYTE1                                                      
         TM    PERSTAT,PERSFOND+PERSSKIP+PERSINVL                               
         BNZ   CHKST60                                                          
         TM    PERSTAT,PERSNFND                                                 
         BZ    CHKST60                                                          
         TM    PERSTAT,PERSMCSU    list and search don't bring back             
         BZ    CHKST60                      zero lines                          
*                                                                               
         MVC   TD#STAD,PERSTDT     LINE TO PROCESS                              
         XR    RF,RF                                                            
         ICM   RF,7,PERENDT                                                     
         LNR   RF,RF                                                            
         STCM  RF,7,TD#ENDD                                                     
         MVC   TD#PNUM,PERNUM                                                   
         CLI   TS#PTYPE,TS#PTSWP                                                
         BNE   CHKST30                                                          
         MVC   TD#ENDD,TD#STAD                                                  
CHKST30  XC    TD#TTYE,TD#TTYE                                                  
         MVC   TD#PERC,TS#PRSAC                                                 
         MVC   TD#OFFC(L'TD#OFFC+L'TD#DEPT+L'TD#SBDP),SPACES                    
         LA    RE,1                IF 2 CHAR OFFICE PERSON CODE SHORTER         
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         BNZ   *+8                                                              
         LA    RE,0                                                             
         MVC   TD#OFFC(0),PERODS                                                
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         LA    RF,PERODS                                                        
         AR    RF,RE                                                            
         SR    R1,R1                                                            
         IC    R1,ONERL2L                                                       
         SR    R1,RE                                                            
         SHI   R1,1                                                             
         MVC   TD#DEPT(0),0(RF)                                                 
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   TD#SBDP(0),0(RF)                                                 
         EX    R1,*-6                                                           
         ZAP   TD#HOUR,PZERO                                                    
         ZAP   TD#CLIHR,PZERO                                                   
                                                                                
         ZAP   TD#MATRS,PZERO                                                   
         MVI   TD#STTV,TS#NSTRT    Not started                                  
         MVI   TD#STTS,TS#NSTRT                                                 
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        RF=Contains Application connected            
         CLC   TD#ENDD,X#CUTDT     is it overdue                                
         BNL   CHKST38             no                                           
         MVI   TD#STTV,TS#OVERD    yes                                          
         CHI   RF,XPRODIKQ         Is BrandOcean or Aura connected              
         JE    CHKST38             Aura                                         
         MVI   TD#STTS,TS#OVERD                                                 
CHKST38  CLI   TS#PTYPE,TS#PTRTP                                                
         BNL   CHKST40                                                          
         CLC   RQ_TLST,SPACES                                                   
         BNH   CHKST45                                                          
         CLC   RQ_TLST,TD#STTV     Does it match requested status               
         BNE   CHKST60                                                          
         B     CHKST45                                                          
*                                                                               
CHKST40  LA    R1,X#REQST                                                       
CHKST42  CLI   0(R1),X'FF'                                                      
         BE    CHKST60                                                          
         CLC   0(1,R1),TD#STTV                                                  
         BE    CHKST45                                                          
         CHI   RF,XPRODIKQ                                                      
         JNE   CHKST44                                                          
         CLC   0(1,R1),TD#STTS     For Aura check both statuses                 
         BE    CHKST45                                                          
CHKST44  LA    R1,1(R1)                                                         
         B     CHKST42                                                          
*                                                                               
CHKST45  SR    R1,R1                                                            
         ICM   R1,7,X#ITEMS                                                     
         AHI   R1,1                                                             
         STCM  R1,7,X#ITEMS                                                     
         MVC   TD#SEQ,X#ITEMS                                                   
         GOTOR (#MTSAR,AMTSAR),DMCB,TSAADD,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    CHKST60                                                          
         TM    TSARERRS,X'80'                                                   
         BO    *+6                 TOO MANY ITEMS                               
         DC    H'0'                BCTSERRS=X'20'=DUPLICATE KEY                 
         MVC   FULL2(2),=AL2(AE$NASRC)                                          
         B     CHKSTN                                                           
CHKST60  CLI   TS#PTYPE,TS#PTSMS   WHAT KIND OF CALL?                           
         BNL   CHKST65               SUMMARY                                    
         LA    R3,PERLENQ(R3)        SEARCH/LIST                                
         CLI   0(R3),0                                                          
         BNE   CHKST02                                                          
         J     CHKST90                                                          
*                                                                               
CHKST65  LA    R2,SUMTABL(R2)                                                   
         CLI   0(R2),0                                                          
         BNE   CHKST02                                                          
*           DONE                                                                
CHKST90  DS    0H                                                               
CHKSTY   CR    RB,RB                                                            
         J     *+6                                                              
CHKSTN   LTR   RB,RB                                                            
         XIT1                                                                   
         DROP  R4,R3                                                            
                                                                                
         LTORG                                                                  
***********************************************************************         
* PROCESS TIMESHEET POINTERS AND PASS DATA INTO TSAR BUFFER           *         
***********************************************************************         
         SPACE 1                                                                
PROPTSR  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*PROPTSR'                                                      
         USING TD#TABD,R4                                                       
         L     R4,AIO1                                                          
         LA    R2,IOKEY                                                         
         USING TDTPASD,R2                                                       
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO1                                        
*                                                                               
         TM    TS#ICPJ,TS#IOFFR                                                 
         BZ    PRTSR008                                                         
         SR    RF,RF                                                            
         IC    RF,ONERL1L                                                       
         TM    TS#ICPJ,TS#IDPTR                                                 
         BZ    PRTSR002                                                         
         IC    RF,ONERL2L                                                       
         TM    TS#ICPJ,TS#ISDPR                                                 
         BZ    PRTSR002                                                         
         IC    RF,ONERL3L                                                       
*                                                                               
PRTSR002 SHI   RF,1                                                             
         LA    RE,TDTPODSP                                                      
         CLI   TDTPSUB,TDTPSUBQ    Time date passive                            
         BE    PRTSR004                                                         
                                                                                
         LA    RE,IOKEY+(TSJPPODS-TSJPASD)                                      
         LLC   R1,ONERL3L                                                       
         LLC   R3,ONERL4L                                                       
         SR    R3,R1               R3=length of person code                     
         AR    RE,R3               RE=A(office dept sub)                        
         CLI   TDTPSUB,TSJPSUBQ    Time account passive                         
         BE    PRTSR004                                                         
         LA    RE,IOKEY+(TSWKODS-TSWRECD)                                       
         CLI   TDTPSUB,TSWKSUBQ    Time person passive                          
         BE    PRTSR004                                                         
*                                                                               
         CLI   TDTPSUB,TAPPSUBQ    Time status passive                          
         BE    *+6                                                              
         DC    H'0'                Unknown type - die                           
         LA    RE,IOKEY+(TAPMODSP-TAPPASD)                                      
         CLI   IOKEY+(TAPPCAT-TAPPASD),TAPPMAN                                  
         BE    PRTSR004                                                         
         LA    RE,IOKEY+(TAPCPODS-TAPPASD)                                      
         LLC   R1,ONERL3L                                                       
         LLC   R3,ONERL4L                                                       
         SR    R3,R1               R3=length of person code                     
         AR    RE,R3               RE=A(office dept sub)                        
                                                                                
PRTSR004 EX    RF,PRTSR006                                                      
         BE    PRTSR008                                                         
         B     PROPTSRX            not valid office                             
*                                                                               
PRTSR006 CLC   0(0,RE),X#1RACT                                                  
*                                                                               
PRTSR008 TM    TS#ICPJ,TS#IPERR    Do we have a person code                     
         BZ    PRTSR013            No                                           
         LLC   R3,ONERL3L                                                       
         LLC   R1,ONERL4L                                                       
         SR    R1,R3               R1=length of person code                     
         SHI   R1,1                                                             
         LA    RE,TDTPODSP                                                      
         AR    RE,R3                                                            
         CLI   TDTPSUB,TDTPSUBQ    Time date passive                            
         BE    PRTSR010                                                         
         LA    RE,IOKEY+(TSJPPODS-TSJPASD)                                      
         CLI   TDTPSUB,TSJPSUBQ    Time account passive                         
         BE    PRTSR010                                                         
         LA    RE,IOKEY+(TSWKPER-TSWRECD)                                       
         CLI   TDTPSUB,TSWKSUBQ    Time person passive                          
         BE    PRTSR010                                                         
                                                                                
         CLI   TDTPSUB,TAPPSUBQ    Time status passive                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,IOKEY+(TAPMODSP-TAPPASD)                                      
         AR    RE,R3                                                            
         CLI   IOKEY+(TAPPCAT-TAPPASD),TAPPMAN                                  
         BE    PRTSR010                                                         
         LA    RE,IOKEY+(TAPCPODS-TAPPASD)                                      
                                                                                
PRTSR010 EX    R1,PRTSR012                                                      
         BE    PRTSR013                                                         
         B     PROPTSRX            not valid office                             
*                                                                               
PRTSR012 CLC   0(0,RE),X#1RPER                                                  
*                                                                               
PRTSR013 MVC   TD#ORD#,FFS                                                      
         MVC   TEMP2(3),TS#PEDT    get period # and dates                       
         GOTOR RETCPFD                                                          
         LA    RF,TEMP2                                                         
         USING PERTABD,RF                                                       
         MVC   TD#STAD,PERSTDT     and pass them on                             
         MVC   TD#ENDD,TS#PEDT                                                  
         MVC   TD#PNUM,PERNUM                                                   
         DROP  RF                                                               
         CLI   TS#PTYPE,TS#PTSMA   for client summary don't save person         
         BE    *+10                                                             
         MVC   TD#PERC,TS#PRSAC                                                 
         CLI   TS#PTYPE,TS#PTSMA                                                
         BE    PRTSR014                                                         
         CLI   TS#PTYPE,TS#PTSMC                                                
         BNE   PRTSR022                                                         
PRTSR014 XR    RF,RF                                                            
         IC    RF,PJOBLEN                                                       
*                                                                               
PRTSR016 DS    0H                                                               
         USING TSJPASD,R2                                                       
         CLI   TSJPSUB,TSJPSUBQ                                                 
         BE    *+6                                                              
         DC    H'0'                UNKNOWN PASSIVE TYPE                         
         MVC   TD#ACC,SPACES                                                    
         LLC   RF,PCLILEN                                                       
         CLI   TS#PTYPE,TS#PTSMC                                                
         BNE   PRTSR018                                                         
         LLC   RF,PJOBLEN                                                       
PRTSR018 BCTR  RF,0                                                             
         MVI   TD#UNT,C'S'                                                      
         MVI   TD#LDG,C'J'                                                      
         MVC   WORK,SPACES                                                      
         MVC   WORK(2),TD#UNT       Save off TSJPACT for later compare          
         MVC   WORK+2(L'TSJPACT),TSJPACT                                        
         LA    RE,TSJPACT                                                       
         CLI   TSJPVIEW,TSJPSJAQ                                                
         BE    PRTSR020                                                         
         LA    RE,TSJPMACT                                                      
         CLI   TSJPVIEW,TSJP1NAQ                                                
         BNE   PRTSR020                                                         
         MVI   TD#UNT,C'1'                                                      
         MVI   TD#LDG,C'N'                                                      
         LA    RE,TSJP1NAC                                                      
         LA    RF,L'TSJP1NAC-1                                                  
PRTSR020 MVC   TD#ACC(0),0(RE)                                                  
         EX    RF,*-6                                                           
         DROP  R2                                                               
*                                                                               
PRTSR022 MVC   TD#TYP,TS#ATYP                                                   
         CLI   TS#PTYPE,TS#PTSMA                                                
         BE    PRTSR024                                                         
         CLI   TS#PTYPE,TS#PTSWA                                                
         BE    PRTSR024                                                         
         MVC   TD#OFFC,TS#1ROFF    NO PERSON DETS FOR CLIENT SUMM               
         MVC   TD#DEPT,TS#1RDPT                                                 
         MVC   TD#SBDP,TS#1RSDP                                                 
*                                                                               
PRTSR024 ZAP   TD#HOUR,X#SVHRS                                                  
         ZAP   TD#CLIHR,PZERO                                                   
         ZAP   X#SVCHR,PZERO                                                    
         ZAP   TD#MATRS,X#SVMTS                                                 
         GOTOR SETSSTA,0                                                        
         MVC   TD#STTS,X#STAT                                                   
         GOTOR CHKAPP                                                           
         JNE   PRTSR026                                                         
         GOTOR SETASTA,DMCB,TS#STAT       set approver status                   
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         JE    PRTSR026                                                         
         MVC   TD#STTS,X#STAT                                                   
PRTSR026 MVC   TD#STTV,X#STAT                                                   
*&&UK                                                                           
         CLI   TS#PTYPE,TS#PTLMD   List My timesheets by Date                   
         BE    *+12                                                             
         CLI   TS#PTYPE,TS#PTLMS   List My timesheets by Status                 
         BNE   PRTSR027                                                         
         TM    SCPYEL+(CPYSTATD-CPYELD),CPYSRSTS                                
         BZ    PRTSR027            No restrict T/S by status                    
         CLI   TD#STTV,TS#APPRO    Is the timesheet approved                    
         BE    PROPTSRX            yes - don't add record to buffer             
         CLI   TD#STTV,TS#PAAPR    Is the timesheet part approved               
         BE    PROPTSRX            yes - don't add record to buffer             
*&&                                                                             
PRTSR027 CLI   TD#STTV,TS#PROGR    Is the timesheet in progress                 
         BNE   PRTSR052            No                                           
         CLC   TD#ENDD,X#CUTDT     Yes - it could be overdue                    
         BNL   PRTSR028            no                                           
         MVI   TD#STTV,TS#OVERD    yes - status accordingly                     
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         BE    PRTSR030                                                         
         MVI   TD#STTS,TS#OVERD                                                 
*                                                                               
PRTSR028 CLI   TS#PTYPE,TS#PTLAS   is it a search                               
         BH    PRTSR054            yes                                          
         CLC   RQ_TLST,SPACES      is this a status request                     
         BNH   PRTSR062            no                                           
         CLC   RQ_TLST,TD#STTS     does it match as required                    
         BNE   PROPTSRX            no - don't add record to buffer              
         B     PRTSR062            yes - add to buffer                          
*                                                                               
PRTSR030 LA    R1,X#REQST          is this a status requested                   
PRTSR032 CLI   0(R1),X'FF'         end of list                                  
         BE    PROPTSRX            yes - no match don't add to buffer           
         CLC   TD#STTV,0(R1)       does it match as required                    
         BE    PRTSR062            yes - add record to buffer                   
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         BNE   PRTSR028                                                         
         CLC   TD#STTS,0(R1)                                                    
         BE    PRTSR062                                                         
         LA    R1,1(R1)                                                         
         B     PRTSR032                                                         
*                                                                               
PRTSR052 CLI   TS#PTYPE,TS#PTLAS   is it a search                               
         BNH   PRTSR062            no                                           
PRTSR054 LA    R1,X#REQST          is this a status requested                   
PRTSR056 CLI   0(R1),X'FF'         end of list                                  
         BE    PROPTSRX            yes - no match don't add to buffer           
         TM    X#SRCIND,X#SRCAPP   is it an approver search                     
         BZ    PRTSR058            no                                           
         CLC   TD#STTV,0(R1)       does it match as required                    
         BE    PRTSR062            yes - add record to buffer                   
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        If Aura we need to check both                
         CHI   RF,XPRODIKQ           statuses                                   
         JNE   PRTSR060                                                         
PRTSR058 CLC   TD#STTS,0(R1)       does it match as required                    
         BE    PRTSR062            yes - add record to buffer                   
PRTSR060 LA    R1,1(R1)                                                         
         B     PRTSR056                                                         
*                                                                               
PRTSR062 CLI   TS#PTYPE,TS#PTSMS   SUMMARIES MAY NEED SPLITTING                 
         BNL   PRTSR072                                                         
*                                                                               
         MVC   IODA,TS#RECDA       SPLIT AT TIMEL LEVEL                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO2                                                          
         LA    R3,TIMRFST-TIMRECD(R2)                                           
         B     PRTSR066                                                         
*                                                                               
         USING TIMELD,R3                                                        
PRTSR064 LLC   RE,TIMLN                                                         
         AR    R3,RE                                                            
*                                                                               
PRTSR066 CLI   TIMEL,0             LOOK FOR TIMELS                              
         BE    PRTSR070            ALL DONE                                     
         CLI   TIMEL,TIMELQ                                                     
         BNE   PRTSR064                                                         
         CLI   TIMETYP,TIMEINP                                                  
         BNE   PRTSR064                                                         
PRTSR068 OC    TIMHRS,TIMHRS       MAY BE NULL                                  
         BZ    PRTSR064                                                         
         CLI   TIMTTYP,TIMTNC      is it non client time                        
         BE    PRTSR064            yes                                          
         CLI   TIMTTYP,TIMTEM      is it empty timesheet                        
         BE    PRTSR064            yes                                          
         AP    X#SVCHR,TIMHRS      add up total client time                     
         B     PRTSR064                                                         
         DROP  R3                                                               
*                                                                               
PRTSR070 ZAP   TD#CLIHR,X#SVCHR                                                 
         SR    R1,R1                                                            
         ICM   R1,7,X#ITEMS                                                     
         AHI   R1,1                                                             
         STCM  R1,7,X#ITEMS                                                     
         MVC   TD#SEQ,X#ITEMS                                                   
         GOTOR (#MTSAR,AMTSAR),DMCB,TSAADD,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    PROPTSRX                                                         
         TM    TSARERRS,X'80'                                                   
         BO    *+6                 TOO MANY ITEMS                               
         DC    H'0'                BCTSERRS=X'20'=DUPLICATE KEY                 
         MVC   ROUERRV,=AL2(AE$NASRC)                                           
         J     XERROR                                                           
*                                                                               
*  IF AGENCY USES DIFFERENT CALENDARS FOR DIFFERENT OFFICES, THIS               
*  CODE WILL SPLIT HOURS BY APPROVER PERIODS (SUMTAB), SO THEY ARE              
*  COMPATIBLE                                                                   
*                                                                               
PRTSR072 DS    0H                                                               
         LA    R2,X#SUMTAB        LOOK FOR MATCH WITH APPROVER PERIODS          
         USING SUMTABD,R2                                                       
PRTSR074 CLI   0(R2),0                                                          
         JE    PRTSR082                                                         
         CLC   STSTART,TD#STAD                                                  
         JNE   PRTSR076                                                         
         CLC   STEND,TD#ENDD                                                    
         JE    PRTSR078           MATCHES, NO NEED TO SPLIT                     
PRTSR076 CLC   STSTART,TD#STAD    PERIOD COMPLETELY WITHIN APPR PERIOD?         
         JH    PRTSR080                                                         
         CLC   STEND,TD#ENDD                                                    
         JL    PRTSR080                                                         
         MVC   TD#STAD,STSTART    YES, JUST RESET DATES AND ADD AS              
         MVC   TD#ENDD,STEND                             NORMAL                 
PRTSR078 OI    STSTAT,STSTFUND     STOP BLDCTSR ADDING A DUMMY FOR THIS         
         B     PRTSR070                                         PERIOD          
*                                                                               
PRTSR080 LA    R2,SUMTABL(R2)                                                   
         J     PRTSR074                                                         
*                                                                               
PRTSR082 DS    0H                                                               
         MVC   IODA,TS#RECDA       SPLIT AT TIMEL LEVEL                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO2                                                          
         LA    R3,TIMRFST-TIMRECD(R2)                                           
         J     PRTSR086                                                         
*                                                                               
         USING TIMELD,R3                                                        
PRTSR084 LLC   RE,TIMLN                                                         
         AR    R3,RE                                                            
*                                                                               
PRTSR086 CLI   TIMEL,0             LOOK FOR TIMELS                              
         JE    PROPTSRX            ALL DONE                                     
         CLI   TIMEL,TIMELQ                                                     
         JNE   PRTSR084                                                         
         CLI   TIMETYP,TIMEINP     Find INPUT DETAIL first                      
         JNE   PRTSR084                                                         
         CLC   TIMACC,WORK         Make sure it's the same CLI/ACC              
         JNE   PRTSR084                                                         
*                                                                               
         LLC   RE,TIMLN            Look for TIMETIME now                        
         AR    R3,RE                                                            
         CLI   TIMETYP,TIMETIME                                                 
         JNE   PRTSR084                                                         
*                                                                               
         LA    R2,X#SUMTAB                                                      
PRTSR088 CLI   STSTART,0          LOOK FOR SUMTAB MATCHING THIS TIMEL           
         JE    PRTSR084                                                         
         CLC   TIMETPDT,STSTART                                                 
         JL    PRTSR090                                                         
         CLC   TIMETDTE,STEND                                                   
         JNH   PRTSR092                                                         
PRTSR090 LA    R2,SUMTABL(R2)                                                   
         J     PRTSR088                                                         
*                                                                               
PRTSR092 MVC   TD#STAD,STSTART    PARTIAL OVERLAP, REBUILD TSAR                 
         MVC   TD#ENDD,STEND      FROM SUMTAB'S SHARE OF THE TIMELS             
         ZAP   TD#HOUR,=P'0'                                                    
         LA    RE,TIMEDAY                                                       
SUMSCAN  USING TIMEDAY,RE                                                       
         LLC   RF,TIMLN                                                         
         AR    RF,R3              NEXT ELEMENT, SO WE KNOW WHEN TO STOP         
PRTSR094 CR    RE,RF                                                            
         JE    PRTSR098           END OF TIMEL, dId we find anything?           
         CLC   SUMSCAN.TIMETDTE,STSTART                                         
         JL    PRTSR096                                                         
         CLC   SUMSCAN.TIMETDTE,STEND                                           
         JH    PRTSR100           NO MORE IN THIS TIMEL, GO TSAADD              
         OC    SUMSCAN.TIMEHRS,SUMSCAN.TIMEHRS                                  
         JZ    *+10                                                             
         AP    TD#HOUR,SUMSCAN.TIMEHRS                                          
*                                                                               
PRTSR096 LA    RE,L'TIMEDAY(RE)    NEXT DAY                                     
         J     PRTSR094                                                         
         DROP  R3,SUMSCAN                                                       
*                                                                               
PRTSR098 CP    TD#HOUR,=P'0'                                                    
         BE    PRTSR090            NO, NEXT SUMTAB                              
*                                                                               
PRTSR100 DS    0H                  FOUND HOURS FOR THIS SUMTAB, ADD             
         SR    R1,R1                                                            
         ICM   R1,7,X#ITEMS                                                     
         AHI   R1,1                                                             
         STCM  R1,7,X#ITEMS                                                     
         MVC   TD#SEQ,X#ITEMS                                                   
         OI    STSTAT,STSTFUND     STOP BLDCTSR ADDING A DUMMY FOR THIS         
         GOTOR (#MTSAR,AMTSAR),DMCB,TSAADD,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    PRTSR090            NEXT SUMTAB                                  
         TM    TSARERRS,X'80'                                                   
         BO    *+6                 TOO MANY ITEMS                               
         DC    H'0'                BCTSERRS=X'20'=DUPLICATE KEY                 
         MVC   ROUERRV,=AL2(AE$NASRC)                                           
         J     XERROR                                                           
         DROP  R2                                                               
*                                                                               
PROPTSRX J     EXITY                                                            
         DROP  R4                                                               
         LTORG                                                                  
***********************************************************************         
* PROCESS TIMESHEET MASTER RECORD INTO TSAR BUFFER                    *         
*                                                                               
* needs PERTAB set up (at least for this t/s's period)                          
* if hours seach with client filter, csvkey3 should hold passive we             
* used to find the record (routine will filter TIMELs with it)                  
***********************************************************************         
         SPACE 1                                                                
PROMTSR  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*PROMTSR'                                                      
         USING TD#TABD,R4                                                       
         L     R4,AIO1                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO1                                        
         MVC   TEMP2(3),TS#PEDT    get period # and dates                       
         GOTOR RETCPFD                                                          
*                                                                               
         USING TIMELD,R3                                                        
         USING TSJPASD,R2                                                       
         LA    R2,IOKEY                                                         
         CLI   TS#PTYPE,TS#PTRHC    SEARCH HOURS BY CLIENT                      
         BE    PMTSR005                                                         
         CLI   TS#PTYPE,TS#PTSWA    Summary client weekly                       
         BE    PMTSR005                                                         
         CLI   TS#PTYPE,TS#PTSWC    Summary client detail weekly                
         BNE   PMTSR010                                                         
PMTSR005 CLI   TSJPSUB,TSJPSUBQ       SET ANYACCD FILTER FROM                   
         BE    *+6                                   KEY                        
         DC    H'0'                WHAT POINTER IS THIS?                        
         MVC   ANYACUL,PRODUL                                                   
         MVC   ANYACCD,TSJPACT                                                  
         CLI   TSJPVIEW,TSJPSJAQ                                                
         BE    PMTSR007                                                         
         MVC   ANYACCD,TSJPMACT                                                 
         CLI   TSJPVIEW,TSJP1NAQ                                                
         BNE   PMTSR007                                                         
         MVI   ANYACUN,C'1'                                                     
         MVI   ANYACLE,C'N'                                                     
         MVC   ANYACCD,TSJP1NAC                                                 
         B     PMTSR008                                                         
*                                                                               
PMTSR007 CLC   ANYACUL,PRODUL                                                   
         BNE   PMTSR008                                                         
         OI    TS#ICPJ,TS#ICLI+TS#IPRD+TS#IJOB                                  
         B     PMTSR010                                                         
PMTSR008 OI    TS#ICPJ,TS#INON                                                  
*                                                                               
         USING TIMRECD,R2                                                       
PMTSR010 ZAP   X#SVHRS,PZERO                                                    
         ZAP   X#SVMTS,PZERO                                                    
         ZAP   X#SVCHR,PZERO                                                    
         L     R2,AIO2                                                          
         TM    TS#ICPJ,TS#IOFFR                                                 
         BZ    PMTSR016                                                         
         SR    RF,RF                                                            
         IC    RF,ONERL1L                                                       
         TM    TS#ICPJ,TS#IDPTR                                                 
         BZ    PMTSR012                                                         
         IC    RF,ONERL2L                                                       
         TM    TS#ICPJ,TS#ISDPR                                                 
         BZ    PMTSR012                                                         
         IC    RF,ONERL3L                                                       
*                                                                               
PMTSR012 SHI   RF,1                                                             
         EX    RF,PMTSR014                                                      
         BE    PMTSR020                                                         
         B     PMTSR580            not valid office                             
*                                                                               
PMTSR014 CLC   TIMKACT(0),X#1RACT                                               
*                                                                               
PMTSR016 TM    TS#ICPJ,TS#IPERR                                                 
         BZ    PMTSR020                                                         
         LLC   R3,ONERL3L                                                       
         LLC   R1,ONERL4L                                                       
         SR    R1,R3               R1=length of person code                     
         SHI   R1,1                                                             
         LA    RE,TIMKACT                                                       
         AR    RE,R3                                                            
         EX    R1,PMTSR018                                                      
         BE    PMTSR020                                                         
         B     PMTSR580            not valid person                             
*                                                                               
PMTSR018 CLC   0(0,RE),X#1RPER                                                  
*                                                                               
PMTSR020 DS    0H                                                               
*&&UK                                                                           
         CLI   TS#PTYPE,TS#PTLMD   List My timesheets by Date                   
         BE    *+12                                                             
         CLI   TS#PTYPE,TS#PTLMS   List My timesheets by Status                 
         BNE   PMTSR050                                                         
         TM    SCPYEL+(CPYSTATD-CPYELD),CPYSRSTS                                
         BZ    PMTSR050            No restrict T/S by status                    
         TM    TIMRSTAT,TIMSFAPP+TIMSPAPP                                       
         BNZ   PMTSR580            Skip fully/part approved T/S                 
*&&                                                                             
PMTSR050 LA    R3,TIMRFST          read through T/S record                      
PMTSR060 CLI   TIMEL,0                                                          
         BE    PMTSR480                                                         
         CLI   TIMEL,TIMELQ                                                     
         BNE   PMTSR470                                                         
         CLI   TIMETYP,TIMEINP     actual time/hours line?                      
         BNE   PMTSR460                                                         
PMTSR080 OC    TIMHRS,TIMHRS       MAY BE NULL                                  
         BZ    PMTSR082                                                         
         AP    X#SVHRS,TIMHRS      add to total for T/S                         
         CLI   TIMTTYP,TIMTNC      is it non client time                        
         BE    PMTSR082            yes                                          
         CLI   TIMTTYP,TIMTEM      is it empty timesheet                        
         BE    PMTSR082            yes                                          
         AP    X#SVCHR,TIMHRS      add up total client time                     
                                                                                
PMTSR082 CLI   TS#PTYPE,TS#PTSWS   is it a summary request                      
         BNL   PMTSR090            yes                                          
         CLI   TS#PTYPE,TS#PTRHC   if searching for hours get timels            
         BE    PMTSR090                   and put record to buffer              
         CLI   TS#PTYPE,TS#PTRHO   office/dept/sub-dept hours search            
         BE    PMTSR090                                                         
         CLI   TS#PTYPE,TS#PTRHP   person hours search                          
         BNE   PMTSR470                                                         
PMTSR090 CLC   ANYACCNT,SPACES                                                  
         BNH   PMTSR150                                                         
         ZAP   X#SVHRS,PZERO                                                    
         ZAP   X#SVCHR,PZERO                                                    
         CLI   TS#PTYPE,TS#PTRHC   hours search: we only want the               
         BNE   PMTSR108                timels matching our filter               
*                                                                               
CK       USING TDTPASD,CSVKEY3                                                  
PMTSR098 CLI   CK.TDTPSUB,TDTPSUBQ                                              
         BNE   PMTSR102                                                         
         CLC   TIMACC,CK.TDTPCULA                                               
         BNE   PMTSR470                                                         
         CLC   PRODUL,TIMACC                                                    
         BNE   PMTSR108                                                         
         CLC   TIMOFF,CK.TDTPCOFF                                               
         BNE   PMTSR470                                                         
         DROP  CK                                                               
*                                                                               
CK       USING TSJPASD,CSVKEY3                                                  
PMTSR102 CLI   CK.TSJPSUB,TSJPSUBQ                                              
         BE    *+6                                                              
         DC    H'0'                  UNKNOWN PASSIVE?                           
         CLC   TIMACC,ANYACCNT            ??? THINK THIS WORKS...               
         BNE   PMTSR470                                                         
         CLC   PRODUL,TIMACC         If SJ look for office match                
         BNE   PMTSR108                                                         
         LA    RF,CK.TSJPCOFF                                                   
         CLI   CK.TSJPVIEW,TSJPSJAQ                                             
         BE    *+8                                                              
         LA    RF,CK.TSJPMOFF                                                   
         CLC   TIMOFF,0(RF)                                                     
         BNE   PMTSR470                                                         
*                                                                               
PMTSR108 TM    TS#ICPJ,TS#ICLI                                                  
         BZ    PMTSR110                                                         
         XR    RF,RF                                                            
         IC    RF,PCLILEN                                                       
         TM    TS#ICPJ,TS#IPRD                                                  
         BZ    PMTSR120                                                         
         IC    RF,PPROLEN                                                       
         TM    TS#ICPJ,TS#IJOB                                                  
         BZ    PMTSR120                                                         
         IC    RF,PJOBLEN                                                       
         B     PMTSR120                                                         
*                                                                               
PMTSR110 TM    TS#ICPJ,TS#INON                                                  
         BZ    PMTSR150                                                         
         LA    RF,12                                                            
PMTSR120 AHI   RF,1                take unit and ledger into account            
         EX    RF,PMTSR130                                                      
         BE    PMTSR140                                                         
         B     PMTSR470                                                         
PMTSR130 CLC   ANYACCNT(0),TIMACC  compare account including unit/ledgr         
*                                                                               
PMTSR140 OC    TIMHRS,TIMHRS                                                    
         BZ    PMTSR150                                                         
         AP    X#SVHRS,TIMHRS                                                   
         CLI   TIMTTYP,TIMTNC      is it non client time                        
         BE    PMTSR150            yes                                          
         CLI   TIMTTYP,TIMTEM      is it empty timesheet                        
         BE    PMTSR150            yes                                          
         AP    X#SVCHR,TIMHRS      add up total client time                     
PMTSR150 CLI   TS#PTYPE,TS#PTRHC   if searching check for workcode              
         BE    PMTSR160                                                         
         CLI   TS#PTYPE,TS#PTRHO                                                
         BE    PMTSR160                                                         
         CLI   TS#PTYPE,TS#PTRHP                                                
         BNE   PMTSR170                                                         
PMTSR160 CLC   TS#WCD,SPACES                                                    
         BNH   PMTSR170                                                         
         CLC   TS#WCD,TIMTSK                                                    
         BNE   PMTSR450                                                         
*                                                                               
PMTSR170 GOTOR (#CLRIO,ACLRIO),DMCB,AIO1                                        
         CLI   TIMTTYP,TIMTCB      is the time billable                         
         BNE   PMTSR180            no                                           
         CLI   TS#PTYPE,TS#PTSWS                                                
         BNL   *+12                                                             
         CLI   RQ_TSBIL,C'Y'       yes - do we want billable                    
         BNE   PMTSR450            no - get next timel                          
         MVI   TD#TTYPE,C'B'                                                    
         B     PMTSR220                                                         
PMTSR180 CLI   TIMTTYP,TIMTCR      is the time chargeable                       
         BNE   PMTSR190            no                                           
         CLI   TS#PTYPE,TS#PTSWS                                                
         BNL   *+12                                                             
         CLI   RQ_TSCHG,C'Y'       yes - do we want chargeable                  
         BNE   PMTSR450            no - get next timel                          
         MVI   TD#TTYPE,C'R'                                                    
         B     PMTSR220            yes                                          
PMTSR190 CLI   TIMTTYP,TIMTCN      Is the time non billable client              
         BNE   PMTSR205            No                                           
         CLI   TS#PTYPE,TS#PTSWS                                                
         BNL   PMTSR200                                                         
         CLI   RQ_TSNBL,C'C'       Do we want non-billable client time          
         BE    *+12                Yes                                          
         CLI   RQ_TSNBL,C'Y'       Do we want all non-billable time             
         BNE   PMTSR450            Yes                                          
         MVI   TD#TTYPE,C'N'       non-billable client                          
         B     PMTSR220                                                         
*                                                                               
PMTSR200 MVI   TD#TTYPE,C'N'                                                    
         B     PMTSR220                                                         
*                                                                               
PMTSR205 CLI   TIMTTYP,TIMTNC      non-client time                              
         BE    PMTSR208                                                         
         CLI   TIMTTYP,TIMTEM                                                   
         BE    *+6                                                              
         DC    H'0'                die if unknown type of time                  
PMTSR208 CLI   TS#PTYPE,TS#PTSWS                                                
         BNL   PMTSR210                                                         
         CLI   RQ_TSNBL,C'1'       Do we want non-billable 1N time              
         BE    *+12                Yes                                          
         CLI   RQ_TSNBL,C'Y'       Do we want all non-billable time             
         BNE   PMTSR450            Yes                                          
PMTSR210 MVI   TD#TTYPE,C'N'       Set non client time for search               
         CLI   TS#PTYPE,TS#PTSMP   Staff summary detail - monthly               
         BE    PMTSR215                                                         
         CLI   TS#PTYPE,TS#PTSWP   Staff summary detail - weekly                
         BNE   PMTSR220                                                         
PMTSR215 MVI   TD#TTYPE,C'C'       Set non client time for staff sum            
                                                                                
PMTSR220 CLI   TS#PTYPE,TS#PTSMP   only want type of time for detail            
         BE    PMTSR230                              staff summary              
         CLI   TS#PTYPE,TS#PTSWP                                                
         BE    PMTSR230                                                         
         CLI   TS#PTYPE,TS#PTSWS  anything else for summary delete type         
         BL    PMTSR240                                                         
         B     *+10                                                             
PMTSR230 MVC   TD#TTYE,TD#TTYPE                                                 
         XC    TD#TTYPE,TD#TTYPE                                                
         USING PERTABD,RF                                                       
PMTSR240 LA    RF,TEMP2                                                         
         MVC   TD#STAD,PERSTDT                                                  
         MVC   TD#ENDD,TS#PEDT                                                  
         MVC   TD#PNUM,PERNUM                                                   
         DROP  RF                                                               
         CLI   TS#PTYPE,TS#PTSWS                                                
         BNL   PMTSR250                                                         
         MVC   TD#ULA,TIMACC                                                    
         MVC   TD#WRKC,TIMTSK                                                   
         CLC   TD#WRKC,SPACES      have we got a workcode                       
         BH    PMTSR270                                                         
         MVC   TD#WRKC,FFS                                                      
         B     PMTSR270                                                         
PMTSR250 CLI   TS#PTYPE,TS#PTSWA                                                
         BL    PMTSR270                                                         
         XR    RF,RF                                                            
         IC    RF,PJOBLEN                                                       
         CLI   TS#PTYPE,TS#PTSWC                                                
         BE    PMTSR260                                                         
         CLC   PRODUL,TIMACC                                                    
         BNE   PMTSR260                                                         
         IC    RF,PCLILEN                                                       
         MVC   TD#ACC,SPACES                                                    
PMTSR260 AHI   RF,1                                                             
         MVC   TD#ULA(0),TIMACC                                                 
         EX    RF,*-6                                                           
         CLI   TS#PTYPE,TS#PTSWA                                                
         BE    *+10                                                             
PMTSR270 MVC   TD#PERC,TS#PRSAC                                                 
         CLI   TS#PTYPE,TS#PTSWS                                                
         BNL   PMTSR280                                                         
         MVC   TD#OFFC,TS#1ROFF                                                 
         MVC   TD#DEPT,TS#1RDPT                                                 
         MVC   TD#SBDP,TS#1RSDP                                                 
         MVC   TD#TYP,TS#ATYP                                                   
PMTSR280 ZAP   TD#HOUR,X#SVHRS                                                  
         ZAP   TD#CLIHR,X#SVCHR                                                 
         GOTOR SETSSTA,1                                                        
         MVC   TD#STTS,X#STAT                                                   
         MVC   TD#STTV,X#STAT                                                   
         CLI   TD#STTV,TS#PROGR                                                 
         BNE   PMTSR290                                                         
         CLC   TD#ENDD,X#CUTDT     is it overdue                                
         BNL   PMTSR290            no                                           
         MVI   TD#STTV,TS#OVERD    yes - status accordingly                     
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         JE    PMTSR302                                                         
         MVI   TD#STTS,TS#OVERD                                                 
PMTSR290 LA    R1,X#REQST          is this a status request                     
PMTSR300 CLI   0(R1),X'FF'                                                      
         BE    PMTSR450                                                         
         CLC   0(1,R1),TD#STTS     Does it match as required                    
         BE    PMTSR310            no - don't add record to buffer              
         LA    R1,1(R1)                                                         
         B     PMTSR300                                                         
*                                                                               
PMTSR302 LA    R1,X#REQST          is this a status request                     
PMTSR304 CLI   0(R1),X'FF'                                                      
         BE    PMTSR450                                                         
         CLC   0(1,R1),TD#STTV     Does it match as required                    
         BE    PMTSR310            no - don't add record to buffer              
         LA    R1,1(R1)                                                         
         B     PMTSR304                                                         
*                                                                               
PMTSR310 LR    R2,R3                                                            
         CLI   TS#PTYPE,TS#PTRHC   if searching check for order and             
         BE    PMTSR320                                   internal ref          
         CLI   TS#PTYPE,TS#PTRHO                                                
         BE    PMTSR320                                                         
         CLI   TS#PTYPE,TS#PTRHP                                                
         BNE   PMTSR330                                                         
PMTSR320 OC    RQ_ORD#,RQ_ORD#                                                  
         BZ    *+8                                                              
         OI    X#IND,X#IORD#                                                    
         OC    RQ_INTRF,RQ_INTRF                                                
         BZ    *+8                                                              
         OI    X#IND,X#IINTRF                                                   
         OC    RQ_EST#,RQ_EST#                                                  
         BZ    *+8                                                              
         OI    X#IND,X#IEST#                                                    
MAT      USING TIMELD,R2                                                        
PMTSR330 XR    R0,R0                                                            
         IC    R0,MAT.TIMLN                                                     
         AR    R2,R0                                                            
         CLI   MAT.TIMEL,0             eor                                      
         BE    PMTSR400                                                         
         CLI   MAT.TIMEL,TIMELQ                                                 
         BNE   PMTSR330                                                         
         CLC   MAT.TIMSEQ,TIMSEQ                                                
         BNE   PMTSR330                                                         
         CLI   MAT.TIMETYP,TIMEITMS    materials                                
         BNE   PMTSR340                                                         
         AP    X#SVMTS,MAT.TIMITOT                                              
         B     PMTSR330                                                         
*                                                                               
PMTSR340 CLI   TS#PTYPE,TS#PTRHC   if searching check for workcode              
         BE    PMTSR350                                                         
         CLI   TS#PTYPE,TS#PTRHO                                                
         BE    PMTSR350                                                         
         CLI   TS#PTYPE,TS#PTRHP                                                
         BNE   PMTSR330                                                         
PMTSR350 CLI   MAT.TIMETYP,TIMEORDR    order                                    
         BNE   PMTSR370                                                         
         TM    X#IND,X#IORD#                                                    
         BZ    PMTSR360                                                         
         CLC   RQ_ORD#,MAT.TIMOORDR                                             
         BNE   PMTSR450                                                         
         NI    X#IND,X'FF'-X#IORD#                                              
PMTSR360 MVC   TD#ORD#,MAT.TIMOORDR                                             
         B     PMTSR330                                                         
*                                                                               
PMTSR370 CLI   MAT.TIMETYP,TIMEINRF    internal reference                       
         BNE   PMTSR382                                                         
         TM    X#IND,X#IINTRF                                                   
         BZ    PMTSR380                                                         
         CLC   RQ_INTRF,MAT.TIMJINRF                                            
         BNE   PMTSR450                                                         
         NI    X#IND,X'FF'-X#IINTRF                                             
PMTSR380 MVC   TD#INTRF,MAT.TIMJINRF                                            
         B     PMTSR330                                                         
*                                                                               
PMTSR382 CLI   MAT.TIMETYP,TIMEEST     internal reference                       
         BNE   PMTSR390                                                         
         TM    X#IND,X#IEST#                                                    
         BZ    PMTSR384                                                         
         CLC   RQ_EST#,MAT.TIMSESNM                                             
         BNE   PMTSR450                                                         
         NI    X#IND,X'FF'-X#IEST#                                              
PMTSR384 MVC   TD#EST#,MAT.TIMSESNM                                             
         B     PMTSR330                                                         
*                                                                               
PMTSR390 CLI   MAT.TIMETYP,TIMENAR     narrative                                
         BNE   PMTSR330                                                         
*                                                                               
         LA    R0,TD#NARR                                                       
         LA    R1,L'TD#NARR                                                     
         SR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XR    RE,RE                                                            
         IC    RE,MAT.TIMLN                                                     
         SHI   RE,TIMHLNQ+1                                                     
         MVC   TD#NARR(0),MAT.TIMNARR                                           
         EX    RE,*-6                                                           
         B     PMTSR330                                                         
         DROP  MAT                                                              
*                                                                               
*  ADD TIMESHEET TSAR, OR SPLIT IF SUMMARY VIEW                                 
*                                                                               
PMTSR400 CLC   TD#ORD#,SPACES      have we got an order                         
         BH    *+10                                                             
         MVC   TD#ORD#,FFS                                                      
         TM    X#IND,X#IINTRF+X#IORD#+X#IEST#                                   
         BNZ   PMTSR450                                                         
         CLI   TS#PTYPE,TS#PTSMS   for summaries split timel hours              
         BL    PMTSR445                       by sumtab                         
*                                                                               
         LR    RE,R3               R3 IS ADDRESS OF TIMEINP TIMEL WE'RE         
TIME     USING TIMELD,RE                     WORKING ON                         
PMTSR410 XR    R0,R0                                                            
         IC    R0,TIME.TIMLN                                                    
         AR    RE,R0                                                            
         CLI   TIME.TIMEL,0             eor                                     
         BE    PMTSR450                                                         
         CLI   TIME.TIMEL,TIMELQ                                                
         BNE   PMTSR410                                                         
         CLI   TIME.TIMETYP,TIMETIME    brandocean timel                        
         BNE   PMTSR410                                                         
         CLC   TIME.TIMSEQ,TIMSEQ       MATCHING TIMEL IN MAIN LOOP             
         BNE   PMTSR410                                                         
         LLC   R0,TIME.TIMLN                                                    
         AR    R0,RE                                                            
         ST    RE,SAVER1               A(TIMETIME EL TO PROCESS)                
*                                                                               
*                                                                               
         LA    R2,X#SUMTAB         EXTRACT HOURS FOR EACH SUMTAB ENTRY          
         USING SUMTABD,R2                AND ADD A TSAR FOR IT                  
*                                                                               
PMTSR420 CLI   0(R2),0             END OF SUMTAB?                               
         BE    PMTSR450            GO NEXT TIMEINP TIMEL                        
         MVC   TD#STAD,STSTART                                                  
         MVC   TD#ENDD,STEND                                                    
         L     RE,SAVER1                                                        
         LLC   R0,TIME.TIMLN                                                    
         AR    R0,RE               A(NEXT ELEMENT)                              
         LA    RE,TIME.TIMEDAY        POINT TO DAILY SUBELS                     
         ZAP   TD#HOUR,PZERO                                                    
*                                                                               
PMTSR424 CR    RE,R0               END OF TIMETIME EL?                          
         BNL   PMTSR430                                                         
         CLC   0(L'TIMETDTE,RE),SPACES                                          
         BNH   PMTSR425                                                         
*                                                                               
         CLC   STSTART,0(RE)                                                    
         BH    PMTSR425                                                         
         CLC   STEND,0(RE)                                                      
         BNL   PMTSR426                                                         
PMTSR425 LA    RE,L'TIMEDAY(RE)                                                 
         B     PMTSR424                                                         
*                                                                               
PMTSR426 DS    0H                  SOME HOURS FOR CURRENT SUMTAB PERD           
         OC    L'TIMETDTE(L'TIME.TIMEHRS,RE),L'TIMETDTE(RE)                     
         JZ    PMTSR425                                                         
         AP    TD#HOUR,L'TIMETDTE(L'TIME.TIMEHRS,RE)                            
         B     PMTSR425                                                         
         DROP  TIME                                                             
*                                                                               
PMTSR430 ZAP   TD#MATRS,X#SVMTS         ADD SPLIT TSAR REC HERE                 
         SR    R1,R1                        THIS AND PMTSR445 CAN GO            
         ICM   R1,7,X#ITEMS                 INTO A ROUTINE, IF YOU HAVE         
         AHI   R1,1                         THE TIME FOR IT                     
         STCM  R1,7,X#ITEMS                                                     
         MVC   TD#SEQ,X#ITEMS                                                   
         OI    STSTAT,STSTFUND     STOP BLDCTSR ADDING A DUMMY FOR THIS         
         GOTOR (#MTSAR,AMTSAR),DMCB,TSAADD,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    PMTSR440                                                         
         TM    TSARERRS,X'80'                                                   
         BO    *+6                 TOO MANY ITEMS                               
         DC    H'0'                BCTSERRS=X'20'=DUPLICATE KEY                 
         MVC   ROUERRV,=AL2(AE$NASRC)                                           
         J     XERROR                                                           
*                                                                               
PMTSR440 LA    R2,SUMTABL(R2)     NEXT SUMTAB                                   
         ZAP   X#SVMTS,PZERO      only put out materials once                   
         ZAP   X#SVCHR,PZERO                                                    
         B     PMTSR420                                                         
*                                                                               
*                                                                               
*                                                                               
PMTSR445 ZAP   TD#MATRS,X#SVMTS    NON-split TSARADD                            
         SR    R1,R1                                                            
         ICM   R1,7,X#ITEMS                                                     
         AHI   R1,1                                                             
         STCM  R1,7,X#ITEMS                                                     
         MVC   TD#SEQ,X#ITEMS                                                   
         GOTOR (#MTSAR,AMTSAR),DMCB,TSAADD,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    PMTSR450            NEXT TIMEINP TIMEL                           
         TM    TSARERRS,X'80'                                                   
         BO    *+6                 TOO MANY ITEMS                               
         DC    H'0'                BCTSERRS=X'20'=DUPLICATE KEY                 
         MVC   ROUERRV,=AL2(AE$NASRC)                                           
         J     XERROR                                                           
*                                                                               
PMTSR450 ZAP   X#SVHRS,PZERO                                                    
         ZAP   X#SVMTS,PZERO                                                    
         ZAP   X#SVCHR,PZERO                                                    
         L     R2,AIO2                                                          
         B     PMTSR470                                                         
*                                                                               
PMTSR460 CLI   TIMETYP,TIMEITMS    MATERIALS                                    
         BNE   PMTSR470                                                         
         CLI   TS#PTYPE,TS#PTSWS                                                
         BNL   PMTSR470                                                         
         CLI   TS#PTYPE,TS#PTRHC                                                
         BE    PMTSR470                                                         
         CLI   TS#PTYPE,TS#PTRHO                                                
         BE    PMTSR470                                                         
         CLI   TS#PTYPE,TS#PTRHP                                                
         BE    PMTSR470                                                         
         AP    X#SVMTS,TIMITOT     add up material totals                       
*                                                                               
PMTSR470 XR    R0,R0                                                            
         IC    R0,TIMLN                                                         
         AR    R3,R0                                                            
         B     PMTSR060                                                         
         DROP  R3                                                               
*                                                                               
PMTSR480 CLI   TS#PTYPE,TS#PTRHC                                                
         BE    PMTSR580                                                         
         CLI   TS#PTYPE,TS#PTRHO                                                
         BE    PMTSR580                                                         
         CLI   TS#PTYPE,TS#PTRHP                                                
         BE    PMTSR580                                                         
         CLI   TS#PTYPE,TS#PTSWS                                                
         BE    PMTSR580                                                         
         CLI   TS#PTYPE,TS#PTSMP                                                
         BE    PMTSR580                                                         
         CLI   TS#PTYPE,TS#PTSWP                                                
         BE    PMTSR580                                                         
         CLI   TS#PTYPE,TS#PTSWA                                                
         BE    PMTSR580                                                         
         CLI   TS#PTYPE,TS#PTSWC                                                
         BE    PMTSR580                                                         
         LA    RF,TEMP2                                                         
         USING PERTABD,RF                                                       
         MVC   TD#STAD,PERSTDT     and pass them on                             
         MVC   TD#ENDD,TS#PEDT                                                  
         MVC   TD#PNUM,PERNUM                                                   
         MVC   TD#PERC,TS#PRSAC                                                 
         DROP  RF                                                               
                                                                                
PMTSR490 MVC   TD#OFFC,TS#1ROFF                                                 
         MVC   TD#DEPT,TS#1RDPT                                                 
         MVC   TD#SBDP,TS#1RSDP                                                 
         MVC   TD#TYP,TS#ATYP                                                   
*                                                                               
PMTSR500 ZAP   TD#HOUR,X#SVHRS                                                  
         ZAP   TD#CLIHR,X#SVCHR                                                 
         ZAP   TD#MATRS,X#SVMTS                                                 
PMTSR510 GOTOR SETSSTA,1                                                        
         MVC   TD#STTS,X#STAT                                                   
         GOTOR CHKAPP             do we want approver status?                   
         JNE   PMTSR515           no                                            
         GOTOR SETASTA,DMCB,TS#STAT       set approver status                   
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         JE    PMTSR515                                                         
         MVC   TD#STTS,X#STAT                                                   
PMTSR515 MVC   TD#STTV,X#STAT                                                   
PMTSR520 CLI   TD#STTV,TS#PROGR                                                 
         BNE   PMTSR540                                                         
         CLC   TD#ENDD,X#CUTDT     is it overdue                                
         BNL   PMTSR530            no                                           
         MVI   TD#STTV,TS#OVERD    yes - status accordingly                     
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         JE    PMTSR531                                                         
         MVI   TD#STTS,TS#OVERD                                                 
PMTSR530 CLI   TS#PTYPE,TS#PTLAS   is it a search                               
         BH    PMTSR550            yes                                          
         CLC   RQ_TLST,SPACES      is this a status request                     
         BNH   PMTSR570            no                                           
         CLC   RQ_TLST,TD#STTS     does it match as required                    
         BNE   PMTSR580            no - don't add record to buffer              
         B     PMTSR570            yes - add to buffer                          
*                                                                               
PMTSR531 CLI   TS#PTYPE,TS#PTLAS   is it a search                               
         BH    PMTSR532            yes                                          
         CLC   RQ_TLST,SPACES      is this a status request                     
         BNH   PMTSR570            no                                           
         CLI   RQ_TLST,TS#OVERD    does it match what we have                   
         BNE   PMTSR580            no - don't add record to buffer              
         B     PMTSR570            yes - add to buffer                          
*                                                                               
PMTSR532 LA    R1,X#REQST          is this a status requested                   
PMTSR534 CLI   0(R1),X'FF'         end of list                                  
         BE    PMTSR580            yes - no match don't add to buffer           
         CLC   TD#STTV,0(R1)       does it match as required                    
         BE    PMTSR570            yes - add record to buffer                   
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ          app is connected                            
         JNE   PMTSR531                                                         
         CLC   TD#STTS,0(R1)                                                    
         BE    PMTSR570                                                         
         LA    R1,1(R1)                                                         
         B     PMTSR534                                                         
                                                                                
PMTSR540 CLI   TS#PTYPE,TS#PTLAS   is it a search                               
         BNH   PMTSR570            no                                           
PMTSR550 LA    R1,X#REQST          is this a status requested                   
PMTSR560 CLI   0(R1),X'FF'         end of list                                  
         BE    PMTSR580            yes - no match don't add to buffer           
         TM    X#SRCIND,X#SRCAPP   is it an approver search                     
         BZ    PMTSR562            no                                           
         CLC   TD#STTV,0(R1)       does it match as required                    
         BE    PMTSR570            yes - add record to buffer                   
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        If Aura we need to check both                
         CHI   RF,XPRODIKQ           statuses                                   
         JNE   PMTSR564                                                         
         CLC   TD#STTS,0(R1)                                                    
         BE    PMTSR570                                                         
         B     PMTSR564                                                         
PMTSR562 CLC   TD#STTS,0(R1)       does it match as required                    
         BE    PMTSR570            yes - add record to buffer                   
PMTSR564 LA    R1,1(R1)                                                         
         B     PMTSR560                                                         
*                                                                               
PMTSR570 SR    R1,R1                                                            
         ICM   R1,7,X#ITEMS                                                     
         AHI   R1,1                                                             
         STCM  R1,7,X#ITEMS                                                     
         MVC   TD#SEQ,X#ITEMS                                                   
         GOTOR (#MTSAR,AMTSAR),DMCB,TSAADD,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    PMTSR580                                                         
         TM    TSARERRS,X'80'                                                   
         BO    *+6                 TOO MANY ITEMS                               
         DC    H'0'                BCTSERRS=X'20'=DUPLICATE KEY                 
         MVC   ROUERRV,=AL2(AE$NASRC)                                           
         J     XERROR                                                           
*                                                                               
PMTSR580 CR    RB,RB                                                            
         XIT1                                                                   
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
***********************************************************************         
* RETRIEVE COST PERIOD ENTRY FROM END DATE                            *         
***********************************************************************         
         SPACE 1                                                                
RETCPFD  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*RETCPFD'                                                      
                                                                                
         L     R2,AIO5                                                          
         USING PERTABD,R2                                                       
RETCPF02 CLI   0(R2),0                                                          
         JE    RETCPF06            (end date not found lookup calendar)         
         XR    RE,RE                                                            
         ICM   RE,7,PERENDT                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,X#PEREND                                                    
         CLC   TEMP2(3),X#PEREND                                                
         JE    RETCPF04                                                         
         AHI   R2,PERLENQ                                                       
         J     RETCPF02                                                         
                                                                                
RETCPF04 MVC   TEMP2(PERLENQ),0(R2)                                             
         J     RETCPFY                                                          
*                                                                               
RETCPF06 L     R2,AIO5             clear first entry of output area             
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO5                                        
                                                                                
         XC    X#DITMS,X#DITMS                                                  
         XR    RE,RE                                                            
         ICM   RE,1,SCPYEL+CPYSFST-CPYELD                                       
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         JH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
                                                                                
         MVC   TS#CDAT,TEMP2       WORK OUT 1ST FISCAL YEAR TO SCAN             
         MVC   TS#CDAT+1(1),BYTE1                                               
                                                                                
         CLC   TS#CSDT+1(1),BYTE1                                               
         JNL   RETCPF08                                                         
         GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CDAT)                              
                                                                                
RETCPF08 GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'11'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CEND)                              
                                                                                
*        MVC   TS#COFF,TS#1ROFF    and office                                   
         GOTOR GETCAL                                                           
         JH    RETCPFN             ERROR                                        
                                                                                
         USING CASRECD,R2                                                       
         USING TMPELD,R3                                                        
         MVI   BYTE1,0                                                          
         L     R2,AIO8                                                          
         LA    R3,CASRFST                                                       
         XR    R0,R0                                                            
                                                                                
RETCPF10 CLI   TMPEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                No match found!                              
         CLI   TMPEL,TMPELQ                                                     
         JE    RETCPF14                                                         
RETCPF12 IC    R0,TMPLN                                                         
         AR    R3,R0                                                            
         J     RETCPF10                                                         
                                                                                
RETCPF14 CLC   TMPEND,TEMP2                                                     
         JNE   RETCPF12                                                         
                                                                                
RETCPFY  CR    RB,RB                                                            
         J     RETCPFX                                                          
                                                                                
RETCPFN  LTR   RB,RB                                                            
                                                                                
RETCPFX  XIT1                                                                   
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* TEST CURRENT SJ ACCOUNT VERSUS GAPTAB ENTRIES                       *         
***********************************************************************         
         SPACE 1                                                                
         USING GAPTABD,R4                                                       
TTSJVGT  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*TTTSJVG'                                                      
                                                                                
         LR    R3,R1               point to SJ account code                     
P        USING TSJPCODE,R3                                                      
                                                                                
         LA    R4,GAPAREA2         point to client entry                        
         MVI   GAPTDAT1,GAPTT2Q    SKIP ANY 1R ENTRIES (RQ_APP1R)               
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
                                                                                
         CLI   GAPTLVL,GAPTSL4     Is table entry lowest level                  
         BE    TTSJVGLY            Yes - accept key                             
                                                                                
         CLI   GAPTLVL,GAPTSL9     Is table entry lowest level                  
         BE    TTSJVGLY            Yes - accept key                             
                                                                                
         XR    RE,RE                                                            
         IC    RE,GAPTLEN                                                       
         AR    RE,R3                                                            
         CLI   0(RE),C' '          Is the code any longer                       
         BE    TTSJVGLY            No - accept key                              
                                                                                
TTSJVG06 GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF     End of buffer                                
         JNZ   TTSJVGLY                                                         
         TM    GAPTSTA,GAPTSMQ                                                  
         BNZ   TTSJVGLY                                                         
         CLI   GAPTDAT1,GAPTT6Q    Is code by media office client               
         BE    TTSJVG08            Yes                                          
         CLI   GAPTDAT1,GAPTT2Q    Is code by office client                     
         BNE   TTSJVG06            No, we don't want this                       
                                                                                
         CLI   GAPTLVL,GAPTSL5     Do we have media code                        
         BL    TTSJVG08            No                                           
                                                                                
         CLC   P.TSJPMED,GAPTCMED                                               
         BNE   TTSJVG06                                                         
                                                                                
TTSJVG08 LLC   RF,GAPTLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   GAPTCODE(0),P.TSJPCODE                                           
         EX    RF,0(RE)                                                         
         BNE   TTSJVG06                                                         
         B     TTSJVGLN                                                         
*                                                                               
TTSJVGLY CR    RB,RB                                                            
         B     TTSJVGLX                                                         
                                                                                
TTSJVGLN LTR   RB,RB                                                            
                                                                                
TTSJVGLX XIT1                                                                   
         DROP  P,R4                                                             
         LTORG                                                                  
***********************************************************************         
* SET X#STAT FROM TIMRECD OR TS#STAT - ON NTRY R1 CONTAINS MODE       *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMRECD,R2                                                       
SETSSTA  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*SETSSTA'                                                      
                                                                                
         LA    RE,TIMRSTAT                                                      
         CH    R1,=H'1'                                                         
         BE    *+8                                                              
         LA    RE,TIMKSTAT                                                      
         CH    R1,=H'2'                                                         
         BNE   *+8                                                              
         LA    RE,TS#STAT                                                       
         MVC   X#STAT,ASTERS                                                    
         MVC   BYTE1,0(RE)                                                      
         NI    BYTE1,FF-TRNSARCH                                                
                                                                                
         LA    RE,SETSTAB                                                       
SETSSTA1 CLI   0(RE),X'FF'                                                      
         BE    SETSSTA3                                                         
         MVC   BYTE2,BYTE1                                                      
         NC    BYTE2,0(RE)                                                      
         OC    BYTE2,BYTE2                                                      
         BNZ   SETSSTA2                                                         
         AHI   RE,2                                                             
         B     SETSSTA1                                                         
                                                                                
SETSSTA2 MVC   X#STAT(1),1(RE)                                                  
         B     SETSSTA4                                                         
SETSSTA3 MVI   X#STAT,TS#PROGR                                                  
                                                                                
SETSSTA4 XIT1                                                                   
         DROP  R2                                                               
                                                                                
SETSTAB  DC    AL1(TIMSFAPP,TS#APPRO)                                           
         DC    AL1(TIMSPAPP,TS#PAAPR)                                           
         DC    AL1(TIMSSUBM,TS#SUBMT)                                           
         DC    AL1(TIMSREJE,TS#REJEC)                                           
         DC    AL1(TAPSSAVQ,TS#PROGR)                                           
         DC    X'FF'                                                            
                                                                                
         DC    AL1(TIMSFAPP,TS#APPRO)             fully approved                
         DC    AL1(TIMSMAAP+TIMSPAPP,TS#PAAPR)    part approved                 
         DC    AL1(TIMSAWAP+TIMSPAPP,TS#PAAPR)    part approved                 
         DC    AL1(TIMSAWAP+TIMSSUBM,TS#SUBMT)    submitted                     
         DC    AL1(TIMSSUBM,TS#SUBMT)             submitted                     
         DC    AL1(TIMSPAPP,TS#PAAPR)             part approved                 
         DC    AL1(TIMSREJE,TS#REJEC)             rejected                      
         DC    AL1(TAPSSAVQ,TS#PROGR)             In progress                   
         DC    AL1(FF),C' '                                                     
                                                                                
         LTORG                                                                  
***********************************************************************         
* Set X#STAT from TS#STAT for approver status from timesheet          *         
***********************************************************************         
         SPACE 1                                                                
SETASTA  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*SETASTA'                                                      
                                                                                
         MVC   X#STAT,ASTERS                                                    
         MVC   BYTE1,TS#STAT                                                    
         NI    BYTE1,FF-TRNSARCH                                                
                                                                                
         LA    RE,SETATAB                                                       
SETASTA1 CLI   0(RE),X'FF'                                                      
         BE    SETASTA3                                                         
         MVC   BYTE2,BYTE1                                                      
         NC    BYTE2,0(RE)                                                      
         OC    BYTE2,BYTE2                                                      
         BNZ   SETASTA2                                                         
         AHI   RE,2                                                             
         B     SETASTA1                                                         
                                                                                
SETASTA2 MVC   X#STAT(1),1(RE)                                                  
         B     SETASTA4                                                         
                                                                                
SETASTA3 MVI   X#STAT,TS#PROGR                                                  
                                                                                
SETASTA4 XIT1                                                                   
                                                                                
SETATAB  DC    AL1(TAPSAWPQ,TS#AWAIT)                                           
         DC    AL1(TAPSAPPQ,TS#APPRO)                                           
         DC    AL1(TAPSPAPQ,TS#PAAPR)                                           
         DC    AL1(TAPSSUBQ,TS#SUBMT)                                           
         DC    AL1(TAPSREJQ,TS#REJEC)                                           
         DC    AL1(TAPSSAVQ,TS#PROGR)                                           
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MODULE TO CHECK WHETHER WE WANT APPROVAL STATUS FOR A GIVEN TIMESHEET         
* ENTRY   TD#   IS TIMESHEET OWNER 1R                                           
* EXIT    CCNEQ IF WE DON'T WANT APPROVER STATUS                                
***********************************************************************         
         SPACE 1                                                                
         USING TSWRECD,R2                                                       
CHKAPP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   TS#PTYPE,TS#PTRHP                                                
         JH    CHKAPPN             SUMMARY, STATUS NOT RELEVANT                 
         CLI   TS#PTYPE,TS#PTRTP                                                
         JNL   CHKAPP19            LIST, STATUS NOT RELEVANT                    
*                                                                               
         CLI   RQ_TLSU,RQ_TLAQ     LIST, do we want approvals?                  
         JNE   CHKAPPN             No, just my time                             
         CLI   RQ_TLTY,RQ_TLDQ                                                  
         JE    CHKAPP45            yes, by date                                 
         CLI   X#FLTST,TAPSAWPQ                                                 
         JE    CHKAPP45            yes, by status=awaiting appr                 
         J     CHKAPPN             not looking for approver status              
*                                                                               
CHKAPP19 LA    R1,X#REQST          are we searching for 'Await. appr'?          
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
CHKAPP20 CLI   0(R1),X'FF'                                                      
         BE    CHKAPPN                                                          
         CLI   0(R1),TS#AWAIT      On Brandocean check we want                  
         BE    CHKAPP45              awaiting approval status                   
         CHI   RF,XPRODIKQ         On Aura check other status that              
         JNE   CHKAPP25              could have awaiting approval               
         CLI   0(R1),TS#SUBMT                                                   
         BE    CHKAPP40                                                         
         CLI   0(R1),TS#PAAPR                                                   
         BE    CHKAPP40                                                         
         CLI   0(R1),TS#REJEC                                                   
         BE    CHKAPP40                                                         
CHKAPP25 LA    R1,1(R1)                                                         
         B     CHKAPP20                                                         
*                                                                               
CHKAPP40 CLI   TS#PTYPE,TS#PTRHO   hours office/dept/sub-dept search            
         BE    CHKAPP50                                                         
         CLI   TS#PTYPE,TS#PTRTO   TS office/dept/sub-dept search               
         BE    CHKAPP50                                                         
CHKAPP45 TM    X#SRCIND,X#SRCAPP                                                
         BNZ   CHKAPPY             APPROVER PASS, DEFINITELY WANT               
*                                                                               
CHKAPP50 DS    0H                                                               
         MVC   TEMP2(2),=C'1R'                                                  
         MVC   TEMP2+2(L'ACTKACT),TS#1RACT                                      
         GOTOR GETMAP,TEMP2        CHECK IF USER IS APPROVER                    
         OC    FULL2,FULL2                                                      
         JZ    CHKAPPN                                                          
         CLC   CCTPID,FULL2                                                     
         JE    CHKAPPY             YES, USE APPROVER STATUS INSTEAD             
*                                                                               
CHKAPPN  LTR   RB,RB                                                            
         J     CHKAPPX                                                          
CHKAPPY  CR    RB,RB                                                            
CHKAPPX  XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MODULE TO DO DIRECTORY IOS FOR TIMESHEET/LISTS 'MINE BY DATE'       *         
* AND 'MINE BY STATUS' ALSO TIMESHEET SEARCH WHERE PERSON IS SPECIFIED*         
***********************************************************************         
         SPACE 1                                                                
         USING TSWRECD,R2                                                       
TLIOMSD  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*TLIOMSD'                                                      
*                                                                               
         MVC   IOKEY,CSVKEY1                                                    
         LA    R2,IOKEY                                                         
         OC    CSVKEY1,CSVKEY1                                                  
         BZ    TLMSD02                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2' RE-ESTABLISH SEQUENCE         
         B     TLMSD10                                                          
*                                                                               
TLMSD02  CLC   TS#CEDT,TS#CSDT                                                  
         BL    TLIOMSDN            nothing to do then                           
*                                                                               
         L     R3,AIO5                                                          
         USING PERTABD,R3                                                       
         ST    R3,X#ACALL                                                       
TLMSD04  CLI   0(R3),0                                                          
         BE    TLIOMSDN                                                         
         XC    TSWKEY,TSWKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ    build start key                              
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CUXCPY                                                   
         MVC   TSWKPER,TS#PRSAC                                                 
         MVC   TSWKEND,PERENDT                                                  
         OC    PERODS,PERODS                                                    
         JZ    TLMSD05                                                          
         MVC   TSWKODS,PERODS                                                   
TLMSD05  CLI   TS#PTYPE,TS#PTLMD   are we coming from list                      
         BE    TLMSD06             yes                                          
         CLI   TS#PTYPE,TS#PTLMS                                                
         BE    TLMSD06             yes                                          
         CLI   TS#PTYPE,TS#PTRTP                                                
         BE    TLMSD06             yes                                          
         MVC   TSWKODS,X#1RACT     if search set office/dept etc                
*                                                                               
TLMSD06  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    TLMSD12                                                          
         DC    H'0'                no errors should occur                       
TLMSD10  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         BE    TLMSD12                                                          
         DC    H'0'                no error should occur                        
*                                                                               
TLMSD12  MVC   CSVKEY1,TSWKEY      save key                                     
*                                                                               
         TM    TSWKSTAT,TIMSDELT                                                
         BNZ   TLMSD10                                                          
         CLC   IOKEYSAV(TSWKEND-TSWKEY),TSWKEY                                  
         BE    TLMSD16             same person                                  
         L     R3,X#ACALL                                                       
TLMSD14  OI    PERSTAT,PERSNFND    no - so therefore no more time               
         LA    R3,PERLENQ(R3)                                                   
         ST    R3,X#ACALL                                                       
         CLI   0(R3),0                                                          
         BNE   TLMSD04                                                          
         B     TLIOMSDN                                                         
*                                                                               
TLMSD16  CLC   IOKEYSAV(TSWKODS-TSWKEY),TSWKEY                                  
         BE    TLMSD22                                                          
         L     R3,X#ACALL                                                       
TLMSD18  CLC   TSWKEND,PERENDT     compare date in table to key                 
         BNE   TLMSD20                                                          
         CLI   TS#PTYPE,TS#PTLMD   are we doing my timesheet list               
         BE    TLMSD19                                                          
         CLI   TS#PTYPE,TS#PTLMS                                                
         BE    TLMSD19                                                          
         CLI   TS#PTYPE,TS#PTSMP   or person summary                            
         BE    TLMSD19                                                          
         CLI   TS#PTYPE,TS#PTSWP                                                
         BE    TLMSD19                                                          
         CLI   TS#PTYPE,TS#PTRHP   or a person hour search                      
         BE    TLMSD19                                                          
         CLI   TS#PTYPE,TS#PTRTP   or a person search                           
         BNE   TLMSD04             date only matters for some requests          
TLMSD19  CLC   TSWKODS,PERODS      for person office dept sub-dept need         
         BE    TLMSD04                                    to match              
TLMSD20  OI    PERSTAT,PERSNFND    set time not found for this period           
         LA    R3,PERLENQ(R3)                                                   
         ST    R3,X#ACALL          store address of current perd entry          
         B     TLMSD04                                                          
*                                                                               
TLMSD22  XR    R1,R1               save office                                  
         LA    RF,CUACCS+1                                                      
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         BZ    *+12                                                             
         AHI   R1,1                                                             
         AHI   RF,1                                                             
                                                                                
         CLI   CUACCS,C'*'         Limit access?                                
         BNE   TLMSD26                                                          
         LR    RE,R1                                                            
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC(0),TSWKODS Move in office and validate                  
         EX    RE,*-6                                                           
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         BE    TLMSD26                                                          
                                                                                
         L     R3,X#ACALL                                                       
         OI    PERSTAT,PERSSKIP    not valid so set to skip                     
         LA    R3,PERLENQ(R3)                                                   
         ST    R3,X#ACALL                                                       
         B     TLMSD04                                                          
*                                                                               
TLMSD26  CLI   TS#PTYPE,TS#PTRHP   are we coming from search                    
         BE    TLMSD28             yes                                          
         CLI   TS#PTYPE,TS#PTRTP                                                
         BNE   TLMSD34             yes                                          
TLMSD28  TM    TS#ICPJ,TS#IOFFR                                                 
         BZ    TLMSD34                                                          
         SR    RF,RF                                                            
         IC    RF,ONERL1L                                                       
         TM    TS#ICPJ,TS#IDPTR                                                 
         BZ    TLMSD30                                                          
         IC    RF,ONERL2L                                                       
         TM    TS#ICPJ,TS#ISDPR                                                 
         BZ    TLMSD30                                                          
         IC    RF,ONERL3L                                                       
*                                                                               
TLMSD30  SHI   RF,1                                                             
         EX    RF,TLMSD32                                                       
         BE    TLMSD34                                                          
         L     R3,X#ACALL                                                       
         OI    PERSTAT,PERSSKIP    not valid so set to skip                     
         LA    R3,PERLENQ(R3)                                                   
         ST    R3,X#ACALL                                                       
         B     TLMSD04                                                          
*                                                                               
TLMSD32  CLC   TSWKODS(0),X#1RACT for execute                                   
                                                                                
TLMSD34  L     R3,X#ACALL                                                       
         CLI   TS#PTYPE,TS#PTLMD   are we doing my timesheet list               
         BE    TLMSD36                                                          
         CLI   TS#PTYPE,TS#PTLMS                                                
         BE    TLMSD36                                                          
         CLI   TS#PTYPE,TS#PTSMP   or person summary                            
         BE    TLMSD36                                                          
         CLI   TS#PTYPE,TS#PTSWP                                                
         BE    TLMSD36                                                          
         CLI   TS#PTYPE,TS#PTRHP   or a person hours search                     
         BE    TLMSD36                                                          
         CLI   TS#PTYPE,TS#PTRTP   or a person TS search                        
         BNE   TLMSD38                                                          
TLMSD36  CLC   TSWKODS,PERODS      for person office dept sub-dept need         
         BNE   TLMSD20                                    to match              
TLMSD38  OI    PERSTAT,PERSFOND    set time found for this period               
         NI    PERSTAT,X'FF'-PERSNFND                                           
         TM    PERSTAT,PERSINVL    is this invalid                              
         BNZ   TLMSD10             yes                                          
         BAS   RE,TSTMYST          check status of IOKEY record                 
         BE    TLMSD40                                                          
         B     TLMSD10                                                          
*                                                                               
TLMSD40  CLI   TS#PTYPE,TS#PTLMD                                                
         BE    TLMSD42                                                          
         CLI   TS#PTYPE,TS#PTLMS                                                
         BNE   TLMSD50                                                          
TLMSD42  CLI   RQ_TLST,TS#NSTRT    not started                                  
         BE    TLMSD10             yes - don't want to save these               
TLMSD50  MVC   TS#RECDA,TSWKDA     save D/A and process it                      
         MVC   CSVKEY3,IOKEY       read for next record                         
         XC    X#SVHRS,X#SVHRS     (no hours from status area)                  
         MVC   TS#1ROFF(L'TS#1ROFF+L'TS#1RDPT+L'TS#1RSDP),SPACES                
         MVC   TS#PRSAC,TSWKPER                                                 
*        XC    TS#ATYP,TS#ATYP                                                  
         MVI   TS#ATYP,TD#1RQ                                                   
         MVC   TS#STAT,TSWKSTAT    for approver status                          
         SR    R1,R1                                                            
         IC    R1,ONERL1L                                                       
         SHI   R1,1                                                             
         MVC   TS#1ROFF(0),TSWKODS                                              
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         LA    RE,TSWKODS                                                       
         AR    RE,R1                                                            
                                                                                
         LR    R0,R1                                                            
         IC    R1,ONERL2L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   TS#1RDPT(0),0(RE)                                                
         EX    R1,*-6                                                           
         LA    RE,1(R1,RE)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         MVC   TS#1RSDP(0),0(RE)                                                
         EX    R1,*-6                                                           
                                                                                
         SR    RF,RF                                                            
         ICM   RF,7,TSWKEND                                                     
         LNR   RF,RF                                                            
         STCM  RF,7,TS#PEDT                                                     
*                                                                               
TLIOMSDY CR    RB,RB                                                            
         J     *+6                                                              
TLIOMSDN LTR   RB,RB                                                            
         XIT1                                                                   
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* TEST STATUS OF DIRECTORY TIMESHEET WEEKLY POINTER AGAINST WHAT WE   *         
* ARE LOOKING FOR.  X#FLTST CONTAINS LIST OF STATUSES, IF FIRST ONE   *         
* IS X'FF' WE WANT EVERY STATUS. X'FF' AFTER FIRST BYTE SIGNIFIES END *         
* OF LIST                                                             *         
***********************************************************************         
         SPACE 1                                                                
TSTMYST  NTR1  ,                   check status for 'my TimeSheets'             
         CLI   X#FLTST,FF                                                       
         BE    TSTMYY                                                           
         LA    R1,X#FLTST                                                       
         CLI   TS#PTYPE,TS#PTLMD   IF SEARCHING BY PASS NOT STARTED             
         BE    TSTMY08                                                          
         CLI   TS#PTYPE,TS#PTLMS                                                
         BE    TSTMY08                                                          
         CLI   X#REQST,TS#NSTRT                                                 
         BNE   TSTMY08                                                          
         LA    R1,1(R1)                                                         
TSTMY08  CLI   0(R1),X'FF'         EOR                                          
         BE    TSTMYN                                                           
         MVC   BYTE1,IOKEY+TSWKSTAT-TSWRECD                                     
         NI    BYTE1,FF-(TIMSDELT+TRNSARCH)                                     
         OC    0(1,R1),0(R1)       DO WE WANT IN PROGRESS OR OVERDUE            
         BNZ   TSTMY10             NO - IT'S EVERYTHING ELSE                    
         CLC   BYTE1,X#FLTST                                                    
         BE    TSTMYY                                                           
         CLI   TS#PTYPE,TS#PTLMD   IF LISTING ONLY HAVE ONE STATUS              
         BE    TSTMYN              THEREFORE EXIT AS ERROR                      
         CLI   TS#PTYPE,TS#PTLMS                                                
         BE    TSTMYN                                                           
         LA    R1,1(R1)                                                         
         B     TSTMY08                                                          
TSTMY10  NC    BYTE1,0(R1)                                                      
         CLC   BYTE1,0(R1)                                                      
         BE    TSTMYY                                                           
         CLI   TS#PTYPE,TS#PTLMD                                                
         BE    TSTMYN                                                           
         CLI   TS#PTYPE,TS#PTLMS                                                
         BE    TSTMYN                                                           
         LA    R1,1(R1)                                                         
         B     TSTMY08                                                          
                                                                                
TSTMYY   CR    RB,RB                                                            
         J     *+6                                                              
TSTMYN   LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
***********************************************************************         
* Get TimeSheet record for list - type 'Approval by status'           *         
***********************************************************************         
         SPACE 1                                                                
         USING GAPTABD,R4                                                       
TLGETAS  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*TLGETAS'                                                      
                                                                                
         LA    R2,IOKEY                                                         
         LA    R4,GAPAREA                                                       
                                                                                
         CLI   X#SVSEQ,0           first time for GAPTAB entry                  
         BNE   TLGAS28             no, READ NEXT FOR LAST PERIOD                
                                                                                
TLGAS01  XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
TLGAS02  TM    ATSRERRS,TSEEOF                                                  
         JNZ   TLGETASN                                                         
         TM    GAPTSTA,GAPTSMQ                                                  
         BZ    TLGAS06                                                          
         CLI   RQ_APP1R,C'Y'       CHECK 1RS LATER?                             
         BE    TLGAS03                                                          
         CLI   GAPTDAT1,GAPTT1Q    1R entry                                     
         BE    TLGAS08                                                          
TLGAS03  CLI   GAPTDAT1,GAPTT3Q    1N entry                                     
         BE    TLGAS20                                                          
         CLI   GAPTDAT1,GAPTT2Q    SJ entry                                     
         BE    TLGAS20                                                          
         CLI   GAPTDAT1,GAPTT6Q    Media with SJ entry                          
         BE    TLGAS20                                                          
         B     TLGAS06                                                          
TLGAS04  GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
TLGAS06  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         J     TLGAS02                                                          
*                                                                               
*                     1R PROCESSING                                             
TLGAS08  DS    0H                  OFFAL CHECK                                  
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         LA    RE,1                IF 2 CHAR OFFICE PERSON CODE SHORTER         
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         JNZ   *+8                                                              
         LA    RE,0                                                             
         MVC   OFFAOFFC(0),GAPTACT                                              
         EX    RE,*-6                                                           
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         JNE   TLGAS06                                                          
         DROP  R1                                                               
*                                                                               
         LLC   RE,ONERL1L          Set current office for calendar              
         BCTR  RE,0                                                             
         MVC   TS#1ROFF,SPACES                                                  
         MVC   TS#1ROFF(0),GAPTCODE                                             
         EX    RE,*-6                                                           
         MVC   TS#COFF,SPACES                                                   
         MVC   TS#COFF(0),GAPTCODE                                              
         EX    RE,*-6                                                           
         L     RF,AIO5             PERTAB ALREADY FOR THIS OFFICE?              
         OC    PERODS-PERTABD(0,RF),PERODS-PERTABD(RF)                          
         EX    RE,*-6                                                           
         BZ    TLGAS10                                                          
         CLC   PERODS-PERTABD(0,RF),TS#COFF                                     
         EX    RE,*-6                                                           
         BE    TLGAS20                                                          
*                                                                               
TLGAS10  GOTOR CALPRDS,RUNNOPER    REBUILD CALENDAR FOR THIS OFFICE             
         JE    TLGAS12                                                          
         MVC   LP_ERROR,FULL2                                                   
         J     XERROR                                                           
TLGAS12  LLC   R4,X#DITMS                                                       
         GOTO1 VXSORT,DMCB,(X'00',AIO5),(R4),PERLENQ,PERKEYQ,0                  
         LA    R4,GAPAREA                                                       
*                                                                               
*                                                                               
*                SJ/1N START HERE                                               
         USING PERTABD,R3                                                       
TLGAS20  L     R3,AIO5                                                          
         ST    R3,X#ACALL                                                       
         MVI   X#SVSEQ,FF          Set not first time                           
         OC    TS#PEDTE,TS#PEDTE   Have we got a period end for key             
         BNZ   *+10                Yes                                          
         MVC   TS#PEDTE,TS#CRED    Set end date from calling routine            
*                                                                               
         USING TAPPASD,R2                                                       
TLGAS22  LA    R2,IOKEY            build general part of key                    
         XC    TAPPAS,TAPPAS                                                    
         MVI   TAPPTYP,TAPPTYPQ                                                 
         MVI   TAPPSUB,TAPPSUBQ                                                 
         MVC   TAPPCPY,CUXCPY                                                   
         MVC   TAPPKYST,X#FLTST                                                 
         MVC   TAPPPEDT,PERENDT                                                 
         CLI   GAPTDAT1,GAPTT1Q    1R entry                                     
         BE    *+10                                                             
         MVC   TAPPPEDT,TS#PEDTE                                                
         MVC   TAPPCAT,GAPTAPPL                                                 
         MVC   TAPCODE,GAPTCODE                                                 
         XC    X#S1RAC,X#S1RAC                                                  
                                                                                
TLGAS24  MVC   CSVKEY1,TAPPAS      save current start key                       
                                                                                
TLGAS26  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    TLGAS32                                                          
         DC    H'0'                no errors should occur                       
*                                                                               
TLGAS28  MVC   IOKEY,CSVKEY1       RESTORE LAST TAPPAS                          
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
*                                                                               
TLGAS30  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         BE    TLGAS32                                                          
         DC    H'0'                no errors should occur                       
*                                                                               
TLGAS32  LA    R2,IOKEY                                                         
         TM    TAPPSTAT,TIMSDELT                                                
         BNZ   TLGAS30                                                          
         MVC   GAPAREA2,GAPAREA                                                 
         CLC   CSVKEY1(TAPPPEDT-TAPPAS),TAPPAS                                  
         BNE   TLGAS90             next GAPTAB entry as end of IO               
                                                                                
         CLC   TAPPPEDT,TS#CRSD                                                 
         BH    TLGAS90             next GAPTAB entry as end of period           
                                                                                
         CLC   CSVKEY1(TAPMODSP-TAPPAS),TAPPAS                                  
         BE    TLGAS50                                                          
         CLI   GAPTDAT1,GAPTT1Q    1R entry                                     
         BE    TLGAS48                                                          
TLGAS34  XR    RE,RE                                                            
         ICM   RE,7,TS#PEDTE                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,7,TS#PEDTE                                                    
         CLC   TS#PEDTE,TS#CRSD                                                 
         BE    TLGAS90                                                          
         CLC   TS#PEDTE,TAPPPEDT                                                
         BL    TLGAS34                                                          
         B     TLGAS22                                                          
                                                                                
TLGAS48  L     R3,X#ACALL                                                       
         LA    R3,PERLENQ(R3)                                                   
         ST    R3,X#ACALL                                                       
         CLI   0(R3),0                                                          
         BE    TLGAS90                                                          
         CLC   PERENDT,TAPPPEDT                                                 
         BL    TLGAS48                                                          
         B     TLGAS22                                                          
*                                                                               
TLGAS50  LLC   RE,GAPTLEN          test same 1R/SJ account                      
         SHI   RE,1                                                             
         EX    RE,TLGAS54                                                       
         BE    TLGAS56             next GAPTAB entry as end of a/c              
         CLI   GAPTDAT1,GAPTT1Q    1R entry                                     
         BNE   TLGAS52                                                          
         L     R3,X#ACALL                                                       
         LA    R3,PERLENQ(R3)                                                   
         ST    R3,X#ACALL                                                       
         CLI   0(R3),0                                                          
         BE    TLGAS90                                                          
         B     TLGAS22                                                          
                                                                                
TLGAS52  XR    RE,RE                                                            
         ICM   RE,7,TS#PEDTE                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,7,TS#PEDTE                                                    
         CLC   TS#PEDTE,TS#CRSD                                                 
         BE    TLGAS90                                                          
         B     TLGAS22                                                          
*                                                                               
TLGAS54  CLC   TAPMODSP(0),CSVKEY1+TAPMODSP-TAPPASD                             
*                                                                               
TLGAS56  MVC   CSVKEY1,IOKEY       UPDATE SAVED KEY                             
         CLI   GAPTDAT1,GAPTT6Q                                                 
         BE    TLGAS58                                                          
         CLI   GAPTDAT1,GAPTT3Q                                                 
         BE    TLGAS58                                                          
         CLI   GAPTDAT1,GAPTT2Q                                                 
         BNE   TLGAS65                                                          
*                                                                               
TLGAS58  LA    R1,TAPCODE          test whether Cli/pro/job wanted              
         GOTOR TTSJVGT                                                          
         BE    TLGAS70                                                          
*                                                                               
TLGAS60  XR    RE,RE               read high for next a/c                       
         IC    RE,TAPCACT+L'TAPCACT-1                                           
         AHI   RE,1                                                             
         STC   RE,TAPCACT+L'TAPCACT-1                                           
         B     TLGAS26                                                          
*                                                                               
TLGAS65  CLI   GAPTDAT1,GAPTT1Q                                                 
         BNE   TLGAS70                                                          
*                                                                               
         MVC   TS#1RACT,TAPMODSP                                                
         CLC   X#S1RAC,TS#1RACT    match on previous                            
         BE    TLGAS70                                                          
*                                                                               
         GOTOR TT1RVGT             test against table                           
         BE    TLGAS68                                                          
*                                                                               
         MVI   TAPMCULA,FF         reset key to next record                     
         B     TLGAS26                                                          
*                                                                               
TLGAS68  MVC   X#S1RAC,TS#1RACT                                                 
*                                                                               
TLGAS70  ZAP   X#SVHRS,TAPPHRS     save values and return to main               
         ZAP   X#SVMTS,TAPPMTS                                                  
         MVC   TS#RECDA,TAPPDA                                                  
         MVC   TS#1ROFF(L'TS#1ROFF+L'TS#1RDPT+L'TS#1RSDP),SPACES                
         MVC   TS#PRSAC,SPACES                                                  
         SR    R1,R1                                                            
         ICM   R1,7,TAPPPEDT                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TS#PEDT                                                     
         MVC   TS#STAT,TAPPKYST                                                 
*                                                                               
         CLI   TAPPCAT,TAPPMAN     1R pointer?                                  
         BNE   TLGAS72                                                          
*                                                                               
         MVI   TS#ATYP,TD#1RQ                                                   
         TM    X#SRCIND,X#SRCRET   ARE WE ON APPROVER PASS?                     
         JZ    *+8                                                              
         MVI   TS#ATYP,TD#1RAQ     YES, DIFFERENT TYPE                          
         LA    RF,TAPMODSP                                                      
         XR    R1,R1                                                            
         IC    R1,ONERL1L                                                       
         AHI   R1,-1                                                            
         MVC   TS#1ROFF(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         SR    R0,R0                                                            
         IC    R0,ONERL1L                                                       
         IC    R1,ONERL2L                                                       
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         MVC   TS#1RDPT(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         MVC   TS#1RSDP(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL3L                                                       
         IC    R1,ONERL4L                                                       
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         MVC   TS#PRSAC(0),0(RF)                                                
         EX    R1,*-6                                                           
         B     TLGETASY                                                         
*                                                                               
TLGAS72  CLI   TAPPCAT,TAPPCLI        handle SJ/1N here                         
         BE    TLGAS74                                                          
         CLI   TAPPCAT,TAPP1NA                                                  
         BE    TLGAS74                                                          
         CLI   TAPPCAT,TAPPMED                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
TLGAS74  MVI   TS#ATYP,TD#SJ1NQ                                                 
         TM    X#SRCIND,X#SRCRET   ARE WE ON APPROVER PASS?                     
         JZ    *+8                                                              
         MVI   TS#ATYP,TD#SJ1AQ    YES, DIFFERENT TYPE                          
         LA    RF,TAPCPODS                                                      
         XR    R1,R1               for main entry save office                   
         XR    R0,R0                                                            
         IC    R0,ONERL3L                                                       
         IC    R1,ONERL4L                                                       
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         MVC   TS#PRSAC(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R1,ONERL1L                                                       
         AHI   R1,-1                                                            
         MVC   TS#1ROFF,SPACES                                                  
         MVC   TS#1ROFF(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL1L                                                       
         IC    R1,ONERL2L                                                       
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         MVC   TS#1RDPT(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         MVC   TS#1RSDP(0),0(RF)                                                
         EX    R1,*-6                                                           
                                                                                
         IC    R1,ONERL1L                                                       
         AHI   R1,-1                                                            
         MVC   TS#COFF,TS#1ROFF                                                 
         L     RF,AIO5             PERTAB ALREADY FOR THIS OFFICE?              
         OC    PERODS-PERTABD(0,RF),PERODS-PERTABD(RF)                          
         EX    R1,*-6                                                           
         BZ    TLGAS76                                                          
         CLC   PERODS-PERTABD(0,RF),TS#COFF                                     
         EX    R1,*-6                                                           
         BE    TLGAS80                                                          
*                                                                               
TLGAS76  GOTOR CALPRDS,RUNNOPER    REBUILD CALENDAR FOR THIS OFFICE             
         JE    TLGAS78                                                          
         MVC   LP_ERROR,FULL2                                                   
         J     XERROR                                                           
TLGAS78  MVC   IOKEY,CSVKEY1       Restore key after CALPRDS routine            
         LLC   R4,X#DITMS                                                       
         GOTO1 VXSORT,DMCB,(X'00',AIO5),(R4),PERLENQ,PERKEYQ,0                  
         LA    R4,GAPAREA                                                       
TLGAS80  L     R3,AIO5             Find period end date                         
*        ST    R3,X#ACALL                                                       
TLGAS82  CLC   PERENDT,TAPPPEDT                                                 
         BE    TLGETASY                                                         
         LA    R3,PERLENQ(R3)                                                   
         CLI   0(R3),0                                                          
         BNE   TLGAS82                                                          
TLGAS88  B     TLGETASY                                                         
*                                                                               
*                                                                               
TLGAS90  MVI   X#SVSEQ,0           prepare next GAPTAB entry                    
         XC    TS#PEDTE,TS#PEDTE                                                
         B     TLGAS04                                                          
*                                                                               
TLGETASY CR    RB,RB                                                            
         J     *+6                                                              
TLGETASN LTR   RB,RB                                                            
                                                                                
         XIT1                                                                   
         DROP  R2,R4                                                            
         LTORG                                                                  
***********************************************************************         
* Get TimeSheet record for list - type 'Approval by date'             *         
***********************************************************************         
         SPACE 1                                                                
         USING GAPTABD,R4                                                       
TLGETAD  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*TLGETAD'                                                      
                                                                                
         LA    R2,IOKEY                                                         
         LA    R4,GAPAREA                                                       
                                                                                
         MVC   X#SEQN,X#SVSEQ                                                   
         CLI   GAPLCALT,GAPLALLC   are we reading all clients                   
         BE    TLGAC00             yes                                          
         CLI   X#SVSEQ,0                                                        
         BNE   TLGAD10                                                          
         XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
TLGAD02  TM    ATSRERRS,TSEEOF                                                  
         JNZ   TLGETADN            Buffer is empty                              
         TM    GAPTSTA,GAPTSMQ     main entry                                   
         BZ    TLGAD06                                                          
         CLI   RQ_APP1R,C'Y'       SAVE 1RS FOR LATER?                          
         BE    TLGAD03                                                          
         CLI   GAPTDAT1,GAPTT1Q    1R entry                                     
         BE    TLGAD08                                                          
TLGAD03  CLI   GAPTDAT1,GAPTT3Q    1N entry                                     
         BE    TLGAD08                                                          
         CLI   GAPTDAT1,GAPTT2Q    SJ entry                                     
         BE    TLGAD08                                                          
         CLI   GAPTDAT1,GAPTT6Q    Media with SJ entry                          
         BE    TLGAD08                                                          
         B     TLGAD06                                                          
TLGAD04  GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
TLGAD06  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         J     TLGAD02                                                          
                                                                                
TLGAD08  MVI   X#SVSEQ,FF                                                       
                                                                                
TLGAD10  CLI   GAPTDAT1,GAPTT1Q    current entry 1R?                            
         BNE   TLGAJ00                                                          
*                                                                               
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         LA    RE,1                IF 2 CHAR OFFICE PERSON CODE SHORTER         
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         BNZ   *+8                                                              
         LA    RE,0                                                             
         MVC   OFFAOFFC(0),GAPTACT                                              
         EX    RE,*-6                                                           
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         BNE   TLGAD06                                                          
         DROP  R1                                                               
*                                                                               
         CLC   GAPTLEN,ONERL1L                                                  
         JL    TLGAT00             NOT ENOUGH TO WORK WITH                      
         CLI   TS#PTYPE,TS#PTLAS   All list calls can avoid this                
         JNH   TLGAT00             search filtering                             
         CLI   TS#PTYPE,TS#PTSMS   All summary calls can avoid this             
         JNL   TLGAT00             search filtering                             
                                                                                
         CLC   RQ_TTOFF,SPACES     OFFICE FILTER?                               
         BNH   TLGAT00                                                          
         LLC   RF,ONERL1L                                                       
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         JNE   TLGAD06             DON'T WANT THIS                              
         CLC   GAPTACT(0),RQ_TTOFF                                              
*                                                                               
         CLC   GAPTLEN,ONERL2L                                                  
         JL    TLGAT00             NOT ENOUGH TO WORK WITH                      
         CLC   RQ_TTDEP,SPACES     OFFICE FILTER?                               
         BNH   TLGAT00                                                          
         LA    RE,GAPTACT                                                       
         LLC   R0,ONERL1L                                                       
         AR    RE,R0               A(DEP IN GAPTACT                             
         LLC   RF,ONERL2L                                                       
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         JNE   TLGAD06             DON'T WANT THIS                              
         CLC   0(0,RE),RQ_TTDEP                                                 
*                                                                               
         CLC   GAPTLEN,ONERL3L                                                  
         JL    TLGAT00             NOT ENOUGH TO WORK WITH                      
         CLC   RQ_TTSUB,SPACES     OFFICE FILTER?                               
         BNH   TLGAT00                                                          
         LA    RE,GAPTACT                                                       
         LLC   R0,ONERL2L                                                       
         AR    RE,R0               A(SUBDEP IN GAPTACT                          
         LLC   RF,ONERL3L                                                       
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         JNE   TLGAD06             DON'T WANT THIS                              
         CLC   0(0,RE),RQ_TTSUB                                                 
*                                                                               
TLGAT00  DS    0H                                                               
         TM    X#ACCSTA,X#CLISR    Are we doing a cli search?                   
         BO    TLGETADN                                                         
*                                                                               
         USING TDTPASD,R2                                                       
         CLI   X#SEQN,0            first time for GAPTAB/1R entry               
         BNE   TLGAT15             no                                           
         TM    TS#ICPJ,TS#IPERR    Do we have a person code                     
         JNZ   TLGAT04             Yes - Period table will be ok                
*                                                                               
         LLC   RE,ONERL1L          make sure we have correct calendar           
         SHI   RE,1                    for office                               
         CLC   TS#1ROFF(0),GAPTACT                                              
         EX    RE,*-6                                                           
         BE    TLGAT04                                                          
         MVC   TS#1ROFF,SPACES                                                  
         MVC   TS#1ROFF(0),GAPTACT                                              
         EX    RE,*-6                                                           
         GOTOR CALPRDS,RUNNOPER        CALPRDS MUSTN'T CALL CALPPER             
         LLC   RF,X#DITMS          AND RESORT                                   
         GOTO1 VXSORT,DMCB,(X'00',AIO5),(RF),PERLENQ,PERKEYQ,0                  
*                                                                               
TLGAT04  L     R3,AIO5                                                          
         ST    R3,X#ACALL                                                       
         USING PERTABD,R3                                                       
TLGAT05  L     R3,X#ACALL                                                       
         CLI   0(R3),0                                                          
         BNE   *+12                                                             
         MVI   X#SEQN,0                                                         
         B     TLGAD04             end of this office                           
         XC    TDTPAS,TDTPAS       build start passive for entry                
         MVI   TDTPTYP,TDTPTYPQ                                                 
         MVI   TDTPSUB,TDTPSUBQ                                                 
         MVC   TDTPCPY,CUXCPY                                                   
         MVC   TDTPODSP,GAPTACT                                                 
         OC    TDTPODSP,SPACES                                                  
         MVC   TDTPPEDT,PERENDT                                                 
         MVC   CSVKEY1,TDTPAS      save start key and clear hours               
         XC    X#S1RAC,X#S1RAC                                                  
*                                                                               
TLGAT10  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    TLGAT22                                                          
         DC    H'0'                no errors should occur                       
*                                                                               
TLGAT15  DS    0H                                                               
         MVC   IOKEY,CSVKEY1       RESTORE READ SEQ AFTER PROPTSR               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
*                                                                               
TLGAT20  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         BE    TLGAT22                                                          
         DC    H'0'                no errors should occur                       
*                                                                               
TLGAT22  CLC   CSVKEY1(TDTPPEDT-TDTPAS),TDTPAS                                  
         BE    TLGAT23                                                          
         MVI   X#SEQN,0                                                         
         B     TLGAD04             end of this company                          
                                                                                
* Code below to optimise i/o reads as no need to read for time that             
* completed before start date                                                   
TLGAT23  DS    0H                                                               
         CLC   TDTPPEDT,TS#CRSD                                                 
         BNH   *+12                                                             
         MVI   X#SEQN,0                                                         
         B     TLGAD04             end of enq period                            
                                                                                
         CLC   CSVKEY1(TDTPODSP-TDTPAS),TDTPAS                                  
         BE    TLGAT26                                                          
*                                                                               
TLGAT24  L     R3,X#ACALL          end of this period.                          
         LA    R3,PERLENQ(R3)                                                   
         ST    R3,X#ACALL                                                       
         CLI   0(R3),0                                                          
         BNE   *+12                                                             
         MVI   X#SEQN,0                                                         
         B     TLGAD04             end of this office                           
         CLC   PERENDT,TDTPPEDT    Skip to first found entry in PERTAB          
         BL    TLGAT24                                                          
         B     TLGAT05                                                          
*                                                                               
TLGAT26  MVC   GAPAREA2,GAPAREA                                                 
         LLC   RE,GAPTLEN                                                       
         SHI   RE,1                                                             
         CLC   TDTPODSP(0),CSVKEY1+TDTPODSP-TDTPASD                             
         EX    RE,*-6                                                           
         BE    TLGAT30                                                          
*                                                                               
TLGAT28  L     R3,X#ACALL      Bump to the next period in table                 
         LR    R1,R3           Use R1 to look ahead                             
TLGAT29  LA    R1,PERLENQ(R1)                                                   
         CLC   PERENDT,0(R1)   Check it's different to current period           
         BE    TLGAT29         No - bump to one that is                         
         ST    R1,X#ACALL       otherwise you'll read duplicates                
         B     TLGAT05                                                          
*                                                                               
TLGAT30  DS    0H                                                               
         MVC   CSVKEY1,IOKEY                                                    
         TM    TDTPSTAT,TIMSDELT                                                
         BNZ   TLGAT20                                                          
*                                                                               
* POST-FILTER FOR OFFICE/DPT/SUBDEP (GAPLST ENTRY MAY BE HIGH LEVEL)            
*                                                                               
         CLI   TS#PTYPE,TS#PTLAS   All list calls can avoid this                
         JNH   TLGAT31             search filtering                             
         CLI   TS#PTYPE,TS#PTSMS   All summary calls can avoid this             
         JNL   TLGAT31             search filtering                             
         CLC   RQ_TTOFF,SPACES     Office filter?                               
         BNH   TLGAT31                                                          
         LA    R1,TDTPODSP                                                      
         LLC   RF,ONERL1L          L'OFFICE CODE                                
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         JNE   TLGAT20             DON'T WANT THIS                              
         CLC   0(0,R1),RQ_TTOFF                                                 
*                                                                               
         CLC   RQ_TTDEP,SPACES     DEPT FILTER?                                 
         BNH   TLGAT31                                                          
         AHI   RF,1                                                             
         AR    R1,RF               A(DEPT)                                      
         LLC   RF,ONERL2L                                                       
         LLC   RE,ONERL1L                                                       
         SR    RF,RE               L'DEPT                                       
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         JNE   TLGAT20             DON'T WANT THIS                              
         CLC   0(0,R1),RQ_TTDEP                                                 
*                                                                               
         CLC   RQ_TTSUB,SPACES     SUBDPT FILTER?                               
         BNH   TLGAT31                                                          
         AHI   RF,1                                                             
         AR    R1,RF               A(DEPT)                                      
         LLC   RF,ONERL3L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RF,RE               L'DEPT                                       
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         JNE   TLGAT20             MATCH                                        
         CLC   0(0,R1),RQ_TTSUB                                                 
*                                                                               
TLGAT31  DS    0H                                                               
         CLI   RQ_TSSRC,RQ_TSTBA   BACK UP APPROVER                             
         BNE   TLGAT32                                                          
         LLC   RE,ONERL3L                                                       
         LLC   RF,ONERL4L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         LA    R1,TDTPODSP                                                      
         AR    R1,RE                                                            
         CLC   0(0,R1),TS#PCOD     BACKUP MUST IGNORE ITSELF                    
         EX    RF,*-6                                                           
         BE    TLGAT20             NEXT TIMESHEET                               
*                                                                               
TLGAT32  CLC   TDTPCOFF,SPACES     SKIP OFFAL TEST IF NO OFFICE                 
         BNH   TLGAT34                                                          
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,TDTPCOFF                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         BNE   TLGAT20                                                          
                                                                                
TLGAT34  MVC   TS#1RACT,TDTPODSP   build 1R account and filter it               
         CLC   X#S1RAC,TS#1RACT    match on previous                            
         BE    TLGAT36                                                          
                                                                                
         GOTOR TT1RVGT             test against table                           
         BE    TLGAT36                                                          
                                                                                
         MVI   TDTPODSP+L'TDTPODSP,FF                                           
         B     TLGAT10                                                          
*                                                                               
TLGAT36  DS    0H                                                               
         MVI   TS#ATYP,TD#1RQ                                                   
         TM    X#SRCIND,X#SRCRET   ARE WE ON APPROVER PASS?                     
         JZ    *+8                                                              
         MVI   TS#ATYP,TD#1RAQ     YES, DIFFERENT TYPE                          
         GOTOR TGDSET,DMCB,TDTPODSP,TDTPPEDT                                    
         B     TLGETADY                                                         
*                                                                               
* HERE FOR 1N/SJ-BASED SEARCHES/LISTS, AND CLIENT SUMMARY IF LIMLIST            
TLGAJ00  DS    0H                                 OR APPLIST APPLIES            
         CLI   X#SEQN,0                                                         
         BNE   TLGAJ07                                                          
         CLI   GAPTDAT1,GAPTT3Q    1N account                                   
         JE    TLGAJ02             Yes - skip office check                      
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         LA    RF,GAPTCOFF                                                      
         CLI   GAPTDAT1,GAPTT2Q    SJ office account                            
         JE    *+8                 Yes - Get office from correct pos            
         LA    RF,GAPTOFF          No - media office account                    
         MVC   OFFAOFFC,0(RF)                                                   
         CLC   OFFAOFFC,SPACES                                                  
         JE    TLGAJ02                                                          
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         BNE   TLGAD06                                                          
         DROP  R1                                                               
TLGAJ02  XC    X#S1RAC,X#S1RAC                                                  
         MVI   X#SVSEQ,FF                                                       
         USING TSJPAS,R2                                                        
         XC    TSJPAS,TSJPAS     clear key                                      
         XC    CSVKEY3,CSVKEY3                                                  
         MVI   TSJPTYP,TSJPTYPQ  build start key                                
         MVI   TSJPSUB,TSJPSUBQ                                                 
         MVC   TSJPCPY,CUXCPY                                                   
         MVC   TSJPVIEW,GAPTAPPL                                                
         MVC   TSJPCODE,GAPTCODE                                                
         OC    TSJPCODE,SPACES                                                  
         MVC   CSVKEY1,TSJPAS    and save                                       
*                                                                               
TLGAJ05  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    TLGAJ15                                                          
         DC    H'0'                no errors should occur                       
*                                                                               
*                       RE-ESTABLISHES READ SEQ, THEN FINDS NEXT                
TLGAJ07  MVC   IOKEY,CSVKEY1       HOPEFULLY PRESERVED SINCE EXIT TO            
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'      PROMTSR                  
         BE    TLGAJ10                                                          
         DC    H'0'                no errors should occur                       
*                                                                               
TLGAJ10  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         BE    TLGAJ15                                                          
         DC    H'0'                no errors should occur                       
*                                                                               
TLGAJ15  DS    0H                                                               
         CLC   TSJPVIEW,GAPTAPPL      CHECK AGAINST gaptab ENTRY                
         BNE   TLGAJ20                                                          
TLGAJ17  CLC   TSJPCODE(0),GAPTCODE                                             
         LLC   RE,GAPTLEN                                                       
         SHI   RE,1                                                             
         EX    RE,TLGAJ17                                                       
         BE    TLGAJ25                                                          
TLGAJ20  MVI   X#SEQN,0                                                         
         CLI   TS#PTYPE,TS#PTSMS   SEARCH READS NEXT GAPLST                     
         JL    TLGAD04                                                          
         CLI   TS#PTYPE,TS#PTSMA   IF SUMMARY TOTALS REQUEST                    
         JE    TLGAJ21             BUILD DUMMY ENTRIES FOR THIS GAPLST          
         CLI   TS#PTYPE,TS#PTSWA                                                
         JNE   TLGAD04             ELSE GET NEXT GAPLST                         
*                                                                               
TLGAJ21  GOTOR BLDCTSR                                                          
         BAS   RE,STRESET                                                       
         B     TLGAD04             NEXT SJ                                      
*                                                                               
TLGAJ25  XR    R1,R1               OFFAL FROM PERSON OFFICE                     
         XR    R0,R0                                                            
         IC    R0,ONERL3L                                                       
         IC    R1,ONERL4L                                                       
         SR    R1,R0                                                            
         LA    RF,TSJPPODS                                                      
         AR    RF,R1                                                            
         MVC   TS#COFF,SPACES                                                   
         IC    R1,ONERL1L                                                       
         SHI   R1,1                                                             
         MVC   TS#COFF(0),0(RF)                                                 
         EX    R1,*-6                                                           
*                                                                               
         CLC   TS#COFF,SPACES     Skip offal test if no office                  
         BNH   TLGAJ30                                                          
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,TS#COFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         BNE   TLGAJ10             Not valid                                    
*                                                                               
TLGAJ30  CLI   TSJPVIEW,TSJP1NAQ   is this from 1N                              
         JE    TLGAJ35             yeah, person office only                     
*                                                                               
         LA    RF,TSJPCOFF         client office validation                     
         CLI   TSJPVIEW,TSJPMEDQ   Are we doing SJ media view?                  
         JNE   *+8                                                              
         LA    RF,TSJPMOFF         No - Get correct field for office            
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,0(RF)                                                   
         CLC   OFFAOFFC,SPACES     Skip offal test if no office                 
         JNH   TLGAJ32                                                          
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         JNE   TLGAJ10             Not valid                                    
         DROP  R1                                                               
*                                                                               
TLGAJ32  LA    R1,TSJPCODE         test whether Cli/pro/job wanted              
         GOTOR TTSJVGT                                                          
         JE    TLGAJ35                                                          
         BAS   RE,STRESET                                                       
         J     TLGAD04             NEXT SJ GAPLST                               
*                                                                               
TLGAJ35  CLI   RQ_APP1R,C'Y'       Does user have limited hr search?            
         JNE   TLGAJ45                                                          
         LA    R1,TS#1RACT         build 1R account and filter it               
         MVC   TS#1RACT,SPACES                                                  
         XR    RF,RF                                                            
         IC    RF,ONERL4L                                                       
         XR    RE,RE                                                            
         IC    RE,ONERL3L                                                       
         SR    RF,RE                                                            
         LA    RE,TSJPPODS                                                      
         AR    RE,RF               Bump past person code                        
*                                                                               
         XR    RF,RF                                                            
         IC    RF,ONERL3L                                                       
         SHI   RF,1                                                             
         MVC   0(0,R1),0(RE)       COPY OFFICE/DEP/SUBDEP                       
         EX    RF,*-6                                                           
         LA    R1,1(RF,R1)                                                      
         XR    RF,RF                                                            
         IC    RF,ONERL4L                                                       
         XR    RE,RE                                                            
         IC    RE,ONERL3L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         MVC   0(0,R1),TSJPPODS    ADD ON PERSON                                
         EX    RF,*-6                                                           
*                                                                               
         CLC   X#S1RAC,TS#1RACT    match on previous                            
         JE    TLGAJ44                                                          
         XC    GAPAREA2,GAPAREA2                                                
         GOTOR TT1RVGT             test against table                           
         JE    TLGAJ44                                                          
         LLC   RF,TSJPPODS+L'TSJPPODS-1                                         
         LA    RF,1(RF)                                                         
         STC   RF,TSJPPODS+L'TSJPPODS-1  FORCE TO NEXT SUBDEP                   
         J     TLGAJ05                                                          
*                                                                               
TLGAJ44  MVC   X#S1RAC,TS#1RACT                                                 
*                                                                               
TLGAJ45  SR    RF,RF                                                            
         ICM   RF,7,TSJPPEDT       set MOAs for GETCAL to read for              
         LNR   RF,RF                                                            
         STCM  RF,7,FULL1                                                       
         STCM  RF,6,TS#CDAT                                                     
         STCM  RF,6,TS#CEND                                                     
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,SCPYEL+CPYSFST-CPYELD                                       
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         BH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
*                                                                               
         MVC   TS#CDAT+1(1),BYTE1  START MONTH IS FISCAL MONTH                  
*                                                                               
         CLC   TS#CEND+1(1),BYTE1  START YEAR IS LAST YEAR IF PERIOD            
         BNL   TLGAJ47               END MONTH BEFORE FISCAL MONTH              
         GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CDAT)                              
*                                                                               
TLGAJ47  GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)    END MONTH IS                
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'11'    FISCAL+11                
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CEND)                              
*                                                                               
         MVC   CSVKEY1,TSJPAS                                                   
         GOTOR GETCAL              MAKE SURE WE HAVE MATCHING CALENDAR          
         MVC   IOKEY,CSVKEY1                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         BE    *+6                                                              
         DC    H'0'                no errors should occur                       
*                                                                               
         L     R1,AIO8             FIND T/S PERIOD                              
         LA    R3,CASRFST-CASKEY(R1)                                            
         USING TMPELD,R3                                                        
         XR    R0,R0                                                            
TLGAJ50  CLI   TMPEL,0                                                          
         BE    TLGAJ10                                                          
         CLI   TMPEL,TMPELQ                                                     
         BNE   TLGAJ55                                                          
         CLC   TMPSTART,FULL1      FULL1 IS UNCOMPLEMENTED TSJPPEND             
         BH    TLGAJ10                                                          
         CLC   TMPEND,FULL1                                                     
         BNL   TLGAJ60                                                          
TLGAJ55  IC    R0,TMPLN                                                         
         AR    R3,R0                                                            
         B     TLGAJ50                                                          
*                                                                               
TLGAJ60  CLC   TMPSTART,TS#CSDT    IS START DATE LOWER THAN PERIOD DATE         
         JNL   TLGAJ62             NO - CHECK END DATE                          
         CLC   TMPEND,TS#CSDT      IS START DATE LOWER THAN PERIOD END          
         JNL   TLGAJ70                                                          
         MVI   TSJPPEDT,X'FF'      BEFORE SEARCH PERIOD, SKIP TO NEXT           
         J     TLGAJ05                                                          
TLGAJ62  CLC   TMPEND,TS#CEDT      IS PERIOD END DATE LOWER THAN END            
         JNH   TLGAJ70             YES                                          
         CLC   TMPSTART,TS#CEDT    IS PERIOD START DATE HIGHER THAN END         
         JNH   TLGAJ70                                                          
*                                                                               
*  NO, FIND FIRST TMPEL AFTER REQ PERIOD AND SKIP I/O TO THAT                   
*            REV CHRON RECORD ORDER WILL MOONWALK ACROSS PERIOD.                
         LA    R3,CASRFST-CASKEY(R1)                                            
TLGAJ65  CLI   TMPEL,0                                                          
         JE    TLGAJ10             END DATE IS LAST PERIOD OF YEAR,             
         CLI   TMPEL,TMPELQ                       CARRY ON                      
         JNE   TLGAJ67                                                          
         CLC   TMPSTART,TS#CEDT                                                 
         JH    TLGAJ68                                                          
*                                                                               
TLGAJ67  LLC   RE,TMPLN                                                         
         AR    R3,RE                                                            
         J     TLGAJ65                                                          
*                                                                               
TLGAJ68  DS    0H                                                               
         ICM   RF,7,TMPSTART       PUSH KEY TO DAY AFTER PERIOD                 
         LNR   RF,RF                                                            
         CLM   RF,7,TSJPPEDT                                                    
         JE    TLGAJ10             READ SEQ IF NO CHANGE                        
         STCM  RF,7,TSJPPEDT                                                    
         MVI   TSJPPODS,C' '                                                    
         J     TLGAJ05             READ HIGH FROM HERE                          
*                                                                               
TLGAJ70  GOTOR DUMPERT             build bummy pertab from tmpel                
*                                                                               
         MVI   TS#ATYP,TD#SJ1NQ                                                 
         TM    X#SRCIND,X#SRCRET   ARE WE ON APPROVER PASS?                     
         JZ    *+8                                                              
         MVI   TS#ATYP,TD#SJ1AQ    YES, DIFFERENT TYPE                          
         MVC   CSVKEY1,TSJPAS      start here on re-entry                       
         GOTOR TGDSET,DMCB,TSJPPODS,TSJPPEDT                                    
         GOTOR PRODUM              Handle dummies                               
         B     TLGETADY                                                         
*                                                                               
* CLIENT SUMMARY CALLS HANDLED HERE, ALSO SJ SEARCH IF NO LIMLIST/              
*                                                    APPROVER LIMITS?           
         USING TSJPASD,R2                                                       
TLGAC00  CLI   X#SEQN,0            first time for GAPTAB/SJ entry               
         BNE   TLGAC15                                                          
         XC    X#S1RAC,X#S1RAC                                                  
*                                                                               
TLGAC02  MVI   X#SVSEQ,FF                                                       
         XC    TSJPAS,TSJPAS     clear key                                      
         XC    CSVKEY3,CSVKEY3                                                  
         MVI   TSJPTYP,TSJPTYPQ  build start key                                
         MVI   TSJPSUB,TSJPSUBQ                                                 
         MVC   TSJPCPY,CUXCPY                                                   
         MVC   CSVKEY1,TSJPAS      save start key                               
         CLI   TS#PTYPE,TS#PTSMC   details run off tsar recs                    
         JE    TLGAC04                                                          
         CLI   TS#PTYPE,TS#PTSWC                                                
         JNE   TLGAC10                                                          
                                                                                
TLGAC04  XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         MVC   TSJPCODE,GAPTACT    office/cli code entries from bldovr          
         MVI   TSJPVIEW,TSJPSJAQ                                                
         CLI   GAPTDAT1,GAPTT3Q    Is it 1N account                             
         JNE   *+8                 No                                           
         MVI   TSJPVIEW,TSJP1NAQ   Yes                                          
         MVC   CSVKEY1,TSJPAS      save start key                               
*                                                                               
TLGAC10  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    TLGAC22                                                          
         DC    H'0'                no errors should occur                       
*                                                                               
TLGAC15  DS    0H                 RESTORE RSEQ AFTER PROPTSR/PROMTSR            
         MVC   IOKEY,CSVKEY1                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
*                                                                               
TLGAC20  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         BE    TLGAC22                                                          
         DC    H'0'                no errors should occur                       
*                                                                               
* Check company, rectype, view                                                  
TLGAC22  LA    RF,TSJPCODE-TSJPAS                                               
         CLI   TS#PTYPE,TS#PTSMC   details run off GAPLST, so can               
         JE    *+16                                 check more                  
         CLI   TS#PTYPE,TS#PTSWC                                                
         JE    *+8                                                              
         LA    RF,TSJPVIEW-TSJPAS                                               
         AHI   RF,-1                                                            
         CLC   CSVKEY1(0),TSJPAS                                                
         EX    RF,*-6                                                           
         BNE   TLGETADN                                                         
         CLI   TSJPVIEW,TSJPMEDQ   Are we at MEDIA view?                        
         BE    TLGETADN            we are done                                  
*                                                                               
         MVC   CSVKEY1,TSJPAS                                                   
         LA    RE,TSJPACT          Assume we are doing SJ view                  
         CLI   TSJPVIEW,TSJPSJAQ   Are we doing SJ view?                        
         BE    *+8                                                              
         LA    RE,TSJP1NAC         Set Address of 1N field instead              
         CLC   0(L'TSJPACT,RE),SPACES      IGNORE NO-CLIENT LINES               
         JNH   TLGAC20                                                          
*                        CHECK OFFICE/CLIENT MATCHES IF DETAIL                  
         CLC   GAPTACT,SPACES                                                   
         JNH   TLGAC32             ASSUMING WE HAVE SOMETHING...                
         CLI   TS#PTYPE,TS#PTSMC                                                
         JE    TLGAC25                                                          
         CLI   TS#PTYPE,TS#PTSWC                                                
         JNE   TLGAC32                                                          
TLGAC25  DS    0H                                                               
         CLI   TSJPVIEW,TSJP1NAQ                                                
         JNE   TLGAC27                                                          
         CLC   TSJP1NAC,GAPTACT                                                 
         JNE   TLGAC29                                                          
         J     TLGAC32                                                          
*                                                                               
TLGAC27  DS    0H                                                               
         LLC   RF,PCLILEN                                                       
         AHI   RF,L'TSJPCOFF-1                                                  
         LA    RE,TSJPCOFF                                                      
         CLI   TSJPVIEW,TSJPMEDQ                                                
         JNE   *+8                                                              
         LA    RE,TSJPMOFF                                                      
         CLC   0(0,RE),GAPTACT                                                  
         EX    RF,*-6                                                           
         JE    TLGAC32                                                          
*                                                                               
TLGAC29  DS    0H                                                               
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF            NEXT BLDOVR ENTRY                            
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   TLGETADN                                                         
         MVC   TSJPCODE,GAPTACT    NEW KEY                                      
         XC    TSJPPEDT,TSJPPEDT                                                
         MVC   CSVKEY1,TSJPAS      save start key                               
         J     TLGAC10                                                          
*                                                                               
TLGAC32  MVC   GAPAREA2,GAPAREA                                                 
         TM    TSJPSTAT,TIMSDELT                                                
         JNZ   TLGAC20                                                          
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,TSJPCOFF   Get office from correct position             
         CLI   TSJPVIEW,TSJPSJAQ   Are we doing SJ view?                        
         JE    TLGAC33             No need to check office                      
         LLC   RF,ONERL4L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RF,RE               L'PERSON                                     
         LA    RF,TSJPPODS(RF)     EXTRACT OFFICE FROM CORRECT POS'N            
         MVC   HALF1,0(RF)                                                      
         LLC   RF,ONERL1L                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   OFFAOFFC(0),HALF1   ALLOW FOR 1CO/2CO                            
         EX    RF,0(RE)                                                         
*                                                                               
TLGAC33  DS    0H                                                               
         CLC   OFFAOFFC,SPACES     Skip offal test if no office                 
         JNH   TLGAC34                                                          
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         JE    TLGAC34             Not valid                                    
         AR    RF,RE                                                            
         MVI   TSJPACT,X'FF'       FORCE TO NEXT OFFICE                         
         J     TLGAC10                                                          
         DROP  R1                                                               
TLGAC34  TM    X#SRCIND,X#SRCAPP   are we an approver                           
         JZ    TLGAC36             no                                           
         CLI   TS#PTYPE,TS#PTLAD   yes - are we listing by date                 
         JNE   TLGAC36             no - could be search or summary              
         CLI   TSJPKSTA,X'FF'      yes - check whether this time needs          
         JE    TLGAC20                            client approval               
*                                                                               
TLGAC36  LA    R1,TS#1RACT         build 1R account and filter it               
         MVC   TS#1RACT,SPACES                                                  
         XR    RF,RF                                                            
         IC    RF,ONERL4L                                                       
         XR    RE,RE                                                            
         IC    RE,ONERL3L                                                       
         SR    RF,RE                                                            
         LA    RE,TSJPPODS                                                      
         AR    RE,RF               Bump past person code                        
*                                                                               
         XR    RF,RF                                                            
         IC    RF,ONERL3L                                                       
         SHI   RF,1                                                             
         MVC   0(0,R1),0(RE)       COPY OFFICE/DEP/SUBDEP                       
         EX    RF,*-6                                                           
         LA    R1,1(RF,R1)                                                      
         XR    RF,RF                                                            
         IC    RF,ONERL4L                                                       
         XR    RE,RE                                                            
         IC    RE,ONERL3L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         MVC   0(0,R1),TSJPPODS    ADD ON PERSON                                
         EX    RF,*-6                                                           
*                                                                               
         CLI   RQ_APP1R,C'Y'       Does user have limited hr search?            
         JNE   TLGAC43                                                          
         CLC   X#S1RAC,TS#1RACT    match on previous                            
         JE    TLGAC42                                                          
         XC    GAPAREA2,GAPAREA2                                                
         GOTOR TT1RVGT             test against table                           
         JE    TLGAC42                                                          
*                                                                               
         LLC   RF,TSJPPODS+L'TSJPPODS-1                                         
         LA    RF,1(RF)                                                         
         STC   RF,TSJPPODS+L'TSJPPODS-1  FORCE TO NEXT SUBDEP                   
         J     TLGAC10                                                          
*                                                                               
TLGAC42  MVC   X#S1RAC,TS#1RACT                                                 
*                                                                               
TLGAC43  CLC   TSJPPEDT,TS#CRSD    CHECK (COMPLEMENTED) END DATE                
         JL    TLGAC44                                                          
         MVI   TSJPPEDT,X'FF'      BEFORE SEARCH PERIOD, SKIP TO NEXT           
         J     TLGAC10                                     SJ                   
*                                                                               
TLGAC44  SR    RF,RF               Read calendar to get start date              
         ICM   RF,7,TSJPPEDT                                                    
         LNR   RF,RF                                                            
         STCM  RF,7,FULL1                                                       
         STCM  RF,6,TS#CDAT        set MOAs for GETCAL to read for              
         STCM  RF,6,TS#CEND                                                     
         XR    RE,RE                                                            
         ICM   RE,1,SCPYEL+CPYSFST-CPYELD                                       
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         BH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
*                                                                               
         MVC   TS#CDAT+1(1),BYTE1  START MONTH IS FISCAL MONTH                  
*                                                                               
         CLC   TS#CEND+1(1),BYTE1  START YEAR IS LAST YEAR IF PERIOD            
         BNL   TLGAC45               END MONTH BEFORE FISCAL MONTH              
         GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CDAT)                              
*                                                                               
TLGAC45  GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)    END MONTH IS                
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'11'    FISCAL+11                
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CEND)                              
         LLC   RF,ONERL1L                                                       
         SHI   RF,1                                                             
TLGAC46  MVC   TS#COFF(0),TS#1RACT                                              
         EX    RF,TLGAC46                                                       
         MVC   CSVKEY1,TSJPAS                                                   
         GOTOR GETCAL              Make sure we have matching calendar          
         JNH   *+6                                                              
         DC    H'0'                Can't find calendar?                         
         MVC   IOKEY,CSVKEY1       and restore read sequence                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    TLGAC47                                                          
         DC    H'0'                no errors should occur                       
*                                                                               
TLGAC47  L     R1,AIO8             Find T/S period (NEAREST MATCH)              
         LA    R3,CASRFST-CASKEY(R1)                                            
         USING TMPELD,R3                                                        
         XR    R0,R0                                                            
TLGAC50  CLI   TMPEL,0                                                          
         JNE   TLGAC51                                                          
         DC    H'0'                Wrong calendar record                        
TLGAC51  CLI   TMPEL,TMPELQ                                                     
         JNE   TLGAC55                                                          
         CLC   TMPSTART,FULL1      Full1 is umcomplemented TSJPPEND             
         JH    TLGAC20                 no matching tmpel                        
         CLC   TMPEND,FULL1                                                     
         JNL   TLGAC60                                                          
*                                                                               
TLGAC55  IC    R0,TMPLN                                                         
         AR    R3,R0                                                            
         J     TLGAC50                                                          
*                                                                               
TLGAC60  CLC   TMPSTART,TS#CSDT    IS START DATE LOWER THAN PERIOD DATE         
         JNL   TLGAC62             NO - CHECK END DATE                          
         CLC   TMPEND,TS#CSDT      IS START DATE LOWER THAN PERIOD END          
         JNL   TLGAC70                                                          
         MVI   TSJPPEDT,X'FF'      BEFORE SEARCH PERIOD, SKIP TO NEXT           
         J     TLGAC10             NO - NOT INTERESTED                          
TLGAC62  CLC   TMPEND,TS#CEDT      IS PERIOD END DATE LOWER THAN END            
         JNH   TLGAC70             YES                                          
         CLC   TMPSTART,TS#CEDT    IS PERIOD START DATE HIGHER THAN END         
         JNH   TLGAC70                                                          
*                                                                               
*  NO, FIND FIRST TMPEL AFTER REQ PERIOD AND SKIP I/O TO THAT                   
*            REV CHRON RECORD ORDER WILL MOONWALK ACROSS PERIOD.                
         LA    R3,CASRFST-CASKEY(R1)                                            
TLGAC65  CLI   TMPEL,0                                                          
         JE    TLGAC20             END DATE IS LAST PERIOD OF YEAR,             
         CLI   TMPEL,TMPELQ                       CARRY ON                      
         JNE   TLGAC67                                                          
         CLC   TMPSTART,TS#CEDT                                                 
         JH    TLGAC68                                                          
*                                                                               
TLGAC67  LLC   RE,TMPLN                                                         
         AR    R3,RE                                                            
         J     TLGAC65                                                          
*                                                                               
TLGAC68  DS    0H                                                               
         ICM   RF,7,TMPSTART       PUSH KEY TO DAY AFTER PERIOD                 
         LNR   RF,RF                                                            
         CLM   RF,7,TSJPPEDT                                                    
         JE    TLGAC20             READ SEQ IF NO CHANGE                        
         STCM  RF,7,TSJPPEDT                                                    
         J     TLGAC10             READ HIGH FROM HERE                          
*                                                                               
TLGAC70  GOTOR DUMPERT             build bummy pertab from tmpel                
*                                                                               
         MVI   TS#ATYP,TD#SJ1NQ                                                 
         TM    X#SRCIND,X#SRCRET   ARE WE ON APPROVER PASS?                     
         JZ    *+8                                                              
         MVI   TS#ATYP,TD#SJ1AQ    YES, DIFFERENT TYPE                          
         GOTOR TGDSET,DMCB,TSJPPODS,TSJPPEDT                                    
         GOTOR PRODUM                                                           
         J     TLGETADY                                                         
         DROP  R3                                                               
*                                                                               
*                                                                               
TLGETADY CR    RB,RB                                                            
         J     TLGETADX                                                         
TLGETADN LTR   RB,RB                                                            
TLGETADX XIT1                                                                   
         DROP  R2,R4                                                            
***********************************************************************         
* CLEAR 'TIMESHEET FOUND' FLAGS FROM SUMTAB                                     
***********************************************************************         
         SPACE 1                                                                
STRESET  DS    0H                                                               
         LA    RF,X#SUMTAB                                                      
STRES10  CLI   0(RF),0                                                          
         BER   RE                                                               
         NI    STSTAT-SUMTABD(RF),FF-STSTFUND                                   
         LA    RF,SUMTABL(RF)                                                   
         J     STRES10                                                          
         LTORG                                                                  
***********************************************************************         
* SET PARAMETERS FOR PROMTSR                                                    
*                                                                               
* P1 IS A(OFFICE/DEPT/SUBDEP/PERSON) OR A(PERSON/OFFICE/DEPT/SUBDPT)            
* P2 IS A(PL3 END DATE)                                                         
* TS#ATYP ALREADY SET APPROPRIATELY                                             
* R2 POINTS TO PASSIVE (INCL STATUS AREA, NOT JUST KEY)                         
***********************************************************************         
         SPACE 1                                                                
TGDSET   NTR1  BASE=*,LABEL=*                                                   
         USING TDTPASD,R2          (MAY BE ANOTHER, COMPATIBLE PASSIVE)         
         MVC   X#S1RAC,TS#1RACT                                                 
         L     RF,4(R1)            A(END DATE)                                  
         ICM   RF,7,0(RF)                                                       
         LNR   RF,RF                                                            
         STCM  RF,7,TS#PEDT                                                     
*                                                                               
         MVC   TS#STAT,TDTPKSTA                                                 
         CLI   TDTPKSTA,FF                                                      
         JNE   TGDSET10                                                         
         MVC   TS#STAT,TDTPSTAT                                                 
*                                                                               
TGDSET10 MVC   CSVKEY3,IOKEY       Save key for when we put out recs            
         ZAP   X#SVHRS,TDTPHRS     save values and return to main               
         ZAP   X#SVMTS,TDTPMTS                                                  
         MVC   TS#RECDA,TDTPDA                                                  
         MVC   TS#1ROFF(L'TS#1ROFF+L'TS#1RDPT+L'TS#1RSDP),SPACES                
         MVC   TS#PRSAC,SPACES                                                  
*                                                                               
         CLI   TS#ATYP,TD#1RQ                                                   
         JH    TGDSET50                                                         
*                          SJ/1N                                                
         L     RF,0(R1)            A(ODSP)                                      
         SR    R1,R1               for main entry save office                   
         SR    R0,R0                                                            
         IC    R1,ONERL1L                                                       
         AHI   R1,-1                                                            
         MVC   TS#1ROFF(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL1L                                                       
         IC    R1,ONERL2L                                                       
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         MVC   TS#1RDPT(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         MVC   TS#1RSDP(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL3L                                                       
         IC    R1,ONERL4L                                                       
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         MVC   TS#PRSAC(0),0(RF)                                                
         EX    R1,*-6                                                           
         J     TGDSETX                                                          
*                                                                               
TGDSET50 L     RF,0(R1)            A(PODS)                                      
         XR    R1,R1               for main entry save office                   
         XR    R0,R0                                                            
         IC    R0,ONERL3L                                                       
         IC    R1,ONERL4L                                                       
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         MVC   TS#PRSAC(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R1,ONERL1L                                                       
         AHI   R1,-1                                                            
         MVC   TS#1ROFF(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL1L                                                       
         IC    R1,ONERL2L                                                       
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         MVC   TS#1RDPT(0),0(RF)                                                
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         IC    R0,ONERL2L                                                       
         IC    R1,ONERL3L                                                       
         SR    R1,R0                                                            
         AHI   R1,-1                                                            
         MVC   TS#1RSDP(0),0(RF)                                                
         EX    R1,*-6                                                           
*                                                                               
TGDSETX  XIT1                                                                   
         DROP  R2                                                               
***********************************************************************         
* Build pertab from TMPEL at R3, to avoid a full CALPRDS call         *         
***********************************************************************         
         SPACE 1                                                                
DUMPERT  NTR1  BASE=*,LABEL=*                                                   
         USING TMPELD,R3                                                        
         CLI   TMPEL,TMPELQ                                                     
         JE    *+6                                                              
         DC    H'0'                SHOULD BE A TMPEL, DUDE                      
         L     R2,AIO5                                                          
         USING PERTABD,R2                                                       
         XC    PERENDT(PERLENQ),PERENDT                                         
         ICM   RE,7,TMPEND                                                      
         LNR   RE,RE                                                            
         STCM  RE,7,PERENDT                                                     
         MVC   PERSTDT,TMPSTART                                                 
         MVC   PERNUM,TMPNUMB                                                   
         LA    R2,PERLENQ(R2)                                                   
         MVI   0(R2),0                                                          
DUMPERX  XIT1                                                                   
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* MANAGE TABLE OF CLIENT/PERSON CODES FOR WHICH WE'VE FOUND TIMESHEETS          
* AS WE WILL NEED DUMMIES TO FILL IN THE BLANKS.                                
***********************************************************************         
         SPACE 1                                                                
         USING TSJPASD,R2                                                       
         USING CLITABD,R4                                                       
PRODUM   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AGAPAREA                                                      
*                                                                               
         LA    R3,TSJPACT          Assume we are doing SJ view                  
         CLI   TSJPVIEW,TSJPSJAQ   Are we doing SJ view?                        
         BE    *+8                                                              
         LA    R3,TSJP1NAC         Set Address of 1N field instead              
*                                                                               
         CLI   TS#PTYPE,TS#PTSMC   DETAIL IS DIFFERENT                          
         JE    PDUM50                                                           
         CLI   TS#PTYPE,TS#PTSWC                                                
         JE    PDUM50                                                           
*                                                                               
* summary version - build list of client/1N in AGAPAREA for BLDCTSR             
         XR    R1,R1                                                            
         IC    R1,PCLILEN                                                       
         CLI   TSJPVIEW,TSJP1NAQ                                                
         JNE   *+8                                                              
         LA    R1,L'TSJPACT        1N NEEDS MORE SPACE!                         
         SHI   R1,1                                                             
PDUM02   CLI   0(R4),GAPTEOT       CHECK FOR DUPLICATES                         
         JE    PDUM08                                                           
*                                                                               
         CLC   TSJPVIEW,CLIVIEW                                                 
         JNE   PDUM06                                                           
*                                                                               
         BASR  RF,0                                                             
         CLC   CLICDE(0),0(R3)                                                  
         EX    R1,0(RF)                                                         
         JE    EXITY                                                            
         J     PDUM06                                                           
*                                                                               
PDUM06   DS    0H                                                               
         LLC   RE,PCLILEN                                                       
         LA    RE,L'CLIVIEW(RE)                                                 
         CLI   CLIVIEW,TSJP1NAQ                                                 
         JNE   *+8                                                              
         LA    RE,CLICLEN                                                       
         AR    R4,RE                                                            
         J     PDUM02                                                           
*                                                                               
PDUM08   CLC   0(L'TSJPACT,R3),SPACES                                           
         JNH   PDUM10                                                           
         MVC   CLICDE,SPACES                                                    
         BASR  RF,0                                                             
         MVC   CLICDE(0),0(R3)                                                  
         EX    R1,0(RF)                                                         
         MVC   CLIVIEW,TSJPVIEW                                                 
         LLC   RE,PCLILEN                                                       
         LA    RE,L'CLIVIEW(RE)                                                 
         CLI   CLIVIEW,TSJP1NAQ                                                 
         JNE   *+8                                                              
         LA    RE,CLICLEN                                                       
         AR    R4,RE                                                            
PDUM10   MVI   0(R4),GAPTEOT                                                    
         J     EXITY                                                            
*                                                                               
*  client detail version- build list of persons with timesheets for Job         
*  table has current a/c ULA as a header - build dummies when new job           
PDUM50   DS    0H                                                               
         CLC   PERHACC,SPACES      FIRST TIME?                                  
         JE    PDUM65                                                           
         CLC   PERHACC,0(R3)       SAME JOB AS BEFORE?                          
         JNE   PDUM60                                                           
         CLC   PERHVIEW,TSJPVIEW   SAME VIEW AS BEFORE?                         
         JE    PDUM70                                                           
PDUM60   DS    0H                                                               
         GOTOR BLDCTSR                  NO, BUILD DUMMIES FOR OLD JOB           
*                                                                               
PDUM65   DS    0H                       (RE)INIT TABLE                          
         MVC   PERHVIEW,TSJPVIEW                                                
         MVC   PERHACC,0(R3)   SAVE JOB                                         
         LA    R4,L'PERHVIEW+L'PERHACC(R4)                                      
         MVI   0(R4),GAPTEOT           'CLEAR' TABLE                            
         J     PDUM78              ADD ENTRY FOR THIS PERSON                    
*                                                                               
PDUM70   LA    R4,L'PERHVIEW+L'PERHACC(R4)   FIND START OF PERSON LIST          
PDUM72   CLI   0(R4),GAPTEOT                                                    
         JE    PDUM78              not yet in table                             
         CLC   PERCOFC,TS#1ROFF                                                 
         JNE   PDUM76                                                           
         CLC   PERCDPT,TS#1RDPT                                                 
         JNE   PDUM76                                                           
         CLC   PERCSDP,TS#1RSDP                                                 
         JNE   PDUM76                                                           
         CLC   PERCDE,TS#PRSAC                                                  
         JE    EXITY               ALREADY HAVE THIS ONE                        
*                                                                               
PDUM76   LA    R4,PERCPODS(R4)                                                  
         J     PDUM72                                                           
*                                                                               
PDUM78   DS    0H                  ADD TO TABLE                                 
*        CLC   PERCOFC,SPACES                                                   
*        JNH   PDUM90                                                           
         MVC   0(PERCPODS,R4),SPACES                                            
         MVC   PERCOFC,TS#1ROFF                                                 
         MVC   PERCDPT,TS#1RDPT                                                 
         MVC   PERCSDP,TS#1RSDP                                                 
         MVC   PERCDE,TS#PRSAC                                                  
         LA    R4,PERCPODS(R4)                                                  
PDUM90   MVI   0(R4),GAPTEOT                                                    
         J     EXITY                                                            
         DROP  R2,R4                                                            
***********************************************************************         
* OUTPUT TIMESHEET RECORDS FROM TSAR BUFFER TO DDLINK                 *         
***********************************************************************         
         SPACE 1                                                                
OUTPTSR  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*OUTPTSR'                                                      
                                                                                
         USING TD#TABD,R4                                                       
PREV     USING TD#TABD,R3                                                       
         L     R3,AIO2                                                          
         L     R4,AIO1                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO1   clear AIO1                           
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO2   clear AIO2                           
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO3   clear AIO3                           
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO4   clear AIO4                           
         GOTOR SORTTSR                                                          
         GOTOR (#MTSAR,AMTSAR),DMCB,TSARDH,TSARTBUF,TSARTDL                     
         TM    TSARERRS,TSEEOF     is it the end of the buffer                  
         BO    OUTPTSRX            YES                                          
*                                                                               
* RETURN HERE AFTER SENDING MERGED DATALINE TO DDLINK                           
OUTPT02  XC    X#IND,X#IND                                                      
         L     R0,AIO2             COPY FIRST FOR KEY INTO 'PREV'               
         LA    R1,L'IOAREA1                                                     
         SR    RF,R1                                                            
         L     RE,AIO1                                                          
         MVCL  R0,RE                                                            
*                                                                               
OUTPT04  GOTOR (#CLRIO,ACLRIO),DMCB,AIO1                                        
         GOTOR (#MTSAR,AMTSAR),DMCB,TSANXT,TSARTBUF,TSARTDL                     
         TM    TSARERRS,TSEEOF     is it the end of the buffer                  
         BO    OUTPT14                                                          
         CLI   TS#PTYPE,TS#PTRHP   search for hours by person                   
         BE    OUTPT16             put each record out                          
         CLI   TS#PTYPE,TS#PTRHC   search for hours by client/non cli           
         BE    OUTPT16             put each record out                          
         CLI   TS#PTYPE,TS#PTRHO   search for hours by office dept              
         BE    OUTPT16             put each record out                          
         CLC   PREV.TD#KEY(TD#KY1Q),TD#KEY                                      
         BNE   OUTPT14                                                          
*                                                                               
* KEY MATCHES, CHECK FOR DUPLICATE DATA READS                                   
* IF 1R AND SJ FOR SAME KEY, IGNORE SJ (DUPLICATE)                              
* IF TD#1RAQ AND TD#1RQ, OR TD#SJ1AQ AND TD#SJ1NQ, IGNORE CURRENT REC           
*        CLI   PREV.TD#TYP,TD#1RQ  * THIS FEELS A BIT BOBBINS.                  
*        BH    OUTPT04             * SHOULD BE IGNORING SJ IF ALREADY           
*        CLI   TD#TYP,TD#1RQ       * HAD RECORD VIA 1R.                         
*        BH    OUTPT12             * NOT CHANGING AS TESTERS ARE HAPPY          
         CLI   PREV.TD#TYP,TD#1RQ  < HAD TO CHANGE ABOVE IT WAS                 
         JH    OUTPT08             < DEFINITELY BOBBINS NOT WORKING             
         CLI   TD#TYP,TD#1RQ       < PROPERLY                                   
         JH    OUTPT12             <                                            
*                                                                               
OUTPT06  DS    0H                                                               
         CLI   PREV.TD#TYP,TD#1RAQ PREV FROM X#SRCAPP                           
         JNE   OUTPT08                                                          
         CLI   TD#TYP,TD#1RQ                                                    
         JE    OUTPT12             THIS FROM X#SRCRET, DUPE                     
*                                                                               
OUTPT08  DS    0H                                                               
         CLI   PREV.TD#TYP,TD#SJ1AQ PREV FROM X#SRCAPP                          
         JNE   OUTPT10                                                          
         CLI   TD#TYP,TD#SJ1NQ                                                  
         JE    OUTPT12              THIS FROM X#SRCRET, DUPE                    
*                                                                               
OUTPT10  AP    PREV.TD#HOUR,TD#HOUR                                             
         AP    PREV.TD#MATRS,TD#MATRS                                           
         AP    PREV.TD#CLIHR,TD#CLIHR                                           
*                                                                               
OUTPT12  CLC   PREV.TD#STTV,TD#STTV Set the status to the lowest                
         BNH   OUTPT04               value possible                             
         MVC   PREV.TD#STTV,TD#STTV                                             
         MVC   PREV.TD#STTS,TD#STTS                                             
         B     OUTPT04                                                          
*                                                                               
* FIRST/NEW KEY                                                                 
OUTPT14  CLI   TS#PTYPE,TS#PTLAS    is this a search or list                    
         BNH   OUTPT16              list                                        
         CP    PREV.TD#HOUR,X#MAXHR fall within hours range search              
         BH    OUTPT100             no                                          
         CP    PREV.TD#HOUR,X#MINHR                                             
         BL    OUTPT100             no                                          
*                                                                               
* REMEMBER SR_VALS COVERS THE SAME AREA AS TS_VALS                              
*                                                                               
OUTPT16  LA    RE,SR_VALS                                                       
         LA    RF,SR_LNQ                                                        
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
         MVC   TS_ENDD,PREV.TD#ENDD                                             
         MVC   TS_STAD,PREV.TD#STAD                                             
         MVC   TS_STTV,PREV.TD#STTV                                             
         MVC   TS_STTS,PREV.TD#STTS                                             
*                                                                               
         ZAP   TS_HOUR,PREV.TD#HOUR                                             
         CLI   TS#PTYPE,TS#PTLAS                                                
         BH    OUTPT18                                                          
         ZAP   TS_MTRS,PREV.TD#MATRS                                            
OUTPT18  CURED (B1,PREV.TD#PNUM),(3,TS_PERD),0,ALIGN=LEFT                       
         L     R2,AIO4                                                          
         USING PERRECD,R2                                                       
         CLC   PREV.TD#PERC,PERKCODE                                            
         BNE   OUTPT20                                                          
         OC    TS#PSPID,TS#PSPID                                                
         BZ    OUTPT100                                                         
         TM    X#IND,X#IPERSN      Person has been shown previously             
         BZ    OUTPT20             no - read the data again                     
         MVC   TS_PERF,UNIQUEQ                                                  
         MVC   TS_PERM,UNIQUEQ                                                  
         MVC   TS_PERL,UNIQUEQ                                                  
         MVC   TS_PERC,UNIQUEQ                                                  
         B     OUTPT56                                                          
*                                                                               
OUTPT20  MVC   TEMP2(8),PREV.TD#PERC                                            
         MVC   TS#PRSAC,PREV.TD#PERC                                            
         MVC   TS#1ROFF,PREV.TD#OFFC                                            
         MVC   TS#1RDPT,PREV.TD#DEPT                                            
         MVC   TS#1RSDP,PREV.TD#SBDP                                            
         OI    X#IND,X#IPERSN                                                   
         MVC   TS_PERF,SPACES                                                   
         MVC   TS_PERM,SPACES                                                   
         MVC   TS_PERL,SPACES                                                   
         MVC   TS_PERC,PREV.TD#PERC                                             
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,IOKEY            READ FOR PERSON RECORD                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,TS#PRSAC                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         L     R2,AIO4                                                          
         AHI   R2,PERRFST-PERRECD  LOCATE ELEMENTS                              
         XR    R0,R0                                                            
         XC    TS#PSPID,TS#PSPID                                                
         USING PIDELD,R2                                                        
                                                                                
OUTPT22  CLI   PIDEL,0                                                          
         JE    OUTPT29                                                          
         CLI   PIDEL,PIDELQ                                                     
         JE    OUTPT26                                                          
         CLI   PIDEL,GPNELQ                                                     
         JE    OUTPT28                                                          
                                                                                
OUTPT24  IC    R0,PIDLN                                                         
         AR    R2,R0                                                            
         J     OUTPT22                                                          
                                                                                
OUTPT26  MVC   TS#PSPID,PIDNO                                                   
         J     OUTPT24                                                          
                                                                                
         USING GPNELD,R2                                                        
OUTPT28  LA    RE,TS_PERF          First name here                              
         CLI   GPNTYP,GPNTLST                                                   
         JNE   *+8                                                              
         LA    RE,TS_PERL          Last name here                               
         XR    R1,R1                                                            
         IC    R1,GPNLN                                                         
         SHI   R1,GPNLNQ+1                                                      
         LTR   R1,R1                                                            
         JM    OUTPT24                                                          
         MVC   0(0,RE),GPNNME                                                   
         EX    R1,*-6                                                           
         J     OUTPT24                                                          
                                                                                
         USING SA0REC,R2                                                        
OUTPT29  OC    TS#PSPID,TS#PSPID                                                
         JZ    OUTPT100            No sec PID on Cost record                    
         LA    R2,IOKEY                                                         
         XC    SA0KEY,SA0KEY       BUILD KEY TO READ                            
         MVI   SA0KTYP,SA0KTYPQ                                                 
         OC    SA0KAGY,CUSALF      USE SECURITY AGENCY IF PRESENT               
         JNZ   *+10                                                             
         MVC   SA0KAGY,CUAALF      ELSE NATIVE AGENCY                           
         MVC   SA0KNUM,TS#PSPID                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTL+IO5'                               
         JNE   OUTPT54                                                          
                                                                                
         L     R2,AIO5                                                          
         AHI   R2,SA0DATA-SA0REC                                                
         USING SAPALD,R2                                                        
         XR    R0,R0                                                            
OUTPT30  CLI   SAPALEL,SAPALELQ                                                 
         JE    OUTPT32                                                          
         CLI   SAPALEL,0                                                        
         JE    OUTPT54                                                          
         IC    R0,SAPALLN                                                       
         AR    R2,R0                                                            
         J     OUTPT30                                                          
                                                                                
OUTPT32  MVC   TS#PIDC,SAPALPID   PASS 8 CHAR PID                               
         MVC   TS_PIDC,SAPALPID                                                 
                                                                                
         USING SAPEREC,R2                                                       
         LA    R2,IOKEY                                                         
         XC    SAPEKEY,SAPEKEY     BUILD KEY TO READ                            
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         OC    SAPEAGY,CUSALF      USE SECURITY AGENCY IF PRESENT               
         JNZ   *+10                                                             
         MVC   SAPEAGY,CUAALF      ELSE NATIVE AGENCY                           
         MVC   SAPEPID,TS#PIDC                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTL+IO5'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO5             MATCH KEY ON ALL BUT EFFECTIVE DATE          
         CLC   SAPEKEY(L'SAPEKEY-L'SAPEDEF),IOKEYSAV                            
         JNE   OUTPT54                                                          
         AHI   R2,SAPEDATA-SAPEREC                                              
         USING SANAMD,R2                                                        
         XR    R0,R0                                                            
OUTPT34  CLI   SANAMEL,SANAMELQ                                                 
         JE    OUTPT38                                                          
         CLI   SANAMEL,SAPEEELQ                                                 
         JE    OUTPT44                                                          
         CLI   SANAMEL,0                                                        
         JE    OUTPT54                                                          
OUTPT36  IC    R0,SANAMLN          L'ELEMENT                                    
         AR    R2,R0                                                            
         J     OUTPT34                                                          
                                                                                
OUTPT38  LA    R1,SANAMELN         L'NAME                                       
         USING SANAMELN,R1                                                      
         TM    SANAMIND,SANAMIFN   TEST FIRST NAME PRESENT                      
         JZ    OUTPT40                                                          
         XR    RF,RF                                                            
         MVC   TS_PERF,SPACES                                                   
         IC    RF,SANAMELN                                                      
         CHI   RF,L'SU_PFNA        TEST > MAX LENGTH                            
         JNH   *+8                                                              
         LA    RF,L'SU_PFNA        SET IT IF GREATER                            
         AHI   RF,-1                                                            
         MVC   TS_PERF(0),SANAME                                                
         EX    RF,*-6                                                           
         IC    RF,SANAMELN                                                      
         LA    R1,1(RF,R1)                                                      
OUTPT40  TM    SANAMIND,SANAMIMN   TEST MIDDLE NAME PRESENT TOO                 
         JZ    OUTPT42                                                          
         IC    RF,SANAMELN                                                      
         MVC   TS_PERM,SPACES                                                   
         CHI   RF,L'SU_PMNA        TEST > MAX LENGTH                            
         JNH   *+8                                                              
         LA    RF,L'SU_PMNA        SET IT IF GREATER                            
         AHI   RF,-1                                                            
         MVC   TS_PERM(0),SANAME                                                
         EX    RF,*-6                                                           
         IC    RF,SANAMELN                                                      
         LA    R1,1(RF,R1)                                                      
                                                                                
OUTPT42  TM    SANAMIND,SANAMILN   TEST LAST NAME PRESENT                       
         JZ    OUTPT36                                                          
         IC    RF,SANAMELN                                                      
         MVC   TS_PERL,SPACES                                                   
         CHI   RF,L'SU_PLNA        TEST > MAX FULL LENGTH                       
         JNH   *+8                                                              
         LHI   RF,L'SU_PLNA                                                     
         SHI   RF,1                                                             
         MVC   TS_PERL(0),SANAME                                                
         EX    RF,*-6                                                           
         J     OUTPT36                                                          
         DROP  R1                                                               
*                                                                               
         USING SAPEED,R2                                                        
OUTPT44  OC    RQ_APIVN,RQ_APIVN                                                
         JZ    OUTPT36                                                          
         XR    RF,RF               EXTRACT EMAIL ADRESS                         
         IC    RF,SAPEELN                                                       
         SHI   RF,3                                                             
         LTR   RF,RF                                                            
         JM    OUTPT36                                                          
         MVC   SR_EMAIL(0),SAPEEID                                              
         EX    RF,*-6                                                           
         J     OUTPT36                                                          
*                                                                               
         USING PERRECD,R2                                                       
OUTPT54  L     R2,AIO4                                                          
*                                                                               
OUTPT56  LA    R2,PERRFST                                                       
         USING LOCELD,R2                                                        
OUTPT58  CLI   LOCEL,0                                                          
         BE    OUTPT62                                                          
         CLI   LOCEL,EMPELQ                                                     
         BNE   OUTPT60                                                          
         ST    R2,X#EMPEL                                                       
OUTPT60  SR    R0,R0                                                            
         IC    R0,LOCLN                                                         
         AR    R2,R0                                                            
         B     OUTPT58                                                          
*                                                                               
OUTPT62  L     R2,AIO4                                                          
         USING PERRECD,R2                                                       
         LA    R2,PERRFST                                                       
         USING LOCELD,R2                                                        
         XC    SAVER2,SAVER2                                                    
OUTPT64  CLI   LOCEL,0                                                          
         BE    OUTPT100                                                         
         ST    R2,SAVER2A                                                       
         CLI   LOCEL,LOCELQ                                                     
         BNE   OUTPT78                                                          
         CLC   LOCOFF,PREV.TD#OFFC                                              
         BNE   OUTPT78                                                          
         CLC   LOCDEPT,PREV.TD#DEPT                                             
         BNE   OUTPT78                                                          
         CLC   LOCSUB,PREV.TD#SBDP                                              
         BNE   OUTPT78                                                          
         CLC   LOCSTART,PREV.TD#ENDD                                            
         BH    OUTPT78                                                          
         OC    LOCEND,LOCEND                                                    
         BZ    OUTPT76                                                          
         CLC   LOCEND,PREV.TD#STAD                                              
         BL    OUTPT78                                                          
         CLC   PREV.TD#ENDD,LOCEND                                              
         BNH   OUTPT76                                                          
OUTPT66  GOTO1 VDATCON,DMCB,(1,LOCEND),(0,WORK)                                 
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,F'1'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,WORK)                                 
         L     R2,AIO4                                                          
         USING PERRECD,R2                                                       
         LA    R2,PERRFST                                                       
         USING LOCELD,R2                                                        
OUTPT68  CLI   LOCEL,0                                                          
         BE    OUTPT74                                                          
         CLI   LOCEL,LOCELQ                                                     
         BNE   OUTPT72                                                          
         CLC   LOCOFF,PREV.TD#OFFC                                              
         BNE   OUTPT72                                                          
         CLC   LOCDEPT,PREV.TD#DEPT                                             
         BNE   OUTPT72                                                          
         CLC   LOCSUB,PREV.TD#SBDP                                              
         BNE   OUTPT72                                                          
         CLC   WORK(L'LOCSTART),LOCSTART                                        
         BL    OUTPT72                                                          
         OC    LOCEND,LOCEND                                                    
         BZ    OUTPT76                                                          
         CLC   WORK(L'LOCEND),LOCEND                                            
         BH    OUTPT72                                                          
OUTPT70  ST    R2,SAVER2                                                        
         CLC   LOCEND,PREV.TD#ENDD                                              
         BH    OUTPT76                                                          
         B     OUTPT66                                                          
                                                                                
OUTPT72  SR    R0,R0                                                            
         IC    R0,LOCLN                                                         
         AR    R2,R0                                                            
         B     OUTPT68                                                          
*                                                                               
OUTPT74  L     R2,SAVER2A                                                       
         OC    SAVER2,SAVER2                                                    
         BZ    *+8                                                              
         L     R2,SAVER2                                                        
         MVC   TS_LOCE,LOCEND                                                   
OUTPT76  L     R2,SAVER2A                                                       
         CLC   LOCSTART,PREV.TD#STAD                                            
         BNH   *+10                                                             
         MVC   TS_LOCS,LOCSTART                                                 
         B     OUTPT80                                                          
*                                                                               
OUTPT78  SR    R0,R0                                                            
         IC    R0,LOCLN                                                         
         AR    R2,R0                                                            
         B     OUTPT64                                                          
*                                                                               
OUTPT80  L     R2,X#EMPEL                                                       
         USING EMPELD,R2                                                        
OUTPT82  CLC   EMPHIR,EMPTRM                                                    
         BE    OUTPT100                                                         
         MVC   X#HIRED,EMPHIR                                                   
         OC    EMPHIR,EMPHIR                                                    
         BZ    OUTPT83                                                          
         CLC   EMPHIR,PREV.TD#STAD  TEST T/S PERIOD START..                     
         BNH   OUTPT83              IS AFTER HIRE DATE                          
         CLC   TS_LOCS,EMPHIR       TEST LOCATION START..                       
         BH    OUTPT83              IS AFTER HIRE DATE                          
         MVC   TS_LOCS,EMPHIR       ELSE USE HIRE DATE AS LOC START             
OUTPT83  OC    EMPTRM,EMPTRM                                                    
         BZ    OUTPT84                                                          
         CLC   EMPTRM,PREV.TD#ENDD                                              
         BNL   OUTPT84                                                          
         CLC   TS_LOCE,EMPTRM                                                   
         BNH   OUTPT84                                                          
         MVC   TS_LOCE,EMPTRM                                                   
*                                                                               
OUTPT84  MVC   TS#PEDT,TS_ENDD                                                  
*                                                                               
         GOTOR CHKSUB              Check previous timesheet submitted           
*                                                                               
         MVC   TS#PEDT,TS_ENDD                                                  
         MVC   TS#EFSTA,TS_LOCS                                                 
         OC    TS_LOCS,TS_LOCS                                                  
         JNZ   *+10                                                             
         MVC   TS#EFSTA,TS_STAD                                                 
         MVC   TS#EFEND,TS_LOCE                                                 
         OC    TS_LOCE,TS_LOCE                                                  
         JNZ   *+10                                                             
         MVC   TS#EFEND,TS_ENDD                                                 
*                                                                               
         GOTOR CHKHRS              Read daily edit hours                        
*&&US                                                                           
         CP    TS#EDHRS,PZERO      Did we find anything                         
         JNE   OUTPT86             Yes                                          
         GOTOR CHKPHRS             No - read period edit hours                  
*&&                                                                             
OUTPT86  ZAP   TS_EHRS,TS#EDHRS                                                 
*                                                                               
OUTPT88  GOTOR GETAUD,DMCB,(1,TS#EFEND)                                         
         XOUT  TS#INDX,TS_IDFN,2                                                
         CLI   TS#PTYPE,TS#PTLAS   A search request?                            
         BH    OUTPT89                                                          
         BL    OUTPT94             list approvals by status                     
         MVC   TS_OFFC,PREV.TD#OFFC                                             
         MVC   TS_OFNM,PREV.TD#OFFNM                                            
         MVC   TS_DEPT,PREV.TD#DEPT                                             
         MVC   TS_DPNM,PREV.TD#DEPNM                                            
         MVC   TS_SDEP,PREV.TD#SBDP                                             
         MVC   TS_SDNM,PREV.TD#SDPNM                                            
         B     OUTPT94                                                          
*                                  Search requests                              
OUTPT89  ZAP   SR_MTRS,PREV.TD#MATRS                                            
         ZAP   SR_TU,X#TUPCT                                                    
         ZAP   SR_CHRS,PREV.TD#CLIHR                                            
         MVC   SR_OFFC,PREV.TD#OFFC                                             
         MVC   SR_OFNM,PREV.TD#OFFNM                                            
         MVC   SR_DEPT,PREV.TD#DEPT                                             
         MVC   SR_DPNM,PREV.TD#DEPNM                                            
         MVC   SR_SBDP,PREV.TD#SBDP                                             
         MVC   SR_SDNM,PREV.TD#SDPNM                                            
         MVC   SR_LMPID,PREV.TD#LMPID                                           
         MVC   SR_LMFNM,PREV.TD#LMFNM                                           
         MVC   SR_LMMNM,PREV.TD#LMMNM                                           
         MVC   SR_LMSNM,PREV.TD#LMSNM                                           
         OC    RQ_APIVN,RQ_APIVN                                                
         JZ    *+10                                                             
         MVC   SR_LMEML,PREV.TD#LMEML                                           
         MVC   SR_TYPE,PREV.TD#TTYPE                                            
         MVC   SR_ORD#,PREV.TD#ORD#2                                            
         MVC   SR_ORDNM,PREV.TD#ORDNM                                           
         MVC   SR_EST#,PREV.TD#EST#2                                            
         MVC   SR_ESTL#,PREV.TD#EST#L                                           
         MVC   SR_ESTNM,PREV.TD#ESTNM                                           
         MVC   SR_NARR,PREV.TD#NARR                                             
         MVC   SR_INTRF,PREV.TD#INTRF                                           
         MVI   SR_ZHTS,YESQ                                                     
         CP    TS_HOUR,PZERO       Check whether zero hour t/s                  
         JNH   *+8                                                              
         MVI   SR_ZHTS,NOQ                                                      
         CLC   PRODUL,PREV.TD#TIMAC                                             
         BNE   OUTPT92                                                          
         MVC   SR_WRKC,PREV.TD#TSKCD                                            
         MVC   SR_WCDE,PREV.TD#TSKNM                                            
         MVC   SR_CLNA,PREV.TD#CLINM                                            
         MVC   SR_PRNA,PREV.TD#PRDNM                                            
         MVC   SR_JONA,PREV.TD#JOBNM                                            
         MVC   SR_JOLK,PREV.TD#JOBLK                                            
         MVC   SR_CLSJB,PREV.TD#JOBCL                                           
         MVC   SR_CLCD,SPACES                                                   
         MVC   SR_PRCD,SPACES                                                   
         MVC   SR_JOCD,SPACES                                                   
         SR    RF,RF                                                            
         IC    RF,PCLILEN                                                       
         SHI   RF,1                                                             
         MVC   SR_CLCD(0),PREV.TD#TIMAC+L'ACTKUNT+L'ACTKLDG                     
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         SR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         LA    R1,PREV.TD#TIMAC+L'ACTKUNT+L'ACTKLDG                             
         AR    R1,RF                                                            
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         MVC   SR_PRCD(0),0(R1)                                                 
         EX    RE,*-6                                                           
         SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    RE,RE                                                            
         IC    RE,PJOBLEN                                                       
         LA    R1,PREV.TD#TIMAC+L'ACTKUNT+L'ACTKLDG                             
         AR    R1,RF                                                            
         SR    RE,RF                                                            
         CH    RE,=H'6'            Is the job length greater than 6             
         BNH   OUTPT90             no                                           
         LA    RE,6                yes - limit to 6 characters                  
OUTPT90  SHI   RE,1                                                             
         MVC   SR_JOCD(0),0(R1)                                                 
         EX    RE,*-6                                                           
         B     OUTPT96                                                          
*                                                                               
OUTPT92  OC    PREV.TD#TIMAC,PREV.TD#TIMAC                                      
         BZ    OUTPT96                                                          
         MVC   SR_1NCD,PREV.TD#TIMAC+L'ACTKUNT+L'ACTKLDG                        
         MVC   SR_1NNA,PREV.TD#CLINM                                            
         B     OUTPT96                                                          
*                                                                               
OUTPT94  ZAP   TS_TU,X#TUPCT                                                    
         ZAP   TS_CHRS,PREV.TD#CLIHR                                            
         MVI   TS_ZHTS,YESQ                                                     
         CP    TS_HOUR,PZERO       Check whether zero hour t/s                  
         JNH   *+8                                                              
         MVI   TS_ZHTS,NOQ                                                      
OUTPT96  CLI   X#GAOV,YESQ                                                      
         BNE   OUTPT98                                                          
         XC    TS_LOCS,TS_LOCS                                                  
         XC    TS_LOCE,TS_LOCE                                                  
         ZAP   TS_EHRS,PZERO                                                    
         ZAP   TS_MTRS,PZERO                                                    
*                                                                               
OUTPT98  GOTOR LP_APUTO,LP_D                  send and clear data               
         TM    TSARERRS,TSEEOF     is it the end of the buffer                  
         BNO   OUTPT02                                                          
         B     OUTPTSRX                                                         
*                                                                               
OUTPT100 NI    X#IND,X'FF'-X#IPERSN                                             
         TM    TSARERRS,TSEEOF     is it the end of the buffer                  
         BNO   OUTPT02                                                          
*                                                                               
OUTPTSRX XIT1                                                                   
         DROP  PREV,R4                                                          
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* SORT TSAR INTO LOCATION CODE WEEK ENDING DATE ORDER                 *         
***********************************************************************         
         SPACE 1                                                                
SORTTSR  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*SORTTSR'                                                      
         GOTOR (#MTSAR,AMTSAR),DMCB,TSASRT,TSARTBUF,                   +        
               TSARTDL,SORTPERC                                                 
SORTTSRX XIT1                                                                   
         LTORG                                                                  
*        sort parameters - ascending, start of sort key, length                 
SORTPERC DC    AL1(0,TD#OFFC-TD#KEY,TD#SEQ-TD#OFFC)                             
         SPACE 2                                                                
***********************************************************************         
* SORT TSAR INTO person CODE WEEK ENDING DATE ORDER                   *         
***********************************************************************         
         SPACE 1                                                                
SORTSUM  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*SORTSUM'                                                      
         GOTOR (#MTSAR,AMTSAR),DMCB,TSASRT,TSARTBUF,                   +        
               TSARTDL,SORTSUMC                                                 
SORTSUMX XIT1                                                                   
         LTORG                                                                  
*        sort parameters - ascending, start of sort key, length                 
SORTSUMC DC    AL1(0,TD#PERC-TD#KEY,TD#TYP-TD#PERC)                             
         SPACE 2                                                                
***********************************************************************         
* SORT TSAR INTO ACCOUNT CODE AND ENDING DATE ORDER                   *         
***********************************************************************         
         SPACE 1                                                                
SORTTSJ  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*SORTTSJ'                                                      
         GOTOR (#MTSAR,AMTSAR),DMCB,TSASRT,TSARTBUF,                   +        
               TSARTDL,SORTSJCD                                                 
SORTTSJX XIT1                                                                   
         LTORG                                                                  
SORTSJCD DC    AL1(0,TD#ULA-TD#KEY,TD#TTYE-TD#ULA)                              
         SPACE 2                                                                
***********************************************************************         
* OUTPUT TIMESHEET RECORDS FROM TSAR BUFFER TO DDLINK                 *         
***********************************************************************         
         SPACE 1                                                                
OUTSUM   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*OUTSUM*'                                                      
                                                                                
         USING TD#TABD,R4                                                       
PREV     USING TD#TABD,R3                                                       
         L     R3,AIO2                                                          
         L     R4,AIO1                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO1   clear AIO1                           
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO3   clear AIO3                           
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO2   clear AIO2                           
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO4   clear AIO4                           
         XC    TS#PRSAC,TS#PRSAC           clear previous person code           
         CLI   TS#PTYPE,TS#PTSMA           client summaries                     
         BNL   OUTSM02                     yes - sort by account code           
         GOTOR SORTSUM                                                          
         B     OUTSM04                                                          
                                                                                
OUTSM02  GOTOR SORTTSJ                                                          
OUTSM04  GOTOR (#MTSAR,AMTSAR),DMCB,TSARDH,TSARTBUF,TSARTDL                     
         TM    TSARERRS,TSEEOF     is it the end of the buffer                  
         BO    OUTSUMX             yes                                          
*                                                                               
OUTSM06  L     R0,AIO2             copy AIO1 to AIO2                            
         LA    R1,L'IOAREA1                                                     
         SR    RF,R1                                                            
         L     RE,AIO1                                                          
         MVCL  R0,RE                                                            
*                                                                               
OUTSM08  GOTOR (#CLRIO,ACLRIO),DMCB,AIO1                                        
         GOTOR (#MTSAR,AMTSAR),DMCB,TSANXT,TSARTBUF,TSARTDL                     
         TM    TSARERRS,TSEEOF     is it the end of the buffer                  
         BO    OUTSM12                                                          
         CLI   TS#PTYPE,TS#PTSMA   client summaries?                            
         JL    OUTSM10                                                          
         CLC   PREV.TD#KEY(TD#TYP-TD#KEY),TD#KEY   yes, check whole key         
         BNE   OUTSM12                                                          
         AP    PREV.TD#HOUR,TD#HOUR                   YES, ROLL                 
         AP    PREV.TD#MATRS,TD#MATRS                                           
         B     OUTSM08                                                          
*                                                                               
OUTSM10  CLC   PREV.TD#PERC(TD#TYP-TD#PERC),TD#PERC  SAME PERSON/DATE?          
         BNE   OUTSM12                                                          
         AP    PREV.TD#HOUR,TD#HOUR                   YES, ROLL                 
         AP    PREV.TD#MATRS,TD#MATRS                                           
         B     OUTSM08                                                          
*                                                                               
OUTSM12  LA    RE,SU_VALS                                                       
         LA    RF,SU_LNQ                                                        
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
         MVC   SU_TODT,PREV.TD#ENDD                                             
         MVC   SU_FRDT,PREV.TD#STAD                                             
         MVC   SU_STTV,PREV.TD#STTV                                             
         MVC   SU_TYPE,PREV.TD#TTYE                                             
*                                                                               
         ZAP   SU_EHRS,PREV.TD#HOUR                                             
         ZAP   SU_MTRS,PREV.TD#MATRS                                            
         CLI   TS#PTYPE,TS#PTSMA                                                
         BE    OUTSM44                                                          
         CLI   TS#PTYPE,TS#PTSWA                                                
         BE    OUTSM44                                                          
         OC    PREV.TD#PERC,PREV.TD#PERC                                        
         BZ    OUTSM44                                                          
         CLC   PREV.TD#PERC,TS#PRSAC                                            
         BNE   OUTSM14                                                          
         MVC   SU_PFNA,UNIQUEQ                                                  
         MVC   SU_PMNA,UNIQUEQ                                                  
         MVC   SU_PLNA,UNIQUEQ                                                  
         MVC   SU_PCOD,PREV.TD#PERC                                             
         B     OUTSM44                                                          
*                                                                               
OUTSM14  MVC   TS#PRSAC,PREV.TD#PERC                                            
         MVC   SU_PFNA,SPACES                                                   
         MVC   SU_PLNA,SPACES                                                   
         MVC   SU_PMNA,SPACES                                                   
         MVC   SU_PCOD,PREV.TD#PERC                                             
         USING PERRECD,R2                                                       
         LA    R2,IOKEY            READ FOR PERSON RECORD                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,TS#PRSAC                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
                                                                                
         L     R2,AIO4                                                          
         AHI   R2,PERRFST-PERRECD  LOCATE ELEMENTS                              
         XR    R0,R0                                                            
         XC    TS#PSPID,TS#PSPID                                                
         USING PIDELD,R2                                                        
                                                                                
OUTSM16  CLI   PIDEL,0                                                          
         JE    OUTSM28                                                          
         CLI   PIDEL,PIDELQ                                                     
         JE    OUTSM20                                                          
         CLI   PIDEL,GPNELQ                                                     
         JE    OUTSM22                                                          
                                                                                
OUTSM18  IC    R0,PIDLN                                                         
         AR    R2,R0                                                            
         J     OUTSM16                                                          
                                                                                
OUTSM20  MVC   TS#PSPID,PIDNO                                                   
         J     OUTSM18                                                          
                                                                                
         USING GPNELD,R2                                                        
OUTSM22  LA    RE,SU_PFNA          First name here                              
         CLI   GPNTYP,GPNTLST                                                   
         BNE   *+8                                                              
         LA    RE,SU_PLNA          Last name here                               
         XR    R1,R1                                                            
         IC    R1,GPNLN                                                         
         SHI   R1,GPNLNQ+1                                                      
         LTR   R1,R1                                                            
         BM    OUTSM18                                                          
         MVC   0(0,RE),GPNNME                                                   
         EX    R1,*-6                                                           
         B     OUTSM18                                                          
                                                                                
         USING SA0REC,R2                                                        
OUTSM28  OC    TS#PSPID,TS#PSPID                                                
         JZ    OUTSM100            No sec PID on Cost record                    
         LA    R2,IOKEY                                                         
         XC    SA0KEY,SA0KEY       BUILD KEY TO READ                            
         MVI   SA0KTYP,SA0KTYPQ                                                 
         OC    SA0KAGY,CUSALF      USE SECURITY AGENCY IF PRESENT               
         JNZ   *+10                                                             
         MVC   SA0KAGY,CUAALF      ELSE NATIVE AGENCY                           
         MVC   SA0KNUM,TS#PSPID                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTL+IO4'                               
         JNE   OUTSM44                                                          
                                                                                
         L     R2,AIO4                                                          
         AHI   R2,SA0DATA-SA0REC                                                
         USING SAPALD,R2                                                        
         XR    R0,R0                                                            
OUTSM30  CLI   SAPALEL,SAPALELQ                                                 
         JE    OUTSM32                                                          
         CLI   SAPALEL,0                                                        
         JE    OUTSM44                                                          
         IC    R0,SAPALLN                                                       
         AR    R2,R0                                                            
         J     OUTSM30                                                          
                                                                                
OUTSM32  MVC   TS#PIDC,SAPALPID   PASS 8 CHAR PID                               
                                                                                
         USING SAPEREC,R2                                                       
         LA    R2,IOKEY                                                         
         XC    SAPEKEY,SAPEKEY     BUILD KEY TO READ                            
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         OC    SAPEAGY,CUSALF      USE SECURITY AGENCY IF PRESENT               
         JNZ   *+10                                                             
         MVC   SAPEAGY,CUAALF      ELSE NATIVE AGENCY                           
         MVC   SAPEPID,TS#PIDC                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTL+IO4'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO4             MATCH KEY ON ALL BUT EFFECTIVE DATE          
         CLC   SAPEKEY(L'SAPEKEY-L'SAPEDEF),IOKEYSAV                            
         JNE   OUTSM44                                                          
         AHI   R2,SAPEDATA-SAPEREC                                              
         USING SANAMD,R2                                                        
         XR    R0,R0                                                            
OUTSM34  CLI   SANAMEL,SANAMELQ                                                 
         JE    OUTSM38                                                          
         CLI   SANAMEL,0                                                        
         JE    OUTSM44                                                          
OUTSM36  IC    R0,SANAMLN          L'ELEMENT                                    
         AR    R2,R0                                                            
         J     OUTSM34                                                          
                                                                                
OUTSM38  LA    R1,SANAMELN         L'NAME                                       
         USING SANAMELN,R1                                                      
         TM    SANAMIND,SANAMIFN   TEST FIRST NAME PRESENT                      
         JZ    OUTSM40                                                          
         MVC   SU_PFNA,SPACES                                                   
         XR    RF,RF                                                            
         IC    RF,SANAMELN                                                      
         CHI   RF,L'SU_PFNA        TEST > MAX LENGTH                            
         JNH   *+8                                                              
         LA    RF,L'SU_PFNA        SET IT IF GREATER                            
         AHI   RF,-1                                                            
         MVC   SU_PFNA(0),SANAME                                                
         EX    RF,*-6                                                           
         IC    RF,SANAMELN                                                      
         LA    R1,1(RF,R1)                                                      
OUTSM40  TM    SANAMIND,SANAMIMN   TEST MIDDLE NAME PRESENT TOO                 
         JZ    OUTSM42                                                          
         MVC   SU_PMNA,SPACES                                                   
         IC    RF,SANAMELN                                                      
         CHI   RF,L'SU_PMNA        TEST > MAX LENGTH                            
         JNH   *+8                                                              
         LA    RF,L'SU_PMNA        SET IT IF GREATER                            
         AHI   RF,-1                                                            
         MVC   SU_PMNA(0),SANAME                                                
         EX    RF,*-6                                                           
         IC    RF,SANAMELN                                                      
         LA    R1,1(RF,R1)                                                      
                                                                                
OUTSM42  TM    SANAMIND,SANAMILN   TEST LAST NAME PRESENT                       
         BZ    OUTSM44                                                          
         MVC   SU_PLNA,SPACES                                                   
         IC    RF,SANAMELN                                                      
         CHI   RF,L'SU_PLNA        TEST > MAX FULL LENGTH                       
         BNH   *+8                                                              
         LHI   RF,L'SU_PLNA                                                     
         SHI   RF,1                                                             
         MVC   SU_PLNA(0),SANAME                                                
         EX    RF,*-6                                                           
         DROP  R1,R2                                                            
*                                                                               
OUTSM44  MVC   SU_1ACC,SPACES                                                   
         MVC   SU_PROC,SPACES                                                   
         MVC   SU_JOBC,SPACES                                                   
         MVC   SU_LEDG,SPACES                                                   
         MVC   SU_1ACC,SPACES                                                   
         MVC   SU_PROC,SPACES                                                   
         MVC   SU_JOBC,SPACES                                                   
         OC    PREV.TD#ULA,PREV.TD#ULA                                          
         JZ    OUTSM60                                                          
         MVC   SU_LEDG,PREV.TD#ULA                                              
         GOTOR GETNAME,DMCB,PREV.TD#ULA                                         
         JNE   OUTSM46                                                          
         MVC   SU_1NAM,X#CLINA                                                  
         MVC   SU_PRON,X#PRONA                                                  
         MVC   SU_JOBN,X#JOBNA                                                  
*                                                                               
OUTSM46  CLC   PRODUL,PREV.TD#ULA                                               
         BNE   OUTSM50                                                          
         CLC   PREV.TD#ACC,SPACES                                               
         BNH   OUTSM100                                                         
         SR    RF,RF                                                            
         IC    RF,PCLILEN                                                       
         SHI   RF,1                                                             
         MVC   SU_1ACC(0),PREV.TD#ACC                                           
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         SR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         LA    R1,PREV.TD#ACC                                                   
         AR    R1,RF                                                            
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         MVC   SU_PROC(0),0(R1)                                                 
         EX    RE,*-6                                                           
         SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    RE,RE                                                            
         IC    RE,PJOBLEN                                                       
         LA    R1,PREV.TD#ACC                                                   
         AR    R1,RF                                                            
         SR    RE,RF                                                            
         LA    R2,L'SU_JOBC                                                     
         CR    R2,RE                                                            
         BNL   OUTSM48                                                          
         LR    RE,R2                                                            
OUTSM48  SHI   RE,1                                                             
         MVC   SU_JOBC(0),0(R1)                                                 
         EX    RE,*-6                                                           
         B     OUTSM60                                                          
                                                                                
OUTSM50  MVC   SU_1ACC,PREV.TD#ACC                                              
*                                                                               
OUTSM60  GOTOR LP_APUTO,LP_D                  send and clear data               
*                                                                               
OUTSM100 TM    TSARERRS,TSEEOF     is it the end of the buffer                  
         BNO   OUTSM06                                                          
*                                                                               
OUTSUMX  XIT1                                                                   
*                                                                               
         DROP  PREV,R4                                                          
         LTORG                                                                  
                                                                                
***********************************************************************         
* Calendar periods retrieval                                          *         
*   This routine is used to build a list of time periods using the    *         
*   company calendar                                                  *         
*   If you came from a routine looking for a particular person it     *         
*   will also add period location information for that person into    *         
*   the period table                                                  *         
*                                                                               
*   R1=RUNNOPER IF CALPPER SHOULD BE SKIPPED                                    
***********************************************************************         
         SPACE 1                                                                
CALPRDS  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*CALPRDS'                                                      
*                                                                               
         NI    RUNFLAGS,FF-RUNNOPER                                             
         STC   R1,BYTE1                                                         
         OC    RUNFLAGS,BYTE1      RESET RUNNOPER FROM P1                       
         USING PERTABD,R4                                                       
         L     R4,AIO5             clear first entry of output area             
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO5                                        
                                                                                
         CLI   TS#PTYPE,TS#PTRTA   TS my approval search                        
         JNE   CALP001                                                          
         GOTOR GETAPCAL            Work out start and end dates                 
         JNE   CALP072               for previous 4 time periods                
*                                                                               
CALP001  XC    X#DITMS,X#DITMS                                                  
         XR    RE,RE                                                            
         ICM   RE,1,SCPYEL+CPYSFST-CPYELD                                       
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         BH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
                                                                                
         MVC   TS#CDAT,TS#CSDT     WORK OUT 1ST FISCAL YEAR TO SCAN             
         MVC   TS#CDAT+1(1),BYTE1                                               
                                                                                
         CLC   TS#CSDT+1(1),BYTE1                                               
         BNL   CALP002                                                          
         GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CDAT)                              
                                                                                
CALP002  GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'11'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CEND)                              
                                                                                
         MVC   TS#COFF,TS#1ROFF    and office                                   
                                                                                
CALP004  GOTOR GETCAL                                                           
         BH    CALP072             ERROR/Cal not found                          
                                                                                
         USING CASRECD,R2                                                       
         USING TMPELD,R3                                                        
         MVI   BYTE1,0                                                          
         L     R2,AIO8                                                          
         LA    R3,CASRFST                                                       
         XR    R0,R0                                                            
                                                                                
CALP006  CLI   TMPEL,0                                                          
         BE    CALP050                                                          
         CLI   TMPEL,TMPELQ                                                     
         BE    CALP010                                                          
                                                                                
CALP008  IC    R0,TMPLN                                                         
         AR    R3,R0                                                            
         B     CALP006                                                          
                                                                                
CALP010  CLC   TMPSTART,TS#CSDT    IS START DATE LOWER THAN PERIOD DATE         
         BNL   CALP012             NO - CHECK END DATE                          
         CLC   TMPEND,TS#CSDT      IS START DATE LOWER THAN PERIOD END          
         BL    CALP008             NO - NOT INTERESTED                          
         B     CALP016                                                          
CALP012  CLC   TMPEND,TS#CEDT      IS PERIOD END DATE LOWER THAN END            
         BNH   CALP016             YES                                          
         CLC   TMPSTART,TS#CEDT    IS PERIOD START DATE HIGHER THAN END         
         BH    CALP008             YES - NOT INTERESTED                         
*                                                                               
CALP016  BAS   RE,SETNONP          Set PERTABD with values                      
         TM    RUNFLAGS,RUNNOPER   Ok to call CALPPER?                          
         BNZ   CALP019                                                          
         CLI   TS#PTYPE,TS#PTRTA   TS my approval search                        
         JE    CALP019                                                          
         CLI   TS#PTYPE,TS#PTLAS   Approval by status                           
         BE    CALP019                                                          
         CLI   TS#PTYPE,TS#PTLAD   Approval by date                             
         BE    CALP019                                                          
         CLI   TS#PTYPE,TS#PTLAS   My timesheets by date or status              
         BNH   CALP018A              should call CALPPER                        
         CLI   TS#PTYPE,TS#PTSMP   Person sum calls should call CALPPER         
         BE    CALP018A                                                         
         CLI   TS#PTYPE,TS#PTSWP   Person sum calls should call CALPPER         
         BE    CALP018A                                                         
         CLI   TS#PTYPE,TS#PTSMS   Most summary calls can avoid this            
         BNL   CALP019                                                          
CALP018A GOTOR CALPPER                                                          
         BH    CALP072             GETCAL error                                 
         B     CALP008                                                          
*                                                                               
CALP019  L     R1,ABRATAB          CHECK MCS USER FOR PERIOD                    
CALP020  OC    0(L'GDADATE,R1),0(R1)                                            
         BZ    CALP040                                                          
         CLC   TMPEND,0(R1)                                                     
         BL    CALP026                                                          
         OC    L'GDADATE(L'GDADATE,R1),L'GDADATE(R1)                            
         BZ    CALP022                                                          
         CLC   TMPEND,L'GDADATE(R1)                                             
         BNL   CALP024                                                          
CALP022  OI    PERSTAT,PERSMCSU                                                 
         B     CALP026                                                          
*                                                                               
CALP024  NI    PERSTAT,X'FF'-PERSMCSU                                           
CALP026  LA    R1,L'GDADATE+L'GDADATE2(R1)                                      
         B     CALP020                                                          
*                                                                               
CALP040  LA    R4,PERLENQ(R4)                                                   
         XR    RE,RE                                                            
         IC    RE,X#DITMS                                                       
         AHI   RE,1                                                             
         STC   RE,X#DITMS                                                       
CALP045  MVI   0(R4),0                                                          
         B     CALP008             NEXT TMPEL                                   
*                                                                               
CALP050  DS    0H                                                               
         L     R2,AIO8                                                          
         LA    R3,CASRFST                                                       
         USING TMRELD,R3                                                        
CALP055  CLI   TMREL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                MISSING                                      
         CLI   TMREL,TMRELQ                                                     
         BE    CALP060                                                          
         LLC   RF,TMRLN                                                         
         AR    R3,RF                                                            
         B     CALP055                                                          
*                                                                               
CALP060  CLC   TS#CEDT(2),TMREND   DO WE NEED ANOTHER YEAR?                     
         BNH   CALP064                                                          
         DROP  R3                                                               
*                                                                               
         GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)      YES                       
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'1'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CDAT)                              
         GOTO1 VDATCON,DMCB,(1,TS#CEND),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'1'                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CEND)                              
         B     CALP004                                                          
*                                                                               
CALP064  CLI   TS#PTYPE,TS#PTRB    do we need person-level info?                
         BNH   CALP066             yes for my timesheets/approvals              
         CLI   TS#PTYPE,TS#PTRHO   office hours search (I think)                
         BE    CALP066                                                          
         CLI   TS#PTYPE,TS#PTRHP   or a person hour search                      
         BL    CALPRDSY             **SUMMARIES CONTROLLED BY RUNFLAGS          
*                                                                               
CALP066  L     R4,AIO5                                                          
         GOTOR CALITMS                                                          
         CLC   PERODS,SPACES       ENTRIES AT LOCATION LEVEL?                   
         BNH   CALPRDSY                                                         
         GOTOR LOAFIX              COMPRESS                                     
         B     CALPRDSY                                                         
*                                                                               
* error handling                                                                
CALP072  CLI   TS#PTYPE,TS#PTLAS   Ignore any missing cal errors                
         BNE   CALP074             if status list                               
         CLC   FULL2(2),=AL2(AE$NOCAL)                                          
         BE    CALPRDSY                                                         
CALP074  CLI   TS#PTYPE,TS#PTRTP   Person search timesheets                     
         BNE   CALPRDSN                                                         
         CLC   TODAYP,TS#CDAT      Is the calendar in the future                
         BNL   CALPRDSN            No                                           
         GOTOR CALITMS                                                          
*                                                                               
CALPRDSY CR    RB,RB                                                            
         J     *+6                                                              
CALPRDSN LTR   RB,RB                                                            
         XIT1                                                                   
*                                                                               
***********************************************************************         
* Get calendar for approver and work out start and end date                     
* for last 4 previous time periods prior to today                               
***********************************************************************         
*                                                                               
GETAPCAL NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETAPC*'                                                      
         XC    X#DITMS,X#DITMS                                                  
         XR    RE,RE                                                            
         ICM   RE,1,SCPYEL+CPYSFST-CPYELD                                       
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         BH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
                                                                                
         MVC   TS#CDAT,TODAYP                                                   
         MVC   TS#CDAT+1(1),BYTE1                                               
                                                                                
         CLC   TODAYP+1(1),BYTE1                                                
         JNL   GETAP02                                                          
         GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CDAT)                              
                                                                                
GETAP02  GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'M',WORK),WORK+6,F'11'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CEND)                              
                                                                                
         MVC   TS#COFF,TS#APOFF    and office                                   
                                                                                
GETAP04  GOTOR GETCAL                                                           
         JH    EXITN               ERROR/Cal not found                          
                                                                                
         USING CASRECD,R2                                                       
         USING TMPELD,R3                                                        
         L     R2,AIO8                                                          
         LA    R3,CASRFST                                                       
         XR    R0,R0                                                            
                                                                                
GETAP06  CLI   TMPEL,0             Should never get to end of record            
         JE    *+2                  so die if you do                            
         CLI   TMPEL,TMPELQ        Find the period that today is in             
         JE    GETAP10                                                          
                                                                                
GETAP08  IC    R0,TMPLN            Bump to next element                         
         AR    R3,R0                                                            
         J     GETAP06                                                          
                                                                                
GETAP10  CLC   TMPSTART,TODAYP     Is today lower than period date              
         JH    GETAP08             no - get next period                         
         CLC   TMPEND,TODAYP       Is today higher than period end date         
         JL    GETAP08             yes - not interested                         
GETAP12  LA    R4,5                R4=counter for previous periods              
         LLC   RE,TMPNUMB          Save current period number                   
         BCTR  RE,0                Subtract 1 to get to previous period         
         STC   RE,X#DITMS          Reset current period number                  
*                                                                               
GETAP14  CHI   RE,0                Do we have a previous period                 
         JH    GETAP22             Yes                                          
                                                                                
* Read previous years calendar to get previous periods                          
                                                                                
         GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CDAT)                              
         GOTO1 VDATCON,DMCB,(1,TS#CEND),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CEND)                              
                                                                                
         GOTOR GETCAL                                                           
         JH    EXITN               ERROR/Cal not found                          
                                                                                
         USING CASRECD,R2                                                       
         USING TMPELD,R3                                                        
         L     R2,AIO8             Find last period on previous                 
         LA    R3,CASRFST           calendar year                               
         XR    R0,R0                                                            
                                                                                
GETAP16  CLI   TMPEL,0             EOR                                          
         JE    GETAP22                                                          
         CLI   TMPEL,TMPELQ                                                     
         JE    GETAP20                                                          
                                                                                
GETAP18  IC    R0,TMPLN            Bump to next element                         
         AR    R3,R0                                                            
         J     GETAP16                                                          
                                                                                
GETAP20  CLC   TMPNUMB,X#DITMS                                                  
         JNH   GETAP18                                                          
         MVC   X#DITMS,TMPNUMB     Save last period number                      
         J     GETAP18                                                          
                                                                                
GETAP22  L     R2,AIO8                                                          
         LA    R3,CASRFST                                                       
         XR    R0,R0                                                            
                                                                                
GETAP24  CLI   TMPEL,0                                                          
         JE    *+2                                                              
         CLI   TMPEL,TMPELQ                                                     
         JE    GETAP28                                                          
                                                                                
GETAP26  IC    R0,TMPLN                                                         
         AR    R3,R0                                                            
         J     GETAP24                                                          
*                                                                               
GETAP28  CLC   TMPNUMB,X#DITMS     Do we have a match for the period            
         JNE   GETAP26                                                          
         CHI   R4,5                last period in previous 4                    
         JNE   *+10                No                                           
         MVC   TS#CEDT,TMPEND      Yes - save end date                          
         CHI   R4,1                First period in previous 4                   
         JNE   *+10                No                                           
         MVC   TS#CSDT,TMPSTART    Yes - save start date                        
*                                                                               
         LLC   RE,X#DITMS          Work out next previous period                
         BCTR  RE,0                                                             
         STC   RE,X#DITMS                                                       
         JCT   R4,GETAP14          Do we have our 4 previous periods            
                                                                                
         XR    R1,R1               Create 2s complement of dates                
         ICM   R1,7,TS#CEDT                                                     
         LNR   R1,R1                                                            
         STCM  R1,7,TS#CRED                                                     
         XR    R1,R1                                                            
         ICM   R1,7,TS#CSDT                                                     
         LNR   R1,R1                                                            
         STCM  R1,7,TS#CRSD                                                     
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* add Person details to PERTAB entry                                            
* sets PERLOCS, PERLOCE, PERODS, PERSINVL and PERSMCSU                          
* ON ENTRY, ASSUME R3 = TMPEL, R4 IS PERTAB ENTRY                               
* on exit, r4 points beyond last entry added, R3 UPDATED IF NEW                 
* CALENDAR READ                                                                 
***********************************************************************         
*                                                                               
         USING TMPELD,R3                                                        
CALPPER  NTR1  ,                                                                
         L     R1,AIO4                                                          
         USING PERRECD,R1                                                       
*                                                                               
         LA    R2,PERRFST                                                       
         USING LOCELD,R2                                                        
CALPP010 CLI   LOCEL,0                                                          
         BE    CALPPXYE            done all locations                           
         CLI   LOCEL,LOCELQ                                                     
         BE    CALPP040                                                         
CALPP020 XR    R0,R0                                                            
         IC    R0,LOCLN                                                         
         AR    R2,R0                                                            
         B     CALPP010                                                         
         DROP  R1                                                               
*                                                                               
CALPP039 BAS   RE,SETNONP          RESET TMPEL VALUES ON NEW CALENDAR           
*                                                                               
CALPP040 CLI   TS#PTYPE,TS#PTLAS   All list calls can avoid this                
         BH    CALPP041            search filtering                             
         CLI   RQ_TLSU,RQ_TLAQ     If approver call then                        
         BE    CALPP050            skip lock date checking                      
         B     CALPP042                                                         
*                                                                               
CALPP041 CLI   TS#PTYPE,TS#PTSMS   All summary calls can avoid this             
         BNL   CALPP042            search filtering                             
                                                                                
         CLC   RQ_TTOFF,SPACES                                                  
         JNH   CALPP042                                                         
         CLC   RQ_TTOFF,LOCOFF                                                  
         JNE   CALPP020                                                         
         CLC   RQ_TTDEP,SPACES                                                  
         JNH   CALPP042                                                         
         CLC   RQ_TTDEP,LOCDEPT                                                 
         JNE   CALPP020                                                         
         CLC   RQ_TTSUB,SPACES                                                  
         JNH   CALPP042                                                         
         CLC   RQ_TTSUB,LOCSUB                                                  
         JNE   CALPP020                                                         
*                                                                               
CALPP042 OC    LOCLOCK,LOCLOCK     HAVE WE GOT A TIMESHEET LOCK DATE            
         BZ    CALPP050                                                         
         OC    LOCEND,LOCEND                                                    
         BZ    CALPP045                                                         
         CLC   LOCLOCK,LOCEND                                                   
         BNL   CALPP050                                                         
CALPP045 CLC   TMPSTART,LOCLOCK    PERIOD AFTER LOCK DATE NO GOOD               
         BH    CALPP020            GET NEXT ELEMENT                             
CALPP050 OC    LOCEND,LOCEND       HAVE WE GOT A LOCATION END DATE              
         BZ    *+14                NO                                           
         CLC   TMPSTART,LOCEND     PERIOD START DATE AFTER END DATE             
         BH    CALPP020            YES - NO GOOD GET NEXT ELEMENT               
         CLC   TMPSTART,LOCSTART   PERIOD START DATE BEFORE LOC START           
         BNL   CALPP055            NO                                           
         CLC   LOCSTART,TMPEND     YES CHECK NOT LOA                            
         BH    CALPP020                                                         
         MVC   PERLOCS,LOCSTART                                                 
*                                                                               
CALPP055 L     RF,AIO8                                                          
         CLC   LOCOFF,CASKOFC-CASKEY(RF)                                        
         BE    CALPP060                                                         
                                                                                
CALPP056 MVC   TS#COFF,LOCOFF      DIFFERENT OFFICE MAY HAVE DIFF.              
         MVC   FULL1,TMPSTART                                                   
         GOTOR GETCAL                             CALENDAR                      
         BL    CALPP060            CAL FOUND, BUT NOT FOR THIS OFF              
         BH    CALPPXNO            ERROR                                        
                                                                                
*   CALENDAR HAS CHANGED, RESET A(tmpel)                                        
                                                                                
         L     R3,AIO8                                                          
         LA    R3,CASRFST-CASRECD(R3)                                           
CALPP058 CLI   0(R3),0             end of the Element?                          
         JNE   *+6                                                              
         DC    H'0'                wrong calendar rec, or logic error           
         LA    RF,TS#CSDT                                                       
         CLC   TS#CSDT,FULL1       Change over date lower than start            
         BH    *+8                 Yes - use start date                         
         LA    RF,FULL1                                                         
         CLC   TMPEND,0(RF)        COVERS start date from 'old' tmpel           
         BL    CALPP059                                                         
         CLC   TMPSTART,0(RF)                                                   
         BNH   CALPP039                                                         
                                                                                
CALPP059 LLC   RE,TMPLN                                                         
         AR    R3,RE                                                            
         B     CALPP058                                                         
*                                                                               
CALPP060 LA    RE,1                IF 2 CHAR OFFICE PERSON CODE SHORTER         
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         BNZ   *+8                                                              
         LA    RE,0                                                             
         MVC   PERODS(0),LOCOFF                                                 
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         LA    R1,PERODS                                                        
         AR    R1,RE                                                            
                                                                                
         LR    R0,RE                                                            
         XR    RF,RF                                                            
         IC    RF,ONERL2L                                                       
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         MVC   0(0,R1),LOCDEPT                                                  
         EX    RF,*-6                                                           
         LA    R1,1(RF,R1)                                                      
         IC    R0,ONERL2L                                                       
         IC    RF,ONERL3L                                                       
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         MVC   0(0,R1),LOCSUB                                                   
         EX    RF,*-6                                                           
*                                                                               
         OC    PERODS,SPACES                                                    
         MVC   TS#1RACT,SPACES                                                  
         MVC   TS#1RACT(L'PERODS),PERODS                                        
         XR    RE,RE                                                            
         IC    RE,ONERL3L                                                       
         LA    R1,TS#1RACT                                                      
         AR    R1,RE                                                            
         L     RF,AIO4                                                          
         USING PERRECD,RF                                                       
         LA    R5,L'ACTKACT                                                     
         SR    R5,RE                                                            
         SHI   R5,1                                                             
         MVC   0(0,R1),PERKCODE                                                 
         EX    R5,*-6                                                           
         DROP  RF                                                               
*                                                                               
         CLI   TS#PTYPE,TS#PTRTP   person search - timesheets                   
         BE    CALPP062                                                         
         CLI   TS#PTYPE,TS#PTSMP   person summary monthly                       
         BE    CALPP063                                                         
         CLI   TS#PTYPE,TS#PTSWP   person summary weekly                        
         BE    CALPP063                                                         
         CLI   TS#PTYPE,TS#PTRHP   person search - hours                        
         BNE   CALPP078                                                         
*                                                                               
CALPP062 CLI   RQ_TSSRC,RQ_TSTMY   my timesheets?                               
         BE    CALPP078            yes - then don't filter limit list           
         CLI   RQ_TPRSR,C'Y'       Are we able to overide person search         
         JE    CALPP078            yes - ignore limlst and approver             
*                                                                               
OR       USING GAPTABD,GAPAREA                                                  
CALPP063 XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         J     CALPP066                                                         
CALPP064 GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
CALPP066 TM    ATSRERRS,TSEEOF                                                  
         JZ    CALPP068                                                         
         OI    PERSTAT,PERSINVL    Buffer is empty - no match                   
         J     CALPP078                                                         
CALPP068 TM    OR.GAPTSTA,GAPTSMQ  main entry                                   
         JZ    CALPP064                                                         
         CLI   OR.GAPTDAT1,GAPTT1Q 1R entry                                     
         JNE   CALPP064                                                         
*                                                                               
         LLC   R1,ONERL1L                                                       
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         CLC   OR.GAPTACT(0),SPACES Have we got an office GAPLST                
         EX    R1,0(RF)                                                         
         JE    CALPP078            No - exit ok                                 
         BASR  RF,0                                                             
         CLC   OR.GAPTACT(0),LOCOFF Yes - does it match location ele            
         EX    R1,0(RF)                                                         
         JNE   CALPP064            No - get next record from GAPLST             
                                                                                
         LA    RE,OR.GAPTACT                                                    
         LLC   R1,ONERL2L                                                       
         LLC   RF,ONERL1L                                                       
         AR    RE,RF               RE=A(Department code in 1R account)          
         SR    R1,RF               R1=length of department code                 
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         CLC   0(0,RE),SPACES      Have we department code                      
         EX    R1,0(RF)                                                         
         JE    CALPP078            No - matched at office so ok                 
         BASR  RF,0                                                             
         CLC   0(0,RE),LOCDEPT     Yes - does it match                          
         EX    R1,0(RF)                                                         
         JNE   CALPP064            No - get next record from GAPLST             
*                                                                               
         AHI   R1,1                                                             
         AR    RE,R1               RE=A(Sub dept code in 1R account)            
         LLC   R1,ONERL3L                                                       
         LLC   RF,ONERL2L                                                       
         SR    R1,RF               R1=length of sub dept code                   
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         CLC   0(0,RE),SPACES      Have we sub dept code                        
         EX    R1,0(RF)                                                         
         JE    CALPP078            No - matched at dept so ok                   
         BASR  RF,0                                                             
         CLC   0(0,RE),LOCSUB      Yes - does it match                          
         EX    R1,0(RF)                                                         
         JNE   CALPP064            No - get next record from GAPLST             
                                                                                
         AHI   R1,1                                                             
         AR    RE,R1               RE=A(Person code in 1R account)              
         LLC   R1,ONERL4L                                                       
         LLC   RF,ONERL3L                                                       
         SR    R1,RF               R1=length of person code                     
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         CLC   0(0,RE),SPACES      Have we person code                          
         EX    R1,0(RF)                                                         
         JE    CALPP078            No - matched at sub so ok                    
         L     R5,AIO4                                                          
         BASR  RF,0                                                             
         CLC   0(0,RE),PERKCODE-PERRECD(R5) Yes - does it match                 
         EX    R1,0(RF)                                                         
         JNE   CALPP064            No - get next record from GAPLST             
*                                                                               
CALPP078 GOTOR CHKMCS              uses AIO3!!!!!                               
         L     R5,ABRATAB          check if person is bo user for this          
CALPP080 OC    0(L'GDADATE,R5),0(R5)                         period             
         BZ    CALPP100                                                         
         CLC   TMPEND,0(R5)                                                     
         BL    CALPP095                                                         
         OC    L'GDADATE(L'GDADATE,R5),L'GDADATE(R5)                            
         BZ    CALPP085                                                         
         CLC   TMPEND,L'GDADATE(R5)                                             
         BNL   CALPP090                                                         
CALPP085 OI    PERSTAT,PERSMCSU                                                 
         B     CALPP095                                                         
*                                                                               
CALPP090 NI    PERSTAT,X'FF'-PERSMCSU                                           
CALPP095 LA    R5,L'GDADATE+L'GDADATE2(R5)                                      
         B     CALPP080                                                         
*                                                                               
CALPP100 MVC   PERLOCE,=X'FFFFFF'                                               
         OC    LOCEND,LOCEND       LOCATION END DATE EXISTS                     
         BZ    CALPPXUP            NO                                           
         CLC   TMPEND,LOCEND       PERIOD END DATE BEFORE LOCATION END          
         BNH   CALPPXUP                                                         
*                                                                               
         MVC   PERLOCE,LOCEND      no, split location                           
         LA    R4,PERLENQ(R4)      WE NEED ANOTHER ENTRY, THEN                  
         MVI   0(R4),0                                                          
CALPP110 LLC   R0,LOCLN                                                         
         AR    R2,R0               force-process next locel on rec?             
         CLI   LOCEL,LOCELQ                                                     
         BE    CALPP115            ISN'T ONE - TERMINATION                      
         MVI   0(R4),0                 STOP HERE.                               
         B     CALPPXYE                                                         
                                                                                
CALPP115 CLI   TS#PTYPE,TS#PTLAS   All list calls can avoid this                
         BNH   CALPP116            search filtering                             
         CLI   TS#PTYPE,TS#PTSMS   All summary calls can avoid this             
         BNL   CALPP116            search filtering                             
                                                                                
         CLC   RQ_TTOFF,SPACES     Check search filters                         
         JNH   CALPP116                                                         
         CLC   RQ_TTOFF,LOCOFF                                                  
         JNE   CALPP110                                                         
         CLC   RQ_TTDEP,SPACES                                                  
         JNH   CALPP116                                                         
         CLC   RQ_TTDEP,LOCDEPT                                                 
         JNE   CALPP110                                                         
         CLC   RQ_TTSUB,SPACES                                                  
         JNH   CALPP116                                                         
         CLC   RQ_TTSUB,LOCSUB                                                  
         JNE   CALPP110                                                         
                                                                                
CALPP116 XR    RE,RE                                                            
         IC    RE,X#DITMS                                                       
         AHI   RE,1                                                             
         STC   RE,X#DITMS                                                       
         CLC   TS#COFF,LOCOFF                                                   
         BE    CALPP125                                                         
         MVC   TS#COFF,LOCOFF      YES                                          
*                                                                               
         LLC   RE,ONERL1L          AMEND 1R ACCOUNT ODS                         
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   TS#1RACT(0),LOCOFF                                               
         EX    RE,0(RF)                                                         
*                                                                               
         LA    R1,TS#1RACT         +1 FOR EX'D RE                               
         LLC   RE,ONERL2L                                                       
         LLC   RF,ONERL1L                                                       
         AR    R1,RF               A(DEPT)                                      
         SR    RE,RF               L'DEPT                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   0(0,R1),LOCDEPT                                                  
         EX    RE,0(RF)            AMEND 1R ACCOUNT ODS                         
*                                                                               
         LA    R1,TS#1RACT                                                      
         LLC   RE,ONERL3L                                                       
         LLC   RF,ONERL2L                                                       
         AR    R1,RF               A(SUBDEPT)                                   
         SR    RE,RF               L'SUBDEPT                                    
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   0(0,R1),LOCSUB                                                   
         EX    RE,0(RF)                                                         
*                                                                               
         GOTOR GETCAL              GET CALENDAR FOR NEW OFFICE CODE             
         BH    CALPPXNO              ERROR                                      
         L     R1,AIO8             AND FIND ENTRY MATCHING THIS LOCEL           
         LA    R3,CASRFST-CASRECD(R1)                                           
CALPP120 CLI   TMPEL,0             IF NOT FOUND SKIP PERIOD                     
         BE    CALPPXYE                                                         
         CLI   TMPEL,TMPELQ                                                     
         BNE   CALPP130                                                         
         CLC   TMPSTART,LOCSTART                                                
         BH    CALPP130                                                         
         CLC   TMPEND,LOCSTART                                                  
         BL    CALPP130                                                         
CALPP125 BAS   RE,SETNONP         SET NON-PERSONAL DATA ON NEW ENTRY            
         MVI   PERSTAT,0                                                        
         B     CALPP040            GO BACK FOR PERSONAL STUFF                   
*                                                                               
CALPP130 LLC   RF,TMPLN                                                         
         AR    R3,RF                                                            
         B     CALPP120                                                         
*                                                                               
CALPPXLO MVC   FULL2,=AL2(AE$IVLOC)   LOCATION ERROR                            
*                                                                               
CALPPXNO CLI   *,0                 GETCAL ERROR  CC=HIGH                        
         B     CALPPERX                                                         
*                                                                               
CALPPXUP LA    R4,PERLENQ(R4)      EXIT WITH ENTRY ADDED    EN                  
         MVI   0(R4),0                  SET TERMINATOR                          
         XR    RE,RE                                                            
         IC    RE,X#DITMS                                                       
         AHI   RE,1                                                             
         STC   RE,X#DITMS                                                       
CALPPXYE XC    PERENDT,PERENDT     EXIT WITH NOTHING ADDED                      
         CR    RB,RB                                                            
CALPPERX XIT1  REGS=(R3,R4)                                                     
         EJECT                                                                  
***********************************************************************         
* SET NON-PERSONAL INFO IN PERTAB ENTRY                                         
* ASSUMES R4 POINTS AT PERTAB ENTRY, R3 AT TMPEL                                
***********************************************************************         
         USING PERTABD,R4                                                       
         USING TMPEL,R3                                                         
SETNONP  MVC   PERNUM,TMPNUMB      SET NON-PERSONAL DATA ON NEW ENTRY           
         MVC   PERSTDT,TMPSTART                                                 
         XR    RF,RF                                                            
         ICM   RF,7,TMPEND                                                      
         LNR   RF,RF                                                            
         STCM  RF,7,PERENDT                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* count pertab entries, set X#DITMS  (for sort etc)                             
***********************************************************************         
         USING PERTABD,R4                                                       
CALITMS  L     R4,AIO5                                                          
         SR    RF,RF                                                            
CALIT10  CLI   0(R4),0                                                          
         JE    CALIT20                                                          
         LA    RF,1(RF)                                                         
         LA    R4,PERLENQ(R4)                                                   
         J     CALIT10                                                          
*                                                                               
CALIT20  STC   RF,X#DITMS                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* COMPRESS PERTAB TO ONE ENTRY PER END DATE/LOCATION.                           
* NECESSARY IF LOA OR PERSON TRANSFERRED WITHOUT CHANGING LOCATION (?)          
***********************************************************************         
*                                                                               
         USING PERTABD,R4                                                       
LOAFIX   NTR1                                                                   
         L     R4,AIO5             I THINK THIS BIT ADJUSTS THE TABLE           
         LA    R2,PERLENQ(R4)      FOR LOA...?                                  
NXT      USING PERTABD,R2          NOT SURE WHY NOT HANDLED IN CALPPER          
         XR    R3,R3                                                            
         IC    R3,X#DITMS                                                       
         LTR   R3,R3                                                            
         BZ    LOAFIXX                                                          
LOAF030  CLC   PERENDT(L'PERENDT+L'PERODS),NXT.PERENDT                          
         BNE   LOAF050                                                          
         OC    PERSTAT,NXT.PERSTAT                                              
         CLC   PERLOCE,NXT.PERLOCE                                              
         BNL   *+10                                                             
         MVC   PERLOCE,NXT.PERLOCE                                              
                                                                                
         CLC   PERLOCS,NXT.PERLOCS                                              
         BNH   *+10                                                             
         MVC   PERLOCS,NXT.PERLOCS                                              
         SHI   R3,1                                                             
         XR    RE,RE                                                            
         IC    RE,X#DITMS          REDUCE COUNT BY 1                            
         SHI   RE,1                                                             
         STC   RE,X#DITMS                                                       
*                                                                               
         LR    R1,R3                                                            
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         LA    RF,PERLENQ                                                       
         MR    RE,R1                                                            
         LR    R1,RF                                                            
         LA    RE,NXT.PERENDT+PERLENQ                                           
         LA    R0,NXT.PERENDT                                                   
         MVCL  R0,RE               MOVE TABLE UP ONE                            
         B     LOAF030                                                          
*                                                                               
LOAF050  LA    R4,PERLENQ(R4)                                                   
         LA    R2,PERLENQ(R2)                                                   
         BCT   R3,LOAF030                                                       
LOAFIXX  XIT1                                                                   
         DROP  NXT,R3                                                           
         LTORG                                                                  
***********************************************************************         
* Test current 1R account versus GAPTAB entries                       *         
* Runs in two modes. The old one is to be passed a main entry in                
* GAPAREA2, and to look for exceptions.                                         
* The new mode I've added runs when GAPAREA2 is null - It'll check the          
* whole table to see if the account in TS#1RACT is accessible.                  
***********************************************************************         
         SPACE 1                                                                
         USING GAPTABD,R4                                                       
TT1RVGT  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*TT1RVGT'                                                      
*                                                                               
         LA    R4,GAPAREA2         point to main/office entry                   
         OC    GAPAREA2,GAPAREA2                                                
         BZ    TT1RV50                                                          
*                                                                               
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         LA    R3,TS#1RACT                                                      
P        USING ACTKACT,R3                                                       
                                                                                
         TM    X#ACCSTA,X#CLISR    Are we doing a cli search?                   
         BZ    TT1RV08                                                          
OR       USING GAPTABD,GAPAREA3                                                 
         XC    GAPAREA3,GAPAREA3                                                
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA3,  +        
               TSARABUF                                                         
         J     TT1RV04                                                          
TT1RV02  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA3,  +        
               TSARABUF                                                         
TT1RV04  TM    ATSRERRS,TSEEOF                                                  
         JNZ   TT1RVGLN                                                         
         CLI   OR.GAPTDAT1,GAPTT1Q    and find first 1R entry                   
         JNE   TT1RV02                                                          
TT1RV06  TM    OR.GAPTSTA,GAPTSMQ                                               
         BZ    TT1RV02                                                          
         LLC   RF,OR.GAPTLEN                                                    
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   P.ACTKACT(0),OR.GAPTACT                                          
         EX    RF,0(RE)                                                         
         BNE   TT1RV02                                                          
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         DROP  OR                                                               
                                                                                
TT1RV08  LLC   RF,GAPTLEN                                                       
         AR    RF,R3                                                            
         CLI   0(RF),C' '          Is the account any longer than entry         
         BE    TT1RV70             No - so keep account                         
                                                                                
TT1RV10  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF     End of buffer                                
         JNZ   TT1RVGLY            Yes                                          
         TM    GAPTSTA,GAPTSMQ     next group/office                            
         BO    TT1RV70                                                          
                                                                                
TT1RV12  LLC   RF,GAPTLEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         CLC   GAPTACT(0),P.ACTKACT                                             
         EX    RF,0(RE)                                                         
         BNE   TT1RV10                                                          
         B     TT1RVGLN                                                         
*                                                                               
*                                  MAIN ENTRY NOT SUPPLIED                      
TT1RV50  DS    0H                                                               
         MVI   GAPTDAT1,GAPTT1Q                                                 
         LLC   RF,ONERL1L                                                       
         SHI   RF,1                                                             
         MVC   GAPTCODE(0),TS#1RACT                                             
         EX    RF,*-6                                                           
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
         J     TT1RV60                                                          
*                                                                               
TT1RV59  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF                                                         
*                                                                               
TT1RV60  TM    ATSRERRS,TSEEOF     LOOK FOR A MAIN ENTRY MATCH                  
         JNZ   TT1RVGLN                                                         
         CLI   GAPTDAT1,GAPTT1Q                                                 
         BNE   TT1RVGLN                                                         
         LLC   RF,GAPTLEN                                                       
         SHI   RF,1                                                             
         CLC   GAPTCODE(0),TS#1RACT                                             
         EX    RF,*-6                                                           
         BNE   TT1RV59                                                          
         TM    GAPTSTA,GAPTSMQ                                                  
         BZ    TT1RV59                                                          
*                                                                               
TT1RV70  GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA2,  +        
               TSARABUF            AND THEN CHECK FOR EXCEPTIONS                
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   TT1RVGLY                                                         
         CLI   GAPTDAT1,GAPTT1Q                                                 
         BNE   TT1RVGLY                                                         
         TM    GAPTSTA,GAPTSMQ     NEXT MAIN ENTRY, WE'RE OK                    
         BO    TT1RV70                                                          
         LLC   RF,GAPTLEN                                                       
         SHI   RF,1                                                             
         CLC   GAPTCODE(0),TS#1RACT                                             
         EX    RF,*-6                                                           
         BNE   TT1RV70             SOME OTHER EXCEPTION, CARRY ON               
         B     TT1RVGLN                                                         
*                                                                               
*    REREAD GAPAREA ENTRY ON EXIT, IN CASE WE BROKE HI/NEXT SEQ                 
TT1RVGLY GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         CR    RB,RB                                                            
         B     TT1RVGLX                                                         
TT1RVGLN GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         LTR   RB,RB                                                            
TT1RVGLX XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* GET CLIENT OR NON CLIENT, PRODUCT, JOB NAME                         *         
*                                                                               
* NOTE THIS WILL SET TD#ULA TO FFs ON EACH RECORD PROCESSED                     
***********************************************************************         
         SPACE 1                                                                
CLINAME  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*CLINAM*'                                                    
         CLI   TS#PTYPE,TS#PTRHP                                                
         BE    CLIN02                                                           
         CLI   TS#PTYPE,TS#PTRHC                                                
         BE    CLIN02                                                           
         CLI   TS#PTYPE,TS#PTRHO                                                
         BNE   CLIN08                                                           
         USING TD#TABD,R4                                                       
CLIN02   MVC   X#CLINA(X#CPJLN),SPACES                                          
         MVC   X#ULAC,SPACES                                                    
CLIN04   L     R4,AIO1                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO1   clear AIO1                           
         GOTOR (#MTSAR,AMTSAR),DMCB,TSARDH,TSARTBUF,TSARTDL                     
         TM    TSARERRS,TSEEOF     is it the end of the buffer                  
         BO    CLIN08              YES                                          
         CLI   TD#UNT,X'FF'                                                     
         BE    CLIN08                                                           
*                                                                               
         GOTOR (#MTSAR,AMTSAR),DMCB,TSADEL,TSARTBUF,TSARTDL                     
         BE    *+6                                                              
         DC    H'0'                yes                                          
         MVC   TD#TIMAC,TD#ULA                                                  
         GOTOR GETNAME,DMCB,TD#ULA                                              
         BNE   CLIN06                                                           
         MVC   TD#CLINM,X#CLINA                                                 
         MVC   TD#PRDNM,X#PRONA                                                 
         MVC   TD#JOBNM,X#JOBNA                                                 
         MVC   TD#JOBLK,X#JOBLK                                                 
         MVC   TD#JOBCL,X#JOBCL                                                 
CLIN06   MVC   TD#ULA,FFS                                                       
         GOTOR (#MTSAR,AMTSAR),DMCB,TSAADD,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    CLIN04              get next record in old buffer                
         DC    H'0'                                                             
*                                                                               
CLIN08   J     EXITY                                                            
         DROP  R4                                                               
         LTORG                                                                  
***********************************************************************         
* GET WORKCODE NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
WCNAME   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*WCNAME*'                                                    
         CLI   TS#PTYPE,TS#PTRHP                                                
         BE    WCNM02                                                           
         CLI   TS#PTYPE,TS#PTRHC                                                
         BE    WCNM02                                                           
         CLI   TS#PTYPE,TS#PTRHO                                                
         BNE   WCNM12                                                           
         USING TD#TABD,R4                                                       
WCNM02   MVC   TEMP2,SPACES                                                     
WCNM04   L     R4,AIO1                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO1   clear AIO1                           
         GOTOR (#MTSAR,AMTSAR),DMCB,TSARDH,TSARTBUF,TSARTDL                     
         TM    TSARERRS,TSEEOF     is it the end of the buffer                  
         BO    WCNM12              YES                                          
         CLI   TD#WRKC,X'FF'                                                    
         BE    WCNM12                                                           
*                                                                               
WCNM06   GOTOR (#MTSAR,AMTSAR),DMCB,TSADEL,TSARTBUF,TSARTDL                     
         BE    *+6                                                              
         DC    H'0'                yes                                          
TST      USING WCOELD,TEMP2                                                     
         CLC   TST.WCOCODE,TD#WRKC                                              
         BE    WCNM08                                                           
         DROP  TST                                                              
         MVC   TEMP2(L'TD#WRKC),TD#WRKC                                         
         GOTOR (#GETWCD,AGETWCD)                                                
WCNM08   MVC   TD#TSKNM,TEMP2                                                   
         MVC   TD#TSKCD,TD#WRKC                                                 
         MVC   TD#WRKC,FFS                                                      
         GOTOR (#MTSAR,AMTSAR),DMCB,TSAADD,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    WCNM04              get next record in old buffer                
         DC    H'0'                                                             
*                                                                               
WCNM12   J     EXITY                                                            
         DROP  R4                                                               
         LTORG                                                                  
***********************************************************************         
* GET ORDER NAME                                                      *         
***********************************************************************         
         SPACE 1                                                                
ORDNAME  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*ORDNAME'                                                    
         CLI   TS#PTYPE,TS#PTRHP                                                
         BE    ORNM02                                                           
         CLI   TS#PTYPE,TS#PTRHC                                                
         BE    ORNM02                                                           
         CLI   TS#PTYPE,TS#PTRHO                                                
         BNE   ORNM20                                                           
         USING TD#TABD,R4                                                       
ORNM02   MVC   TEMP2,SPACES                                                     
ORNM04   L     R4,AIO1                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO1   clear AIO1                           
         GOTOR (#MTSAR,AMTSAR),DMCB,TSARDH,TSARTBUF,TSARTDL                     
         TM    TSARERRS,TSEEOF     is it the end of the buffer                  
         BO    ORNM20              YES                                          
         CLI   TD#ORD#,X'FF'                                                    
         BE    ORNM20                                                           
*                                                                               
ORNM06   GOTOR (#MTSAR,AMTSAR),DMCB,TSADEL,TSARTBUF,TSARTDL                     
         BE    *+6                                                              
         DC    H'0'                yes                                          
         MVC   TD#ORDNM,SPACES                                                  
         OC    TD#ORD#,TD#ORD#                                                  
         BZ    ORNM18                                                           
         USING ORDRECD,R3                                                       
         LA    R3,IOKEY                                                         
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUXCPY                                                   
         MVC   ORDKORD,TD#ORD#                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3                                                          
         LA    R2,ORDRFST                                                       
         USING ENMELD,R2                                                        
         XR    R0,R0                                                            
ORNM08   CLI   ENMEL,0                                                          
         BE    ORNM18                                                           
         CLI   ENMEL,ENMELQ                                                     
         BE    ORNM10                                                           
         IC    R0,ENMLN                                                         
         AR    R2,R0                                                            
         B     ORNM08                                                           
*                                                                               
ORNM10   XR    RF,RF                                                            
         IC    RF,ENMLN                                                         
         SHI   RF,ENMLNQ+1                                                      
         MVC   TD#ORDNM(0),ENMNAME                                              
         EX    RF,*-6                                                           
         DROP  R2                                                               
ORNM18   MVC   TD#ORD#2,TD#ORD#                                                 
         MVC   TD#ORD#,FFS                                                      
         GOTOR (#MTSAR,AMTSAR),DMCB,TSAADD,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    ORNM04              get next record in old buffer                
         DC    H'0'                                                             
*                                                                               
ORNM20   J     EXITY                                                            
         DROP  R4                                                               
         LTORG                                                                  
***********************************************************************         
* GET ESTIMATE NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
ESTNAME  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*ESTNAME'                                                    
         CLI   TS#PTYPE,TS#PTRHP                                                
         BE    ESNM02                                                           
         CLI   TS#PTYPE,TS#PTRHC                                                
         BE    ESNM02                                                           
         CLI   TS#PTYPE,TS#PTRHO                                                
         BNE   ESNM20                                                           
         USING TD#TABD,R4                                                       
ESNM02   MVC   TEMP2,SPACES                                                     
ESNM04   L     R4,AIO1                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO1   clear AIO1                           
         GOTOR (#MTSAR,AMTSAR),DMCB,TSARDH,TSARTBUF,TSARTDL                     
         TM    TSARERRS,TSEEOF     is it the end of the buffer                  
         BO    ESNM20              YES                                          
         CLI   TD#EST#,X'FF'                                                    
         BE    ESNM20                                                           
*                                                                               
ESNM06   GOTOR (#MTSAR,AMTSAR),DMCB,TSADEL,TSARTBUF,TSARTDL                     
         BE    *+6                                                              
         DC    H'0'                yes                                          
         MVC   TD#ESTNM,SPACES                                                  
         OC    TD#EST#,TD#EST#                                                  
         BZ    ESNM18                                                           
         USING EGNPASD,R3                                                       
         LA    R3,IOKEY                                                         
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,CUXCPY                                                   
         MVC   EGNPNUM,TD#EST#                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   EGNPAS(EGNPCLI-EGNPASD),IOKEYSAV                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3                                                          
         USING ESTRECD,R3                                                       
         LA    R2,ESTRFST                                                       
         USING ENMELD,R2                                                        
         XR    R0,R0                                                            
ESNM08   CLI   ENMEL,0                                                          
         BE    ESNM18                                                           
         CLI   ENMEL,ENMELQ                                                     
         BE    ESNM10                                                           
         IC    R0,ENMLN                                                         
         AR    R2,R0                                                            
         B     ESNM08                                                           
*                                                                               
ESNM10   XR    RF,RF                                                            
         IC    RF,ENMLN                                                         
         SHI   RF,ENMLNQ+1                                                      
         MVC   TD#ESTNM(0),ENMNAME                                              
         EX    RF,*-6                                                           
         DROP  R2                                                               
ESNM18   MVC   TD#EST#2,TD#EST#                                                 
         MVC   TD#EST#L,ESTKLNO                                                 
         MVC   TD#EST#,FFS                                                      
         GOTOR (#MTSAR,AMTSAR),DMCB,TSAADD,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    ESNM04              get next record in old buffer                
         DC    H'0'                                                             
*                                                                               
ESNM20   J     EXITY                                                            
         DROP  R4                                                               
         LTORG                                                                  
***********************************************************************         
* GET OFFICE DEPT AND SUB-DEPARTMENT NAME                             *         
***********************************************************************         
         SPACE 1                                                                
OFDPNAME NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*OFDPNM*'                                                    
         USING TD#TABD,R4                                                       
         MVC   X#OFFNA(X#OFFLN),SPACES                                          
         XC    X#LMPIN(X#LML),X#LMPIN                                           
         MVC   X#ULAC,SPACES                                                    
OFDP08   L     R4,AIO1                                                          
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO1   clear AIO1                           
         LA    R1,DMCB                                                          
         XC    0(12,R1),0(R1)                                                   
         CLI   TS#PTYPE,TS#PTRHP                                                
         BE    OFDP10                                                           
         CLI   TS#PTYPE,TS#PTRHC                                                
         BE    OFDP10                                                           
         CLI   TS#PTYPE,TS#PTRHO                                                
         BNE   OFDP12                                                           
OFDP10   MVC   TD#ULA,FFS                                                       
         MVC   TD#WRKC,FFS                                                      
OFDP12   GOTOR (#MTSAR,AMTSAR),DMCB,TSARDH,TSARTBUF,TSARTDL                     
         TM    TSARERRS,TSEEOF     is it the end of the buffer                  
         BO    OFDP28              YES                                          
         B     OFDP16                                                           
*                                                                               
OFDP14   GOTOR (#MTSAR,AMTSAR),DMCB,TSANXT,TSARTBUF,TSARTDL                     
         TM    TSARERRS,TSEEOF     is it the end of the buffer                  
         BO    OFDP28              YES                                          
*                                                                               
OFDP16   MVC   TEMP2(L'ACTKULA),SPACES                                          
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),=C'1R'                                
         SR    RF,RF                                                            
         IC    RF,ONERL1L                                                       
         SHI   RF,1                                                             
         MVC   L'ACTKUNT+L'ACTKLDG+TEMP2(0),TD#OFFC                             
         EX    RF,*-6                                                           
         LA    RE,TEMP2+L'ACTKUNT+L'ACTKLDG                                     
         AHI   RF,1                                                             
         AR    RE,RF                                                            
         LR    R0,RF                                                            
         IC    RF,ONERL2L                                                       
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         MVC   0(0,RE),TD#DEPT                                                  
         EX    RF,*-6                                                           
         LA    RE,1(RF,RE)                                                      
         IC    R0,ONERL2L                                                       
         IC    RF,ONERL3L                                                       
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         MVC   0(0,RE),TD#SBDP                                                  
         EX    RF,*-6                                                           
         LA    RE,1(RF,RE)                                                      
         IC    R0,ONERL3L                                                       
         IC    RF,ONERL4L                                                       
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         MVC   0(0,RE),TD#PERC                                                  
         EX    RF,*-6                                                           
         CLC   X#ULAC,TEMP2                                                     
         BE    OFDP20                                                           
         CLI   TS#PTYPE,TS#PTLAS                                                
         BE    OFDP17                                                           
         GOTOR GETMAP,TEMP2                                                     
         MVC   TD#LMPIN,FULL2                                                   
OFDP17   GOTOR GETNAME,DMCB,TEMP2                                               
         BNE   OFDP18                                                           
         MVC   TD#OFFNM,X#OFFNA                                                 
         MVC   TD#DEPNM,X#DPTNA                                                 
         MVC   TD#SDPNM,X#SDPNA                                                 
OFDP18   CLI   TS#PTYPE,TS#PTLAS                                                
         BE    OFDP26                                                           
         CLC   TD#LMPIN,X#LMPIN                                                 
         BE    OFDP22                                                           
         MVC   TEMP2,TD#LMPIN                                                   
         MVC   X#LMPIN,TD#LMPIN                                                 
         L     R0,AIO3             copy IO1 record to IO3                       
         LA    R1,IOLENQ                                                        
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTOR (#GETPID,AGETPID)                                                
         BNE   OFDP24                                                           
         MVC   X#LMPID,TEMP2                                                    
         GOTOR (#GETPIN,AGETPIN)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   X#LMFNM,TEMP2                                                    
         MVC   X#LMMNM,TEMP2+32                                                 
         MVC   X#LMSNM,WORK2                                                    
         MVC   X#LMEML,APPEMAIL                                                 
         L     R0,AIO1             copy IO3 record to IO1                       
         LA    R1,IOLENQ                                                        
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     OFDP22                                                           
OFDP20   MVC   TD#OFFNM(X#OFFLN),X#OFFNA                                        
OFDP22   MVC   TD#LMPIN(X#LML),X#LMPIN                                          
         B     OFDP26                                                           
                                                                                
OFDP24   L     R0,AIO1             copy IO3 record to IO1                       
         LA    R1,IOLENQ                                                        
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
OFDP26   GOTOR (#MTSAR,AMTSAR),DMCB,TSAWRT,TSARTBUF,TSARTDL                     
         CLI   TSARERRS,0                                                       
         BE    OFDP14              get next record in old buffer                
         DC    H'0'                                                             
*                                                                               
OFDP28   J     EXITY                                                            
         DROP  R4                                                               
         LTORG                                                                  
***********************************************************************         
* GET NAME FOR TIME ACCOUNT PARM1 = ACCOUNT CODE INCLUDES UNIT LEDGER *         
***********************************************************************         
         SPACE 1                                                                
GETNAME  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*GETNAM*'                                                    
         L     R2,0(R1)                                                         
         CLC   X#ULAC(L'ACTKULA),0(R2)                                          
         BE    GETNAMEY                                                         
         CLC   PRODUL,0(R2)                                                     
         BNE   GETNM50                                                          
         SR    RF,RF                                                            
         IC    RF,PCLILEN                                                       
         AHI   RF,1                                                             
         EX    RF,GETNM02                                                       
         BE    GETNM10                                                          
         B     GETNM04                                                          
*                                                                               
GETNM02  CLC   X#ULAC(0),0(R2)                                                  
*                                                                               
GETNM04  MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(0),0(R2)                                                   
         EX    RF,*-6                                                           
         MVC   X#CLINA,SPACES                                                   
         MVC   X#PRONA,SPACES                                                   
         MVC   X#JOBNA,SPACES                                                   
         MVC   X#JOBLK,SPACES                                                   
         MVC   X#JOBCL,SPACES                                                   
         GOTOR (#GETACN,AGETACN)                                                
         MVC   X#CLINA,TEMP2                                                    
*                                                                               
GETNM10  SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         SR    RF,RE                                                            
         AR    RE,R2                                                            
         SHI   RF,1                                                             
         EX    RF,GETNM12                                                       
         BE    GETNM40                                                          
         B     GETNM14                                                          
*                                                                               
GETNM12  CLC   SPACES(0),L'ACTKUNT+L'ACTKLDG(RE)                                
*                                                                               
GETNM14  SR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SHI   RF,1                                                             
         EX    RF,GETNM16                                                       
         BE    GETNM24                                                          
         B     GETNM18                                                          
*                                                                               
GETNM16  CLC   X#ULAC+L'ACTKUNT+L'ACTKLDG(0),L'ACTKLDG+L'ACTKUNT(R2)            
*                                                                               
GETNM18  MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'PRODUL),PRODUL                                           
         MVC   TEMP2+L'PRODUL(0),L'ACTKUNT+L'ACTKLDG(R2)                        
         EX    RF,*-6                                                           
         MVC   X#PRONA,SPACES                                                   
         MVC   X#JOBNA,SPACES                                                   
         GOTOR (#GETACN,AGETACN)                                                
         MVC   X#PRONA,TEMP2                                                    
*                                                                               
GETNM24  SR    RF,RF                                                            
         IC    RF,PJOBLEN                                                       
         SR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         SR    RF,RE                                                            
         AR    RE,R2                                                            
         SHI   RF,1                                                             
         EX    RF,GETNM26                                                       
         BE    GETNM40                                                          
         B     GETNM28                                                          
*                                                                               
GETNM26  CLC   SPACES(0),L'ACTKUNT+L'ACTKLDG(RE)                                
*                                                                               
GETNM28  SR    RF,RF                                                            
         IC    RF,PJOBLEN                                                       
         SHI   RF,1                                                             
         EX    RF,GETNM30                                                       
         BE    GETNM40                                                          
         B     GETNM32                                                          
*                                                                               
GETNM30  CLC   X#ULAC+L'ACTKUNT+L'ACTKLDG(0),L'ACTKLDG+L'ACTKUNT(R2)            
*                                                                               
GETNM32  MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'PRODUL),PRODUL                                           
         MVC   TEMP2+L'PRODUL(0),L'ACTKUNT+L'ACTKLDG(R2)                        
         EX    RF,*-6                                                           
         MVC   X#JOBNA,SPACES                                                   
         GOTOR (#GETACN,AGETACN)                                                
         MVC   X#JOBNA,TEMP2                                                    
         USING ACTRECD,R3                                                       
         L     R3,AIO3                                                          
         MVI   X#JOBLK,NOQ                                                      
         TM    ACTRSTAT,ACTSLOCK                                                
         JZ    *+8                                                              
         MVI   X#JOBLK,YESQ                                                     
         MVI   X#JOBCL,NOQ                                                      
         TM    ACTRSTAT,ACTSCLOS                                                
         JZ    GETNM40                                                          
         MVI   X#JOBCL,YESQ                                                     
*                                                                               
GETNM40  MVC   X#ULAC,0(R2)                                                     
         B     GETNAMEY                                                         
*                                                                               
GETNM50  CLC   =C'1R',0(R2)                                                     
         BNE   GETNM90                                                          
         SR    RF,RF                                                            
         IC    RF,ONERL1L                                                       
         AHI   RF,1                                                             
         EX    RF,GETNM52                                                       
         BE    GETNM60                                                          
         B     GETNM54                                                          
*                                                                               
GETNM52  CLC   X#ULAC(0),0(R2)                                                  
*                                                                               
GETNM54  LA    R3,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(0),0(R2)                                                 
         EX    RF,*-6                                                           
         MVC   X#OFFNA,SPACES                                                   
         MVC   X#DPTNA,SPACES                                                   
         MVC   X#SDPNA,SPACES                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BNE   GETNAMEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3                                                          
         LA    R4,ACTRFST                                                       
         USING NAMELD,R4                                                        
         XR    R0,R0                                                            
GETNM56  CLI   NAMEL,NAMELQ                                                     
         BE    GETNM58                                                          
         CLI   NAMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,NAMLN                                                         
         AR    R4,R0                                                            
         B     GETNM56                                                          
                                                                                
GETNM58  XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         MVC   X#OFFNA(0),NAMEREC                                               
         EX    RE,*-6                                                           
*                                                                               
GETNM60  SR    RF,RF                                                            
         IC    RF,ONERL2L                                                       
         SHI   RF,1                                                             
         EX    RF,GETNM62                                                       
         BE    GETNM70                                                          
         B     GETNM64                                                          
*                                                                               
GETNM62  CLC   X#ULAC+L'ACTKUNT+L'ACTKLDG(0),L'ACTKLDG+L'ACTKUNT(R2)            
*                                                                               
         USING ACTRECD,R3                                                       
GETNM64  LA    R3,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'PRODUL),=C'1R'                                         
         MVC   ACTKACT(0),L'ACTKUNT+L'ACTKLDG(R2)                               
         EX    RF,*-6                                                           
         MVC   X#DPTNA,SPACES                                                   
         MVC   X#SDPNA,SPACES                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BNE   GETNAMEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3                                                          
         LA    R4,ACTRFST                                                       
         USING NAMELD,R4                                                        
         XR    R0,R0                                                            
GETNM66  CLI   NAMEL,NAMELQ                                                     
         BE    GETNM68                                                          
         CLI   NAMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,NAMLN                                                         
         AR    R4,R0                                                            
         B     GETNM66                                                          
                                                                                
GETNM68  XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         MVC   X#DPTNA(0),NAMEREC                                               
         EX    RE,*-6                                                           
*                                                                               
GETNM70  SR    RF,RF                                                            
         IC    RF,ONERL3L                                                       
         SHI   RF,1                                                             
         EX    RF,GETNM72                                                       
         BE    GETNM80                                                          
         B     GETNM74                                                          
*                                                                               
GETNM72  CLC   X#ULAC+L'ACTKUNT+L'ACTKLDG(0),L'ACTKLDG+L'ACTKUNT(R2)            
*                                                                               
         USING ACTRECD,R3                                                       
GETNM74  LA    R3,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'PRODUL),=C'1R'                                         
         MVC   ACTKACT(0),L'ACTKUNT+L'ACTKLDG(R2)                               
         EX    RF,*-6                                                           
         MVC   X#SDPNA,SPACES                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BNE   GETNAMEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3                                                          
         LA    R4,ACTRFST                                                       
         USING NAMELD,R4                                                        
         XR    R0,R0                                                            
GETNM76  CLI   NAMEL,NAMELQ                                                     
         BE    GETNM78                                                          
         CLI   NAMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,NAMLN                                                         
         AR    R4,R0                                                            
         B     GETNM76                                                          
                                                                                
GETNM78  XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         MVC   X#SDPNA(0),NAMEREC                                               
         EX    RE,*-6                                                           
*                                                                               
GETNM80  MVC   X#ULAC,0(R2)                                                     
         B     GETNAMEY                                                         
                                                                                
GETNM90  CLC   ONENUL,0(R2)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         LA    RF,L'ACTKACT                                                     
         AHI   RF,1                                                             
         EX    RF,GETNM92                                                       
         BE    GETNM100                                                         
         B     GETNM94                                                          
*                                                                               
GETNM92  CLC   X#ULAC(0),0(R2)                                                  
*                                                                               
         USING ACTRECD,R3                                                       
GETNM94  LA    R3,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(0),0(R2)                                                 
         EX    RF,*-6                                                           
         MVC   X#CLINA,SPACES                                                   
         MVC   X#PRONA,SPACES                                                   
         MVC   X#JOBNA,SPACES                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BNE   GETNAMEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3                                                          
         LA    R4,ACTRFST                                                       
         USING NAMELD,R4                                                        
         XR    R0,R0                                                            
GETNM96  CLI   NAMEL,NAMELQ                                                     
         BE    GETNM98                                                          
         CLI   NAMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,NAMLN                                                         
         AR    R4,R0                                                            
         B     GETNM96                                                          
*                                                                               
GETNM98  XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,3                                                             
         MVC   X#CLINA(0),NAMEREC                                               
         EX    RE,*-6                                                           
*                                                                               
GETNM100 MVC   X#ULAC,0(R2)                                                     
         B     GETNAMEY                                                         
*                                                                               
GETNAMEY J     EXITY                                                            
*                                                                               
GETNAMEN J     EXITN                                                            
         DROP  R3,R4                                                            
         LTORG                                                                  
***********************************************************************         
* Get approver and back-up approver PID for time                      *         
* ENTRY R1 IS 1R U/L/ACCOUNT OF APPROVEE                                        
* EXIT  FULL2 IS PID NUMBER (HEX)                                               
***********************************************************************         
                                                                                
GETMAP   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*GETMAP*'                                                      
                                                                                
         LR    R2,R1                                                            
P        USING ACTKULA,R2                                                       
         USING TD#TABD,R4                                                       
         L     R4,AIO1                                                          
                                                                                
         XC    FULL2,FULL2                                                      
MN       USING DPAPASD,IOKEY                                                    
         XC    MN.DPAPAS,MN.DPAPAS                                              
         MVI   MN.DPAPTYP,DPAPTYPQ                                              
         MVI   MN.DPAPSUB,DPAPSUBQ                                              
         MVI   MN.DPAPAPPL,DPAPATIM                                             
         MVC   MN.DPAPCPY,CUXCPY                                                
         ZAP   MN.DPAPXVAL,PZERO                                                
         LA    R3,ONERL4L          R3=A(Person length)                          
         LHI   R0,4                R0=Number of levels to search                
                                                                                
GETMAP02 MVC   MN.DPAP1RAC,SPACES                                               
         LLC   RF,0(R3)                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   MN.DPAP1RAC(0),P.ACTKACT                                         
         EX    RF,0(RE)                                                         
         MVC   CSVKEY2,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    GETMAP06                                                         
         DC    H'0'                                                             
                                                                                
GETMAP06 CLC   MN.DPAPAS(DPAPPIDB-DPAPASD),CSVKEY2                              
         JNE   GETMAP08                                                         
         MVC   FULL2(L'DPAPPIDB),MN.DPAPPIDB                                    
         J     GETMAP10                                                         
                                                                                
GETMAP08 MVC   MN.DPAPAS,CSVKEY2   Reset key                                    
         SHI   R3,L'ONERL4L        Point to previous key length                 
         JCT   R0,GETMAP02         Do for number of 1R levels                   
         J     GETMAPN                                                          
         DROP  MN,P,R4                                                          
                                                                                
GETMAP10 J     EXITY                                                            
                                                                                
GETMAPN  MVC   LP_ERROR,=AL2(AE$INAPP)                                          
         J     EXITN                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET AUDIT DETAILS                                                   *         
* ON NTRY PARM1 BYTE 1 TYPE OF QUERY 1=GET INDX 2=GET APPROVER TIMELS *         
*         PARM1 BYTE 1-3 ADDRESS OF DATE                              *         
* ON EXIT DATA IN ELEAREA                                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
GETAUD   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*GTAUD**'                                                    
         L     R4,AELEAREA                                                      
         ST    R4,TS#AGPT                                                       
         MVI   0(R4),X'FF'                                                      
         MVC   BYTE1,0(R1)                                                      
         L     R2,0(R1)                                                         
         ICM   R1,7,0(R2)                                                       
         LNR   R1,R1                                                            
         STCM  R1,7,TS#DATE                                                     
         XC    TS#INDX,TS#INDX                                                  
AUD      USING AUDRECD,IOKEY                                                    
         XC    AUD.AUDKEY,AUD.AUDKEY                                            
         MVI   AUD.AUDKTYP,AUDKTYPQ                                             
         MVI   AUD.AUDKSUB,AUDKSUBQ                                             
         MVI   AUD.AUDKAUDT,AUDKTIME                                            
         MVC   AUD.AUDKCPY,CUXCPY                                               
         MVC   AUD.AUDKPEDT,TS#DATE                                             
         MVC   AUD.AUDKPIDB,TS#PSPID                                            
         MVC   CSVKEY2,AUD.AUDKEY                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         B     GTAUD15                                                          
GTAUD10  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
GTAUD15  CLC   CSVKEY2(AUDKSEQ-AUDKEY),AUD.AUDKEY DO ANY RECORDS EXIST          
         JNE   EXITY               NO                                           
         MVC   TS#INDX,AUD.AUDKINDX                                             
         MVC   TS#STAT,AUD.AUDKSTAT                                             
         CLI   BYTE1,1                                                          
         JE    EXITY                                                            
                                                                                
GTAUD20  MVC   CSVKEY2,AUD.AUDKEY                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  AUD                                                              
                                                                                
         L     R3,AIO4                                                          
         USING AUDRECD,R3                                                       
GTAUD25  LA    R2,AUDRFST                                                       
         USING STCELD,R2                                                        
         L     R4,TS#AGPT                                                       
GTAUD30  CLI   STCEL,0                                                          
         BE    GTAUD40                                                          
         SR    RE,RE                                                            
         IC    RE,STCLN                                                         
         CLI   BYTE1,2                                                          
         BNE   GTAUD32                                                          
         USING TIMELD,R2                                                        
         CLI   TIMEL,TIMELQ                                                     
         BNE   GTAUD38                                                          
         CLI   TIMETYP,TIMEARIN                                                 
         BNE   GTAUD38                                                          
*                                                                               
         USING STCELD,R2                                                        
GTAUD32  SHI   RE,1                                                             
         MVC   0(0,R4),STCEL                                                    
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         AR    R4,RE               R4=A(NEXT AREA IN ELEAREA)                   
                                                                                
GTAUD38  AR    R2,RE               R2=A(NEXT STCLE ELEMENT                      
         B     GTAUD30                                                          
*                                                                               
GTAUD40  ST    R4,TS#AGPT                                                       
         MVI   0(R4),X'FF'                                                      
         B     GTAUD10                                                          
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD OVERRIDE OF ACCOUNTS IN GAPTABLE FOR IO TIME READS            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
BLDOVR   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*BLDOVR*'                                                    
                                                                                
         LA    R2,GAPAREA                                                       
         XC    GAPAREA,GAPAREA                                                  
         CLI   TS#PTYPE,TS#PTSMC   client detail sum monthly                    
         BE    BOVR00                                                           
         CLI   TS#PTYPE,TS#PTSWC   client detail sum weekly                     
         BE    BOVR00                                                           
         CLI   TS#PTYPE,TS#PTRHC   client search                                
         BNE   BOVR30                                                           
         USING GAPTABD,R2                                                       
BOVR00   MVI   GAPTDAT1,GAPTT3Q                                                 
         MVI   GAPTSTA,GAPTSMQ                                                  
         MVI   GAPTLVL,GAPTSL1                                                  
         MVI   GAPTLEN,L'ACTKACT                                                
         MVC   GAPTACT,ANYACCD     PUT FULL ACCOUNT TO TABLE                    
         MVI   GAPTAPPL,TSJP1NAQ                                                
         CLC   PRODUL,ANYACCNT                                                  
         BNE   BOVR10                                                           
         MVI   GAPTAPPL,TSJPSJAQ                                                
         MVC   GAPTACC,ANYACCD     PUT FULL ACCOUNT TO TABLE                    
         MVI   GAPTDAT1,GAPTT2Q                                                 
         MVC   GAPTCOFF,TS#SJOFF                                                
         LLC   RF,PCLILEN                                                       
         AHI   RF,L'TS#SJOFF                                                    
         STC   RF,GAPTLEN                                                       
         MVI   GAPTLVL,GAPTSL2                                                  
         TM    TS#ICPJ,TS#IJOB                                                  
         BZ    BOVR04                                                           
         MVI   GAPTLVL,GAPTSL4                                                  
         LLC   RF,PJOBLEN                                                       
         AHI   RF,L'TS#SJOFF                                                    
         STC   RF,GAPTLEN                                                       
         B     BOVR10                                                           
                                                                                
BOVR04   TM    TS#ICPJ,TS#IPRD                                                  
         BZ    BOVR10                                                           
         LLC   RF,PPROLEN                                                       
         AHI   RF,L'TS#SJOFF                                                    
         STC   RF,GAPTLEN                                                       
         MVI   GAPTLVL,GAPTSL3                                                  
         B     BOVR10                                                           
                                                                                
BOVR10   GOTOR (#GOATSR,AGOATSR),DMCB,('TSAADD',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TS#PTYPE,TS#PTSMC   client detail sum monthly                    
         BE    BOVR20                                                           
         CLI   TS#PTYPE,TS#PTSWC   client detail sum weekly                     
         BNE   BOVRYES                                                          
*                                                                               
BOVR20   GOTOR BPROOFF             ADD PRODUCT OFFICES (IF DIFFERENT)           
         B     BOVRYES                                                          
*                                                                               
* person searches                                                               
*                                                                               
BOVR30   CLI   TS#PTYPE,TS#PTRTO                                                
         BE    BOVR35                                                           
         CLI   TS#PTYPE,TS#PTRHO                                                
         BNE   BOVRYES                                                          
         USING GAPTABD,R2                                                       
BOVR35   MVI   GAPTDAT1,GAPTT1Q                                                 
         MVI   GAPTSTA,GAPTSMQ                                                  
         MVI   GAPTLVL,GAPTSL1                                                  
         MVC   GAPTACT,X#1RACT                                                  
         TM    TS#ICPJ,TS#ISDPR                                                 
         BZ    BOVR40                                                           
         MVI   GAPTLVL,GAPTSL3     SET LEVEL 3                                  
         MVC   GAPTLEN,ONERL3L                                                  
         B     BOVR50                                                           
                                                                                
BOVR40   TM    TS#ICPJ,TS#IDPTR                                                 
         BZ    BOVR45                                                           
         MVI   GAPTLVL,GAPTSL2     SET LEVEL 2                                  
         MVC   GAPTLEN,ONERL2L                                                  
         B     BOVR50                                                           
                                                                                
BOVR45   TM    TS#ICPJ,TS#IOFFR                                                 
         BZ    BOVR50                                                           
         MVI   GAPTLVL,GAPTSL1     SET LEVEL 1                                  
         MVC   GAPTLEN,ONERL1L                                                  
*                                                                               
BOVR50   GOTOR (#GOATSR,AGOATSR),DMCB,('TSAADD',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         BE    BOVRYES                                                          
         DC    H'0'                                                             
*                                                                               
BOVRYES  J     EXITY                                                            
*                                                                               
BOVRNO   J     EXITN                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD OVERRIDE OF ACCOUNTS IN GAPTABLE FOR IO TIME READS            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
BPROOFF  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*BPROOFF'                                                    
*                                                                               
*  READ CLIENT RECORD                                                           
         MVC   IOKEY,SPACES                                                     
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,ANYACCNT                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
*  GET LIDEL                                                                    
         L     R2,AIO3                                                          
         LA    R1,ACTRFST                                                       
         SR    RE,RE                                                            
         DROP  R2                                                               
BPOFF20  DS    0H                                                               
         USING LIDELD,R1                                                        
         CLI   LIDEL,0                                                          
         JE    BPOFFX                                                           
         CLI   LIDEL,LIDELQ                                                     
         JNE   BPOFF25                                                          
         CLI   LIDTYPE,LIDTPOFC                                                 
         JE    BPOFF30                                                          
BPOFF25  IC    RE,LIDLN                                                         
         AR    R1,RE                                                            
         J     BPOFF20                                                          
*  ADD ENTRIES PER LIDEL ENTRY (WHERE NOT EQ CLI OFF)                           
BPOFF30  LA    R2,LIDDATA                                                       
         LLC   R3,LIDLN                                                         
         SHI   R3,LIDLNDQ          L'OFFICE LIST                                
BPOFF40  DS    0H                  LOOP OVER LIST                               
         CLC   TS#SJOFF,0(R2)                                                   
         JNE   BPOFF45                                                          
BPOFF44  AHI   R2,2                                                             
         SHI   R3,1                                                             
         JCT   R3,BPOFF40                                                       
         J     BPOFFX              DONE                                         
*                                                                               
BPOFF45  DS    0H                  ADD GAPLST ENTRY FOR PRODUCT OFFICE          
         LA    R4,GAPAREA                                                       
         USING GAPTABD,R4                                                       
         MVC   GAPTACT(L'TS#SJOFF),0(R2)                                        
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAADD',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         BE    BPOFF44                                                          
         DC    H'0'                                                             
*                                                                               
BPOFFX   J     EXITY                                                            
         DROP  R4,R1                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK MCS TIMESHEET USER                                            *         
* ON NTRY PARM1 BYTE 1-3 ADDRESS OF 1R ACCOUNT                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
CHKMCS   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*CHKMCS*'                                                    
         L     R4,ABRATAB                                                       
         XC    0(BRATABX-BRATAB,R4),0(R4)                                       
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),=C'1R'                                
         GOTOR (#GETACN,AGETACN)                                                
         GOTOR CHKE5EL                                                          
         LA    R2,ONERL1L                                                       
         LA    R3,4                                                             
CKMCS02  XR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         SHI   RE,1                                                             
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'ACTKUNT+L'ACTKLDG),=C'1R'                                
         MVC   TEMP2+L'ACTKUNT+L'ACTKLDG(0),TS#1RACT                            
         EX    RE,*-6                                                           
         GOTOR (#GETACN,AGETACN)                                                
         GOTOR CHKE5EL                                                          
         AHI   R2,1                                                             
         BCT   R3,CKMCS02                                                       
         J     EXITY                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK E5 ELEMENT                                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
CHKE5EL  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*CHKE5L*'                                                    
         L     R3,AIO3                                                          
         USING ACTRECD,R3                                                       
         LA    R3,ACTRFST                                                       
         USING GDAELD,R3                                                        
         SR    R0,R0                                                            
CKE5L002 CLI   GDAEL,0                                                          
         BE    CKE5L030                                                         
         CLI   GDAEL,GDAELQ                                                     
         BE    CKE5L006                                                         
CKE5L004 IC    R0,GDALN                                                         
         AR    R3,R0                                                            
         B     CKE5L002                                                         
*                                                                               
CKE5L006 CLI   GDATYPE,GDATMCST                                                 
         BNE   CKE5L004                                                         
*                                                                               
CKE5L012 MVC   0(L'GDADATE,R4),GDADATE                                          
         MVC   L'GDADATE(L'GDADATE2,R4),GDADATE2                                
         LA    R4,L'GDADATE2+L'GDADATE(R4)                                      
         B     CKE5L004                                                         
*                                                                               
CKE5L030 XIT1  REGS=(R4)                                                        
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET EDIT HOURS AMOUNT FOR TIMESHEET PERIOD                          *         
* AMOUNT OF HOURS IN TS#EDHRS                                         *         
***********************************************************************         
         SPACE 1                                                                
                                                                                
CHKPHRS  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*CKPHRS*'                                                    
         LA    R4,IOKEY                                                         
         ZAP   TS#EDHRS,PZERO                                                   
         ZAP   X#TUPCT,PZERO                                                    
         OI    TS#IEDT,TS#IOFF+TS#IDPT+TS#ISDP+TS#IPER                          
         USING EDTRECD,R4                                                       
CKPHR10  MVC   EDTKEY,SPACES                                                    
         MVI   EDTKTYP,EDTKTYPQ                                                 
         MVI   EDTKSUB,EDTKSUBQ                                                 
         MVC   EDTKCPY,CUXCPY                                                   
         TM    TS#IEDT,TS#IOFF                                                  
         BZ    *+14                                                             
         MVC   EDTKOFC,TS#1ROFF                                                 
         NI    TS#IEDT,X'FF'-TS#IOFF                                            
         TM    TS#IEDT,TS#IDPT                                                  
         BZ    *+18                                                             
         MVC   EDTKDPT(L'TS#1RDPT),TS#1RDPT                                     
         NI    TS#IEDT,X'FF'-TS#IDPT                                            
         OI    TS#IEDT,TS#IOFF                                                  
         TM    TS#IEDT,TS#ISDP                                                  
         BZ    *+18                                                             
         MVC   EDTKSBD(L'TS#1RSDP),TS#1RSDP                                     
         NI    TS#IEDT,X'FF'-TS#ISDP                                            
         OI    TS#IEDT,TS#IDPT                                                  
         TM    TS#IEDT,TS#IPER                                                  
         BZ    *+18                                                             
         MVC   EDTKPER,TS#PRSAC                                                 
         NI    TS#IEDT,X'FF'-TS#IPER                                            
         OI    TS#IEDT,TS#ISDP                                                  
         XC    EDTKSEQ,EDTKSEQ                                                  
         XR    RE,RE                                                            
         IC    RE,SCPYEL+CPYSFST-CPYELD                                         
         CHI   RE,X'F0'                                                         
         BH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
         MVC   TS#CDAT,TS#PEDT                                                  
         MVC   TS#CDAT+1(1),BYTE1                                               
         CLC   TS#PEDT+1(1),BYTE1                                               
         BNL   CKPHR20                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CDAT)                              
*                                                                               
CKPHR20  MVC   EDTKYR,TS#CDAT                                                   
         MVI   EDTKKSTA,EDTKSPER   Set to read periods                          
         MVC   CSVKEY2,EDTKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    CKPHR30                                                          
         MVC   IOKEY,CSVKEY2                                                    
         CLC   EDTKOFC,SPACES      have we read lowest level                    
         BE    CKPHR80             yes - exit                                   
         B     CKPHR10                                                          
*                                                                               
CKPHR30  CLC   EDTKSTDT,TS#EFSTA                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         CLC   EDTKENDT,TS#EFSTA   if date earlier we need to look at           
         BNL   CKPHR40                     next year edit hour rec              
                                                                                
         GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'+1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CDAT)                              
         B     CKPHR20                                                          
*                                                                               
CKPHR40  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO3                                                          
         LA    R4,EDTRFST                                                       
         USING SHRELD,R4                                                        
CKPHR50  CLI   SHREL,0                                                          
         BE    CKPHR80                                                          
         CLI   SHREL,SHRELQ                                                     
         BE    CKPHR70                                                          
*                                                                               
CKPHR60  SR    R0,R0                                                            
         IC    R0,SHRLN                                                         
         AR    R4,R0                                                            
         B     CKPHR50                                                          
*                                                                               
CKPHR70  DS    0H                                                               
         CLC   TS#EFSTA,SHREND                                                  
         BH    CKPHR60                                                          
         CLC   TS#EFSTA,SHRSTART                                                
         BL    CKPHR60                                                          
         ZAP   TS#EDHRS,SHRHOURS                                                
*                                                                               
CKPHR80  J     EXITY                                                            
         DROP  R4                                                               
         LTORG                                                                  
                                                                                
         EJECT                                                                  
***********************************************************************         
* GET EDIT HOURS AMOUNT FOR TIMESHEET PERIOD                          *         
* AMOUNT OF HOURS IN TS#EDHRS                                         *         
***********************************************************************         
         SPACE 1                                                                
                                                                                
CHKHRS   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*CKHRS**'                                                    
         LA    R4,IOKEY                                                         
         ZAP   TS#EDHRS,PZERO                                                   
         ZAP   X#TUPCT,PZERO                                                    
         OI    TS#IEDT,TS#IOFF+TS#IDPT+TS#ISDP+TS#IPER                          
         MVI   BYTE2,EDTKSDAY   Set to read daily time                          
         USING EDTRECD,R4                                                       
CKHRS02  MVC   EDTKEY,SPACES                                                    
         MVI   EDTKTYP,EDTKTYPQ                                                 
         MVI   EDTKSUB,EDTKSUBQ                                                 
         MVC   EDTKCPY,CUXCPY                                                   
         TM    TS#IEDT,TS#IOFF                                                  
         BZ    *+14                                                             
         MVC   EDTKOFC,TS#1ROFF                                                 
         NI    TS#IEDT,X'FF'-TS#IOFF                                            
         TM    TS#IEDT,TS#IDPT                                                  
         BZ    *+18                                                             
         MVC   EDTKDPT(L'TS#1RDPT),TS#1RDPT                                     
         NI    TS#IEDT,X'FF'-TS#IDPT                                            
         OI    TS#IEDT,TS#IOFF                                                  
         TM    TS#IEDT,TS#ISDP                                                  
         BZ    *+18                                                             
         MVC   EDTKSBD(L'TS#1RSDP),TS#1RSDP                                     
         NI    TS#IEDT,X'FF'-TS#ISDP                                            
         OI    TS#IEDT,TS#IDPT                                                  
         TM    TS#IEDT,TS#IPER                                                  
         BZ    *+18                                                             
         MVC   EDTKPER,TS#PRSAC                                                 
         NI    TS#IEDT,X'FF'-TS#IPER                                            
         OI    TS#IEDT,TS#ISDP                                                  
         XC    EDTKSEQ,EDTKSEQ                                                  
         XR    RE,RE                                                            
         ICM   RE,1,SCPYEL+CPYSFST-CPYELD                                       
         JNZ   *+8                 Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
         CHI   RE,X'F0'                                                         
         BH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
         MVC   TS#CDAT,TS#PEDT                                                  
         MVC   TS#CDAT+1(1),BYTE1                                               
         CLC   TS#PEDT+1(1),BYTE1                                               
         BNL   CKHRS04                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CDAT)                              
*                                                                               
CKHRS04  MVC   EDTKYR,TS#CDAT                                                   
         MVI   EDTKKSTA,EDTKSDAY   set daily time                               
         MVC   CSVKEY2,EDTKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    CKHRS06                                                          
         MVC   IOKEY,CSVKEY2                                                    
         CLC   EDTKOFC,SPACES      have we read lowest level                    
         BE    CKHRS30             yes - exit                                   
         B     CKHRS02                                                          
*                                                                               
CKHRS06  CLC   EDTKSTDT,TS#EFSTA                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         CLC   EDTKENDT,TS#EFSTA   if date earlier we need to look at           
         BNL   CKHRS07                     next year edit hour rec              
                                                                                
         GOTO1 VDATCON,DMCB,(1,TS#CDAT),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),WORK+6,F'+1'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,TS#CDAT)                              
         B     CKHRS04                                                          
*                                                                               
CKHRS07  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   BYTE2,0                                                          
         MVC   FULL1,TS#EFSTA                                                   
CKHRS08  DS    0H                                                               
         ZAP   DUB,PZERO                                                        
         GOTO1 VDATCON,DMCB,(1,FULL1),(0,WORK)                                  
         GOTO1 VGETDAY,DMCB,WORK,DUB1                                           
         CLC   DUB1(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    RF,DAYTAB                                                        
CKHRS10  CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,RF),0(R1)                                                    
         BE    CKHRS18                                                          
         LA    RF,DAYTABL(RF)                                                   
         B     CKHRS10                                                          
*                                                                               
CKHRS18  MVC   BYTE1,1(RF)                                                      
CKHRS20  L     R4,AIO3                                                          
         LA    R4,EDTRFST                                                       
         USING DEDELD,R4                                                        
CKHRS22  CLI   DEDEL,0                                                          
         BE    CKHRS28                                                          
         CLI   DEDEL,DEDELQ                                                     
         BE    CKHRS23                                                          
         CLI   DEDEL,FFRELQ                                                     
         BNE   CKHRS26                                                          
         USING FFRELD,R4                                                        
         CLI   FFRTYPE,FFRTPCT     Is it a percentage?                          
         BNE   CKHRS26                                                          
         ZAP   X#TUPCT,FFRPCT                                                   
         USING DEDELD,R4                                                        
CKHRS23  CLI   DEDLN,DEDLN1Q                                                    
         BNE   CKHRS24                                                          
         CLC   BYTE1,DEDIND                                                     
         BNE   CKHRS26                                                          
         ZAP   DUB,DEDHRS                                                       
         B     CKHRS26                                                          
*                                                                               
CKHRS24  CLC   DEDDATE,FULL1                                                    
         BNE   CKHRS26                                                          
         ZAP   DUB,DEDHRS                                                       
         B     CKHRS28                                                          
*                                                                               
CKHRS26  SR    R0,R0                                                            
         IC    R0,DEDLN                                                         
         AR    R4,R0                                                            
         B     CKHRS22                                                          
*                                                                               
CKHRS28  AP    TS#EDHRS,DUB                                                     
         CLC   TS#EFEND,FULL1      DONE LAST DAY?                               
         BNH   CKHRS30                                                          
         GOTO1 VDATCON,DMCB,(1,FULL1),(0,WORK+6)   FULL+1DAY                    
         GOTO1 VADDAY,DMCB,(C'D',WORK+6),WORK,F'1'                              
         GOTO1 VDATCON,DMCB,(0,WORK),(1,FULL1)                                  
         B     CKHRS08                                                          
*                                                                               
CKHRS30  J     EXITY                                                            
         DROP  R4                                                               
*                                                                               
DAYTAB   DS    0X                                                               
         DC    AL1(1,DEDIMON)                                                   
DAYTABL  EQU   *-DAYTAB                                                         
         DC    AL1(2,DEDITUE)                                                   
         DC    AL1(3,DEDIWED)                                                   
         DC    AL1(4,DEDITHU)                                                   
         DC    AL1(5,DEDIFRI)                                                   
         DC    AL1(6,DEDISAT)                                                   
         DC    AL1(7,DEDISUN)                                                   
         DC    X'FF'                                                            
         LTORG                                                                  
                                                                                
         EJECT                                                                  
***********************************************************************         
* CHECK PREVIOUS PERIOD OF TIME WAS SUBMITTED                         *         
***********************************************************************         
         SPACE 1                                                                
                                                                                
CHKSUB   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*CHKSUB*'                                                    
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         MVI   TS_PRNS,YESQ        Default to previous t/s submitted            
         MVC   TS_COPTA,COPTA      Set profile value                            
         CLI   COPTA,YESQ          Do we need to check                          
         JNE   EXITY                                                            
         J     EXITY                                                            
         MVC   TS#CEDT,TS_ENDD                                                  
         GOTO1 VDATCON,DMCB,(1,TS_ENDD),(0,WORK+6)                              
         GOTO1 VADDAY,DMCB,(C'D',WORK+6),WORK,F'-21'                            
         GOTO1 VDATCON,DMCB,(0,WORK),(1,TS#CSDT)                                
                                                                                
         XR    R1,R1               for dates: 2's complement required           
         GOTOR CALPRDS,0           calendar periods retrieval                   
         JE    *+14                                                             
         MVC   LP_ERROR,FULL2                                                   
         J     XERROR                                                           
                                                                                
         L     R2,AIO5                                                          
         USING PERTABD,R2                                                       
                                                                                
CHKSUB02 CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                (end date not in calendar?)                  
         XR    RE,RE                                                            
         ICM   RE,7,PERENDT                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,X#PEREND                                                    
         CLC   TS#PEDT,X#PEREND                                                 
         JE    CHKSUB04                                                         
         AHI   R2,PERLENQ                                                       
         J     CHKSUB02                                                         
                                                                                
CHKSUB04 MVC   TEMP2(PERLENQ),0(R2)                                             
CHKSUB06 AHI   R2,PERLENQ                                                       
         ST    R2,APREVPER          set previous period for pta profile         
                                                                                
* If this period has edit hours recs, but 0 in period, go further back          
                                                                                
         TM    PERSTAT,PERSMCSU     Brandocean user for this period             
         JZ    CHKSUB12             No, stick with this entry                   
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,7,PERENDT                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,X#PEREND                                                    
         MVC   TS#EFSTA,PERLOCS                                                 
         OC    PERLOCS,PERLOCS                                                  
         JNZ   *+10                                                             
         MVC   TS#EFSTA,PERSTDT                                                 
*                                                                               
         MVC   TS#EFEND,PERLOCE                                                 
         OC    PERLOCE,PERLOCE                                                  
         JNZ   *+10                                                             
         MVC   TS#EFEND,X#PEREND                                                
*                                                                               
         MVC   TS#PEDT,X#PEREND                                                 
*                                                                               
CHKSUB10 GOTOR CHKHRS                                                           
         CLI   BYTE2,0                                                          
         JNE   CHKSUB12             Edit hours not used                         
         CP    TS#EDHRS,PZERO       Does prev period have hours?                
         JNH   CHKSUB06             yep - finish                                
         DROP  R2                                                               
                                                                                
CHKSUB12 L     R4,APREVPER        Check previous period to check it's           
         USING PERTABD,R4         submitted                                     
         TM    PERSTAT,PERSMCSU   Are we a brandocean user for this             
         JZ    EXITY                                        period              
         CLC   PERLOCE,X#HIRED     Is period end date before hire date          
         JL    EXITY               Yes                                          
         XC    IOKEY,IOKEY                                                      
         USING TSWRECD,IOKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CUXCPY                                                   
         MVC   TSWKPER,TS#PRSAC                                                 
         MVC   TSWKEND,PERENDT                                                  
         MVC   TSWKODS,PERODS                                                   
         MVC   CSVKEY1,TSWRECD                                                  
         MVI   TS_PRNS,C'N'                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         CLC   CSVKEY1(TSWKULC-TSWRECD),TSWKEY                                  
         JNE   EXITY              prev timesheet not even started               
         OC    TSWKSTAT,TSWKSTAT  no status means not submitted                 
         JZ    EXITY                                                            
         TM    TSWKSTAT,TIMSREJE  Rejected needs to be resubmitted              
         JNZ   EXITY                                                            
         MVI   TS_PRNS,C'Y'                                                     
         J     EXITY                                                            
         DROP  R4                                                               
***********************************************************************         
* TIMESHEET VALIDATION MODULE: CLI/PRO/JOB ACCOUNT                    *         
* - DATA RECEIVED IN TEMP2                                            *         
* - DATA RETURNED IN ANYACCNT (ERROR IN ROUERRV)                      *         
* - SETS CC                                                           *         
***********************************************************************         
         DS    0H                                                               
VTSCPJ   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*VTSCPJ*'                                                    
         MVC   TS#SJOFF,SPACES                                                  
         MVC   TS#CLINM,SPACES                                                  
         MVC   TS#PRDNM,SPACES                                                  
         MVC   TS#JOBNM,SPACES                                                  
         OC    TEMP2(L'RQ_TTCLI+L'RQ_TTPRO+L'RQ_TTJOB),SPACES                   
         CLC   TEMP2(L'RQ_TTCLI+L'RQ_TTPRO+L'RQ_TTJOB),SPACES                   
         JNH   EXITY                                                            
                                                                                
         XR    R1,R1                                                            
         IC    R1,PCLILEN                                                       
         SHI   R1,1                                                             
         MVC   ANYACCNT+2(0),TEMP2                                              
         EX    R1,*-6                                                           
         LA    R1,ANYACCNT+3(R1)                                                
         XR    RE,RE                                                            
         IC    RE,PCLILEN                                                       
         XR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         MVC   0(0,R1),TEMP2+L'RQ_TTCLI                                         
         EX    RF,*-6                                                           
         LA    R1,1(RF,R1)                                                      
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         XR    RF,RF                                                            
         IC    RF,PJOBLEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         MVC   0(0,R1),TEMP2+L'RQ_TTCLI+L'RQ_TTPRO                              
         EX    RF,*-6                                                           
         OC    ANYACCNT,SPACES                                                  
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(2),PRODUL                                                
         MVC   ANYACCNT(2),PRODUL                                               
         XR    RF,RF                                                            
         IC    RF,PCLILEN                                                       
         SHI   RF,1                                                             
         MVC   ACTKACT(0),ANYACCNT+L'ACTKUNT+L'ACTKLDG                          
         EX    RF,*-6                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    VTSCPJ14                                                         
         MVC   ROUERRV,=AL2(AE$INCLI)                                           
         J     EXITN                                                            
                                                                                
VTSCPJ14 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         LA    R2,ACTRFST                                                       
         OI    TS#ICPJ,TS#ICLI     CLIENT LEVEL                                 
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
VTSCPJ16 CLI   PPREL,0                                                          
         JE    VTSCPJ26                                                         
         CLI   PPREL,PPRELQ                                                     
         BE    VTSCPJ20                                                         
         CLI   PPREL,RSTELQ                                                     
         BE    VTSCPJ22                                                         
         CLI   PPREL,NAMELQ                                                     
         BE    VTSCPJ24                                                         
                                                                                
VTSCPJ18 IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         B     VTSCPJ16                                                         
                                                                                
VTSCPJ20 MVC   TS#SJOFF,PPRGAOFF                                                
         OC    TS#SJOFF,SPACES                                                  
         B     VTSCPJ18                                                         
                                                                                
         USING RSTELD,R2                                                        
VTSCPJ22 CLC   RSTSECY+1(1),CUAUTH+1                                            
         BNH   VTSCPJ18                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
*                                                                               
         USING NAMELD,R2                                                        
VTSCPJ24 CLI   NAMLN,NAMLN1Q                                                    
         BNH   VTSCPJ18                                                         
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         MVC   TS#CLINM(0),NAMEREC                                              
         EX    RE,*-6                                                           
         J     VTSCPJ18                                                         
*                                                                               
VTSCPJ26 DS    0H                                                               
         CLC   TEMP2+L'RQ_TTCLI(L'RQ_TTPRO),SPACES  PROCESS PRODUCT             
         BNH   VTSCPJ68           YES                                           
         USING ACTRECD,R2                                                       
VTSCPJ28 LA    R2,IOKEY                                                         
         XR    RF,RF                                                            
         IC    RF,PPROLEN                                                       
         SHI   RF,1                                                             
         MVC   ACTKACT(0),ANYACCNT+2                                            
         EX    RF,*-6                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    VTSCPJ32                                                         
         MVC   ROUERRV,=AL2(AE$INPRO)                                           
         J     EXITN                                                            
                                                                                
VTSCPJ32 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         OI    TS#ICPJ,TS#IPRD     PRODUCT LEVEL                                
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
VTSCPJ38 CLI   PPREL,0                                                          
         BE    VTSCPJ46                                                         
         CLI   PPREL,PPRELQ                                                     
         BE    VTSCPJ42                                                         
         CLI   PPREL,RSTELQ                                                     
         BE    VTSCPJ44                                                         
         CLI   PPREL,NAMELQ                                                     
         BE    VTSCPJ45                                                         
                                                                                
VTSCPJ40 IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         B     VTSCPJ38                                                         
                                                                                
VTSCPJ42 CLI   PPRGAOFF,X'40'                                                   
         BNH   VTSCPJ40                                                         
         MVC   TS#SJOFF,PPRGAOFF                                                
         OC    TS#SJOFF,SPACES                                                  
         B     VTSCPJ40                                                         
                                                                                
         USING RSTELD,R2                                                        
VTSCPJ44 CLC   RSTSECY+1(1),CUAUTH+1                                            
         BNH   VTSCPJ40                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
*                                                                               
         USING NAMELD,R2                                                        
VTSCPJ45 CLI   NAMLN,NAMLN1Q                                                    
         BNH   VTSCPJ40                                                         
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         MVC   TS#PRDNM(0),NAMEREC                                              
         EX    RE,*-6                                                           
         J     VTSCPJ40                                                         
                                                                                
VTSCPJ46 DS    0H                                                               
         CLC   TEMP2+L'RQ_TTCLI+L'RQ_TTPRO(L'RQ_TTJOB),SPACES                   
         BNH   VTSCPJ68                                                         
         USING ACTRECD,R2                                                       
VTSCPJ48 LA    R2,IOKEY                                                         
         MVC   ACTKACT,ANYACCNT+2                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    VTSCPJ50                                                         
         MVC   ROUERRV,=AL2(AE$INJOB)                                           
         J     EXITN                                                            
                                                                                
VTSCPJ50 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         OI    TS#ICPJ,TS#IJOB     JOB LEVEL                                    
         LA    R2,ACTRFST                                                       
         USING PPRELD,R2                                                        
         XR    R0,R0                                                            
VTSCPJ60 CLI   PPREL,0                                                          
         JE    VTSCPJ68                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    VTSCPJ64                                                         
         CLI   PPREL,RSTELQ                                                     
         JE    VTSCPJ66                                                         
         CLI   PPREL,NAMELQ                                                     
         JE    VTSCPJ67                                                         
                                                                                
VTSCPJ62 IC    R0,PPRLN                                                         
         AR    R2,R0                                                            
         B     VTSCPJ60                                                         
                                                                                
VTSCPJ64 CLI   PPRGAOFF,X'40'                                                   
         BNH   VTSCPJ62                                                         
         MVC   TS#SJOFF,PPRGAOFF                                                
         OC    TS#SJOFF,SPACES                                                  
         B     VTSCPJ62                                                         
                                                                                
         USING RSTELD,R2                                                        
VTSCPJ66 CLC   RSTSECY+1(1),CUAUTH+1                                            
         BNH   VTSCPJ62                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
*                                                                               
         USING NAMELD,R2                                                        
VTSCPJ67 CLI   NAMLN,NAMLN1Q                                                    
         BNH   VTSCPJ62                                                         
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         MVC   TS#JOBNM(0),NAMEREC                                              
         EX    RE,*-6                                                           
         J     VTSCPJ62                                                         
*                                                                               
VTSCPJ68 MVC   X#SJACC,ANYACCNT+2                                               
         CLI   OFFIND,FULLYQ       OFFICE CHECKING                              
         BNE   VTSCPJ98                                                         
         CLC   TS#SJOFF,SPACES                                                  
         BNE   VTSCPJ72                                                         
         MVC   ROUERRV,=AL2(AE$INVPO)                                           
         J     EXITN                                                            
                                                                                
         USING OFFALD,R1                                                        
VTSCPJ72 L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,TS#SJOFF                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              VALIDATE OFFICE                              
         BE    VTSCPJ98                                                         
*                                                                               
         XR    R0,R0                                                            
         L     RF,AIO3                                                          
         AHI   RF,ACTRFST-ACTRECD                                               
*                                                                               
         USING LIDELD,RF                                                        
VTSCPJ74 CLI   LIDEL,0                                                          
         JE    VTSCPJ80                                                         
         CLI   LIDEL,LIDELQ                                                     
         JNE   *+12                                                             
         CLI   LIDTYPE,LIDTPOFC    CHECK FOR PRODUCT OVERRIDE OFFICE            
         JE    VTSCPJ76                                                         
         IC    R0,LIDLN                                                         
         AR    RF,R0                                                            
         B     VTSCPJ74                                                         
*                                                                               
VTSCPJ76 LA    R2,LIDDATA                                                       
         LLC   R3,LIDLN                                                         
         AR    R3,RF               L'OFFICE LIST                                
         DROP  RF                                                               
*                                                                               
VTSCPJ78 L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,0(R2)                                                   
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              VALIDATE OFFICE                              
         BE    VTSCPJ98                                                         
         AHI   R2,L'OFFKOFFC                                                    
         CR    R2,R3                                                            
         JL    VTSCPJ78                                                         
*                                                                               
VTSCPJ80 MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
         DROP  R1,R2                                                            
*                                                                               
VTSCPJ98 DS    0H                                                               
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TIMESHEET VALIDATION MODULE: WORK CODE                              *         
* - DATA RECEIVED IN TEMP2                                            *         
* - DATA RETURNED IN TS#WCD (ERROR IN CL#ERRV)                        *         
* - SETS CC                                                           *         
***********************************************************************         
         DS    0H                                                               
VTSWCD   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*VTSWCD*'                                                    
         OC    TEMP2(2),SPACES                                                  
         MVC   TS#WCD,TEMP2                                                     
                                                                                
VTSWCD02 CLC   TS#WCD,SPACES                                                    
         JE    EXITY                                                            
                                                                                
VTSWCD03 CLI   CUCTRY,CTRYGER      TYPE OF W/C OK?                              
         BNE   VTSWCD04                                                         
                                                                                
         CLI   TS#WCD,C'0'         (disallow external)                          
         BL    VTSWCD04                                                         
                                                                                
         MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     EXITN                                                            
                                                                                
VTSWCD04 CLC   TS#WCD,=C'99'                                                    
         BE    VTSWCD06                                                         
         CLC   TS#WCD,=C'**'                                                    
         BNE   VTSWCD08                                                         
VTSWCD06 MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     EXITN                                                            
         USING WCORECD,R2                                                       
VTSWCD08 LA    R2,IOKEY                                                         
         MVC   WCOKEY,SPACES       BUILD KEY TO READ                            
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUXCPY                                                   
         MVC   WCOKUNT(L'PRODUL),PRODUL                                         
         MVC   WCOKWRK,TS#WCD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    VTSWCD10                                                         
                                                                                
         MVC   ROUERRV,=AL2(AE$WRKNF)                                           
         J     EXITN                                                            
                                                                                
VTSWCD10 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         LA    R2,WCORFST                                                       
         USING WCOELD,R2                                                        
         XR    R0,R0                                                            
                                                                                
VTSWCD12 CLI   WCOEL,WCOELQ                                                     
         BE    VTSWCD16                                                         
         CLI   WCOEL,0                                                          
         BNE   VTSWCD14                                                         
         MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     EXITN                                                            
                                                                                
VTSWCD14 IC    R0,WCOLN                                                         
         AR    R1,R0                                                            
         B     VTSWCD12                                                         
                                                                                
VTSWCD16 TM    WCOSTAT,WCOSHCOE                                                 
*&&UK*&& BO    VTSWCD20                                                         
*&&US*&& B     VTSWCD20                                                         
         MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     EXITN                                                            
                                                                                
VTSWCD20 J     EXITY                                                            
         DROP  RB,R2                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TIMESHEET VALIDATION MODULE: 1N ACCOUNT CODE                        *         
* - DATA RECEIVED IN TEMP                                             *         
* - DATA RETURNED IN ANYACCNT (ON ERROR ROUERRV)                      *         
* - SETS CC                                                           *         
***********************************************************************         
         DS    0H                                                               
VTS1NC   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*VTS1NC*'                                                    
         XC    TS#ICPJ,TS#ICPJ                                                  
         MVC   TS#CLINM,SPACES                                                  
         CLC   TEMP2(L'ACTKACT),SPACES                                          
         JNH   EXITY                                                            
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(2),=CL2'1N'                                              
         MVC   ACTKACT,TEMP2                                                    
         MVC   ANYACCNT,ACTKULA                                                 
         MVC   TEMP2(14),ACTKULA   SAVE ENTERED ACCOUNT                         
         MVC   X#1NACC,ACTKACT                                                  
         MVC   ACTKACT,SPACES                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO3                                                          
         LA    R2,ACTRFST                                                       
         USING LDGELD,R2                                                        
         XR    R0,R0                                                            
         MVI   BYTE1,0                                                          
                                                                                
VTS1NC02 CLI   LDGEL,0                                                          
         BE    VTS1NC10                                                         
         CLI   LDGEL,LDGELQ                                                     
         BE    VTS1NC06                                                         
         CLI   LDGEL,RSTELQ                                                     
         BE    VTS1NC08                                                         
                                                                                
VTS1NC04 IC    R0,LDGLN                                                         
         AR    R2,R0                                                            
         B     VTS1NC02                                                         
                                                                                
VTS1NC06 MVC   BYTE1(1),LDGOPOS                                                 
         B     VTS1NC04                                                         
                                                                                
         USING RSTELD,R2                                                        
VTS1NC08 CLC   RSTSECY+1(1),CUAUTH+1                                            
         BNH   VTS1NC04                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
*                                                                               
*                                                                               
VTS1NC10 CLI   OFFIND,NONEQ        NO OFFICE - NO CHECKING                      
         JE    VTS1NC20                                                         
         CLI   BYTE1,0             OFFPOS=T SCENARIOS - ARE OK                  
         JE    VTS1NC20                                                         
         CLI   BYTE1,LDGOTRAN                                                   
         JE    VTS1NC20                                                         
         XR    RF,RF                                                            
         TM    SCPYEL+CPYSTAT4-CPYELD,CPYSOFF2                                  
         BZ    VTS1NC12                                                         
         AHI   RF,1                                                             
         NI    BYTE1,FF-LDGOKEY2                                                
                                                                                
VTS1NC12 XR    RE,RE                                                            
         IC    RE,BYTE1                                                         
         SHI   RE,1                                                             
         LA    RE,TEMP2+2(RE)                                                   
                                                                                
         USING OFFALD,R1                                                        
VTS1NC14 L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,SPACES                                                  
         MVC   OFFAOFFC(0),0(RE)                                                
         EX    RF,*-6                                                           
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              VALIDATE OFFICE                              
         JE    VTS1NC20                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
                                                                                
         USING ACTRECD,R2                                                       
VTS1NC20 LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,TEMP2                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    VTS1NC22                                                         
                                                                                
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         J     EXITN                                                            
                                                                                
VTS1NC22 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                PROD'N LEDGER NOT ON FILE                    
*                                                                               
VTS1NC23 L     R2,AIO3                                                          
VTS1NC26 TM    ACTRSTAT,ACTSABLP                                                
         BNZ   VTS1NC28                                                         
                                                                                
         MVC   ROUERRV,=AL2(AE$INACP)                                           
         J     EXITN                                                            
*                                                                               
VTS1NC28 LA    R2,ACTRFST                                                       
         USING RSTELD,R2                                                        
         XR    R0,R0                                                            
VTS1NC30 CLI   RSTEL,0                                                          
         BE    VTS1NC38                                                         
         CLI   RSTEL,RSTELQ                                                     
         BE    VTS1NC34                                                         
         CLI   RSTEL,NAMELQ                                                     
         BE    VTS1NC36                                                         
VTS1NC32 IC    R0,RSTLN                                                         
         AR    R2,R0                                                            
         B     VTS1NC30                                                         
*                                                                               
VTS1NC34 CLI   TWAAUTH+1,0                                                      
         JE    VTS1NC32                                                         
         CLC   RSTSECY+1(1),TWAAUTH+1                                           
         JNH   VTS1NC32                                                         
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     EXITN                                                            
*                                                                               
         USING NAMELD,R2                                                        
VTS1NC36 CLI   NAMLN,NAMLN1Q                                                    
         BNH   VTS1NC32                                                         
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         MVC   TS#CLINM(0),NAMEREC                                              
         EX    RE,*-6                                                           
         J     VTS1NC32                                                         
*                                                                               
VTS1NC38 OI    TS#ICPJ,TS#INON     NON CLIENT FOUND                             
         J     EXITY                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET PERSON DETAILS (USE TS#PBLK)                                    *         
***********************************************************************         
         DS    0H                                                               
PERDTL   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*PERDTL*'                                                    
                                                                                
         XC    TS#CDAT,TS#CDAT                                                  
         XC    TS#CEND,TS#CEND                                                  
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,IOKEY            READ FOR PERSON RECORD                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,TS#PCOD                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    PERDTL10                                                         
         MVC   FULL2(2),=AL2(AE$IVPER)                                          
         J     EXITN                                                            
                                                                                
PERDTL10 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
                                                                                
         L     R2,AIO1                                                          
         LA    R3,PERRFST          LOCATE ELEMENTS                              
         USING EMPELD,R3                                                        
         XR    R0,R0                                                            
                                                                                
PERDTL20 CLI   EMPEL,0                                                          
         JE    PERDTL80                                                         
         CLI   EMPEL,EMPELQ                                                     
         JE    PERDTL40                                                         
         CLI   EMPEL,LOCELQ                                                     
         JE    PERDTL60                                                         
         CLI   EMPEL,PIDELQ                                                     
         JE    PERDTL70                                                         
                                                                                
PERDTL30 IC    R0,EMPLN                                                         
         AR    R3,R0                                                            
         J     PERDTL20                                                         
                                                                                
PERDTL40 ST    R3,TS#PEMP          SAVE EMPEL ADDRESS                           
         CLC   TS#PDAT,EMPHIR      VALIDATE EMPEL                               
         JNL   PERDTL45                                                         
         OC    TS#PEST,TS#PEST     WAS START DATE CALCULATED?                   
         JZ    PERDTL44                                                         
         MVC   TS#PDAT,EMPHIR                                                   
         MVC   TS#PEND,EMPHIR                                                   
         J     PERDTL45                                                         
PERDTL44 MVC   FULL2(2),=AL2(AE$OHIRE)                                          
         J     EXITN                                                            
PERDTL45 OC    EMPTRM,EMPTRM                                                    
         JZ    PERDTL50                                                         
         CLC   TS#PEND,EMPTRM      VALIDATE EMPEL                               
         JNH   PERDTL50                                                         
         MVC   FULL2(2),=AL2(AE$OHIRE)                                          
         J     EXITN                                                            
PERDTL50 DS    0H                                                               
         J     PERDTL30                                                         
                                                                                
         USING LOCELD,R3                                                        
PERDTL60 ST    R3,TS#PCLE2                                                      
         CLC   TS#CDAT,LOCSTART                                                 
         JH    *+16                                                             
         MVC   TS#CDAT,LOCSTART    SAVE OFF MOST RECENT START DATE              
         MVC   TS#CEND,LOCEND      AND END DATE                                 
         CLC   TS#PDAT,LOCSTART    FIND LOCATION FOR CURRENT DATE               
         JL    PERDTL30                                                         
         OC    LOCEND,LOCEND                                                    
         JZ    PERDTL65                                                         
         CLC   TS#PEND,LOCEND                                                   
         JH    PERDTL30                                                         
PERDTL65 ST    R3,TS#PCLE                                                       
         J     PERDTL30                                                         
                                                                                
         USING PIDELD,R3                                                        
PERDTL70 MVC   TS#PPID,PIDNO       EXTRACT PID                                  
         J     PERDTL30                                                         
                                                                                
PERDTL80 OC    TS#PCLE,TS#PCLE     ENSURE THIS IS SET                           
         JNZ   PERDTL85                                                         
         CLI   TS#PTYPE,TS#PTSMS   ARE WE DOING A SEARCH?                       
         JH    PERDTL82                                                         
         CLI   TS#PTYPE,TS#PTRTP                                                
         JL    PERDTL82                                                         
         CLC   TS#CDAT,TS#CSDT     USER IS NOT ACTIVE ANY MORE BUT TO           
         JNH   PERDTL82            ALLOW SEARCHING CHECK WITH SEARCH            
         MVC   TS#PCLE,TS#PCLE2                                                 
         CLC   TS#CEND,TS#CEDT     DATES                                        
         JL    EXITY                                                            
PERDTL82 MVC   FULL2(2),=AL2(AE$IVLDT)                                          
         J     EXITN                                                            
                                                                                
PERDTL85 OC    TS#PPID,TS#PPID                                                  
         JNZ   PERDTL90                                                         
         MVC   FULL2(2),=AL2(AE$NCPID)                                          
         J     EXITN                                                            
                                                                                
PERDTL90 J     EXITY                                                            
                                                                                
         DROP  RB,R2,R3                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET CALENDAR FOR TS#CDAT AND TS#COFF                                *         
* ENTRY: TS#CDAT compressed start date                                          
*        TS#CEND compressed end date                                            
*        TS#COFF office to read for.                                            
* EXIT:  calendar record in IO1                                                 
*        CC low if not found                                                    
***********************************************************************         
         DS    0H                                                               
GETCAL   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*GETCAL*'                                                    
                                                                                
         CLC   TS#COFF,SPACES                                                   
         JNH   GETCAL10                                                         
*                                                                               
* Look for office level calendar                                                
*                                                                               
         USING CASRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    CASKEY,CASKEY                                                    
         MVI   CASKTYP,CASKTYPQ                                                 
         MVI   CASKSUB,CASKSUBQ                                                 
         MVC   CASKCPY,CUXCPY                                                   
         MVC   CASKEMOA,TS#CEND                                                 
         MVC   CASKSMOA,TS#CDAT                                                 
         MVC   CASKOFC,TS#COFF                                                  
         L     R3,AIO8                                                          
         CLC   CASKEY,0(R3)         Do we have this one already?                
         JE    EXITL                Yes - tell caller not to loop               
         MVC   WORK(L'CASKEY),0(R3) Save key we had before                      
*                                                                               
* Check to see if calendar in buffer                                            
*                                                                               
         MVC   0(L'CASKEY,R3),CASKEY    pass NEW KEY to getbuf                  
         GOTOR GETBUF,(R3)                                                      
         JE    GETCAL40            Got it                                       
         MVC   0(L'CASKEY,R3),WORK                                              
         JH    GETCAL10            Read before and didn't exist                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO8'                               
         JE    GETCAL20            Found it                                     
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO8                                        
         MVC   0(L'CASKEY,R3),IOKEYSAV                                          
         GOTOR ADDBUF,(R3)                                                      
*                                                                               
* No match at office level, get agency level calendar                           
*                                                                               
GETCAL10 LA    R2,IOKEY                                                         
         XC    CASKEY,CASKEY                                                    
         MVI   CASKTYP,CASKTYPQ                                                 
         MVI   CASKSUB,CASKSUBQ                                                 
         MVC   CASKCPY,CUXCPY                                                   
         MVC   CASKEMOA,TS#CEND                                                 
         MVC   CASKSMOA,TS#CDAT                                                 
         MVC   CASKOFC,SPACES                                                   
         L     R3,AIO8                                                          
         CLC   CASKEY,0(R3)         Do we have this one already?                
         JE    EXITL                Yes - tell caller not to loop               
         MVC   0(L'CASKEY,R3),CASKEY                                            
         GOTOR GETBUF,(R3)                                                      
         JE    GETCAL40                                                         
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO8'                               
         JE    GETCAL20                                                         
         MVC   FULL2(2),=AL2(AE$NOCAL)                                          
         J     EXITH               HIGH=ERROR                                   
*                                                                               
GETCAL20 DS    0H                  CALENDAR FOUND                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO8'                              
         JE    GETCAL30                                                         
         DC    H'0'                FATAL ERROR                                  
*                                                                               
GETCAL30 L     R2,AIO8             include in optim buffer                      
         GOTOR ADDBUF,CASKEY                                                    
         J     EXITY                                                            
*                                                                               
GETCAL40 L     R2,AIO8             READ REC FROM BUFFER                         
         CLC   CASKEY,WORK         BUT IS IT A NEW ONE?                         
         JNE   EXITY                                                            
         J     EXITL               NO, SAME AS WE HAD BEFORE                    
         DROP  RB,R2                     WARN CALLER NOT TO LOOP.               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET OFFICE CODE FOR PERSON                                          *         
* ENTRY: Person record in AIO4                                                  
*        TS#CSDT compressed period start date                                   
* EXIT:  TS#1ROFF set from this person                                          
***********************************************************************         
         DS    0H                                                               
PERSOFF  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*PERSOFF'                                                    
*     GET LOCATION FOR START DATE                                               
         L     R2,AIO4                                                          
         LA    R2,PERRFST-PERRECD(R2)                                           
         USING LOCELD,R2                                                        
         XC    FULL1,FULL1                                                      
POFF010  CLI   LOCEL,0                                                          
         BE    POFF025           no locel for start date                        
         CLI   LOCEL,LOCELQ                                                     
         BNE   POFF020                                                          
         OC    FULL1,FULL1                                                      
         BNZ   POFF015                                                          
         ST    R2,FULL1            SAVE FIRST LOCEL                             
POFF015  CLC   LOCSTART,TS#CSDT                                                 
         BH    POFF020                                                          
         OC    LOCEND,LOCEND                                                    
         BZ    POFF030                                                          
         CLC   LOCEND,TS#CSDT                                                   
         BH    POFF030                                                          
POFF020  LLC   RF,LOCLN                                                         
         AR    R2,RF                                                            
         B     POFF010                                                          
*                                                                               
POFF025  L     R2,FULL1          start date before effective date               
*                                                                               
POFF030  MVC   TS#1ROFF,LOCOFF                                                  
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* Get my reports reports - get PINs of these people into WMP          *         
***********************************************************************         
         SPACE 1                                                                
GETMYRR  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*GETMYRR'                                                    
                                                                                
         USING LW_D,R2                                                          
         XR    R2,R2               Point to list in WMP                         
         ICM   R2,7,DAPID2                                                      
         JZ    GETMRR02                                                         
         XR    R3,R3                                                            
         XC    LW_NUMN,LW_NUMN                                                  
         LA    R0,LW_DATA2         Clear entries                                
         LHI   R1,DPIDMAXQ*L'PIDKNUM                                            
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE               Clear area                                   
                                                                                
GAP      USING GAPTABD,GAPAREA                                                  
GETMRR02 XC    GAPAREA,GAPAREA                                                  
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   EXITY               end of buffer                                
         J     GETMRR06                                                         
GETMRR04 GOTOR (#GOATSR,AGOATSR),DMCB,('TSANXT',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   EXITY               end of buffer                                
GETMRR06 TM    GAP.GAPTSTA,GAPTSMQ     look for main entry                      
         JZ    GETMRR04                                                         
                                                                                
         USING OFFALD,R1                                                        
GETMRR08 L     R1,AOFFAREA                                                      
         LA    RE,1                                                             
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         JNZ   *+8                                                              
         LA    RE,0                                                             
         BASR  RF,0                                                             
         MVC   OFFAOFFC(0),GAP.GAPTACT                                          
         EX    RE,0(RF)                                                         
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         JNE   GETMRR04                                                         
         DROP  R1                                                               
                                                                                
         USING ACTRECD,R2                                                       
GETMRR10 LA    R2,IOKEY            build 1R account record key                  
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA(L'ACTKUNT+L'ACTKLDG),=C'1R'                              
         MVC   ACTKACT,GAP.GAPTACT                                              
         MVC   CSVKEY1,ACTKEY      save key    (NOTE CSVKEY1 NOW FREE)          
                                                                                
GETMRR12 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JE    GETMRR16                                                         
         DC    H'0'                no errors should occur                       
                                                                                
GETMRR14 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         JE    GETMRR16                                                         
         DC    H'0'                no errors should occur                       
                                                                                
GETMRR16 CLC   CSVKEY1(ACTKACT-ACTRECD),ACTKEY                                  
         JNE   GETMRR04                                                         
                                                                                
         MVC   GAPAREA2,GAPAREA                                                 
         LLC   RE,GAP.GAPTLEN          for main entry save office               
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         CLC   GAP.GAPTACT(0),ACTKACT                                           
         EX    RE,0(RF)                                                         
         JNE   GETMRR04                                                         
                                                                                
         MVC   CSVKEY3,ACTKEY                                                   
         CLC   ACTKEY+ACTKEND(L'ACTKEY-ACTKEND),SPACES                          
         JE    GETMRR26                                                         
GETMRR24 LA    R2,IOKEY                                                         
         MVC   ACTKEY,CSVKEY3                                                   
         MVI   ACTKEY+ACTKEND,FF   if no account read for next                  
         J     GETMRR12                                                         
                                                                                
GETMRR26 TM    ACTKSTAT,ACTSABLP   test low level account                       
         JZ    GETMRR14                                                         
         MVC   DACT,ACTKACT                                                     
         GOTOR TT1RVGT             test against table                           
         JNE   GETMRR24                                                         
         MVC   DPERSON,SPACES      retrieve current person                      
         XR    RE,RE                                                            
         IC    RE,ONERL3L                                                       
         LA    RE,ACTKACT(RE)                                                   
         MVC   DPERSON(7),0(RE)                                                 
                                                                                
         USING PERRECD,R2                                                       
GETMRR30 LA    R2,IOKEY            and read for person record                   
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,DPERSON                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   GETMRR24                                                         
                                                                                
GETMRR32 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO4'                              
         JE    *+6                                                              
         DC    H'0'                Fatal error                                  
*                                                                               
GETMRR34 L     R2,AIO4                                                          
         USING PIDELD,R3                                                        
         LA    R3,PERRFST                                                       
GETMRR36 CLI   PIDEL,0                                                          
         JE    GETMRR24                                                         
         CLI   PIDEL,PIDELQ                                                     
         JE    GETMRR40                                                         
GETMRR38 LLC   R0,PIDLN                                                         
         AR    R3,R0                                                            
         J     GETMRR36                                                         
                                                                                
GETMRR40 GOTOR LP_AAWMP,DMCB,(L'PIDNO,PIDNO),DPIDIND2,                 +        
               DPIDMAXQ,LP_D                                                    
         J     GETMRR24                                                         
                                                                                
***********************************************************************         
* Check limlist/access security for saved searches                    *         
***********************************************************************         
         SPACE 1                                                                
CHKLSEC  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    CL8'*CHKLSE*'                                                    
         CLI   RQ_TSSRC,RQ_TSTBA   Back up approver                             
         BNE   CHKLS00                                                          
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT22Q',TSARABUF),           +        
               ('GAPLBKAP',SPACES),('GAPLTDTE',GAPLPARM),('QTIME',0)            
         BE    CHKLSECY                                                         
         MVC   ROUERRV,FULL2                                                    
         B     CHKLSECN                                                         
CHKLS00  CLI   RQ_TSSRC,RQ_TSTMY   my timesheets?                               
         BE    CHKLSECY            yes - ignore limit list for yourself         
         CLI   RQ_TPRSR,C'Y'       are we able to overide person search         
         BE    CHKLSECY            yes - ignore limlst and approver             
*                                                                               
         USING GSECTABD,R2                                                      
CHKLS02  LA    R2,GSECTAB                                                       
*                                                                               
CHKLS04  CLI   0(R2),X'FF'         End of table                                 
         BE    CHKLSECY                                                         
*                                                                               
         LA    RF,SAVED            Anything input?                              
         LLH   RE,GSECDSP                                                       
         AR    RF,RE                                                            
         LLC   R1,GSECLEN                                                       
         CLC   0(0,RF),SPACES                                                   
         EX    R1,*-6                                                           
         BNH   CHKLS06                                                          
*                                                                               
         MVC   GAPLRUNL,GSECULG                                                 
         MVC   GAPLRACT,SPACES                                                  
         MVC   GAPLRACT,0(RF)      Extract account                              
         EX    R1,*-6                                                           
         GOTOR (#GAPLST,AGAPLST),DMCB,(GSECTYP,TSARABUF),              +        
               ('GAPLLIML',GAPLFILT),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         BE    CHKLS06                                                          
         GOTOR (#GAPLST,AGAPLST),DMCB,(GSECTYP,TSARABUF),              +        
               ('GAPLAPPR',GAPLFILT),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         BE    CHKLS06                                                          
*&&US                                                                           
         CLI   RQ_TTYPE,RQ_THOUR   Are we the hours search                      
         BNE   CHKLSERR            No - timesheet search                        
         CLC   CUAALF,=C'H7'       GroupM US only behaviour                     
         BNE   CHKLS05                                                          
         GOTOR (#GAPLST,AGAPLST),DMCB,(GSECTYP,TSARABUF),              +        
               ('GAPLBKAP',GAPLFILT),('GAPLTDTE',GAPLPARM),('QTIME',0)          
         BE    CHKLS06                                                          
*&&                                                                             
CHKLS05  CLC   GSECULG,=C'SJ'      For SJ default access for no liml            
         BNE   CHKLSERR            /approver record is all access               
*                                  gaplst needs to know buffer not              
CHKLS06  MVI   GAPLPARM,GAPLPADD+GAPLACLS                                       
         AHI   R2,GSECTABL                                                      
         B     CHKLS04                                                          
*                                                                               
CHKLSECY CR    RB,RB                                                            
         B     CHKLSECX                                                         
CHKLSERR MVC   ROUERRV,=AL2(AE$SECLK)  Security lockout                         
CHKLSECN LTR   RB,RB                                                            
CHKLSECX XIT1                                                                   
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
LLINUSE  DS    XL1                 Limlist in use                               
GSECTAB  DS    0H                                                               
         DC    AL1(GAPTT1Q),AL2(X#1RACT-SAVED),AL1(L'X#1RACT),C'1R'             
         DC    AL1(GAPTT2Q),AL2(X#SJACC-SAVED),AL1(L'X#SJACC),C'SJ'             
         DC    AL1(GAPTT3Q),AL2(X#1NACC-SAVED),AL1(L'X#1NACC),C'1N'             
         DC    AL1(GAPTTAQ),AL2(TS#WCD-SAVED),AL1(L'X#WRKC),C'SJ'               
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* Initialise optimisation buffer (uses WSSVR buffer online)                     
**********************************************************************          
                                                                                
INIBUF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*INIBUF*'                                                      
                                                                                
T        USING TSARD,TSAROBUF                                                   
         XC    TSAROBUF(TSPNEWL),TSAROBUF   RETAIN A(BUFFER) IF POSS            
         MVI   T.TSACTN,TSAINI     Set action to 'Initialise'                   
         MVI   T.TSRECI,TSRXTN+TSRMINB1      use Minio1                         
         MVI   T.TSKEYL,L'OB_KEY                                                
         LHI   R0,ONEK                                                          
         STCM  R0,3,T.TSBUFFL                                                   
         LHI   R0,OB_LNQ                                                        
         STCM  R0,3,T.TSRECL                                                    
         MVC   T.TSACOM,ACOMFACS                                                
         GOTOR VTSAR,T.TSARD                                                    
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  T                                                                
         EJECT                                                                  
**********************************************************************          
* Add a record to optimisation buffer                                           
*                                                                               
* Ntry:- R1 points to caller's OB_D                                             
**********************************************************************          
                                                                                
ADDBUF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*ADDBUF*'                                                      
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSAADD     Set action to 'Add'                          
         ST    R1,T.TSAREC                                                      
         GOTOR VTSAR,T.TSARD                                                    
         JE    EXITY                                                            
         DC    H'0'                buffer full or duplicate key                 
         DROP  T                        extend if full, and offline             
         EJECT                                                                  
**********************************************************************          
* Get a record from optimisation buffer                                         
*                                                                               
* Ntry:- R1 points to caller's OB_D                                             
* Exit:- CC=Low if record not found in buffer                                   
*        CC=Equal if record found and is not posted with an error               
*           - record is returned in caller's OB_D                               
*        CC=High if record found and is posted with an error (set in            
*           ROUERRV)                                                            
**********************************************************************          
                                                                                
GETBUF   NTR1  LABEL=NO,WORK=(RC,OB_LNQ)                                        
         J     *+12                                                             
         DC    C'*GETBUF*'                                                      
                                                                                
W        USING OB_D,RC             routine's working storage                    
         LR    R2,R1                                                            
P        USING OB_D,R2             R2=A(caller's OB_D)                          
         MVC   W.OB_KEY,P.OB_KEY                                                
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSARDH     Set action to 'Read High'                    
         LA    R0,W.OB_D                                                        
         ST    R0,T.TSAREC         Read into acquired storage                   
         GOTOR VTSAR,T.TSARD                                                    
         JNE   EXITL               Not found/EOF                                
         DROP  T                                                                
                                                                                
         OC    W.OB_D+CASRLEN-CASRECD(L'CASRLEN),W.OB_D+CASRLEN-CASRECD         
         JZ    EXITH              cc=high                                       
         LA    R3,OB_LNQ          Copy from working storage to                  
         LR    R0,RC                             Callers area                   
         LR    R1,R3                                                            
         MVCL  R2,R0                                                            
                                                                                
         J     EXITY                                                            
         DROP  W,P                                                              
         EJECT                                                                  
***********************************************************************         
* LIST OF ACCOUNT FILES TO OPEN IN ALL SYSTEMS                        *         
***********************************************************************         
                                                                                
FILES    DS    0X                  ** FILE INFO **                              
         DC    C'ACCOUNT'          SYSTEM NAME FOR OPEN                         
                                                                                
         DC    C'NCTFILE '         FILE LIST                                    
         DC    C'NGENFIL '                                                      
         DC    C'NGENDIR '                                                      
         DC    C'NACCDIR '                                                      
         DC    C'NACCMST '                                                      
         DC    C'NACCARC '                                                      
         DC    C'X'                                                             
*                                                                               
*&&UK                                                                           
FILES2   DS    0C                  ** File Info **                              
         DC    C'MEDIA  '                                                       
         DC    C'U'                                                             
MEDDIR   DC    C'MEDDIR '                                                       
         DC    C'U'                                                             
MEDFIL   DC    C'MEDFIL '                                                       
         DC    C'U'                                                             
         DC    C'MEDRCV '                                                       
         DC    C'X'                                                             
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* LIST OF CORE RESIDENT FACILITIES (MAPS TO SYSADDR IN WORKD)         *         
***********************************************************************         
                                                                                
FACS     DS    0X                                                               
                                                                                
***********************************************************************         
* REQUEST DEFINITIONS (REQUEST and OUTPUT)                            *         
***********************************************************************         
                                                                                
*** TimeSheet Audit Trail D/load **************************************         
                                                                                
REQTAUD  LKREQ H,A#TSAT,OUTTAUD                                                 
                                                                                
Person   LKREQ F,01,(D,B#SAVED,RQ_ATPER),CHAR,OLEN=L'RQ_ATPER,         X        
               MAXLEN=L'RQ_ATPER,TEXT=AC#PRSN,COL=*                             
Date     LKREQ F,02,(D,B#SAVED,RQ_ATDAT),EDAT,OLEN=L'RQ_ATDAT,         X        
               MAXLEN=L'RQ_ATDAT,TEXT=AC#DATE,COL=*                             
Ldate    LKREQ F,03,(D,B#SAVED,RQ_ALDAT),EDAT,OLEN=L'RQ_ALDAT,         X        
               MAXLEN=L'RQ_ALDAT,TEXT=AC#DATE,COL=*                             
                                                                                
                                                                                
         LKREQ E                                                                
                                                                                
*** TimeLines search TimeSheets *************************************           
                                                                                
REQTSTL  LKREQ H,A#TSTL,OUTTSTL                                                 
                                                                                
Type     LKREQ F,01,(D,B#SAVED,RQ_TTYPE),CHAR,OLEN=L'RQ_TTYPE,         X        
               MAXLEN=L'RQ_TTYPE,TEXT=AC#TYPE,COL=*                             
StartDt  LKREQ F,02,(D,B#SAVED,RQ_TTSTD),CHAR,OLEN=L'RQ_TTSTD,         X        
               MAXLEN=L'RQ_TTSTD,TEXT=AC#STRDT,COL=*                            
EndDt    LKREQ F,03,(D,B#SAVED,RQ_TTEND),CHAR,OLEN=L'RQ_TTEND,         X        
               MAXLEN=L'RQ_TTEND,TEXT=AC#ENDDT,COL=*                            
NStrt    LKREQ F,04,(D,B#SAVED,RQ_NSTRT),CHAR,OLEN=L'RQ_NSTRT,         X        
               MAXLEN=L'RQ_NSTRT,TEXT=AC#STT,COL=*                              
ODue     LKREQ F,05,(D,B#SAVED,RQ_OVERD),CHAR,OLEN=L'RQ_OVERD,         X        
               MAXLEN=L'RQ_OVERD,TEXT=AC#STT,COL=*                              
InPgr    LKREQ F,06,(D,B#SAVED,RQ_INPRG),CHAR,OLEN=L'RQ_INPRG,         X        
               MAXLEN=L'RQ_INPRG,TEXT=AC#STT,COL=*                              
Submit   LKREQ F,07,(D,B#SAVED,RQ_SUBMT),CHAR,OLEN=L'RQ_SUBMT,         X        
               MAXLEN=L'RQ_SUBMT,TEXT=AC#STT,COL=*                              
Apprvd   LKREQ F,08,(D,B#SAVED,RQ_APPRD),CHAR,OLEN=L'RQ_APPRD,         X        
               MAXLEN=L'RQ_APPRD,TEXT=AC#STT,COL=*                              
Reject   LKREQ F,09,(D,B#SAVED,RQ_REJED),CHAR,OLEN=L'RQ_REJED,         X        
               MAXLEN=L'RQ_REJED,TEXT=AC#STT,COL=*                              
Await    LKREQ F,10,(D,B#SAVED,RQ_AWAIT),CHAR,OLEN=L'RQ_AWAIT,         X        
               MAXLEN=L'RQ_AWAIT,TEXT=AC#STT,COL=*                              
PartAp   LKREQ F,11,(D,B#SAVED,RQ_PRTAP),CHAR,OLEN=L'RQ_PRTAP,         X        
               MAXLEN=L'RQ_PRTAP,TEXT=AC#STT,COL=*                              
Type     LKREQ F,12,(D,B#SAVED,RQ_TSSRC),CHAR,OLEN=L'RQ_TSSRC,         X        
               MAXLEN=L'RQ_TSSRC,TEXT=AC#TYPE,COL=*                             
Uapprd   LKREQ F,13,(D,B#SAVED,RQ_TSUNA),CHAR,OLEN=L'RQ_TSUNA,         X        
               MAXLEN=L'RQ_TSUNA,TEXT=AC#STT,COL=*                              
Bill     LKREQ F,14,(D,B#SAVED,RQ_TSBIL),CHAR,OLEN=L'RQ_TSBIL,         X        
               MAXLEN=L'RQ_TSBIL,TEXT=AC#STT,COL=*                              
NonBil   LKREQ F,15,(D,B#SAVED,RQ_TSNBL),CHAR,OLEN=L'RQ_TSNBL,         X        
               MAXLEN=L'RQ_TSNBL,TEXT=AC#STT,COL=*                              
Charge   LKREQ F,16,(D,B#SAVED,RQ_TSCHG),CHAR,OLEN=L'RQ_TSCHG,         X        
               MAXLEN=L'RQ_TSCHG,TEXT=AC#STT,COL=*                              
Client   LKREQ F,17,(D,B#SAVED,RQ_TTCLI),CHAR,OLEN=L'RQ_TTCLI,         X        
               MAXLEN=L'RQ_TTCLI,TEXT=AC#CLIC,COL=*                             
Product  LKREQ F,18,(D,B#SAVED,RQ_TTPRO),CHAR,OLEN=L'RQ_TTPRO,         X        
               MAXLEN=L'RQ_TTPRO,TEXT=AC#PROC,COL=*                             
Job      LKREQ F,19,(D,B#SAVED,RQ_TTJOB),CHAR,OLEN=L'RQ_TTJOB,         X        
               MAXLEN=L'RQ_TTJOB,TEXT=AC#JOBC,COL=*                             
WorkCode LKREQ F,20,(D,B#SAVED,RQ_TTWCD),CHAR,OLEN=L'RQ_TTWCD,         X        
               MAXLEN=L'RQ_TTWCD,TEXT=AC#WC,COL=*                               
Acc1N    LKREQ F,21,(D,B#SAVED,RQ_TT1NA),CHAR,OLEN=L'RQ_TT1NA,         X        
               MAXLEN=L'RQ_TT1NA,TEXT=AC#1N,COL=*                               
Office   LKREQ F,22,(D,B#SAVED,RQ_TTOFF),CHAR,OLEN=L'RQ_TTOFF,         X        
               MAXLEN=L'RQ_TTOFF,TEXT=AC#OFFC,COL=*                             
Depart   LKREQ F,23,(D,B#SAVED,RQ_TTDEP),CHAR,OLEN=L'RQ_TTDEP,         X        
               MAXLEN=L'RQ_TTDEP,TEXT=AC#DEPC,COL=*                             
SubDep   LKREQ F,24,(D,B#SAVED,RQ_TTSUB),CHAR,OLEN=L'RQ_TTSUB,         X        
               MAXLEN=L'RQ_TTSUB,TEXT=AC#SUBDP,COL=*                            
Person   LKREQ F,25,(D,B#SAVED,RQ_TTPRS),CHAR,OLEN=L'RQ_TTPRS,         X        
               MAXLEN=L'RQ_TTPRS,TEXT=AC#STFC,COL=*                             
MinHours LKREQ F,26,(D,B#SAVED,RQ_TTHMI),CHAR,OLEN=L'RQ_TTHMI,         X        
               MAXLEN=L'RQ_TTHMI,TEXT=AC#MIN,COL=*                              
MaxHours LKREQ F,27,(D,B#SAVED,RQ_TTHMA),CHAR,OLEN=L'RQ_TTHMA,         X        
               MAXLEN=L'RQ_TTHMA,TEXT=AC#MAX,COL=*                              
PerSr    LKREQ F,28,(D,B#SAVED,RQ_TPRSR),CHAR,OLEN=L'RQ_TPRSR,         X        
               MAXLEN=L'RQ_TPRSR,TEXT=(*,TLPRSLIT),COL=*                        
OrdNum   LKREQ F,29,(D,B#SAVED,RQ_ORD#),CHAR,OLEN=L'RQ_ORD#,           +        
               MAXLEN=L'RQ_ORD#,TEXT=(*,TLORDLIT),COL=*                         
IntRef   LKREQ F,30,(D,B#SAVED,RQ_INTRF),CHAR,OLEN=L'RQ_INTRF,         +        
               MAXLEN=L'RQ_INTRF,TEXT=(*,TLINTLIT),COL=*                        
EstNum   LKREQ F,31,(D,B#SAVED,RQ_EST#),CHAR,OLEN=L'RQ_EST#,           +        
               MAXLEN=L'RQ_EST#,TEXT=(*,TLESTLIT),COL=*                         
Ovr1R    LKREQ F,32,(D,B#SAVED,RQ_APP1R),CHAR,OLEN=L'RQ_APP1R,         +        
               MAXLEN=L'RQ_APP1R,TEXT=(*,TLOVRLIT),COL=*                        
API      LKREQ F,33,(D,B#SAVED,RQ_APIVN),LBIN,OLEN=L'RQ_APIVN,         +        
               MAXLEN=3,TEXT=(*,TLAPILIT),COL=*                                 
MyRRs    LKREQ F,34,(D,B#SAVED,RQ_MYRR),CHAR,OLEN=L'RQ_MYRR,           +        
               MAXLEN=L'RQ_MYRR,TEXT=(*,TLMRRLIT),COL=*                         
Linmn    LKREQ F,35,(D,B#SAVED,RQ_LMPID),CHAR,OLEN=L'RQ_LMPID,         +        
               MAXLEN=L'RQ_LMPID,TEXT=AC#RSLMG,COL=*                            
                                                                                
         LKREQ E                                                                
                                                                                
*** TimeSheet List D/load *********************************************         
                                                                                
REQTIML  LKREQ H,A#TIML,OUTTIML                                                 
                                                                                
Type     LKREQ F,01,(D,B#SAVED,RQ_TLTY),CHAR,OLEN=L'RQ_TLTY,           X        
               MAXLEN=L'RQ_TLTY,TEXT=AC#TYPE,COL=*                              
SubTyp   LKREQ F,02,(D,B#SAVED,RQ_TLSU),CHAR,OLEN=L'RQ_TLSU,           X        
               MAXLEN=L'RQ_TLSU,TEXT=AC#SUBTY,COL=*                             
Status   LKREQ F,03,(D,B#SAVED,RQ_TLST),CHAR,OLEN=L'RQ_TLST,           X        
               MAXLEN=L'RQ_TLST,TEXT=AC#STT,COL=*                               
StartDt  LKREQ F,04,(D,B#SAVED,RQ_TLSD),CHAR,OLEN=L'RQ_TLSD,           X        
               MAXLEN=L'RQ_TLSD,TEXT=AC#STRDT,COL=*                             
EndDt    LKREQ F,05,(D,B#SAVED,RQ_TLED),CHAR,OLEN=L'RQ_TLED,           X        
               MAXLEN=L'RQ_TLED,TEXT=AC#ENDDT,COL=*                             
GAOV     LKREQ F,06,(D,B#SAVED,RQ_TLGC),CHAR,OLEN=L'RQ_TLGC,           X        
               MAXLEN=L'RQ_TLGC,TEXT=AC#GAOV,COL=*                              
                                                                                
         LKREQ E                                                                
                                                                                
*** Time Summary View(s) **********************************************         
                                                                                
REQTSUM  LKREQ H,A#TSUM,OUTTSUM                                                 
                                                                                
Date     LKREQ F,01,(D,B#SAVED,RQ_SUDTE),CHAR,OLEN=L'RQ_SUDTE,         X        
               MAXLEN=L'RQ_SUDTE,TEXT=AC#DATE,COL=*                             
View     LKREQ F,02,(D,B#SAVED,RQ_SUVIE),CHAR,OLEN=L'RQ_SUVIE,         X        
               MAXLEN=L'RQ_SUVIE,TEXT=AC#QV,COL=*                               
Type     LKREQ F,03,(D,B#SAVED,RQ_SUTYP),CHAR,OLEN=L'RQ_SUTYP,         X        
               MAXLEN=L'RQ_SUTYP,TEXT=AC#TYPE,COL=*                             
Person   LKREQ F,04,(D,B#SAVED,RQ_SUPER),CHAR,OLEN=L'RQ_SUPER,         X        
               MAXLEN=L'RQ_SUPER,TEXT=AC#STFC,COL=*                             
Client   LKREQ F,05,(D,B#SAVED,RQ_SUCLI),CHAR,OLEN=L'RQ_SUCLI,         X        
               MAXLEN=L'RQ_SUCLI,TEXT=AC#CLIC,COL=*                             
OneNac   LKREQ F,06,(D,B#SAVED,RQ_SU1NA),CHAR,OLEN=L'RQ_SU1NA,         X        
               MAXLEN=L'RQ_SU1NA,TEXT=AC#1N,COL=*                               
Excl1N   LKREQ F,07,(D,B#SAVED,RQ_SUX1N),CHAR,OLEN=L'RQ_SUX1N,         X        
               MAXLEN=L'RQ_SUX1N,TEXT=AC#SEC,COL=*                              
                                                                                
         LKREQ E                                                                
                                                                                
                                                                                
         LKREQ X                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST LITERALS                                                    *         
***********************************************************************         
         SPACE 1                                                                
TLPEDLIT DC    C'Period End Date'                                               
TLLEDLIT DC    C'Location End Date'                                             
TLPRSLIT DC    C'Person search'                                                 
TLORDLIT DC    C'Order number'                                                  
TLINTLIT DC    C'Internal reference'                                            
TLESTLIT DC    C'Estimate number'                                               
TLOVRLIT DC    C'Override 1R rights'                                            
TLAPILIT DC    C'API version'                                                   
TLMRRLIT DC    C'Include my reports reports'                                    
                                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS                                                         *         
***********************************************************************         
                                                                                
*** TimeSheet Audit Trail DownLoad ************************************         
                                                                                
OUTTAUD  LKOUT H                                                                
OUTTAUDS LKOUT R,R#TSAT                                                         
                                                                                
AdTyp    LKOUT C,01,(D,B#SAVED,AT_TYPE),CHAR                                    
AdDate   LKOUT C,02,(D,B#SAVED,AT_DATE),PDAT                                    
AdTime   LKOUT C,03,(D,B#SAVED,AT_TIME),CHAR                                    
AdPerf   LKOUT C,04,(D,B#SAVED,AT_PRFN),CHAR                                    
AdPerl   LKOUT C,05,(D,B#SAVED,AT_PRLN),CHAR                                    
AdRow    LKOUT C,06,(D,B#SAVED,AT_ROW),CHAR                                     
AdStfr   LKOUT C,07,(D,B#SAVED,AT_STFR),CHAR                                    
AdStto   LKOUT C,08,(D,B#SAVED,AT_STTO),CHAR                                    
AdCmnts  LKOUT C,09,(D,B#SAVED,AT_CMNTS),CHAR                                   
AdPID    LKOUT C,10,(D,B#SAVED,AT_PID),CHAR                                     
AdUsrID  LKOUT C,11,(D,B#SAVED,AT_USER),CHAR                                    
MRow     LKOUT C,12,(D,B#SAVED,AT_MROW),LBIN                                    
ToAc     LKOUT C,13,(D,B#SAVED,AT_TACC),CHAR                                    
ToTyp    LKOUT C,14,(D,B#SAVED,AT_TTY),CHAR                                     
ToWC     LKOUT C,15,(D,B#SAVED,AT_TWC),CHAR                                     
ToHrs    LKOUT C,16,(D,B#SAVED,AT_THRS),SPAK                                    
ToNar    LKOUT C,17,(D,B#SAVED,AT_TNAR),CHAR                                    
ToORD    LKOUT C,18,(D,B#SAVED,AT_TORD),CHAR                                    
ToInt    LKOUT C,19,(D,B#SAVED,AT_TINT),CHAR                                    
ToTot    LKOUT C,20,(D,B#SAVED,AT_TTOT),CPAK                                    
ToPrce   LKOUT C,21,(D,B#SAVED,AT_TPRC),CPAK                                    
ToMult   LKOUT C,22,(D,B#SAVED,AT_TMUL),CPAK                                    
ToCode   LKOUT C,23,(D,B#SAVED,AT_TCDE),CHAR                                    
ToEst    LKOUT C,24,(D,B#SAVED,AT_TEST),CHAR                                    
AdPerm   LKOUT C,25,(D,B#SAVED,AT_PRMN),CHAR                                    
Applic   LKOUT C,26,(D,B#SAVED,AT_APPL),LBIN                                    
                                                                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
*** TimeLine Search Download ******************************************         
                                                                                
OUTTSTL  LKOUT H                                                                
OUTTSTLS LKOUT R,R#TSTL                                                         
                                                                                
PersC    LKOUT C,01,(D,B#SAVED,SR_PERC),CHAR,ND=Y                               
PrsFN    LKOUT C,02,(D,B#SAVED,SR_PERF),CHAR,ND=Y                               
PrsLN    LKOUT C,03,(D,B#SAVED,SR_PERL),CHAR,ND=Y                               
StaDt    LKOUT C,04,(D,B#SAVED,SR_STAD),PDAT,ND=Y                               
EndDt    LKOUT C,05,(D,B#SAVED,SR_ENDD),PDAT,ND=Y                               
PerdN    LKOUT C,06,(D,B#SAVED,SR_PERD),CHAR,ND=Y                               
LocSt    LKOUT C,07,(D,B#SAVED,SR_LOCS),PDAT,ND=Y                               
LocEn    LKOUT C,08,(D,B#SAVED,SR_LOCE),PDAT,ND=Y                               
Hours    LKOUT C,09,(D,B#SAVED,SR_HOUR),CPAK,ND=Y                               
Statv    LKOUT C,10,(D,B#SAVED,SR_STTV),CHAR,ND=Y                               
Statt    LKOUT C,11,(D,B#SAVED,SR_STTS),CHAR,ND=Y                               
IDNum    LKOUT C,12,(D,B#SAVED,SR_IDFN),CHAR,ND=Y                               
EditHr   LKOUT C,13,(D,B#SAVED,SR_EHRS),CPAK,ND=Y                               
CodOf    LKOUT C,14,(D,B#SAVED,SR_OFFC),CHAR,ND=Y                               
NamOf    LKOUT C,15,(D,B#SAVED,SR_OFNM),CHAR,ND=Y                               
CodDp    LKOUT C,16,(D,B#SAVED,SR_DEPT),CHAR,ND=Y                               
NamDp    LKOUT C,17,(D,B#SAVED,SR_DPNM),CHAR,ND=Y                               
CodSDp   LKOUT C,18,(D,B#SAVED,SR_SBDP),CHAR,ND=Y                               
NamSDp   LKOUT C,19,(D,B#SAVED,SR_SDNM),CHAR,ND=Y                               
Type     LKOUT C,20,(D,B#SAVED,SR_TYPE),CHAR,ND=Y                               
CodCL    LKOUT C,21,(D,B#SAVED,SR_CLCD),CHAR,ND=Y                               
NamCL    LKOUT C,22,(D,B#SAVED,SR_CLNA),CHAR,ND=Y                               
CodPR    LKOUT C,23,(D,B#SAVED,SR_PRCD),CHAR,ND=Y                               
NamPR    LKOUT C,24,(D,B#SAVED,SR_PRNA),CHAR,ND=Y                               
CodJO    LKOUT C,25,(D,B#SAVED,SR_JOCD),CHAR,ND=Y                               
NamJO    LKOUT C,26,(D,B#SAVED,SR_JONA),CHAR,ND=Y                               
LckJb    LKOUT C,27,(D,B#SAVED,SR_JOLK),CHAR,ND=Y                               
CodWC    LKOUT C,28,(D,B#SAVED,SR_WRKC),CHAR,ND=Y                               
DesWC    LKOUT C,29,(D,B#SAVED,SR_WCDE),CHAR,ND=Y                               
Cod1N    LKOUT C,30,(D,B#SAVED,SR_1NCD),CHAR,ND=Y                               
Nam1N    LKOUT C,31,(D,B#SAVED,SR_1NNA),CHAR,ND=Y                               
MtrTot   LKOUT C,32,(D,B#SAVED,SR_MTRS),CPAK,ND=Y                               
OrdNum   LKOUT C,33,(D,B#SAVED,SR_ORD#),CHAR,ND=Y                               
IntRef   LKOUT C,34,(D,B#SAVED,SR_INTRF),CHAR,ND=Y                              
OrdName  LKOUT C,35,(D,B#SAVED,SR_ORDNM),CHAR,ND=Y                              
Narr     LKOUT C,36,(D,B#SAVED,SR_NARR),CHAR,ND=Y                               
TarUtil  LKOUT C,37,(D,B#SAVED,SR_TU),CPAK,ND=Y                                 
CliHrs   LKOUT C,38,(D,B#SAVED,SR_CHRS),CPAK,ND=Y                               
EstNum   LKOUT C,39,(D,B#SAVED,SR_EST#),CHAR,ND=Y                               
EstName  LKOUT C,40,(D,B#SAVED,SR_ESTNM),CHAR,ND=Y                              
EstLNum  LKOUT C,41,(D,B#SAVED,SR_ESTL#),LBIN,ND=Y                              
MngrPid  LKOUT C,42,(D,B#SAVED,SR_LMPID),CHAR,ND=Y                              
MngrFnm  LKOUT C,43,(D,B#SAVED,SR_LMFNM),CHAR,ND=Y                              
MngrMnm  LKOUT C,44,(D,B#SAVED,SR_LMMNM),CHAR,ND=Y                              
MngrSnm  LKOUT C,45,(D,B#SAVED,SR_LMSNM),CHAR,ND=Y                              
PrsMN    LKOUT C,46,(D,B#SAVED,SR_PERM),CHAR,ND=Y                               
PrvSub   LKOUT C,47,(D,B#SAVED,SR_PRNS),CHAR,ND=Y                               
DTSprev  LKOUT C,48,(D,B#SAVED,SR_COPTA),CHAR,ND=Y                              
ClseJb   LKOUT C,49,(D,B#SAVED,SR_CLSJB),CHAR,ND=Y                              
PIDcd    LKOUT C,50,(D,B#SAVED,SR_PIDC),CHAR,ND=Y                               
Zhts     LKOUT C,51,(D,B#SAVED,SR_ZHTS),CHAR,ND=Y                               
Email    LKOUT C,52,(D,B#SAVED,SR_EMAIL),CHAR,ND=Y                              
MngrEml  LKOUT C,53,(D,B#SAVED,SR_LMEML),CHAR,ND=Y                              
                                                                                
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
*** TimeSheet List DownLoad *******************************************         
                                                                                
OUTTIML  LKOUT H                                                                
OUTTIMLS LKOUT R,R#TIML                                                         
                                                                                
PerCd    LKOUT C,01,(D,B#SAVED,TS_PERC),CHAR,ND=Y                               
PerFN    LKOUT C,02,(D,B#SAVED,TS_PERF),CHAR,ND=Y                               
PerLN    LKOUT C,03,(D,B#SAVED,TS_PERL),CHAR,ND=Y                               
StaDt    LKOUT C,04,(D,B#SAVED,TS_STAD),PDAT,ND=Y                               
EndDt    LKOUT C,05,(D,B#SAVED,TS_ENDD),PDAT,ND=Y                               
PerdN    LKOUT C,06,(D,B#SAVED,TS_PERD),CHAR,ND=Y                               
LocSt    LKOUT C,07,(D,B#SAVED,TS_LOCS),PDAT,ND=Y                               
LocEn    LKOUT C,08,(D,B#SAVED,TS_LOCE),PDAT,ND=Y                               
Hours    LKOUT C,09,(D,B#SAVED,TS_HOUR),CPAK,ND=Y                               
Statv    LKOUT C,10,(D,B#SAVED,TS_STTV),CHAR,ND=Y                               
Statt    LKOUT C,11,(D,B#SAVED,TS_STTS),CHAR,ND=Y                               
IDNum    LKOUT C,12,(D,B#SAVED,TS_IDFN),CHAR,ND=Y                               
EditHr   LKOUT C,13,(D,B#SAVED,TS_EHRS),CPAK,ND=Y                               
MtrTot   LKOUT C,14,(D,B#SAVED,TS_MTRS),CPAK,ND=Y                               
CliHrs   LKOUT C,15,(D,B#SAVED,TS_CHRS),CPAK,ND=Y                               
TarUtil  LKOUT C,16,(D,B#SAVED,TS_TU),CPAK,ND=Y                                 
PerMN    LKOUT C,17,(D,B#SAVED,TS_PERM),CHAR,ND=Y                               
PrvSub   LKOUT C,18,(D,B#SAVED,TS_PRNS),CHAR,ND=Y                               
DTSprev  LKOUT C,19,(D,B#SAVED,TS_COPTA),CHAR,ND=Y                              
Offcod   LKOUT C,20,(D,B#SAVED,TS_OFFC),CHAR,ND=Y                               
Offnam   LKOUT C,21,(D,B#SAVED,TS_OFNM),CHAR,ND=Y                               
Depcod   LKOUT C,22,(D,B#SAVED,TS_DEPT),CHAR,ND=Y                               
Depnam   LKOUT C,23,(D,B#SAVED,TS_DPNM),CHAR,ND=Y                               
Subdep   LKOUT C,24,(D,B#SAVED,TS_SDEP),CHAR,ND=Y                               
Subdnm   LKOUT C,25,(D,B#SAVED,TS_SDNM),CHAR,ND=Y                               
Nooftl   LKOUT C,26,(D,B#SAVED,TS_ZHTS),CHAR,ND=Y                               
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
*** Time Summary Download *********************************************         
                                                                                
OUTTSUM  LKOUT H                                                                
OUTTSUMS LKOUT R,R#TSUM                                                         
                                                                                
Hours    LKOUT C,01,(D,B#SAVED,SU_EHRS),CPAK,ND=Y                               
PCod1    LKOUT C,02,(D,B#SAVED,SU_PCOD),CHAR,ND=Y                               
FNam1    LKOUT C,03,(D,B#SAVED,SU_PFNA),CHAR,ND=Y                               
LNam1    LKOUT C,04,(D,B#SAVED,SU_PLNA),CHAR,ND=Y                               
FrDte    LKOUT C,05,(D,B#SAVED,SU_FRDT),PDAT,ND=Y                               
ToDte    LKOUT C,06,(D,B#SAVED,SU_TODT),PDAT,ND=Y                               
TType    LKOUT C,07,(D,B#SAVED,SU_TYPE),CHAR,ND=Y                               
Ldgr     LKOUT C,08,(D,B#SAVED,SU_LEDG),CHAR,ND=Y                               
AccC1    LKOUT C,09,(D,B#SAVED,SU_1ACC),CHAR,ND=Y                               
AccN1    LKOUT C,10,(D,B#SAVED,SU_1NAM),CHAR,ND=Y                               
ProC     LKOUT C,11,(D,B#SAVED,SU_PROC),CHAR,ND=Y                               
ProN     LKOUT C,12,(D,B#SAVED,SU_PRON),CHAR,ND=Y                               
JobC     LKOUT C,13,(D,B#SAVED,SU_JOBC),CHAR,ND=Y                               
JobN     LKOUT C,14,(D,B#SAVED,SU_JOBN),CHAR,ND=Y                               
Status   LKOUT C,15,(D,B#SAVED,SU_STTV),CHAR,ND=Y                               
MtrTot   LKOUT C,16,(D,B#SAVED,SU_MTRS),CPAK,ND=Y                               
MNam1    LKOUT C,17,(D,B#SAVED,SU_PMNA),CHAR,ND=Y                               
                                                                                
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
                                                                                
         EJECT                                                                  
SAVED    DSECT                                                                  
                                                                                
REQVALS  DS    0A                  REQUEST VALUES                               
REQIND   DS    XL1                                                              
REQAREQ  DS    AL3                                                              
REQVALLQ EQU   *-REQVALS                                                        
                                                                                
         DS    0D                                                               
BLNKLNS  DS    F                   NUMBER OF BLANK LINES                        
SAVER1   DS    F                                                                
SAVER2   DS    F                                                                
SAVER2A  DS    F                                                                
ABRATAB  DS    A                   address of mcs date table                    
APRINTER DS    A                   A(PRINTER) - offline                         
ARCPRINT DS    A                   A(RCPRINT)                                   
APREVPER DS    A                   Address of previous period                   
                                                                                
OVALUES  DS    0D                                                               
DMKEY    DS    CL8                                                              
ACCDIR   DS    CL8                                                              
ACCMST   DS    CL8                                                              
ACCARC   DS    CL8                                                              
CTFILE   DS    CL8                                                              
LARE     DS    H                                                                
LAREADDR DS    S                                                                
PZERO    DS    PL1                                                              
ASTERS   DS    CL16                                                             
FFS      DS    XL16                                                             
PHRSMAX  DS    PL5                                                              
PHRSMIN  DS    PL5                                                              
ONENUL   DS    CL2                                                              
ONERUL   DS    CL2                                                              
TIMPRG   DS    XL2                                                              
OVALUESL EQU   *-OVALUES                                                        
*                                                                               
         DS    0D                                                               
AMASTC   DS    AL4                 A(MASTC)                                     
ABUFFRIN DS    AL4                 A(BUFFERIN)                                  
RUNMODE  DS    XL(L'RUNPMODE)      RUNNER/DDLINK MODE                           
*                                                                               
TSARABUF DS    XL(TSPXTNL)         TSAR block for approval buffer               
GAPAREA  DS    XL(GAPTLNQ)         Area to use for buffer rec                   
GAPAREA2 DS    XL(GAPTLNQ)         Area to use for buffer rec                   
GAPAREA3 DS    XL(GAPTLNQ)         Area to use for buffer rec                   
ATSRERRS DS    XL1                 Error area for buffer                        
TSAROBUF DS    XL(TSPXTNL)         TSAR block for optimisation buffer           
TSARTBUF DS    XL(TSPXTNL)         TSAR block for time output buffer            
RUNFLAGS DS    XL1                                                              
RUNNOPER EQU   X'80'                                                            
MYBYTE1  DS    XL1                                                              
                                                                                
DSDICTL  DS    0C                                                               
MCS@ACC  DS    CL18                                                             
MCS@RSTY DS    CL18                                                             
MCS@HRS  DS    CL18                                                             
MCS@WC   DS    CL18                                                             
MCS@INTR DS    CL18                                                             
MCS@ORDR DS    CL18                                                             
MCS@ITMM DS    CL18                                                             
MCS@ITMT DS    CL18                                                             
MCS@ITMP DS    CL18                                                             
MCS@TEXT DS    CL18                                                             
MCS@ITMC DS    CL18                                                             
MCS@NRTV DS    CL18                                                             
MCS@EST  DS    CL18                                                             
MCS@MOBL DS    CL18                                                             
*                                                                               
DPIDIND2 DS    X                                                                
DAPID2   DS    AL3                 BUILT PID IN WMP                             
DPIDMAXQ EQU   700                                                              
*                                                                               
SAVEVAR  DS    0F                                                               
AGAPAREA DS    F                   A(Table used for GAPLST)                     
X#IND    DS    XL1                                                              
X#ILNMA  EQU   X'10'               USE LINE MANAGER TIME ONLY                   
X#IORD#  EQU   X'80'               Order number required                        
X#IINTRF EQU   X'40'               Internal reference required                  
X#IPERSN EQU   X'20'               Person has been shown previously             
X#IEST#  EQU   X'08'               Estimate number required                     
X#ITEMS  DS    XL3                 number of items                              
X#SRCIND DS    XL1                 search indicator                             
X#SRCAPP EQU   X'10'               approver view                                
X#SRCRET EQU   X'20'               approver for awaiting approval               
X#SRCFND EQU   X'40'               found approver rules                         
X#SRCMRR EQU   X'80'               Get my reports reports                       
X#SRCLMR EQU   X'08'               Do limit list followed by approvals          
*                                   and my approvers reports                    
X#SRCIRR EQU   X'04'               Initialise my reports report                 
                                                                                
                                                                                
X#DITMS  DS    XL1                 num of periods saved from calendar           
X#SEQN   DS    XL1                                                              
X#SVSEQ  DS    XL1                                                              
X#TU     DS    CL1                 Profile Target Utilisation used              
X#HIRED  DS    PL3                                                              
X#TERMD  DS    PL3                                                              
X#CUTDT  DS    PL3                                                              
X#MINHR  DS    PL6                                                              
X#MAXHR  DS    PL6                                                              
X#SVHRS  DS    PL6                                                              
X#SVCHR  DS    PL4                                                              
X#SVMTS  DS    PL6                                                              
X#TUPCT  DS    PL4                 Target Utilisation percentage                
X#WRKDT  DS    PL3                                                              
X#PEREND DS    PL3                                                              
X#PHOLAC DS    CL(L'ACTKULA)                                                    
X#SHOLAC DS    CL(L'ACTKULA)                                                    
X#STAT   DS    CL1                                                              
X#GAOV   DS    CL1                                                              
X#REQST  DS    CL10                                                             
X#FLTST  DS    XL10                                                             
X#LCOMP  DS    CL(L'LOCOFF+L'LOCDEPT+L'LOCSUB)                                  
*                                                                               
X#LMPIN  DS    XL2                 LINE MANAGER PIN (BINARY PID)                
X#LMPID  DS    CL8                 LINE MANAGER PID                             
X#LMFNM  DS    CL16                LINE MANAGER FIRST NAME                      
X#LMMNM  DS    CL16                LINE MANAGER MIDDLE NAME                     
X#LMSNM  DS    CL58                LINE MANAGER LAST NAME                       
X#LMEML  DS    CL52                LINE MANAGER EMAIL ADDRESS                   
X#LML    EQU   *-X#LMPIN                                                        
                                                                                
TS#AGPT  DS    A                   POINTS TO NEXT GENAREA ENTRY                 
                                                                                
TS#PSPID DS    XL2                 SECURITY PID BINARY                          
TS#PIDC  DS    CL8                 SECURITY PID CHARACTER                       
TS#LMPIN DS    XL2                 LINE MANAGER PIN BINARY                      
TS#PRSAC DS    CL8                 ACCOUNTING PERSON CODE                       
TS#RECDA DS    XL4                 DISK ADDRESS                                 
*                                                                               
TS#EFSTA DS    PL3                 EFFECTIVE START DATE (LOC, ELSE PER)         
TS#EFEND DS    PL3                 EFFECTIVE END DATE (LOC, ELSE PER)           
*                                                                               
TS#ICPJ  DS    XL1                                                              
TS#ICLI  EQU   X'80'               CLIENT LEVEL FOUND                           
TS#IPRD  EQU   X'01'               PRODUCT LEVEL FOUND                          
TS#IJOB  EQU   X'10'               JOB LEVEL FOUND                              
TS#INON  EQU   X'20'               NON CLIENT LEVEL FOUND                       
TS#IOFFR EQU   X'02'               OFFICE LEVEL FOUND                           
TS#IDPTR EQU   X'04'               DEPARTMENT LEVEL FOUND                       
TS#ISDPR EQU   X'08'               SUB-DEPARMENT LEVEL FOUND                    
TS#IPERR EQU   X'40'               PERSON LEVEL FOUND                           
*                                                                               
TS#ACNMS DS    0CL36                                                            
TS#CLINM DS    CL36                CLIENT NAME                                  
TS#PRDNM DS    CL36                PRODUCT NAME                                 
TS#JOBNM DS    CL36                JOB NAME                                     
TS#ACNMQ EQU   *-TS#ACNMS                                                       
*                                                                               
TS#1RACT DS    CL12                1R ACCOUNT CODE                              
TS#1ROFF DS    CL2                 OFFICE                                       
TS#1RDPT DS    CL6                 DEPARTMENT                                   
TS#1RSDP DS    CL6                 SUB-DEPARTMENT                               
TS#SJOFF DS    CL2                 OFFICE OF PRODUCTION ACCOUNT                 
TS#WCD   DS    CL2                 WORKCODE                                     
TS#ATYP  DS    CL1                 APPROVER TYPE                                
TS#INDX  DS    H                   TIMESHEET INDEX NUMBER                       
TS#STAT  DS    XL1                 TIMESHEET STATUS FROM TIMRECD                
TS#IEDT  DS    XL1                 EDIT HOURS INDICATOR                         
TS#IOFF  EQU   X'80'               SET OFFICE IN KEY                            
TS#IDPT  EQU   X'40'               SET DEPARTMENT IN KEY                        
TS#ISDP  EQU   X'20'               SET SUB-DEPARTMENT IN KEY                    
TS#IPER  EQU   X'10'               SET PERSON CODE IN KEY                       
TS#ISHOL EQU   X'08'               READ FOR STAFF HOLIDAYS                      
TS#EDHRS DS    PL4                 EDIT HOURS TOTAL                             
*                                                                               
TS#PEDTE DS    PL3                 PERIOD END DATE 2'S COMPLEMENTED             
TS#PEDT  DS    PL3                 PERIOD END DATE                              
TS#DATE  DS    PL3                 DATE FIELD FOR COMMON ROUTINES               
*                                                                               
TS#PBLK  DS    0X                  PERSON DETAILS (CONNECTED)                   
TS#PCOD  DS    CL8                 - PERSON CODE                                
TS#PPID  DS    XL2                 - PERSON PID                                 
TS#PDAT  DS    XL3                 - REQUIRED DATE/START DATE                   
TS#PEND  DS    XL3                 - END DATE                                   
TS#PEST  DS    XL3                 - ESTIMATED START DATE                       
TS#PREC  DS    A                   - ADDRESS OF PERRECD                         
TS#PEMP  DS    A                   - ADDRESS OF EMPELD                          
TS#PCLE  DS    A                   - ADDRESS OF CURRENT LOCELD                  
TS#PCLE2 DS    A                   - ADDRESS OF MOST RECENT LOCELD              
TS#PBLQ  EQU   *-TS#PBLK                                                        
                                                                                
TS#APOFF DS    CL2                 The approvers office                         
TS#COFF  DS    CL2                                                              
TS#CDAT  DS    XL3                                                              
TS#CEND  DS    XL3                                                              
                                                                                
TS#CSDT  DS    XL3                 Start date                                   
TS#CEDT  DS    XL3                 End date                                     
TS#CRSD  DS    XL3                 Start date 2's complement                    
TS#CRED  DS    XL3                 End date 2's complement                      
                                                                                
TS#PTYPE DS    XL1                 VARIOUS SEARCHS                              
TS#PTLMD EQU   X'01'               List My timesheets by Date                   
TS#PTLAD EQU   X'02'               List Approvals by Date                       
TS#PTLMS EQU   X'03'               List My timesheets by Status                 
TS#PTLAS EQU   X'04'               List Approvals by Status                     
TS#PTRTP EQU   X'05'               seaRch Timesheets by Person (date)           
TS#PTRB  EQU   X'06'               seaRch Back up approvals                     
TS#PTRHC EQU   X'07'               seaRch Hours by Client (or non-cli)          
TS#PTRTO EQU   X'08'               seaRch Timesheets by Off/dpt/subdpt          
TS#PTRTA EQU   X'09'               seaRch Timesheets by approver                
TS#PTRHO EQU   X'0A'               seaRch Hours by Off/dpt/subdpt               
TS#PTRHP EQU   X'0B'               seaRch Hours for a Person (by date)          
TS#PTSMS EQU   X'0C'               Summary Month by Staff                       
TS#PTSWS EQU   X'0D'               Summary Week by Staff                        
TS#PTSMP EQU   X'0E'               Summary Month for a Person                   
TS#PTSWP EQU   X'0F'               Summary Week for a Person                    
TS#PTSMA EQU   X'10'               Summ. Month by (cli/non-cli) Account         
TS#PTSWA EQU   X'11'               Summ. Week by (Cli/non-cli) Account          
TS#PTSMC EQU   X'12'               Summ. Month for a Client(/non-cli)           
TS#PTSWC EQU   X'13'               Summ. Week for a Client(/non-cli)            
                                                                                
GAPLCALT DS    XL1                 Type of GAPLST                               
GAPLFILT DS    0CL14               Filter for GAPLST                            
GAPLRUNL DS    CL2                 Unit ledger                                  
GAPLRACT DS    CL12                Account                                      
GAPLPARM DS    XL1                                                              
                                                                                
X#FLTVL  DS    0X                                                               
X#WRKC   DS    CL2                                                              
X#SVUL   DS    CL2                                                              
X#SJACC  DS    CL12               c/p/j account filter                          
X#1NACC  DS    CL12               1N acc filter                                 
X#1RACT  DS    CL12                                                             
X#1ROFF  DS    CL2                                                              
X#1RDEP  DS    CL3                                                              
X#1RSUB  DS    CL3                                                              
X#1RPER  DS    CL8                                                              
X#APPID  DS    XL2                                                              
X#FLTVQ  EQU   *-X#FLTVL                                                        
                                                                                
X#ACCSTA DS   XL1                 Access Status                                 
X#CLISR  EQU  X'40'               Currently Doing an CLI Search                 
                                                                                
MAP      DS    XL2                 REQUEST MAP NUMBER                           
X#ACTVT  DS    XL1                                                              
X#KEYEX  DS    XL1                                                              
                                                                                
X#FILST  DS    XL1                                                              
X#S1RAC  DS    XL12                                                             
X#JOBAC  DS    XL15                                                             
X#ULAC   DS    XL14                                                             
                                                                                
X#CLIAC  DS    XL15                                                             
X#PROAC  DS    XL15                                                             
X#CPJLQ  EQU   *-X#CLIAC                                                        
                                                                                
X#CLINA  DS    CL36                                                             
X#PRONA  DS    CL36                                                             
X#JOBNA  DS    CL36                                                             
X#JOBLK  DS    CL1                                                              
X#JOBCL  DS    CL1                                                              
X#CPJLN  EQU   *-X#CLINA                                                        
*                                                                               
X#OFFNA  DS    CL36                                                             
X#DPTNA  DS    CL36                                                             
X#SDPNA  DS    CL36                                                             
X#OFFLN  EQU   *-X#OFFNA                                                        
*                                                                               
DACT     DS    CL12                1R ACCOUNT CODE                              
DPERSON  DS    CL8                 PERSON CODE                                  
*                                                                               
X#EMPEL  DS    A                   ADDRESS OF EMPLOYMENT ELEMENT                
X#AGADR  DS    A                   ADDRESS OF ROUTINE FOR IO                    
X#ATSTT  DS    A                   ADDRESS OF STATUS TABLE                      
X#ACALL  DS    A                   ADDRESS OF CALENDAR TABLE                    
SVADRPN  DS    A                   Saved address of WMP of PINs                 
COUNTHW  DS    H                   Counter for number of PINs                   
                                                                                
X#LSTNT  DS    XL(SU_LNQ)                                                       
X#SUMTAB DS    7XL(SUMTABL)        SUMMARY DATE TABLE, SEE SUMTABD              
X#STEOT  DS    XL1                    7 days, 6 months or 5/6 PERIODS           
SAVELN1Q EQU   *-SAVEVAR                                                        
                                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST IN/OUT DEFINITION IN STORAGE                                *         
***********************************************************************         
                                                                                
RQ_VALS  DS    0X                  ** REQUEST VALUES                            
RQ_TYPE  DS    XL1                 - Request type                               
RQ_DATA  DS    0XL1                - Request data                               
                                                                                
* TimeSheet Audit Trail                                                         
RQ_ATPER DS    CL8                 - Person accounting code                     
RQ_ATDAT DS    CL8                 - Period end date                            
RQ_ALDAT DS    CL8                 - Location end date                          
         ORG   RQ_DATA                                                          
                                                                                
* TimeSheet List D/load                                                         
RQ_TLTY  DS    CL1                 - Type                                       
RQ_TLDQ  EQU   C'D'                  'by date'                                  
RQ_TLSQ  EQU   C'S'                  'by status'                                
RQ_TLSU  DS    CL1                 - Sub type                                   
RQ_TLAQ  EQU   C'A'                  'approver'                                 
RQ_TLMQ  EQU   C'M'                  'mine'                                     
RQ_TLST  DS    CL1                 - Status values                              
RQ_TLSD  DS    CL8                 - Start date                                 
RQ_TLED  DS    CL8                 - End date                                   
RQ_TLGC  DS    CL1                 - GAOV call Y/N                              
         ORG   RQ_DATA                                                          
                                                                                
* Time Summary View Download                                                    
RQ_SUDTE DS    CL8                 - Date                                       
RQ_SUVIE DS    CL1                 - Week or Month view                         
RQ_SUVMQ EQU   C'M'                  'month'                                    
RQ_SUVWQ EQU   C'W'                  'week' (period)                            
RQ_SUTYP DS    CL1                 - Type                                       
RQ_SUT1Q EQU   C'1'                  'staff summary - totals'                   
RQ_SUT2Q EQU   C'2'                  'staff summary - details'                  
RQ_SUT3Q EQU   C'3'                  'client summary - totals'                  
RQ_SUT4Q EQU   C'4'                  'client summary - details'                 
RQ_SUPER DS    CL8                 - Person for RQ_SUT2Q                        
RQ_SUCLI DS    CL6                 - Client for RQ_SUT4Q                        
RQ_SU1NA DS    CL12                - 1N a/c for RQ_SUT4Q                        
RQ_SUX1N DS    CL1                 - Security Y/N to exclude 1N if '3'          
         ORG   RQ_DATA                                                          
                                                                                
* TimeLine filtered TimeSheet                                                   
RQ_TTYPE DS    CL1                 - TYPE                                       
RQ_THOUR EQU   C'1'                - HOURS                                      
RQ_TTS   EQU   C'2'                - TIMESHEETS                                 
RQ_TTSTD DS    CL8                 - START DATE                                 
RQ_TTEND DS    CL8                 - END DATE                                   
RQ_NSTRT DS    CL1                 - NOT STARTED                                
RQ_OVERD DS    CL1                 - OVERDUE                                    
RQ_INPRG DS    CL1                 - IN PROGRESS                                
RQ_SUBMT DS    CL1                 - SUBMITTED                                  
RQ_APPRD DS    CL1                 - APPROVED                                   
RQ_REJED DS    CL1                 - REJECTED                                   
RQ_AWAIT DS    CL1                 - AWAITING APPROVAL                          
RQ_PRTAP DS    CL1                 - PART APPROVED                              
RQ_TSSRC DS    CL1                 - SEARCH REQUIRED                            
RQ_TSTMY EQU   C'1'                - MY TIMESHEETS                              
RQ_TSTST EQU   C'2'                - STAFF                                      
RQ_TSTBA EQU   C'3'                - BACK UP APPROVALS                          
RQ_TSTAP EQU   C'4'                - MY STAFF FOR 4 PREVIOUS PERIODS            
RQ_TSUNA DS    CL1                 - INCLUDE UNAPPROVED TIME                    
RQ_TSBIL DS    CL1                 - BILLABLE                                   
RQ_TSNBL DS    CL1                 - NON-BILLABLE                               
RQ_TSCHG DS    CL1                 - CHARGEABLE                                 
*&&UK                                                                           
RQ_TTCLI DS    CL5                 - CLIENT                                     
RQ_TTPRO DS    CL2                 - PRODUCT                                    
*&&                                                                             
*&&US                                                                           
RQ_TTCLI DS    CL3                 - CLIENT                                     
RQ_TTPRO DS    CL3                 - PRODUCT                                    
*&&                                                                             
RQ_TTJOB DS    CL6                 - JOB                                        
RQ_TTWCD DS    CL2                 - WORK CODE                                  
RQ_TT1NA DS    CL12                - 1N ACCOUNT                                 
RQ_TTOFF DS    CL2                 - OFFICE                                     
RQ_TTDEP DS    CL6                 - DEPARTMENT                                 
RQ_TTSUB DS    CL6                 - SUB DEPARTMENT                             
RQ_TTPRS DS    CL8                 - PERSON CODE                                
RQ_TTHMI DS    CL16                - HOURS ENTERED (MIN)                        
RQ_TTHMA DS    CL16                - HOURS ENTERED (MAX)                        
RQ_TPRSR DS    CL1                 - PERSON SEARCH OVERRIDE                     
RQ_ORD#  DS    CL6                 - ORDER NUMBER                               
RQ_INTRF DS    CL12                - INTERNAL REFERENCE                         
RQ_EST#  DS    CL6                 - ESTIMATE NUMBER                            
RQ_APP1R DS    CL1                 - APPLY 1R ACCESS RIGHTS                     
RQ_APIVN DS    XL1                 - API version number                         
RQ_MYRR  DS    XL1                 - show my reports reports                    
RQ_LMPID DS    CL8                 - line manager pid                           
                                                                                
RQMAXQ   EQU   *-RQ_DATA+64                                                     
                                                                                
         ORG   RQ_DATA+RQMAXQ                                                   
                                                                                
         EJECT                                                                  
SAVEVALS DS    0X                                                               
                                                                                
* Audit trail values                                                            
AT_VALS  DS    0X                                                               
AT_TYPE  DS    CL1                   Type of change                             
AT_TRWCH EQU   C'1'                  Row change                                 
AT_TRWDL EQU   C'2'                  Row deletion                               
AT_TRWAD EQU   C'3'                  Row addition                               
AT_TTSST EQU   C'4'                  Timesheet status change                    
AT_TTSAD EQU   C'5'                  Timesheet added                            
AT_TAPPR EQU   C'6'                  Approved                                   
AT_TREJT EQU   C'7'                  Rejected                                   
AT_TMTCH EQU   C'8'                  Materials change                           
AT_TMTDL EQU   C'9'                  Materials deletion                         
AT_TMTAD EQU   C'A'                  Materials addition                         
AT_DATE  DS    XL3                   Date                                       
AT_TIME  DS    CL6                   Time                                       
AT_PRFN  DS    CL16                  Person First Name                          
AT_PRMN  DS    CL16                  Person Middle Name                         
AT_PRLN  DS    CL58                  Person Last Name                           
AT_ROW   DS    CL4                   Row number                                 
AT_STFR  DS    CL1                   Status from                                
AT_STTO  DS    CL1                   Status to                                  
AT_CMNTS DS    CL100                 Comments                                   
AT_PID   DS    CL8                   Security char PID                          
AT_USER  DS    CL30                  User-id                                    
AT_MROW  DS    XL2                   Row number - materials                     
AT_TACC  DS    CL14                  To: Account                                
AT_TTY   DS    CL1                   To: Type of time                           
AT_TWC   DS    CL2                   To: Workcode                               
AT_THRS  DS    PL3                   To: Hours                                  
AT_TNAR  DS    CL60                  To: Narrative/materials text               
AT_TORD  DS    CL6                   To: Order number                           
AT_TINT  DS    CL12                  To: Internal reference                     
AT_TTOT  DS    PL6                   To: Materials total                        
AT_TPRC  DS    PL6                   To: Materials price                        
AT_TMUL  DS    PL6                   To: Materials multiplier                   
AT_TCDE  DS    CL4                   To: Materials code                         
AT_TEST  DS    CL6                   To: Estimate number                        
AT_APPL  DS    XL1                   Application                                
AT_ABROQ EQU   0                     BrandOcean                                 
AT_AMOBQ EQU   1                     Mobile                                     
AT_AAURQ EQU   2                     Aura                                       
AT_ASPEQ EQU   3                     Spectra legacy applications                
AT_ATIMO EQU   4                     Timeoff                                    
AT_AWIDG EQU   5                     Widget                                     
AT_AAPI  EQU   6                     API                                        
AT_LNQ   EQU   *-AT_VALS                                                        
         ORG   SAVEVALS                                                         
                                                                                
* TimeLine TimeSheet Search                                                     
SR_VALS  DS    0X                                                               
SR_PERC  DS    CL8                   Person Code                                
SR_PIDC  DS    CL8                   PID Code                                   
SR_PERF  DS    CL16                  Person First name                          
SR_PERM  DS    CL16                  Person First name                          
SR_PERL  DS    CL58                  Person Last Name                           
SR_STAD  DS    PL3                   Start date (Period)                        
SR_ENDD  DS    PL3                   End date (Period)                          
SR_PERD  DS    CL3                   Period Number                              
SR_LOCS  DS    PL3                   Location start date                        
SR_LOCE  DS    PL3                   Location end date                          
SR_HOUR  DS    PL4                   Hours                                      
SR_STTV  DS    CL1                   Status to viewer                           
SR_STTS  DS    CL1                   Status of timesheet                        
SR_IDFN  DS    CL4                   ID Number                                  
SR_EHRS  DS    PL4                   Edit Hours                                 
SR_PRNS  DS    CL1                   Previous timesheet submitted               
SR_COPTA DS    CL1                   Disallow input if prev t/s not sub         
SR_OFFC  DS    CL2                   Office code                                
SR_OFNM  DS    CL36                  Office name                                
SR_DEPT  DS    CL6                   Department code                            
SR_DPNM  DS    CL36                  Department name                            
SR_SBDP  DS    CL6                   Sub-Department code                        
SR_SDNM  DS    CL36                  Sub-Department name                        
SR_TYPE  DS    CL1                   Type of Timeline (R/N/B/X)                 
*&&UK                                                                           
SR_CLCD  DS    CL5                   Client code                                
*&&                                                                             
*&&US                                                                           
SR_CLCD  DS    CL3                   Client code                                
*&&                                                                             
SR_CLNA  DS    CL36                  Client name                                
*&&UK                                                                           
SR_PRCD  DS    CL2                   Product code                               
*&&                                                                             
*&&US                                                                           
SR_PRCD  DS    CL3                   Product code                               
*&&                                                                             
SR_PRNA  DS    CL36                  Product name                               
SR_JOCD  DS    CL6                   Job code                                   
SR_JONA  DS    CL36                  Job name                                   
SR_JOLK  DS    CL1                   Job locked                                 
SR_CLSJB DS    CL1                   Job closed                                 
SR_WRKC  DS    CL2                   Work code                                  
SR_WCDE  DS    CL15                  Work code description                      
SR_1NCD  DS    CL12                  1N account code                            
SR_1NNA  DS    CL36                  1N account name                            
SR_MTRS  DS    PL6                   materials                                  
SR_ORD#  DS    CL6                   order number                               
SR_INTRF DS    CL12                  internal reference                         
SR_ORDNM DS    CL100                 order name                                 
SR_NARR  DS    CL(L'TIMNARR)         narrative                                  
SR_TU    DS    PL4                   target utilisation                         
SR_CHRS  DS    PL4                   client hours                               
SR_EST#  DS    CL6                   estimate global number                     
SR_ESTNM DS    CL50                  estimate name                              
SR_ESTL# DS    XL1                   estimate local number                      
SR_LMPID DS    CL8                   line manager PID                           
SR_LMFNM DS    CL16                  line manager first name                    
SR_LMMNM DS    CL16                  line manager middle name                   
SR_LMSNM DS    CL58                  line manager surname                       
SR_LMEML DS    CL52                  line manager email address                 
SR_ZHTS  DS    CL1                   Zero hour timesheet                        
SR_EMAIL DS    CL70                  Email address of person                    
SR_LNQ   EQU   *-SR_VALS                                                        
         ORG   SAVEVALS                                                         
                                                                                
* TimeSheet List Download                                                       
TS_VALS  DS    0X                                                               
TS_PERC  DS    CL8                   Person Code                                
TS_PIDC  DS    CL8                   PID Code                                   
TS_PERF  DS    CL16                  Person First name                          
TS_PERM  DS    CL16                  Person Middle Name                         
TS_PERL  DS    CL58                  Person Last Name                           
TS_STAD  DS    PL3                   Start date (Period)                        
TS_ENDD  DS    PL3                   End date (Period)                          
TS_PERD  DS    CL3                   Period Number                              
TS_LOCS  DS    PL3                   Location start date                        
TS_LOCE  DS    PL3                   Location end date                          
TS_HOUR  DS    PL4                   Total Hours                                
TS_STTV  DS    CL1                   Status to viewer                           
TS_STTS  DS    CL1                   Status of timesheet                        
TS_IDFN  DS    CL4                   ID Number                                  
TS_EHRS  DS    PL4                   Edit Hours                                 
TS_PRNS  DS    CL1                   Previous timesheet submitted               
TS_COPTA DS    CL1                   Disallow input if prev t/s not sub         
TS_MTRS  DS    PL6                   materials                                  
TS_CHRS  DS    PL4                   Client hours                               
TS_TU    DS    PL4                   Target Utilisation                         
TS_OFFC  DS    CL2                   Office code                                
TS_OFNM  DS    CL36                  Office name                                
TS_DEPT  DS    CL6                   Department code                            
TS_DPNM  DS    CL36                  Department name                            
TS_SDEP  DS    CL2                   Subdepartment code                         
TS_SDNM  DS    CL36                  Subdepartment name                         
TS_ZHTS  DS    CL1                   Zero hour timesheet Y/N                    
TS_LNQ   EQU   *-TS_VALS                                                        
         ORG   SAVEVALS                                                         
                                                                                
* Time Summary Download                                                         
SU_VALS  DS    0X                                                               
SU_EHRS  DS    PL4                   Hours for Entry                            
SU_PCOD  DS    CL7                   Person Code                                
SU_PFNA  DS    CL16                  Person first name                          
SU_PMNA  DS    CL16                  Person middle name                         
SU_PLNA  DS    CL58                  Person last name                           
SU_FRDT  DS    PL3                   Period from date                           
SU_TODT  DS    PL3                   Period to date                             
SU_TYPE  DS    CL1                   Type of time B R N C                       
SU_LEDG  DS    CL2                   Ledger (SJ or 1N)                          
SU_1ACC  DS    CL12                  Client /1N a/c code                        
SU_1NAM  DS    CL36                  Client/1N a/c name                         
SU_PROC  DS    CL6                   Product code                               
SU_PRON  DS    CL36                  Product name                               
SU_JOBC  DS    CL6                   Job code                                   
SU_JOBN  DS    CL36                  Job name                                   
SU_STTV  DS    CL1                   status to viewer                           
SU_MTRS  DS    PL6                   materials                                  
SU_LNQ   EQU   *-SU_VALS                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
                                                                                
PERTABD  DSECT                                                                  
PERENDT  DS    PL3                 Period end date 2's complement               
PERODS   DS    CL(L'TSWKODS)       office/dept/sub-dept for this period         
PERLOCE  DS    PL3                 location period end date                     
PERKEYQ  EQU   *-PERTABD                                                        
PERSTDT  DS    PL3                 Period start date                            
PERNUM   DS    XL1                 Period number                                
PERSTAT  DS    XL1                 Period status                                
PERSNFND EQU   X'80'               Not found                                    
PERSFOND EQU   X'40'               Found timesheet                              
PERSSKIP EQU   X'20'               skip this period as security lockout         
PERSREAD EQU   X'10'               Read for this period                         
PERSMCSU EQU   X'08'               MCS user for this period                     
PERSINVL EQU   X'04'               skip location as invalid for viewer          
PERLOCS  DS    PL3                 location period start date                   
PERLENQ  EQU   *-PERTABD                                                        
*                                                                               
COMNTTBD DSECT                                                                  
COMNTDIC DS    S                                                                
COMNTSTA DS    XL1                                                              
COMNTST2 DS    XL1                                                              
COMNTTBL EQU   *-COMNTTBD                                                       
*                                                                               
CLITABD  DSECT                                                                  
CLIVIEW  DS    CL(L'TSJPVIEW)                                                   
CLICDE   DS    CL(L'TSJPACT)                                                    
CLICLEN  EQU   *-CLITABD           CLI CODE ENTRY LENGTH                        
         ORG   CLIVIEW                                                          
PERHACC  DS    CL(L'TSJPACT)       SUMMARY DETAIL HEADER (A/C PLUS VIEW         
PERHVIEW DS    XL(L'TSJPVIEW)                              FOR LEDGER           
         ORG   CLIVIEW                                                          
PERCOFC  DS    CL(L'TD#OFFC)                                                    
PERCDPT  DS    CL(L'TD#DEPT)                                                    
PERCSDP  DS    CL(L'TD#SBDP)                                                    
PERCDE   DS    CL(L'TD#PERC)                                                    
PERCPODS EQU   *-CLITABD                                                        
*                                                                               
SUMTABD  DSECT                     COVERS X#SUMTAB                              
STSTART  DS    XL(L'TS#CSDT)                                                    
STEND    DS    XL(L'TS#CEDT)                                                    
STSTAT   DS    XL1                                                              
STSTFUND EQU   X'80'               HOURS FOUND FOR THIS PERIOD                  
SUMTABL  EQU   *-SUMTABD                                                        
         EJECT                                                                  
*                                                                               
GSECTABD DSECT                                                                  
GSECTYP  DS    CL1                 TYPE                                         
GSECDSP  DS    XL2                 DISPLACEMENT                                 
GSECLEN  DS    XL1                 LENGTH                                       
GSECULG  DS    CL2                 UNIT/LEDGER                                  
GSECTABL EQU   *-GSECTABD                                                       
*                                                                               
***********************************************************************         
* Optimisation buffer record layout - defined to save whole accmst rec*         
***********************************************************************         
                                                                                
OB_D     DSECT                                                                  
*                                                                               
OB_KEY   DS    XL42                Record key                                   
*                                                                               
OB_DATA  DS    0X                  Data starts here                             
         ORG   OB_D+2048           allow 2K, same as accmst                     
                                                                                
OB_DATAL EQU   *-OB_DATA                                                        
OB_LNQ   EQU   *-OB_D                                                           
         EJECT                                                                  
***********************************************************************         
* included books                                                      *         
***********************************************************************         
                                                                                
         PRINT OFF                                                              
*PREFIX=X_                                                                      
       ++INCLUDE DDDPRINTL                                                      
*PREFIX=                                                                        
         ORG   X_P                                                              
PLINEL   DS    0CL(L'X_P)           ** DATAMGR trace line **                    
PDESC    DS    CL50                                                             
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'140ACBRA12   11/02/20'                                      
         END                                                                    
